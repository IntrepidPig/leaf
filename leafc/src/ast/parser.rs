use ast::lexer::{Bracket, BracketState};
use ast::tokenizer::{Token, Keyword, Symbol as TokenSymbol};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Symbol {
	Colon,
	Equals,
	Semicolon,
	ExpressionPointer,
	Exclamation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
	Namespace,
	Dot,
	Equality,
	Asterisk,
	Greater,
	GreaterEqual,
	Less,
	LessEqual,
	Ampersand,
	Question,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxTree {
	pub block: Vec<Statement>,
	pub output: Option<Expression>,
}

impl SyntaxTree {
	pub fn push(&mut self, syntax: Statement) -> Result<(), ParseError> {
		self.block.push(syntax);
		
		Ok(())
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
	Binding(Binding),
	Expression(Expression),
	Debug(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
	pub name: String,
	pub args: Vec<Binding>,
	pub return_type: String,
	pub body: SyntaxTree,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
	Binary {
		left: Box<Expression>,
		right: Box<Expression>,
		op: BinaryOp,
	},
	FunctionCall {
		base: Option<Box<Expression>>,
		function: String,
		args: Vec<Expression>,
	},
	Identifier(String),
	Block(Box<SyntaxTree>),
	StringLiteral(String),
	NumberLiteral(u64),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
	Add,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding {
	pub mutable: bool,
	pub ident: String,
	pub bind_type: Option<String>,
	pub val: Option<Expression>,
}

impl SyntaxTree {
	pub fn new() -> Self {
		SyntaxTree {
			block: Vec::new(),
			output: None,
		}
	}
}

#[derive(Debug, Clone)]
pub enum ParseError {
	UnexpectedToken(usize),
	UnclosedBrace(usize),
	Other,
}

pub fn parse(tokens: &[Token]) -> Result<SyntaxTree, ParseError> {
	if let Some(ast) = next_syntaxtree(tokens)? {
		Ok(ast)
	} else {
		Err(ParseError::Other)
	}
}

pub fn next_syntaxtree<'a>(mut tokens: &'a [Token]) -> Result<Option<SyntaxTree>, ParseError> {
	let mut stree = SyntaxTree::new();

	while let Some((stmnt, leftovers)) = next_statement(tokens)? {
		stree.block.push(stmnt);
		tokens = leftovers;
	}
	if let Some((expr, leftovers)) = next_expression(tokens)? {
		if !leftovers.is_empty() {
			return Err(ParseError::UnexpectedToken(0))
		}
		stree.output = Some(expr);
	}

	Ok(Some(stree))
}

pub fn next_statement<'a>(tokens: &'a [Token]) -> Result<Option<(Statement, &'a [Token])>, ParseError> {
	if let Some((binding, leftovers)) = next_binding(tokens)? {
		return Ok(Some((Statement::Binding(binding), leftovers)))
	} else if let Some((expression, leftovers)) = next_expression(tokens)? {
		if leftovers.get(0) == Some(&Token::Symbol(TokenSymbol::Semicolon)) {
			return Ok(Some((Statement::Expression(expression), &leftovers[1..leftovers.len()])))
		} else {
			return Ok(None)
		}
	} else if let Some((debug, leftovers)) = next_debug(tokens)? {
		return Ok(Some((Statement::Debug(debug), leftovers)))
	}

	Ok(None)
}

pub fn next_block<'a>(tokens: &'a [Token]) -> Result<Option<(SyntaxTree, &'a [Token])>, ParseError> {
	if tokens.is_empty() {
		return Ok(None)
	}
	match tokens[0] {
		Token::Bracket(Bracket::Curly, BracketState::Open) => {
			let mut buf = 0;
			for (i, token) in tokens[0..tokens.len()].iter().enumerate() {
				if let Some((bracket, bracketstate)) = token.as_bracket() {
					match (bracket, bracketstate) {
						(Bracket::Curly, BracketState::Open) => buf += 1,
						(Bracket::Curly, BracketState::Close) => buf -= 1,
						_ => {},
					}
				}
				if buf == 0 {
					// Parse whatever's inside this block as an AST [after first brace..last brace]
					let ast = if let Some(ast) = next_syntaxtree(&tokens[1..i])? {
						ast
					} else {
						return Err(ParseError::UnexpectedToken(0))
					};
					// And the leftover tokens [after last brace..end of tokens]
					return Ok(Some((ast, &tokens[i + 1..tokens.len()])));
				}
			}

			return Err(ParseError::UnclosedBrace(0))
		},
		_ => return Ok(None)
	}
}

#[allow(unused_mut)]
fn next_binding<'a>(tokens: &'a [Token]) -> Result<Option<(Binding, &'a [Token])>, ParseError> {
	if tokens.is_empty() {
		return Ok(None)
	}

	let mut tokens = tokens;

	match tokens[0] {
		Token::Keyword(Keyword::Let) => {
			let ident = if let Token::Name(ref name) = tokens[1] {
				name.to_owned()
			} else {
				return Err(ParseError::UnexpectedToken(0)) // TODO add expected 'identifier'
			};
			
			let mut offset = 0;
			let mut type_annotation: Option<String> = if let Token::Symbol(TokenSymbol::Colon) = tokens[2] {
				// if there's a type annotation
				// change token offset
				unimplemented!()
			} else {
				None
			};

			if let Token::Symbol(TokenSymbol::Equals) = tokens[2 + offset] {  } else {
				return Err(ParseError::UnexpectedToken(0))
			}

			let expr = if let Some((expr, leftovers)) = next_expression(&tokens[2 + offset + 1..tokens.len()])? {
				tokens = leftovers;
				expr
			} else {
				return Err(ParseError::UnexpectedToken(0))
			};

			if let Some(&Token::Symbol(TokenSymbol::Semicolon)) = tokens.get(0) {
				tokens = &tokens[1..tokens.len()];
			} else {
				return Err(ParseError::UnexpectedToken(0))
			}

			Ok(Some((Binding {
				mutable: false,
				ident,
				bind_type: type_annotation,
				val: Some(expr),
			}, tokens)))
		},
		_ => Ok(None),
	}
}

fn next_debug<'a>(tokens: &'a [Token]) -> Result<Option<(Expression, &'a [Token])>, ParseError> {
	if tokens.is_empty() {
		return Ok(None)
	}

	let mut tokens = tokens;

	match tokens[0] {
		Token::Keyword(Keyword::Debug) => {
			let expr = if let Some((expr, leftovers)) = next_expression(&tokens[1..tokens.len()])? {
				tokens = leftovers;
				expr
			} else {
				return Err(ParseError::UnexpectedToken(0))
			};

			
			if let Token::Symbol(TokenSymbol::Semicolon) = tokens[0] {
				tokens = &tokens[1..tokens.len()];
			} else {
				return Err(ParseError::UnexpectedToken(0))
			}

			Ok(Some((expr, tokens)))
		},
		_ => Ok(None),
	}
}


fn next_expression<'a>(tokens: &'a [Token]) -> Result<Option<(Expression, &'a [Token])>, ParseError> {
	if tokens.is_empty() {
		return Ok(None)
	}

	// TODO keep pulling until semicolon
	match tokens[0] {
		Token::NumberLiteral(num) => {
			Ok(Some((Expression::NumberLiteral(num), &tokens[1..tokens.len()])))
		},
		Token::Name(ref name) => {
			Ok(Some((Expression::Identifier(name.to_owned()), &tokens[1..tokens.len()])))
		},
		Token::Bracket(Bracket::Curly, BracketState::Open) => {
			Ok(next_block(tokens)?.map(|b| (Expression::Block(Box::new(b.0)), b.1)))
		},
		_ => Ok(None)
	}
}
