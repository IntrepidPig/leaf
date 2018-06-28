use ast::lexer::{Bracket, BracketState};
use ast::tokenizer::{Token, Keyword, Symbol as TokenSymbol};
use failure::{Error};

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
	Binary(BinaryOp),
	Prefix(PrefixOp),
	Postifx(PostfixOp),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
	Assign,
	Add,
	Equality,
	Greater,
	GreaterEqual,
	Less,
	LessEqual,
	Dot,
	Namespace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefixOp {
	Asterisk,
	Ampersand,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PostfixOp {
	Question,
	Exclamation,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxTree {
	pub block: Vec<Statement>,
	pub output: Option<Expression>,
}

impl SyntaxTree {
	pub fn push(&mut self, syntax: Statement) -> Result<(), Error<ParseError>> {
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
pub struct Assignment {
	pub ident: String,
	pub expr: Expression,
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
	UnexpectedToken,
	UnclosedBrace,
	Other,
}

impl ::std::fmt::Display for ParseError {
	fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
		write!(f, "{:?}", self)
	}
}

impl ::std::error::Error for ParseError {
	
}

struct TerminatorCounter {
	brackets: i32, // square brackets
	parens: i32,
	braces: i32, // curly braces
	semicolon: i32,
}

impl TerminatorCounter {
	pub fn new() -> Self {
		TerminatorCounter {
			brackets: 0,
			parens: 0,
			braces: 0,
			semicolon: 0,
		}
	}
	pub fn split_until_end(tokens: &[Token]) -> (&[Token], &[Token]) {
		let mut counter = TerminatorCounter::new();

		for (i, token) in tokens.iter().enumerate() {
			counter.add(token);
			if counter.is_terminated() {
				return (&tokens[0..i], &tokens[i..tokens.len()])
			}
		}

		(tokens, &[])
	}

	pub fn add(&mut self, token: &Token) {
		match token {
			Token::Symbol(symbol) => match symbol {
				TokenSymbol::Semicolon => {
					if !self.is_contained() {
						self.semicolon -= 1;
					}
				},
				_ => {},
			},
			Token::Bracket(bracket, bracketstate) => {
				let target = match bracket {
					Bracket::Curly => &mut self.braces,
					Bracket::Paren => &mut self.parens,
					Bracket::Square => &mut self.brackets,
				};

				*target += 1 * match bracketstate {
					BracketState::Open => 1,
					BracketState::Close => -1
				};
			},
			_ => {},
		}
	}

	pub fn is_terminated(&self) -> bool {
		self.braces < 0 || self.brackets < 0 || self.parens < 0 || self.semicolon < 0
	}

	pub fn is_contained(&self) -> bool {
		self.braces > 0 || self.brackets > 0 || self.parens > 0
	}
}

pub fn parse(tokens: &[Token]) -> Result<SyntaxTree, Error<ParseError>> {
	if let Some(ast) = next_syntaxtree(tokens)? {
		Ok(ast)
	} else {
		Err(ParseError::Other.into())
	}
}

pub fn next_syntaxtree<'a>(mut tokens: &'a [Token]) -> Result<Option<SyntaxTree>, Error<ParseError>> {
	let mut stree = SyntaxTree::new();

	while let Some((stmnt, leftovers)) = next_statement(tokens)? {
		stree.block.push(stmnt);
		tokens = leftovers;
	}
	if let Some((expr, leftovers)) = next_expression(tokens)? {
		if !leftovers.is_empty() {
			return Err(ParseError::UnexpectedToken.into())
		}
		stree.output = Some(expr);
	}

	Ok(Some(stree))
}

pub fn next_statement<'a>(tokens: &'a [Token]) -> Result<Option<(Statement, &'a [Token])>, Error<ParseError>> {
	if tokens.is_empty() {
		return Ok(None)
	}

	if let Some((binding, leftovers)) = next_binding(tokens)? {
		return Ok(Some((Statement::Binding(binding), leftovers)))
	} else if let Some((debug, leftovers)) = next_debug(tokens)? {
		return Ok(Some((Statement::Debug(debug), leftovers)))
	} else if let Some((expression, leftovers)) = next_expression(tokens)? {
		if leftovers.get(0) == Some(&Token::Symbol(TokenSymbol::Semicolon)) {
			return Ok(Some((Statement::Expression(expression), &leftovers[1..leftovers.len()])))
		} else {
			return Ok(None)
		}
	}

	Err(ParseError::Other.into())
}

pub fn next_block<'a>(tokens: &'a [Token]) -> Result<Option<(SyntaxTree, &'a [Token])>, Error<ParseError>> {
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
						return Err(ParseError::UnexpectedToken.into())
					};
					// And the leftover tokens [after last brace..end of tokens]
					return Ok(Some((ast, &tokens[i + 1..tokens.len()])));
				}
			}

			return Err(ParseError::UnclosedBrace.into())
		},
		_ => return Ok(None)
	}
}

#[allow(unused_mut)]
fn next_binding<'a>(tokens: &'a [Token]) -> Result<Option<(Binding, &'a [Token])>, Error<ParseError>> {
	if tokens.is_empty() {
		return Ok(None)
	}

	let mut tokens = tokens;

	match tokens[0] {
		Token::Keyword(Keyword::Let) => {
			let ident = if let Token::Name(ref name) = tokens[1] {
				name.to_owned()
			} else {
				return Ok(None) // TODO add expected 'identifier'
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
				return Ok(None)
			}

			let expr = if let Some((expr, leftovers)) = next_expression(&tokens[2 + offset + 1..tokens.len()])? {
				tokens = leftovers;
				expr
			} else {
				return Ok(None)
			};

			if let Some(&Token::Symbol(TokenSymbol::Semicolon)) = tokens.get(0) {
				tokens = &tokens[1..tokens.len()];
			} else {
				return Ok(None)
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

fn next_debug<'a>(tokens: &'a [Token]) -> Result<Option<(Expression, &'a [Token])>, Error<ParseError>> {
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
				return Ok(None)
			};

			
			if let Token::Symbol(TokenSymbol::Semicolon) = tokens[0] {
				tokens = &tokens[1..tokens.len()];
			} else {
				return Ok(None)
			}

			Ok(Some((expr, tokens)))
		},
		_ => Ok(None),
	}
}

fn next_expression<'a>(tokens: &'a [Token]) -> Result<Option<(Expression, &'a [Token])>, Error<ParseError>> {
	if tokens.is_empty() {
		return Ok(None)
	}

	let (expr_tokens, leftovers) = TerminatorCounter::split_until_end(tokens);

	expressions::parse_expression(expr_tokens).map(|expr| Some((expr, leftovers)))
}

mod expressions {
	use super::*;

	#[derive(Debug, Clone, PartialEq, Eq)]
	pub enum ExpressionItem {
		Operator(Operator),
		Operand(Expression),
	}

	pub trait ItemTaker: ::std::fmt::Debug {
		fn next_item<'a>(&self, tokens: &'a [Token]) -> Result<Option<(ExpressionItem, &'static [&'static ItemTaker], &'a [Token])>, Error<ParseError>>;
	}

	pub fn parse_expression(tokens: &[Token]) -> Result<Expression, Error<ParseError>> {
		let mut expr_items: Vec<ExpressionItem> = Vec::new();
		let mut item_takers: &[&ItemTaker] = &[&PrefixTaker, &OperandTaker];
		let mut tokens = tokens;

		// Separate operators from operands
		'outer: while !tokens.is_empty() {
			println!("\nStill have tokens {:?}", tokens);
			for item_taker in item_takers {
				if let Some((item, next_item_takers, leftover_tokens)) = item_taker.next_item(tokens)? {
					expr_items.push(item);
					item_takers = next_item_takers;
					tokens = leftover_tokens;
					continue 'outer;
				}
			}

			return Err(ParseError::Other.into())
		}

		// Jesus christ
		while expr_items.len() > 1 {
			let item1 = expr_items.remove(0);
			match item1 {
				ExpressionItem::Operand(item1expr) => {
					if !expr_items.is_empty() {
						let operator = expr_items.remove(0);
						match operator {
							ExpressionItem::Operator(operator) => match operator {
								Operator::Binary(binop) => {
									if !expr_items.is_empty() {
									let item2 = expr_items.remove(0);
										match item2 {
											ExpressionItem::Operand(item2expr) => {
												expr_items.insert(0, ExpressionItem::Operand(Expression::Binary {
													left: Box::new(item1expr),
													right: Box::new(item2expr),
													op: binop,
												}))
											},
											_ => return Err(ParseError::Other.into())
										}
									} else {
										return Err(ParseError::Other.into())
									}
								},
								_ => return Err(ParseError::Other.into())
							},
							_ => return Err(ParseError::Other.into())
						}
					} else {
						return Err(ParseError::Other.into())
					}
				},
				ExpressionItem::Operator(Operator::Prefix(operator)) => {
					unimplemented!()
				},
				_ => return Err(ParseError::Other.into())
			}
		}

		match expr_items.pop().unwrap() {
			ExpressionItem::Operand(expr) => {
				Ok(expr)
			},
			_ => Err(ParseError::Other.into())
		}
	}

	/// Tries to get a prefix operator
	#[derive(Debug)]
	struct PrefixTaker;
	impl ItemTaker for PrefixTaker {
		fn next_item<'a>(&self, tokens: &'a [Token]) -> Result<Option<(ExpressionItem, &'static [&'static ItemTaker], &'a [Token])>, Error<ParseError>> {
			if let Some(token) = tokens.get(0) {
				Ok(Some((ExpressionItem::Operator(Operator::Prefix(match token {
					Token::Symbol(symbol) => match symbol {
						TokenSymbol::Asterisk => PrefixOp::Asterisk,
						TokenSymbol::Ampersand => PrefixOp::Ampersand,
						_ => return Ok(None)
					},
					_ => return Ok(None)
				})), &[&PrefixTaker, &OperandTaker], &tokens[1..tokens.len()])))
			} else {
				Ok(None)
			}
		}
	}

	/// Tries to get a binary operator
	#[derive(Debug)]
	struct BinaryTaker;
	impl ItemTaker for BinaryTaker {
		fn next_item<'a>(&self, tokens: &'a [Token]) -> Result<Option<(ExpressionItem, &'static [&'static ItemTaker], &'a [Token])>, Error<ParseError>> {
			if let Some(token) = tokens.get(0) {
				Ok(Some((ExpressionItem::Operator(Operator::Binary(match token {
					Token::Symbol(symbol) => match symbol {
						TokenSymbol::Equals => BinaryOp::Equality,
						TokenSymbol::Plus => BinaryOp::Add,
						_ => return Ok(None)
					},
					_ => return Ok(None)
				})), &[&PrefixTaker, &OperandTaker], &tokens[1..tokens.len()])))
			} else {
				Ok(None)
			}
		}
	}

	/// Tries to get a binary operator
	#[derive(Debug)]
	struct OperandTaker;
	impl ItemTaker for OperandTaker {
		fn next_item<'a>(&self, tokens: &'a [Token]) -> Result<Option<(ExpressionItem, &'static [&'static ItemTaker], &'a [Token])>, Error<ParseError>> {
			let mut remaining = &tokens[1..tokens.len()];
			if let Some(token) = tokens.get(0) {
				Ok(Some((ExpressionItem::Operand(match token {
					Token::NumberLiteral(num) => Expression::NumberLiteral(*num),
					Token::Name(ref name) => Expression::Identifier(name.to_owned()),
					Token::Bracket(Bracket::Curly, BracketState::Open) => {
						// Try to parse the block as a syntax tree
						if let Some((block, leftovers)) = next_block(tokens)? {
							remaining = leftovers;
							Expression::Block(Box::new(block))
						} else {
							return Ok(None)
						}
					}
					_ => return Ok(None),
				}), &[&BinaryTaker], remaining)))
			} else {
				Ok(None)
			}
		}
	}
}
