use std::mem;

use ast::lexer::{Bracket, BracketState};
use ast::tokenizer::{Token, Keyword, Symbol as TokenSymbol, Tokens};
use ast::Ast;

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
pub enum SyntaxTree {
	Block(Block),
	Statement(Statement),
}

impl SyntaxTree {
	pub fn push(&mut self, syntax: SyntaxTree) -> Result<(), ParseError> {
		match *self {
			SyntaxTree::Block(ref mut block) => {
				block.block.push(syntax);
			},
			SyntaxTree::Statement(ref mut stmnt) => {
				return Err(ParseError::Other);
			}
		}

		Ok(())
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
	block: Vec<SyntaxTree>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
	Binding(Binding),
	Expression(Expression),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
	name: String,
	args: Vec<Binding>,
	return_type: String,
	body: Block,
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
	Block(Block),
	StringLiteral(String),
	NumberLiteral(u64),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
	Add,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding {
	mutable: bool,
	ident: String,
	bind_type: Option<String>,
	val: Option<Expression>,
}

impl SyntaxTree {
	pub fn new() -> Self {
		SyntaxTree::Block(Block {
			block: Vec::new(),
		})
	}
}

#[derive(Debug, Clone)]
pub enum ParseError {
	UnexpectedToken(usize),
	UnclosedBrace(usize),
	Other,
}

pub fn parse(full_tokens: &[Token]) -> Result<SyntaxTree, ParseError> {
	let mut stree = SyntaxTree::new();

	let mut tokens = full_tokens;

	let mut syntax_parsers: &[&SyntaxParser] = &[&BlockTaker, &StatementTaker];

	'outer: while !tokens.is_empty() {
		println!("Tokens: {:?}", tokens);
		for syntax_parser in syntax_parsers {
			if let Some((syntax, leftovers)) = syntax_parser.next_element(tokens)? {
				stree.push(syntax)?;
				tokens = leftovers;
				continue 'outer;
			}
		}

		println!("Now tokens: {:?}", tokens);

		return Err(ParseError::UnexpectedToken(full_tokens.len() - tokens.len()))
	}

	Ok(stree)
}

trait SyntaxParser {
	fn next_element<'a>(&self, tokens: &'a [Token]) -> Result<Option<(SyntaxTree, &'a [Token])>, ParseError> {
		Ok(None)
	}
}

#[derive(Debug)]
struct StatementTaker;
impl SyntaxParser for StatementTaker {
	fn next_element<'a>(&self, tokens: &'a [Token]) -> Result<Option<(SyntaxTree, &'a [Token])>, ParseError> {
		let statement_parsers: &[&StatementParser] = &[&BindingTaker];

		for statement_parser in statement_parsers {
			if let Some((statement, leftovers)) = statement_parser.next_element(tokens)? {
				return Ok(Some((SyntaxTree::Statement(statement), leftovers)))
			}
		}

		Ok(None)
	}
}

#[derive(Debug)]
struct BlockTaker;
impl SyntaxParser for BlockTaker {
	fn next_element<'a>(&self, tokens: &'a [Token]) -> Result<Option<(SyntaxTree, &'a [Token])>, ParseError> {
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
						let ast = parse(&tokens[1..i])?;
						// I have no idea why this works
						return Ok(Some((ast, &tokens[i + 1..tokens.len()])));
					}
				}

				return Err(ParseError::UnclosedBrace(0))
			},
			_ => return Ok(None)
		}
	}
}

trait StatementParser {
	fn next_element<'a>(&self, tokens: &'a [Token]) -> Result<Option<(Statement, &'a [Token])>, ParseError> {
		unimplemented!();
	}
}

#[derive(Debug)]
struct BindingTaker;
impl StatementParser for BindingTaker {
	fn next_element<'a>(&self, tokens: &'a [Token]) -> Result<Option<(Statement, &'a [Token])>, ParseError> {
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

				let val = if let Some((expr, leftovers)) = ExpressionTaker.next_element(&tokens[2 + offset + 1..tokens.len()])? {
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

				Ok(Some((Statement::Binding(Binding {
					mutable: false,
					ident,
					bind_type: type_annotation,
					val: Some(val),
				}), tokens)))
			},
			_ => Ok(None),
		}
	}
}

trait ExpressionParser {
	fn next_element<'a>(&self, tokens: &'a [Token]) -> Result<Option<(Expression, &'a [Token])>, ParseError> {
		unimplemented!();
	}
}

#[derive(Debug)]
struct ExpressionTaker;
impl ExpressionParser for ExpressionTaker {
	fn next_element<'a>(&self, tokens: &'a [Token]) -> Result<Option<(Expression, &'a [Token])>, ParseError> {
		if tokens.is_empty() {
			return Ok(None)
		}

		match tokens[0] {
			Token::NumberLiteral(num) => {
				Ok(Some((Expression::NumberLiteral(num), &tokens[1..tokens.len()])))
			},
			_ => Ok(None)
		}
	}
}