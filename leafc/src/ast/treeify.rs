use std::fmt;

use ast::lexer::{Bracket, BracketState, Location};
use ast::tokenizer::{Keyword, Symbol, Token as OldToken, TokenKind as OldTokenKind};
use failure::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
	pub kind: TokenKind,
	pub location: Location,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
	Keyword(Keyword),
	Name(String),
	NumberLiteral(u64),
	StringLiteral(String),
	Symbol(Symbol),
}

impl Token {
	pub fn from_old_token(old: OldToken) -> Result<Token, Error<TreeifyError>> {
		let kind = match old.kind {
			OldTokenKind::Keyword(keyword) => TokenKind::Keyword(keyword),
			OldTokenKind::Name(name) => TokenKind::Name(name),
			OldTokenKind::NumberLiteral(num) => TokenKind::NumberLiteral(num),
			OldTokenKind::StringLiteral(string) => TokenKind::StringLiteral(string),
			OldTokenKind::Symbol(symbol) => TokenKind::Symbol(symbol),
			OldTokenKind::Bracket(_, _) => return Err(TreeifyError::IncorrectBrace.into()),
		};
		Ok(Token {
			kind,
			location: old.location,
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenTree {
	Token(Token),
	Block(BlockType, Vec<TokenTree>, Location, Location),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlockType {
	Paren,
	Brace,
	Bracket,
	If,
	Loop,
}

impl TokenTree {
	pub fn is_semicolon(&self) -> bool {
		match self {
			TokenTree::Token(Token {
				kind: TokenKind::Symbol(Symbol::Semicolon),
				..
			}) => true,
			_ => false,
		}
	}

	pub fn is_then(&self) -> bool {
		match self {
			TokenTree::Token(Token {
				kind: TokenKind::Keyword(Keyword::Then),
				..
			}) => true,
			_ => false,
		}
	}

	pub fn is_else(&self) -> bool {
		match self {
			TokenTree::Token(Token {
				kind: TokenKind::Keyword(Keyword::Else),
				..
			}) => true,
			_ => false,
		}
	}

	pub fn is_end(&self) -> bool {
		match self {
			TokenTree::Token(Token {
				kind: TokenKind::Keyword(Keyword::End),
				..
			}) => true,
			_ => false,
		}
	}

	pub fn is_brace_expr(&self) -> bool {
		match self {
			TokenTree::Block(BlockType::Brace, _, _, _) => true,
			_ => false,
		}
	}

	pub fn is_comma(&self) -> bool {
		match self {
			TokenTree::Token(Token {
				kind: TokenKind::Symbol(Symbol::Comma),
				..
			}) => true,
			_ => false,
		}
	}

	pub fn get_location(&self) -> Location {
		match self {
			TokenTree::Token(token) => token.location,
			TokenTree::Block(_, _, location, _end_location) => *location,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TreeifyError {
	UnclosedBrace,
	IncorrectBrace,
	Other,
}

impl fmt::Display for TreeifyError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{:?}", self)
	}
}

impl ::std::error::Error for TreeifyError {}

pub fn treeify(in_tokens: &[OldToken]) -> Result<Vec<TokenTree>, Error<TreeifyError>> {
	let tokens = treeify_brackets(in_tokens)?;
	let tokens = treeify_loops(&tokens)?;
	let tokens = treeify_ifs(&tokens)?;

	Ok(tokens)
}

fn treeify_brackets(in_tokens: &[OldToken]) -> Result<Vec<TokenTree>, Error<TreeifyError>> {
	let mut tokens: Vec<TokenTree> = Vec::new();
	let mut old_tokens = in_tokens;
	while !old_tokens.is_empty() {
		match &old_tokens[0] {
			OldToken {
				kind: OldTokenKind::Bracket(bracket, state),
				location,
			} => {
				if *state == BracketState::Close {
					return Err(TreeifyError::IncorrectBrace.into());
				}
				let (sub, leftovers, end_location) = get_sub(old_tokens, |old_token| match old_token.kind {
					OldTokenKind::Bracket(test_bracket, test_state) => {
						if test_bracket == *bracket {
							match test_state {
								BracketState::Open => 1,
								BracketState::Close => -1,
							}
						} else {
							0
						}
					},
					_ => 0,
				})?;
				match bracket {
					Bracket::Curly => tokens.push(TokenTree::Block(
						BlockType::Brace,
						treeify_brackets(sub)?,
						*location,
						end_location,
					)),
					Bracket::Paren => tokens.push(TokenTree::Block(
						BlockType::Paren,
						treeify_brackets(sub)?,
						*location,
						end_location,
					)),
					Bracket::Square => tokens.push(TokenTree::Block(
						BlockType::Bracket,
						treeify_brackets(sub)?,
						*location,
						end_location,
					)),
				}

				if let Some(OldTokenKind::Bracket(test_bracket, test_state)) = leftovers.get(0).map(|t| &t.kind) {
					if !(test_bracket == bracket && test_state == &BracketState::Close) {
						return Err(TreeifyError::UnclosedBrace.into()); // There was no close bracket
					}
				}

				// cut off the close bracket
				old_tokens = &leftovers[1..];
			},
			_ => {
				tokens.push(TokenTree::Token(Token::from_old_token(
					old_tokens[0].clone(),
				)?));
				old_tokens = &old_tokens[1..];
			},
		}
	}

	Ok(tokens)
}

fn treeify_ifs(in_tokens: &[TokenTree]) -> Result<Vec<TokenTree>, Error<TreeifyError>> {
	let mut tokens: Vec<TokenTree> = Vec::new();
	let mut old_tokens = in_tokens;

	while !old_tokens.is_empty() {
		match &old_tokens[0] {
			TokenTree::Token(Token {
				kind: TokenKind::Keyword(Keyword::If),
				location,
			}) => {
				let (sub, leftovers, end_location) = get_sub(old_tokens, |token| match token {
					TokenTree::Token(Token {
						kind: TokenKind::Keyword(Keyword::If),
						..
					}) => 1,
					TokenTree::Token(Token {
						kind: TokenKind::Keyword(Keyword::End),
						..
					}) => -1,
					_ => 0,
				})?;

				tokens.push(TokenTree::Block(
					BlockType::If,
					treeify_ifs(sub)?,
					*location,
					end_location,
				));
				old_tokens = &leftovers[1..];
			},
			TokenTree::Block(block_type, sub_tokens, start_location, end_location) => {
				tokens.push(TokenTree::Block(
					*block_type,
					treeify_ifs(sub_tokens)?,
					*start_location,
					*end_location,
				));
				old_tokens = &old_tokens[1..];
			},
			token => {
				tokens.push(token.clone());
				old_tokens = &old_tokens[1..];
			},
		}
	}

	Ok(tokens)
}

fn treeify_loops(in_tokens: &[TokenTree]) -> Result<Vec<TokenTree>, Error<TreeifyError>> {
	let mut tokens: Vec<TokenTree> = Vec::new();
	let mut old_tokens = in_tokens;

	while !old_tokens.is_empty() {
		match &old_tokens[0] {
			TokenTree::Token(Token {
				kind: TokenKind::Keyword(Keyword::Loop),
				location,
			}) => {
				let (sub, leftovers, end_location) = get_sub(old_tokens, |token| match token {
					TokenTree::Token(Token {
						kind: TokenKind::Keyword(Keyword::Loop),
						..
					}) => 1,
					TokenTree::Token(Token {
						kind: TokenKind::Keyword(Keyword::Back),
						..
					}) => -1,
					_ => 0,
				})?;

				tokens.push(TokenTree::Block(
					BlockType::Loop,
					treeify_loops(sub)?,
					*location,
					end_location,
				));
				old_tokens = &leftovers[1..];
			},
			TokenTree::Block(block_type, sub_tokens, start_location, end_location) => {
				tokens.push(TokenTree::Block(
					*block_type,
					treeify_loops(sub_tokens)?,
					*start_location,
					*end_location,
				));
				old_tokens = &old_tokens[1..];
			},
			token => {
				tokens.push(token.clone());
				old_tokens = &old_tokens[1..];
			},
		}
	}

	Ok(tokens)
}

#[cfg_attr(feature = "cargo-clippy", allow(type_complexity))]
fn get_sub<T: Locate, F: FnMut(&T) -> i32>(
	in_tokens: &[T],
	mut predicate: F,
) -> Result<(&[T], &[T], Location), Error<TreeifyError>> {
	let mut count = 0;
	for (i, token) in in_tokens.iter().enumerate() {
		count += predicate(token);

		if count == 0 {
			return Ok((&in_tokens[1..i], &in_tokens[i..], token.get_location()));
		}
	}

	Err(TreeifyError::UnclosedBrace.into())
}

pub trait Locate {
	fn get_location(&self) -> Location;
}

impl Locate for OldToken {
	fn get_location(&self) -> Location {
		self.location
	}
}

impl Locate for TokenTree {
	fn get_location(&self) -> Location {
		self.get_location()
	}
}
