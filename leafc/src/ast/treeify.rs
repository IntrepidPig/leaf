use std::fmt;

use ast::lexer::{Bracket, BracketState};
use ast::tokenizer::{Keyword, Symbol, Token as OldToken};
use failure::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
	Keyword(Keyword),
	Name(String),
	NumberLiteral(u64),
	StringLiteral(String),
	Symbol(Symbol),
}

impl Token {
	pub fn from_old_token(old: OldToken) -> Result<Token, Error<TreeifyError>> {
		Ok(match old {
			OldToken::Keyword(keyword) => Token::Keyword(keyword),
			OldToken::Name(name) => Token::Name(name),
			OldToken::NumberLiteral(num) => Token::NumberLiteral(num),
			OldToken::StringLiteral(string) => Token::StringLiteral(string),
			OldToken::Symbol(symbol) => Token::Symbol(symbol),
			OldToken::Bracket(_, _) => return Err(TreeifyError::IncorrectBrace.into()),
		})
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenTree {
	Token(Token),
	Paren(Vec<TokenTree>),
	Brace(Vec<TokenTree>),
	Bracket(Vec<TokenTree>),
}

impl TokenTree {
	pub fn is_semicolon(&self) -> bool {
		match self {
			TokenTree::Token(Token::Symbol(Symbol::Semicolon)) => true,
			_ => false,
		}
	}

	pub fn is_then(&self) -> bool {
		match self {
			TokenTree::Token(Token::Keyword(Keyword::Then)) => true,
			_ => false,
		}
	}

	pub fn is_else(&self) -> bool {
		match self {
			TokenTree::Token(Token::Keyword(Keyword::Else)) => true,
			_ => false,
		}
	}

	pub fn is_brace_expr(&self) -> bool {
		match self {
			TokenTree::Brace(_) => true,
			_ => false,
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

pub fn treeify(mut old_tokens: &[OldToken]) -> Result<Vec<TokenTree>, Error<TreeifyError>> {
	let mut tokens: Vec<TokenTree> = Vec::new();
	while !old_tokens.is_empty() {
		match &old_tokens[0] {
			OldToken::Bracket(bracket, state) => {
				if *state == BracketState::Close {
					return Err(TreeifyError::IncorrectBrace.into());
				}
				let (sub, leftovers) = get_sub(old_tokens, *bracket)?;
				match bracket {
					Bracket::Curly => tokens.push(TokenTree::Brace(treeify(sub)?)),
					Bracket::Paren => tokens.push(TokenTree::Paren(treeify(sub)?)),
					Bracket::Square => tokens.push(TokenTree::Bracket(treeify(sub)?)),
				}
				old_tokens = leftovers;
			},
			token => {
				tokens.push(TokenTree::Token(Token::from_old_token(token.clone())?));
				old_tokens = &old_tokens[1..];
			},
		}
	}

	Ok(tokens)
}

fn get_sub(
	old_tokens: &[OldToken],
	bracket: Bracket,
) -> Result<(&[OldToken], &[OldToken]), Error<TreeifyError>> {
	let mut count = 0;
	for (i, token) in old_tokens.iter().enumerate() {
		match token {
			OldToken::Bracket(test_bracket, bracket_state) => {
				if bracket == *test_bracket {
					match bracket_state {
						BracketState::Open => count += 1,
						BracketState::Close => count -= 1,
					}
				}
			},
			_ => {},
		}

		if count == 0 {
			return Ok((&old_tokens[1..i], &old_tokens[i + 1..]));
		}
	}

	Err(TreeifyError::UnclosedBrace.into())
}
