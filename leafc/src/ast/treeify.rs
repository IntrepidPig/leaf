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
	If(Vec<TokenTree>),
	Loop(Vec<TokenTree>),
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
	
	pub fn is_end(&self) -> bool {
		match self {
			TokenTree::Token(Token::Keyword(Keyword::End)) => true,
			_ => false,
		}
	}
	
	pub fn is_brace_expr(&self) -> bool {
		match self {
			TokenTree::Brace(_) => true,
			_ => false,
		}
	}
	
	pub fn is_comma(&self) -> bool {
		match self {
			TokenTree::Token(Token::Symbol(Symbol::Comma)) => true,
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
			OldToken::Bracket(bracket, state) => {
				if *state == BracketState::Close {
					return Err(TreeifyError::IncorrectBrace.into());
				}
				let (sub, leftovers) = get_sub(old_tokens, |old_token| {
					match old_token {
						OldToken::Bracket(test_bracket, test_state) => {
							if test_bracket == bracket {
								match test_state {
									BracketState::Open => 1,
									BracketState::Close => -1,
								}
							} else {
								0
							}
						},
						_ => 0
					}
				})?;
				match bracket {
					Bracket::Curly => tokens.push(TokenTree::Brace(treeify_brackets(sub)?)),
					Bracket::Paren => tokens.push(TokenTree::Paren(treeify_brackets(sub)?)),
					Bracket::Square => tokens.push(TokenTree::Bracket(treeify_brackets(sub)?)),
				}

				match leftovers.get(0) {
					Some(OldToken::Bracket(test_bracket, test_state)) => {
						if !(test_bracket == bracket && *test_state == BracketState::Close) {
							return Err(TreeifyError::UnclosedBrace.into()) // There was no close bracket
						}
					},
					_ => {}
				}

				// cut off the close bracket
				old_tokens = &leftovers[1..];
			},
			token => {
				tokens.push(TokenTree::Token(Token::from_old_token(token.clone())?));
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
			TokenTree::Token(Token::Keyword(Keyword::If)) => {
				let (sub, leftovers) = get_sub(old_tokens, |token| match token {
					TokenTree::Token(Token::Keyword(Keyword::If)) => 1,
					TokenTree::Token(Token::Keyword(Keyword::End)) => -1,
					_ => 0,
				})?;
				
				tokens.push(TokenTree::If(treeify_ifs(sub)?));
				old_tokens = &leftovers[1..];
			},
			TokenTree::Brace(sub_tokens) => {
				tokens.push(TokenTree::Brace(treeify_ifs(sub_tokens)?));
				old_tokens = &old_tokens[1..];
			},
			TokenTree::Paren(sub_tokens) => {
				tokens.push(TokenTree::Paren(treeify_ifs(sub_tokens)?));
				old_tokens = &old_tokens[1..];
			},
			TokenTree::Bracket(sub_tokens) => {
				tokens.push(TokenTree::Bracket(treeify_ifs(sub_tokens)?));
				old_tokens = &old_tokens[1..];
			},
			TokenTree::Loop(sub_tokens) => {
				tokens.push(TokenTree::Loop(treeify_ifs(sub_tokens)?));
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
			TokenTree::Token(Token::Keyword(Keyword::Loop)) => {
				let (sub, leftovers) = get_sub(old_tokens, |token| match token {
					TokenTree::Token(Token::Keyword(Keyword::Loop)) => 1,
					TokenTree::Token(Token::Keyword(Keyword::Back)) => -1,
					_ => 0,
				})?;
				
				tokens.push(TokenTree::Loop(treeify_loops(sub)?));
				old_tokens = &leftovers[1..];
			},
			TokenTree::Brace(sub_tokens) => {
				tokens.push(TokenTree::Brace(treeify_loops(sub_tokens)?));
				old_tokens = &old_tokens[1..];
			},
			TokenTree::Paren(sub_tokens) => {
				tokens.push(TokenTree::Paren(treeify_loops(sub_tokens)?));
				old_tokens = &old_tokens[1..];
			},
			TokenTree::Bracket(sub_tokens) => {
				tokens.push(TokenTree::Bracket(treeify_loops(sub_tokens)?));
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

fn get_sub<T, F: FnMut(&T) -> i32>(
	in_tokens: &[T],
	mut predicate: F,
) -> Result<(&[T], &[T]), Error<TreeifyError>> {
	let mut count = 0;
	for (i, token) in in_tokens.iter().enumerate() {
		count += predicate(token);

		if count == 0 {
			return Ok((&in_tokens[1..i], &in_tokens[i..]));
		}
	}

	Err(TreeifyError::UnclosedBrace.into())
}
