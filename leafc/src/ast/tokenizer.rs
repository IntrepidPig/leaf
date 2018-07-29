use std::fmt;

use ast::lexer::{Bracket, BracketState, Lexeme, Lexemes};

#[derive(Debug, Clone)]
pub struct Tokenizer<'a> {
	lexemes: Lexemes<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
	Keyword(Keyword),
	Name(String),
	NumberLiteral(u64),
	StringLiteral(String),
	Bracket(Bracket, BracketState),
	Symbol(Symbol),
}

impl Token {
	pub fn as_bracket(&self) -> Option<(Bracket, BracketState)> {
		match *self {
			Token::Bracket(bracket, state) => Some((bracket, state)),
			_ => None,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
	Function,
	Return,
	Let,
	While,
	If,
	Else,
	Debug,
	Loop,
	Break,
	Then,
	End,
	Back,
	True,
	False,
	Type,
	Module,
	Use,
	Mutable,
	Extern,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Symbol {
	Colon,
	Comma,
	Dot,
	Equals,
	Semicolon,
	Asterisk,
	Greater,
	Less,
	Assign,
	Equality,
	Ampersand,
	Question,
	Exclamation,
	Plus,
	Minus,
	Slash,
	Namespace,
}

impl ::std::fmt::Display for Symbol {
	fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
		write!(
			f,
			"{}",
			match self {
				Symbol::Colon => "colon (:)",
				Symbol::Comma => "comma (,)",
				Symbol::Dot => "dot (.)",
				Symbol::Equals => "equals (=)",
				Symbol::Semicolon => "semicolon (;)",
				Symbol::Asterisk => "asterisk (*)",
				Symbol::Greater => "greater (>)",
				Symbol::Less => "less (<)",
				Symbol::Assign => "assign (:=)",
				Symbol::Equality => "equality (==)",
				Symbol::Ampersand => "ampersand (&)",
				Symbol::Question => "question (?)",
				Symbol::Exclamation => "exclamation (!)",
				Symbol::Plus => "plus (+)",
				Symbol::Minus => "minus (-)",
				Symbol::Slash => "slash (/)",
				Symbol::Namespace => "namespace (::)",
			}
		)
	}
}

impl ::std::fmt::Display for Keyword {
	fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
		write!(
			f,
			"{}",
			match self {
				Keyword::Function => "fn",
				Keyword::Return => "return",
				Keyword::Let => "let",
				Keyword::While => "while",
				Keyword::If => "if",
				Keyword::Else => "else",
				Keyword::Debug => "debug",
				Keyword::Loop => "loop",
				Keyword::Break => "break",
				Keyword::Then => "then",
				Keyword::End => "end",
				Keyword::Back => "back",
				Keyword::True => "true",
				Keyword::False => "false",
				Keyword::Type => "type",
				Keyword::Module => "mod",
				Keyword::Use => "use",
				Keyword::Mutable => "mut",
				Keyword::Extern => "extern",
			}
		)
	}
}

#[derive(Debug, Clone)]
pub struct Tokens {
	pub tokens: Vec<Token>,
}

#[derive(Debug, Clone)]
pub enum TokenizeError {
	Other,
}

impl fmt::Display for TokenizeError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(
			f,
			"{}",
			match self {
				TokenizeError::Other => "An error occurred while tokenizing".to_owned(),
			}
		)
	}
}

impl ::std::error::Error for TokenizeError {}

impl<'a> Tokenizer<'a> {
	pub fn new(lexemes: Lexemes<'a>) -> Self {
		Tokenizer { lexemes }
	}

	pub fn tokenize(&mut self) -> Result<Tokens, TokenizeError> {
		let mut tokens: Vec<Token> = Vec::new();

		while !self.lexemes.lexemes.is_empty() {
			let lexeme = self.lexemes.lexemes.remove(0);

			match lexeme {
				Lexeme::Word(word) => match word {
					"fn" => tokens.push(Token::Keyword(Keyword::Function)),
					"return" => tokens.push(Token::Keyword(Keyword::Return)),
					"let" => tokens.push(Token::Keyword(Keyword::Let)),
					"while" => tokens.push(Token::Keyword(Keyword::While)),
					"if" => tokens.push(Token::Keyword(Keyword::If)),
					"else" => tokens.push(Token::Keyword(Keyword::Else)),
					"debug" => tokens.push(Token::Keyword(Keyword::Debug)),
					"loop" => tokens.push(Token::Keyword(Keyword::Loop)),
					"break" => tokens.push(Token::Keyword(Keyword::Break)),
					"then" => tokens.push(Token::Keyword(Keyword::Then)),
					"end" => tokens.push(Token::Keyword(Keyword::End)),
					"back" => tokens.push(Token::Keyword(Keyword::Back)),
					"true" => tokens.push(Token::Keyword(Keyword::True)),
					"false" => tokens.push(Token::Keyword(Keyword::False)),
					"type" => tokens.push(Token::Keyword(Keyword::Type)),
					"mod" => tokens.push(Token::Keyword(Keyword::Module)),
					"use" => tokens.push(Token::Keyword(Keyword::Use)),
					"mut" => tokens.push(Token::Keyword(Keyword::Mutable)),
					"extern" => tokens.push(Token::Keyword(Keyword::Extern)),
					word => tokens.push(Token::Name(word.to_owned())),
				},
				Lexeme::Bracket(bracket, state) => {
					tokens.push(Token::Bracket(bracket, state));
				},
				Lexeme::String(string) => {
					tokens.push(Token::StringLiteral(string));
				},
				Lexeme::Number(num_string) => match num_string.parse() {
					Ok(n) => tokens.push(Token::NumberLiteral(n)),
					Err(_e) => return Err(TokenizeError::Other),
				},
				Lexeme::Colon => {
					tokens.push(Token::Symbol(Symbol::Colon));
				},
				Lexeme::Comma => {
					tokens.push(Token::Symbol(Symbol::Comma));
				},
				Lexeme::Dot => {
					tokens.push(Token::Symbol(Symbol::Dot));
				},
				Lexeme::Equals => {
					tokens.push(Token::Symbol(Symbol::Equals));
				},
				Lexeme::Semicolon => {
					tokens.push(Token::Symbol(Symbol::Semicolon));
				},
				Lexeme::Asterisk => {
					tokens.push(Token::Symbol(Symbol::Asterisk));
				},
				Lexeme::Greater => {
					tokens.push(Token::Symbol(Symbol::Greater));
				},
				Lexeme::Less => {
					tokens.push(Token::Symbol(Symbol::Less));
				},
				Lexeme::Equality => {
					tokens.push(Token::Symbol(Symbol::Equality));
				},
				Lexeme::Assign => {
					tokens.push(Token::Symbol(Symbol::Assign));
				},
				Lexeme::Ampersand => {
					tokens.push(Token::Symbol(Symbol::Ampersand));
				},
				Lexeme::Question => {
					tokens.push(Token::Symbol(Symbol::Question));
				},
				Lexeme::Exclamation => {
					tokens.push(Token::Symbol(Symbol::Exclamation));
				},
				Lexeme::Plus => {
					tokens.push(Token::Symbol(Symbol::Plus));
				},
				Lexeme::Minus => {
					tokens.push(Token::Symbol(Symbol::Minus));
				},
				Lexeme::Slash => {
					tokens.push(Token::Symbol(Symbol::Slash));
				},
				Lexeme::Namespace => {
					tokens.push(Token::Symbol(Symbol::Namespace));
				},
				Lexeme::Whitespace { .. } => {}, // whitespace isn't syntax python
			}
		}

		Ok(Tokens { tokens })
	}
}
