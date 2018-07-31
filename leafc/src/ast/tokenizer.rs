use std::fmt;

use ast::lexer::{Bracket, BracketState, LexemeKind, Lexemes, Location};

#[derive(Debug, Clone)]
pub struct Tokenizer<'a> {
	lexemes: Lexemes<'a>,
}

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
	Bracket(Bracket, BracketState),
	Symbol(Symbol),
}

impl TokenKind {
	pub fn as_bracket(&self) -> Option<(Bracket, BracketState)> {
		match *self {
			TokenKind::Bracket(bracket, state) => Some((bracket, state)),
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

			let kind = match lexeme.kind {
				LexemeKind::Word(word) => match word {
					"fn" => TokenKind::Keyword(Keyword::Function),
					"return" => TokenKind::Keyword(Keyword::Return),
					"let" => TokenKind::Keyword(Keyword::Let),
					"while" => TokenKind::Keyword(Keyword::While),
					"if" => TokenKind::Keyword(Keyword::If),
					"else" => TokenKind::Keyword(Keyword::Else),
					"debug" => TokenKind::Keyword(Keyword::Debug),
					"loop" => TokenKind::Keyword(Keyword::Loop),
					"break" => TokenKind::Keyword(Keyword::Break),
					"then" => TokenKind::Keyword(Keyword::Then),
					"end" => TokenKind::Keyword(Keyword::End),
					"back" => TokenKind::Keyword(Keyword::Back),
					"true" => TokenKind::Keyword(Keyword::True),
					"false" => TokenKind::Keyword(Keyword::False),
					"type" => TokenKind::Keyword(Keyword::Type),
					"mod" => TokenKind::Keyword(Keyword::Module),
					"use" => TokenKind::Keyword(Keyword::Use),
					"mut" => TokenKind::Keyword(Keyword::Mutable),
					"extern" => TokenKind::Keyword(Keyword::Extern),
					word => TokenKind::Name(word.to_owned()),
				},
				LexemeKind::Bracket(bracket, state) => TokenKind::Bracket(bracket, state),
				LexemeKind::String(string) => TokenKind::StringLiteral(string),
				LexemeKind::Number(num_string) => match num_string.parse() {
					Ok(n) => TokenKind::NumberLiteral(n),
					Err(_e) => return Err(TokenizeError::Other),
				},
				LexemeKind::Colon => TokenKind::Symbol(Symbol::Colon),
				LexemeKind::Comma => TokenKind::Symbol(Symbol::Comma),
				LexemeKind::Dot => TokenKind::Symbol(Symbol::Dot),
				LexemeKind::Equals => TokenKind::Symbol(Symbol::Equals),
				LexemeKind::Semicolon => TokenKind::Symbol(Symbol::Semicolon),
				LexemeKind::Asterisk => TokenKind::Symbol(Symbol::Asterisk),
				LexemeKind::Greater => TokenKind::Symbol(Symbol::Greater),
				LexemeKind::Less => TokenKind::Symbol(Symbol::Less),
				LexemeKind::Equality => TokenKind::Symbol(Symbol::Equality),
				LexemeKind::Assign => TokenKind::Symbol(Symbol::Assign),
				LexemeKind::Ampersand => TokenKind::Symbol(Symbol::Ampersand),
				LexemeKind::Question => TokenKind::Symbol(Symbol::Question),
				LexemeKind::Exclamation => TokenKind::Symbol(Symbol::Exclamation),
				LexemeKind::Plus => TokenKind::Symbol(Symbol::Plus),
				LexemeKind::Minus => TokenKind::Symbol(Symbol::Minus),
				LexemeKind::Slash => TokenKind::Symbol(Symbol::Slash),
				LexemeKind::Namespace => TokenKind::Symbol(Symbol::Namespace),
				LexemeKind::Whitespace { .. } => continue, // whitespace isn't syntax python
			};
			tokens.push(Token {
				kind,
				location: lexeme.location,
			});
		}

		Ok(Tokens { tokens })
	}
}
