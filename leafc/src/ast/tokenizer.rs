use ast::lexer::{Lexeme, Lexemes, Bracket, BracketState};

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
	Symbol(Symbol)
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
	Ampersand,
	Question,
	Exclamation,
}

#[derive(Debug, Clone)]
pub struct Tokens {
	pub tokens: Vec<Token>,
}

#[derive(Debug, Clone)]
pub enum TokenizeError {
	Other,
}

impl<'a> Tokenizer<'a> {
	pub fn new(lexemes: Lexemes<'a>) -> Self {
		Tokenizer {
			lexemes,
		}
	}

	pub fn tokenize(&mut self) -> Result<Tokens, TokenizeError> {
		let mut tokens: Vec<Token> = Vec::new();

		while !self.lexemes.lexemes.is_empty() {
			let lexeme = self.lexemes.lexemes.remove(0);

			match lexeme {
				Lexeme::Word(word) => {
					match word {
						"fn" => tokens.push(Token::Keyword(Keyword::Function)),
						"return" => tokens.push(Token::Keyword(Keyword::Return)),
						"let" => tokens.push(Token::Keyword(Keyword::Let)),
						"while" => tokens.push(Token::Keyword(Keyword::While)),
						"if" => tokens.push(Token::Keyword(Keyword::If)),
						"else" => tokens.push(Token::Keyword(Keyword::Else)),
						word => tokens.push(Token::Name(word.to_owned()))
					}
				},
				Lexeme::Bracket(bracket, state) => {
					tokens.push(Token::Bracket(bracket, state));
				},
				Lexeme::String(string) => {
					tokens.push(Token::StringLiteral(string));
				},
				Lexeme::Number(num_string) => {
					match num_string.parse() {
						Ok(n) => tokens.push(Token::NumberLiteral(n)),
						Err(_e) => return Err(TokenizeError::Other),
					}
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
				Lexeme::Ampersand => {
					tokens.push(Token::Symbol(Symbol::Ampersand));
				},
				Lexeme::Question => {
					tokens.push(Token::Symbol(Symbol::Question));
				},
				Lexeme::Exclamation => {
					tokens.push(Token::Symbol(Symbol::Exclamation));
				},
				Lexeme::Whitespace { .. } => {} // whitespace isn't syntax python
			}
		}

		Ok(Tokens {
			tokens
		})
	}
}