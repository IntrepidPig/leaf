use std::path::Path;

pub mod lexer;
pub mod tokenizer;
pub mod parser;

use self::parser::SyntaxTree;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ast {
	pub tree: SyntaxTree,
}

#[derive(Debug, Clone)]
pub enum AstCreationError {
	LexError(lexer::LexError),
	TokenizeError(tokenizer::TokenizeError),
	ParseError(parser::ParseError),
}

impl From<lexer::LexError> for AstCreationError {
	fn from(t: lexer::LexError) -> Self {
		AstCreationError::LexError(t)
	}
}

impl From<tokenizer::TokenizeError> for AstCreationError {
	fn from(t: tokenizer::TokenizeError) -> Self {
		AstCreationError::TokenizeError(t)
	}
}

impl From<parser::ParseError> for AstCreationError {
	fn from(t: parser::ParseError) -> Self {
		AstCreationError::ParseError(t)
	}
}

pub fn create_ast<P: AsRef<Path>>(_path: P) -> Result<Ast, AstCreationError> {
	let _program_string = unimplemented!();
}
