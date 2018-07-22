use std::path::Path;

pub mod lexer;
pub mod tokenizer;
pub mod parser;
pub mod treeify;

use std::fs::File;
use std::io::Read;
use failure;

use self::parser::{SyntaxTree, Module};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ast {
	pub tree: SyntaxTree,
}

#[derive(Debug, Clone)]
pub enum AstCreationError {
	LexError(lexer::LexError),
	TokenizeError(tokenizer::TokenizeError),
	TreeifyError(treeify::TreeifyError),
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

impl From<treeify::TreeifyError> for AstCreationError {
	fn from(t: treeify::TreeifyError) -> Self {
		AstCreationError::TreeifyError(t)
	}
}

impl<T> From<failure::Error<T>> for AstCreationError where T: Into<AstCreationError> + ::std::error::Error {
	fn from(t: failure::Error<T>) -> Self {
		t.error.into()
	}
}

pub fn create_ast(input: &str) -> Result<SyntaxTree, AstCreationError> {
	let lexemes = lexer::lex(&input)?;
	let mut tokenizer = tokenizer::Tokenizer::new(lexemes);
	let tokens = tokenizer.tokenize()?;
	let tokentree = treeify::treeify(&tokens.tokens)?;
	let st = parser::parse(&tokentree)?;
	Ok(st)
}

pub fn create_ast_with_includes(input: &str, includes: &[(String, &Path)]) -> Result<SyntaxTree, AstCreationError> {
	let mut st = create_ast(input)?;
	for include in includes {
		let include_st = create_ast_from_file(&include.1, &[])?; // TODO support includes with includes? maybe should only be solved by libraries
		st.modules.push(Module::new(include.0.clone(), include_st));
	}
	
	Ok(st)
}

pub fn create_ast_from_file<P: AsRef<Path>>(path: P, includes: &[(String, &Path)]) -> Result<SyntaxTree, AstCreationError> {
	let mut main_file = File::open(&path).expect("Failed to open input file");
	let input = {
		let mut buf = String::new();
		main_file.read_to_string(&mut buf).unwrap();
		buf
	};
	create_ast_with_includes(&input, includes)
}
