use std::path::Path;

pub mod lexer;
pub mod tokenizer;
pub mod parser;
//pub mod treeify;
pub mod stream;

use std::fs::File;
use std::io::{self, Read};
use std::fmt;
use failure;

use self::parser::{Identifier, Module, TokenStream};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ast {
	pub tree: Module,
}

#[derive(Debug)]
pub enum AstCreationError {
	LexError(lexer::LexError),
	TokenizeError(tokenizer::TokenizeError),
	ParseError(parser::ParseError),
	IoError(io::Error),
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

impl From<io::Error> for AstCreationError {
	fn from(t: io::Error) -> Self {
		AstCreationError::IoError(t)
	}
}

impl fmt::Display for AstCreationError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(
			f,
			"{}",
			match self {
				AstCreationError::LexError(ref err) => err.to_string(),
				AstCreationError::TokenizeError(ref err) => err.to_string(),
				AstCreationError::ParseError(ref err) => err.to_string(),
				AstCreationError::IoError(ref err) => err.to_string(),
			}
		)
	}
}

impl ::std::error::Error for AstCreationError {}

impl<T> From<failure::Error<T>> for AstCreationError
where
	T: Into<AstCreationError> + ::std::error::Error,
{
	fn from(t: failure::Error<T>) -> Self {
		t.error.into()
	}
}

pub fn create_ast(input: &str) -> Result<Module, failure::Error<AstCreationError>> {
	info!("Text input:\n{}\n\t", input);
	let lexemes = lexer::lex(&input).map_err(|e| failure::transfer_bt(e.into()))?;
	info!("\n{:?}\n\t", lexemes);
	let mut tokenizer = tokenizer::Tokenizer::new(lexemes);
	let tokens = tokenizer.tokenize().map_err(|e| failure::transfer_bt(e.into()))?;
	info!("\n{:?}\n\t", tokens);
	//let tokentree = treeify::treeify(&tokens.tokens)?;
	//info!("\n{:?}\n\t", tokentree);
	let st = parser::parse(&mut TokenStream::new(tokens.tokens.as_slice())).map_err(|e| failure::transfer_bt(e))?;
	info!("\n{:#?}", st);
	Ok(st)
}

pub fn create_ast_with_includes(input: &str, includes: &[(String, &Path)]) -> Result<Module, failure::Error<AstCreationError>> {
	let mut modules = Vec::new();
	for include in includes {
		let include_st = create_ast_from_file(&include.1, &[])?; // TODO support includes with includes? maybe should only be solved by libraries
		modules.push((
			Identifier::from_string(include.0.clone()),
			include_st,
		));
	}
	let mut st = create_ast(input)?;
	st.modules.append(&mut modules);

	Ok(st)
}

pub fn create_ast_from_file<P: AsRef<Path>>(
	path: P,
	includes: &[(String, &Path)],
) -> Result<Module, failure::Error<AstCreationError>> {
	let mut main_file = File::open(&path).expect("Failed to open input file");
	let input = {
		let mut buf = String::new();
		main_file.read_to_string(&mut buf).map_err(|e| AstCreationError::IoError(e))?;
		buf
	};
	create_ast_with_includes(&input, includes)
}
