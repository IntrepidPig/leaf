use std::path::Path;

pub mod lexer;
pub mod tokenizer;
pub mod parser;
pub mod treeify;

use std::fs::File;
use std::io::{self, Read};
use std::fmt;
use failure;

use self::parser::{Identifier, Module, SyntaxTree};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ast {
	pub tree: SyntaxTree,
}

#[derive(Debug)]
pub enum AstCreationError {
	LexError(lexer::LexError),
	TokenizeError(tokenizer::TokenizeError),
	TreeifyError(treeify::TreeifyError),
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

impl From<treeify::TreeifyError> for AstCreationError {
	fn from(t: treeify::TreeifyError) -> Self {
		AstCreationError::TreeifyError(t)
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
				AstCreationError::TreeifyError(ref err) => err.to_string(),
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

pub fn create_ast(input: &str) -> Result<SyntaxTree, AstCreationError> {
	info!("Text input:\n{}\n\t", input);
	let lexemes = lexer::lex(&input)?;
	info!("\n{:?}\n\t", lexemes);
	let mut tokenizer = tokenizer::Tokenizer::new(lexemes);
	let tokens = tokenizer.tokenize()?;
	info!("\n{:?}\n\t", tokens);
	let tokentree = treeify::treeify(&tokens.tokens)?;
	info!("\n{:?}\n\t", tokentree);
	let st = parser::parse(&tokentree)?;
	info!("\n{:?}", st);
	Ok(st)
}

pub fn create_ast_with_includes(input: &str, includes: &[(String, &Path)]) -> Result<SyntaxTree, AstCreationError> {
	let mut modules = Vec::new();
	for include in includes {
		let include_st = create_ast_from_file(&include.1, &[])?; // TODO support includes with includes? maybe should only be solved by libraries
		modules.push((
			Identifier::from_string(include.0.clone()),
			Module::new(include_st),
		));
	}
	let mut st = create_ast(input)?;
	st.modules.append(&mut modules);

	Ok(st)
}

pub fn create_ast_from_file<P: AsRef<Path>>(
	path: P,
	includes: &[(String, &Path)],
) -> Result<SyntaxTree, AstCreationError> {
	let mut main_file = File::open(&path).expect("Failed to open input file");
	let input = {
		let mut buf = String::new();
		main_file.read_to_string(&mut buf)?;
		buf
	};
	create_ast_with_includes(&input, includes)
}
