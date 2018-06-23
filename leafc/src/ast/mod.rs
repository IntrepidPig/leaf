use std::path::Path;

pub mod lexer;

pub struct Ast {

}

pub enum AstCreationError {
	LexError(lexer::LexError),
	TokenizeError,
}

impl From<lexer::LexError> for AstCreationError {
	fn from(t: lexer::LexError) -> Self {
		AstCreationError::LexError(t)
	}
}

pub fn create_ast<P: AsRef<Path>>(_path: P) -> Result<Ast, AstCreationError> {
	let _program_string = unimplemented!();
}