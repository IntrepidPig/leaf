use ast::parser::*;

pub fn take_typedef(_in_tokens: &[TokenTree]) -> Result<Option<Type>, Error<ParseError>> {
	Ok(None)
}