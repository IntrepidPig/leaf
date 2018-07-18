use ast::parser::*;

// TODO don't return result
pub fn parse_separated<F: FnMut(&TokenTree) -> bool>(in_tokens: &[TokenTree], predicate: F) -> Result<Vec<&[TokenTree]>, Error<ParseError>> {
	Ok(in_tokens.split(predicate).collect())
}