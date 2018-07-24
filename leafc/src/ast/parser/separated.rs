use ast::parser::*;

// TODO don't return result
pub fn parse_separated<F: FnMut(&TokenTree) -> bool>(
	in_tokens: &[TokenTree],
	predicate: F,
) -> Result<Vec<&[TokenTree]>, Error<ParseError>> {
	let mut split: Vec<&[TokenTree]> = in_tokens.split(predicate).collect();
	if split.last().map(|last| last.is_empty()).unwrap_or(false) {
		split.pop();
	}

	Ok(split)
}
