use ast::parser::*;

pub fn parse_separated<'a, F: FnMut(&TokenTree) -> bool>(
	stream: &mut TokenStream<'a>,
	mut predicate: F,
) -> Result<Vec<TokenStream<'a>>, Error<ParseError>> {
	let mut split: Vec<TokenStream> = Vec::new();
	let mut last_split = stream.duplicate();
	let mut last_position = last_split.get_position();
	while let Some(token) = last_split.opt_next_tokentree()? {
		if predicate(&token) {
			last_split.seek(last_position);
			let (first, mut second) = last_split.split_here(true)?;
			split.push(first);
			last_split = second;
		}
		last_position = last_split.get_position();
	}
	last_split.reset();
	split.push(last_split);

	if split.last().map(|last| last.is_empty()).unwrap_or(false) {
		split.pop();
	}

	Ok(split)
}
