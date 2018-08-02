use ast::parser::*;

// TODO don't return result
pub fn parse_separated<'a, F: FnMut(&TokenTree) -> bool>(
	stream: &mut TokenStream<'a>,
	mut predicate: F,
) -> Result<Vec<TokenStream<'a>>, Error<ParseError>> {
	let mut split: Vec<TokenStream> = Vec::new();
	let mut last_split = stream.duplicate();
	while let Some(token) = last_split.opt_next_tokentree()? {
		if predicate(&token) {
			let (first, mut second) = last_split.split_here(true);
			eprintln!("Splits:\nFirst: {:?}\n\nSecond: {:?}\n", first, second);
			split.push(first);
			last_split = second;			
		}
	}
	last_split.reset();
	split.push(last_split);
	
	if split.last().map(|last| last.is_empty()).unwrap_or(false) {
		split.pop();
	}
	
	eprintln!("\nSplit into {:?}\n", split);

	Ok(split)
}
