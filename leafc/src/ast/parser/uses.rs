use ast::parser::*;

pub fn take_use(in_tokens: &[TokenTree]) -> Result<Option<(PathItem<TypeName>, &[TokenTree])>, Error<ParseError>> {
	let mut tokens = in_tokens;
	
	match tokens.get(0) {
		Some(TokenTree::Token(Token::Keyword(Keyword::Use))) => {
			tokens = &tokens[1..];
		},
		_ => return Ok(None),
	}
	
	let (use_path, leftovers) = if let Some(res) = pathitem::next_type(tokens)? {
		res
	} else {
		return Err(ParseError::Other.into());
	};
	
	tokens = leftovers;
	
	Ok(Some((use_path, tokens)))
}