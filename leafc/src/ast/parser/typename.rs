use ast::parser::*;

/*pub fn next_typename<'a>(
	in_tokens: &'a [TokenTree],
) -> Result<Option<(PathIdentifier, &'a [TokenTree])>, Error<ParseError>> {
	let mut relative = true;
	let pathitems: Vec<Identifier> = Vec::new();
	
	let mut tokens = in_tokens;
	
	match tokens.get(0) {
		Some(TokenTree::Token(Token::Symbol(TokenSymbol::Namespace))) => {
			tokens = &tokens[1..];
			relative = false;
		},
		_ => {},
	}
	
	let ident = loop {
		let ident = match tokens.get(0) {
			Some(TokenTree::Token(Token::Name(ref name))) => {
				Identifier::from_string(name.clone())
			},
			_ => return Ok(None),
		};
		
		match tokens.get(1) {
			Some(TokenTree::Token(Token::Symbol(TokenSymbol::Namespace))) => {
				pathitems.push(ident);
			},
			_ => {
				tokens = &tokens[1..];
				break ident;
			}
		}
		
		tokens = &tokens[2..];
	};
	Ok(Some((PathIdentifier {
		relative,
		module_path: ModulePath::new(pathitems),
		ident,
	}, &tokens[1..])))
}*/