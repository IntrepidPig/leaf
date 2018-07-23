use ast::parser::*;

pub fn next_type<'a>(
	in_tokens: &'a [TokenTree],
) -> Result<Option<(PathItem<TypeName>, &'a [TokenTree])>, Error<ParseError>> {
	let mut tokens = in_tokens;
	
	let (modpath, leftovers) = if let Some(res) = next_path(tokens)? {
		res
	} else {
		return Ok(None)
	};
	tokens = leftovers;
	
	let typename = match leftovers.get(0) {
		Some(TokenTree::Token(Token::Name(ref name))) => {
			tokens = &tokens[1..];
			TypeName::from_ident(Identifier::from_str(name))
		},
		_ => return Ok(None)
	};
	
	Ok(Some((PathItem {
		module_path: modpath,
		item: typename,
	}, &tokens)))
}

pub fn next_path<'a>(
	in_tokens: &'a [TokenTree]
) -> Result<Option<(ModulePath, &'a [TokenTree])>, Error<ParseError>> {
	let mut relative = true;
	let mut pathitems: Vec<Identifier> = Vec::new();
	
	let mut tokens = in_tokens;
	
	match tokens.get(0) {
		Some(TokenTree::Token(Token::Symbol(TokenSymbol::Namespace))) => {
			relative = false;
			tokens = &tokens[1..];
		},
		Some(_) => {},
		None => return Ok(None),
	}
	
	loop {
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
				break;
			}
		}
		
		tokens = &tokens[2..];
	};
	
	Ok(Some((ModulePath::new(relative, pathitems), tokens)))
}