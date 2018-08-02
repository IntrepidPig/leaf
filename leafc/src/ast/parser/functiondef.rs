use ast::parser::*;

/// A function definition
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDefinition {
	pub name: Identifier,
	pub args: Vec<(Identifier, PathItem<TypeName>)>,
	pub return_type: Option<PathItem<TypeName>>,
	pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternFunction {
	pub name: Identifier,
	pub args: Vec<(Identifier, PathItem<TypeName>)>,
	pub return_type: Option<PathItem<TypeName>>,
}

pub fn next_functiondef(stream: &mut TokenStream) -> ParseResult<FunctionDefinition> {
	if let TokenTree::Token(Token { kind: TokenKind::Keyword(Keyword::Function), .. }) = stream.take_tokentree()? { } else {
		return Ok(None)
	}
	
	let name = if let TokenTree::Token(Token { kind: TokenKind::Name(ref name), .. }) = stream.take_tokentree()? {
		Identifier::try_from_str(name)
	} else {
		return Err(ParseError::expected(vec![Expected::Identifier], &*stream).into());
	};
	
	let args: Vec<(Identifier, PathItem<TypeName>)> = if let TokenTree::Block(Bracket::Paren, ref mut args_token_stream, _, _) = stream.take_tokentree()? {
		let mut args = Vec::new();
		let args_token_streams = separated::parse_separated(args_token_stream, |t| t.is_comma())?;
		for mut args_token_stream in args_token_streams {
			let ident = if let TokenTree::Token(Token { kind: TokenKind::Name(ref name), .. }) = args_token_stream.take_tokentree()? {
				Identifier::try_from_str(name)
			} else {
				return Err(ParseError::expected(vec![Expected::Identifier], &args_token_stream).into());
			};
			
			let typename = if let Some(typename) = typeann::next_typeann(&mut args_token_stream)? {
				typename
			} else {
				return Err(ParseError::expected(vec![Expected::Colon], &args_token_stream).into());
			};
			
			args.push((ident, typename));
		}
		args
	} else {
		return Err(ParseError::expected(vec![Expected::Parentheses], &*stream).into())
	};
	
	let old_position = stream.get_position();
	let return_type: Option<PathItem<TypeName>> = if let Some(typeann) = typeann::next_typeann(stream)? {
		Some(typeann)
	} else {
		stream.seek(old_position);
		None
	};
	
	let body = if let TokenTree::Block(Bracket::Curly, ref mut body_token_stream, _, _) = stream.take_tokentree()? {
		parse_block(body_token_stream)?
	} else {
		return Err(ParseError::expected(vec![Expected::Block], &*stream).into());
	};
	
	Ok(Some(FunctionDefinition {
		name,
		args,
		return_type,
		body,
	}))
}

pub fn next_externfn(stream: &mut TokenStream) -> ParseResult<ExternFunction> {
	if let TokenTree::Token(Token { kind: TokenKind::Keyword(Keyword::Extern), .. }) = stream.take_tokentree()? { } else {
		return Ok(None)
	}
	
	if let TokenTree::Token(Token { kind: TokenKind::Keyword(Keyword::Function), .. }) = stream.take_tokentree()? { } else {
		return Ok(None)
	}
	
	let name = if let TokenTree::Token(Token { kind: TokenKind::Name(ref name), .. }) = stream.take_tokentree()? {
		Identifier::try_from_str(name)
	} else {
		return Err(ParseError::expected(vec![Expected::Identifier], &*stream).into());
	};
	
	let args: Vec<(Identifier, PathItem<TypeName>)> = if let TokenTree::Block(Bracket::Paren, ref mut args_token_stream, _, _) = stream.take_tokentree()? {
		Vec::new() // TODO!!
	} else {
		return Err(ParseError::expected(vec![Expected::Parentheses], &*stream).into())
	};
	
	let old_position = stream.get_position();
	let return_type: Option<PathItem<TypeName>> = if let Some(typeann) = typeann::next_typeann(stream)? {
		Some(typeann)
	} else {
		stream.seek(old_position);
		None
	};
	
	Ok(Some(ExternFunction {
		name,
		args,
		return_type,
	}))
}
