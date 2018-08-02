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
		Vec::new() // TODO!!
	} else {
		return Err(ParseError::expected(vec![Expected::Parentheses], &*stream).into())
	};
	
	let return_type: Option<PathItem<TypeName>> = {
		if let &[Token { kind: TokenKind::Symbol(Symbol::Colon), .. }] = stream.peek_tokens(1) {
			stream.take_tokentree()?;
			unimplemented!()
		} else {
			None
		}
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

pub fn next_externfn<'a>(stream: &mut TokenStream<'a>) -> ParseResult<ExternFunction> {
	unimplemented!()
}
