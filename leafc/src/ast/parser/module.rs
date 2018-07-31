use ast::parser::*;

pub fn take_module(in_tokens: &[TokenTree]) -> ParseResult<(Identifier, Module)> {
	let mut tokens = in_tokens;

	let last_span = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Keyword(Keyword::Module),
			span,
		})) => {
			tokens = &tokens[1..];
			*span
		},
		_ => return Ok(None),
	};

	let (name, last_span) = match tokens.get(0) {
		Some(TokenTree::Token(Token {
			kind: TokenKind::Name(ref name),
			span,
		})) => {
			tokens = &tokens[1..];
			(Identifier::try_from_str(name), *span)
		},
		t => {
			return Err(ParseError {
				kind: ParseErrorKind::Expected(vec![Expected::Identifier]),
				span: t.map(|t| t.get_span()).unwrap_or(last_span),
			}.into())
		}, // Module was not given a name
	};

	let body = match tokens.get(0) {
		Some(TokenTree::Block(BlockType::Brace, ref mod_tokens, _start_location, _end_location)) => parse(mod_tokens)?,
		t => {
			return Err(ParseError {
				kind: ParseErrorKind::Expected(vec![Expected::Block]),
				span: t.map(|t| t.get_span()).unwrap_or(last_span),
			}.into())
		}, // needed a a block of code after module
	};

	tokens = &tokens[1..];

	Ok(Some(((name, Module { body }), tokens)))
}
