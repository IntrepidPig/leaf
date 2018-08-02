use ast::parser::*;

#[derive(Default)]
pub struct InstantiationTaker;

impl InstantiationTaker {
	pub fn new() -> Self {
		InstantiationTaker
	}
}

impl ExpressionTaker for InstantiationTaker {
	type Args = ();

	fn next_expression<'a>(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		let typename = if let Some(typename) = next_pathitem(stream, next_typename)? {
			typename
		} else {
			return Ok(None)
		};
		
		let mut fields = Vec::new();
		if let Some(TokenTree::Block(Bracket::Curly, ref mut instantiation_token_stream, _, _)) = stream.opt_next_tokentree()? {
			let field_token_streams = separated::parse_separated(instantiation_token_stream, |t| t.is_comma())?;
			for mut field_token_stream in field_token_streams {
				let ident = if let Some(ident) = next_ident(&mut field_token_stream)? {
					ident
				} else {
					return Err(ParseError::expected(vec![Expected::Identifier], &field_token_stream).into());
				};
				
				if let TokenTree::Token(Token { kind: TokenKind::Symbol(Symbol::Colon), .. }) = field_token_stream.take_tokentree()? { } else {
					return Err(ParseError::expected(vec![Expected::Symbol(Symbol::Colon)], &field_token_stream).into())
				}
				
				let expr = operation::parse_expression(&mut field_token_stream)?;
				
				fields.push((ident, expr));
			}
		} else {
			return Ok(None);
		}
		
		Ok(Some(Expression::Instantiation(typename, fields)))
	}
}
