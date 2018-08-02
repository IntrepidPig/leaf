use ast::parser::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct If {
	pub condition: Expression,
	pub body: Expression,
	pub elif: Option<Box<If>>,
	pub else_block: Option<Expression>,
}

pub struct IfTaker;

impl ExpressionTaker for IfTaker {
	type Args = ();

	fn next_expression(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		if let TokenTree::Token(Token { kind: TokenKind::Keyword(Keyword::If), .. }) = stream.take_tokentree()? { } else {
			return Ok(None)
		}
		
		eprintln!("If tokens: {:?}", stream);
		let (mut condition_tokens, exhausted) = stream.split_when(|t| t.is_then(), true)?;
		eprintln!("Condition tokens: {:?}\nAfter condition tokens: {:?}\n", condition_tokens, stream);
		let condition = operation::parse_expression(&mut condition_tokens)?;
		
		let body = if let TokenTree::Block(Bracket::Curly, ref mut body_token_stream, _, _) = stream.take_tokentree()? {
			Expression::Block(Box::new(parse_block(body_token_stream)?))
		} else {
			return Err(ParseError::expected(vec![Expected::Block], &*stream).into());
		};
		
		let else_block = if let Some(TokenTree::Token(Token { kind: TokenKind::Keyword(Keyword::Else), .. })) = stream.opt_next_tokentree()? {
			if let TokenTree::Block(Bracket::Curly, ref mut else_token_stream, _, _) = stream.take_tokentree()? {
				Some(Expression::Block(Box::new(parse_block(else_token_stream)?)))
			} else {
				return Err(ParseError::expected(vec![Expected::Block], &*stream).into());
			}
		} else {
			None
		};
		
		Ok(Some(Expression::If(Box::new(If {
			condition,
			body,
			elif: None, // TODO
			else_block,
		}))))
	}
}
