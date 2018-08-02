use ast::parser::*;

/// A let binding. Contains the identifier being bound to, the
// type of the binding, the expression being bound, and whether is mutable
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding {
	pub mutable: bool,
	pub ident: Identifier,
	pub bind_type: Option<PathItem<TypeName>>,
	pub val: Option<Expression>,
}

/// Gets the next let binding
pub struct BindingTaker;

impl ExpressionTaker for BindingTaker {
	type Args = ();

	fn next_expression(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		if let TokenTree::Token(Token {
			kind: TokenKind::Keyword(Keyword::Let),
			..
		}) = stream.take_tokentree()?
		{
		} else {
			return Ok(None);
		}

		let old_position = stream.get_position();
		let mutable = {
			if let TokenTree::Token(Token {
				kind: TokenKind::Keyword(Keyword::Mutable),
				..
			}) = stream.take_tokentree()?
			{
				true
			} else {
				stream.seek(old_position);
				false
			}
		};

		let ident = if let TokenTree::Token(Token {
			kind: TokenKind::Name(ref name),
			..
		}) = stream.take_tokentree()?
		{
			Identifier::try_from_str(name)
		} else {
			return Err(ParseError::expected(vec![Expected::Identifier], &*stream).into());
		};

		let old_position = stream.get_position();
		let bind_type: Option<PathItem<TypeName>> = if let Some(typeann) = typeann::next_typeann(stream)? {
			Some(typeann)
		} else {
			stream.seek(old_position);
			None
		};

		/*if let TokenTree::Token(Token { kind: TokenKind::Symbol(Symbol::Assign), .. }) = stream.take_tokentree()? { } else {
			return Err(ParseError::expected(vec![Expected::Symbol(Symbol::Assign)], &*stream).into());
		}
		
		let mut expr_token_stream = TokenStream::new_with(&*stream);
		let val = Some(parse_expression(&mut expr_token_stream)?); // TODO make optional
		stream.merge(&expr_token_stream);*/

		Ok(Some(Expression::Binding(Box::new(Binding {
			mutable,
			ident,
			bind_type,
			val: None,
		}))))
	}
}
