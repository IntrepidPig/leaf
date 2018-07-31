use ast::parser::*;

/// Gets the next let binding
pub struct BindingTaker;

impl ExpressionTaker for BindingTaker {
	type Args = ();

	fn take_expression<'a>(&self, in_tokens: &'a [TokenTree], _args: Self::Args) -> ParseResult<'a, Expression> {
		if in_tokens.is_empty() {
			return Ok(None);
		}

		let mut tokens = in_tokens;

		let last_span = match tokens.get(0) {
			Some(TokenTree::Token(Token {
				kind: TokenKind::Keyword(Keyword::Let),
				span,
			})) => {
				tokens = &tokens[1..];
				*span
			},
			_ => return Ok(None),
		};

		let (mutable, last_span) = match tokens.get(0) {
			Some(TokenTree::Token(Token {
				kind: TokenKind::Keyword(Keyword::Mutable),
				span,
			})) => {
				tokens = &tokens[1..];
				(true, *span)
			},
			_ => (false, last_span),
		};

		let (ident, last_span) = match tokens.get(0) {
			Some(TokenTree::Token(Token {
				kind: TokenKind::Name(ref name),
				span,
			})) => (Identifier::try_from_str(name), *span),
			Some(t) => {
				return Err(ParseError {
					kind: ParseErrorKind::Expected(vec![Expected::Identifier]),
					span: t.get_span(),
				}.into())
			},
			None => {
				return Err(ParseError {
					kind: ParseErrorKind::Expected(vec![Expected::Identifier]),
					span: last_span,
				}.into())
			},
		};
		tokens = &tokens[1..];

		let (bindtype, last_span) = match tokens.get(0) {
			Some(TokenTree::Token(Token {
				kind: TokenKind::Symbol(TokenSymbol::Colon),
				span,
			})) => {
				tokens = &tokens[1..];
				let (typename, leftovers) = if let Some(res) = next_type(tokens)? {
					res
				} else {
					return Err(ParseError {
						kind: ParseErrorKind::Expected(vec![Expected::Typename]),
						span: *span,
					}.into());
				};
				tokens = leftovers;
				(Some(typename), *span)
			},
			_ => (None, last_span),
		};

		let last_span = match tokens.get(0) {
			Some(TokenTree::Token(Token {
				kind: TokenKind::Symbol(TokenSymbol::Assign),
				span,
			})) => {
				tokens = &tokens[1..];
				*span
			},
			// expected assign symbol in let binding
			t => {
				return Err(ParseError {
					kind: ParseErrorKind::Expected(vec![Expected::Symbol(TokenSymbol::Assign)]),
					span: t.map(|t| t.get_span()).unwrap_or(last_span),
				}.into())
			},
		};

		let (bindexpr, leftovers) = if let Some(res) = next_expression(tokens, Box::new(|token| token.is_semicolon()))?
		{
			res
		} else {
			// expected an expression after the assign symbol
			return Err(ParseError {
				kind: ParseErrorKind::Expected(vec![Expected::Expression]),
				span: last_span,
			}.into());
		};

		tokens = leftovers;

		Ok(Some((
			Expression::Binding(Box::new(Binding {
				mutable,
				ident,
				bind_type: bindtype,
				val: Some(bindexpr),
			})),
			tokens,
		)))
	}
}
