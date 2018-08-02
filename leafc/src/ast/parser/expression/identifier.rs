use ast::parser::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
	pub name: String,
}

impl Identifier {
	pub fn from_string(string: String) -> Self {
		// TODO validate identifier string and return result
		Identifier { name: string }
	}

	pub fn try_from_str(string: &str) -> Self {
		// TODO validate identifier string
		Identifier {
			name: string.to_owned(),
		}
	}
}

#[derive(Default)]
pub struct IdentifierTaker;

impl IdentifierTaker {
	pub fn new() -> Self {
		IdentifierTaker
	}
}

impl ExpressionTaker for IdentifierTaker {
	type Args = ();

	fn next_expression(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		if let Some(ident) = next_ident(stream)? {
			Ok(Some(Expression::Identifier(ident)))
		} else {
			Ok(None)
		}
	}
}

pub fn next_ident(stream: &mut TokenStream) -> ParseResult<Identifier> {
	let ident = if let TokenTree::Token(Token { kind: TokenKind::Name(ref name), .. }) = stream.take_tokentree()? {
		Identifier::try_from_str(name)
	} else {
		return Ok(None)
	};
	
	Ok(Some(ident))
}