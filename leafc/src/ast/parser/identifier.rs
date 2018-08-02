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

	fn next_expression<'a>(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		unimplemented!()
	}
}