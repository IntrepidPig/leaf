use ast::parser::*;

#[derive(Default)]
pub struct LiteralTaker;

impl LiteralTaker {
	pub fn new() -> Self {
		LiteralTaker
	}
}

impl ExpressionTaker for LiteralTaker {
	type Args = ();

	fn take_expression(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		unimplemented!()
	}
}
