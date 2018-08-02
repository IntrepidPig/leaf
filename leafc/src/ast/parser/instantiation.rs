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

	fn take_expression<'a>(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		unimplemented!()
	}
}
