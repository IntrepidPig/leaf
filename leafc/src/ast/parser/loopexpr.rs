use ast::parser::*;

#[derive(Default)]
pub struct LoopTaker;

impl LoopTaker {
	pub fn new() -> Self {
		LoopTaker
	}
}

impl ExpressionTaker for LoopTaker {
	type Args = ();

	fn take_expression<'a>(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		unimplemented!()
	}
}
