use ast::parser::*;

/// Gets the next break statement
pub struct BreakTaker;

impl ExpressionTaker for BreakTaker {
	type Args = ();

	fn take_expression(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		unimplemented!()
	}
}
