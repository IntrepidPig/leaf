use ast::parser::*;

/// Gets the next debug statement
// TODO include expression original string repr with debug
pub struct DebugTaker;

impl ExpressionTaker for DebugTaker {
	type Args = ();

	fn take_expression(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		unimplemented!()
	}
}
