use ast::parser::*;

/// Gets the next break statement
pub struct FunctionCallTaker;

impl ExpressionTaker for FunctionCallTaker {
	type Args = ();

	fn take_expression<'a>(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		unimplemented!()
	}
}

fn parse_args(stream: &mut TokenStream) -> Result<Vec<Expression>, Error<ParseError>> {
	unimplemented!()
}
