use ast::parser::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct If {
	pub condition: Expression,
	pub body: Expression,
	pub elif: Option<Box<If>>,
	pub else_block: Option<Expression>,
}

pub struct IfTaker;

impl ExpressionTaker for IfTaker {
	type Args = ();

	fn next_expression<'a>(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		unimplemented!()
	}
}
