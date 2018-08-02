use ast::parser::*;

/// A let binding. Contains the identifier being bound to, the
// type of the binding, the expression being bound, and whether is mutable
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding {
	pub mutable: bool,
	pub ident: Identifier,
	pub bind_type: Option<PathItem<Identifier>>, // TODO! change to typename
	pub val: Option<Expression>,
}

/// Gets the next let binding
pub struct BindingTaker;

impl ExpressionTaker for BindingTaker {
	type Args = ();

	fn next_expression(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		unimplemented!()
	}
}
