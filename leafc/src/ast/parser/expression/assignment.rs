use ast::parser::*;

/// An assignment
/// Contains the left hand side
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assignment {
	pub ident: Identifier,
	pub expr: Expression,
}