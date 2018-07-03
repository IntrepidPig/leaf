/// An operator
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
	Binary(BinaryOp),
	Prefix(PrefixOp),
	Postfix(PostfixOp),
}

impl Operator {
	pub fn precedence(self) -> i32 {
		match self {
			Operator::Binary(binop) => binop.precedence(),
			Operator::Prefix(preop) => preop.precedence(),
			Operator::Postfix(postop) => postop.precedence(),
		}
	}

	pub fn left_associative(self) -> bool {
		match self {
			Operator::Binary(binop) => binop.left_associative(),
			_ => false,
		}
	}
}

/// A binary operator
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
	Assign,
	Add,
	Equality,
	Greater,
	GreaterEqual,
	Less,
	LessEqual,
	Dot,
	Namespace,
}

impl BinaryOp {
	pub fn precedence(self) -> i32 {
		match self {
			BinaryOp::Dot => 6,
			BinaryOp::Equality => 4,
			BinaryOp::Add => 5,
			BinaryOp::Assign => 3,
			_ => unimplemented!(),
		}
	}

	pub fn left_associative(self) -> bool {
		match self {
			BinaryOp::Assign => false,
			_ => true,
		}
	}
}

/// A prefix operator
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefixOp {
	Asterisk,
	Ampersand,
}

impl PrefixOp {
	pub fn precedence(self) -> i32 {
		match self {
			PrefixOp::Asterisk => 1,
			PrefixOp::Ampersand => 1,
		}
	}
}

/// A postfix operator
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PostfixOp {
	Question,
	Exclamation,
}

impl PostfixOp {
	pub fn precedence(self) -> i32 {
		match self {
			PostfixOp::Question => 2,
			PostfixOp::Exclamation => 2,
		}
	}
}