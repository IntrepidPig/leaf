use ast::parser::*;

pub mod block;
pub mod identifier;
pub mod ifexpr;
pub mod binding;
pub mod assignment;
pub mod literal;
pub mod functioncall;
pub mod debug;
pub mod loopexpr;
pub mod breakexpr;
pub mod instantiation;
pub mod operation;
pub use self::block::*;
pub use self::identifier::*;
pub use self::ifexpr::*;
pub use self::binding::*;
pub use self::assignment::*;
pub use self::literal::*;
pub use self::functioncall::*;
pub use self::debug::*;
pub use self::loopexpr::*;
pub use self::breakexpr::*;
pub use self::instantiation::*;

/// An expression
/// Can be a literal, an operations, or a block that contains more expressions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
	Root,
	Binary {
		left: Box<Expression>,
		right: Box<Expression>,
		op: BinaryOp,
	},
	Prefix {
		right: Box<Expression>,
		op: PrefixOp,
	},
	Postfix {
		left: Box<Expression>,
		op: PostfixOp,
	},
	FunctionCall {
		name: PathItem<Identifier>,
		args: Vec<Expression>,
	},
	Debug(Box<Expression>),
	Break(Option<Box<Expression>>),
	Binding(Box<Binding>),
	Assign(Box<Assignment>),
	Loop(Box<Expression>),
	If(Box<If>),
	Identifier(Identifier),
	Block(Box<Block>),
	StringLiteral(String),
	NumberLiteral(u64),
	BoolLiteral(bool),
	FieldAccess(Box<Expression>, Identifier),
	Instantiation(PathItem<TypeName>, Vec<(Identifier, Expression)>),
}

impl Expression {
	pub fn traverse_expressions_mut<F: FnMut(&mut Expression)>(&mut self, f: &mut F) {
		f(self);
		match self {
			Expression::Root => {},
			Expression::Binary { left, right, .. } => {
				left.traverse_expressions_mut(f);
				right.traverse_expressions_mut(f);
			},
			Expression::Prefix { right, .. } => {
				right.traverse_expressions_mut(f);
			},
			Expression::Postfix { left, .. } => {
				left.traverse_expressions_mut(f);
			},
			Expression::FunctionCall { args, .. } => for arg in args {
				arg.traverse_expressions_mut(f);
			},
			Expression::Debug(ref mut expr) => {
				expr.traverse_expressions_mut(f);
			},
			Expression::Break(ref mut expr) => {
				if let Some(ref mut expr) = expr {
					expr.traverse_expressions_mut(f)
				};
			},
			Expression::Binding(ref mut binding) => {
				if let Some(ref mut val) = binding.val {
					val.traverse_expressions_mut(f)
				};
			},
			Expression::Assign(ref mut assignment) => {
				assignment.expr.traverse_expressions_mut(f);
			},
			Expression::Loop(ref mut expr) => {
				expr.traverse_expressions_mut(f);
			},
			Expression::If(ref mut ifexpr) => {
				ifexpr.condition.traverse_expressions_mut(f);
				ifexpr.body.traverse_expressions_mut(f);
				// TODO elifs
				if let Some(ref mut expr) = ifexpr.else_block {
					expr.traverse_expressions_mut(f)
				}
			},
			Expression::Identifier(_) => {},
			Expression::Block(ref mut block) => {
				block.traverse_expressions_mut(f);
			},
			Expression::StringLiteral(_) => {},
			Expression::NumberLiteral(_) => {},
			Expression::BoolLiteral(_) => {},
			Expression::FieldAccess(ref mut lhs, _) => {
				lhs.traverse_expressions_mut(f);
			},
			Expression::Instantiation(_, ref mut fields) => for field in fields {
				field.1.traverse_expressions_mut(f);
			},
		}
	}
}

/// Trait that takes an expression from a stream of tokens. If it was successfull, then it returns an option with
/// the expression and the tokens that are still left over. If the expression wasn't there, then it returns None.
/// It can also return an error if there was an unrecoverable error.
pub trait ExpressionTaker {
	type Args;

	fn next_expression(&self, stream: &mut TokenStream, args: Self::Args) -> ParseResult<Expression>;
}

pub fn parse_operand(stream: &mut TokenStream) -> Result<Expression, Error<ParseError>> {
	let expression_takers: &[&ExpressionTaker<Args = ()>] = &[
		&BindingTaker,
		&DebugTaker,
		&LoopTaker,
		&IfTaker,
		&BreakTaker,
		&LiteralTaker,
		&FunctionCallTaker,  // has to be before identifier
		&InstantiationTaker, // has to be before block and identifier
		&IdentifierTaker,
		&BlockTaker,
	];

	let mut expr_opt: Option<Expression> = None;
	let mut old_position;
	for expression_taker in expression_takers {
		old_position = stream.get_position();
		let expr = expression_taker.next_expression(stream, ())?;
		if let Some(expr) = expr {
			expr_opt = Some(expr);
			break;
		} else {
			stream.seek(old_position);
		}
	}

	if !stream.is_empty() || expr_opt.is_none() {
		return Err(ParseError::unexpected(stream).into());
	}

	Ok(expr_opt.unwrap())
}
