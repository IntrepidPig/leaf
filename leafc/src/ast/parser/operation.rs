use ast::parser::*;
use self::parse::*;

/// Gets the entirety of the next expression available as a single expression. This taker only stops upon recieving a token
/// that matches the predicate given
#[derive(Default)]
pub struct OperationTaker;

impl OperationTaker {
	pub fn new() -> Self {
		OperationTaker
	}
}

impl ExpressionTaker for OperationTaker {
	// The end of the expression predicate
	type Args = Box<FnMut(&TokenTree) -> bool>;

	fn take_expression<'a>(&self, tokens: &'a [TokenTree], args: Self::Args) -> ParseResult<'a, Expression> {
		if tokens.is_empty() {
			return Ok(None);
		}

		// Get the tokens involved in the next expression
		let (expr_tokens, leftovers) = split_at(tokens, args, false);

		parse_operation(expr_tokens).map(|expr| Some((expr, leftovers)))
	}
}

pub fn split_at(
	tokens: &[TokenTree],
	mut condition: Box<FnMut(&TokenTree) -> bool>,
	exclusive: bool,
) -> (&[TokenTree], &[TokenTree]) {
	for (i, token) in tokens.iter().enumerate() {
		if condition(token) {
			return (&tokens[0..i], &tokens[i + exclusive as usize..]);
		}
	}

	(&tokens[..], &tokens[tokens.len()..])
}

mod parse {
	use ast::parser::*;

	type ItemTakerResult<'a> = Result<
		Option<
			(
				ExpressionItem,
				&'static [&'static ItemTaker],
				&'a [TokenTree],
			),
		>,
		Error<ParseError>,
	>;

	#[derive(Debug, Clone, PartialEq, Eq)]
	pub struct ExpressionItem {
		kind: ExpressionItemKind,
		span: Span,
	}

	#[derive(Debug, Clone, PartialEq, Eq)]
	pub enum ExpressionItemKind {
		Operator(Operator),
		Operand(Expression),
	}

	/// Trait for a struct that takes an ExpressionItem
	pub trait ItemTaker: ::std::fmt::Debug {
		fn next_item<'a>(&self, tokens: &'a [TokenTree]) -> ItemTakerResult<'a>;
	}

	/// Parse the entirety of the tokens passed as an expression. If not all the tokens are used up, then there was an error.
	pub fn parse_operation(tokens: &[TokenTree]) -> Result<Expression, Error<ParseError>> {
		let mut expr_items: Vec<ExpressionItem> = Vec::new();
		let mut item_takers: &[&ItemTaker] = &[&PrefixTaker, &OperandTaker];
		let mut tokens = tokens;

		// Separate operators from operands and put them in expr_items
		'outer: while !tokens.is_empty() {
			for item_taker in item_takers {
				if let Some((item, next_item_takers, leftover_tokens)) = item_taker.next_item(tokens)? {
					expr_items.push(item);
					item_takers = next_item_takers;
					tokens = leftover_tokens;
					continue 'outer;
				}
			}

			return Err(ParseError {
				kind: ParseErrorKind::UnexpectedToken,
				span: tokens[0].get_span(),
			}.into());
		}

		// Simplifies the list of expression items into a single expression. Works recursively, step by step, with a lot of
		// heap allocation.
		// TODO disallow chained binary operators, require parentheses for explicit order of operations
		fn simplify(items: &[ExpressionItem]) -> Result<Expression, Error<ParseError>> {
			if items.is_empty() {
				// A previous iteration returned something bad
				panic!("An operand was empty")
			}

			// If there's only one item left it should be an expression and not an operator
			if items.len() == 1 {
				match items[0] {
					ExpressionItem {
						kind: ExpressionItemKind::Operand(ref expr),
						..
					} => return Ok(expr.clone()),
					// The expression parsed into an operator, hopefully this is actually unreachable
					ExpressionItem {
						kind: ExpressionItemKind::Operator(_),
						..
					} => unreachable!(),
				}
			}

			// Get the highest (lowest precedence) priority operator in this list.
			// Priority is the lowest precedence so for
			let mut priority = 100;
			// Priority op is the index of the lowest precedence so far
			let mut priority_op: usize = 0;

			for (i, item) in items.iter().enumerate() {
				if let ExpressionItem {
					kind: ExpressionItemKind::Operator(op),
					..
				} = item
				{
					if (op.precedence() < priority) || (op.left_associative() && op.precedence() == priority) {
						priority = op.precedence();
						priority_op = i;
					}
				}
			}

			// Recursively call simplify on each side of an operator that will have an expression
			match items[priority_op] {
				ExpressionItem {
					kind: ExpressionItemKind::Operator(op),
					span,
				} => {
					match op {
						Operator::Binary(binop) => {
							// Simplify the tokens to the left and to the right of the binary operation
							let lhs = &items[..priority_op];
							let rhs = &items[priority_op + 1..];
							let lhs = simplify(lhs)?;
							let rhs = simplify(rhs)?;
							match binop {
								// Assign is a special case that returns a special expression, not a binary operation
								// and requires the lhs to be an identifier
								BinaryOp::Assign => {
									match lhs {
										Expression::Identifier(ref ident) => {
											Ok(Expression::Assign(Box::new(Assignment {
												ident: ident.to_owned(),
												expr: rhs,
											})))
										},
										_ => Err(ParseError {
											kind: ParseErrorKind::Expected(vec![Expected::Identifier]),
											span,
										}.into()), // The left hand side wasn't an identifier
										           // TODO get location of expression
									}
								},
								BinaryOp::Dot => {
									match rhs {
										Expression::Identifier(ref ident) => {
											Ok(Expression::FieldAccess(Box::new(lhs), ident.clone()))
										},
										_ => Err(ParseError {
											kind: ParseErrorKind::Expected(vec![Expected::Identifier]),
											span,
										}.into()), // Right hand side wasn't an identifier
									}
								},
								binop => Ok(Expression::Binary {
									left: Box::new(lhs),
									right: Box::new(rhs),
									op: binop,
								}),
							}
						},
						Operator::Prefix(preop) => {
							// Simplify the items to the right of a prefix operator
							let rhs = &items[priority_op + 1..];
							let rhs = simplify(rhs)?;
							Ok(Expression::Prefix {
								right: Box::new(rhs),
								op: preop,
							})
						},
						Operator::Postfix(postop) => {
							// Simplify the items to the left of a postfix operator
							let lhs = &items[..priority_op];
							let lhs = simplify(lhs)?;
							Ok(Expression::Postfix {
								left: Box::new(lhs),
								op: postop,
							})
						},
					}
				},
				ExpressionItem {
					kind: ExpressionItemKind::Operand(_),
					span,
				} => {
					Err(ParseError {
						kind: ParseErrorKind::Expected(vec![Expected::Operator]),
						span,
					}.into()) // The token at this index should be an operator
				},
			}
		}

		let expr = simplify(&expr_items)?;

		if !tokens.is_empty() {
			return Err(ParseError {
				kind: ParseErrorKind::UnexpectedToken,
				span: tokens[0].get_span(),
			}.into()); // Not all the tokens were used up
		}

		Ok(expr)
	}

	/// Tries to get a prefix operator
	#[derive(Debug)]
	struct PrefixTaker;
	impl ItemTaker for PrefixTaker {
		fn next_item<'a>(&self, tokens: &'a [TokenTree]) -> ItemTakerResult<'a> {
			if let Some(token) = tokens.get(0) {
				Ok(Some((
					ExpressionItem {
						kind: ExpressionItemKind::Operator(Operator::Prefix(match token {
							TokenTree::Token(Token {
								kind: TokenKind::Symbol(symbol),
								..
							}) => match symbol {
								TokenSymbol::Asterisk => PrefixOp::Asterisk,
								TokenSymbol::Ampersand => PrefixOp::Ampersand,
								_ => return Ok(None),
							},
							_ => return Ok(None),
						})),
						span: token.get_span(),
					},
					&[&PrefixTaker, &OperandTaker],
					&tokens[1..],
				)))
			} else {
				Ok(None)
			}
		}
	}

	/// Tries to get a binary operator (eg '+', '='?)
	#[derive(Debug)]
	struct BinaryTaker;
	impl ItemTaker for BinaryTaker {
		fn next_item<'a>(&self, tokens: &'a [TokenTree]) -> ItemTakerResult<'a> {
			if let Some(token) = tokens.get(0) {
				Ok(Some((
					ExpressionItem {
						kind: ExpressionItemKind::Operator(Operator::Binary(match token {
							TokenTree::Token(Token {
								kind: TokenKind::Symbol(symbol),
								..
							}) => match symbol {
								TokenSymbol::Plus => BinaryOp::Add,
								TokenSymbol::Minus => BinaryOp::Sub,
								TokenSymbol::Asterisk => BinaryOp::Mul,
								TokenSymbol::Slash => BinaryOp::Div,
								TokenSymbol::Assign => BinaryOp::Assign,
								TokenSymbol::Equality => BinaryOp::Equality,
								TokenSymbol::Dot => BinaryOp::Dot,
								_ => return Ok(None),
							},
							_ => return Ok(None),
						})),
						span: token.get_span(),
					},
					&[&PrefixTaker, &OperandTaker],
					&tokens[1..],
				)))
			} else {
				Ok(None)
			}
		}
	}

	/// Tries to get an operand
	/// The operand can be either a literal, an identifier, or a block (which will be classified as a single operand)
	#[derive(Debug)]
	struct OperandTaker;
	impl ItemTaker for OperandTaker {
		fn next_item<'a>(&self, in_tokens: &'a [TokenTree]) -> ItemTakerResult<'a> {
			let expression_takers: &[&ExpressionTaker<Args = ()>] = &[
				&binding::BindingTaker,
				&debug::DebugTaker,
				&loopexpr::LoopTaker,
				&ifexpr::IfTaker,
				&breakexpr::BreakTaker,
				&literal::LiteralTaker,
				&functioncall::FunctionCallTaker,   // has to be before identifier
				&instantiation::InstantiationTaker, // has to be before block and identifier
				&identifier::IdentifierTaker,
				&block::BlockTaker,
			];

			for expression_taker in expression_takers {
				let expr_opt = expression_taker.take_expression(in_tokens, ())?;
				if let Some((expr, leftovers)) = expr_opt {
					return Ok(Some((
						ExpressionItem {
							kind: ExpressionItemKind::Operand(expr),
							span: in_tokens[0].get_span(),
						},
						&[&BinaryTaker],
						leftovers,
					)));
				}
			}

			Err(ParseError {
				kind: ParseErrorKind::UnexpectedToken,
				span: in_tokens[0].get_span(),
			}.into()) // Failed to parse operand
		}
	}
}
