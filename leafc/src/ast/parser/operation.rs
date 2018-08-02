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
	type Args = ();

	fn take_expression(&self, stream: &mut TokenStream, _args: Self::Args) -> ParseResult<Expression> {
		Ok(Some(parse_operation(stream)?))
	}
}

pub fn split_at<'a>(
	stream: &mut TokenStream<'a>,
	mut condition: Box<FnMut(TokenTree) -> bool>,
	exclusive: bool,
) -> Result<(TokenStream<'a>, TokenStream<'a>), Error<ParseError>> {
	while let Some(tokentree) = stream.opt_next_tokentree()? {
		if condition(tokentree) {
			break;
		}
	}

	Ok(stream.split_here(exclusive))
}

mod parse {
	use ast::parser::*;

	type ItemTakerResult<'a> = Result<
		Option<
			(
				ExpressionItem,
				&'static [&'static ItemTaker],
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
		fn next_item<'a>(&self, stream: &mut TokenStream) -> ItemTakerResult<'a>;
	}

	/// Parse the entirety of the tokens passed as an expression. If not all the tokens are used up, then there was an error.
	pub fn parse_operation(stream: &mut TokenStream) -> Result<Expression, Error<ParseError>> {
		let mut expr_items: Vec<ExpressionItem> = Vec::new();
		let mut item_takers: &[&ItemTaker] = &[&PrefixTaker, &OperandTaker];
		
		eprintln!("Parsing operation from {:?}", stream);

		// Separate operators from operands and put them in expr_items
		'outer: while !stream.is_empty() {
			for item_taker in item_takers {
				if let Some((item, next_item_takers)) = item_taker.next_item(stream)? {
					stream.commit();
					expr_items.push(item);
					item_takers = next_item_takers;
					continue 'outer;
				} else {
					stream.reset();
				}
			}

			return Err(ParseError {
				kind: ParseErrorKind::UnexpectedToken(stream.next_token().unwrap().clone()),
				span: stream.current_span,
			}.into());
		}
		
		eprintln!("Expr items: {:?}", expr_items);

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

		Ok(expr)
	}

	/// Tries to get a prefix operator
	#[derive(Debug)]
	struct PrefixTaker;
	impl ItemTaker for PrefixTaker {
		fn next_item<'a>(&self, stream: &mut TokenStream) -> ItemTakerResult<'a> {
			let item = 
				ExpressionItem {
					kind: ExpressionItemKind::Operator(Operator::Prefix(match stream.next_tokentree()? {
						TokenTree::Token(Token {
							kind: TokenKind::Symbol(symbol),
							..
						}) => match symbol {
							TokenSymbol::Asterisk => PrefixOp::Asterisk,
							TokenSymbol::Ampersand => PrefixOp::Ampersand,
							_ => {
								stream.reset();
								return Ok(None)
							},
						},
						_ => {
							stream.reset();
							return Ok(None)
						},
					})),
					span: stream.current_span,
				};
			
			stream.commit();
			Ok(Some((item,
				&[&PrefixTaker, &OperandTaker],
			)))
		}
	}

	/// Tries to get a binary operator (eg '+', '='?)
	#[derive(Debug)]
	struct BinaryTaker;
	impl ItemTaker for BinaryTaker {
		fn next_item<'a>(&self, stream: &mut TokenStream) -> ItemTakerResult<'a> {
			let item = 
				ExpressionItem {
					kind: ExpressionItemKind::Operator(Operator::Binary(match stream.next_tokentree()? {
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
							_ => {
								stream.reset();
								return Ok(None)
							},
						},
						_ => {
							stream.reset();
							return Ok(None)
						},
					})),
					span: stream.current_span,
				};
				
			stream.commit();
			return Ok(Some((item, &[&PrefixTaker, &OperandTaker])))
		}
	}

	/// Tries to get an operand
	/// The operand can be either a literal, an identifier, or a block (which will be classified as a single operand)
	#[derive(Debug)]
	struct OperandTaker;
	impl ItemTaker for OperandTaker {
		fn next_item<'a>(&self, stream: &mut TokenStream) -> ItemTakerResult<'a> {
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
				let expr_opt = expression_taker.take_expression(stream, ())?;
				if let Some(expr) = expr_opt {
					stream.commit();
					return Ok(Some((
						ExpressionItem {
							kind: ExpressionItemKind::Operand(expr),
							span: stream.current_span,
						},
						&[&BinaryTaker],
					)));
				} else {
					stream.reset();
				}
			}

			Err(ParseError {
				kind: ParseErrorKind::UnexpectedToken(stream.next_token().unwrap().clone()),
				span: stream.current_span,
			}.into()) // Failed to parse operand
		}
	}
}
