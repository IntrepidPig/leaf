use ast::parser::*;

type ItemTakerResult<'a> = Result<Option<(ExpressionItem, &'static [&'static ItemTaker])>, Error<ParseError>>;

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

impl<'a> TokenTree<'a> {
	pub fn is_operator(&self) -> bool {
		match self {
			TokenTree::Token(Token {
				kind: TokenKind::Symbol(Symbol::Ampersand),
				..
			}) => true,
			TokenTree::Token(Token {
				kind: TokenKind::Symbol(Symbol::Assign),
				..
			}) => true,
			TokenTree::Token(Token {
				kind: TokenKind::Symbol(Symbol::Asterisk),
				..
			}) => true,
			TokenTree::Token(Token {
				kind: TokenKind::Symbol(Symbol::Dot),
				..
			}) => true,
			TokenTree::Token(Token {
				kind: TokenKind::Symbol(Symbol::Equality),
				..
			}) => true,
			TokenTree::Token(Token {
				kind: TokenKind::Symbol(Symbol::Equals),
				..
			}) => true,
			TokenTree::Token(Token {
				kind: TokenKind::Symbol(Symbol::Exclamation),
				..
			}) => true,
			TokenTree::Token(Token {
				kind: TokenKind::Symbol(Symbol::Greater),
				..
			}) => true,
			TokenTree::Token(Token {
				kind: TokenKind::Symbol(Symbol::Less),
				..
			}) => true,
			TokenTree::Token(Token {
				kind: TokenKind::Symbol(Symbol::Minus),
				..
			}) => true,
			TokenTree::Token(Token {
				kind: TokenKind::Symbol(Symbol::Plus),
				..
			}) => true,
			TokenTree::Token(Token {
				kind: TokenKind::Symbol(Symbol::Question),
				..
			}) => true,
			TokenTree::Token(Token {
				kind: TokenKind::Symbol(Symbol::Slash),
				..
			}) => true,
			_ => false,
		}
	}
}

/// Parse the entirety of the tokens passed as an expression. If not all the tokens are used
/// up, then there was an error.
pub fn parse_expression(stream: &mut TokenStream) -> Result<Expression, Error<ParseError>> {
	let mut expr_items: Vec<ExpressionItem> = Vec::new();
	let mut item_takers: &[&ItemTaker] = &[];

	'outer: while !stream.is_empty() {
		let (mut expr_tokens, _exhausted) = stream.split_when(|t| t.is_operator(), false)?;
		if !expr_tokens.is_empty() {
			expr_items.push(ExpressionItem {
				kind: ExpressionItemKind::Operand(expression::parse_operand(&mut expr_tokens)?),
				span: expr_tokens.current_span, // TODO make correct span
			});
			item_takers = &[&BinaryTaker] // TODO postfix
		}
		if stream.is_empty() {
			break;
		}
		for item_taker in item_takers {
			let old_position = stream.get_position();
			if let Some((item, next_item_takers)) = item_taker.next_item(stream)? {
				expr_items.push(item);
				item_takers = next_item_takers;
				continue 'outer;
			} else {
				stream.seek(old_position);
			}
		}
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
									Expression::Identifier(ref ident) => Ok(Expression::Assign(Box::new(Assignment {
										ident: ident.to_owned(),
										expr: rhs,
									}))),
									Expression::Binding(ref binding) => {
										Ok(Expression::Binding(Box::new(Binding {
											ident: binding.ident.clone(),
											mutable: binding.mutable,
											bind_type: binding.bind_type.clone(),
											val: Some(rhs), // TODO understand
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

/*/// Tries to get a prefix operator
#[derive(Debug)]
struct PrefixTaker;
impl ItemTaker for PrefixTaker {
	fn next_item<'a>(&self, stream: &mut TokenStream) -> ItemTakerResult<'a> {
		let item = 
			ExpressionItem {
				kind: ExpressionItemKind::Operator(Operator::Prefix(match stream.take_tokentree()? {
					TokenTree::Token(Token {
						kind: TokenKind::Symbol(symbol),
						..
					}) => match symbol {
						Symbol::Asterisk => PrefixOp::Asterisk,
						Symbol::Ampersand => PrefixOp::Ampersand,
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
}*/

/// Tries to get a binary operator (eg '+', '='?)
#[derive(Debug)]
struct BinaryTaker;
impl ItemTaker for BinaryTaker {
	fn next_item<'a>(&self, stream: &mut TokenStream) -> ItemTakerResult<'a> {
		let old_position = stream.get_position();
		let item = ExpressionItem {
			kind: ExpressionItemKind::Operator(Operator::Binary(match stream.take_tokentree()? {
				TokenTree::Token(Token {
					kind: TokenKind::Symbol(symbol),
					..
				}) => match symbol {
					Symbol::Plus => BinaryOp::Add,
					Symbol::Minus => BinaryOp::Sub,
					Symbol::Asterisk => BinaryOp::Mul,
					Symbol::Slash => BinaryOp::Div,
					Symbol::Assign => BinaryOp::Assign,
					Symbol::Equality => BinaryOp::Equality,
					Symbol::Dot => BinaryOp::Dot,
					_ => {
						stream.seek(old_position);
						return Ok(None);
					},
				},
				_ => {
					stream.seek(old_position);
					return Ok(None);
				},
			})),
			span: stream.current_span,
		};

		Ok(Some((item, &[])))
	}
}
