use ast::tokenizer::{Symbol as TokenSymbol};
use ast::treeify::{Token, TokenTree};
use ast::parser::*;
use failure::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionItem {
	Operator(Operator),
	Operand(Expression),
}

/// Trait for a struct that takes an ExpressionItem
pub trait ItemTaker: ::std::fmt::Debug {
	fn next_item<'a>(
		&self,
		tokens: &'a [TokenTree],
	) -> Result<
		Option<
			(
				ExpressionItem,
				&'static [&'static ItemTaker],
				&'a [TokenTree],
			),
		>,
		Error<ParseError>,
	>;
}

/// Parse the entirety of the tokens passed as an expression. If not all the tokens are used up, then there was an error.
pub fn parse_expression(tokens: &[TokenTree]) -> Result<Expression, Error<ParseError>> {
	debug!("Parsing expression from: {:?}", tokens);
	let mut expr_items: Vec<ExpressionItem> = Vec::new();
	let mut item_takers: &[&ItemTaker] = &[&PrefixTaker, &OperandTaker];
	let mut tokens = tokens;

	// Separate operators from operands and put them in expr_items
	'outer: while !tokens.is_empty() {
		for item_taker in item_takers {
			if let Some((item, next_item_takers, leftover_tokens)) =
				item_taker.next_item(tokens)?
			{
				expr_items.push(item);
				item_takers = next_item_takers;
				tokens = leftover_tokens;
				continue 'outer;
			}
		}

		return Err(ParseError::Other.into());
	}

	trace!("Got operators and operands {:?}", expr_items);

	// Simplifies the list of expression items into a single expression. Works recursively, step by step, with a lot of
	// heap allocation.
	// TODO disallow chained binary operators, require parentheses for explicit order of operations
	fn simplify(items: &[ExpressionItem]) -> Result<Expression, Error<ParseError>> {
		if items.len() == 0 {
			// A previous iteration returned something bad
			return Err(ParseError::Other.into());
		}

		// If there's only one item left it should be an expression and not an operator
		if items.len() == 1 {
			match items[0] {
				ExpressionItem::Operand(ref expr) => return Ok(expr.clone()),
				ExpressionItem::Operator(_) => return Err(ParseError::Other.into()), // The expression parsed into an operator
			}
		}

		// Get the highest (lowest precedence) priority operator in this list.
		// Priority is the lowest precedence so for
		let mut priority = 100;
		// Priority op is the index of the lowest precedence so far
		let mut priority_op: usize = 0;

		for (i, item) in items.iter().enumerate() {
			match item {
				ExpressionItem::Operator(op) => if op.precedence() < priority {
					priority = op.precedence();
					priority_op = i;
				} else if op.left_associative() && op.precedence() == priority {
					priority = op.precedence();
					priority_op = i;
				},
				_ => {},
			}
		}

		// Recursively call simplify on each side of an operator that will have an expression
		match items[priority_op] {
			ExpressionItem::Operator(op) => match op {
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
								_ => Err(ParseError::Other.into()), // The left hand side wasn't an identifier
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
			},
			ExpressionItem::Operand(_) => {
				return Err(ParseError::Other.into()); // The token at this index should be an operator
			},
		}
	}

	simplify(&expr_items)
}

/// Tries to get a prefix operator
#[derive(Debug)]
struct PrefixTaker;
impl ItemTaker for PrefixTaker {
	fn next_item<'a>(
		&self,
		tokens: &'a [TokenTree],
	) -> Result<
		Option<
			(
				ExpressionItem,
				&'static [&'static ItemTaker],
				&'a [TokenTree],
			),
		>,
		Error<ParseError>,
	> {
		if let Some(token) = tokens.get(0) {
			Ok(Some((
				ExpressionItem::Operator(Operator::Prefix(match token {
					TokenTree::Token(Token::Symbol(symbol)) => match symbol {
						TokenSymbol::Asterisk => PrefixOp::Asterisk,
						TokenSymbol::Ampersand => PrefixOp::Ampersand,
						_ => return Ok(None),
					},
					_ => return Ok(None),
				})),
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
	fn next_item<'a>(
		&self,
		tokens: &'a [TokenTree],
	) -> Result<
		Option<
			(
				ExpressionItem,
				&'static [&'static ItemTaker],
				&'a [TokenTree],
			),
		>,
		Error<ParseError>,
	> {
		debug!("Taking binary from: {:?}", tokens);
		if let Some(token) = tokens.get(0) {
			Ok(Some((
				ExpressionItem::Operator(Operator::Binary(match token {
					TokenTree::Token(Token::Symbol(symbol)) => match symbol {
						TokenSymbol::Plus => BinaryOp::Add,
						TokenSymbol::Assign => BinaryOp::Assign,
						_ => return Ok(None),
					},
					_ => return Ok(None),
				})),
				&[&PrefixTaker, &OperandTaker],
				&tokens[1..],
			)))
		} else {
			Ok(None)
		}
	}
}

/// TODO allow break statements to be operands I guess... so if 1 then break;` works
/// Tries to get an operand
/// The operand can be either a literal, an identifier, or a block (which will be classified as a single operand)
#[derive(Debug)]
struct OperandTaker;
impl ItemTaker for OperandTaker {
	fn next_item<'a>(
		&self,
		tokens: &'a [TokenTree],
	) -> Result<
		Option<
			(
				ExpressionItem,
				&'static [&'static ItemTaker],
				&'a [TokenTree],
			),
		>,
		Error<ParseError>,
	> {
		debug!("Taking operand from: {:?}", tokens);
		if let Some(token) = tokens.get(0) {
			let mut remaining = &tokens[1..];
			Ok(Some((
				ExpressionItem::Operand(match token {
					TokenTree::Token(Token::Name(ref name)) => {
						Expression::Identifier(name.clone())
					},
					TokenTree::Token(Token::NumberLiteral(num)) => {
						Expression::NumberLiteral(*num)
					},
					TokenTree::Token(Token::StringLiteral(ref string)) => {
						Expression::StringLiteral(string.clone())
					},
					TokenTree::Brace(ref sub_tokens) => {
						// Try to parse the block as a syntax tree
						if let Some(block) = next_syntaxtree(sub_tokens)? {
							Expression::Block(Box::new(block))
						} else {
							return Ok(None);
						}
					},
					_ => {
						if let Some(block) = next_syntaxtree(&tokens)? {
							remaining = &tokens[tokens.len()..];
							Expression::Block(Box::new(block))
						} else {
							return Ok(None);
						}
					},
				}),
				&[&BinaryTaker],
				remaining,
			)))
		} else {
			Ok(None)
		}
	}
}