use ast::lexer::{Bracket, BracketState};
use ast::tokenizer::{Keyword, Symbol as TokenSymbol, Token};
use failure::Error;

/// A symbol
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Symbol {
	Colon,
	Equals,
	Semicolon,
	ExpressionPointer,
	Exclamation,
}

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

/// A list of statements and an optional return expression
/// The output is the value that will be returned from the entire block if a value is set to the syntax tree
/// TODO rename to Block
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxTree {
	pub block: Vec<Statement>,
	pub output: Option<Expression>,
}

impl SyntaxTree {
	pub fn push(&mut self, syntax: Statement) -> Result<(), Error<ParseError>> {
		self.block.push(syntax);

		Ok(())
	}
}

/// A statement
/// Can be either a `let` binding, a standalone expression (whos result will be immediately dropped), or a debug statement
/// A statement always ends with a semicolon
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
	Binding(Binding),
	Expression(Expression),
	Debug(Expression),
}

/// An assignment
/// Contains the left hand side
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assignment {
	pub ident: String,
	pub expr: Expression,
}

/// A function definition
/// Not used for now
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
	pub name: String,
	pub args: Vec<Binding>,
	pub return_type: String,
	pub body: SyntaxTree,
}

/// An expression
/// Can be a literal, an operations, or a block that contains more expressions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
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
		base: Option<Box<Expression>>,
		function: String,
		args: Vec<Expression>,
	},
	Assign(Box<Assignment>),
	Loop(Box<SyntaxTree>),
	Identifier(String),
	Block(Box<SyntaxTree>),
	StringLiteral(String),
	NumberLiteral(u64),
}

/// A let binding. Contains the identifier being bound to, the type of the binding, the expression being bound, and whether is mutable
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding {
	pub mutable: bool,
	pub ident: String,
	pub bind_type: Option<String>,
	pub val: Option<Expression>,
}

impl SyntaxTree {
	pub fn new() -> Self {
		SyntaxTree {
			block: Vec::new(),
			output: None,
		}
	}
}

/// An error that occurs while parsing
#[derive(Debug, Clone)]
pub enum ParseError {
	UnexpectedToken,
	UnclosedBrace,
	Other,
}

impl ::std::fmt::Display for ParseError {
	fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
		write!(f, "{:?}", self)
	}
}

impl ::std::error::Error for ParseError {}

/// A struct that counts the amunot of braces
struct TerminatorCounter {
	brackets: i32, // square brackets
	parens: i32,
	braces: i32, // curly braces
	semicolon: i32,
}

impl TerminatorCounter {
	pub fn new() -> Self {
		TerminatorCounter {
			brackets: 0,
			parens: 0,
			braces: 0,
			semicolon: 0,
		}
	}

	/// Split the tokens into a slic of tokens before a terminating token, and the tokens after and including
	/// the terminating token.
	/// A semicolon is a terminating token if it's not inside any brackets.
	/// A closing bracket is a terminating token if it has no matching opening bracket.
	pub fn split_until_end(tokens: &[Token]) -> (&[Token], &[Token]) {
		let mut counter = TerminatorCounter::new();

		for (i, token) in tokens.iter().enumerate() {
			counter.add(token);
			if counter.is_terminated() {
				return (&tokens[0..i], &tokens[i..]);
			}
		}

		(tokens, &[])
	}

	/// Update the counts based on a token
	pub fn add(&mut self, token: &Token) {
		match token {
			Token::Symbol(symbol) => match symbol {
				TokenSymbol::Semicolon => {
					// Only subtract from semicolon count if it's not inside some other brackets
					if !self.is_contained() {
						self.semicolon -= 1;
					}
				},
				_ => {},
			},
			Token::Bracket(bracket, bracketstate) => {
				let target = match bracket {
					Bracket::Curly => &mut self.braces,
					Bracket::Paren => &mut self.parens,
					Bracket::Square => &mut self.brackets,
				};

				*target += 1 * match bracketstate {
					BracketState::Open => 1,
					BracketState::Close => -1,
				};
			},
			_ => {},
		}
	}

	/// Check if a terminating token has been found
	pub fn is_terminated(&self) -> bool {
		self.braces < 0 || self.brackets < 0 || self.parens < 0 || self.semicolon < 0
	}

	/// Check if is currently inside a sub-block
	pub fn is_contained(&self) -> bool {
		self.braces > 0 || self.brackets > 0 || self.parens > 0
	}
}

/// Parse a block from the tokens (will use all of the tokens or error)
pub fn parse(tokens: &[Token]) -> Result<SyntaxTree, Error<ParseError>> {
	if let Some(ast) = next_syntaxtree(tokens)? {
		Ok(ast)
	} else {
		Err(ParseError::Other.into())
	}
}

/// Gets statements until it can't from the tokens, and then gets a final expression if there is one
pub fn next_syntaxtree<'a>(
	mut tokens: &'a [Token],
) -> Result<Option<SyntaxTree>, Error<ParseError>> {
	let mut stree = SyntaxTree::new();

	// Parse each statement until none are left
	while let Some((stmnt, leftovers)) = next_statement(tokens)? {
		stree.block.push(stmnt);
		tokens = leftovers;
	}
	// Parse the final expression if there is one
	if let Some((expr, leftovers)) = next_expression(tokens)? {
		if !leftovers.is_empty() {
			return Err(ParseError::UnexpectedToken.into()); // There were tokens after the final expression which there shouldn't be
		}
		stree.output = Some(expr);
	}

	Ok(Some(stree))
}

/// Gets the next statement until a terminating semicolon
pub fn next_statement<'a>(
	tokens: &'a [Token],
) -> Result<Option<(Statement, &'a [Token])>, Error<ParseError>> {
	if tokens.is_empty() {
		return Ok(None);
	}

	// Try to parse a let binding
	if let Some((binding, leftovers)) = next_binding(tokens)? {
		return Ok(Some((Statement::Binding(binding), leftovers)));
	// Try to parse a debug statement
	} else if let Some((debug, leftovers)) = next_debug(tokens)? {
		return Ok(Some((Statement::Debug(debug), leftovers)));
	// Try to parse an expression followed by a semicolon
	} else if let Some((expression, leftovers)) = next_expression(tokens)? {
		if leftovers.get(0) == Some(&Token::Symbol(TokenSymbol::Semicolon)) {
			return Ok(Some((Statement::Expression(expression), &leftovers[1..])));
		} else {
			return Ok(None);
		}
	}

	Err(ParseError::Other.into()) // maybe should be (Ok(None))
}

/// Gets the next block from a token list, starting from an open brace ('{') and parsing everything
/// until the close brace, and returning everything after the last close brace as leftovers
pub fn next_block<'a>(
	tokens: &'a [Token],
) -> Result<Option<(SyntaxTree, &'a [Token])>, Error<ParseError>> {
	if tokens.is_empty() {
		return Ok(None);
	}
	match tokens[0] {
		Token::Bracket(Bracket::Curly, BracketState::Open) => {
			let mut buf = 0;
			// TODO use TerminatorCounter
			for (i, token) in tokens[0..].iter().enumerate() {
				if let Some((bracket, bracketstate)) = token.as_bracket() {
					match (bracket, bracketstate) {
						(Bracket::Curly, BracketState::Open) => buf += 1,
						(Bracket::Curly, BracketState::Close) => buf -= 1,
						_ => {},
					}
				}
				if buf == 0 {
					// Parse whatever's inside this block as an AST [after first brace..last brace]
					let ast = if let Some(ast) = next_syntaxtree(&tokens[1..i])? {
						ast
					} else {
						// Did not parse what was within the block (will never happen)
						return Err(ParseError::UnexpectedToken.into());
					};
					// And the leftover tokens [after last brace..end of tokens]
					return Ok(Some((ast, &tokens[i + 1..])));
				}
			}

			// A brace was not closed by the end of the token list
			return Err(ParseError::UnclosedBrace.into());
		},
		_ => return Ok(None),
	}
}

/// Gets the next `let` binding
#[allow(unused_mut)]
fn next_binding<'a>(
	tokens: &'a [Token],
) -> Result<Option<(Binding, &'a [Token])>, Error<ParseError>> {
	if tokens.is_empty() {
		return Ok(None);
	}

	let mut tokens = tokens;

	match tokens[0] {
		// If the first token is `let` then it's a let binding
		Token::Keyword(Keyword::Let) => {
			// Get the identifier (which should always be the second token)
			let ident = if let Token::Name(ref name) = tokens[1] {
				name.to_owned()
			} else {
				return Ok(None); // TODO add expected 'identifier'
			};

			// If there's a type annotation, parse it
			// Set offset to how many tokens the type annotation occupied
			let mut offset = 0;
			let mut type_annotation: Option<String> =
				if let Token::Symbol(TokenSymbol::Colon) = tokens[2] {
					// if there's a type annotation
					// change token offset
					unimplemented!()
				} else {
					None
				};

			// Make sure there's an equals sign after the identifier or type annotation
			if let Token::Symbol(TokenSymbol::Assign) = tokens[2 + offset] {
			} else {
				return Ok(None);
			}

			// Parse the expression part of the binding
			let expr = if let Some((expr, leftovers)) = next_expression(&tokens[2 + offset + 1..])?
			{
				tokens = leftovers;
				expr
			} else {
				return Ok(None);
			};

			// Make sure there's a semicolon
			if let Some(&Token::Symbol(TokenSymbol::Semicolon)) = tokens.get(0) {
				tokens = &tokens[1..];
			} else {
				return Ok(None);
			}

			Ok(Some((
				Binding {
					mutable: false,
					ident,
					bind_type: type_annotation,
					val: Some(expr),
				},
				tokens,
			)))
		},
		_ => Ok(None),
	}
}

/// Gets the next debug statement
// TODO include expression original string repr with debug
fn next_debug<'a>(
	tokens: &'a [Token],
) -> Result<Option<(Expression, &'a [Token])>, Error<ParseError>> {
	if tokens.is_empty() {
		return Ok(None);
	}

	let mut tokens = tokens;

	match tokens[0] {
		// If the first token is debug it's a debug statement
		Token::Keyword(Keyword::Debug) => {
			// Get the expression after the keyword
			let expr = if let Some((expr, leftovers)) = next_expression(&tokens[1..])? {
				tokens = leftovers;
				expr
			} else {
				return Ok(None);
			};

			// Make sure there's a semicolon
			if let Token::Symbol(TokenSymbol::Semicolon) = tokens[0] {
				tokens = &tokens[1..];
			} else {
				return Ok(None);
			}

			// Return the expression being debugged
			Ok(Some((expr, tokens)))
		},
		_ => Ok(None),
	}
}

/// Gets the entirety of the next expression available as a single expression. This taker only stops upon recieving a semicolon outside of
/// a block or an unmatched closing brace. The expression it returns may contain nested expressions, parsed as their own blocks.
fn next_expression<'a>(
	tokens: &'a [Token],
) -> Result<Option<(Expression, &'a [Token])>, Error<ParseError>> {
	if tokens.is_empty() {
		return Ok(None);
	}

	// Get the tokens before the terminator
	let (expr_tokens, leftovers) = TerminatorCounter::split_until_end(tokens);

	// Handle all different kinds of expressions, like loop blocks, or just plain arithmetic
	match expr_tokens[0] {
		Token::Keyword(Keyword::Loop) => {
			if let Some((block, extra_leftovers)) = next_block(&expr_tokens[1..])? {
				if !extra_leftovers.is_empty() {
					// The TerminatorCounter above should have ensured that only a block was included in expr_tokens
					return Err(ParseError::Other.into())
				}
				Ok(Some((Expression::Loop(Box::new(block)), leftovers)))
			} else {
				return Err(ParseError::Other.into()) // Loop keywords needs a block directly after
			}
		},
		_ => expressions::parse_expression(expr_tokens).map(|expr| Some((expr, leftovers)))
	}	
}

/// Contains logic for parsing expressions
mod expressions {
	use super::*;

	#[derive(Debug, Clone, PartialEq, Eq)]
	pub enum ExpressionItem {
		Operator(Operator),
		Operand(Expression),
	}

	/// Trait for a struct that takes an ExpressionItem
	pub trait ItemTaker: ::std::fmt::Debug {
		fn next_item<'a>(
			&self,
			tokens: &'a [Token],
		) -> Result<
			Option<(ExpressionItem, &'static [&'static ItemTaker], &'a [Token])>,
			Error<ParseError>,
		>;
	}

	/// Parse the entirety of the tokens passed as an expression. If not all the tokens are used up, then there was an error.
	pub fn parse_expression(tokens: &[Token]) -> Result<Expression, Error<ParseError>> {
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
			tokens: &'a [Token],
		) -> Result<
			Option<(ExpressionItem, &'static [&'static ItemTaker], &'a [Token])>,
			Error<ParseError>,
		> {
			if let Some(token) = tokens.get(0) {
				Ok(Some((
					ExpressionItem::Operator(Operator::Prefix(match token {
						Token::Symbol(symbol) => match symbol {
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
			tokens: &'a [Token],
		) -> Result<
			Option<(ExpressionItem, &'static [&'static ItemTaker], &'a [Token])>,
			Error<ParseError>,
		> {
			if let Some(token) = tokens.get(0) {
				Ok(Some((
					ExpressionItem::Operator(Operator::Binary(match token {
						Token::Symbol(symbol) => match symbol {
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

	/// Tries to get an operand
	/// The operand can be either a literal, an identifier, or a block (which will be classified as a single operand)
	#[derive(Debug)]
	struct OperandTaker;
	impl ItemTaker for OperandTaker {
		fn next_item<'a>(
			&self,
			tokens: &'a [Token],
		) -> Result<
			Option<(ExpressionItem, &'static [&'static ItemTaker], &'a [Token])>,
			Error<ParseError>,
		> {
			let mut remaining = &tokens[1..];
			if let Some(token) = tokens.get(0) {
				Ok(Some((
					ExpressionItem::Operand(match token {
						Token::NumberLiteral(num) => Expression::NumberLiteral(*num),
						Token::Name(ref name) => Expression::Identifier(name.to_owned()),
						Token::Bracket(Bracket::Curly, BracketState::Open) => {
							// Try to parse the block as a syntax tree
							if let Some((block, leftovers)) = next_block(tokens)? {
								remaining = leftovers;
								Expression::Block(Box::new(block))
							} else {
								return Ok(None);
							}
						},
						_ => return Ok(None),
					}),
					&[&BinaryTaker],
					remaining,
				)))
			} else {
				Ok(None)
			}
		}
	}
}
