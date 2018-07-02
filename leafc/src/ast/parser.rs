use ast::tokenizer::{Keyword, Symbol as TokenSymbol};
use ast::treeify::{Token, TokenTree};
use ast::expression;
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
pub struct Block {
	pub block: Vec<Statement>,
	pub output: Option<Expression>,
}

impl Block {
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
	Break(Option<Expression>),
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
	pub body: Block,
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
	Loop(Box<Expression>),
	If(Box<If>),
	Identifier(String),
	Block(Box<Block>),
	StringLiteral(String),
	NumberLiteral(u64),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct If {
	pub condition: Expression,
	pub body: Expression,
	pub elif: Option<Box<If>>,
	pub else_block: Option<Expression>,
}

/// A let binding. Contains the identifier being bound to, the type of the binding, the expression being bound, and whether is mutable
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding {
	pub mutable: bool,
	pub ident: String,
	pub bind_type: Option<String>,
	pub val: Option<Expression>,
}

impl Block {
	pub fn new() -> Self {
		Block {
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

/// Parse a block from the tokens (will use all of the tokens or error)
pub fn parse(tokens: &[TokenTree]) -> Result<Block, Error<ParseError>> {
	debug!("Parsing tokens: {:?}", tokens);
	if let Some(ast) = next_syntaxtree(tokens)? {
		Ok(ast)
	} else {
		Err(ParseError::Other.into())
	}
}

static mut RECURSION_LEVEL: usize = 0;

/// Gets statements until it can't from the tokens, and then gets a final expression if there is one
// TODO allow statements that don't end in semicolons as expression
pub fn next_syntaxtree<'a>(
	mut tokens: &'a [TokenTree],
) -> Result<Option<Block>, Error<ParseError>> {
	debug!("Parsing syntaxtree from: {:?}", tokens);
	unsafe {
		RECURSION_LEVEL = RECURSION_LEVEL + 1;
		if RECURSION_LEVEL > 10 {
			return Err(ParseError::Other.into()); // Too much nesting DEBUG purposes only
		}
	}

	let mut stree = Block::new();

	// Parse each statement until none are left
	while let Some((stmnt, leftovers)) = next_statement(tokens, true)? {
		stree.block.push(stmnt);
		tokens = leftovers;
	}
	// Parse the final expression if there is one
	if let Some((expr, leftovers)) = next_expression(tokens, Box::new(|_| false))? {
		if !leftovers.is_empty() {
			return Err(ParseError::UnexpectedToken.into()); // There were tokens after the final expression which there shouldn't be
		}
		stree.output = Some(expr);
	}

	debug!("Got syntaxtree {:?}", stree);

	Ok(Some(stree))
}

/// Gets the next statement until a terminating semicolon
pub fn next_statement<'a>(
	start_tokens: &'a [TokenTree],
	require_semicolon: bool,
) -> Result<Option<(Statement, &'a [TokenTree])>, Error<ParseError>> {
	debug!("Parsing statement from: {:?}", start_tokens);
	if start_tokens.is_empty() {
		return Ok(None);
	}

	let mut tokens = start_tokens;

	// Try to parse a let binding
	let statement = if let Some((binding, leftovers)) = next_binding(tokens)? {
		tokens = leftovers;
		Statement::Binding(binding)
	// Try to parse a debug statement
	} else if let Some((debug_expr, leftovers)) = next_debug(tokens)? {
		tokens = leftovers;
		Statement::Debug(debug_expr)
	// Try to parse a break statement
	} else if let Some((break_expr, leftovers)) = next_break(tokens)? {
		tokens = leftovers;
		Statement::Break(break_expr)
	// Try to parse an expression followed by a semicolon
	} else if let Some((expression, leftovers)) =
		next_expression(tokens, Box::new(|token| token.is_semicolon()))?
	{
		tokens = leftovers;
		Statement::Expression(expression)
	} else {
		return Ok(None);
	};

	if require_semicolon {
		if let Some(TokenTree::Token(Token::Symbol(TokenSymbol::Semicolon))) = tokens.get(0) {
			tokens = &tokens[1..];
		} else {
			return Ok(None);
		}
	}

	debug!("Got statement {:?} from {:?} with leftovers {:?}", statement, start_tokens, tokens);

	Ok(Some((statement, tokens)))
}

/// Gets the next block from a token list, starting from an open brace ('{') and parsing everything
/// until the close brace, and returning everything after the last close brace as leftovers
pub fn next_block<'a>(
	tokens: &'a [TokenTree],
) -> Result<Option<(Block, &'a [TokenTree])>, Error<ParseError>> {
	debug!("Parsing block from: {:?}", tokens);
	if tokens.is_empty() {
		return Ok(None);
	}
	match tokens[0] {
		TokenTree::Brace(ref tokens) => {
			if let Some(ast) = next_syntaxtree(tokens)? {
				return Ok(Some((ast, &tokens[1..])));
			} else {
				// Should never happen
				return Err(ParseError::UnexpectedToken.into());
			}
		},
		_ => return Ok(None),
	}
}

/// Gets the next `let` binding
#[allow(unused_mut)]
fn next_binding<'a>(
	tokens: &'a [TokenTree],
) -> Result<Option<(Binding, &'a [TokenTree])>, Error<ParseError>> {
	debug!("Parsing binding from {:?}", tokens);
	if tokens.is_empty() {
		return Ok(None);
	}

	let mut tokens = tokens;

	match tokens[0] {
		// If the first token is `let` then it's a let binding
		TokenTree::Token(Token::Keyword(Keyword::Let)) => {
			// Get the identifier (which should always be the second token)
			let ident = if let TokenTree::Token(Token::Name(ref name)) = tokens[1] {
				name.to_owned()
			} else {
				return Ok(None); // TODO add expected 'identifier'
			};

			// If there's a type annotation, parse it
			// Set offset to how many tokens the type annotation occupied
			let mut offset = 0;
			let mut type_annotation: Option<String> =
				if let TokenTree::Token(Token::Symbol(TokenSymbol::Colon)) = tokens[2] {
					// if there's a type annotation
					// change token offset
					unimplemented!()
				} else {
					None
				};

			// Make sure there's an equals sign after the identifier or type annotation
			if let TokenTree::Token(Token::Symbol(TokenSymbol::Assign)) = tokens[2 + offset] {
			} else {
				return Ok(None);
			}

			// Parse the expression part of the binding
			let expr = if let Some((expr, leftovers)) = next_expression(
				&tokens[2 + offset + 1..],
				Box::new(|token| token.is_semicolon()),
			)? {
				tokens = leftovers;
				expr
			} else {
				return Ok(None);
			};

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
	tokens: &'a [TokenTree],
) -> Result<Option<(Expression, &'a [TokenTree])>, Error<ParseError>> {
	debug!("Parsing debug from: {:?}", tokens);
	if tokens.is_empty() {
		return Ok(None);
	}

	let mut tokens = tokens;

	match tokens[0] {
		// If the first token is debug it's a debug statement
		TokenTree::Token(Token::Keyword(Keyword::Debug)) => {
			// Get the expression after the keyword
			let expr = if let Some((expr, leftovers)) =
				next_expression(&tokens[1..], Box::new(|token| token.is_semicolon()))?
			{
				tokens = leftovers;
				expr
			} else {
				return Ok(None);
			};

			// Return the expression being debugged
			Ok(Some((expr, tokens)))
		},
		_ => Ok(None),
	}
}

/// Gets the next debug statement
// TODO include expression original string repr with debug
fn next_break<'a>(
	tokens: &'a [TokenTree],
) -> Result<Option<(Option<Expression>, &'a [TokenTree])>, Error<ParseError>> {
	debug!("Parsing break from: {:?}", tokens);
	if tokens.is_empty() {
		return Ok(None);
	}

	match tokens[0] {
		// If the first token is debug it's a break statement
		TokenTree::Token(Token::Keyword(Keyword::Break)) => {
			// Get the expression after the keyword
			if let Some((expr, leftovers)) =
				next_expression(&tokens[1..], Box::new(|token| token.is_semicolon()))?
			{
				Ok(Some((Some(expr), leftovers)))
			} else {
				Ok(Some((None, &tokens[1..])))
			}
		},
		_ => Ok(None),
	}
}

/// Gets the entirety of the next expression available as a single expression. This taker only stops upon recieving a token
/// that matches the predicate given
fn next_expression<'a>(
	tokens: &'a [TokenTree],
	end_predicate: Box<FnMut(&TokenTree) -> bool>,
) -> Result<Option<(Expression, &'a [TokenTree])>, Error<ParseError>> {
	debug!("Parsing next expression from: {:?}", tokens);
	if tokens.is_empty() {
		return Ok(None);
	}

	// Get the tokens involved in the next expression
	let (expr_tokens, leftovers) = split_at(tokens, end_predicate, false);

	trace!("Expr tokens : {:?}", expr_tokens);

	// Handle all different kinds of expressions, like loop blocks, or just plain arithmetic
	match expr_tokens[0] {
		TokenTree::Token(Token::Keyword(Keyword::Loop)) => {
			// If theres a brace block after loop keyword, just include that in the loop expression
			if let Some(TokenTree::Brace(_)) = expr_tokens.get(1) {
				if let Some((block, extra_leftovers)) = next_expression(&expr_tokens[1..], Box::new(|token| !token.is_brace_expr()))? {
					if !extra_leftovers.is_empty() {
						return Err(ParseError::Other.into()) // There where leftover tokens in the expression
					}

					Ok(Some((Expression::Loop(Box::new(block)), leftovers)))
				} else {
					Ok(None)
				}
			} else {
				if let Some((block, extra_leftovers)) = next_expression(&expr_tokens[1..], Box::new(|token| token.is_semicolon()))? {
					if !extra_leftovers.is_empty() {
						return Err(ParseError::Other.into()) // There where leftover tokens in the expression
					}

					Ok(Some((Expression::Loop(Box::new(block)), leftovers)))
				} else {
					Ok(None)
				}
			}
		},
		TokenTree::Token(Token::Keyword(Keyword::If)) => {
			let (condition, sub_leftovers) = if let Some((condition, extra_leftovers)) =
				next_expression(&expr_tokens[1..], Box::new(|token| token.is_then()))?
			{
				match extra_leftovers.get(0) {
					Some(TokenTree::Token(Token::Keyword(Keyword::Then))) => {},
					_ => return Err(ParseError::Other.into()), // Missing a then keyword in the if
				}

				(condition, extra_leftovers)
			} else {
				return Err(ParseError::Other.into()); // The if statement had no condition
			};

			let (body, sub_leftovers) = if let Some(token) = sub_leftovers.get(0) {
				match token {
					TokenTree::Token(Token::Keyword(Keyword::Then)) => {
						if let Some((body, extra_leftovers)) = next_expression(
							&sub_leftovers[1..],
							Box::new(|token| token.is_else() || token.is_semicolon()),
						)? {
							(body, extra_leftovers)
						} else {
							// There was no condition block after the is statement
							return Err(ParseError::Other.into());
						}
					},
					// There was no then keyword after the condition
					_ => return Err(ParseError::Other.into()),
				}
			} else {
				// There was no then keyword after the condition
				return Err(ParseError::Other.into());
			};

			let (else_block, sub_leftovers) = if let Some(token) = sub_leftovers.get(0) {
				match token {
					TokenTree::Token(Token::Symbol(TokenSymbol::Semicolon)) => {
						(None, sub_leftovers)
					},
					TokenTree::Token(Token::Keyword(Keyword::Else)) => {
						if let Some((body, extra_leftovers)) = next_expression(
							&sub_leftovers[1..],
							Box::new(|token| token.is_semicolon()),
						)? {
							(Some(body), extra_leftovers)
						} else {
							// There should be an expression after the else keyword
							// and before the semicolon
							return Err(ParseError::Other.into());
						}
					},
					// There should be an expression after the else keyword and before the semicolon
					_ => return Err(ParseError::Other.into()),
				}
			} else {
				(None, sub_leftovers)
			};

			if !sub_leftovers.is_empty() {
				return Err(ParseError::Other.into()); // Could parse it as an expression
			}

			// TODO support elif

			Ok(Some((
				Expression::If(Box::new(If {
					condition,
					body,
					elif: None,
					else_block,
				})),
				leftovers,
			)))
		},
		_ => expression::parse_expression(expr_tokens).map(|expr| Some((expr, leftovers))),
	}
}

fn split_at(
	tokens: &[TokenTree],
	mut condition: Box<FnMut(&TokenTree) -> bool>,
	exclusive: bool,
) -> (&[TokenTree], &[TokenTree]) {
	for (i, token) in tokens.iter().enumerate() {
		if condition(token) {
			return (&tokens[0..i], &tokens[i + exclusive as usize..]);
		}
	}

	return (&tokens[..], &tokens[tokens.len()..]);
}
