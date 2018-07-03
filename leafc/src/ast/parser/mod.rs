use ast::tokenizer::{Keyword, Symbol as TokenSymbol};
use ast::treeify::*;
use ast::parser::operators::*;
use failure::Error;

pub use self::structures::*;
pub use self::errors::*;

pub mod operators;
pub mod debug;
pub mod breakexpr;
pub mod block;
pub mod binding;
pub mod ifexpr;
pub mod loopexpr;
pub mod operation;
pub mod literal;
pub mod identifier;

/// TODO
/// refactor entire thing again to parse into an expression tree, like a buffed up treeify, where
/// it's just operators and operands, so a loop block would be an operand, a plus would be an operator,
/// and wait it's already like that hmm so next_expression and operation_taker need to be merged to
/// work together better somehow hmmmmmmm....

/// Trait that takes an expression from a stream of tokens. If it was successfull, then it returns an option with
/// the expression and the tokens that are still left over. If the expression wasn't there, then it returns None.
/// It can also return an error if there was an unrecoverable error.
pub trait ExpressionTaker {
	type Args;

	fn take_expression<'a>(
		&self,
		in_tokens: &'a [TokenTree],
		args: Self::Args,
	) -> Result<Option<(Expression, &'a [TokenTree])>, Error<ParseError>>;
}

mod structures {
	use super::*;

	/// A symbol
	#[derive(Debug, Clone, Copy, PartialEq, Eq)]
	pub enum Symbol {
		Colon,
		Equals,
		Semicolon,
		ExpressionPointer,
		Exclamation,
	}

	/// A list of statements and an optional return expression
	/// The output is the value that will be returned from the entire
	/// block if a value is set to the syntax tree
	/// TODO rename to Block
	#[derive(Debug, Clone, PartialEq, Eq)]
	pub struct Block {
		pub block: Vec<Expression>, // TODO rename
		pub output: Option<Expression>,
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
	/// TODO make Bind actually hold a 'Binding', not an Assignment
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
		Debug(Box<Expression>),
		Break(Option<Box<Expression>>),
		Binding(Box<Binding>),
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

	/// A let binding. Contains the identifier being bound to, the
	// type of the binding, the expression being bound, and whether is mutable
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
}

mod errors {
	use super::*;

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
		if let Some(ast) = next_syntaxtree(tokens)? {
			Ok(ast)
		} else {
			Err(ParseError::Other.into())
		}
	}
}

static mut RECURSION_LEVEL: usize = 0;

/// Gets statements until it can't from the tokens, and then gets a final expression if there is one
// TODO allow statements that don't end in semicolons as expression
pub fn next_syntaxtree<'a>(
	mut tokens: &'a [TokenTree],
) -> Result<Option<Block>, Error<ParseError>> {
	unsafe {
		RECURSION_LEVEL = RECURSION_LEVEL + 1;
		if RECURSION_LEVEL > 10 {
			return Err(ParseError::Other.into()); // Too much nesting DEBUG purposes only
		}
	}

	let mut stree = Block::new();

	// Parse each statement until none are left
	while let Some((stmnt, leftovers)) = next_statement(tokens)? {
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


	Ok(Some(stree))
}

/// Gets the next statement requiring a terminating semicolon
pub fn next_statement<'a>(
	in_tokens: &'a [TokenTree],
) -> Result<Option<(Expression, &'a [TokenTree])>, Error<ParseError>> {
	if let Some((expr, leftovers)) =
		next_expression(in_tokens, Box::new(|token| token.is_semicolon()))?
	{
		if let Some(TokenTree::Token(Token::Symbol(TokenSymbol::Semicolon))) = leftovers.get(0) {
			Ok(Some((expr, &leftovers[1..])))
		} else {
			Ok(None)
		}
	} else {
		Ok(None)
	}
}

pub fn next_expression<'a>(
	in_tokens: &'a [TokenTree],
	end_predicate: Box<FnMut(&TokenTree) -> bool>,
) -> Result<Option<(Expression, &'a [TokenTree])>, Error<ParseError>> {
	if in_tokens.is_empty() {
		return Ok(None);
	}

	if let Some((expression, leftovers)) =
		operation::OperationTaker::new().take_expression(in_tokens, end_predicate)?
	{
		return Ok(Some((expression, leftovers)));
	}

	Err(ParseError::Other.into()) // Could not parse the next expression
}
