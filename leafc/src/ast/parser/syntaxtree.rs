use ast::tokenizer::Symbol as TokenSymbol;
use ast::treeify::*;
use ast::parser::*;
use ast::parser::operators::*;
use failure::Error;

pub use self::structures::*;
pub use self::errors::*;

pub type ParseResult<'a, T> = Result<Option<(T, &'a [TokenTree])>, Error<ParseError>>;

/// Trait that takes an expression from a stream of tokens. If it was successfull, then it returns an option with
/// the expression and the tokens that are still left over. If the expression wasn't there, then it returns None.
/// It can also return an error if there was an unrecoverable error.
pub trait ExpressionTaker {
	type Args;

	fn take_expression<'a>(&self, in_tokens: &'a [TokenTree], args: Self::Args) -> ParseResult<'a, Expression>;
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

	#[derive(Debug, Clone, PartialEq, Eq)]
	pub struct Module {
		pub body: SyntaxTree,
	}

	impl Module {
		pub fn traverse_mut<F: FnMut(&ModulePath, &mut Module)>(&mut self, f: &mut F, start_path: &mut ModulePath) {
			f(&start_path, self);
			for (name, module) in &mut self.body.modules {
				start_path.path.push(name.clone());
				module.traverse_mut(f, start_path);
				start_path.path.pop().unwrap();
			}
		}

		pub fn traverse<F: FnMut(&ModulePath, &Module)>(&self, f: &mut F, start_path: &mut ModulePath) {
			f(&start_path, self);
			for (name, module) in &self.body.modules {
				start_path.path.push(name.clone());
				module.traverse(f, start_path);
				start_path.path.pop().unwrap();
			}
		}
	}

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub struct Identifier {
		name: String,
	}

	impl Identifier {
		pub fn from_string(string: String) -> Self {
			// TODO validate identifier string and return result
			Identifier { name: string }
		}

		pub fn try_from_str(string: &str) -> Self {
			// TODO validate identifier string
			Identifier {
				name: string.to_owned(),
			}
		}
	}

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub struct TypeName {
		pub name: Identifier,
	}

	impl TypeName {
		pub fn from_ident(identifier: Identifier) -> Self {
			TypeName { name: identifier }
		}
	}

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub struct PathItem<T> {
		pub module_path: ModulePath,
		pub item: T,
	}

	impl<T> PathItem<T> {
		pub fn map<O, F: Fn(T) -> O>(self, f: F) -> PathItem<O> {
			PathItem {
				module_path: self.module_path,
				item: f(self.item),
			}
		}
	}

	impl ::std::fmt::Display for PathItem<Identifier> {
		fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
			if !self.module_path.relative {
				write!(f, "::")?;
			}
			for module in &self.module_path.path {
				write!(f, "{}::", module.name)?;
			}
			write!(f, "{}", self.item.name)?;

			Ok(())
		}
	}

	impl Module {
		pub fn new(body: SyntaxTree) -> Self {
			Module { body }
		}
	}

	#[derive(Debug, Clone, PartialEq, Eq, Hash)]
	pub struct ModulePath {
		pub relative: bool,
		pub path: Vec<Identifier>,
	}

	impl ModulePath {
		pub fn new(relative: bool, path: Vec<Identifier>) -> Self {
			ModulePath { relative, path }
		}
	}

	#[derive(Debug, Clone, PartialEq, Eq)]
	pub struct SyntaxTree {
		pub uses: Vec<PathItem<Identifier>>,
		pub types: Vec<Type>,
		pub functions: Vec<Function>,
		pub extern_fns: Vec<ExternFunction>,
		pub modules: Vec<(Identifier, Module)>,
	}

	#[derive(Debug, Clone, PartialEq, Eq)]
	pub struct ExternFunction {
		pub name: Identifier,
		pub args: Vec<(Identifier, PathItem<TypeName>)>,
		pub return_type: Option<PathItem<TypeName>>,
	}

	impl SyntaxTree {
		pub fn new(
			uses: Vec<PathItem<Identifier>>,
			types: Vec<Type>,
			functions: Vec<Function>,
			extern_fns: Vec<ExternFunction>,
			modules: Vec<(Identifier, Module)>,
		) -> Self {
			SyntaxTree {
				uses,
				types,
				functions,
				extern_fns,
				modules,
			}
		}
	}

	/// A list of statements and an optional return expression
	/// The output is the value that will be returned from the entire
	/// block if a value is set to the syntax tree
	#[derive(Debug, Clone, PartialEq, Eq, Default)]
	pub struct Block {
		pub block: Vec<Expression>, // TODO rename
		pub output: Option<Expression>,
	}

	/// An assignment
	/// Contains the left hand side
	#[derive(Debug, Clone, PartialEq, Eq)]
	pub struct Assignment {
		pub ident: Identifier,
		pub expr: Expression,
	}

	/// A function definition
	#[derive(Debug, Clone, PartialEq, Eq)]
	pub struct Function {
		pub name: Identifier,
		pub args: Vec<(Identifier, PathItem<TypeName>)>,
		pub return_type: Option<PathItem<TypeName>>,
		pub body: Block,
	}

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

	#[derive(Debug, Clone, PartialEq, Eq)]
	pub struct If {
		pub condition: Expression,
		pub body: Expression,
		pub elif: Option<Box<If>>,
		pub else_block: Option<Expression>,
	}

	#[derive(Debug, Clone, PartialEq, Eq)]
	pub struct Type {
		pub name: TypeName,
		pub members: Vec<(Identifier, PathItem<TypeName>)>,
	}

	/// A let binding. Contains the identifier being bound to, the
	// type of the binding, the expression being bound, and whether is mutable
	#[derive(Debug, Clone, PartialEq, Eq)]
	pub struct Binding {
		pub mutable: bool,
		pub ident: Identifier,
		pub bind_type: Option<PathItem<TypeName>>,
		pub val: Option<Expression>,
	}

	impl Block {
		pub fn new() -> Self {
			Block {
				block: Vec::new(),
				output: None,
			}
		}

		pub fn traverse_expressions_mut<F: FnMut(&mut Expression)>(&mut self, f: &mut F) {
			for expression in &mut self.block {
				expression.traverse_expressions_mut(f);
			}
			if let Some(ref mut expr) = self.output {
				expr.traverse_expressions_mut(f)
			};
		}
	}
}

mod errors {
	use super::*;
	/// An error that occurs while parsing
	#[derive(Debug, Clone, PartialEq, Eq)]
	pub enum ParseError {
		UnexpectedToken,
		Expected(Vec<Expected>),
		LoopWithOutput,
	}

	#[derive(Debug, Clone, PartialEq, Eq)]
	pub enum Expected {
		Identifier,
		Semicolon,
		Colon,
		Typename,
		Block,
		Brackets,
		Parentheses,
		Keyword(Keyword),
		Symbol(TokenSymbol),
		Expression,
		Operator,
		ModulePath,
	}

	impl ::std::fmt::Display for ParseError {
		fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
			write!(
				f,
				"{}",
				match *self {
					ParseError::UnexpectedToken => "Unexpected token".to_owned(),
					ParseError::Expected(ref expected) => {
						let mut items_str = "Expected one of: ".to_owned();
						if expected.len() > 1 {
							for (i, item) in expected.iter().enumerate() {
								items_str.push_str(&item.to_string());
								if i < expected.len() - 2 {
									items_str.push_str(", ");
								} else if i == expected.len() - 2 {
									items_str.push_str(", or ");
								}
							}
						} else {
							items_str = format!("Expected: {}", &expected[0].to_string());
						}
						items_str
					},
					ParseError::LoopWithOutput => "Loop blocks cannot have an output".to_owned(),
				}
			)
		}
	}

	impl ::std::fmt::Display for Expected {
		fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
			write!(
				f,
				"{}",
				match *self {
					Expected::Identifier => "identifier".to_owned(),
					Expected::Semicolon => "semicolon".to_owned(),
					Expected::Colon => "colon".to_owned(),
					Expected::Typename => "type name".to_owned(),
					Expected::Block => "block".to_owned(),
					Expected::Brackets => "brackets".to_owned(),
					Expected::Parentheses => "parentheses".to_owned(),
					Expected::Keyword(keyword) => keyword.to_string(),
					Expected::Symbol(symbol) => symbol.to_string(),
					Expected::Expression => "expression".to_owned(),
					Expected::Operator => "operator".to_owned(),
					Expected::ModulePath => "module path".to_owned(),
				}
			)
		}
	}

	impl ::std::error::Error for ParseError {}
}

/// Gets statements until it can't from the tokens, and then gets a final expression if there is one
/// TODO don't return option
pub fn parse_block(mut tokens: &[TokenTree]) -> Result<Block, Error<ParseError>> {
	let mut block = Block::new();

	// Parse each statement until none are left
	while let Some((stmnt, leftovers)) = next_statement(tokens)? {
		block.block.push(stmnt);
		tokens = leftovers;
	}
	// Parse the final expression if there is one
	if let Some((expr, leftovers)) = next_expression(tokens, Box::new(|_| false))? {
		if !leftovers.is_empty() {
			return Err(ParseError::UnexpectedToken.into()); // There were tokens after the final expression which there shouldn't be
		}
		block.output = Some(expr);
	}

	Ok(block)
}

/// Gets the next statement requiring a terminating semicolon
pub fn next_statement(in_tokens: &[TokenTree]) -> ParseResult<Expression> {
	if let Some((expr, leftovers)) = next_expression(in_tokens, Box::new(|token| token.is_semicolon()))? {
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
) -> ParseResult<'a, Expression> {
	if in_tokens.is_empty() {
		return Ok(None);
	}

	if let Some((expression, leftovers)) = operation::OperationTaker::new().take_expression(in_tokens, end_predicate)? {
		return Ok(Some((expression, leftovers)));
	}

	Err(ParseError::UnexpectedToken.into()) // Could not parse the next expression
}
