//! Code related to generation of HIR (High-level Intermediate Representation). Use HIRGenerator to
//! generate HIR from an AST.

use std::collections::HashMap;
use ast::parser::{self, SyntaxTree, BinaryOp, PrefixOp, PostfixOp, Identifier, ModulePath, PathItem, TypeName};

/// A function definition
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDefinition {
	/// The name of the function
	pub name: Identifier,
	/// The named arguments of the function
	pub args: Vec<(Identifier, PathItem<TypeName>)>,
	/// The return type of the function
	pub return_type: PathItem<TypeName>,
	/// The body of the function. Currently it is only possible for it to be a block but in the future
	/// other expressions may be valid function bodies as well.
	pub body: Expression,
}

/// A function call (not a method call)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionCall {
	/// The path to the function being called
	pub name: PathItem<Identifier>,
	/// The arguments passed to the function
	pub args: Vec<Expression>,
}

impl PathItem<TypeName> {
	/// Gets a PathItem that refers to `::core::Root`
	pub fn root() -> Self {
		PathItem {
			module_path: ModulePath {
				relative: false,
				path: vec![Identifier::from_string("core".to_string())],
			},
			item: TypeName::from_ident(Identifier::from_string("Root".to_owned()))
		}
	}
	
	/// Checks if the `PathItem` refers to a primitive type in `::core`
	/// Returns true for:
	/// - `::core::Int`
	/// - `::core::Bool`
	/// - `::core::Root`
	pub fn is_primitive(&self) -> bool {
		let int = PathItem {
			module_path: ModulePath {
				relative: false,
				path: vec![Identifier::from_string("core".to_string())],
			},
			item: TypeName::from_ident(Identifier::from_string("Int".to_owned()))
		};
		
		let boolean = PathItem {
			module_path: ModulePath {
				relative: false,
				path: vec![Identifier::from_string("core".to_string())],
			},
			item: TypeName::from_ident(Identifier::from_string("Bool".to_owned()))
		};
		
		let root = PathItem {
			module_path: ModulePath {
				relative: false,
				path: vec![Identifier::from_string("core".to_string())],
			},
			item: TypeName::from_ident(Identifier::from_string("Root".to_owned()))
		};
		
		match self {
			_ if *self == root => true,
			_ if *self == int => true,
			_ if *self == boolean => true,
			_ => false,
		}
	}
	
	/// Returns a PathItem that refers to a type in `::core`
	fn core_type(name: &str) -> PathItem<TypeName> {
		PathItem {
			module_path: ModulePath {
				relative: false,
				path: vec![Identifier::from_string("core".to_string())],
			},
			item: TypeName::from_ident(Identifier::from_string(name.to_owned()))
		}
	}
}

impl<T> PathItem<T> {
	/// Returns a `PathItem` that refers to the specified element in the root
	pub fn root_item(t: T) -> Self {
		PathItem {
			module_path: ModulePath {
				relative: false,
				path: Vec::new(),
			},
			item: t,
		}
	}
}

/// A type definition
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDefinition {
	pub name: TypeName,
	pub fields: Vec<(Identifier, PathItem<TypeName>)>,
}

impl From<parser::Type> for TypeDefinition {
	fn from(t: parser::Type) -> Self {
		TypeDefinition {
			name: t.name,
			fields: t.members,
		}
	}
}

/// An expression. Has the expression itself, as well as the return type of the expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expression {
	pub expr: ExpressionType,
	pub expr_out: PathItem<TypeName>,
}

impl Expression {
	/// Returns an expression that evaluates to the `Root` primitive
	pub fn root() -> Self {
		Expression {
			expr: ExpressionType::Root,
			expr_out: PathItem::<TypeName>::root(),
		}
	}
}

/// A type of expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionType {
	/// The root type
	Root,
	/// A binary operation
	Binary(Box<Binary>),
	/// A prefix operation
	Prefix(Box<Prefix>),
	/// A postfix operation
	Postfix(Box<Postfix>),
	/// A function call
	FunctionCall(FunctionCall),
	/// A debug expression
	Debug(Box<Expression>),
	/// A break expression with an optional value
	Break(Option<Box<Expression>>),
	/// A variable binding
	Binding(Box<Binding>),
	/// A (re)assignment
	Assignment(Box<Assignment>),
	/// A loop expression
	Loop(Box<Expression>),
	/// An if statement
	If(Box<If>),
	/// An identifier (variable name)
	Identifier(Identifier),
	/// A string literal
	StringLiteral(String),
	/// An integer literal
	IntLiteral(u64),
	/// A boolean literal
	BoolLiteral(bool),
	/// A block of expressions and an optional output
	Block(Box<Block>),
	/// A field access of a type. Contains the base expression and the field being accessed respectively
	FieldAccess(Box<Expression>, Identifier),
	/// An instantiation of a type
	Instantiation(Instantiation),
}

/// An instantiation of a type
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instantiation {
	/// The type being instantiated
	pub typename: PathItem<TypeName>,
	/// The fields given to the type
	pub fields: Vec<(Identifier, Expression)>
}

/// An if expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct If {
	/// The condition of the if expression
	pub condition: Expression,
	/// The body of the if expression
	pub body: Expression,
	pub elif: Option<Box<If>>,
	/// The body of the else block
	pub else_block: Option<Expression>,
}

/// A (re)assignment
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assignment {
	/// The variable being assigned to
	pub ident: Identifier,
	/// The value to assign to the variable
	pub expr: Expression,
}

/// A binary operation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binary {
	/// The left hand side
	pub left: Expression,
	/// The right hand side
	pub right: Expression,
	/// The operator
	pub op: BinaryOp,
}

/// A prefix operation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Prefix {
	/// The operand (on the right of the operator)
	pub right: Expression,
	/// The operator
	pub op: PrefixOp,
}

/// A postfix operation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Postfix {
	/// The left hand side (on the left of the operator)
	pub left: Expression,
	/// The operator
	pub op: PostfixOp,
}

/// A binding
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding {
	/// The name of the binding
	pub ident: Identifier,
	/// THe mutability of the binding
	pub mutable: bool,
	/// The type of the variable being bound
	pub bind_type: PathItem<TypeName>,
	/// The expression the variable is being bound to
	pub expr: Option<Expression>,
}

/// A block expression
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
	/// The statements in the block
	pub statements: Vec<Expression>,
	/// The optional output of the expression (will be `Root` otherwise)
	pub output: Option<Expression>,
}

/// A module of code. Contains type definitions, function definitions, and more modules.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
	/// The modules within this modules
	pub modules: Vec<(Identifier, Module)>,
	/// The type definitions within this module
	pub types: Vec<TypeDefinition>,
	/// The function definitions within this module
	pub functions: Vec<FunctionDefinition>,
}

impl Module {
	/// Traverse this module and the modules within it mutably. Performs `f` on each module including
	/// the current one.
	pub fn traverse_mut<F: FnMut(&ModulePath, &mut Module)>(&mut self, f: &mut F, start_path: &mut ModulePath) {
		f(start_path, self);
		for (name, module) in &mut self.modules {
			start_path.path.push(name.clone());
			module.traverse_mut(f, start_path);
			start_path.path.pop().unwrap();
		}
	}
	
	/// Traverse this module and the modules within it immutably. Performs `f` on each module including
	/// the current one.
	pub fn traverse<F: FnMut(&ModulePath, &Module)>(&self, f: &mut F, start_path: &mut ModulePath) {
		f(start_path, &self);
		for (name, module) in &self.modules {
			start_path.path.push(name.clone());
			module.traverse(f, start_path);
			start_path.path.pop().unwrap();
		}
	}
}

/// A High-level Intermediate Representation of a Leaf program. HIR will have a type for all expressions that
/// is the type they output, and it will have all reference to items that can be in seperate modules resolved
/// to absolute paths from the root of the crate. Also, operators on non primitive types will be converted to
/// using the associated methods of the types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HIR {
	pub modules: Vec<Module>,
}

/// A struct that will generate HIR from an AST, resolving all `PathItem`s to absolute ones and giving
/// all expressions a type in the process.
pub struct HIRGenerator {
	/// A map of all types in the application referenced by their absolute module path to avoid conflict
	types: HashMap<PathItem<TypeName>, TypeDefinition>,
	/// A map of the return types of all function definitions referenced by their absolute module path to
	/// avoid conflict.
	function_defs_returns: HashMap<PathItem<Identifier>, PathItem<TypeName>>,
	/// A list of maps of currently available variable bindings.
	// Is currently implemented as a stack to allow for nested function definitions, but that isn't
	// supported by the parser yet.
	bindings_stack: Vec<HashMap<Identifier, PathItem<TypeName>>>,
}

impl HIRGenerator {
	/// Creates a new `HIRGenerator`
	pub fn new() -> Self {
		HIRGenerator {
			types: HashMap::new(),
			function_defs_returns: HashMap::new(),
			bindings_stack: Vec::new(),
		}
	}
	
	/// Convert a `SyntaxTree` to `HIR`. Make sure that the `SyntaxTree` has all the modules it
	/// references in it's list of modules.
	pub fn ast_to_hir(&mut self, ast: SyntaxTree) -> HIR {
		// Create the root module from the SyntaxTree given.
		// TODO change this to not require the name root
		let mut root_module = parser::Module {
			body: ast,
		};
		
		// Resolve all paths of the syntax tree to absolute paths with this module
		// as the root.
		self.resolve_paths(&mut root_module, &mut ModulePath::new(false, Vec::new()));
		
		HIR {
			modules: vec![self.ast_module_to_hir_module(&root_module, &mut ModulePath::new(false, Vec::new()))]
		}
	}
	
	/// Resolve all paths in a module to absolute paths with this module as a root.
	pub fn resolve_paths(&mut self, module: &mut parser::Module, module_path: &mut ModulePath) {
		// Only does anything if `to_resolve` isn't already relative.
		// If to_resolve is in the uses, change it to that. If not, then it must be declared in this module so
		// append it's path to the current module path.
		fn resolve_path<T: Eq + Clone>(current: &ModulePath, uses: &Vec<PathItem<T>>, to_resolve: &mut PathItem<T>) {
			if to_resolve.module_path.relative {
				for u in uses {
					if u.item == to_resolve.item {
						*to_resolve = u.clone();
						return;
					}
				}
				
				// The type is relative and not used, it must be part of this module
				let mut new_path = current.clone();
				new_path.path.append(&mut to_resolve.module_path.path);
				to_resolve.module_path = new_path;
			}
		}
		
		// Resolve all paths to types and functions to absolute paths
		module.traverse_mut(&mut |current_path: &ModulePath, module: &mut parser::Module| {
			let types = &mut module.body.types;
			let functions = &mut module.body.functions;
			let uses = &module.body.uses;
				
			// Resolve all the paths in each function of this module
			for function in functions {
				// Resolve the types of the arguments of the function
				for (_arg_name, arg_type) in &mut function.args {
					let mut arg_type_name = arg_type.clone().map(|arg_type_name| arg_type_name.name);
					resolve_path(&current_path, uses, &mut arg_type_name);
					arg_type.module_path = arg_type_name.module_path;
				}
				
				// Resolve the return type of the function
				
				function.return_type.as_mut().map(|return_type| {
					let mut arg_type_name = return_type.clone().map(|arg_type_name| arg_type_name.name);
					resolve_path(&current_path, uses, &mut arg_type_name);
					return_type.module_path = arg_type_name.module_path;
				});
				
				// Resolve all of the function calls and instantiations in this function
				function.body.traverse_expressions_mut(&mut |expr: &mut parser::Expression| {
					match expr {
						parser::Expression::Instantiation(ref mut name, _) => {
							let mut arg_type_name = name.clone().map(|arg_type_name| arg_type_name.name);
							resolve_path(&current_path, uses, &mut arg_type_name);
							name.module_path = arg_type_name.module_path;
						},
						parser::Expression::FunctionCall { ref mut name, .. } => {
							resolve_path(&current_path, uses, name);
						},
						_ => {},
					}
				});
			}
			
			// Resolve all of the types of fields in each type definition of this module
			for typedef in types {
				for (_member_name, member_type) in &mut typedef.members {
					let mut arg_type_name = member_type.clone().map(|arg_type_name| arg_type_name.name);
					resolve_path(&current_path, uses, &mut arg_type_name);
					member_type.module_path = arg_type_name.module_path;
				}
			}
		}, module_path);
	}
	
	/// Convert an AST module to an HIR module. Assumes all paths are resolved as absolute, and will fail
	/// if they aren't.
	pub fn ast_module_to_hir_module(&mut self, module: &parser::Module, module_path: &mut ModulePath) -> Module {		
		// Add all type definitions to the global list of type definitions with it's full path
		// Add all functions to the global list of function return types with it's full path
		module.traverse(&mut |current_path: &ModulePath, module: &parser::Module| {
			// Typedefs
			for typedef in &module.body.types {
				self.types.insert({
					PathItem {
						module_path: current_path.clone(),
						item: typedef.name.clone(),
					}
				}, typedef.clone().into());
			}
			
			// Functions
			for function in &module.body.functions {
				self.function_defs_returns.insert({
					PathItem {
						module_path: current_path.clone(),
						item: function.name.clone(),
					}
				}, function.return_type.clone().unwrap_or(PathItem::<TypeName>::root()));
			}
		}, module_path);
		
		// Convert all AST function definitions to HIR function definitions
		let mut functiondefs = Vec::new();
		module.traverse(&mut |_: &ModulePath, module: &parser::Module| {
			for function in &module.body.functions {
				// TODO! account for blocks
				// Push a new list of bindings on the binding stack
				self.bindings_stack.push(HashMap::new());
				let name = function.name.clone();
				
				// Add the function arguments to the binding list
				let args = function.args.clone();
				for arg in &args {
					self.bindings_stack.last_mut().unwrap().insert(arg.0.clone(), arg.1.clone());
				}
				
				// Get the return type and ensure it matches the block output, or just use the block output as the return type
				let expr = self.block_to_expression(&function.body);
				let body = Expression {
					expr_out: self.get_expr_type(&expr),
					expr,
				};
				let return_type = match (&function.return_type, &body.expr_out) {
					(None, b_return) => {
						// TODO fail if b_return isn't root
						b_return.clone()
					},
					(Some(f_return), b_return) => {
						if f_return == b_return {
							b_return.clone()
						} else {
							panic!("Mismatched return types")
						}
					},
				};
				
				functiondefs.push({
					FunctionDefinition {
						name,
						args,
						body,
						return_type,
					}
				});
				self.bindings_stack.pop();
			}
		}, module_path);
		
		// Put all typedefs in global typedef list
		let mut typedefs = Vec::new();
		module.traverse(&mut |_module_path: &ModulePath, module: &parser::Module| { 
			let mut mod_typedefs: Vec<TypeDefinition> = module.body.types.iter().map(|typedef| typedef.clone().into()).collect();
			typedefs.append(&mut mod_typedefs);
		}, module_path);
		
		// Convert all the modules within to HIR modules recursively
		let mut modules = Vec::new();
		for (name, module) in &module.body.modules {
			module_path.path.push(name.clone());
			modules.push((name.clone(), self.ast_module_to_hir_module(module, module_path)));
			module_path.path.pop().unwrap();
		}
		
		Module {
			modules,
			types: typedefs,
			functions: functiondefs,
		}
	}
	
	// Convert an ast expression to an HIR expression
	pub fn ast_expr_to_hir_expr(&mut self, expr: &parser::Expression) -> Expression {
		let expr = self.ast_expr_to_exprtype(expr);
		Expression {
			expr_out: self.get_expr_type(&expr),
			expr,
		}
	}
	
	// TODO rename ExpressionType to ExpressionKind
	// Convert an ast expression to an HIR ExpressionType
	pub fn ast_expr_to_exprtype(&mut self, expr: &parser::Expression) -> ExpressionType {
		match expr {
			parser::Expression::Block(ref block) => {
				self.block_to_expression(block)
			},
			parser::Expression::Binding(ref binding) => {
				let val = binding.val.clone().expect("Uninitialized bindings not supported");
				let expr = self.ast_expr_to_hir_expr(&val);
				self.bindings_stack.last_mut().unwrap().insert(binding.ident.clone(), expr.expr_out.clone());
				
				ExpressionType::Binding(Box::new(Binding {
					ident: binding.ident.clone(),
					mutable: binding.mutable,
					bind_type: expr.expr_out.clone(),
					expr: Some(expr),
				}))
			},
			parser::Expression::Root => ExpressionType::Root,
			parser::Expression::NumberLiteral(val) => ExpressionType::IntLiteral(*val),
			parser::Expression::Identifier(ref ident) => ExpressionType::Identifier(ident.clone()),
			parser::Expression::Binary { left, right, op } => {
				let left = self.ast_expr_to_hir_expr(left);
				let right = self.ast_expr_to_hir_expr(right);
				if left.expr_out.is_primitive() && right.expr_out.is_primitive() {
					ExpressionType::Binary(Box::new(Binary {
						left,
						right,
						op: *op,
					}))
				} else {
					println!("{:?}\n{:?}\n{:?}", left, right, op);
					// TODO operation traits
					unimplemented!()
				}
			},
			parser::Expression::Prefix { right, op } => {
				ExpressionType::Prefix(Box::new(Prefix {
					right: self.ast_expr_to_hir_expr(right),
					op: *op,
				}))
			},
			parser::Expression::Postfix { left, op } => {
				ExpressionType::Postfix(Box::new(Postfix {
					left: self.ast_expr_to_hir_expr(left),
					op: *op,
				}))
			},
			parser::Expression::FunctionCall { name, args } => {
				ExpressionType::FunctionCall(FunctionCall {
					name: name.clone(),
					args: args.iter().map(|arg| self.ast_expr_to_hir_expr(arg)).collect()
				})
			},
			parser::Expression::Debug(ref expr) => {
				ExpressionType::Debug(Box::new(self.ast_expr_to_hir_expr(expr)))
			},
			parser::Expression::Break(ref expr_opt) => {
				ExpressionType::Break(expr_opt.as_ref().map(|expr| Box::new(self.ast_expr_to_hir_expr(expr.as_ref()))))
			},
			parser::Expression::Assign(ref assignment) => {
				ExpressionType::Assignment(Box::new(Assignment {
					ident: assignment.ident.clone(),
					expr: self.ast_expr_to_hir_expr(&assignment.expr)
				}))
			},
			parser::Expression::Loop(ref expr) => {
				ExpressionType::Loop(Box::new(self.ast_expr_to_hir_expr(expr)))
			},
			parser::Expression::If(ref if_expr) => {
				ExpressionType::If(Box::new(If {
					condition: self.ast_expr_to_hir_expr(&if_expr.condition),
					body: self.ast_expr_to_hir_expr(&if_expr.body),
					elif: None,
					else_block: if_expr.else_block.as_ref().map(|expr| self.ast_expr_to_hir_expr(expr))
				}))
			},
			parser::Expression::StringLiteral(ref string) => {
				ExpressionType::StringLiteral(string.clone())
			},
			parser::Expression::BoolLiteral(ref b) => {
				ExpressionType::BoolLiteral(*b)
			},
			parser::Expression::Instantiation(ref name, ref fields) => {
				ExpressionType::Instantiation(Instantiation {
					typename: name.clone(),
					fields: fields.iter().map(|field| (field.0.clone(), self.ast_expr_to_hir_expr(&field.1))).collect()
				})
			},
			parser::Expression::FieldAccess(ref base, ref field) => {
				ExpressionType::FieldAccess(Box::new(self.ast_expr_to_hir_expr(base)), field.clone())
			},
		}
	}
	
	pub fn get_expr_type(&mut self, expr: &ExpressionType) -> PathItem<TypeName> {
		match expr {
			ExpressionType::Block(ref block) => block.output.as_ref().map(|expr| expr.expr_out.clone()).unwrap_or(PathItem::<TypeName>::root()),
			ExpressionType::Root => PathItem::<TypeName>::root(),
			ExpressionType::Binding(_) => PathItem::<TypeName>::root(),
			ExpressionType::IntLiteral(_) => PathItem::<TypeName>::core_type("Int"),
			ExpressionType::Identifier(ref ident) => {
				let mut bind_type = None;
				for bind_frame in &self.bindings_stack {
					if let Some(test_bind_type) = bind_frame.get(ident) {
						bind_type = Some(test_bind_type.clone());
						break;
					}
					
					panic!("Variable '{:?}' not bound", ident)
				}
				bind_type.unwrap()
			},
			ExpressionType::Prefix(ref _prefix) => {
				unimplemented!()
			},
			ExpressionType::Postfix(ref _postfix) => {
				unimplemented!()
			},
			ExpressionType::Binary(ref binary) => {
				// TODO actually get type of desugared function call
				binary.left.expr_out.clone()
			},
			ExpressionType::BoolLiteral(_) => {
				PathItem::<TypeName>::core_type("Bool")
			},
			ExpressionType::Assignment(ref assignment) => {
				self.get_expr_type(&ExpressionType::Identifier(assignment.ident.clone()))
			},
			ExpressionType::Break(ref break_expr) => {
				break_expr.clone().map(|expr| expr.expr_out).unwrap_or(PathItem::<TypeName>::root())
			},
			ExpressionType::Debug(_) => PathItem::<TypeName>::root(),
			ExpressionType::FunctionCall(ref call) => {
				self.function_defs_returns.get(&call.name).cloned().unwrap()
			},
			ExpressionType::Loop(ref expr) => {
				let mut break_type: Option<PathItem<TypeName>> = None;
				expr.traverse(&mut |test_expr: &Expression| {
					// TODO handle break labels and nested loops
					match test_expr.expr {
						ExpressionType::Break(ref break_expr) => {
							let test_break_type = break_expr.as_ref().map(|expr| expr.expr_out.clone()).unwrap_or(PathItem::<TypeName>::root()).clone();
							if let Some(old_break_type) = break_type.clone() {
								if old_break_type != test_break_type {
									panic!("Multiple break types in loop")
								}
							} else {
								break_type = Some(test_break_type)
							}
						},
						_ => {},
					}
				});
				
				// TODO change this to diverging like rusts !
				break_type.unwrap_or(PathItem::<TypeName>::root())
			},
			ExpressionType::If(ref if_expr) => {
				let body_type = if_expr.body.expr_out.clone();
				if let Some(else_block) = &if_expr.else_block {
					if body_type == else_block.expr_out.clone() {
						body_type
					} else {
						panic!("If blocks return different types")
					}
				} else {
					body_type
				}
				// TODO check elifs
			},
			ExpressionType::StringLiteral(_) => PathItem::<TypeName>::core_type("String"),
			ExpressionType::FieldAccess(ref base, ref ident) => {
				// Find the type of the base in the global types list
				let typedef = self.types.get(&base.expr_out).expect("No type with that name found");
				
				// Find the field referred to and return it's type
				let field = typedef.fields.iter().find(|field| ident == &field.0).expect("Type does not have that field");
				field.1.clone()
			},
			ExpressionType::Instantiation(ref instantiation) => {
				instantiation.typename.clone()
			},
		}
	}
	
	pub fn block_to_expression(&mut self, block: &parser::Block) -> ExpressionType {
		let mut statements = Vec::new();
		for statement in &block.block {
			statements.push(self.ast_expr_to_hir_expr(statement));
		}
		
		let out = block.output.as_ref().map(|out| self.ast_expr_to_hir_expr(out));
		
		ExpressionType::Block(Box::new(Block {
			statements: statements,
			output: out
		}))
	}
}

impl Expression {
	pub fn traverse<F: FnMut(&Expression)>(&self, f: &mut F) {
		f(self);
		match &self.expr {
			ExpressionType::Root => {
				// no action necessary
			},
			ExpressionType::Binary(ref binary) => {
				binary.left.traverse(f);
				binary.right.traverse(f);
			},
			ExpressionType::Prefix(ref prefix) => {
				prefix.right.traverse(f);
			},
			ExpressionType::Postfix(ref postfix) => {
				postfix.left.traverse(f);
			},
			ExpressionType::FunctionCall(ref call) => {
				for arg in &call.args {
					f(arg);
				}
			},
			ExpressionType::Debug(ref expr) => {
				f(expr);
			},
			ExpressionType::Break(ref expr_opt) => {
				expr_opt.as_ref().map(|expr| f(expr));
			},
			ExpressionType::Binding(ref binding) => {
				binding.expr.as_ref().map(|expr| f(expr));
			},
			ExpressionType::Assignment(ref assignment) => {
				assignment.expr.traverse(f);
			},
			ExpressionType::Loop(ref expr) => {
				expr.traverse(f);
			},
			ExpressionType::If(ref if_expr) => {
				if_expr.condition.traverse(f);
				if_expr.body.traverse(f);
				// TODO elif
				if_expr.else_block.as_ref().map(|expr| expr.traverse(f));
			},
			ExpressionType::Identifier(_) => {
				// no action necesssary
			},
			ExpressionType::StringLiteral(_) => {
				// no action necesssary
			},
			ExpressionType::IntLiteral(_) => {
				// no action necesssary
			},
			ExpressionType::BoolLiteral(_) => {
				// no action necesssary
			},
			ExpressionType::Block(ref block) => {
				for statement in &block.statements {
					statement.traverse(f);
				}
				block.output.as_ref().map(|expr| expr.traverse(f));
			},
			ExpressionType::FieldAccess(ref base, _) => {
				base.traverse(f);
			},
			ExpressionType::Instantiation(ref instantiation) => {
				for (_, expr) in &instantiation.fields {
					expr.traverse(f);
				}
			},
		}
	}
}