use std::collections::HashMap;

use ast::parser::{self, SyntaxTree, BinaryOp, PrefixOp, PostfixOp, Identifier, ModulePath, PathItem, TypeName};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDefinition {
	pub name: Identifier,
	pub args: Vec<(Identifier, PathItem<TypeName>)>,
	pub return_type: PathItem<TypeName>,
	pub body: Expression, // should always be block?
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionCall {
	pub name: PathItem<Identifier>,
	pub args: Vec<Expression>,
}

impl PathItem<TypeName> {
	pub fn root() -> Self {
		PathItem {
			module_path: ModulePath {
				relative: false,
				path: vec![Identifier::from_str("root"), Identifier::from_string("core".to_string())],
			},
			item: TypeName::from_ident(Identifier::from_string("Root".to_owned()))
		}
	}
	
	pub fn is_primitive(&self) -> bool {
		let int = PathItem {
			module_path: ModulePath {
				relative: false,
				path: vec![Identifier::from_str("root"), Identifier::from_string("core".to_string())],
			},
			item: TypeName::from_ident(Identifier::from_string("Int".to_owned()))
		};
		
		let boolean = PathItem {
			module_path: ModulePath {
				relative: false,
				path: vec![Identifier::from_str("root"), Identifier::from_string("core".to_string())],
			},
			item: TypeName::from_ident(Identifier::from_string("Bool".to_owned()))
		};
		
		let root = PathItem {
			module_path: ModulePath {
				relative: false,
				path: vec![Identifier::from_str("root"), Identifier::from_string("core".to_string())],
			},
			item: TypeName::from_ident(Identifier::from_string("Root".to_owned()))
		};
		
		match self {
			root => true,
			int => true,
			boolean => true,
			_ => false,
		}
	}
	
	fn core_type(name: &str) -> PathItem<TypeName> {
		PathItem {
			module_path: ModulePath {
				relative: false,
				path: vec![Identifier::from_str("root"), Identifier::from_string("core".to_string())],
			},
			item: TypeName::from_ident(Identifier::from_string(name.to_owned()))
		}
	}
}

impl<T> PathItem<T> {
	pub fn root_item(t: T) -> Self {
		PathItem {
			module_path: ModulePath {
				relative: false,
				path: vec![Identifier::from_str("root")]
			},
			item: t,
		}
	}
}

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expression {
	pub expr: ExpressionType,
	pub expr_out: PathItem<TypeName>,
}

impl Expression {
	pub fn root() -> Self {
		Expression {
			expr: ExpressionType::Root,
			expr_out: PathItem::<TypeName>::root(),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpressionType {
	Root,
	Binary(Box<Binary>),
	Prefix(Box<Prefix>),
	Postfix(Box<Postfix>),
	FunctionCall(FunctionCall),
	Debug(Box<Expression>),
	Break(Option<Box<Expression>>),
	Binding(Box<Binding>),
	Assignment(Box<Assignment>),
	Loop(Box<Expression>),
	If(Box<If>),
	Identifier(Identifier),
	StringLiteral(String),
	IntLiteral(u64),
	BoolLiteral(bool),
	Block(Box<Block>),
	FieldAccess(Box<Expression>, Identifier),
	Instantiation(Instantiation),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instantiation {
	pub typename: PathItem<TypeName>,
	pub fields: Vec<(Identifier, Expression)>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct If {
	pub condition: Expression,
	pub body: Expression,
	pub elif: Option<Box<If>>,
	pub else_block: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Assignment {
	pub ident: Identifier,
	pub expr: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binary {
	pub left: Expression,
	pub right: Expression,
	pub op: BinaryOp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Prefix {
	pub right: Expression,
	pub op: PrefixOp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Postfix {
	pub left: Expression,
	pub op: PostfixOp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding {
	pub ident: Identifier,
	pub mutable: bool,
	pub bind_type: PathItem<TypeName>,
	pub expr: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
	pub statements: Vec<Expression>,
	pub output: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
	pub name: Identifier,
	pub modules: Vec<Module>,
	pub types: Vec<TypeDefinition>,
	pub functions: Vec<FunctionDefinition>,
}

impl Module {
	pub fn traverse_mut<F: FnMut(&mut Module)>(&mut self, f: &mut F) {
		f(self);
		for module in &mut self.modules {
			module.traverse_mut(f);
		}
	}
	
	pub fn traverse<F: FnMut(&Module)>(&self, f: &mut F) {
		f(&self);
		for module in &self.modules {
			module.traverse(f);
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HIR {
	pub modules: Vec<Module>,
}

pub struct HIRGenerator {
	types: HashMap<PathItem<TypeName>, TypeDefinition>,
	function_defs_returns: HashMap<PathItem<Identifier>, PathItem<TypeName>>,
	bindings_stack: Vec<HashMap<Identifier, PathItem<TypeName>>>,
}

impl HIRGenerator {
	pub fn new() -> Self {
		HIRGenerator {
			types: HashMap::new(),
			function_defs_returns: HashMap::new(),
			bindings_stack: Vec::new(),
		}
	}
	
	pub fn ast_to_hir(&mut self, ast: SyntaxTree) -> HIR {
		let mut root_module = parser::Module {
			name: Identifier::from_str("root"),
			body: ast,
		};
		
		self.resolve_paths(&mut root_module);
		
		HIR {
			modules: vec![self.ast_module_to_hir_module(&root_module)]
		}
	}
	
	pub fn resolve_paths(&mut self, module: &mut parser::Module) {
		fn resolve_path(current: &ModulePath, to_resolve: &mut ModulePath) {
			if to_resolve.relative {
				let mut new_path = current.clone();
				new_path.path.append(&mut to_resolve.path);
				to_resolve.relative = false;
				*to_resolve = new_path;
			}
		}
		
		let mut current_path = ModulePath::new(false, Vec::new());
		module.traverse_mut(&mut |module: &mut parser::Module| {
			current_path.path.push(module.name.clone());
			for function in &mut module.body.functions {
				for (arg_name, arg_type) in &mut function.args {
					resolve_path(&current_path, &mut arg_type.module_path);
				}
				
				function.return_type.as_mut().map(|return_type| resolve_path(&current_path, &mut return_type.module_path));
				
				function.body.traverse_expressions_mut(&mut |expr: &mut parser::Expression| {
					match expr {
						parser::Expression::Instantiation(ref mut name, _) => {
							resolve_path(&current_path, &mut name.module_path);
						},
						parser::Expression::FunctionCall { ref mut name, .. } => {
							resolve_path(&current_path, &mut name.module_path);
						},
						_ => {},
					}
				})
			}
			
			for typedef in &mut module.body.types {
				for (member_name, member_type) in &mut typedef.members {
					resolve_path(&current_path, &mut member_type.module_path);
				}
			}
			
			current_path.path.pop().unwrap();
		});
	}
	
	pub fn ast_module_to_hir_module(&mut self, module: &parser::Module) -> Module {
		let mut current_path = ModulePath::new(false, Vec::new());
		module.traverse(&mut |module: &parser::Module| {
			current_path.path.push(module.name.clone());
			
			for typedef in &module.body.types {
				self.types.insert({
					PathItem {
						module_path: current_path.clone(),
						item: typedef.name.clone(),
					}
				}, typedef.clone().into());
			}
			
			for function in &module.body.functions {
				self.function_defs_returns.insert({
					PathItem {
						module_path: current_path.clone(),
						item: function.name.clone(),
					}
				}, function.return_type.clone().unwrap_or(PathItem::<TypeName>::root()));
			}
			
			current_path.path.pop().unwrap();
		});
		
		let mut functiondefs = Vec::new();
		for function in &module.body.functions {
			self.bindings_stack.push(HashMap::new());
			let name = function.name.clone();
			let args: Vec<(Identifier, PathItem<TypeName>)> = function.args.iter().map(|(name, typename)| (name.clone(), typename.clone())).collect();
			for arg in &args {
				self.bindings_stack.last_mut().unwrap().insert(arg.0.clone(), arg.1.clone());
			}
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
		
		let typedefs = module.body.types.iter().map(|typedef| typedef.clone().into()).collect();
		
		let mut modules = Vec::new();
		for module in &module.body.modules {
			modules.push(self.ast_module_to_hir_module(module));
		}
		
		Module {
			name: module.name.clone(),
			modules,
			types: typedefs,
			functions: functiondefs,
		}
	}
	
	pub fn ast_expr_to_hir_expr(&mut self, expr: &parser::Expression) -> Expression {
		let expr = self.ast_expr_to_exprtype(expr);
		Expression {
			expr_out: self.get_expr_type(&expr),
			expr,
		}
	}
	
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
			ExpressionType::Prefix(ref prefix) => {
				unimplemented!()
			},
			ExpressionType::Postfix(ref postfix) => {
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
				let typedef = self.types.get(&base.expr_out).expect("No type with that name found");
				
				let field = typedef.fields.iter().find(|typedef| typedef == typedef).expect("Type does not have that field");
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