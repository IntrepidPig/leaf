use std::collections::HashMap;

use ast::parser::{self, SyntaxTree, BinaryOp, PrefixOp, PostfixOp};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDefinition {
	pub name: String,
	pub args: Vec<(String, TypeIdentifier)>,
	pub return_type: TypeIdentifier,
	pub body: Expression, // should always be block?
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionCall {
	pub name: String,
	pub args: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeIdentifier {
	pub name: String,
}

impl TypeIdentifier {
	pub fn root() -> Self {
		TypeIdentifier {
			name: "Root".to_string(),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDefinition {
	pub name: String,
	pub fields: Vec<(String, TypeIdentifier)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expression {
	pub expr: ExpressionType,
	pub expr_out: TypeIdentifier,
}

impl Expression {
	pub fn root() -> Self {
		Expression {
			expr: ExpressionType::Root,
			expr_out: TypeIdentifier::root(),
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
	Identifier(String),
	StringLiteral(String),
	IntLiteral(u64),
	BoolLiteral(bool),
	Block(Box<Block>),
	FieldAccess(Box<Expression>, String),
	Instantiation(Instantiation),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instantiation {
	pub typename: TypeIdentifier,
	pub fields: Vec<(String, Expression)>
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
	pub ident: String,
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
	pub ident: String,
	pub mutable: bool,
	pub bind_type: TypeIdentifier,
	pub expr: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
	pub statements: Vec<Expression>,
	pub output: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path {
	pub modules: Vec<String>,
	pub item: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
	pub modules: Vec<Module>,
	pub types: Vec<TypeDefinition>,
	pub functions: Vec<FunctionDefinition>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HIR {
	pub modules: Vec<Module>,
}

pub struct HIRGenerator {
	types: HashMap<String, TypeDefinition>,
	function_defs_returns: HashMap<String, TypeIdentifier>,
	bindings_stack: Vec<HashMap<String, TypeIdentifier>>,
}

impl HIRGenerator {
	pub fn new() -> Self {
		HIRGenerator {
			types: HashMap::new(),
			function_defs_returns: HashMap::new(),
			bindings_stack: Vec::new(),
		}
	}
	
	pub fn ast_to_hir(&mut self, ast: &SyntaxTree) -> HIR {
		HIR {
			modules: vec![self.ast_to_module(ast)]
		}
	}
	
	pub fn ast_to_module(&mut self, ast: &SyntaxTree) -> Module {
		let mut typedefs = Vec::new();
		for typedef in &ast.types {
			typedefs.push(TypeDefinition {
				name: typedef.name.clone(),
				fields: typedef.members.iter().map(|(fieldname, typename)| (fieldname.clone(), TypeIdentifier {
					name: typename.clone()
				})).collect()
			})
		}
		for typedef in &typedefs {
			self.types.insert(typedef.name.clone(), typedef.clone());
		}
		
		for function in &ast.functions {
			self.function_defs_returns.insert(function.name.clone(), function.return_type.clone().map(|name| TypeIdentifier {
				name,
			}).unwrap_or(TypeIdentifier::root()));
		}
		
		let mut functiondefs = Vec::new();
		for function in &ast.functions {
			self.bindings_stack.push(HashMap::new());
			let name = function.name.clone();
			let args: Vec<(String, TypeIdentifier)> = function.args.iter().map(|(name, typename)| (name.clone(), TypeIdentifier {
				name: typename.clone(),
			})).collect();
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
					if f_return == &b_return.name {
						TypeIdentifier {
							name: f_return.clone()
						}
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
		
		Module {
			modules: Vec::new(), // TODO
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
			parser::Expression::Unit => ExpressionType::Root,
			parser::Expression::NumberLiteral(val) => ExpressionType::IntLiteral(*val),
			parser::Expression::Identifier(ref ident) => ExpressionType::Identifier(ident.clone()),
			parser::Expression::Binary { left, right, op } => {
				ExpressionType::Binary(Box::new(Binary {
					left: self.ast_expr_to_hir_expr(left),
					right: self.ast_expr_to_hir_expr(right),
					op: *op,
				}))
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
					typename: TypeIdentifier {
						name: name.clone(),
					},
					fields: fields.iter().map(|field| (field.0.clone(), self.ast_expr_to_hir_expr(&field.1))).collect()
				})
			},
			parser::Expression::FieldAccess(ref base, ref field) => {
				ExpressionType::FieldAccess(Box::new(self.ast_expr_to_hir_expr(base)), field.clone())
			},
		}
	}
	
	pub fn get_expr_type(&mut self, expr: &ExpressionType) -> TypeIdentifier {
		match expr {
			ExpressionType::Block(ref block) => block.output.as_ref().map(|expr| expr.expr_out.clone()).unwrap_or(TypeIdentifier::root()),
			ExpressionType::Root => TypeIdentifier::root(),
			ExpressionType::Binding(_) => TypeIdentifier::root(),
			ExpressionType::IntLiteral(_) => TypeIdentifier {
				name: "Int".to_string(),
			},
			ExpressionType::Identifier(ref ident) => {
				let mut bind_type = None;
				for bind_frame in &self.bindings_stack {
					if let Some(test_bind_type) = bind_frame.get(ident) {
						bind_type = Some(test_bind_type.clone());
						break;
					}
					
					panic!("Variable '{}' not bound", ident)
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
				TypeIdentifier {
					name: "Bool".to_string(),
				}
			},
			ExpressionType::Assignment(ref assignment) => {
				self.get_expr_type(&ExpressionType::Identifier(assignment.ident.clone()))
			},
			ExpressionType::Break(ref break_expr) => {
				break_expr.clone().map(|expr| expr.expr_out).unwrap_or(TypeIdentifier::root())
			},
			ExpressionType::Debug(_) => TypeIdentifier::root(),
			ExpressionType::FunctionCall(ref call) => {
				self.function_defs_returns.get(&call.name).cloned().unwrap()
			},
			ExpressionType::Loop(ref expr) => {
				let mut break_type: Option<TypeIdentifier> = None;
				traverse_expressions(expr, &mut |test_expr: &Expression| {
					// TODO handle break labels and nested loops
					match test_expr.expr {
						ExpressionType::Break(ref break_expr) => {
							let test_break_type = break_expr.as_ref().map(|expr| expr.expr_out.clone()).unwrap_or(TypeIdentifier::root()).clone();
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
				break_type.unwrap_or(TypeIdentifier::root())
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
			ExpressionType::StringLiteral(_) => TypeIdentifier {
				name: "String".to_string(),
			},
			ExpressionType::FieldAccess(ref base, ref ident) => {
				let typedef = self.types.get(&base.expr_out.name).expect("No type with that name found");
				
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

pub fn traverse_expressions<F: FnMut(&Expression)>(expression: &Expression, f: &mut F) {
	f(expression);
	match &expression.expr {
		ExpressionType::Root => {
			// no action necessary
		},
		ExpressionType::Binary(ref binary) => {
			f(&binary.left);
			f(&binary.right);
		},
		ExpressionType::Prefix(ref prefix) => {
			f(&prefix.right);
		},
		ExpressionType::Postfix(ref postfix) => {
			f(&postfix.left);
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
			f(&assignment.expr);
		},
		ExpressionType::Loop(ref expr) => {
			f(expr);
		},
		ExpressionType::If(ref if_expr) => {
			f(&if_expr.condition);
			f(&if_expr.body);
			// TODO elif
			if_expr.else_block.as_ref().map(|expr| f(expr));
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
				f(statement);
			}
			block.output.as_ref().map(|expr| f(expr));
		},
		ExpressionType::FieldAccess(ref base, _) => {
			f(base);
		},
		ExpressionType::Instantiation(ref instantiation) => {
			for (_, expr) in &instantiation.fields {
				f(expr);
			}
		},
	}
}