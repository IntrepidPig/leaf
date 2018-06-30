extern crate leaf;
extern crate leafc;

use std::io::{self, Read};

use leafc::codegen::vmgen::{Instruction, Value};
use std::collections::HashMap;

fn main() {
	let mut input = String::new();
	io::stdin().read_to_string(&mut input).unwrap();
	println!("'\n{}\n'\n\t=>", input);
	let lexed = leafc::ast::lexer::lex(&input).unwrap();
	println!("{:?}\n\t=>", lexed);
	let mut tokenizer = leafc::ast::tokenizer::Tokenizer::new(lexed);
	let tokens = tokenizer.tokenize().unwrap();
	println!("{:?}\n\t=>", tokens);
	let ast = leafc::ast::parser::parse(tokens.tokens.as_slice()).unwrap();
	println!("{:?}\n\t=>", ast);
	let code_generator = leafc::codegen::vmgen::CodeGenerator::new();
	let instructions = code_generator.gen_instructions(ast);
	println!("{:?}", instructions);
	run_instructions(&instructions).unwrap();
}

fn run_instructions(instructions: &[Instruction]) -> Result<(), ()> {
	let mut ptr: usize = 0;
	let mut stack: Vec<Vec<Value>> = vec![Vec::new()];
	let mut vars: HashMap<String, usize> = HashMap::new();

	let mut instr_ptr: usize = 0;
	let mut iter: usize = 0;
	let debug = true;

	loop {
		if instr_ptr == instructions.len() {
			break;
		}

		if debug {
			iter += 1;
			if iter > 50 {
				println!("Reached maximum instruction execution");
				return Ok(());
			}

			println!(
				"Instruction {}: {:?}",
				instr_ptr,
				instructions[instr_ptr],
			);
		}

		match instructions[instr_ptr] {
			Instruction::Bind(ref ident) => {
				vars.insert(ident.clone(), ptr);
			},
			Instruction::Push(ref value) => {
				stack.last_mut().unwrap().push(value.clone());
				ptr += 1;
			},
			Instruction::Pop => {
				stack.last_mut().unwrap().pop().unwrap();
				ptr -= 1;
			},
			Instruction::Load(ref ident) => {
				let val = deref_stack(&stack, *vars.get(ident).unwrap());
				stack.last_mut().unwrap().push(val);
				ptr += 1;
			},
			Instruction::Debug => {
				let val = stack.last_mut().unwrap().pop().unwrap();
				println!("\t => Value: {}", val.val);
				ptr -= 1;
			},
			Instruction::Frame => {
				stack.push(Vec::new());
			},
			Instruction::Exit => {
				ptr -= stack.last().unwrap().len();
				stack.pop().unwrap();
				// TODO drop all items in stack
			},
			Instruction::Set(ref ident) => {
				let val = stack.last_mut().unwrap().pop().unwrap();
				*deref_stack_mut(&mut stack, *vars.get(ident).unwrap()) = val;
				ptr -= 1;
			},
			Instruction::Return => {
				let val = stack.last_mut().unwrap().pop().unwrap();
				let len = stack.len();
				stack[len - 2].push(val);
			},
			Instruction::Add => {
				let right = stack.last_mut().unwrap().pop().unwrap();
				let left = stack.last_mut().unwrap().pop().unwrap();
				let output = left.val + right.val;
				stack.last_mut().unwrap().push(Value { val: output });
				ptr -= 1;
			},
			Instruction::Jump(ptr) => {
				instr_ptr = ptr;
				continue;
			}
		}

		instr_ptr += 1;

		if debug {
			println!(
				"Stack: {:?}\n\
				Vars: {:?}\n",
				stack,
				vars
			);
		}
	}

	println!(
		"Program output: {}",
		stack.pop().unwrap().pop().unwrap().val
	);

	Ok(())
}

fn deref_stack(stack: &Vec<Vec<Value>>, mut ptr: usize) -> Value {
	for frame in stack {
		for var in frame {
			ptr -= 1;
			if ptr == 0 {
				return var.clone();
			}
		}
	}

	panic!("Index not found")
}

fn deref_stack_mut(stack: &mut Vec<Vec<Value>>, mut ptr: usize) -> &mut Value {
	for frame in stack.iter_mut() {
		for var in frame.iter_mut() {
			ptr -= 1;
			if ptr == 0 {
				return var;
			}
		}
	}

	panic!("Index not found")
}
