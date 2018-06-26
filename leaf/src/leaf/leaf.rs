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
	let mut code_generator = leafc::codegen::vmgen::CodeGenerator::new();
	let instructions = code_generator.gen_instructions(ast);
	println!("{:?}", instructions);
	run_instructions(&instructions).unwrap();
}

fn run_instructions(instructions: &[Instruction]) -> Result<(), ()> {
	let mut stack: Vec<Value> = Vec::new();
	let mut vars: HashMap<String, Value> = HashMap::new();

	for instruction in instructions {
		match instruction {
			Instruction::Bind(ref ident) => {
				vars.insert(ident.clone(), stack.pop().unwrap());
			},
			Instruction::Push(ref value) => {
				stack.push(value.clone());
			},
			Instruction::Pop => {
				stack.pop().unwrap();
			},
			Instruction::PushVar(ref ident) => {
				stack.push(vars.get(ident).unwrap().clone());
			},
			Instruction::Debug => {
				let val = stack.pop().unwrap();
				println!("Value: {}", val.val);
			},
			Instruction::Drop(ref ident) => {
				vars.remove(ident);
			}
		}
	}

	Ok(())
}