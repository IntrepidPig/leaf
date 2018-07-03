extern crate clap;
extern crate fern;
extern crate leaf;
extern crate leafc;
extern crate log;

use std::io::{self, Read};

use leafc::codegen::vmgen::{Instruction, Value};
use std::collections::HashMap;

fn main() {
	let matches = clap::App::new("Leaf")
		.author("IntrepidPig")
		.about("Leaf bytecode VM")
		.arg(
			clap::Arg::with_name("debug")
				.short("d")
				.long("debug")
				.help("Print debugging info"),
		)
		.arg(
			clap::Arg::with_name("program")
				.short("p")
				.long("prog")
				.help("The program to be run")
				.takes_value(true),
		)
		.get_matches();

	let debug = matches.is_present("debug");

	fern::Dispatch::new()
		.format(|out, message, record| {
			if let (Some(file), Some(line)) = (record.file(), record.line()) {
				out.finish(format_args!(
					"{}:{} [{}] {}",
					file,
					line,
					record.level(),
					message
				))
			} else {
				out.finish(format_args!("[{}] {}", record.level(), message))
			}
		})
		.level(if debug {
			log::LevelFilter::Trace
		} else {
			log::LevelFilter::Info
		})
		.chain(std::io::stdout())
		.apply()
		.expect("Failed to initialize logger");

	let input = if let Some(program) = matches.value_of("program") {
		program.to_owned()
	} else {
		let mut input = String::new();
		io::stdin().read_to_string(&mut input).unwrap();
		input
	};
	println!("'\n{}\n'\n\t=>", input);
	let lexed = leafc::ast::lexer::lex(&input).unwrap();
	println!("{:?}\n\t=>", lexed);
	let mut tokenizer = leafc::ast::tokenizer::Tokenizer::new(lexed);
	let tokens = tokenizer.tokenize().unwrap();
	println!("{:?}\n\t=>", tokens);
	let tokentree = leafc::ast::treeify::treeify(&tokens.tokens).unwrap();
	println!("{:?}\n\t=>", tokentree);
	let ast = leafc::ast::parser::parse(tokentree.as_slice()).unwrap();
	println!("{:?}\n\t=>", ast);
	let code_generator = leafc::codegen::vmgen::CodeGenerator::new();
	let instructions = code_generator.gen_instructions(ast);
	print_instructions(&instructions);
	run_instructions(&instructions, debug).unwrap();
}

fn run_instructions(instructions: &[Instruction], debug: bool) -> Result<(), ()> {
	let mut ptr: usize = 0;
	let mut stack: Vec<Vec<Value>> = vec![Vec::new()];
	let mut vars: HashMap<String, usize> = HashMap::new();

	let mut instr_ptr: usize = 0;
	let mut iter: usize = 0;

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

			println!("Instruction {}: {:?}", instr_ptr, instructions[instr_ptr],);
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
			Instruction::Jump(target_instr_ptr) => {
				instr_ptr = target_instr_ptr;
				continue;
			},
			Instruction::Check(target_instr_ptr) => {
				let val = stack.last_mut().unwrap().pop().unwrap();
				ptr -= 1;
				// If the value is false jump past the if block
				if val.val == 0 {
					instr_ptr = target_instr_ptr;
					continue;
				}
			},
		}

		instr_ptr += 1;

		if debug {
			println!(
				"Stack: {:?}\n\
				 Vars: {:?}\n\
				 Stack ptr: {:?}\n",
				stack, vars, ptr,
			);
		}
	}

	println!(
		"Program output: {}",
		stack.pop().unwrap().pop().unwrap().val
	);

	Ok(())
}

fn print_instructions(instructions: &[Instruction]) {
	let max_length = instructions.len().to_string().len();
	for (i, instr) in instructions.iter().enumerate() {
		let i_str = i.to_string();
		for _ in 0..max_length - i_str.len() {
			print!(" ");
		}
		println!("{}: {:?}", i_str, instr);
	}
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
