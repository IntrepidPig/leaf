extern crate clap;
extern crate fern;
extern crate leaf;
extern crate leafc;
extern crate log;

use std::io::{self, Read};

use leafc::codegen::vmgen::{Instruction, Var, VarInfo, Primitive};
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackFrame {
	locals: Vec<Var>,
	block_frames: Vec<BlockFrame>,
}

impl StackFrame {
	pub fn new() -> Self {
		StackFrame {
			locals: Vec::new(),
			block_frames: Vec::new(),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockFrame {
	operands: Vec<Var>,
}

impl BlockFrame {
	pub fn new() -> Self {
		BlockFrame {
			operands: Vec::new(),
		}
	}
}

fn run_instructions(instructions: &[Instruction], debug: bool) -> Result<(), ()> {
	let mut ptr: usize = 0;
	let mut stack: Vec<StackFrame> = Vec::new();

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
			Instruction::Block => {
				stack.last_mut().unwrap().block_frames.push(BlockFrame::new());
			},
			Instruction::Bind => {
				let var = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				stack.last_mut().unwrap().locals.push(var);
				ptr -= 1;
			},
			Instruction::Push(ref var) => {
				stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.push(var.clone());
				ptr += 1;
			},
			Instruction::Pop => {
				stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				ptr -= 1;
			},
			Instruction::Load(ref index) => {
				let var = stack.last_mut().unwrap().locals.get(*index).unwrap().clone();
				stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.push(var.clone());
				ptr += 1;
			},
			Instruction::Debug => {
				let var = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				println!("\t => Var: {:?}", var);
				ptr -= 1;
			},
			Instruction::Frame => {
				stack.push(StackFrame::new());
			},
			Instruction::Set(ref index) => {
				let var = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				*stack.last_mut().unwrap().locals.get_mut(*index).unwrap() = var;
				ptr -= 1;
			},
			Instruction::Output => {
				let var = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				let len = stack.last().unwrap().block_frames.len();
				stack.last_mut().unwrap().block_frames.get_mut(len - 2).unwrap().operands.push(var);
				ptr -= stack.last().unwrap().block_frames.last().unwrap().operands.len();
				stack.last_mut().unwrap().block_frames.pop().unwrap();
				// TODO drop all items in stack
			},
			Instruction::Return => {
				let var = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				let len = stack.len();
				stack.get_mut(len - 2).unwrap().block_frames.last_mut().unwrap().operands.push(var);
				for block_frame in &stack.last().unwrap().block_frames {
					ptr -= block_frame.operands.len()
				}
				stack.pop().unwrap();
			}
			Instruction::Add => {
				let right = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				let left = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				let output = match (left.var_info, right.var_info) {
					(VarInfo::Primitive(Primitive::U64(left)), VarInfo::Primitive(Primitive::U64(right))) => {
						left + right
					},
					_ => panic!("Tried to add types that don't support it")
				};
				stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.push(Var::new_u64(output));
				ptr -= 1;
			},
			Instruction::Equal => {
				let right = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				let left = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				let output = Var::new_bool(left == right);
				stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.push(output);
				ptr -= 1;
			},
			Instruction::Jump(target_instr_ptr) => {
				instr_ptr = target_instr_ptr;
				continue;
			},
			Instruction::Check(target_instr_ptr) => {
				let var = stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop().unwrap();
				ptr -= 1;
				// If the value is false jump past the if block
				if var.is_false() {
					instr_ptr = target_instr_ptr;
					continue;
				}
			},
		}

		instr_ptr += 1;

		if debug {
			println!(
				"Stack: {:?}\n\
				 Stack ptr: {:?}\n",
				stack, ptr,
			);
		}
	}

	println!(
		"Program output: {:?}",
		stack.last_mut().unwrap().block_frames.last_mut().unwrap().operands.pop()
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

fn deref_stack(stack: &Vec<Vec<Var>>, mut ptr: usize) -> Var {
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

fn deref_stack_mut(stack: &mut Vec<Vec<Var>>, mut ptr: usize) -> &mut Var {
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
