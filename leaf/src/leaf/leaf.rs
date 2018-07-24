extern crate clap;
extern crate fern;
extern crate leaf;
extern crate leafc;
extern crate log;

use std::io::{self, Read};
use std::path::Path;

use leafc::codegen::vmgen::Instruction;

fn main() {
	let matches = clap::App::new("Leaf")
		.author("IntrepidPig")
		.about("Leaf bytecode VM")
		.arg(
			clap::Arg::with_name("debug")
				.short("d")
				.long("debug")
				.help("Print debugging info")
		)
		.arg(
			clap::Arg::with_name("includes")
				.short("I")
				.long("include")
				.help("Includes another leaf source file as a module available in the current file")
				.takes_value(true)
				.multiple(true)
		)
		.arg(
			clap::Arg::with_name("FILE")
				.takes_value(true)
				.required(true)
				.value_name("FILE")
				.help("The leaf file to compile. '-' for stdin")
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
	
	let includes: Vec<&Path> = if let Some(includes) = matches.values_of_os("includes") {
		includes.into_iter().map(|include| Path::new(include)).collect()
	} else {
		Vec::new()
	};
	let input_file = matches.value_of_os("FILE").unwrap();
	
	let instructions = if input_file == "-" {
		let mut input = String::new();
		io::stdin().read_to_string(&mut input).unwrap();
		leafc::leafc_str(&input, Path::new("../leafc/src/libcore/core.leaf"), &includes).unwrap()
	} else {
		leafc::leafc(Path::new(input_file), Path::new("../leafc/src/libcore/core.leaf"), &includes).unwrap()
	};
	
	if debug {
		print_instructions(&instructions);
	}
	leaf::run_instructions(&instructions, debug).unwrap();
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
