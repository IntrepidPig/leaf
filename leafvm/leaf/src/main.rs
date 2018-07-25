extern crate clap;
extern crate fern;
extern crate leafc;
extern crate leafvm;
extern crate log;

use std::fs;
use std::io::{self, Read};

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
			clap::Arg::with_name("FILE")
				.takes_value(true)
				.required(true)
				.value_name("FILE")
				.help("The leaf binary file to run. '-' for stdin"),
		)
		.get_matches();

	let debug = matches.is_present("debug");
	let input_file = matches.value_of_os("FILE").unwrap();

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
			log::LevelFilter::Warn
		})
		.chain(std::io::stderr())
		.apply()
		.expect("Failed to initialize logger");

	let instruction_input: Box<Read> = if input_file == "-" {
		Box::new(io::stdin())
	} else {
		Box::new(fs::File::open(input_file).unwrap())
	};

	let instructions = leafvm::instruction::read_instructions(instruction_input);

	if debug {
		leafvm::instruction::print_instructions(&instructions);
	}

	leafvm::vm::run_instructions(&instructions, debug).unwrap();
}
