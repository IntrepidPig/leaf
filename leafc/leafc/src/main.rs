extern crate clap;
extern crate fern;
extern crate leafc;
extern crate leafvm;
extern crate log;

use std::fs;
use std::path::Path;
use std::io::{self, Read, Write};

fn main() {
	let matches = clap::App::new("Leafc")
		.author("IntrepidPig")
		.about("Leaf compiler")
		.arg(
			clap::Arg::with_name("debug")
				.short("d")
				.long("debug")
				.help("Print debugging information"),
		)
		.arg(
			clap::Arg::with_name("includes")
				.short("I")
				.long("include")
				.help("Includes another leaf source file as a module available in the current file")
				.takes_value(true)
				.multiple(true),
		)
		.arg(
			clap::Arg::with_name("FILE")
				.index(1)
				.takes_value(true)
				.required(true)
				.value_name("FILE")
				.help("The leaf file to compile. '-' for stdin"),
		)
		.arg(
			clap::Arg::with_name("OUTPUT")
				.index(2)
				.takes_value(true)
				.required(true)
				.value_name("OUTPUT")
				.help("The name of the leaf bytecode binary file to output. '-' for stdout"),
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
			log::LevelFilter::Warn
		})
		.chain(std::io::stderr())
		.apply()
		.unwrap_or_else(|e| eprintln!("Failed to initialize the logger.\n{}", e));

	let includes: Vec<&Path> = if let Some(includes) = matches.values_of_os("includes") {
		includes
			.into_iter()
			.map(|include| Path::new(include))
			.collect()
	} else {
		Vec::new()
	};
	let input_file = matches.value_of_os("FILE").unwrap();
	let output_file = matches.value_of_os("OUTPUT").unwrap();

	let lir_result = if input_file == "-" {
		let mut input = String::new();
		io::stdin().read_to_string(&mut input).unwrap();
		leafc::leafc_str(
			&input,
			Path::new("/usr/local/lib/leaf/libcore/core.leaf"),
			&includes,
		)
	} else {
		leafc::leafc(
			Path::new(input_file),
			Path::new("/usr/local/lib/leaf/libcore/core.leaf"),
			&includes,
		)
	};

	let lir = match lir_result {
		Ok(lir) => lir,
		Err(e) => {
			eprintln!("Errors occurred during compilation.\n{}", e);
			std::process::exit(1);
		},
	};

	let mut output_file: Box<Write> = if output_file == "-" {
		Box::new(io::stdout())
	} else {
		match fs::File::create(output_file) {
			Ok(f) => Box::new(f),
			Err(e) => {
				eprintln!("Failed to open the output file for writing.\n{}", e);
				std::process::exit(2);
			},
		}
	};

	if debug {
		leafvm::binary::parse::print_instructions(&lir.instructions);
	}

	match leafc::codegen::output::serialize_lir_bin(&lir, &mut output_file) {
		Ok(_) => {},
		Err(e) => {
			eprintln!(
				"An error occurred while writing the compiled output to a file.\n{}",
				e
			);
			std::process::exit(3);
		},
	};
}
