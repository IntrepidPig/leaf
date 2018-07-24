extern crate backtrace;
#[macro_use]
extern crate log;

use std::path::Path;

pub mod ast;
pub mod codegen;
pub mod failure;
pub mod hir;
#[cfg(test)]
mod tests;

use codegen::vmgen::Instruction;

pub fn leafc_str<P: AsRef<Path>>(
	input: &str,
	core_path: P,
	includes: &[P],
) -> Result<Vec<Instruction>, failure::GenericError> {
	let mut includes: Vec<(String, &Path)> = {
		includes
			.iter()
			.map(|include_path| {
				let include_path = include_path.as_ref();
				let include_file_name = include_path.file_name().unwrap().to_str().unwrap();
				let include_name = include_file_name.split(".").next().unwrap();
				(include_name.to_owned(), include_path)
			})
			.collect()
	};
	includes.push(("core".to_owned(), core_path.as_ref()));
	let ast = ast::create_ast_with_includes(input, &includes).unwrap();
	let mut hir_generator = hir::HIRGenerator::new();
	let hir = hir_generator.ast_to_hir(ast);
	let mut code_generator = codegen::vmgen::CodeGenerator::new(&hir);
	code_generator.gen_instructions();
	Ok(code_generator.instructions)
}

pub fn leafc<P: AsRef<Path>>(
	path: P,
	core_path: P,
	includes: &[P],
) -> Result<Vec<Instruction>, failure::GenericError> {
	let mut includes: Vec<(String, &Path)> = {
		includes
			.iter()
			.map(|include_path| {
				let include_path = include_path.as_ref();
				let include_file_name = include_path.file_name().unwrap().to_str().unwrap();
				let include_name = include_file_name.split(".").next().unwrap();
				(include_name.to_owned(), include_path)
			})
			.collect()
	};
	includes.push(("core".to_owned(), core_path.as_ref()));
	let ast = ast::create_ast_from_file(path, &includes).unwrap();
	let mut hir_generator = hir::HIRGenerator::new();
	let hir = hir_generator.ast_to_hir(ast);
	let mut code_generator = codegen::vmgen::CodeGenerator::new(&hir);
	code_generator.gen_instructions();
	Ok(code_generator.instructions)
}
