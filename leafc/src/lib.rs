extern crate backtrace;
extern crate leafvm;
#[macro_use]
extern crate log;

use std::path::Path;
use std::fmt;

pub mod ast;
pub mod codegen;
pub mod failure;
pub mod hir;
#[cfg(test)]
mod tests;

use codegen::vmgen::LIR;
use ast::AstCreationError;

#[derive(Debug)]
pub enum CompileError {
	AstCreationError(AstCreationError),
	// TODO HIR gen error and lir gen error
}

impl fmt::Display for CompileError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(
			f,
			"{}",
			match *self {
				CompileError::AstCreationError(ref err) => err.to_string(),
			}
		)
	}
}

impl From<AstCreationError> for CompileError {
	fn from(t: AstCreationError) -> Self {
		CompileError::AstCreationError(t)
	}
}

impl ::std::error::Error for CompileError {}

pub fn leafc_str<P: AsRef<Path>>(
	input: &str,
	core_path: P,
	includes: &[P],
) -> Result<LIR, failure::Error<CompileError>> {
	let mut includes: Vec<(String, &Path)> = {
		includes
			.iter()
			.map(|include_path| {
				let include_path = include_path.as_ref();
				let include_file_name = include_path.file_name().unwrap().to_str().unwrap();
				let include_name = include_file_name.split('.').next().unwrap();
				(include_name.to_owned(), include_path)
			})
			.collect()
	};
	includes.push(("core".to_owned(), core_path.as_ref()));
	let ast = ast::create_ast_with_includes(input, &includes).map_err(failure::transfer_bt)?;
	let mut hir_generator = hir::HIRGenerator::new();
	let hir = hir_generator.ast_to_hir(ast);
	let mut code_generator = codegen::vmgen::CodeGenerator::new(&hir);
	Ok(code_generator.gen_lir())
}

pub fn leafc<P: AsRef<Path>>(path: P, core_path: P, includes: &[P]) -> Result<LIR, failure::Error<CompileError>> {
	let mut includes: Vec<(String, &Path)> = {
		includes
			.iter()
			.map(|include_path| {
				let include_path = include_path.as_ref();
				let include_file_name = include_path.file_name().unwrap().to_str().unwrap();
				let include_name = include_file_name.split('.').next().unwrap();
				(include_name.to_owned(), include_path)
			})
			.collect()
	};
	includes.push(("core".to_owned(), core_path.as_ref()));
	let ast = ast::create_ast_from_file(path, &includes).map_err(failure::transfer_bt)?;
	let mut hir_generator = hir::HIRGenerator::new();
	let hir = hir_generator.ast_to_hir(ast);
	let mut code_generator = codegen::vmgen::CodeGenerator::new(&hir);
	Ok(code_generator.gen_lir())
}
