extern crate backtrace;
#[macro_use]
extern crate log;

use std::path::Path;

pub mod ast;
pub mod codegen;
pub mod failure;

pub fn leafc<P: AsRef<Path>>(path: P) {
	let _ast = ast::create_ast(path);
}
