#![feature(nll)]

use std::path::Path;

pub mod ast;

pub fn leafc<P: AsRef<Path>>(path: P) {
	let _ast = ast::create_ast(path);
}