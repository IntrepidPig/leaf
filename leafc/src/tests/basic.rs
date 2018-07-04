use std::path::Path;

use ast;
use codegen;

fn _load_file<P: AsRef<Path>>(_path: P) -> String {
	unimplemented!()
}

#[test]
fn basic_tests() {
	let tests = [
		"let a := 5;".to_owned(),
		"let a := 7".to_owned(),
		"let a := loop { break 3; };".to_owned(),
		"let a := if 1 then loop break 15; back + 10 else { 200 + if 0 then 250 else loop back end } + 500 end; a".to_owned(),
	];
	
	for test in tests.into_iter() {
		let lexed = ast::lexer::lex(&test).unwrap();
		let mut tokenizer = ast::tokenizer::Tokenizer::new(lexed);
		let tokens = tokenizer.tokenize().unwrap();
		let tokentree = ast::treeify::treeify(&tokens.tokens).unwrap();
		let ast = ast::parser::parse(tokentree.as_slice()).unwrap();
		let code_generator = codegen::vmgen::CodeGenerator::new();
		let _instructions = code_generator.gen_instructions(ast);
	}
}