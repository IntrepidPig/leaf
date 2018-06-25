extern crate leafc;

static INPUT_TEST: &'static str = 
r###"let d = 1;{{{let qwrty_ok = 5;} let x = 3;}} let c = 4;"###;

fn main() {
	let input = INPUT_TEST;
	let lexemes = leafc::ast::lexer::lex(input).unwrap();

	let de_lexed = &lexemes.de_lex();
	println!("'{}'\n\t=>\n{:?}\n\t=>", input, lexemes);
	assert_eq!(input, de_lexed);

	let mut tokenizer = leafc::ast::tokenizer::Tokenizer::new(lexemes);
	let tokens = tokenizer.tokenize().unwrap();
	
	println!("{:?}\n\t=>", tokens);

	let ast = leafc::ast::parser::parse(tokens.tokens.as_slice()).unwrap();
	println!("{:#?}", ast);
}