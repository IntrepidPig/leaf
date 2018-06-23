extern crate leafc;

static INPUT_TEST: &'static str = 
r###"fn true {
	let a_num = 246;
	let this: &str = "so true";
	print(*this);
}
"###;

fn main() {
	let input = INPUT_TEST;
	let lexemes = leafc::ast::lexer::lex(input).unwrap();

	let de_lexed = &lexemes.de_lex();
	println!("'{}'\n\t=>\n{:?}\n\t=>", input, lexemes);
	assert_eq!(input, de_lexed);

	let mut tokenizer = leafc::ast::tokenizer::Tokenizer::new(lexemes);
	let tokens = tokenizer.tokenize();
	
	println!("{:?}", tokens);
}