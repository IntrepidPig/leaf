extern crate leafc;

static INPUT_TEST: &'static str = 
r###"
let a = 21;
{
	let b = 360;
	debug b;
}
let c = 2;
debug a;
debug c;
"###;

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
	println!("{:?}\n\t=>", ast);

	let mut code_generator = leafc::codegen::vmgen::CodeGenerator::new();
	let instructions = code_generator.gen_instructions(ast);
	println!("{:#?}", instructions);
}