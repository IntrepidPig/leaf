use std::fs;

use vm::Var;

fn test(name: &str) {
	let tests_path = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/programs/");
	let mut test_path = tests_path.to_owned();
	test_path.push_str(name);
	test_path.push_str(".lfb");
	let mut test_file = fs::File::open(&test_path).unwrap();
	let instructions = ::instruction::parse::read_instructions(&mut test_file).unwrap();
	::instruction::parse::print_instructions(&instructions);
	let result = ::vm::run_instructions(&instructions, true).unwrap();
	assert_eq!(result, Var::new_bool(true));
}

#[test]
fn basic() {
	test("basic");
}

#[test]
fn factorial() {
	test("factorial");
}

#[test]
fn fibonacci() {
	test("fibonacci");
}

#[test]
fn functions() {
	test("functions");
}

#[test]
fn modules() {
	test("modules");
}
