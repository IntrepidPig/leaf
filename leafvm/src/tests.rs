use std::fs;
use std::collections::HashMap;

use vm::{LeafFn, Primitive, Var, VarInfo, VM};

fn test(name: &str, externs: HashMap<String, Box<LeafFn>>) {
	let tests_path = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/programs/");
	let mut test_path = tests_path.to_owned();
	test_path.push_str(name);
	test_path.push_str(".lfb");
	let mut test_file = fs::File::open(&test_path).unwrap();
	let bin = ::binary::parse::read_binary(&mut test_file).unwrap();
	::binary::parse::print_bin(&bin);
	let mut vm = VM::new(bin, externs);
	let result = vm.run(true).unwrap();
	assert_eq!(result, Var::new_bool(true));
}

#[test]
fn basic() {
	let externs: HashMap<String, Box<LeafFn>> = HashMap::new();
	test("basic", externs);
}

#[test]
fn factorial() {
	let externs: HashMap<String, Box<LeafFn>> = HashMap::new();
	test("factorial", externs);
}

#[test]
fn fibonacci() {
	let externs: HashMap<String, Box<LeafFn>> = HashMap::new();
	test("fibonacci", externs);
}

#[test]
fn functions() {
	let externs: HashMap<String, Box<LeafFn>> = HashMap::new();
	test("functions", externs);
}

#[test]
fn modules() {
	let externs: HashMap<String, Box<LeafFn>> = HashMap::new();
	test("modules", externs);
}

#[test]
fn externs() {
	let mut externs: HashMap<String, Box<LeafFn>> = HashMap::new();
	externs.insert(
		"::print_int".to_owned(),
		Box::new(|args: Vec<Var>| -> Var {
			if let VarInfo::Primitive(Primitive::U64(val)) = args[0].var_info {
				println!("{}", val);
			} else {
				panic!("Expected an integer")
			}
			Var::root()
		}),
	);
	externs.insert(
		"::flip_bool".to_owned(),
		Box::new(|args: Vec<Var>| -> Var {
			if let VarInfo::Primitive(Primitive::Bool(val)) = args[0].var_info {
				Var::new_bool(!val)
			} else {
				panic!("Expected a boolean")
			}
		}),
	);
	test("externs", externs);
}
