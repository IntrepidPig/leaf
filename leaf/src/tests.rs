extern crate leafc;

use std::path::Path;

fn test_file<P: AsRef<Path>>(file: P) {
	let instructions = leafc::leafc(file.as_ref(), Path::new("../leafc/src/libcore/core.leaf"), &[]).unwrap();
	let res = ::run_instructions(&instructions, false).unwrap();
	println!("Program output: {:?}", res);
	assert!(!res.is_false());
}

#[test]
fn basic() {
	test_file("tests/programs/basic.leaf");
}

#[test]
fn factorial() {
	test_file("tests/programs/factorial.leaf");
}

#[test]
fn fibonacci() {
	test_file("tests/programs/fibonacci.leaf");
}

#[test]
fn functions() {
	test_file("tests/programs/functions.leaf");
}