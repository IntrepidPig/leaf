use std::fs;

fn compile(name: &str) {
	let tests_path = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/programs/");
	let vm_tests_path = concat!(env!("CARGO_MANIFEST_DIR"), "/../leafvm/tests/programs/");
	let mut test_path = tests_path.to_owned();
	test_path.push_str(name);
	test_path.push_str(".leaf");
	let instructions = ::leafc(
		test_path,
		"/usr/local/lib/leaf/libcore/core.leaf".to_owned(),
		&[],
	).unwrap();
	let mut vm_test_path = vm_tests_path.to_owned();
	vm_test_path.push_str(name);
	vm_test_path.push_str(".lfb");
	let mut out_file = fs::File::create(&vm_test_path).unwrap();
	::codegen::serialize_instructions(instructions, &mut out_file);
}

#[test]
fn basic() {
	compile("basic");
}

#[test]
fn factorial() {
	compile("factorial");
}

#[test]
fn fibonacci() {
	compile("fibonacci");
}

#[test]
fn functions() {
	compile("functions");
}

#[test]
fn modules() {
	compile("modules");
}
