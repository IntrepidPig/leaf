/// Instructions that don't break control flow
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LILInstruction {
	AddInt,
	PushInt(i32),
	PushBool(bool),
	PushRoot,
	Load(usize),
	Set(usize),
	Equal,
	Pop,
	Bind,
	Output,
	Debug,
}