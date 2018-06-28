## This document will describe how the compiler works

Block:
	New stack frame; (Frame)
	-> Gen instructions for each statement;
	-> If there's a final expression:
		-> Gen instructions for expression;
		Pop the top of the stack to the previous stack frame; (Return)
	Exit the stack frame; (Exit)

Let binding:
	-> Gen instructions for expression;
	Set binding name to refer to the current stack index; (Bind)

Expression:
	-> If Expression is a value:
		Push value onto stack; (Push)
	-> If the Expression is a Binary operation;
		-> Gen instructions for left Expression;
		-> Gen instructions for right Expression;
		-> Gen instruction for operation;
		Push result onto stack; (Push)
	-> If the Expression is a variable name:
		Copy the value from the index the binding refers to to the top of the stack; (Load)
	-> If the Expression is a block;
		-> Gen instructions for the block;

Expression Statement:
	-> Gen instructions for expression;
	Pop top value off the stack (into oblivion); (Pop)

Debug:
	-> Gen instructions for expression;
	Debug instructions; (Debug)

Assignment:
	-> Gen instructions for expression;
	Pop top of stack to stack location referred to binding name; (Set)
