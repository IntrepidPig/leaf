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

Loop:
	-> gen instructions for each statement
	-> If a statement is a break:
		-> If it has a value:
			-> Gen instructions for the expression
			Return the value (Return)
			Jump to the instruction after the jump back to beginning instruction (Jump)
		-> If not:
			Push a nil value (Push)
			Return it (Return)
			Jump to the instruction after the jump back to beginning instruction (Jump)
	-> If not
		-> Generate the normal instructions
	Jump back to the beginning of the loop (Jump)
	Exit the stack frame (Exit)

If statement:
	-> Gen instructions for the condition expression
	Push instruction that jumps to the end of the if statement if it's false (Check)
	-> Gen instructions for the block inside the if statement

## TODO: Errors

The current error handling system for the compiler sucks. It returns a ParseError::Other 99% of the time. The plan is to give each token an ID, as well as a range of
lexemes it refers to. Then each error will be able to reference a token, and in turn, a set of lexemes, and so errors will be much more helpful. This will require a major refactoring though.