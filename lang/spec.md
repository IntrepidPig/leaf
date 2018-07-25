# VM Instructions Spec

## `Call <ptr> <argc>`
#### opcode: 2
#### operands: 2
#### description:
Jump to the instruction specified by `ptr`, and pop `argc` arguments off of the operand stack of the current stack frame and into the new stack frame, preserving their order.

## `Block`
#### opcode: 3
#### operands: 0
#### description:
Start a new block frame.

#### Note:
May be removed in the future

## `PushInt <val>`
#### opcode: 4
#### operands: 1
#### description:
Push an integer onto the operand stack

#### Notes:
The value type may need to be specified in the future

## `Output`
#### opcode: 5
#### operands: 0
#### description:
Pop the top value off the the top operand stack and push it to the previous operand stack. Drop the top operand stack.

#### Notes:
May be removed in the future

## `Bind`
#### opcode: 6
#### operands: 0
#### description:
Pop a value off the top of the operands stack and store it in the local variable stack

## `Load <ptr>`
#### opcode: 7
#### operands: 1
#### description:
Copy the value in the local variable stack of the current stack frame at the index of `ptr` into the current operand stack.

## `Set <ptr>`
#### opcode: 8
#### operands 1
#### description:
Pop a value off the operand stack and set the value of the local variable in the local variable stack of the current frame to that value.

## `Pop`
#### opcode: 9
#### operands: 0
#### description:
Pop a value off the top of the operands stack

## `Add`
#### opcode: 10
#### operands: 0
#### description:
Pop a two values off the operands stack and push their sum back

## `Sub`
#### opcode: 11
#### operands: 0
#### description:
Pop a two values off the operands stack and push their difference back

## `Mul`
#### opcode: 12
#### operands: 0
#### description:
Pop a two values off the operands stack and push their product back

## `Div`
#### opcode: 13
#### operands: 0
#### description:
Pop a two values off the operands stack and push their quotient back

## `Debug`
#### opcode: 14
#### operands: 0
#### description:
Pop a value off the top of the operand stack and print it in an implementation-specified manner

## `Jump <ptr>`
#### opcode: 15
#### operands: 1
#### description:
Set the current instruction pointer to `ptr`

## `Check <ptr>`
#### opcode: 16
#### operands: 1
#### description:
Pop the top of the stack and set the current instruction pointer to `ptr` if the value is falsy.

## `Equal`
#### opcode: 17
#### operands: 0
#### description:
Pop a two values off the operands stack and boolean back representing their equality

## `Retrieve <ptr>`
#### opcode: 18
#### operands: 1
#### description:
Pop a reference type off the top of the stack, and the indexed field of the reference type back.

## `Return`
#### opcode: 19
#### operands: 0
#### description:
Pop a value off the operand stack and push it to the operand stack of the previous stack frame. Drop the current stack frame.

## `Ref <size>`
#### opcode: 20
#### operands: 1
#### description:
Pop `size` values of the operand stack, retaining their order, and push a reference value back with the fields set to each value that was popped off of the stack in order.

## `Terminate`
#### opcode: 21
#### operands: 0
#### description:
Stop the execution of the VM

## `PushRoot`
#### opcode: 22
#### operands: 0
#### description:
Push a Root value onto the operands stack

## `PushBool <val>`
#### opcode: 22
#### operands: 1
#### description:
Push a boolean value onto the operands stack