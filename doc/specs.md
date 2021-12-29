# Specifications

Specifications for Ada-Gence Tout RISC

AGTR is a 16bit processor emulator.

## Instruction Set

* MOV reg1, imm8/reg2

Copies the content of `imm8/reg2` into `reg1`.

* ADD reg1, imm8/reg2

Adds the contents of `reg1` and `imm8/reg2` and store the result in `reg1`.

* SUB reg1, imm8/reg2

Subtracts the contents of `reg1` and `imm8/reg2` and store the result in `reg1`.

* AND reg1, imm8/reg2

Performs a bitwise logical AND operation on the contents of `reg1` and
`imm8/reg2` and store the result in `reg1`.

* OR reg1, imm8/reg2

Performs a bitwise logical OR operation on the contents of `reg1` and
`imm8/reg2` and store the result in `reg1`.

* NOR reg1, imm8/reg2

Performs a bitwise logical NOR operation on the contents of `reg1` and
`imm8/reg2` and store the result in `reg1`.

* CMP reg1, imm8/reg2

Compares the contents of `reg1` and `imm8/reg2`. The operation is performed by
subtracting the second operand from the first operand and then setting the
status flags in the same manner as the SUB instruction.

* PUSH reg

Push the content of `reg` onto the stack. Decrements the SP accordingly.

* POP reg

Pop a value from the stack and store it in `reg`. Increments the SP accordingly.

* LOAD reg1, imm8/reg2

Load a value from memory at address `imm8/reg2` and store it in `reg1`.

* STORE imm8/reg1, reg2

Store the content of `reg2` in the memory at address `imm8/reg1`.

* JMPZ label/reg

Jumps at the address of `label` or the address stored in `reg` if ZERO flag is
set.

## Registers

A  : General Purpose Register 1 - Argument 1
B  : General Purpose Register 2 - Argument 2
C  : General Purpose Register 3 - Argument 3
D  : General Purpose Register 4 - Argument 4
R  : General Purpose Register 5 - Return value
SP : Stack pointer
BP : Base pointer
F  : Flags register

## Flags register

Each of the following flags is unset when the following conditions does not
apply.

* O: OVERFLOW

The OVERFLOW flag is set when the result of an operation is too large to fit in
the register width.

The flag can be modified by the following instructions:

- ADD
- SUB
- CMP

* Z: ZERO

The zero flag is set when an operation results in a zero value.

The flag can be modified by the following instructions:

- ADD
- SUB
- CMP
- AND
- OR
- NOR

* N: NEGATIVE

The NEGATIVE flag is set when an operation results in a negative value.

The flag can be modified by the following instructions:

- ADD
- SUB
- CMP
- AND
- OR
- NOR