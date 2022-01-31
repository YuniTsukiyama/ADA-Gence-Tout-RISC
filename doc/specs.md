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

* PUSH imm8/reg

Push the content of `reg` onto the stack. Decrements the SP accordingly.

* POP reg

Pop a value from the stack and store it in `reg`. Increments the SP accordingly.

* LOAD reg1, imm8/reg2

Load a value from memory at address `imm8/reg2` and store it in `reg1`.

* STORE imm8/reg1, imm8/reg2

Store the content of `imm8/reg2` in the memory at address `imm8/reg1`.

* JMP label/reg

Jumps at the address of `label` or the address stored in `reg`.

* JMPZ label/reg

Jumps at the address of `label` or the address stored in `reg` if ZERO flag is
set.

* EXIT

Stop the program execution. The return value of the program is the value stored
in the register %R.

* SYSCALL

Invoke the syscall referenced by the value stored in the register A. More
information in the *Syscalls Table* section.

## Registers

A  : General Purpose Register 1 - Argument 1
B  : General Purpose Register 2 - Argument 2
C  : General Purpose Register 3 - Argument 3
D  : General Purpose Register 4 - Argument 4
R  : General Purpose Register 5 - Return value
SP : Stack pointer
BP : Base pointer
IP : Instruction Pointer
F  : Flags register

## Flags register

Flags can be modified by the following instructions:

- ADD
- SUB
- CMP
- AND
- OR
- NOR

The flags register is organized as follow: `.....OZN'.

Each of the following flags is unset when the following conditions does not
apply.

* O: OVERFLOW

The OVERFLOW flag is set when the result of an operation is too large to fit in
the register width.

* Z: ZERO

The zero flag is set when an operation results in a zero value.

* N: NEGATIVE

The NEGATIVE flag is set when an operation results in a negative value.

## Syscalls Table

Syscalls are invoked using the SYSCALL instruction. The instruction looks for
the syscall using the value stored in register A. Each arguments of the syscall
shall be retrieved from the general purpose registers in the same order. Extra
arguments shall be retrieved from the stack.

Here is the list of all implemented syscalls:

* 1: Write

  Arguments:
    - FD  : File Descriptor
    - Addr: Buffer Address
    - Size: Buffer Size

  Description:
    Write **Size** characters from the buffer at the address **Addr** in the
    file referenced by **FD**.

  Return Value:
    None

* 2: Read

  Arguments:
    - FD  : File Descriptor
    - Addr: Buffer Address
    - Size: Buffer Size

  Description:
    Read **Size** characters from the file referenced by **FD** into the buffer
    at the address **Addr**.

  Return Value:
    None

* 3: Open

  Arguments:
    - Addr: File Name Address
    - Mode: Opening Mode

  Description:
    Open the file specified by **Addr**.

    Modes:
      - 1: Create a file and Write to it
      - 2: Read from an existing file
      - 3: Write to an existing file
      - 4: Append to an existing file
    There is no Read/Write mode.

    When creating a file, if the file already exists, it will be overwrited.

  Return Value:
    File Descriptor referencing the opened file.

* 4: Close

  Arguments:
    - FD: File Descriptor


  Description:
    Close the file descriptor **FD**.

  Return Value:
    None

## File Descriptors

There are 256 file descriptors available numbered from 0 to 255.

Three file descriptors are opened by default and cannot be closed:
- 0: Standard Input
- 1: Standard Output
- 2: Standard Error
