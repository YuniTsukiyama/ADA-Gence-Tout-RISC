# Assembly syntax

The syntax for AGTR assembly is inspired from AT&T syntax.

The syntax is quite rigid due to the desired simplicity of the emulator.

Here are some sample instructions:

```
main:
    mov %A, $1
    load %R, -8(%BP)
```

- Labels are lowercase letters;
- Labels are followed by a colon;
- Instructions are suffixed by a 4 spaces indentation;
- Instruction mnemonics are lower case;
- Registers reference are prefixed by a `%`;
- Immediate values are prefixed by a `$`;
- Address dereferencing is performed by using parentheses around the value;
- Is is possible to add an offset before dereferencing an address.
