main:
    mov %A, $3
    mov %B, toto
    mov %C, $1
    syscall
    mov %A, $2
    mov %B, %R
    mov %C, $50
    mov %D, $1
    syscall
    mov %A, $4
    syscall
    load %R, $50
    exit

.data
   toto "tests/good/simple_read.in"
