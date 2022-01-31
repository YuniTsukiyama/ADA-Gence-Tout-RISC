main:
    mov %A, $3
    mov %B, toto
    mov %C, $1
    syscall
    mov %A, $2
    mov %B, %R
    mov %C, $100
    mov %D, $20
    syscall
    mov %A, $1
    mov %B, $1
    mov %C, $100
    mov %D, $20
    syscall
    mov %B, %R
    mov %A, $4
    syscall
    exit

.data
   toto "tests/good/open_read_write.in"
   tata "Hello World!"
