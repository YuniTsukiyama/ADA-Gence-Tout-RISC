main:
    mov %A, $3
    mov %B, toto
    mov %C, $0
    syscall
    mov %A, $1
    mov %B, %R
    mov %C, tata
    mov %D, $12
    syscall
    mov %A, $4
    syscall
    exit

.data
   toto "open_write.in"
   tata "Hello World!"
