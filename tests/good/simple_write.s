main:
    mov %A, $1
    mov %B, $1
    mov %C, tata
    mov %D, $12
    syscall
    exit

.data
    tata "Simple write"
