main:

    mov %A, $10

loop:

    sub %A, $1
    cmp %A, $0
    jmpz loop

end:
    exit
