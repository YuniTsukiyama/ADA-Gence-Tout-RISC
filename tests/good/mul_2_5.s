main:

    mov %C, $2
    mov %A, $5
    mov %R, $0

loop:

    add %R, %A
    sub %C, $1
    jmpz end

    cmp $0, $0
    jmpz loop

end:
    exit
