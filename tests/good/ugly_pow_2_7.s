mul:

    mov %R, $0

loopmul:

    add %R, %A
    sub %B, $1
    jmpz endmul

    cmp $0, $0
    jmpz loopmul


main:

    mov %A, $2
    mov %C, $7

    mov %R, $1

loop:

    mov %B, %R
    cmp $0, $0
    jmpz mul
endmul:
    sub %C, $1
    jmpz end

    cmp $0, $0
    jmpz loop

end:
    exit
