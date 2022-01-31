mul:

    mov %R, $0

loopmul:

    add %R, %A
    sub %B, $1
    jmpz endmul

    jmp loopmul

endmul:
    ret


main:

    mov %A, $2
    mov %C, $7

    mov %R, $1

loop:

    mov %B, %R
    call mul
    sub %C, $1
    jmpz end

    jmp loop

end:
    exit
