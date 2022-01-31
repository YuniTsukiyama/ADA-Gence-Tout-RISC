main:
    mov %A, $92
    nor %A, $78
    mov %R, $83
    nor %R, %A
    exit
