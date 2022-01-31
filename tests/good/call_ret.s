foo:
    push $101
    push $51
    pop  %R
    ret

main:
    push $42
    call foo
    pop  %A
    add  %R, %A
    exit
