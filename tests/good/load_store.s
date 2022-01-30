main:
    store $51, $42
    load  %A,  $51

    mov   %B,  $101

    store %B,  %A
    load  %R,  %B

    exit
