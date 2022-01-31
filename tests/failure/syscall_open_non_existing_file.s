main:
    mov %A, $3
    mov %B, file
    mov %C, $2
    syscall

    exit

.data
    file "non_existing_file.in"
