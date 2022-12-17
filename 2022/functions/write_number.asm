%define NUM_write 1
%define cur rsi
%define temp rdi
%define temp_b dil


; void write_number(int esi)
; touches esi, edi, eax, ecx
write_number:
    mov cur, rsp
    dec cur
    mov [cur], byte 10 ; newline
    xor temp, temp
    mov temp_b, 10 ; divisor

    mov cl, 9
.digit_loop:
    dec cur
    xor edx, edx ; set to zero
    idiv temp
    add dl, byte 48
    mov [cur], dl
    dec cl
    jnz .digit_loop

    xor eax, eax
    xor edi, edi
    xor edx, edx

    ; Set up parameters and write to stdout
    mov al, NUM_write    
    inc edi                ; file descriptor stdout
    ; mov rsi, input       ; buffer ; this still holds
    mov dl, 10             ; count
    syscall
    
    ret
