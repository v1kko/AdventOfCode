%define NUM_read  0
%define NUM_brk   12
%define max_rw_len 0x7ffff000
%define input      esi       ; pointer to input
%define temp       ecx       ; temp


;int read_input(unused, char * file)
;eax read_input(unused, esi)
;touches eax, ecx, edi, esi
read_input:
    ; Allocate memory
    mov temp, max_rw_len
    xor eax, eax
    mov al, NUM_brk
    xor edi, edi
    syscall
    
    mov input, eax
    lea edi, [eax + temp]
    xor eax,eax
    mov al, NUM_brk
    syscall

    ; read in our input, oneshot 
    xor eax, eax
    mov  al, NUM_read
    xor edi, edi          ; file descriptor stdin
    mov edx, temp         ; maximum read size
    syscall

    ret
