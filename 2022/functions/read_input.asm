%define NUM_read  0
%define NUM_open  2
%define O_RDONLY  0
%define NUM_brk   12
%define max_rw_len 0x7ffff000
%define input      esi       ; pointer to input
%define temp       ecx       ; temp


;int read_input(filename, char * file)
;eax read_input(edi, esi)
;touches eax, ecx, edi, esi, edx
read_input:
    xor eax, eax
    ; open file
    mov al, NUM_open
    ; filename is already in edi
    xor esi, esi
    syscall
    mov edx, eax ; store file descriptor

    ; Allocate memory
    mov temp, max_rw_len
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
    mov edi, edx          ; file descriptor 
    mov edx, temp         ; maximum read size
    syscall

    ret
