default rel

%define NUM_read  0
%define NUM_write 1
%define NUM_brk   12
%define max_rw_len 0x7ffff000

section .text
global _start

_start:

%define amount     eax       ;the final answer
%define amount_b   al        ;the final answer
%define input      esi       ; input file
%define cur        ebx       ; cursor
%define eof        edi       ;REGISTER address of end of input
%define temp       ecx       ; temp
%define temp_b      cl       ; temp
%define one        edx     
%define window     ebp       ;
%define temp2      esp       ;
%define temp2b      spl      ;


%define char1      r8b
%define char2      r9b
%define char3      r10b
%define char4      r11b

    ; Allocate memory
    mov eax, NUM_brk
    xor edi, edi
    syscall
    mov input, eax
    lea edi, [eax + max_rw_len]
    mov eax, NUM_brk
    syscall

    ; read in our input, oneshot 
    mov eax, NUM_read
    xor edi, edi          ; file descriptor stdin
    mov rdx, max_rw_len   ; maximum read size
    syscall

    ; prepare registers
    mov cur, input ; cursor for input

    ; get location of EOF
    lea eof, [input + eax]

    ; Set registers to zero where necessary
    mov one,  1
    xor window, window
    xor amount, amount

    mov temp2, one
    mov cl, byte [cur]
    shl temp2, cl
    xor window, temp2
    inc cur
    mov temp2, one
    mov cl, byte [cur]
    shl temp2, cl
    xor window, temp2
    inc cur
    mov temp2, one
    mov cl, byte [cur]
    shl temp2, cl
    xor window, temp2
    inc cur

.begin:
    ;add
    mov temp2, one
    mov cl, byte [cur]
    shl temp2, cl
    xor window, temp2
    inc cur
    
    popcnt amount, window

    ;remove
    mov temp2, one
    lea temp, [cur-4]
    mov cl, byte [temp]
    shl temp2, cl
    xor window, temp2

    cmp amount, 4
    jne .begin

    sub cur, input
    mov amount, cur

%define count rdi

    ; Convert maximum to ascii, reuse spent input
    ;mov eax, amount
    mov cur, input
    add cur, dword 9
    mov count, dword 9
    mov [cur], byte 10 ; newline
    dec cur
    mov temp, 10 ; divisor

.addchar:

    xor rdx, rdx ; set to zero
    idiv temp

    add dl, byte 48
    mov [cur], dl

    dec cur
    dec count

    cmp count,qword 0
    ja .addchar

end_addchar:

    ; Set up parameters and write to stdout
    mov eax, NUM_write    
    mov  edi, 1            ; file descriptor stdout
    ; mov rsi, input       ; buffer ; this still holds
    mov edx, 10            ; count
    syscall
    
    xor  edi,edi     ; result 0
    mov  eax,60      ; exit
    syscall

