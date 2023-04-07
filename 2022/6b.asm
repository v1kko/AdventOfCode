default rel

%define NUM_read  0
%define NUM_brk   12
%define max_rw_len 0x7ffff000

section .text
global _start

%include "functions/write_number.asm"

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
%define temp2      r8d       ;
%define temp2b     r8b       ;


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
    xor one, one
    inc one
    xor window, window

    mov amount_b, 13
.prefill:
    mov temp2, one
    mov cl, byte [cur]
    shl temp2, cl
    xor window, temp2
    inc cur
    dec amount_b
    jnz .prefill

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
    lea temp, [cur-14]
    mov cl, byte [temp]
    shl temp2, cl
    xor window, temp2

    cmp amount, 14
    jne .begin

    sub cur, input
    mov amount, cur
    
    call write_number

    xor  edi,edi     ; result 0
    mov  al,60      ; exit
    syscall
    
