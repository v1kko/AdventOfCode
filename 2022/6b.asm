default rel

%define NUM_read  0
%define NUM_write 1
%define NUM_brk   12
%define max_rw_len 0x7ffff000

section .text
global _start

_start:

%define amount     r11d      ;the final answer
%define amount_b   al        ;the final answer
%define input      r10       ; input file
%define cur        rbx       ; cursor
%define cur_d      ebx       ; cursor
%define eof        r12       ;REGISTER address of end of input

%define temp       r15       ; temp
%define temp_d     r15d      ; temp
%define temp_b     r15b      ; temp

%define i          r8
%define id         r8d
%define j          r9
%define jd         r9d

    ; Allocate memory
    mov rax, NUM_brk
    xor rdi, rdi
    syscall
    mov rsi, rax
    lea rdi, [rax + max_rw_len]
    mov rax, NUM_brk
    syscall

    ; read in our input, oneshot 
    mov rax, NUM_read
    xor rdi, rdi          ; file descriptor stdin
    mov rdx, max_rw_len   ; maximum read size
    syscall

    ; prepare registers
    mov input, rsi
    mov cur, input ; cursor for input

    ; get location of EOF
    lea eof, [input + rax]

    ; Set registers to zero where necessary
    xor amount, amount

.begin:
    mov id, 12
.loop2:
    mov jd, 13
.loop1:
    lea rsi, [cur+i]
    lea rdi, [cur+j]
    cmpsb 
    je .duplicate

    dec jd
    cmp jd, id
    jne .loop1

    dec id
    jl .end
    jmp .loop2

.duplicate:
    lea cur, [cur+i+1]
    lea temp, [cur+14]
    cmp temp, eof
    jb .begin

.end:
    sub cur, input
    lea amount, [cur + 14]


%define count rdi
%define temp r15

    ; Convert maximum to ascii, reuse spent input
    mov eax, amount
    mov cur, input
    add cur, qword 9
    mov count, qword 9
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
    mov rax, NUM_write    
    mov  edi, 1            ; file descriptor stdout
    mov rsi, input       ; buffer ; this still holds
    mov edx, 10            ; count
    syscall
    
    xor  edi,edi     ; result 0
    mov  eax,60      ; exit
    syscall

