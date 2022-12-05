default rel

%define NUM_read  0
%define NUM_write 1
%define NUM_brk   12
%define max_rw_len 0x7ffff000

section .text
global _start

_start:

%define cur        r8        ;REGISTER for keeping track of where we are
%define eof        r9        ;REGISTER address of end of input
%define amount     r10d      ;Register for the final answer
%define temp       r15d      ; temp
%define temp_b     r15b      ; temp
%define temp2      r14d      ; temp

%define elf1_low   ebx 
%define elf1_high  ecx 
%define elf2_low   edx 
%define elf2_high  esi 
%define state      edi
%define number     esp
%define input      rbp

    ; Allocate memory
    mov rax, NUM_brk
    xor rdi, rdi
    syscall
    mov input, rax
    lea rdi, [rax + max_rw_len]
    mov rax, NUM_brk
    syscall

    ; read in our input, oneshot 
    mov rax, NUM_read
    xor rdi, rdi          ; file descriptor stdin
    mov rsi, input        ; buffer
    mov rdx, max_rw_len   ; maximum read size
    syscall


    ; prepare registers
    mov cur, input ; cursor for input

    ; get location of EOF
    mov eof, input
    add eof, rax  

    ; Set registers to zero where necessary
    xor amount, amount
    xor number, number
    xor state,state
    
.begin: 
    cmp eof, cur
    je .end

    ; Create our number
    xor temp, temp
    mov temp_b, [cur]
    imul number,  dword 10
    sub temp_b, byte 48
    add number, temp

    inc cur
    cmp [cur], byte 46
    ja .begin
    inc cur

    cmp state, 1
    cmovb elf1_low, number
    cmove elf1_high, number
    cmp state, 2
    cmove elf2_low, number
    cmova elf2_high, number

    inc state
    xor number, number
    cmp state, 4
    jne .begin

    xor state, state

    lea temp, [amount+1]
    cmp elf1_low, elf2_high
    cmova temp, amount
    cmp elf1_high, elf2_low
    cmovae amount, temp

    jmp .begin
.end:

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

addchar:
    cmp count,qword 0
    je end_addchar

    xor rdx, rdx ; set to zero
    idiv temp

    add dl, byte 48
    mov [cur], dl

    dec cur
    dec count

    jmp addchar

end_addchar:
    ; Set up parameters and write to stdout
    mov rax, NUM_write    
    mov  edi, 1            ; file descriptor stdout
    mov rsi, input        ; buffer
    mov edx, 10           ; count
    syscall
    
    xor  rdi,rdi     ; result
    mov  eax,60      ; exit
    syscall

