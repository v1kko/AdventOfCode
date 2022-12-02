default rel
%define NUM_read 0
%define NUM_write 1
%define max_rw_len 0x7ffff000

global _start

SECTION .bss
  input: RESB max_rw_len

%define cur     r8        ;REGISTER for keeping track of where we are
%define eof     r9        ;REGISTER address of end of input
%define amount  r10       ;REGISTER cur elf amount
%define one     r11       ;REGISTER with the number one
%define temp    r12       ;REGISTER temporary
%define temp_b  r12b      ;REGISTER temporary byte
%define first   r13 
%define second  r14
%define first_b   r13b 
%define second_b  r14b
%define count   r15
%define three   r15

SECTION .text
_start:

    ; read in our input, oneshot 
    mov rax, NUM_read
    mov rdi, 0            ; file descriptor stdin
    mov rsi, input        ; buffer
    mov rdx, max_rw_len   ; maximum read size
    syscall

    ; prepare registers
    mov cur, input ; cursor for input
    mov one, 1
    mov three, 3

    ; get location of EOF
    mov eof, input
    add eof, rax  

    ; Set registers to zero where necessary
    xor amount, amount     
    
begin: 
    cmp eof, cur
    je end

    mov temp, [cur]

    mov first_b, temp_b
    sub first_b, byte 64
    shr temp, 16 ; shift to last value
    mov second_b, temp_b
    sub second_b, byte 87

    cmp second_b, 2
    ja above
    jl below
    add amount, 3
    jmp cont

above:
    add first_b, 1
    cmp first_b, 4
    cmove first, one
    add amount, 6
    jmp cont

below:
    sub first_b, 1
    cmovz first, three

cont:
    add amount, first ; score for choice

    add cur, 4
    jmp begin

end:

    ; Convert maximum to ascii, reuse spent input
    mov rax, amount
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
    ; Set up parameters and call the C function
    mov rax, NUM_write    
    mov  rdi, 1            ; file descriptor stdout
    mov rsi, input        ; buffer
    mov rdx, 10           ; count
    syscall
    
    mov  rdi,0      ; result
    mov  rax,60      ; exit(2)
    syscall

