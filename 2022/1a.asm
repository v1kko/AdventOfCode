default rel
%define NUM_read 0
%define NUM_write 1
%define max_rw_len 0x7ffff000

global _start

SECTION .bss
  input: RESB max_rw_len

%define cur     r8        ;REGISTER for keeping track of where we are
%define amount  r9        ;REGISTER cur elf amount
%define maximum r10       ;REGISTER max elf amount
%define eof     r12       ;REGISTER address of end of input
%define number  r13       ;REGISTER number we are decoding
%define temp    r14       ;REGISTER temporary
%define temp_b  r14b      ;REGISTER temporary byte
%define count   r9

SECTION .text
_start:

    ; read in our input, oneshot 
    mov rax, NUM_read
    mov rdi, 0            ; file descriptor stdin
    mov rsi, input        ; buffer
    mov rdx, max_rw_len   ; maximum read size
    syscall

    ; prepare registers
    mov cur, input
    xor amount, amount
    xor maximum, maximum
    xor number, number
    mov eof, input
    add eof, rax  ; Add the number of bytes read
    
begin: 
    cmp eof, cur
    je end
    
    cmp [cur], byte 10 ; is there a newline
    je newline

    ; Create our number
    xor temp, temp
    mov temp_b, [cur]
    imul number,  qword 10
    sub temp_b, byte 48
    add number, temp

    inc cur
    jmp begin

newline:
    add amount, number

    inc cur

    cmp [cur], byte 10 ; is there a newline
    je newelf

    xor number, number
    jmp begin
  
newelf:
    cmp amount, maximum
    cmova maximum, amount

    xor amount, amount
    inc cur

    xor number, number
    jmp begin

end:

    ; Convert maximum to ascii, reuse spent input
    mov cur, input
    add cur, qword 9
    mov rax, maximum
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
    mov  rdi, 1            ; file descriptor stdout
    mov rsi, input        ; buffer
    mov rdx, 10           ; count
    syscall
    
    mov  rdi,0      ; result
    mov  rax,60      ; exit(2)
    syscall

