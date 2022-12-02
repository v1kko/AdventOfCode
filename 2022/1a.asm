default rel
%define NUM_read 0
%define NUM_write 1
%define max_rw_len 0x7ffff000

global _start

section .readonly
  bases: dq 1000000000, 100000000, 10000000, 1000000, 100000, 10000, 1000, 100, 10, 1, 0

SECTION .bss
  input: RESB max_rw_len

%define cur     r8        ;REGISTER for keeping track of where we are
%define amount  r9        ;REGISTER cur elf amount
%define maximum r10       ;REGISTER max elf amount
%define elf     r11       ;REGISTER which elf has the max amount
%define eof     r12       ;REGISTER address of end of input
%define number  r13       ;REGISTER number we are decoding
%define temp    r14       ;REGISTER number we are decoding
%define temp_b  r14b      ;REGISTER number we are decoding
%define curelf  r15       ;REGISTER current elf
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
    mov curelf, 0
    mov amount, 0
    mov maximum, 0
    mov number, 0
    mov elf, -1
    mov eof, input
    add eof, rax  ; Add the number of bytes read
    
begin: 
    cmp eof, cur
    je end
    
    cmp [cur], byte 10 ; is there a newline
    je newline

    ; Create our number
    mov temp, qword 0
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

    mov number, qword 0
    jmp begin
  
newelf:
    cmp amount, maximum
    cmova maximum, amount
    cmova elf, curelf

    mov amount, qword 0
    inc elf
    inc cur

    mov number, qword 0
    jmp begin

end:

    ; Convert maximum to ascii, reuse spent input
    mov cur, input
    add cur, qword 9
    mov rax, maximum
    mov count, qword 9
    mov [cur], byte 10 ; newline
    dec cur

addchar:
    cmp count,qword 0
    je end_addchar

    xor rdx, rdx ; set to zero
    mov temp, 10
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

