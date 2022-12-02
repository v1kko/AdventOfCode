default rel
%define NUM_read 0
%define NUM_write 1
%define max_rw_len 0x7ffff000

global _start

SECTION .bss
  input: RESB max_rw_len

%define cur     r8        ;REGISTER for keeping track of where we are
%define amount  r9        ;REGISTER cur elf amount
%define maximum1 r10       ;REGISTER max elf amount
%define maximum2 r11       ;REGISTER max elf amount
%define maximum3 r15       ;REGISTER max elf amount
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
    xor amount, amount      ; Set all registers to zero
    xor maximum1, maximum1
    xor maximum2, maximum2
    xor maximum3, maximum3
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
    cmp amount, maximum1
    cmova maximum3, maximum2
    cmova maximum2, maximum1
    cmova maximum1, amount
    ja endnewelf

    cmp amount, maximum2
    cmova maximum3, maximum2
    cmova maximum2, amount
    ja endnewelf

    cmp amount, maximum3
    cmova maximum3, amount
    ja endnewelf

endnewelf:

    xor amount, amount
    inc cur

    xor number, number
    jmp begin

end:

    ; Convert maximum to ascii, reuse spent input
    mov cur, input
    add cur, qword 9
    add maximum1, maximum2
    add maximum1, maximum3
    mov rax, maximum1
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

