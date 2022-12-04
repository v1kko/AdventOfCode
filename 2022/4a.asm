default rel
%define NUM_read 0
%define NUM_write 1
%define max_rw_len 0x7ffff000

global _start

SECTION .bss
  input: RESB max_rw_len

SECTION .text
_start:

    ; read in our input, oneshot 
    mov rax, NUM_read
    mov rdi, 0            ; file descriptor stdin
    mov rsi, input        ; buffer
    mov rdx, max_rw_len   ; maximum read size
    syscall

%define cur        r8        ;REGISTER for keeping track of where we are
%define eof        r9        ;REGISTER address of end of input
%define amount     r10d      ;Register for the final answer
%define temp       r15d      ; temp
%define temp_b     r15b      ; temp

%define elf1_low   ebx 
%define elf1_high  ecx 
%define elf2_low   edx 
%define elf2_high  esi 
%define number     esp
%define state      edi
%define temp2      r11d
%define flags      r12d

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
    xor number, number

    lea state, [state+1]
    cmp state, 4
    jne .begin
    xor state, state

    xor flags, flags
    lea temp, [amount+1]
    mov temp2, 1
    cmp elf1_low, elf2_low
    cmove amount, temp
    je .begin
    cmova flags, temp2
    shl flags, 1
    lea temp2, [flags + 1]
    cmp elf1_high, elf2_high
    cmove amount, temp
    je .begin
    cmovb flags, temp2
    cmp flags, 3
    cmove amount, temp
    cmp flags, 0
    cmove amount, temp

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
    mov  rdi, 1            ; file descriptor stdout
    mov rsi, input        ; buffer
    mov rdx, 10           ; count
    syscall
    
    mov  rdi,0      ; result
    mov  rax,60      ; exit(2)
    syscall

