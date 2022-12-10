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
%define input      rsi       ; input file
%define cur        rbx       ; cursor
%define cur_d      ebx       ; cursor
%define eof        rdi       ;REGISTER address of end of input

%define temp       r15       ; temp
%define temp_d     r15d      ; temp
%define temp_b     r15b      ; temp

%define char1      r8b
%define char2      r9b
%define char3      r10b
%define char4      r11b

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
    mov rdx, max_rw_len   ; maximum read size
    syscall

    ; prepare registers
    mov cur, input ; cursor for input

    ; get location of EOF
    lea eof, [input + rax]

    ; Set registers to zero where necessary
    xor amount, amount

    mov char1, [cur]
    inc cur
    mov char2, [cur]
    inc cur
    mov char3, [cur]
    inc cur


.begin:
    mov char4, byte [cur]
    inc cur
    
    cmp char4, char3
    je .continue
    cmp char4, char2
    je .continue
    cmp char4, char1
    je .continue
    cmp char3, char2
    je .continue
    cmp char3, char1
    je .continue
    cmp char2, char1
    je .continue
    
    jmp .end
.continue

    mov char1, char2
    mov char2, char3
    mov char3, char4

    cmp cur, eof
    jb .begin

.end:
    sub cur, input
    mov amount, cur_d


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
    ; mov rsi, input       ; buffer ; this still holds
    mov edx, 10            ; count
    syscall
    
    xor  edi,edi     ; result 0
    mov  eax,60      ; exit
    syscall

