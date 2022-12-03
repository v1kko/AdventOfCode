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

%define cur     r8        ;REGISTER for keeping track of where we are
%define eof     r9        ;REGISTER address of end of input
%define length r10        ; could be probably combined with halflength
%define halflength r11
%define first  r12        ;REGISTER with content of first half rugsack
%define second r13        ;REGISTER with content of secon half rugsack
%define zero   r14        ;REGISTER zero
%define temp   r15        ;REGISTER char temp
%define temp_b r15b       ;REGISTER char temp
%define amount rax          

    ; prepare registers
    mov cur, input ; cursor for input

    ; get location of EOF
    mov eof, input
    add eof, rax  

    ; Set registers to zero where necessary
    xor zero, zero
    xor amount, amount
    
.begin: 
    cmp eof, cur
    je .end

    mov length, cur 

.lengthloop:
    inc length
    cmp length, eof
    je .lastline
    cmp [length], byte 10
    jne .lengthloop
.lastline:
    sub length, cur
    mov halflength, length
    shr halflength, 1 ; divide by two, convenience function
    add halflength, cur
    add length, cur

    xor second, second ; second is also the temp for first
.readloop:
    cmp length, cur
    je .endreadloop

    cmp halflength, cur
    cmove first, second
    cmove second, zero

    xor temp, temp

    mov temp_b, [cur]
    cmp temp_b, 92
    jl .uppercase
    sub temp_b, 96
    jmp .casecont
.uppercase:
    sub temp_b, 38
.casecont:

; temp has the priority
    mov cl, temp_b
    mov temp, 1
    shl temp,cl; temp has 2^prio (single bit set)

    ; use second as dict
    or second, temp

    inc cur
    jmp .readloop

.endreadloop:
    ; end of the line, we should extract which item is duplicated, luckily we have bitmaps :)
    and first, second ; and we are done! (well almost, we have to convert bit to number still)

    bsr second, first ; abuse bsr to find the index of the set bit (hacky hacky)

    add amount, second ; amazing

    inc cur
    jmp .begin

.end:

%define count rdi
%define temp r15

    ; Convert maximum to ascii, reuse spent input
    ; rax is reserved
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

