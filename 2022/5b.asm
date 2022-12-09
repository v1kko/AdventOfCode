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
%define eof        rdi       ;REGISTER address of end of input

%define temp       r15       ; temp
%define temp_d     r15d      ; temp
%define temp_b     r15b      ; temp

%define data       rcx       ; 'heap'

%define offset     r9       ; spacing between stacks
%define offset_d   r9d       ; spacing between stacks

%define fill       r11       ; loc of fill of stack
%define fill_d     r11d      ; loc of fill of stack
%define fill_b     r11b      ; loc of fill of stack

%define offset_now r8        ; loc of fill of stack

%define entries    r12d      ; number of entries

%define box        r13       

%define number    r14d
%define number_b  r14b

%define stack1    r8d       ;
%define stack1_b  r8b       ;

%define stack2    ebp       ;
%define stack2_b  bpl       ;

%define loop       r10d


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

    ; get location of our 'heap'
    lea data, [eof + 1]

    ; Set registers to zero where necessary
    xor amount, amount
    xor temp_d, temp_d

.getlinelength:
    inc cur
    inc temp_d
    cmp [cur], byte 10
    jne .getlinelength
    inc temp_d
    lea cur, [input + 1]

%define lines r10d
    xor lines, lines
.getmaxboxes:
    lea cur, [cur + temp]
    inc lines
    cmp [cur], byte 49
    jne .getmaxboxes

    shr temp_d, 2 ; temp == r15d == length
    mov stack1, lines
    imul stack1, temp_d 
    lea offset, [stack1+1]

%define cur_entry ebp
    mov entries, temp_d
    mov cur_entry, entries
.initfill:
    mov temp_d, offset_d
    imul temp_d, cur_entry
    lea fill, [data+temp]
    mov [fill], byte 0
    sub cur_entry, 1
    jnz .initfill
    

    lea cur, [cur - 4] ; cursor for input
    mov cur_entry, entries
.fillstack:
    cmp [cur], byte 32
    je .noentry

    mov temp_d, offset_d
    imul temp_d, cur_entry
    lea fill, [data+temp]
    add byte [fill], byte 1
    xor temp_d, temp_d
    mov temp_b, byte [fill]
    lea box, [fill + temp]
    mov temp_b, [cur]
    mov [box], temp_b

.noentry:
    lea cur, [cur - 4]
    dec cur_entry
    cmp cur_entry, 0
    cmove cur_entry, entries

    cmp cur, input
    ja .fillstack
    
    mov cur, input
    mov temp_d, entries
    imul temp_d, 4
    inc lines
    imul temp_d, lines
  
%undef lines
    add cur, temp
    inc cur

%undef cur_entry


.begin: 
    lea cur, [cur + 5]
    xor number, number
    xor stack1, stack1
    xor stack2, stack2

.firstnum:
    ; Get our numbers
    xor temp_d, temp_d
    mov byte temp_b, [cur]
    imul number,  dword 10
    sub temp_b, byte 48
    add number, temp_d
    inc cur
    cmp [cur], byte 32
    jne .firstnum

    lea cur, [cur + 6]
    mov stack1_b, [cur]
    sub stack1_b, byte 48
    lea cur, [cur + 5]
    mov stack2_b, [cur]
    sub stack2_b, byte 48
    lea cur, [cur + 2]

    ; translate to pointers
    ; stack uno
    mov temp_d, offset_d
    imul temp_d, stack1
    lea fill, [data+temp]
    xor temp_d, temp_d
    sub byte [fill], number_b
    mov temp_b, [fill]
    lea stack1, [fill+temp]

    ; stack duo
    mov temp_d, offset_d
    imul temp_d, stack2
    lea fill, [data+temp]
    xor temp_d, temp_d
    mov temp_b, [fill]
    lea stack2, [fill+temp]
    add byte [fill], number_b

    xor loop, loop
.moveloop:
    inc loop

    inc stack1
    inc stack2
    mov temp_b, [stack1]
    mov [stack2], temp_b

    cmp loop, number
    jle .moveloop

    cmp cur, eof
    jb .begin

    mov cur, input
    xor loop, loop
    xor offset_now, offset_now
.answerloop:
    inc loop
    add offset_now, offset
    lea fill, [data+offset_now]
    xor temp_d, temp_d
    mov temp_b, [fill]
    lea box, [fill+temp]
    mov temp_b, [box]
    mov byte [cur], temp_b

    inc cur
    cmp loop, entries
    jne .answerloop
    mov byte [cur], 10

.end:
    ; Set up parameters and write to stdout
    mov rax, NUM_write    
    mov  edi, 1            ; file descriptor stdout
    ; mov rsi, input       ; buffer ; this still holds
    inc entries
    mov edx, entries       ; count
    syscall
    
    xor  edi,edi     ; result 0
    mov  eax,60      ; exit
    syscall

