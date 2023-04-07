default rel

section .text
global _start

%include "functions/read_input.asm"
%include "functions/write_number.asm"

_start:

%define amount     eax       ;the final answer
%define amount_b   al        ;the final answer
%define input      esi       ; input file
%define cur        ebx       ; cursor
%define eof        edi       ;REGISTER address of end of input
%define temp       ecx       ; temp
%define temp_b      cl       ; temp

%define line       ebp       ; line length
%define newamount  edx       ; temporary
%define height      r8b      ; temporary
%define visible     r9d      ; temporary

    mov rdi, rsp          
    add rdi, 16              ; argv[1]
    mov rdi, [rdi]
    call read_input

    ; prepare registers
    mov cur, input ; cursor for input

    ; get location of EOF
    lea eof, [input + eax]

    ; Set registers to zero where necessary
    xor amount, amount

    ; find line length
.linelength:
    inc cur
    cmp [cur], byte 10
    jne .linelength
    inc cur
    mov line, cur
    sub line, input

.mainloop:
    cmp [cur], byte 10
    je .next

    ; check if border
    mov temp, cur
    dec temp
    cmp [temp], byte 10
    je .next
    inc temp
    inc temp
    cmp [temp], byte 10
    je .next

    mov height, [cur]
    xor newamount, newamount
     
    ;west
    mov temp, cur
    dec temp
.west:
    inc newamount
    cmp height, byte [temp]
    jle .nowest
    dec temp
    cmp [temp], byte 10
    jne .west
.nowest:

    ; east
    xor visible, visible
    mov temp, cur
    inc temp
.east:
    inc visible
    cmp height, byte [temp]
    jle .noeast
    inc temp
    cmp [temp], byte 10
    jne .east
.noeast:
    imul newamount, visible

    ; north
    xor visible, visible
    mov temp, cur
    sub temp, line
.north:
    inc visible
    cmp height, byte [temp]
    jle .nonorth
    sub temp, line
    cmp temp, input
    jg .north
.nonorth:
    imul newamount, visible

    ; south
    xor visible, visible
    mov temp, cur
    add temp, line
.south:
    inc visible
    cmp height, byte [temp]
    jle .nosouth
    add temp, line
    cmp temp, eof
    jl .south
.nosouth:
    imul newamount, visible
    cmp newamount, amount
    cmovg amount, newamount
    xor newamount, newamount

.next:
    inc cur
    mov temp, eof
    sub temp, line
    cmp cur, temp
    jne .mainloop

.end:
    call write_number

    xor  edi,edi     ; result 0
    mov  al,60      ; exit
    syscall
    
