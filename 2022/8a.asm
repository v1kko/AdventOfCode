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

    mov rdi, rsp          
    add rdi, 16              ; argv[1]
    mov rdi, [rdi]
    call read_input

    ; prepare registers
    mov cur, input ; cursor for input

    ; get location of EOF
    lea eof, [input + eax]

    ; Set registers to zero where necessary

    ; find line length
.linelength:
    inc cur
    cmp [cur], byte 10
    jne .linelength
    mov line, cur
    sub line, input
    inc cur

    mov amount, line ; lets take the last and first line as free
    shl amount, 1
    inc line ; take newline as well

.mainloop:
    cmp [cur], byte 10
    je .next

    mov newamount, amount
    inc newamount
    ; check if border
    mov temp, cur
    dec temp
    cmp [temp], byte 10
    cmove amount, newamount
    je .next
    inc temp
    inc temp
    cmp [temp], byte 10
    cmove amount, newamount
    je .next

    mov height, [cur]
     
    ;west
    mov temp, cur
    dec temp
.west:
    cmp height, byte [temp]
    jle .nowest
    dec temp
    cmp [temp], byte 10
    jne .west
    mov amount, newamount
    jmp .next
.nowest:

    ; east
    mov temp, cur
    inc temp
.east:
    cmp height, byte [temp]
    jle .noeast
    inc temp
    cmp [temp], byte 10
    jne .east
    mov amount, newamount
    jmp .next
.noeast:

    ; north
    mov temp, cur
    sub temp, line
.north:
    cmp height, byte [temp]
    jle .nonorth
    sub temp, line
    cmp temp, input
    jg .north
    mov amount, newamount
    jmp .next
.nonorth:

    ; south
    mov temp, cur
    add temp, line
.south:
    cmp height, byte [temp]
    jle .next
    add temp, line
    cmp temp, eof
    jl .south
    mov amount, newamount
    jmp .next


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
    
