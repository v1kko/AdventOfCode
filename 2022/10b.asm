default rel

%define NUM_read  0
%define NUM_write 1
%define NUM_brk   12
%define max_rw_len 0x7ffff000

section .text
global _start

_start:

%define amount     eax       ;the final answer
%define amount_b    al       ;the final answer
%define input      esi       ; input file
%define cur        ebx       ; cursor
%define eof        edi       ;REGISTER address of end of input
%define temp       ecx      ; temp
%define temp_b      cl      ; temp


%define half        dl      
%define count      ebp
%define value      r9d
%define number     r8d
%define negate     r11b
%define answer     r12
%define pixel_b    r13b
%define pixel      r13d
%define scanline   r14d

    ; Allocate memory
    mov eax, NUM_brk
    xor edi, edi
    syscall
    mov input, eax
    lea edi, [eax + max_rw_len]
    mov eax, NUM_brk
    syscall

    ; read in our input, oneshot 
    mov eax, NUM_read
    xor edi, edi          ; file descriptor stdin
    mov edx, max_rw_len   ; maximum read size
    syscall

    ; prepare registers
    mov cur, input ; cursor for input

    ; get location of EOF
    lea eof, [input + eax]
    lea answer, [eof + 1]

    ; Set registers to zero where necessary
    xor amount, amount
    xor count, count
    xor scanline, scanline
    xor value, value
    xor half, half
    inc value

.begin:
    inc count
    inc scanline
    mov pixel_b, ' '
    mov temp_b, '#'
    mov number, value
    sub number, scanline
    cmovz pixel, temp
    add number, 1
    cmovz pixel, temp
    add number, 1
    cmovz pixel, temp
    mov [answer], pixel_b
    inc answer

    cmp count, 40
    je .addenter
    cmp count, 80
    je .addenter
    cmp count, 120
    je .addenter
    cmp count, 160
    je .addenter
    cmp count, 200
    je .addenter
    cmp count, 240
    je .addenter
.back:

    xor temp, temp
    cmp scanline, 40
    cmove scanline, temp

    cmp count, 240
    je .end

    cmp [cur], byte 110
    jne .addx
.continue:
    inc cur
    cmp [cur], byte 10
    jne .continue
    inc cur
    cmp cur, eof
    jl .begin
    jmp .end

.addx:
    cmp half, 0
    sete half
    je .begin

.getnum:
    inc cur
    cmp [cur], byte 32
    jne .getnum
    inc cur
    xor number, number
    xor negate, negate
    lea temp, [cur+1]
   
    cmp [cur], byte 45
    cmove cur, temp
    sete negate

.num:
    ; Get our numbers
    xor temp, temp
    mov byte temp_b, [cur]
    imul number,  dword 10
    sub temp_b, byte 48
    add number, temp
    inc cur
    cmp [cur], byte 10
    jne .num
    mov temp, number
    neg temp

    cmp negate, 0
    cmovne number, temp

    add value, number

    inc cur
    cmp cur, eof
    jl .begin
    jmp .end

.addenter:
    mov [answer], byte 10
    inc answer
    jmp .back

.end:
    lea answer, [eof + 1]

    ; Set up parameters and write to stdout
    mov eax, NUM_write    
    mov  edi, 1            ; file descriptor stdout
    mov rsi, answer
    mov edx, 246           ; count
    syscall
    
    xor  edi,edi     ; result 0
    mov  eax,60      ; exit
    syscall

