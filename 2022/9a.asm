default rel

%define NUM_read  0
%define NUM_brk   12
%define max_rw_len 0x7ffff000

section .text
global _start

%include "functions/write_number.asm"

_start:

%define amount     eax       ;the final answer
%define amount_b   al        ;the final answer
%define cur        ebx       ; cursor
%define eof        edi       ;REGISTER address of end of input
%define temp       ecx       ; temp
%define temp_b      cl       ; temp
%define input      r13d

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
    mov rdx, max_rw_len   ; maximum read size
    syscall

    ; prepare registers
    mov input, esi
    mov cur, esi ; cursor for input

    ; get location of EOF
    lea eof, [esi + eax]

    ; Set registers to zero where necessary

    ; Get xmin, xmax, ymin, ymax
%define curx       esi     
%define cury       ebp     
%define xmin       r10d     
%define xmax       edx     
%define ymin       r9d     
%define ymax       r8d     
%define num        r11d
%define temp2d     r12d
%define temp2      r12b
%define temp3      r14d

xor curx, curx
xor cury, cury
xor xmin, xmin
xor xmax, xmax
xor ymin, ymin
xor ymax, ymax

.preloop:
    mov temp_b, [cur]
    inc cur
    inc cur
    xor temp2d, temp2d
    xor num, num
.pregetnum:
    mov temp2, byte [cur]
    imul num,  dword 10
    sub temp2, byte 48
    add num, temp2d
    inc cur
    cmp [cur], byte 10
    jne .pregetnum
    
    cmp temp_b, byte 'U'
    jne .D
    add cury, num
    cmp cury, ymax
    cmovg ymax, cury

.D:
    cmp temp_b, byte 'D'
    jne .R
    sub cury, num
    cmp cury, ymin
    cmovl ymin, cury

.R:
    cmp temp_b, byte 'R'
    jne .L
    add curx, num
    cmp curx, xmax
    cmovg xmax, curx

.L:
    cmp temp_b, byte 'L'
    jne .preloop_next
    sub curx, num
    cmp curx, ymin
    cmovl xmin, curx
    
.preloop_next:
    inc cur
    cmp cur, eof
    jne .preloop

; Prepare field
    mov curx, xmin
    neg curx
    add xmax, curx
    mov cury, ymin
    neg cury
    add ymax, cury

%undef xmin
%undef ymin
%define x r10d
%define y r9d
    inc xmax
    inc ymax

    xor x, x
    xor y, y

.prefield:
    mov [cur], byte '.'
    inc x 
    inc cur
    cmp x, xmax
    jl .prefield
    xor x, x
    mov [cur], byte 10
    inc cur
    inc y
    cmp y, ymax
    jl .prefield
    mov [cur], byte 10

    mov x, curx
    mov y, cury
    inc xmax

    mov cur, input

.mainloop:
    mov temp_b, [cur]
    inc cur
    inc cur
    xor temp2d, temp2d
    xor num, num
.getnum:
    mov temp2, byte [cur]
    imul num,  dword 10
    sub temp2, byte 48
    add num, temp2d
    inc cur
    cmp [cur], byte 10
    jne .getnum

    xor temp2d, temp2d
    
    cmp temp_b, byte 'U'
    jne .DD
.ULOOP:
    cmp cury, y
    cmovg x, curx
    cmovg y, cury

    mov temp3, xmax
    imul temp3, y
    add temp3, x
    add temp3, eof
    mov [temp3], byte '#'

    inc cury
    inc temp2d
    cmp temp2d, num
    jne .ULOOP

.DD:
    cmp temp_b, byte 'D'
    jne .RR
.DLOOP:
    cmp cury, y
    cmovl x, curx
    cmovl y, cury

    mov temp3, xmax
    imul temp3, y
    add temp3, x
    add temp3, eof
    mov [temp3], byte '#'

    dec cury
    inc temp2d
    cmp temp2d, num
    jne .DLOOP

.RR:
    cmp temp_b, byte 'R'
    jne .LL
.RLOOP:
    cmp curx, x
    cmovg x, curx
    cmovg y, cury

    mov temp3, xmax
    imul temp3, y
    add temp3, x
    add temp3, eof
    mov [temp3], byte '#'

    inc curx
    inc temp2d
    cmp temp2d, num
    jne .RLOOP

.LL:
    cmp temp_b, byte 'L'
    jne .mainloop_next
.LLOOP:
    cmp curx, x
    cmovl x, curx
    cmovl y, cury

    mov temp3, xmax
    imul temp3, y
    add temp3, x
    add temp3, eof
    mov [temp3], byte '#'

    dec curx
    inc temp2d
    cmp temp2d, num
    jne .LLOOP
    
.mainloop_next:
    inc cur
    cmp cur, eof
    jne .mainloop


    xor amount, amount
    imul xmax, ymax
    mov cur, eof
    lea eof, [eof+xmax]

.count:
    mov temp, amount
    inc temp
    cmp [cur], byte '#'
    cmove amount, temp
    inc cur
    cmp cur, eof
    jne .count

.end:
    call write_number

    xor  edi,edi    ; result 0
    xor  eax, eax   ;
    mov  al,60      ; exit
    syscall
    
