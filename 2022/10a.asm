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

    ; Set registers to zero where necessary
    xor amount, amount
    xor count, count
    xor value, value
    xor half, half
    inc value

.begin:
    inc count

    cmp count, 20
    je .store
    cmp count, 60
    je .store
    cmp count, 100
    je .store
    cmp count, 140
    je .store
    cmp count, 180
    je .store
    cmp count, 220
    je .store
.back:

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

.store:
   mov temp, value
   imul temp, count
   add amount, temp
   jmp .back

.end:

%define count edi

    ; Convert maximum to ascii, reuse spent input
    ;mov eax, amount
    mov temp, eax
    neg temp
    cmp eax, 0
    setl negate
    cmovl eax, temp

    mov cur, input
    add cur, dword 10
    mov count, dword 10
    mov [cur], byte 10 ; newline
    mov temp, dword 10 ; divisor
    dec cur

.addchar:

    xor edx, edx ; set to zero
    idiv temp

    add dl, byte 48
    mov [cur], dl

    dec cur
    dec count

    cmp count,dword 0
    ja .addchar

    inc cur
    mov dl, [cur]
    mov temp_b, 45
    cmp negate, 0
    cmovne edx, temp
    mov [cur], dl

    ; Set up parameters and write to stdout
    mov eax, NUM_write    
    mov  edi, 1            ; file descriptor stdout
    ; mov rsi, input       ; buffer ; this still holds
    mov edx, 11            ; count
    syscall
    
    xor  edi,edi     ; result 0
    mov  eax,60      ; exit
    syscall

