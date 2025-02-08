
buflen:
    xor rax,rax
.next:
    cmp byte [rdi+rax], 0
    je .done
    inc rax
    jmp .next
.done:
    ret

alloc:
    push rdi
    mov rax,12
    xor rdi,rdi
    syscall
    pop rdi
    push rax
    add rdi,rax
    mov rax,12
    syscall
    pop rax
    ret

print:
    push rbp
    mov rbp,rsp
    mov rdi,[rbp+16]
    call buflen
    mov rdx,rax
    mov rsi,rdi
    mov rax,1
    mov rdi,1
    syscall
    mov rsp,rbp
    pop rbp
    ret

input:
    push rbp
    mov rbp,rsp
    sub rsp,8
    mov rdi,1
    call alloc
    mov [rbp-8],rax
.loop:
    mov rsi,rax
    mov rax,0
    mov rdi,0
    mov rdx,1
    syscall
    test rax,rax
    jle .done
    cmp byte [rsi], 10
    je .remove_new_line
    mov rdi,1
    call alloc
    jmp .loop
.remove_new_line:
    mov byte [rsi],0
.done:
    mov rax,[rbp-8]
    mov rsp,rbp
    pop rbp
    ret

concat:
    push rbp
    mov rbp,rsp
    sub rsp,24
    mov rdi,[rbp+24]
    call buflen
    mov [rbp-8],rax
    mov rdi,[rbp+16]
    call buflen
    inc rax
    mov [rbp-16],rax
    add rax,[rbp-8]
    mov rdi,rax
    call alloc
    mov [rbp-24],rax
    mov rdi,rax
    mov rsi,[rbp+24]
    mov rcx,[rbp-8]
    rep movsb
    mov rsi,[rbp+16]
    mov rcx,[rbp-16]
    rep movsb
    pop rax
    mov rsp,rbp
    pop rbp
    ret

int_to_string:
    push rbp
    mov rbp, rsp
    mov rdi,21
    call alloc
    mov rdi,rax
    add rdi,21
    mov byte [rdi],0
    dec rdi
    mov rax,[rbp+16]
    xor rdx,rdx
    mov rcx,10
    xor rbx,rbx
    cmp rax,0
    jge .loop
    neg rax
    mov bl,'-'
.loop:
    mov rdx,0
    div rcx
    add dl,'0'
    dec rdi
    mov [rdi],dl
    test rax,rax
    jnz .loop
    test bl,bl
    jz .done
    dec rdi
    mov [rdi],bl
.done:
    mov rax,rdi
    mov rsp,rbp
    pop rbp
    ret

string_to_int:
    push rbp
    mov rbp, rsp
    mov rcx,1
    xor r8,r8
    mov rdi,[rbp+16]
    call buflen
    test rax,rax
    je .done
    add rax,[rbp+16]
    dec rax
    mov rdi,rax
.loop:
    xor rax,rax
    mov byte al,[rdi]
    cmp al,'-'
    je .negate
    sub rax,'0'
    mul rcx
    add r8,rax
    mov rax,rcx
    mov rcx,10
    mul rcx
    mov rcx,rax
    cmp [rbp+16],rdi
    jge .done
    dec rdi
    jmp .loop
.negate:
    neg r8
.done:
    mov rax,r8
    mov rsp,rbp
    pop rbp
    ret

float_to_string:
    push rbp
    mov rbp, rsp
    sub rsp,16
    movsd xmm0, qword [rbp+16]
    cvttsd2si rax, xmm0
    mov [rbp-16], rax
    push rax
    call int_to_string
    add rsp, 8
    mov [rbp-8], rax
    call buflen
    add rax,[rbp-8]
    mov byte [rax], '.'
    cvtsi2sd xmm1, qword [rbp-16]
    subsd xmm0, xmm1
    movsd xmm1, qword [abs_mask]
    andpd xmm0,xmm1
    mov rax, 1000000000000000000
    cvtsi2sd xmm1, rax
    mulsd xmm0, xmm1
    cvttsd2si rax, xmm0
    push rax
    call int_to_string
    push qword [rbp-8]
    push rax
    call concat
    sub rdi,2
.loop:
    cmp byte [rdi], '-'
    jne .continue
    mov byte [rdi], '0'
.continue:
    cmp byte [rdi], '0'
    jne .done
    cmp byte [rdi-1], '.'
    je .done
    mov byte [rdi], 0
    dec rdi
    jmp .loop
.done:
    mov rsp,rbp
    pop rbp
    ret

bool_to_string:
    push rbp
    mov rbp, rsp
    mov rax,[rbp+16]
    test rax,rax
    jz .false
    mov rax,true
    jmp .done
.false:
    mov rax,false
.done:
    mov rsp,rbp
    pop rbp
    ret

range0:
    push rbp
    mov rbp, rsp
    mov rdi,[rbp+16]
    inc rdi
    shl rdi,3
    call alloc
    mov rsi,rax
    mov rdi,[rbp+16]
    mov [rax],rdi
    xor rcx,rcx
.loop:
    cmp rcx,rdi
    jge .done
    add rax,8
    mov qword [rax],rcx
    inc rcx
    jmp .loop
.done:
    mov rax,rsi
    mov rsp,rbp
    pop rbp
    ret

range:
    push rbp
    mov rbp, rsp
    mov rdi,[rbp+16]
    inc rdi
    sub rdi,[rbp+24]
    shl rdi,3
    call alloc
    mov rsi,rax
    mov rdi,[rbp+16]
    sub rdi,[rbp+24]
    mov [rax],rdi
    mov rdi,[rbp+16]
    mov rcx,[rbp+24]
.loop:
    cmp rcx,rdi
    jge .done
    add rax,8
    mov qword [rax],rcx
    inc rcx
    jmp .loop
.done:
    mov rax,rsi
    mov rsp,rbp
    pop rbp
    ret

equals_string:
    push rbp
    mov rbp, rsp
    
    mov rdi,[rbp+16]
    mov rsi,[rbp+24]
.loop:
    mov al,[rdi]
    mov cl,[rsi]

    cmp al,cl
    jne .fail
    cmp byte al,0
    je .success
    inc rdi
    inc rsi
    jmp .loop

.success:
    mov rax,1
    pop rbp
    ret
.fail:
    xor rax,rax
    pop rbp
    ret

concat_list:
    push rbp
    mov rbp, rsp
    mov r8,[rbp+24]
    mov r9,[rbp+16]
    mov rdi,[r8]
    add rdi,[r9]
    mov r10,rdi
    inc rdi
    shl rdi,3
    call alloc
    push rax
    mov [rax],r10
    add rax,8
    mov rdi,rax
    mov rsi,[rbp+24]
    add rsi,8
    mov rcx,[r8]
    shl rcx,3
    rep movsb
    mov rsi,[rbp+16]
    add rsi,8
    mov rcx,[r9]
    shl rcx,3
    rep movsb
    pop rax
    pop rbp
    ret
