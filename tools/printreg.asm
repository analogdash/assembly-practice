include C:\masm32\include\masm32rt.inc    
	
.data

 buf db 9 DUP(0)

.data?



.code
main:


	mov ebx, [esp]
	
	keep:
	mov ax, WORD PTR [ebx]
	cmp ax, 5A4Dh
	je goon
	dec ebx
	jmp keep
	goon:
	
	;add ebx, 3Ch
	
	mov ecx, [ebx+3Ch]
	add ecx, ebx
	mov eax, [ecx]
	
	;mov eax, [ebx]
	;add ebx 
	
	push offset buf
	push eax
	call dw2hex
	
	push offset buf
	call StdOut	
	
	call ExitProcess
end main