include C:\masm32\include\masm32rt.inc    
	
.data

 buf db "IZ EQUAL!", 0

.data?



.code
main:


	mov eax, HKEY_USERS 
	mov ebx, 80000003h
	
	cmp eax, ebx
	jne notequal
	
	push offset buf
	call StdOut	
	
	notequal:
	
	call ExitProcess
end main

;HKEY_LOCAL_MACHINE 8000 0002h
;HKEY_USERS 8000 0003h