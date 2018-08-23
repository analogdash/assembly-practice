include C:\masm32\include\masm32rt.inc    
	
.data

	printme db "I want to be printed.", 0
	
.data?


.code

StdOut_106 proc toprint:DWORD

;https://docs.microsoft.com/en-us/windows/console/console-handles
	
	mov edx, toprint
	
	mov ecx, edx
	keepcounting:
	mov al, [ecx]
	cmp al, 0
	je donecounting
	inc ecx
	jmp keepcounting
	donecounting:
	dec ecx
	sub ecx, edx

	push STD_OUTPUT_HANDLE
	call GetStdHandle

	push NULL ;reserved, must be null
	push NULL ;A pointer to a variable that receives the number of characters actually written.
	push ecx ;The number of characters to be written. If the total size of the specified number of characters exceeds the available heap, the function fails with 
	push edx ;A pointer to a buffer that contains characters to be written to the console screen buffer.
	push eax ;handle of std out
	call WriteConsole
	
	ret 4

StdOut_106 endp

main:
	
	push offset printme
	call StdOut_106

	
	call ExitProcess
end main