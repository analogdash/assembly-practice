include C:\masm32\include\masm32rt.inc

.data

	arg1 db 128 DUP(0)
	arg2 db 128 DUP(0)
	arg3 db 128 DUP(0)
	ArgErrorMsg db "Error with arguments.",0
	
	Output db 260 DUP (0)
	Arguments db 260 DUP (0)
	Space db " ", 0
	
	notepad db "notepad", 0
	notepadexe db "notepad.exe", 0
	calcexe db "calc.exe", 0
	
	msg db "No notepad for you! Calculator only!", 0
	msgtitle db "Notepad restriction", 0

.data?

.code
main:

	push offset arg1
	push 1
	call GetCL
	
	cmp al, 1
	jne argerror ;first argument broken
	
	push offset arg2
	push 2
	call GetCL
	
	cmp al, 1
	jne argerror ;second argument broken
	
	push offset arg3
	push 3
	call GetCL
	
	cmp al, 2
	jne argerror ;third argument exists when it shouldn't
	
	lea eax, [arg1]
	lea ebx, [arg2]
	
	;checks for proper argument syntax "/?="
	cmp BYTE PTR [eax], "/"
	jne argerror
	cmp BYTE PTR [ebx], "/"
	jne argerror
	add eax, 2
	add ebx, 2
	cmp BYTE PTR [eax], "="
	jne argerror
	cmp BYTE PTR [eax], "="
	jne argerror
	dec eax
	dec ebx
	
	cmp BYTE PTR [eax], "p"
	je itsp
	cmp BYTE PTR [eax], "P"
	je itsp
	cmp BYTE PTR [eax], "A"
	je itsa
	cmp BYTE PTR [eax], "a"
	je itsa
	jmp argerror
	
	itsp:
	cmp BYTE PTR [ebx], "A"
	je formatpa
	cmp BYTE PTR [ebx], "a"
	je formatpa
	jmp argerror
	
	itsa:
	cmp BYTE PTR [ebx], "P"
	je formatap
	cmp BYTE PTR [ebx], "p"
	je formatap
	jmp argerror
	
	formatpa:
	add eax, 2
	add ebx, 2
	push ebx ; this contains the arguments
	push eax
	push offset Output
	call lstrcatA
	jmp notepadcheck
	
	formatap:
	add ebx, 2
	add eax, 2
	push eax ;this contains the arguments
	push ebx
	push offset Output
	call lstrcatA
	
	notepadcheck:
	push offset Output
	push offset notepad
	call lstrcmpiA
	cmp eax, 0
	je isnotepad
	push offset Output
	push offset notepadexe
	call lstrcmpiA
	je isnotepad
	
	;pop eax ;pointer to arguments is at top of stack
	;push eax
	push offset Arguments
	call lstrcatA
	jmp stringready
	
	isnotepad:
	pop eax ;this is to get rid of arguments still in stack
	mov BYTE PTR [Output], 0
	push offset calcexe
	push offset Output
	call lstrcatA
	
	push MB_OK
	push offset msgtitle
	push offset msg
	push 0
	call MessageBox
		
	stringready:
	;push offset Output
	;call StdOut
	
	call GetDesktopWindow
	
	push SW_SHOWNORMAL
	push 0
	push offset Arguments
	push offset Output
	push 0
	push eax ;return value of GetDesktopWindow
	call ShellExecuteA
	
	call ExitProcess
	argerror:
	push offset ArgErrorMsg
	call StdOut
	call ExitProcess
end main