include C:\masm32\include\masm32rt.inc   
;include C:\masm32\

.data
	ittrue db "It fail", 0
	crlf db 0dh, 0ah, 0
	space db " ", 0
	pid db 128 DUP (0)

	filename db "test.txt", 0
	
	processpath db 256 DUP (0)
	
.data?
	snapHandle dd ?
	snapHandleSpecific dd ?
	Procie PROCESSENTRY32<>
	Moddie MODULEENTRY32<>
	filehandle dd ? 
	writtenbytes dd ?
	
	; processpath lpExeName 
	pathsize dd ?
	
.code

printProcs proc

	push 0
    push TH32CS_SNAPALL
    call CreateToolhelp32Snapshot
	
	mov snapHandle, eax

    mov edx, sizeof Procie ;need to set size
    mov Procie.dwSize, edx
    push offset Procie
    push [snapHandle]
    call Process32First
	
	push offset pid
	push [Procie.th32ProcessID] ; Process ID
	call dwtoa
	
	push offset pid
	call StdOut
	push offset pid
	call StrLen
	push 0
	push offset writtenbytes
	push eax
	push offset pid
	push [filehandle]
	call WriteFile
	
	push offset space
	call StdOut
	push 0
	push offset writtenbytes
	push 1
	push offset space
	push [filehandle]
	call WriteFile
	
	push offset Procie.szExeFile ; Process name, this is [System Process]
	call StdOut
	push offset Procie.szExeFile
	call StrLen
	push 0
	push offset writtenbytes
	push eax
	push offset Procie.szExeFile
	push [filehandle]
	call WriteFile
	
	push offset crlf
	call StdOut
	push 0
	push offset writtenbytes
	push 2
	push offset crlf
	push [filehandle]
	call WriteFile
	
	push offset Procie
	push [snapHandle]
	call Process32Next
	
	jumphere:
	
	push offset pid
	push [Procie.th32ProcessID] ; Process ID
	call dwtoa
	
	push offset pid
	call StdOut
	push offset pid
	call StrLen
	push 0
	push offset writtenbytes
	push eax
	push offset pid
	push [filehandle]
	call WriteFile
	
	push offset space ; space
	call StdOut
	push 0
	push offset writtenbytes
	push 1
	push offset space
	push [filehandle]
	call WriteFile
	
	push offset Procie.szExeFile ; Process name
	call StdOut
	push offset Procie.szExeFile
	call StrLen
	push 0
	push offset writtenbytes
	push eax
	push offset Procie.szExeFile
	push [filehandle]
	call WriteFile
;----------------------------------------------------------
	
	; HANDLE OpenProcess(
  ; DWORD dwDesiredAccess,
  ; BOOL  bInheritHandle,
  ; DWORD dwProcessId
; );
	
	push Procie.th32ProcessID
	push 0
	push PROCESS_QUERY_LIMITED_INFORMATION
	call OpenProcess
	
; BOOL QueryFullProcessImageNameA(
  ; HANDLE hProcess,
  ; DWORD  dwFlags,
  ; LPSTR  lpExeName,
  ; PDWORD lpdwSize
; );
	
	push offset pathsize
	push offset processpath
	push 0
	; push PROCESS_NAME_NATIVE
	push eax
	call QueryFullProcessImageNameA
	
	push offset space
	call StdOut
	push 0
	push offset writtenbytes
	push 1
	push offset space
	push [filehandle]
	call WriteFile
	
	
	push offset processpath
	call StdOut

	push 0
	push offset writtenbytes
	push pathsize
	push offset processpath
	push [filehandle]
	call WriteFile
	
		push 0
		push offset writtenbytes
		push 2
		push offset crlf
		push [filehandle]
		call WriteFile
	
	
	
	
	
	
	
	
	
	
	
	
	
	;----------------------------------------------------------
		push Procie.th32ProcessID
		push TH32CS_SNAPMODULE
		call CreateToolhelp32Snapshot
		
		mov snapHandleSpecific, eax
	
		mov edx, sizeof Moddie ;need to set size
		mov Moddie.dwSize, edx
		push offset Moddie
		push [snapHandleSpecific]
		call Module32First
	
		; push offset space
		; call StdOut
		; push 0
		; push offset writtenbytes
		; push 1
		; push offset space
		; push [filehandle]
		; call WriteFile
	
		; push offset Moddie.szExePath ; Process path
		; call StdOut
		; push offset Moddie.szExePath
		; call StrLen
		; push 0
		; push offset writtenbytes
		; push eax
		; push offset Moddie.szExePath
		; push [filehandle]
		; call WriteFile
		
		jmp skipfirst

		jumphere2:
		
		; push offset space
		; call StdOut
		; push offset space
		; call StdOut
		; push offset space
		; call StdOut
		; push offset space
		; call StdOut
		push 0
		push offset writtenbytes
		push 1
		push offset space
		push [filehandle]
		call WriteFile
		push 0
		push offset writtenbytes
		push 1
		push offset space
		push [filehandle]
		call WriteFile
		push 0
		push offset writtenbytes
		push 1
		push offset space
		push [filehandle]
		call WriteFile
		push 0
		push offset writtenbytes
		push 1
		push offset space
		push [filehandle]
		call WriteFile
		push 0
		push offset writtenbytes
		push 1
		push offset space
		push [filehandle]
		call WriteFile
		
		; push offset Moddie.szModule
		; call StdOut
		push offset Moddie.szModule
		call StrLen
		push 0
		push offset writtenbytes
		push eax
		push offset Moddie.szModule
		push [filehandle]
		call WriteFile
		
		
		skipfirst:

		; push offset crlf
		; call StdOut
		push 0
		push offset writtenbytes
		push 2
		push offset crlf
		push [filehandle]
		call WriteFile
		
		push offset Moddie
		push [snapHandleSpecific]
		call Module32Next

		cmp eax, FALSE
		jne jumphere2
		
	push offset crlf
	call StdOut
	
	push offset Procie
	push [snapHandle]
	call Process32Next
	
	cmp eax, FALSE
	jne jumphere
	
	ret
printProcs endp

main:

	push 0
	push FILE_ATTRIBUTE_NORMAL
	push CREATE_ALWAYS
	push 0
	push 0
	push GENERIC_ALL
	push offset filename
	call CreateFileA
	
	mov [filehandle], eax
	
	call printProcs
	
	call ExitProcess
end main