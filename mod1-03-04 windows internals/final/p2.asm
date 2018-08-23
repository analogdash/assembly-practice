include C:\masm32\include\masm32rt.inc

.data
	expl db "explorer.exe", 0
	knl db "kernel32.dll", 0
	loadlib db "LoadLibraryA", 0
	rbytes db 123 DUP (0)
	ittrue db "It fail", 0
	crlf db 0dh, 0ah, 0
	space db " ", 0
	pid db 128 DUP (0)

      msgNoArgs           db "Error! No argument is inputted. Please try again.", 0
      msgInvalidArgs      db "Error! Argument/s inputted invalid.", 0


  argFilename         db "/l=",       0
  argDLLName          db "/i=",       0
firstArgument       db 50 DUP       (?)
  secondArgument      db 50 DUP       (?)
txtFilenameBuffer   db 50 DUP       (?)
  dllNameBuffer       db 50 DUP       (?)

		processpath db 256 DUP (0)

	;--------------------
	failedopen db "Failed to open Process", 0
	failedalloc db "Failed to virtual alloc", 0
	failedwritemem db "Failed to write process memory", 0
	failedgetmodhandle db "Failed to getmodhandle", 0
	GetProcAddressfail db "GetProcAddressfail" ,0
	createthreadfail db "createthreadfail" ,0



.data?
	snapHandleSpecific dd ?
	Procie PROCESSENTRY32<>
	snapHandle dd ?
	procHandle dd ?
	modhandle dd ?
	baseaddress dd ?
	modaddress dd ?
	readbytes dd ?
	Moddie MODULEENTRY32<>
	filehandle dd ?
	writtenbytes dd ?
  pathsize dd ?
.code
main:

inkey
inkey

;----------------------------------------------------------------------------;
;                              Get Arguments                                 ;
;----------------------------------------------------------------------------;

push offset secondArgument                                                   ; Get the second argument
push 2
call GetCL

push offset firstArgument                                                    ; Get the first argument
push 1
call GetCL

cmp eax, 2
je _errorNoArgs

;----------------------------------------------------------------------------;
;                              Set Arguments                                 ;
;----------------------------------------------------------------------------;

;Convert all arguments to lowercase
push offset firstArgument
call szLower
push offset secondArgument
call szLower

_firstArgument:
;Check the commandlines for first argument
push offset argFilename
push offset firstArgument
push 1
call InString
cmp eax, 1
je _setFirstArgumentFileName

push offset argDLLName
push offset firstArgument
push 1
call InString
cmp eax, 1
je _setFirstArgumentDLLName
jmp _errorInvalidArgument

_setFirstArgumentFileName:
push offset argFilename
push offset txtFilenameBuffer
push offset firstArgument
call szRemove
jmp _secondArgument

_setFirstArgumentDLLName:
push offset argDLLName
push offset dllNameBuffer
push offset firstArgument
call szRemove
jmp _secondArgument

_secondArgument:
lea eax, [secondArgument]
mov dl, BYTE PTR [eax ]
cmp dl, 0
je _checking

push offset argFilename
push offset secondArgument
push 1
call InString
cmp eax, 1
je _setSecondArgumentFileName

push offset argDLLName
push offset secondArgument
push 1
call InString
cmp eax, 1
je _setSecondArgumentDLLName
jmp _checking

_setSecondArgumentFileName:
push offset argFilename
push offset txtFilenameBuffer
push offset secondArgument
call szRemove
jmp _checking

_setSecondArgumentDLLName:
push offset argDLLName
push offset dllNameBuffer
push offset secondArgument
call szRemove
jmp _checking

_checking:
push offset txtFilenameBuffer
call StrLen

.IF eax == 0
  print "No write command"
.ELSE
  print "Write to textfile"

  call printProcs

.ENDIF

push offset dllNameBuffer
call StrLen

.IF eax == 0
  print "No dll inject command"
.ELSE
  print "Inject Command"
  call injectMe
.ENDIF

push 0
call ExitProcess
;===========================================;
;  GETTING PROCESSES AND PRINTING IN NOTEPAD;
;===========================================;
printProcs proc

push 0
push FILE_ATTRIBUTE_NORMAL
push CREATE_ALWAYS
push 0
push 0
push GENERIC_ALL
push offset txtFilenameBuffer
call CreateFileA

mov [filehandle], eax

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

	; push Procie.th32ProcessID
	; push 0
	; push PROCESS_QUERY_LIMITED_INFORMATION
	; call OpenProcess

	; push offset pathsize
	; push offset processpath
	; push 0
	; ; push PROCESS_NAME_NATIVE
	; push eax
	; call QueryFullProcessImageNameA

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

		push offset space
		call StdOut
		push 0
		push offset writtenbytes
		push 1
		push offset space
		push [filehandle]
		call WriteFile

		push offset Moddie.szExePath ; Process path
		call StdOut
		push offset Moddie.szExePath
		call StrLen
		push 0
		push offset writtenbytes
		push eax
		push offset Moddie.szExePath
		push [filehandle]
		call WriteFile

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

;=========================;
; INJECTION OF DLL LIBRARY;
;=========================;
injectMe proc

	push 0
    push TH32CS_SNAPALL
    call CreateToolhelp32Snapshot

	mov snapHandle, eax

	mov edx, sizeof Procie ;need to set size
    mov Procie.dwSize, edx
    push offset Procie
    push [snapHandle]
    call Process32First

	mainloop:

	push offset Procie
	push [snapHandle]
	call Process32Next

	push offset Procie.szExeFile
	push offset expl
	call lstrcmpA
	cmp eax, 0
	jne mainloop

	;----------------
	push offset Procie.szExeFile
	call StdOut
	;----------------
	; Explorer.exe is found

	push Procie.th32ProcessID
	push FALSE
	push PROCESS_CREATE_THREAD OR PROCESS_QUERY_INFORMATION OR PROCESS_VM_READ OR PROCESS_VM_WRITE OR PROCESS_VM_OPERATION
	call OpenProcess
	mov procHandle, eax

	;--------------------
	cmp eax, 0
	jne succeedopen

	push offset failedopen
	call StdOut

	succeedopen:
	;--------------------

	push PAGE_EXECUTE_READWRITE ;PAGE_EXECUTE_READWRITE
	push MEM_RESERVE OR MEM_COMMIT
	push 1024 ;INSERT SIZE IN BYTES ; strlen of dll name + 1
	push 0
	push procHandle
	call VirtualAllocEx

	mov baseaddress, eax ;?????? Pointer

	;--------------------
	cmp eax, 0
	jne succeedopen2

	push offset failedopen
	call StdOut

	succeedopen2:
	;--------------------
	push NULL
	push 1024 ; SIZE OF DLL + 1
	push offset dllNameBuffer
	push baseaddress
	push procHandle
	call WriteProcessMemory

	;--------------------
	cmp eax, 0
	jne succeedopen3

	push offset failedwritemem
	call StdOut

	succeedopen3:
	;--------------------

	push offset knl
	call GetModuleHandle
	mov modhandle, eax

	;--------------------
	cmp eax, 0
	jne succeedopen4

	push offset failedgetmodhandle
	call StdOut

	succeedopen4:
	;--------------------

	push offset loadlib
	push modhandle
	call GetProcAddress
	mov modaddress, eax

	;--------------------
	cmp eax, 0
	jne succeedopen5

	push offset GetProcAddressfail
	call StdOut

	succeedopen5:
	;--------------------

	push NULL
	push NULL
	push baseaddress
	push modaddress
	push NULL
	push NULL
	push procHandle
	call CreateRemoteThread

	;--------------------
	cmp eax, 0
	jne succeedopen6

	push offset createthreadfail
	call StdOut

	succeedopen6:
	;--------------------
    ;push offset dllNameBuffer
    ;call LoadLibrary

    ret
injectMe endp

_errorNoArgs:
push offset msgNoArgs
call StdOut
push 0
call ExitProcess

_errorInvalidArgument:
PUSH offset msgInvalidArgs
call StdOut
push 0
call ExitProcess

end main
