include C:\masm32\include\masm32rt.inc    
	
.data
	expl db "explorer.exe", 0
	knl db "kernel32.dll", 0
	loadlib db "LoadLibraryA", 0
	loadedDLL db "test.dll",0
	testdllname db "test.dll",0
	rbytes db 123 DUP (0)
	
	;--------------------
	failedopen db "Failed to open Process", 0
	failedalloc db "Failed to virtual alloc", 0
	failedwritemem db "Failed to write process memory", 0
	failedgetmodhandle db "Failed to getmodhandle", 0
	GetProcAddressfail db "GetProcAddressfail" ,0 
	createthreadfail db "createthreadfail" ,0 
	
	
	
.data?
	Procie PROCESSENTRY32<>
	snapHandle dd ?
	procHandle dd ?
	modhandle dd ?
	baseaddress dd ?
	modaddress dd ?
	readbytes dd ?
.code
main:

	inkey
	inkey
	inkey
	
	; push 0
	; push FILE_ATTRIBUTE_NORMAL
	; push OPEN_EXISTING
	; push 0
	; push 0
	; push GENERIC_READ
	; push offset testdllname
	; call CreateFileA
	
	
	; push 0
	; push offset readbytes
	; push 204800
	; push offset loadedDLL
	; push eax
	; call ReadFile
	
	;------------
	; push offset rbytes
	; push readbytes
	; call dwtoa
	; push offset rbytes
	; call StdOut
	;------------
	
	
	
	

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
	
; LPVOID WINAPI VirtualAllocEx(
  ; _In_     HANDLE hProcess,
  ; _In_opt_ LPVOID lpAddress,
  ; _In_     SIZE_T dwSize,
  ; _In_     DWORD  flAllocationType,
  ; _In_     DWORD  flProtect
; );
	
	push PAGE_EXECUTE_READWRITE ;PAGE_EXECUTE_READWRITE
	push MEM_RESERVE OR MEM_COMMIT
	push 9 ;INSERT SIZE IN BYTES ; strlen of dll name + 1
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
	
	
	
	
	
; BOOL WINAPI WriteProcessMemory(
  ; _In_  HANDLE  hProcess,
  ; _In_  LPVOID  lpBaseAddress,
  ; _In_  LPCVOID lpBuffer,
  ; _In_  SIZE_T  nSize,
  ; _Out_ SIZE_T  *lpNumberOfBytesWritten
; );
	
	push NULL
	push 1024 ; SIZE OF DLL + 1
	push offset loadedDLL ; MUST be ABSOLUTE
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
	
	
	
	
		;LoadLibrary = (LPVOID)GetProcAddress(GetModuleHandle("kernel32.dll"), "LoadLibraryA");
	
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
	
	
; FARPROC WINAPI GetProcAddress(
  ; _In_ HMODULE hModule,
  ; _In_ LPCSTR  lpProcName
; );
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
	
	


	
; HANDLE CreateRemoteThread(
  ; HANDLE                 hProcess,
  ; LPSECURITY_ATTRIBUTES  lpThreadAttributes,
  ; SIZE_T                 dwStackSize,
  ; LPTHREAD_START_ROUTINE lpStartAddress,
  ; LPVOID                 lpParameter,
  ; DWORD                  dwCreationFlags,
  ; LPDWORD                lpThreadId
; );
	
	
	
	;push offset loadedDLL
    ;call LoadLibrary
	
	
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

	
	
	call ExitProcess
end main