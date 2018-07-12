.386
.model flat, stdcall
option casemap:none

include C:\masm32\include\windows.inc
include C:\masm32\include\kernel32.inc
include C:\masm32\include\user32.inc
includelib C:\masm32\lib\kernel32.lib
includelib C:\masm32\lib\user32.lib

.data
	Caption db "Test",0
	HelloWorld db "Hello World!",0

	FirstVar db 30
	
.code
main:
	
	mov eax, 30



	push MB_OK
	push offset Caption
	push offset FirstVar
	push NULL
	call MessageBoxA
	
	push 0
	call ExitProcess
end main