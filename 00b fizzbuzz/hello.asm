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
	Fizz db "Fizz",0
	Buzz db "Buzz",0
	FizzBuzz db "FizzBuzz", 0
	CountMe db "CountMe", 0

	FirstVar db 50
	
.code
main:
	push MB_OK
	push offset Caption
	
	mov ebx, 15
	mov eax, DWORD PTR [FirstVar]
	xor edx, edx

	idiv ebx

	cmp edx, 0
	je fbz

	mov ebx, 5
	mov eax, DWORD PTR [FirstVar]
	xor edx, edx

	idiv ebx

	cmp edx, 0
	je bz

	mov ebx, 3
	mov eax, DWORD PTR [FirstVar]
	xor edx, edx

	idiv ebx

	cmp edx, 0
	je fz

	push offset CountMe
	jmp lastpart

fz:
	push offset Fizz
	jmp lastpart

bz:
	push offset Buzz
	jmp lastpart

fbz:
	push offset FizzBuzz
	jmp lastpart

lastpart:
	push NULL
	call MessageBoxA
	
	push 0
	call ExitProcess
end main