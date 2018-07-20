.386
.model flat, stdcall
option casemap:none

include C:\masm32\include\windows.inc
include C:\masm32\include\kernel32.inc
include C:\masm32\include\user32.inc
include C:\masm32\include\masm32.inc
includelib C:\masm32\lib\kernel32.lib
includelib C:\masm32\lib\user32.lib
includelib C:\masm32\lib\masm32.lib

.data
	Prompt db "Input here:",0
	Input db 128 DUP(0) ;THIS IS IRRESPONSIBLE. Any input greater than 128 chars will overrite final 0 terminater and into adjacent memory
	
	;messages
	OnlyNumbers db "Please enter only numbers",10,0
	TooBig db "Entered Number is too big. 5 digits only", 10, 0
	Oddish db "That number is odd!",10,0
	Evenish db "That number is even!", 10, 0
	Divisor db 2
	char db "&",10,0
	lowend db 48
	highend db 57
	
.code
main:
	push offset Prompt
	call StdOut

	push offset 128
	push offset Input
	call StdIn

	mov al, BYTE PTR [Input] ;this is to check if an empty string was inputted
	cmp al,0
	je main
	
	lea ebx, [Input]

reiterate:
	mov al, BYTE PTR [ebx]
	
	;;; TESTING BLOCK
	;mov ah, [lowend]
	;mov [char], ah
	;xor ah,ah
	
	;push offset char
	;call StdOut
	
	cmp al, 0
	je endofstring ;This must be the first compare. Checks end of string
	
	cmp al, 48
	jl tryagain ;checks for letters or symbols below 0
	
	cmp al, 57
	jg tryagain ;checks for letters or symbols above 9

	inc ebx
	sub ebx,6 ;6th character must be null
	lea edx, Input
	cmp ebx, edx
	je InputTooBig
	add ebx,6
	jmp reiterate
	
endofstring:
	dec ebx
	mov edx, 0 ;edx needs to be initialized as zero for idiv
	mov eax, 0
	mov al, BYTE PTR [ebx]
	mov ecx, 2
	idiv ecx
	cmp edx, 0
	je ItsEven
	jne ItsOdd
	
tryagain:
	push offset OnlyNumbers
	call StdOut
	jmp ClearInput
InputTooBig:
	push offset TooBig
	call StdOut
	jmp ClearInput
ClearInput:
	mov ecx,100
	lea eax, [Input]
	l1:;this clears the input
		mov BYTE PTR [eax], 0
		inc eax
	loop l1
	jmp main
ItsEven:
	push offset Evenish
	jmp Ending
ItsOdd:
	push offset Oddish
	jmp Ending
Ending:
	call StdOut
	ret

end main