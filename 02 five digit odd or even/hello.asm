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
	Input db 100 DUP(0) ;THIS IS VERY IRRESPONSIBLE. Any input greater than 100 chars will overrite final 0 terminater and into adjacent memory
	
	;messages
	OnlyNumbersLow db "Please enter only numbers LOW",10,0
	OnlyNumbersHigh db "Please enter only numbers HIGH",10,0
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

	push offset 100
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
	je endofstring ;THIS MUST BE ABOVE THE jl jump otherwise 0 triggers tryagain low!!
	
	cmp al, 48
	jl tryagainlow ;THIS KEEPS TRIGGERING FOR SOME REASON THIS IS THE BROKEN PART NVM I FIXED IT
	
	cmp al, 57
	jg tryagainhigh
	
	;sub al, 48
	;cmp al, 9
	;jb tryagainlow
	;add al, 48
	

	inc ebx
	sub ebx,6
	lea edx, Input
	cmp ebx, edx
	je InputTooBig
	add ebx,6
	jmp reiterate
	
endofstring:
	dec ebx
	mov edx, 0 ;this is necessary for some reason
	mov eax, 0
	mov al, BYTE PTR [ebx]
	mov ecx, 2
	idiv ecx
	cmp edx, 0
	je ItsEven
	jne ItsOdd
	
tryagainlow:
	push offset OnlyNumbersLow
	call StdOut
	jmp ClearInput
tryagainhigh:
	push offset OnlyNumbersHigh
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