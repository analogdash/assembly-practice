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
	Caption db "Input here:",0
	Input db 100 DUP(0)
	Output db 100 DUP(0)
.code
main:
	push offset Caption
	call StdOut

	push offset 100
	push offset Input
	call StdIn

	lea ebx, [Input]
	lea ecx, [Output]

iterate:
	mov al, BYTE PTR [ebx]
	cmp al, 0
	je ending
	inc ebx

	cmp al, 65
	jl passon
	cmp al, 90
	jle isanupper
	cmp al, 97
	jl passon
	cmp al, 122
	jle isalower

passon:
	mov BYTE PTR [ecx],al
	inc ecx
	jmp iterate

isanupper:
	mov BYTE PTR [ecx], al
	add BYTE PTR [ecx], 32
	inc ecx
	jmp iterate

isalower:
	mov BYTE PTR [ecx],al
	sub BYTE PTR [ecx], 32
	inc ecx
	jmp iterate

ending:
	push offset Output
	call StdOut
	ret

end main