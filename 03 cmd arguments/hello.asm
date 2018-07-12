include C:\masm32\include\masm32rt.inc

.data

	arg1 db 128 DUP(0)
	arg2 db 128 DUP(0)
	arg3 db 128 DUP(0)
	ArgErrorMsg db "Error with arguments.",0
	outpoot db 128 DUP(0)

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
	jne argerror ;third argument has something going on
	
	mov al, [arg2]
	cmp al, 45
	je continue
	cmp al, 47
	je continue
	jmp argerror

continue:
	lea ecx, [arg1]
	lea edx, [outpoot]
	mov al, [arg2+2]
	cmp al, 0
	jne argerror
	mov al, [arg2+1]
	cmp al, 117
	je makeitbigger
	cmp al, 85
	je makeitbigger
	cmp al, 108
	je makeitsmaller
	cmp al, 76
	je makeitsmaller
	jmp argerror

makeitsmaller:
	mov al, BYTE PTR [ecx]
	cmp al, 0
	je endthefunk
	cmp al, 90
	jg dontsmallit
	cmp al, 65
	jl dontsmallit
	add al, 32
dontsmallit:
	mov BYTE PTR [edx], al
	inc ecx
	inc edx
	jmp makeitsmaller
	
makeitbigger:
	mov al, BYTE PTR [ecx]
	cmp al, 0
	je endthefunk
	cmp al, 122
	jg dontbigit
	cmp al, 97
	jl dontbigit
	sub al, 32
dontbigit:
	mov BYTE PTR [edx], al
	inc ecx
	inc edx
	jmp makeitbigger

argerror:
	push offset ArgErrorMsg
	call StdOut

endthefunk:
	push offset outpoot
	call StdOut
	
endme:
	ret
end main

;GetCL return value at EAX
;1 = successful operation
;2 = no argument exists at specified arg number
;3 = non matching quotation marks
;4 = empty quotation marks