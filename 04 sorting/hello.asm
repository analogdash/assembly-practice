include C:\masm32\include\masm32rt.inc

.data

	arg1 db 128 DUP(0)
	arg2 db 128 DUP(0)
	arg3 db 128 DUP(0)

	ArgErrorMsg db "Error with arguments.",0
	
	fa db "first asc", 0
	fd db "first des", 0
	sa db "second asc", 0
	sd db "second des", 0

	
.data?

	sortme dd ?

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
	cmp al, 45	; -
	je SecondNexus
	cmp al, 47	; /
	je SecondNexus
	jmp checkonfirst

	
	
SecondNexus:
	mov al, [arg2+1]
	cmp al, 100
		je SecondDesc1
	cmp al, 68
		je SecondDesc1
	cmp al, 97
		je SecondAsc1
	cmp al, 65
		je SecondAsc1
	jmp checkonfirst

SecondDesc1:
	mov al, [arg2+2]
	cmp al, 101
		je SecondDesc2
	cmp al, 69
		je SecondDesc2
	jmp checkonfirst
SecondAsc1:
	mov al, [arg2+2]
	cmp al, 115
		je SecondAsc2
	cmp al, 83
		je SecondAsc2
	jmp checkonfirst
	
SecondDesc2:
	mov al, [arg2+3]
	cmp al, 115
		je SecondDesc3
	cmp al, 83
		je SecondDesc3
	jmp checkonfirst
SecondAsc2:
	mov al, [arg2+3]
	cmp al, 99
		je SecondAsc3
	cmp al, 67
		je SecondAsc3
	jmp checkonfirst
	
SecondDesc3:
	mov al, [arg2+4]
	cmp al, 0
	je SecondDescFinal
	jmp checkonfirst

SecondAsc3:
	mov al, [arg2+4]
	cmp al, 0
	je SecondAscFinal
	jmp checkonfirst
	
checkonfirst:
	mov al, [arg1]
	cmp al, 45	; -
	je FirstNexus
	cmp al, 47	; /
	je FirstNexus
	jmp argerror

FirstNexus:
	mov al, [arg1+1]
	cmp al, 100
		je FirstDesc1
	cmp al, 68
		je FirstDesc1
	cmp al, 97
		je FirstAsc1
	cmp al, 65
		je FirstAsc1
	jmp argerror

FirstDesc1:
	mov al, [arg1+2]
	cmp al, 101
		je FirstDesc2
	cmp al, 69
		je FirstDesc2
	jmp argerror
FirstAsc1:
	mov al, [arg1+2]
	cmp al, 115
		je FirstAsc2
	cmp al, 83
		je FirstAsc2
	jmp argerror
	
FirstDesc2:
	mov al, [arg1+3]
	cmp al, 115
		je FirstDesc3
	cmp al, 83
		je FirstDesc3
	jmp argerror
FirstAsc2:
	mov al, [arg1+3]
	cmp al, 99
		je FirstAsc3
	cmp al, 67
		je FirstAsc3
	jmp argerror
	
FirstDesc3:
	mov al, [arg1+4]
	cmp al, 0
	je FirstDescFinal
	jmp argerror

FirstAsc3:
	mov al, [arg1+4]
	cmp al, 0
	je FirstAscFinal
	jmp argerror

FirstDescFinal:
	lea ecx, [arg2]
	mov [sortme], ecx
	jmp Descending ;Descending dapaat
	
FirstAscFinal:
	lea ecx, [arg2]
	mov [sortme], ecx
	jmp GetSorting ;GetSorting dapat
	
SecondDescFinal:
	lea ecx, [arg1]
	mov [sortme], ecx
	jmp Descending ;Descending dapaat

SecondAscFinal:
	lea ecx, [arg1]
	mov [sortme], ecx
	jmp GetSorting ;GetSorting dapat
	

Descending:	
	mov ecx, [sortme]
	getneggy:
	neg BYTE PTR [ecx]
	inc ecx
	cmp BYTE PTR [ecx], 0
	jne getneggy
	
GetSorting:
	mov ecx, [sortme]
;SORTING ALGO STARTS HERE

moveonward:
	inc ecx
	xor eax,eax
	mov al, BYTE PTR [ecx]
	cmp al, 0
		je donesorting
	mov edx, ecx

movedown:
	cmp edx, [sortme]
		je insertbegin
	dec edx
	cmp al, BYTE PTR [edx]
	jl movedown
insert:
	inc edx
insertbegin:
	mov bl, BYTE PTR [edx]
	mov BYTE PTR [edx], al
	mov al,bl
	cmp edx,ecx
	je moveonward
	jne insert

donesorting:
	mov ecx, [sortme]
	cmp BYTE PTR [ecx], 128
		jb printit
	getneggy2:
	neg BYTE PTR [ecx]
	inc ecx
	cmp BYTE PTR [ecx], 0
	jne getneggy2
	
printit:
	push [sortme]
	call StdOut
	jmp endme
	
argerror:
	push offset ArgErrorMsg
	call StdOut
endme:
	ret
end main

;GetCL return value at EAX
;1 = successful operation
;2 = no argument exists at specified arg number
;3 = non matching quotation marks
;4 = empty quotation marks