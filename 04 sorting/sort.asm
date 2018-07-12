include C:\masm32\include\masm32rt.inc
.data

	array db "askjgsiueghoiuawehgiuoHBWEG",0
	
.data?
	
	sortme dd ?
	
.code
main:

lea ecx, [array]

getneggy:
neg BYTE PTR [ecx]
inc ecx
cmp BYTE PTR [ecx], 0
jne getneggy

lea ecx, [array]
mov [sortme], ecx

moveonward:
	inc ecx
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
	jge insert
	
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

lea ecx, [array]
goagain2:
neg BYTE PTR [ecx]
inc ecx
cmp BYTE PTR [ecx], 0
jne goagain2

	push offset array
	call StdOut
	ret
	
end main