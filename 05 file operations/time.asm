include C:\masm32\include\masm32rt.inc

.data
	timenow db "2000-00-00",0
	yrs db 120 DUP (0)
	
.data?
	systime SYSTEMTIME <>
	
	
.code
main:

	push offset systime
	call GetLocalTime
	
	xor eax, eax
	mov ax, systime.wYear
	push offset timenow
	push eax
	call dwtoa
	
	mov BYTE PTR [timenow+4], "-"
	
	xor eax, eax
	mov ax, systime.wMonth
	
	cmp ax, 9
	jg highmonth
	push offset timenow+6
	push eax
	call dwtoa
	jmp donemonth
	highmonth:
		push offset timenow+5
		push eax
		call dwtoa
	donemonth:
	
	mov BYTE PTR [timenow+7], "-"
	
	xor eax, eax
	mov ax, systime.wDay
	cmp ax, 9
	jg highday
	push offset timenow+9
	push eax
	call dwtoa
	jmp doneday
	highday:
		push offset timenow+8
		push eax
		call dwtoa
	doneday:
	
	push offset timenow
	call StdOut
	
	ret
	
end main