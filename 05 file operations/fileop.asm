include C:\masm32\include\masm32rt.inc

.data

	currentdirectory db 260 DUP(0) ; :::WARNING::: UNIDENTIFIED BEHAVIOR IF FILE PATH EXCEEDS MAX_PATH
	backslash db "\",0
	currentmaskTXT db 260 DUP (0)
	currentmaskALL db 260 DUP (0)

	crlf db 0dh, 0ah, 0
	
	txtmask db "hello there.txt", 0
	
	writeme db 0dh, 0ah, "dashley wuz here - " ;-------------- the order of declaration of these two memory blocks is important
	timenow db "2000-00-00",0 ;------------- previous string is designed to overflow into this one
	
	infectmsg db "Do you want to infect this file?", 0
	infecttitle db "Infection query", 0
	
.data?
	foundfile WIN32_FIND_DATA<>
	systime SYSTEMTIME <>
	written dd ?
.code

;-----------------------------------------------
refreshtime proc

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
	ret

refreshtime endp

appendsig proc

	push 0
	push FILE_ATTRIBUTE_NORMAL
	push OPEN_EXISTING
	push 0
	push 0
	push GENERIC_WRITE
	push offset foundfile.cFileName
	call CreateFileA
	;call StdOut
	
	push eax
	
	push FILE_END
	push 0
	push 0
	push eax
	call SetFilePointer
	
	pop eax
	push 0
	push offset written
	push 31
	push offset writeme
	push eax
	call WriteFile
	
	ret
appendsig endp




;---------------------------------------------------
main:



	push offset foundfile
	push offset txtmask
	call FindFirstFileA

	push MB_YESNO
	push offset infecttitle
	push offset infectmsg
	push 0
	call MessageBoxA
	
	cmp eax, IDYES
	je doappend
	
	push offset foundfile.cFileName
	call DeleteFileA
	jmp done
	
	doappend:
	call appendsig
	done:

	ret
end main
