include C:\masm32\include\masm32rt.inc

.data
	currentdirectory db 260 DUP(0) ; :::WARNING::: UNIDENTIFIED BEHAVIOR IF FILE PATH EXCEEDS MAX_PATH
	currentmaskTXT db 260 DUP (0)
	currentmaskALL db 260 DUP (0)
	filetowork db 260 DUP(0)
	crlf db 0dh, 0ah, 0

	arg1 db 128 DUP(0)
	arg2 db 128 DUP(0)
	arg3 db 128 DUP(0)
	ArgErrorMsg db "Error with arguments.",0
	action db 0 ;1 for encryption, 0 for decryption
	
.data?
	foundfile WIN32_FIND_DATA<>


	
.code

loadpath proc
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

getFiles proc
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
	
	lea ecx, [arg1]
	appendmask:
	mov al, [ecx]
	cmp al, 0
		je doneappendingmask
	mov BYTE PTR [edx], al
	inc edx
	inc ecx
	jmp appendmask
	doneappendingmask:

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
	
	;------------------------------------------ FOUND SECOND FILE PUT FILEOPS HERE
	
	call loadpath
	
	push offset filetowork
	call StdOut
	push offset crlf
	call StdOut
	
	;--------------------------------------------------done working on 2nd file
	
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

	push offset arg1
	push 1
	call GetCL
	
	cmp al, 1
	jne argerror ;first argument broken
	
	push offset arg3
	push 3
	call GetCL
	
	cmp al, 2
	jne argerror ;third argument has something going on
	
	push offset arg2
	push 2
	call GetCL
	
	cmp al, 2
	je encryptan ;blank second argument
	
	cmp BYTE PTR [arg2], "/"
	jne argerror
	cmp BYTE PTR [arg2+1], "r"
	jne argerror
	cmp BYTE PTR [arg2+2], "e"
	jne argerror
	cmp BYTE PTR [arg2+3], "s"
	jne argerror
	cmp BYTE PTR [arg2+4], "t"
	jne argerror
	cmp BYTE PTR [arg2+5], "o"
	jne argerror
	cmp BYTE PTR [arg2+6], "r"
	jne argerror
	cmp BYTE PTR [arg2+7], "e"
	jne argerror
	cmp BYTE PTR [arg2+8], 0
	jne argerror

	jmp decryptan

encryptan:
	mov BYTE PTR [action], 1

decryptan:
	
	lea ecx, [currentdirectory]
	mov BYTE PTR [ecx], "." ;----------------- initialize currentdirectory as "."
	call getFiles
	ret
	
argerror:
	push offset ArgErrorMsg
	call StdOut
	ret

end main