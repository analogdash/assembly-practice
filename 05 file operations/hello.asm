include C:\masm32\include\masm32rt.inc

.data

	;mypath db 128 DUP(0)
	;searchmaskTXT db "*.txt",0
	;searchmaskALL db "*",0
	;basedirectory db "./",0
	currentdirectory db 260 DUP(0) ; :::WARNING::: UNIDENTIFIED BEHAVIOR IF FILE PATH EXCEEDS MAX_PATH
	;nothingheremsg db "--- End of Files ---", 0
	;nothingheremsg2 db "--- End of Directories	---", 0
	;divider db " - DIRECTORY - ",0
	backslash db "\",0
	currentmaskTXT db 260 DUP (0)
	currentmaskALL db 260 DUP (0)

	crlf db 0dh, 0ah, 0
	
	;currenttxt db "current txt mask is: ",0
	;currentall db "current all mask is: ",0
	
.data?
	foundfile WIN32_FIND_DATA<>
	;handle dd ?
	

.code

;main:

	;push offset mypath
	;call GetAppPath

	;lea edx, [basedirectory]
	;lea ecx, [currentdirectory]
	;L1:
    ;mov  al,[edx]       
    ;mov  BYTE PTR [ecx],al       
    ;inc  edx            
    ;inc  ecx            
    ;cmp al,0
	;jne L1
	
	;push offset basedirectory
	;call StdOut
	;push offset crlf
	;call StdOut
	;push offset currentdirectory
	;call StdOut
	;push offset crlf
	;call StdOut

	;lea ecx, [mypath]
	
	;sfindlast:
	;mov bl, [ecx]
	;cmp bl, 0
	;je sfoundlast
	;inc ecx
	;jmp sfindlast
	;sfoundlast:

	
	
	;mov BYTE PTR [ecx], "/"
	;inc ecx
	;mov BYTE PTR [ecx], "*"

	;push offset mypath
	;call StdOut
	
	;push offset crlf
	;call StdOut
	
	
	;need to push offset currentdirectory first
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
	mov BYTE PTR [edx], "*"
	inc edx
	mov BYTE PTR [edx], "."
	inc edx
	mov BYTE PTR [edx], "t"
	inc edx
	mov BYTE PTR [edx], "x"
	inc edx
	mov BYTE PTR [edx], "t"

	;push offset currenttxt
	;call StdOut
	;push offset currentmaskALL
	;call StdOut
	;push offset crlf
	;call StdOut
	;push offset currentall
	;call StdOut
	;push offset currentmaskTXT
	;call StdOut
	;push offset crlf
	;call StdOut
	

;---------------------------------------- Find all text files
	
	push offset foundfile
	push offset currentmaskTXT
	call FindFirstFileA 	
	push eax
	
	cmp eax, INVALID_HANDLE_VALUE
	je nothinghere
	
	push offset currentdirectory
	call StdOut
	push offset backslash
	call StdOut
	push offset foundfile.cFileName
	call StdOut
	push offset crlf
	call StdOut
	
;------------------------------------------- find next text files
	printerrythang:
	pop eax
	push eax
	push offset foundfile
	push eax
	call FindNextFileA
	
	cmp eax, 0
	je nothinghere
	
	push offset currentdirectory
	call StdOut
	push offset backslash
	call StdOut
	push offset foundfile.cFileName
	call StdOut
	push offset crlf
	call StdOut
	
	jmp printerrythang
	
;---------------------------------------------- done finding textfiles
	nothinghere:
	;push offset nothingheremsg
	;call StdOut
	;push offset crlf
	;call StdOut
	pop eax
;-----------------------------------------------find all directories

	push offset foundfile
	push offset currentmaskALL
	call FindFirstFileA 	
	push eax
	
	cmp eax, INVALID_HANDLE_VALUE
	je nothinghere2

	mov eax, foundfile.dwFileAttributes
	and eax, FILE_ATTRIBUTE_DIRECTORY
	jz printerrythang2
	
	;---------------------------------------- first directory found
	;push offset basedirectory
	;call StdOut
	;push offset foundfile.cFileName
	;call StdOut
	;push offset crlf
	;call StdOut
	
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
	
	
	cmp BYTE PTR [foundfile.cFileName], "."
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
	
	;push offset currentdirectory
	;call StdOut
	;push offset backslash
	;call StdOut
	;push offset foundfile.cFileName
	;call StdOut
	;push offset crlf
	;call StdOut
	
	jmp printerrythang2
	
	nothinghere2:
	;push offset nothingheremsg2
	;call StdOut
	;push offset crlf
	;call StdOut
	
	pop eax
	
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
	lea ecx, [currentdirectory]
	mov BYTE PTR [ecx], "."
	;push offset currentdirectory
	call getFiles
	ret
end main

;GetCL return value at EAX
;1 = successful operation
;2 = no argument exists at specified arg number
;3 = non matching quotation marks
;4 = empty quotation marks