include C:\masm32\include\masm32rt.inc

.data

	crlf db 0dh, 0ah, 0
	txtmask db "the_last_question.txt", 0
	
	
.data?
	foundfile WIN32_FIND_DATA<>
	written dd ?
	
.code
main:

	push offset foundfile
	push offset txtmask
	call FindFirstFileA

	push 0
	push FILE_ATTRIBUTE_NORMAL
	push OPEN_EXISTING
	push 0
	push 0
	push GENERIC_WRITE
	push offset foundfile.cFileName
	call CreateFileA

	push eax
	
	;pop eax
	push 0
	push offset written
	push 31
	push offset writeme
	push eax
	call WriteFile
	
	;BOOL ReadFile(
  ;HANDLE                        hFile,
  ;__out_data_source(FILE)LPVOID lpBuffer,
  ;DWORD                         nNumberOfBytesToRead,
  ;LPDWORD                       lpNumberOfBytesRead,
  ;LPOVERLAPPED                  lpOverlapped
;)

;BOOL WriteFile(
  ;HANDLE       hFile,
  ;LPCVOID      lpBuffer,
  ;DWORD        nNumberOfBytesToWrite,
  ;LPDWORD      lpNumberOfBytesWritten,
  ;LPOVERLAPPED lpOverlapped
;)








end main