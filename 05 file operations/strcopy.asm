include C:\masm32\include\masm32rt.inc

.data

	mypath db 128 DUP(0)


	crlf db 0dh, 0ah, 0
	
.data?

.code
main:

	push offset mypath
	call GetAppPath
	
	push offset mypath
	call StdOut
	
ret
end main