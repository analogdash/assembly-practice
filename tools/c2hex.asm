include C:\masm32\include\masm32rt.inc    
	
.data

 buf db 9 DUP (0)

.data?



.code
main:

	push offset buf
	push DRIVE_REMOVABLE


	call dw2hex
	
	push offset buf
	call StdOut
	
	call ExitProcess
end main

;HKEY_LOCAL_MACHINE 8000 0002h
;HKEY_USERS 8000 0003h