include C:\masm32\include\masm32rt.inc    
	
.data
	classname db "TestWindow",0 
	windowtitle db "Test Window Title", 0
	editclassname db "EDIT",0
	buttclassname db "BUTTON", 0
	runtext db "Run",0
	writetext db "Write",0
	blank db 0
	
	runid dd 0FACEh
	writeid dd 0BEEFh
	
	text1id dd 0ABEh
	text2id dd 0BEDh
	
	text1conts db 128 DUP (0)
	text2conts db 128 DUP (0)
	
	notexist db "File does not exist", 0

.data?
	windclass WNDCLASS <>
	message MSG <>
	instance_handle dd ?
	window_handle dd ?
	cursor_handle dd ?
	icon_handle dd ?
	button_handle dd ?
	writtenbytes dd ?

.code

; WINDOW PROCESS
wndproc proc hWin:dword, uMsg:dword, wParam:dword,lParam:dword
  
	.if uMsg == WM_DESTROY

		invoke 	PostQuitMessage, 0

		xor	eax, eax
		ret
	.elseif uMsg == WM_COMMAND
	
		mov eax, wParam
		cmp eax, 0FACEh
		je dorun
		cmp eax,0BEEFh
		je dowrite
		jmp nah
		
		dorun:
		push 128
		push offset text1conts
		push text1id
		push window_handle
		call GetDlgItemTextA
		
		push 128
		push offset text2conts
		push text2id
		push window_handle
		call GetDlgItemTextA
		
		call GetDesktopWindow
		
		push SW_SHOWNORMAL
		push 0
		push offset text2conts
		push offset text1conts
		push 0
		push eax ;return value of GetDesktopWindow
		call ShellExecuteA
		ret
	
	
		dowrite:
		push 128
		push offset text1conts
		push text1id
		push window_handle
		call GetDlgItemTextA
		
		push 128
		push offset text2conts
		push text2id
		push window_handle
		call GetDlgItemTextA
		
			
		push 0
		push FILE_ATTRIBUTE_NORMAL
		push OPEN_EXISTING
		push 0
		push 0
		push GENERIC_ALL
		push offset text1conts ;filetowork goes here
		call CreateFileA
			
		cmp eax, INVALID_HANDLE_VALUE
		je nofile
		
		push eax
		push eax
		push offset text2conts
		call lstrlenA
		pop ecx
			
		push 0
		push offset writtenbytes
		push eax ;numbytes
		push offset text2conts
		push ecx
		call WriteFile
		
		call CloseHandle 
				
		ret
		jmp nah
		nofile:
		
		push 0
		push offset notexist
		push offset notexist
		push 0
		call MessageBoxA
		ret
		nah:
	
	.else
	
		push lParam
		push wParam
		push uMsg
		push hWin
		call DefWindowProc

	.endif
	


	ret
wndproc endp

main:
	;get own process handle
	push NULL
	call GetModuleHandle
	mov instance_handle, eax
	
	;get cursor handle
	push IDC_ARROW
	push instance_handle
	call LoadCursor
	mov cursor_handle, eax

	;get icon handle
	push IDI_APPLICATION
	push instance_handle
	call LoadIcon
	mov icon_handle, eax
	
	;prepare WNDCLASS structure
	;mov windclass.cbSize, sizeof WNDCLASSEX
    mov windclass.style, 0; or CS_BYTEALIGNWINDOW
    mov windclass.lpfnWndProc, offset wndproc
    mov windclass.cbClsExtra, 0;
    mov windclass.cbWndExtra, 0;
	
	push instance_handle
	pop windclass.hInstance
	
    mov windclass.hIcon, 0;
	
	push cursor_handle
    pop windclass.hCursor; find a cursor handle
	
    mov windclass.hbrBackground, COLOR_BTNFACE + 1; paint background?
    mov windclass.lpszMenuName, 0; give a resource name?
    mov windclass.lpszClassName, offset classname;
	
	;register class
	push offset windclass
	call RegisterClass
	
	;Create Window
	push 0
	push instance_handle
	push 0 ; menu
	push 0 ; parent
	push 400 ; h
	push 400 ; w
	push 212 ; y
	push 440 ; x
	push WS_OVERLAPPEDWINDOW 
	push offset windowtitle
	push offset classname
	push WS_EX_APPWINDOW
	call CreateWindowExA
	
	mov window_handle, eax
	
	;show window
	push SW_SHOWDEFAULT
	push window_handle
	call ShowWindow
	
	;Update Window
	push window_handle
	call UpdateWindow
	
	;--------- Children windows
	
	;BUTTON for RUN
	push 0
	push 0 ;instance handle
	push runid ;menu ID of 
	push window_handle ;parent
	push 80 ;h
	push 100 ;w
	push 200 ;y
	push 50 ;x
	push WS_CHILD OR WS_BORDER
	push offset runtext
	push offset buttclassname
	push 0
	call CreateWindowExA
	mov button_handle, eax
	push SW_SHOWDEFAULT
	push button_handle
	call ShowWindow
	push button_handle
	call UpdateWindow

	;BUTTON for WRITE
	push 0
	push 0 ;instance handle
	push writeid ;menu ID of 
	push window_handle ;parent
	push 80 ;h
	push 100 ;w
	push 200 ;y
	push 250 ;x
	push WS_CHILD OR WS_BORDER
	push offset writetext
	push offset buttclassname
	push 0
	call CreateWindowExA
	mov button_handle, eax
	push SW_SHOWDEFAULT
	push button_handle
	call ShowWindow
	push button_handle
	call UpdateWindow

	;FIRST TEXT BOX
	push 0
	push 0 ;instance handle
	push text1id ;menu ID of 
	push window_handle ;parent
	push 20 ;h
	push 300 ;w
	push 50 ;y
	push 40 ;x
	push WS_CHILD OR WS_BORDER
	push offset blank
	push offset editclassname
	push 0
	call CreateWindowExA
	mov button_handle, eax
	push SW_SHOWDEFAULT
	push button_handle
	call ShowWindow
	push button_handle
	call UpdateWindow

	;SECOND TEXT BOX
	push 0
	push 0 ;instance handle
	push text2id ;menu ID of 
	push window_handle ;parent
	push 20 ;h
	push 300 ;w
	push 120 ;y
	push 40 ;x
	push WS_CHILD OR WS_BORDER
	push offset blank
	push offset editclassname
	push 0
	call CreateWindowExA
	mov button_handle, eax
	push SW_SHOWDEFAULT
	push button_handle
	call ShowWindow
	push button_handle
	call UpdateWindow
	
	;MESSAGE LOOP
	moremessaging:
	push 0
	push 0
	push 0
	push offset message 
	call GetMessage
	
	cmp eax, 0
	je quit
	
	push offset message
	call TranslateMessage
	
	push offset message
	call DispatchMessage
	
	jmp moremessaging
	
	
	quit:
	call ExitProcess
end main