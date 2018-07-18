include C:\masm32\include\masm32rt.inc

.data

	msg db "Today is ", 128 DUP (0)
	
	sun db "Sunday! Good ", 0
	mon db "Monday! Good ",0
	tue db "Tuesday! Good ", 0
	wed db "Wednesday! Good ", 0
	thurs db "Thursday! Good ", 0
	fri db "Friday! Good ", 0
	sat db "Saturday! Good ", 0
	
	morn db "Morning, Dashley! Are you doing good today?", 0
	aft db "Afternoon, Dashley! Are you doing good today?", 0
	eve db "Evening, Dashley! Are you doing good today?", 0
	
	msgtitle db "Dashley's daily reminder",0
	
.data?

	systime SYSTEMTIME <>

	
.code

main:
	
	push offset systime
	call GetLocalTime
	
	lea ecx, [msg]
	add ecx, 9 ;move to the end of the string
	
	lea edx, [sun]
	cmp systime.wDayOfWeek, 0
	je dayset
	lea edx, [mon]
	cmp systime.wDayOfWeek, 1
	je dayset
	lea edx, [tue]
	cmp systime.wDayOfWeek, 2
	je dayset
	lea edx, [wed]
	cmp systime.wDayOfWeek, 3
	je dayset
	lea edx, [thurs]
	cmp systime.wDayOfWeek, 4
	je dayset
	lea edx, [fri]
	cmp systime.wDayOfWeek, 5
	je dayset
	lea edx, [sat]
	dayset:
	
	copydaygreet:
	mov al, [edx]
	cmp al, 0
		je daygreet
	mov BYTE PTR [ecx], al
	inc ecx
	inc edx
	jmp copydaygreet
	daygreet:
	
	lea edx, [eve]
	cmp systime.wHour, 4
	jl timeset
	lea edx, [morn]
	cmp systime.wHour, 12
	jl timeset
	lea edx, [aft]
	cmp systime.wHour, 18
	jl timeset
	lea edx, [eve]
	timeset:

	copytimegreet:
	mov al, [edx]
	cmp al, 0
		je timegreet
	mov BYTE PTR [ecx], al
	inc ecx
	inc edx
	jmp copytimegreet
	timegreet:

	;push offset msg
	;call StdOut
	
	mov ebx, 10
	
	querying:
	dec ebx
	push MB_YESNO
	push offset msgtitle
	push offset msg
	push 0
	call MessageBoxA
	
	cmp ebx, 0
	je wedonehere
	
	cmp eax, IDNO
	je querying
	
	wedonehere:
	
	ret
end main
