include C:\masm32\include\masm32rt.inc

.data
it1 db "HKEY_CLASSES_ROOT",0
; je go1
it2 db "HKEY_CURRENT_CONFIG",0
; je go2
it3 db "HKEY_CURRENT_USER",0
; je go3
it4 db "HKEY_CURRENT_USER_LOCAL_SETTINGS",0
; je go4
it5 db "HKEY_LOCAL_MACHINE",0
; je go5
it6 db "HKEY_PERFORMANCE_DATA",0
; je go6
it7 db "HKEY_PERFORMANCE_NLSTEXT",0
; je go7
it8 db "HKEY_PERFORMANCE_TEXT",0

it9 db "HKEY_USERS",0

.code

main:

mov eax, -7FFFFFFFh

cmp eax, HKEY_CLASSES_ROOT
je go1
cmp eax, HKEY_CURRENT_CONFIG
je go2
cmp eax, HKEY_CURRENT_USER
je go3
;cmp eax, HKEY_CURRENT_USER_LOCAL_SETTINGS
je go4
cmp eax, HKEY_LOCAL_MACHINE
je go5
cmp eax, HKEY_PERFORMANCE_DATA
je go6
;cmp eax, HKEY_PERFORMANCE_NLSTEXT
je go7
;cmp eax, HKEY_PERFORMANCE_TEXT
je go8
cmp eax, HKEY_USERS
je go9

go1: push offset it1
jmp getout
go2: push offset it2
jmp getout
go3: push offset it3
jmp getout
go4: push offset it4
jmp getout
go5: push offset it5
jmp getout
go6: push offset it6
jmp getout
go7: push offset it7
jmp getout
go8: push offset it8
jmp getout
go9: push offset it9

getout:
call StdOut

call ExitProcess
end main