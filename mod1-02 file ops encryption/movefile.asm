include C:\masm32\include\masm32rt.inc

.data
	oldname db "the_last_question.txt",0
	newname db "the_first_question.txt",0
.code

main:

push offset newname
push offset oldname
call MoveFile




ret

end main