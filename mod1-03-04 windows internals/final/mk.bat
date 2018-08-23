@echo off
set arg1=%1
ml.exe /c /coff %arg1%.asm || exit /B
link.exe %arg1%.obj /subsystem:console /opt:noref