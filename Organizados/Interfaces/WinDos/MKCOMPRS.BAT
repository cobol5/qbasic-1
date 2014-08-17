@echo off

bc compress.bas /e/O/Ot/G2/S/Fs/Lr/FPi/T/C:512;
bc mouse.bas /O/Ot/G2/S/Fs/Lr/FPi/T/C:512;

link @mkcomprs.lst

del compress.obj > nul
del mouse.obj > nul

echo.
apack compress.exe compress.exe
