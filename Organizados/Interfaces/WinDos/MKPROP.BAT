@echo off

bc property.bas /e/O/Ot/G2/S/Fs/Lr/FPi/T/C:512;
bc mouse.bas /O/Ot/G2/S/Fs/Lr/FPi/T/C:512;

link @mkprop.lst

del property.obj > nul
del mouse.obj > nul

echo.
apack property.exe property.exe
