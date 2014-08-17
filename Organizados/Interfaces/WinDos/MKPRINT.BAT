@echo off

bc print.bas /e/O/Ot/G2/S/Fs/Lr/FPi/T/C:512;
bc mouse.bas /O/Ot/G2/S/Fs/Lr/FPi/T/C:512;

link @mkprint.lst

del mouse.obj > nul
del print.obj > nul

echo.
apack print.exe print.exe
