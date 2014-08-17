@echo off

bc setup.bas /O/Ot/G2/S/E/X/Lr/FPi/T/C:512;
link @mksetup.lst

del setup.obj > nul

echo.
apack setup.exe setup.exe
