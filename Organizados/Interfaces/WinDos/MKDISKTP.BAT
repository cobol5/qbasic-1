@echo off

bc disktop.bas /e/O/Ot/G2/S/Fs/Lr/FPi/T/C:512;
bc mouse.bas /O/Ot/G2/S/Fs/Lr/FPi/T/C:512;
bc graphics.bas /O/Ot/G2/S/Fs/Lr/FPi/T/C:512;
bc fileio.bas /e/x/O/Ot/G2/S/Fs/Lr/FPi/T/C:512;
bc reg.bas /O/Ot/G2/S/Fs/Lr/FPi/T/C:512;

link @mkdisktp.lst

del disktop.obj > nul
del mouse.obj > nul
del graphics.obj > nul
del fileio.obj > nul
del reg.obj > nul

echo.
apack di_main.exe di_main.exe
