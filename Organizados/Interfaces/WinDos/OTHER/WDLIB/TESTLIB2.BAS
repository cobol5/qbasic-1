DECLARE FUNCTION Execute% (BYVAL PathSeg%, BYVAL PathOff%, BYVAL CommandSeg%, BYVAL CommandOff%)

Path$ = "c:\dos\mem.exe" + CHR$(0)
CommandLine$ = CHR$(0)

Temp% = SETMEM(-655360)

PRINT Execute%(SSEG(Path$), SADD(Path$), SSEG(CommandLine$), SADD(CommandLine$))
SLEEP
SHELL Path$

