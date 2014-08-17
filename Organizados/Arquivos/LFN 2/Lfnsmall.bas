
TYPE dos
F AS STRING * 128
R AS STRING * 95
U AS STRING * 1
END TYPE

DIM SHARED LX(1 TO 3) AS dos
COMMON SHARED errnum, LFX, true, false
ON ERROR GOTO errhand
true = -1: false = 0: LFX = 3
FOR i = 1 TO LFX: LX(i).U = " ": NEXT i

testlfn

errhand:
FOR i = 1 TO LFX: lfnclose (i): NEXT i
PRINT ERR: BEEP: STOP
RESUME NEXT

SUB lfnchdir (LFW AS STRING)
a$ = LFNS$(LFW)
SHELL "dir " + LFNQ$((a$)) + " >c:\temp\temp.txt"
LFF = FREEFILE
OPEN "c:\temp\temp.txt" FOR INPUT AS LFF
FOR i = 1 TO 4: LINE INPUT #LFF, b$: NEXT i
CLOSE LFF
KILL "c:\temp\temp.txt"
IF INSTR(b$, a$) = 0 THEN LFNER (176)
IF MID$(a$, 2, 1) = ":" THEN
SHELL LEFT$(a$, 2)
SHELL "chdir " + LFNQ$(LFNS$(MID$(a$, 3)))
ELSE
SHELL "chdir " + LFNQ$(LFNS$(a$))
END IF
END SUB

SUB lfnclose (LFN AS INTEGER)
IF LX(LFN).U = " " THEN EXIT SUB
CLOSE LFN
IF LX(LFN).U = "*" THEN a = lfnrename(LX(LFN).R, LX(LFN).F)
LX(LFN).U = " "
END SUB

SUB lfncloseall
FOR i = 1 TO LFX: lfnclose (i): NEXT i
END SUB

FUNCTION LFND$ (LFS AS STRING)
a$ = LFS
IF INSTR(a$, "\") THEN p$ = LFNDP$(a$)
a$ = LFNDR$(a$)
LFND$ = p$ + a$
END FUNCTION

FUNCTION LFNDH$ (LFS AS STRING)
a$ = LFS
DO
x = INSTR(a$, "\")
IF x = 0 THEN EXIT DO
b$ = b$ + LEFT$(a$, x)
a$ = MID$(a$, x + 1)
LOOP
LFNDH$ = b$
END FUNCTION

FUNCTION LFNDP$ (LFS AS STRING)
U$ = LFNDH$(LFS)
IF LFNDT(LFS) THEN
ix = INSTR(U$, "\")
D$ = LEFT$(U$, ix - 1)
H$ = D$
U$ = MID$(U$, ix)
p$ = LFNDS$(H$, "")
ELSE
ix = INSTR(U$, "\")
D$ = D$ + LEFT$(U$, ix)
U$ = MID$(U$, ix + 1)
ix = INSTR(U$, "\")
IF ix = 0 THEN LFNER (187)
H$ = LEFT$(U$, ix - 1)
p$ = D$ + p$ + LFNDS$(H$, D$) + "\"
END IF
DO
ix = INSTR(U$, "\")
IF ix = LEN(U$) THEN EXIT DO
D$ = D$ + LEFT$(U$, ix)
U$ = MID$(U$, ix + 1)
ix = INSTR(U$, "\")
IF ix = 0 THEN LFNER (187)
H$ = LEFT$(U$, ix - 1)
p$ = p$ + LFNDS$(H$, p$) + "\"
LOOP
LFNDP$ = p$
END FUNCTION

FUNCTION LFNDR$ (LFS AS STRING)
a$ = LFS
SHELL "dir " + LFNQ$(LFNS$(LFS)) + " >c:\temp\temp.txt"
LFF = FREEFILE
OPEN "c:\temp\temp.txt" FOR INPUT AS LFF
FOR i = 1 TO 5: LINE INPUT #LFF, a$: NEXT i
LINE INPUT #LFF, a$
CLOSE LFF
KILL "c:\temp\temp.txt"
a$ = LFNS$(LEFT$(a$, INSTR(a$, " ") - 1) + "." + MID$(a$, 10, 3))
LFNDR$ = a$
END FUNCTION

FUNCTION LFNDS$ (LFS AS STRING, LFW AS STRING)
LFS = UCASE$(LFS)
SHELL "dir " + LFW + "*. >c:\temp\readit.txt"
LFF = FREEFILE
OPEN "c:\temp\readit.txt" FOR INPUT AS LFF
FOR i = 1 TO 5: LINE INPUT #LFF, a$: NEXT i
DO
LINE INPUT #LFF, a$
IF ASC(a$) = 32 THEN LFNER (186)
IF UCASE$(MID$(a$, 45)) = LFS THEN EXIT DO
LOOP
CLOSE LFF
KILL "c:\temp\readit.txt"
b$ = LEFT$(a$, INSTR(a$, " ") - 1)
IF ASC(MID$(a$, 10)) <> 32 THEN b$ = b$ + "." + LFNS$(MID$(a$, 10, 3))
LFNDS$ = b$
END FUNCTION

FUNCTION LFNDT% (LFS AS STRING)
IF LEFT$(LFS, 1) = "\" THEN EXIT FUNCTION
IF LEFT$(LFS, 3) = "..\" THEN EXIT FUNCTION
IF MID$(LFS, 2, 1) = ":" THEN EXIT FUNCTION
LFNDT = true
END FUNCTION

SUB LFNER (what AS INTEGER)
PRINT "LFN Error: ";
SELECT CASE (what)
CASE (154)
PRINT "Bad file mode"
CASE (155)
PRINT "File already open or name already used"
CASE (176)
PRINT "Path not found or path/file access error"
CASE (164)
PRINT "File not found"
CASE (178)
PRINT "General catchall error message"
CASE (186)
PRINT "Couldn't resolve path"
CASE (187)
PRINT "Path irregularity"
END SELECT
PRINT "Active files:"
FOR i = 1 TO LFX
IF ASC(LX(i).U) <> 32 THEN PRINT i; LX(i).F
lfnclose (i)
NEXT i
BEEP
STOP
END SUB

FUNCTION LFNEX% (LFN AS STRING)
a$ = LFN
errnum = 0
SHELL "dir /b/v " + LFNQ$(LFNS$(a$)) + " >c:\temp\readit.txt"
LFF = FREEFILE
OPEN "c:\temp\readit.txt" FOR INPUT AS LFF
IF EOF(LFF) THEN errnum = true
CLOSE LFF
KILL "c:\temp\readit.txt"
IF errnum THEN LFNEX = false ELSE LFNEX = true
END FUNCTION

FUNCTION LFNIA% (LFF AS STRING, LFN AS INTEGER)
IF LFNEX(LFF) THEN
a = LFNNO(LFF, LFN)
ELSE
a = LFNNN(LFF, LFN)
END IF
OPEN LFNS$(LX(LFN).R) FOR APPEND AS LFN
END FUNCTION

FUNCTION LFNII% (LFF AS STRING, LFN AS INTEGER)
IF NOT LFNEX(LFF) THEN LFNER (164)
a = LFNNO(LFF, LFN)
OPEN LFNS$(LX(LFN).R) FOR INPUT AS LFN
END FUNCTION

FUNCTION LFNIO% (LFF AS STRING, LFN AS INTEGER)
IF LFNEX(LFF) THEN
a = LFNNO(LFF, LFN)
ELSE
a = LFNNN(LFF, LFN)
END IF
OPEN LFNS$(LX(LFN).R) FOR OUTPUT AS LFN
END FUNCTION

FUNCTION LFNIR% (LFF AS STRING, LFN AS INTEGER, LFZ AS INTEGER)
IF LFNEX(LFF) THEN
a = LFNNO(LFF, LFN)
ELSE
a = LFNNN(LFF, LFN)
END IF
OPEN LFNS$(LX(LFN).R) FOR RANDOM AS LFN LEN = LFZ
END FUNCTION

FUNCTION LFNNN% (LFS AS STRING, LFN AS INTEGER)
STATIC O$
IF INSTR(LFS, "\") THEN p$ = LFNDP$(LFS)
LX(LFN).F = LFNS$(LFS)
LX(LFN).U = "*"
DO
a$ = "tt" + LEFT$(TIME$, 2) + MID$(TIME$, 4, 2) + RIGHT$(TIME$, 2) + "." + CHR$(LFN + 65) + "lf"
IF a$ <> O$ THEN EXIT DO
LOOP
O$ = a$
LX(LFN).R = p$ + a$
END FUNCTION

FUNCTION LFNNO% (LFS AS STRING, LFN AS INTEGER)
LX(LFN).F = LFS
LX(LFN).R = LFND$(LFS)
LX(LFN).U = "!"
END FUNCTION

FUNCTION lfnopen% (LFF AS STRING, LFM AS STRING, LFN AS INTEGER, LFZ AS INTEGER)
IF LFN = 0 THEN LFN = FREEFILE
IF LX(LFN).U <> " " THEN LFNER (155)
SELECT CASE (LFM)
CASE ("INPUT")
a = LFNII(LFF, LFN)
CASE ("OUTPUT")
a = LFNIO(LFF, LFN)
CASE ("APPEND")
a = LFNIA(LFF, LFN)
CASE ("RANDOM")
a = LFNIR(LFF, LFN, LFZ)
CASE ELSE
LFNER (154)
END SELECT
lfnopen = LFN
END FUNCTION

FUNCTION lfnopens% (LFF AS STRING, LFM AS STRING)
lfnopens = lfnopen(LFF, LFM, 0, 0)
END FUNCTION

FUNCTION LFNQ$ (LFS AS STRING)
IF ASC(LFS) <> 34 THEN LFS = CHR$(34) + LFS + CHR$(34)
LFNQ$ = LFS
END FUNCTION

FUNCTION lfnrename% (FMO AS STRING, FMN AS STRING)
a$ = LFNS$(FMN)
IF INSTR(a$, "\") THEN
 DO
  ix = INSTR(a$, "\")
  IF ix = 0 THEN EXIT DO
  a$ = MID$(a$, ix + 1)
 LOOP
END IF
SHELL "rename " + LFNQ$(LFNS$(FMO)) + " " + LFNQ$(a$)
END FUNCTION

FUNCTION LFNS$ (LFS AS STRING)
DO
IF LFS = " " THEN EXIT DO
IF ASC(RIGHT$(LFS, 1)) <> 32 THEN EXIT DO
LFS = LEFT$(LFS, LEN(LFS) - 1)
LOOP
LFNS$ = LFS
END FUNCTION

SUB testlfn
lfnchdir ("C:\Program Files\123 Free Solitaire")
file1 = lfnopen("another readme.txt", "OUTPUT", 1, 0)
PRINT #1, "Fresh Text " + TIME$ + " " + DATE$
lfnclose (1)
file2 = lfnopen("D:\Program Files\Buttonz & Tilez\readme.txt", "INPUT", 2, 0)
file1 = lfnopen("another readme.txt", "INPUT", 1, 0)
INPUT #file2, a$
PRINT a$
INPUT #file1, a$
PRINT a$
lfncloseall
SYSTEM
END SUB

