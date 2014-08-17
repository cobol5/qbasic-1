DECLARE SUB testlfn ()
DECLARE SUB lfnclose (which AS INTEGER)
DECLARE SUB lfnchdir (where AS STRING)
DECLARE SUB lfnerror (what AS INTEGER)
DECLARE SUB lfncloseall ()
DECLARE FUNCTION lfndos$ (item AS STRING)
DECLARE FUNCTION lfndoshunt$ (item AS STRING)
DECLARE FUNCTION lfndosname$ (item AS STRING)
DECLARE FUNCTION lfndospath$ (item AS STRING)
DECLARE FUNCTION lfndossearch$ (item AS STRING, where AS STRING)
DECLARE FUNCTION lfndostest% (item AS STRING)
DECLARE FUNCTION lfnexist% (which AS STRING)
DECLARE FUNCTION lfnioappend% (file AS STRING, which AS INTEGER)
DECLARE FUNCTION lfnioinput% (file AS STRING, which AS INTEGER)
DECLARE FUNCTION lfniooutput% (file AS STRING, which AS INTEGER)
DECLARE FUNCTION lfniorandom% (file AS STRING, which AS INTEGER, size AS INTEGER)
DECLARE FUNCTION lfnnamenew% (item AS STRING, which AS INTEGER)
DECLARE FUNCTION lfnnameold% (item AS STRING, which AS INTEGER)
DECLARE FUNCTION lfnopen% (file AS STRING, mode AS STRING, which AS INTEGER, size AS INTEGER)
DECLARE FUNCTION lfnopens% (file AS STRING, mode AS STRING)
DECLARE FUNCTION lfnrename% (oldname AS STRING, newname AS STRING)
DECLARE FUNCTION quote$ (item AS STRING)
DECLARE FUNCTION stripl$ (item AS STRING)
DECLARE FUNCTION stripr$ (item AS STRING)

TYPE dos
full AS STRING * 128
real AS STRING * 95
used AS STRING * 1
END TYPE

DIM SHARED lfn(1 TO 3) AS dos
COMMON SHARED errnum, lfnmax, true, false
ON ERROR GOTO errhand
true = -1: false = 0: lfnmax = 3
FOR i = 1 TO lfnmax: lfn(i).used = " ": NEXT i

testlfn

errhand:
FOR i = 1 TO lfnmax: lfnclose (i): NEXT i
PRINT ERR: BEEP: STOP
RESUME NEXT

SUB lfnchdir (where AS STRING)
a$ = stripr$(where)
SHELL "dir " + quote$((a$)) + " >c:\temp\temp.txt"
file = FREEFILE
OPEN "c:\temp\temp.txt" FOR INPUT AS file
FOR i = 1 TO 4: LINE INPUT #file, b$: NEXT i
CLOSE file
KILL "c:\temp\temp.txt"
IF INSTR(b$, a$) = 0 THEN lfnerror (176)
IF MID$(a$, 2, 1) = ":" THEN
SHELL LEFT$(a$, 2)
SHELL "chdir " + quote$(stripr$(MID$(a$, 3)))
ELSE
SHELL "chdir " + quote$(stripr$(a$))
END IF
END SUB

SUB lfnclose (which AS INTEGER)
IF lfn(which).used = " " THEN EXIT SUB
CLOSE which
IF lfn(which).used = "*" THEN a = lfnrename(lfn(which).real, lfn(which).full)
lfn(which).used = " "
END SUB

SUB lfncloseall
FOR i = 1 TO lfnmax: lfnclose (i): NEXT i
END SUB

FUNCTION lfndos$ (item AS STRING)
a$ = item
IF INSTR(a$, "\") THEN p$ = lfndospath$(a$)
a$ = lfndosname$(a$)
lfndos$ = p$ + a$
END FUNCTION

FUNCTION lfndoshunt$ (item AS STRING)
a$ = item
DO
ix = INSTR(a$, "\")
IF ix = 0 THEN EXIT DO
b$ = b$ + LEFT$(a$, ix)
a$ = MID$(a$, ix + 1)
LOOP
lfndoshunt$ = b$
END FUNCTION

FUNCTION lfndosname$ (item AS STRING)
a$ = item
SHELL "dir " + quote$(stripr$(item)) + " >c:\temp\temp.txt"
file = FREEFILE
OPEN "c:\temp\temp.txt" FOR INPUT AS file
FOR i = 1 TO 5: LINE INPUT #file, a$: NEXT i
LINE INPUT #file, a$
CLOSE file
KILL "c:\temp\temp.txt"
a$ = stripr$(LEFT$(a$, INSTR(a$, " ") - 1) + "." + MID$(a$, 10, 3))
lfndosname$ = a$
END FUNCTION

FUNCTION lfndospath$ (item AS STRING)
undone$ = lfndoshunt$(item)
IF lfndostest(item) THEN
ix = INSTR(undone$, "\")
done$ = LEFT$(undone$, ix - 1)
hunt$ = done$
undone$ = MID$(undone$, ix)
path$ = lfndossearch$(hunt$, "")
ELSE
ix = INSTR(undone$, "\")
done$ = done$ + LEFT$(undone$, ix)
undone$ = MID$(undone$, ix + 1)
ix = INSTR(undone$, "\")
IF ix = 0 THEN lfnerror (187)
hunt$ = LEFT$(undone$, ix - 1)
path$ = done$ + path$ + lfndossearch$(hunt$, done$) + "\"
END IF
DO
ix = INSTR(undone$, "\")
IF ix = LEN(undone$) THEN EXIT DO
done$ = done$ + LEFT$(undone$, ix)
undone$ = MID$(undone$, ix + 1)
ix = INSTR(undone$, "\")
IF ix = 0 THEN lfnerror (187)
hunt$ = LEFT$(undone$, ix - 1)
path$ = path$ + lfndossearch$(hunt$, path$) + "\"
LOOP
lfndospath$ = path$
END FUNCTION

FUNCTION lfndossearch$ (item AS STRING, where AS STRING)
item = UCASE$(item)
SHELL "dir " + where + "*. >c:\temp\readit.txt"
file = FREEFILE
OPEN "c:\temp\readit.txt" FOR INPUT AS file
FOR i = 1 TO 5: LINE INPUT #file, a$: NEXT i
DO
LINE INPUT #file, a$
IF ASC(a$) = 32 THEN lfnerror (186)
IF UCASE$(MID$(a$, 45)) = item THEN EXIT DO
LOOP
CLOSE file
KILL "c:\temp\readit.txt"
b$ = LEFT$(a$, INSTR(a$, " ") - 1)
IF ASC(MID$(a$, 10)) <> 32 THEN b$ = b$ + "." + stripr$(MID$(a$, 10, 3))
lfndossearch$ = b$
END FUNCTION

FUNCTION lfndostest% (item AS STRING)
IF LEFT$(item, 1) = "\" THEN EXIT FUNCTION
IF LEFT$(item, 3) = "..\" THEN EXIT FUNCTION
IF MID$(item, 2, 1) = ":" THEN EXIT FUNCTION
lfndostest = true
END FUNCTION

SUB lfnerror (what AS INTEGER)
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
FOR i = 1 TO lfnmax
IF ASC(lfn(i).used) <> 32 THEN PRINT i; lfn(i).full
lfnclose (i)
NEXT i
BEEP
STOP
END SUB

FUNCTION lfnexist% (which AS STRING)
a$ = which
errnum = 0
SHELL "dir /b/v " + quote$(stripr$(a$)) + " >c:\temp\readit.txt"
file = FREEFILE
OPEN "c:\temp\readit.txt" FOR INPUT AS file
IF EOF(file) THEN errnum = true
CLOSE file
KILL "c:\temp\readit.txt"
IF errnum THEN lfnexist% = false ELSE lfnexist% = true
END FUNCTION

FUNCTION lfnioappend% (file AS STRING, which AS INTEGER)
IF lfnexist(file) THEN
a = lfnnameold(file, which)
ELSE
a = lfnnamenew(file, which)
END IF
OPEN stripr$(lfn(which).real) FOR APPEND AS which
END FUNCTION

FUNCTION lfnioinput% (file AS STRING, which AS INTEGER)
IF NOT lfnexist(file) THEN lfnerror (164)
a = lfnnameold(file, which)
OPEN stripr$(lfn(which).real) FOR INPUT AS which
END FUNCTION

FUNCTION lfniooutput% (file AS STRING, which AS INTEGER)
IF lfnexist(file) THEN
a = lfnnameold(file, which)
ELSE
a = lfnnamenew(file, which)
END IF
OPEN stripr$(lfn(which).real) FOR OUTPUT AS which
END FUNCTION

FUNCTION lfniorandom% (file AS STRING, which AS INTEGER, size AS INTEGER)
IF lfnexist(file) THEN
a = lfnnameold(file, which)
ELSE
a = lfnnamenew(file, which)
END IF
OPEN stripr$(lfn(which).real) FOR RANDOM AS which LEN = size
END FUNCTION

FUNCTION lfnnamenew% (item AS STRING, which AS INTEGER)
STATIC old$
IF INSTR(item, "\") THEN p$ = lfndospath$(item)
lfn(which).full = stripr$(item)
lfn(which).used = "*"
DO
a$ = "tt" + LEFT$(TIME$, 2) + MID$(TIME$, 4, 2) + RIGHT$(TIME$, 2) + "." + CHR$(which + 65) + "lf"
IF a$ <> old$ THEN EXIT DO
LOOP
old$ = a$
lfn(which).real = p$ + a$
END FUNCTION

FUNCTION lfnnameold% (item AS STRING, which AS INTEGER)
lfn(which).full = item
lfn(which).real = lfndos$(item)
lfn(which).used = "!"
END FUNCTION

FUNCTION lfnopen% (file AS STRING, mode AS STRING, which AS INTEGER, size AS INTEGER)
IF which = 0 THEN which = FREEFILE
IF lfn(which).used <> " " THEN lfnerror (155)
SELECT CASE (mode)
CASE ("INPUT")
a = lfnioinput(file, which)
CASE ("OUTPUT")
a = lfniooutput(file, which)
CASE ("APPEND")
a = lfnioappend(file, which)
CASE ("RANDOM")
a = lfniorandom(file, which, size)
CASE ELSE
lfnerror (154)
END SELECT
lfnopen = which
END FUNCTION

FUNCTION lfnopens% (file AS STRING, mode AS STRING)
lfnopens = lfnopen(file, mode, 0, 0)
END FUNCTION

FUNCTION lfnrename% (oldname AS STRING, newname AS STRING)
a$ = stripr$(newname)
IF INSTR(a$, "\") THEN
 DO
  ix = INSTR(a$, "\")
  IF ix = 0 THEN EXIT DO
  a$ = MID$(a$, ix + 1)
 LOOP
END IF
SHELL "rename " + quote$(stripr$(oldname)) + " " + quote$(a$)
END FUNCTION

FUNCTION quote$ (item AS STRING)
IF ASC(item) <> 34 THEN item = CHR$(34) + item + CHR$(34)
quote$ = item
END FUNCTION

FUNCTION stripl$ (item AS STRING)
DO
IF item = " " THEN EXIT DO
IF ASC(LEFT$(item, 1)) <> 32 THEN EXIT DO
item = MID$(item, 2)
LOOP
stripl$ = item
END FUNCTION

FUNCTION stripr$ (item AS STRING)
DO
IF item = " " THEN EXIT DO
IF ASC(RIGHT$(item, 1)) <> 32 THEN EXIT DO
item = LEFT$(item, LEN(item) - 1)
LOOP
stripr$ = item
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

