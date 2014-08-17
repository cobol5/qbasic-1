'strips current puzzle file then adds line breaks and centers
'puzzle in puzzle window

DEFINT A-Z
CLS

FIL1$ = "wheel.puz"
FIL2$ = "temp.DAT"


OPEN FIL2$ FOR OUTPUT AS #2

OPEN FIL1$ FOR INPUT AS #1
WHILE NOT EOF(1)
 LINE INPUT #1, a$
 PRINT a$; "."
 GOSUB strip
 PRINT a$; "."
 GOSUB PARSE
 PRINT a$
 GOSUB center
 PRINT a$
 PRINT #2, a$
 'ZZ$ = INPUT$(1): IF ZZ$ = CHR$(27) THEN CLOSE : END
WEND
CLOSE

KILL "wheel.puz"
OPEN "temp.dat" FOR INPUT AS #1
OPEN "wheel.puz" FOR OUTPUT AS #2
WHILE NOT EOF(1)
 LINE INPUT #1, a$
 PRINT #2, a$
WEND
CLOSE

KILL "temp.dat"

END

'=======================================================
center:
 count = 0
 FOR i = 1 TO LEN(a$)
  IF MID$(a$, i, 1) = "*" THEN count = count + 1
 NEXT i

 IF count = 1 THEN a$ = "**" + a$
 IF count = 2 THEN a$ = "*" + a$
 IF count = 3 THEN a$ = "*" + a$

RETURN

'=======================================================
strip:
 i = INSTR(1, a$, "~")
 trlr$ = MID$(a$, i)
 a$ = MID$(a$, 1, i - 1)
 
 FOR i = 1 TO LEN(a$)
  IF MID$(a$, i, 1) = "*" THEN MID$(a$, i, 1) = " "
 NEXT i
 a$ = LTRIM$(RTRIM$(a$))

RETURN

'=======================================================
PARSE:
 PTR = 1
 aa$ = ""
 TMP$ = ""
 FOR i = 1 TO LEN(a$) - 1
  B$ = MID$(a$, i, 1)
  BF$ = MID$(a$, i + 1, 1)
  TMP$ = LTRIM$(TMP$) + B$
  TMP = LEN(TMP$)
  IF TMP = 13 THEN
     IF B$ = " " THEN
        MID$(TMP$, 13, 1) = "*"
        aa$ = aa$ + TMP$
        TMP$ = ""
        PTR = 0
       ELSE
        IF BF$ = " " THEN
           aa$ = aa$ + TMP$ + "*"
           TMP$ = ""
           PTR = 0
          ELSE
           IF PTR = 0 THEN STOP
           aa$ = aa$ + MID$(TMP$, 1, PTR - 1) + "*"
           TMP$ = MID$(TMP$, PTR + 1)
           PTR = 0
        END IF
     END IF
    ELSE
     IF B$ = " " THEN PTR = TMP
  END IF
 NEXT i
 a$ = aa$ + TMP$ + BF$ + "*" + trlr$
RETURN

