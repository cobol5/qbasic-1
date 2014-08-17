'by Eric McCormick



DECLARE SUB CT (CTEXT$)

DECLARE SUB INKEY ()

DECLARE SUB PL (LN, TEXT$)

DECLARE SUB S ()



' $STATIC

DIM DC(1 TO 10) AS INTEGER

DIM EC(1 TO 10) AS INTEGER

DIM FB(1 TO 10) AS INTEGER



ON ERROR GOTO ER

SCREEN 12

MENU:

CLOSE

TempFile$ = "~!@##@!~.tmp"

OPEN TempFile$ FOR BINARY AS #1

CLOSE

KILL TempFile$

FOR L = 1 TO 10

RANDOMIZE TIMER

REC = INT(RND * 500) + 1

EC(L) = REC - 255

NEXT L

ERASE FB, DC

S

CT "File Crypt Version 1.0"

PL 10, "1.  Encrypt File"

PL 11, "2.  Decrypt File"

PL 13, "3.  Exit File Crypt"

PL 20, "Type MENU at any prompt to go to this menu."

PL 22, "File Crypt by Eric McCormick"

PL 17, "Your choice(1-4) ?"

MC$ = INPUT$(1)

IF MC$ = "1" THEN GOTO EF

IF MC$ = "2" THEN GOTO DF

IF MC$ = "3" THEN CLS : END

GOTO MENU



EF:

S

CT "Encrypt File"

PL 9, "Type the filename of the file to encrypt:"

LOCATE 10, 15: INPUT "", SourceFile$

IF SourceFile$ = "MENU" THEN GOTO MENU

OPEN SourceFile$ FOR BINARY AS #1

IF LOF(1) = 0 THEN

CLOSE

KILL SourceFile$

PL 12, "The file you have typed cannot be found."

PL 13, "Type another filename."

PL 15, "PRESS ANY KEY TO CONTINUE"

INKEY

GOTO EF

END IF

PL 12, "Type the destination filename:"

PL 21, "To write encryption to the same file,"

PL 22, "type OVERWRITE."

LOCATE 13, 15: INPUT "", DestinationFile$

IF DestinationFile$ = "MENU" THEN GOTO MENU

DCom$ = "DIFFILE"

IF DestinationFile$ = SourceFile$ OR DestinationFile$ = "OVERWRITE" THEN DCom$ = "OVERWRITE"

OPEN TempFile$ FOR BINARY AS #2

PL 15, "Password (optional, 5-10 characters):"

PL 21, "If you want, type a password for this file"

PL 22, "which will be need upon decrypting."

COLOR 0

GETPASS:

LOCATE 16, 15: INPUT "", Pass$

IF Pass$ = "MENU" THEN GOTO MENU

IF Pass$ = "" THEN Pass = 0 ELSE Pass = 1

IF Pass = 1 AND (LEN(Pass$) lt&; 5 OR LEN(Pass$) &gt; 10) THEN GOTO GETPASS

COLOR 15

PL 21, "                                          "

PL 22, "                                   "

IF Pass = 1 THEN

PL 16, "Confirm your password:"

PL 22, "Retype your password to confirm."

COLOR 0

LOCATE 17, 15: INPUT "", Confirm$

COLOR 15

IF Confirm$ = "MENU" THEN GOTO MENU

IF Pass$ &lt;&gt; Confirm$ THEN PL 21, "Passwords don't match.  Try again.": PL 22, "PRESS ANY KEY TO CONTINUE       ": INKEY: CLOSE : GOTO EF

IF LEN(Pass$) &lt;&gt; 10 THEN AddOn$ = MID$(Pass$, 1, 10 - LEN(Pass$)): Pass$ = Pass$ + AddOn$

END IF

PL 22, "                                "

PL 19, "Encryption in progress.........."

PL 21, "File Size:  " + LTRIM$(STR$(LOF(1))) + " bytes"

IF Pass = 1 THEN

FOR L = 1 TO 10

EC(L) = ASC(MID$(Pass$, L, 1))

NEXT L

END IF

FP&amp; = 0: L = 1

DO

A$ = " "

GET #1, , A$

ASCV = ASC(A$)

IF ASCV + EC(L) &gt; 255 THEN A$ = CHR$((ASCV + EC(L)) - 256): GOTO BYE

IF ASCV + EC(L) &lt; 0 THEN A$ = CHR$((ASCV + EC(L)) + 256): GOTO BYE

A$ = CHR$(ASCV + EC(L))

BYE:

IF FP&amp; &gt;= 0 AND FP&amp; &lt; 10 THEN FB(L) = ASC(A$)

PUT #2, , A$

IF L = 10 THEN L = 0

L = L + 1

FP&amp; = FP&amp; + 1

IF FP&amp; / 250 = INT(FP&amp; / 250) THEN LOCATE 22, 15: PRINT "Encrypted: "; FP&amp;; "bytes"

LOOP UNTIL FP&amp; = LOF(1)

IF Pass = 1 THEN A$ = "=PASSWORD ACCESS $$ ": PUT #2, , A$

IF Pass = 0 THEN

FOR L = 1 TO 10

ASCSAV&amp; = (FB(L) + (-EC(L) + FB(L)) - (EC(L) + 1))

lt = 100

DO

IF ASCSAV&amp; &gt; 255 THEN ASCSAV&amp; = ASCSAV&amp; - 256: LT = LT + 1

IF ASCSAV&amp; &lt; 0 THEN ASCSAV&amp; = ASCSAV&amp; + 256: LT = LT - 1

IF ASCSAV&amp; &gt;= 0 AND ASCSAV&amp; &lt;= 255 THEN EXIT DO

LOOP

A$ = CHR$(ASCSAV&amp;) + CHR$(LT)

PUT #2, , A$

NEXT L

END IF

LOCATE 22, 15: PRINT "Encrypted: "; FP&; amp; ; "bytes"

CLOSE

IF DCom$ = "DIFFILE" THEN

OPEN DestinationFile$ FOR BINARY AS #1

CLOSE

KILL DestinationFile$

NAME TempFile$ AS DestinationFile$

END IF

IF DCom$ = "OVERWRITE" THEN

KILL SourceFile$

NAME TempFile$ AS SourceFile$

END IF

PL 19, "Encryption successful!           "

INKEY

GOTO MENU



DF:

S

CT "Decrypt File"

PL 9, "Type the filename of the file to decrypt:"

LOCATE 10, 15: INPUT "", SourceFile$

IF SourceFile$ = "MENU" THEN GOTO MENU

OPEN SourceFile$ FOR BINARY AS #1

IF LOF(1) = 0 THEN

CLOSE

KILL SourceFile$

PL 12, "The file you have typed cannot be found."

PL 13, "Type another filename."

PL 15, "PRESS ANY KEY TO CONTINUE"

INKEY

GOTO DF

END IF

PL 12, "Type the destination filename:"

PL 21, "To write decryption to the same file,"

PL 22, "type OVERWRITE."

LOCATE 13, 15: INPUT "", DestinationFile$

IF DestinationFile$ = "MENU" THEN GOTO MENU

DCom$ = "DIFFILE"

IF DestinationFile$ = SourceFile$ OR DestinationFile$ = "OVERWRITE" THEN DCom$ = "OVERWRITE"

OPEN TempFile$ FOR BINARY AS #2

PL 21, "                                     "

PL 19, "Decryption in progress.........."

FOR L = 1 TO 10

A$ = " "

GET #1, , A$

FB(L) = ASC(A$)

NEXT L

A$ = "                    "

GET #1, LOF(1) - 19, A$

IF A$ = "=PASSWORD ACCESS $$ " THEN Pass = 1 ELSE Pass = 0

IF Pass = 0 THEN

FOR L = 1 TO 10

A$ = "  "

GET #1, (LOF(1) - 21) + (L * 2), A$

DC(L) = (((ASC(MID$(A$, 1, 1)) + ((ASC(MID$(A$, 2, 1)) - 100) * 256) + 1)) - (FB(L) * 2)) / -2

NEXT L

END IF

CLOSE #1

IF Pass = 1 THEN

PL 15, "Password:"

PL 21, "This file requires a password to decrypt  "

PL 22, "it.  Type the password now.        "

COLOR 0

NEEDPASS:

LOCATE 16, 15: INPUT "", Pass$

IF Pass$ = "MENU" THEN GOTO MENU

IF LEN(Pass$) &lt; 5 OR LEN(Pass$) &gt; 10 THEN GOTO NEEDPASS

COLOR 15

PL 21, "                                          "

PL 22, "                                   "

PL 16, "Confirm your password:"

PL 22, "Retype your password to confirm."

COLOR 0

LOCATE 17, 15: INPUT "", Confirm$

COLOR 15

IF Confirm$ = "MENU" THEN GOTO MENU

IF Pass$ &lt;&gt; Confirm$ THEN PL 21, "Passwords don't match.  Try again.": PL 22, "PRESS ANY KEY TO CONTINUE       ": INKEY: CLOSE : GOTO DF

IF LEN(Pass$) &lt;&gt; 10 THEN AddOn$ = MID$(Pass$, 1, 10 - LEN(Pass$)): Pass$ = Pass$ + AddOn$

FOR L = 1 TO 10

DC(L) = ASC(MID$(Pass$, L, 1))

NEXT L

END IF

PL 22, "                                "

OPEN SourceFile$ FOR BINARY AS #1

PL 21, "File Size:  " + LTRIM$(STR$(LOF(1) - 20)) + " bytes"

FP&amp; = 0: L = 1

DO

A$ = " "

GET #1, , A$

ASCV = ASC(A$)

IF ASCV - DC(L) &gt; 255 THEN A$ = CHR$((ASCV - DC(L)) - 256): GOTO BYD

IF ASCV - DC(L) &lt; 0 THEN A$ = CHR$((ASCV - DC(L)) + 256): GOTO BYD

A$ = CHR$(ASCV - DC(L))

BYD:

PUT #2, , A$

IF L = 10 THEN L = 0

L = L + 1

FP&amp; = FP&amp; + 1

IF FP&amp; / 250 = INT(FP&amp; / 250) THEN LOCATE 22, 15: PRINT "Decrypted: "; FP&amp;; "bytes"

LOOP UNTIL FP&amp; = LOF(1) - 20

LOCATE 22, 15: PRINT "Decrypted: "; FP&; amp; ; "bytes"

CLOSE

IF DCom$ = "DIFFILE" THEN

OPEN DestinationFile$ FOR BINARY AS #1

CLOSE

KILL DestinationFile$

NAME TempFile$ AS DestinationFile$

END IF

IF DCom$ = "OVERWRITE" THEN

KILL SourceFile$

NAME TempFile$ AS SourceFile$

END IF

PL 19, "Decryption successful!           "

INKEY

GOTO MENU



ER:

CLOSE

S

PL 7, "File Crypt has performed an illegal operation"

PL 8, "and will be restarted."

PL 10, "Error " + LTRIM$(STR$(ERR)) + " at line " + LTRIM$(STR$(ERL)) + "."

INKEY

RUN



</PRE></BODY></HTML>

SUB CT (CTEXT$)

LOCATE 7, 41 - (LEN(CTEXT$) / 2): PRINT CTEXT$

END SUB

SUB INKEY

DO: LOOP WHILE INKEY$ = ""

END SUB

SUB PL (LN, TEXT$)

LOCATE LN, 15: PRINT TEXT$

END SUB

SUB S

CLS

PAINT (0, 0), 1

LINE (100, 80)-(540, 360), 4, B

LINE (99, 81)-(541, 359), 4, B

LINE (98, 82)-(542, 358), 4, B

LINE (97, 83)-(543, 357), 4, B

LINE (96, 84)-(544, 356), 4, B

LINE (101, 85)-(539, 355), 0, BF

END SUB

