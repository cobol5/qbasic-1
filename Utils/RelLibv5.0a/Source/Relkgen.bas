DEFINT A-Z
REM $INCLUDE: 'RelLib.BI'

DECLARE SUB RelPrint (DestSeg%, X%, Y%, Font$, Mincolor%, Italic%)
DECLARE SUB RelPrintB (DestSeg%, X%, Y%, Font$, Mincolor%, Italic%)
DECLARE SUB RelPrintC (DestSeg%, X%, Y%, Font$, Col%, Italic%)
DECLARE SUB RelPrintS (DestSeg%, X%, Y%, Font$, Mincolor%, Xscale%, Yscale%, Italic%)
DECLARE SUB RelPrintSB (DestSeg%, X%, Y%, Font$, Mincolor%, Xscale%, Yscale%, Italic%)
DECLARE SUB RelPrintSC (DestSeg%, X%, Y%, Font$, Col%, Xscale%, Yscale%, Italic%)
DECLARE SUB RelPrintScoreL (DestSeg%, Score#, X%, Y%, Mincolor%, Italic%, Shade%)
DECLARE SUB RelPrintScoreR (DestSeg%, Score#, X%, Y%, Mincolor%, Italic%, Shade%)
DECLARE SUB RelPrnScShadowSL (DestSeg%, Score#, X%, Y%, Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%, Shade%)
DECLARE SUB RelPrnScShadowSR (DestSeg%, Score#, X%, Y%, Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%, Shade%)
DECLARE SUB RelPrnScShadowL (DestSeg%, Score#, X%, Y%, Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%, Shade%)
DECLARE SUB RelPrnScShadowR (DestSeg%, Score#, X%, Y%, Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%, Shade%)
DECLARE SUB RelPrintShadow (DestSeg%, X%, Y%, Font$, Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%)
DECLARE SUB RelPrintShadowB (DestSeg%, X%, Y%, Font$, Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%)
DECLARE SUB RelPrintShadowC (DestSeg%, X%, Y%, Font$, Col%, Italic%, ShaX%, ShaY%, ShadCol%, ShadTrans%)
DECLARE SUB RelPrintShadowS (DestSeg%, X%, Y%, Font$, Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%)
DECLARE SUB RelPrintShadowSB (DestSeg%, X%, Y%, Font$, Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%)
DECLARE SUB RelPrintShadowSC (DestSeg%, X%, Y%, Font$, Col%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadCol%, ShadTrans%)
DECLARE SUB RelPrintTrans (DestSeg%, X%, Y%, Font$, Mincolor%, Italic%)
DECLARE SUB RelPrintTransB (DestSeg%, X%, Y%, Font$, Mincolor%, Italic%)
DECLARE SUB RelPrintTransC (DestSeg%, X%, Y%, Font$, Col%, Italic%)
DECLARE SUB RelPrintTransS (DestSeg%, X%, Y%, Font$, Mincolor%, Xscale%, Yscale%, Italic%)
DECLARE SUB RelPrintTransSB (DestSeg%, X%, Y%, Font$, Mincolor%, Xscale%, Yscale%, Italic%)
DECLARE SUB RelPrintTransSC (DestSeg%, X%, Y%, Font$, Col%, Xscale%, Yscale%, Italic%)

SUB RelPrint (DestSeg, X, Y, Font$, Mincolor, Italic) STATIC

'=======Prints system fonts on screen  specified by X,Y
'=======Uses 8 colors from mincolor to Mincolor+8
'=======Font$ is the string, italic? Duh!!!!!
'=====Sample Code
        'Note Kgen....Min are constants
        'X = 261
        'Y = 183
        'Font$ = "RelSoft"
        'Italic = True
        'KgenFont X - 1, Y - 1, Font$, KgenGreenMin, Italic
        'KgenFont X, Y, Font$, KgenBlueMin, Italic
'End Sample
'======================================================

DIM E(7): E(0) = 1: FOR F = 1 TO 7: E(F) = E(F - 1) + E(F - 1): NEXT F

XXX = X%
YYY = Y%

IF X% = 320 THEN X% = 160 - (4 * LEN(Font$))




DEF SEG = &HFFA6
FOR A = 1 TO LEN(Font$)
KC = 0
IF Italic THEN
        Ita = 8
ELSE
        Ita = 0
END IF

 X = X + 8
 D = ASC(MID$(Font$, A, 1)) * 8 + 14
 FOR B = 0 TO 7
  FOR C = 0 TO 7
   IF PEEK(B + D) AND E(C) THEN RelPset DestSeg, ((X - C) + Ita), Y + B, Mincolor + KC
  NEXT C
        KC = KC MOD 8 + 1
           IF Italic THEN
                Ita = Ita - 1
           END IF
 NEXT B
NEXT A

DEF SEG

X = XXX
Y = YYY


END SUB

SUB RelPrintB (DestSeg, X, Y, Font$, Mincolor, Italic) STATIC

'Coding for "RelPrintB" by Richard Eric M. Lope and Adigun Azikiwe Polack.

'=======Prints system fonts on screen  specified by X,Y
'=======Uses 8 colors from mincolor to Mincolor-8
'=======Font$ is the string, italic? Duh!!!!!
'=====Sample Code
        'Note Kgen....Min are constants
        'X = 261
        'Y = 183
        'Font$ = "RelSoft"
        'Italic = True
        'KgenFont X - 1, Y - 1, Font$, KgenGreenMin, Italic
        'KgenFont X, Y, Font$, KgenBlueMin, Italic
'End Sample
'======================================================

DIM E(7): E(0) = 1: FOR F = 1 TO 7: E(F) = E(F - 1) + E(F - 1): NEXT F

XXX = X%
YYY = Y%

IF X% = 320 THEN X% = 160 - (4 * LEN(Font$))




DEF SEG = &HFFA6
FOR A = 1 TO LEN(Font$)
KC = 0
IF Italic THEN
        Ita = 8
ELSE
        Ita = 0
END IF

 X = X + 8
 D = ASC(MID$(Font$, A, 1)) * 8 + 14
 FOR B = 0 TO 7
  FOR C = 0 TO 7
   IF PEEK(B + D) AND E(C) THEN RelPset DestSeg, ((X - C) + Ita), Y + B, Mincolor - KC
  NEXT C
        KC = KC MOD 8 + 1
           IF Italic THEN
                Ita = Ita - 1
           END IF
 NEXT B
NEXT A

DEF SEG

X = XXX
Y = YYY


END SUB

SUB RelPrintC (DestSeg, X, Y, Font$, Col, Italic) STATIC

'Coding for "RelPrintC" by Richard Eric M. Lope and Adigun Azikiwe Polack.

'=======Prints system fonts on screen  specified by X,Y
'=======Uses only ONE color.
'=======Font$ is the string, italic? Duh!!!!!
'=====Sample Code
        'Note Kgen....Min are constants
        'X = 261
        'Y = 183
        'Font$ = "RelSoft"
        'Italic = True
        'KgenFont X - 1, Y - 1, Font$, KgenGreenMin, Italic
        'KgenFont X, Y, Font$, KgenBlueMin, Italic
'End Sample
'======================================================

DIM E(7): E(0) = 1: FOR F = 1 TO 7: E(F) = E(F - 1) + E(F - 1): NEXT F

XXX = X%
YYY = Y%

IF X% = 320 THEN X% = 160 - (4 * LEN(Font$))




DEF SEG = &HFFA6
FOR A = 1 TO LEN(Font$)
KC = 0
IF Italic THEN
        Ita = 8
ELSE
        Ita = 0
END IF

 X = X + 8
 D = ASC(MID$(Font$, A, 1)) * 8 + 14
 FOR B = 0 TO 7
  FOR C = 0 TO 7
   IF PEEK(B + D) AND E(C) THEN RelPset DestSeg, ((X - C) + Ita), Y + B, Col
  NEXT C
        KC = KC MOD 8 + 1
           IF Italic THEN
                Ita = Ita - 1
           END IF
 NEXT B
NEXT A

DEF SEG

X = XXX
Y = YYY


END SUB

SUB RelPrintS (DestSeg, X, Y, Font$, Mincolor, Xscale, Yscale, Italic) STATIC

'=======Prints scalable system fonts on screen  specified by X,Y
'=======Uses 8 colors from mincolor to Mincolor+8
'=======Font$ is the string, italic? Duh!!!!!
'=======Xscale/Yscale are scale to enlarge the font
'=====Sample Code
        'Note Kgen....Min are constants
        'X = 261
        'Y = 183
        'Xscale=3
        'Yscale=2
        'Font$ = "RelSoft"
        'Italic = True
        'KgenTTFont X - 1, Y - 1, Font$, KgenGreenMin,Xscale,Yscale Italic
        'KgenTTFont X, Y, Font$, KgenBlueMin,,Xscale,Yscale Italic
'End Sample
'======================================================

DIM E(7): E(0) = 1: FOR F = 1 TO 7: E(F) = E(F - 1) + E(F - 1): NEXT F

XXX = X
YYY = Y
XSS = Xscale
YSS = Yscale

IF X = 320 THEN X = 160 - ((4 * Xscale * LEN(Font$)))



IF Italic THEN
        Ita = 8
ELSE
        Ita = 0
END IF


DEF SEG = &HFFA6
FOR A = 1 TO LEN(Font$)

KC = 0
YY = 0
XX = 0

 X = X + (8 * Xscale)
 D = ASC(MID$(Font$, A, 1)) * 8 + 14
 FOR B = 0 TO 7
        YY = YY + Yscale
        XX = 0
  FOR C = 0 TO 7
        IF PEEK(B + D) AND E(C) THEN RelBoxF DestSeg, (X - (C * Xscale)) + Ita, Y + YY, (X - (C * Xscale)) + Ita - (Xscale - 1), Y + YY + Yscale - 1, Mincolor + KC
        XX = XX + Xscale
  NEXT C
        KC = KC MOD 8 + 1

           IF Italic THEN
                Ita = Ita - 1
                IF Ita < 1 THEN Ita = 8
           END IF

 NEXT B

NEXT A

DEF SEG

X = XXX
Y = YYY
Xscale = XSS
Yscale = YSS

END SUB

SUB RelPrintSB (DestSeg, X, Y, Font$, Mincolor, Xscale, Yscale, Italic) STATIC

'Coding for "RelPrintSB" by Richard Eric M. Lope and Adigun Azikiwe Polack.

'=======Prints scalable system fonts on screen  specified by X,Y
'=======Uses 8 colors from mincolor to Mincolor-8
'=======Font$ is the string, italic? Duh!!!!!
'=======Xscale/Yscale are scale to enlarge the font
'=====Sample Code
        'Note Kgen....Min are constants
        'X = 261
        'Y = 183
        'Xscale=3
        'Yscale=2
        'Font$ = "RelSoft"
        'Italic = True
        'KgenTTFont X - 1, Y - 1, Font$, KgenGreenMin,Xscale,Yscale Italic
        'KgenTTFont X, Y, Font$, KgenBlueMin,,Xscale,Yscale Italic
'End Sample
'======================================================

DIM E(7): E(0) = 1: FOR F = 1 TO 7: E(F) = E(F - 1) + E(F - 1): NEXT F

XXX = X
YYY = Y
XSS = Xscale
YSS = Yscale

IF X = 320 THEN X = 160 - ((4 * Xscale * LEN(Font$)))



IF Italic THEN
        Ita = 8
ELSE
        Ita = 0
END IF


DEF SEG = &HFFA6
FOR A = 1 TO LEN(Font$)

KC = 0
YY = 0
XX = 0

 X = X + (8 * Xscale)
 D = ASC(MID$(Font$, A, 1)) * 8 + 14
 FOR B = 0 TO 7
        YY = YY + Yscale
        XX = 0
  FOR C = 0 TO 7
        IF PEEK(B + D) AND E(C) THEN RelBoxF DestSeg, (X - (C * Xscale)) + Ita, Y + YY, (X - (C * Xscale)) + Ita - (Xscale - 1), Y + YY + Yscale - 1, Mincolor - KC
        XX = XX + Xscale
  NEXT C
        KC = KC MOD 8 + 1

           IF Italic THEN
                Ita = Ita - 1
                IF Ita < 1 THEN Ita = 8
           END IF

 NEXT B

NEXT A

DEF SEG

X = XXX
Y = YYY
Xscale = XSS
Yscale = YSS

END SUB

SUB RelPrintSC (DestSeg, X, Y, Font$, Col, Xscale, Yscale, Italic) STATIC

'Coding for "RelPrintSC" by Richard Eric M. Lope and Adigun Azikiwe Polack.

'=======Prints scalable system fonts on screen  specified by X,Y
'=======Uses only ONE color.
'=======Font$ is the string, italic? Duh!!!!!
'=======Xscale/Yscale are scale to enlarge the font
'=====Sample Code
        'Note Kgen....Min are constants
        'X = 261
        'Y = 183
        'Xscale=3
        'Yscale=2
        'Font$ = "RelSoft"
        'Italic = True
        'KgenTTFont X - 1, Y - 1, Font$, KgenGreenMin,Xscale,Yscale Italic
        'KgenTTFont X, Y, Font$, KgenBlueMin,,Xscale,Yscale Italic
'End Sample
'======================================================

DIM E(7): E(0) = 1: FOR F = 1 TO 7: E(F) = E(F - 1) + E(F - 1): NEXT F

XXX = X
YYY = Y
XSS = Xscale
YSS = Yscale

IF X = 320 THEN X = 160 - ((4 * Xscale * LEN(Font$)))



IF Italic THEN
        Ita = 8
ELSE
        Ita = 0
END IF


DEF SEG = &HFFA6
FOR A = 1 TO LEN(Font$)

KC = 0
YY = 0
XX = 0

 X = X + (8 * Xscale)
 D = ASC(MID$(Font$, A, 1)) * 8 + 14
 FOR B = 0 TO 7
        YY = YY + Yscale
        XX = 0
  FOR C = 0 TO 7
        IF PEEK(B + D) AND E(C) THEN RelBoxF DestSeg, (X - (C * Xscale)) + Ita, Y + YY, (X - (C * Xscale)) + Ita - (Xscale - 1), Y + YY + Yscale - 1, Col
        XX = XX + Xscale
  NEXT C
        KC = KC MOD 8 + 1

           IF Italic THEN
                Ita = Ita - 1
                IF Ita < 1 THEN Ita = 8
           END IF

 NEXT B

NEXT A

DEF SEG

X = XXX
Y = YYY
Xscale = XSS
Yscale = YSS

END SUB

SUB RelPrintScoreL (DestSeg%, Score#, X%, Y%, Mincolor%, Italic%, Shade%) STATIC

'Coding for "RelPrintScoreL" by Adigun Azikiwe Polack.

' 10 digits in the score                         - Justification: LEFT -

IF Shade% <= 0 THEN Shade% = 0
IF Shade% >= 2 THEN Shade% = 2

IF Score# < 0# THEN Score# = 0#
IF Score# > 9999999999# THEN Score# = 9999999999#

SELECT CASE Shade%
  CASE 0: RelPrintC DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrint DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintB DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
END SELECT
END SUB

SUB RelPrintScoreR (DestSeg%, Score#, X%, Y%, Mincolor%, Italic%, Shade%) STATIC

'Coding for "RelPrintScoreR" by Adigun Azikiwe Polack.

' 10 digits in the score                         - Justification: RIGHT -

IF Score# < 0# THEN Score# = 0#
IF Score# > 9999999999# THEN Score# = 9999999999#
Scotab# = Score#

IF Shade% <= 0 THEN Shade% = 0
IF Shade% >= 2 THEN Shade% = 2

IF Scotab# >= 0 AND Scotab# <= 9.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintC DestSeg%, X% + 72, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrint DestSeg%, X% + 72, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintB DestSeg%, X% + 72, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10 AND Scotab# <= 99.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintC DestSeg%, X% + 64, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrint DestSeg%, X% + 64, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintB DestSeg%, X% + 64, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100 AND Scotab# <= 999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintC DestSeg%, X% + 56, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrint DestSeg%, X% + 56, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintB DestSeg%, X% + 56, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000 AND Scotab# <= 9999.999900000001# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintC DestSeg%, X% + 48, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrint DestSeg%, X% + 48, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintB DestSeg%, X% + 48, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000 AND Scotab# <= 99999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintC DestSeg%, X% + 40, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrint DestSeg%, X% + 40, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintB DestSeg%, X% + 40, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000 AND Scotab# <= 999999.9999000001# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintC DestSeg%, X% + 32, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrint DestSeg%, X% + 32, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintB DestSeg%, X% + 32, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000 AND Scotab# <= 9999999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintC DestSeg%, X% + 24, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrint DestSeg%, X% + 24, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintB DestSeg%, X% + 24, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000000 AND Scotab# <= 99999999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintC DestSeg%, X% + 16, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrint DestSeg%, X% + 16, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintB DestSeg%, X% + 16, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000000 AND Scotab# <= 999999999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintC DestSeg%, X% + 8, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrint DestSeg%, X% + 8, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintB DestSeg%, X% + 8, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000000 AND Scotab# <= 9999999999.99# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintC DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrint DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintB DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

END SUB

SUB RelPrintShadow (DestSeg, X, Y, Font$, Mincolor, Italic, ShaX%, ShaY%, ShadMinCol, ShadTrans%) STATIC

'Coding for "RelPrintShadow" by Adigun Azikiwe Polack.

IF ShadTrans = TRUE THEN
  RelPrintTrans DestSeg, X + ShaX, Y + ShaY, Font$, ShadMinCol, Italic
  ELSE
  RelPrint DestSeg, X + ShaX, Y + ShaY, Font$, ShadMinCol, Italic
END IF
RelPrint DestSeg, X, Y, Font$, Mincolor, Italic

END SUB

SUB RelPrintShadowB (DestSeg, X, Y, Font$, Mincolor, Italic, ShaX%, ShaY%, ShadMinCol, ShadTrans%) STATIC

'Coding for "RelPrintShadowB" by Adigun Azikiwe Polack.

IF ShadTrans = TRUE THEN
  RelPrintTransB DestSeg, X + ShaX, Y + ShaY, Font$, ShadMinCol, Italic
  ELSE
  RelPrintB DestSeg, X + ShaX, Y + ShaY, Font$, ShadMinCol, Italic
END IF
RelPrintB DestSeg, X, Y, Font$, Mincolor, Italic

END SUB

SUB RelPrintShadowC (DestSeg%, X%, Y%, Font$, Col%, Italic%, ShaX%, ShaY%, ShadCol%, ShadTrans%) STATIC

'Coding for "RelPrintShadowC" by Adigun Azikiwe Polack.

IF ShadTrans = TRUE THEN
  RelPrintTransC DestSeg, X + ShaX, Y + ShaY, Font$, ShadCol, Italic
  ELSE
  RelPrintC DestSeg, X + ShaX, Y + ShaY, Font$, ShadCol, Italic
END IF
RelPrintC DestSeg, X, Y, Font$, Col%, Italic

END SUB

SUB RelPrintShadowS (DestSeg, X, Y, Font$, Mincolor, Xscale, Yscale, Italic, ShaX%, ShaY%, ShadMinCol, ShadTrans%) STATIC

'Coding for "RelPrintShadowS" by Adigun Azikiwe Polack.

IF ShadTrans = TRUE THEN
  RelPrintTransS DestSeg, X + ShaX, Y + ShaY, Font$, ShadMinCol, Xscale, Yscale, Italic
  ELSE
  RelPrintS DestSeg, X + ShaX, Y + ShaY, Font$, ShadMinCol, Xscale, Yscale, Italic
END IF
RelPrintS DestSeg, X, Y, Font$, Mincolor, Xscale, Yscale, Italic

END SUB

SUB RelPrintShadowSB (DestSeg, X, Y, Font$, Mincolor, Xscale, Yscale, Italic, ShaX%, ShaY%, ShadMinCol, ShadTrans%) STATIC

'Coding for "RelPrintShadowSB" by Adigun Azikiwe Polack.

IF ShadTrans = TRUE THEN
  RelPrintTransSB DestSeg, X + ShaX, Y + ShaY, Font$, ShadMinCol, Xscale, Yscale, Italic
  ELSE
  RelPrintSB DestSeg, X + ShaX, Y + ShaY, Font$, ShadMinCol, Xscale, Yscale, Italic
END IF
RelPrintSB DestSeg, X, Y, Font$, Mincolor, Xscale, Yscale, Italic

END SUB

SUB RelPrintShadowSC (DestSeg, X, Y, Font$, Col%, Xscale, Yscale, Italic, ShaX%, ShaY%, ShadCol, ShadTrans%) STATIC

'Coding for "RelPrintShadowSC" by Adigun Azikiwe Polack.

IF ShadTrans% THEN
  RelPrintTransSC DestSeg, X + ShaX, Y + ShaY, Font$, ShadCol, Xscale, Yscale, Italic
  ELSE
  RelPrintSC DestSeg, X + ShaX, Y + ShaY, Font$, ShadCol, Xscale, Yscale, Italic
END IF
RelPrintSC DestSeg, X, Y, Font$, Col%, Xscale, Yscale, Italic

END SUB

SUB RelPrintTrans (DestSeg, X, Y, Font$, Mincolor, Italic) STATIC

'Coding for "RelPrintTrans" by Richard Eric M. Lope and
'                                            Adigun Azikiwe Polack.

'=======Prints system fonts TRANSLUCENTLY on screen specified by X,Y!!!  ;D
'=======Uses 8 colors from mincolor to Mincolor+8
'=======Font$ is the string, italic? Duh!!!!!
'=====Sample Code
        'Note Kgen....Min are constants
        'X = 261
        'Y = 183
        'Font$ = "RelSoft"
        'Italic = True
        'KgenFont X - 1, Y - 1, Font$, KgenGreenMin, Italic
        'KgenFont X, Y, Font$, KgenBlueMin, Italic
'End Sample
'======================================================

DIM E(7): E(0) = 1: FOR F = 1 TO 7: E(F) = E(F - 1) + E(F - 1): NEXT F

XXX = X%
YYY = Y%

IF X% = 320 THEN X% = 160 - (4 * LEN(Font$))




DEF SEG = &HFFA6
FOR A = 1 TO LEN(Font$)
KC = 0
IF Italic THEN
        Ita = 8
ELSE
        Ita = 0
END IF

 X = X + 8
 D = ASC(MID$(Font$, A, 1)) * 8 + 14
 FOR B = 0 TO 7
  FOR C = 0 TO 7
   IF PEEK(B + D) AND E(C) THEN RelPsetTrans DestSeg, ((X - C) + Ita), Y + B, Mincolor + KC
  NEXT C
        KC = KC MOD 8 + 1
           IF Italic THEN
                Ita = Ita - 1
           END IF
 NEXT B
NEXT A

DEF SEG

X = XXX
Y = YYY


END SUB

SUB RelPrintTransB (DestSeg, X, Y, Font$, Mincolor, Italic) STATIC

'Coding for "RelPrintTransB" by Richard Eric M. Lope and
'                                             Adigun Azikiwe Polack.

'=======Prints system fonts TRANSLUCENTLY on screen specified by X,Y!!!  ;D
'=======Uses 8 colors from mincolor to Mincolor-8
'=======Font$ is the string, italic? Duh!!!!!
'=====Sample Code
        'Note Kgen....Min are constants
        'X = 261
        'Y = 183
        'Font$ = "RelSoft"
        'Italic = True
        'KgenFont X - 1, Y - 1, Font$, KgenGreenMin, Italic
        'KgenFont X, Y, Font$, KgenBlueMin, Italic
'End Sample
'======================================================

DIM E(7): E(0) = 1: FOR F = 1 TO 7: E(F) = E(F - 1) + E(F - 1): NEXT F

XXX = X%
YYY = Y%

IF X% = 320 THEN X% = 160 - (4 * LEN(Font$))




DEF SEG = &HFFA6
FOR A = 1 TO LEN(Font$)
KC = 0
IF Italic THEN
        Ita = 8
ELSE
        Ita = 0
END IF

 X = X + 8
 D = ASC(MID$(Font$, A, 1)) * 8 + 14
 FOR B = 0 TO 7
  FOR C = 0 TO 7
   IF PEEK(B + D) AND E(C) THEN RelPsetTrans DestSeg, ((X - C) + Ita), Y + B, Mincolor - KC
  NEXT C
        KC = KC MOD 8 + 1
           IF Italic THEN
                Ita = Ita - 1
           END IF
 NEXT B
NEXT A

DEF SEG

X = XXX
Y = YYY


END SUB

SUB RelPrintTransC (DestSeg, X, Y, Font$, Col, Italic) STATIC

'Coding for "RelPrintTransC" by Richard Eric M. Lope and
'                                             Adigun Azikiwe Polack.

'=======Prints system fonts on screen specified by X,Y
'=======Uses only ONE color, TRANSLUCENTLY!!!  ;D
'=======Font$ is the string, italic? Duh!!!!!
'=====Sample Code
        'Note Kgen....Min are constants
        'X = 261
        'Y = 183
        'Font$ = "RelSoft"
        'Italic = True
        'KgenFont X - 1, Y - 1, Font$, KgenGreenMin, Italic
        'KgenFont X, Y, Font$, KgenBlueMin, Italic
'End Sample
'======================================================

DIM E(7): E(0) = 1: FOR F = 1 TO 7: E(F) = E(F - 1) + E(F - 1): NEXT F

XXX = X%
YYY = Y%

IF X% = 320 THEN X% = 160 - (4 * LEN(Font$))




DEF SEG = &HFFA6
FOR A = 1 TO LEN(Font$)
KC = 0
IF Italic THEN
        Ita = 8
ELSE
        Ita = 0
END IF

 X = X + 8
 D = ASC(MID$(Font$, A, 1)) * 8 + 14
 FOR B = 0 TO 7
  FOR C = 0 TO 7
   IF PEEK(B + D) AND E(C) THEN RelPsetTrans DestSeg, ((X - C) + Ita), Y + B, Col
  NEXT C
        KC = KC MOD 8 + 1
           IF Italic THEN
                Ita = Ita - 1
           END IF
 NEXT B
NEXT A

DEF SEG

X = XXX
Y = YYY


END SUB

SUB RelPrintTransS (DestSeg, X, Y, Font$, Mincolor, Xscale, Yscale, Italic) STATIC

'=======Prints scalable system fonts on screen  specified by X,Y
'translucently
'=======Uses 8 colors from mincolor to Mincolor+8
'=======Font$ is the string, italic? Duh!!!!!
'=======Xscale/Yscale are scale to enlarge the font
'=====Sample Code
        'Note Kgen....Min are constants
        'X = 261
        'Y = 183
        'Xscale=3
        'Yscale=2
        'Font$ = "RelSoft"
        'Italic = True
        'KgenTTFont X - 1, Y - 1, Font$, KgenGreenMin,Xscale,Yscale Italic
        'KgenTTFont X, Y, Font$, KgenBlueMin,,Xscale,Yscale Italic
'End Sample
'======================================================

DIM E(7): E(0) = 1: FOR F = 1 TO 7: E(F) = E(F - 1) + E(F - 1): NEXT F

XXX = X
YYY = Y
XSS = Xscale
YSS = Yscale

IF X = 320 THEN X = 160 - ((4 * Xscale * LEN(Font$)))



IF Italic THEN
        Ita = 8
ELSE
        Ita = 0
END IF


DEF SEG = &HFFA6
FOR A = 1 TO LEN(Font$)

KC = 0
YY = 0
XX = 0

 X = X + (8 * Xscale)
 D = ASC(MID$(Font$, A, 1)) * 8 + 14
 FOR B = 0 TO 7
        YY = YY + Yscale
        XX = 0
  FOR C = 0 TO 7
        IF PEEK(B + D) AND E(C) THEN RelBoxTransF DestSeg, (X - (C * Xscale)) + Ita, Y + YY, (X - (C * Xscale)) + Ita - (Xscale - 1), Y + YY + Yscale - 1, Mincolor + KC
        XX = XX + Xscale
  NEXT C
        KC = KC MOD 8 + 1

           IF Italic THEN
                Ita = Ita - 1
                IF Ita < 1 THEN Ita = 8
           END IF

 NEXT B

NEXT A

DEF SEG

X = XXX
Y = YYY
Xscale = XSS
Yscale = YSS

END SUB

SUB RelPrintTransSB (DestSeg, X, Y, Font$, Mincolor, Xscale, Yscale, Italic) STATIC

'Coding for "RelPrintTransSB" by Richard Eric M. Lope and
'                                             Adigun Azikiwe Polack.

'=======Prints scalable system fonts on screen  specified by X,Y
'translucently
'=======Uses 8 colors from mincolor to Mincolor-8
'=======Font$ is the string, italic? Duh!!!!!
'=======Xscale/Yscale are scale to enlarge the font
'=====Sample Code
        'Note Kgen....Min are constants
        'X = 261
        'Y = 183
        'Xscale=3
        'Yscale=2
        'Font$ = "RelSoft"
        'Italic = True
        'KgenTTFont X - 1, Y - 1, Font$, KgenGreenMin,Xscale,Yscale Italic
        'KgenTTFont X, Y, Font$, KgenBlueMin,,Xscale,Yscale Italic
'End Sample
'======================================================

DIM E(7): E(0) = 1: FOR F = 1 TO 7: E(F) = E(F - 1) + E(F - 1): NEXT F

XXX = X
YYY = Y
XSS = Xscale
YSS = Yscale

IF X = 320 THEN X = 160 - ((4 * Xscale * LEN(Font$)))



IF Italic THEN
        Ita = 8
ELSE
        Ita = 0
END IF


DEF SEG = &HFFA6
FOR A = 1 TO LEN(Font$)

KC = 0
YY = 0
XX = 0

 X = X + (8 * Xscale)
 D = ASC(MID$(Font$, A, 1)) * 8 + 14
 FOR B = 0 TO 7
        YY = YY + Yscale
        XX = 0
  FOR C = 0 TO 7
        IF PEEK(B + D) AND E(C) THEN RelBoxTransF DestSeg, (X - (C * Xscale)) + Ita, Y + YY, (X - (C * Xscale)) + Ita - (Xscale - 1), Y + YY + Yscale - 1, Mincolor - KC
        XX = XX + Xscale
  NEXT C
        KC = KC MOD 8 + 1

           IF Italic THEN
                Ita = Ita - 1
                IF Ita < 1 THEN Ita = 8
           END IF

 NEXT B

NEXT A

DEF SEG

X = XXX
Y = YYY
Xscale = XSS
Yscale = YSS

END SUB

SUB RelPrintTransSC (DestSeg, X, Y, Font$, Col, Xscale, Yscale, Italic) STATIC

'Coding for "RelPrintTransSC" by Richard Eric M. Lope and
'                                             Adigun Azikiwe Polack.

'=======Prints scalable system fonts on screen  specified by X,Y
'=======Uses only ONE color, TRANSLUCENTLY!!!  ;D
'=======Font$ is the string, italic? Duh!!!!!
'=======Xscale/Yscale are scale to enlarge the font
'=====Sample Code
        'Note Kgen....Min are constants
        'X = 261
        'Y = 183
        'Xscale=3
        'Yscale=2
        'Font$ = "RelSoft"
        'Italic = True
        'KgenTTFont X - 1, Y - 1, Font$, KgenGreenMin,Xscale,Yscale Italic
        'KgenTTFont X, Y, Font$, KgenBlueMin,,Xscale,Yscale Italic
'End Sample
'======================================================

DIM E(7): E(0) = 1: FOR F = 1 TO 7: E(F) = E(F - 1) + E(F - 1): NEXT F

XXX = X
YYY = Y
XSS = Xscale
YSS = Yscale

IF X = 320 THEN X = 160 - ((4 * Xscale * LEN(Font$)))



IF Italic THEN
        Ita = 8
ELSE
        Ita = 0
END IF


DEF SEG = &HFFA6
FOR A = 1 TO LEN(Font$)

KC = 0
YY = 0
XX = 0

 X = X + (8 * Xscale)
 D = ASC(MID$(Font$, A, 1)) * 8 + 14
 FOR B = 0 TO 7
        YY = YY + Yscale
        XX = 0
  FOR C = 0 TO 7
        IF PEEK(B + D) AND E(C) THEN RelBoxTransF DestSeg, (X - (C * Xscale)) + Ita, Y + YY, (X - (C * Xscale)) + Ita - (Xscale - 1), Y + YY + Yscale - 1, Col
        XX = XX + Xscale
  NEXT C
        KC = KC MOD 8 + 1

           IF Italic THEN
                Ita = Ita - 1
                IF Ita < 1 THEN Ita = 8
           END IF

 NEXT B

NEXT A

DEF SEG

X = XXX
Y = YYY
Xscale = XSS
Yscale = YSS

END SUB

SUB RelPrnScShadowL (DestSeg%, Score#, X%, Y%, Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%, Shade%) STATIC

'Coding for "RelPrnScShadowL" by Adigun Azikiwe Polack.

' 10 digits in the score                         - Justification: LEFT -

IF Shade% <= 0 THEN Shade% = 0
IF Shade% >= 2 THEN Shade% = 2

IF Score# < 0# THEN Score# = 0#
IF Score# > 9999999999# THEN Score# = 9999999999#

SELECT CASE Shade%
  CASE 0: RelPrintShadowC DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 1: RelPrintShadow DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 2: RelPrintShadowB DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
END SELECT

END SUB

SUB RelPrnScShadowR (DestSeg%, Score#, X%, Y%, Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%, Shade%) STATIC

'Coding for "RelPrnScShadowR" by Adigun Azikiwe Polack.

' 10 digits in the score                         - Justification: RIGHT -

IF Score# < 0# THEN Score# = 0#
IF Score# > 9999999999# THEN Score# = 9999999999#
Scotab# = Score#

IF Shade% <= 0 THEN Shade% = 0
IF Shade% >= 2 THEN Shade% = 2

IF Scotab# >= 0 AND Scotab# <= 9.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintShadowC DestSeg%, X% + 72, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 1: RelPrintShadow DestSeg%, X% + 72, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 2: RelPrintShadowB DestSeg%, X% + 72, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10 AND Scotab# <= 99.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintShadowC DestSeg%, X% + 64, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 1: RelPrintShadow DestSeg%, X% + 64, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 2: RelPrintShadowB DestSeg%, X% + 64, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100 AND Scotab# <= 999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintShadowC DestSeg%, X% + 56, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 1: RelPrintShadow DestSeg%, X% + 56, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 2: RelPrintShadowB DestSeg%, X% + 56, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000 AND Scotab# <= 9999.999900000001# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintShadowC DestSeg%, X% + 48, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 1: RelPrintShadow DestSeg%, X% + 48, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 2: RelPrintShadowB DestSeg%, X% + 48, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000 AND Scotab# <= 99999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintShadowC DestSeg%, X% + 40, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 1: RelPrintShadow DestSeg%, X% + 40, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 2: RelPrintShadowB DestSeg%, X% + 40, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000 AND Scotab# <= 999999.9999000001# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintShadowC DestSeg%, X% + 32, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 1: RelPrintShadow DestSeg%, X% + 32, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 2: RelPrintShadowB DestSeg%, X% + 32, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000 AND Scotab# <= 9999999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintShadowC DestSeg%, X% + 24, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 1: RelPrintShadow DestSeg%, X% + 24, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 2: RelPrintShadowB DestSeg%, X% + 24, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000000 AND Scotab# <= 99999999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintShadowC DestSeg%, X% + 16, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 1: RelPrintShadow DestSeg%, X% + 16, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 2: RelPrintShadowB DestSeg%, X% + 16, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000000 AND Scotab# <= 999999999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintShadowC DestSeg%, X% + 8, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 1: RelPrintShadow DestSeg%, X% + 8, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 2: RelPrintShadowB DestSeg%, X% + 8, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000000 AND Scotab# <= 9999999999.99# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintShadowC DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 1: RelPrintShadow DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 2: RelPrintShadowB DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF


END SUB

SUB RelPrnScShadowSL (DestSeg%, Score#, X%, Y%, Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%, Shade%) STATIC

'Coding for "RelPrnScShadowSL" by Adigun Azikiwe Polack.

' 10 digits in the score                         - Justification: LEFT -

IF Shade% <= 0 THEN Shade% = 0
IF Shade% >= 2 THEN Shade% = 2

IF Score# < 0# THEN Score# = 0#
IF Score# > 9999999999# THEN Score# = 9999999999#

IF ShaX% = 0 AND ShaY% = 0 THEN
SELECT CASE Shade%
  CASE 0: RelPrintSC DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintS DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintSB DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
END SELECT
ELSE
SELECT CASE Shade%
  CASE 0: RelPrintShadowSC DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 1: RelPrintShadowS DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
  CASE 2: RelPrintShadowSB DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
END SELECT
END IF

END SUB

SUB RelPrnScShadowSR (DestSeg%, Score#, X%, Y%, Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%, Shade%)

'Coding for "RelPrnScShadowSR" by Adigun Azikiwe Polack.

' 10 digits in the score                         - Justification: RIGHT -

IF Score# < 0# THEN Score# = 0#
IF Score# > 9999999999# THEN Score# = 9999999999#
Scotab# = Score#

IF Shade% <= 0 THEN Shade% = 0
IF Shade% >= 2 THEN Shade% = 2

IF Scotab# >= 0 AND Scotab# <= 9.9999# THEN
  IF ShaX% = 0 AND ShaY% = 0 THEN
    SELECT CASE Shade%
      CASE 0: RelPrintSC DestSeg%, (X% + (72 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 1: RelPrintS DestSeg%, (X% + (72 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 2: RelPrintSB DestSeg%, (X% + (72 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
    END SELECT
  ELSE
    SELECT CASE Shade%
      CASE 0: RelPrintShadowSC DestSeg%, (X% + (72 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 1: RelPrintShadowS DestSeg%, (X% + (72 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 2: RelPrintShadowSB DestSeg%, (X% + (72 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
    END SELECT
  END IF
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10 AND Scotab# <= 99.9999# THEN
  IF ShaX% = 0 AND ShaY% = 0 THEN
    SELECT CASE Shade%
      CASE 0: RelPrintSC DestSeg%, (X% + (64 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 1: RelPrintS DestSeg%, (X% + (64 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 2: RelPrintSB DestSeg%, (X% + (64 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
    END SELECT
  ELSE
    SELECT CASE Shade%
      CASE 0: RelPrintShadowSC DestSeg%, (X% + (64 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 1: RelPrintShadowS DestSeg%, (X% + (64 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 2: RelPrintShadowSB DestSeg%, (X% + (64 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
    END SELECT
  END IF
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100 AND Scotab# <= 999.9999# THEN
  IF ShaX% = 0 AND ShaY% = 0 THEN
    SELECT CASE Shade%
      CASE 0: RelPrintSC DestSeg%, (X% + (56 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 1: RelPrintS DestSeg%, (X% + (56 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 2: RelPrintSB DestSeg%, (X% + (56 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
    END SELECT
  ELSE
    SELECT CASE Shade%
      CASE 0: RelPrintShadowSC DestSeg%, (X% + (56 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 1: RelPrintShadowS DestSeg%, (X% + (56 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 2: RelPrintShadowSB DestSeg%, (X% + (56 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
    END SELECT
  END IF
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000 AND Scotab# <= 9999.999900000001# THEN
  IF ShaX% = 0 AND ShaY% = 0 THEN
    SELECT CASE Shade%
      CASE 0: RelPrintSC DestSeg%, (X% + (48 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 1: RelPrintS DestSeg%, (X% + (48 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 2: RelPrintSB DestSeg%, (X% + (48 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
    END SELECT
  ELSE
    SELECT CASE Shade%
      CASE 0: RelPrintShadowSC DestSeg%, (X% + (48 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 1: RelPrintShadowS DestSeg%, (X% + (48 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 2: RelPrintShadowSB DestSeg%, (X% + (48 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
    END SELECT
  END IF
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000 AND Scotab# <= 99999.9999# THEN
  IF ShaX% = 0 AND ShaY% = 0 THEN
    SELECT CASE Shade%
      CASE 0: RelPrintSC DestSeg%, (X% + (40 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 1: RelPrintS DestSeg%, (X% + (40 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 2: RelPrintSB DestSeg%, (X% + (40 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
    END SELECT
  ELSE
    SELECT CASE Shade%
      CASE 0: RelPrintShadowSC DestSeg%, (X% + (40 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 1: RelPrintShadowS DestSeg%, (X% + (40 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 2: RelPrintShadowSB DestSeg%, (X% + (40 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
    END SELECT
  END IF
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000 AND Scotab# <= 999999.9999000001# THEN
  IF ShaX% = 0 AND ShaY% = 0 THEN
    SELECT CASE Shade%
      CASE 0: RelPrintSC DestSeg%, (X% + (32 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 1: RelPrintS DestSeg%, (X% + (32 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 2: RelPrintSB DestSeg%, (X% + (32 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
    END SELECT
  ELSE
    SELECT CASE Shade%
      CASE 0: RelPrintShadowSC DestSeg%, (X% + (32 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 1: RelPrintShadowS DestSeg%, (X% + (32 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 2: RelPrintShadowSB DestSeg%, (X% + (32 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
    END SELECT
  END IF
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000 AND Scotab# <= 9999999.9999# THEN
  IF ShaX% = 0 AND ShaY% = 0 THEN
    SELECT CASE Shade%
      CASE 0: RelPrintSC DestSeg%, (X% + (24 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 1: RelPrintS DestSeg%, (X% + (24 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 2: RelPrintSB DestSeg%, (X% + (24 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
    END SELECT
  ELSE
    SELECT CASE Shade%
      CASE 0: RelPrintShadowSC DestSeg%, (X% + (24 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 1: RelPrintShadowS DestSeg%, (X% + (24 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 2: RelPrintShadowSB DestSeg%, (X% + (24 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
    END SELECT
  END IF
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000000 AND Scotab# <= 99999999.9999# THEN
  IF ShaX% = 0 AND ShaY% = 0 THEN
    SELECT CASE Shade%
      CASE 0: RelPrintSC DestSeg%, (X% + (16 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 1: RelPrintS DestSeg%, (X% + (16 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 2: RelPrintSB DestSeg%, (X% + (16 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
    END SELECT
  ELSE
    SELECT CASE Shade%
      CASE 0: RelPrintShadowSC DestSeg%, (X% + (16 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 1: RelPrintShadowS DestSeg%, (X% + (16 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 2: RelPrintShadowSB DestSeg%, (X% + (16 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
    END SELECT
  END IF
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000000 AND Scotab# <= 999999999.9999# THEN
  IF ShaX% = 0 AND ShaY% = 0 THEN
    SELECT CASE Shade%
      CASE 0: RelPrintSC DestSeg%, (X% + (8 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 1: RelPrintS DestSeg%, (X% + (8 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 2: RelPrintSB DestSeg%, (X% + (8 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
    END SELECT
  ELSE
    SELECT CASE Shade%
      CASE 0: RelPrintShadowSC DestSeg%, (X% + (8 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 1: RelPrintShadowS DestSeg%, (X% + (8 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 2: RelPrintShadowSB DestSeg%, (X% + (8 * Xscale%)), Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
    END SELECT
  END IF
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000000 AND Scotab# <= 9999999999.99# THEN
  IF ShaX% = 0 AND ShaY% = 0 THEN
    SELECT CASE Shade%
      CASE 0: RelPrintSC DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 1: RelPrintS DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
      CASE 2: RelPrintSB DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
    END SELECT
  ELSE
    SELECT CASE Shade%
      CASE 0: RelPrintShadowSC DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 1: RelPrintShadowS DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
      CASE 2: RelPrintShadowSB DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%
    END SELECT
  END IF
  Scotab# = Scotab# - Scotab#
END IF

END SUB

