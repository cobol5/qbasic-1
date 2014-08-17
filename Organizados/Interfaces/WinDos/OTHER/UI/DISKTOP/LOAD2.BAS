DECLARE SUB SetPal (c%, r%, g%, B%)

SCREEN 12
CLS



Red$ = " "
Green$ = " "
Blue$ = " "

OPEN "di_htech.pal" FOR BINARY AS #1
Text$ = SPACE$(8)
GET #1, , Text$
PRINT Text$,
GET #1, , TextLen%
Text$ = SPACE$(TextLen%)
GET #1, , Text$
PRINT Text$
  
  FOR ColourAtt% = 0 TO 15
  GET #1, , Red$
  GET #1, , Green$
  GET #1, , Blue$
  SetPal ColourAtt%, ASC(Red$), ASC(Green$), ASC(Blue$)
  NEXT

CLOSE #1



  FOR i = 0 TO 15
  COLOR i
  PRINT i;
  NEXT



REDIM Picture(5000) AS INTEGER

DEF SEG = VARSEG(Picture(0))
BLOAD "status.bin", 0
DEF SEG


  DO
  CLS

    DO
    KH$ = INKEY$
    LOOP UNTIL LEN(KH$)

    FOR y% = 0 TO 400 STEP 27
    PUT (0, y%), Picture, PSET
    NEXT

    IF KH$ = CHR$(27) THEN EXIT DO

  LOOP


DEFINT A-Z
SUB SetPal (c, r, g, B)

OUT &H3C8, c
OUT &H3C9, r
OUT &H3C9, g
OUT &H3C9, B

END SUB

