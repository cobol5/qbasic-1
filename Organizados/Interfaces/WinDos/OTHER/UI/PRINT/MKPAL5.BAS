DECLARE SUB SetPal (c%, r%, g%, B%)
DECLARE FUNCTION getcol% ()
DECLARE SUB MakePal (Filename$, Util$, Text$)



SetPal 0, 0, 0, 0
SetPal 1, 0, 4, 3
SetPal 2, 0, 8, 7
SetPal 3, 0, 12, 11
SetPal 4, 0, 16, 15
SetPal 5, 0, 20, 19
SetPal 6, 0, 24, 23
SetPal 7, 0, 28, 27
SetPal 8, 0, 32, 30
SetPal 9, 0, 36, 34
SetPal 10, 0, 40, 38
SetPal 11, 0, 44, 42
SetPal 12, 0, 48, 46
SetPal 13, 0, 52, 50
SetPal 14, 63, 63, 0
SetPal 15, 63, 63, 63


OPEN "pn_cold.pal" FOR BINARY AS #1
Text$ = "PRINT   "
PUT #1, , Text$
Text$ = "Cold-Blue Palette"
Text% = LEN(Text$)
PUT #1, , Text%
PUT #1, , Text$
  FOR k% = 1 TO (16 * 3) / 2
  num1% = getcol%
  num2% = getcol%
  num3% = num2% * 256 + num1%
  PUT #1, , num3%
  NEXT
CLOSE #1

DEFINT A-Z
FUNCTION getcol%

STATIC i%
STATIC times%

  IF times% = 3 THEN times% = 0
times% = times% + 1

  SELECT CASE times%
    CASE 1
    OUT &H3C7, i%
    i% = i% + 1
  END SELECT


getcol% = INP(&H3C9)

END FUNCTION

DEFSNG A-Z
SUB SetPal (c%, r%, g%, B%)

OUT &H3C8, c%
OUT &H3C9, r%
OUT &H3C9, g%
OUT &H3C9, B%

END SUB

