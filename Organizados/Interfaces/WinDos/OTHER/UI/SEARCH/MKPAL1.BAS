DECLARE SUB SetPal (c%, r%, g%, B%)
DECLARE FUNCTION getcol% ()
DECLARE SUB MakePal (Filename$, Util$, Text$)



FOR i% = 0 TO 13
SetPal i%, 0, i% * 4, 0
NEXT

SetPal 14, 63, 63, 0
SetPal 15, 63, 63, 63


OPEN "co_htech.pal" FOR BINARY AS #1
Text$ = "COMPRESS"
PUT #1, , Text$
Text$ = "HiTech (green) Palette"
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

