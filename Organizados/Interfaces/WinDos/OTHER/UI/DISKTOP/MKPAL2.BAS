DECLARE SUB SetPal (c%, r%, g%, B%)
DECLARE FUNCTION getcol% ()


SetPal 0, 0, 0, 0
SetPal 1, 0, 38, 39
SetPal 2, 0, 43, 42
SetPal 3, 0, 45, 44
SetPal 4, 0, 47, 47
SetPal 5, 0, 49, 48
SetPal 6, 0, 51, 50
SetPal 7, 0, 53, 53
SetPal 8, 0, 63, 62
SetPal 9, 32, 0, 0
SetPal 10, 63, 0, 0
SetPal 11, 0, 0, 63
SetPal 12, 63, 0, 63
SetPal 13, 0, 45, 0
SetPal 14, 63, 63, 0
SetPal 15, 63, 63, 63


OPEN "di_cold.pal" FOR BINARY AS #1
Text$ = "DISKTOP "
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
CLOSE

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

