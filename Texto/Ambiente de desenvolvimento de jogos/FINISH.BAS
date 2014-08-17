'end-screen for KABOOM-3D-Engine
SCREEN 12
OUT &H3C8, 0                'set 16 green-shades
FOR a% = 0 TO 15
 OUT &H3C9, (0)
 OUT &H3C9, (a% * 4)
 OUT &H3C9, (0)
NEXT a%

 
DIM s%(79)
FOR i% = 0 TO 79
s%(i%) = RND * -10
NEXT i%

DIM sm%(79)
FOR i% = 0 TO 79
sm%(i%) = s%(i%) + 20'((50 - s%(i%)) * RND)
NEXT i%
LOCATE 15, 25: PRINT " CONGRATULATION END OF THIS GAME "

WHILE a$ <> CHR$(27)
a$ = INKEY$
FOR i% = 0 TO 78
    IF s%(i%) >= 0 AND s%(i%) <= 29 AND (s%(i%) <> 14 OR (i% < 24 OR i% > 56)) THEN
    LOCATE s%(i%) + 1, i% + 1
    COLOR 15 'farbe!
    PRINT CHR$(128 + (RND * 100));
    END IF

 IF s%(i%) > 0 THEN
farbe! = 8
  FOR j% = s%(i%) - 1 TO 0 STEP -1
   farbe! = farbe! - .4
   IF farbe! < 3 THEN farbe! = 3
    IF j% <= 29 AND (j% <> 14 OR (i% < 24 OR i% > 56)) THEN
   LOCATE j% + 1, i% + 1
    COLOR farbe!
     PRINT CHR$(128 + (RND * 100));
    END IF
  NEXT j%
 END IF
 IF s%(i%) < sm%(i%) THEN
  s%(i%) = s%(i%) + 1
  IF s%(i%) > 70 THEN
   s%(i%) = 0
   sm%(i%) = RND * 50
  END IF
 ELSE
  IF RND < .05 THEN
   s%(i%) = 0
   sm%(i%) = RND * 50
  END IF
 END IF
NEXT i%
COLOR 15
 
WEND

'reset KABOOM Result-Information to Initial Settings.
OPEN "booreslt.dat" FOR OUTPUT AS #2
CLOSE #2
OPEN "booreslt.dat" FOR APPEND AS #2
PRINT #2, "0"
PRINT #2, "0"
CLOSE #2

END

FOR i% = 0 TO 15
COLOR i%
'PRINT i%;
NEXT i%

