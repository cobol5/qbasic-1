DIM shotx%(100), shoty%(100)
DIM ex%(100), ey%(100), edx%(100), edy%(100)
FOR i% = 0 TO 100
 ex%(i%) = RND * 78 + 1
 ey%(i%) = -(RND * 78 + 1)
' edx%(i%) = 1'RND * 2 + 1
 edy%(i%) = 1'RND * 5 + 1
NEXT i%
e% = ASC("*")
ez! = 10
SCREEN 0
WIDTH 80, 25
CLS
WHILE e$ <> CHR$(27) AND health% < 100
ez% = ez!
e$ = INKEY$
x% = STICK(0) / 2
IF x% < 1 THEN x% = 1
IF x% > 79 THEN x% = 79
u$ = SPACE$(80)
MID$(u$, x%, 1) = CHR$(219)
IF STRIG(0) <> 0 THEN
 shotx%(sz%) = x%
 shoty%(sz%) = 24
 sz% = sz% + 1
 IF sz% > 100 THEN sz% = 0
END IF
FOR i% = 0 TO ez%
IF ey%(i%) >= 1 AND ey%(i%) <= 79 THEN
 LOCATE ey%(i%), ex%(i%)
 PRINT " ";
END IF
ey%(i%) = ey%(i%) + edy%(i%)
IF ey%(i%) > 25 THEN
 ey%(i%) = -(RND * 20 + 1)
 IF RND > .49 THEN
 ' edx%(i%) = 1
 ELSE
 ' edx%(i%) = -1
 END IF
END IF
ex%(i%) = ex%(i%) + edx%(i%)
IF ex%(i%) < 1 THEN
 ex%(i%) = 1
 edx%(i%) = 1' + RND * 3
END IF
IF ex%(i%) > 79 THEN
 ex%(i%) = 79
 edx%(i%) = -1  '(1 + RND * 3)
END IF
IF ey%(i%) >= 1 AND ey%(i%) <= 79 THEN
 LOCATE ey%(i%), ex%(i%)
 COLOR 1 + i% \ 7
 PRINT "*";
END IF
NEXT i%
'LOCATE 1, 1: PRINT e%
ich% = SCREEN(25, x%)
IF ich% = e% THEN
 FOR i% = 1000 TO 100 STEP -10
 SOUND i%, .1
 NEXT i%
 health% = health% + 10
END IF
COLOR 12
FOR i% = 0 TO 100
 IF shoty%(i%) > -1 AND shotx%(i%) <> 0 THEN
  IF shoty%(i%) > 0 THEN
'   da% = SCREEN(shoty%(i%), shotx%(i%))
   da% = SCREEN(shoty%(i%), shotx%(i%))
   IF da% = e% THEN
    FOR ii% = 0 TO ez%
     IF ex%(ii%) = shotx%(i%) AND ey%(ii%) = shoty%(i%) THEN
     score% = score% + 1
      SOUND 1000, .1
      ey%(ii%) = -(RND * 20 + 1)
      IF RND > .49 THEN
  '     edx%(ii%) = 1
      ELSE
   '    edx%(ii%) = -1
      END IF
      EXIT FOR
     END IF
    NEXT ii%
   END IF
   LOCATE shoty%(i%), shotx%(i%)
   PRINT ".";
  END IF
  LOCATE shoty%(i%) + 1, shotx%(i%)
  PRINT " ";
  shoty%(i%) = shoty%(i%) - 1
 END IF
NEXT i%
IF ez! < 100 THEN ez! = ez! + .1
COLOR 15
LOCATE 1, 1: PRINT "ScOrE:  "; score%; "     This could be YOUR FANTASTIC Bonus-Event"
LOCATE 1, 74: PRINT ":hEaLtH"
LOCATE 1, 69: PRINT 100 - health%; " ";
COLOR 13
LOCATE 25, 1: PRINT u$;
PALETTE 0, 0
WEND
CLS
SHELL "mem"
INPUT dodo$

FOR i% = 0 TO 10000
LOCATE 1 + RND * 24, 1 + RND * 79
COLOR 1 + RND * 14
PRINT CHR$(176 + RND * 3);
NEXT i%
FOR i% = 0 TO 10000
LOCATE 1 + RND * 24, 1 + RND * 79
COLOR 15
PRINT " ";
NEXT i%
CLS

