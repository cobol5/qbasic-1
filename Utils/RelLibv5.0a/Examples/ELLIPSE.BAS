'Ellipse and RelBorder example for Rellib,
'by Adigun Azikiwe Polack.

'$INCLUDE: 'rellib.bi'
REDIM SHARED Vpage(31999) AS INTEGER
DIM SHARED Layer%
Layer% = VARSEG(Vpage(0))

RelInit320x133
SLEEP
FOR R = 800 TO 0 STEP -1
RelEllipseF Layer%, 160, 66, R, R, 18
RelBorderH Layer%, 20, 0
RelPrintShadowB Layer%, 0, 0, LTRIM$(STR$(R)), 31, 0, 1, 0, 27, 0
RelPrintShadowC Layer%, 220, 0, LTRIM$(STR$(R)), 31, 0, 1, 0, 27, FALSE
RelScanlinesH Layer%, 0, 0, 320, 200, 26, 1, 1
RelScanlinesH Layer%, 0, 1, 320, 200, 26, 1, 3
RelPCopy VIDEO, Layer%

RelCLS Layer%, 0
NEXT
RelEllipseFI Layer%, 160, 66, 56, 35, 31, 1
RelPCopy VIDEO, Layer%
RelCLS Layer%, 0
SLEEP
RelFfix
RelCLS Layer%, 0
FOR R = 0 TO 80
RelEllipseFI Layer%, 160, 66, R, R, 31, 3
RelPrintShadowB Layer%, 0, 125, "1P-        0 HI-  1748000 INSERT COIN!", 31, 0, 1, 0, 27, 0
RelPCopy VIDEO, Layer%
RelCLS Layer%, 0
NEXT
RelFadeToNegative 13
SLEEP
RelFade 0, 0, 0, 163
RelInitText

