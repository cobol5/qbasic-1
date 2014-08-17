'Ellipse, RelBorder, and Mosaic Example for Rellib,
'by Adigun Azikiwe Polack.

'$INCLUDE: 'rellib.bi'
REDIM SHARED Vpage(31999) AS INTEGER
DIM SHARED Layer%
Layer% = VARSEG(Vpage(0))

RelInit320x133
SLEEP
FOR R = 800 TO 0 STEP -1
RelEllipseF Layer%, 160, 66, R, R, 1
RelBorderH Layer%, 20, 0
RelPrintShadowB Layer%, 0, 100, LTRIM$(STR$(R)), 31, 0, 1, 0, 27, 0
RelPrintShadowC Layer%, 220, 100, LTRIM$(STR$(R)), 31, 0, 1, 0, 27, FALSE
RelPrintES Layer%, 0, 24, "God bless you and WELCOME!!!  ;) !", 16, 21, 1, 2, TRUE, TRUE, 1
'RelPrintESB Layer%, 0, 24, "God bless you and WELCOME!!!  ;) !", 26, 31, 1, 1, True, True, 1
'RelPrintESC Layer%, 0, 48, "God bless you and WELCOME!!!  ;) !", 26, 31, 1, 2, True, True, 1
RelPCopy VIDEO, Layer%
RelCLS Layer%, 0
NEXT
RelEllipseFI Layer%, 160, 66, 56, 35, 31, 1
RelPCopy VIDEO, Layer%
RelCLS Layer%, 0
SLEEP
RelFfix
RelCLS Layer%, 0
x = 64
FOR R = 0 TO 130
RelEllipseFI Layer%, 160, 66, 70, 70, 31, 2
IF x > 1 THEN x = x - 1
RelMosaic Layer%, 0, 0, 319, 199, x
RelScanlinesH Layer%, 0, 0, 320, 200, 0, 0, 4
RelScanlinesH Layer%, 0, 1, 320, 200, 26, 1, 3
RelPrintShadowB Layer%, 0, 125, "1P-        0 HI-  1748000 INSERT COIN!", 31, 0, 1, 0, 27, 0
RelPCopy VIDEO, Layer%
RelCLS Layer%, 0
NEXT
RelFadeToNegative 13
SLEEP
RelPCopy Layer%, VIDEO

RelFade 0, 0, 0, 163
RelInitText

