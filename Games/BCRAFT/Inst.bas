SCREEN 0
CLS
KEY 16, CHR$(0) + CHR$(73)
KEY 17, CHR$(0) + CHR$(81)
ON KEY(16) GOSUB pgUp
ON KEY(17) GOSUB pgDwn
KEY(16) ON
KEY(17) ON
DIM SHARED help$(140)
COLOR 7
OPEN "help.dat" FOR INPUT AS #1
FOR x = 1 TO 138
INPUT #1, help$(x)
NEXT
FOR x = 1 TO 22
LOCATE , 2: PRINT help$(x)
NEXT
DO
k$ = INKEY$
LOCATE 1
IF k$ = CHR$(0) + "P" THEN GOSUB dwn
IF k$ = CHR$(0) + "H" THEN GOSUB up
IF k$ = CHR$(0) + "I" THEN GOSUB pgUp
IF k$ = CHR$(0) + "Q" THEN GOSUB pgDwn
IF k$ = CHR$(27) THEN
RUN "present.bas"
END IF
LOOP
dwn:
pix = pix + 1
IF pix > 116 THEN SOUND 100, .1: pix = 116
GOSUB reput
RETURN
up:
pix = pix - 1
IF pix < 1 THEN SOUND 100, .1: pix = 1
GOSUB reput
RETURN
pgDwn:
pix = pix + 10
IF pix > 116 THEN SOUND 100, .1: pix = 116
GOSUB reput
RETURN
pgUp:
pix = pix - 10
IF pix < 1 THEN SOUND 100, .1: pix = 1
GOSUB reput
RETURN
reput:
FOR x = pix TO pix + 22
LOCATE x - pix + 1, 2: PRINT help$(x)
NEXT
RETURN
reput0:
RETURN

