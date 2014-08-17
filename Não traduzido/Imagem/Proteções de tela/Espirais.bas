DECLARE SUB SPI ()
DECLARE SUB lil.circles ()
DECLARE SUB Sarah ()
DECLARE SUB star ()
DECLARE SUB cosine ()
DECLARE SUB parab ()
DECLARE SUB blank ()
DECLARE SUB cone ()
DECLARE SUB trig ()
DECLARE SUB INTRO ()
DECLARE SUB lines ()
DECLARE SUB PACMAN ()
DECLARE SUB dots ()
DECLARE SUB lil.ovals ()
DECLARE SUB Oval ()
DECLARE SUB password ()
INTRO
begin:

FOR qqq = 0 TO 10
CLS : SCREEN 12
RANDOMIZE TIMER
pick = qqq

IF pick = 0 THEN Oval
IF pick = 1 THEN SPI
IF pick = 2 THEN lil.ovals
IF pick = 3 THEN PACMAN
IF pick = 4 THEN lines
IF pick = 5 THEN trig
IF pick = 6 THEN star
IF pick = 7 THEN cosine
IF pick = 8 THEN Sarah
IF pick = 9 THEN lil.circles
IF pick = 10 THEN cone


SCREEN 12
start:
x = 320: y = 225: d = 0: R = 0

f:
f = RND / 6
IF f < .1 THEN GOTO f

spiral:
d = d + .01
IF d + 1 >= 6.28 THEN d = .01
R = R + f
IF R >= 300 GOTO beginy
q = q + 1: IF q / 2 = INT(q / 2) THEN c = 1 ELSE c = 4
CIRCLE (x, y), R, c, d, d + 1

IF INKEY$ = CHR$(27) THEN password
GOTO spiral
beginy:
NEXT qqq
GOTO begin

SUB cone
SCREEN 12
FOR times = 1 TO 25
x = RND * 620
y = RND * 440
FOR R = 1 TO 50
c = RND * 15
CIRCLE (x, y + R), R, c
IF INKEY$ = CHR$(27) THEN password
FOR stall = 1 TO 100
NEXT stall
NEXT R
NEXT times

END SUB

SUB cosine
SCREEN 12
FOR add = 1 TO 481 STEP 30
c = RND * 15
IF c > 15 OR c < 1 THEN c = 1

FOR x = 1 TO 640
y = COS(6 * x)
a = 10 * SIN(x)
PSET (x, y + add), c
PSET (x, a + add), c
NEXT x

NEXT add

END SUB

SUB INTRO
SCREEN 13
LOCATE 10, 13
COLOR 9
PRINT "Spiral Screen Saver"
LOCATE 11, 13
COLOR 10
PRINT "Progamming by"
LOCATE 12, 13
COLOR 11
PRINT "Justin Rosenberg"
SLEEP (3)

END SUB

SUB lil.circles
SCREEN 12
CLS

FOR x = 1 TO 680 STEP 10
FOR y = 680 TO 1 STEP -10
IF INKEY$ = CHR$(27) THEN password
c = RND * 15
IF c < 1 OR c > 15 THEN c = 1
CIRCLE (x, y), 10, c
NEXT y
NEXT x


END SUB

SUB lil.ovals
SCREEN 12
FOR times = 1 TO 100
x = RND * 640
y = RND * 480

FOR e = 0 TO 1 STEP .04
CIRCLE (x, y), 50, 15, , , e
IF INKEY$ = CHR$(27) THEN password
FOR stall = 1 TO 10
NEXT stall
NEXT e

FOR e = 1 TO 0 STEP -.04
c = RND * 15
CIRCLE (x, y), 50, c, , , e
IF INKEY$ = CHR$(27) THEN password
FOR stall = 1 TO 10
NEXT stall
NEXT e
NEXT times

END SUB

SUB lines
SCREEN 12

FOR times = 1 TO 100
y1 = RND * 450
y2 = RND * 450
c = RND * 15
LINE (0, y1)-(640, y2), c
FOR stall = 1 TO 1000
NEXT stall
NEXT times
END SUB

SUB Oval
SCREEN 12
FOR e = 0 TO 1 STEP .05
c = 15
CIRCLE (320, 240), 200, c, , , e
IF INKEY$ = CHR$(27) THEN password
FOR stall = 1 TO 1000
NEXT stall
NEXT e

FOR e = 0 TO 1 STEP .05
c = RND * 15
CIRCLE (320, 240), 200, c, 0, 3, e
IF INKEY$ = CHR$(27) THEN password
CIRCLE (320, 240), 200, c, 3, 0, e
FOR stall = 1 TO 2000
NEXT stall
NEXT e

END SUB

SUB PACMAN
10 CLS : KEY OFF: SCREEN 12
FOR times = 1 TO 20
20 x = RND * 680
30 y = RND * 440
40 R = 50
50 PI = 3.14
60 a = -(PI / 5)
70 c = c + 1: IF c = 15 THEN c = 1
80 a = a + (PI / 5)
90 IF a >= 2 * PI THEN GOTO 200 ELSE GOTO 106
106 CIRCLE (x, y), R + 50, c, -a, -a + (PI / 5), a
FOR stall = 1 TO 500
NEXT stall
CIRCLE (x, y), R + 50, 0, -a, -a + (PI / 5), a
140 GOTO 80
150 GOTO 70
160 GOTO 20
200 REM 
201 a = a - (PI / 5)
210 IF a < 0 THEN GOTO 260
CIRCLE (x, y), R + 50, 0, -a, -a + (PI / 5), a
FOR stall = 1 TO 500
NEXT stall
CIRCLE (x, y), R + 50, 0, -a, -a + (PI / 5), a
254 GOTO 201
260
CIRCLE (x, y), R + 50, c, -PI, -PI + (PI / 5)
NEXT times

END SUB

SUB password
password:
'COLOR 3
'LOCATE 15, 30
'PRINT "Password =";
'COLOR 0
'INPUT ; a$
'IF a$ = "None" THEN SYSTEM ELSE LOCATE 15, 30: PRINT "           "
SYSTEM
END SUB

SUB Sarah
SCREEN 12

CLS

RANDOMIZE TIMER
a = INT(RND * 1000)
IF a < 500 THEN a = 500
b = a * 10

FOR x = 1 TO 640 STEP .02
y = a * COS(b * x)
c = RND * 15
IF INKEY$ = CHR$(27) THEN password
PSET (x, y), c
NEXT x

END SUB

SUB SPI
FOR times = 1 TO 10
PI = 0
x = RND * 620
y = RND * 440
c = RND * 15
IF c < 1 OR c > 15 THEN c = 1
FOR R = 0 TO 100 STEP .1
PI = PI + .01
IF PI >= 6.28 THEN PI = 0
CIRCLE (x, y), R, c, PI, PI
IF INKEY$ = CHR$(27) THEN password
NEXT R
NEXT times

END SUB

SUB star
SCREEN 12
FOR times = 1 TO 600
x = RND * 640
y = RND * 480
PSET (x, y), 15
NEXT times
END SUB

SUB trig
SCREEN 12
FOR add = 0 TO 480 STEP 30
c = RND * 15
IF c > 15 OR c < 1 THEN c = 1

FOR x = 1 TO 640
y = TAN(6 * x)
PSET (x, y + add), c
NEXT x

NEXT add

END SUB


