'Welcome to tank 2000 by Gordin Media
'(Http://www.geocities.com/southbeach/sands/6315/menu.html)
'this program was made by Vadim Gordin
'the instructions are pretty simple, just push any key to fire a missile
'when the number of shots left gets down to zero the program terminates
'if anyone would like to help out on a command and conquer type version
'of this game please leave a message at my site.
'
'******************** needed files ****************
'test.tun.......................played when you hit target
'dance.tun......................intro music
'tank.bas.......................this file (actual game)
'
'
'********************** enjoy *********************
SCREEN 7
OPEN "Test.TUN" FOR INPUT AS #2
INPUT #2, D$
LINE (210, 150)-(225, 152), 3, BF
LINE (225, 148)-(240, 155), 3, BF
LINE (208, 155)-(254, 160), 3, BF
CIRCLE (214, 160), 4, 3
CIRCLE (248, 160), 4, 3
LINE (248, 163)-(214, 163), 3
PAINT (220, 162), 3
PAINT (214, 161), 3
PAINT (248, 161), 3
DIM TANK%(5000)
GET (200, 100)-(290, 170), TANK%
X! = 210
OPEN "DANCE.TUN" FOR INPUT AS #1
INPUT #1, PY$
PLAY PY$
LET HIT! = 0
DO
X! = X! - 1
LINE (X!, 150)-(X! + 15, 152), 3, BF
LINE (X! + 15, 148)-(X! + 30, 155), 3, BF
LINE (X! - 2, 155)-(X! + 44, 160), 3, BF
CIRCLE (X! + 4, 160), 4, 3
CIRCLE (X! + 38, 160), 4, 3
LINE (X! + 38, 163)-(X! + 4, 163), 3
PAINT (X! + 10, 162), 3
PAINT (X! + 4, 161), 3
PAINT (X! + 38, 161), 3
FOR w! = 0 TO 1000: NEXT w!
LINE (X! + 30, 148)-(X! + 60, 154), 0, BF
LINE (X! + 45, 155)-(X! + 45, 160), 0, BF
LINE (X! + 43, 163)-(X! + 45, 160), 0
LOOP UNTIL X! = 5
LOCATE 3, 1
COLOR 3
PRINT "TANK 2000"
FOR w! = 0 TO 10000: NEXT w!
PRINT "BY THE GORDIN MEDIA GROUP"
LOCATE 23, 1
PRINT "            <PRESS ANY KEY>"
DO
LOOP UNTIL INKEY$ <> ""
CLS
COLOR 4
500
SCREEN 0
COLOR 15
LOCATE 5, 3
INPUT "HOW MANY SHOTS DO YOU WANT? ", SHOT!
LOCATE 5, 3
PRINT "                                      "
IF SHOT! < 0 OR SHOT! > 50 OR SHOT! = 0 THEN
        GOTO 500
END IF
SCREEN 13
100 LET missilelength! = 10
IF HIT! = 1 THEN
        LINE (0, 0)-(320, 250), 0, BF
END IF
IF SHOT! = 0 THEN
        END
END IF
LET speed! = 200
2400
5700
LET colr! = 12
LET X! = 200
LET Y! = 150
C! = 1
LINE (210, 150)-(225, 152), 3, BF
LINE (225, 148)-(240, 155), 3, BF
LINE (208, 155)-(254, 160), 3, BF
CIRCLE (214, 160), 4, 3
CIRCLE (248, 160), 4, 3
LINE (-1, 164)-(390, 164), 15
LINE (248, 163)-(214, 163), 3
PAINT (220, 162), 3
PAINT (214, 161), 3
PAINT (248, 161), 3
LOCATE 2, 5
PRINT SHOT!; " shot(s) left"
FOR w! = 0 TO 1000: NEXT w!
LET YTOP! = 90
LET YB! = 100
DO
LINE (50, YTOP!)-(60, YB!), 3, BF
FOR w! = 0 TO 500: NEXT w!
LINE (50, YTOP!)-(60, YTOP!), 0, BF
YB! = YB! + 1
YTOP! = YTOP! + 1
IF INKEY$ <> "" THEN
        IF INKEY$ = CHR$(27) THEN
                END
        ELSE
                GOTO 10
        END IF
END IF
LOOP UNTIL YB! = 163
DO
LINE (50, YTOP!)-(60, YB!), 3, BF
FOR w! = 0 TO 500: NEXT w!
LINE (50, YB!)-(60, YB!), 0, BF
YB! = YB! - 1
YTOP! = YTOP! - 1
IF INKEY$ <> "" THEN
        IF INKEY$ = CHR$(27) THEN
                END
        ELSE
                GOTO 10
        END IF
END IF
LOOP UNTIL YTOP! = 90
GOTO 100
10 DO
PSET (X!, Y! + 2), colr!
PSET (X! - 1, Y! + 1), colr!
PSET (X! - 2, Y!), colr!
PSET (X! - 1, Y! - 1), colr!
PSET (X!, Y! - 2), colr!
FOR w! = 0 TO speed!: NEXT w!
PRESET (X! + missilelength!, Y!), 0
PRESET (X! + missilelength, Y! + 1), 0
PRESET (X! + missilelength, Y! - 1), 0
PRESET (X! + missilelength, Y! - 2), 0
PRESET (X! + missilelength, Y! + 2), 0
X! = X! - 1
LOOP UNTIL X! = 62
FOR w! = 0 TO 50: NEXT w!
LET col% = 4
IF YB! + 2 > 150 AND YTOP! - 2 < 150 THEN
        HIT! = 1
        score! = score! + 1
        LOCATE 5, 10
        PRINT "HIT"
        FOR w! = 0 TO 100: NEXT w!
        LOCATE 5, 10
        PRINT "                "
        SHOT! = SHOT! - 1
        GOTO 20
ELSE
        LINE (50, 90)-(90, 159), 0, BF
        HIT! = 0
        score! = score! - 1
        LOCATE 5, 10
        PRINT "Sorry You Missed"
        FOR w! = 0 TO 1000: NEXT w!
        LOCATE 5, 10
        PRINT "                   "
        SHOT! = SHOT! - 1
        GOTO 100
END IF
20 LINE (90, 163)-(50, 130), 0, BF
DO
C! = C! + 1
'col% = INT(RND * 10)
X! = INT(RND * 150)
Y! = INT(RND * 150)
LINE (60, 150)-(X!, Y!), col%
LOOP UNTIL C! = 10
PLAY D$
FOR w! = 0 TO 10000: NEXT w!
GOTO 100

