'*********************************
'*********************************
'Simple-Pong Version 1.0
'(C)Copyright 1997 Versed Technology By Paul Lowe
'01-20-1997
'Written for FRAME Version 1.0
'*********************************
'*********************************












start:
RANDOMIZE TIMER
CLS


   ' screen mode
 SCREEN 12
 oon$ = "111111"    ' kind of like "True" or "False"
 ooff$ = "000000"          ' T or F
 sound$ = oon$
 ccymin = 6
 ccymax = 12
 jjj = 1
 arrow$ = "Ф"

 quit$ = CHR$(27)

 up$ = CHR$(0) + CHR$(72)   ' up arrow key

 lleft$ = CHR$(0) + CHR$(75)   ' left arrow key

 rright$ = CHR$(0) + CHR$(77)   ' right arrow key

 down$ = CHR$(0) + CHR$(80)    ' down arrow key

 execute$ = CHR$(13)  ' enter key, A.K.A. right arrow key in frame

 backup$ = CHR$(8)    ' backspace key

 defcol = 15          ' default color
 curamt = 2

 ccx = 6     ' cursor x- coordinate for a couple cursors
 ccy = 6     ' cursor y= coordinate for a couple cursors

'boxxlen = 20
'boxylen = 8
'boxcorx = 5
'boxcory = 5
'boxcol = 12












menu:
CLS
COLOR 14
PRINT
PRINT "                                    Simple"
PRINT
PRINT "                   ллллллл   ллллллл   ллл    лл   лллллллл"
PRINT "                   лл   лл   лл   лл   лл     лл   лл      "
PRINT "                   ллллллл   лл   лл   лл     лл   лл  лллл"
PRINT "                   лл        лл   лл   лл     лл   лл    лл"
PRINT "                   лл        ллллллл   лл    ллл   лллллллл"
PRINT
PRINT "                                  Version 1.0"
PRINT "                       (C)Copyright 1997 Versed Technology"
PRINT "                                  By Paul Lowe"
COLOR 14

LINE (316, 47)-(359, 127)
LINE (317, 47)-(336, 47)
LINE (336, 47)-(375, 127)
LINE (375, 127)-(360, 127)

PAINT (340, 75), 14
boxcorx = 30
boxcory = 13
boxcol = 10
boxxlen = 20
boxylen = 8
GOSUB drawbox:
COLOR 15
PLAY "o" + STR$(2)
PLAY "MBL16aaL8bp4L16bbL8>c#"
LOCATE 15, 36: PRINT "Play Pong"
LOCATE 17, 36: PRINT "About"
LOCATE 19, 36: PRINT "Quit"
ccymin = 15
ccymax = 19
ccy = 15
ccx = 33
curcol = 12

' left box

LINE (2, 10)-(125, 10) '  right box top
LINE (125, 10)-(125, 400) ' right box right side
LINE (125, 400)-(2, 400)' right box bottom side
LINE (2, 400)-(2, 10)  ' right box left side


' right box

LINE (500, 10)-(626, 10) ' left box top side
LINE (626, 10)-(626, 400) ' left box right side
LINE (626, 400)-(500, 400) ' left box bottom side
LINE (500, 400)-(500, 10)' left box ' left side



ball1x = 60
ball1y = 200

ball2x = 570
ball2y = 200
slope1x = 3
slope2x = -2
slope1y = 4
slope2y = -1







Menuaction:
IF key$ = "s" OR key$ = "S" THEN IF sound$ = oon$ THEN sound$ = ooff$: key$ = ""
IF key$ = "s" OR key$ = "S" THEN IF sound$ = ooff$ THEN sound$ = oon$: key$ = ""



COLOR curcol: LOCATE ccy, ccx: PRINT arrow$     ' update arrow location
key$ = INKEY$                                     ' reassign inkey$ to key$

IF key$ = quit$ THEN COLOR 7: CLS : SYSTEM                ' if they hit ESC
IF key$ = down$ THEN GOSUB curdown:                ' down arrow
IF key$ = up$ THEN GOSUB curup:                           ' up arrow
IF key$ = execute$ OR key$ = rright$ THEN GOTO menuselect:    ' enter and right are same thing



' left has no function


bballs:
x = TIMER
x = x + .01
DO WHILE x > TIMER: LOOP

CIRCLE (ball1x, ball1y), 5, 0
PAINT (ball1x, ball1y), 0

ball1x = ball1x + slope1x
ball1y = ball1y + slope1y

IF ball1y < 20 OR ball1y > 385 THEN g = 275: GOSUB boardsound: slope1y = -(slope1y)
IF ball1x < 20 OR ball1x > 115 THEN g = 275: GOSUB boardsound: slope1x = -(slope1x)




CIRCLE (ball1x, ball1y), 5, 14
PAINT (ball1x, ball1y), 14


'second ball



'LINE (500, 10)-(626, 10) ' left box top side
'LINE (626, 10)-(626, 400) ' left box right side
'LINE (626, 400)-(500, 400) ' left box bottom side
'LINE (500, 400)-(500, 10)' left box ' left side


CIRCLE (ball2x, ball2y), 5, 0
PAINT (ball2x, ball2y), 0

ball2x = ball2x + slope2x
ball2y = ball2y + slope2y

IF ball2x < 520 OR ball2x > 610 THEN g = 225: GOSUB boardsound: slope2x = -(slope2x)
IF ball2y < 30 OR ball2y > 390 THEN g = 225: GOSUB boardsound: slope2y = -(slope2y)


CIRCLE (ball2x, ball2y), 5, 14
PAINT (ball2x, ball2y), 14












GOTO Menuaction:

eexit:

menuselect:
IF ccy = 15 THEN GOTO begin: ' go there
IF ccy = 17 THEN GOTO about:   ' go there
IF ccy = 19 THEN SYSTEM      ' this is all based on the y- coordinate of the arrow
GOTO begin:

about:
CLS
PRINT
PRINT
PRINT
COLOR 14
PRINT "Controls are: "
PRINT
PRINT "Player 1 - Up = W / Down = A"
PRINT
PRINT "Player 2 - Up = UP ARROW KEY / Down = DOWN ARROW KEY"
PRINT
PRINT "Hit ESC to quit a game in progress."
PRINT
PRINT "Hit P to pause the game."
PRINT
PRINT "Hit S to toggle sound ON and OFF "
PRINT
COLOR 15
PRINT "Simple-Pong Version 1.0"
PRINT "(C)Copyright 1997 Versed Technology By Paul Lowe"
PRINT "01-19-1997"
PRINT
COLOR 13
PRINT "Any donation would be greatly appreciated!"
PRINT "Help and information may be reached at (916) 483-5887"
PRINT
COLOR 12
PRINT "This program is FREEWARE -- COPY AS MUCH AS YOU PLEASE!"
PRINT
COLOR 10
PRINT "Press any key to continue..."
SLEEP INKEY$ = ""
key$ = ""
key$ = INKEY$
GOTO start:








curup:
IF ccy = ccymin THEN RETURN ' if at the bottom of list...return
ccy = ccy - curamt
COLOR 12: LOCATE ccy, ccx: PRINT arrow$ ' print new arrow

COLOR 0: LOCATE ccy + 2, ccx: PRINT arrow$' erase current cursor




RETURN

curdown:
IF ccy = ccymax THEN RETURN ' if at the bottom of list...return
ccy = ccy + curamt

COLOR 0: LOCATE ccy - 2, ccx: PRINT arrow$' erase current cursor
COLOR 12: LOCATE ccy, ccx: PRINT arrow$ ' print new arrow
RETURN
















begin:



CLS
' distance between top and bottom wall = 340
' each wall is 550 units long r to l
' pongs are 10 units long r to l
' pongs 70 units tall
' top wall hit point is 100
' bottom wall hit point is 410
' front of player1 is 50
' front of player2 is 610

ballx = 225 + 50
bally = 170 + 85
board1 = 50
board2 = 610



LINE (40, 220)-(50, 290), 12, BF  ' left player
LINE (600, 220)-(610, 290), 9, BF  ' right player

player1x = 40
player2x = 600
player1y = 220
player2y = 220

choser = FIX(RND * 102)

IF choser > 49 THEN slopex = -15
IF choser < 51 THEN slopex = 15
slopey = 0

sound$ = oon$

loadgraphics:
LINE (50, 410)-(600, 440), 10, BF
LINE (50, 70)-(600, 100), 10, BF
LINE (player1x, player1y)-(player1x + 10, player1y + 70), 12, BF
LINE (player2x, player2y)-(player2x + 10, player2y + 70), 9, BF
COLOR 8
LOCATE 1, 1: PRINT STRING$(80, "л")
LOCATE 4, 1: PRINT STRING$(80, "л")
a = 1
FOR l = 1 TO 4
LOCATE a, 1: PRINT "лл"
a = a + 1
NEXT l


a = 1
FOR l = 1 TO 4
LOCATE a, 79: PRINT "лл"
a = a + 1
NEXT l





COLOR 15
LOCATE 17, 33: COLOR 15: PRINT "Hit any key to start"
SLEEP INKEY$ = "\"
LOCATE 17, 33: COLOR 15: PRINT "                    "









COLOR 10
LOCATE 2, 63: PRINT "Sound = "
actioninput:
COLOR 8


COLOR 15
COLOR 15
IF sound$ = ooff$ THEN LOCATE 2, 72: PRINT "OFF"
IF sound$ = oon$ THEN LOCATE 2, 72: PRINT "ON "
key$ = INKEY$

IF key$ = up$ THEN movey = -30: GOSUB moveplayer2:
IF key$ = down$ THEN movey = 30: GOSUB moveplayer2:
IF key$ = "w" OR key$ = "W" THEN movey = -30: GOSUB moveplayer1:
IF key$ = "a" OR key$ = "A" THEN movey = 30: GOSUB moveplayer1:
IF key$ = CHR$(27) THEN boxcol = 14: textcol = 15: GOTO quit:
IF key$ = "p" OR key$ = "P" THEN GOTO pause:

IF key$ = "s" OR key$ = "S" THEN IF sound$ = oon$ THEN sound$ = ooff$: key$ = ""
IF key$ = "s" OR key$ = "S" THEN IF sound$ = ooff$ THEN sound$ = oon$: key$ = ""

MoveBall:

CIRCLE (ballx, bally), 5, 0    ' erase circle
PAINT (ballx, bally), 0        ' erase circ;e


IF ballx < board1 THEN GOSUB badsound: player$ = "Player 2": GOSUB winner:
IF ballx > board2 THEN GOSUB badsound: player$ = "Player 1": GOSUB winner:




IF bally <= 110 + 20 THEN slopey = -(slopey): GOSUB bouncesound: LINE (50, 410)-(600, 440), 10, BF    ' hit a wall ' top wall
IF bally >= 410 - 20 THEN slopey = -(slopey): GOSUB bouncesound: LINE (50, 70)-(600, 100), 10, BF            ' hit a wall ' bottom wall
                                                                                                                                                                                                                                        
IF bally > player1y AND bally < player1y + 70 THEN IF ballx < board1 + ABS(slopex) THEN : g = 250: GOSUB boardhit1   ' if pong hit... reverse movement
IF bally > player2y AND bally < player2y + 70 THEN IF ballx > board2 - ABS(slopex) THEN : g = 225: GOSUB boardhit2 ' if pong hit... reverse movement

ballx = ballx + slopex ' add to x slope
bally = bally + slopey ' add to y slope
COLOR 15
COLOR 12
LOCATE 2, 3: PRINT "Player 1 = "; INT(player1points); " Points"
COLOR 9
LOCATE 3, 3: PRINT "Player 2 = "; INT(player2points); " Points"
COLOR 15
IF player1points > player2points THEN lead$ = "Player 1"
IF player2points > player1points THEN lead$ = "Player 2"
IF player1points = player2points THEN lead$ = "TIE!"
LOCATE 2, 40: PRINT "Lead Winner = "; lead$
LOCATE 3, 40: PRINT "Speed = "; INT(ABS(slopex))

speed = INT(ABS(slopex))
CIRCLE (ballx, bally), 5, 14
PAINT (ballx, bally), 14
IF speed < 17 THEN hardness$ = "(Super Easy)": GOTO ph:

IF speed >= 17 AND speed < 22 THEN hardness$ = "(Easy)      ": GOTO ph:

IF speed >= 22 AND speed < 27 THEN hardness$ = "(Medium)    ": GOTO ph:
IF speed >= 27 AND speed < 35 THEN hardness$ = "(Hard)": GOTO ph:

IF speed >= 35 AND speed < 40 THEN hardness$ = "(Very Hard) ": GOTO ph:

IF speed >= 40 AND speed < 201 THEN hardness$ = "(Super Hard)": GOTO ph:
IF speed >= 202 AND speed < 225 THEN hardness$ = "(YIKES!)    "
ph:
COLOR 12
LOCATE 3, 63: PRINT hardness$









CIRCLE (ballx, bally), 5, 14   ' paint new circle
PAINT (ballx, bally), 14       ' paint new circle

x = TIMER
x = x + .01


DO UNTIL x < TIMER
LOOP










GOTO actioninput:













moveplayer1:
IF movey > 0 THEN IF player1y = 370 THEN RETURN
IF movey < 0 THEN IF player1y = 70 THEN RETURN


'лллллллLINE (player1x, player1y)-(player1x + 10, player1y + 70), 12, BF ' left player
'лллллллLINE (player2x, player2y)-(player2x + 10, player2y + 70), 1, BF ' right player


LINE (player1x, player1y)-(player1x + 10, player1y + 70), 0, BF ' left player

player1y = player1y + movey
LINE (player1x, player1y)-(player1x + 10, player1y + 70), 12, BF ' left player
RETURN

















moveplayer2:
IF movey > 0 THEN IF player2y = 370 THEN RETURN
IF movey < 0 THEN IF player2y = 70 THEN RETURN


'лллллллLINE (player1x, player1y)-(player1x + 10, player1y + 70), 12, BF ' left player
'лллллллLINE (player2x, player2y)-(player2x + 10, player2y + 70), 1, BF ' right player


LINE (player2x, player2y)-(player2x + 10, player2y + 70), 0, BF ' left player

player2y = player2y + movey
LINE (player2x, player2y)-(player2x + 10, player2y + 70), 9, BF ' left player
RETURN

































bouncesound:
IF sound$ = ooff$ THEN RETURN
I = 75
FOR l = 1 TO 5
SOUND I, 15 / 100
I = I + 40
NEXT l
RETURN



















boardsound:

IF slopex > 0 AND slopex < 225 THEN slopex = slopex + .5
IF slopex < 0 AND slopex > -225 THEN slopex = slopex - .5

IF sound$ = ooff$ THEN RETURN
FOR l = 1 TO 5
SOUND g, 15 / 100
NEXT l
RETURN


















badsound:
IF sound$ = ooff$ THEN RETURN
I = 50
FOR l = 1 TO 5
SOUND I, 15 / 10
NEXT l
RETURN

















boardhit1:
LINE (player1x, player1y)-(player1x + 10, player1y + 70), 12, BF
slopex = -(slopex)
IF slopey = 0 THEN IF bally > player1y + 30 AND bally < player1y + 40 THEN GOSUB boardsound:   RETURN

IF slopey > 0 THEN addon = 2
IF slopey < 0 THEN addon = -2

IF slopey = 0 THEN IF bally > player1y AND bally < player1y + 30 THEN addon = -2: GOSUB boardsound: slopey = slopey + addon:  RETURN
IF slopey = 0 THEN IF bally > player1y + 50 AND bally < player1y + 70 THEN addon = 2: GOSUB boardsound: slopey = slopey + addon:  RETURN

GOSUB boardsound:




















slopechange:

IF bally > player1y + 30 AND bally < player1y + 40 THEN slopey = slopey - addon: RETURN
IF ABS(slopey) > 19 THEN RETURN
IF bally > player1y AND bally < player1y + 20 THEN slopey = slopey + addon: RETURN
IF bally > player1y + 50 AND bally < player1y + 70 THEN slopey = slopey + addon: RETURN

RETURN























boardhit2:
LINE (player2x, player2y)-(player2x + 10, player2y + 70), 9, BF
slopex = -(slopex)
IF slopey = 0 THEN IF bally > player2y + 30 AND bally < player2y + 40 THEN GOSUB boardsound:   RETURN

IF slopey > 0 THEN addon = 2
IF slopey < 0 THEN addon = -2

IF slopey = 0 THEN IF bally > player2y AND bally < player2y + 30 THEN addon = -2: GOSUB boardsound: slopey = slopey + addon:  RETURN
IF slopey = 0 THEN IF bally > player2y + 50 AND bally < player2y + 70 THEN addon = 2: GOSUB boardsound: slopey = slopey + addon:  RETURN



GOSUB boardsound:

















slopechange2:

IF bally > player2y + 30 AND bally < player2y + 40 THEN slopey = slopey - addon: RETURN
IF ABS(slopey) > 19 THEN RETURN
IF bally > player2y AND bally < player2y + 30 THEN slopey = slopey + addon: RETURN
IF bally > player2y + 40 AND bally < player2y + 70 THEN slopey = slopey + addon: RETURN

RETURN



















drawbox:
COLOR boxcol
LOCATE boxcory, boxcorx: PRINT "Щ": LOCATE boxcory, boxcorx + 1: PRINT STRING$(boxxlen, "Э"): LOCATE boxcory, boxcorx + boxxlen: PRINT "Л"
newboxylen = boxcory
FOR l = 1 TO boxylen
LOCATE newboxylen + 1, boxcorx: PRINT "К" + STRING$(boxxlen - 2, " ")
LOCATE newboxylen + 1, boxcorx + boxxlen: PRINT "К"
newboxylen = newboxylen + 1
NEXT l
LOCATE boxcory + boxylen, boxcorx: PRINT "Ш": LOCATE boxcory + boxylen, boxcorx + 1: PRINT STRING$(boxxlen, "Э"): LOCATE boxcory + boxylen, boxcorx + boxxlen: PRINT "М"
RETURN
























enc:

I = 1
x = 23
y = 1

IF deenc$ = oon$ THEN x = -23
encoutput$ = ""


DO
encchar$ = MID$(encinput$, I, 1)
I = I + 1
IF LEN(encoutput$) = LEN(encinput$) THEN RETURN
encnum = ASC(encchar$)
encnum = encnum + x

IF encnum > 255 THEN encnum = encnum - 255

encoutput$ = encoutput$ + CHR$(encnum)
encnum = 0
IF LEN(encoutput$) = LEN(encinput$) THEN RETURN
LOOP
RETURN




















plinput:
plinput$ = INKEY$
IF a = 1 THEN chg$ = TIME$: a = 0     'let chg$= time$, to initiate cursor animation
IF chg$ <> TIME$ THEN LET curnum = curnum + 1: LET chg$ = TIME$
IF curnum = 5 THEN curnum = 1
LOCATE initcory, initcorx: COLOR curcol: PRINT cur$
LOCATE initcory, initcorx + 1: PRINT " "
IF curnum < 1 OR curnum > 5 THEN curnum = 1
IF curnum = 1 THEN cur$ = "|"
IF curnum = 2 THEN cur$ = "/"
IF curnum = 3 THEN cur$ = "Ф"
IF curnum = 4 THEN cur$ = "\"
IF curnum = 5 THEN curnum = 1
IF inputcum > 0 THEN IF plinput$ = backup$ THEN initcorx = initcorx - 1: restof = LEN(retinput$): retinput$ = LEFT$(retinput$, restof - 1): curnum = curnum + 1: inputcum = inputcum - 1
IF plinput$ = execute$ THEN LOCATE initcory, initcorx: PRINT "  ":  RETURN
IF inputlen = inputcum THEN GOTO plinput:
IF wrap$ = oon$ THEN IF initcorx + 2 > initcorxend THEN initcory = initcory + 1: initcorx = initcorxstart:
IF plinput$ <> "" AND plinput$ <> backup$ THEN LOCATE initcory, initcorx: COLOR textcol: PRINT plinput$: retinput$ = retinput$ + plinput$: initcorx = initcorx + 1: inputcum = inputcum + 1: curnum = curnum + 1: IF curnum = 5 THEN curnum = 1
IF inputcum > 0 THEN IF passfunc$ = oon$ THEN LOCATE initcory, initcorx - 1: COLOR 7: PRINT "№"
GOTO plinput:

































quit:
boxcol = 14
BEEP
GOSUB mesbox:
LOCATE 15, 34: PRINT " End Game(Y/N)?"
initcorx = 41
initcory = 17
inputlen = 1
curcol = 12
DO

choice$ = INKEY$
IF choice$ = CHR$(27) THEN CLS : GOTO loadgraphics:
IF choice$ = "Y" OR choice$ = "y" THEN GOTO start:
IF choice$ = "N" OR choice$ = "n" THEN CLS : GOSUB loadgraphics

LOOP

GOTO quit:

















mesbox:
boxcol = 15
boxcorx = 31
boxcory = 13
boxxlen = 20
boxylen = 7
GOSUB drawbox:
RETURN



















winner:
GOSUB mesbox:
LOCATE 15, 35: PRINT player$; " WINS!"
COLOR 12
LOCATE 17, 34: PRINT " + "; (INT(ABS(slopex))); " Points"
PLAY "o" + STR$(3)
PLAY "L16ab>c"
SLEEP INKEY$ = ""
IF player$ = "Player 1" THEN player1points = player1points + INT(ABS(slopex))
IF player$ = "Player 2" THEN player2points = player2points + INT(ABS(slopex))

CLS
GOTO begin:



















pause:
GOSUB mesbox
SOUND 100, 10 / 1
LOCATE 16, 39: PRINT "Pause"
DO WHILE INKEY$ = ""
LOOP
key$ = INKEY$
CLS
GOTO loadgraphics

