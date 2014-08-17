
DECLARE SUB hit ()
DECLARE SUB ext ()
DECLARE SUB stik ()
DECLARE SUB CHOICES ()
DECLARE SUB intro ()
DECLARE SUB makedeck ()
DECLARE SUB first3 ()
DECLARE SUB backgrnd ()
DECLARE SUB ASSIGN ()

'       Game
'       Black Jack
'       Programming by Justin Rosenberg

RANDOMIZE TIMER

you = 0
deal = 0
push = 0

                           
intro
makedeck
start:
backgrnd
first3

x = 1
y = 300
a = 45
b = 330
control = 0

again:
SLOW = SLOW + 1: IF SLOW / 50 = INT(SLOW / 50) THEN C = RND * 15
LINE (x, y)-(a, b), C, B
key$ = INKEY$
IF key$ = CHR$(0) + "M" THEN x = x + 60: a = a + 60
IF key$ = CHR$(0) + "K" THEN x = x - 60: a = a - 60
IF x > 151 THEN x = x - 60: a = a - 60
IF x < 1 THEN x = x + 60: a = a + 60
IF key$ = CHR$(13) THEN GOTO sort
LINE (1, 300)-(45, 330), 4, B
LINE (61, 300)-(105, 330), 4, B
LINE (121, 300)-(165, 330), 4, B
IF control = 1 THEN GOTO endy

GOTO again
sort:
IF x = 1 THEN hit
IF x = 61 THEN stik
IF x = 121 THEN ext

GOTO again
endy:

GOTO start

SUB ASSIGN
SHARED CHOICE, card$, card
card = 0
IF CHOICE <= 36 THEN card = VAL(card$): GOTO finish
IF card$ = "J  " OR card$ = "Q  " OR card$ = "K  " THEN card = 10: GOTO finish
IF card$ = "  J" OR card$ = "  Q" OR card$ = "  K" THEN card = 10: GOTO finish
IF card$ = "J" OR card$ = "Q" OR card$ = "K" THEN card = 10: GOTO finish
card = 11
finish:
END SUB

SUB backgrnd
SHARED uX, dx

CLS
SCREEN 9
'       Drawing background graphics

CIRCLE (320, 10), 425, 6
PAINT (320, 1), 6
CIRCLE (320, 1), 425, 0
CIRCLE (320, 10), 425, 0
CIRCLE (320, 10), 425.8, 4
PAINT (360, 349), 4
LINE (120, 25)-(520, 250), 2, BF

' Drawing boxes where the cards will go
LINE (125, 30)-(195, 125), 15, BF
LINE (125, 150)-(195, 245), 15, BF

LINE (205, 30)-(275, 125), 1, BF
FOR y = 30 TO 125 STEP 5
LINE (205, y)-(275, y), 14
NEXT y
FOR x = 205 TO 275 STEP 5
LINE (x, 30)-(x, 125), 0
NEXT x

LINE (205, 150)-(275, 245), 15, BF

LINE (285, 30)-(355, 125), 1, B
LINE (285, 150)-(355, 245), 1, B

LINE (365, 30)-(435, 125), 1, B
LINE (365, 150)-(435, 245), 1, B

LINE (445, 30)-(515, 125), 1, B
LINE (445, 150)-(515, 245), 1, B

dx = 125
uX = 205


'       Labeling areas
COLOR 1
LOCATE 1, 1
PRINT "Spider Casino"
LOCATE 2, 1
PRINT "Dealer hits "
LOCATE 3, 1
PRINT "on 16 and less"

LOCATE 23, 2
PRINT "HIT"

LOCATE 23, 9
PRINT "STICK"

LOCATE 23, 17
PRINT "EXIT"

LOCATE 23, 63
PRINT "YOUR HAND ="

LOCATE 1, 62
PRINT "DEALER'S HAND="

END SUB

SUB ext
SHARED you, dealer, push, control, utot, dtot, card5

IF card5 = 1 THEN LOCATE 20, 20: PRINT "You got 5 cards without going over.  You win": card5 = 0
LOCATE 21, 27
PRINT "Press the enter to continue"
enter:
IF INKEY$ <> CHR$(13) THEN GOTO enter
LOCATE 10, 1
PRINT "Wins"; you
PRINT "Loses"; dealer
PRINT "Ties"; push
PRINT "Winning"
PRINT "Percentage ";
IF you = 0 THEN PRINT 0 ELSE PRINT INT(you / (you + dealer + push) * 100);

LOCATE 20, 2
PRINT "Play"

LOCATE 21, 2
PRINT "Again?"

LOCATE 23, 2
PRINT "YES"

LOCATE 23, 9
PRINT "NO   "

LOCATE 23, 17
PRINT "    "

x = 1
y = 300
a = 45
b = 330
SLOW = 0

REM Covers up white space around text
LINE (121, 300)-(165, 330), 4, BF
LINE (80, 290)-(104, 325), 4, BF

trap:
SLOW = SLOW + 1: IF SLOW / 50 = INT(SLOW / 50) THEN C = RND * 15
LINE (x, y)-(a, b), C, B
key$ = INKEY$
IF key$ = CHR$(0) + "M" THEN x = x + 60: a = a + 60
IF key$ = CHR$(0) + "K" THEN x = x - 60: a = a - 60
IF x > 120 THEN x = x - 60: a = a - 60
IF x < 1 THEN x = x + 60: a = a + 60
IF key$ = CHR$(13) THEN GOTO endtrap
LINE (1, 300)-(45, 330), 4, B
LINE (61, 300)-(105, 330), 4, B
GOTO trap
endtrap:

IF x = 1 THEN control = 1: GOTO ENDIT
IF x = 61 THEN GOTO Gameover:

ENDIT:
utot = 0
dtot = 0
GOTO endend:

Gameover:
SCREEN 0, 0, 0
x = 1
y = 1

a = 1
b = 23

times = 1
WHILE INKEY$ = "" AND times < 333
times = times + 1
LOCATE y, x
PRINT "         "
IF x = 71 THEN controlx$ = "left"
IF x = 1 THEN controlx$ = "right"
IF y = 1 THEN controly$ = "down"
IF y = 23 THEN controly$ = "up"

IF controlx$ = "left" THEN x = x - 2
IF controlx$ = "right" THEN x = x + 2
IF controly$ = "down" THEN y = y + 1
IF controly$ = "up" THEN y = y - 1

LOCATE b, a
PRINT "                   "
IF a = 61 THEN controla$ = "left"
IF a = 1 THEN controla$ = "right"
IF b = 1 THEN controlb$ = "down"
IF b = 23 THEN controlb$ = "up"

IF controla$ = "left" THEN a = a - 1
IF controla$ = "right" THEN a = a + 1
IF controlb$ = "down" THEN b = b + 1
IF controlb$ = "up" THEN b = b - 1

IF (y = b AND x > a AND x < a + 19) OR (b = y AND a > x AND a < x + 9) THEN GOTO collision

RETURN.back:

LOCATE y, x
COLOR RND * 10
PRINT "Game Over"

LOCATE b, a
COLOR RND * 10
PRINT "Thanks for playing!"

FOR stall = 1 TO 1900
NEXT stall
WEND
SYSTEM

collision:
IF controly$ = "down" THEN controly$ = "up" ELSE controly$ = "down"
IF controlb$ = "down" THEN controlb$ = "up" ELSE controlb$ = "down"
GOTO RETURN.back

endend:
END SUB

SUB first3
SHARED card, CHOICE, card$, utot, dtot, uX, ul1, ul2, dl1, dl2, dx, you

'       Dealing fist 3 cards

over:
CLOSE

OPEN "Deck.dat" FOR RANDOM AS #1 LEN = 4
FIELD #1, 3 AS card$, 1 AS SUIT$

Q = 1
WHILE Q <> 0
TRYAGAIN:
CHOICE = INT(RND * 52) + 1
IF CHOICE < 1 OR CHOICE > 52 THEN GOTO TRYAGAIN
GET #1, CHOICE
IF VAL(card$) <> 1 THEN Q = 0: ASSIGN: GOTO oto1

qq = 0
FOR x = 1 TO 52
GET #1, x
IF VAL(card$) <> 1 THEN qq = 1
NEXT x
IF qq = 0 THEN LOCATE 20, 30: PRINT "Deck reshuffled.": makedeck: GOTO over
oto1:
WEND
CHOICE1 = CHOICE
CARD1$ = card$
CARD1 = card
SUIT1$ = SUIT$
LSET card$ = STR$(1)
PUT #1, CHOICE

Q = 1
WHILE Q <> 0
TRYAGAIN1:
CHOICE = INT(RND * 52) + 1
IF CHOICE < 1 OR CHOICE > 52 THEN GOTO TRYAGAIN1
GET #1, CHOICE
IF VAL(card$) <> 1 THEN Q = 0: ASSIGN: GOTO oto2:

qq = 0
FOR x = 1 TO 52
GET #1, x
IF VAL(card$) <> 1 THEN qq = 1
NEXT x
IF qq = 0 THEN LOCATE 20, 30: PRINT "Deck reshuffled.": makedeck: GOTO over

oto2:
WEND
card2$ = card$
card2 = card
SUIT2$ = SUIT$
LSET card$ = STR$(1)
PUT #1, CHOICE

Q = 1
WHILE Q <> 0
TRYAGAIN2:
CHOICE = INT(RND * 52) + 1
IF CHOICE < 1 OR CHOICE > 52 THEN GOTO TRYAGAIN2
GET #1, CHOICE
IF VAL(card$) <> 1 THEN Q = 0: ASSIGN: GOTO oto3

qq = 0
FOR x = 1 TO 52
GET #1, x
IF VAL(card$) <> 1 THEN qq = 1
NEXT x
IF qq = 0 THEN LOCATE 20, 30: PRINT "Deck reshuffled.": makedeck: GOTO over

oto3:
WEND
card3$ = card$
card3 = card
SUIT3$ = SUIT$
LSET card$ = STR$(1)
PUT #1, CHOICE
CLOSE

'  Displaying cards

LOCATE 4, 21
IF SUIT1$ = "S" OR SUIT1$ = "C" THEN COLOR 8, 63
IF SUIT1$ = "D" OR SUIT1$ = "H" THEN COLOR 4, 63
IF SUIT1$ = "S" THEN chr = 6
IF SUIT1$ = "H" THEN chr = 3
IF SUIT1$ = "D" THEN chr = 4
IF SUIT1$ = "C" THEN chr = 5
PRINT CARD1$; CHR$(chr)

dl1 = 21
dl2 = 17

LOCATE 9, 17
IF SUIT1$ = "S" OR SUIT1$ = "C" THEN COLOR 8, 63
IF SUIT1$ = "D" OR SUIT1$ = "H" THEN COLOR 4, 63
IF SUIT1$ = "S" THEN chr = 6
IF SUIT1$ = "H" THEN chr = 3
IF SUIT1$ = "D" THEN chr = 4
IF SUIT1$ = "C" THEN chr = 5
PRINT CARD1$; CHR$(chr)

LOCATE 17, 17
IF SUIT2$ = "S" OR SUIT2$ = "C" THEN COLOR 8, 63
IF SUIT2$ = "D" OR SUIT2$ = "H" THEN COLOR 4, 63
IF SUIT2$ = "S" THEN chr = 6
IF SUIT2$ = "H" THEN chr = 3
IF SUIT2$ = "D" THEN chr = 4
IF SUIT2$ = "C" THEN chr = 5
PRINT card2$; CHR$(chr)

LOCATE 12, 21
IF SUIT2$ = "S" OR SUIT2$ = "C" THEN COLOR 8, 63
IF SUIT2$ = "D" OR SUIT2$ = "H" THEN COLOR 4, 63
IF SUIT2$ = "S" THEN chr = 6
IF SUIT2$ = "H" THEN chr = 3
IF SUIT2$ = "D" THEN chr = 4
IF SUIT2$ = "C" THEN chr = 5
PRINT card2$; CHR$(chr)

LOCATE 12, 31
IF SUIT3$ = "S" OR SUIT3$ = "C" THEN COLOR 8, 63
IF SUIT3$ = "D" OR SUIT3$ = "H" THEN COLOR 4, 63
IF SUIT3$ = "S" THEN chr = 6
IF SUIT3$ = "H" THEN chr = 3
IF SUIT3$ = "D" THEN chr = 4
IF SUIT3$ = "C" THEN chr = 5
PRINT card3$; CHR$(chr)

LOCATE 17, 27
IF SUIT3$ = "S" OR SUIT3$ = "C" THEN COLOR 8, 63
IF SUIT3$ = "D" OR SUIT3$ = "H" THEN COLOR 4, 63
IF SUIT3$ = "S" THEN chr = 6
IF SUIT3$ = "H" THEN chr = 3
IF SUIT3$ = "D" THEN chr = 4
IF SUIT3$ = "C" THEN chr = 5
PRINT card3$; CHR$(chr)

ul1 = 31
ul2 = 27

' adding cards

dtot = CARD1
utot = card2 + card3

COLOR 1
LOCATE 1, 76
PRINT dtot

LOCATE 23, 74
PRINT utot

IF utot = 21 THEN LOCATE 20, 30: PRINT "Black Jack.  You win": you = you + 1: ext: backgrnd: GOTO over
IF utot = 22 THEN utot = 12

END SUB

SUB hit
SHARED utot, dtot, CHOICE, card, card$, uX, dx, ul1, ul2, dl1, dl2, you, dealer, push, control2, card5

over2:
CLOSE
OPEN "Deck.dat" FOR RANDOM AS #1 LEN = 4
FIELD #1, 3 AS card$, 1 AS SUIT$

Q = 1
WHILE Q <> 0
TRYAGAINhit:
CHOICE = INT(RND * 100) + 1
IF CHOICE < 1 OR CHOICE > 52 THEN GOTO TRYAGAINhit
GET #1, CHOICE
IF VAL(card$) <> 1 THEN Q = 0: ASSIGN: GOTO oto4


qq = 0
FOR x = 1 TO 52
GET #1, x
IF VAL(card$) <> 1 THEN qq = 1
NEXT x
IF qq = 0 THEN LOCATE 20, 30: PRINT "Deck reshuffled.": makedeck: GOTO over2

oto4:
WEND
cardd$ = card$
LSET card$ = STR$(1)
PUT #1, CHOICE

COLOR 1
utot = utot + card
LOCATE 23, 74
PRINT utot

uX = uX + 80
LINE (uX, 150)-(uX + 70, 245), 15, BF
ul1 = ul1 + 10
ul2 = ul2 + 10

LOCATE 12, ul1
IF SUIT$ = "S" OR SUIT$ = "C" THEN COLOR 8, 63
IF SUIT$ = "D" OR SUIT$ = "H" THEN COLOR 4, 63
IF SUIT$ = "S" THEN chr = 6
IF SUIT$ = "H" THEN chr = 3
IF SUIT$ = "D" THEN chr = 4
IF SUIT$ = "C" THEN chr = 5
PRINT cardd$; CHR$(chr)

LOCATE 17, ul2
IF SUIT$ = "S" OR SUIT$ = "C" THEN COLOR 8, 63
IF SUIT$ = "D" OR SUIT$ = "H" THEN COLOR 4, 63
IF SUIT$ = "S" THEN chr = 6
IF SUIT$ = "H" THEN chr = 3
IF SUIT$ = "D" THEN chr = 4
IF SUIT$ = "C" THEN chr = 5
PRINT cardd$; CHR$(chr)

IF utot > 21 THEN LOCATE 20, 30: PRINT "You Busted.  Dealer wins":  dealer = dealer + 1: ext
IF utot = 21 THEN LOCATE 20, 30: PRINT "You win": you = you + 1: ext
IF uX = 445 AND utot < 21 THEN card5 = 1: you = you + 1: backgrnd: ext


CLOSE
END SUB

SUB intro

' Intro to Black Jack Game
' By: Justin Rosenberg
' This is the intro to Black Jack


CLS
COLOR 2

'       CIRCLES

SCREEN 12
RANDOMIZE TIMER

SCREEN 9

FOR Q = 1 TO 5
FOR E = 0 TO 1 STEP .01
C = RND * 15
x = RND * 600
y = RND * 400
FOR R = 10 TO 100 STEP 10
CIRCLE (x, y), R, C, , , E
IF INKEY$ <> "" THEN GOTO skippy
NEXT R
NEXT E
C = INT(RND * 15)
IF C = 0 THEN C = 1
COLOR C
LOCATE 13, 35
PRINT "BLACK JACK"
LOCATE 23, 65
PRINT "SPIDER SOFTWARE"
FOR PAUSE = 1 TO 5000
NEXT PAUSE
NEXT Q
skippy:
COLOR 2

CLS
SCREEN 9
COLOR 3
PRINT "SPIDER SOFTWARE PRESENTS BLACK JACK."
PRINT "PROGRAMMED BY JUSTIN ROSENBERG."
PRINT "ANY SUGESTIONS SHOULD BE SENT TO JPROSE@UIT.NET"
PRINT "INSTRUCTIONS"
PRINT "THE GAME IS PLAYED LIKE REGULAR BLACK JACK EXCEPT FOR A FEW EXCEPTIONS."
PRINT "-----------------------------------------------------------------------"
PRINT "1.> ACES ARE 11 UNLESS YOU ARE DEALT 2 ACES."
PRINT "2.> DEALER HITS ON 16 OR LESS."
PRINT "3.> USE LEFT AND RIGHT ARROW KEYS TO SELECT ACTION."
PRINT "4.> ACTIONS ARE AS FOLLOWS HIT,STICK,AND EXIT."
PRINT "5.> ONLY ONE DECK IS USED."
PRINT "6.> YOU WILL NOT BE TOLD WHEN THAT DECK IS SHUFFLED."
PRINT "7.> THE DECK WILL NOT BE SHUFFLED TILL IT IS OUT OF CARDS."
PRINT "8.> HAVE FUN."
PRINT "Press the enter to continue."
enter1:
IF INKEY$ <> CHR$(13) THEN GOTO enter1

END SUB

SUB makedeck
'       Creating Deck
CLOSE
OPEN "Deck.dat" FOR RANDOM AS #1 LEN = 4
FIELD #1, 3 AS card$, 1 AS SUIT$

location = 0
FOR x = 2 TO 10
location = location + 1
crd$ = STR$(x)
LSET card$ = crd$
LSET SUIT$ = "C"
PUT #1, location
NEXT x

FOR x = 2 TO 10
location = location + 1
crd$ = STR$(x)
LSET card$ = crd$
LSET SUIT$ = "D"
PUT #1, location
NEXT x

FOR x = 2 TO 10
location = location + 1
crd$ = STR$(x)
LSET card$ = crd$
LSET SUIT$ = "H"
PUT #1, location
NEXT x

FOR x = 2 TO 10
location = location + 1
crd$ = STR$(x)
LSET card$ = crd$
LSET SUIT$ = "S"
PUT #1, location
NEXT x

FOR x = 1 TO 4
crd$ = "J"
LSET card$ = crd$
IF x = 1 THEN LSET SUIT$ = "C"
IF x = 2 THEN LSET SUIT$ = "D"
IF x = 3 THEN LSET SUIT$ = "H"
IF x = 4 THEN LSET SUIT$ = "S"
location = location + 1
PUT #1, location
NEXT x

FOR x = 1 TO 4
crd$ = "Q"
IF x = 1 THEN LSET SUIT$ = "C"
IF x = 2 THEN LSET SUIT$ = "D"
IF x = 3 THEN LSET SUIT$ = "H"
IF x = 4 THEN LSET SUIT$ = "S"
LSET card$ = crd$
location = location + 1
PUT #1, location
NEXT x

FOR x = 1 TO 4
crd$ = "K"
LSET card$ = crd$
IF x = 1 THEN LSET SUIT$ = "C"
IF x = 2 THEN LSET SUIT$ = "D"
IF x = 3 THEN LSET SUIT$ = "H"
IF x = 4 THEN LSET SUIT$ = "S"
location = location + 1
PUT #1, location
NEXT x

FOR x = 1 TO 4
crd$ = "A"
LSET card$ = crd$
IF x = 1 THEN LSET SUIT$ = "C"
IF x = 2 THEN LSET SUIT$ = "D"
IF x = 3 THEN LSET SUIT$ = "H"
IF x = 4 THEN LSET SUIT$ = "S:"
location = location + 1

PUT #1, location
NEXT x
CLOSE

END SUB

SUB stik

SHARED dtot, utot, dl1, dl2, dx, card, card$, you, push, dealer, control, CHOICE, card5

'       Dealer recives his cards

WHILE dtot < 17

over1:
CLOSE
OPEN "Deck.dat" FOR RANDOM AS #1 LEN = 4
FIELD #1, 3 AS card$, 1 AS SUIT$

Q = 1
WHILE Q <> 0
TRYAGAINstik:
CHOICE = INT(RND * 52) + 1
IF CHOICE < 1 OR CHOICE > 52 THEN GOTO TRYAGAINstik
GET #1, CHOICE
IF VAL(card$) <> 1 THEN Q = 0: ASSIGN: GOTO oto5

qq = 0
FOR x = 1 TO 52
GET #1, x
IF VAL(card$) <> 1 THEN qq = 1
NEXT x
IF qq = 0 THEN LOCATE 20, 30: PRINT "Deck reshuffled.": makedeck: GOTO over1

oto5:
WEND
cardd$ = card$
LSET card$ = STR$(1)
PUT #1, CHOICE

dtot = dtot + card

COLOR 1
LOCATE 1, 75
PRINT dtot

dx = dx + 80
LINE (dx, 30)-(dx + 70, 125), 15, BF
dl1 = dl1 + 10
dl2 = dl2 + 10

LOCATE 4, dl1
IF SUIT$ = "S" OR SUIT$ = "C" THEN COLOR 8, 63
IF SUIT$ = "D" OR SUIT$ = "H" THEN COLOR 4, 63
IF SUIT$ = "S" THEN chr = 6
IF SUIT$ = "H" THEN chr = 3
IF SUIT$ = "D" THEN chr = 4
IF SUIT$ = "C" THEN chr = 5
PRINT cardd$; CHR$(chr)

LOCATE 9, dl2
IF SUIT$ = "S" OR SUIT$ = "C" THEN COLOR 8, 63
IF SUIT$ = "D" OR SUIT$ = "H" THEN COLOR 4, 63
IF SUIT$ = "S" THEN chr = 6
IF SUIT$ = "H" THEN chr = 3
IF SUIT$ = "D" THEN chr = 4
IF SUIT$ = "C" THEN chr = 5
PRINT cardd$; CHR$(chr)

IF dtot = 21 THEN LOCATE 20, 30: PRINT "Dealer wins.":  dealer = dealer + 1: ext
IF dtot > 21 THEN LOCATE 20, 30: PRINT "Dealer busts. You win":  you = you + 1: ext
IF dx = 445 AND dtot < 21 THEN card5 = 1:  dealer = dealer + 1: ext
IF control = 1 THEN GOTO indY

WEND

IF utot = dtot THEN LOCATE 20, 30: PRINT "Push.": push = push + 1: GOTO skip
IF utot > dtot THEN LOCATE 20, 30: PRINT "You Win.": you = you + 1 ELSE LOCATE 20, 30: PRINT "Dealer Wins.": dealer = dealer + 1
skip:
ext

indY:
END SUB

