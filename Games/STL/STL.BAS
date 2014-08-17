DECLARE SUB HARD ()
DECLARE SUB win ()
DECLARE SUB TITLE ()
DECLARE SUB MMENU ()
DECLARE SUB CHOOSE ()
DECLARE SUB HELP ()
DECLARE SUB XIT ()
DECLARE SUB easy ()


REM See the Light
REM Another puzzle game by Pete Collins

COLOR 14, 0
TITLE

SUB CHOOSE
x = 6
s:
IF x < 6 THEN x = 7
IF x > 7 THEN x = 6
CLS
LOCATE 4, 1
COLOR 10: PRINT "Please select the difficulty level you would like to play at....": COLOR 15
PRINT
PRINT "1. Easy (3 x 3 Grid) - Baby Stuff"
PRINT "2. Hard (5 x 5 Grid) - The stuff suicide notes are made of.."
IF x = 6 THEN LOCATE 6, 1: COLOR 1, 15: PRINT "1. Easy (3 x 3 Grid) - Baby Stuff": COLOR 15, 1
IF x = 7 THEN LOCATE 7, 1: COLOR 1, 15: PRINT "2. Hard (5 x 5 Grid) - The stuff suicide notes are made of..": COLOR 15, 1

ch:
test$ = INKEY$
IF test$ = "H" OR test$ = "P" THEN GOTO ch
IF RIGHT$(test$, 1) = "H" THEN x = x - 1: GOTO s
IF RIGHT$(test$, 1) = "P" THEN x = x + 1: GOTO s
IF test$ = CHR$(13) THEN GOTO sel2
GOTO ch

sel2:
SELECT CASE x
CASE 6
easy
CASE 7
HARD
END SELECT
END SUB

SUB easy
DIM a(9)
init = 0
x = 1
CLS
LOCATE 1, 1
COLOR 14
PRINT TAB(33); "SEE THE LIGHT"
LOCATE 22, 32
PRINT "PRESS Q TO QUIT": COLOR 15
REM LET'S START EVERYTHING OFF
start1:
IF x > 9 THEN x = oldx
IF x < 1 THEN x = oldx
LOCATE 6, 1
PRINT TAB(27); "ÉÍÍÍÍÍÍÍËÍÍÍÍÍÍÍËÍÍÍÍÍÍÍ»"
PRINT TAB(27); "º       º       º       º"
PRINT TAB(27); "º       º       º       º"
PRINT TAB(27); "º       º       º       º"
PRINT TAB(27); "ÌÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍ¹"
PRINT TAB(27); "º       º       º       º"
PRINT TAB(27); "º       º       º       º"
PRINT TAB(27); "º       º       º       º"
PRINT TAB(27); "ÌÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍ¹"
PRINT TAB(27); "º       º       º       º"
PRINT TAB(27); "º       º       º       º"
PRINT TAB(27); "º       º       º       º"
PRINT TAB(27); "ÈÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍ¼"
REM Now randomly pick lights on or off
IF init = 0 THEN
RANDOMIZE TIMER
FOR i = 1 TO 9
a(i) = INT(RND * 2)
NEXT i
IF a(1) = 0 AND a(2) = 0 AND a(3) = 0 AND a(4) = 0 AND a(5) = 0 AND a(6) = 0 AND a(7) = 0 AND a(8) = 0 AND a(9) = 0 THEN GOTO start1
init = 1
END IF

GOSUB putlights


IF x = 1 THEN
COLOR 2
LOCATE 7, 29: PRINT "ÚÄÄÄ¿"
LOCATE 8, 29: PRINT "³": LOCATE 8, 33: PRINT "³"
LOCATE 9, 29: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 2 THEN
COLOR 2
LOCATE 7, 37: PRINT "ÚÄÄÄ¿"
LOCATE 8, 37: PRINT "³": LOCATE 8, 41: PRINT "³"
LOCATE 9, 37: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 3 THEN
COLOR 2
LOCATE 7, 45: PRINT "ÚÄÄÄ¿"
LOCATE 8, 45: PRINT "³": LOCATE 8, 49: PRINT "³"
LOCATE 9, 45: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 4 THEN
COLOR 2
LOCATE 11, 29: PRINT "ÚÄÄÄ¿"
LOCATE 12, 29: PRINT "³": LOCATE 12, 33: PRINT "³"
LOCATE 13, 29: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 5 THEN
COLOR 2
LOCATE 11, 37: PRINT "ÚÄÄÄ¿"
LOCATE 12, 37: PRINT "³": LOCATE 12, 41: PRINT "³"
LOCATE 13, 37: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 6 THEN
COLOR 2
LOCATE 11, 45: PRINT "ÚÄÄÄ¿"
LOCATE 12, 45: PRINT "³": LOCATE 12, 49: PRINT "³"
LOCATE 13, 45: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 7 THEN
COLOR 2
LOCATE 15, 29: PRINT "ÚÄÄÄ¿"
LOCATE 16, 29: PRINT "³": LOCATE 16, 33: PRINT "³"
LOCATE 17, 29: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 8 THEN
COLOR 2
LOCATE 15, 37: PRINT "ÚÄÄÄ¿"
LOCATE 16, 37: PRINT "³": LOCATE 16, 41: PRINT "³"
LOCATE 17, 37: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 9 THEN
COLOR 2
COLOR 2
LOCATE 15, 45: PRINT "ÚÄÄÄ¿"
LOCATE 16, 45: PRINT "³": LOCATE 16, 49: PRINT "³"
LOCATE 17, 45: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

choice1:
oldx = x
test$ = INKEY$
IF test$ = "H" OR test$ = "P" OR test$ = "M" OR test$ = "K" THEN GOTO choice1
IF RIGHT$(test$, 1) = "H" THEN x = x - 3: GOTO start1
IF RIGHT$(test$, 1) = "P" THEN x = x + 3: GOTO start1
IF RIGHT$(test$, 1) = "K" THEN x = x - 1: GOTO start1
IF RIGHT$(test$, 1) = "M" THEN x = x + 1: GOTO start1
IF test$ = " " THEN GOTO modify
IF test$ = "Q" OR test$ = "q" THEN GOSUB ask
GOTO choice1

modify:
SOUND 4000, 2
IF x = 1 THEN
IF a(1) = 1 THEN a(1) = 0 ELSE a(1) = 1
IF a(2) = 1 THEN a(2) = 0 ELSE a(2) = 1
IF a(4) = 1 THEN a(4) = 0 ELSE a(4) = 1
END IF

IF x = 2 THEN
IF a(2) = 1 THEN a(2) = 0 ELSE a(2) = 1
IF a(1) = 1 THEN a(1) = 0 ELSE a(1) = 1
IF a(3) = 1 THEN a(3) = 0 ELSE a(3) = 1
IF a(5) = 1 THEN a(5) = 0 ELSE a(5) = 1
END IF

IF x = 3 THEN
IF a(3) = 1 THEN a(3) = 0 ELSE a(3) = 1
IF a(2) = 1 THEN a(2) = 0 ELSE a(2) = 1
IF a(6) = 1 THEN a(6) = 0 ELSE a(6) = 1
END IF

IF x = 4 THEN
IF a(4) = 1 THEN a(4) = 0 ELSE a(4) = 1
IF a(1) = 1 THEN a(1) = 0 ELSE a(1) = 1
IF a(5) = 1 THEN a(5) = 0 ELSE a(5) = 1
IF a(7) = 1 THEN a(7) = 0 ELSE a(7) = 1
END IF

IF x = 5 THEN
IF a(5) = 1 THEN a(5) = 0 ELSE a(5) = 1
IF a(2) = 1 THEN a(2) = 0 ELSE a(2) = 1
IF a(4) = 1 THEN a(4) = 0 ELSE a(4) = 1
IF a(6) = 1 THEN a(6) = 0 ELSE a(6) = 1
IF a(8) = 1 THEN a(8) = 0 ELSE a(8) = 1
END IF

IF x = 6 THEN
IF a(6) = 1 THEN a(6) = 0 ELSE a(6) = 1
IF a(3) = 1 THEN a(3) = 0 ELSE a(3) = 1
IF a(5) = 1 THEN a(5) = 0 ELSE a(5) = 1
IF a(9) = 1 THEN a(9) = 0 ELSE a(9) = 1
END IF

IF x = 7 THEN
IF a(7) = 1 THEN a(7) = 0 ELSE a(7) = 1
IF a(4) = 1 THEN a(4) = 0 ELSE a(4) = 1
IF a(8) = 1 THEN a(8) = 0 ELSE a(8) = 1
END IF

IF x = 8 THEN
IF a(8) = 1 THEN a(8) = 0 ELSE a(8) = 1
IF a(5) = 1 THEN a(5) = 0 ELSE a(5) = 1
IF a(7) = 1 THEN a(7) = 0 ELSE a(7) = 1
IF a(9) = 1 THEN a(9) = 0 ELSE a(9) = 1
END IF

IF x = 9 THEN
IF a(9) = 1 THEN a(9) = 0 ELSE a(9) = 1
IF a(6) = 1 THEN a(6) = 0 ELSE a(6) = 1
IF a(8) = 1 THEN a(8) = 0 ELSE a(8) = 1
END IF

GOTO start1


putlights:
IF a(1) = 1 THEN LOCATE 8, 31: COLOR 4: PRINT "Û": COLOR 15
IF a(2) = 1 THEN LOCATE 8, 39: COLOR 4: PRINT "Û": COLOR 15
IF a(3) = 1 THEN LOCATE 8, 47: COLOR 4: PRINT "Û": COLOR 15
IF a(4) = 1 THEN LOCATE 12, 31: COLOR 4: PRINT "Û": COLOR 15
IF a(5) = 1 THEN LOCATE 12, 39: COLOR 4: PRINT "Û": COLOR 15
IF a(6) = 1 THEN LOCATE 12, 47: COLOR 4: PRINT "Û": COLOR 15
IF a(7) = 1 THEN LOCATE 16, 31: COLOR 4: PRINT "Û": COLOR 15
IF a(8) = 1 THEN LOCATE 16, 39: COLOR 4: PRINT "Û": COLOR 15
IF a(9) = 1 THEN LOCATE 16, 47: COLOR 4: PRINT "Û": COLOR 15
REM Check to see if they're a winner.
IF a(1) = 0 AND a(2) = 0 AND a(3) = 0 AND a(4) = 0 AND a(5) = 0 AND a(6) = 0 AND a(7) = 0 AND a(8) = 0 AND a(9) = 0 THEN win
RETURN

ask:
COLOR 30
LOCATE 20, 12
PRINT "Are you sure you want to quit and abandon this game (Y/N)? ": COLOR 15
choice3:
test$ = INKEY$
IF test$ = "Y" OR test$ = "y" THEN MMENU
IF test$ = "n" OR test$ = "N" THEN
LOCATE 20, 12
PRINT "                                                                "
GOTO choice1
END IF
GOTO choice3
END SUB

SUB HARD
REM HERE WE GO
DIM a(25)
init = 0
x = 1
CLS
a:
LOCATE 1, 1
COLOR 14
PRINT TAB(24); "SEE THE LIGHT - Press Q to Quit": COLOR 15
REM LET'S START EVERYTHING OFF
start2:
IF x > 25 THEN x = oldx
IF x < 1 THEN x = oldx
LOCATE 3, 1
PRINT TAB(20); "ÉÍÍÍÍÍÍÍËÍÍÍÍÍÍÍËÍÍÍÍÍÍÍËÍÍÍÍÍÍÍËÍÍÍÍÍÍÍ»"
PRINT TAB(20); "º       º       º       º       º       º     "
PRINT TAB(20); "º       º       º       º       º       º   "
PRINT TAB(20); "º       º       º       º       º       º  "
PRINT TAB(20); "ÌÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍ¹   "
PRINT TAB(20); "º       º       º       º       º       º     "
PRINT TAB(20); "º       º       º       º       º       º     "
PRINT TAB(20); "º       º       º       º       º       º      "
PRINT TAB(20); "ÌÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍ¹       "
PRINT TAB(20); "º       º       º       º       º       º      "
PRINT TAB(20); "º       º       º       º       º       º         "
PRINT TAB(20); "º       º       º       º       º       º        "
PRINT TAB(20); "ÌÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍ¹           "
PRINT TAB(20); "º       º       º       º       º       º    "
PRINT TAB(20); "º       º       º       º       º       º     "
PRINT TAB(20); "º       º       º       º       º       º     "
PRINT TAB(20); "ÌÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍÎÍÍÍÍÍÍÍ¹      "
PRINT TAB(20); "º       º       º       º       º       º       "
PRINT TAB(20); "º       º       º       º       º       º    "
PRINT TAB(20); "º       º       º       º       º       º     "
PRINT TAB(20); "ÈÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍ¼                   "



REM *** NOW RANDOMLY PICK LIGHTS ON OR LIGHTS OFF ***
IF init = 0 THEN
FOR i = 1 TO 25
RANDOMIZE TIMER
a(i) = INT(RND * 2)
NEXT i
IF a(1) = 0 AND a(2) = 0 AND a(3) = 0 AND a(4) = 0 AND a(5) = 0 AND a(6) = 0 AND a(7) = 0 AND a(8) = 0 AND a(9) = 0 AND a(10) = 0 THEN
IF (11) = 0 AND a(12) = 0 AND a(13) = 0 AND a(14) = 0 AND a(15) = 0 AND a(16) = 0 AND a(17) = 0 AND a(18) = 0 AND a(19) = 0 THEN
IF a(20) = 0 AND a(21) = 0 AND a(22) = 0 AND a(23) = 0 AND a(24) = 0 AND a(25) = 0 THEN GOTO start2
END IF
END IF

init = 1
END IF

GOSUB putlightsh

IF x = 1 THEN
COLOR 2
LOCATE 4, 22: PRINT "ÚÄÄÄ¿"
LOCATE 5, 22: PRINT "³": LOCATE 5, 26: PRINT "³"
LOCATE 6, 22: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 2 THEN
COLOR 2
LOCATE 4, 30: PRINT "ÚÄÄÄ¿"
LOCATE 5, 30: PRINT "³": LOCATE 5, 34: PRINT "³"
LOCATE 6, 30: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 3 THEN
COLOR 2
LOCATE 4, 38: PRINT "ÚÄÄÄ¿"
LOCATE 5, 38: PRINT "³": LOCATE 5, 42: PRINT "³"
LOCATE 6, 38: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 4 THEN
COLOR 2
LOCATE 4, 46: PRINT "ÚÄÄÄ¿"
LOCATE 5, 46: PRINT "³": LOCATE 5, 50: PRINT "³"
LOCATE 6, 46: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 5 THEN
COLOR 2
LOCATE 4, 54: PRINT "ÚÄÄÄ¿"
LOCATE 5, 54: PRINT "³": LOCATE 5, 58: PRINT "³"
LOCATE 6, 54: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 6 THEN
COLOR 2
LOCATE 8, 22: PRINT "ÚÄÄÄ¿"
LOCATE 9, 22: PRINT "³": LOCATE 9, 26: PRINT "³"
LOCATE 10, 22: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 7 THEN
COLOR 2
LOCATE 8, 30: PRINT "ÚÄÄÄ¿"
LOCATE 9, 30: PRINT "³": LOCATE 9, 34: PRINT "³"
LOCATE 10, 30: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 8 THEN
COLOR 2
LOCATE 8, 38: PRINT "ÚÄÄÄ¿"
LOCATE 9, 38: PRINT "³": LOCATE 9, 42: PRINT "³"
LOCATE 10, 38: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 9 THEN
COLOR 2
LOCATE 8, 46: PRINT "ÚÄÄÄ¿"
LOCATE 9, 46: PRINT "³": LOCATE 9, 50: PRINT "³"
LOCATE 10, 46: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 10 THEN
COLOR 2
LOCATE 8, 54: PRINT "ÚÄÄÄ¿"
LOCATE 9, 54: PRINT "³": LOCATE 9, 58: PRINT "³"
LOCATE 10, 54: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 11 THEN
COLOR 2
LOCATE 12, 22: PRINT "ÚÄÄÄ¿"
LOCATE 13, 22: PRINT "³": LOCATE 13, 26: PRINT "³"
LOCATE 14, 22: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

IF x = 12 THEN
COLOR 2
LOCATE 12, 30: PRINT "ÚÄÄÄ¿"
LOCATE 13, 30: PRINT "³": LOCATE 13, 34: PRINT "³"
LOCATE 14, 30: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF
IF x = 13 THEN
COLOR 2
LOCATE 12, 38: PRINT "ÚÄÄÄ¿"
LOCATE 13, 38: PRINT "³": LOCATE 13, 42: PRINT "³"
LOCATE 14, 38: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF
IF x = 14 THEN
COLOR 2
LOCATE 12, 46: PRINT "ÚÄÄÄ¿"
LOCATE 13, 46: PRINT "³": LOCATE 13, 50: PRINT "³"
LOCATE 14, 46: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF
IF x = 15 THEN
COLOR 2
LOCATE 12, 54: PRINT "ÚÄÄÄ¿"
LOCATE 13, 54: PRINT "³": LOCATE 13, 58: PRINT "³"
LOCATE 14, 54: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF
IF x = 16 THEN
COLOR 2
LOCATE 16, 22: PRINT "ÚÄÄÄ¿"
LOCATE 17, 22: PRINT "³": LOCATE 17, 26: PRINT "³"
LOCATE 18, 22: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF
IF x = 17 THEN
COLOR 2
LOCATE 16, 30: PRINT "ÚÄÄÄ¿"
LOCATE 17, 30: PRINT "³": LOCATE 17, 34: PRINT "³"
LOCATE 18, 30: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF
IF x = 18 THEN
COLOR 2
LOCATE 16, 38: PRINT "ÚÄÄÄ¿"
LOCATE 17, 38: PRINT "³": LOCATE 17, 42: PRINT "³"
LOCATE 18, 38: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF
IF x = 19 THEN
COLOR 2
LOCATE 16, 46: PRINT "ÚÄÄÄ¿"
LOCATE 17, 46: PRINT "³": LOCATE 17, 50: PRINT "³"
LOCATE 18, 46: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF
IF x = 20 THEN
COLOR 2
LOCATE 16, 54: PRINT "ÚÄÄÄ¿"
LOCATE 17, 54: PRINT "³": LOCATE 17, 58: PRINT "³"
LOCATE 18, 54: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF
IF x = 21 THEN
COLOR 2
LOCATE 20, 22: PRINT "ÚÄÄÄ¿"
LOCATE 21, 22: PRINT "³": LOCATE 21, 26: PRINT "³"
LOCATE 22, 22: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF
IF x = 22 THEN
COLOR 2
LOCATE 20, 30: PRINT "ÚÄÄÄ¿"
LOCATE 21, 30: PRINT "³": LOCATE 21, 34: PRINT "³"
LOCATE 22, 30: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF
IF x = 23 THEN
COLOR 2
LOCATE 20, 38: PRINT "ÚÄÄÄ¿"
LOCATE 21, 38: PRINT "³": LOCATE 21, 42: PRINT "³"
LOCATE 22, 38: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF
IF x = 24 THEN
COLOR 2
LOCATE 20, 46: PRINT "ÚÄÄÄ¿"
LOCATE 21, 46: PRINT "³": LOCATE 21, 50: PRINT "³"
LOCATE 22, 46: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF
IF x = 25 THEN
COLOR 2
LOCATE 20, 54: PRINT "ÚÄÄÄ¿"
LOCATE 21, 54: PRINT "³": LOCATE 21, 58: PRINT "³"
LOCATE 22, 54: PRINT "ÀÄÄÄÙ"
COLOR 15
END IF

choice4:
oldx = x
test$ = INKEY$
IF test$ = "H" OR test$ = "P" OR test$ = "K" OR test$ = "M" THEN GOTO start2
IF RIGHT$(test$, 1) = "H" THEN x = x - 5: GOTO start2
IF RIGHT$(test$, 1) = "P" THEN x = x + 5: GOTO start2
IF RIGHT$(test$, 1) = "K" THEN x = x - 1: GOTO start2
IF RIGHT$(test$, 1) = "M" THEN x = x + 1: GOTO start2
IF test$ = " " THEN GOTO modify1
IF test$ = "Q" OR test$ = "q" THEN GOSUB ask1
GOTO choice4

END
modify1:
SOUND 4000, 2
IF x = 1 THEN
IF a(1) = 1 THEN a(1) = 0 ELSE a(1) = 1
IF a(2) = 1 THEN a(2) = 0 ELSE a(2) = 1
IF a(6) = 1 THEN a(6) = 0 ELSE a(6) = 1
END IF

IF x = 2 THEN
IF a(2) = 1 THEN a(2) = 0 ELSE a(2) = 1
IF a(1) = 1 THEN a(1) = 0 ELSE a(1) = 1
IF a(3) = 1 THEN a(3) = 0 ELSE a(3) = 1
IF a(7) = 1 THEN a(7) = 0 ELSE a(7) = 1
END IF

IF x = 3 THEN
IF a(3) = 1 THEN a(3) = 0 ELSE a(3) = 1
IF a(2) = 1 THEN a(2) = 0 ELSE a(2) = 1
IF a(4) = 1 THEN a(4) = 0 ELSE a(4) = 1
IF a(8) = 1 THEN a(8) = 0 ELSE a(8) = 1
END IF

IF x = 4 THEN
IF a(4) = 1 THEN a(4) = 0 ELSE a(4) = 1
IF a(3) = 1 THEN a(3) = 0 ELSE a(3) = 1
IF a(5) = 1 THEN a(5) = 0 ELSE a(5) = 1
IF a(9) = 1 THEN a(9) = 0 ELSE a(9) = 1
END IF

IF x = 5 THEN
IF a(5) = 1 THEN a(5) = 0 ELSE a(5) = 1
IF a(4) = 1 THEN a(4) = 0 ELSE a(4) = 1
IF a(10) = 1 THEN a(10) = 0 ELSE a(10) = 1
END IF

IF x = 6 THEN
IF a(6) = 1 THEN a(6) = 0 ELSE a(6) = 1
IF a(1) = 1 THEN a(1) = 0 ELSE a(1) = 1
IF a(7) = 1 THEN a(7) = 0 ELSE a(7) = 1
IF a(11) = 1 THEN a(11) = 0 ELSE a(11) = 1
END IF

IF x = 7 THEN
IF a(7) = 1 THEN a(7) = 0 ELSE a(7) = 1
IF a(2) = 1 THEN a(2) = 0 ELSE a(2) = 1
IF a(6) = 1 THEN a(6) = 0 ELSE a(6) = 1
IF a(8) = 1 THEN a(8) = 0 ELSE a(8) = 1
IF a(12) = 1 THEN a(12) = 0 ELSE a(12) = 1
END IF

IF x = 8 THEN
IF a(8) = 1 THEN a(8) = 0 ELSE a(8) = 1
IF a(3) = 1 THEN a(3) = 0 ELSE a(3) = 1
IF a(7) = 1 THEN a(7) = 0 ELSE a(7) = 1
IF a(9) = 1 THEN a(9) = 0 ELSE a(9) = 1
IF a(13) = 1 THEN a(13) = 0 ELSE a(13) = 1
END IF

IF x = 9 THEN
IF a(9) = 1 THEN a(9) = 0 ELSE a(9) = 1
IF a(4) = 1 THEN a(4) = 0 ELSE a(4) = 1
IF a(8) = 1 THEN a(8) = 0 ELSE a(8) = 1
IF a(10) = 1 THEN a(10) = 0 ELSE a(10) = 1
IF a(14) = 1 THEN a(14) = 0 ELSE a(14) = 1
END IF

IF x = 10 THEN
IF a(10) = 1 THEN a(10) = 0 ELSE a(10) = 1
IF a(5) = 1 THEN a(5) = 0 ELSE a(5) = 1
IF a(9) = 1 THEN a(9) = 0 ELSE a(9) = 1
IF a(15) = 1 THEN a(15) = 0 ELSE a(15) = 1
END IF

IF x = 11 THEN
IF a(11) = 1 THEN a(11) = 0 ELSE a(11) = 1
IF a(6) = 1 THEN a(6) = 0 ELSE a(6) = 1
IF a(12) = 1 THEN a(12) = 0 ELSE a(12) = 1
IF a(16) = 1 THEN a(16) = 0 ELSE a(16) = 1
END IF

IF x = 12 THEN
IF a(12) = 1 THEN a(12) = 0 ELSE a(12) = 1
IF a(7) = 1 THEN a(7) = 0 ELSE a(7) = 1
IF a(11) = 1 THEN a(11) = 0 ELSE a(11) = 1
IF a(13) = 1 THEN a(13) = 0 ELSE a(13) = 1
IF a(17) = 1 THEN a(17) = 0 ELSE a(17) = 1
END IF

IF x = 13 THEN
IF a(13) = 1 THEN a(13) = 0 ELSE a(13) = 1
IF a(8) = 1 THEN a(8) = 0 ELSE a(8) = 1
IF a(12) = 1 THEN a(12) = 0 ELSE a(12) = 1
IF a(14) = 1 THEN a(14) = 0 ELSE a(14) = 1
IF a(18) = 1 THEN a(18) = 0 ELSE a(18) = 1
END IF

IF x = 14 THEN
IF a(14) = 1 THEN a(14) = 0 ELSE a(14) = 1
IF a(9) = 1 THEN a(9) = 0 ELSE a(9) = 1
IF a(13) = 1 THEN a(13) = 0 ELSE a(13) = 1
IF a(15) = 1 THEN a(15) = 0 ELSE a(15) = 1
IF a(19) = 1 THEN a(19) = 0 ELSE a(19) = 1
END IF

IF x = 15 THEN
IF a(15) = 1 THEN a(15) = 0 ELSE a(15) = 1
IF a(10) = 1 THEN a(10) = 0 ELSE a(10) = 1
IF a(14) = 1 THEN a(14) = 0 ELSE a(14) = 1
IF a(20) = 1 THEN a(20) = 0 ELSE a(20) = 1
END IF

IF x = 16 THEN
IF a(16) = 1 THEN a(16) = 0 ELSE a(16) = 1
IF a(11) = 1 THEN a(11) = 0 ELSE a(11) = 1
IF a(17) = 1 THEN a(17) = 0 ELSE a(17) = 1
IF a(21) = 1 THEN a(21) = 0 ELSE a(21) = 1
END IF

IF x = 17 THEN
IF a(17) = 1 THEN a(17) = 0 ELSE a(17) = 1
IF a(12) = 1 THEN a(12) = 0 ELSE a(12) = 1
IF a(16) = 1 THEN a(16) = 0 ELSE a(16) = 1
IF a(18) = 1 THEN a(18) = 0 ELSE a(18) = 1
IF a(22) = 1 THEN a(22) = 0 ELSE a(22) = 1
END IF

IF x = 18 THEN
IF a(18) = 1 THEN a(18) = 0 ELSE a(18) = 1
IF a(13) = 1 THEN a(13) = 0 ELSE a(13) = 1
IF a(17) = 1 THEN a(17) = 0 ELSE a(17) = 1
IF a(19) = 1 THEN a(19) = 0 ELSE a(19) = 1
IF a(23) = 1 THEN a(23) = 0 ELSE a(23) = 1
END IF

IF x = 19 THEN
IF a(19) = 1 THEN a(19) = 0 ELSE a(19) = 1
IF a(14) = 1 THEN a(14) = 0 ELSE a(14) = 1
IF a(18) = 1 THEN a(18) = 0 ELSE a(18) = 1
IF a(20) = 1 THEN a(20) = 0 ELSE a(20) = 1
IF a(24) = 1 THEN a(24) = 0 ELSE a(24) = 1
END IF

IF x = 20 THEN
IF a(20) = 1 THEN a(20) = 0 ELSE a(20) = 1
IF a(15) = 1 THEN a(15) = 0 ELSE a(15) = 1
IF a(19) = 1 THEN a(19) = 0 ELSE a(19) = 1
IF a(25) = 1 THEN a(25) = 0 ELSE a(25) = 1
END IF

IF x = 21 THEN
IF a(21) = 1 THEN a(21) = 0 ELSE a(21) = 1
IF a(16) = 1 THEN a(16) = 0 ELSE a(16) = 1
IF a(22) = 1 THEN a(22) = 0 ELSE a(22) = 1
END IF

IF x = 22 THEN
IF a(22) = 1 THEN a(22) = 0 ELSE a(22) = 1
IF a(17) = 1 THEN a(17) = 0 ELSE a(17) = 1
IF a(21) = 1 THEN a(21) = 0 ELSE a(21) = 1
IF a(23) = 1 THEN a(23) = 0 ELSE a(23) = 1
END IF

IF x = 23 THEN
IF a(23) = 1 THEN a(23) = 0 ELSE a(23) = 1
IF a(18) = 1 THEN a(18) = 0 ELSE a(18) = 1
IF a(22) = 1 THEN a(22) = 0 ELSE a(22) = 1
IF a(24) = 1 THEN a(24) = 0 ELSE a(24) = 1
END IF

IF x = 24 THEN
IF a(24) = 1 THEN a(24) = 0 ELSE a(24) = 1
IF a(19) = 1 THEN a(19) = 0 ELSE a(19) = 1
IF a(23) = 1 THEN a(23) = 0 ELSE a(23) = 1
IF a(25) = 1 THEN a(25) = 0 ELSE a(25) = 1
END IF

IF x = 25 THEN
IF a(25) = 1 THEN a(25) = 0 ELSE a(25) = 1
IF a(20) = 1 THEN a(20) = 0 ELSE a(20) = 1
IF a(24) = 1 THEN a(24) = 0 ELSE a(24) = 1
END IF
GOTO start2

ask1:
CLS
LOCATE 10, 9
COLOR 30: PRINT "Are you sure you want to abandon this game and quit (Y/N)? ": COLOR 15
c:
test$ = INKEY$
IF test$ = "y" OR test$ = "Y" THEN MMENU
IF test$ = "n" OR test$ = "N" THEN
CLS
GOTO a
END IF
GOTO c

putlightsh:
IF a(1) = 1 THEN LOCATE 5, 24: COLOR 4: PRINT "Û": COLOR 15
IF a(2) = 1 THEN LOCATE 5, 32: COLOR 4: PRINT "Û": COLOR 15
IF a(3) = 1 THEN LOCATE 5, 40: COLOR 4: PRINT "Û": COLOR 15
IF a(4) = 1 THEN LOCATE 5, 48: COLOR 4: PRINT "Û": COLOR 15
IF a(5) = 1 THEN LOCATE 5, 56: COLOR 4: PRINT "Û": COLOR 15
IF a(6) = 1 THEN LOCATE 9, 24: COLOR 4: PRINT "Û": COLOR 15
IF a(7) = 1 THEN LOCATE 9, 32: COLOR 4: PRINT "Û": COLOR 15
IF a(8) = 1 THEN LOCATE 9, 40: COLOR 4: PRINT "Û": COLOR 15
IF a(9) = 1 THEN LOCATE 9, 48: COLOR 4: PRINT "Û": COLOR 15
IF a(10) = 1 THEN LOCATE 9, 56: COLOR 4: PRINT "Û": COLOR 15
IF a(11) = 1 THEN LOCATE 13, 24: COLOR 4: PRINT "Û": COLOR 15
IF a(12) = 1 THEN LOCATE 13, 32: COLOR 4: PRINT "Û": COLOR 15
IF a(13) = 1 THEN LOCATE 13, 40: COLOR 4: PRINT "Û": COLOR 15
IF a(14) = 1 THEN LOCATE 13, 48: COLOR 4: PRINT "Û": COLOR 15
IF a(15) = 1 THEN LOCATE 13, 56: COLOR 4: PRINT "Û": COLOR 15
IF a(16) = 1 THEN LOCATE 17, 24: COLOR 4: PRINT "Û": COLOR 15
IF a(17) = 1 THEN LOCATE 17, 32: COLOR 4: PRINT "Û": COLOR 15
IF a(18) = 1 THEN LOCATE 17, 40: COLOR 4: PRINT "Û": COLOR 15
IF a(19) = 1 THEN LOCATE 17, 48: COLOR 4: PRINT "Û": COLOR 15
IF a(20) = 1 THEN LOCATE 17, 56: COLOR 4: PRINT "Û": COLOR 15
IF a(21) = 1 THEN LOCATE 21, 24: COLOR 4: PRINT "Û": COLOR 15
IF a(22) = 1 THEN LOCATE 21, 32: COLOR 4: PRINT "Û": COLOR 15
IF a(23) = 1 THEN LOCATE 21, 40: COLOR 4: PRINT "Û": COLOR 15
IF a(24) = 1 THEN LOCATE 21, 48: COLOR 4: PRINT "Û": COLOR 15
IF a(25) = 1 THEN LOCATE 21, 56: COLOR 4: PRINT "Û": COLOR 15
IF a(1) = 0 AND a(2) = 0 AND a(3) = 0 AND a(4) = 0 AND a(5) = 0 AND a(6) = 0 AND a(7) = 0 AND a(8) = 0 AND a(9) = 0 AND a(10) = 0 THEN
IF (11) = 0 AND a(12) = 0 AND a(13) = 0 AND a(14) = 0 AND a(15) = 0 AND a(16) = 0 AND a(17) = 0 AND a(18) = 0 AND a(19) = 0 THEN
IF a(20) = 0 AND a(21) = 0 AND a(22) = 0 AND a(23) = 0 AND a(24) = 0 AND a(25) = 0 THEN win
END IF
END IF
RETURN

RETURN
END SUB

SUB HELP
CLS
COLOR 14: PRINT "INSTRUCTIONS": COLOR 15
PRINT
COLOR 10: PRINT "SEE THE LIGHT "; : COLOR 15: PRINT "is a puzzle game. It is intended for you to figure"
PRINT "out the pattern to the puzzle, but if you really need some help then"
PRINT "read the pattern section.  The object is to turn all the red lights ("; : COLOR 4: PRINT "Û"; : COLOR 15: PRINT ") off."
PRINT
COLOR 14: PRINT "CONTROLS": COLOR 15
PRINT
PRINT "Controlling "; : COLOR 10: PRINT "SEE THE LIGHT "; : COLOR 15: PRINT "is easy.  You simply use your arrow"
PRINT "keys to move the small green square withing the grid.  Pressing the "
PRINT "spacebar will activate (you get to figure out what that means) that grid."
PRINT
COLOR 14: PRINT "PATTERN"
PRINT : COLOR 15
PRINT "You really should try to figure out the pattern to the puzzle yourself, but"
PRINT "since I'm not a cruel person :-), I'll tell you what it is.  Each time you"
PRINT "press the spacebar to activate a panel, it will change the state of that panel"
PRINT "and every panel that it is touching (no diagonals).  Change the state means that"
PRINT "if a light is on it will be turned off, and vice versa."
PRINT ""
COLOR 30
PRINT "GOOD LUCK"
PRINT
COLOR 15
PRINT "Press any key to return to the main menu..."
DO WHILE INKEY$ = ""
LOOP
MMENU

END SUB

SUB MMENU
COLOR 15, 1
CLS
x = 6
start:
IF x < 6 THEN x = 10
IF x > 10 THEN x = 6
LOCATE 4, 1
PRINT "******************************** SEE THE LIGHT *********************************"
PRINT
PRINT TAB(20); "1. Play A Game"
PRINT
PRINT TAB(20); "2. Instructions & Help"
PRINT
PRINT TAB(20); "3. Exit to Operating System"
PRINT
PRINT "********************************************************************************"
PRINT
PRINT "     Please highlight your selection with the arrow keys and press enter..."
IF x = 6 THEN COLOR 1, 15: LOCATE 6, 20: PRINT "1. Play A Game": COLOR 15, 1
IF x = 8 THEN COLOR 1, 15: LOCATE 8, 20: PRINT "2. Instructions & Help": COLOR 15, 1
IF x = 10 THEN COLOR 1, 15: LOCATE 10, 20: PRINT "3. Exit to Operating System": COLOR 15, 1
choice:
test$ = INKEY$
IF test$ = "H" OR test$ = "P" THEN GOTO choice
IF RIGHT$(test$, 1) = "H" THEN x = x - 2: GOTO start
IF RIGHT$(test$, 1) = "P" THEN x = x + 2: GOTO start
IF test$ = CHR$(13) THEN GOTO sel
GOTO choice

sel:
SELECT CASE x
CASE 6
CHOOSE
CASE 8
HELP
CASE 10
XIT
END SELECT

END SUB

SUB TITLE
SCREEN 7, 0, 1, 0
COLOR 10
PRINT "SEE THE LIGHT"
DIM h(1000)
GET (0, 0)-(150, 20), h
CLS
REM PUT (100, 60), H
FOR i = 0 TO 100
PUT (i, 0), h
PCOPY 1, 0
CLS
NEXT i
FOR i = 0 TO 60
PUT (100, i), h
PCOPY 1, 0
CLS
NEXT i
LOCATE 17, 13
PUT (100, 60), h
COLOR 14: PRINT "Press any key..."
PCOPY 1, 0
DO WHILE INKEY$ = ""
LOOP
SCREEN 9
SCREEN 0
COLOR 15, 1
MMENU
END SUB

SUB win
CLS
PLAY "mbt140o3l4fl8ffl4gfgl2a"
LOCATE 4, 1
PRINT "********************************************************************************"
PRINT "*                                                                              *"
PRINT "*                         "; : COLOR 14: PRINT "CONGRATULATIONS!!!!!"; : COLOR 15: PRINT "                                 *"
PRINT "*                                                                              *"
PRINT "*                        "; : COLOR 30: PRINT " YOU ARE A WINNER!!!"; : COLOR 15: PRINT "                                  *"
PRINT "*                                                                              *"
PRINT "********************************************************************************"
PRINT
PRINT "                             Press any key...."
DO WHILE INKEY$ = ""
LOOP
MMENU
END
END SUB

SUB XIT
CLS
COLOR 30
PRINT "Thank you for playing "; : COLOR 10: PRINT "SEE THE LIGHT "; : COLOR 15: PRINT "by Pete Collins."
PRINT
PRINT "Press any key to return to your operating system..."
DO WHILE INKEY$ = ""
LOOP
COLOR 7, 0
SYSTEM
END SUB

