
'THANK YOU FOR GETTING THIS GREAT GAME
'DON'T FORGET, YOU CAN E-MAIL ME AT AOL SCREEN NAME RJD42
'E-MAIL FOR MORE INFO ABOUT VERSION 3 OR MY NEW GAMES
'AND BE SURE TO ASK ABOUT MY NEW QBASIC ROLE PLAYING GAME!

DECLARE SUB cone (ch%, cv%, HEIGHT%, size%)
DECLARE SUB gethighscore ()
DECLARE SUB drawenemyplain ()
DECLARE SUB drawenemyblock ()
DECLARE SUB sbsoundtest ()
DECLARE SUB font (fh%, fv%, letter$, col%)
DECLARE FUNCTION DetectCard% ()
DECLARE SUB SBInit ()
DECLARE SUB delay (break%)
DECLARE SUB soundtest ()
DECLARE SUB grafxtest ()
DECLARE SUB drawfireball ()
DECLARE SUB fireballexplode ()
DECLARE SUB puthighscore ()
DECLARE SUB song1 ()
DECLARE SUB song2 ()
DECLARE SUB note (level%, legnth%)
DECLARE SUB background1 ()
DECLARE SUB background2 ()
DECLARE SUB backgroundpit ()
DECLARE SUB intro ()
DECLARE SUB drawguy1stick ()
DECLARE SUB drawguy2stretch ()
DECLARE SUB drawguy1knee ()
DECLARE SUB statusbar ()
DECLARE SUB soundeffect (kind$)
DECLARE SUB soundoptions ()
DECLARE SUB endpicture ()
DECLARE SUB WriteReg (Reg%, Value%)
DECLARE SUB SBPlay (note%)
DECLARE SUB drawguy1plain ()
DECLARE SUB drawguy2plain ()
DECLARE SUB drawguy2punching ()
DECLARE SUB drawguy2kicking ()
DECLARE SUB drawguy2victory ()
DECLARE SUB drawguy2ducking ()
DECLARE SUB drawguy1punching ()
DECLARE SUB drawguy1kicking ()
DECLARE SUB drawguy1blocking ()
DECLARE SUB drawguy1ducking ()
DECLARE SUB enemypattern ()
DECLARE SUB exitprogram ()
DECLARE SUB drawenemykicking ()
DECLARE SUB enemyfall ()
DECLARE SUB punchoffledge ()
DECLARE SUB ending ()
DECLARE SUB guy1ending ()
DECLARE SUB guy2ending ()
DECLARE SUB reward ()
DECLARE SUB movelist ()
DECLARE SUB drawenemyfallen ()
DECLARE SUB characterpick ()
DECLARE SUB round (roundnum%)
DECLARE SUB fight (fightnumber%)
DECLARE SUB fightstartup ()
DECLARE SUB fightwithchar1 ()
DECLARE SUB fightwithchar1duck ()
DECLARE SUB fightwithchar1fire ()
DECLARE SUB fightwithenemyfall ()
DECLARE SUB fightwithchar1blck ()
DECLARE SUB fightwithchar2 ()
DECLARE SUB fightwithchar2duck ()
CONST false = 0, true = NOT false
DIM SHARED practicemode$, mcounttimer%, colytimer%, mcount%, coly%, dmode$, dcount%, zcount%, no$, v%, inspike$, change$, name$, firsttime$, ch%, cv%, HEIGHT%, col%, backcol%, size%, firetimer%, rndnum%, highrating$, difficulty%, col1%, col2%, col3%, oldcombodamage%, combodamage%, oldcombohits%, hittimer%, combohits%, COMBOTIMER%, highfightwins%, fightwins%, enemyposition$, fireball$, fightnum%, blocking$, LESSLIMIT%, MORELIMIT%, bg%, ph%, pvv%, charpick%, kind$, enemylife%, guy1position$, soundcard$, sounds$, gl%, dsoundcard$, ncount%, break%, life%, time%, evv%, eh%, score%, scount%, bh%, bv%, highscore%, yesjump%, nname$
nname$ = "Noah": name$ = "Noah": highscore% = 2500: highfightwins% = 10
col1% = 8: col2% = 4: col3% = 11: inspike$ = "NO": dmode$ = "NO"
difficulty% = 4: coly% = 10: practicemode$ = "NO"
firsttime$ = "YES"
RANDOMIZE TIMER
bg% = INT(RND * 1) + 1'2'1
IF bg% = 1 THEN LESSLIMIT% = 10: MORELIMIT% = 310
IF bg% = 2 THEN LESSLIMIT% = 20: MORELIMIT% = 290
IF DetectCard = true THEN
soundcard$ = "YES": GOTO 15
ELSE soundcard$ = "NO": dsoundcard$ = "NO": GOTO 15
END IF
IF dsoundcard$ = "NO" THEN soundcard$ = "NO"
15 CLS : SCREEN 13: PAINT (1, 1), 15
FOR delay28% = 0 TO 10 STEP 1
delay 32766
NEXT delay28%
FOR HEIGHT% = 210 TO -10 STEP -1
font 110, HEIGHT%, "P", 0
font 120, HEIGHT%, "R", 0
font 130, HEIGHT%, "O", 0
font 140, HEIGHT%, "G", 0
font 150, HEIGHT%, "R", 0
font 160, HEIGHT%, "A", 0
font 170, HEIGHT%, "M", 0
font 180, HEIGHT%, "E", 0
font 190, HEIGHT%, "D", 0
font 145, HEIGHT% + 10, "B", 0
font 155, HEIGHT% + 10, "Y", 0
delay 4000
font 110, HEIGHT%, "P", 15
font 120, HEIGHT%, "R", 15
font 130, HEIGHT%, "O", 15
font 140, HEIGHT%, "G", 15
font 150, HEIGHT%, "R", 15
font 160, HEIGHT%, "A", 15
font 170, HEIGHT%, "M", 15
font 180, HEIGHT%, "E", 15
font 190, HEIGHT%, "D", 15
font 145, HEIGHT% + 10, "B", 15
font 155, HEIGHT% + 10, "Y", 15
NEXT HEIGHT%
FOR HEIGHT% = 210 TO -10 STEP -1
font 100, HEIGHT%, "N", 0
font 110, HEIGHT%, "O", 0
font 120, HEIGHT%, "A", 0
font 130, HEIGHT%, "H", 0
font 150, HEIGHT%, "D", 0
font 160, HEIGHT%, "A", 0
font 170, HEIGHT%, "L", 0
font 180, HEIGHT%, "E", 0
font 190, HEIGHT%, "O", 0
delay 4000
font 100, HEIGHT%, "N", 15
font 110, HEIGHT%, "O", 15
font 120, HEIGHT%, "A", 15
font 130, HEIGHT%, "H", 15
font 150, HEIGHT%, "D", 15
font 160, HEIGHT%, "A", 15
font 170, HEIGHT%, "L", 15
font 180, HEIGHT%, "E", 15
font 190, HEIGHT%, "O", 15
NEXT HEIGHT%
FOR delay29% = 0 TO 10 STEP 1
delay 32766
NEXT delay29%
CLS : CALL soundoptions
IF dsoundcard$ = "NO" THEN soundcard$ = "NO"
CALL intro
CALL puthighscore
CALL characterpick
fightnum% = 1
fight:
IF fightnum% > 11 THEN GOTO 680
fight fightnum%
IF firsttime$ = "YES" THEN firsttime$ = "NO": GOTO 768
IF bg% = 1 THEN bg% = 2: GOTO 768
IF bg% = 2 THEN bg% = 1
768 IF bg% = 1 THEN LESSLIMIT% = 10: MORELIMIT% = 310
IF bg% = 2 THEN LESSLIMIT% = 20: MORELIMIT% = 290
wins% = 0
rndnum% = 1
IF charpick% = 1 THEN CALL fightwithchar1
IF charpick% = 2 THEN CALL fightwithchar2
IF life% > enemylife% THEN wins% = wins% + 1
rndnum% = 2
IF charpick% = 1 THEN CALL fightwithchar1
IF charpick% = 2 THEN CALL fightwithchar2
IF life% > enemylife% THEN wins% = wins% + 1
IF wins% = 2 THEN status$ = "WIN": GOTO 678
IF wins% = 0 THEN status$ = "LOSE": GOTO 678
rndnum% = 3
IF charpick% = 1 THEN CALL fightwithchar1
IF charpick% = 2 THEN CALL fightwithchar2
IF life% > enemylife% THEN wins% = wins% + 1
IF wins% = 2 THEN status$ = "WIN": GOTO 678
IF wins% < 2 THEN status$ = "LOSE": GOTO 678
678 IF status$ = "WIN" THEN fightwins% = fightwins% + 1: fightnum% = fightnum% + 1: col1% = col1% + 1: col2% = col2% + 1: GOTO fight
679 CLS : SCREEN 13: LOCATE 13, 15: COLOR 10: PAINT (1, 1), 0
font 110, 100, "Y", 4
font 120, 100, "O", 4
font 130, 100, "U", 4
font 150, 100, "L", 4
font 160, 100, "O", 4
font 170, 100, "S", 4
font 180, 100, "E", 4
SOUND 50, 5
delay 32766: delay 32766: delay 32766: delay 32766: delay 32766:
delay 32766: delay 32766: delay 32766: delay 32766: CALL exitprogram
680 CLS : SCREEN 13: LOCATE 13, 15: COLOR 10: PAINT (1, 1), 0
font 110, 100, "Y", 15
font 120, 100, "O", 15
font 130, 100, "U", 15
font 150, 100, "W", 15
font 160, 100, "I", 15
font 170, 100, "N", 15
coly% = 10'!
delay 32766: delay 32766: delay 32766: delay 32766: delay 32766:
CALL song2: delay 32766: delay 32766: delay 32766: delay 32766:
CALL ending
CALL reward
CALL exitprogram

DEFINT A-Z
SUB background1
LINE (0, 184)-(319, 200), 0, BF
PAINT (1, 1), 9
LINE (0, gl% + 8)-(319, gl% + 8), 0
PAINT (1, 199), 2, 0
COLOR 4'?
CIRCLE (40, 40), 7, 15
PAINT (40, 40), 15, 15
CIRCLE (50, 36), 9, 15
PAINT (50, 36), 15, 15
CIRCLE (60, 40), 7, 15
PAINT (60, 40), 15, 15
CIRCLE (50, 40), 8, 15
PAINT (50, 40), 15, 15
PAINT (50, 45), 15, 15
CIRCLE (140, 60), 7, 15
PAINT (140, 60), 15, 15
CIRCLE (150, 56), 9, 15
PAINT (150, 56), 15, 15
CIRCLE (160, 60), 7, 15
PAINT (160, 60), 15, 15
CIRCLE (150, 60), 8, 15
PAINT (150, 60), 15, 15
PAINT (150, 65), 15, 15
CIRCLE (240, 30), 7, 15
PAINT (240, 30), 15, 15
CIRCLE (250, 26), 9, 15
PAINT (250, 26), 15, 15
CIRCLE (260, 30), 7, 15
PAINT (260, 30), 15, 15
CIRCLE (250, 30), 8, 15
PAINT (250, 30), 15, 15
PAINT (250, 35), 15, 15
LINE (5, gl% + 7)-(5, gl% - 35), 9
END SUB

DEFSNG A-Z
SUB background2
LINE (0, 184)-(319, 200), 0, BF
PAINT (1, 1), 9
LINE (0, gl% + 8)-(319, gl% + 8), 0
PAINT (1, 199), 2, 0
COLOR 4
LINE (0, gl% - 100)-(35, gl% - 125), 0
LINE (0, gl% - 100)-(320, gl% - 100), 0
LINE (320, gl% - 100)-(285, gl% - 125), 0
PAINT (100, 20), 6, 0
LINE (0, gl% + 8)-(5, gl% - 100), 0, BF
LINE (320, gl% + 8)-(315, gl% - 100), 0, BF
END SUB

SUB backgroundpit
PAINT (1, 100), backcol%
LINE (35, 0)-(35, 175), 0
LINE (285, 0)-(285, 175), 0
LINE (0, 170)-(320, 200), 8, BF
LINE (0, 0)-(34, 200), 8, BF
LINE (320, 0)-(286, 200), 8, BF
LINE (35, 170)-(285, 170), 0
END SUB

SUB characterpick
CLS : SCREEN 13: PAINT (1, 1), 8: COLOR 15
LOCATE 1, 1: PRINT "                                        "
LOCATE 2, 1: PRINT "         Pick your character:                                                   "
LOCATE 22, 1: PRINT "                                        "
LOCATE 23, 1: PRINT "                                        "
LINE (0, 180)-(320, 200), 0, BF
ph% = 100: pvv% = 55:
CIRCLE (ph%, pvv% + 30), 4, 0
LINE (ph% - 3, pvv% + 33)-(ph% + 2, pvv% + 40), 0, B
LINE (ph% - 6, pvv% + 34)-(ph% + 5, pvv% + 34), 0
LINE (ph% + 6, pvv% + 35)-(ph% + 6, pvv% + 39), 0
LINE (ph% + 4, pvv% + 40)-(ph% + 5, pvv% + 40), 0
LINE (ph% + 4, pvv% + 39)-(ph% + 4, pvv% + 36), 0
LINE (ph% + 3, pvv% + 36)-(ph% + 3, pvv% + 36), 0
LINE (ph%, pvv% + 30)-(ph%, pvv% + 30), 0
LINE (ph% + 2, pvv% + 30)-(ph% + 2, pvv% + 30), 0
LINE (ph% - 2, pvv% + 27)-(ph% + 2, pvv% + 27), 0
LINE (ph% - 7, pvv% + 35)-(ph% - 7, pvv% + 39), 0
LINE (ph% - 6, pvv% + 40)-(ph% - 5, pvv% + 40), 0
LINE (ph% - 5, pvv% + 40)-(ph% - 5, pvv% + 36), 0
LINE (ph% - 5, pvv% + 36)-(ph% - 4, pvv% + 36), 0
LINE (ph% + 2, pvv% + 40)-(ph% + 5, pvv% + 48), 0
LINE (ph% - 3, pvv% + 40)-(ph% - 6, pvv% + 47.75), 0
LINE (ph% + 5, pvv% + 48)-(ph% + 2, pvv% + 48), 0
LINE (ph% - 6, pvv% + 48)-(ph% - 6, pvv% + 48), 3
LINE (ph% - 5, pvv% + 48)-(ph% - 2, pvv% + 48), 0
LINE (ph% - 3, pvv% + 48)-(ph% - 1, pvv% + 45), 0
LINE (ph% + 2, pvv% + 48)-(ph%, pvv% + 44), 0
LINE (ph% + 2, pvv% + 48)-(ph% + 6, pvv% + 50), 0, BF
LINE (ph% - 2, pvv% + 48)-(ph% - 6, pvv% + 50), 0, BF
PAINT (ph% + 3, pvv% + 47), 1, 0
PAINT (ph%, pvv% + 38), 2, 0
PAINT (ph% + 1, pvv% + 31), 11, 0
PAINT (ph% + 5, pvv% + 36), 11, 0
PAINT (ph% - 6, pvv% + 36), 11, 0
ph% = 200: pvv% = 96
CIRCLE (ph%, pvv%), 7, 0
LINE (ph% - 2, pvv%)-(ph% - 2, pvv% - 1), 0
LINE (ph% + 2, pvv%)-(ph% + 2, pvv% - 1), 0
LINE (ph% - 3, pvv% + 2)-(ph% + 1, pvv% + 3), 0
LINE (ph% + 2, pvv% + 3)-(ph% + 3, pvv% + 2), 0
PAINT (ph%, pvv%), 10, 0
LINE (ph% + 3, pvv% + 5)-(ph% + 5, pvv% + 8), 0, B
LINE (ph% - 3, pvv% + 5)-(ph% - 5, pvv% + 8), 0, B
PAINT (ph% + 4, pvv% + 6), 12, 0
PAINT (ph% - 4, pvv% + 6), 12, 0
LINE (ph% + 6, pvv% + 8)-(ph% + 3, pvv% + 9), 0, BF
LINE (ph% - 6, pvv% + 8)-(ph% - 3, pvv% + 9), 0, BF
LINE (ph% + 8, pvv%)-(ph% + 10, pvv% + 5), 0, B
PSET (ph% + 9, pvv% - 1), 0
LINE (ph% + 9, pvv%)-(ph% + 9, pvv% + 3), 12
PSET (ph% + 8, pvv% - 1), 0
LINE (ph% + 8, pvv%)-(ph% + 8, pvv%), 12
LINE (ph% - 8, pvv%)-(ph% - 10, pvv% + 5), 0, B
PSET (ph% - 9, pvv% - 1), 0
LINE (ph% - 9, pvv%)-(ph% - 9, pvv% + 3), 12
PSET (ph% - 8, pvv% - 1), 0
LINE (ph% - 8, pvv%)-(ph% - 8, pvv%), 12
PSET (ph% + 9, pvv% + 4), 0
PSET (ph% - 9, pvv% + 4), 0
charpick% = 1
COLOR 4
LOCATE 17, 10: PRINT "Derrick"
LOCATE 18, 10: PRINT "Leirbag"
LOCATE 17, 23: PRINT "Fatty "
LOCATE 18, 23: PRINT "Samson"
COLOR 15
689 DO
IF charpick% = 1 THEN
LINE (75, 70)-(125, 120), 0, B
END IF
IF charpick% = 2 THEN
LINE (175, 70)-(225, 120), 0, B
END IF
SELECT CASE INKEY$
CASE IS = CHR$(0) + "M"
IF charpick% = 1 THEN
GOSUB DING
LINE (75, 70)-(125, 120), 8, B: charpick% = 2: GOTO 689
END IF
IF charpick% = 2 THEN
GOSUB DING
LINE (175, 70)-(225, 120), 8, B: charpick% = 1: GOTO 689
END IF
CASE IS = CHR$(0) + "K"
IF charpick% = 1 THEN
GOSUB DING
LINE (75, 70)-(125, 120), 8, B: charpick% = 2: GOTO 689
END IF
IF charpick% = 2 THEN
GOSUB DING
LINE (175, 70)-(225, 120), 8, B: charpick% = 1: GOTO 689
END IF
CASE IS = CHR$(13)
GOSUB DING
EXIT DO
CASE IS = " "
GOSUB DING
EXIT DO
END SELECT
LOOP
CLS : SLEEP 1: GOTO 89
DING:
soundeffect "DING"
RETURN
89
END SUB

SUB cone (ch%, cv%, HEIGHT%, size%)
oldsize% = size%: oldcv% = cv%
56 CIRCLE (ch%, cv%), size%, col%
FOR this% = 1 TO (HEIGHT% / 16)
CIRCLE (ch%, cv% + this%), size%, col%
NEXT this%
IF size% > 1 THEN size% = size% - 1: cv% = cv% - HEIGHT% / oldsize%: GOTO 56
size% = oldsize%: cv% = oldcv%  ':col%=col%+1
CIRCLE (ch%, cv%), size%, 8
LINE (ch%, cv% - HEIGHT%)-(ch%, cv% + size% - (size% / 5)), 31
LINE (ch%, cv% - HEIGHT%)-(ch%, cv% - size% + (size% / 5)), 31
LINE (ch%, cv% - HEIGHT%)-(ch% - size%, cv%), 16
LINE (ch%, cv% - HEIGHT%)-(ch% + size%, cv%), 16
LINE (ch%, cv% - HEIGHT%)-(ch% + (size% * .75), cv% + ((size% - (size% / 5)) * .75) - 1), 24
LINE (ch%, cv% - HEIGHT%)-(ch% - (size% * .75), cv% + ((size% - (size% / 5)) * .75) - 1), 24
LINE (ch%, cv% - HEIGHT%)-(ch% + (size% * .75), cv% - ((size% - (size% / 5)) * .75) + 1), 24
LINE (ch%, cv% - HEIGHT%)-(ch% - (size% * .75), cv% - ((size% - (size% / 5)) * .75) + 1), 24
LINE (ch%, cv% - HEIGHT%)-(ch% + (size% * .4), cv% + ((size% - (size% / 5)) * .9) - 1), 28
LINE (ch%, cv% - HEIGHT%)-(ch% - (size% * .4), cv% + ((size% - (size% / 5)) * .9) - 1), 28
LINE (ch%, cv% - HEIGHT%)-(ch% + (size% * .4), cv% - ((size% - (size% / 5)) * .9) + 1), 28
LINE (ch%, cv% - HEIGHT%)-(ch% - (size% * .4), cv% - ((size% - (size% / 5)) * .9) + 1), 28
LINE (ch%, cv% - HEIGHT%)-(ch% + (size% * .9), cv% + ((size% - (size% / 5)) * .4) - 1), 20
LINE (ch%, cv% - HEIGHT%)-(ch% - (size% * .9), cv% + ((size% - (size% / 5)) * .4) - 1), 20
LINE (ch%, cv% - HEIGHT%)-(ch% + (size% * .9), cv% - ((size% - (size% / 5)) * .4) + 1), 20
LINE (ch%, cv% - HEIGHT%)-(ch% - (size% * .9), cv% - ((size% - (size% / 5)) * .4) + 1), 20
LINE (ch%, cv% - HEIGHT%)-(ch% + (size% * .85), cv% + ((size% - (size% / 5)) * .6) - 1), 22
LINE (ch%, cv% - HEIGHT%)-(ch% - (size% * .85), cv% + ((size% - (size% / 5)) * .6) - 1), 22'col%: col% = col% - 1
LINE (ch%, cv% - HEIGHT%)-(ch% + (size% * .85), cv% - ((size% - (size% / 5)) * .6) + 1), 22'col%: col% = col% - 1
LINE (ch%, cv% - HEIGHT%)-(ch% - (size% * .85), cv% - ((size% - (size% / 5)) * .6) + 1), 22'col%: col% = col% - 1
LINE (ch%, cv% - HEIGHT%)-(ch% + (size% * .6), cv% + ((size% - (size% / 5)) * .85) - 1), 26'col%: col% = col% - 1
LINE (ch%, cv% - HEIGHT%)-(ch% - (size% * .6), cv% + ((size% - (size% / 5)) * .85) - 1), 26'col%: col% = col% - 1
LINE (ch%, cv% - HEIGHT%)-(ch% + (size% * .6), cv% - ((size% - (size% / 5)) * .85) + 1), 26'col%: col% = col% - 1
LINE (ch%, cv% - HEIGHT%)-(ch% - (size% * .6), cv% - ((size% - (size% / 5)) * .85) + 1), 26'col%: col% = col% - 1
LINE (ch%, cv% - HEIGHT%)-(ch% + (size% * .2), cv% + ((size% - (size% / 5)) * 1) - 1), 30'col%: col% = col% - 1
LINE (ch%, cv% - HEIGHT%)-(ch% - (size% * .2), cv% + ((size% - (size% / 5)) * 1) - 1), 30'col%: col% = col% - 1
LINE (ch%, cv% - HEIGHT%)-(ch% + (size% * .2), cv% - ((size% - (size% / 5)) * 1) + 1), 30'col%: col% = col% - 1
LINE (ch%, cv% - HEIGHT%)-(ch% - (size% * .2), cv% - ((size% - (size% / 5)) * 1) + 1), 30'col%: col% = col% - 1
LINE (ch%, cv% - HEIGHT%)-(ch% + (size% * .95), cv% + ((size% - (size% / 5)) * .2) - 1), 18'col%: col% = col% - 1
LINE (ch%, cv% - HEIGHT%)-(ch% - (size% * .95), cv% + ((size% - (size% / 5)) * .2) - 1), 18'col%: col% = col% - 1
LINE (ch%, cv% - HEIGHT%)-(ch% + (size% * .95), cv% - ((size% - (size% / 5)) * .2) + 1), 18'col%: col% = col% - 1
LINE (ch%, cv% - HEIGHT%)-(ch% - (size% * .95), cv% - ((size% - (size% / 5)) * .2) + 1), 18'col%: col% = col% - 1
END SUB

DEFINT A-Z
SUB delay (break%)
FOR W% = 1 TO break% STEP 1: NEXT W%
END SUB

FUNCTION DetectCard%
CALL WriteReg(&H4, &H60)
CALL WriteReg(&H4, &H80)
B = INP(&H388)
CALL WriteReg(&H2, &HFF)
CALL WriteReg(&H4, &H21)
FOR x = 0 TO 130
A = INP(&H388)
NEXT x
c = INP(&H388)
CALL WriteReg(&H4, &H60)
CALL WriteReg(&H4, &H80)
Success = 0
IF (B AND &HE0) = &H0 THEN
IF (c AND &HE0) = &HC0 THEN
Success = -1
END IF
END IF
DetectCard% = Success
END FUNCTION

DEFSNG A-Z
SUB drawenemyblock
CIRCLE (eh% - 1, evv% + 30), 4, 0
LINE (eh% - 3, evv% + 33)-(eh% + 2, evv% + 40), 0, B
LINE (eh% - 5, evv% + 34)-(eh% + 1, evv% + 34), 0
LINE (eh% - 8, evv% + 40 - 5)-(eh% - 8, evv% + 40 - 9), 0
LINE (eh% - 6, evv% + 40 - 10)-(eh% - 7, evv% + 40 - 10), 0
LINE (eh% - 6, evv% + 40 - 9)-(eh% - 6, evv% + 40 - 6), 0
LINE (eh% - 5, evv% + 40 - 6)-(eh% - 5, evv% + 40 - 6), 0
LINE (eh% - 7, evv% + 40 - 4)-(eh% - 3, evv% + 40 - 4), 0
LINE (eh% - 1, evv% + 30)-(eh% - 1, evv% + 30), 0
LINE (eh% - 3, evv% + 30)-(eh% - 3, evv% + 30), 0
LINE (eh% + 2, evv% + 40)-(eh% + 5, evv% + 48), 0
LINE (eh% - 3, evv% + 40)-(eh% - 6, evv% + 47.75), 0
LINE (eh% + 5, evv% + 48)-(eh% + 2, evv% + 48), 0
LINE (eh% - 6, evv% + 48)-(eh% - 6, evv% + 48), 3
LINE (eh% - 5, evv% + 48)-(eh% - 2, evv% + 48), 0
LINE (eh% - 3, evv% + 48)-(eh% - 1, evv% + 45), 0
LINE (eh% + 2, evv% + 48)-(eh%, evv% + 44), 0
LINE (eh% + 2, evv% + 48)-(eh% + 6, evv% + 50), 0, BF
LINE (eh% - 2, evv% + 48)-(eh% - 6, evv% + 50), 0, BF
PAINT (eh% + 3, evv% + 47), col1%, 0
PAINT (eh%, evv% + 38), col2%, 0
PAINT (eh% + 1, evv% + 31), col3%, 0
PAINT (eh% - 5, evv% + 35), col3%, 0
END SUB

SUB drawenemyfallen
CIRCLE (eh% + 22, evv% + 47), 4, 0
LINE (eh% + 18, evv% + 50)-(eh% + 9, evv% + 44), 0, B
LINE (eh% + 16, evv% + 48)-(eh% + 11, evv% + 46), 0, B
LINE (eh% + 22, evv% + 46)-(eh% + 22, evv% + 46), 0
PAINT (eh% + 15, evv% + 47), col3%, 0
PAINT (eh% + 17, evv% + 48), col2%, 0
PAINT (eh% + 22, evv% + 48), col3%, 0
LINE (eh% + 9, evv% + 45)-(eh% + 0, evv% + 50), 0, B
PAINT (eh% + 8, evv% + 46), col1%, 0
LINE (eh% + 0, evv% + 49)-(eh% - 2, evv% + 44), 0, BF
IF col3% = 0 AND inspike$ = "NO" AND fightnum% <> 11 THEN LINE (eh% + 16, evv% + 45)-(eh% + 18, evv% + 39), 0, BF
IF col3% = 0 AND inspike$ <> "NO" THEN LINE (eh% + 16, evv% + 45)-(eh% + 18, evv% + 39), 9, BF
END SUB

SUB drawenemykicking
CIRCLE (eh% - 1, evv% + 30), 4, 0
LINE (eh% - 3, evv% + 33)-(eh% + 2, evv% + 40), 0, B
LINE (eh% - 6, evv% + 34)-(eh% + 5, evv% + 34), 0
LINE (eh% + 6, evv% + 35)-(eh% + 6, evv% + 39), 0
LINE (eh% + 4, evv% + 40)-(eh% + 5, evv% + 40), 0
LINE (eh% + 4, evv% + 39)-(eh% + 4, evv% + 36), 0
LINE (eh% + 3, evv% + 36)-(eh% + 3, evv% + 36), 0
LINE (eh%, evv% + 30)-(eh%, evv% + 30), 0
LINE (eh% - 2, evv% + 30)-(eh% - 2, evv% + 30), 0
LINE (eh% - 7, evv% + 35)-(eh% - 7, evv% + 39), 0
LINE (eh% - 6, evv% + 40)-(eh% - 5, evv% + 40), 0
LINE (eh% - 5, evv% + 40)-(eh% - 5, evv% + 36), 0
LINE (eh% - 5, evv% + 36)-(eh% - 4, evv% + 36), 0
LINE (eh% + 2, evv% + 40)-(eh% + 5, evv% + 47.75), 0
LINE (eh% + 2, evv% + 48)-(eh% + 0, evv% + 45), 0
LINE (eh% + 1, evv% + 48)-(eh% + 5, evv% + 50), 0, BF
LINE (eh% - 6, evv% + 44)-(eh% - 8, evv% + 40), 0
LINE (eh% - 9, evv% + 40)-(eh% - 9, evv% + 44), 0
LINE (eh% - 9, evv% + 44)-(eh% - 7, evv% + 44), 0
PAINT (eh% - 8, evv% + 43), 0, 0
LINE (eh% - 8, evv% + 41)-(eh% - 3, evv% + 41), 0
LINE (eh% - 1, evv% + 44)-(eh% - 6, evv% + 44), 0
PSET (eh% + 2, evv% + 27), 9
PAINT (eh% + 2, evv% + 43), col1%, 0
PAINT (eh%, evv% + 38), col2%, 0
PAINT (eh% + 1, evv% + 31), col3%, 0
PAINT (eh% + 5, evv% + 36), col3%, 0
PAINT (eh% - 6, evv% + 36), col3%, 0
soundeffect "KICK"
END SUB

DEFINT A-Z
SUB drawenemyplain
CIRCLE (eh% - 1, evv% + 30), 4, 0
LINE (eh% - 3, evv% + 33)-(eh% + 2, evv% + 40), 0, B
LINE (eh% - 6, evv% + 34)-(eh% + 5, evv% + 34), 0
LINE (eh% + 6, evv% + 35)-(eh% + 6, evv% + 39), 0
LINE (eh% + 4, evv% + 40)-(eh% + 5, evv% + 40), 0
LINE (eh% + 4, evv% + 39)-(eh% + 4, evv% + 36), 0
LINE (eh% + 3, evv% + 36)-(eh% + 3, evv% + 36), 0
LINE (eh% - 2, evv% + 30)-(eh% - 2, evv% + 30), 0
LINE (eh%, evv% + 30)-(eh%, evv% + 30), 0
LINE (eh% - 2, evv% + 27)-(eh% + 2, evv% + 27), 0
LINE (eh% - 7, evv% + 35)-(eh% - 7, evv% + 39), 0
LINE (eh% - 6, evv% + 40)-(eh% - 5, evv% + 40), 0
LINE (eh% - 5, evv% + 40)-(eh% - 5, evv% + 36), 0
LINE (eh% - 5, evv% + 36)-(eh% - 4, evv% + 36), 0
LINE (eh% + 2, evv% + 40)-(eh% + 5, evv% + 48), 0
LINE (eh% - 3, evv% + 40)-(eh% - 6, evv% + 47.75), 0
LINE (eh% + 5, evv% + 48)-(eh% + 2, evv% + 48), 0
LINE (eh% - 6, evv% + 48)-(eh% - 6, evv% + 48), 3
LINE (eh% - 5, evv% + 48)-(eh% - 2, evv% + 48), 0
LINE (eh% - 3, evv% + 48)-(eh% - 1, evv% + 45), 0
LINE (eh% + 2, evv% + 48)-(eh%, evv% + 44), 0
LINE (eh% + 2, evv% + 48)-(eh% + 6, evv% + 50), 0, BF
LINE (eh% - 2, evv% + 48)-(eh% - 6, evv% + 50), 0, BF
LINE (eh% + 2, evv% + 27)-(eh% + 2, evv% + 27), 9
PAINT (eh% - 4, evv% + 47), col1%, 0
PAINT (eh%, evv% + 38), col2%, 0
PAINT (eh% + 1, evv% + 31), col3%, 0
PAINT (eh% + 5, evv% + 36), col3%, 0
PAINT (eh% - 6, evv% + 36), col3%, 0
END SUB

DEFSNG A-Z
SUB drawfireball
CIRCLE (bh%, bv%), 3, 0
CIRCLE (bh%, bv%), 2, 23
CIRCLE (bh%, bv%), 1, 14
PSET (bh% + 1, bv% + 1), 2
PSET (bh% - 1, bv% + 1), 2
PSET (bh% - 1, bv% - 1), 2
PSET (bh% + 1, bv% - 1), 2
END SUB

SUB drawguy1blocking
CIRCLE (ph%, pvv% + 30), 4, 0
LINE (ph% - 3, pvv% + 33)-(ph% + 2, pvv% + 40), 0, B
LINE (ph% - 3, pvv% + 34)-(ph% + 5, pvv% + 34), 0
LINE (ph% + 8, pvv% + 40 - 5)-(ph% + 8, pvv% + 40 - 9), 0
LINE (ph% + 6, pvv% + 40 - 10)-(ph% + 7, pvv% + 40 - 10), 0
LINE (ph% + 6, pvv% + 40 - 9)-(ph% + 6, pvv% + 40 - 6), 0
LINE (ph% + 5, pvv% + 40 - 6)-(ph% + 5, pvv% + 40 - 6), 0
LINE (ph% + 7, pvv% + 40 - 4)-(ph% + 3, pvv% + 40 - 4), 0
LINE (ph%, pvv% + 30)-(ph%, pvv% + 30), 0
LINE (ph% + 2, pvv% + 30)-(ph% + 2, pvv% + 30), 0
LINE (ph% + 2, pvv% + 40)-(ph% + 5, pvv% + 48), 0
LINE (ph% - 3, pvv% + 40)-(ph% - 6, pvv% + 47.75), 0
LINE (ph% + 5, pvv% + 48)-(ph% + 2, pvv% + 48), 0
LINE (ph% - 6, pvv% + 48)-(ph% - 6, pvv% + 48), 3
LINE (ph% - 5, pvv% + 48)-(ph% - 2, pvv% + 48), 0
LINE (ph% - 3, pvv% + 48)-(ph% - 1, pvv% + 45), 0
LINE (ph% + 2, pvv% + 48)-(ph%, pvv% + 44), 0
LINE (ph% + 2, pvv% + 48)-(ph% + 6, pvv% + 50), 0, BF
LINE (ph% - 2, pvv% + 48)-(ph% - 6, pvv% + 50), 0, BF
PAINT (ph% + 3, pvv% + 47), 1, 0
PAINT (ph%, pvv% + 38), 2, 0
PAINT (ph% + 1, pvv% + 31), 11, 0
PAINT (ph% + 5, pvv% + 35), 11, 0
END SUB

DEFINT A-Z
SUB drawguy1ducking
CIRCLE (ph% - 1, pvv% + 35), 4, 0
LINE (ph% - 4, pvv% + 38)-(ph% + 2, pvv% + 43), 0, B
LINE (ph% - 6, pvv% + 39)-(ph% + 4, pvv% + 39), 0
LINE (ph% + 5, pvv% + 40)-(ph% + 5, pvv% + 44), 0
LINE (ph% + 3, pvv% + 44)-(ph% + 4, pvv% + 44), 0
LINE (ph% + 3, pvv% + 44)-(ph% + 3, pvv% + 41), 0
LINE (ph% - 1, pvv% + 35)-(ph% - 1, pvv% + 35), 0
LINE (ph% + 1, pvv% + 35)-(ph% + 1, pvv% + 35), 0
LINE (ph% - 7, pvv% + 40)-(ph% - 7, pvv% + 44), 0
LINE (ph% - 6, pvv% + 44)-(ph% - 5, pvv% + 44), 0
LINE (ph% - 5, pvv% + 44)-(ph% - 5, pvv% + 41), 0
LINE (ph% - 5, pvv% + 41)-(ph% - 4, pvv% + 41), 0
LINE (ph% + 5, pvv% + 45)-(ph% + 3, pvv% + 48), 0
LINE (ph% + 5, pvv% + 45)-(ph% + 3, pvv% + 43), 0
LINE (ph% - 7, pvv% + 45)-(ph% - 5, pvv% + 48), 0
LINE (ph% - 7, pvv% + 45)-(ph% - 5, pvv% + 43), 0
LINE (ph% - 2, pvv% + 48)-(ph% - 3, pvv% + 45), 0
LINE (ph% + 0, pvv% + 47)-(ph% + 1, pvv% + 45), 0
LINE (ph% + 0, pvv% + 48)-(ph% + 4, pvv% + 50), 0, BF
LINE (ph% - 2, pvv% + 48)-(ph% - 6, pvv% + 50), 0, BF
LINE (ph% + 0, pvv% + 45)-(ph% - 2, pvv% + 45), 0, BF
PAINT (ph% + 3, pvv% + 47), 1, 0
PAINT (ph% - 1, pvv% + 42), 2, 0
PAINT (ph% - 1, pvv% + 36), 11, 0
PAINT (ph% + 4, pvv% + 41), 11, 0
PAINT (ph% - 6, pvv% + 41), 11, 0
END SUB

DEFSNG A-Z
SUB drawguy1kicking
CIRCLE (ph%, pvv% + 30), 4, 0
LINE (ph% - 3, pvv% + 33)-(ph% + 2, pvv% + 40), 0, B
LINE (ph% - 6, pvv% + 34)-(ph% + 5, pvv% + 34), 0
LINE (ph% + 6, pvv% + 35)-(ph% + 6, pvv% + 39), 0
LINE (ph% + 4, pvv% + 40)-(ph% + 5, pvv% + 40), 0
LINE (ph% + 4, pvv% + 39)-(ph% + 4, pvv% + 36), 0
LINE (ph% + 3, pvv% + 36)-(ph% + 3, pvv% + 36), 0
LINE (ph%, pvv% + 30)-(ph%, pvv% + 30), 0
LINE (ph% + 2, pvv% + 30)-(ph% + 2, pvv% + 30), 0
LINE (ph% - 2, pvv% + 27)-(ph% + 2, pvv% + 27), 0
LINE (ph% - 7, pvv% + 35)-(ph% - 7, pvv% + 39), 0
LINE (ph% - 6, pvv% + 40)-(ph% - 5, pvv% + 40), 0
LINE (ph% - 5, pvv% + 40)-(ph% - 5, pvv% + 36), 0
LINE (ph% - 5, pvv% + 36)-(ph% - 4, pvv% + 36), 0
LINE (ph% - 3, pvv% + 40)-(ph% - 6, pvv% + 47.75), 0
LINE (ph% - 3, pvv% + 48)-(ph% - 1, pvv% + 45), 0
LINE (ph%, pvv% + 44)-(ph%, pvv% + 44), 0
LINE (ph% - 2, pvv% + 48)-(ph% - 6, pvv% + 50), 0, BF
LINE (ph% + 5, pvv% + 44)-(ph% + 7, pvv% + 40), 0
LINE (ph% + 7, pvv% + 40)-(ph% + 9, pvv% + 40), 0
LINE (ph% + 9, pvv% + 40)-(ph% + 8, pvv% + 44), 0
LINE (ph% + 8, pvv% + 44)-(ph% + 6, pvv% + 44), 0
PAINT (ph% + 7, pvv% + 43), 0, 0
LINE (ph% + 7, pvv% + 41)-(ph% + 2, pvv% + 41), 0
LINE (ph%, pvv% + 44)-(ph% + 5, pvv% + 44), 0
PAINT (ph% + 3, pvv% + 43), 1, 0
PAINT (ph%, pvv% + 38), 2, 0
PAINT (ph% + 1, pvv% + 31), 11, 0
PAINT (ph% + 5, pvv% + 36), 11, 0
PAINT (ph% - 6, pvv% + 36), 11, 0
soundeffect "KICK"
END SUB

SUB drawguy1knee
ph% = ph% + 5
pvv% = pvv% - 7'2
CIRCLE (ph% - 2, pvv% + 35), 4, 0
LINE (ph% - 4, pvv% + 38)-(ph% + 1, pvv% + 43), 0, B
LINE (ph% - 6, pvv% + 39)-(ph% + 1, pvv% + 39), 0
LINE (ph% + 1, pvv% + 43)-(ph% + 5, pvv% + 43), 0 'here we go<> | <>
LINE (ph% - 2, pvv% + 35)-(ph% - 2, pvv% + 35), 0 'eye
LINE (ph% + 0, pvv% + 35)-(ph% + 0, pvv% + 35), 0 'eye
LINE (ph% - 7, pvv% + 40)-(ph% - 7, pvv% + 44), 0  'l.a'
LINE (ph% - 5, pvv% + 44)-(ph% - 5, pvv% + 41), 0  'l.a
LINE (ph% + 5, pvv% + 44)-(ph% + 4, pvv% + 48), 0 'part of right leg
PSET (ph% - 6, pvv% + 45), 0
LINE (ph% - 5, pvv% + 45)-(ph% - 7, pvv% + 48), 0 'part of left leg
LINE (ph% - 5, pvv% + 50)-(ph% - 0, pvv% + 45), 0 'left leg
LINE (ph% + 2, pvv% + 45)-(ph% + 2, pvv% + 47), 0
PSET (ph% + 1, pvv% + 45), 0
LINE (ph% - 0, pvv% + 48)-(ph% + 2, pvv% + 47), 0
LINE (ph% + 0, pvv% + 49)-(ph% + 3, pvv% + 50), 0, BF 'right foot
PSET (ph% - 4, pvv% + 49), 0
PSET (ph% - 7, pvv% + 49), 0
LINE (ph% - 4, pvv% + 50)-(ph% - 7, pvv% + 51), 0, BF'left foot
LINE (ph% + 0, pvv% + 45)-(ph% - 1, pvv% + 45), 0
PAINT (ph% + 3, pvv% + 47), 1, 0
PAINT (ph% - 1, pvv% + 42), 2, 0
PAINT (ph% - 1, pvv% + 36), 11, 0
PAINT (ph% - 6, pvv% + 41), 11, 0
soundeffect "PUNCH"
delay 32766
pvv% = pvv% + 7'2
ph% = ph% + 5
LINE (ph% - 14, gl% + 7)-(ph% + 23, gl% - 18), 9, BF:
END SUB

SUB drawguy1plain
CIRCLE (ph%, pvv% + 30), 4, 0
LINE (ph% - 3, pvv% + 33)-(ph% + 2, pvv% + 40), 0, B
LINE (ph% - 6, pvv% + 34)-(ph% + 5, pvv% + 34), 0
LINE (ph% + 6, pvv% + 35)-(ph% + 6, pvv% + 39), 0
LINE (ph% + 4, pvv% + 40)-(ph% + 5, pvv% + 40), 0
LINE (ph% + 4, pvv% + 39)-(ph% + 4, pvv% + 36), 0
LINE (ph% + 3, pvv% + 36)-(ph% + 3, pvv% + 36), 0
LINE (ph%, pvv% + 30)-(ph%, pvv% + 30), 0
LINE (ph% + 2, pvv% + 30)-(ph% + 2, pvv% + 30), 0
LINE (ph% - 2, pvv% + 27)-(ph% + 2, pvv% + 27), 0
LINE (ph% - 7, pvv% + 35)-(ph% - 7, pvv% + 39), 0
LINE (ph% - 6, pvv% + 40)-(ph% - 5, pvv% + 40), 0
LINE (ph% - 5, pvv% + 40)-(ph% - 5, pvv% + 36), 0
LINE (ph% - 5, pvv% + 36)-(ph% - 4, pvv% + 36), 0
LINE (ph% + 2, pvv% + 40)-(ph% + 5, pvv% + 48), 0
LINE (ph% - 3, pvv% + 40)-(ph% - 6, pvv% + 47.75), 0
LINE (ph% + 5, pvv% + 48)-(ph% + 2, pvv% + 48), 0
LINE (ph% - 6, pvv% + 48)-(ph% - 6, pvv% + 48), 3
LINE (ph% - 5, pvv% + 48)-(ph% - 2, pvv% + 48), 0
LINE (ph% - 3, pvv% + 48)-(ph% - 1, pvv% + 45), 0
LINE (ph% + 2, pvv% + 48)-(ph%, pvv% + 44), 0
LINE (ph% + 2, pvv% + 48)-(ph% + 6, pvv% + 50), 0, BF
LINE (ph% - 2, pvv% + 48)-(ph% - 6, pvv% + 50), 0, BF
PAINT (ph% + 3, pvv% + 47), 1, 0
PAINT (ph%, pvv% + 38), 2, 0
PAINT (ph% + 1, pvv% + 31), 11, 0
PAINT (ph% + 5, pvv% + 36), 11, 0
PAINT (ph% - 6, pvv% + 36), 11, 0
END SUB

SUB drawguy1punching
CIRCLE (ph%, pvv% + 30), 4, 0
LINE (ph% - 3, pvv% + 33)-(ph% + 2, pvv% + 40), 0, B
LINE (ph% - 6, pvv% + 34)-(ph% + 10, pvv% + 34), 0
LINE (ph% + 11, pvv% + 35)-(ph% + 11, pvv% + 36), 0
LINE (ph% + 2, pvv% + 36)-(ph% + 10, pvv% + 36), 0   '13
LINE (ph% + 3, pvv% + 35)-(ph% + 10, pvv% + 35), 15
LINE (ph%, pvv% + 30)-(ph%, pvv% + 30), 0
LINE (ph% + 2, pvv% + 30)-(ph% + 2, pvv% + 30), 0
LINE (ph% - 2, pvv% + 27)-(ph% + 2, pvv% + 27), 0
LINE (ph% - 7, pvv% + 35)-(ph% - 7, pvv% + 39), 0
LINE (ph% - 6, pvv% + 40)-(ph% - 5, pvv% + 40), 0
LINE (ph% - 5, pvv% + 40)-(ph% - 5, pvv% + 36), 0
LINE (ph% - 5, pvv% + 36)-(ph% - 4, pvv% + 36), 0
LINE (ph% + 2, pvv% + 40)-(ph% + 5, pvv% + 48), 0
LINE (ph% - 3, pvv% + 40)-(ph% - 6, pvv% + 47.75), 0
LINE (ph% + 5, pvv% + 48)-(ph% + 2, pvv% + 48), 0
LINE (ph% - 6, pvv% + 48)-(ph% - 6, pvv% + 48), 3
LINE (ph% - 5, pvv% + 48)-(ph% - 2, pvv% + 48), 0
LINE (ph% - 3, pvv% + 48)-(ph% - 1, pvv% + 45), 0
LINE (ph% + 2, pvv% + 48)-(ph%, pvv% + 44), 0
LINE (ph% + 2, pvv% + 48)-(ph% + 6, pvv% + 50), 0, BF
LINE (ph% - 2, pvv% + 48)-(ph% - 6, pvv% + 50), 0, BF
PAINT (ph% + 3, pvv% + 47), 1, 0
PAINT (ph%, pvv% + 38), 2, 0
PAINT (ph% + 1, pvv% + 31), 11, 0
PAINT (ph% + 5, pvv% + 36), 11, 0
PAINT (ph% - 6, pvv% + 36), 11, 0
soundeffect "PUNCH"
END SUB

SUB drawguy1stick
LINE (ph% - 7, pv% - 15)-(ph% + 23, pv% + 7), 9, BF:
CIRCLE (ph%, pvv% + 30), 4, 0
LINE (ph% - 3, pvv% + 33)-(ph% + 2, pvv% + 40), 0, B
LINE (ph% - 6, pvv% + 34)-(ph% + 10, pvv% + 34), 0
LINE (ph% + 11, pvv% + 35)-(ph% + 11, pvv% + 36), 0
LINE (ph% + 2, pvv% + 36)-(ph% + 10, pvv% + 36), 0   '13
LINE (ph% + 3, pvv% + 35)-(ph% + 10, pvv% + 35), 15
LINE (ph%, pvv% + 30)-(ph%, pvv% + 30), 0
LINE (ph% + 2, pvv% + 30)-(ph% + 2, pvv% + 30), 0
LINE (ph% - 2, pvv% + 27)-(ph% + 2, pvv% + 27), 0
LINE (ph% - 7, pvv% + 35)-(ph% - 7, pvv% + 39), 0
LINE (ph% - 6, pvv% + 40)-(ph% - 5, pvv% + 40), 0
LINE (ph% - 5, pvv% + 40)-(ph% - 5, pvv% + 36), 0
LINE (ph% - 5, pvv% + 36)-(ph% - 4, pvv% + 36), 0
LINE (ph% + 2, pvv% + 40)-(ph% + 5, pvv% + 48), 0
LINE (ph% - 3, pvv% + 40)-(ph% - 6, pvv% + 47.75), 0
LINE (ph% + 5, pvv% + 48)-(ph% + 2, pvv% + 48), 0
LINE (ph% - 6, pvv% + 48)-(ph% - 6, pvv% + 48), 3
LINE (ph% - 5, pvv% + 48)-(ph% - 2, pvv% + 48), 0
LINE (ph% - 3, pvv% + 48)-(ph% - 1, pvv% + 45), 0
LINE (ph% + 2, pvv% + 48)-(ph%, pvv% + 44), 0
LINE (ph% + 2, pvv% + 48)-(ph% + 6, pvv% + 50), 0, BF
LINE (ph% - 2, pvv% + 48)-(ph% - 6, pvv% + 50), 0, BF
LINE (ph% + 11, pvv% + 34)-(ph% + 23, pvv% + 36), 0, B
LINE (ph% + 12, pvv% + 35)-(ph% + 22, pvv% + 35), 6
PAINT (ph% + 3, pvv% + 47), 1, 0
PAINT (ph%, pvv% + 38), 2, 0
PAINT (ph% + 1, pvv% + 31), 11, 0
PAINT (ph% + 5, pvv% + 36), 11, 0
PAINT (ph% - 6, pvv% + 36), 11, 0
soundeffect "STICK"
END SUB

DEFINT A-Z
SUB drawguy2ducking
pvvv% = pvv%
CIRCLE (ph%, pvvv% + 3), 7, 0
LINE (ph% - 2, pvvv% + 3)-(ph% - 2, pvvv% + 2), 0
LINE (ph% + 2, pvvv% + 3)-(ph% + 2, pvvv% + 2), 0
LINE (ph% - 3, pvvv% + 5)-(ph% + 1, pvvv% + 6), 0
LINE (ph% + 2, pvvv% + 6)-(ph% + 3, pvvv% + 5), 0
PAINT (ph%, pvv% + 3), coly%, 0
LINE (ph% - 6, pvvv% + 9)-(ph% + 6, pvvv% + 9), 0
LINE (ph% - 6, pvvv% + 9)-(ph% - 4, pvvv% + 8), 0, BF
LINE (ph% + 6, pvvv% + 9)-(ph% + 4, pvvv% + 8), 0, BF
LINE (ph% + 8, pvvv% + 3)-(ph% + 10, pvvv% + 8), 0, B
PSET (ph% + 9, pvvv% + 2), 0
LINE (ph% + 9, pvvv% + 3)-(ph% + 9, pvvv% + 6), 12
PSET (ph% + 8, pvvv% + 2), 0
LINE (ph% + 8, pvvv% + 3)-(ph% + 8, pvvv% + 3), 12
LINE (ph% - 8, pvvv% + 3)-(ph% - 10, pvvv% + 8), 0, B
PSET (ph% - 9, pvvv% + 5), 0
LINE (ph% - 9, pvvv% + 3)-(ph% - 9, pvvv% + 6), 12
LINE (ph% - 9, pvvv% + 2)-(ph% - 7, pvvv% + 2), 0
PSET (ph% - 8, pvvv% + 5), 0
LINE (ph% - 8, pvvv% + 3)-(ph% - 8, pvvv% + 3), 12
PSET (ph% + 9, pvvv% + 7), 0
PSET (ph% - 9, pvvv% + 7), 0
END SUB

DEFSNG A-Z
SUB drawguy2kicking
CIRCLE (ph%, pvv%), 7, 0
LINE (ph% - 2, pvv%)-(ph% - 2, pvv% - 1), 0
LINE (ph% + 2, pvv%)-(ph% + 2, pvv% - 1), 0
LINE (ph% - 3, pvv% + 2)-(ph% + 1, pvv% + 3), 0
LINE (ph% + 2, pvv% + 3)-(ph% + 3, pvv% + 2), 0
PAINT (ph%, pvv%), coly%, 0
LINE (ph% + 5, pvv% + 5)-(ph% + 10, pvv% + 7), 0
LINE (ph% + 1, pvv% + 6)-(ph% + 10, pvv% + 9), 0
LINE (ph% - 3, pvv% + 5)-(ph% - 5, pvv% + 8), 0, B
PAINT (ph% - 4, pvv% + 6), 12, 0
LINE (ph% + 10, pvv% + 6)-(ph% + 12, pvv% + 9), 0, BF
LINE (ph% - 6, pvv% + 8)-(ph% - 3, pvv% + 9), 0, BF
PSET (ph% + 10, pvv% + 6), 9
PAINT (ph% + 4, pvv% + 6), 12, 0
LINE (ph% + 8, pvv%)-(ph% + 10, pvv% + 3), 0, B
PSET (ph% + 9, pvv% - 1), 0
LINE (ph% + 9, pvv%)-(ph% + 9, pvv% + 2), 12
PSET (ph% + 8, pvv% - 1), 0
LINE (ph% + 8, pvv%)-(ph% + 8, pvv%), 12
LINE (ph% - 8, pvv%)-(ph% - 10, pvv% + 5), 0, B
PSET (ph% - 9, pvv% - 1), 0
LINE (ph% - 9, pvv%)-(ph% - 9, pvv% + 3), 12
PSET (ph% - 8, pvv% - 1), 0
LINE (ph% - 8, pvv%)-(ph% - 8, pvv%), 12
PSET (ph% + 9, pvv% + 2), 0
PSET (ph% - 9, pvv% + 4), 0
soundeffect "KICK"
END SUB

SUB drawguy2plain
CIRCLE (ph%, pvv%), 7, 0
LINE (ph% - 2, pvv%)-(ph% - 2, pvv% - 1), 0
LINE (ph% + 2, pvv%)-(ph% + 2, pvv% - 1), 0
LINE (ph% - 3, pvv% + 2)-(ph% + 1, pvv% + 3), 0
LINE (ph% + 2, pvv% + 3)-(ph% + 3, pvv% + 2), 0
PAINT (ph%, pvv%), coly%, 0
LINE (ph% + 3, pvv% + 5)-(ph% + 5, pvv% + 8), 0, B
LINE (ph% - 3, pvv% + 5)-(ph% - 5, pvv% + 8), 0, B
PAINT (ph% + 4, pvv% + 6), 12, 0
PAINT (ph% - 4, pvv% + 6), 12, 0
LINE (ph% + 6, pvv% + 8)-(ph% + 3, pvv% + 9), 0, BF
LINE (ph% - 6, pvv% + 8)-(ph% - 3, pvv% + 9), 0, BF
LINE (ph% + 8, pvv%)-(ph% + 10, pvv% + 5), 0, B
PSET (ph% + 9, pvv% - 1), 0
LINE (ph% + 9, pvv%)-(ph% + 9, pvv% + 3), 12
PSET (ph% + 8, pvv% - 1), 0
LINE (ph% + 8, pvv%)-(ph% + 8, pvv%), 12
LINE (ph% - 8, pvv%)-(ph% - 10, pvv% + 5), 0, B
PSET (ph% - 9, pvv% - 1), 0
LINE (ph% - 9, pvv%)-(ph% - 9, pvv% + 3), 12
PSET (ph% - 8, pvv% - 1), 0
LINE (ph% - 8, pvv%)-(ph% - 8, pvv%), 12
PSET (ph% + 9, pvv% + 4), 0
PSET (ph% - 9, pvv% + 4), 0
END SUB

SUB drawguy2punching
CIRCLE (ph%, pvv%), 7, 0
LINE (ph% - 2, pvv%)-(ph% - 2, pvv% - 1), 0
LINE (ph% + 2, pvv%)-(ph% + 2, pvv% - 1), 0
LINE (ph% - 3, pvv% + 2)-(ph% + 1, pvv% + 3), 0
LINE (ph% + 2, pvv% + 3)-(ph% + 3, pvv% + 2), 0
PAINT (ph%, pvv%), coly%, 0
LINE (ph% + 3, pvv% + 5)-(ph% + 5, pvv% + 8), 0, B
LINE (ph% - 3, pvv% + 5)-(ph% - 5, pvv% + 8), 0, B
PAINT (ph% + 4, pvv% + 6), 12, 0
PAINT (ph% - 4, pvv% + 6), 12, 0
LINE (ph% + 6, pvv% + 8)-(ph% + 3, pvv% + 9), 0, BF
LINE (ph% - 6, pvv% + 8)-(ph% - 3, pvv% + 9), 0, BF
LINE (ph% + 7, pvv%)-(ph% + 12, pvv% + 2), 0, B
LINE (ph% + 13, pvv%)-(ph% + 13, pvv% + 2), 0
LINE (ph% + 8, pvv% + 1)-(ph% + 11, pvv% + 1), 12
LINE (ph% - 8, pvv%)-(ph% - 10, pvv% + 5), 0, B
PSET (ph% - 9, pvv% - 1), 0
LINE (ph% - 9, pvv%)-(ph% - 9, pvv% + 3), 12
PSET (ph% - 8, pvv% - 1), 0
LINE (ph% - 8, pvv%)-(ph% - 8, pvv%), 12
PSET (ph% - 9, pvv% + 4), 0
soundeffect "PUNCH"
END SUB

SUB drawguy2stretch
CIRCLE (ph%, pvv%), 7, 0
LINE (ph% - 2, pvv%)-(ph% - 2, pvv% - 1), 0
LINE (ph% + 2, pvv%)-(ph% + 2, pvv% - 1), 0
LINE (ph% - 3, pvv% + 2)-(ph% + 1, pvv% + 3), 0
LINE (ph% + 2, pvv% + 3)-(ph% + 3, pvv% + 2), 0
PAINT (ph%, pvv%), coly%, 0
LINE (ph% + 3, pvv% + 5)-(ph% + 5, pvv% + 8), 0, B
LINE (ph% - 3, pvv% + 5)-(ph% - 5, pvv% + 8), 0, B
PAINT (ph% + 4, pvv% + 6), 12, 0
PAINT (ph% - 4, pvv% + 6), 12, 0
LINE (ph% + 6, pvv% + 8)-(ph% + 3, pvv% + 9), 0, BF
LINE (ph% - 6, pvv% + 8)-(ph% - 3, pvv% + 9), 0, BF
LINE (ph% + 7, pvv%)-(ph% + 22, pvv% + 2), 0, B
LINE (ph% + 23, pvv%)-(ph% + 23, pvv% + 2), 0
LINE (ph% + 8, pvv% + 1)-(ph% + 21, pvv% + 1), 12
LINE (ph% - 8, pvv%)-(ph% - 10, pvv% + 5), 0, B
PSET (ph% - 9, pvv% - 1), 0
LINE (ph% - 9, pvv%)-(ph% - 9, pvv% + 3), 12
PSET (ph% - 8, pvv% - 1), 0
LINE (ph% - 8, pvv%)-(ph% - 8, pvv%), 12
PSET (ph% - 9, pvv% + 4), 0
soundeffect "STICK"
END SUB

SUB drawguy2victory
CIRCLE (ph%, pvv%), 7, 0
LINE (ph% - 2, pvv%)-(ph% - 2, pvv% - 1), 0
LINE (ph% + 2, pvv%)-(ph% + 2, pvv% - 1), 0
LINE (ph% - 3, pvv% + 2)-(ph% + 1, pvv% + 3), 0
LINE (ph% + 2, pvv% + 3)-(ph% + 3, pvv% + 2), 0
PAINT (ph%, pvv%), coly%, 0
LINE (ph% + 3, pvv% + 5)-(ph% + 5, pvv% + 8), 0, B
LINE (ph% - 3, pvv% + 5)-(ph% - 5, pvv% + 8), 0, B
PAINT (ph% + 4, pvv% + 6), 12, 0
PAINT (ph% - 4, pvv% + 6), 12, 0
LINE (ph% + 6, pvv% + 8)-(ph% + 3, pvv% + 9), 0, BF
LINE (ph% - 6, pvv% + 8)-(ph% - 3, pvv% + 9), 0, BF
LINE (ph% + 8, pvv%)-(ph% + 10, pvv% - 5), 0, B
PSET (ph% + 9, pvv% - 1), 0
LINE (ph% + 9, pvv%)-(ph% + 9, pvv% - 3), 12
PSET (ph% + 8, pvv% - 1), 0
LINE (ph% + 9, pvv% + 1)-(ph% + 7, pvv% + 1), 0
LINE (ph% + 8, pvv%)-(ph% + 8, pvv%), 12
LINE (ph% - 8, pvv%)-(ph% - 10, pvv% + 5), 0, B
PSET (ph% - 9, pvv% - 1), 0
LINE (ph% - 9, pvv%)-(ph% - 9, pvv% + 3), 12
PSET (ph% - 8, pvv% - 1), 0
LINE (ph% - 8, pvv%)-(ph% - 8, pvv%), 12
PSET (ph% + 9, pvv% - 4), 0
PSET (ph% - 9, pvv% + 4), 0
END SUB

SUB ending
CALL punchoffledge
CALL enemyfall
CALL enemyfall
CLS : SCREEN 13:
col% = 25: backcol% = 9
backgroundpit
delay 32766: delay 32766: delay 32766
FOR ty% = 40 TO 280 STEP 8    '25
cone ty%, 170, 15, 3
FOR ty2% = 50 TO 100 STEP 1
IF sounds$ = "NO" THEN delay 10: GOTO 58
IF soundcard$ = "YES" THEN note ty2% + 17500, 5
IF soundcard$ = "NO" THEN SOUND ty2%, .025
IF soundcard$ = "NO" THEN SOUND 32766, .025
58 NEXT ty2%
NEXT ty%
eh% = 160: evv% = -50
col1% = 0: col2% = 0: col3% = 0
DO: CALL drawenemyfallen
delay 2500
LINE (155, evv% + 39)-(186, evv% + 51), 9, BF
IF evv% < 110 THEN evv% = evv% + 1
IF evv% = 110 THEN EXIT DO
LOOP
FOR ty% = 160 TO 192 STEP 8
cone ty%, 170, 15, 3
CALL drawenemyfallen
NEXT ty%
CALL drawenemyfallen
FOR hy% = 1 TO 4
CIRCLE (eh% + 22, evv% + 47), hy%, 0
NEXT hy%
LINE (eh% + 9, evv% + 45)-(eh% + 0, evv% + 50), 0, BF
inspike$ = "YES"
LINE (eh% + 16, evv% + 43)-(eh% + 18, evv% + 39), 9, BF
566 bbv% = 7
RANDOMIZE TIMER
bdh% = INT(RND * 20)
567 LINE (eh% + bdh%, 160 - bbv%)-((eh% + bdh%) + 1, (160 - bbv%) - 1), 4, BF'PSET (eh% + bdh%, 160 - bbv%), 4
delay 5000:
LINE (eh% + bdh%, 160 - bbv%)-((eh% + bdh%) + 1, (160 - bbv%) - 1), 9, BF'PSET (eh% + bdh%, 160 - bbv%), 9
IF bbv% < 15 THEN bbv% = bbv% + 1: GOTO 567
IF timesy% < 20 THEN timesy% = timesy% + 1: GOTO 566
FOR tu% = 1 TO 10: delay 32766: NEXT tu%
IF charpick% = 1 THEN CALL guy1ending
IF charpick% = 2 THEN CALL guy2ending
END SUB

SUB endpicture
CLS : SCREEN 13: col% = 25: backcol% = 25
PAINT (1, 100), backcol%
LINE (0, 0)-(320, 80), 9, BF
cone 160, 125, 48, 20
cone 100, 100, 25, 10
cone 97, 170, 75, 30
cone 197, 190, 85, 35
ph% = 250: pvv% = 120: v% = 2
FOR twomore% = 1 TO 25
delay 32766: delay 32766
LINE (ph% - 10, pvv% - 15)-(ph% + 23, pvv% + 10), 25, BF:
IF v% = 1 THEN CALL drawguy2victory: v% = 2: GOTO 8585
IF v% = 2 THEN CALL drawguy2plain: v% = 1
8585 NEXT twomore%
END SUB

SUB enemyfall
CLS : SCREEN 13:
col% = 25: backcol% = 9
eh% = 160: evv% = -50
col1% = 0: col2% = 0: col3% = 0
PAINT (1, 100), backcol%
LINE (35, 0)-(35, 200), 0
LINE (285, 0)-(285, 200), 0
LINE (0, 0)-(34, 200), 8, BF
LINE (320, 0)-(286, 200), 8, BF
DO: CALL drawenemyfallen
delay 2500
LINE (155, evv% + 39)-(186, evv% + 51), 9, BF
IF evv% < 160 THEN evv% = evv% + 1
IF evv% = 160 THEN EXIT DO
LOOP
END SUB

SUB enemypattern
IF practicemode$ = "YES" THEN eh% = 325: EXIT SUB
IF hittimer% = 0 THEN combohits% = 1: combodamage% = 0
IF combohits% > 1 THEN
IF COMBOTIMER% > 0 THEN COMBOTIMER% = COMBOTIMER% - 1
END IF
IF fireball$ = "YES" AND enemyposition$ <> "BLOCKING" THEN LINE (eh% - 7, evv% + 24)-(eh% + 7, gl% + 7), 9, BF
IF bg% = 1 THEN CALL background1: IF bg% = 2 THEN CALL background2
IF fireball$ = "YES" AND enemyposition$ <> "BLOCKING" THEN LINE (eh% - 7, evv% + 24)-(eh% + 7, gl% + 7), 9, BF
79 SELECT CASE enemyposition$
CASE IS = "PLAIN"
CALL drawenemyplain
CASE IS = "KICKING"
LINE (eh% - 7, evv% + 24)-(eh% + 7, gl% + 7), 9, BF
CALL drawenemykicking
CASE IS = "BLOCKING"
CALL drawenemyblock
END SELECT
IF enemyposition$ = "KICKING" THEN
IF eh% - 20 < ph% THEN
soundeffect "DING": coly% = 10
IF blocking$ <> "YES" THEN damage% = 5
IF blocking$ = "YES" THEN LINE (ph% - 7, gl% - 15)-(ph% + 18, gl% + 7), 9, BF: damage% = 1
damage% = fightnum% + difficulty%
IF blocking$ = "YES" THEN LINE (ph% - 7, gl% - 15)-(ph% + 18, gl% + 7), 9, BF: damage% = 1
IF fightnum% = 11 THEN
IF blocking$ <> "YES" THEN damage% = difficulty% + 16
IF blocking$ = "YES" THEN LINE (ph% - 7, gl% - 15)-(ph% + 18, gl% + 7), 9, BF: damage% = difficulty% + 6
END IF
life% = life% - damage%: LINE (ph% - 7, gl% - 10)-(ph% + 13, gl% + 7), 9, BF: ph% = ph% - damage%'5
END IF
END IF
IF fireball$ <> "YES" AND enemyposition$ = "BLOCKING" THEN LINE (eh% - 7, evv% + 24)-(eh% + 7, gl% + 7), 9, BF
IF fireball$ <> "YES" THEN enemyposition$ = "PLAIN"
IF guy1position$ = "PUNCHING" OR guy1position$ = "KICKING" THEN
IF yesjump% = 2 THEN damage% = 1
IF yesjump% <> 2 THEN damage% = 5
IF coly% = 4 THEN damage% = 8
IF combohits% > 4 THEN damage% = 4
IF combohits% > 6 THEN damage% = 3
IF combohits% > 8 THEN damage% = 2
IF combohits% > 10 THEN damage% = 1
IF combohits% > 12 THEN damage% = 0
IF eh% - 20 < ph% THEN
combodamage% = combodamage% + damage%
IF hittimer% > 0 THEN combohits% = combohits% + 1: combodamage% = combodamage% + damage%: COMBOTIMER% = 250
soundeffect "DING": enemylife% = enemylife% - damage%: score% = score% + 10: hittimer% = 10: GOTO 78
END IF                                                                                  '25!
END IF
IF guy1position$ = "STICK" THEN
combodamage% = combodamage% + 10
IF hittimer% > 0 THEN combohits% = combohits% + 1: combodamage% = combodamage% + 10: COMBOTIMER% = 250                                                                         '25!
IF eh% - 30 < ph% THEN soundeffect "DING": enemylife% = enemylife% - 10: score% = score% + 40: LINE (eh% - 7, evv% + 24)-(eh% + 7, gl% + 7), 9, BF: eh% = eh% + 10: hittimer% = 0: LINE (ph% - 7, pv% - 15)-(ph% + 23, pv% + 7), 4, BF: CALL fightwithenemyfall: EXIT SUB
END IF
IF guy1position$ = "KNEE" THEN
combodamage% = combodamage% + 10
IF hittimer% > 0 THEN combohits% = combohits% + 1: combodamage% = combodamage% + 10: COMBOTIMER% = 250                                                                         '25!
IF eh% - 20 < ph% THEN soundeffect "DING": enemylife% = enemylife% - 14: score% = score% + 40: LINE (eh% - 7, evv% + 24)-(eh% + 7, gl% + 7), 9, BF: eh% = eh% + 10: hittimer% = 0: EXIT SUB
END IF
IF fireball$ = "YES" AND enemyposition$ <> "BLOCKING" THEN LINE (eh% - 7, evv% + 24)-(eh% + 7, gl% + 7), 9, BF
IF fireball$ = "YES" THEN
enemyposition$ = "BLOCKING"
END IF
IF fireball$ = "YES" AND enemyposition$ <> "BLOCKING" THEN LINE (eh% - 7, evv% + 24)-(eh% + 7, gl% + 7), 9, BF
IF enemyposition$ = "BLOCKING" THEN EXIT SUB
IF eh% - 50 < ph% THEN GOTO 87
IF eh% - 100 < ph% THEN
RANDOMIZE TIMER
echoice1% = INT(RND * 99) + 1
IF echoice1% < 10 THEN LINE (eh% - 7, evv% + 24)-(eh% + 7, gl% + 7), 9, BF: eh% = eh% - 5
IF echoice1% = 11 THEN
78 LINE (eh% - 7, evv% + 24)-(eh% + 7, gl% + 7), 9, BF: eh% = eh% + 5
END IF
EXIT SUB
END IF
RANDOMIZE TIMER
echoice1% = INT(RND * 49) + 1
IF echoice1% < 10 THEN LINE (eh% - 7, evv% + 24)-(eh% + 7, gl% + 7), 9, BF: eh% = eh% - 5
IF echoice1% = 11 THEN LINE (eh% - 7, evv% + 24)-(eh% + 7, gl% + 7), 9, BF: eh% = eh% + 5
EXIT SUB
87
IF eh% - 20 < ph% THEN
RANDOMIZE TIMER  '49 = how often computer kicks (lower = more often)
echoice1% = INT(RND * 24) + 1
IF echoice1% = 1 THEN enemyposition$ = "KICKING": GOTO 79
END IF
IF NOT (eh% - 20 < ph%) THEN
itit% = 50 - fightnum%
RANDOMIZE TIMER
echoice1% = INT(RND * itit%) + 1
IF echoice1% < 10 THEN LINE (eh% - 7, evv% + 24)-(eh% + 7, gl% + 7), 9, BF: eh% = eh% - 5
IF echoice1% = 11 THEN LINE (eh% - 7, evv% + 24)-(eh% + 7, gl% + 7), 9, BF: eh% = eh% + 5
END IF
END SUB

DEFINT A-Z
SUB exitprogram
IF score% <= 32766 THEN rating$ = "ULTRA-MEGA-SUPREME DEATH FIGHT CHAMPION!"
IF score% <= 10000 THEN rating$ = "champion!!!"
IF score% <= 9000 THEN rating$ = "you're great at this!"
IF score% <= 8000 THEN rating$ = "wow... you're good!"
IF score% <= 7000 THEN rating$ = "great!"
IF score% <= 6000 THEN rating$ = "good."
IF score% <= 5000 THEN rating$ = "pretty good."
IF score% <= 4000 THEN rating$ = "not bad."
IF score% <= 3000 THEN rating$ = "beginner."
IF score% <= 2000 THEN rating$ = "keep trying."
IF score% <= 1000 THEN rating$ = "loser!"
IF score% <= 500 THEN rating$ = "ULTRA LOSER!"
CLS : LOCATE 1, 2: COLOR 10: PRINT "You won"; fightwins%; "fights and had a score of"
LOCATE 2, 1: PRINT score%; "points."
LOCATE 4, 2: PRINT "You got a rating of..."
COLOR 12: LOCATE 4, 25: PRINT rating$
FOR timestimes% = 1 TO 25
delay 32766
NEXT timestimes%
IF score% > highscore% THEN CALL gethighscore
CALL puthighscore
CLS : SCREEN 13: PAINT (1, 1), 15
FOR HEIGHT% = 210 TO 100 STEP -1
font 110, HEIGHT%, "T", 0
font 120, HEIGHT%, "H", 0
font 130, HEIGHT%, "A", 0
font 140, HEIGHT%, "N", 0
font 150, HEIGHT%, "K", 0
font 160, HEIGHT%, "S", 0
font 180, HEIGHT%, "F", 0
font 190, HEIGHT%, "O", 0
font 200, HEIGHT%, "R", 0
font 120, HEIGHT% + 10, "P", 0
font 130, HEIGHT% + 10, "L", 0
font 140, HEIGHT% + 10, "A", 0
font 150, HEIGHT% + 10, "Y", 0
font 160, HEIGHT% + 10, "I", 0
font 170, HEIGHT% + 10, "N", 0
font 180, HEIGHT% + 10, "G", 0
delay 4000
font 110, HEIGHT%, "T", 15
font 120, HEIGHT%, "H", 15
font 130, HEIGHT%, "A", 15
font 140, HEIGHT%, "N", 15
font 150, HEIGHT%, "K", 15
font 160, HEIGHT%, "S", 15
font 180, HEIGHT%, "F", 15
font 190, HEIGHT%, "O", 15
font 200, HEIGHT%, "R", 15
font 120, HEIGHT% + 10, "P", 15
font 130, HEIGHT% + 10, "L", 15
font 140, HEIGHT% + 10, "A", 15
font 150, HEIGHT% + 10, "Y", 15
font 160, HEIGHT% + 10, "I", 15
font 170, HEIGHT% + 10, "N", 15
font 180, HEIGHT% + 10, "G", 15
NEXT HEIGHT%
font 110, HEIGHT%, "T", 0
font 120, HEIGHT%, "H", 0
font 130, HEIGHT%, "A", 0
font 140, HEIGHT%, "N", 0
font 150, HEIGHT%, "K", 0
font 160, HEIGHT%, "S", 0
font 180, HEIGHT%, "F", 0
font 190, HEIGHT%, "O", 0
font 200, HEIGHT%, "R", 0
font 120, HEIGHT% + 10, "P", 0
font 130, HEIGHT% + 10, "L", 0
font 140, HEIGHT% + 10, "A", 0
font 150, HEIGHT% + 10, "Y", 0
font 160, HEIGHT% + 10, "I", 0
font 170, HEIGHT% + 10, "N", 0
font 180, HEIGHT% + 10, "G", 0
FOR delay30% = 0 TO 10
delay 32766
NEXT delay30%
CLS : SCREEN 9
LOCATE 2, 2: COLOR 15: PRINT "Hey, you!  One more thing:"
LOCATE 4, 2: PRINT "There is another version of this game!  Get it now!"
LOCATE 5, 2: PRINT "It features 3 playable characers- the 2 original ones"
LOCATE 6, 2: PRINT "and a wizard who levitates and has 5 special moves."
LOCATE 7, 2: PRINT "Also added is a bonus game, different paths to take,"
LOCATE 8, 2: PRINT "different difficulty levels, a better ending, more of"
LOCATE 9, 2: PRINT "a challenge, and a cheat menu with lots of cheats,"
LOCATE 10, 2: PRINT "including some new ones not in previous versions."
LOCATE 11, 2: PRINT "To order this great game, turn your printer on now,"
LOCATE 12, 2: PRINT "and press any key.  An order form will automatically"
LOCATE 13, 2: PRINT "be printed.  Fill it out COMPLETELY, attach $, and"
LOCATE 14, 2: PRINT "send it to the address shown on the order form."
LOCATE 16, 2: PRINT "So turn on your printer, and wait..."
FOR delaytimere% = 1 TO 200
delay 32766
NEXT delaytimere%
LPRINT "DEATH FIGHT VERSION 3.00 ORDER FORM:"
LPRINT "---------------------------------------------------------"
LPRINT
LPRINT "  THE COST IS 3$, CASH ONLY, IF YOU HAVE AMERICA ONLINE."
LPRINT "  IF YOU DO NOT HAVE AMERICA ONLINE, THEN THE COST IS 5$."
LPRINT "  THE REASON FOR THIS IS BECAUSE IF YOU HAVE AMERICA"
LPRINT "  ONLINE, I WILL SEND THE FILE TO YOU THROUGH E-MAIL.  IF"
LPRINT "  YOU DON'T HAVE AMERICA ONLINE, OR FOR SOME REASON YOU"
LPRINT "  DON'T WANT TO GIVE ME YOUR SCREEN NAME, THEN I MUST"
LPRINT "  SEND IT TO YOU THROUGH THE MAIL, AND I WILL HAVE TO PAY"
LPRINT "  FOR THE DISK AND MAIL COST, MAKING YOUR COST MORE."
LPRINT "  WHEN YOU HAVE COMPLETED THIS FORM, SEND IT TO:"
LPRINT "  NOAH DALEO,   546 DIAMOND ROAD,   JACKSON, NJ  08527"
LPRINT "  NAME (OPTIONAL)  ______________________________________"
LPRINT "  AOL SCREEN NAME  ______________________________________"
LPRINT "  COMMENTS QUESTIONS, OR SUGGESTIONS"
LPRINT "  _______________________________________________________"
LPRINT "  _______________________________________________________"
LPRINT "  _______________________________________________________"
LPRINT "  _______________________________________________________"
LPRINT "  _______________________________________________________"
LPRINT "  IF YOU DON'T HAVE AOL, WRITE YOUR ADDRESS HERE"
LPRINT "  _______________________________________________________"
LPRINT "  _______________________________________________________"
LPRINT "  _______________________________________________________"
LPRINT "  IF YOU DON'T FILL OUT THE NEEDED INFO PROPERLY,"
LPRINT "  (IT'S MAINLY JUST THE SCREEN NAME THAT'S IMPORTANT), OR"
LPRINT "  FORGET TO INCLUDE THE MONEY, YOU WILL NOT GET THE GAME."
CLS : COLOR 0: SYSTEM
END SUB

DEFSNG A-Z
SUB fight (fightnumber%)
CLS : SLEEP 1
LOCATE 13, 15: COLOR 12: PRINT "FIGHT"; fightnumber%
IF fightnumber% = 11 THEN
col1% = 0: col2% = 0: col3% = 0: SLEEP 1
LOCATE 15, 12: COLOR 4: PRINT "Final fight:  this is the final face-off    with Evan Recneps!  Can you win?"
SLEEP 1: SLEEP 1: song1
END IF
SLEEP 1: SLEEP 1: SLEEP 1
CLS : SLEEP 1
END SUB

SUB fightstartup
CLS : SCREEN 13
LINE (0, 160)-(320, 200), 0, BF:
guy1position$ = "PLAIN"
life% = 0
col% = 4
coly% = 10
enemylife% = 0
gl% = 125
yesjump% = 1
yeshot% = 1
eh% = 240
ev% = gl% - 83
ph% = 100
time% = 4900
pv% = gl%
IF charpick% = 1 THEN pvv% = pv% - 42
'evv% = pvv%
evv% = pv% - 42
LINE (0, 160)-(320, 200), 0, BF:
IF bg% = 1 THEN CALL background1: LINE (0, 160)-(320, 200), 0, BF:
IF bg% = 2 THEN
CALL background2
FOR lk% = 0 TO -35 STEP -1
LINE (lk%, gl% - 100)-(lk% + 35, gl% - 125), 0
NEXT lk%
FOR lk2% = 320 TO 355 STEP 1
LINE (lk2%, gl% - 100)-(lk2% - 35, gl% - 125), 0
NEXT lk2%
FOR ww% = 30 TO 320 STEP 80
LINE (ww%, 40)-(ww% + 20, 57), 0, B
PAINT (ww% + 5, 50), 148, 0
NEXT ww%
END IF
IF bg% = 1 THEN LINE (0, 160)-(320, 200), 0, BF:
LINE (0, 160)-(320, 200), 0, BF:
43 CALL statusbar

'IF bg% = 1 THEN LINE (0, 160)-(320, 200), 0, BF:

'IF bg% = 2 THEN CALL background2
IF bg% = 2 THEN
'CALL background2
FOR lk% = 0 TO -35 STEP -1
LINE (lk%, gl% - 100)-(lk% + 35, gl% - 125), 0
NEXT lk%
FOR lk2% = 320 TO 355 STEP 1
LINE (lk2%, gl% - 100)-(lk2% - 35, gl% - 125), 0
NEXT lk2%
FOR ww% = 30 TO 320 STEP 80
LINE (ww%, 40)-(ww% + 20, 57), 0, B
PAINT (ww% + 5, 50), 148, 0
NEXT ww%
END IF
IF charpick% = 1 THEN CALL drawguy1plain
IF charpick% = 2 THEN pvv% = pv% - 2: CALL drawguy2plain
CALL drawenemyplain
IF life% < 100 THEN GOSUB startsound: life% = life% + 1: enemylife% = enemylife% + 1: GOTO 43
CALL statusbar
round rndnum%: EXIT SUB
startsound:
IF sounds$ = "NO" THEN delay 250: GOTO 5568
IF soundcard$ = "NO" THEN SOUND 750, .3: SOUND 500, .1: GOTO 5568
note 13000, 100: note 13500, 150
5568 RETURN
END SUB

SUB fightwithchar1
IF bg% = 1 THEN LINE (0, 160)-(320, 200), 0, BF:
CALL fightstartup
guy1position$ = "PLAIN"
life% = 100
enemylife% = 100
gl% = 125
yesjump% = 1
yeshot% = 1
eh% = 240
ev% = gl% - 83
ph% = 100
time% = 4900
pv% = gl%
pvv% = pv% - 42
evv% = pvv%
IF bg% = 1 THEN LINE (0, 160)-(320, 200), 0, BF:
IF bg% = 2 THEN
CALL background2
FOR lk% = 0 TO -35 STEP -1
LINE (lk%, gl% - 100)-(lk% + 35, gl% - 125), 0
NEXT lk%
FOR lk2% = 320 TO 355 STEP 1
LINE (lk2%, gl% - 100)-(lk2% - 35, gl% - 125), 0
NEXT lk2%
FOR ww% = 30 TO 320 STEP 80
LINE (ww%, 40)-(ww% + 20, 57), 0, B
PAINT (ww% + 5, 50), 148, 0
NEXT ww%
END IF
IF bg% = 1 THEN LINE (0, 160)-(320, 200), 0, BF:
DO
IF firetimer% > 0 THEN firetimer% = firetimer% - 1
IF hittimer% > 0 THEN hittimer% = hittimer% - 1
IF life% < 0 OR time% < 0 OR enemylife% < 0 THEN GOTO e1
CALL statusbar
IF bg% = 1 THEN CALL background1: IF bg% = 2 THEN CALL background2               '7
LINE (ph% - 7, pv% - 15)-(ph% + 23, pv% + 7), 9, BF:
SELECT CASE guy1position$
CASE IS = "PLAIN"             '16!'
LINE (ph% - 7, pv% - 15)-(ph% + 12, pv% + 7), 9, BF:
CALL drawguy1plain
CASE IS = "PUNCHING"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy1punching
CASE IS = "KICKING"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy1kicking
CASE IS = "STICK"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy1stick
CASE IS = "KNEE"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy1knee
END SELECT
guy1position$ = "PLAIN"
SELECT CASE INKEY$
CASE IS = "*"
CALL fightwithchar1fire
CASE IS = "-"
CALL fightwithchar1blck
CASE IS = "+"
IF scount% = 13 THEN guy1position$ = "STICK": scount% = 0: GOTO 68
guy1position$ = "PUNCHING"
68 CASE IS = CHR$(13)
IF scount% = 13 THEN guy1position$ = "KNEE": scount% = 0: GOTO 69
guy1position$ = "KICKING"
69 CASE IS = CHR$(27)
CALL exitprogram
CASE IS = CHR$(0) + "M"
IF scount% = 7 THEN scount% = scount% + 6
IF ph% + 20 > eh% THEN GOTO 868
LINE (ph% - 7, pv% - 15)-(ph% + 7, pv% + 7), 9, BF: ph% = ph% + 5
868 CASE IS = CHR$(0) + "K"
IF scount% = 0 THEN scount% = scount% + 4
LINE (ph% - 7, pv% - 15)-(ph% + 7, pv% + 7), 9, BF: ph% = ph% - 5
CASE IS = CHR$(0) + "P"
IF scount% = 4 THEN scount% = scount% + 3
CALL fightwithchar1duck
CASE IS = CHR$(0) + "H"                                                                                                                                                                                                                                                                                                                                                                   'start +
yesjump% = 2: LINE (ph% - 7, pvv% + 10)-(ph% + 16, pv% + 7), 9, BF: pvv% = pvv% - 5: GOSUB delay: LINE (ph% - 7, pvv% + 10)-(ph% + 7, pv% + 7), 9, BF: pvv% = pvv% - 5: GOSUB delay: LINE (ph% - 7, pvv% + 10)-(ph% + 7, pv% + 7), 9, BF: pvv% = pvv% - 5: GOSUB delay: LINE (ph% - 7, pvv% + 10)-(ph% + 7, pv% + 7), 9, BF: pvv% = pvv% - 5: GOSUB delay: LINE (ph% - 7, pvv% + 10)-(ph% + 7, pv% + 7), 9, BF: pvv% = pvv% - 5: GOSUB delay: LINE (ph% - 7, pvv% + 10)-(ph% + 7, pv% + 7), 9, BF: pvv% = pvv% + 5: GOSUB delay: LINE (ph% - 7, pvv% + 10)-(ph% + 7, pv% + 7), 9, BF: pvv% = pvv% + 5: GOSUB delay: LINE (ph% - 7, pvv% + 10)-(ph% + 7, pv% + 7), 9, BF: pvv% = pvv% + 5: GOSUB delay: LINE (ph% - 7, pvv% + 10)-(ph% + 7, pv% + 7), 9, BF: pvv% = pvv% + 5: GOSUB delay: LINE (ph% - 7, pvv% + 10)-(ph% + 7, pv% + 7), 9, BF: pvv% = pvv% + 5
yesjump% = 1
END SELECT
IF ph% < LESSLIMIT% THEN LINE (ph% - 7, pv% - 15)-(ph% + 7, pv% + 7), 9, BF: ph% = LESSLIMIT%
IF ph% > MORELIMIT% THEN LINE (ph% - 7, pv% - 15)-(ph% + 7, pv% + 7), 9, BF: ph% = MORELIMIT%
time% = time% - 3
CALL enemypattern
LOOP
delay:
259
LINE (ph% - 14, pv% - 35)-(ph% + 15, gl% + 7), 9, BF
SELECT CASE INKEY$
CASE IS = "+"
IF guy1position$ = "PLAIN" THEN guy1position$ = "PUNCHING"
CASE IS = CHR$(27)
CALL exitprogram
CASE IS = CHR$(13)
IF guy1position$ = "PLAIN" THEN guy1position$ = "KICKING"
CASE IS = CHR$(0) + "M"
IF ph% + 20 > eh% THEN GOTO 8682
LINE (ph% - 7, pv% - 7)-(ph% + 7, pv% + 7), 9, BF: ph% = ph% + 5
8682 CASE IS = CHR$(0) + "K"
LINE (ph% - 7, pv% - 7)-(ph% + 7, pv% + 7), 9, BF: ph% = ph% - 5
END SELECT
IF ph% < LESSLIMIT% THEN ph% = LESSLIMIT%
IF ph% > MORELIMIT% THEN ph% = MORELIMIT%
CALL statusbar
LINE (ph% - 7, pv% - 15)-(ph% + 7, pv% + 7), 9, BF:
SELECT CASE guy1position$
CASE IS = "PLAIN"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy1plain
CASE IS = "PUNCHING"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy1punching
CASE IS = "KICKING"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy1kicking
END SELECT
B% = 0    ''''''speed#
1012 IF B% < 500 THEN B% = B% + 1: GOTO 1012
time% = time% - 10
RANDOMIZE TIMER
A% = 0    ''''''speed#
10 IF A% < 7500 THEN A% = A% + 1: GOTO 10
CALL enemypattern
RETURN
e1: CLS
END SUB

SUB fightwithchar1blck
blocking$ = "YES"
gl% = 125
yesjump% = 1
yeshot% = 1
'eh% = 240
ev% = gl% - 83
'ph% = 100
'time% = 20000
pv% = gl%
IF charpick% = 1 THEN pvv% = pv% - 42
IF charpick% = 2 THEN pvv% = pv% - 2
LINE (ph% - 7, pv% - 15)-(ph% + 23, pv% + 7), 9, BF:
IF charpick% = 1 THEN CALL drawguy1blocking
IF charpick% = 2 THEN CALL drawguy2victory
IF charpick% = 1 THEN bv% = 115
IF charpick% = 2 THEN bv% = 121
bh% = ph% + 15
LINE (ph% - 7, pv% - 15)-(ph% + 23, pv% + 7), 9, BF:
DO: blocking$ = "YES"
IF hittimer% > 0 THEN hittimer% = hittimer% - 1
IF life% < 0 OR time% < 0 OR enemylife% < 0 THEN GOTO e6
COLOR 4
CALL statusbar
IF bg% = 1 THEN CALL background1: IF bg% = 2 THEN CALL background2
IF charpick% = 1 THEN CALL drawguy1blocking
IF charpick% = 2 THEN CALL drawguy2victory
SELECT CASE INKEY$
CASE IS = CHR$(27)
CALL exitprogram
CASE IS = ""
CASE IS = "-"
CASE ELSE
blocking$ = "NO": EXIT SUB
END SELECT
time% = time% - 3 '^5?
CALL enemypattern
IF ph% < LESSLIMIT% THEN LINE (ph% - 7, pv% - 15)-(ph% + 7, pv% + 7), 9, BF: ph% = LESSLIMIT%
LOOP
e6: CLS
END SUB

DEFINT A-Z
SUB fightwithchar1duck
guy1position$ = "PLAIN"
gl% = 125
yesjump% = 1
yeshot% = 1
ev% = gl% - 83
pv% = gl%
pvv% = pv% - 42
DO
IF hittimer% > 0 THEN hittimer% = hittimer% - 1
IF life% < 0 OR time% < 0 OR enemylife% < 0 THEN GOTO e3
COLOR 4
CALL statusbar
IF bg% = 1 THEN CALL background1: IF bg% = 2 THEN CALL background2
LINE (ph% - 7, pv% - 15)-(ph% + 7, pv% + 7), 9, BF:
SELECT CASE guy1position$
CASE IS = "DUCKING"'"PLAIN"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy1ducking
CASE IS = "PUNCHING"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy1punching
CASE IS = "KICKING"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy1kicking
END SELECT
guy1position$ = "DUCKING"'"PLAIN"
SELECT CASE INKEY$
CASE IS = "+"
guy1position$ = "PUNCHING"
CASE IS = CHR$(13)
guy1position$ = "KICKING"
CASE IS = CHR$(27)
CALL exitprogram
CASE IS = CHR$(0) + "H"                                                                                                                                                                                                                                                                                                                                                                   'start +
EXIT SUB
END SELECT
IF ph% < LESSLIMIT% THEN LINE (ph% - 7, pv% - 15)-(ph% + 7, pv% + 7), 9, BF: ph% = LESSLIMIT%
IF ph% > MORELIMIT% THEN LINE (ph% - 7, pv% - 15)-(ph% + 7, pv% + 7), 9, BF: ph% = MORELIMIT%
time% = time% - 3
CALL enemypattern
LOOP
e3: CLS
END SUB

DEFSNG A-Z
SUB fightwithchar1fire
IF firetimer% > 0 THEN EXIT SUB
fireball$ = "YES"
firetimer% = 75
gl% = 125
yesjump% = 1
yeshot% = 1
ev% = gl% - 83
pv% = gl%
IF charpick% = 1 THEN pvv% = pv% - 42
IF charpick% = 2 THEN pvv% = pv% - 2
LINE (ph% - 7, pv% - 15)-(ph% + 23, pv% + 7), 9, BF:
IF charpick% = 1 THEN CALL drawguy1blocking
IF charpick% = 2 THEN CALL drawguy2victory
soundeffect "STICK"
IF charpick% = 1 THEN bv% = 115
IF charpick% = 2 THEN bv% = 121
bh% = ph% + 15
LINE (ph% - 7, pv% - 15)-(ph% + 23, pv% + 7), 9, BF:
DO
IF life% < 0 OR time% < 0 OR enemylife% < 0 THEN GOTO e5
COLOR 4
CALL statusbar
IF bg% = 1 THEN CALL background1: IF bg% = 2 THEN CALL background2
IF charpick% = 1 THEN CALL drawguy1blocking
IF charpick% = 2 THEN CALL drawguy2victory
CALL drawfireball
SELECT CASE INKEY$
CASE IS = CHR$(27)
CALL exitprogram
END SELECT
bh% = bh% + 3
IF bh% + 10 > eh% THEN
soundeffect "PUNCH":
LINE (bh% - 6, 112)-(bh% + 3, 117), 9, BF: LINE (eh% - 7, evv% + 24)-(eh% + 7, gl% + 7), 9, BF:
IF enemyposition$ = "BLOCKING" THEN GOTO 86
combodamage% = combodamage% + 10
IF hittimer% > 0 THEN combohits% = combohits% + 1: combodamage% = combodamage% + 10: COMBOTIMER% = 250
eh% = eh% + 10: score% = score% + 25: enemylife% = enemylife% - 8: '5'25'8
86 CALL fireballexplode: fireball$ = "NO": EXIT SUB
END IF
time% = time% - 3
CALL enemypattern
delay 10000
LINE (bh% - 6, bv% - 3)-(bh% + 3, bv% + 2), 9, BF
CALL drawfireball
LOOP
e5: CLS
END SUB

SUB fightwithchar2
CALL fightstartup
guy1position$ = "PLAIN"
life% = 100
enemylife% = 100
gl% = 125
yesjump% = 1
yeshot% = 1
eh% = 240
ev% = gl% - 83
ph% = 100
time% = 4900
pv% = gl%
pvv% = pv% - 2
evv% = pvv% - 40
IF bg% = 2 THEN
CALL background2
'omit this!!!!!
FOR lk% = 0 TO -35 STEP -1
LINE (lk%, gl% - 100)-(lk% + 35, gl% - 125), 0
NEXT lk%
FOR lk2% = 320 TO 355 STEP 1
LINE (lk2%, gl% - 100)-(lk2% - 35, gl% - 125), 0
NEXT lk2%
FOR ww% = 30 TO 320 STEP 80
LINE (ww%, 40)-(ww% + 20, 57), 0, B
PAINT (ww% + 5, 50), 148, 0
NEXT ww%
END IF
DO
IF mcounttimer% > 0 THEN mcounttimer% = mcounttimer% - 1
IF mcounttimer% = 0 THEN mcount% = 0
IF colytimer% > 0 THEN colytimer% = colytimer% - 1
IF firetimer% > 0 THEN firetimer% = firetimer% - 1
IF hittimer% > 0 THEN hittimer% = hittimer% - 1
IF colytimer% = 0 THEN coly% = 10
IF life% < 0 OR time% < 0 OR enemylife% < 0 THEN GOTO e2
CALL statusbar
IF bg% = 1 THEN CALL background1: IF bg% = 2 THEN CALL background2               '23?
LINE (ph% - 7, pv% - 15)-(ph% + 23, pv% + 7), 9, BF:
SELECT CASE guy1position$
CASE IS = "PLAIN"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy2plain
CASE IS = "PUNCHING"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy2punching
CASE IS = "KICKING"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy2kicking
CASE IS = "STICK"
LINE (ph% - 7, pv% - 15)-(ph% + 18, pv% + 7), 9, BF:
CALL drawguy2stretch
END SELECT
guy1position$ = "PLAIN"
SELECT CASE INKEY$
CASE IS = "*"
CALL fightwithchar1fire
CASE IS = "-"
IF mcount% = 18 THEN soundeffect "HIGH": mcount% = 0: coly% = 4: colytimer% = 500: GOTO 75
CALL fightwithchar1blck
75 CASE IS = "+"
IF scount% = 13 THEN scount% = 0: guy1position$ = "STICK": GOTO 76
guy1position$ = "PUNCHING"
IF mcount% = 13 THEN mcounttimer% = 30: mcount% = mcount% + 5
76 CASE IS = CHR$(13)
guy1position$ = "KICKING"
IF mcount% = 7 THEN mcounttimer% = 30: mcount% = mcount% + 6
CASE IS = CHR$(27)
CALL exitprogram
CASE IS = CHR$(0) + "M"
IF scount% = 0 THEN scount% = scount% + 4
IF ph% + 20 > eh% THEN GOTO 8683
LINE (ph% - 15, pv% - 15)-(ph% + 7, pv% + 7), 9, BF: ph% = ph% + 5
8683 CASE IS = CHR$(0) + "K"
IF scount% = 4 THEN scount% = scount% + 3
LINE (ph% - 7, pv% - 15)-(ph% + 7, pv% + 7), 9, BF: ph% = ph% - 5
CASE IS = CHR$(0) + "P"
LINE (ph% - 15, pv% - 15)-(ph% + 7, pv% + 7), 9, BF:
IF scount% = 7 THEN scount% = scount% + 6
IF mcount% = 4 THEN mcounttimer% = 30: mcount% = mcount% + 3'DUDU yes!!!!!!!! ! ! ! ! before)
IF mcount% = 0 THEN mcounttimer% = 60: mcount% = mcount% + 4
CALL fightwithchar2duck
CASE IS = CHR$(0) + "H"                                                                                                                                                                                                                                                                                                                                                                 'start +
yesjump% = 2: LINE (ph% - 7, pvv% + 9)-(ph% + 16, pv% + 7), 9, BF: pvv% = pvv% - 5: GOSUB delay2: LINE (ph% - 7, pvv% + 10)-(ph% + 7, pv% + 7), 9, BF: pvv% = pvv% - 5: GOSUB delay2: LINE (ph% - 7, pvv% + 10)-(ph% + 7, pv% + 7), 9, BF: pvv% = pvv% - 5: GOSUB delay2: LINE (ph% - 7, pvv% + 10)-(ph% + 7, pv% + 7), 9, BF: pvv% = pvv% - 5: GOSUB delay2: LINE (ph% - 7, pvv% + 10)-(ph% + 7, pv% + 7), 9, BF: pvv% = pvv% - 5: GOSUB delay2: LINE (ph% - 7, pvv% + 10)-(ph% + 7, pv% + 7), 9, BF: pvv% = pvv% + 5: GOSUB delay2: LINE (ph% - 7, pvv% + 10)-(ph% + 7, pv% + 7), 9, BF: pvv% = pvv% + 5: GOSUB delay2: LINE (ph% - 7, pvv% + 10)-(ph% + 7, pv% + 7), 9, BF: pvv% = pvv% + 5: GOSUB delay2: LINE (ph% - 7, pvv% + 10)-(ph% + 7, pv% + 7), 9, BF: pvv% = pvv% + 5: GOSUB delay2: LINE (ph% - 7, pvv% + 10)-(ph% + 7, pv% + 7), 9, BF: pvv% = pvv% + 5
LINE (ph% - 15, pv% - 15)-(ph% + 7, pv% + 7), 9, BF:
IF bg% = 2 THEN LINE (5, gl% + 8)-(5, gl% - 18), 0:
yesjump% = 1
END SELECT
IF ph% < LESSLIMIT% THEN ph% = LESSLIMIT%
IF ph% > MORELIMIT% THEN ph% = MORELIMIT%
time% = time% - 3
CALL enemypattern
LOOP
delay2:
2592
LINE (ph% - 14, pv% - 35)-(ph% + 15, gl% + 7), 9, BF
IF bg% = 2 THEN LINE (5, gl% + 8)-(5, gl% - 18), 0
SELECT CASE INKEY$
CASE IS = CHR$(27)
CALL exitprogram
CASE IS = "+"
IF guy1position$ = "PLAIN" THEN guy1position$ = "PUNCHING"
CASE IS = CHR$(13)
IF guy1position$ = "PLAIN" THEN guy1position$ = "KICKING"
CASE IS = CHR$(0) + "M"
IF ph% + 20 > eh% THEN GOTO 8684
LINE (ph% - 7, pv% - 7)-(ph% + 7, pv% + 7), 9, BF: ph% = ph% + 5
8684 CASE IS = CHR$(0) + "K"
LINE (ph% - 7, pv% - 7)-(ph% + 7, pv% + 7), 9, BF: ph% = ph% - 5
END SELECT
IF bg% = 2 THEN LINE (5, gl% + 8)-(5, gl% - 18), 0
IF ph% < LESSLIMIT% THEN ph% = LESSLIMIT%
IF ph% > MORELIMIT% THEN ph% = MORELIMIT%
CALL statusbar
LINE (ph% - 15, pv% - 15)-(ph% + 7, pv% + 7), 9, BF:
IF bg% = 2 THEN LINE (5, gl% + 8)-(5, gl% - 18), 0
SELECT CASE guy1position$
CASE IS = "PLAIN"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy2plain
CASE IS = "PUNCHING"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy2punching
CASE IS = "KICKING"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy2kicking
END SELECT
IF bg% = 3 THEN LINE (5, gl% + 8)-(5, gl% - 18), 0
B% = 0    ''''''speed#
10122 IF B% < 500 THEN B% = B% + 1: GOTO 10122
time% = time% - 10
RANDOMIZE TIMER
A% = 0    ''''''speed#
102 IF A% < 7500 THEN A% = A% + 1: GOTO 102
CALL enemypattern
IF bg% = 2 THEN LINE (5, gl% + 8)-(5, gl% - 18), 0
RETURN
e2: CLS
END SUB

DEFINT A-Z
SUB fightwithchar2duck
guy1position$ = "PLAIN"
gl% = 125
yesjump% = 1
yeshot% = 1
ev% = gl% - 83
pv% = gl%
pvv% = pv% - 2
DO
IF mcounttimer% > 0 THEN mcounttimer% = mcounttimer% - 1
IF mcounttimer% = 0 THEN mcount% = 0
IF hittimer% > 0 THEN hittimer% = hittimer% - 1
IF life% < 8 OR time% < 0 OR enemylife% < 0 THEN GOTO e4
CALL statusbar
IF bg% = 1 THEN CALL background1: IF bg% = 2 THEN CALL background2 '7''15'7'7'9
LINE (ph% - 15, pv% - 15)-(ph% + 7, pv% + 7), 9, BF:
LINE (5, gl% + 8)-(5, gl% - 18), 0
SELECT CASE guy1position$
CASE IS = "DUCKING"'"PLAIN"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy2ducking
CASE IS = "PUNCHING"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy2punching
CASE IS = "KICKING"
LINE (ph% - 7, pv% - 15)-(ph% + 16, pv% + 7), 9, BF:
CALL drawguy2kicking
END SELECT
guy1position$ = "DUCKING"'"PLAIN"
SELECT CASE INKEY$
CASE IS = "+"
guy1position$ = "PUNCHING"
CASE IS = CHR$(13)
guy1position$ = "KICKING"
CASE IS = CHR$(27)
CALL exitprogram
LINE (5, gl% + 8)-(5, gl% - 18), 0
CASE IS = CHR$(0) + "H"
LINE (ph% - 15, pv% - 15)-(ph% + 7, pv% + 7), 9, BF:
LINE (5, gl% + 8)-(5, gl% - 18), 0
EXIT SUB
END SELECT
IF ph% < LESSLIMIT% THEN LINE (ph% - 7, pv% - 15)-(ph% + 7, pv% + 7), 9, BF: ph% = LESSLIMIT%
IF ph% > MORELIMIT% THEN LINE (ph% - 7, pv% - 15)-(ph% + 7, pv% + 7), 9, BF: ph% = MORELIMIT%
LINE (5, gl% + 8)-(5, gl% - 18), 0
time% = time% - 3
CALL enemypattern
LOOP
e4: CLS
END SUB

DEFSNG A-Z
SUB fightwithenemyfall
LINE (ph% - 7, pvv% - 15)-(ph% + 23, pvv% + 7), 9, BF:
LINE (ph% - 7, pv% - 15)-(ph% + 23, pv% + 7), 9, BF:
gl% = 125
yesjump% = 1
yeshot% = 1
ev% = gl% - 83
pv% = gl%
IF charpick% = 1 THEN pvv% = pv% - 42
IF charpick% = 2 THEN pvv% = pv% - 2
LINE (ph% - 7, pvv% - 15)-(ph% + 23, gl% + 7), 9, BF:
IF charpick% = 1 AND guy1position$ <> "STICK" THEN CALL drawguy1plain
IF charpick% = 2 AND guy1position$ <> "STICK" THEN CALL drawguy2plain
LINE (ph% - 7, pvv% - 15)-(ph% + 23, gl% + 7), 9, BF:
LINE (ph% - 7, pv% - 15)-(ph% + 23, pv% + 7), 9, BF:
IF charpick% = 1 AND guy1position$ = "STICK" THEN CALL drawguy1stick
IF charpick% = 2 AND guy1position$ = "STICK" THEN CALL drawguy2stretch
COLOR 4
CALL statusbar
IF bg% = 1 THEN CALL background1: IF bg% = 2 THEN CALL background2
'IF charpick% = 1 THEN CALL drawguy1plain
'IF charpick% = 2 THEN CALL drawguy2plain
SELECT CASE INKEY$
CASE IS = CHR$(27)
CALL exitprogram
END SELECT
CALL drawenemyfallen
delay 32766: delay 20000
LINE (eh% - 7, gl%)-(eh% + 26, gl% + 7), 9, BF:
END SUB

SUB fireballexplode
LINE (bh% - 6, bv% - 3)-(bh% + 3, bv% + 2), 9, BF
CALL drawenemyplain
bh% = bh% + 2       '4 OR 5!
FOR sizeofthis% = 1 TO 4 STEP 1
CIRCLE (bh%, bv%), sizeofthis%, 4
IF sounds$ = "NO" THEN delay 10000: GOTO 780
IF soundcard$ = "NO" THEN SOUND (sizeofthis% * 5) + 50, 1: GOTO 780
IF soundcard$ = "YES" THEN thy% = (sizeofthis% * 750) + 13000: note thy%, 3500
780 NEXT sizeofthis%'4 OR 5!
FOR sizeofthis2% = 4 TO 1 STEP -1
CIRCLE (bh%, bv%), sizeofthis2%, 9
IF sounds$ = "NO" THEN delay 10000: GOTO 781
IF soundcard$ = "NO" THEN SOUND (sizeofthis2% * 5) + 50, 1: GOTO 781
IF soundcard$ = "YES" THEN thy% = (sizeofthis% * 750) + 13000: note thy%, 3500
781 NEXT sizeofthis2%
END SUB

SUB font (fh%, fv%, letter$, col%)
SELECT CASE letter$
CASE IS = "A"
LINE (fh% - 3, fv%)-(fh%, fv% - 8), col%
LINE (fh% + 3, fv%)-(fh%, fv% - 8), col%
LINE (fh% - 1, fv% - 3)-(fh% + 1, fv% - 3), col%
CASE IS = "B"
LINE (fh% - 3, fv%)-(fh% - 3, fv% - 8), col%
LINE (fh% - 3, fv% - 4)-(fh% + 1, fv% - 4), col%
LINE (fh% - 3, fv%)-(fh% + 2, fv%), col%
LINE (fh% - 3, fv% - 8)-(fh% + 2, fv% - 8), col%
LINE (fh% + 2, fv% - 8)-(fh% + 1, fv% - 4), col%
LINE (fh% + 2, fv%)-(fh% + 1, fv% - 4), col%
CASE IS = "C"
LINE (fh% - 3, fv%)-(fh% - 3, fv% - 8), col%
LINE (fh% - 3, fv%)-(fh% + 2, fv%), col%
LINE (fh% - 3, fv% - 8)-(fh% + 2, fv% - 8), col%
CASE IS = "D"
LINE (fh% - 3, fv%)-(fh% - 3, fv% - 8), col%
LINE (fh% - 3, fv%)-(fh% + 1, fv%), col%
LINE (fh% - 3, fv% - 8)-(fh% + 1, fv% - 8), col%
LINE (fh% + 2, fv% - 1)-(fh% + 2, fv% - 7), col%
CASE IS = "E"
LINE (fh% - 3, fv%)-(fh% - 3, fv% - 8), col%
LINE (fh% - 3, fv% - 4)-(fh% + 1, fv% - 4), col%
LINE (fh% - 3, fv%)-(fh% + 2, fv%), col%
LINE (fh% - 3, fv% - 8)-(fh% + 2, fv% - 8), col%
CASE IS = "F"
LINE (fh% - 3, fv%)-(fh% - 3, fv% - 8), col%
LINE (fh% - 3, fv% - 4)-(fh% + 1, fv% - 4), col%
LINE (fh% - 3, fv% - 8)-(fh% + 2, fv% - 8), col%
CASE IS = "G"
LINE (fh% - 3, fv%)-(fh% - 3, fv% - 8), col%
LINE (fh% - 3, fv%)-(fh% + 2, fv%), col%
LINE (fh% - 3, fv% - 8)-(fh% + 2, fv% - 8), col%
LINE (fh% + 2, fv%)-(fh% + 2, fv% - 4), col%
LINE (fh% + 2, fv% - 4)-(fh%, fv% - 4), col%
CASE IS = "H"
LINE (fh% - 3, fv%)-(fh% - 3, fv% - 8), col%
LINE (fh% + 3, fv%)-(fh% + 3, fv% - 8), col%
LINE (fh% + 3, fv% - 4)-(fh% - 3, fv% - 4), col%
CASE IS = "I"
LINE (fh%, fv%)-(fh%, fv% - 8), col%
LINE (fh% - 1, fv%)-(fh% + 1, fv%), col%
LINE (fh% - 1, fv% - 8)-(fh% + 1, fv% - 8), col%
CASE IS = "J"
LINE (fh% + 1, fv%)-(fh% + 1, fv% - 8), col%
LINE (fh% - 3, fv%)-(fh% + 1, fv%), col%
LINE (fh% - 3, fv%)-(fh% - 3, fv% - 3), col%
LINE (fh% - 1, fv% - 8)-(fh% + 3, fv% - 8), col%
CASE IS = "K"
LINE (fh% - 3, fv%)-(fh% - 3, fv% - 8), col%
LINE (fh% - 3, fv% - 4)-(fh% + 2, fv%), col%
LINE (fh% - 3, fv% - 4)-(fh% + 2, fv% - 8), col%
CASE IS = "L"
LINE (fh% - 3, fv%)-(fh% - 3, fv% - 8), col%
LINE (fh% - 3, fv%)-(fh% + 2, fv%), col%
CASE IS = "M"
LINE (fh% - 3, fv%)-(fh% - 3, fv% - 8), col%
LINE (fh% + 3, fv%)-(fh% + 3, fv% - 8), col%
LINE (fh% + 3, fv% - 8)-(fh%, fv% - 5), col%
LINE (fh% - 3, fv% - 8)-(fh%, fv% - 5), col%
CASE IS = "N"
LINE (fh% - 3, fv%)-(fh% - 3, fv% - 8), col%
LINE (fh% + 3, fv%)-(fh% + 3, fv% - 8), col%
LINE (fh% - 3, fv% - 7)-(fh% + 3, fv% - 1), col%
CASE IS = "O"
LINE (fh% - 3, fv% - 1)-(fh% - 3, fv% - 7), col%
LINE (fh% - 2, fv%)-(fh% + 1, fv%), col%
LINE (fh% - 2, fv% - 8)-(fh% + 1, fv% - 8), col%
LINE (fh% + 2, fv% - 1)-(fh% + 2, fv% - 7), col%
CASE IS = "P"
LINE (fh% - 3, fv%)-(fh% - 3, fv% - 8), col%
LINE (fh% - 3, fv% - 4)-(fh% + 1, fv% - 4), col%
LINE (fh% - 3, fv% - 8)-(fh% + 2, fv% - 8), col%
LINE (fh% + 2, fv% - 8)-(fh% + 1, fv% - 4), col%
CASE IS = "Q"
LINE (fh% - 3, fv% - 1)-(fh% - 3, fv% - 7), col%
LINE (fh% - 2, fv%)-(fh% + 1, fv%), col%
LINE (fh% - 2, fv% - 8)-(fh% + 1, fv% - 8), col%
LINE (fh% + 2, fv% - 1)-(fh% + 2, fv% - 7), col%
LINE (fh%, fv% - 3)-(fh% + 3, fv%), col%
CASE IS = "R"
LINE (fh% - 3, fv%)-(fh% - 3, fv% - 8), col%
LINE (fh% - 3, fv% - 4)-(fh% + 1, fv% - 4), col%
LINE (fh% - 3, fv% - 8)-(fh% + 2, fv% - 8), col%
LINE (fh% + 2, fv% - 8)-(fh% + 1, fv% - 4), col%
LINE (fh% - 2, fv% - 4)-(fh% + 2, fv%), col%
CASE IS = "S"
LINE (fh% - 3, fv% - 4)-(fh% - 3, fv% - 8), col%
LINE (fh% - 3, fv% - 4)-(fh% + 1, fv% - 4), col%
LINE (fh% - 3, fv%)-(fh% + 2, fv%), col%
LINE (fh% - 3, fv% - 8)-(fh% + 2, fv% - 8), col%
LINE (fh% + 2, fv%)-(fh% + 1, fv% - 4), col%
CASE IS = "T"
LINE (fh%, fv%)-(fh%, fv% - 8), col%
LINE (fh% - 3, fv% - 8)-(fh% + 3, fv% - 8), col%
CASE IS = "U"
LINE (fh% - 3, fv% - 1)-(fh% - 3, fv% - 8), col%
LINE (fh% - 2, fv%)-(fh% + 1, fv%), col%
LINE (fh% + 2, fv% - 1)-(fh% + 2, fv% - 8), col%
CASE IS = "V"
LINE (fh%, fv%)-(fh% - 3, fv% - 8), col%
LINE (fh%, fv% - 1)-(fh% + 3, fv% - 8), col%
CASE IS = "W"
LINE (fh% + 3, fv%)-(fh% + 3, fv% - 8), col%
LINE (fh% + 3, fv%)-(fh%, fv% - 5), col%
LINE (fh% - 3, fv%)-(fh%, fv% - 5), col%
LINE (fh% - 3, fv%)-(fh% - 3, fv% - 8), col%
CASE IS = "X"
LINE (fh% + 3, fv%)-(fh% - 3, fv% - 8), col%
LINE (fh% - 3, fv%)-(fh% + 3, fv% - 8), col%
PSET (fh% + 1, fv% - 4), col%
CASE IS = "Y"
LINE (fh%, fv% - 5)-(fh% - 3, fv% - 8), col%
LINE (fh%, fv% - 5)-(fh% + 3, fv% - 8), col%
LINE (fh%, fv% - 5)-(fh%, fv%), col%
CASE IS = "Z"
LINE (fh% - 3, fv%)-(fh% + 3, fv% - 8), col%
LINE (fh% - 3, fv%)-(fh% + 3, fv%), col%
LINE (fh% - 3, fv% - 8)-(fh% + 3, fv% - 8), col%
END SELECT
END SUB

SUB gethighscore
CLS : SCREEN 13
LOCATE 2, 2: COLOR 10: PRINT "You've got a high score!"
LOCATE 4, 2: COLOR 12: PRINT "Enter your name:"
LOCATE 5, 2: COLOR 12: INPUT "", nname$
CLS : SCREEN 13
highscore% = score%
highfightwins% = fightwins%
name$ = nname$
END SUB

SUB grafxtest
CLS : SCREEN 13: PAINT (1, 1), 9
no$ = "YES": sounds$ = "NO"
col1% = 8: col2% = 4
FOR eh% = 25 TO 320 STEP 30
CALL drawenemyplain
col1% = col1% + 1: col2% = col2% + 1
NEXT eh%
pvv% = 40: ph% = 25
CALL drawguy1plain
ph% = 55
CALL drawguy1punching
ph% = 85
CALL drawguy1kicking
ph% = 115
CALL drawguy1ducking
ph% = 145
CALL drawguy1blocking
ph% = 175
CALL drawguy1stick
pvv% = 111: ph% = 25
CALL drawguy2plain
ph% = 55
CALL drawguy2punching
ph% = 85
CALL drawguy2kicking
ph% = 115
CALL drawguy2ducking
ph% = 145
CALL drawguy2victory
ph% = 175
CALL drawguy2stretch
LOCATE 19, 8: COLOR 15: PRINT "                              "
LOCATE 20, 8: COLOR 15: PRINT " Press any key to continue... "
LOCATE 21, 8: COLOR 15: PRINT "                              "
SLEEP
IF no$ = "YES" THEN sounds$ = "YES"
CLS : SCREEN 13: ncount% = 0: zcount% = 0: kcount% = 0
END SUB

SUB guy1ending
CLS : SCREEN 13
LOCATE 2, 2: COLOR 10: PRINT "    You win!  Derrick is triumphant!"
LOCATE 4, 2: COLOR 8: PRINT "Now that Derrick has won the tournament and gotten revenge, he sees no reason togo on fighting.  He mysteriously dissape-ars one night, and is never seen again.At least for now..."
FOR onemore% = 1 TO 75
delay 32766
NEXT onemore%
END SUB

SUB guy2ending
CLS : SCREEN 13
LOCATE 2, 2: COLOR 10: PRINT "    You win!  Samson is triumphant!"
LOCATE 4, 2: COLOR 8: PRINT "Now that Samson has the key to EyeKlomiahe decides to settle down in the city ofKuham, or as the locals call it, Cone-  land.  He is very happy there, and is   content to remain there, at least until the next tournament..."
FOR onemore% = 1 TO 75
delay 32766
NEXT onemore%
CALL endpicture
END SUB

DEFINT A-Z
SUB intro
COLOR 15
FOR y% = 0 TO 50:
GOSUB fadedelay
NEXT y%
CLS : SCREEN 13
FOR I = 1 TO 63
PALETTE 0, I
IF soundcard$ = "YES" THEN SBPlay I + 100
IF soundcard$ = "NO" THEN
IF sounds$ = "YES" THEN SOUND I * 100, .3
END IF
GOSUB fadedelay
NEXT I
IF soundcard$ = "YES" THEN CALL WriteReg(&HB0, &H0)
GOSUB fadedelay
LOCATE 12, 14: PRINT "DEATH FIGHT!"
delay 32766: delay 32766: delay 32766
delay 32766: delay 32766: delay 32766
delay 32766: delay 32766: CALL song1
GOTO 104
fadedelay:
delayb% = 0
103 IF delayb% < 2500 THEN delayb% = delayb% + 1: GOTO 103
RETURN
104
FOR I = 63 TO 0 STEP -1
PALETTE 0, I
GOSUB fadedelay
NEXT I
LOCATE 12, 14: PRINT "DEATH FIGHT!"
LOCATE 14, 14: PRINT "Version 2.00"
FOR y% = 0 TO 25:
GOSUB fadedelay
NEXT y%
CLS : SCREEN 13
COLOR 14
CIRCLE (50, 75), 50, , 5.5, 5, 1
CIRCLE (48, 70), 30, , , , .8
PSET (65, 122)
LINE -STEP(8, 20): LINE -STEP(30, 0): LINE -STEP(-18, -30)
PSET (110, 50)
LINE -STEP(0, 100)
CIRCLE (120, 75), 35, , 4.6, 1.9, .8
CIRCLE (120, 128), 40, , 4.4, 1.5, .6
CIRCLE (127, 81), 10, , , , .9: CIRCLE (128, 125), 12, , , , .9
CIRCLE (180, 120), 20, , .5, 5.6, .6
CIRCLE (180, 120), 10, , , , .7
CIRCLE (180, 105), 20, , 0, 2, .7: CIRCLE (180, 113), 15, , 0, 2, .7
PSET (171, 92): LINE -STEP(2, 10)
PSET (200, 105): LINE -STEP(8, 30): LINE -STEP(-10, 1): LINE -STEP(-2, -8)
CIRCLE (220, 70), 30, , 1, 4, .9: CIRCLE (220, 71), 20, , 1, 4, .9
CIRCLE (215, 130), 30, , 4, 1, .9: CIRCLE (215, 128), 20, , 4, 1, .9
PSET (235, 48): LINE -STEP(-5, 8): PSET (207, 85): LINE -STEP(27, 23)
PSET (200, 90): LINE -STEP(27, 23): PSET (202, 141): LINE -STEP(-8, 10)
PSET (250, 80): LINE -STEP(5, 50): LINE -STEP(12, 0): LINE -STEP(-2, -50)
LINE -STEP(-14, 0): CIRCLE (257, 68), 10, , , , .7
CIRCLE (310, 100), 35, , 1.5, 5, 1: CIRCLE (310, 95), 25, , 1.5, 5, 1
PSET (312, 65): LINE -STEP(-1, 5): PSET (318, 120): LINE -STEP(1, 15)
LOCATE 3, 15: PRINT "All programming for"
LOCATE 4, 14: PRINT "this game was done in..."
FOR y% = 0 TO 100:
GOSUB fadedelay
NEXT y%
CLS : SCREEN 13
GOSUB fadedelay
END SUB

DEFSNG A-Z
SUB movelist
CLS : SCREEN 9
LOCATE 2, 2: COLOR 10: PRINT "DEATH FIGHT! - MOVE LIST:"
LOCATE 4, 2: COLOR 12: PRINT "Stick Attack:"
LOCATE 5, 2: COLOR 12: PRINT "With Derrick, press Left, Down, Up, Right, Punch"
LOCATE 6, 2: COLOR 12: PRINT "He will hit with a wooden stick taking off 8% damage"
LOCATE 8, 2: COLOR 12: PRINT "Knee Attack:"
LOCATE 9, 2: COLOR 12: PRINT "With Derrick, press Left, Down, Up, Right, Kick"
LOCATE 10, 2: COLOR 12: PRINT "He will jump up and knee you in the face (14% damage!)"
LOCATE 12, 2: COLOR 12: PRINT "Stretch Punch:"
LOCATE 13, 2: COLOR 12: PRINT "With Samson, press Right, Left, Down, Up, Punch"
LOCATE 14, 2: COLOR 12: PRINT "He will stretch out his arm to punch you (8% damage)"
LOCATE 16, 2: COLOR 12: PRINT "Rage Attack:"
LOCATE 17, 2: COLOR 12: PRINT "With Samson, press Down, Up, Down, Up, Kick, Punch, Block"
LOCATE 18, 2: COLOR 12: PRINT "He will get angry, making all his moves hurt more (until hit)"
LOCATE 20, 2: COLOR 10: PRINT "Press any key - if you want to print, turn on your printer first"
DO: IF INKEY$ <> "" THEN GOSUB printlist:
LOOP
printlist:
LPRINT "DEATH FIGHT! - MOVE LIST:"
LPRINT " "
LPRINT "Stick Attack:"
LPRINT "With Derrick, press Left, Down, Up, Right, Punch"
LPRINT "He will hit with a wooden stick taking off 8% damage"
LPRINT " "
LPRINT "Knee Attack:"
LPRINT "With Derrick, press Left, Down, Up, Right, Kick"
LPRINT "He will jump up and knee you in the face (14% damage!)"
LPRINT " "
LPRINT "Stretch Punch:"
LPRINT "With Samson, press Right, Left, Down, Up, Punch"
LPRINT "He will stretch out his arm to punch you (8% damage)"
LPRINT " "
LPRINT "Rage Attack:"
LPRINT "With Samson, press Down, Up, Down, Up, Kick, Punch, Block"
LPRINT "He will get angry, making all his moves hurt more (until hit)"
CLS : SCREEN 13: EXIT SUB
RETURN
END SUB

DEFINT A-Z
SUB note (level%, legnth%)
SBPlay level%
FOR delay1% = 0 TO legnth% STEP 1: NEXT delay1%
FOR delay1% = 0 TO legnth% STEP 1: NEXT delay1%
CALL WriteReg(&HB0, &H0)
END SUB

DEFSNG A-Z
SUB punchoffledge
CLS : SCREEN 13
PAINT (1, 100), 9
LINE (-1, 100)-(135, 200), 0, B
PAINT (1, 101), 8, 0
ph% = 112: pvv% = 49
IF charpick% = 1 THEN pvv% = 49
IF charpick% = 2 THEN pvv% = 90
IF charpick% = 1 THEN ph% = 112
IF charpick% = 2 THEN ph% = 110
IF charpick% = 1 THEN CALL drawguy1plain
IF charpick% = 2 THEN CALL drawguy2plain
eh% = 127: evv% = 49: col1% = 0: col2% = 0: col3% = 0
CALL drawenemyplain
delay 32766: delay 32766: delay 32766: delay 32766
LINE (105, 99)-(118, 76), 9, BF
IF charpick% = 1 THEN CALL drawguy1punching
IF charpick% = 2 THEN CALL drawguy2punching
IF sounds$ = "YES" THEN sounds$ = "NO": change$ = "YES"
IF sounds$ = "NO" THEN change$ = "YES"
DO
LINE (105, 99)-(155, 76), 9, BF
IF charpick% = 1 THEN CALL drawguy1punching
IF charpick% = 2 THEN CALL drawguy2punching
IF eh% < 145 THEN eh% = eh% + 1
IF eh% = 145 THEN EXIT DO
CALL drawenemyplain
LOOP
'change$ = "NO"
IF change$ = "YES" THEN sounds$ = "YES"
LINE (105, 99)-(155, 76), 9, BF
IF charpick% = 1 THEN CALL drawguy1plain
IF charpick% = 2 THEN CALL drawguy2plain
LINE (136, 0)-(175, 200), 9, BF
CALL drawenemyfallen
DO
CALL drawenemyfallen
delay 1500
LINE (136, 0)-(320, 200), 9, BF
IF evv% < 160 THEN evv% = evv% + 1
IF evv% = 160 THEN EXIT DO
LOOP
END SUB

SUB puthighscore
CLS : SCREEN 13:
name$ = LEFT$(nname$, 12)
SLEEP 1: ABC% = 1: ABCD% = 200
DO: LOCATE 2, 7: COLOR 10: PRINT "The high-score belongs to..."
LOCATE 4, 17: COLOR ABC%: PRINT name$'RIGHT$(name$, 12)
LOCATE 6, 7: COLOR 10: PRINT "He or she had a score of..."
LOCATE 8, 16: COLOR ABC%: PRINT highscore%
LOCATE 9, 25: COLOR 10: PRINT "    He or she won this many fights..."
LOCATE 12, 17: COLOR ABC%: PRINT highfightwins%
ABC% = ABC% + 1: IF ABC% > 200 THEN ABC% = 1
IF sounds$ = "NO" THEN delay 7500: GOTO 568
IF soundcard$ = "YES" THEN GOSUB sbsound
IF soundcard$ = "NO" THEN GOSUB nosbsound
568 IF INKEY$ <> "" AND INKEY$ <> CHR$(27) THEN delay 32766: delay 32766: delay 32766: delay 32766: delay 32766: delay 32766: CLS : EXIT SUB
IF INKEY$ = CHR$(27) THEN CALL exitprogram
LOOP
nosbsound:
RANDOMIZE TIMER
fgh% = INT(RND * 1000) + 50
SOUND fgh%, .3
RETURN
sbsound:
RANDOMIZE TIMER
gh% = INT(RND * 3000) + 13000
note gh%, 7500
RETURN
END SUB

SUB reward
IF score% < 5000 THEN
CLS : SCREEN 9
LOCATE 2, 2: COLOR 4: PRINT "Hint:"
LOCATE 4, 2: COLOR 12: PRINT "Cheat codes can be inputed at the sound options screen."
LOCATE 5, 2: COLOR 12: PRINT "Win the game and do good enough and maybe I'll tell ya some."
FOR yep% = 1 TO 35
delay 32766
NEXT yep%
CLS : SCREEN 13
EXIT SUB
END IF
CLS : SCREEN 9: ABCDE% = 1
LOCATE 2, 2: COLOR 4: PRINT "Psst... hey you!  I got some secrets for ya!"
FOR yep% = 1 TO 15
delay 32766
NEXT yep%
LOCATE 4, 2: COLOR 12: PRINT "Sound Test:"
LOCATE 5, 2: COLOR 12: PRINT "At the sound options screen, type in GABE"
LOCATE 6, 2: COLOR 12: PRINT "You will get a different one depending on your sound card"
LOCATE 8, 2: COLOR 12: PRINT "Graphics Test:"
LOCATE 9, 2: COLOR 12: PRINT "At the sound options screen type in SPENCE"
LOCATE 10, 2: COLOR 12: PRINT "You will get to view about 2 dozen character frames"
LOCATE 12, 2: COLOR 12: PRINT "Practice Mode:"
LOCATE 13, 2: COLOR 12: PRINT "At the sound options screen type in DMODE"
LOCATE 14, 2: COLOR 12: PRINT "This puts you in a strange mode to practice moves"
LOCATE 16, 2: COLOR 12: PRINT "And, since your sooooooo good..."
LOCATE 17, 2: COLOR 12: PRINT "At the sound options screen type in KLOBO"
LOCATE 18, 2: COLOR 12: PRINT "This will max out the difficulty level!"
LOCATE 20, 2: COLOR 12: PRINT "Move List:"
LOCATE 21, 2: COLOR 12: PRINT "At the sound options screen type in MOVE"
LOCATE 22, 2: COLOR 12: PRINT "You will get to a screen of moves, which you can print"
FOR youstinkincheater% = 0 TO 200
delay 32766
NEXT youstinkincheater%
CLS : SCREEN 9
LOCATE 2, 2: COLOR 10: PRINT "Do you want to print that list? (Y/N)"
DO
IF INKEY$ = "Y" OR INKEY$ = "y" THEN GOSUB printreward
IF INKEY$ = "N" OR INKEY$ = "n" THEN GOTO 57
LOOP
printreward:
LPRINT "DEATH FIGHT! - CHEAT SHEET:"
LPRINT " "
LPRINT "Sound Test:"
LPRINT "At the sound options screen, type in GABE"
LPRINT "You will get a different one depending on your sound card"
LPRINT " "
LPRINT "Graphics Test:"
LPRINT "At the sound options screen type in SPENCE"
LPRINT "You will get to view about 2 dozen character frames"
LPRINT " "
LPRINT "Practice Mode:"
LPRINT "At the sound options screen type in DMODE"
LPRINT "This puts you in a strange mode to practice moves"
LPRINT " "
LPRINT "And, since your sooooooo good..."
LPRINT "At the sound options screen type in KLOBO"
LPRINT "This will max out the difficulty level!"
LPRINT " "
LPRINT "Move List:"
LPRINT "At the sound options screen type in MOVE"
LPRINT "You will get to a screen of moves, which you can print"
GOTO 57
RETURN
57 CLS : SCREEN 13
END SUB

SUB round (roundnum%)
delay 32766: delay 32766: delay 32766: delay 32766: delay 32766
LOCATE 12, 15: COLOR 12: PRINT " ROUND"; roundnum%
delay 32766: delay 32766: delay 32766: delay 32766: delay 32766
LOCATE 14, 15: COLOR 4: PRINT " FIGHT ! "
delay 32766: delay 32766: delay 32766: delay 32766: delay 32766
LINE (0, gl% + 7)-(320, 0), 9, BF
END SUB

DEFINT A-Z
SUB SBInit
FOR q = 1 TO &HF5
CALL WriteReg(q, 0)
NEXT q
END SUB

SUB SBPlay (freq%)
IF sounds$ = "NO" THEN EXIT SUB
CALL WriteReg(&H20, &H7)
CALL WriteReg(&H40, &HF)
CALL WriteReg(&H60, &HF0)
CALL WriteReg(&H80, &HF0)
CALL WriteReg(&HA0, freq%)
CALL WriteReg(&H23, &HF)
CALL WriteReg(&H43, &H0)
CALL WriteReg(&H63, &HF0)
CALL WriteReg(&H83, &HFF)
CALL WriteReg(&HB0, &H20)
CALL WriteReg(&HE0, &H0)
TimeUp! = TIMER + .0005
WHILE TimeUp! > TIMER: WEND
END SUB

SUB sbsoundtest
1542 CLS : SCREEN 9
LOCATE 2, 2:  COLOR 12: PRINT "Welcome to the secret sound and music test room!"
LOCATE 4, 2: COLOR 10: PRINT "1) ATTACK (sound card)"
LOCATE 5, 2: COLOR 10: PRINT "2) DING (sound card)"
LOCATE 6, 2: COLOR 10: PRINT "3) ATTACK (PC speaker)"
LOCATE 7, 2: COLOR 10: PRINT "4) DING (PC speaker)"
LOCATE 8, 2: COLOR 10: PRINT "5) WHOOP! (sound card)"
LOCATE 9, 2: COLOR 10: PRINT "6) WHOOP! (PC speaker)"
LOCATE 10, 2: COLOR 10: PRINT "7) SONG 1 (sound card)"
LOCATE 11, 2: COLOR 10: PRINT "8) SONG 2 (sound card)"
LOCATE 12, 2: COLOR 10: PRINT "9) STICK (sound card)"
LOCATE 13, 2: COLOR 10: PRINT "10) STICK (PC speaker)"
LOCATE 14, 2: COLOR 10: PRINT "11) RANDOM (sound card)"
LOCATE 15, 2: COLOR 10: PRINT "12) RANDOM (PC speaker)"
LOCATE 16, 2: COLOR 10: PRINT "13) NEVERMIND"
LOCATE 18, 2: COLOR 12: INPUT "Enter the number of the sound you want to hear... ", snumber%
IF snumber% = 1 THEN soundeffect "PUNCH": GOTO 1542
IF snumber% = 2 THEN soundeffect "DING": GOTO 1542
IF snumber% = 3 THEN SOUND 100, .75: GOTO 1542
IF snumber% = 4 THEN SOUND 250, .75: GOTO 1542
IF snumber% = 5 THEN FOR IT% = 1 TO 63: SBPlay IT% + 100: NEXT IT%: CALL WriteReg(&HB0, &H0): GOTO 1542
IF snumber% = 6 THEN FOR IT% = 1 TO 63: SOUND IT% * 100, .3: NEXT IT%: CALL WriteReg(&HB0, &H0): GOTO 1542
IF snumber% = 7 THEN CALL song1: GOTO 1542
IF snumber% = 8 THEN CALL song2: GOTO 1542
IF snumber% = 9 THEN soundeffect "STICK": GOTO 1542
IF snumber% = 10 THEN SOUND 150, .75: GOTO 1542
IF snumber% = 11 THEN
DO: RANDOMIZE TIMER
gh% = INT(RND * 3000) + 13000
note gh%, 7500
IF INKEY$ <> "" THEN GOTO 1542
LOOP
END IF
IF snumber% = 12 THEN
DO: RANDOMIZE TIMER
fgh% = INT(RND * 1000) + 50
SOUND fgh%, .3
IF INKEY$ <> "" THEN GOTO 1542
LOOP
END IF
IF snumber% <> 1 AND snumber% <> 2 AND snumber% <> 3 AND snumber% <> 4 AND snumber% <> 5 AND snumber% <> 6 AND snumber% <> 7 AND snumber% <> 8 AND snumber% <> 9 AND snumber% <> 10 AND snumber% <> 11 AND snumber% <> 12 THEN CLS : SCREEN 13: ncount% = 0: zcount% = 0: kcount% = 0: EXIT SUB
END SUB

SUB song1
IF sounds$ = "NO" THEN EXIT SUB
IF soundcard$ = "NO" THEN EXIT SUB
delay 32766
delay 32766
note 15500, 20000
note 14000, 15000
note 14500, 15000
note 15000, 32766
note 16250, 17500
note 15000, 32766
note 16250, 17500
note 16000, 15000
note 16250, 17500
note 15000, 32766
note 16250, 17500
note 15000, 32766
note 14500, 15000
note 14000, 15000
note 15500, 20000
delay 20000
note 14750, 15000
note 14000, 15000
note 15500, 20000
delay 32766
END SUB

SUB song2
IF sounds$ = "NO" THEN EXIT SUB
IF soundcard$ = "NO" THEN EXIT SUB
note 13000, 32766
note 16000, 10000
note 15000, 10000
delay 10000
note 14500, 15000
note 13000, 20000
note 11000, 32766
note 12000, 32766
note 13000, 32766
END SUB

SUB soundeffect (kind$)
IF change$ = "YES" THEN EXIT SUB
IF sounds$ = "NO" THEN delay 10000: EXIT SUB
SELECT CASE kind$
CASE IS = "STICK"
IF soundcard$ = "NO" THEN SOUND 150, .75: delay 10000: EXIT SUB
FOR this% = 1000 TO 1010
SBPlay this%
FOR delayc% = 0 TO 1000 STEP 1: NEXT delayc%
NEXT this%
CALL WriteReg(&HB0, &H0)
CASE IS = "PUNCH"
IF soundcard$ = "NO" THEN SOUND 100, .75: delay 10000: EXIT SUB
FOR nt = 0 TO 255
NEXT nt
SBPlay 2000
FOR delayc% = 0 TO 10000 STEP 1: NEXT delayc%
CALL WriteReg(&HB0, &H0)
CASE IS = "KICK"
IF soundcard$ = "NO" THEN SOUND 100, .75: delay 10000: EXIT SUB
FOR nt = 0 TO 255
NEXT nt
SBPlay 2000
FOR delayd% = 0 TO 10000 STEP 1: NEXT delayd%
CALL WriteReg(&HB0, &H0)
CASE IS = "DING"
IF soundcard$ = "NO" THEN SOUND 250, .75: EXIT SUB
FOR nt = 0 TO 255
NEXT nt
SBPlay 5000
FOR delaye% = 0 TO 10000 STEP 1: NEXT delaye%
CALL WriteReg(&HB0, &H0)
CASE IS = "HIGH"
IF soundcard$ = "NO" THEN SOUND 450, 1.5: EXIT SUB
note 13000, 12500
END SELECT
END SUB

SUB soundoptions
ncount% = 0: SPD = 50: CLS : SCREEN 13: SLEEP 1: SLEEP 1
210 LOCATE 9, 15: COLOR 12: PRINT "  DEATH FIGHT!": LOCATE 11, 14
COLOR 10: IF SELCT = 0 THEN PRINT CHR$(16); " SOUND CARD? ";  ELSE PRINT "  SOUND CARD? ";
IF player = 0 THEN PRINT "YES" ELSE IF player = 1 THEN PRINT "NO "
LOCATE 13, 14
IF SELCT = 1 THEN PRINT CHR$(16); " SOUND ON? ";  ELSE PRINT "  SOUND ON? ";
IF STYLE = 0 THEN PRINT "YES" ELSE IF STYLE = 1 THEN PRINT "NO "
14 LOCATE 15, 14
IF SELCT = 2 THEN PRINT CHR$(16); " GO TO DEATH FIGHT!";  ELSE PRINT "  GO TO DEATH FIGHT!";
DO: A$ = UCASE$(INKEY$): LOOP WHILE A$ = ""

IF A$ = "B" AND ncount% = 7 THEN ncount% = ncount% + 6
IF A$ = "A" AND ncount% = 4 THEN ncount% = ncount% + 3
IF A$ = "G" AND ncount% < 30000 THEN ncount% = ncount% + 4
IF A$ = "E" AND ncount% = 13 THEN ncount% = ncount% + 5
IF A$ = "N" AND zcount% = 13 THEN zcount% = zcount% + 5
IF A$ = "S" AND zcount% < 30000 THEN zcount% = zcount% + 4
IF A$ = "P" AND zcount% = 4 THEN zcount% = zcount% + 3
IF A$ = "E" AND zcount% = 7 THEN zcount% = zcount% + 6
IF A$ = "E" AND zcount% = 19 THEN zcount% = zcount% + 6
IF A$ = "M" AND dcount% = 4 THEN dcount% = dcount% + 3
IF A$ = "C" AND zcount% = 18 THEN zcount% = zcount% + 1
IF A$ = "D" AND dcount% < 10 THEN dcount% = dcount% + 4
IF A$ = "O" AND dcount% = 7 THEN dcount% = dcount% + 6
IF A$ = "D" AND dcount% = 13 THEN dcount% = dcount% + 5
IF A$ = "E" AND dcount% = 18 THEN dcount% = dcount% + 7
IF A$ = "K" AND kcount% < 30000 THEN kcount% = kcount% + 4
IF A$ = "L" AND kcount% = 4 THEN kcount% = kcount% + 3
IF A$ = "O" AND kcount% = 7 THEN kcount% = kcount% + 6
IF A$ = "O" AND kcount% = 18 THEN kcount% = kcount% + 7
IF A$ = "B" AND kcount% = 13 THEN kcount% = kcount% + 5
IF A$ = "V" AND mcount% = 7 THEN mcount% = mcount% + 6
IF A$ = "O" AND mcount% = 4 THEN mcount% = mcount% + 3
IF A$ = "M" AND mcount% < 30000 THEN mcount% = mcount% + 4
IF A$ = "E" AND mcount% = 13 THEN mcount% = mcount% + 5
IF A$ = " " THEN dcount% = 0: zcount% = 0: ncount% = 0: mcount% = 0: kcount% = 0
IF dcount% = 25 THEN BEEP: practicemode$ = "YES": dmode$ = "YES": dcount% = 0: mcount% = 0: kcount% = 0: zcount% = 0: ncount% = 0
IF zcount% = 25 THEN CALL grafxtest
IF mcount% = 18 THEN mcount% = 0: CALL movelist
IF kcount% = 25 THEN BEEP: difficulty = 20: dcount% = 0: kcount% = 0: zcount% = 0: mcount% = 0: ncount% = 0
IF ncount% = 18 THEN
IF dsoundcard$ = "NO" THEN CALL soundtest
IF dsoundcard$ <> "NO" THEN CALL sbsoundtest
END IF

IF A$ = CHR$(27) THEN CALL exitprogram
IF A$ = CHR$(0) + "H" THEN SELCT = SELCT - 1: IF SELCT < 0 THEN SELCT = 0: GOTO 210 ELSE GOTO 210
IF A$ = CHR$(0) + "P" THEN SELCT = SELCT + 1: IF SELCT > 2 THEN SELCT = 2: GOTO 210 ELSE GOTO 210
IF SELCT = 0 AND player = 0 THEN player = 1 ELSE IF SELCT = 0 AND player = 1 THEN player = 0
IF SELCT = 1 AND STYLE = 0 THEN STYLE = 1 ELSE IF SELCT = 1 AND STYLE = 1 THEN STYLE = 0
IF SELCT = 2 AND SPD = 50 THEN SPD = 250 ELSE IF SELCT = 2 AND SPD = 250 THEN SPD = 500 ELSE IF SELCT = 2 AND SPD = 500 THEN SPD = 50
IF SELCT <> 2 THEN GOTO 210
CLS : SCREEN 13
IF player = 0 THEN soundcard$ = "YES"
IF player = 1 THEN soundcard$ = "NO"
IF STYLE = 0 THEN sounds$ = "YES"
IF STYLE = 1 THEN sounds$ = "NO"
END SUB

SUB soundtest
IF dsoundcard$ = "NO" THEN soundcard$ = "NO"
154 CLS : SCREEN 9
LOCATE 2, 2:  COLOR 12: PRINT "Welcome to the secret sound and music test room for losers with no sound cards!"
LOCATE 4, 2: COLOR 10: PRINT "1) ATTACK"
LOCATE 5, 2: COLOR 10: PRINT "2) DING"
LOCATE 6, 2: COLOR 10: PRINT "3) STICK"
LOCATE 7, 2: COLOR 10: PRINT "4) RANDOM"
LOCATE 8, 2: COLOR 10: PRINT "5) NEVERMIND"
LOCATE 10, 2: COLOR 12: INPUT "Enter the number of the sound you want to hear, loser... ", snumber%
IF snumber% = 1 THEN soundeffect "PUNCH": GOTO 154
IF snumber% = 2 THEN soundeffect "DING": GOTO 154
IF snumber% = 3 THEN SOUND 150, .75: GOTO 154
IF snumber% = 4 THEN
DO: RANDOMIZE TIMER
fgh% = INT(RND * 1000) + 50
SOUND fgh%, .3
IF INKEY$ <> "" THEN GOTO 154
LOOP
END IF
IF snumber% = 5 THEN CLS : SCREEN 13: ncount% = 0: EXIT SUB
IF snumber% <> 1 AND snumber% <> 2 AND snumber% <> 3 AND snumber% <> 4 THEN CLS : SCREEN 13: ncount% = 0: zcount% = 0: kcount% = 0: EXIT SUB
END SUB

DEFSNG A-Z
SUB statusbar
COLOR 12
IF dmode$ = "YES" THEN GOTO 66
IF combohits% > 14 THEN combohits% = 14
IF COMBOTIMER% = 0 THEN GOTO 66
IF COMBOTIMER% > 0 AND combohits% > 1 AND hittimer% = 0 THEN oldcombohits% = combohits%: oldcombodamage% = combodamage%: GOTO 67
IF COMBOTIMER% > 0 AND hittimer% = 0 THEN LOCATE 21, 1: PRINT "COMBO -"; oldcombohits%; "HITS,"; (oldcombohits% * 4); "% DAMAGE!": GOTO 67
66 COLOR 4
LOCATE 21, 1: PRINT "                                        "
67 COLOR 4
LOCATE 22, 3: PRINT "You-"
LOCATE 23, 3: PRINT "Him-"
LOCATE 23, 31: PRINT time% / 100; "   "
LOCATE 23, 21: PRINT "Time left-"
LOCATE 22, 21: PRINT "Score-"; score%
LINE (150, 169)-(life% + 50, 174), 4, BF
LINE (150, 177)-(enemylife% + 50, 182), 4, BF
LINE (50, 169)-(life% + 50, 174), 12, BF
LINE (50, 177)-(enemylife% + 50, 182), 12, BF
IF dmode$ = "YES" THEN COLOR 4: LOCATE 21, 1: PRINT hittimer%; firetimer%; ph%; pvv%; eh%; bh%; life%; enemylife%; combodamage%
END SUB

DEFINT A-Z
SUB WriteReg (Reg%, Value%)
OUT &H388, Reg%
FOR x = 0 TO 5
A = INP(&H388)
NEXT x
OUT &H389, Value%
FOR x = 0 TO 34
A = INP(&H388)
NEXT x
END SUB

