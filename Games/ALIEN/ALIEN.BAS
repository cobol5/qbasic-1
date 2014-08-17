'  Alien Invaders Version 2
'  By Andrew Morison
SCREEN 12
speed: CLS
PRINT "Alien Invaders Version 2"
PRINT "~~~~~~~~~~~~~~~~~~~~~~~~"
PRINT "Graphics, Storyline and Code by Andrew Morison"
PRINT "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
PRINT "Please select your computer speed-"
PRINT "1. 386DX to 486SX"
PRINT "2. 486DX or faster"
PRINT "3. Pentium+"
INPUT "What is your computer speed?- ", spd
IF spd < 1 OR spd > 3 GOTO speed

verystart: CLS
x = 300
ax = 100
ay = 300
score = 0
aldown = 0
alenergy = 1500
cursorpos = 4
cursorpos2 = 22
backgame = 0
alfallspd = 1
selgame: CLS
IF newgame = 1 THEN LOCATE 1, 24: PRINT "Please choose your Nee fighter"
IF newgame = 1 THEN LOCATE 2, 24: PRINT "------------------------------"
IF newgame = 1 THEN LOCATE 4, 34: PRINT "Ernie"
IF newgame = 1 THEN LOCATE 6, 34: PRINT "George"
IF newgame = 1 THEN LOCATE 8, 34: PRINT "Bernie"
IF newgame = 1 THEN LOCATE 10, 34: PRINT "Maurice"
IF newgame = 1 THEN LOCATE 12, 34: PRINT "Herbert"
IF newgame = 1 THEN LOCATE 4, 26: PRINT CHR$(26)
playerchoose: a$ = INKEY$
IF newgame = 1 THEN IF cursorpos > 4 THEN IF a$ = CHR$(0) + "H" THEN cursorpos = cursorpos - 2: LOCATE 4, 26: PRINT " ": LOCATE 6, 26: PRINT " ": LOCATE 8, 26: PRINT " ": LOCATE 10, 26: PRINT " ": LOCATE 12, 26: PRINT " ": LOCATE cursorpos, 26:  _
PRINT CHR$(26)
IF newgame = 1 THEN IF cursorpos < 12 THEN IF a$ = CHR$(0) + "P" THEN cursorpos = cursorpos + 2: LOCATE 4, 26: PRINT " ": LOCATE 6, 26: PRINT " ": LOCATE 8, 26: PRINT " ": LOCATE 10, 26: PRINT " ": LOCATE 12, 26: PRINT " ": LOCATE cursorpos, 26:  _
PRINT CHR$(26)
IF newgame = 1 THEN IF a$ = " " OR a$ = CHR$(13) THEN newgame = 0: GOTO startgame
IF newgame = 1 THEN IF a$ = CHR$(27) OR a$ = "q" OR a$ = "Q" GOTO menu
IF newgame = 1 GOTO playerchoose
menu:
CLS
DRAW "bl50 c14 l15 d10 r15 u10 l8 c5 u5 d5 c12 d3 l3 r6 l3 d3 l4 r8"
DRAW "br30 bd3 c7 u8"
CIRCLE (279, 200), 15, 13
DRAW "p13,13 c4 g7 r2 e5 f5 r2 h7 bd g5 e5 f5"
PSET (273, 193), 10
PSET (285, 193), 10
PSET (274, 193), 10
PSET (284, 193), 10
IF getpic = 0 THEN DIM alien(200)
IF getpic = 0 THEN GET (260, 180)-(298, 219), alien
IF getpic = 0 THEN DIM ship(100)
IF getpic = 0 THEN GET (245, 234)-(280, 250), ship
IF getpic = 0 THEN DIM shot(13)
IF getpic = 0 THEN GET (295, 239)-(297, 251), shot
getpic = 1
cursorpos2 = 22
LOCATE 1, 20: PRINT "Alien Invaders of the Planet Nee!"
LOCATE 22, 28: PRINT "N)ew game"
LOCATE 24, 28: PRINT "B)ack to game"
LOCATE 26, 28: PRINT "I)nstructions and story"
LOCATE 28, 28: PRINT "Q)uit"
LOCATE 22, 26: PRINT CHR$(26)
10 a$ = INKEY$
IF cursorpos2 < 28 THEN IF a$ = CHR$(0) + "P" THEN SOUND 1000, .1: cursorpos2 = cursorpos2 + 2: LOCATE 22, 26: PRINT " ": LOCATE 24, 26: PRINT " ": LOCATE 26, 26: PRINT " ": LOCATE 28, 26: PRINT " ": LOCATE cursorpos2, 26: PRINT CHR$(26)
IF cursorpos2 > 22 THEN IF a$ = CHR$(0) + "H" THEN SOUND 1000, .1: cursorpos2 = cursorpos2 - 2: LOCATE 22, 26: PRINT " ": LOCATE 24, 26: PRINT " ": LOCATE 26, 26: PRINT " ": LOCATE 28, 26: PRINT " ": LOCATE cursorpos2, 26: PRINT CHR$(26)
IF cursorpos2 = 22 AND a$ = " " OR a$ = CHR$(13) THEN GOSUB selct: newgame = 1: GOTO verystart
IF cursorpos2 = 24 AND backgame = 1 AND a$ = " " OR a$ = CHR$(13) THEN GOSUB selct: GOTO startgame
IF cursorpos2 = 26 AND a$ = " " OR a$ = CHR$(13) THEN GOSUB selct: GOTO inst
IF cursorpos2 = 28 AND a$ = " " THEN GOSUB selct: GOTO aresure
IF backgame = 1 THEN IF a$ = "b" OR a$ = "B" GOTO startgame
IF a$ = "n" OR a$ = "N" THEN newgame = 1: GOTO verystart
IF a$ = "i" OR a$ = "I" GOTO inst
IF a$ = CHR$(27) OR a$ = "q" OR a$ = "Q" GOTO aresure
GOTO 10
inst:
CLS
LOCATE 10, 1
PRINT "The story-"
PRINT "Your planet is being invaded by 5 aliens that are threatening to destroy the"
PRINT "entire Nee population. Your mission is to destroy the 5 aliens before they"
PRINT "destroy your planet."
PRINT
PRINT "The keys-"
PRINT CHR$(27); " = Move cannon left"
PRINT CHR$(26); " = Move cannon right"
PRINT CHR$(24); " = Move cursor up"
PRINT CHR$(25); " = Move cursor down"
PRINT "Space bar = Shoot"
PRINT "ESC or "; CHR$(34); "q"; CHR$(34); " = Return to main menu"
PRINT "Read the text file for more information."
LOCATE 24, 22: PRINT "* * * Press any key to continue * * *"
ak: a$ = INKEY$: IF a$ = "" GOTO ak
GOTO menu
startgame:
CLS ' The following section sets up the screen with the appropriate writing
    ' and pictures.
IF cursorpos = 4 THEN n$ = "Ernie"
IF cursorpos = 6 THEN n$ = "George"
IF cursorpos = 8 THEN n$ = "Bernie"
IF cursorpos = 10 THEN n$ = "Maurice"
IF cursorpos = 12 THEN n$ = "Herbert"
cursorpos = 4
PUT (x, 430), ship
PUT (ax, ay), alien
LOCATE 1, 1: PRINT "Ready to fire..."
LOCATE 2, 1: PRINT "Aliens down:"; aldown
LOCATE 3, 1: PRINT "Alien's energy:"; alenergy
LOCATE 4, 1: PRINT "<"; n$; ">"
100 a$ = INKEY$
PUT (ax, ay), alien, PSET
IF shoot = 1 THEN IF shx > ax AND shx < ax + 30 AND shy > ay AND shy < ay + 30 THEN score = score + 10: LOCATE 3, 1: alenergy = 1500 - score: PRINT "Alien's energy:"; alenergy
IF alenergy < 1 THEN alenergy = 1500: LOCATE 3, 1: PRINT "Alien's energy:"; alenergy; : aldown = aldown + 1: score = 0: LOCATE 2, 1: PRINT "Aliens down:"; aldown: ay = ay - 50: IF aldown = 0 OR aldown = 2 OR aldown = 4 THEN alfallspd = alfallspd + 1 _
: IF aldown = 5 GOTO finish
IF alie = 0 THEN ax = ax - 3: IF ax < 1 THEN ay = ay + alfallspd: alie = 1: IF ay > 391 GOTO dead
IF alie = 1 THEN ax = ax + 3: IF ax > 590 THEN ay = ay + alfallspd: alie = 0: IF ay > 391 GOTO dead
IF shoot = 0 AND a$ = " " OR a$ = CHR$(13) THEN shx = x + 16: shy = 419: shoot = 1
IF shoot = 1 THEN shy = shy - 2: IF shy < 62 THEN shoot = 0: LOCATE 1, 1: PRINT "Ready to fire...": LOCATE 5, 1: PRINT TAB(80); : prntrload = 0
IF shoot = 1 THEN PUT (shx, shy), shot, PSET
IF shoot = 1 AND prntrload = 0 THEN LOCATE 1, 1: PRINT "Reloading...    ": prntrload = 1
IF x > 10 THEN IF a$ = CHR$(0) + "K" THEN x = x - 10: PUT (x, 430), ship, PSET
IF x < 595 THEN IF a$ = CHR$(0) + "M" THEN x = x + 10: PUT (x, 430), ship, PSET
IF a$ = CHR$(27) OR a$ = "q" OR a$ = "Q" THEN backgame = 1: GOTO menu
IF aldown = 4 GOTO finish
IF spd = 2 THEN FOR brk = 1 TO 50: NEXT brk
IF spd = 3 THEN FOR brk = 1 TO 150: NEXT brk
GOTO 100

aresure:
LOCATE 5, 21: PRINT "Are you sure you want to quit?"
sure: a$ = INKEY$
IF a$ = "y" OR a$ = "Y" THEN END
IF a$ = "n" OR a$ = "N" THEN LOCATE 5, 21: PRINT TAB(51); : GOTO 10
GOTO sure
selct:
FOR snd = 100 TO 300 STEP 10
SOUND snd, .1
NEXT snd
RETURN
finish: CLS
PRINT "Congratulations! You stopped the aliens and saved your planet!": END

dead: CLS
PRINT "One of the aliens reached your planet and killed you all. Game over.": END

