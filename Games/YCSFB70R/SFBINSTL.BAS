SCREEN 0, 0, 0, 0
CLS
PRINT "Entering Install for Stick Fighters Brawl."
PRINT "": PRINT "This Game Requires a 486 and QBASIC."
PRINT "Quick BASIC is Highly Recommended."
PRINT "If You Have Any Problems Please View Readme.txt"
LOCATE 22: PRINT "Press Any Key To Begin..."
6 IF INKEY$ = "" THEN 6
SCREEN 9
WINDOW SCREEN (0, 0)-(100, 100)
FOR wee = 0 TO 50 STEP .1
LINE (wee, 0)-(wee, 100), 1
LINE (100 - wee, 0)-(100 - wee, 100), 1
NEXT wee

FOR wee = 0 TO 100 STEP .1
LINE (0, 0)-(wee, wee), 15, B
LINE (0, 0)-(wee - .2, wee - .2), 1, B
NEXT wee

COLOR 15, 1

FOR wee = 0 TO 1000
PSET (30 + (RND * 40), 48 + (RND * 3)), 15
NEXT wee
       
LINE (0, 0)-(100, 100), 15, B
LOCATE 13, 24
PRINT "Enter Drive Letter to Install to."
1 drv$ = INKEY$
IF drv$ = "" THEN 1

CLS
WINDOW SCREEN (0, 0)-(100, 100)
LINE (0, 0)-(100, 100), 15, B
LOCATE 2, 6
PRINT drv$ + ":"
LINE (5, 3)-(95, 10), 15, B
LOCATE 11, 26
PRINT "Enter Path to Install to."
LOCATE 12, 26
PRINT "EX. \GAMES\SFB"
LOCATE 13, 26
2 INPUT "", path$
IF path$ = "" THEN 2

CLS
WINDOW SCREEN (0, 0)-(100, 100)
LINE (0, 0)-(100, 100), 15, B
LOCATE 2, 6
PRINT drv$ + ":" + path$
LINE (5, 3)-(95, 10), 15, B
LOCATE 12, 19
PRINT "OK to Create C:\Windows\System\SFBPath.dat ?"
LOCATE 13, 19
PRINT "Y=YES N=EXIT INSTALL"
3 wee$ = INKEY$
IF wee$ = "" THEN 3
IF wee$ = "N" OR wee$ = "n" THEN SYSTEM

CLS
WINDOW SCREEN (0, 0)-(100, 100)
LINE (0, 0)-(100, 100), 15, B
LOCATE 2, 6
PRINT drv$ + ":" + path$
LINE (5, 3)-(95, 10), 15, B
LOCATE 12, 19
PRINT "Use Sound Blaster Compatable Sound ?"
LOCATE 13, 19
PRINT "Y=YES N=NO"
5 wee$ = INKEY$
IF wee$ = "" THEN 5
IF wee$ = "N" OR wee$ = "n" THEN SBSound = 0 ELSE SBSound = 1

CLS
WINDOW SCREEN (0, 0)-(100, 100)
LINE (0, 0)-(100, 100), 15, B
LOCATE 2, 6
PRINT drv$ + ":" + path$
LINE (5, 3)-(95, 10), 15, B
LOCATE 13, 24
PRINT "Installing Stick Fighters Brawl..."

'---Installing...
SCREEN 0, 0, 0: CLS
Dif = 5
S = 9
satan = 0
Smear = 0
hp1 = 100
hp2 = 100
grav = .02
delay = 300
offy = 0
detail = 0
sspeed = .01
stage = 2
boby = 0
bobv = .1
sbx = 100
sfx = 250
zom = 10
joy = 0
cap = 0
joyfix = 0
joytest = 0
sense = 50
players = 1
gal = 0
yk = 0
ac1 = 7
bc1 = 8
lc1 = 7
ac2 = 8
bc2 = 7
lc2 = 8
SBTimes% = 3
P1D = 0
P2D = 0
speed = 3

OPEN drv$ + ":" + path$ + "\Config.dat" FOR OUTPUT AS #1
WRITE #1, Dif, S, satan, Smear, hp1, hp2, grav, delay, offy, detail, sspeed, stage, boby, bobv, sbx, sfx, zom, joy, cap, joyfix, joytest, sense, players, gal, yk, ac1, bc1, lc1, ac2, bc2, lc2, SBTimes%, P1D, P2D, spl, speed
CLOSE #1
PRINT "        Spawned "; drv$ + ":" + path$ + "\Config.dat"

OPEN "C:\Windows\System\SFBPath.dat" FOR OUTPUT AS #1
WRITE #1, drv$, path$, SBSound
CLOSE #1
PRINT "        Spawned C:\Windows\System\SFBPath.dat"

SHELL "copy SFB.bas " + drv$ + ":" + path$
SHELL "copy Install.BAT " + drv$ + ":" + path$
SHELL "copy InstallQ.BAT " + drv$ + ":" + path$
SHELL "copy SFBINSTL.bas " + drv$ + ":" + path$
SHELL "copy Readme.TXT " + drv$ + ":" + path$
SHELL "copy SFB.TXT " + drv$ + ":" + path$
SHELL "copy Masscot.pal " + drv$ + ":" + path$
SHELL "copy Masscot1.qbi " + drv$ + ":" + path$
SHELL "copy Masscot2.qbi " + drv$ + ":" + path$
SHELL "copy Title.pal " + drv$ + ":" + path$
SHELL "copy Title1.qbi " + drv$ + ":" + path$
SHELL "copy Title2.qbi " + drv$ + ":" + path$
SHELL "copy Shades.pal " + drv$ + ":" + path$
SHELL "copy Bostrom.pal " + drv$ + ":" + path$
SHELL "copy Bostrom.qbi " + drv$ + ":" + path$
SHELL "copy SFBk.pal " + drv$ + ":" + path$
SHELL "copy sfbk1.qbi " + drv$ + ":" + path$
SHELL "copy sfbk2.qbi " + drv$ + ":" + path$
PRINT "        Install Complete"

4 IF INKEY$ = "" THEN 4
SYSTEM

