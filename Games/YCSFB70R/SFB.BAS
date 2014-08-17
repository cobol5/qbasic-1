REM----------------------------------------------------------------Setup heap
DECLARE FUNCTION DetectCard% ()
DECLARE SUB SETHUES (HUE() AS ANY, SC%, EC%)
DECLARE SUB move ()
DECLARE SUB Paused ()
DECLARE SUB intro1 ()
DECLARE SUB BloodFans ()
DECLARE SUB AI ()
DECLARE SUB Save ()
DECLARE SUB SetDelay ()
DECLARE SUB Title ()
DECLARE SUB P1Animation1 ()
DECLARE SUB P1Animation2 ()
DECLARE SUB P2Animation1 ()
DECLARE SUB P2Animation2 ()
DECLARE SUB ColorSelect ()
DECLARE SUB SB ()
DECLARE SUB SBinit ()
DECLARE SUB SBPLAY1 (Freq%, Wave%, Feedback%, Modl%, Clen%)
DECLARE SUB WriteReg (Reg%, Value%)
DECLARE SUB FX (fxx%, SBTime%, SBTimer%)
COMMON SHARED spl, wee2$, weeb$, fan, LK, sbx, sfx, zom, joy, dk1, dk2, eyes, sense, gal, speed
COMMON SHARED bc1, ac1, lc1, bc2, ac2, lc2, SBSound%, P1d, P2d, SBTimes%, players, cap, yk, akck1, akck2
COMMON SHARED dif, s, satan, smear, hp1, hp2, grav, Delay, offy, detail, sspeed, stage, boby, bobv
COMMON SHARED zoom, xx, yy, fel1, fel2, chrg1, chrg2, r1, r2, rh1, frt1, rh2, frt2, head1, head2
COMMON SHARED h1, h2, v1, v2, wee$, Drv$, Path$, ptx, pty, ptv, POS1, POS2, joyfix, joytest, addx1, addx2
COMMON SHARED firx1, laax, lbax, labx, lbbx, aaax, abax, aabx, abbx, b1x, b2x, h1x
COMMON SHARED firy1, laay, lbay, laby, lbby, aaay, abay, aaby, abby, b1y, b2y, h1y
COMMON SHARED firh1, laah, lbah, labh, lbbh, aaah, abah, aabh, abbh, b1h, b2h, h1h
COMMON SHARED firv1, laav, lbav, labv, lbbv, aaav, abav, aabv, abbv, b1v, b2v, h1v

COMMON SHARED firx2, laax2, lbax2, labx2, lbbx2, aaax2, abax2, aabx2, abbx2, b1x2, b2x2, h2x
COMMON SHARED firy2, laay2, lbay2, laby2, lbby2, aaay2, abay2, aaby2, abby2, b1y2, b2y2, h2y
COMMON SHARED firh2, laah2, lbah2, labh2, lbbh2, aaah2, abah2, aabh2, abbh2, b1h2, b2h2, h2h
COMMON SHARED firv2, laav2, lbav2, labv2, lbbv2, aaav2, abav2, aabv2, abbv2, b1v2, b2v2, h2v

COMMON SHARED bx0, bx1, bx2, bx3, bx4, bx5, bx6, bx7, bx8, bx9
COMMON SHARED bh0, bh1, bh2, bh3, bh4, bh5, bh6, bh7, bh8, bh9
COMMON SHARED by0, by1, by2, by3, by4, by5, by6, by7, by8, by9
COMMON SHARED bv0, bv1, bv2, bv3, bv4, bv5, bv6, bv7, bv8, bv9

COMMON SHARED bxa, bxb, bxc, bxd, bxe, bxf, bxg, bxh, bxi, bxj
COMMON SHARED bha, bhb, bhc, bhd, bhe, bhf, bhg, bhh, bhi, bhj
COMMON SHARED bya, byb, byc, byd, bye, byf, byg, byh, byi, byj
COMMON SHARED bva, bvb, bvc, bvd, bve, bvf, bvg, bvh, bvi, bvj

REM---Set NumLock to ON
DEF SEG = 0
KeyFlags = PEEK(1047)
POKE 1047, KeyFlags OR 32
IF (KeyFlags AND 32) = 0 THEN DEF SEG : DEF SEG = 0
CLEAR
TYPE HUES
RED AS INTEGER
GRN AS INTEGER
BLU AS INTEGER
END TYPE
RANDOMIZE (TIMER)
22 RESTORE: SCREEN 0, 0, 0: CLS
OPEN "C:\Windows\System\SFBPath.dat" FOR INPUT AS #1
INPUT #1, Drv$, Path$, SBSound
CLOSE #1
SHELL Drv$ + ":"
SHELL "cd\"

IF SBSound = 1 THEN SBSound% = 1 ELSE SBSound% = 2
IF SBSound% = 1 THEN CALL SB

wee$ = ""
REDIM starsbrx(1 TO 20)
REDIM starsbry(1 TO 20)
REDIM starsx(1 TO 20)
REDIM starsy(1 TO 20)
REDIM starsltx(1 TO 20)
REDIM starslty(1 TO 20)
REDIM starsclx!(1 TO 5)
REDIM starscly!(1 TO 5)
REDIM statsclr(1 TO 5)
FOR I = 1 TO 5
starsclx!(I) = INT(319 * RND) + 1
starscly!(I) = INT(199 * RND) + 1
starsclr(I) = INT(6 * RND) + 1
NEXT I
FOR I = 1 TO 20
starsbrx(I) = INT(319 * RND) + 1
starsx(I) = INT(319 * RND) + 1
starsltx(I) = INT(319 * RND) + 1
starsbry(I) = INT(199 * RND) + 1
starsy(I) = INT(199 * RND) + 1
starslty(I) = INT(199 * RND) + 1
NEXT I
I = 0: s = 7
REM-----------------------------------------------OPENING SCREEN(S)
DATA "S","t","i","c","k"," ","F","i","g","h","t","e","r","s"," ","B","r","a","w","l"
SCREEN 13
WINDOW SCREEN (0, 0)-(100, 100)
PALBLUE = INT(P(I) / 65536)
PALGREEN = INT((P(I) - RM3 * 65536) MOD 256)
PALBLUE = INT(P(I) - RM3 * 65536 - RM2 * 256)
CALL intro1
IF wee$ <> "" THEN wee$ = "": GOTO 100
CLS
CALL Title
100 CLS : bndx = 0: bndy = 0: SBTime% = 0
REM----------------------------------------------------------Start / Options
REDIM PAL(255) AS HUES
DEF SEG = VARSEG(PAL(0))
BLOAD Drv$ + ":" + Path$ + "\shades.PAL", 0
SETHUES PAL(), 0, 255
colh = 1: col = 33
pnt = 50: pot = 50
WINDOW SCREEN (0, 0)-(100, 100)
ccol = 33
FOR wee = 15 TO 70 STEP .3
ccol = ccol + .2
CIRCLE (50, 50), wee, ccol
NEXT wee

80 wee$ = INKEY$
IF wee$ = CHR$(13) THEN wee$ = " "

IF col < 40 THEN colh = 1
IF col > 62 THEN colh = -1
cc = cc + 1: IF cc = 25 THEN cc = 0: col = col + colh
COLOR col
LOCATE 11, 18: PRINT "Start"
LOCATE 13, 18: PRINT "Options"
LOCATE 15, 18: PRINT "Quit"

IF bndc = 4 THEN bndc = 0 ELSE bndc = 4
bndx = bndx + bndh
bndy = bndy + bndv

LINE (36, pot - 5)-(40, pot + 3), 0, BF
IF pnt > pot THEN pot = pot + .1
IF pnt < pot THEN pot = pot - .1
'Draw Pointer
PSET (37 + ((RND - .5) * 1.5), pot + ((RND - .5) * 1.5)), 35 + (RND * 25)
CIRCLE (37, pot), 1, 45
LINE (38, pot)-(40, pot), 45
PSET (36.7, pot - .6), 60
PSET (36.4, pot - .6), 50

IF wee$ = CHR$(0) + "H" THEN wee$ = "8"
IF wee$ = CHR$(0) + "K" THEN wee$ = "4"
IF wee$ = CHR$(0) + "P" THEN wee$ = "5"
IF wee$ = CHR$(0) + "M" THEN wee$ = "6"
'input
IF wee$ = "8" THEN pnt = pnt - 8: FX 11, SBTime%, SBTimer%
IF wee$ = "5" THEN pnt = pnt + 8: FX 11, SBTime%, SBTimer%
IF wee$ = " " OR wee$ = CHR$(19) THEN IF pnt = 42 THEN 81
IF wee$ = " " OR wee$ = CHR$(19) THEN IF pnt = 50 THEN GOSUB 82
IF wee$ = " " OR wee$ = CHR$(19) THEN IF pnt = 58 THEN CLEAR : SCREEN 0, 0, 0: CLS : CALL SBinit: SYSTEM
IF wee$ = CHR$(27) THEN pnt = 58

IF pnt > 60 THEN pnt = 42
IF pnt < 40 THEN pnt = 58
GOTO 80
81
REM----------------------------------------------------------End Start / Opt.
FX 12, SBTime%, SBTimer%
FOR wee = 0 TO 70 STEP .05
IF wee < 30 THEN LINE (50 - (wee / 2), 50 - (wee / 2))-(50 + (wee / 2), 50 + (wee / 2)), 0, BF
CIRCLE (50, 50), wee, 0
NEXT wee
REM---
start = 0
OPEN Drv$ + ":" + Path$ + "\Config.dat" FOR INPUT AS #1
INPUT #1, dcl
CLOSE #1

mat = 1: round = 1
666
REM------------------------------------------------------------------DEFAULTS
RANDOMIZE (TIMER)
flashx = 5000
decap = 0
gush = 0
headh = 0
headv = 0
headx = 5000
bx0 = 5000
bx1 = 5000
bx2 = 5000
bx3 = 5000
bx4 = 5000
bx5 = 5000
bx6 = 5000
bx7 = 5000
bx8 = 5000
bx9 = 5000
bxa = 5000
bxb = 5000
bxc = 5000
bxd = 5000
bxe = 5000
bxf = 5000
bxg = 5000
bxh = 5000
bxi = 5000
bxj = 5000
by0 = 95
by1 = 95
by2 = 95
by3 = 95
by4 = 95
by5 = 95
by6 = 95
by7 = 95
by8 = 95
by9 = 95
bya = 95
byb = 95
byc = 95
byd = 95
bye = 95
byf = 95
byg = 95
byh = 95
byi = 95
byj = 95
laah = 0
lbah = 0
labh = 0
lbbh = 0
aaah = 0
abah = 0
aabh = 0
abbh = 0
b1h = 0
b2h = 0
h1h = 0
laav = 0
lbav = 0
labv = 0
lbbv = 0
aaav = 0
abav = 0
aabv = 0
abbv = 0
b1v = 0
b2v = 0
h1v = 0

laah2 = 0
lbah2 = 0
labh2 = 0
lbbh2 = 0
aaah2 = 0
abah2 = 0
aabh2 = 0
abbh2 = 0
b1h2 = 0
b2h2 = 0
h2h = 0
laav2 = 0
lbav2 = 0
labv2 = 0
lbbv2 = 0
aaav2 = 0
abav2 = 0
aabv2 = 0
abbv2 = 0
b1v2 = 0
b2v2 = 0
h2v = 0
head1 = 0
head2 = 0
jump1 = 0
jump2 = 0
'Starting X/Y (Player 1)
p1sx = 25: p1sy = 10
r1 = 0
'body
b1x = p1sx: b1y = p1sy
b2x = p1sx: b2y = p1sy
'left leg
laax = p1sx: laay = p1sy
labx = p1sx: laby = p1sy
'right leg
lbax = p1sx: lbay = p1sy
lbbx = p1sx: lbby = p1sy
'right arm
aaax = p1sx: aaay = p1sy
aabx = p1sx: aaby = p1sy
'left arm
abax = p1sx: abay = p1sy
abbx = p1sx: abby = p1sy

'Starting X/Y (Player 2)
r2 = 1
p2sx = 75: p2sy = 10
'body
b1x2 = p2sx: b1y2 = p2sy
b2x2 = p2sx: b2y2 = p2sy
'left leg
laax2 = p2sx: laay2 = p2sy
labx2 = p2sx: laby2 = p2sy
'right leg
lbax2 = p2sx: lbay2 = p2sy
lbbx2 = p2sx: lbby2 = p2sy
'right arm
aaax2 = p2sx: aaay2 = p2sy
aabx2 = p2sx: aaby2 = p2sy
'left arm
abax2 = p2sx: abay2 = p2sy
abbx2 = p2sx: abby2 = p2sy

h1 = 0: v1 = 0: h2 = 0: v2 = 0

OPEN Drv$ + ":" + Path$ + "\Config.dat" FOR INPUT AS #1
INPUT #1, temp, s, satan, smear, hp1, hp2, grav, Delay, offy, detail, sspeed, stage, boby, bobv, sbx, sfx, zom, joy, cap, joyfix, joytest, sense, players, gal, yk, ac1, bc1, lc1, ac2, bc2, lc2, SBTimes%, P1d, P2d, spl, speed
CLOSE #1
IF start = 0 THEN dif = dcl: start = 1
pty = 95: ptv = 0: ptx = 50 + ((RND - .5) * 20)
IF stage = 7 THEN sspeed = 1
SBTimer% = 0
SBTime% = SBTimes%
fan1 = RND * 10
fan2 = RND * 10
fan = 0
lbx = 50
lbh = 0
beam = -100
ssx = -20
wavy = 110
wavh = 0
conh = 0
conc = 0
REM--------------------------------------------------------------END DEFAULTS
IF stage <> 3 THEN smear = 0
chrg1 = 0: chrg2 = 0: POS1 = 0: POS2 = 0: fire1 = 0: fire2 = 0
IF players = 1 AND say$ = "Player 1 Wins The Match" THEN dif = dif + 1
IF match = 1 THEN match = 0: win1 = 0: win2 = 0: say = 0
IF players = 2 AND cap = 1 THEN cap = 0
IF cap = 1 THEN joyfix = 0: joy = 1
REM-----------------------------------------------Joystick Calibration
IF joy = 0 OR cali = 1 OR cap = 1 THEN 36
LOCATE 1, 1
PRINT "Center Joystick(s) and press SpaceBar"
34 IF INKEY$ <> " " THEN 34
cali = 1
x1m = STICK(0): 'Joystick 1
y1m = STICK(1)
x2m = STICK(2): 'Joystick 2
y2m = STICK(3)
IF joyfix = 0 THEN 36
IF x1 < 130 THEN x1 = x1 + 256: '-----------------JoyStick Correction
IF y1 < 130 THEN y1 = y1 + 256
IF x1 < 130 THEN x2 = x2 + 256
IF y2 < 130 THEN y2 = y2 + 256
36

say = 100
say$ = "Matchinfo"

2 'Start Loop
REM-----------------------------------------------INPUT
wee$ = INKEY$
wee2$ = ""
weeb$ = ""
IF wee$ = "2" THEN wee$ = "5"
IF spl = 1 THEN IF wee$ = "e" OR wee$ = "E" THEN wee$ = ""
IF wee$ = "Û" THEN GOSUB 108
REM-----------------------------------------------Joystick
IF joy = 0 THEN 35
x1 = STICK(0): 'Joystick 1
y1 = STICK(1)
x2 = STICK(2): 'Joystick 2
IF cap = 0 THEN y2 = STICK(3)

IF joyfix = 0 THEN 35
IF x1 < 130 THEN x1 = x1 + 256: '-----------------JoyStick Correction
IF y1 < 130 THEN y1 = y1 + 256
IF x2 < 130 THEN x2 = x2 + 256
IF y2 < 130 THEN y2 = y2 + 256
35
REM-----------------------------------------------Arrow Keys
IF wee$ = CHR$(0) + "H" THEN wee$ = "8"
IF wee$ = CHR$(0) + "K" THEN wee$ = "4"
IF wee$ = CHR$(0) + "P" THEN wee$ = "5"
IF wee$ = CHR$(0) + "M" THEN wee$ = "6"

REM-----------------------------------------------Debug Keys
IF wee$ = "z" THEN wee2$ = "5"
IF wee$ = "x" THEN weeb$ = "W"
REM-----------------------------------------------Command Keys
IF wee$ = "p" OR wee$ = "P" THEN CALL Paused
IF wee$ = CHR$(27) THEN GOSUB 18
REM-----------------------------------------------JoyStick Input
IF joy = 0 OR cap = 1 THEN 38
x1d = x1 - x1m
y1d = y1 - y1m
x2d = x2 - x2m
y2d = y2 - y2m
IF x1d < -sense THEN wee$ = "4"
IF x1d > sense THEN wee$ = "6"
IF y1d < -sense THEN wee$ = "8"
IF y1d > sense THEN wee$ = "5"
IF x1d < -sense AND y1d < -sense THEN wee$ = "7"
IF x1d > sense AND y1d < -sense THEN wee$ = "9"

IF x2d < -sense THEN wee2$ = "4"
IF x2d > sense THEN wee2$ = "6"
IF y2d < -sense THEN wee2$ = "8"
IF y2d > sense THEN wee2$ = "5"
IF x2d < -sense AND y2d < -sense THEN wee2$ = "7"
IF x2d > sense AND y2d < -sense THEN wee2$ = "9"
38
REM-----------------------------------------------Capcom Pad Input
IF cap = 0 THEN 66
IF x1 < 40 THEN wee$ = "4"
IF x1 > 40 AND x1 < 80 THEN wee$ = "6"
IF y1 < 40 THEN wee$ = "8"
IF y1 > 40 AND y1 < 80 THEN wee$ = "5"
IF x1 < 40 AND y1 < 40 THEN wee$ = "7"
IF x1 < 40 AND y1 > 40 AND y1 < 80 THEN wee$ = "9"
IF STRIG(0) = -1 THEN wee$ = "Q"
IF STRIG(2) = -1 THEN wee$ = "A"
IF STRIG(4) = -1 THEN wee$ = "W"
IF STRIG(6) = -1 THEN wee$ = "S"
IF x2 < 40 THEN wee$ = "E"
66

CALL AI

IF spl = 1 THEN IF weeb$ = "e" OR weeb$ = "E" THEN weeb$ = ""

REM-----------------------------------------------Pre-Graphic Calculations
REM---Control of Sound Wait
IF SBTimer% > 0 THEN SBTimer% = SBTimer% - 1

REM---Spawn Dragging Sound
IF POS1 = 9 AND stage <> 12 AND b2y > 93 THEN IF h1 < -.1 OR h1 > .1 THEN FX 5, SBTime%, SBTimer%
IF POS2 = 9 AND stage <> 12 AND b2y2 > 93 THEN IF h2 < -.1 OR h2 > .1 THEN FX 5, SBTime%, SBTimer%

REM---Disable Player When Down
IF POS1 = 9 OR POS1 = 10 THEN wee$ = "": llc1 = lc1: light1 = 0
IF POS2 = 9 OR POS2 = 10 THEN weeb$ = "": LK = 0: llc2 = lc2: light2 = 0

REM---Spikes on Stage 5
IF stage = 5 AND b2y > 0 THEN IF b2x < -55 OR b2x > 155 THEN v1 = 0: hp1 = hp1 - 1: blood2 = 1: bl2x = b2x: bl2y = h1y + (RND * 20): chrg1 = chrg1 - 2
IF stage = 5 AND b2y > 0 THEN IF b2x2 < -55 OR b2x2 > 155 THEN v2 = 0: hp2 = hp2 - 1: blood1 = 1: bl1x = b2x2: bl1y = h2y + (RND * 20): chrg2 = chrg2 - 2

REM---Spikes on Stage 6
IF pty > 102 THEN ptv = ptv - .01
IF pty < 100 THEN ptv = ptv + .001
IF pty < 95 THEN pty = 95
pty = pty + ptv
IF pty > 100 THEN ptx = 50 + ((RND - .5) * 210)
IF stage = 6 AND pty < 99 AND b2x - 3 < ptx AND b2x + 3 > ptx AND lbby > 93 THEN chrg1 = chrg1 - 1: hp1 = hp1 - 1: blood2 = 1: bl2x = ptx + ((RND - .5) * 4): bl2y = 93 - (RND * 3)
IF stage = 6 AND pty < 99 AND b2x2 - 3 < ptx AND b2x2 + 3 > ptx AND lbby2 > 93 THEN chrg2 = chrg2 - 1: hp2 = hp2 - 1: blood1 = 1: bl1x = ptx + ((RND - .5) * 4): bl1y = 93 - (RND * 3)

REM---Laser Beam Stage 9
IF stage = 9 AND beam > 75 AND b2x > lbx - 4 AND b2x < lbx + 4 AND lbby > 25 THEN chrg1 = chrg1 - 2: hp1 = hp1 - 2: blood2 = 1: bl2x = b2x + ((RND - .5) * 4): bl2y = b2y - (RND * 10): v1 = RND / -2: fri1 = 1
IF stage = 9 AND beam > 75 AND b2x2 > lbx - 4 AND b2x2 < lbx + 4 AND lbby2 > 25 THEN chrg2 = chrg2 - 2: hp2 = hp2 - 2: blood1 = 1: bl1x = b2x2 + ((RND - .5) * 4): bl1y = b2y2 - (RND * 10): v2 = RND / -2: fri2 = 1
              
REM---Fan Stage 11
IF stage = 11 AND fan = 1 AND b2y > 50 AND b2x > 35 AND b2x < 65 THEN v1 = v1 - .02
IF stage = 11 AND fan = 1 AND b2y2 > 50 AND b2x2 > 35 AND b2x2 < 65 THEN v2 = v2 - .02

REM---Conveyer Stage 13
IF stage = 13 AND b2x > -55 AND b2x < 45 AND conh <> 0 THEN IF laby > 94 OR lbby > 93 THEN addx1 = addx1 - conh
IF stage = 13 AND b2x > 55 AND b2x < 155 AND conh <> 0 THEN IF laby > 94 OR lbby > 93 THEN addx1 = addx1 + conh

IF stage = 13 AND b2x2 > -55 AND b2x2 < 45 AND conh <> 0 THEN IF laby2 > 94 OR lbby2 > 93 THEN addx2 = addx2 - conh
IF stage = 13 AND b2x2 > 55 AND b2x2 < 155 AND conh <> 0 THEN IF laby2 > 94 OR lbby2 > 93 THEN addx2 = addx2 + conh

REM---Move Head the Right Way
IF headx < 5000 THEN headx = headx + headh: heady = heady + headv: headv = headv + grav

REM---Face Players Body the Right Way Part-A (For Decap.)
IF r1 = 1 THEN b1x = b2x - (b1x - b2x)
IF r2 = 1 THEN b1x2 = b2x2 - (b1x2 - b2x2)

REM---Pop Off Head
IF decap = 1 THEN hbc = hbc1: head1 = 1: decap = 0: gush = 1: headx = h1x: heady = h1y: headh = RND - .5: headv = -1 - RND
IF decap = 2 THEN hbc = hbc2: head2 = 1: decap = 0: gush = 2: headx = h2x: heady = h2y: headh = RND - .5: headv = -1 - RND

REM---Spawn Blood from Neck
IF gush = 1 AND b1y < 90 THEN blood1 = 1: blood2 = 1: bl1x = b1x: bl1y = b1y: bl2x = bl1x: bl2y = bl1y
IF gush = 2 AND b1y2 < 90 THEN blood1 = 1: blood2 = 1: bl1x = b1x2: bl1y = b1y2: bl2x = bl1x: bl2y = bl1y

REM---Face Players Body the Right Way Part-B
IF r1 = 1 THEN b1x = b2x + (b2x - b1x)
IF r2 = 1 THEN b1x2 = b2x2 + (b2x2 - b1x2)

REM---Make Head Squirt
IF heady > 50 AND heady < 95 THEN blood1 = 1: bl1x = headx: bl1y = heady

REM---Blood Stuff
IF blood1 = 1 AND by0 > 90 THEN bx0 = bl1x: by0 = bl1y: bv0 = -RND: bh0 = RND - .5
IF blood1 = 1 AND by1 > 90 THEN bx1 = bl1x: by1 = bl1y: bv1 = -RND * 2: bh1 = RND - .5
IF blood1 = 1 AND by2 > 90 THEN bx2 = bl1x: by2 = bl1y: bv2 = -RND: bh2 = RND - .5
IF blood1 = 1 AND by3 > 90 THEN bx3 = bl1x: by3 = bl1y: bv3 = -RND * 2: bh3 = RND - .5
IF blood1 = 1 AND by4 > 90 THEN bx4 = bl1x: by4 = bl1y: bv4 = -RND: bh4 = RND - .5
IF gal = 0 THEN 90
IF blood1 = 1 AND bya > 90 THEN bxa = bl1x: bya = bl1y: bva = -RND: bha = RND - .5
IF blood1 = 1 AND byb > 90 THEN bxb = bl1x: byb = bl1y: bvb = -RND * 3: bhb = RND - .5
IF blood1 = 1 AND byc > 90 THEN bxc = bl1x: byc = bl1y: bvc = -RND: bhc = RND - .5
IF blood1 = 1 AND byd > 90 THEN bxd = bl1x: byd = bl1y: bvd = -RND * 3: bhd = RND - .5
IF blood1 = 1 AND bye > 90 THEN bxe = bl1x: bye = bl1y: bve = -RND: bhe = RND - .5
90
IF blood2 = 1 AND by5 > 90 THEN bx5 = bl2x: by5 = bl2y: bv5 = -RND: bh1 = RND - .5
IF blood2 = 1 AND by6 > 90 THEN bx6 = bl2x: by6 = bl2y: bv6 = -RND * 2: bh2 = RND - .5
IF blood2 = 1 AND by7 > 90 THEN bx7 = bl2x: by7 = bl2y: bv7 = -RND: bh3 = RND - .5
IF blood2 = 1 AND by8 > 90 THEN bx8 = bl2x: by8 = bl2y: bv8 = -RND * 2: bh4 = RND - .5
IF blood2 = 1 AND by9 > 90 THEN bx9 = bl2x: by9 = bl2y: bv9 = -RND: bh5 = RND - .5
IF gal = 0 THEN 91
IF blood2 = 1 AND byf > 90 THEN bxf = bl2x: byf = bl2y: bvf = -RND: bhf = RND - .5
IF blood2 = 1 AND byg > 90 THEN bxg = bl2x: byg = bl2y: bvg = -RND * 3: bhg = RND - .5
IF blood2 = 1 AND byh > 90 THEN bxh = bl2x: byh = bl2y: bvh = -RND: bhh = RND - .5
IF blood2 = 1 AND byi > 90 THEN bxi = bl2x: byi = bl2y: bvi = -RND * 3: bhi = RND - .5
IF blood2 = 1 AND byj > 90 THEN bxj = bl2x: byj = bl2y: bvj = -RND: bhj = RND - .5
91
blood1 = 0: blood2 = 0
IF by0 > 95 THEN by0 = 95: bx0 = 5000: bv0 = 0: bh0 = 0
IF by1 > 95 THEN by1 = 95: bx1 = 5000: bv1 = 0: bh1 = 0
IF by2 > 95 THEN by2 = 95: bx2 = 5000: bv2 = 0: bh2 = 0
IF by3 > 95 THEN by3 = 95: bx3 = 5000: bv3 = 0: bh3 = 0
IF by4 > 95 THEN by4 = 95: bx4 = 5000: bv4 = 0: bh4 = 0
IF by5 > 95 THEN by5 = 95: bx5 = 5000: bv5 = 0: bh5 = 0
IF by6 > 95 THEN by6 = 95: bx6 = 5000: bv6 = 0: bh6 = 0
IF by7 > 95 THEN by7 = 95: bx7 = 5000: bv7 = 0: bh7 = 0
IF by8 > 95 THEN by8 = 95: bx8 = 5000: bv8 = 0: bh8 = 0
IF by9 > 95 THEN by9 = 95: bx9 = 5000: bv9 = 0: bh9 = 0

IF gal = 0 THEN 89
IF bya > 95 THEN bya = 95: bva = 0: bha = 0: IF bxa < 2000 THEN bxa = 5000: FX 13, SBTime%, SBTimer%
IF byb > 95 THEN byb = 95: bvb = 0: bhb = 0: IF bxb < 2000 THEN bxb = 5000: FX 13, SBTime%, SBTimer%
IF byc > 95 THEN byc = 95: bvc = 0: bhc = 0: IF bxc < 2000 THEN bxc = 5000: FX 13, SBTime%, SBTimer%
IF byd > 95 THEN byd = 95: bvd = 0: bhd = 0: IF bxd < 2000 THEN bxd = 5000: FX 13, SBTime%, SBTimer%
IF bye > 95 THEN bye = 95: bve = 0: bhe = 0: IF bxe < 2000 THEN bxe = 5000: FX 13, SBTime%, SBTimer%
IF byf > 95 THEN byf = 95: bvf = 0: bhf = 0: IF bxf < 2000 THEN bxf = 5000: FX 13, SBTime%, SBTimer%
IF byg > 95 THEN byg = 95: bvg = 0: bhg = 0: IF bxg < 2000 THEN bxg = 5000: FX 13, SBTime%, SBTimer%
IF byh > 95 THEN byh = 95: bvh = 0: bhh = 0: IF bxh < 2000 THEN bxh = 5000: FX 13, SBTime%, SBTimer%
IF byi > 95 THEN byi = 95: bvi = 0: bhi = 0: IF bxi < 2000 THEN bxi = 5000: FX 13, SBTime%, SBTimer%
IF byj > 95 THEN byj = 95: bvj = 0: bhj = 0: IF bxj < 2000 THEN bxj = 5000: FX 13, SBTime%, SBTimer%
89

CALL BloodFans

REM---Stop Glowing if Dead
IF hp1 = 0 OR hp2 = 0 THEN light1 = 0: light2 = 0

REM---Max Horizontal Speed
IF ABS(h1) > 5 THEN h1 = h1 / 1.5
IF ABS(h2) > 5 THEN h2 = h2 / 1.5

REM---Fire Balls Hit Each Other
IF fire1 > 0 AND fire2 > 0 AND firx1 > firx2 - 7 AND firx1 < firx2 + 7 AND firy1 > firy2 - 7 AND firy1 < firy2 + 7 THEN fire1 = fire1 / 1.5: fire2 = fire2 / 1.5: flashx = (firx1 + firx2) / 2: flashy = (firy1 + firy2) / 2

REM---Fire Ball Hits fan
IF stage = 8 AND fan = 1 AND fire1 > 0 AND firy1 > 65 THEN IF firx1 < -55 OR firx1 > 155 THEN flashx = firx1: flashy = firy1: fire1 = fire1 / 1.5
IF stage = 8 AND fan = 1 AND fire2 > 0 AND firy2 > 65 THEN IF firx2 < -55 OR firx2 > 155 THEN flashx = firx2: flashy = firy2: fire2 = fire2 / 1.5

REM---Hit Ground or fall in pit
IF stage <> 2 AND b2y > 96 THEN b2y = 95
IF stage <> 2 AND b2y2 > 96 THEN b2y2 = 95

REM---Forground/Background Movement
sbx = sbx - (sspeed * 4)
sfx = sfx - (sspeed * 10)
IF boby < 0 THEN bobv = bobv + .001
IF boby > 0 THEN bobv = bobv - .001
boby = boby + bobv
IF boby = 0 THEN boby = .001

REM---Keep Legs From Stretching Down
IF laay > b2y + 10 THEN laay = b2y + 10
IF laby > b2y + 10 THEN laby = b2y + 10
IF lbay > b2y + 10 THEN lbay = b2y + 10
IF lbby > b2y + 10 THEN lbby = b2y + 10

IF laay2 > b2y2 + 10 THEN laay2 = b2y2 + 10
IF laby2 > b2y2 + 10 THEN laby2 = b2y2 + 10
IF lbay2 > b2y2 + 10 THEN lbay2 = b2y2 + 10
IF lbby2 > b2y2 + 10 THEN lbby2 = b2y2 + 10

IF stage = 2 THEN 122
IF laay > 97 THEN laay = 97
IF laby > 97 THEN laby = 97
IF lbay > 97 THEN lbay = 96
IF lbby > 97 THEN lbby = 96

IF laay2 > 97 THEN laay2 = 97
IF laby2 > 97 THEN laby2 = 97
IF lbay2 > 97 THEN lbay2 = 96
IF lbby2 > 97 THEN lbby2 = 96
122
REM---Shocking
IF frc = 9 THEN frc = 15 ELSE frc = 9
IF fri1 = 0 THEN 120
fri1 = 0
hbc1 = frc
lac1 = frc
llc1 = frc
rac1 = frc
rlc1 = frc
FX 19, SBTime%, SBTimer%
120
IF fri2 = 0 THEN 121
fri2 = 0
hbc2 = frc
lac2 = frc
llc2 = frc
rac2 = frc
rlc2 = frc
FX 19, SBTime%, SBTimer%
121

REM---Midpoint of P1 & P2
xx = (b2x + b2x2) / 2
yy = (b2y + b2y2) / 2

REM---Push Away
IF b2x > b2x2 - 3 AND b2x < b2x2 AND b2y > b2y2 - 5 AND b2y < b2y2 + 5 THEN h1 = h1 - .05
IF b2x > b2x2 AND b2x < b2x2 + 3 AND b2y > b2y2 - 5 AND b2y < b2y2 + 5 THEN h1 = h1 + .05
IF b2x2 > b2x - 3 AND b2x2 < b2x AND b2y2 > b2y - 5 AND b2y2 < b2y + 5 THEN h2 = h2 - .05
IF b2x2 > b2x AND b2x2 < b2x + 3 AND b2y2 > b2y - 5 AND b2y2 < b2y + 5 THEN h2 = h2 + .05

REM---Air-Kick Stuff
IF lbby > 90 THEN akck1 = 0
IF lbby2 > 90 THEN akck2 = 0

REM---Ring Out Stuff
IF POS1 = 9 AND stage = 2 THEN 71
IF b2x < -60 THEN b2x = -60: h1 = .1: 'Keep you in ring
IF b2x > 160 THEN b2x = 160: h1 = -.1
IF stage = 8 THEN 71
IF b2x < -45 THEN h1 = h1 + .01: 'Push you in ring
IF b2x > 145 THEN h1 = h1 - .01
71
IF POS2 = 9 AND stage = 2 THEN 72
IF b2x2 < -60 THEN b2x2 = -60: h2 = .1: 'Keep you in ring
IF b2x2 > 160 THEN b2x2 = 160: h2 = -.1
IF stage = 8 THEN 72
IF b2x2 < -45 THEN h2 = h2 + .01: 'Push you in ring
IF b2x2 > 145 THEN h2 = h2 - .01
72
REM---Camera Stuff
zoomx = ABS(b2x - b2x2): zoomx = zoomx * -1
zoomy = ABS(b2y - b2y2): zoomy = zoomy * -1
zoom = zoomx + zoomy
yy = yy - offy

REM---Can't Kick When Down
IF wee$ = "a" OR wee$ = "A" THEN IF POS1 > 8 THEN wee$ = ""
IF weeb$ = "a" OR weeb$ = "A" THEN IF POS2 > 8 THEN weeb$ = ""

REM---Special Moves Off (Lightning Kick)
IF spl = 1 THEN light1 = 0: light2 = 0

REM-----------------------------------------------PAGE FLIPPING
IF anisp = 1 THEN 127
IF ap = 0 THEN ap = 1: bp = 0 ELSE ap = 0: bp = 1
SCREEN s, , ap, bp
FOR wee = 0 TO Delay: NEXT wee
127

REM-----------------------------------------------START GRAPHICS
anisp = 0
IF ani > 0 THEN ani = ani + 1
IF ani > speed THEN ani = 1 ELSE anisp = 1
IF speed = 0 THEN anisp = 0 ELSE IF ani = 0 THEN ani = 1

REM---Skip Animation
IF anisp = 1 THEN 124

REM-----------------------------------------------Backgrounds
WINDOW SCREEN (xx - zom + zoom, yy - zom + zoom)-(xx + zom - zoom, yy + zom - zoom)
t = t + 1: IF t < smear THEN 99 ELSE t = 0
LINE (xx - zom + zoom, yy - zom + zoom)-(xx + zom - zoom, yy + zom - zoom), 0, BF
99
IF stage <> 1 THEN 14: '----------------------------------------------Stage 1
REM-----------------------------------------------Draw stars
IF detail = 0 THEN 15
WINDOW SCREEN (0, 0)-(320, 300)
FOR I = 1 TO 5
PSET (starsclx!(I), starscly!(I)), 1
NEXT I

FOR I = 1 TO 20
PSET (starsbrx(I), starsbry(I)), 15
PSET (starsx(I), starsy(I)), 7
PSET (starsltx(I), starslty(I)), 8
NEXT I

FOR I = 1 TO 5
starsclx!(I) = starsclx!(I) - .1
IF starsclx!(I) < 0 THEN starsclx!(I) = 320 + starsclx!(I)
NEXT I

FOR I = 1 TO 20
starsbrx(I) = starsbrx(I) - (sspeed * 4)
starsx(I) = starsx(I) - (sspeed * 2)
starsltx(I) = starsltx(I) - sspeed
IF starsbrx(I) < 0 THEN starsbrx(I) = 320 + starsbrx(I)
IF starsx(I) < 0 THEN starsx(I) = 320 + starsx(I)
IF starsltx(I) < 0 THEN starsltx(I) = 320 + starsltx(I)
NEXT I
REM-----------------------------------------End Stars
15
WINDOW SCREEN (xx - zom + zoom, yy - zom + zoom)-(xx + zom - zoom, yy + zom - zoom)
LINE (xx - zom + zoom, 50)-(xx + zom - zoom, yy + zom - zoom), 1, BF
REM---Background scrolling
'island (Background)
LINE (sbx, 54)-(sbx + 18, 56), 6, BF
LINE (sbx - 3, 54.8)-(sbx + 20, 55.2), 6, BF
LINE (sbx + 5, 30)-(sbx + 6, 54.5), 4
LINE (sbx + 5, 30)-(sbx + 8, 54.5), 4
LINE (sbx + 6, 54.5)-(sbx + 8, 54.5), 4
PAINT (sbx + 7, 53), 4, 4
CIRCLE (sbx + 4, 32), .5, 6
CIRCLE (sbx + 5, 34), .6, 6
CIRCLE (sbx + 6, 33), .5, 6
LINE (sbx + 5, 30)-(sbx + 1, 33), 2
LINE (sbx + 5, 30)-(sbx + 2, 35), 2
LINE (sbx + 5, 30)-(sbx + 4, 38), 2
LINE (sbx + 5, 30)-(sbx + 6, 36), 2
LINE (sbx + 5, 30)-(sbx + 8, 37), 2
LINE (sbx + 5, 30)-(sbx + 9, 32), 2

REM---Plat
LINE (-60, 101 + boby)-(160, 101 - boby), 8
LINE (-60, 101 + boby)-(-60 + (boby / 7), 110), 8
LINE (160, 101 - boby)-(160 + (boby / 7), 110), 8
LINE (-60 + (boby / 7), 110)-(160 + (boby / 7), 110), 8
PAINT (0, 104), 8, 8
PAINT (50, 104), 8, 8
PAINT (100, 104), 8, 8

LINE (-60, 101 + boby)-(-55, 90 + boby), 8
LINE (160, 101 - boby)-(155, 90 - boby), 8
LINE (-55, 90 + boby)-(155, 90 - boby), 8

PAINT (-55, 95 + boby), 7, 8
PAINT (0, 95 + (boby / 2)), 7, 8
PAINT (50, 94), 7, 8
PAINT (100, 95 - (boby / 2)), 7, 8
PAINT (155, 95 - boby), 7, 8

REM--------Slide
IF b2x < 25 AND laby > 94 THEN IF boby > 0 THEN h1 = h1 - .01 ELSE h1 = h1 + .01
IF b2x > 75 AND laby > 94 THEN IF boby > 0 THEN h1 = h1 - .01 ELSE h1 = h1 + .01
14

IF stage <> 2 THEN 8: '-----------------------------------------------Stage 2
LINE (xx - zom + zoom, 130)-(xx + zom - zoom, yy + zom - zoom), 4, BF
LINE (xx - zom + zoom, 123)-(xx + zom - zoom, 128), 4, BF
LINE (xx - zom + zoom, 120)-(xx + zom - zoom, 121), 4, BF
LINE (xx - zom + zoom, 118.5)-(xx + zom - zoom, 118.5), 4
IF satan = 0 THEN 59
LINE (sbx + 2, 122 - boby)-(sbx, 135 - boby), 4
LINE (sbx + 2.2, 122 - boby)-(sbx + .2, 135 - boby), 0
LINE (sbx - 2, 125 - boby)-(sbx + 6, 126 - boby), 4
LINE (sbx - 2, 125.2 - boby)-(sbx + 6, 126.2 - boby), 0

LINE (sbx + 2 - 90, 122 + boby)-(sbx - 90, 135 + boby), 4
LINE (sbx + 2.2 - 90, 122 + boby)-(sbx + .2 - 90, 135 + boby), 0
LINE (sbx - 2 - 90, 125 + boby)-(sbx + 6 - 90, 126 + boby), 4
LINE (sbx - 2 - 90, 125.2 + boby)-(sbx + 6 - 90, 126.2 + boby), 0

LINE (sbx + 2 + 140, 127 + boby)-(sbx + 140, 140 + boby), 4
LINE (sbx + 2.2 + 140, 127 + boby)-(sbx + .2 + 140, 140 + boby), 0
LINE (sbx - 2 + 140, 132 + boby)-(sbx + 6 + 140, 131 + boby), 4
LINE (sbx - 2 + 140, 132.2 + boby)-(sbx + 6 + 140, 131.2 + boby), 0

LINE (sbx + 2 - 150, 122 - 5 - boby)-(sbx - 150, 130 - boby), 4
LINE (sbx + 2.2 - 150, 122 - 5 - boby)-(sbx + .2 - 150, 130 - boby), 0
LINE (sbx - 2 - 150, 120 - boby)-(sbx + 6 - 150, 121 - boby), 4
LINE (sbx - 2 - 150, 120.2 - boby)-(sbx + 6 - 150, 121.2 - boby), 0
59
LINE (-60, 90.5)-(160, 100), 1, BF
LINE (45, 70)-(55, 91), 3, BF
LINE (25, 75)-(30, 91), 3, BF
LINE (70, 75)-(75, 91), 3, BF
LINE (xx - zom + zoom, 150)-(xx + zom - zoom, 163), 1, BF
LINE (xx - zom + zoom, 160)-(xx + zom - zoom, 160), 0
LINE (-20, 103)-(-10, 155), 8, BF
LINE (110, 103)-(120, 155), 8, BF
IF detail = 0 THEN 8
LINE (-18, 103)-(-12, 103), 0
LINE (118, 103)-(112, 103), 0
LINE (-18, 155)-(-12, 156), 8, BF
LINE (118, 155)-(112, 156), 8, BF
PSET (45, 70), 0
PSET (55, 70), 0
PSET (25, 75), 0
PSET (75, 75), 0
PSET (30, 75), 0
PSET (70, 75), 0
LINE (-60, 98)-(160, 98), 0
LINE (-61, 98)-(-55, 90), 0
LINE (161, 98)-(155, 90), 0
PAINT (-58, 91.5), 0, 0
PAINT (158, 91.5), 0, 0
CIRCLE (50.1, 80), 4.5, 0
IF satan = 0 THEN 8
LINE (47, 76)-(50, 86), 0
LINE (53, 76)-(50, 86), 0
LINE (47, 76)-(54, 82), 0
LINE (53, 76)-(46, 82), 0
LINE (46, 82)-(54, 82), 0
8
IF stage <> 3 THEN 67: '----------------------------------------------Stage 3
LINE (-60, 90)-(160, 100), 1, BF
IF detail = 0 THEN 67
LINE (45, 85)-(55, 92), 3, BF
PSET (-60, 90), 0
PSET (160, 90), 0
PSET (-60, 100), 0
PSET (160, 100), 0
LINE (-25, 92)-(-25, 87), 3
LINE (125, 92)-(125, 87), 3
67
IF stage <> 4 THEN 68: '----------------------------------------------Stage 4
LINE (xx - zom + zoom, 67 + (yy / 5))-(xx + zom - zoom, 75 + (yy / 5)), 5, BF
LINE (xx - zom + zoom, 73 + (yy / 5))-(xx + zom - zoom, 73 + (yy / 5)), 0
LINE (-44 + (xx / 12), -20 + (yy / 5))-(-39 + (xx / 12), 70 + (yy / 5)), 1, BF
LINE (40 + (xx / 7), -20 + (yy / 5))-(45 + (xx / 7), 70 + (yy / 5)), 1, BF
LINE (136 + (xx / 12), -20 + (yy / 5))-(141 + (xx / 12), 70 + (yy / 5)), 1, BF

LINE (xx - zom + zoom, 90)-(xx + zom - zoom, 100), 4, BF
LINE (xx - zom + zoom, 90)-(xx + zom - zoom, 90), 0
LINE (-46, -20)-(-51, 92), 3, BF
LINE (47, -20)-(53, 92), 3, BF
LINE (147, -20)-(154, 92), 3, BF
IF detail = 0 THEN 68
LINE (xx - zom + zoom, 98)-(xx + zom - zoom, 98), 0
LINE (-47, 92)-(-50, 92.5), 3, BF
LINE (48, 92)-(52, 92.5), 3, BF
LINE (148, 92)-(153, 92.5), 3, BF
68
IF stage <> 5 THEN 92: REM--------------------------------------------Stage 5
LINE (xx - zom + zoom, 90)-(xx + zom - zoom, 100), 4, BF
LINE (-65, 0)-(-60, 92), 6, BF
LINE (160, 0)-(165, 92), 6, BF
FOR wee = 5 TO 90 STEP 5
LINE (-61, wee)-(-55, wee - 1), 7
LINE (155, wee - 1)-(161, wee), 7
NEXT wee
LINE (45, 75)-(55, 92), 6, BF
LINE (0, 92)-(0, 87), 6
LINE (100, 92)-(100, 87), 6
IF detail = 0 THEN 92
LINE (xx - zom + zoom, 98)-(xx + zom - zoom, 98), 0
LINE (-64, 0)-(-64, 92), 0
LINE (164, 0)-(164, 92), 0
PSET (45, 75), 0
PSET (55, 75), 0
92
IF stage <> 6 THEN 93: '----------------------------------------------Stage 6
LINE (-60, 90)-(160, 95), 6, BF
LINE (ptx, pty)-(ptx - 3, pty - 3), 7
LINE (ptx, pty)-(ptx, pty - 5), 7
LINE (ptx, pty)-(ptx + 3, pty - 3), 7
LINE (-60, 95)-(160, 110), 6, BF
IF detail = 0 THEN 93
LINE (50, 85)-(50, 95), 1
PSET (-60, 90), 0
PSET (160, 90), 0
LINE (-25, 95)-(-25, 87), 1
LINE (125, 95)-(125, 87), 1
93
IF stage <> 7 THEN 94: '----------------------------------------------Stage 7
REM-----------------------------------------------Draw stars
IF detail = 0 THEN 95
WINDOW SCREEN (0, 0)-(320, 95)
FOR I = 1 TO 5
PSET (starsclx!(I), starscly!(I)), 1
NEXT I

FOR I = 1 TO 20
PSET (starsbrx(I), starsbry(I)), 15
PSET (starsx(I), starsy(I)), 7
PSET (starsltx(I), starslty(I)), 8
NEXT I

FOR I = 1 TO 5
starsclx!(I) = starsclx!(I) - .1
IF starsclx!(I) < 0 THEN starsclx!(I) = 320 + starsclx!(I)
NEXT I

FOR I = 1 TO 20
starsbrx(I) = starsbrx(I) - (sspeed * 4)
starsx(I) = starsx(I) - (sspeed * 2)
starsltx(I) = starsltx(I) - sspeed
IF starsbrx(I) < 0 THEN starsbrx(I) = 320 + starsbrx(I)
IF starsx(I) < 0 THEN starsx(I) = 320 + starsx(I)
IF starsltx(I) < 0 THEN starsltx(I) = 320 + starsltx(I)
NEXT I
WINDOW SCREEN (xx - zom + zoom, yy - zom + zoom)-(xx + zom - zoom, yy + zom - zoom)
LINE (xx - zom + zoom, 105 + (yy / 5))-(33 + (xx / 15), 110 + (yy / 5)), 8, BF
LINE (xx - zom + zoom, 100 + (yy / 5))-(43 + (xx / 15), 105 + (yy / 5)), 7, BF
LINE (-20, 100)-(-20 + (xx / 15), 100 + (yy / 5)), 8
LINE (-70, 100)-(-70 + (xx / 15), 100 + (yy / 5)), 8
95
REM---
LINE (xx - zom + zoom, 90)-(160, 100), 7, BF
LINE (160, 95)-(180, 100), 7, BF
LINE (xx - zom + zoom, 70)-(-75, 90), 7, BF
IF detail = 0 THEN 94
LINE (xx - zom + zoom, 65)-(-65, 70), 8, BF
LINE (xx - zom + zoom, 60)-(-60, 65), 7, BF
LINE (160, 90)-(225, 105), 8
LINE (xx - zom + zoom, 100)-(195, 105), 8, BF
PSET (-60, 90), 0
PSET (160, 90), 0
LINE (-24, 92)-(-26, 87), 8
LINE (126, 92)-(124, 87), 8
LINE (-20, 100)-(-20 - (xx / 10), 150 - (yy / 5)), 7
LINE (-70, 100)-(-70 - (xx / 10), 150 - (yy / 5)), 7
94
IF stage <> 8 THEN 106: REM-------------------------------------------Stage 8

LINE (-70, 100)-(-60, 105), 1, BF
LINE (160, 100)-(170, 105), 1, BF
LINE (xx - zom + zoom, 90)-(xx + zom - zoom, 100), 2, BF
LINE (-70, 75)-(-60, 95), 1, BF
LINE (160, 75)-(170, 95), 1, BF
LINE (45, 75)-(55, 92), 1, BF
LINE (0, 92)-(0, 87), 1
LINE (100, 92)-(100, 87), 1

IF fan = 0 THEN 112
IF fanc = 0 THEN fanc = 1 ELSE fanc = 0
IF fanc = 0 THEN rn1 = 7: rn2 = 8 ELSE rn1 = 8: rn2 = 7
LINE (-58, 80)-(-58, 70), rn1
LINE (-58, 80)-(-58, 90), rn2
LINE (158, 80)-(158, 70), rn2
LINE (158, 80)-(158, 90), rn1
fan1 = RND * 10
fan2 = RND * 10
112

IF fan = 1 THEN 113
LINE (-58, 80)-(-58, 80 - fan1), 7
LINE (-58, 80)-(-58, 80 + fan1), 8
LINE (158, 80)-(158, 80 - fan2), 7
LINE (158, 80)-(158, 80 + fan2), 8
113

IF detail = 0 THEN 106
LINE (-70, 76)-(-60, 76), 3
LINE (160, 76)-(170, 76), 3
LINE (-60, 77)-(-58.4, 83), 8, BF
LINE (160, 77)-(158.4, 83), 8, BF
LINE (-60, 78)-(-58.4, 78), 7
LINE (160, 78)-(158.4, 78), 7
LINE (xx - zom + zoom, 98)-(xx + zom - zoom, 98), 0
PSET (45, 75), 0
PSET (55, 75), 0
PSET (-70, 75), 0
PSET (170, 75), 0
106

IF stage <> 9 THEN 117: '---------------------------------------------Stage 9
LINE (-60, 90)-(160, 100), 7, BF
LINE (-25, 92)-(-25, 87), 8
LINE (50, 92)-(50, 87), 8
LINE (125, 92)-(125, 87), 8

IF beam = 0 AND RND > .95 AND ABS(lbh) < .01 THEN lbh = ((RND - .5) * 15)
lbx = lbx + lbh
lbh = lbh / 1.2
IF lbx < -60 THEN lbh = 10
IF lbx > 160 THEN lbh = -10

IF beam = 75 THEN FX 18, SBTime%, SBTimer%
IF beam <> 0 THEN beam = beam + 1
IF beam > 75 THEN FOR wee = 20 TO 70 STEP 25: lbx2 = lbx + ((RND - .5) * 5): LINE (lbx, wee)-(lbx2, wee + 13), firc1: LINE (lbx2, wee + 13)-(lbx, wee + 25), firc1: NEXT wee
IF beam = 0 AND lbx > b2x - 10 AND lbx < b2x + 10 THEN beam = 1
IF beam = 0 AND lbx > b2x2 - 10 AND lbx < b2x2 + 10 THEN beam = 1
IF beam > 125 THEN beam = 0
LINE (lbx - 4, 20)-(lbx + 4, 25), 8, BF
IF beam < 25 THEN LINE (lbx - 1, 22)-(lbx + 1, 23), 1, BF ELSE LINE (lbx - 1, 22)-(lbx + 1, 23), 4, BF
IF detail = 0 THEN 117
IF beam > 75 THEN FOR wee = 25 TO 85 STEP 10: lbx2 = lbx + ((RND - .5) * 3): LINE (lbx, wee)-(lbx2, wee + 5), firc1: LINE (lbx2, wee + 5)-(lbx, wee + 10), 9: NEXT wee
LINE (-60, 100)-(160, 105), 8, BF
LINE (lbx - 4, 20)-(lbx + 4, 20), 7
LINE (lbx - 1, 22)-(lbx + 1, 23), 7, B
LINE (-65, 20)-(-60, 105), 8, BF
LINE (165, 20)-(160, 105), 8, BF
LINE (-60, 21)-(lbx - 4, 23), 8, BF
LINE (lbx + 4, 21)-(160, 23), 8, BF
LINE (-60, 21.5)-(lbx - 4, 21.5), 7
LINE (lbx + 4, 21.5)-(160, 21.5), 7
117

IF stage <> 10 THEN 118: '-------------------------------------------Stage 10
LINE (xx - zom + zoom, 70)-(xx + zom - zoom, 85), 1, BF
IF detail = 0 THEN 118
ssx = ssx + .1
IF RND < .01 THEN dtl = dtl + 1: IF dtl > 1 THEN dtl = -1
LINE (ssx, 72)-(ssx + 3, 72.5), 8, BF
LINE (ssx, 72 - ((dtl + .001) / 10))-(ssx + 3, 72 + ((dtl + .001) / 10)), 7
PSET (ssx + .2 + (b2x / 50), 63 + (b2y / 10)), lac1
PSET (ssx + .2 + (b2x2 / 50), 63 + (b2y2 / 10)), lac2
118

IF stage <> 11 THEN 146: REM-----------------------------------------Stage 11

LINE (xx - zom + zoom, 90)-(xx + zom - zoom, 100), 2, BF
LINE (45, 110)-(55, 125), 1, BF
LINE (47, 105)-(53, 110), 8, BF
LINE (-60, 85)-(-65, 92), 1, BF
LINE (160, 85)-(165, 92), 1, BF

IF fanc = 0 THEN fanc = 1 ELSE fanc = 0
IF fanc = 0 THEN rn1 = 7: rn2 = 8 ELSE rn1 = 8: rn2 = 7
IF fan = 0 THEN 142
LINE (35, 104)-(50, 104), rn1
LINE (50, 104)-(65, 104), rn2
fan1 = RND * 15
142

IF fan = 1 THEN 143
LINE (50 - fan1, 104)-(50, 104), 8
LINE (50, 104)-(50 + fan1, 104), 7
143

IF detail = 0 THEN 146
LINE (48, 105)-(48, 110), 7
LINE (46, 110)-(46, 125), 3
LINE (xx - zom + zoom, 98)-(xx + zom - zoom, 98), 0
PSET (45, 110), 0
PSET (55, 110), 0
PSET (45, 125), 0
PSET (55, 125), 0
146
IF stage <> 12 THEN 147: '-------------------------------------------Stage 12
LINE (-60, 90)-(160, 100), 3, BF
IF detail = 0 THEN 67
LINE (45, 85)-(55, 92), 7, BF
PSET (-60, 90), 0
PSET (160, 90), 0
PSET (-60, 100), 0
PSET (160, 100), 0
LINE (-25, 92)-(-25, 87), 7
LINE (125, 92)-(125, 87), 7
rn1 = 0
FOR wee = -60 TO 160 STEP 35
IF wee < 50 THEN rn1 = rn1 + 5 ELSE rn1 = rn1 - 5
LINE (wee, 100)-(wee + 5, 110 + rn1), 3
LINE (wee + 5, 110 + rn1)-(wee + 10, 100), 3
NEXT wee
147
IF stage <> 13 THEN 148: '-------------------------------------------Stage 13
LINE (-60, 90)-(160, 100), 8, BF
t13 = t13 + 1: IF t13 = 100 THEN t13 = 0: conh = conh + ((RND - .5) / 3)
IF ABS(conh) > 5 THEN conv = conv / 1.5
conx = conx + conh
IF conx > 5 THEN conx = 0
IF conx < 0 THEN conx = 5
FOR wee = conx TO 110 STEP 5
LINE (wee + 50, 98)-(wee + 50, 100), 7
LINE (50 - wee, 98)-(50 - wee, 100), 7
NEXT wee
IF detail = 0 THEN 148
FOR wee = -55 TO 155 STEP 5
cont = cont + ABS(conh)
IF cont > 20 THEN cont = 0: IF conc = 4 THEN conc = 12 ELSE conc = 4
PSET (wee, 97), conc
NEXT wee
LINE (-60, 92)-(50, 75), 7
LINE (50, 75)-(160, 92), 7
LINE (0, 92)-(50, 50), 7
LINE (50, 50)-(100, 92), 7
148

IF stage <> 14 THEN 149: '-------------------------------------------Stage 14

149

IF stage <> 15 THEN 152: '-------------------------------------------Stage 15

152

REM---Skip Animation
124
REM-----------------------------------------------Player-1
REM-----------------------------------------------FLIP-X
IF r1 = 0 THEN 4
b1x = b2x - (b1x - b2x)
h1x = b2x - (h1x - b2x)
laax = b2x - (laax - b2x)
lbax = b2x - (lbax - b2x)
labx = b2x - (labx - b2x)
lbbx = b2x - (lbbx - b2x)

aaax = b2x - (aaax - b2x)
abax = b2x - (abax - b2x)
aabx = b2x - (aabx - b2x)
abbx = b2x - (abbx - b2x)
4

IF fire1 < 1 THEN fire1 = 0
IF fire1 = 0 THEN firx1 = 5000: firh1 = 0 ELSE fire1 = fire1 / 1.01

REM---Skip Animation
IF anisp = 1 THEN 125

'body
LINE (b1x, b1y)-(b2x, b2y), hbc1
'head
IF head1 = 0 THEN CIRCLE (h1x, h1y), 1, hbc1

IF light1 = 1 THEN llc1 = 9
'leg-aa upper-left
LINE (b2x, b2y)-(laax, laay), llc1
'leg-ab lower-left
LINE (laax, laay)-(labx, laby), llc1
IF light1 = 1 THEN IF labx < b2x - 7 OR labx > b2x + 7 THEN LINE (b2x, b2y)-(labx, b2y + 5 - (RND * 15)), llc1

'leg-ba upper-right
LINE (b2x, b2y)-(lbax, lbay), rlc1
'leg-bb lower-right
LINE (lbax, lbay)-(lbbx, lbby), rlc1

'arm-ba upper-left
LINE (b1x, b1y)-(abax, abay), lac1
'leg-bb lower-left
LINE (abax, abay)-(abbx, abby), lac1

'arm-aa upper-right
LINE (b1x, b1y)-(aaax, aaay), rac1
'leg-ab lower-right
LINE (aaax, aaay)-(aabx, aaby), rac1
REM-----------------------------------------------High Detail
IF P1d = 0 THEN 109
'body
LINE (b1x, b1y + ((b2y - b1y) / 2.5))-(b1x, b1y), hbc1
LINE (b1x, b1y + ((b2y - b1y) / 2.5))-(b2x, b2y), hbc1

IF light1 = 1 THEN llc1 = 9
'leg-aa upper-left
LINE (b2x, b2y + 1)-(laax, laay), llc1
'leg-ab lower-left
LINE (laax, laay + .5)-(labx, laby), llc1
IF light1 = 1 THEN IF labx < b2x - 7 OR labx > b2x + 7 THEN LINE (b2x, b2y)-(labx, b2y + 5 - (RND * 15)), llc1

'leg-ba upper-right
LINE (b2x, b2y + 1)-(lbax, lbay), rlc1
'leg-bb lower-right
LINE (lbax, lbay + .5)-(lbbx, lbby), rlc1

'arm-ba upper-left
LINE (b1x, b1y + 1)-(abax, abay), lac1
'leg-bb lower-left
LINE (abax, abay + .5)-(abbx, abby), lac1

'arm-aa upper-right
LINE (b1x, b1y + 1)-(aaax, aaay), rac1
'leg-ab lower-right
LINE (aaax, aaay + .5)-(aabx, aaby), rac1
109

IF firc1 = 9 THEN firc1 = 1 ELSE firc1 = 9

'Charge
IF POS1 <> 5 THEN 10
rn# = RND * chrg1
IF rn# > 25 THEN CIRCLE ((aabx + abbx) / 2, (aaby + abby) / 2), RND, INT(RND * 3) + 7: FX 7, SBTime%, SBTimer%
IF detail = 0 THEN 10
rn1 = (RND - .5) * (chrg1 / 10)
rn2 = (RND - .5) * (chrg1 / 10)
LINE ((aabx + abbx) / 2, (aaby + abby) / 2)-(((aabx + abbx) / 2) + rn1, ((aaby + abby) / 2) + rn2), firc1
IF RND > .9 THEN LINE ((aabx + abbx) / 2, (aaby + abby) / 2)-(((aabx + abbx) / 2) + (rn1 * 10), ((aaby + abby) / 2) + (rn2 * 10)), firc1
10

'Fire Ball
CIRCLE (firx1, firy1), ABS(fire1), firc1

IF detail = 0 THEN 12
CIRCLE (firx1, firy1), RND * (ABS(fire1)), firc1
rn# = (RND - .5) * (fire1 * 2)
LINE (firx1, firy1 + rn#)-(firx1 - (firh1 * (RND * 5)), firy1 + rn#), firc1
12
IF fire1 > 5 THEN fire1 = 5

IF POS1 = 8 AND detail = 1 AND v1 < 0 THEN LINE (aabx, aaby)-(aabx + ((RND - .5)), aaby - (v1 * (RND * 50))), firc1

REM-----------------------------------------------Player-2
REM---Skip Animation
125

REM-----------------------------------------------FLIP-X
IF r2 = 0 THEN 24
b1x2 = b2x2 - (b1x2 - b2x2)
h2x = b2x2 - (h2x - b2x2)
laax2 = b2x2 - (laax2 - b2x2)
lbax2 = b2x2 - (lbax2 - b2x2)
labx2 = b2x2 - (labx2 - b2x2)
lbbx2 = b2x2 - (lbbx2 - b2x2)

aaax2 = b2x2 - (aaax2 - b2x2)
abax2 = b2x2 - (abax2 - b2x2)
aabx2 = b2x2 - (aabx2 - b2x2)
abbx2 = b2x2 - (abbx2 - b2x2)
24

IF fire2 < 1 THEN fire2 = 0
IF fire2 = 0 THEN firx2 = 5000: firh2 = 0 ELSE fire2 = fire2 / 1.01

REM---Skip Animation
IF anisp = 1 THEN 126

'body
LINE (b1x2, b1y2)-(b2x2, b2y2), hbc2
'head
IF head2 = 0 THEN CIRCLE (h2x, h2y), 1, hbc2

IF light2 = 1 THEN llc2 = 9
'leg-aa upper-left
LINE (b2x2, b2y2)-(laax2, laay2), llc2
'leg-ab lower-left
LINE (laax2, laay2)-(labx2, laby2), llc2
IF light2 = 1 THEN IF labx2 < b2x2 - 7 OR labx2 > b2x2 + 7 THEN LINE (b2x2, b2y2)-(labx2, b2y2 + 5 - (RND * 15)), llc2

'leg-ba upper-right
LINE (b2x2, b2y2)-(lbax2, lbay2), rlc2
'leg-bb lower-right
LINE (lbax2, lbay2)-(lbbx2, lbby2), rlc2

'arm-ba upper-left
LINE (b1x2, b1y2)-(abax2, abay2), lac2
'leg-bb lower-left
LINE (abax2, abay2)-(abbx2, abby2), lac2

'arm-aa upper-right
LINE (b1x2, b1y2)-(aaax2, aaay2), rac2
'leg-ab lower-right
LINE (aaax2, aaay2)-(aabx2, aaby2), rac2
REM-----------------------------------------------High Detail
IF P2d = 0 THEN 110
'body
LINE (b1x2, b1y2 + ((b2y2 - b1y2) / 2.5))-(b1x2, b1y2), hbc2
LINE (b1x2, b1y2 + ((b2y2 - b1y2) / 2.5))-(b2x2, b2y2), hbc2

IF light2 = 1 THEN llc2 = 9
'leg-aa upper-left
LINE (b2x2, b2y2 + 1)-(laax2, laay2), llc2
'leg-ab lower-left
LINE (laax2, laay2 + .5)-(labx2, laby2), llc2
IF light2 = 1 THEN IF labx2 < b2x2 - 7 OR labx2 > b2x2 + 7 THEN LINE (b2x2, b2y2)-(labx2, b2y2 + 5 - (RND * 15)), llc2

'leg-ba upper-right
LINE (b2x2, b2y2 + 1)-(lbax2, lbay2), rlc2
'leg-bb lower-right
LINE (lbax2, lbay2 + .5)-(lbbx2, lbby2), rlc2

'arm-ba upper-left
LINE (b1x2, b1y2 + 1)-(abax2, abay2), lac2
'leg-bb lower-left
LINE (abax2, abay2 + .5)-(abbx2, abby2), lac2

'arm-aa upper-right
LINE (b1x2, b1y2 + 1)-(aaax2, aaay2), rac2
'leg-ab lower-right
LINE (aaax2, aaay2 + .5)-(aabx2, aaby2), rac2
110


IF firc2 = 9 THEN firc2 = 1 ELSE firc2 = 9

'Charge
IF POS2 <> 5 THEN 25
rn# = RND * chrg2
IF rn# > 25 THEN CIRCLE ((aabx2 + abbx2) / 2, (aaby2 + abby2) / 2), RND, INT(RND * 3) + 7: FX 7, SBTime%, SBTimer%
IF detail = 0 THEN 25
rn1 = (RND - .5) * (chrg2 / 10)
rn2 = (RND - .5) * (chrg2 / 10)
LINE ((aabx2 + abbx2) / 2, (aaby2 + abby2) / 2)-(((aabx2 + abbx2) / 2) + rn1, ((aaby2 + abby2) / 2) + rn2), firc2
IF RND > .9 THEN LINE ((aabx2 + abbx2) / 2, (aaby2 + abby2) / 2)-(((aabx2 + abbx2) / 2) + (rn1 * 10), ((aaby2 + abby2) / 2) + (rn2 * 10)), firc2
25

'Fire Ball
CIRCLE (firx2, firy2), ABS(fire2), firc2

IF detail = 0 THEN 26
CIRCLE (firx2, firy2), RND * (ABS(fire2)), firc2
rn# = (RND - .5) * (fire2 * 2)
LINE (firx2, firy2 + rn#)-(firx2 - (firh2 * (RND * 5)), firy2 + rn#), firc2
26
IF fire2 > 5 THEN fire2 = 5

IF POS2 = 8 AND detail = 1 AND v2 < 0 THEN LINE (aabx2, aaby2)-(aabx2 + ((RND - .5)), aaby2 - (v2 * (RND * 50))), firc2

REM-----------------------------------------------Flash
IF flashx = 5000 THEN 56
CIRCLE (flashx, flashy), RND * 15, 9
rn1 = (RND - .5) * 100
rn2 = (RND - .5) * 100
LINE (flashx, flashy)-(flashx + rn1, flashy + rn2), 9
LINE (flashx, flashy)-(flashx + rn2, flashy + rn1), 1
flashx = 5000
56
REM-----------------------------------------------Head
IF heady < 94 THEN CIRCLE (headx, heady), 1, hbc
REM-----------------------------------------------Blood
IF satan = 0 THEN 87
CIRCLE (bx0, by0), .07, 4
CIRCLE (bx1, by1), .08, 4
CIRCLE (bx2, by2), .09, 4
CIRCLE (bx3, by3), .1, 4
CIRCLE (bx4, by4), .2, 4
CIRCLE (bx5, by5), .07, 4
CIRCLE (bx6, by6), .08, 4
CIRCLE (bx7, by7), .09, 4
CIRCLE (bx8, by8), .1, 4
CIRCLE (bx9, by9), .2, 4
IF detail = 0 THEN 64
LINE (bx0, by0)-(bx0 - bh0, by0 - bv0), 4
LINE (bx1, by1)-(bx1 - bh1, by1 - bv1), 4
LINE (bx2, by2)-(bx2 - bh2, by2 - bv2), 4
LINE (bx3, by3)-(bx3 - bh3, by3 - bv3), 4
LINE (bx4, by4)-(bx4 - bh4, by4 - bv4), 4
LINE (bx5, by5)-(bx5 - bh5, by5 - bv5), 4
LINE (bx6, by6)-(bx6 - bh6, by6 - bv6), 4
LINE (bx7, by7)-(bx7 - bh7, by7 - bv7), 4
LINE (bx8, by8)-(bx8 - bh8, by8 - bv8), 4
LINE (bx9, by9)-(bx9 - bh9, by9 - bv9), 4
64
IF gal = 0 THEN 87
CIRCLE (bxa, bya), .07, 4
CIRCLE (bxb, byb), .08, 4
CIRCLE (bxc, byc), .09, 4
CIRCLE (bxd, byd), .1, 4
CIRCLE (bxe, bye), .2, 4
CIRCLE (bxf, byf), .07, 4
CIRCLE (bxg, byg), .08, 4
CIRCLE (bxh, byh), .09, 4
CIRCLE (bxi, byi), .1, 4
CIRCLE (bxj, byj), .2, 4
IF detail = 0 THEN 87
LINE (bxa, bya)-(bxa - bha, bya - bva), 4
LINE (bxb, byb)-(bxb - bhb, byb - bvb), 4
LINE (bxc, byc)-(bxc - bhc, byc - bvc), 4
LINE (bxd, byd)-(bxd - bhd, byd - bvd), 4
LINE (bxe, bye)-(bxe - bhe, bye - bve), 4
LINE (bxf, byf)-(bxf - bhf, byf - bvf), 4
LINE (bxg, byg)-(bxg - bhg, byg - bvg), 4
LINE (bxh, byh)-(bxh - bhh, byh - bvh), 4
LINE (bxi, byi)-(bxi - bhi, byi - bvi), 4
LINE (bxj, byj)-(bxj - bhj, byj - bvj), 4
87

REM-----------------------------------------------Stage-1 Forground
IF stage <> 1 THEN 23
'island (Forground)
LINE (sfx + 5, 152)-(sfx + 35, 157), 6, BF
LINE (sfx, 153)-(sfx + 40, 156), 6, BF
LINE (sfx + 5, 80)-(sfx + 10, 154.5), 4
LINE (sfx + 5, 80)-(sfx + 8, 154.5), 4
LINE (sfx + 10, 154.5)-(sfx + 8, 154.5), 4
PAINT (sfx + 6.1, 100), 4, 4
PAINT (sfx + 7.7, 130), 4, 4
CIRCLE (sfx + 3, 83), 1, 6
CIRCLE (sfx + 5, 86), 1.5, 6
CIRCLE (sfx + 7, 84), .7, 6
LINE (sfx + 5, 80)-(sfx - 10, 90), 2
LINE (sfx + 5, 80)-(sfx - 5, 95), 2
LINE (sfx + 5, 80)-(sfx + 1, 99), 2
LINE (sfx + 5, 80)-(sfx + 5, 97), 2
LINE (sfx + 5, 80)-(sfx + 10, 103), 2
LINE (sfx + 5, 80)-(sfx + 15, 92), 2
23
REM-----------------------------------------------Stage-4 Forground
IF stage <> 4 THEN 69
LINE (xx - zom + zoom, 115 - (yy / 5))-(xx + zom - zoom, 125 - (yy / 5)), 5, BF
LINE (xx - zom + zoom, 115 - (yy / 5))-(xx + zom - zoom, 115 - (yy / 5)), 0
LINE (xx - zom + zoom, 123 - (yy / 5))-(xx + zom - zoom, 123 - (yy / 5)), 0
LINE (-55 - (xx / 10), -20 - (yy / 5))-(-66 - (xx / 10), 119 - (yy / 5)), 1, BF
LINE (56.5 - (xx / 5), -20 - (yy / 5))-(63.5 - (xx / 5), 119 - (yy / 5)), 1, BF
LINE (160 - (xx / 10), -20 - (yy / 5))-(167 - (xx / 10), 119 - (yy / 5)), 1, BF
IF detail = 0 THEN 69
LINE (-57 - (xx / 10), 119 - (yy / 5))-(-64 - (xx / 10), 120 - (yy / 5)), 1, BF
LINE (58 - (xx / 5), 119 - (yy / 5))-(62 - (xx / 5), 120 - (yy / 5)), 1, BF
LINE (162 - (xx / 10), 119 - (yy / 5))-(166 - (xx / 10), 120 - (yy / 5)), 1, BF
69
REM-----------------------------------------------Stage-7 Forground
IF stage <> 7 OR detail = 0 THEN 96
LINE (xx - zom + zoom, 150 - (yy / 5))-(40 - (xx / 10), 160 - (yy / 5)), 8, BF
LINE (xx - zom + zoom, 140 - (yy / 5))-(50 - (xx / 10), 150 - (yy / 5)), 7, BF
96

REM----------------------------------------------Stage-10 Forground
IF stage <> 10 THEN 119
LINE (xx - zom + zoom, 85 - (ABS(boby)))-(xx + zom - zoom, yy + zom - zoom), 1, BF
119

REM---Skip Animation
126

REM-----------------------------------------------Color Reset P1
hbc1 = bc1: 'body color
lac1 = ac1: 'left arm color
rac1 = ac1: 'Right arm color
llc1 = lc1: 'left leg color
rlc1 = lc1: 'right leg color
REM-----------------------------------------------Color Reset P2
hbc2 = bc2: 'body color
lac2 = ac2: 'left arm color
rac2 = ac2: 'Right arm color
llc2 = lc2: 'left leg color
rlc2 = lc2: 'right leg color

REM-----------------------------------------------APPLY HITS P1
IF POS2 = 9 OR POS2 = 10 THEN 51
'Jab High
IF j1 = 1 THEN IF abbx > h2x - 3 AND abbx < h2x + 3 AND abby > h2y - 2 AND abby < b2y2 - 2 THEN FX 9, SBTime%, SBTimer%: blood1 = 1: bl1x = abbx: bl1y = abby: hp2 = hp2 - 3: b1x2 = b2x2: IF b2x2 < b2x THEN h2 = h2 - .2 ELSE h2 = h2 + .2
'Jab Low
IF j1 = 1 THEN IF abbx > b2x2 - 4 AND abbx < b2x2 + 4 AND abby > b2y2 - 2 AND abby < lbby2 THEN FX 9, SBTime%, SBTimer%: blood1 = 1: bl1x = abbx: bl1y = abby: hp2 = hp2 - 2: IF b2x2 < b2x THEN h2 = h2 - .2 ELSE h2 = h2 + .2
'Strong High
IF sr1 = 1 THEN IF aabx > h2x - 3 AND aabx < h2x + 3 AND aaby > h2y - 2 AND aaby < b2y2 - 2 THEN FX 10, SBTime%, SBTimer%: ckcap = 1: blood1 = 1: bl1x = aabx: bl1y = aaby: hp2 = hp2 - 5: b1x2 = b2x2: IF b2x2 < b2x THEN h2 = h2 - .2 ELSE h2 = h2 + .2
'Strong Low
IF sr1 = 1 THEN IF aabx > b2x2 - 4 AND aabx < b2x2 + 4 AND aaby > b2y2 - 2 AND aaby < lbby2 + 1 THEN FX 10, SBTime%, SBTimer%: blood1 = 1: bl1x = aabx: bl1y = aaby: hp2 = hp2 - 3: IF b2x2 < b2x THEN h2 = h2 - .2 ELSE h2 = h2 + .2

'Short High
IF s1 = 1 THEN IF labx > h2x - 3 AND labx < h2x + 3 AND laby > h2y - 2 AND laby < b2y2 - 3 THEN FX 9, SBTime%, SBTimer%: blood1 = 1: bl1x = labx: bl1y = laby: hp2 = hp2 - 1: b1x2 = b2x2: h2 = h2 / 1.1
'Short Low
IF s1 = 1 THEN IF labx > b2x2 - 4 AND labx < b2x2 + 4 AND laby > b2y2 - 3 AND laby < lbby2 + 1 THEN FX 9, SBTime%, SBTimer%: blood1 = 1: bl1x = labx: bl1y = laby: hp2 = hp2 - 1: IF RND > .95 THEN POS2 = 9
'RoundHouse
IF rh1 = 1 OR POS1 = 15 THEN IF lbbx > h2x - 3 AND lbbx < h2x + 3 AND lbby > h2y - 2 AND lbby < lbby2 THEN FX 10, SBTime%, SBTimer%: blood1 = 1: bl1x = lbbx: bl1y = lbby: hp2 = hp2 - 2: b1x2 = b2x2: IF RND > .95 THEN POS2 = 9
IF rh1 = 1 AND POS1 = 15 THEN IF lbax > h2x - 3 AND lbax < h2x + 3 AND lbay > h2y - 2 AND lbay < lbby2 THEN FX 9, SBTime%, SBTimer%: blood2 = 1: bl2x = lbax: bl2y = lbay: hp2 = hp2 - 1
'FireBall
IF firx1 > b2x2 - 4 AND firx1 < b2x2 + 4 AND firy1 > h2y - 2 AND firy1 < lbby2 THEN FX 8, SBTime%, SBTimer%: blood1 = 1: bl1x = firx1: bl1y = firy1: hp2 = hp2 - 3: fire1 = fire1 / 1.1: h2 = h2 + ((firh1 / 25) * ABS(fire1)): IF light2 = 1 THEN POS2  _
= 9: light2 = 0
'Dragon Punch
IF POS1 = 8 THEN IF aabx > h2x - 7 AND aabx < h2x + 7 AND aaby > h2y - 2 AND aaby < lbby2 THEN FX 10, SBTime%, SBTimer%: ckcap = 1: blood1 = 1: bl1x = aabx: bl1y = aaby: chrg2 = chrg2 - chrg1: hp2 = hp2 - (chrg1 / 5): hp2 = hp2 - ABS(v1 * 2): v2 = 0 _
: rac1 = 9: IF RND > .5 THEN POS2 = 9
IF POS1 = 8 AND lbby > 90 AND blood1 = 0 AND aabx > h2x - 7 AND aabx < h2x + 7 AND lbby2 > 93 AND dk2 = 0 THEN FX 10, SBTime%, SBTimer%: ckcap = 1: blood1 = 1: bl1x = h2x: bl1y = h2y: chrg2 = chrg2 - chrg1: hp2 = hp2 - (chrg1 / 7): h2 = h2 + (h1 * 2 _
): hp2 = hp2 - ABS(v1 * 2): rac1 = 9: POS2 = 9
'Upper-Cut
IF POS1 = 16 AND b1x > h2x - 4 AND b1x < h2x + 4 AND lbby2 > h1y AND dk2 = 0 THEN FX 10, SBTime%, SBTimer%: ckcap = 1: blood2 = 1: bl2x = h2x: bl2y = h2y: hp2 = hp2 - 5: v2 = v2 - 2: lbby2 = lbby2 - 15: POS2 = 9

51
REM-----------------------------------------------APPLY HITS P2
IF POS1 = 9 OR POS1 = 10 THEN 52
'Jab High
IF j2 = 1 THEN IF abbx2 > h1x - 3 AND abbx2 < h1x + 3 AND abby2 > h1y - 2 AND abby2 < b2y - 2 THEN FX 9, SBTime%, SBTimer%: blood2 = 1: bl2x = abbx2: bl2y = abby2: hp1 = hp1 - 3: b1x = b2x: IF b2x < b2x2 THEN h1 = h1 - .2 ELSE h1 = h1 + .2
'Jab Low
IF j2 = 1 THEN IF abbx2 > b2x - 4 AND abbx2 < b2x + 4 AND abby2 > b2y - 2 AND abby2 < lbby THEN FX 9, SBTime%, SBTimer%: blood2 = 1: bl2x = abbx2: bl2y = abby2: hp1 = hp1 - 2: IF b2x < b2x2 THEN h1 = h1 - .2 ELSE h1 = h1 + .2
'Strong High
IF sr2 = 1 THEN IF aabx2 > h1x - 3 AND aabx2 < h1x + 3 AND aaby2 > h1y - 2 AND aaby2 < b2y - 2 THEN FX 10, SBTime%, SBTimer%: ckcap = 2: blood2 = 1: bl2x = aabx2: bl2y = aaby2: hp1 = hp1 - 5: b1x = b2x: IF b2x < b2x2 THEN h1 = h1 - .2 ELSE h1 = h1 + _
 .2
'Strong Low
IF sr2 = 1 THEN IF aabx2 > b2x - 4 AND aabx2 < b2x + 4 AND aaby2 > b2y - 2 AND aaby2 < lbby + 1 THEN FX 10, SBTime%, SBTimer%: blood2 = 1: bl2x = aabx2: bl2y = aaby2: hp = hp1 - 3: IF b2x < b2x2 THEN h1 = h1 - .2 ELSE h1 = h1 + .2

'Short High
IF s2 = 1 THEN IF labx2 > h1x - 3 AND labx2 < h1x + 3 AND laby2 > h1y - 2 AND laby2 < b2y - 3 THEN FX 9, SBTime%, SBTimer%: blood2 = 1: bl2x = labx2: bl2y = laby2: hp1 = hp1 - 1: b1x = b2x: h2 = h2 / 1.1
'Short Low
IF s2 = 1 THEN IF labx2 > b2x - 4 AND labx2 < b2x + 4 AND laby2 > b2y - 3 AND laby2 < lbby + 1 THEN FX 9, SBTime%, SBTimer%: blood2 = 1: bl2x = labx2: bl2y = laby2: hp1 = hp1 - 1: IF RND > .95 THEN POS1 = 9
'RoundHouse
IF rh2 = 1 OR POS2 = 15 THEN IF lbbx2 > h1x - 3 AND lbbx2 < h1x + 3 AND lbby2 > h1y - 2 AND lbby2 < lbby THEN FX 10, SBTime%, SBTimer%: blood2 = 1: bl2x = lbbx2: bl2y = lbby2: hp1 = hp1 - 2: b1x = b2x: IF RND > .95 THEN POS1 = 9
IF rh2 = 1 AND POS2 = 15 THEN IF lbax2 > h1x - 3 AND lbax2 < h1x + 3 AND lbay2 > h1y - 2 AND lbay2 < lbby THEN FX 9, SBTime%, SBTimer%: blood1 = 1: bl1x = lbax2: bl1y = lbay2: hp1 = hp1 - 1
'FireBall
IF firx2 > b2x - 4 AND firx2 < b2x + 4 AND firy2 > h1y - 2 AND firy2 < lbby THEN FX 8, SBTime%, SBTimer%: blood2 = 1: bl2x = firx2: bl2y = firy2: hp1 = hp1 - 3: fire2 = fire2 / 1.1: h1 = h1 + ((firh2 / 25) * ABS(fire2)): IF light1 = 1 THEN POS1 = 9 _
: light1 = 0
'Dragon Punch
IF POS2 = 8 THEN IF aabx2 > h1x - 7 AND aabx2 < h1x + 7 AND aaby2 > h1y - 2 AND aaby2 < lbby THEN FX 10, SBTime%, SBTimer%: ckcap = 2: blood2 = 1: bl2x = aabx2: bl2y = aaby2: chrg1 = chrg1 - chrg2: hp1 = hp1 - (chrg2 / 5): hp1 = hp1 - ABS(v2 * 2): _
                                               v1 = 0: rac2 = 9: IF RND > .5 THEN POS1 = 9
IF POS2 = 8 AND lbby2 > 90 AND blood2 = 0 AND aabx2 > h1x - 7 AND aabx2 < h1x + 7 AND lbby > 93 AND dk1 = 0 THEN FX 10, SBTime%, SBTimer%: ckcap = 2: blood2 = 1: bl2x = h1x: bl2y = h1y: chrg1 = chrg1 - chrg2: hp1 = hp1 - (chrg2 / 7): h1 = h1 + (h2 * _
 2): hp1 = hp1 - ABS(v2 * 2): rac2 = 9: POS1 = 9
'Upper-Cut
IF POS2 = 16 AND b1x2 > h1x - 4 AND b1x2 < h1x + 4 AND lbby > h2y AND dk1 = 0 THEN FX 10, SBTime%, SBTimer%: ckcap = 2: blood1 = 1: bl1x = h1x: bl1y = h1y: hp1 = hp1 - 5: v1 = v1 - 2: lbby = lbby - 15: POS1 = 9
52
REM---
IF satan = 0 THEN ckcap = 0
IF ckcap = 1 AND win1 = 1 AND hp2 < 0 THEN decap = 2: FX 14, SBTime%, SBTimer%
IF ckcap = 2 AND win2 = 1 AND hp1 < 0 THEN decap = 1: FX 14, SBTime%, SBTimer%
ckcap = 0
REM-----------------------------------------------Reset Hit Switches P1
j1 = 0
sr1 = 0
s1 = 0
rh1 = 0
REM-----------------------------------------------Reset Hit Switches P2
j2 = 0
sr2 = 0
s2 = 0
rh2 = 0
REM---
IF hp1 < 0 THEN hp1 = 0: IF decap <> 1 THEN POS1 = 9
IF hp2 < 0 THEN hp2 = 0: IF decap <> 2 THEN POS2 = 9

IF hp1 = 0 AND gush = 1 AND RND > .99 THEN POS1 = 9
IF hp2 = 0 AND gush = 2 AND RND > .99 THEN POS2 = 9

IF POS1 = 9 AND stage = 2 AND b2x < 55 THEN h1 = h1 - .001
IF POS1 = 9 AND stage = 2 AND b2x > 155 THEN h1 = h1 + .001

IF POS2 = 9 AND stage = 2 AND b2x2 < 55 THEN h2 = h2 - .001
IF POS2 = 9 AND stage = 2 AND b2x2 > 155 THEN h2 = h2 + .001

REM-----------------------------------------------UN-FLIP-X P1
IF r1 = 0 THEN 5
b1x = b2x + (b2x - b1x)
h1x = b2x + (b2x - h1x)
laax = b2x + (b2x - laax)
lbax = b2x + (b2x - lbax)
labx = b2x + (b2x - labx)
lbbx = b2x + (b2x - lbbx)

aaax = b2x + (b2x - aaax)
abax = b2x + (b2x - abax)
aabx = b2x + (b2x - aabx)
abbx = b2x + (b2x - abbx)
5
REM-----------------------------------------------UN-FLIP-X P2
IF r2 = 0 THEN 27
b1x2 = b2x2 + (b2x2 - b1x2)
h2x = b2x2 + (b2x2 - h2x)
laax2 = b2x2 + (b2x2 - laax2)
lbax2 = b2x2 + (b2x2 - lbax2)
labx2 = b2x2 + (b2x2 - labx2)
lbbx2 = b2x2 + (b2x2 - lbbx2)

aaax2 = b2x2 + (b2x2 - aaax2)
abax2 = b2x2 + (b2x2 - abax2)
aabx2 = b2x2 + (b2x2 - aabx2)
abbx2 = b2x2 + (b2x2 - abbx2)
27

REM-----------------------------------------------Forground
WINDOW SCREEN (0, 0)-(100, 100)
LINE (0, 0)-(100, 100), 0, B
REM-----------------------------------------------Say Code
'Ring Outs
IF b2y > 125 AND b2y < 135 THEN sayp = 1: say = 200: say$ = "Player 1 -Ring Out"
IF b2y2 > 125 AND b2y2 < 135 THEN sayp = 2: say = 200: say$ = "Player 2 -Ring Out"
IF say > 0 THEN 53
'KO
IF hp2 = 0 AND hp2s > 0 AND hp1 > 0 THEN sayp = 1: say = 200: say$ = "Player 1 Wins The Round": POS1 = 0: IF decap = 0 THEN POS2 = 9
IF hp1 = 0 AND hp1s > 0 AND hp2 > 0 THEN sayp = 2: say = 200: say$ = "Player 2 Wins The Round": POS2 = 0: IF decap = 0 THEN POS1 = 9
IF hp1 = 0 AND hp2 = 0 THEN sayp = 3: say = 50: say$ = "Draw": POS1 = 0: POS2 = 0
53
IF say = 0 THEN sayp = 1
IF sayp = 2 THEN sayx = 45 ELSE sayx = 1
IF sayp = 3 THEN sayx = 40
IF say > 0 AND say$ = "Matchinfo" THEN say = say - 1: COLOR 4: LOCATE 3, 1: IF players = 2 THEN PRINT "Match "; mat; " Round "; round: GOTO 54 ELSE PRINT "Match "; mat; " Round "; round; " Difficulty "; dif: GOTO 54
IF say > 0 THEN say = say - 1: COLOR 4: LOCATE 3, sayx: PRINT say$
54
REM-----------------------------------------------JoyTest
IF joytest = 0 THEN 37
LOCATE 5, 30: PRINT x1; y1
LOCATE 5, 60: PRINT x2
LINE (0, 10)-(10, 20), 15, B
LINE (90, 10)-(100, 20), 15, B

LINE (5 - (sense / 20), 15 - (sense / 20))-(5 + (sense / 20), 15 + (sense / 20)), 1, B
LINE (95 - (sense / 20), 15 - (sense / 20))-(95 + (sense / 20), 15 + (sense / 20)), 1, B

LINE (0, ((y1 - y1m) / 20) + 15)-(10, ((y1 - y1m) / 20) + 15), 15
LINE (((x1 - x1m) / 20) + 5, 10)-(((x1 - x1m) / 20) + 5, 20), 15
LINE (90, ((y2 - y2m) / 20) + 15)-(100, ((y2 - y2m) / 20) + 15), 15
LINE (((x2 - x2m) / 20) + 95, 10)-(((x2 - x2m) / 20) + 95, 20), 15
37
REM---Warning Lights
IF stage = 8 OR stage = 11 THEN IF fan = 0 THEN LINE (49, 3)-(51, 4.5), 1, BF ELSE LINE (49, 3)-(51, 4.5), 4, BF
IF stage = 9 THEN IF beam < 25 THEN LINE (49, 3)-(51, 4.5), 1, BF ELSE LINE (49, 3)-(51, 4.5), 4, BF
LINE (49, 3)-(51, 4.5), 8, B
REM---Life Bars
'P1
IF hp1 > hp1s THEN hp1s = hp1s + .1
IF hp1 < hp1s THEN hp1s = hp1s - .1
IF hp1 > hp1s + 10 THEN hp1s = hp1s + .4
IF hp1 < hp1s - 10 THEN hp1s = hp1s - .4
LINE (45, 2)-(45 - hp1s / 2.33, 5), 4, BF
LINE (2, 2)-(45, 5), 8, B
IF chrg1 > 0 THEN LINE (45, 6)-(45 - chrg1 / 2.33, 7), 9, BF
LINE (45, 8)-(45 - (jump1 * 40), 8), 1
'P2
IF hp2 > hp2s THEN hp2s = hp2s + .1
IF hp2 < hp2s THEN hp2s = hp2s - .1
IF hp2 > hp2s + 10 THEN hp2s = hp2s + .4
IF hp2 < hp2s - 10 THEN hp2s = hp2s - .4
LINE (55, 2)-(55 + hp2s / 2.33, 5), 4, BF
LINE (98, 2)-(55, 5), 8, B
IF chrg2 > 0 THEN LINE (55, 6)-(55 + chrg2 / 2.33, 7), 9, BF
LINE (55, 8)-(55 + (jump2 * 40), 8), 1
REM-----------------------------------------------Win Icons
IF win1 = 1 THEN LINE (44, 8)-(44, 12), 8: IF satan = 1 THEN LINE (43, 11)-(45, 11), 8
IF win2 = 1 THEN LINE (56, 8)-(56, 12), 8: IF satan = 1 THEN LINE (55, 11)-(57, 11), 8
REM-----------------------------------------------END GRAPHICS

REM-----------------------Fans
REM---Turn Fans On/Off
IF stage <> 8 AND stage <> 11 THEN 115
IF fan = 0 AND RND > .99 THEN fan = 1: fant = 200
IF fant > 0 THEN fant = fant - 1: IF fant = 0 THEN fan = 0

IF fan = 0 OR stage <> 8 THEN 115
IF b2y > 65 AND POS1 < 9 THEN IF b2x < -55 OR b2x > 155 THEN FX 17, SBTime%, SBTimer%: hp1 = hp1 - 1: blood1 = 1: bl1x = b2x: bl1y = h1y + (RND * 10): b2x = b2x + ((RND - .5) * 2): b2y = b2y - (RND * 2): POS1 = 0: IF hp1 < 2 THEN POS1 = 9
IF b2y2 > 65 AND POS2 < 9 THEN IF b2x2 < -55 OR b2x2 > 155 THEN FX 17, SBTime%, SBTimer%: hp2 = hp2 - 1: blood2 = 1: bl2x = b2x2: bl2y = h2y + (RND * 10): b2x2 = b2x2 + ((RND - .5) * 2): b2y2 = b2y2 - (RND * 2): POS2 = 0: IF hp2 < 2 THEN POS2 = 9
IF b2y > 65 AND POS1 < 9 AND b2x < -50 THEN h1 = h1 - .1: b1x = b2x
IF b2y > 65 AND POS1 < 9 AND b2x > 150 THEN h1 = h1 + .1: b1x = b2x
IF b2y2 > 65 AND POS2 < 9 AND b2x2 < -50 THEN h2 = h2 - .1: b1x2 = b2x2
IF b2y2 > 65 AND POS2 < 9 AND b2x2 > 150 THEN h2 = h2 + .1: b1x2 = b2x2
REM FX 16, SBTime%, SBTimer%:'Un-REM for Fan-Sound
115

REM-----------------------------------------------CHARACTER ANIMATION
CALL P1Animation1
CALL P1Animation2
CALL P2Animation1
CALL P2Animation2

REM------------------------------------------------------CONTROL For Player 1
IF wee$ = "s" OR wee$ = "S" THEN IF akck1 = 1 THEN wee$ = "A"
IF wee$ = "s" OR wee$ = "S" THEN IF POS1 = 1 OR POS1 = 2 THEN wee$ = ""
IF wee$ = "8" OR wee$ = "7" OR wee$ = "9" THEN IF laby > 94 AND lbby > 94 THEN IF POS1 = 0 OR POS1 = 5 THEN h1 = h1 / 2: v1 = -.5: IF jump1 > 0 THEN v1 = v1 - jump1 / 2: jump1 = jump1 / 2
IF wee$ = "5" AND lbby > 94 AND b2y < 92 THEN dkt1 = 25
IF wee$ = "4" OR wee$ = "7" THEN IF dk1 = 0 AND laby > 94 AND lbby > 94 THEN IF POS1 = 0 OR POS1 = 5 THEN h1 = h1 - .3
IF wee$ = "6" OR wee$ = "9" THEN IF dk1 = 0 AND laby > 94 AND lbby > 94 THEN IF POS1 = 0 OR POS1 = 5 THEN h1 = h1 + .3
IF wee$ = "7" AND dk1 = 0 AND lbby > 94 THEN IF POS1 = 0 OR POS1 = 5 THEN h1 = h1 - .5: v1 = v1 - .5
IF wee$ = "9" AND dk1 = 0 AND lbby > 94 THEN IF POS1 = 0 OR POS1 = 5 THEN h1 = h1 + .5: v1 = v1 - .5
IF wee$ = "8" AND dk1 = 0 AND lbby > 94 THEN IF POS1 = 0 OR POS1 = 5 THEN v1 = v1 - .5
IF dkt1 > 0 THEN dkt1 = dkt1 - 1: dk1 = 1 ELSE dk1 = 0
IF dk1 = 1 AND lbby < 95 THEN dk1 = 0

IF laby < 95 AND lbby < 95 THEN v1 = v1 + grav: '-Gravity
IF laby > 93 AND lbby > 93 AND stage <> 12 THEN h1 = h1 / 1.1
'Special Moves
REM-----------------------------------------------FIRE BALL
IF wee$ = "q" OR wee$ = "Q" THEN IF chrg1 > 2 AND fire1 = 0 AND POS1 = 5 THEN skip1 = 1: hbc1 = 9: rac1 = 7: lac1 = 7: b1x = b2x + 3: aaax = b1x + 3: aabx = b1x + 5: aaay = b1y - .5: aaby = aaay: abay = b1y + .5: abby = abay: abax = aaax: abbx =  _
aabx: firx1 = aabx: firy1 = aaby: fire1 = ABS(chrg1 / 10): POS1 = 0: IF r1 = 0 THEN firh1 = 2 ELSE firh1 = -2: firx1 = firx1 - (firx1 - b2x)
REM-----------------------------------------------DRAGON PUNCH
IF wee$ = "w" OR wee$ = "W" THEN IF chrg1 > 25 AND ABS(v1) < .1 AND POS1 = 5 AND lbby > 93 AND dk1 = 0 THEN skip1 = 1: POS1 = 8: v1 = v1 - (chrg1 / 60) - jump1: jump1 = 0: aaax = b2x + 2: aabx = b2x + 1: abax = b2x - 1: abbx = b2x - 1: aaah = -.1:   _
aabh = -.1: abah = .05: abbh = .05: laax = b2x + 3: labx = b2x + 2: laah = -.1: labh = -.1: laay = b2y + 5: lbay = laay: laby = b2y + 8: lbby = b2y + 11: b1y = b2y - 8: h1y = b1y - 3: aaay = h1y: aaby = h1y - 4: abay = b1y + 5: abby = b1y + 7: aaav  _
= 0: abav = 0
REM-----------------------------------------------Super Jump
IF wee$ = "s" OR wee$ = "S" THEN IF chrg1 > 1 AND POS1 = 5 THEN jump1 = jump1 + (chrg1 / 100): POS1 = 0: skip1 = 1
IF jump1 > 1 THEN jump1 = 1

IF POS1 = 8 THEN skip1 = 1
IF skip1 = 1 THEN skip1 = 0: GOTO 11
'punches
REM-----------------------------------------------JAB        (Left)
IF jt1 > 0 THEN jt1 = jt1 - 1
IF wee$ = "q" OR wee$ = "Q" THEN IF jt1 = 0 THEN j1 = 1: jt1 = 15: abay = b1y: abby = b1y - 1: abbx = b1x + 5: FX 1, SBTime%, SBTimer%
REM-----------------------------------------------STRONG     (Right)
IF st1 > 0 THEN st1 = st1 - 1
IF wee$ = "w" OR wee$ = "W" THEN IF dk1 = 1 AND POS1 = 0 THEN POS1 = 16: b2y = b2y - 5
IF wee$ = "w" OR wee$ = "W" THEN IF st1 = 0 AND chrg1 = 0 THEN st1 = 35: sr1 = 1: aaay = b1y: aaby = b1y - 1: aaax = b1x + 3: aabx = b1x + 6: FX 2, SBTime%, SBTimer%

REM-----------------------------------------------CHARGE
IF wee$ = "e" OR wee$ = "E" THEN IF POS1 = 0 AND chrg1 = 0 THEN POS1 = 5: aabh = 0: abbh = 0: aaah = 0: abah = 0

'kicks
IF wee$ = "s" OR wee$ = "S" THEN IF dk1 = 1 THEN wee$ = "A"
REM-----------------------------------------------SHORT      (left)
rn# = RND * 15
IF wee$ = "a" OR wee$ = "A" THEN s1 = 1: IF dk1 = 0 THEN lbbx = lbax: b2x = lbbx: b1x = b2x - 2: laby = b2y + 5 - rn#: labx = b2x + 8: laay = (b2y + laby) / 2: laax = (b2x + labx) / 2: Lt1 = 1: FX 3, SBTime%, SBTimer% ELSE FX 2, SBTime%, SBTimer%: _
                                                                    laay = 93.5: laby = 95: laax = b2x + 4: labx = b2x + 8
IF Lt1 = 1 THEN lgt1 = lgt1 + 3 ELSE IF lgt1 > 0 THEN lgt1 = lgt1 - 1
Lt1 = 0
IF lgt1 = 0 THEN light1 = 0
IF lgt1 > 20 THEN light1 = 1
IF lgt1 > 35 THEN lgt1 = 35
REM-----------------------------------------------ROUNDHOUSE (right)
IF rt1 > 0 THEN rt1 = rt1 - 1
IF wee$ = "s" OR wee$ = "S" THEN IF lbby > 90 AND POS1 = 0 AND rt1 = 0 THEN POS1 = 1: frt1 = 25: rt1 = 100: FX 4, SBTime%, SBTimer%
IF wee$ = "s" OR wee$ = "S" THEN IF POS1 = 0 AND lbby < 90 THEN POS1 = 15

11 REM--------------------------------------------Skipped moves (SKIP1=1)

IF hp1 = 0 AND players = 1 THEN 33
REM------------------------------------------------------CONTROL For Player 2
IF weeb$ = "S" OR weeb$ = "S" THEN IF akck2 = 1 THEN weeb$ = "A"
IF wee2$ = "s" OR wee2$ = "S" THEN IF POS2 = 1 OR POS2 = 2 THEN wee2$ = ""
IF wee2$ = "8" OR wee2$ = "7" OR wee2$ = "9" THEN IF laby2 > 94 AND lbby2 > 94 THEN IF POS2 = 0 OR POS2 = 5 THEN h2 = h2 / 2: v2 = -.5: IF jump2 > 0 THEN v2 = v2 - jump2 / 2: jump2 = jump2 / 2
IF wee2$ = "5" AND lbby2 > 94 AND b2y2 < 92 THEN dkt2 = 25
IF wee2$ = "4" OR wee2$ = "7" THEN IF dk2 = 0 AND laby2 > 94 AND lbby2 > 94 THEN IF POS2 = 0 OR POS2 = 5 THEN h2 = h2 - .3
IF wee2$ = "6" OR wee2$ = "9" THEN IF dk2 = 0 AND laby2 > 94 AND lbby2 > 94 THEN IF POS2 = 0 OR POS2 = 5 THEN h2 = h2 + .3
IF wee2$ = "7" AND dk2 = 0 AND lbby2 > 94 THEN IF POS2 = 0 OR POS2 = 5 THEN h2 = h2 - .5: v2 = v2 - .5
IF wee2$ = "9" AND dk2 = 0 AND lbby2 > 94 THEN IF POS2 = 0 OR POS2 = 5 THEN h2 = h2 + .5: v2 = v2 - .5
IF wee2$ = "8" AND dk2 = 0 AND lbby2 > 94 THEN IF POS2 = 0 OR POS2 = 5 THEN v2 = v2 - .5
IF dkt2 > 0 THEN dkt2 = dkt2 - 1: dk2 = 1 ELSE dk2 = 0
IF dk2 = 1 AND lbby2 < 95 THEN dk2 = 0

IF laby2 < 95 AND lbby2 < 95 THEN v2 = v2 + grav: '-Gravity
IF laby2 > 93 AND lbby2 > 93 AND stage <> 12 THEN h2 = h2 / 1.1
'Special Moves
REM-----------------------------------------------FIRE BALL
IF weeb$ = "Q" THEN IF chrg2 > 2 AND fire2 = 0 AND POS2 = 5 THEN skip2 = 1: hbc2 = 9: rac2 = 7: lac2 = 7: b1x2 = b2x2 + 3: aaax2 = b1x2 + 3: aabx2 = b1x2 + 5: aaay2 = b1y2 - .5: aaby2 = aaay2: abay2 = b1y2 + .5: abby2 = abay2: abax2 = aaax2: abbx2  _
= aabx2: firx2 = aabx2: firy2 = aaby2: fire2 = ABS(chrg2 / 10): POS2 = 0: IF r2 = 0 THEN firh2 = 2 ELSE firh2 = -2: firx2 = firx2 - (firx2 - b2x2)
REM-----------------------------------------------DRAGON PUNCH
IF weeb$ = "W" THEN IF chrg2 > 25 AND POS2 = 5 AND ABS(v2) < .1 AND lbby2 > 93 AND dk2 = 0 THEN skip2 = 1: POS2 = 8: v2 = v2 - (chrg2 / 60) - jump2: jump2 = 0: aaax2 = b2x2 + 2: aabx2 = b2x2 + 1: abax2 = b2x2 - 1: abbx2 = b2x2 - 1: aaah2 = -.1:   _
aabh2 = -.1: abah2 = .05: abbh2 = .05: laax2 = b2x2 + 3: labx2 = b2x2 + 2: laah2 = -.1: labh2 = -.1: laay2 = b2y2 + 5: lbay2 = laay2: laby2 = b2y2 + 8: lbby2 = b2y2 + 11: b1y2 = b2y2 - 8: h2y = b1y2 - 3: aaay2 = h2y: aaby2 = h2y - 4: abay2 = b1y2 +  _
5: abby2 = b1y2 + 7: aaav2 = 0: abav2 = 0
REM-----------------------------------------------Super Jump
IF weeb$ = "S" THEN IF chrg2 > 1 AND POS2 = 5 THEN jump2 = jump2 + (chrg2 / 100): POS2 = 0: skip2 = 1
IF jump2 > 1 THEN jump2 = 1

IF POS2 = 8 THEN skip2 = 1
IF skip2 = 1 THEN skip2 = 0: GOTO 33
'punches
REM-----------------------------------------------JAB        (Left)
IF jt2 > 0 THEN jt2 = jt2 - 1
IF weeb$ = "Q" THEN IF jt2 = 0 THEN jt2 = 15: j2 = 1: abay2 = b1y2: abby2 = b1y2 - 1: abbx2 = b1x2 + 5: FX 1, SBTime%, SBTimer%
REM-----------------------------------------------STRONG     (Right)
IF st2 > 0 THEN st2 = st2 - 1
IF weeb$ = "W" THEN IF dk2 = 1 AND POS2 = 0 THEN POS2 = 16: b2y2 = b2y2 - 5
IF weeb$ = "W" THEN IF st2 = 0 AND chrg2 = 0 THEN st2 = 35: sr2 = 1: aaay2 = b1y2: aaby2 = b1y2 - 1: aaax2 = b1x2 + 3: aabx2 = b1x2 + 6: FX 2, SBTime%, SBTimer%

REM-----------------------------------------------Charge
IF weeb$ = "E" THEN IF POS2 = 0 AND chrg2 = 0 THEN POS2 = 5: aabh2 = 0: abbh2 = 0: aaah2 = 0: abah2 = 0

'kicks
IF weeb$ = "S" THEN IF laby2 < 93 OR lbby2 < 93 OR dk2 = 1 THEN weeb$ = "A"
REM-----------------------------------------------SHORT      (left)
rn# = RND * 15
IF weeb$ = "A" THEN s2 = 1: IF dk2 = 0 THEN lbbx2 = lbax2: b2x2 = lbbx2: b1x2 = b2x2 - 2: laby2 = b2y2 + 5 - rn#: labx2 = b2x2 + 8: laay2 = (b2y2 + laby2) / 2: laax2 = (b2x2 + labx2) / 2: Lt2 = 1: FX 3, SBTime%, SBTimer% ELSE FX 2, SBTime%, SBTimer% _
: laay2 = 93.5: laby2 = 95: laax2 = b2x2 + 4: labx2 = b2x2 + 8
IF Lt2 = 1 THEN lgt2 = lgt2 + 3 ELSE IF lgt2 > 0 THEN lgt2 = lgt1 - 1
Lt2 = 0
IF lgt2 = 0 THEN light2 = 0
IF lgt2 > 20 THEN light2 = 1
IF lgt2 > 35 THEN lgt2 = 35

REM-----------------------------------------------ROUNDHOUSE (right)
IF rt2 > 0 THEN rt2 = rt2 - 1
IF weeb$ = "S" THEN IF POS2 = 0 AND rt2 = 0 THEN POS2 = 1: frt2 = 25: rt2 = 100: FX 4, SBTime%, SBTimer%
IF weeb$ = "S" THEN IF POS2 = 0 AND lbby2 < 90 THEN POS2 = 15

33 REM--------------------------------------------Skipped moves (SKIP2=1)

CALL move

REM-----------------------------------Round/Match Over
IF hp1 = 0 OR hp2 = 0 THEN IF say = 1 THEN round = round + 1
IF match = 1 THEN round = 1
'P1 Wins
IF hp2 = 0 AND say = 1 THEN win1 = win1 + 1: IF win1 = 2 THEN match = 1: mat = mat + 1: zom = 100: say = 100: sayp = 1: say$ = "Player 1 Wins The Match" ELSE 666
'P2 Wins
IF hp1 = 0 AND say = 1 THEN win2 = win2 + 1: IF win2 = 2 THEN match = 1: mat = mat + 1: zom = 100: say = 100: sayp = 2: say$ = "Player 2 Wins The Match" ELSE 666
IF match = 1 AND say = 1 THEN 666

GOTO 2: 'Loop back

18 REM--------------------------------------------------------------Quit Game
SCREEN , , , ap
COLOR 4
LOCATE 3, 36
PRINT "QUIT ?"
19 wee$ = INKEY$
IF wee$ = "" THEN 19
IF wee$ = "Y" OR wee$ = "y" THEN 20
SCREEN , , , bp
RETURN
20
LOCATE 3, 35
PRINT "Drop to DOS ?"
21 wee$ = INKEY$
IF wee$ = "" THEN 21
IF wee$ = "Y" OR wee$ = "y" THEN SCREEN 0, 0, 0: CALL SBinit: SYSTEM
CLEAR
GOTO 22

82 REM----------------------------------------------------------------Options
FX 14, SBTime%, SBTimer%
FOR wee = 0 TO 50 STEP .1
LINE (0, wee - 5)-(100, wee), 0, BF
LINE (0, 100 - wee)-(100, 100 - wee + 5), 0, BF
NEXT wee
OPEN Drv$ + ":" + Path$ + "\Config.dat" FOR INPUT AS #1
INPUT #1, dif, s, satan, smear, hp1, hp2, grav, Delay, offy, detail, sspeed, stage, boby, bobv, sbx, sfx, zom, joy, cap, joyfix, joytest, sense, players, gal, yk, ac1, bc1, lc1, ac2, bc2, lc2, SBTimes%, P1d, P2d, spl, speed
CLOSE #1
cols = 40
colsh = 1
eyes = 0
eye = 300
pnt = 5
84 REM---
COLOR 50
LOCATE 2, 5: PRINT "Player-1 Energy "; hp1; "%"
LOCATE 3, 5: PRINT "Player-2 Energy "; hp2; "%"
LOCATE 4, 5: PRINT "Difficulty "; dif
LOCATE 5, 5: PRINT "Stage "; stage
LOCATE 5, 16
IF stage = 1 THEN PRINT "Slow"
IF stage = 2 THEN PRINT "Average"
IF stage = 3 THEN PRINT "Fast"
IF stage = 4 THEN PRINT "Slow"
IF stage = 5 THEN PRINT "Average"
IF stage = 6 THEN PRINT "Fast"
IF stage = 7 AND detail = 0 THEN PRINT "Fast"
IF stage = 7 AND detail = 1 THEN PRINT "Average"
IF stage = 8 THEN PRINT "Average"
IF stage = 9 AND detail = 0 THEN PRINT "Fast"
IF stage = 9 AND detail = 1 THEN PRINT "Average"
IF stage = 10 THEN PRINT "Average"
IF stage = 11 THEN PRINT "Average"
IF stage = 12 AND detail = 0 THEN PRINT "Fast"
IF stage = 12 AND detail = 1 THEN PRINT "Average"
IF stage = 13 AND detail = 0 THEN PRINT "Fast"
IF stage = 13 AND detail = 1 THEN PRINT "Average"

LOCATE 6, 5: IF stage <> 3 THEN PRINT "FX Mode N/A" ELSE IF smear = 0 THEN PRINT "FX Mode No FX" ELSE IF smear = 2 THEN PRINT "FX Mode Half Sight" ELSE PRINT "FX Mode Smear x"; smear
LOCATE 12, 5: IF spl = 1 THEN PRINT "Special Moves Off" ELSE PRINT "Special Moves On"
LOCATE 13, 5: PRINT "Edit Fighter Options"
LOCATE 14, 5: IF joy = 0 AND cap = 0 THEN PRINT "Joystick(s) Disabled" ELSE IF cap = 0 AND joy = 1 THEN PRINT "Joystick(s) Enabled" ELSE PRINT "Capcom PC Fighter 6"
LOCATE 15, 5: IF joy = 0 THEN PRINT "Sensitivity N/A" ELSE PRINT "Sensitivity "; sense
LOCATE 16, 5: IF joyfix = 0 THEN PRINT "Joystick Correction Off" ELSE PRINT "Joystick Correction On"
LOCATE 17, 5: IF SBSound% = 2 THEN PRINT "Sound Wait (Sound is Off)" ELSE PRINT "Sound Wait "; SBTimes%
LOCATE 18, 5: PRINT "Gravity "; grav; " (Normal .02)"
LOCATE 19, 5: IF detail = 0 THEN PRINT "Graphic Detail Low" ELSE PRINT "Graphic Detail High"
LOCATE 20, 5: IF satan = 0 THEN PRINT "Blood/Evil Off" ELSE PRINT "Blood/Evil On"
LOCATE 20, 20: IF satan = 1 THEN IF gal = 0 THEN PRINT "(10 Gallons)" ELSE PRINT "(20 Gallons)"
LOCATE 21, 5: PRINT "Game Delay "; Delay
IF speed = 0 THEN speed$ = "100%"
IF speed = 1 THEN speed$ = " 80%"
IF speed = 3 THEN speed$ = " 60%"
IF speed = 5 THEN speed$ = " 40%"
IF speed = 7 THEN speed$ = " 20%"
IF speed = 9 THEN speed$ = " 10%"
LOCATE 22, 5: PRINT "Animation "; speed$
LOCATE 23, 5: PRINT "Done           S.F.B. Version 7.0"
REM---
wee$ = INKEY$
IF wee$ = CHR$(13) THEN wee$ = " "

IF satan = 0 THEN 88
blov = blov + grav
bloy = bloy + blov
LINE (93, bloy - (blov * 3) - 2)-(97, bloy + 7), 0, BF
LINE (93, bloy - 3)-(97, bloy + 3), 0, BF
CIRCLE (95, bloy), .5, 45
PSET (94.7, bloy + .3), col - 10
PSET (95.3, bloy - .3), col
IF bloy > 105 THEN blov = -1
IF detail = 0 THEN 88
LINE (95.5, bloy)-(95, bloy - (blov * 3)), col - 5
LINE (94, bloy)-(95, bloy - (blov * 3)), 42
LINE (96, bloy)-(95, bloy - (blov * 3)), 45
88
IF satan = 0 THEN 102
IF eye > 0 THEN eye = eye - 1
IF eyes = 0 AND eye = 0 THEN eyes = 1: eye = 700: FX 15, SBTime%, SBTimer%
IF cols < 40 THEN colsh = 1: eyes = 0: LINE (10, 25)-(90, 43), 0, BF
IF cols > 62 THEN colsh = -1
cols = cols + colsh
IF eyes = 0 THEN 102
LINE (50 - 35, 25)-(50 - 25, 35), cols - 5
LINE (50 - 25, 35)-(50 - 15, 42), cols - 5
LINE (50 - 15, 42)-(50 - 25, 40), cols - 5
LINE (50 - 25, 40)-(50 - 35, 25), cols - 5

LINE (50 + 35, 25)-(50 + 25, 35), cols - 5
LINE (50 + 25, 35)-(50 + 15, 42), cols - 5
LINE (50 + 15, 42)-(50 + 25, 40), cols - 5
LINE (50 + 25, 40)-(50 + 35, 25), cols - 5
IF detail = 0 THEN 102
PAINT (50 - 25, 38), cols, cols - 5
PAINT (50 + 25, 38), cols, cols - 5
102

IF col < 45 THEN colh = 1
IF col > 50 THEN colh = -1
col = col + colh
LINE (0, 0)-(100, 100), col, B

IF wee$ = " " AND pnt < 80 THEN CLS
LINE (1, pot - 4)-(5, pot + 4), 0, BF

IF pnt > pot THEN pot = pot + 1
IF pnt < pot THEN pot = pot - 1
IF pnt > pot + 20 THEN pot = pot + 3
IF pnt < pot - 20 THEN pot = pot - 3

'Draw Pointer
PSET (2 + ((RND - .5) * 1.5), pot + ((RND - .5) * 1.5)), 35 + (RND * 25)
CIRCLE (2, pot), 1, 45
LINE (3, pot)-(5, pot), 45
PSET (1.7, pot - .6), 60
PSET (1.4, pot - .6), 50

IF wee$ = CHR$(0) + "H" THEN wee$ = "8"
IF wee$ = CHR$(0) + "K" THEN wee$ = "4"
IF wee$ = CHR$(0) + "P" THEN wee$ = "5"
IF wee$ = CHR$(0) + "M" THEN wee$ = "6"
'input
IF wee$ = "8" THEN pnt = pnt - 4.07: FX 11, SBTime%, SBTimer%
IF wee$ = "5" THEN pnt = pnt + 4.07: FX 11, SBTime%, SBTimer%

IF pnt > 91 THEN pnt = 5
IF pnt < 4 THEN pnt = 90.4699
IF pnt > 4 AND pnt < 5 THEN pnt = 5
REM---
IF wee$ <> " " THEN 84
py = INT(pnt)
IF py = 5 THEN hp1 = hp1 + 25: IF hp1 > 100 THEN hp1 = 25
IF py = 9 THEN hp2 = hp2 + 25: IF hp2 > 100 THEN hp2 = 25
IF py = 13 THEN dif = dif + 1: IF dif > 10 THEN dif = 0
IF py = 17 THEN stage = stage + 1: IF stage > 15 THEN stage = 1
IF py = 21 THEN IF smear = 0 THEN smear = 2 ELSE smear = smear + 4
IF smear > 25 THEN smear = 0
IF smear = 6 THEN smear = 3
IF py = 49 THEN CALL ColorSelect
IF py = 53 THEN in = in + 1: IF in = 3 THEN in = 0
IF in = 0 THEN joy = 0: cap = 0
IF in = 1 THEN joy = 1: cap = 0
IF in = 2 THEN joy = 0: cap = 1
IF py = 57 THEN sense = sense - 5: IF sense < 4 THEN sense = 100
IF py = 61 THEN IF joyfix = 0 THEN joyfix = 1 ELSE joyfix = 0
IF py = 70 THEN grav = grav + .005: IF grav > .04 THEN grav = .005
IF py = 74 THEN IF detail = 0 THEN detail = 1 ELSE detail = 0
IF py = 78 THEN yk = yk + 1
IF yk > 2 THEN yk = 0
IF yk = 0 THEN satan = 0: gal = 0
IF yk = 1 THEN satan = 1: gal = 0
IF yk = 2 THEN satan = 1: gal = 1
IF py = 82 THEN CALL SetDelay
IF py = 90 THEN 85
IF py = 66 THEN SBTimes% = SBTimes% + 1: IF SBTimes% > 15 THEN SBTimes% = 0
IF py = 45 THEN IF spl = 0 THEN spl = 1 ELSE spl = 0
IF py = 86 THEN IF speed = 0 THEN speed = 1 ELSE speed = speed + 2
IF speed > 9 THEN speed = 0

GOTO 84

85 REM-----------------------------------------------------------Save Options
CALL Save
col = 104
FOR wee = 70 TO 0 STEP -.1
IF col < 105 THEN colh = 2
IF col > 150 THEN colh = -2
col = col + colh
CIRCLE (50, 50), wee, col
CIRCLE (50, 50), wee + 10, 0
NEXT wee
GOTO 100

108
LOCATE 1, 1: INPUT Q$
FX VAL(Q$), SBTime%, SBTimer%
RETURN

SUB AI
REM-----------------------------------------------CPU Player AI
IF players = 2 THEN 39
IF RND * 10 > dif + 2 THEN 39
REM---
IF laby2 < 93 OR lbby2 < 93 THEN IF POS2 <> 0 AND POS2 <> 5 THEN 57

'jump away
IF light1 = 1 THEN 48
IF RND * 10 > dif OR hp2 > dif * 2 OR RND > .4 THEN 42
48
IF b2x > b2x2 - (dif * 2) AND b2x < b2x2 THEN wee2$ = "9"
IF b2x > b2x2 AND b2x < b2x2 + (dif * 2) THEN wee2$ = "7"
42
'Lightning Kick
IF RND * 10 > dif OR RND < .998 OR POS2 = 9 OR POS2 = 10 OR spl = 1 THEN 46
IF r2 = 1 AND b2x < b2x2 - 10 THEN weeb$ = "A": wee2$ = "4": LK = dif * 10
IF r2 = 0 AND b2x > b2x2 + 10 THEN weeb$ = "A": wee2$ = "6": LK = dif * 10
46

'walk closer (With-out Special Moves)
IF spl = 0 THEN 111
IF b2x > b2x2 + 20 THEN wee2$ = "6"
IF b2x < b2x2 - 20 THEN wee2$ = "4"
111

'jump closer
IF RND > .01 THEN 41
IF b2x < b2x2 - 30 THEN wee2$ = "7"
IF b2x > b2x2 + 30 THEN wee2$ = "9"
41

'walk closer
IF RND * 10 > dif OR RND > .3 THEN 40
IF light2 = 1 THEN weeb$ = ""
IF b2x > b2x2 + 10 THEN wee2$ = "6"
IF b2x < b2x2 - 10 THEN wee2$ = "4"
40

'Duck
IF RND < .95 THEN 47
IF b2x > b2x2 - 5 AND b2x < b2x2 + 5 THEN wee2$ = "5"
47
IF b2y < b2y2 - 10 THEN 45
'Jab
IF RND < 1 - ((dif + .001) / 10) OR RND < .9 THEN 43
IF r2 = 1 AND b1x > b1x2 - 10 AND b1x < b1x2 THEN weeb$ = "Q"
IF r2 = 0 AND b1x < b1x2 + 10 AND b1x > b1x2 THEN weeb$ = "Q"
43
'Strong
IF RND < 1 - ((dif + .001) / 10) OR RND < .95 OR st2 > 0 THEN 44
IF r2 = 1 AND b1x > b1x2 - 10 AND b1x < b1x2 THEN weeb$ = "W"
IF r2 = 0 AND b1x < b1x2 + 10 AND b1x > b1x2 THEN weeb$ = "W"
44
'Short
IF RND < 1 - ((dif + .001) / 10) OR RND < .99 OR POS1 > 8 THEN 45
IF r2 = 1 AND b2x > b2x2 - 15 AND b2x < b2x2 - 5 THEN weeb$ = "A"
IF r2 = 0 AND b2x < b2x2 + 15 AND b2x > b2x2 + 5 THEN weeb$ = "A"
45
'Avoid Fire Ball
IF firx1 > b2x2 - 40 AND firx1 < b2x2 + 40 AND firy1 > b2y2 - 20 THEN IF lbby2 > 90 AND RND > .5 THEN wee2$ = "8" ELSE IF chrg2 > 50 THEN weeb$ = "Q"
55
'RoundHouse
IF POS1 <> 16 THEN IF RND < 1 - ((dif + .001) / 10) OR RND < .99 THEN 49
IF r2 = 1 AND b2x > b2x2 - 15 AND b2x < b2x2 - 5 THEN weeb$ = "S"
IF r2 = 0 AND b2x < b2x2 + 15 AND b2x > b2x2 + 5 THEN weeb$ = "S"
49
'Charge
IF RND * 10 < dif AND RND > .95 AND chrg2 = 0 THEN weeb$ = "E"
'Dragon Punch
IF chrg2 > 25 AND RND > .89 AND b2x > b2x2 - 4 AND b2x < b2x2 + 4 THEN weeb$ = "W"
50 REM---End Standing AI

REM---Charge Jump
IF chrg2 > 95 AND RND > .5 THEN weeb$ = "S"

'Fire Ball
IF chrg2 > 15 AND RND > .99 AND b2y > b2y2 - 10 AND b2y < b2y2 + 10 AND b2x < b2x2 - 15 AND r2 = 1 THEN weeb$ = "Q"
IF chrg2 > 15 AND RND > .99 AND b2y > b2y2 - 10 AND b2y < b2y2 + 10 AND b2x > b2x2 + 15 AND r2 = 0 THEN weeb$ = "Q"
57
REM---Jump up to P1
IF dif < 7 OR RND > .1 OR b2y > 85 THEN 116
'Stand Still if P1 is overhead
IF b2x > b2x2 - 6 AND b2x < b2x2 + 6 AND lbby < 90 THEN wee2$ = ""
'Jump up or Dragon if P1 is overhead
IF b2x > b2x2 - 4 AND b2x < b2x2 + 4 AND v1 > 0 AND chrg2 > 35 THEN IF RND > .5 THEN weeb$ = "W" ELSE wee2$ = "8"
IF lbby < 90 AND h1 < -.5 AND b2x2 < b2x - 20 THEN wee2$ = "9"
IF lbby < 90 AND h1 > .5 AND b2x2 > b2x + 20 THEN wee2$ = "7"
116
REM---In Air
IF POS2 > 0 OR b2y2 > 80 THEN 70
IF b2x < b2x2 - 15 OR b2x > b2x2 + 15 OR h1y > lbby2 + 5 OR lbby < h2y - 5 THEN 70
IF RND > .3 THEN weeb$ = "W"
IF RND > .3 THEN weeb$ = "A"
IF RND > .3 THEN weeb$ = "S"
70
'Misc.
IF LK > 0 THEN weeb$ = "A": LK = LK - 1: 'Lightning Kick Stuff
IF b2x2 < b2x AND h2 < 0 AND weeb$ = "A" THEN weeb$ = ""
IF b2x2 > b2x AND h2 > 0 AND weeb$ = "A" THEN weeb$ = ""
REM---Don't Jump OVER they're Head
IF dif > 5 AND wee2$ = "7" AND lbby > 90 AND b2x2 < b2x + 20 AND b2x2 > b2x THEN wee2$ = "4"
IF dif > 5 AND wee2$ = "9" AND lbby > 90 AND b2x2 > b2x - 20 AND b2x2 < b2x THEN wee2$ = "6"

REM---Keep Away From Spikes/Fans
IF stage = 5 OR stage = 8 THEN IF b2x2 < 25 AND wee2$ = "7" THEN wee2$ = "8"
IF stage = 5 OR stage = 8 THEN IF b2x2 > 75 AND wee2$ = "9" THEN wee2$ = "8"
IF stage = 5 OR stage = 8 THEN IF b2x2 < -50 AND wee2$ = "" THEN wee2$ = "6"
IF stage = 5 OR stage = 8 THEN IF b2x2 > 150 AND wee2$ = "" THEN wee2$ = "4"
IF stage = 5 OR stage = 8 THEN IF b2x2 < -30 AND wee2$ = "4" THEN wee2$ = ""
IF stage = 5 OR stage = 8 THEN IF b2x2 > 130 AND wee2$ = "6" THEN wee2$ = ""
39
END SUB

SUB BloodFans
REM---Suck Blood into fan(s)
IF stage <> 8 OR fan = 0 THEN 114
IF by0 > 65 AND bx0 < 20 THEN bh0 = bh0 - .01
IF by1 > 65 AND bx1 < 20 THEN bh1 = bh1 - .01
IF by2 > 65 AND bx2 < 20 THEN bh2 = bh2 - .01
IF by3 > 65 AND bx3 < 20 THEN bh3 = bh3 - .01
IF by4 > 65 AND bx4 < 20 THEN bh4 = bh4 - .01
IF by5 > 65 AND bx5 < 20 THEN bh5 = bh5 - .01
IF by6 > 65 AND bx6 < 20 THEN bh6 = bh6 - .01
IF by7 > 65 AND bx7 < 20 THEN bh7 = bh7 - .01
IF by8 > 65 AND bx8 < 20 THEN bh8 = bh8 - .01
IF by9 > 65 AND bx9 < 20 THEN bh9 = bh9 - .01

IF by0 > 65 AND bx0 > 80 THEN bh0 = bh0 + .01
IF by1 > 65 AND bx1 > 80 THEN bh1 = bh1 + .01
IF by2 > 65 AND bx2 > 80 THEN bh2 = bh2 + .01
IF by3 > 65 AND bx3 > 80 THEN bh3 = bh3 + .01
IF by4 > 65 AND bx4 > 80 THEN bh4 = bh4 + .01
IF by5 > 65 AND bx5 > 80 THEN bh5 = bh5 + .01
IF by6 > 65 AND bx6 > 80 THEN bh6 = bh6 + .01
IF by7 > 65 AND bx7 > 80 THEN bh7 = bh7 + .01
IF by8 > 65 AND bx8 > 80 THEN bh8 = bh8 + .01
IF by9 > 65 AND bx9 > 80 THEN bh9 = bh9 + .01

IF gal = 0 THEN 114
IF bya > 65 AND bxa < 20 THEN bha = bha - .05
IF byb > 65 AND bxb < 20 THEN bhb = bhb - .05
IF byc > 65 AND bxc < 20 THEN bhc = bhc - .05
IF byd > 65 AND bxd < 20 THEN bhd = bhd - .05
IF bye > 65 AND bxe < 20 THEN bhe = bhe - .05
IF byf > 65 AND bxf < 20 THEN bhf = bhf - .05
IF byg > 65 AND bxg < 20 THEN bhg = bhg - .05
IF byh > 65 AND bxh < 20 THEN bhh = bhh - .05
IF byi > 65 AND bxi < 20 THEN bhi = bhi - .05
IF byj > 65 AND bxj < 20 THEN bhj = bhj - .05
                                         
IF bya > 65 AND bxa > 80 THEN bha = bha + .05
IF byb > 65 AND bxb > 80 THEN bhb = bhb + .05
IF byc > 65 AND bxc > 80 THEN bhc = bhc + .05
IF byd > 65 AND bxd > 80 THEN bhd = bhd + .05
IF bye > 65 AND bxe > 80 THEN bhe = bhe + .05
IF byf > 65 AND bxf > 80 THEN bhf = bhf + .05
IF byg > 65 AND bxg > 80 THEN bhg = bhg + .05
IF byh > 65 AND bxh > 80 THEN bhh = bhh + .05
IF byi > 65 AND bxi > 80 THEN bhi = bhi + .05
IF byj > 65 AND bxj > 80 THEN bhj = bhj + .05
114

REM---Suck Blood into fan(s)
IF stage <> 11 OR fan = 0 THEN 134
IF by0 > 55 AND bx0 > 35 AND bx0 < 65 THEN bv0 = bv0 - .06
IF by1 > 55 AND bx1 > 35 AND bx1 < 65 THEN bv1 = bv1 - .06
IF by2 > 55 AND bx2 > 35 AND bx2 < 65 THEN bv2 = bv2 - .06
IF by3 > 55 AND bx3 > 35 AND bx3 < 65 THEN bv3 = bv3 - .06
IF by4 > 55 AND bx4 > 35 AND bx4 < 65 THEN bv4 = bv4 - .06
IF by5 > 55 AND bx5 > 35 AND bx5 < 65 THEN bv5 = bv5 - .06
IF by6 > 55 AND bx6 > 35 AND bx6 < 65 THEN bv6 = bv6 - .06
IF by7 > 55 AND bx7 > 35 AND bx7 < 65 THEN bv7 = bv7 - .06
IF by8 > 55 AND bx8 > 35 AND bx8 < 65 THEN bv8 = bv8 - .06
IF by9 > 55 AND bx9 > 35 AND bx9 < 65 THEN bv9 = bv9 - .06

IF gal = 0 THEN 114
IF bya > 55 AND bxa > 35 AND bxa < 65 THEN bva = bva - .06
IF byb > 55 AND bxb > 35 AND bxb < 65 THEN bvb = bvb - .06
IF byc > 55 AND bxc > 35 AND bxc < 65 THEN bvc = bvc - .06
IF byd > 55 AND bxd > 35 AND bxd < 65 THEN bvd = bvd - .06
IF bye > 55 AND bxe > 35 AND bxe < 65 THEN bve = bve - .06
IF byf > 55 AND bxf > 35 AND bxf < 65 THEN bvf = bvf - .06
IF byg > 55 AND bxg > 35 AND bxg < 65 THEN bvg = bvg - .06
IF byh > 55 AND bxh > 35 AND bxh < 65 THEN bvh = bvh - .06
IF byi > 55 AND bxi > 35 AND bxi < 65 THEN bvi = bvi - .06
IF byj > 55 AND bxj > 35 AND bxj < 65 THEN bvj = bvj - .06
                                         
134
END SUB

SUB ColorSelect
eyes = 0
pnt = 34
pot = 34
SCREEN 7, 0, 0, 0: WINDOW SCREEN (0, 0)-(100, 100)
FX 5, SBTime%, SBTimer%
FOR wee = 0 TO 100 STEP .1
LINE (wee, 40)-(wee, 60), 4
LINE (wee - 5, 45)-(wee - 5, 55), 0
LINE (wee - 95.4, 45)-(wee - 95.4, 55), 4
NEXT wee
FOR wee = 0 TO 25 STEP .1
LINE (0, 45 - wee)-(100, 45 - wee), 4
LINE (0, 55 + wee)-(100, 55 + wee), 4
LINE (5, 50 - wee)-(95, 50 - wee), 0
LINE (5, 50 + wee)-(95, 50 + wee), 0
NEXT wee
COLOR 4
LOCATE 8, 10: PRINT "Player-1"
LOCATE 9, 10: PRINT " Arms"
LOCATE 10, 10: PRINT " Body"
LOCATE 11, 10: PRINT " Legs"
LOCATE 13, 10: PRINT "Player-2"
LOCATE 14, 10: PRINT " Arms"
LOCATE 15, 10: PRINT " Body"
LOCATE 16, 10: PRINT " Legs"
LOCATE 18, 10: PRINT "Done"
103 wee$ = INKEY$
IF wee$ = CHR$(13) THEN wee$ = " "
LOCATE 12, 10: IF P1d = 0 THEN PRINT " Detail Low " ELSE PRINT " Detail High"
LOCATE 17, 10: IF P2d = 0 THEN PRINT " Detail Low " ELSE PRINT " Detail High"
'Draw Colors
LINE (35, 33)-(37, 35), ac1, BF
LINE (35, 33)-(37, 35), 4, B
LINE (35, 37)-(37, 39), bc1, BF
LINE (35, 37)-(37, 39), 4, B
LINE (35, 41)-(37, 43), lc1, BF
LINE (35, 41)-(37, 43), 4, B

LINE (35, 53.5)-(37, 55.5), ac2, BF
LINE (35, 53.5)-(37, 55.5), 4, B
LINE (35, 57.5)-(37, 59.5), bc2, BF
LINE (35, 57.5)-(37, 59.5), 4, B
LINE (35, 61.5)-(37, 63.5), lc2, BF
LINE (35, 61.5)-(37, 63.5), 4, B

IF pnt > pot THEN pot = pot + 1
IF pnt < pot THEN pot = pot - 1
IF pnt > pot + 20 THEN pot = pot + 3
IF pnt < pot - 20 THEN pot = pot - 3

'Draw Pointer
LINE (11, pot - 4)-(15, pot + 4), 0, BF
PSET (12 + ((RND - .5) * 1.5), pot + ((RND - .5) * 1.5)), 4
CIRCLE (12, pot), 1, 4
LINE (13, pot)-(15, pot), 4
PSET (11.7, pot - .6), 5
PSET (11.4, pot - .6), 4

IF wee$ = CHR$(0) + "H" THEN wee$ = "8"
IF wee$ = CHR$(0) + "K" THEN wee$ = "4"
IF wee$ = CHR$(0) + "P" THEN wee$ = "5"
IF wee$ = CHR$(0) + "M" THEN wee$ = "6"
'input
IF wee$ = "8" THEN pnt = pnt - 4.07: FX 11, SBTime%, SBTimer%
IF wee$ = "5" THEN pnt = pnt + 4.07: FX 11, SBTime%, SBTimer%

IF pnt > 70.63 THEN pnt = 70.63
IF pnt < 34 THEN pnt = 34

py = INT(pnt)
IF wee$ <> " " THEN 103
IF py = 34 AND ac1 = 0 THEN ac1 = 4: GOTO 103
IF py = 34 AND ac1 = 4 THEN ac1 = 2: GOTO 103
IF py = 34 AND ac1 = 2 THEN ac1 = 8: GOTO 103
IF py = 34 AND ac1 = 8 THEN ac1 = 7: GOTO 103
IF py = 34 AND ac1 = 7 THEN ac1 = 0: GOTO 103

IF py = 38 AND bc1 = 0 THEN bc1 = 4: GOTO 103
IF py = 38 AND bc1 = 4 THEN bc1 = 2: GOTO 103
IF py = 38 AND bc1 = 2 THEN bc1 = 8: GOTO 103
IF py = 38 AND bc1 = 8 THEN bc1 = 7: GOTO 103
IF py = 38 AND bc1 = 7 THEN bc1 = 0: GOTO 103

IF py = 42 AND lc1 = 0 THEN lc1 = 4: GOTO 103
IF py = 42 AND lc1 = 4 THEN lc1 = 2: GOTO 103
IF py = 42 AND lc1 = 2 THEN lc1 = 8: GOTO 103
IF py = 42 AND lc1 = 8 THEN lc1 = 7: GOTO 103
IF py = 42 AND lc1 = 7 THEN lc1 = 0: GOTO 103

IF py = 54 AND ac2 = 0 THEN ac2 = 4: GOTO 103
IF py = 54 AND ac2 = 4 THEN ac2 = 2: GOTO 103
IF py = 54 AND ac2 = 2 THEN ac2 = 8: GOTO 103
IF py = 54 AND ac2 = 8 THEN ac2 = 7: GOTO 103
IF py = 54 AND ac2 = 7 THEN ac2 = 0: GOTO 103

IF py = 58 AND bc2 = 0 THEN bc2 = 4: GOTO 103
IF py = 58 AND bc2 = 4 THEN bc2 = 2: GOTO 103
IF py = 58 AND bc2 = 2 THEN bc2 = 8: GOTO 103
IF py = 58 AND bc2 = 8 THEN bc2 = 7: GOTO 103
IF py = 58 AND bc2 = 7 THEN bc2 = 0: GOTO 103

IF py = 62 AND lc2 = 0 THEN lc2 = 4: GOTO 103
IF py = 62 AND lc2 = 4 THEN lc2 = 2: GOTO 103
IF py = 62 AND lc2 = 2 THEN lc2 = 8: GOTO 103
IF py = 62 AND lc2 = 8 THEN lc2 = 7: GOTO 103
IF py = 62 AND lc2 = 7 THEN lc2 = 0: GOTO 103

IF py = 46 THEN IF P1d = 0 THEN P1d = 1 ELSE P1d = 0
IF py = 66 THEN IF P2d = 0 THEN P2d = 1 ELSE P2d = 0

IF py = 70 THEN 104
GOTO 103
104
FX 5, SBTime%, SBTimer%
FOR wee = 0 TO 40 STEP .1
LINE (0, 35 - wee)-(100, 35 - wee), 4
LINE (0, 65 + wee)-(100, 65 + wee), 4
LINE (0, 50 - wee)-(100, 50 - wee), 0
LINE (0, 50 + wee)-(100, 50 + wee), 0
NEXT wee
CLS
SCREEN 13: WINDOW SCREEN (0, 0)-(100, 100)
REDIM PAL(255) AS HUES
DEF SEG = VARSEG(PAL(0))
BLOAD Drv$ + ":" + Path$ + "\shades.PAL", 0
SETHUES PAL(), 0, 255
END SUB

DEFINT A-Z
FUNCTION DetectCard%

'Returns -1 (true) if detected and 0 (false) if not.
                                                         
CALL WriteReg(&H4, &H60)
CALL WriteReg(&H4, &H80)
B = INP(&H388)
CALL WriteReg(&H2, &HFF)
CALL WriteReg(&H4, &H21)
FOR X = 0 TO 130
a = INP(&H388)
NEXT X
C = INP(&H388)
CALL WriteReg(&H4, &H60)
CALL WriteReg(&H4, &H80)
SUCCESS = 0
IF (B AND &HE0) = &H0 THEN
IF (C AND &HE0) = &HC0 THEN
SUCCESS = -1
END IF
END IF
DetectCard% = SUCCESS
END FUNCTION

SUB FX (fxx%, SBTime%, SBTimer%)
IF SBSound% = 2 THEN 107
IF SBTimer% = 0 THEN
SBTimer% = SBTime
'Freq%, Wave%, FEEDBACK%, Modl%, CLEN%
SELECT CASE fxx%
CASE IS = 1
FOR n = 1 TO 40
SBPLAY1 100, 1, 1, 0, 1: 'Jab
NEXT n
CALL WriteReg(&HB0, &H0)
CASE IS = 2
FOR n = 1 TO 60
SBPLAY1 50, 1, 1, 0, 1: 'Strong
NEXT n
CALL WriteReg(&HB0, &H0)
CASE IS = 3
FOR n = 1 TO 45
SBPLAY1 25, 1, 1, 0, 1: 'Short
NEXT n
CALL WriteReg(&HB0, &H0)
CASE IS = 4
FOR n = 1 TO 40
SBPLAY1 1, 1, 1, 0, 1: 'Round House
NEXT n
CALL WriteReg(&HB0, &H0)
CASE IS = 5
FOR n = 1 TO 45
SBPLAY1 200, 0, 12, 131, 262: 'Hit Ground
NEXT n
CALL WriteReg(&HB0, &H0)
CASE IS = 6
FOR n = 1 TO 45
SBPLAY1 25, 0, 1000, 10, 261: 'Throw Fire/Dragon
NEXT n
CALL WriteReg(&HB0, &H0)
CASE IS = 7
FOR n = 1 TO 45
SBPLAY1 255, 0, 14, 16, 9: 'Charge
NEXT n
CALL WriteReg(&HB0, &H0)
CASE IS = 8
FOR n = 1 TO 45
SBPLAY1 25, 0, 1000, 10, 259: 'Burning
NEXT n
CALL WriteReg(&HB0, &H0)
CASE IS = 9
FOR n = 1 TO 45
SBPLAY1 75, 4, 14, 0, 6: 'Hit 1
NEXT n
CALL WriteReg(&HB0, &H0)
CASE IS = 10
FOR n = 1 TO 45
SBPLAY1 123, 4, 14, 0, 6: 'Hit 2
NEXT n
CALL WriteReg(&HB0, &H0)
CASE IS = 11
FOR n = 1 TO 45
SBPLAY1 50, 1, 0, 50, 259: 'Selected
NEXT n
CALL WriteReg(&HB0, &H0)
CASE IS = 12
FOR n = 1 TO 45
SBPLAY1 50, 2, 0, 13, 258: 'Gong
NEXT n
CALL WriteReg(&HB0, &H0)
CASE IS = 13
FOR n = 1 TO 45
SBPLAY1 0, 1, 10, 1, 7: 'Blood (Landing)
NEXT n
CALL WriteReg(&HB0, &H0)
CASE IS = 14
FOR n = 1 TO 45
SBPLAY1 100, 2, 16, 135, 258: 'Decap.
NEXT n
CALL WriteReg(&HB0, &H0)
CASE IS = 15
FOR n = 1 TO 45
SBPLAY1 50, 2, 0, 13, 259: 'Eyes
NEXT n
CALL WriteReg(&HB0, &H0)
CASE IS = 16
FOR n = 1 TO 45
SBPLAY1 14, 1, 14, 7, 260: 'Fan
NEXT n
CALL WriteReg(&HB0, &H0)
CASE IS = 17
FOR n = 1 TO 45
SBPLAY1 69, 1, 14, 5, 4: 'Chopping
NEXT n
CALL WriteReg(&HB0, &H0)
CASE IS = 18
FOR n = 1 TO 45
SBPLAY1 35, -1, 62, 0, 260: 'Laser
NEXT n
CALL WriteReg(&HB0, &H0)
CASE IS = 19
FOR n = 1 TO 45
SBPLAY1 60, 3, 14, 15, 19: 'Laser
NEXT n
CALL WriteReg(&HB0, &H0)

END SELECT
END IF
107
END SUB

DEFSNG A-Z
SUB intro1
1 READ letter$
col = 16
slot = slot + 1
FOR wee = 21 TO 1 STEP -2
COLOR col: col = col + 1: IF col > 31 THEN col = 31
LOCATE wee, slot
PRINT letter$: LOCATE wee + 2, slot: PRINT " "
FOR dly = 0 TO 1000: NEXT dly
NEXT wee
wee$ = INKEY$: IF wee$ <> "" THEN 98
SOUND 37, .1
IF slot < 20 THEN 1
RANDOMIZE (TIMER): KEY OFF
FOR wee = 100 TO 300 STEP 75: SOUND wee, .5: NEXT wee: PLAY "MBT255L35N1L2N80"
COLOR 15: LOCATE 1, 1: PRINT "Stick Fighters Brawl": WINDOW SCREEN (0, 0)-(100, 100)

FOR wee = 0 TO 40000: NEXT wee
FOR wee = 10 TO 0 STEP -.005: LINE (0, wee)-(100, wee), 0: NEXT wee
FOR wee = 500 TO 100 STEP -50: SOUND wee, .3: NEXT wee

FOR wee = 31 TO 16 STEP -.5
COLOR wee
LOCATE 23, 1: PRINT "Programed by Kevin Reems"
FOR dly = 0 TO 4000: NEXT dly
NEXT wee
98
END SUB

SUB move
REM ----------------------------------------------MOMENTOM
IF satan = 0 THEN 65
REM---Blood
bv0 = bv0 + grav
bv1 = bv1 + grav
bv2 = bv2 + grav
bv3 = bv3 + grav
bv4 = bv4 + grav
bv5 = bv5 + grav
bv6 = bv6 + grav
bv7 = bv7 + grav
bv8 = bv8 + grav
bv9 = bv9 + grav

bx0 = bx0 + bh0
bx1 = bx1 + bh1
bx2 = bx2 + bh2
bx3 = bx3 + bh3
bx4 = bx4 + bh4
bx5 = bx5 + bh5
bx6 = bx6 + bh6
bx7 = bx7 + bh7
bx8 = bx8 + bh8
bx9 = bx9 + bh9

by0 = by0 + bv0
by1 = by1 + bv1
by2 = by2 + bv2
by3 = by3 + bv3
by4 = by4 + bv4
by5 = by5 + bv5
by6 = by6 + bv6
by7 = by7 + bv7
by8 = by8 + bv8
by9 = by9 + bv9

IF gal = 0 THEN 65
bva = bva + grav
bvb = bvb + grav
bvc = bvc + grav
bvd = bvd + grav
bve = bve + grav
bvf = bvf + grav
bvg = bvg + grav
bvh = bvh + grav
bvi = bvi + grav
bvj = bvj + grav

bxa = bxa + bha
bxb = bxb + bhb
bxc = bxc + bhc
bxd = bxd + bhd
bxe = bxe + bhe
bxf = bxf + bhf
bxg = bxg + bhg
bxh = bxh + bhh
bxi = bxi + bhi
bxj = bxj + bhj

bya = bya + bva
byb = byb + bvb
byc = byc + bvc
byd = byd + bvd
bye = bye + bve
byf = byf + bvf
byg = byg + bvg
byh = byh + bvh
byi = byi + bvi
byj = byj + bvj
65
REM---Player 1
firx1 = firx1 + firh1
b1x = b1x + b1h
b1y = b1y + b1v
b2x = b2x + b2h
b2y = b2y + b2v
aaax = aaax + aaah
aaay = aaay + aaav
abax = abax + abah
abay = abay + abav
aabx = aabx + aabh
aaby = aaby + aabv
abbx = abbx + abbh
abby = abby + abbv

laax = laax + laah
labx = labx + labh
lbax = lbax + lbah
lbbx = lbbx + lbbh
laay = laay + laav
laby = laby + labv
lbay = lbay + lbav
lbby = lbby + lbbv

b1x = b1x + h1
b1y = b1y + v1
b2x = b2x + h1
b2y = b2y + v1
h1x = h1x + h1
h1y = h1y + v1
aaax = aaax + h1
aaay = aaay + v1
abax = abax + h1
abay = abay + v1
aabx = aabx + h1
aaby = aaby + v1
abbx = abbx + h1
abby = abby + v1
laax = laax + h1
labx = labx + h1
lbax = lbax + h1
lbbx = lbbx + h1
laay = laay + v1
laby = laby + v1
lbay = lbay + v1
lbby = lbby + v1

b1x = b1x + addx1
b2x = b2x + addx1
h1x = h1x + addx1
aaax = aaax + addx1
abax = abax + addx1
aabx = aabx + addx1
abbx = abbx + addx1
laax = laax + addx1
labx = labx + addx1
lbax = lbax + addx1
lbbx = lbbx + addx1
addx1 = 0

REM---Player 2
IF hp1 = 0 OR hp2 = 0 THEN IF POS2 = 9 AND b2y2 > 90 THEN h2 = h2 / 1.1
firx2 = firx2 + firh2
IF hp1 = 0 AND players = 1 THEN 58
b1x2 = b1x2 + b1h2
b1y2 = b1y2 + b1v2
b2x2 = b2x2 + b2h2
b2y2 = b2y2 + b2v2
aaax2 = aaax2 + aaah2
aaay2 = aaay2 + aaav2
abax2 = abax2 + abah2
abay2 = abay2 + abav2
aabx2 = aabx2 + aabh2
aaby2 = aaby2 + aabv2
abbx2 = abbx2 + abbh2
abby2 = abby2 + abbv2

laax2 = laax2 + laah2
labx2 = labx2 + labh2
lbax2 = lbax2 + lbah2
lbbx2 = lbbx2 + lbbh2
laay2 = laay2 + laav2
laby2 = laby2 + labv2
lbay2 = lbay2 + lbav2
lbby2 = lbby2 + lbbv2
58
IF hp1 = 0 AND v2 < 0 THEN v2 = 0
b1x2 = b1x2 + h2
b1y2 = b1y2 + v2
b2x2 = b2x2 + h2
b2y2 = b2y2 + v2
h2x = h2x + h2
h2y = h2y + v2
aaax2 = aaax2 + h2
aaay2 = aaay2 + v2
abax2 = abax2 + h2
abay2 = abay2 + v2
aabx2 = aabx2 + h2
aaby2 = aaby2 + v2
abbx2 = abbx2 + h2
abby2 = abby2 + v2
laax2 = laax2 + h2
labx2 = labx2 + h2
lbax2 = lbax2 + h2
lbbx2 = lbbx2 + h2
laay2 = laay2 + v2
laby2 = laby2 + v2
lbay2 = lbay2 + v2
lbby2 = lbby2 + v2

b1x2 = b1x2 + addx2
b2x2 = b2x2 + addx2
h2x = h2x + addx2
aaax2 = aaax2 + addx2
abax2 = abax2 + addx2
aabx2 = aabx2 + addx2
abbx2 = abbx2 + addx2
laax2 = laax2 + addx2
labx2 = labx2 + addx2
lbax2 = lbax2 + addx2
lbbx2 = lbbx2 + addx2
addx2 = 0

END SUB

SUB P1Animation1
REM---Player 1                                                       Player 1
REM--------------------------------------------POS-0 Standing/Jumping/Ducking
IF POS1 <> 0 THEN 3

'body
IF b1y < b2y - 6 THEN b1v = b1v + .01
IF b1y > b2y - 5 THEN b1v = b1v - .01
IF b1y > b2y - 6 AND b1y < b2y - 5 THEN b1v = b1v / 2
IF b1x < b2x + 1 THEN b1h = b1h + .001
IF b1x > b2x + 1 THEN b1h = b1h - .001
IF b1h < -.05 OR b1h > .05 THEN b1h = b1h / 1.1
IF b1y < b2y - 7 THEN b1y = b2y - 7
IF dk1 = 1 THEN b2t = lbby - 5 ELSE b2t = lbby - 10
IF b2y > b2t THEN b2y = b2y - .5
IF b2y < b2t THEN b2y = b2y + .5
IF b2y > lbby - 10 THEN b2v = b2v - .005
IF b2y < lbby - 10 AND v1 > -1 THEN b2y = lbby - 10: b2v = b2v / 2
IF b1y > b2y THEN b1y = b2y

'left leg
IF labx > b2x + 3 THEN labx = labx - .5
IF labx < b2x + 2 THEN labh = labh + .01
IF labx > b2x + 2 AND labx < b2x + 3 AND labh <> 0 THEN labh = labh / 2
IF laax < labx THEN laax = labx - .01: IF b1h > 0 THEN laah = b1h
IF laax > b2x + 4 THEN laax = b2x + 4: laah = -.01
IF laax > labx + 1 THEN laah = laah - .01
IF laay < laby - 5 THEN laay = laay + .5
IF laay > laby - 5 THEN laay = laay - .5
IF laby < b2y + 10 THEN laby = laby + .1
IF laby > b2y + 10 THEN laby = laby - .5

IF laby < b2y + 10 THEN labv = labv + .01

IF dk1 = 1 THEN laax = b2x + 4: laay = b2y + 1: laby = 95
IF b2y < lbby - 10 AND v1 < -1 THEN lbby = b2y + 10
IF laby > 95 THEN laby = 95
IF b2y < 80 AND laby > b2y + 5 THEN laby = laby - .5
IF laby > b2y + 10 THEN laby = b2y + 10

'Right leg
IF lbbx > b2x - 1.5 THEN lbbh = lbbh - .001
IF lbbx < b2x - 2 THEN lbbh = lbbh + .001
IF lbbx < b2x - 3 THEN lbbx = b2x - 3
IF lbbx > b2x - 2 AND lbbx < b2x - 1.5 AND lbbh <> 0 THEN lbbh = lbbh / 2
IF lbax < b2x THEN lbah = lbah + .001: lbax = b2x
IF lbax > b2x THEN lbah = lbah - .001
IF lbax > b2x - 1 AND lbax < b2x THEN lbah = lbah / 2
IF lbay < lbby - 5 THEN lbav = lbav + .01
IF lbay > lbby - 5 THEN lbav = lbav - .01
IF lbay > b2y + 4.5 AND lbay < b2y + 5.5 THEN lbav = lbav / 2
IF lbby > 95 THEN lbby = 95
IF lbby > b2y + 10 THEN lbby = b2y + 10

'left arm
abav = 0
IF abby < b1y THEN abby = b1y
IF abay < b1y + 4 THEN abay = abay + .2
IF abay > b1y + 4 THEN abay = abay - .2
IF abay > b1y + 5 THEN abay = b1y + 5
IF abax < b1x THEN abax = abax + .2
IF abax > b1x THEN abax = abax - .2
IF abbx < b1x + 2 THEN abbh = abbh + .01: abbx = b1x + 2
IF abbx > b1x + 2 THEN abbh = abbh - .04
IF abbx > b1x + 2 AND abbx < b2x + 3 AND abbh <> 0 THEN abbh = abbh / 1.2
IF abby < b1y + 1 THEN abbv = abbv + .01
IF abby > b1y + 2 THEN abbv = abbv - .01
IF abby > b1y + 1 AND abby < b1y + 2 THEN abbv = abbv / 2

'right arm
aaav = 0
IF aabx < b1x - 2 THEN aabx = b1x - 2
IF aaay > b1y + 5 THEN aaay = b1y + 5
IF aaay < b1y + 2 THEN aaay = aaay + .2
IF aaay > b1y + 2 THEN aaay = aaay - .2
IF aaax < b1x - 2 THEN aaax = aaax + .2
IF aaax > b1x - 2 THEN aaax = aaax - .2
IF aabx < b1x + 1 THEN aabx = aabx + .001
IF aabx > b1x + 2 THEN aabx = aabx - .5
IF aabx < b1x + 1 THEN aabh = aabh + .001
IF aabx > b1x AND aabx < b2x + 2 THEN aabh = aabh / 2
IF aaby < b1y + 2.5 THEN aaby = aaby + .5
IF aaby > b1y + 3.5 THEN aaby = aaby - .5

IF laby > 93 AND lbby > 93 AND b2x < b2x2 THEN r1 = 0
IF laby > 93 AND lbby > 93 AND b2x > b2x2 THEN r1 = 1

h1x = b1x: h1y = b1y - 3
IF b1x > b2x + 3 THEN b1x = b2x + 3
3

REM----------------------------------------------------------------POS-1 Kick
IF POS1 <> 1 THEN 6
rh1 = 1
aabh = 0
abbh = 0
aabv = 0
abbv = 0
h1 = h1 / 1.1
b2x = labx
b1h = 0: b1v = 0
aaah = 0: abah = 0
IF lbbx < b2x + 8 THEN lbbx = lbbx + 1
IF lbby > b2y - 10 THEN lbby = lbby - 1
IF lbax < b2x + 3 THEN lbax = lbax + 2
IF lbay > b2y - 5 THEN lbay = lbay - 2

frt1 = frt1 - 1
IF frt1 = 0 THEN POS1 = 2: frt1 = 15

h1x = b1x: h1y = b1y - 3
6
REM----------------------------------------------------------------POS-2 Kick
IF POS1 <> 2 THEN 7
h1 = h1 / 1.1
b2x = b2x - .1
IF lbbx > b2x - 4 THEN lbbx = lbbx - 1
IF lbby < b2y + 10 THEN lbby = lbby + 1
IF lbax > b2x - 3 THEN lbax = lbax - 1
IF lbay < b2y + 5 THEN lbay = lbay + 1

frt1 = frt1 - 1
IF frt1 = 0 THEN POS1 = 0

h1x = b1x: h1y = b1y - 3
7
END SUB

SUB P1Animation2
REM--------------------------------------------------------------POS-5 Charge
IF POS1 <> 5 THEN 9

'body
IF b1y < b2y - 5 THEN b1v = b1v + .01
IF b1y > b2y - 4 THEN b1v = b1v - .01
IF b1y > b2y - 5 AND b1y < b2y - 5 THEN b1v = b1v / 2
IF b1x < b2x + 1 THEN b1h = b1h + .001
IF b1x > b2x + 1 THEN b1h = b1h - .001
IF b1h < -.05 OR b1h > .05 THEN b1h = b1h / 1.1
IF b1y < b2y - 7 THEN b1y = b2y - 7
IF dk1 = 1 THEN b2t = lbby - 5 ELSE b2t = lbby - 10
IF b2y > b2t THEN b2y = b2y - .5
IF b2y < b2t THEN b2y = b2y + .5
IF b2y > lbby - 10 THEN b2v = b2v - .005
IF b2y < lbby - 10 AND v1 > -1 THEN b2y = lbby - 10: b2v = b2v / 2

'left leg
IF labx > b2x + 3 THEN labx = labx - .5
IF labx < b2x + 2 THEN labh = labh + .01
IF labx > b2x + 2 AND labx < b2x + 3 AND labh <> 0 THEN labh = labh / 2
IF laax < labx THEN laax = labx - .01: IF b1h > 0 THEN laah = b1h
IF laax > b2x + 4 THEN laax = b2x + 4: laah = -.01
IF laax > labx + 1 THEN laah = laah - .01
IF laay < laby - 5 THEN laay = laay + .5
IF laay > laby - 5 THEN laay = laay - .5
IF laby < b2y + 10 THEN laby = laby + .1
IF laby > b2y + 10 THEN laby = laby - .5

IF laby < b2y + 10 THEN labv = labv + .01

IF dk1 = 1 THEN laax = b2x + 4: laay = b2y + 1: laby = 95
IF b2y < lbby - 10 AND v1 < -1 THEN lbby = b2y + 10
IF laby > 95 THEN laby = 95
IF b2y < 80 AND laby > b2y + 5 THEN laby = laby - .5
IF laby > b2y + 10 THEN laby = b2y + 10

'Right leg
IF lbbx > b2x - 1.5 THEN lbbh = lbbh - .001
IF lbbx < b2x - 2 THEN lbbh = lbbh + .001
IF lbbx > b2x - 2 AND lbbx < b2x - 1.5 AND lbbh <> 0 THEN lbbh = lbbh / 2
IF lbax < b2x THEN lbah = lbah + .001
IF lbax > b2x THEN lbah = lbah - .001
IF lbax > b2x - 1 AND lbax < b2x THEN lbah = lbah / 2
IF lbay < lbby - 5 THEN lbav = lbav + .01
IF lbay > lbby - 5 THEN lbav = lbav - .01
IF lbay > b2y + 4.5 AND lbay < b2y + 5.5 THEN lbav = lbav / 2
IF lbby > 95 THEN lbby = 95
IF lbby > b2y + 10 THEN lbby = b2y + 10

'left arm
IF abay > b1y + 5 THEN abay = b1y + 5
IF abay < b1y + 4 THEN abay = abay + .1
IF abay > b1y + 4 THEN abay = abay - .1
IF abax < b1x + 2 THEN abax = abax + .1
IF abax > b1x + 2 THEN abax = abax - .1
IF abbx > b1x - 1 THEN abbx = abbx - .1
IF abbx < b1x - 1 THEN abbx = abbx + .1
IF abby < b1y + 1 THEN abbv = abbv + .01
IF abby > b1y + 2 THEN abbv = abbv - .01
IF abby > b1y + 1 AND abby < b1y + 2 THEN abbv = abbv / 2

'right arm
IF aaay > b1y + 5 THEN aaay = b1y + 5
IF aaay < b1y + 2 THEN aaay = aaay + .1
IF aaay > b1y + 2 THEN aaay = aaay - .1
IF aaax < b1x - 4 THEN aaax = aaax + .1
IF aaax > b1x - 4 THEN aaax = aaax - .1
IF aabx < b1x - 2 THEN aabx = aabx + .1
IF aabx > b1x - 2 THEN aabx = aabx - .1
IF aabx < b1x THEN aabh = aabh / 2
IF aaby < b1y + 2.5 THEN aaby = aaby + .5
IF aaby > b1y + 3.5 THEN aaby = aaby - .5

IF laby > 94 AND lbby > 94 AND b2x < b2x2 THEN r1 = 0
IF laby > 94 AND lbby > 94 AND b2x > b2x2 THEN r1 = 1

IF h1y < b1y - 3 THEN h1y = b1y - 3
IF h1y > b1y - 2 THEN h1y = b1y - 2
IF h1y < b1y - 2 THEN h1y = h1y + .1
IF h1y > b1y - 2 THEN h1y = h1y - .1
h1x = b1x + .5
IF chrg1 > 100 THEN POS1 = 0
chrg1 = chrg1 + 1.5
IF chrg1 > 100 THEN POS1 = 0
9
REM--------------------------------------------------------POS-8 DRAGON PUNCH
IF POS1 <> 8 THEN 13
b1x = b2x
b1y = b2y - 8
h1x = b1x: h1y = b1y - 3
labv = 0: lbbv = 0
laav = 0: lbav = 0
laay = b2y + 5
lbay = b2y + 6
lbbx = b1x + ((b2x - aaax) / 4)
labx = laax
IF laby > b2y + 10 THEN laby = b2y + 10
IF lbby > b2y + 10 THEN lbby = b2y + 10
abay = b1y + 5
abby = b1y + 8
IF aaax < b2x - 3 THEN aaah = .1: aabh = .1: abah = -.1: abbh = -.1: laah = .1: labh = .1: aaav = .05: aabv = .03: abav = 0: abbv = 0
IF v1 > 0 AND b2y > 75 THEN POS1 = 0
IF aaax > b2x + 3 OR lbby > 95 THEN POS1 = 0
h1 = h1 / 1.05
13
REM--------------------------------------------------------POS-9 Fall
IF POS1 <> 9 THEN 61
lbbh1 = 0
light1 = 0
wee$ = "": llc1 = 0
v1 = v1 + grav
b1v = 0
IF b2x > -60 AND b2x < 160 AND lbby > 93 THEN v1 = 0: lbbv = 0: b2v = 0
IF b2x < -60 OR b2x > 160 THEN hp1 = 0: IF b2y > 155 THEN v1 = 0: b2y = 155 ELSE v1 = v1 + grav

IF b1x > b2x - 5 THEN b1x = b1x - .3 ELSE b1h = 0
IF b1y < b2y THEN b1y = b1y + .3
IF b2x > lbbx - 5 THEN b2x = b2x - .5
IF b2y < lbby THEN b2y = b2y + .5
IF lbax > lbbx - 3 THEN lbax = lbax - .5
IF lbay < lbby - 2 THEN lbay = lbay + .5
IF laax > lbbx - 3 THEN laax = laax - .5
IF laay < lbby - 1 THEN laay = laay + .5
laby = lbby
IF aabx > b1x - 5 THEN aabx = aabx - .5 ELSE aabx = b1x - 5
IF abbx > b1x - 5 THEN abbx = abbx - .5 ELSE abbx = b1x - 5
IF aaax > b1x - 3 THEN aaax = aaax - .5 ELSE aaax = b1x - 3
IF abax > b1x - 3 THEN abax = abax - .5 ELSE abax = b1x - 3
IF b2y > laay + 5 THEN b2y = laay + 5
labh = 0: lbbh = 0
aaby = b1y - 1
abby = b1y + 1
aaay = b1y - .5
abay = b1y + 1
laav = 0
lbav = 0
h1x = b1x - 2
h1y = b1y
IF b1y > b2y THEN b1y = b2y
IF hp1 = 0 OR ABS(h1) > .5 OR ABS(v1) > .5 OR b2y < 94 THEN 61
fel1 = fel1 + (hp1 / 25) + 5
IF fel1 > 100 THEN POS1 = 10: fel1 = 0
61
REM--------------------------------------------------------POS-10 Getting Up
IF POS1 <> 10 THEN 63
wee$ = ""

IF b2y > b1y - 5 THEN b2y = b2y - .4
IF laax > b1x + 2 THEN laax = laax - 1
IF lbax > b1x + 3 THEN lbax = lbax - 1
IF laay > b1y - 4 THEN laay = laay - .5
IF lbay > b1y - 5 THEN lbay = lbay - .5

IF labx > b1x - 2 THEN labx = labx - .5
IF lbbx > b1x - 1 THEN lbbx = lbbx - .5
IF laby > b1y - 8 THEN laby = laby - 1
IF lbby > b1y - 9 THEN lbby = lbby - 1
b1h = 0: b1v = 0
b2h = 0: b2v = 0
h1 = 0: v1 = 0
aaby = b1y - 2.5
abby = b1y - 2
aaay = b1y - .5
abay = b1y + 1
h1x = b1x - 1
h1y = b1y - 2

fel1 = fel1 + 1
IF fel1 > 25 THEN POS1 = 0: b1h = .3: v1 = -.2: fel1 = 0: laby = b2y: lbby = b2y: laax = b2x + 4: lbax = b2x + 5: labx = b2x + 4: lbbx = b2x + 5: b1x = b2x - 2: b1y = b2y - 5: aaay = b1y: abby = b1y: aaby = b1y: abay = b1y: aaax = b1x: abbx = b1x: _
                                                                                                                         aabx = b1x: abax = b1x: IF r1 = 0 THEN h1 = .2 ELSE h1 = -.2
63

REM-----------------------------------------------------------POS-15 Air-Kick
IF POS1 <> 15 THEN 150
akck1 = 1
b1h = 0
b2h = 0
h1h = 0
laav = 0
lbav = 0
labv = 0
lbbv = 0
aaav = 0
abav = 0
aabv = 0
abbv = 0
b1v = 0
b2v = 0
h1v = 0
b1x = b2x - 5
b1y = b2y - 3

aaay = b1y + 3
aaby = b1y + 3
aaax = b1x - 2
aabx = b1x

abay = b1y
abby = b1y + 1
abax = b1x + 3
abbx = b1x + 7

h1x = b1x - 1
h1y = b1y - 3

laax = b2x - 5
labx = b2x + 3
laay = b2y + 2
laby = b2y + 3

lbax = b2x + 5
lbbx = b2x + 10
lbay = b2y + 1
lbby = b2y + 2

IF v1 < 0 THEN v1 = v1 / 1.5

fel1 = fel1 + 1
IF fel1 > 25 OR b2y > 80 THEN POS1 = 0: fel1 = 0: b1h = 1: lbbx = b2x
150

REM----------------------------------------------------------POS-16 Upper-Cut
IF POS1 <> 16 THEN 130
b1x = b2x
b1y = b2y - 8
h1x = b1x
h1y = b1y - 3
labh = 0: lbbh = 0
laah = 0: lbah = 0
abav = .5
abbv = 0
aaav = .5
aabv = 0
laay = b2y + 5
lbay = b2y + 6
labx = laax
abax = b2x + 2
abbx = b2x + 1
aaax = b2x - 1
aabx = b2x - 1
abay = h1y
abby = h1y - 3
aaay = b1y + 5
aaby = b1y + 8
h1 = h1 / 2
fel1 = fel1 + 1
IF fel1 > 15 THEN POS1 = 0: fel1 = 0: lbbx = b2x

130
      
REM-----------------------------------------------Charge Stuff
IF chrg1 > 100 THEN chrg1 = 100
IF chrg1 > 0 THEN chrg1 = chrg1 - 1
IF chrg1 < 0 THEN chrg1 = 0
REM-----------------------------------------------LANDING AI
IF POS1 = 9 THEN 74
IF laby > 90 AND v1 > 0 THEN labv = 0: lbbv = 0: laav = v1: labv = v1: b2v = v1: IF v1 > 0 THEN v1 = v1 / 1.3
IF laby > 95 THEN v1 = 0
74

END SUB

SUB P2Animation1
REM---Player 2                                                       Player 2
REM--------------------------------------------POS-0 Standing/Jumping/Ducking

IF POS2 <> 0 THEN 28
'body
IF b1y2 < b2y2 - 6 THEN b1v2 = b1v2 + .01
IF b1y2 > b2y2 - 5 THEN b1v2 = b1v2 - .01
IF b1y2 > b2y2 - 6 AND b1y2 < b2y2 - 5 THEN b1v2 = b1v2 / 2
IF b1x2 < b2x2 + 1 THEN b1h2 = b1h2 + .001
IF b1x2 > b2x2 + 1 THEN b1h2 = b1h2 - .001
IF b1h2 < -.05 OR b1h2 > .05 THEN b1h2 = b1h2 / 1.1
IF b1y2 < b2y2 - 7 THEN b1y2 = b2y2 - 7
IF dk2 = 1 THEN b2t2 = lbby2 - 5 ELSE b2t2 = lbby2 - 10
IF b2y2 > b2t2 THEN b2y2 = b2y2 - .5
IF b2y2 < b2t2 THEN b2y2 = b2y2 + .5
IF b2y2 > lbby2 - 10 THEN b2v2 = b2v2 - .005
IF b2y2 < lbby2 - 10 AND v2 > -1 THEN b2y2 = lbby2 - 10: b2v2 = b2v2 / 2
IF b1y2 > b2y2 THEN b1y2 = b2y2

'left leg
IF labx2 > b2x2 + 3 THEN labx2 = labx2 - .5
IF labx2 < b2x2 + 2 THEN labh2 = labh2 + .01
IF labx2 > b2x2 + 2 AND labx2 < b2x2 + 3 AND labh2 <> 0 THEN labh2 = labh2 / 2
IF laax2 < labx2 THEN laax2 = labx2 - .01: IF b1h2 > 0 THEN laah2 = b1h2
IF laax2 > b2x2 + 4 THEN laax2 = b2x2 + 4: laah2 = -.01
IF laax2 > labx2 + 1 THEN laah2 = laah2 - .01
IF laay2 < laby2 - 5 THEN laay2 = laay2 + .5
IF laay2 > laby2 - 5 THEN laay2 = laay2 - .5
IF laby2 < b2y2 + 10 THEN laby2 = laby2 + .1
IF laby2 > b2y2 + 10 THEN laby2 = laby2 - .5

IF laby2 < b2y2 + 10 THEN labv2 = labv2 + .01

IF dk2 = 1 THEN laax2 = b2x2 + 4: laay2 = b2y2 + 1: laby2 = 95
IF b2y2 < lbby2 - 10 AND v2 < -1 THEN lbby2 = b2y2 + 10
IF laby2 > 95 THEN laby2 = 95
IF b2y2 < 80 AND laby2 > b2y2 + 5 THEN laby2 = laby2 - .5
IF laby2 > b2y2 + 10 THEN laby2 = b2y2 + 10

'Right leg
IF lbbx2 > b2x2 - 1.5 THEN lbbh2 = lbbh2 - .001
IF lbbx2 < b2x2 - 2 THEN lbbh2 = lbbh2 + .001
IF lbbx2 < b2x2 - 3 THEN lbbx2 = b2x2 - 3
IF lbbx2 > b2x2 - 2 AND lbbx2 < b2x2 - 1.5 AND lbbh2 <> 0 THEN lbbh2 = lbbh2 / 2
IF lbax2 < b2x2 THEN lbah2 = lbah2 + .001: lbax2 = b2x2
IF lbax2 > b2x2 THEN lbah2 = lbah2 - .001
IF lbax2 > b2x2 - 1 AND lbax2 < b2x2 THEN lbah2 = lbah2 / 2
IF lbay2 < lbby2 - 5 THEN lbav2 = lbav2 + .01
IF lbay2 > lbby2 - 5 THEN lbav2 = lbav2 - .01
IF lbay2 > b2y2 + 4.5 AND lbay2 < b2y2 + 5.5 THEN lbav2 = lbav2 / 2
IF lbby2 > 95 THEN lbby2 = 95
IF lbby2 > b2y2 + 10 THEN lbby2 = b2y2 + 10

'left arm
abav2 = 0
IF abby2 < b1y2 THEN abby2 = b1y2
IF abay2 > b1y2 + 5 THEN abay2 = b1y2 + 5
IF abay2 < b1y2 + 4 THEN abay2 = abay2 + .2
IF abay2 > b1y2 + 4 THEN abay2 = abay2 - .2
IF abax2 < b1x2 THEN abax2 = abax2 + .2
IF abax2 > b1x2 THEN abax2 = abax2 - .2
IF abbx2 < b1x2 + 2 THEN abbh2 = abbh2 + .01: abbx2 = b1x2 + 2
IF abbx2 > b1x2 + 2 THEN abbh2 = abbh2 - .04
IF abbx2 > b1x2 + 2 AND abbx2 < b2x2 + 3 AND abbh2 <> 0 THEN abbh2 = abbh2 / 1.2
IF abby2 < b1y2 + 1 THEN abbv2 = abbv2 + .01
IF abby2 > b1y2 + 2 THEN abbv2 = abbv2 - .01
IF abby2 > b1y2 + 1 AND abby2 < b1y2 + 2 THEN abbv2 = abbv2 / 2

'right arm
aaav2 = 0
IF aabx2 < b1x2 - 2 THEN aabx2 = b1x2 - 2
IF aaay2 > b1y2 + 5 THEN aaay2 = b1y2 + 5
IF aaay2 < b1y2 + 2 THEN aaay2 = aaay2 + .2
IF aaay2 > b1y2 + 2 THEN aaay2 = aaay2 - .2
IF aaax2 < b1x2 - 2 THEN aaax2 = aaax2 + .2
IF aaax2 > b1x2 - 2 THEN aaax2 = aaax2 - .2
IF aabx2 < b1x2 + 1 THEN aabx2 = aabx2 + .001
IF aabx2 > b1x2 + 2 THEN aabx2 = aabx2 - .5
IF aabx2 < b1x2 + 1 THEN aabh2 = aabh2 + .001
IF aabx2 > b1x2 AND aabx2 < b2x2 + 2 THEN aabh2 = aabh2 / 2
IF aaby2 < b1y2 + 2.5 THEN aaby2 = aaby2 + .5
IF aaby2 > b1y2 + 3.5 THEN aaby2 = aaby2 - .5

IF laby2 > 93 AND lbby2 > 93 AND b2x2 < b2x THEN r2 = 0
IF laby2 > 93 AND lbby2 > 93 AND b2x2 > b2x THEN r2 = 1

h2x = b1x2: h2y = b1y2 - 3
28

REM----------------------------------------------------------------POS-1 Kick
IF POS2 <> 1 THEN 29
rh2 = 1
aabh2 = 0
abbh2 = 0
aabv2 = 0
abbv2 = 0
h2 = h2 / 1.1
b2x2 = labx2
b1h2 = 0: b1v2 = 0
aaah2 = 0: abah2 = 0
IF lbbx2 < b2x2 + 8 THEN lbbx2 = lbbx2 + 1
IF lbby2 > b2y2 - 10 THEN lbby2 = lbby2 - 1
IF lbax2 < b2x2 + 3 THEN lbax2 = lbax2 + 2
IF lbay2 > b2y2 - 5 THEN lbay2 = lbay2 - 2

frt2 = frt2 - 1
IF frt2 = 0 THEN POS2 = 2: frt2 = 15

h2x = b1x2: h2y = b1y2 - 3
29
REM----------------------------------------------------------------POS-2 Kick
IF POS2 <> 2 THEN 30
h2 = h2 / 1.1
b2x2 = b2x2 - .1
IF lbbx2 > b2x2 - 4 THEN lbbx2 = lbbx2 - 1
IF lbby2 < b2y2 + 10 THEN lbby2 = lbby2 + 1
IF lbax2 > b2x2 - 3 THEN lbax2 = lbax2 - 1
IF lbay2 < b2y2 + 5 THEN lbay2 = lbay2 + 1

frt2 = frt2 - 1
IF frt2 = 0 THEN POS2 = 0

h2x = b1x2: h2y = b1y2 - 3
30
END SUB

SUB P2Animation2
REM--------------------------------------------------------------POS-5 Charge
IF POS2 <> 5 THEN 31

'body
IF b1y2 < b2y2 - 5 THEN b1v2 = b1v2 + .01
IF b1y2 > b2y2 - 4 THEN b1v2 = b1v2 - .01
IF b1y2 > b2y2 - 5 AND b1y2 < b2y2 - 5 THEN b1v2 = b1v2 / 2
IF b1x2 < b2x2 + 1 THEN b1h2 = b1h2 + .001
IF b1x2 > b2x2 + 1 THEN b1h2 = b1h2 - .001
IF b1h2 < -.05 OR b1h2 > .05 THEN b1h2 = b1h2 / 1.1
IF b1y2 < b2y2 - 7 THEN b1y2 = b2y2 - 7
IF dk2 = 1 THEN b2t2 = lbby2 - 5 ELSE b2t2 = lbby2 - 10
IF b2y2 > b2t2 THEN b2y2 = b2y2 - .5
IF b2y2 < b2t2 THEN b2y2 = b2y2 + .5
IF b2y2 > lbby2 - 10 THEN b2v2 = b2v2 - .005
IF b2y2 < lbby2 - 10 AND v2 > -1 THEN b2y2 = lbby2 - 10: b2v2 = b2v2 / 2

'left leg
IF labx2 > b2x2 + 3 THEN labx2 = labx2 - .5
IF labx2 < b2x2 + 2 THEN labh2 = labh2 + .01
IF labx2 > b2x2 + 2 AND labx2 < b2x2 + 3 AND labh2 <> 0 THEN labh2 = labh2 / 2
IF laax2 < labx2 THEN laax2 = labx2 - .01: IF b1h2 > 0 THEN laah2 = b1h2
IF laax2 > b2x2 + 4 THEN laax2 = b2x2 + 4: laah2 = -.01
IF laax2 > labx2 + 1 THEN laah2 = laah2 - .01
IF laay2 < laby2 - 5 THEN laay2 = laay2 + .5
IF laay2 > laby2 - 5 THEN laay2 = laay2 - .5
IF laby2 < b2y2 + 10 THEN laby2 = laby2 + .1
IF laby2 > b2y2 + 10 THEN laby2 = laby2 - .5

IF laby2 < b2y2 + 10 THEN labv2 = labv2 + .01

IF dk2 = 1 THEN laax2 = b2x2 + 4: laay2 = b2y2 + 1: laby2 = 95
IF b2y2 < lbby2 - 10 AND v2 < -1 THEN lbby2 = b2y2 + 10
IF laby2 > 95 THEN laby2 = 95
IF b2y2 < 80 AND laby2 > b2y2 + 5 THEN laby2 = laby2 - .5
IF laby2 > b2y2 + 10 THEN laby2 = b2y2 + 10

'Right leg
IF lbbx2 > b2x2 - 1.5 THEN lbbh2 = lbbh2 - .001
IF lbbx2 < b2x2 - 2 THEN lbbh2 = lbbh2 + .001
IF lbbx2 > b2x2 - 2 AND lbbx2 < b2x2 - 1.5 AND lbbh2 <> 0 THEN lbbh2 = lbbh2 / 2
IF lbax2 < b2x2 THEN lbah2 = lbah2 + .001
IF lbax2 > b2x2 THEN lbah2 = lbah2 - .001
IF lbax2 > b2x2 - 1 AND lbax2 < b2x2 THEN lbah2 = lbah2 / 2
IF lbay2 < lbby2 - 5 THEN lbav2 = lbav2 + .01
IF lbay2 > lbby2 - 5 THEN lbav2 = lbav2 - .01
IF lbay2 > b2y2 + 4.5 AND lbay2 < b2y2 + 5.5 THEN lbav2 = lbav2 / 2
IF lbby2 > 95 THEN lbby2 = 95
IF lbby2 > b2y2 + 10 THEN lbby2 = b2y2 + 10

'left arm
IF abay2 > b1y2 + 5 THEN abay2 = b1y2 + 5
IF abay2 < b1y2 + 4 THEN abay2 = abay2 + .1
IF abay2 > b1y2 + 4 THEN abay2 = abay2 - .1
IF abax2 < b1x2 + 2 THEN abax2 = abax2 + .1
IF abax2 > b1x2 + 2 THEN abax2 = abax2 - .1
IF abbx2 > b1x2 - 1 THEN abbx2 = abbx2 - .1
IF abbx2 < b1x2 - 1 THEN abbx2 = abbx2 + .1
IF abby2 < b1y2 + 1 THEN abbv2 = abbv2 + .01
IF abby2 > b1y2 + 2 THEN abbv2 = abbv2 - .01
IF abby2 > b1y2 + 1 AND abby2 < b1y2 + 2 THEN abbv2 = abbv2 / 2

'right arm
IF aaay2 > b1y2 + 5 THEN aaay2 = b1y2 + 5
IF aaay2 < b1y2 + 2 THEN aaay2 = aaay2 + .1
IF aaay2 > b1y2 + 2 THEN aaay2 = aaay2 - .1
IF aaax2 < b1x2 - 4 THEN aaax2 = aaax2 + .1
IF aaax2 > b1x2 - 4 THEN aaax2 = aaax2 - .1
IF aabx2 < b1x2 - 2 THEN aabx2 = aabx2 + .1
IF aabx2 > b1x2 - 2 THEN aabx2 = aabx2 - .1
IF aabx2 < b1x2 THEN aabh2 = aabh2 / 2
IF aaby2 < b1y2 + 2.5 THEN aaby2 = aaby2 + .5
IF aaby2 > b1y2 + 3.5 THEN aaby2 = aaby2 - .5

IF laby2 > 94 AND lbby2 > 94 AND b2x2 < b2x THEN r2 = 0
IF laby2 > 94 AND lbby2 > 94 AND b2x2 > b2x THEN r2 = 1

IF h2y < b1y2 - 3 THEN h2y = b1y2 - 3
IF h2y > b1y2 - 2 THEN h2y = b1y2 - 2
IF h2y < b1y2 - 2 THEN h2y = h2y + .1
IF h2y > b1y2 - 2 THEN h2y = h2y - .1
h2x = b1x2 + .5
IF chrg2 > 100 THEN POS2 = 0
chrg2 = chrg2 + 1.5
IF chrg2 > 100 THEN POS2 = 0
31
REM--------------------------------------------------------POS-8 DRAGON PUNCH
IF POS2 <> 8 THEN 32
b1x2 = b2x2
b1y2 = b2y2 - 8
h2x = b1x2: h2y = b1y2 - 3
labv2 = 0: lbbv2 = 0
laav2 = 0: lbav2 = 0
laay2 = b2y2 + 5
lbay2 = b2y2 + 6
lbbx2 = b1x2 + ((b2x2 - aaax2) / 4)
labx2 = laax2
IF laby2 > b2y2 + 10 THEN laby2 = b2y2 + 10
IF lbby2 > b2y2 + 10 THEN lbby2 = b2y2 + 10
abay2 = b1y2 + 5
abby2 = b1y2 + 8
IF aaax2 < b2x2 - 3 THEN aaah2 = .1: aabh2 = .1: abah2 = -.1: abbh2 = -.1: laah2 = .1: labh2 = .1: aaav2 = .05: aabv2 = .03: abav2 = 0: abbv2 = 0
IF v2 > 0 AND b2y2 > 75 THEN POS2 = 0
IF aaax2 > b2x2 + 3 OR lbby2 > 95 THEN POS2 = 0
h2 = h2 / 1.05
32
REM--------------------------------------------------------POS-9 Fall
IF POS2 <> 9 THEN 60
lbbh2 = 0
LK = 0: weeb$ = "": wee2$ = "": light2 = 0: llc2 = 0
v2 = v2 + grav
IF b2x2 > -60 AND b2x2 < 160 AND lbby2 > 93 THEN v2 = 0: lbbv2 = 0: b2v2 = 0
IF b2x2 < -60 OR b2x2 > 160 THEN hp2 = 0: IF b2y2 > 155 THEN v2 = 0: b2y2 = 155 ELSE v2 = v2 + grav
b1v2 = 0
IF b1x2 > b2x2 - 5 THEN b1x2 = b1x2 - .3 ELSE b1h2 = 0
IF b1y2 < b2y2 THEN b1y2 = b1y2 + .3
IF b2x2 > lbbx2 - 5 THEN b2x2 = b2x2 - .5
IF b2y2 < lbby2 THEN b2y2 = b2y2 + .5
IF lbax2 > lbbx2 - 3 THEN lbax2 = lbax2 - .5
IF lbay2 < lbby2 - 2 THEN lbay2 = lbay2 + .5
IF laax2 > lbbx2 - 3 THEN laax2 = laax2 - .5
IF laay2 < lbby2 - 1 THEN laay2 = laay2 + .5
laby2 = lbby2
IF aabx2 > b1x2 - 5 THEN aabx2 = aabx2 - .5 ELSE aabx2 = b1x2 - 5
IF abbx2 > b1x2 - 5 THEN abbx2 = abbx2 - .5 ELSE abbx2 = b1x2 - 5
IF aaax2 > b1x2 - 3 THEN aaax2 = aaax2 - .5 ELSE aaax2 = b1x2 - 3
IF abax2 > b1x2 - 3 THEN abax2 = abax2 - .5 ELSE abax2 = b1x2 - 3
IF b2y2 > laay2 + 5 THEN b2y2 = laay2 + 5
labh2 = 0: lbbh2 = 0
aaby2 = b1y2 - 1
abby2 = b1y2 + 1
aaay2 = b1y2 - .5
abay2 = b1y2 + 1
laav2 = 0
lbav2 = 0
h2x = b1x2 - 2
h2y = b1y2
IF b1y2 > b2y2 THEN b1y2 = b2y2
IF hp2 = 0 OR ABS(h2) > .5 OR ABS(v2) > .5 OR b2y2 < 94 THEN 60
fel2 = fel2 + (hp2 / 25) + 5
IF fel2 > 100 THEN POS2 = 10: fel2 = 0
60
REM--------------------------------------------------------POS-10 Getting Up
IF POS2 <> 10 THEN 62
weeb$ = "": wee2$ = ""

IF b2y2 > b1y2 - 5 THEN b2y2 = b2y2 - .4
IF laax2 > b1x2 + 2 THEN laax2 = laax2 - 1
IF lbax2 > b1x2 + 3 THEN lbax2 = lbax2 - 1
IF laay2 > b1y2 - 4 THEN laay2 = laay2 - .5
IF lbay2 > b1y2 - 5 THEN lbay2 = lbay2 - .5

IF labx2 > b1x2 - 2 THEN labx2 = labx2 - .5
IF lbbx2 > b1x2 - 1 THEN lbbx2 = lbbx2 - .5
IF laby2 > b1y2 - 8 THEN laby2 = laby2 - 1
IF lbby2 > b1y2 - 9 THEN lbby2 = lbby2 - 1
b1h2 = 0: b1v2 = 0
b2h2 = 0: b2v2 = 0
h2 = 0: v2 = 0
aaby2 = b1y2 - 2.5
abby2 = b1y2 - 2
aaay2 = b1y2 - .5
abay2 = b1y2 + 1
h2x = b1x2 - 1
h2y = b1y2 - 2

fel2 = fel2 + 1
IF fel2 > 25 THEN POS2 = 0: b1h2 = .3: v2 = -.2: fel2 = 0: laby2 = b2y2: lbby2 = b2y2: laax2 = b2x2 + 4: lbax2 = b2x2 + 5: labx2 = b2x2 + 4: lbbx2 = b2x2 + 5: b1x2 = b2x2 - 2: b1y2 = b2y2 - 5: aaay2 = b1y2: abby2 = b1y2: aaby2 = b1y2: abay2 = b1y2: _
                                                                                                                         aaax2 = b1x2: abbx2 = b1x2: aabx2 = b1x2: abax2 = b1x2: IF r2 = 0 THEN h2 = .2 ELSE h2 = -.2
62

REM-----------------------------------------------------------POS-15 Air-Kick
IF POS2 <> 15 THEN 151
akck2 = 1
b1h2 = 0
b1h2 = 0
h2h = 0
laav2 = 0
lbav2 = 0
labv2 = 0
lbbv2 = 0
aaav2 = 0
abav2 = 0
aabv2 = 0
abbv2 = 0
b1v2 = 0
b2v2 = 0
h2v = 0
b1x2 = b2x2 - 5
b1y2 = b2y2 - 3

aaay2 = b1y2 + 3
aaby2 = b1y2 + 3
aaax2 = b1x2 - 2
aabx2 = b1x2
    
abay2 = b1y2
abby2 = b1y2 + 1
abax2 = b1x2 + 3
abbx2 = b1x2 + 7

h2x = b1x2 - 1
h2y = b1y2 - 3

laax2 = b2x2 - 5
labx2 = b2x2 + 3
laay2 = b2y2 + 2
laby2 = b2y2 + 3

lbax2 = b2x2 + 5
lbbx2 = b2x2 + 10
lbay2 = b2y2 + 1
lbby2 = b2y2 + 2

IF v2 < 0 THEN v2 = v2 / 1.5

fel2 = fel2 + 1
IF fel2 > 25 OR b2y2 > 80 THEN POS2 = 0: fel2 = 0: b1h2 = 1: lbbx2 = b2x2
151

REM----------------------------------------------------------POS-16 Upper-Cut
IF POS2 <> 16 THEN 131
b1x2 = b2x2
b1y2 = b2y2 - 8
h2x = b1x2
h2y = b1y2 - 3
labh2 = 0: lbbh2 = 0
laah2 = 0: lbah2 = 0
abav2 = .5
abbv2 = 0
aaav2 = .5
aabv2 = 0
laay2 = b2y2 + 5
lbay2 = b2y2 + 6
labx2 = laax2
abax2 = b2x2 + 2
abbx2 = b2x2 + 1
aaax2 = b2x2 - 1
aabx2 = b2x2 - 1
abay2 = h2y
abby2 = h2y - 3
aaay2 = b1y2 + 5
aaby2 = b1y2 + 8
h2 = h2 / 2
fel2 = fel2 + 1
IF fel2 > 15 THEN POS2 = 0: fel2 = 0: lbbx2 = b2x2

131

REM-----------------------------------------------Charge Stuff
IF chrg2 > 100 THEN chrg2 = 100
IF chrg2 > 0 THEN chrg2 = chrg2 - 1
IF chrg2 < 0 THEN chrg2 = 0
REM-----------------------------------------------LANDING AI
IF POS2 = 9 THEN 73
IF laby2 > 90 AND v2 > 0 THEN labv2 = 0: lbbv2 = 0: laav2 = v2: labv2 = v2: b2v2 = v2: IF v2 > 0 THEN v2 = v2 / 1.3
IF laby2 > 95 THEN v2 = 0
73

END SUB

SUB Paused
REM------------------------------------------------------------Game Paused
SCREEN , , , ap
COLOR 4
LOCATE 3, 37
PRINT "PAUSE"
17 IF INKEY$ = "" THEN 17
SCREEN , , , bp
END SUB

SUB Save
LOCATE 1, 1: PRINT "Saving . . ."
OPEN Drv$ + ":" + Path$ + "\Config.dat" FOR OUTPUT AS #1
WRITE #1, dif, s, satan, smear, hp1, hp2, grav, Delay, offy, detail, sspeed, stage, boby, bobv, sbx, sfx, zom, joy, cap, joyfix, joytest, sense, players, gal, yk, ac1, bc1, lc1, ac2, bc2, lc2, SBTimes%, P1d, P2d, spl, speed
CLOSE #1
END SUB

DEFINT A-Z
SUB SB
SCREEN 9
COLOR 4
CONST false = 0, true = NOT false
IF DetectCard = true THEN
LOCATE 1, 1: PRINT "Sound Blaster (Compatible) Card Detected"
FOR I = 1 TO 30000
NEXT I
SBinit
END IF
DEF SEG = 0
SCH% = 4: 'Number of Sound Channels
LOCATE 2, 1: PRINT "Sound Card Initialized"
END SUB

SUB SBinit
FOR Q = 1 TO &HF5
CALL WriteReg(Q, 0)
NEXT Q
END SUB

SUB SBPLAY1 (Freq%, Wave%, Feedback%, Modl%, Clen%)
                                'Channel 1
                                'Operator 1
CALL WriteReg(&H20, Modl%)      '&H51
CALL WriteReg(&H40, 10)         '49
CALL WriteReg(&H60, &H40)       '&HF0
CALL WriteReg(&H80, &H240)      '&H77
CALL WriteReg(&HA0, Freq%)      'Freq%
                                'Operator 2
CALL WriteReg(&H23, 1)          '
CALL WriteReg(&H43, 0)          '49
CALL WriteReg(&H63, &HF0)       '
CALL WriteReg(&H83, Clen%)      'CLEN%
CALL WriteReg(&HB0, &H31)       '&H31
                                                    
CALL WriteReg(&HE0, Wave%)      '0 or 1
CALL WriteReg(&HC0, Feedback%)  '

END SUB

DEFSNG A-Z
SUB SetDelay
eyes = 0
FX 5, SBTime%, SBTimer%
FOR wee = 0 TO 100 STEP .1
LINE (wee, 40)-(wee, 60), 40
LINE (wee - 5, 45)-(wee - 5, 55), 0
LINE (wee - 95.4, 45)-(wee - 95.4, 55), 40
NEXT wee
FOR wee = 0 TO 15 STEP .1
LINE (0, 45 - wee)-(100, 45 - wee), 40
LINE (0, 55 + wee)-(100, 55 + wee), 40
LINE (5, 50 - wee)-(95, 50 - wee), 0
LINE (5, 50 + wee)-(95, 50 + wee), 0
NEXT wee
LOCATE 10, 4: PRINT "Enter a value. (0 through 20000)"
LOCATE 11, 4: PRINT "Low values are fast, but may"
LOCATE 12, 4: PRINT "have screen flicker."
LOCATE 13, 4: PRINT "High values have little/no flicker,"
LOCATE 14, 4: PRINT "but are slower."
LOCATE 15, 4: INPUT "", Delay$
Delay = VAL(Delay$)
FX 5, SBTime%, SBTimer%
FOR wee = 0 TO 40 STEP .1
LINE (0, 35 - wee)-(100, 35 - wee), 40
LINE (0, 65 + wee)-(100, 65 + wee), 40
LINE (0, 50 - wee)-(100, 50 - wee), 0
LINE (0, 50 + wee)-(100, 50 + wee), 0
NEXT wee
CLS
END SUB

DEFINT A-Z
SUB SETHUES (HUE() AS HUES, SC, EC)

 OUT &H3C8, SC

 FOR I = SC TO EC
   OUT &H3C9, HUE(I).RED: OUT &H3C9, HUE(I).GRN: OUT &H3C9, HUE(I).BLU
 NEXT

END SUB

DEFSNG A-Z
SUB Title

REM-----------------------------------------------------------Title Screen(s)
CLS
REDIM PAL(255) AS HUES
DEF SEG = VARSEG(PAL(0))
BLOAD Drv$ + ":" + Path$ + "\sfbk.PAL", 0
SETHUES PAL(), 0, 255

OPEN Drv$ + ":" + Path$ + "\sfbk1.QBI" FOR INPUT AS #1
FILESIZE& = LOF(1)
CLOSE #1
ARYSIZE = (FILESIZE& - 7) \ 2 - 1
REDIM sfbk1(ARYSIZE)
DEF SEG = VARSEG(sfbk1(0))
BLOAD Drv$ + ":" + Path$ + "\sfbk1.QBI", 0

PUT (0, 0), sfbk1, PSET
ERASE sfbk1

OPEN Drv$ + ":" + Path$ + "\sfbk2.QBI" FOR INPUT AS #1
FILESIZE& = LOF(1)
CLOSE #1
ARYSIZE = (FILESIZE& - 7) \ 2 - 1
REDIM sfbk2(ARYSIZE)
DEF SEG = VARSEG(sfbk2(0))
BLOAD Drv$ + ":" + Path$ + "\sfbk2.QBI", 0

PUT (0, 50), sfbk2, PSET
ERASE sfbk2

gal = TIMER
200 IF TIMER < gal + 2 THEN 200

CLS
REDIM PAL(255) AS HUES
DEF SEG = VARSEG(PAL(0))
BLOAD Drv$ + ":" + Path$ + "\masscot.PAL", 0
SETHUES PAL(), 0, 255

OPEN Drv$ + ":" + Path$ + "\masscot1.QBI" FOR INPUT AS #1
FILESIZE& = LOF(1)
CLOSE #1
ARYSIZE = (FILESIZE& - 7) \ 2 - 1
REDIM masscot1(ARYSIZE)
DEF SEG = VARSEG(masscot1(0))
BLOAD Drv$ + ":" + Path$ + "\masscot1.QBI", 0

PUT (27, 0), masscot1, PSET
ERASE masscot1

OPEN Drv$ + ":" + Path$ + "\masscot2.QBI" FOR INPUT AS #1
FILESIZE& = LOF(1)
CLOSE #1
ARYSIZE = (FILESIZE& - 7) \ 2 - 1
REDIM masscot2(ARYSIZE)
DEF SEG = VARSEG(masscot2(0))
BLOAD Drv$ + ":" + Path$ + "\masscot2.QBI", 0

PUT (21, 53.5), masscot2, PSET
ERASE masscot2

gal = TIMER
201 IF TIMER < gal + 2 THEN 201

CLS
REDIM PAL(255) AS HUES
DEF SEG = VARSEG(PAL(0))
BLOAD Drv$ + ":" + Path$ + "\bostrom.PAL", 0
SETHUES PAL(), 0, 255

OPEN Drv$ + ":" + Path$ + "\BOSTROM.QBI" FOR INPUT AS #1
FILESIZE& = LOF(1)
CLOSE #1
ARYSIZE = (FILESIZE& - 7) \ 2 - 1
REDIM bostrom(ARYSIZE)
DEF SEG = VARSEG(bostrom(0))
BLOAD Drv$ + ":" + Path$ + "\bostrom.QBI", 0

PUT (15, 20), bostrom, PSET
COLOR 42
LOCATE 1, 1: PRINT "Sound Made Possible by"
ERASE bostrom

gal = TIMER
202 IF TIMER < gal + 2 THEN 202

CLS
REDIM PAL(255) AS HUES
DEF SEG = VARSEG(PAL(0))
BLOAD Drv$ + ":" + Path$ + "\Title.PAL", 0
SETHUES PAL(), 0, 255

OPEN Drv$ + ":" + Path$ + "\title1.QBI" FOR INPUT AS #1
FILESIZE& = LOF(1)
CLOSE #1
ARYSIZE = (FILESIZE& - 7) \ 2 - 1
REDIM title1(ARYSIZE)
DEF SEG = VARSEG(title1(0))
BLOAD Drv$ + ":" + Path$ + "\title1.QBI", 0

PUT (10.2, 5), title1, PSET
ERASE title1

OPEN Drv$ + ":" + Path$ + "\title2.QBI" FOR INPUT AS #1
FILESIZE& = LOF(1)
CLOSE #1
ARYSIZE = (FILESIZE& - 7) \ 2 - 1
REDIM title2(ARYSIZE)
DEF SEG = VARSEG(title2(0))
BLOAD Drv$ + ":" + Path$ + "\title2.QBI", 0

PUT (50.5, 5), title2, PSET
ERASE title2

COLOR 118
LOCATE 21, 5
PRINT ; "is Freeware by Yummy Creations"
gal = TIMER
203 IF TIMER < gal + 2 THEN 203
REM---
FOR wee = 0 TO 50 STEP .1
LINE (wee, 0)-(wee + 1, 100), 0, BF
LINE (100 - wee, 0)-(99 - wee, 100), 0, BF
NEXT wee
END SUB

DEFINT A-Z
SUB WriteReg (Reg%, Value%)
OUT &H220, Reg%
FOR X = 0 TO writereg.delay1
   a = INP(&H220)
NEXT X
OUT &H223, Value%
FOR X = 0 TO writereg.delay2
   a = INP(&H220)
NEXT X
END SUB

