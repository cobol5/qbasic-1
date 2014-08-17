DECLARE SUB initimages ()
DECLARE SUB drawbox (x!, y!, x2!, y2!)
DECLARE SUB getbutton ()
DECLARE FUNCTION Attacknumber! (type$, life!, etype$, elife!, atk!)
DECLARE FUNCTION showname$ (type$)
DECLARE SUB PUTimage (x!, y!, omap$)
DECLARE SUB mousehide ()
DECLARE SUB openpic (filename$)
DECLARE SUB mousestatus (lb%, rb%, xmouse%, ymouse%)
DECLARE SUB mouseshow ()
DECLARE FUNCTION MouseInit% ()
DECLARE SUB MouseDriver (AX%, bx%, CX%, DX%)
DECLARE SUB bmp (filename$, x$)
ON ERR GOSUB wrong
RANDOMIZE TIMER
CLEAR
DIM SHARED terrain$
DIM SHARED level
DIM SHARED mpdead(480)
DIM SHARED medead(480)
DIM SHARED menemylife(480)
DIM SHARED mplayerlife(480)
DIM SHARED menemyspd(480)
DIM SHARED mplayerspd(480)
DIM SHARED menemytype$(480)
DIM SHARED mplayertype$(480)
DIM SHARED mxplayerbox(480)
DIM SHARED myplayerbox(480)
DIM SHARED mxenemybox(480)
DIM SHARED myenemybox(480)
DIM SHARED movealready(480)
DIM SHARED attackturn(480)
DIM SHARED moveturn(480)
DIM SHARED Box$(17, 10)
DIM SHARED map$(32, 32)
DIM SHARED barracks(1 TO 500)
DIM SHARED man2(1 TO 500)
DIM SHARED man1(1 TO 500)
DIM SHARED grass(1 TO 500)
DIM SHARED ogre(1 TO 500)
DIM SHARED arch(1 TO 500)
DIM SHARED pult(1 TO 500)
DIM SHARED ogre2(1 TO 500)
DIM SHARED arch2(1 TO 500)
DIM SHARED pult2(1 TO 500)
DIM SHARED skirm1(1 TO 500)
DIM SHARED skirm2(1 TO 500)
DIM SHARED rock(1 TO 600)
DIM SHARED xboundmin
DIM SHARED yboundmin
DIM SHARED xboundmax
DIM SHARED yboundmax
KEY 15, CHR$(0) + CHR$(1)
KEY 16, CHR$(4) + CHR$(38)'L
KEY 17, CHR$(4) + CHR$(36)'J
KEY 18, CHR$(4) + CHR$(30)'a
KEY 19, CHR$(4) + CHR$(37)'k
ON KEY(16) GOSUB add10
ON KEY(17) GOSUB bcome100
ON KEY(18) GOSUB speed
ON KEY(15) GOSUB exiter
ON KEY(19) GOSUB gamewin
ON KEY(10) GOSUB pauser
KEY(10) ON
KEY(15) ON
KEY(16) ON
KEY(17) ON
KEY(18) ON
KEY(19) ON
OPEN "game.dat" FOR INPUT AS #1
INPUT #1, filename$
INPUT #1, level
INPUT #1, terrain$
CLOSE #1
IF terrain$ = "snow" THEN
bmp "pal2.bmp", "Image1"
END IF
IF terrain$ = "dry" THEN
bmp "pal4.bmp", "Image1"
END IF
IF terrain$ = "night" THEN
bmp "pal5.bmp", "Image1"
END IF
IF terrain$ = "grass" THEN
bmp "test2.bmp", "Image1"
END IF
initimages
DIM SHARED mouse$
RESTORE
mouse$ = SPACE$(57)
FOR I% = 1 TO 57
  READ a$
  H$ = CHR$(VAL("&H" + a$))
  MID$(mouse$, I%, 1) = H$
NEXT I%
DATA 55,89,E5,8B,5E,0C,8B,07,50,8B,5E,0A,8B,07,50,8B
DATA 5E,08,8B,0F,8B,5E,06,8B,17,5B,58,1E,07,CD,33,53
DATA 8B,5E,0C,89,07,58,8B,5E,0A,89,07,8B,5E,08,89,0F
DATA 8B,5E,06,89,17,5D,CA,08,00
CLS
MS% = MouseInit%
IF NOT MS% THEN
  PRINT "Mouse not found": LET AMOUSE$ = "NO":
  PRINT "This Game Wont Run Without Mouse"
  k$ = INPUT$(1)
  RUN "present.bas"
END IF
LET AMOUSE$ = "YES"
OPEN "timer.dat" FOR INPUT AS #1
INPUT #1, delay
CLOSE #1
OPEN filename$ FOR INPUT AS #1
FOR mapinitx = 1 TO 30
FOR mapinity = 1 TO 30
        INPUT #1, koi$
        map$(mapinitx, mapinity) = koi$
        IF LEFT$(koi$, 1) = "1" THEN GOSUB addman
        IF LEFT$(koi$, 1) = "2" THEN GOSUB addenmy
NEXT
NEXT
'***init box contents
FOR boxinitx = 1 TO 15
FOR boxinity = 1 TO 6
        Box$(boxinitx, boxinity) = map$(boxinitx, boxinity)
NEXT boxinity
NEXT boxinitx
CLS
FOR x = 1 TO 300 STEP 20
FOR y = 1 TO 170 STEP 30
xt = INT(x / 20) + 1
yt = INT(y / 30) + 1
IF Box$(xt, yt) = "grass" THEN PUT (x, y), grass
IF Box$(xt, yt) = "rock" THEN PUT (x, y), rock
IF Box$(xt, yt) = "farm" THEN PUT (x, y), barracks
NEXT y
NEXT x
'init players
mann = 1
mst = 1
guideline = 1
viewmove = 1
ksj = ksj + 1
ks = ks + 1
maxman = ks - 1
maxenmy = ksj
FOR m = 1 TO ks
attackturn(m) = 1
moveturn(m) = 1
movealready(m) = 0
NEXT
'init
mousescroll = 1
playerlife = mplayerlife(1)
enemylife = menemylife(1)
enemyspd = menemyspd(1)
playerspd = mplayerspd(1)
playertype$ = mplayertype$(1)
enemytype$ = menemytype$(1)
movemode = 0
moveturn = 1
attackturn = 1
oldmap$ = "grass"
pcoldmap$ = "grass"
'****boundaries set***
xboundmin = 0
yboundmin = 0
xboundmax = 15
yboundmax = 6
xplayerbox = mxplayerbox(1)
yplayerbox = myplayerbox(1)
xenemybox = mxenemybox(1)
yenemybox = myenemybox(1)
playerx = (mxplayerbox(1) - 1 - xboundmin) * 20 + 1
playery = (myplayerbox(1) - 1 - yboundmin) * 30 + 1
COLOR 15
mouseshow
map$(xplayerbox, yplayerbox) = playertype$
map$(xenemybox, yenemybox) = enemytype$
pturn:
movealready(mann) = 0
xboundmin = xplayerbox - 7
yboundmin = yplayerbox - 3
xboundmax = xplayerbox + 8
yboundmax = yplayerbox + 3
GOSUB checkbound
GOSUB refresh
'*******LOOP START******
DO
IF maxman = 1 THEN GOTO gameover
IF maxenmy = 1 THEN GOTO gamewin
GOSUB cbound
'******check keys******
        k$ = INKEY$
        IF k$ = CHR$(13) THEN RUN "present.bas"
        IF k$ = "r" THEN GOSUB refresh
        IF k$ = "n" THEN GOSUB switchpersonx
        IF k$ = "a" THEN GOSUB switchpersonxa
        IF k$ = CHR$(0) + CHR$(72) THEN GOSUB screenup
        IF k$ = CHR$(0) + "P" THEN GOSUB screendwn
        IF k$ = CHR$(0) + "M" THEN GOSUB screenrt
        IF k$ = CHR$(0) + "K" THEN GOSUB screenlft
        IF k$ = "m" THEN GOSUB map
        IF mousescroll = 1 THEN
                IF xmouse% = 0 THEN mousehide: GOSUB screenlft: mouseshow
                IF xmouse% = 638 THEN mousehide: GOSUB screenrt: mouseshow
                IF ymouse% = 0 THEN mousehide: GOSUB screenup: mouseshow
                IF ymouse% = 199 THEN mousehide: GOSUB screendwn: mouseshow
        END IF
'*****end key check*****
minkuso3:
minkuso34:
cc = cc + 1
mousestatus lb%, rb%, xmouse%, ymouse%
xmousebox = INT(xmouse% / 40) + 1
ymousebox = INT(ymouse% / 30) + 1
IF ymousebox > 6 THEN ymousebox = 6
IF xmousebox > 15 THEN xmousebox = 15
xmousebox = xmousebox + xboundmin
ymousebox = ymousebox + yboundmin
'FIND jarak ***
                seljarakx = xplayerbox - xmousebox
                seljaraky = yplayerbox - ymousebox
                IF seljarakx < 0 THEN seljarakx = xmousebox - xplayerbox
                IF seljaraky < 0 THEN seljaraky = ymousebox - yplayerbox
'Find jarak end***
IF LEFT$(map$(xmousebox, ymousebox), 1) = "1" THEN LOCATE 23: PRINT "Select": GOTO moolano
IF attackturn(mann) = 0 AND LEFT$(map$(xmousebox, ymousebox), 1) = "2" THEN LOCATE 23: COLOR 16: PRINT "Attack": COLOR 15: GOTO moolano
IF LEFT$(map$(xmousebox, ymousebox), 1) = "2" THEN LOCATE 23: PRINT "Attack": GOTO moolano
IF seljarakx > playerspd OR seljaraky > playerspd THEN LOCATE 23: COLOR 16: PRINT "Move  ": COLOR 15: GOTO moolano
IF movealready(mann) = 1 THEN LOCATE 23: COLOR 16: PRINT "Move  ": COLOR 15: GOTO moolano
IF map$(xmousebox, ymousebox) <> "grass" THEN LOCATE 23: COLOR 16: PRINT "Move  ": COLOR 15: GOTO moolano
LOCATE 23: PRINT "Move  "
moolano:
'***RIGHT-CLICK***
IF rb% = -1 THEN
   turn = 1
END IF
IF turn = 1 THEN GOTO pcturn:
'***CLICK***
IF lb% = -1 THEN
kjp:
mousestatus lb%, rb%, xmouse%, ymouse%
IF lb% = -1 THEN GOTO kjp
'FIND jarak ***
                seljarakx = xplayerbox - xmousebox
                seljaraky = yplayerbox - ymousebox
                IF seljarakx < 0 THEN seljarakx = xmousebox - xplayerbox
                IF seljaraky < 0 THEN seljaraky = ymousebox - yplayerbox
'Find jarak end***
   IF LEFT$(map$(xmousebox, ymousebox), 1) = "1" THEN
   IF xmousebox = xplayerbox AND ymousebox = yplayerbox THEN GOSUB playerinfo: GOTO moola
   GOSUB switchperson
   GOTO moola
   END IF
   IF seljarakx > playerspd OR seljaraky > playerspd THEN LOCATE 23: COLOR 16: PRINT "Move  ": COLOR 15: GOSUB enemyinfo: GOTO moola
   IF LEFT$(map$(xmousebox, ymousebox), 1) = "2" THEN attackmode = 1: GOTO outbox
   IF map$(xmousebox, ymousebox) <> "grass" THEN LOCATE 23: COLOR 16: PRINT "Move  ": COLOR 15: GOTO moola
   targetx = xmousebox
   targety = ymousebox
   IF targetx = xplayerbox AND targety = yplayerbox THEN GOTO moola
   IF movealready(mann) = 1 THEN SOUND 200, .08: GOTO moola
   movemode = 1
moola:
END IF
outbox:
cc = cc + 1
cc2 = cc2 + 1
skipper2:
IF movemode = 1 THEN
        IF moveturn(mann) = 0 THEN GOTO pooimovened
        mousehide
        'movemode
        PUTimage xplayerbox, yplayerbox, oldmap$
minkuso:
        GOSUB cbound
        map$(xplayerbox, yplayerbox) = oldmap$
whirl:
        pox = 0
        IF targetx < xplayerbox THEN xplayerbox = xplayerbox - 1
        IF targetx > xplayerbox THEN xplayerbox = xplayerbox + 1
        IF targety < yplayerbox THEN yplayerbox = yplayerbox - 1
        IF targety > yplayerbox THEN yplayerbox = yplayerbox + 1
        oldmap$ = map$(xplayerbox, yplayerbox)
        PUTimage xplayerbox, yplayerbox, playertype$
minkuso234:
        GOSUB cbound
        map$(xplayerbox, yplayerbox) = playertype$
        IF targetx = xplayerbox AND targety = yplayerbox THEN SOUND 100, .1: movealready(mann) = 1: movemode = 0: moveturn(mann) = 0: GOTO pooimovened
pooimovened:
mouseshow
END IF
'*****ATTACK!!!******


IF attackmode = 1 THEN
        IF attackturn(mann) = 0 THEN GOSUB enemyinfo: GOTO pooiaway
        'attackmode
        FOR kkk = 1 TO maxenmy
        IF mxenemybox(kkk) = xmousebox AND myenemybox(kkk) = ymousebox THEN pdp = kkk
        NEXT
minkuso28:
'FIND jarak ***
                jarakx = xplayerbox - mxenemybox(pdp)
                jaraky = yplayerbox - myenemybox(pdp)
                IF jarakx < 0 THEN jarakx = mxenemybox(pdp) - xplayerbox
                IF jaraky < 0 THEN jaraky = myenemybox(pdp) - yplayerbox
'Find jarak end***
                molo = 2
                IF playertype$ = "1pult" THEN molo = 4
                IF jarakx >= molo AND jaraky >= molo THEN GOSUB enemyinfo
                IF jarakx < molo THEN
                IF jaraky < molo THEN
                   FOR T = 1 TO 5
'bound check**
    IF mxenemybox(pdp) + 1 < xboundmin THEN GOTO minkuso22
    IF mxenemybox(pdp) - 1 > xboundmax THEN GOTO minkuso22
    IF myenemybox(pdp) + 1 < yboundmin THEN GOTO minkuso22
    IF myenemybox(pdp) - 1 > yboundmax THEN GOTO minkuso22
        enemyx = (mxenemybox(pdp) - 1 - xboundmin) * 20 + 1
        enemyy = (myenemybox(pdp) - 1 - yboundmin) * 30 + 1
        playerx = (xplayerbox - 1 - xboundmin) * 20 + 1
        playery = (yplayerbox - 1 - yboundmin) * 30 + 1
        IF playerx < 1 THEN GOTO moori
        IF playery < 1 THEN GOTO moori
        IF playerx > 300 THEN GOTO moori
        IF playery > 171 THEN GOTO moori
        IF enemyx < 1 THEN GOTO minkuso22
        IF enemyy < 1 THEN GOTO minkuso22
        IF enemyx > 300 THEN GOTO minkuso22
        IF enemyy > 171 THEN GOTO minkuso22
                   IF guideline = 1 THEN LINE (enemyx + 10, enemyy + 15)-(playerx + 10, playery + 15), 78
moori:
                   CIRCLE (enemyx + 10, enemyy + 15), T, 15
minkuso22:
                    FOR lag = 1 TO 1000: NEXT lag
                    SOUND 200, .2
                   NEXT T
'****Attack****


                   damage = Attacknumber(playertype$, playerlife, menemytype$(pdp), menemylife(pdp), 1)
                   damage2 = Attacknumber(menemytype$(pdp), menemylife(pdp), playertype$, playerlife, 0)
                   playerlife = playerlife - damage2
                   menemylife(pdp) = menemylife(pdp) - damage
                   attackturn(mann) = 0
                   name$ = showname$(playertype$)
                   name2$ = showname$(menemytype$(pdp))
                   mousehide
                   drawbox 30, 60, 280, 120
                   COLOR 48: LOCATE 8, 17: PRINT "Damage"
                   COLOR 78: LOCATE 11, 6: PRINT name$; " hit "; damage2; " Life:"; playerlife: COLOR 15
                   COLOR 78: LOCATE 12, 6: PRINT name2$; " hit "; damage; " Life:"; menemylife(pdp): COLOR 15
                   mouseshow
                   getbutton
                   FOR lag = 1 TO 1000: NEXT lag
                   IF playerlife < 1 THEN pdead = 1: GOSUB deadhuman2
                   IF menemylife(pdp) < 1 THEN medead(pdp) = 1: GOSUB deadcomp2
                   GOSUB refresh
                   GOTO pooi
               END IF
               END IF
pooi:
        PUTimage mxenemybox(pdp), myenemybox(pdp), menemytype$(pdp)
poali:
        PUTimage xplayerbox, yplayerbox, playertype$
poalin:
pooiaway:
        attackmode = 0
END IF
cc = 0
skipper:
LOOP
'********************COMPUTER MOVEMENT*****************************



pcturn:
monk = 0
GOSUB reinit
GOSUB switchpc
k$ = INKEY$
IF k$ = CHR$(13) THEN RUN "present.bas"
DO
turn = 0
spoldcall = 0
spcall = 0
'FIND jarak ***
            monko = 0
            xot = xenemybox - enemyspd
            xott = xenemybox + enemyspd
            yot = yenemybox - enemyspd
            yott = yenemybox + enemyspd
            IF xot < 1 THEN xot = 1
            IF yot < 1 THEN yot = 1
            IF xott > 30 THEN xott = 30
            IF yott > 30 THEN yott = 30
            FOR xcom = xot TO xott
            FOR ycom = yot TO yott
                ttzy$ = LEFT$(map$(xcom, ycom), 1)
                IF ttzy$ = "1" THEN GOSUB xcon
            NEXT
            NEXT
IF map$(pctargetx, pctargety) <> ttzy$ THEN GOSUB toto
rea:
            monko = 0
            IF map$(pctargetx, pctargety) = "grass" THEN GOTO noneedlar
            FOR xcom = xot TO xott
            FOR ycom = yot TO yott
                IF pts = 1 THEN pts = 0: GOTO minkto
                IF map$(xcom, ycom) = "grass" THEN : GOSUB zcon
            NEXT
            NEXT
minkto:
            pctargetx = tyx
            pctargety = tyy
noneedlar:
DO
        'movemode for pc
        map$(xenemybox, yenemybox) = pcoldmap$
        PUTimage xenemybox, yenemybox, pcoldmap$
minku:
        GOSUB cbound
        IF pctargetx < xenemybox THEN xenemybox = xenemybox - 1
        IF pctargetx > xenemybox THEN xenemybox = xenemybox + 1
        IF pctargety < yenemybox THEN yenemybox = yenemybox - 1
        IF pctargety > yenemybox THEN yenemybox = yenemybox + 1
        enemyx = (xenemybox - 1 - xboundmin) * 20 + 1
        enemyy = (yenemybox - 1 - yboundmin) * 30 + 1
       GOSUB cbound
        pcoldmap$ = map$(xenemybox, yenemybox)
        map$(xenemybox, yenemybox) = enemytype$
        PUTimage xenemybox, yenemybox, enemytype$
minkusz:
        IF pctargetx = xenemybox AND pctargety = yenemybox THEN pcmovemode = 0: pcmoveturn = 0: SOUND 100, .1: EXIT DO
    FOR lag = 1 TO 1000: NEXT lag
LOOP
pcoldmap$ = "grass"
FOR lag = 1 TO 10000: NEXT lag
fig:
fin:
FOR mst = 1 TO maxman
IF mxplayerbox(mst) = pctargetx AND myplayerbox(mst) = pctargety AND mpdead(mst) = 0 THEN mic = mst: GOTO real
NEXT
real:
commyx = xenemybox - 1
commyx2 = xenemybox + 1
commyy = yenemybox - 1
commyy2 = yenemybox + 1
FOR mot = 1 TO maxman
FOR ckx = commyx TO commyx2
FOR cky = commyy TO commyy2
IF mxplayerbox(mot) = ckx AND myplayerbox(mot) = cky THEN mic = mot
NEXT: NEXT: NEXT
'*********ATTACK!!!**************
        'attackmode
'FIND jarak ***
                jarakx = mxplayerbox(mic) - xenemybox
                jaraky = myplayerbox(mic) - yenemybox
                IF jarakx < 0 THEN jarakx = xenemybox - mxplayerbox(mic)
                IF jaraky < 0 THEN jaraky = yenemybox - myplayerbox(mic)
'Find jarak end***
                tombraid = 2
                IF enemytype$ = "2pult" THEN tombraid = 4
                IF jarakx < tombraid THEN
                IF jaraky < tombraid THEN
                   FOR T = 1 TO 5
    IF mxplayerbox(mic) + 1 < xboundmin THEN GOTO mink
    IF mxplayerbox(mic) - 1 > xboundmax THEN GOTO mink
    IF myplayerbox(mic) + 1 < yboundmin THEN GOTO mink
    IF myplayerbox(mic) - 1 > yboundmax THEN GOTO mink
        playerx = (mxplayerbox(mic) - 1 - xboundmin) * 20 + 1
        playery = (myplayerbox(mic) - 1 - yboundmin) * 30 + 1
        enemyx = (xenemybox - 1 - xboundmin) * 20 + 1
        enemyy = (yenemybox - 1 - yboundmin) * 30 + 1
        IF enemyx < 1 THEN GOTO minkty
        IF enemyy < 1 THEN GOTO minkty
        IF enemyx > 300 THEN GOTO minkty
        IF enemyy > 171 THEN GOTO minkty
        IF playerx < 1 THEN GOTO mink
        IF playery < 1 THEN GOTO mink
        IF playerx > 300 THEN GOTO mink
        IF playery > 171 THEN GOTO mink
                   IF guideline = 1 THEN LINE (enemyx + 10, enemyy + 15)-(playerx + 10, playery + 15), 78
minkty:
                   CIRCLE (playerx + 10, playery + 15), T, 15
mink:
                    FOR lag = 1 TO 1000: NEXT lag
                    SOUND 200, .2
                   NEXT T
                   damage = Attacknumber(enemytype$, enemylife, mplayertype$(mic), mplayerlife(mic), 1)
                   damage2 = Attacknumber(mplayertype$(mic), mplayerlife(mic), enemytype$, enemylife, 0)
                   mplayerlife(mic) = mplayerlife(mic) - damage
                   enemylife = enemylife - damage2
                   name$ = showname$(mplayertype$(mic))
                   name2$ = showname$(enemytype$)
                   mousehide
                   drawbox 30, 60, 280, 120
                   COLOR 48: LOCATE 8, 15: PRINT "Damage"
                   COLOR 78: LOCATE 11, 6: PRINT name$; " hit "; damage; " Life:"; mplayerlife(mic): COLOR 15
                   COLOR 78: LOCATE 12, 6: PRINT name2$; " hit "; damage2; " Life:"; enemylife: COLOR 15
                   mouseshow
                   getbutton
                   GOSUB refresh
                   IF mplayerlife(mic) < 1 THEN mpdead(mic) = 1: GOSUB deadhuman
                   IF enemylife < 1 THEN edead = 1: GOSUB deadcomp
                   pcattackturn = 0
                   PUTimage mxplayerbox(mic), myplayerbox(mic), mplayertype$(mic)
pcpooiaway:
               END IF
               END IF
pooipc:
        PUTimage xenemybox, yenemybox, enemytype$
minkp:
'*****Outside bound*******
        pcattackmode = 0
'******finishing touch********
turn = 0
GOSUB switchpc
LOOP UNTIL enemie >= maxenmy
FOR m = 1 TO maxman
attackturn(m) = 1
moveturn(m) = 1
movealready(m) = 0
NEXT
enemie = 0
GOTO pturn
'**********SCREEN REFRESH**********
refresh:
mousehide
FOR xt = 1 TO 15
FOR yt = 1 TO 6
x3 = (xt - 1) * 20 + 1
y3 = (yt - 1) * 30 + 1
Box$(xt, yt) = map$(xt + xboundmin, yt + yboundmin)
IF Box$(xt, yt) = "farm" THEN PUT (x3, y3), barracks, PSET
IF Box$(xt, yt) = "rock" THEN PUT (x3, y3), rock, PSET
IF Box$(xt, yt) = "grass" THEN PUT (x3, y3), grass, PSET
IF Box$(xt, yt) = "2man" THEN PUT (x3, y3), man2, PSET
IF Box$(xt, yt) = "1man" THEN PUT (x3, y3), man1, PSET
IF Box$(xt, yt) = "2arch" THEN PUT (x3, y3), arch2, PSET
IF Box$(xt, yt) = "1arch" THEN PUT (x3, y3), arch, PSET
IF Box$(xt, yt) = "1ogre" THEN PUT (x3, y3), ogre, PSET
IF Box$(xt, yt) = "1pult" THEN PUT (x3, y3), pult, PSET
IF Box$(xt, yt) = "2ogre" THEN PUT (x3, y3), ogre2, PSET
IF Box$(xt, yt) = "2pult" THEN PUT (x3, y3), pult2, PSET
IF Box$(xt, yt) = "2skirm" THEN PUT (x3, y3), skirm2, PSET
IF Box$(xt, yt) = "1skirm" THEN PUT (x3, y3), skirm1, PSET
NEXT
NEXT
mouseshow
RETURN


screendwn:
yboundmin = yboundmin + 1
yboundmax = yboundmax + 1
IF yboundmin > 24 THEN yboundmin = 24
IF yboundmax > 30 THEN yboundmax = 30
FOR xt = 1 TO 15
FOR yt = 1 TO 6
xxxx = (xt - 1) * 20 + 1
yyyy = (yt - 1) * 30 + 1
Box$(xt, yt) = map$(xt + xboundmin, yt + yboundmin)
GOSUB minti
NEXT yt
NEXT xt
RETURN


screenup:
yboundmin = yboundmin - 1
yboundmax = yboundmax - 1
IF yboundmax < 6 THEN yboundmax = 6
IF yboundmin < 0 THEN yboundmin = 0
FOR xt = 1 TO 15
FOR yt = 1 TO 6
xxxx = (xt - 1) * 20 + 1
yyyy = (yt - 1) * 30 + 1
Box$(xt, yt) = map$(xt + xboundmin, yt + yboundmin)
GOSUB minti
NEXT yt
NEXT xt
RETURN


screenlft:
xboundmin = xboundmin - 1
xboundmax = xboundmax - 1
IF xboundmax < 15 THEN xboundmax = 15
IF xboundmin < 0 THEN xboundmin = 0
FOR xt = 1 TO 15
FOR yt = 1 TO 6
xxxx = (xt - 1) * 20 + 1
yyyy = (yt - 1) * 30 + 1
Box$(xt, yt) = map$(xt + xboundmin, yt + yboundmin)
GOSUB minti
NEXT yt
NEXT xt
RETURN


screenrt:
xboundmin = xboundmin + 1
xboundmax = xboundmax + 1
IF xboundmax > 30 THEN xboundmax = 30
IF xboundmin > 15 THEN xboundmin = 15
FOR xt = 1 TO 15
FOR yt = 1 TO 6
xxxx = (xt - 1) * 20 + 1
yyyy = (yt - 1) * 30 + 1
Box$(xt, yt) = map$(xt + xboundmin, yt + yboundmin)
GOSUB minti
NEXT yt
NEXT xt
RETURN

cbound:
        IF xenemybox > 30 THEN xenemybox = 30
        IF yenemybox > 30 THEN yenemybox = 30
        IF xenemybox < 0 THEN xenemybox = 0
        IF yenemybox < 0 THEN yenemybox = 0
RETURN


map:
mousehide
CLS
LINE (210, 25)-(220, 30), 23, BF: LOCATE 4, 30: PRINT "Terrain"
LINE (210, 40)-(220, 45), 14, BF: LOCATE 6, 30: PRINT "Army"
LINE (210, 55)-(220, 60), 10, BF: LOCATE 8, 30: PRINT "Enemy"
LINE (210, 70)-(220, 75), 146, BF: LOCATE 10, 30: PRINT "Tree"
LINE (210, 85)-(220, 90), 70, BF: LOCATE 12, 30: PRINT "Rock"
COLOR 72: LOCATE 22, 13: PRINT "Tactical Map": COLOR 15
FOR xt = 1 TO 30
FOR yt = 1 TO 30
xxxx = (xt - 1) * 5 + 1
yyyy = (yt - 1) * 5 + 1
IF map$(xt, yt) = "rock" THEN LINE (xxxx, yyyy)-(xxxx + 3, yyyy + 3), 70, BF
IF map$(xt, yt) = "farm" THEN LINE (xxxx, yyyy)-(xxxx + 3, yyyy + 3), 146, BF
IF map$(xt, yt) = "grass" THEN LINE (xxxx, yyyy)-(xxxx + 3, yyyy + 3), 23, BF
IF LEFT$(map$(xt, yt), 1) = "2" THEN LINE (xxxx, yyyy)-(xxxx + 3, yyyy + 3), 10, BF
IF LEFT$(map$(xt, yt), 1) = "1" THEN LINE (xxxx, yyyy)-(xxxx + 3, yyyy + 3), 14, BF
NEXT yt
NEXT xt
LINE (xboundmin * 5, yboundmin * 5)-(xboundmax * 5, yboundmax * 5), 79, B
getbutton
CLS
GOSUB refresh
mouseshow
RETURN


switchperson:
oldman = mann
FOR mst = 1 TO maxman
IF mxplayerbox(mst) = xmousebox AND myplayerbox(mst) = ymousebox THEN mann = mst
NEXT
mxplayerbox(oldman) = xplayerbox
myplayerbox(oldman) = yplayerbox
mplayertype$(oldman) = playertype$
mpdead(oldman) = pdead
pdead = mpdead(mann)
playertype$ = mplayertype$(mann)
mplayerlife(oldman) = playerlife
playerlife = mplayerlife(mann)
mplayerspd(oldman) = playerspd
playerspd = mplayerspd(mann)
xplayerbox = mxplayerbox(mann)
yplayerbox = myplayerbox(mann)
playerx = (xplayerbox - 1 - xboundmin) * 20 + 1
playery = (yplayerbox - 1 - yboundmin) * 30 + 1
LINE (playerx, playery)-(playerx + 18, playery + 28), 72, B
FOR lag = 1 TO 2000: NEXT lag
PUTimage xplayerbox, yplayerbox, playertype$
SOUND 100, .5
RETURN

switchpersonx:
oldman = mann
rpgo = 1
rpbox:
IF rpgo > maxman THEN SOUND 50, .5: RETURN
mann = mann + 1
IF mann > maxman THEN mann = 1
IF moveturn(mann) = 0 THEN rpgo = rpgo + 1: GOTO rpbox
mxplayerbox(oldman) = xplayerbox
myplayerbox(oldman) = yplayerbox
mplayertype$(oldman) = playertype$
mpdead(oldman) = pdead
pdead = mpdead(mann)
playertype$ = mplayertype$(mann)
mplayerlife(oldman) = playerlife
playerlife = mplayerlife(mann)
mplayerspd(oldman) = playerspd
playerspd = mplayerspd(mann)
xplayerbox = mxplayerbox(mann)
yplayerbox = myplayerbox(mann)
xboundmin = xplayerbox - 7
yboundmin = yplayerbox - 3
xboundmax = xplayerbox + 8
yboundmax = yplayerbox + 3
GOSUB checkbound
GOSUB refresh
playerx = (xplayerbox - 1 - xboundmin) * 20 + 1
playery = (yplayerbox - 1 - yboundmin) * 30 + 1
LINE (playerx, playery)-(playerx + 18, playery + 28), 72, B
FOR lag = 1 TO 2000: NEXT lag
PUTimage xplayerbox, yplayerbox, playertype$
SOUND 100, .5
RETURN
switchpersonxa:
oldman = mann
ripgo = 1
ripbox:
IF ripgo > maxman THEN SOUND 50, .5: RETURN
mann = mann + 1
IF mann > maxman THEN mann = 1
IF attackturn(mann) = 0 THEN ripgo = ripgo + 1: GOTO ripbox
mxplayerbox(oldman) = xplayerbox
myplayerbox(oldman) = yplayerbox
mplayertype$(oldman) = playertype$
mpdead(oldman) = pdead
pdead = mpdead(mann)
playertype$ = mplayertype$(mann)
mplayerlife(oldman) = playerlife
playerlife = mplayerlife(mann)
mplayerspd(oldman) = playerspd
playerspd = mplayerspd(mann)
xplayerbox = mxplayerbox(mann)
yplayerbox = myplayerbox(mann)
xboundmin = xplayerbox - 7
yboundmin = yplayerbox - 3
xboundmax = xplayerbox + 8
yboundmax = yplayerbox + 3
GOSUB checkbound
GOSUB refresh
playerx = (xplayerbox - 1 - xboundmin) * 20 + 1
playery = (yplayerbox - 1 - yboundmin) * 30 + 1
LINE (playerx, playery)-(playerx + 18, playery + 28), 72, B
FOR lag = 1 TO 2000: NEXT lag
PUTimage xplayerbox, yplayerbox, playertype$
SOUND 100, .5
RETURN

switchpc:
yyy = 0
yyx = 0
oldenemy = enemie
enemie = enemie + 1
IF enemie > maxenmy THEN enemie = enemie - 2
medead(oldenemy) = edead
edead = medead(enemie)
mxenemybox(oldenemy) = xenemybox
myenemybox(oldenemy) = yenemybox
menemytype$(oldenemy) = enemytype$
menemylife(oldenemy) = enemylife
enemylife = menemylife(enemie)
enemytype$ = menemytype$(enemie)
menemyspd(oldenemy) = enemyspd
enemyspd = menemyspd(enemie)
xenemybox = mxenemybox(enemie)
yenemybox = myenemybox(enemie)
IF viewmove = 1 THEN
xboundmin = xenemybox - 7
yboundmin = yenemybox - 3
xboundmax = xenemybox + 8
yboundmax = yenemybox + 3
GOSUB checkbound
GOSUB refresh
END IF
enemyx = (xenemybox - 1 - xboundmin) * 20 + 1
enemyy = (yenemybox - 1 - yboundmin) * 30 + 1
FOR lag = 1 TO 2000: NEXT lag
SOUND 100, .5
RETURN
wrong:
                LOCATE 10, 30: PRINT "Avilable Space", FRE("")
                LOCATE 11, 30: PRINT "Unused Stack Space", FRE(-2)
                LOCATE 12, 30: PRINT "Array Space", FRE(-1)
                m$ = INPUT$(1)
mig:
        IF pctargetx < mxplayerbox(mic) THEN pctargetx = pctargetx - 1: IF map$(pctargetx, pctargety) = "grass" THEN RETURN
        IF pctargetx > mxplayerbox(mic) THEN pctargetx = pctargetx + 1: IF map$(pctargetx, pctargety) = "grass" THEN RETURN
        IF pctargety < myplayerbox(mic) THEN pctargety = pctargety - 1: IF map$(pctargetx, pctargety) = "grass" THEN RETURN
        IF pctargety > myplayerbox(mic) THEN pctargety = pctargety + 1: IF map$(pctargetx, pctargety) = "grass" THEN RETURN
RETURN
reinit:
oldman = mann
mxplayerbox(oldman) = xplayerbox
myplayerbox(oldman) = yplayerbox
RETURN

xcon:
'FIND jarak ***
                yx = xcom - xenemybox
                yy = ycom - yenemybox
                IF yx < 0 THEN yx = xenemybox - xcom
                IF yy < 0 THEN yy = yenemybox - ycom
'***Find jarak end***
movb = yx * yx + yy * yy
movb = SQR(movb)
IF monk = 0 THEN movold = movb + 1
IF movb < movold THEN pctargetx = xcom: pctargety = ycom
monk = 1
movold = movb
RETURN



zcon:
'FIND jarak ***
                yyx = pctargetx - xcom
                yyy = pctargety - ycom
                IF yyx < 0 THEN yyx = xcom - pctargetx
                IF yyy < 0 THEN yyy = ycom - pctargety
'***Find jarak end***
IF yyy <= 1 AND yyx <= 1 THEN pts = 1: tyx = xcom: tyy = ycom: RETURN
IF monko = 0 THEN oyyx = yyx: oyyy = yyy
IF yyy <= oyyy AND yyx <= oyyx THEN tyx = xcom: tyy = ycom
monko = 1
oyyy = yyy
oyyx = yyx
RETURN
toto:
'FIND jarak ***
            FOR tee = 1 TO maxman
                IF mpdead(tee) = 1 THEN GOTO exte
                spcx = mxplayerbox(tee) - xenemybox
                spcy = myplayerbox(tee) - yenemybox
                IF spcx < 0 THEN spcx = xenemybox - mxplayerbox(tee)
                IF spcy < 0 THEN spcy = yenemybox - myplayerbox(tee)
                IF spcy <= 1 AND spcx <= 1 THEN mic = tee: GOTO fin
                spcall = spcx * spcx + spcy * spcy
                spcall = INT(SQR(spcall))
                IF spcall <= spoldcall THEN mic = tee
                spoldcall = spcall
exte:
            NEXT
'***Find jarak end***
'****check placement******
IF xenemybox < mxplayerbox(mic) THEN pmentx = -1
IF xenemybox > mxplayerbox(mic) THEN pmentx = 1
IF yenemybox < myplayerbox(mic) THEN pmenty = -1
IF yenemybox > myplayerbox(mic) THEN pmenty = 1
'****movement limit******
pctargetx = mxplayerbox(mic) + pmentx
pctargety = myplayerbox(mic) + pmenty
'FIND jarak ***
                selpcjarakx = pctargetx - xenemybox
                selpcjaraky = pctargety - yenemybox
                IF selpcjarakx < 0 THEN selpcjarakx = xenemybox - pctargetx
                IF selpcjaraky < 0 THEN selpcjaraky = yenemybox - pctargety
'***Find jarak end***
   IF selpcjarakx > enemyspd OR selpcjaraky > enemyspd THEN
      seljarakxtra = selpcjarakx - enemyspd
      seljarakytra = selpcjaraky - enemyspd
        IF xenemybox < mxplayerbox(mic) THEN pctargetx = pctargetx - seljarakxtra: GOTO piti
        IF xenemybox > mxplayerbox(mic) THEN pctargetx = pctargetx + seljarakxtra: GOTO piti
piti:
        IF yenemybox < myplayerbox(mic) THEN pctargety = pctargety - seljarakytra: GOTO tiri
        IF yenemybox > myplayerbox(mic) THEN pctargety = pctargety + seljarakytra
tiri:
   END IF
IF pctargetx < 1 THEN pctargetx = 0
IF pctargetx > 30 THEN pctargetx = 30
IF pctargety < 1 THEN pctargetx = 0
IF pctargety > 30 THEN pctargetx = 30
RETURN
minti:
IF Box$(xt, yt) = "farm" THEN PUT (xxxx, yyyy), barracks, PSET
IF Box$(xt, yt) = "rock" THEN PUT (xxxx, yyyy), rock, PSET
IF Box$(xt, yt) = "grass" THEN PUT (xxxx, yyyy), grass, PSET
IF Box$(xt, yt) = "2man" THEN PUT (xxxx, yyyy), man2, PSET
IF Box$(xt, yt) = "1man" THEN PUT (xxxx, yyyy), man1, PSET
IF Box$(xt, yt) = "2arch" THEN PUT (xxxx, yyyy), arch2, PSET
IF Box$(xt, yt) = "1arch" THEN PUT (xxxx, yyyy), arch, PSET
IF Box$(xt, yt) = "1ogre" THEN PUT (xxxx, yyyy), ogre, PSET
IF Box$(xt, yt) = "1pult" THEN PUT (xxxx, yyyy), pult, PSET
IF Box$(xt, yt) = "2ogre" THEN PUT (xxxx, yyyy), ogre2, PSET
IF Box$(xt, yt) = "2pult" THEN PUT (xxxx, yyyy), pult2, PSET
IF Box$(xt, yt) = "2skirm" THEN PUT (xxxx, yyyy), skirm2, PSET
IF Box$(xt, yt) = "1skirm" THEN PUT (xxxx, yyyy), skirm1, PSET
RETURN


deadhuman:
map$(mxplayerbox(mic), myplayerbox(mic)) = "grass"
oldmap$ = "grass"
IF mic = maxman THEN maxman = maxman - 1: GOTO yoman
FOR iff = mic TO maxman - 1
mxplayerbox(iff) = mxplayerbox(iff + 1)
myplayerbox(iff) = myplayerbox(iff + 1)
mplayertype$(iff) = mplayertype$(iff + 1)
mplayerspd(iff) = mplayerspd(iff + 1)
mplayerlife(iff) = mplayerlife(iff + 1)
NEXT iff
maxman = maxman - 1
yoman:
IF maxman = 0 THEN GOTO gameover
dh = dh + 1
GOSUB switchperson2
GOSUB refresh
RETURN

deadcomp:
map$(xenemybox, yenemybox) = "grass"
IF enemie = maxenmy THEN maxenmy = maxenmy - 1: GOTO yoman2
FOR iff = enemie TO maxenmy - 1
mxenemybox(iff) = mxenemybox(iff + 1)
myenemybox(iff) = myenemybox(iff + 1)
menemytype$(iff) = menemytype$(iff + 1)
menemyspd(iff) = menemyspd(iff + 1)
menemylife(iff) = menemylife(iff + 1)
NEXT iff
maxenmy = maxenmy - 1
yoman2:
IF maxenmy = 0 THEN GOTO gamewin
dp = dp + 1
GOSUB switchpc2
GOSUB refresh
RETURN

deadhuman2:
map$(xplayerbox, yplayerbox) = "grass"
oldmap$ = "grass"
IF mann = maxman THEN maxman = maxman - 1: GOTO yoman3
FOR iff = mann TO maxman - 1
mxplayerbox(iff) = mxplayerbox(iff + 1)
myplayerbox(iff) = myplayerbox(iff + 1)
mplayertype$(iff) = mplayertype$(iff + 1)
mplayerspd(iff) = mplayerspd(iff + 1)
mplayerlife(iff) = mplayerlife(iff + 1)
NEXT iff
maxman = maxman - 1
yoman3:
IF maxman = 0 THEN GOTO gameover
dh = dh + 1
GOSUB switchperson2
GOSUB refresh
RETURN

deadcomp2:
map$(mxenemybox(pdp), myenemybox(pdp)) = "grass"
IF pdp = maxenmy THEN maxenmy = maxenmy - 1: GOTO yoil
FOR iff = pdp TO maxenmy - 1
mxenemybox(iff) = mxenemybox(iff + 1)
myenemybox(iff) = myenemybox(iff + 1)
menemytype$(iff) = menemytype$(iff + 1)
menemyspd(iff) = menemyspd(iff + 1)
menemylife(iff) = menemylife(iff + 1)
NEXT iff
maxenmy = maxenmy - 1
IF maxenmy = 0 THEN GOTO gamewin
yoil:
dp = dp + 1
GOSUB refresh
GOSUB switchpc2
RETURN

switchperson2:
GOSUB refresh
IF oldman > maxman THEN oldman = maxman
IF oldman < 1 THEN oldman = 1
oldman = mann
ore = 0
pdead = mpdead(mann)
playertype$ = mplayertype$(mann)
playerlife = mplayerlife(mann)
playerspd = mplayerspd(mann)
xplayerbox = mxplayerbox(mann)
yplayerbox = myplayerbox(mann)
FOR lag = 1 TO 2000: NEXT lag
SOUND 100, .5
RETURN

switchpc2:
GOSUB refresh
yyy = 0
yyx = 0
oldenemy = enemie
medead(oldenemy) = edead
edead = medead(enemie)
enemylife = menemylife(enemie)
enemytype$ = menemytype$(enemie)
enemyspd = menemyspd(enemie)
xenemybox = mxenemybox(enemie)
yenemybox = myenemybox(enemie)
enemyx = (xenemybox - 1 - xboundmin) * 20 + 1
enemyy = (yenemybox - 1 - yboundmin) * 30 + 1
FOR lag = 1 TO 2000: NEXT lag
SOUND 100, .5
RETURN


addman:
ks = ks + 1
        mpdead(ks) = 0
        mxplayerbox(ks) = mapinitx
        myplayerbox(ks) = mapinity
IF RIGHT$(koi$, LEN(koi$) - 1) = "man" THEN
        mplayertype$(ks) = "1man"
        mplayerlife(ks) = 20
        mplayerspd(ks) = 5
END IF
IF RIGHT$(koi$, LEN(koi$) - 1) = "ogre" THEN
        mplayertype$(ks) = "1ogre"
        mplayerlife(ks) = 25
        mplayerspd(ks) = 3
END IF
IF RIGHT$(koi$, LEN(koi$) - 1) = "pult" THEN
        mplayertype$(ks) = "1pult"
        mplayerlife(ks) = 20
        mplayerspd(ks) = 2
END IF
IF RIGHT$(koi$, LEN(koi$) - 1) = "arch" THEN
        mplayertype$(ks) = "1arch"
        mplayerlife(ks) = 20
        mplayerspd(ks) = 4
END IF
IF RIGHT$(koi$, LEN(koi$) - 1) = "skirm" THEN
        mplayertype$(ks) = "1skirm"
        mplayerlife(ks) = 20
        mplayerspd(ks) = 4
END IF
RETURN
addenmy:
ksj = ksj + 1
        medead(ksj) = 0
        mxenemybox(ksj) = mapinitx
        myenemybox(ksj) = mapinity
IF RIGHT$(koi$, LEN(koi$) - 1) = "man" THEN
        menemytype$(ksj) = "2man"
        menemylife(ksj) = 20
        menemyspd(ksj) = 5
END IF
IF RIGHT$(koi$, LEN(koi$) - 1) = "ogre" THEN
        menemytype$(ksj) = "2ogre"
        menemylife(ksj) = 25
        menemyspd(ksj) = 3
END IF
IF RIGHT$(koi$, LEN(koi$) - 1) = "pult" THEN
        menemytype$(ksj) = "2pult"
        menemylife(ksj) = 20
        menemyspd(ksj) = 2
END IF
IF RIGHT$(koi$, LEN(koi$) - 1) = "arch" THEN
        menemytype$(ksj) = "2arch"
        menemylife(ksj) = 20
        menemyspd(ksj) = 4
END IF
IF RIGHT$(koi$, LEN(koi$) - 1) = "skirm" THEN
        menemytype$(ksj) = "2skirm"
        menemylife(ksj) = 20
        menemyspd(ksj) = 4
END IF
RETURN

playerinfo:
drawbox 50, 30, 250, 100
utype$ = showname$(playertype$)
COLOR 253: LOCATE 6, 16: PRINT utype$
COLOR 78: LOCATE 9, 9: PRINT "Life              "; playerlife
COLOR 78: LOCATE 10, 9: PRINT "Speed             "; playerspd
COLOR 15
getbutton
GOSUB refresh
RETURN
enemyinfo:
pck = 0
FOR hoy = 1 TO maxenmy
IF mxenemybox(hoy) = xmousebox AND myenemybox(hoy) = ymousebox THEN pck = hoy
NEXT hoy
IF pck = 0 OR pck > maxenmy THEN RETURN
drawbox 50, 30, 250, 100
utype$ = showname$(menemytype$(pck))
COLOR 254: LOCATE 6, 16: PRINT utype$
COLOR 78: LOCATE 9, 9: PRINT "Life              "; menemylife(pck)
COLOR 78: LOCATE 10, 9: PRINT "Speed             "; menemyspd(pck)
COLOR 15
getbutton
GOSUB refresh
RETURN

gamewin:
mousehide
CLS
FOR x = 1 TO 11
COLOR x + 68: LOCATE x + 1, 10: PRINT "Mission Successful"
COLOR x + 68: LOCATE 23 - x, 10: PRINT "Mission Successful"
FOR lag = 1 TO delay / 10: NEXT
NEXT
FOR x = 1 TO 11
COLOR 0: LOCATE x + 1, 10: PRINT "Mission Successful"
COLOR 0: LOCATE 23 - x, 10: PRINT "Mission Successful"
FOR lag = 1 TO delay / 10: NEXT
NEXT
COLOR 255: LOCATE 12, 10: PRINT "Mission Successful"
k$ = INPUT$(1)
CLS
LOCATE 13, 8: PRINT "Code To Next Level("; level + 1; ")"
IF level = 1 THEN COLOR 254: LOCATE 14, 11: PRINT "     sonc        "
IF level = 2 THEN COLOR 254: LOCATE 14, 11: PRINT "     mole        "
IF level = 3 THEN COLOR 254: LOCATE 14, 11: PRINT "     goat        "
IF level = 4 THEN COLOR 254: LOCATE 14, 11: PRINT "     sock        "
IF level = 5 THEN COLOR 254: LOCATE 14, 11: PRINT "     rune        "
IF level = 6 THEN COLOR 254: LOCATE 14, 11: PRINT "     bean        "
IF level = 7 THEN COLOR 254: LOCATE 14, 11: PRINT "     wtts        "
IF level = 8 THEN COLOR 254: LOCATE 14, 11: PRINT "    quaint       "
k$ = INPUT$(1)
RUN "present.bas"
gameover:
mousehide
CLS
FOR x = 1 TO 12
COLOR x + 15: LOCATE x + 1, 13: PRINT "Mission Failed"
COLOR x + 15: LOCATE 23 - x, 13: PRINT "Mission Failed"
FOR lag = 1 TO delay / 10: NEXT
NEXT
FOR x = 1 TO 11
COLOR 0: LOCATE x + 1, 13: PRINT "Mission Failed"
COLOR 0: LOCATE 23 - x, 13: PRINT "Mission Failed"
FOR lag = 1 TO delay / 10: NEXT
NEXT
COLOR 27: LOCATE 11, 13: PRINT "Mission Failed"
COLOR 20: LOCATE 12, 14: PRINT "Replay(Y/N)"
DO
y$ = INKEY$
IF y$ = "y" THEN RUN "war2.bas"
IF y$ = "n" THEN RUN "present.bas"
LOOP

pauser:
KEY(10) OFF
KEY(15) OFF
COLOR 78: LOCATE 2, 16: PRINT "Pause"
getbutton
GOSUB refresh
KEY(10) ON
KEY(15) ON
COLOR 15
RETURN
exiter:
RUN "present.bas"

checkbound:
IF xboundmax < 15 THEN xboundmax = 15
IF xboundmin < 0 THEN xboundmin = 0
IF yboundmax < 6 THEN yboundmax = 6
IF yboundmin < 0 THEN yboundmin = 0
IF xboundmax > 30 THEN xboundmax = 30
IF yboundmax > 30 THEN yboundmax = 30
IF xboundmin > 15 THEN xboundmin = 15
IF yboundmin > 24 THEN yboundmin = 24
RETURN

add10:
playerlife = playerlife + 10
IF mann < 1 THEN mann = 1
IF mann > maxman THEN mann = maxman
mplayerlife(mann) = mplayerlife(mann) + 10
FOR xy = 200 TO 1000 STEP 100
SOUND xy, .1
NEXT
RETURN
bcome100:
playerlife = 100
IF mann < 1 THEN mann = 1
IF mann > maxman THEN mann = maxman
mplayerlife(mann) = 100
FOR xy = 200 TO 1000 STEP 100
SOUND xy, .1
NEXT
RETURN
speed:
playerspd = playerspd + 3
IF mann < 1 THEN mann = 1
IF mann > maxman THEN mann = maxman
mplayerspd(mann) = mplayerspd(mann) + 3
FOR xy = 200 TO 1000 STEP 100
SOUND xy, .1
NEXT
RETURN

FUNCTION Attacknumber (jtype$, life, etype$, elife, atk) STATIC
ktype$ = RIGHT$(jtype$, LEN(jtype$) - 1)
ftype$ = RIGHT$(etype$, LEN(etype$) - 1)
IF ktype$ = "man" THEN syo = 30
IF ktype$ = "skirm" THEN syo = 35
IF ktype$ = "arch" THEN syo = 25
IF ktype$ = "pult" THEN syo = 80
IF ktype$ = "ogre" THEN syo = 50
IF ktype$ = "man" AND ftype$ = "arch" THEN syo = 35
IF ktype$ = "man" AND ftype$ = "skirm" THEN syo = 35
IF ktype$ = "man" AND ftype$ = "pult" THEN syo = 50
IF ktype$ = "ogre" AND ftype$ = "arch" THEN syo = 40
IF ktype$ = "ogre" AND ftype$ = "skirm" THEN syo = 50
IF ktype$ = "ogre" AND ftype$ = "man" THEN syo = 85
IF ktype$ = "skirm" AND ftype$ = "arch" THEN syo = 20
IF ktype$ = "arch" AND ftype$ = "skirm" THEN syo = 30
syo = syo + life * 2
IF ftype$ = "skirm" THEN defence = 30
IF ftype$ = "ogre" THEN defence = 25
IF ftype$ = "pult" THEN defence = 10
IF ftype$ = "arch" THEN defence = 20
IF ftype$ = "man" AND ktype$ = "ogre" THEN defence = 30
IF ftype$ = "man" AND ktype$ = "skirm" THEN defence = 25
IF ftype$ = "man" AND ktype$ = "arch" THEN defence = 25
IF ftype$ = "man" AND ktype$ = "pult" THEN defence = 20
IF ftype$ = "ogre" AND ktype$ = "arch" THEN defence = 20
IF ftype$ = "ogre" AND ktype$ = "skirm" THEN defence = 20
IF ftype$ = "skirm" AND ktype$ = "arch" THEN defence = 10
IF ftype$ = "arch" AND ktype$ = "skirm" THEN defence = 10
syo = syo - defence
IF atk = 1 THEN syo = syo * 1.5
IF syo < 0 THEN syo = 0
Attacknumber = INT(syo / 15)
END FUNCTION

SUB bmp (filename$, x$)
CLS
OPEN filename$ FOR BINARY AS #1

header$ = SPACE$(14)
sizing$ = SPACE$(4)
GET #1, 1, header$
GET #1, 15, sizing$
bmpinfosize = CVI(sizing$)
IF bmpinfosize = 12 THEN
   infoheader$ = SPACE$(12)
   GET #1, 15, infoheader$
   nbits = CVI(MID$(infoheader$, 15, 4))
 
   IF nbits = 1 THEN
      palet$ = SPACE$(6)
      GET #1, bmpinfosize + 15, palet$
   ELSEIF nbits = 4 THEN
      palet$ = SPACE$(48)
      GET #1, bmpinfosize + 15, palet$
   ELSEIF nbits = 8 THEN
      palet$ = SPACE$(768)
      GET #1, bmpinfosize + 15, palet$
   END IF
ELSEIF bmpinfosize = 40 THEN
   infoheader$ = SPACE$(40)
   GET #1, 15, infoheader$
   nbits = CVI(MID$(infoheader$, 15, 4))
   IF nbits = 1 THEN
      palet$ = SPACE$(8)
      GET #1, bmpinfosize + 15, palet$
   ELSEIF nbits = 4 THEN
      palet$ = SPACE$(64)
      GET #1, bmpinfosize + 15, palet$
   ELSEIF nbits = 8 THEN
      palet$ = SPACE$(1024)
      GET #1, bmpinfosize + 15, palet$
   END IF
END IF
    

ft$ = MID$(header$, 1, 2)

filesize = CVL(MID$(header$, 3, 4))

r1 = CVI(MID$(header$, 7, 2))

r2 = CVI(MID$(header$, 9, 2))

offset = CVL(MID$(header$, 11, 4))


headersize = CVL(MID$(infoheader$, 1, 4))

picwidth = CVL(MID$(infoheader$, 5, 4))

picheight = CVL(MID$(infoheader$, 9, 4))

nplanes = CVI(MID$(infoheader$, 13, 4))


IF headersize = 40 THEN
   comptype = CVL(MID$(infoheader$, 17, 4))
 
   imagesize = CVL(MID$(infoheader$, 21, 4))
 
   xsize = CVL(MID$(infoheader$, 25, 4))
 
   ysize = CVL(MID$(infoheader$, 29, 4))
 
   colorsused = CVL(MID$(infoheader$, 33, 4))
 
   neededcolours = CVL(MID$(infoheader$, 37, 4))
END IF

IF nbits = 1 THEN
   SCREEN 11
ELSEIF nbits = 4 THEN
   SCREEN 12
ELSEIF nbits = 8 OR nbits = 24 THEN
   SCREEN 13
END IF
LOCATE 1, 40: PRINT "Loading Palette..."
IF bmpinfosize = 40 THEN ngroups = 4
IF bmpinfosize = 12 THEN ngroups = 3

IF nbits = 24 THEN
   IF ngroups = 3 THEN
      FOR c = 0 TO 63
         d = c * 4
         palet$ = palet$ + CHR$(d) + CHR$(d) + CHR$(d)
         palet$ = palet$ + CHR$(d) + CHR$(d) + CHR$(d + 1)
         palet$ = palet$ + CHR$(d) + CHR$(d + 1) + CHR$(d)
         palet$ = palet$ + CHR$(d + 1) + CHR$(d) + CHR$(d)
      NEXT c
   ELSEIF ngroups = 4 THEN
      FOR c = 0 TO 63
         d = c * 4
         palet$ = palet$ + CHR$(d) + CHR$(d) + CHR$(d) + CHR$(0)
         palet$ = palet$ + CHR$(d) + CHR$(d) + CHR$(d + 1) + CHR$(0)
         palet$ = palet$ + CHR$(d) + CHR$(d + 1) + CHR$(d) + CHR$(0)
         palet$ = palet$ + CHR$(d + 1) + CHR$(d) + CHR$(d) + CHR$(0)
      NEXT c
   END IF
END IF

FOR x = 1 TO LEN(palet$) STEP ngroups
   zb# = INT((ASC(MID$(palet$, x, 1))) / 4)
   zg# = INT((ASC(MID$(palet$, x + 1, 1))) / 4)
   zr# = INT((ASC(MID$(palet$, x + 2, 1))) / 4)
   zc# = zb# * 65536# + zg# * 256# + zr#
   cres = ASC(MID$(palet$, x + 3, 1))
   PALETTE ((x - 1) / ngroups), zc#
NEXT x

IF nbits = 24 THEN
   y = picheight - 1
   x = 0
   dat$ = "   "
   WHILE y >= 0
      WHILE x < picwidth
         GET 1, , dat$
         p1 = INT((ASC(MID$(dat$, 1, 1)) + ASC(MID$(dat$, 1, 1)) + ASC(MID$(dat$, 1, 1))) / 3)
         x = x + 1
      WEND
      y = y - 1
      x = 0
   WEND
ELSEIF nbits = 8 THEN
   y = picheight - 1
   x = 0
   dat$ = " "
   WHILE y >= 0
      WHILE x < picwidth
         GET 1, , dat$
         x = x + 1
      WEND
      y = y - 1
      x = 0
   WEND
ELSEIF nbits = 4 THEN
   y = picheight - 1
   x = 0
   dat$ = " "
   WHILE y >= 0
      WHILE x < picwidth
        GET 1, , dat$
        LOCATE 1, 1
        p1 = ASC(dat$) AND 15
        p2 = ASC(dat$) AND 240 / 16
        x = x + 2
      WEND
      y = y - 1
      x = 0
   WEND
ELSEIF nbits = 1 THEN
   y = picheight - 1
   x = 0
   dat$ = " "
   WHILE y >= 0
      WHILE x < picwidth
        GET 1, , dat$
        p1 = ASC(dat$)
        FOR p = 0 TO 7
        NEXT p
        x = x + 8
      WEND
      y = y - 1
      x = 0
   WEND
END IF

CLOSE

END SUB

SUB drawbox (x, y, x2, y2)
mousehide
p = 0
LINE (x, y)-(x2, y2), 0, BF
FOR colorx = 70 TO 78 STEP 2
p = p + 1
LINE (x - p, y - p)-(x2 + p, y2 + p), colorx, B
NEXT
p = 0
mouseshow
END SUB

SUB getbutton
DO
mousestatus lb%, rb%, xmouse%, ymouse%
IF lb% = -1 THEN EXIT DO
LOOP UNTIL INKEY$ <> ""
hotmail:
mousestatus lb%, rb%, xmouse%, ymouse%
IF lb% = -1 THEN GOTO hotmail
END SUB

SUB initimages
DEF SEG = VARSEG(grass(1))
BLOAD "image2.img", VARPTR(grass(1))
DEF SEG = VARSEG(man1(1))
BLOAD "footman.img", VARPTR(man1(1))
DEF SEG = VARSEG(man2(1))
BLOAD "foot2.img", VARPTR(man2(1))
DEF SEG = VARSEG(ogre(1))
BLOAD "ogre.img", VARPTR(ogre(1))
DEF SEG = VARSEG(ogre2(1))
BLOAD "ogre2.img", VARPTR(ogre2(1))
DEF SEG = VARSEG(arch(1))
BLOAD "archer.img", VARPTR(arch(1))
DEF SEG = VARSEG(arch2(1))
BLOAD "archer2.img", VARPTR(arch2(1))
DEF SEG = VARSEG(pult(1))
BLOAD "catapult.img", VARPTR(pult(1))
DEF SEG = VARSEG(pult2(1))
BLOAD "cat2.img", VARPTR(pult2(1))
DEF SEG = VARSEG(skirm1(1))
BLOAD "skirmish.img", VARPTR(skirm1(1))
DEF SEG = VARSEG(skirm2(1))
BLOAD "skirm2.img", VARPTR(skirm2(1))
DEF SEG = VARSEG(barracks(1))
BLOAD "tree.img", VARPTR(barracks(1))
IF terrain$ = "night" OR terrain$ = "snow" OR terrain$ = "dry" THEN
DEF SEG = VARSEG(barracks(1))
BLOAD "tree2.img", VARPTR(barracks(1))
END IF
DEF SEG = VARSEG(rock(1))
BLOAD "rock.img", VARPTR(rock(1))
IF level = 3 OR level = 6 THEN
DEF SEG = VARSEG(rock(1))
BLOAD "house.img", VARPTR(rock(1))
END IF
IF level = 9 OR level = 8 THEN
DEF SEG = VARSEG(rock(1))
BLOAD "fort.img", VARPTR(rock(1))
END IF
IF level = 4 THEN
DEF SEG = VARSEG(rock(1))
BLOAD "wall.img", VARPTR(rock(1))
END IF
END SUB

SUB MouseDriver (AX%, bx%, CX%, DX%)
  DEF SEG = VARSEG(mouse$)
  mouse% = SADD(mouse$)
  CALL Absolute(AX%, bx%, CX%, DX%, mouse%)

END SUB

SUB mousehide
 AX% = 2
 MouseDriver AX%, 0, 0, 0

END SUB

FUNCTION MouseInit%
  AX% = 0
  MouseDriver AX%, 0, 0, 0
  MouseInit% = AX%

END FUNCTION

SUB MousePut
  AX% = 4
  CX% = x%
  DX% = y%
  MouseDriver AX%, 0, CX%, DX%

END SUB

SUB mouseshow
  AX% = 1
  MouseDriver AX%, 0, 0, 0

END SUB

SUB mousestatus (lb%, rb%, xmouse%, ymouse%)
  AX% = 3
  MouseDriver AX%, bx%, CX%, DX%
  lb% = ((bx% AND 1) <> 0)
  rb% = ((bx% AND 2) <> 0)
  xmouse% = CX%
  ymouse% = DX%

END SUB

SUB openpic (filename$)
OPEN filename$ FOR INPUT AS #1
FOR x = 1 TO 20
FOR y = 1 TO 30
INPUT #1, j
PSET (x, y), j
NEXT y
NEXT x
CLOSE #1

END SUB

SUB PUTimage (xp, yp, omap$)
IF xp + 1 < xboundmin THEN GOTO minkuso341
IF xp - 1 > xboundmax THEN GOTO minkuso341
IF yp + 1 < yboundmin THEN GOTO minkuso341
IF yp - 1 > yboundmax THEN GOTO minkuso341
x = (xp - 1 - xboundmin) * 20 + 1
y = (yp - 1 - yboundmin) * 30 + 1
IF y < 1 THEN GOTO minkuso3222
IF x < 1 THEN GOTO minkuso3222
IF x > 300 THEN GOTO minkuso3222
IF y > 171 THEN GOTO minkuso3222
IF omap$ = "rock" THEN PUT (x, y), rock, PSET
IF omap$ = "farm" THEN PUT (x, y), barracks, PSET
IF omap$ = "grass" THEN PUT (x, y), grass, PSET
IF omap$ = "2man" THEN PUT (x, y), man2, PSET
IF omap$ = "1man" THEN PUT (x, y), man1, PSET
IF omap$ = "2arch" THEN PUT (x, y), arch2, PSET
IF omap$ = "1arch" THEN PUT (x, y), arch, PSET
IF omap$ = "1ogre" THEN PUT (x, y), ogre, PSET
IF omap$ = "1pult" THEN PUT (x, y), pult, PSET
IF omap$ = "2ogre" THEN PUT (x, y), ogre2, PSET
IF omap$ = "2pult" THEN PUT (x, y), pult2, PSET
IF omap$ = "2skirm" THEN PUT (x, y), skirm2, PSET
IF omap$ = "1skirm" THEN PUT (x, y), skirm1, PSET
minkuso3222:
minkuso341:
END SUB

FUNCTION showname$ (type$)
IF type$ = "1man" THEN showname$ = "Calvary   ": EXIT FUNCTION
IF type$ = "2man" THEN showname$ = "Swordman  ": EXIT FUNCTION
IF type$ = "1ogre" THEN showname$ = "Ogre      ": EXIT FUNCTION
IF type$ = "2ogre" THEN showname$ = "Ogre      ": EXIT FUNCTION
IF type$ = "1pult" THEN showname$ = "Catapult  ": EXIT FUNCTION
IF type$ = "2pult" THEN showname$ = "Ballista  ": EXIT FUNCTION
IF type$ = "1arch" THEN showname$ = "Elv Archer": EXIT FUNCTION
IF type$ = "2arch" THEN showname$ = "Quivery   ": EXIT FUNCTION
IF type$ = "1skirm" THEN showname$ = "Skirmisher": EXIT FUNCTION
IF type$ = "2skirm" THEN showname$ = "Skirmisher": EXIT FUNCTION
END FUNCTION

