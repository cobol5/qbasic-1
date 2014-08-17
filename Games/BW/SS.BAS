DECLARE SUB Restart ()
DECLARE SUB Opt ()
'A.S.S.H.O.L.E - The Barney Wars v.(insert positive number
'less than 1 here) prerelease by Matt Bross
'E-mail:oh_bother@GeoCities.Com
'HomePage:http://www.GeoCities.Com/SoHo/7067/
'This is level one of my little game and it hasn't been really optimized or
'commented or anything yet, whatever.  Distribute freely but remember,
'Try to sell it and I'll sue your A.S.S. off!  Enjoy!

'Things to come:
'   -better music
'   -better hit detection (so background doesn't smear)
'   -power-ups
'   -more levels
'   -more enemies
'   -more sound effects
'   -fast, readable, debugged code!

DEFINT A-Z
DECLARE FUNCTION EnHit% ()
DECLARE SUB Explode ()
DECLARE SUB Story ()
DECLARE SUB Info ()
DECLARE SUB Menu ()
DECLARE SUB Delay (sec!)
DECLARE SUB PalLoad (FileName$)
DECLARE SUB Title ()
DECLARE SUB LoadIns ()
DECLARE SUB Scale (sx%, sy%, srx%, sry%, ScaleX%, ScaleY%, LocX%, LocY%)
DECLARE SUB Font (text$, tx!, ty%, sc%, ec%, slant!, Space%)
DECLARE SUB MakeMask (MaskX1%, MaskY1%, MaskX2%, MaskY2%)
DECLARE SUB PlayNote (note$, octave%, hold!, j)
DECLARE SUB MasterVolume (Right%, Left%, Getvol%)
DECLARE SUB WavPlay (File$)
DECLARE SUB SetReg (Reg%, Value%)
DECLARE SUB WriteDSP (byte%)
DECLARE SUB DMAPlay (Segment&, Offset&, Length&, Freq&)
DECLARE SUB SpeakerState (OnOff%)
DECLARE FUNCTION ResetDSP% ()
DECLARE SUB GetBLASTER (DMA%, BasePort%, Irq%)
DECLARE SUB MouseIn ()
DECLARE FUNCTION Dead ()
DECLARE SUB MoveShip ()
DECLARE SUB MoveEnemies ()
DECLARE SUB ShootIt ()
DECLARE SUB GetKey ()
DECLARE SUB Stars ()
DECLARE SUB Init ()
DECLARE SUB LoadImg ()
DECLARE SUB CustomPal ()
DECLARE SUB RestorePal ()
DECLARE SUB ReadPal ()
DECLARE SUB SetRGB (ColToChange%, r%, G%, B%)
DECLARE SUB Mouse (cx%, dx%, bx%)
DECLARE SUB MousePointer (SW%)
DECLARE SUB Dot (x%, y%, Value%)
CONST BaseAddr = &H220 'Change if your sound card uses another base address
CONST RegAddr = BaseAddr + 8, DataAddr = BaseAddr + 9
CONST TRUE = -1, FALSE = NOT TRUE

SHELL "intro.exe"

'common variables
DIM SHARED NumStar, Shoot, x, y, sx, sy, mx, my, score
DIM SHARED ex, ey, esx, esy, lives
DIM SHARED UseMouse, UseKeyboard, k$
DIM SHARED BasePort%, LenPort%, channel%, t!, sel
'================================variables===================================
NumStar = 100
lives = 9
'HaveStars = TRUE
UseMouse = TRUE
'UseKeyboard = TRUE
'============================================================================
TYPE StarType
        x AS INTEGER
        y AS INTEGER
        c AS INTEGER
END TYPE

SCREEN 13: RANDOMIZE TIMER
Font "", 0, 0, 0, 0, 0, 0 'init font sub
Title 'load title screen

'$DYNAMIC
DIM SHARED spr(16651 / 2)'for sprites
DIM SHARED Shot(106), ShotMask(106), ShotBkg(106)
DIM SHARED ship1(613), Ship1Mask(613), ShipBkg(613)
DIM SHARED Flame1(15), Flame1Mask(15)
DIM SHARED Flame2(19), Flame2Mask(19)
DIM SHARED enemy1(482), Enemy1Mask(482), Enemy1Bkg(482)
DIM SHARED Boom(393), BoomMask(393), BoomBkg(393)
DIM SHARED Stats(766) 'Hold stats so background doesn't erase it
DIM SHARED ExplodeBkg(3770)
DIM SHARED micheal(901)
DIM SHARED Start(474), StartMask(474), StartBkg(1074)
DIM SHARED boss1(1000), Boss1Mask(1000), Boss1Bkg(1000)

DIM SHARED a(9) 'for mouse
DIM SHARED Star(NumStar) AS StarType 'for stars
DIM SHARED WavBuffer(0)  AS STRING * 17138 'make Buffer for sound (.wav's)
DIM SHARED Clr AS STRING * 1 'make string for loading files (i.e. font)
'******************************Mouse Stuff***********************************
DEF SEG = VARSEG(a(0))
    FOR I = 0 TO 17
       READ r
       POKE VARPTR(a(0)) + I, r
    NEXT I
DEF SEG
DATA &HB8,&H00,&H00
DATA &H55
DATA &H8B,&HEC
DATA &HCD,&H33
DATA &H92
DATA &H8B,&H5E,&H06
DATA &H89,&H07
DATA &H5D
DATA &HCA,&H02,&H00

CALL MousePointer(0)
CALL MousePointer(3)

'*********************************Sound Stuff********************************
CLS
GetBLASTER channel%, BasePort%, Irq% 'Parses BLASTER environment
IF ResetDSP% THEN PRINT 'resets DSP (returns true if sucessful)

SpeakerState 1 'turn speaker on
MasterVolume Right%, Left%, -1 'this puts the mixer volumn in Right% and Left%
MasterVolume 15, 15, 0 '15, 15, 0 cranks the volumn all the way up.

FOR I = 0 TO 244
  SetReg I, 0 'Clear all registers
NEXT
LoadIns
'********************************start***************************************
Menu
IF lives = -1 THEN GOTO ShutDown
LoadImg
Init
PalLoad "game.pal"
PUT (0, 150), micheal, PSET
FOR c = 111 TO 130
r = r + 3
  SetRGB c, r, 0, 0
NEXT
SetRGB 0, 0, 0, 0
Restart
'+++++++++++++++++++++++++++++++++main loop++++++++++++++++++++++++++++++++++
DO
  PUT (0, 0), Stats, PSET
  LINE (0, 200)-(320, 197), 0, BF
IF HaveStars THEN Stars
IF UseKeyboard THEN GetKey
IF UseMouse THEN MouseIn
IF Shoot >= 0 THEN ShootIt
  IF TIMER >= t! THEN
  j = 0: SetReg &HB0 + j, 0                   'BAD music
  j = 2: SetReg &HB0 + j, 0
    READ oct1, n1$, oct2, n2$, dur!           'someone please write
        j = 0                                 'me better music!
      PlayNote n1$, oct1, dur!, j             'this routine is increbily
        j = 2                                 'unorganized right now
      PlayNote n2$, oct2, dur!, j
    NumNotes = NumNotes + 1
  END IF
IF NumNotes > 20 THEN RESTORE music: NumNotes = 0
MoveShip
MoveEnemies
IF lives <= 0 THEN GOTO ShutDown
'++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
LOOP UNTIL k$ = CHR$(27)
ShutDown:
  FOR j = 0 TO 2
    SetReg &HB0 + j, 0 'Switch voices off
  NEXT j
  CALL MousePointer(2): SpeakerState 0
  SCREEN 0, 0, 0, 0: WIDTH 80, 25
  PRINT FRE(0); FRE(-1); FRE(-2)
  DEF SEG : CLOSE : END

music:
DATA 5,A,2,C,.25
DATA 5,B,2,C,.25
DATA 5,C,2,C,.25
DATA 5,B,2,C,.25
DATA 5,A,2,E,.25
DATA 5,B,2,C,.25
DATA 5,A,2,C,.25
DATA 5,G,2,C,.25

DATA 5,A,2,C,.25
DATA 5,B,2,C,.25
DATA 5,C,2,C,.25
DATA 5,B,2,C,.25
DATA 5,A,2,E,.25
DATA 5,B,2,C,.25
DATA 5,A,2,C,.25
DATA 5,G,2,C,.25

DATA 5,A,2,C,.5
DATA 5,G,2,C,.5
DATA 5,A,2,A,.25
DATA 5,G,2,C,.25
DATA 5,0,2,0,.5

REM $STATIC
FUNCTION Dead
'****************************Check if You are dead***************************
IF ex + esx > x AND ey + esy > y AND ey < y + 33 AND ex < x + 35 THEN
  lives = lives - 1
    PUT (ex, ey), Enemy1Bkg
    PUT (x, y), ShipBkg, PSET
      LOCATE 2, 1: PRINT "Lives Left:"; lives
      GET (0, 0)-(101, 14), Stats
    WavPlay "dead"
    Explode
  x = 160: y = 100
  GET (x, y)-(x + 35, y + 33), ShipBkg
  Dead = -1
  ex = 0: ey = 0
  GET (0, 0)-(28, 29), Enemy1Bkg
Restart
ELSE
  Dead = 0
END IF
END FUNCTION

SUB Delay (sec!)
   DEF SEG = 0
   d& = FIX(sec! * 18.20444444#)
   FOR t& = t& TO d&
     d% = PEEK(&H46C) AND 255
     WHILE d% = (PEEK(&H46C) AND 255): WEND
   NEXT
DEF SEG
'sec! = sec! + TIMER
'DO: LOOP UNTIL sec! < TIMER
END SUB

SUB DMAPlay (Segment&, Offset&, Length&, Freq&)
' Transfers and plays the contents of the buffer.
Length& = Length& - 1
Page% = 0
MemLoc& = Segment& * 16 + Offset&
       channel% = 1
       PgPort% = &H83
       AddPort% = &H2
       LenPort% = &H3
       ModeReg% = &H49
OUT &HA, &H4 + channel%
OUT &HC, &H0
OUT &HB, ModeReg%
OUT AddPort%, MemLoc& AND &HFF
OUT AddPort%, (MemLoc& AND &HFFFF&) \ &H100
IF (MemLoc& AND 65536) THEN Page% = Page% + 1
IF (MemLoc& AND 131072) THEN Page% = Page% + 2
IF (MemLoc& AND 262144) THEN Page% = Page% + 4
IF (MemLoc& AND 524288) THEN Page% = Page% + 8
OUT PgPort%, Page%
OUT LenPort%, Length& AND &HFF
OUT LenPort%, (Length& AND &HFFFF&) \ &H100
OUT &HA, channel%

IF Freq& < 23000 THEN
   TimeConst% = 256 - 1000000 \ Freq&
   WriteDSP &H40
   WriteDSP TimeConst%
   WriteDSP &H14
   WriteDSP (Length& AND &HFF)
   WriteDSP ((Length& AND &HFFFF&) \ &H100)
ELSE
   IF DSPVersion! >= 3 THEN
      TimeConst% = ((65536 - 256000000 \ Freq&) AND &HFFFF&) \ &H100
      WriteDSP &H40
      WriteDSP TimeConst%
      WriteDSP (Length& AND &HFF)
      WriteDSP ((Length& AND &HFFFF&) \ &H100)
      WriteDSP &H91
   ELSE
      EXIT SUB
   END IF
END IF
END SUB

SUB Dot (x%, y%, c%)
DEF SEG = &HA000
  POKE y * 320& + x, c
DEF SEG
END SUB

FUNCTION EnHit
IF sx < ex + esx AND sx > ex AND sy < ey + esx AND sy > ey THEN
  enemy = 3: score = score + 1
  WavPlay "boom"
  LOCATE 1, 1: PRINT "Score:"; score: GET (0, 0)-(101, 14), Stats
  EnHit = -1
ELSE
  EnHit = 0
END IF

END FUNCTION

SUB Explode
PUT (x, y), ShipBkg, PSET
PUT (ex, ey), Enemy1Bkg, PSET
Explodex1 = x - 31: ExplodeY1 = y - 16
ExplodeX2 = x + 65: ExplodeY2 = y + 60
IF Explodex1 < 0 THEN Explodex1 = 0
IF ExplodeY1 < 0 THEN ExplodeY1 = 0
IF ExplodeX2 > 319 THEN ExplodeX2 = 319
IF ExplodeY2 > 199 THEN ExplodeY2 = 199
GET (Explodex1, ExplodeY1)-(ExplodeX2, ExplodeY2), ExplodeBkg
DO
  epx = INT(RND * 35) + x: epy = INT(RND * 33) + y
  r = 31: xp = 10: c = 111
    DO
      r = r - 1
      c = c + 1
      xp = xp * .8
      CIRCLE (epx, xp + epy), r, c, , , 1 / 2
      PAINT (epx, xp + epy), c
    LOOP UNTIL r = 16
  FOR r = 16 TO 0 STEP -1
    c = c - 1: IF c < 0 THEN c = 0
    CIRCLE (x, xp + y), r, c, , , 1 / 2
  NEXT
  t = t + 1
LOOP UNTIL t >= 6
PUT (Explodex1, ExplodeY1), ExplodeBkg, PSET
END SUB

SUB Font (text$, tx!, ty%, sc%, ec%, slant!, Space%)
IF text$ = "" THEN
  OPEN "font.dat" FOR BINARY AS #1
END IF

IF sc = ec THEN cc! = 0: GOTO travel
IF ec > sc THEN cc! = (ec - sc) / 8 ELSE cc! = (ec - sc) / 8

travel:
FOR t = 1 TO LEN(text$)
c! = sc
  chr = ASC(MID$(text$, t, 1))
  l = (chr * 64) - (33 * 64)
    IF chr = 33 THEN l = l + 1
    IF chr = 32 THEN GOTO pass
   SEEK #1, l
      FOR q = ty TO (ty + 7)
      FOR s = tx! TO (tx! + 7)
        GET #1, , Clr
        IF Clr = "?" THEN Dot s, q, CINT(c!)
      NEXT
        IF slant! <> 0 THEN tx! = tx! + slant!
        IF cc! <> 0 THEN c! = c! + cc!
      NEXT
pass:
      tx! = tx! + Space
NEXT
text$ = ""
END SUB

SUB GetBLASTER (DMA%, BasePort%, Irq%)
' This subroutine parses the BLASTER environment string and returns settings.
IF LEN(ENVIRON$("BLASTER")) = 0 THEN PRINT "BLASTER environment variable NOT set.": EXIT SUB
FOR Length% = 1 TO LEN(ENVIRON$("BLASTER"))
   SELECT CASE MID$(ENVIRON$("BLASTER"), Length%, 1)
      CASE "A"
        BasePort% = VAL("&H" + MID$(ENVIRON$("BLASTER"), Length% + 1, 3))
      CASE "I"
        Irq% = VAL(MID$(ENVIRON$("BLASTER"), Length% + 1, 1))
      CASE "D"
        DMA% = VAL(MID$(ENVIRON$("BLASTER"), Length% + 1, 1))
   END SELECT
NEXT
END SUB

SUB GetKey
'*********************************get key************************************
  k$ = INKEY$
  k$ = UCASE$(k$)
  SELECT CASE k$
        CASE "4"
    mx = mx - .5
        CASE "6"
    mx = mx + .5
        CASE "2"
    my = my + .5
        CASE "8"
    my = my - .5
        CASE " "
    IF Shoot = 0 THEN Shoot = 1
        CASE CHR$(27)
    lives = -1
  END SELECT
END SUB

SUB Info
CLS

Font "DISCLAIMER!", 100, 0, 32, 40, -.5, 15
Font "This game has little, if any", 20, 20, 16, 31, 0, 10
Font "taste and should only be played", 0, 30, 16, 31, 0, 10
Font "by resposible consending people.", 0, 40, 16, 31, 0, 10
Font "Distrubute this freely as long", 0, 50, 16, 31, 0, 10
Font "as it is not sold or altered.", 0, 60, 16, 31, 0, 10
Font "Once you have received this, you", 0, 70, 16, 31, 0, 10
Font "may have access to any of the", 0, 80, 16, 31, 0, 10
Font "subs as long as the author", 0, 90, 16, 31, 0, 10
Font "recieves credit.  I, Matt Bross", 0, 100, 16, 31, 0, 10
Font "wrote everything and modified or", 0, 110, 16, 31, 0, 10
Font "copied the following:", 0, 120, 16, 31, 0, 10
Font "DMAPlay by Mike Huff", 20, 130, 32, 40, 0, 10
Font "MKFont by M/K Productions", 20, 140, 32, 40, 0, 10
Font "QBDelay by Kurt Kuzba", 20, 150, 32, 40, 0, 10
Font "QMouse", 20, 160, 32, 40, 0, 10

Font "Press A Key", 40, 190, 32, 40, 0, 10
SLEEP: CLS

Font "Barney is a trademark of (insert", 0, 0, 16, 31, 0, 10
Font "name of Barney producers here if", 0, 10, 16, 31, 0, 10
Font "you know it, in which case, you ", 0, 20, 16, 31, 0, 10
Font "have", 0, 30, 16, 31, 0, 10
Font "WAY", 50, 30, 32, 40, -.5, 15
Font "too much free time). ", 90, 30, 16, 31, 0, 10
Font "This is a Parody of Barney's", 0, 40, 16, 31, 0, 10
Font "(insert everything that you find", 0, 50, 16, 31, 0, 10
Font "annoying about him here) and I", 0, 60, 16, 31, 0, 10
Font "retain my right as an American", 0, 70, 16, 31, 0, 10
Font "to make fun of and destroy ", 0, 80, 16, 31, 0, 10
Font "the image of anything I want.", 0, 90, 16, 31, 0, 10
Font "The sounds I'm are still", 0, 100, 16, 31, 0, 10
Font "property of their owners.", 0, 110, 16, 31, 0, 10
Font "I got them off of the internet", 0, 120, 16, 31, 0, 10
Font "off sites that shouldn't have", 0, 130, 16, 31, 0, 10
Font "anything illegal on them so I", 0, 140, 16, 31, 0, 10
Font "assume it is safe to use them.", 0, 150, 16, 31, 0, 10

Font "Press A Key", 40, 190, 32, 40, 0, 10
SLEEP: CLS

Font "Either use the NUM pad or", 0, 0, 16, 31, 0, 10
Font "the mouse to move your", 0, 10, 16, 31, 0, 10
Font "ship around.  Press", 0, 20, 16, 31, 0, 10
Font "SPACE or the left", 0, 30, 16, 31, 0, 10
Font "mouse button to fire.", 0, 40, 16, 31, 0, 10

Font "Press A Key", 40, 190, 32, 40, 0, 10
SLEEP: CLS

sel = 1
END SUB

SUB Init
'********************************init stars**********************************
  FOR t = 0 TO NumStar
    Star(t).x = INT(RND * 320)
    Star(t).y = INT(RND * 200)
      rc = INT(RND * 3) + 1
   IF rc = 1 THEN rc = 24
   IF rc = 2 THEN rc = 25
   IF rc = 3 THEN rc = 26
    Star(t).c = rc
  NEXT
'*******************************load background******************************
lev = 0
REDIM LevBkg(32001)
DEF SEG = VARSEG(LevBkg(0))
  BLOAD "bkg" + RIGHT$(STR$(lev), 1) + ".bkg", VARPTR(LevBkg(0))
DEF SEG
PUT (0, 0), LevBkg, PSET
ERASE LevBkg
'**********************************setup*************************************
x = 160: y = 100
sx = x + 13: sy = y - 20
ex = INT(RND * 290): ey = INT(RND * 50)
GET (sx, sy)-(sx + 9, sy + 20), ShotBkg
GET (x, y)-(x + 35, y + 33), ShipBkg
GET (ex, ey)-(ex + 29, ey + 28), Enemy1Bkg
COLOR 23
PRINT "Score:"; score
PRINT "Lives left:"; lives
GET (0, 0)-(101, 14), Stats
END SUB

SUB LoadImg
'***************************load images into arrays**************************
CLS

FOR c = 0 TO 255
'  SetRGB c, 0, 0, 0
NEXT

DEF SEG = VARSEG(spr(0))
  BLOAD "imgs.img", VARPTR(spr(0))
DEF SEG

PUT (0, 0), spr
GET (1, 1)-(35, 33), ship1
GET (38, 1)-(67, 30), enemy1
GET (69, 1)-(78, 21), Shot
GET (80, 1)-(119, 45), micheal
'GET (121, 1)-(206, 78), boss2
GET (38, 32)-(44, 35), Flame1
GET (69, 23)-(75, 27), Flame2
'GET (69, 29)-(78, 49), acid
GET (1, 37)-(63, 51), Start
GET (1, 53)-(100, 65), boss1
MakeMask 1, 1, 35, 33
  GET (1, 1)-(35, 33), Ship1Mask
MakeMask 38, 1, 67, 30
  GET (38, 1)-(67, 30), Enemy1Mask
MakeMask 69, 1, 78, 21
  GET (69, 1)-(78, 21), ShotMask
'MakeMask 121, 1, 206, 78
  'GET (121, 1)-(206, 78), boss2
'MakeMask 69, 29, 78, 49
  'GET (69, 29)-(78, 49), acid
MakeMask 1, 37, 63, 51
  GET (1, 37)-(63, 51), StartMask
MakeMask 1, 53, 100, 65
  GET (1, 53)-(100, 65), Boss1Mask
CLS
REDIM spr(739 / 2)
DEF SEG = VARSEG(spr(0))
  BLOAD "boom.img", VARPTR(spr(0))
DEF SEG

PUT (0, 0), spr, PSET

GET (0, 0)-(28, 26), Boom
MakeMask 0, 0, 28, 26
  GET (0, 0)-(28, 26), BoomMask
CLS
END SUB

SUB LoadIns
'InsFile = FREEFILE
'OPEN ins$ + ".ins" FOR BINARY AS InsFile
'FOR n = 0 TO 7
'GET #InsFile, , first
'GET #InsFile, , Second
'SetReg First, Second
'NEXT
'CLOSE #InsFile

SetReg &H20, &H1 'Plays carrier note at specified octave ch. 1
SetReg &H23, &H1 'Plays modulator note at specified octave ch. 1
SetReg &H40, &H1F 'Set carrier total level to softest ch. 1
SetReg &H43, &H0 'Set modulator level to loudest ch. 1
SetReg &H60, &HE4 'Set carrier attack and decay ch. 1
SetReg &H63, &HE4 'Set modulator attack and decay ch. 1
SetReg &H80, &H9D 'Set carrier sustain and release ch. 1
SetReg &H83, &H9D 'Set modulator sustain and release ch. 1
END SUB

SUB MakeMask (MaskX1, MaskY1, MaskX2, MaskY2)

FOR y = MaskY1 TO MaskY2
FOR x = MaskX1 TO MaskX2
  IF POINT(x, y) = 0 THEN PSET (x, y), 255 ELSE PSET (x, y), 0
NEXT: NEXT
END SUB

SUB MasterVolume (Right%, Left%, Getvol%)
OUT BasePort% + 4, &H22
IF Getvol% THEN
   Left% = INP(BasePort% + 5) \ 16
   Right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (Right% + Left% * 16) AND &HFF
END IF
END SUB

SUB Menu

r = 20
FOR c = 16 TO 32
  SetRGB c, r, r, r
  r = r + 3
NEXT
r = 30
FOR c = 32 TO 40
  SetRGB c, 0, 0, r
  r = r + 2
NEXT
r = 20
FOR c = 41 TO 50
  SetRGB c, r, r, 0
  r = r + 3
NEXT

SetRGB 0, 0, 0, 0

CLS : sel = 1
DO
IF sel = 1 THEN
  Font "Start Game", 100, 20, 16, 31, 0, 10
  Font "Options", 100, 40, 32, 40, 0, 10
  Font "Info", 100, 60, 32, 40, 0, 10
  Font "Story", 100, 80, 32, 40, 0, 10
  Font "Exit", 100, 100, 32, 40, 0, 10
ELSEIF sel = 2 THEN
  Font "Start Game", 100, 20, 32, 40, 0, 10
  Font "Options", 100, 40, 16, 31, 0, 10
  Font "Info", 100, 60, 32, 40, 0, 10
ELSEIF sel = 3 THEN
  Font "Options", 100, 40, 32, 40, 0, 10
  Font "Info", 100, 60, 16, 31, 0, 10
  Font "Story", 100, 80, 32, 40, 0, 10
ELSEIF sel = 4 THEN
  Font "Info", 100, 60, 32, 40, 0, 10
  Font "Story", 100, 80, 16, 31, 0, 10
  Font "Exit", 100, 100, 32, 40, 0, 10
ELSEIF sel = 5 THEN
  Font "Start Game", 100, 20, 32, 40, 0, 10
  Font "Story", 100, 80, 32, 40, 0, 10
  Font "Exit", 100, 100, 16, 31, 0, 10
END IF
 
  DO
    k$ = INKEY$
    k$ = UCASE$(k$)
  LOOP UNTIL k$ <> ""

SELECT CASE k$
  CASE "2"
    sel = sel + 1
  CASE "8"
    sel = sel - 1
  CASE CHR$(13)
    IF sel = 1 THEN EXIT SUB
    IF sel = 2 THEN Opt
    IF sel = 3 THEN Info
    IF sel = 4 THEN Story
    IF sel = 5 THEN lives = -1: EXIT SUB
END SELECT

IF sel < 1 THEN sel = 5
IF sel > 5 THEN sel = 1
LOOP
END SUB

SUB Mouse (cx, dx, bx)
DEF SEG = VARSEG(a(0))
  POKE VARPTR(a(4)), &H92
    CALL ABSOLUTE(cx, VARPTR(a(0)))
  POKE VARPTR(a(4)), &H91
    CALL ABSOLUTE(dx, VARPTR(a(0)))
  POKE VARPTR(a(4)), &H93
    CALL ABSOLUTE(bx, VARPTR(a(0)))
DEF SEG
END SUB

SUB MouseIn
'*******************************get mouse input******************************
CALL Mouse(cx, dx, bx)
  IF cx > 160 THEN my = my + 1
  IF cx < 160 THEN my = my - 1
  IF dx > 100 THEN mx = mx + 1
  IF dx < 100 THEN mx = mx - 1
    IF bx = 1 THEN
      IF Shoot = 0 THEN Shoot = 1
    END IF
  IF bx = 2 THEN lives = -1
END SUB

SUB MousePointer (SW)
DEF SEG = VARSEG(a(0))
  POKE VARPTR(a(0)) + 1, SW
    CALL ABSOLUTE(c, VARPTR(a(0)))
DEF SEG
END SUB

SUB MoveEnemies STATIC
'**********************************enemies***********************************
IF enemy = 0 THEN
  NumEnemy& = NumEnemy& + 1
  PUT (ex, ey), Enemy1Bkg, PSET
   ex = INT(RND * 290): ey = INT(RND * 50): ExtraX = INT(RND * 10) - 5
   esx = 29: esy = 28
   enemy = 1
  IF NOT Dead THEN GET (ex, ey)-(ex + 29, ey + 29), Enemy1Bkg
END IF

IF NumEnemy& = 10 THEN
  ey = 0: ex = 80: esx = 100: esy = 16
  enemy = 4: ExtraX = 1
  GET (ex, ey)-(ex + 100, ey + 12), Boss1Mask
  NumEnemy& = NumEnemy& + 1
END IF

IF enemy = 1 THEN
  er = er + 1
  WhX = COS(er / 10)
  WhY = SIN(er / 10)
    WAIT &H3DA, 8
     PUT (ex, ey), Enemy1Bkg, PSET
    ey = ey + 1 + WhX * 5
    ex = ex + WhY * 5 + ExtraX
  IF ex < 0 THEN ex = 0
  IF ex > 290 THEN ex = 290
  IF ey > 170 THEN ey = 170: enemy = 0: er = 0
  IF ey < 0 THEN ey = 0
    IF NOT Dead THEN
      GET (ex, ey)-(ex + 29, ey + 29), Enemy1Bkg
      PUT (ex, ey), Enemy1Mask, AND
      PUT (ex, ey), enemy1, OR
    END IF
END IF

IF sx < ex + esx AND sx > ex AND sy < ey + esx AND sy > ey THEN
  enemy = 3: score = score + 1
  WavPlay "boom"
  LOCATE 1, 1: PRINT "Score:"; score: GET (0, 0)-(101, 14), Stats
END IF

IF EnemyB = 1 THEN
  WAIT &H3DA, 8
    PUT (oex, oey), BoomBkg, PSET
    PUT (oex, oey), BoomMask, AND: PUT (oex, oey), Boom, OR
  IF enemy = 3 THEN PUT (oex, oey), BoomBkg, PSET: EnemyB = 0
  IF TIMER >= B! THEN EnemyB = 0: PUT (oex, oey), BoomBkg, PSET
END IF

IF enemy = 3 THEN
  oex = ex: oey = ey
  enemy = 0
  EnemyB = 1: B! = TIMER + 1
    PUT (ex, ey), Enemy1Bkg, PSET
    PUT (sx, sy), ShotBkg, PSET
    GET (oex, oey)-(oex + 28, oey + 26), BoomBkg
END IF


IF enemy = 4 THEN
IF ex < 0 THEN ex = 0
IF ex > 220 THEN ex = 220
PUT (ex, ey), Boss1Mask, PSET
ex = ex + ExtraX
IF ex < 0 THEN ExtraX = 1
IF ex > 220 THEN ExtraX = -1
GET (ex, ey)-(ex + 100, ey + 12), Boss1Mask
PUT (ex, ey), Boss1Mask, AND
PUT (ex, ey), boss1, OR
END IF

END SUB

SUB MoveShip STATIC
'**********************set movement boundries for ship***********************
IF my > 3 THEN my = 3
IF mx > 3 THEN mx = 3
IF my < -3 THEN my = -3
IF mx < -3 THEN mx = -3
  WAIT &H3DA, 8
  PUT (x, y), ShipBkg, PSET
    x = x + mx: y = y + my
  IF x < 0 THEN x = 0
  IF x > 280 THEN x = 280
  IF y < 0 THEN y = 0
  IF y > 165 THEN y = 165
'*******************************ship animation*******************************
  GET (x, y)-(x + 35, y + 33), ShipBkg
  PUT (x, y), Ship1Mask, AND
  PUT (x, y), ship1, OR

'flame animation
IF my < 0 THEN
FlameFrame = FlameFrame + 1
  IF FlameFrame <= 5 THEN
    PUT (x + 9, y + 29), Flame1, PSET
    PUT (x + 20, y + 29), Flame1, PSET
  END IF

  IF FlameFrame >= 5 THEN
     PUT (x + 9, y + 29), Flame2, PSET
     PUT (x + 20, y + 29), Flame2, PSET
  END IF
END IF
IF FlameFrame > 10 THEN FlameFrame = 0
END SUB

SUB Opt
CLS
Font "Scene:", 40, 6, 16, 31, 0, 10
Font "Sound:", 40, 16, 16, 31, 0, 10
Font "BasePort Sound Address:", 40, 26, 16, 31, 0, 10
Font "Exit", 40, 36, 16, 31, 0, 10
x = 11
scene = 1
sdfx = 1

DO
LINE (100, 6)-(160, 20), 0, BF

IF scene = 1 THEN
  Font "Earth", 100, 6, 32, 40, 0, 10
ELSE
  Font "Space", 100, 6, 32, 40, 0, 10
END IF

IF sdfx = 1 THEN
  Font "Yes", 100, 15, 32, 40, 0, 10
ELSE
  Font "No", 100, 15, 32, 40, 0, 10
END IF

SELECT CASE k$
  CASE "8"
    x = x - 10
  CASE "2"
    x = x + 10
  CASE CHR$(13)
    IF x = 11 THEN scene = -scene
    IF x = 21 THEN sdfx = -sdfx
    IF x = 31 THEN LOCATE 4, 35: INPUT bas
    IF x = 41 THEN sel = 1: CLS : EXIT SUB
  END SELECT
IF x > 41 THEN x = 11
IF x < 11 THEN x = 41

LINE (0, 0)-(30, 55), 0, BF
CIRCLE (15, x), 5, 1
PAINT (15, x), 4, 1
DO: k$ = INKEY$: LOOP UNTIL k$ <> ""
LOOP
END SUB

SUB PalLoad (FileName$)
OPEN FileName$ FOR BINARY AS #2
 FOR Att% = 0 TO 255
  OUT &H3C8, Att%
   FOR RedGreenAndBlue% = 1 TO 3
    GET #2, , Clr
    Valew% = ASC(Clr)
    OUT &H3C9, Valew%
   NEXT RedGreenAndBlue%
 NEXT Att%
CLOSE #2
END SUB

SUB PlayNote (note$, octave, hold!, j)
note$ = UCASE$(note$)
    SELECT CASE note$
    CASE "C#"
      SetReg &HA0 + j, &H6B 'Set note number
      SetReg &HB0 + j, &H21 + 4 * octave 'Set octave and turn on voice
    CASE "D"
      SetReg &HA0 + j, &H81
      SetReg &HB0 + j, &H21 + 4 * octave
    CASE "D#"
      SetReg &HA0 + j, &H98
      SetReg &HB0 + j, &H21 + 4 * octave
    CASE "E"
      SetReg &HA0 + j, &HB0
      SetReg &HB0 + j, &H21 + 4 * octave
    CASE "F"
      SetReg &HA0 + j, &HCA
      SetReg &HB0 + j, &H21 + 4 * octave
    CASE "F#"
      SetReg &HA0 + j, &HE5
      SetReg &HB0 + j, &H21 + 4 * octave
    CASE "G"
      SetReg &HA0 + j, &H2
      SetReg &HB0 + j, &H22 + 4 * octave
    CASE "G#"
      SetReg &HA0 + j, &H20
      SetReg &HB0 + j, &H22 + 4 * octave
    CASE "A"
      SetReg &HA0 + j, &H41
      SetReg &HB0 + j, &H22 + 4 * octave
    CASE "A#"
      SetReg &HA0 + j, &H63
      SetReg &HB0 + j, &H22 + 4 * octave
    CASE "B"
      SetReg &HA0 + j, &H87
      SetReg &HB0 + j, &H22 + 4 * octave
    CASE "C"
      SetReg &HA0 + j, &HAE
      SetReg &HB0 + j, &H22 + 4 * octave
    END SELECT
t! = TIMER + hold!
END SUB

FUNCTION ResetDSP%
' Resets the DSP
OUT BasePort% + 6, 1
FOR Count% = 1 TO 4
   junk% = INP(BasePort% + 6)
NEXT
OUT BasePort% + 6, 0
IF INP(BasePort% + 14) AND &H80 = &H80 AND INP(BasePort% + 10) = &HAA THEN
   ResetDSP% = -1
ELSE
   ResetDSP% = 0
END IF
END FUNCTION

SUB Restart
GET (130, 90)-(206, 104), StartBkg
PUT (130, 90), StartMask, AND
PUT (130, 90), Start, OR
SLEEP
PUT (130, 90), StartBkg, PSET
END SUB

SUB Scale (sx, sy, srx, sry, ScaleX, ScaleY, LocX, LocY)
REDIM array(sx TO sx + srx, sy TO sy + sry)

FOR y3 = sy TO sy + sry
FOR x3 = sx TO sx + srx
  array(x3, y3) = POINT(x3, y3)
NEXT: NEXT

FOR y3 = sy TO sy + sry
FOR x3 = sx TO sx + srx
    x1 = (x3 * ScaleX) + LocX
    y1 = (y3 * ScaleY) + LocY
    x2 = (x3 * ScaleX) + ScaleX + LocX
    y2 = (y3 * ScaleY) + ScaleY + LocY
      LINE (x1, y1)-(x2, y2), array(x3, y3), BF
NEXT: NEXT
ERASE array
END SUB

SUB SetReg (Reg, Value)
  OUT RegAddr, Reg
  OUT DataAddr, Value
END SUB

SUB SetRGB (ColToChange%, r%, G%, B%)
OUT &H3C8, ColToChange%
OUT &H3C9, r%
OUT &H3C9, G%
OUT &H3C9, B%
END SUB

SUB ShootIt
IF Shoot = 0 THEN sx = -10: sy = -10
IF Shoot = 1 THEN
        sx = x + 13: sy = y - 20
        IF sy < 0 THEN sy = 0
        Shoot = 2
        my = 1
        GET (sx, sy)-(sx + 9, sy + 20), ShotBkg
        WavPlay "Laser"
END IF
IF Shoot = 2 THEN
        IF sy < 0 THEN sy = 0
        IF sx < 0 THEN sx = 0
        IF sy > 180 THEN sy = 180
        IF sx > 290 THEN sx = 290
                PUT (sx, sy), ShotBkg, PSET
                sy = sy - 5: IF sy < 0 THEN sy = 0
                GET (sx, sy)-(sx + 9, sy + 20), ShotBkg
        END IF
                IF sy = 0 THEN
                        Shoot = 0
                        PUT (sx, sy), ShotBkg, PSET: EXIT SUB
                END IF
IF Shoot = 2 THEN
  PUT (sx, sy), ShotMask%, AND
  PUT (sx, sy), Shot%, OR
END IF
END SUB

SUB SpeakerState (OnOff%)
' Turns speaker on or off.
IF OnOff% THEN WriteDSP &HD1 ELSE WriteDSP &HD3
END SUB

SUB Stars
'*******************************star scroller********************************
FOR t = 0 TO NumStar
    IF Star(t).c = 24 THEN d = 3
    IF Star(t).c = 25 THEN d = 2
    IF Star(t).c = 26 THEN d = 1
      Dot Star(t).x, Star(t).y - d, 0
      Dot Star(t).x, Star(t).y, Star(t).c
      Star(t).y = Star(t).y + d
        IF Star(t).y >= 200 THEN
          Dot Star(t).x, 200, 0
            Star(t).y = 0
            Star(t).x = INT(RND * 320)
        END IF
   NEXT
END SUB

SUB Story

CLS
Font "Dear Player,", 0, 0, 16, 31, 0, 10
Font "Hi!  It's Micheal, Barney's", 15, 10, 16, 31, 0, 10
Font "Friend.  Guess what!  I", 0, 20, 16, 31, 0, 10
Font "finally grew some balls and", 0, 30, 16, 31, 0, 10
Font "became an A.S.S.  That's right!", 0, 40, 16, 31, 0, 10
Font "An Annoying Space Shooter.", 0, 50, 16, 31, 0, 10
Font "Now Barney's gone completely", 0, 60, 16, 31, 0, 10
Font "evil and is no longer trying", 0, 70, 16, 31, 0, 10
Font "take over the world by", 0, 80, 16, 31, 0, 10
Font "brainwashing our young and has", 0, 90, 16, 31, 0, 10
Font "launched a full scale attack on", 0, 100, 16, 31, 0, 10
Font "the world!  I have to stop him!", 0, 110, 16, 31, 0, 10
Font "Love,", 108, 120, 16, 31, 0, 10
Font "Gay Boy", 108, 130, 16, 31, 0, 10
Font "Oops, I mean Micheal", 108, 140, 16, 31, 0, 10

Font "Press A Key", 0, 190, 32, 40, 0, 10
SLEEP: sel = 1: CLS
END SUB

SUB Title
REDIM ScnBuf(32001)
REDIM bm1(6936)
REDIM bm2(6029)
REDIM bm3(5167)
REDIM bm4(2899)
REDIM bm5(1293)
REDIM bm6(321)

FOR c = 0 TO 255
  SetRGB c, 0, 0, 0
NEXT

DEF SEG = VARSEG(ScnBuf(0))
  BLOAD "title.img", VARPTR(ScnBuf(0))
DEF SEG

PUT (0, 0), ScnBuf, PSET
  LINE (80, 10)-(100, 70), 0, BF
    GET (0, 0)-(94, 145), bm1
PUT (0, 0), ScnBuf, PSET
  LINE (80, 80)-(94, 100), 0, BF
  LINE (170, 0)-(180, 70), 0, BF
    GET (88, 0)-(175, 136), bm2
PUT (0, 0), ScnBuf, PSET
    GET (171, 0)-(252, 125), bm3
PUT (0, 0), ScnBuf, PSET
    GET (31, 159)-(50, 190), bm6
    GET (192, 134)-(232, 196), bm5
  LINE (237, 104)-(240, 130), 0, BF
    GET (237, 104)-(297, 198), bm4
CLS
PalLoad "title.pal"
SetRGB 0, 0, 0, 0

r = 30
FOR c = 126 TO 134
  SetRGB c, r, 0, 0
  r = r + 3

NEXT
r = 30
FOR c = 135 TO 143
  SetRGB c, 0, 0, r
  r = r + 3
NEXT
r = 10
FOR c = 144 TO 162
  SetRGB c, r, r, r
  r = r + 3
NEXT
r = 30
FOR c = 163 TO 171
  SetRGB c, r, 0, r
  r = r + 3
NEXT

Font "The", 0, 0, 144, 162, 0, 10
  Font "A", 0, 10, 126, 134, 0, 10
  Scale 0, 8, 8, 8, 2, 2, 0, -5
    LINE (0, 10)-(20, 14), 0, BF
    Font "nnoying", 20, 20, 135, 143, -.5, 12
  Font "S", 0, 30, 126, 134, 0, 10
  Scale 0, 30, 8, 8, 2, 2, 0, -25
    LINE (0, 30)-(20, 34), 0, BF
    Font "pace ", 20, 40, 135, 143, -.5, 12
  Font "S", 0, 50, 126, 134, 0, 10
  Scale 0, 50, 8, 8, 2, 2, 0, -45
    LINE (0, 50)-(20, 54), 0, BF
    Font "hooter's", 20, 60, 135, 143, -.5, 12
  Font "H", 0, 70, 126, 134, 0, 10
  Scale 0, 70, 8, 8, 2, 2, 0, -65
    LINE (0, 70)-(20, 74), 0, BF
    Font "uge", 20, 80, 135, 143, -.5, 12
  Font "O", 0, 90, 126, 134, 0, 10
  Scale 0, 90, 8, 8, 2, 2, 0, -85
    LINE (0, 90)-(20, 94), 0, BF
    Font "rganization of", 20, 100, 135, 143, -.5, 12
  Font "L", 0, 110, 126, 134, 0, 10
  Scale 0, 110, 8, 8, 2, 2, 0, -105
    LINE (0, 110)-(20, 114), 0, BF
    Font "arge", 20, 120, 135, 143, -.5, 12
  Font "E", 0, 130, 126, 134, 0, 10
  Scale 0, 130, 8, 8, 2, 2, 0, -125
    LINE (0, 130)-(20, 134), 0, BF
    Font "nemies ", 20, 140, 135, 143, -.5, 12

Font "The", 220, 150, 144, 162, 0, 10
Font "Barney Wars", 180, 170, 163, 172, 0, 10

x = -1
DO
FOR y = 0 TO 100
  LINE (250, y - 1)-(270, y - 1), 0, BF
  PUT (250, y), bm6, PSET
NEXT
  Delay .5
PUT (240, 70), bm5, PSET
  Delay .1
PUT (230, 40), bm4, PSET
  Delay .1
PUT (217, 10), bm3, PSET
  LINE (210, 80)-(222, 100), 0, BF
  LINE (290, 110)-(310, 150), 0, BF
  Delay .1
PUT (213, 5), bm2, PSET
  Delay .1
PUT (210, 0), bm1, PSET
  Delay 1
  LINE (210, 0)-(320, 145), 0, BF
PUT (213, 5), bm2, PSET
  Delay .1
  LINE (210, 0)-(320, 145), 0, BF
PUT (217, 10), bm3, PSET
  LINE (210, 80)-(222, 100), 0, BF
  LINE (290, 110)-(310, 150), 0, BF
  Delay .1
  LINE (210, 0)-(320, 145), 0, BF
PUT (230, 40), bm4, PSET
  Delay .1
  LINE (210, 0)-(320, 145), 0, BF
PUT (240, 70), bm5, PSET
  Delay .1
  LINE (210, 0)-(320, 145), 0, BF
FOR y = 100 TO 0 STEP -1
  IF r >= 63 THEN x = -x
  IF r < 1 THEN x = -x
  r = r + x
  SetRGB 0, r, 0, r
  PUT (250 + SIN(y), y), bm6, PSET
  LINE (250 + SIN(y), y)-(250 + SIN(y) + 20, y + 40), 0, B
NEXT
  Delay 2
LOOP UNTIL INKEY$ <> ""
ERASE bm1, bm2, bm3, bm4, bm5, bm6, ScnBuf
CLS : PalLoad "default.pal"
END SUB

SUB WavPlay (File$)
WavFile = FREEFILE
File$ = File$ + ".wav"
OPEN File$ FOR BINARY AS #WavFile
GET #WavFile, 44, WavBuffer(0) 'get 32K and skip header
Length& = LOF(1) - 44
IF Length& > 32767 THEN Length& = 32767 'adjust length if needed
Freq& = 2930
IF File$ = "dead.wav" THEN Freq& = 100
DMAPlay VARSEG(WavBuffer(0)), VARPTR(WavBuffer(0)), Length&, Freq&
CLOSE #WavFile
END SUB

SUB WriteDSP (byte%)
' Writes a byte to the DSP
DO
LOOP WHILE INP(BasePort% + 12) AND &H80
OUT BasePort% + 12, byte%
END SUB

