'3D-Engine for QBasic/QB-Compiler (C)2000 Didi Marfurt
'"""""""""""""""""""""""""""""""""""""""""""""""""""""

'Postcardware, enjoy!
'

'soundblaster declaring...
DECLARE FUNCTION SpeakerStatus% ()
DECLARE FUNCTION DMAStatus% ()
DECLARE FUNCTION DMADone% ()
DECLARE FUNCTION ResetDSP% ()
DECLARE SUB MasterVolume (Right%, Left%, Getvol%)
DECLARE SUB WriteDSP (byte%)
DECLARE SUB SetStereo (OnOff%)
DECLARE FUNCTION ReadDSP% ()
DECLARE SUB SpeakerState (OnOff%)
DECLARE SUB DMAState (StopGo%)
DECLARE SUB DMAPlay (Segment&, Offset&, Length&, Freq&)
DECLARE SUB GetBLASTER (DMA%, BasePort%, IRQ%)
DECLARE FUNCTION DSPVersion! ()
'
DECLARE FUNCTION MouseInit% ()
DECLARE SUB MouseStatus (Lb%, Rb%, xMouse%, yMouse%)
DECLARE SUB MouseRange (X1%, Y1%, x2%, Y2%)
DECLARE SUB MousePut (x%, y%)
DECLARE SUB MouseHide ()
DECLARE SUB MouseDriver (ax%, bx%, cx%, dx%)
DECLARE SUB MouseShow ()
COMMON SHARED BasePort%, LenPort%, Channel%

'init mouse
DEFINT A-Z: DEF SEG = &HA000:
DIM SHARED mouse$: mouse$ = SPACE$(57)
'CLS
'SCREEN 12
FOR i% = 1 TO 57:  READ a$:  h$ = CHR$(VAL("&H" + a$))
MID$(mouse$, i%, 1) = h$: NEXT i%
DATA 55,89,E5,8B,5E,0C,8B,07,50,8B,5E,0A,8B,07,50,8B
DATA 5E,08,8B,0F,8B,5E,06,8B,17,5B,58,1E,07,CD,33,53
DATA 8B,5E,0C,89,07,58,8B,5E,0A,89,07,8B,5E,08,89,0F
DATA 8B,5E,06,89,17,5D,CA,08,00

RESTORE
ms% = MouseInit%
IF NOT ms% THEN
  PRINT "Mouse not found"
  INPUT "press enter"; ok$
  END
END IF
'MouseShow
MouseRange 0, 0, 630, 199
MousePut 320, 100

'init blaster
DIM SHARED karte%
GetBLASTER Channel%, BasePort%, IRQ% ' Parses BLASTER environment
IF karte% = 1 THEN
SpeakerState 1 'turn the speaker on
 'PRINT "Sound Card DSP version:"; DSPVersion!
 Right% = 3: Left% = 3
 MasterVolume Right%, Left%, 0 '0 setzen, -1 abfragen
DIM WavBuffer1(1 TO 1) AS STRING * 5200 'Make a 32k buffer for file.
DIM WavBuffer2(1 TO 1) AS STRING * 5200 'Make a 32k buffer for file.
Filename$ = "shot1.wav"
OPEN Filename$ FOR BINARY AS #1
GET #1, 44, WavBuffer1(1)
CLOSE #1
Filename$ = "shot2.wav"
OPEN Filename$ FOR BINARY AS #1
GET #1, 44, WavBuffer2(1)
CLOSE #1
Length& = 5200
Freq& = 5500
END IF'if blaster present

' end init dmaplay
'-----------------------------------------------------
' load joystick-defaults
OPEN "boostick.dat" FOR INPUT AS #1
INPUT #1, scala$
CLOSE 1#
scala% = VAL(scala$)
'init gifload
DEFINT A-Z
RANDOMIZE TIMER
 
REDIM SHARED Prefix(4095), Suffix(4095), OutStack(4095), ShiftOut(8)
DIM Ybase AS LONG, Powersof2(11) AS LONG, WorkCode AS LONG
'eo gif init

gifname$ = "boostd.gif"
wichplt% = 0

REDIM SHARED plt1%(2, 255), plt2%(2, 255), aehn%(255)

GOSUB fadeout
SCREEN 13
GOSUB fadeout
GOSUB gifload
REDIM SHARED hand2&(404)
REDIM SHARED hand1&(300)
REDIM SHARED hand2m&(404)
REDIM SHARED hand1m&(300)
maske% = POINT(0, 190)
yellow% = POINT(128, 145)
GET (0, 185)-(66, 199), hand1&(0)
GET (67, 176)-(132, 199), hand2&(0)
FOR j% = 176 TO 199
FOR i% = 0 TO 132
IF POINT(i%, j%) = maske% THEN
 PSET (i%, j%), 255
ELSE
 PSET (i%, j%), 0
END IF
NEXT i%
NEXT j%
GET (0, 185)-(66, 199), hand1m&(0)
GET (67, 176)-(132, 199), hand2m&(0)

PUT (0, 185), hand1&(0), PSET
PUT (67, 176), hand2&(0), PSET
FOR j% = 176 TO 199
FOR i% = 0 TO 132
IF POINT(i%, j%) = maske% THEN
 PSET (i%, j%), 0
ELSE
 'PSET (i%, j%), 0
END IF
NEXT i%
NEXT j%
GET (0, 185)-(66, 199), hand1&(0)
GET (67, 176)-(132, 199), hand2&(0)
 
REDIM SHARED schw%(200)
 
'greyscales...

FOR i% = 0 TO 255
r% = plt1%(0, i%)
g% = plt1%(1, i%)
B% = plt1%(2, i%)
IF (r% >= g% - 0 AND r% <= g% < 8) AND (r% >= B% - 4 AND r% <= B% + 4) THEN
 schw%((r% + g% + B%) / 3) = i%
END IF
IF g% > 60 AND r% < 40 AND B% < 40 AND i% <> maske% THEN
 green% = i%
END IF
IF g% < B% AND r% < B% AND B% > 50 AND i% <> maske% THEN
 sky% = i%
END IF
NEXT i%
' ...sortiert nach schw%()
FOR i% = 1 TO 63
 IF schw%(i%) = 0 THEN
  schw%(i%) = schw%(i% - 1)
 END IF
NEXT i%
FOR i% = 64 TO 200
 schw%(i%) = schw%(63)
NEXT i%


'dim textures
REDIM SHARED tx10%(63, 63), tx11%(63, 63), tx12%(63, 63), tx13%(63, 63), tx14%(63, 63)
REDIM SHARED tx15%(63, 63), tx16%(63, 63), tx17%(63, 63), tx18%(63, 63), tx19%(63, 63)
REDIM SHARED tx20%(63, 63), tx21%(63, 63), tx22%(63, 63), tx23%(63, 63), tx24%(63, 63) ' , tx25%(63, 63)
REDIM SHARED tx3floor%(63, 63), tx4floor%(63, 63), tx5floor%(63, 63), tx6floor%(63, 63)
REDIM SHARED txmerk1%(3, 3)
REDIM SHARED tx1%(63, 63), tx2%(63, 63), tx3%(63, 63), tx1floor%(63, 63), tx2floor%(63, 63)
REDIM SHARED tx4%(127, 127), tx5%(63, 63), tx5bunker%(32, 63), tx6%(63, 63), tx7%(63, 63), tx8%(63, 63), tx9%(63, 63)
REDIM SHARED tx1dark%(63, 63), tx2dark%(63, 63), tx3dark%(63, 63)
GOSUB texture ' textur in 64*64-int-felder einlesen
 
GOSUB cockpit
' rem if no further gifload(e.g. bonus) then this 32kB can be realloced:
' REDIM SHARED Prefix(0), Suffix(0), OutStack(0), ShiftOut(0)

'vga offscreen init-----------------------------------

REDIM SHARED fe&(8740)
GET (0, 50)-(319, 151), fe&(0)
fe0& = fe&(0)
fe1& = fe&(1)
feseg% = VARSEG(fe&(0))
fes1% = ASC(LEFT$(MKI$(feseg%), 1))
fes2% = ASC(RIGHT$(MKI$(feseg%), 1))
ha0001% = ASC(LEFT$(MKI$(&HA000), 1))
ha0002% = ASC(RIGHT$(MKI$(&HA000), 1))
REDIM SHARED m&(100)
aref% = 0: arefseg% = VARSEG(aref%)
DEF SEG = arefseg%
FOR i& = 0 TO 63998 'check ram for "$a000"
 a1% = PEEK(i&)
 a2% = PEEK(i& + 1)
 a1$ = CHR$(a1%)
 a2$ = CHR$(a2%)
 a% = CVI(CHR$(a1%) + CHR$(a2%))
IF a% = &HA000 THEN
 m&(mz%) = i&
 mz% = mz% + 1
ww1% = PEEK(i& + 2)
ww2% = PEEK(i& + 3)
ww3% = PEEK(i& + 4)
ww4% = PEEK(i& + 5)
ww5% = PEEK(i& + 6)
IF ww1% = 0 AND ww2% = 0 AND ww3% = 0 AND ww4% = 0 AND ww5% = 0 THEN
einer% = mz% - 1 ' got adress of QB-Grafics-Output (used to redirect later)
EXIT FOR
END IF
END IF
a$ = INKEY$
NEXT i&
IF i& > 63000 THEN
END
END IF
'ende vga offscreen init

'enemy movement
REDIM SHARED fxwf%(3), fywf%(3)
fxwf%(0) = 0
fywf%(0) = 1
fxwf%(1) = 1
fywf%(1) = 0
fxwf%(2) = 0
fywf%(2) = -1
fxwf%(3) = -1
fywf%(3) = 0

'recursiv rememberers
REDIM SHARED xintmerk%(50)
REDIM SHARED yintmerk%(50)
REDIM SHARED gridmerk%(50)

' table improve speed(tis)
REDIM SHARED l2250%(512)
FOR i& = 1 TO 512
l2250%(i&) = (2250 / i&)
NEXT i&
'caster mask
REDIM SHARED spritecolumn%(199)
'tis
REDIM SHARED l320&(199)
FOR i& = 0 TO 199
l320&(i&) = ((i& - 50) * 320) + 4
NEXT i&
'tis
REDIM SHARED l320r&(199)
FOR i& = 0 TO 199
l320r&(i&) = (((200 - i&) - 50) * 320) + 4
NEXT i&
'tis
REDIM SHARED h64i%(1200)
FOR i% = 1 TO 1200
h64i%(i%) = (63 / i%) * 500
NEXT i%
'tis headcam-buffer (press s to make a snapshot)
REDIM SHARED camx%(63), camy%(16 TO 47)
 FOR i% = 0 TO 63
  camx%(i%) = 20 + (i% * 4.2)
 NEXT i%
 FOR j% = 16 TO 47
  camy%(j%) = 50 + ((j% - 16) * 3.124)
 NEXT j%
 

CONST UpArrow = -72, DnArrow = -80, LArrow = -75, RArrow = -77
RANDOMIZE TIMER
DIM Grid%(-1 TO 13, -1 TO 26)' Level Map !!!
DIM Grid2%(-1 TO 13, -1 TO 26)' Floor Map !!!
DIM Grid3%(-1 TO 13, -1 TO 26)' Roof Map !!!
GOSUB gload 'load Grid
GOSUB wichlevel ' handle last-level-infofile
' mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm init player etc.
'
REDIM SHARED STable!(0 - 191 TO (360 * 5) + 192), CTable!(0 - 191 TO (360 * 5) + 192)
REDIM SHARED iSTable%(0 - 191 TO (360 * 5) + 192), iCTable%(0 - 191 TO (360 * 5) + 192)
REDIM SHARED iStable2%(0 - 191 TO (360 * 5) + 192), iCtable2%(0 - 191 TO (360 * 5) + 192)
IF PX& = 0 THEN
 PX& = 39181: PY& = 116826 'the starting coords of player if not in .MAP file Game
END IF
'stride% = 1' 10 '3   'doesent seem to be used anymore
                     'by pressing the up or down arrow keys
'turn% = 90 '25       'number of degrees of rotation produced by
                     'pressing the right or left arrow keys
dark% = 10'shade value walls 1,2,3
fwz2% = 1'turn dir spider  ?
'enemy init,bonus
friendx% = 3
friendy% = 3
DIM friendx%(50)
DIM friendy%(50)
DIM boxx%(50)
DIM boxy%(50)
'DIM friendmotion%(50)
DIM friendboden%(50)
DIM friendhealth%(50)
friendx%(0) = 3
friendy%(0) = 3
FOR y% = 1 TO 24
  FOR x% = 1 TO 12
    'READ Grid%(x%, y%)
    IF Grid%(x%, y%) = 4 AND friendzell% < 50 THEN
      friendx%(friendzell%) = x%
      friendy%(friendzell%) = y%
      friendzell% = friendzell% + 1
    END IF
    IF Grid%(x%, y%) = 7 AND boxes% < 50 THEN
      boxx%(boxes%) = x%
      boxy%(boxes%) = y%
      boxes% = boxes% + 1
    END IF
   NEXT
NEXT
IF friendzell% > 0 THEN friends% = friendzell% - 1
IF boxes% > 0 THEN boxes% = boxes% - 1
DIM box$(boxes% + 1)
GOSUB bonusload' load bonus-event info

DIM fxw%(50), fwz%(50), fyw%(ii%), fwz2%(50)' what the..
 
FOR ii% = 0 TO 50
 fwz2%(ii%) = 1
 fxw%(ii%) = 1
 fyw%(ii%) = 0
NEXT ii%

'sic cos tables + shift to ints
Factor! = (ATN(1) * 8) / (360)
FOR a% = 0 TO 1799
  Angle! = CSNG((a% / 5)) * Factor!
  STable!(a%) = SIN(Angle!) * .05
  CTable!(a%) = COS(Angle!) * .05
NEXT
FOR a% = -191 TO -1
  STable!(a%) = STable!(a% + (360 * 5))
  CTable!(a%) = CTable!(a% + (360 * 5))
NEXT
FOR a% = (5 * 360) TO (5 * 360) + 192
  STable!(a%) = STable!(a% - (360 * 5))
  CTable!(a%) = CTable!(a% - (360 * 5))
NEXT
FOR a% = -191 TO 1992
  iSTable%(a%) = 10000 * STable!(a%)
  iCTable%(a%) = 10000 * CTable!(a%)
  iStable2%(a%) = 100000 * STable!(a%)
  iCtable2%(a%) = 100000 * CTable!(a%)
NEXT a%
REDIM SHARED STable!(0), CTable!(0)
nostick% = 1
bremse% = 1
t# = TIMER
DO 'mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm main loop
IF bremse% = 1 THEN t2# = TIMER + .03
ei% = 0
ein$ = INKEY$
IF mousekr% <> 0 THEN ein$ = " "
IF ein$ <> "" THEN
 IF LEN(ein$) = 2 THEN 'cursorkeys (very unfinished)
  ei% = ASC(RIGHT$(ein$, 1))
  IF ei% = 75 THEN
   turn% = 45
   heading% = (heading% + turn%) MOD 1800
   richtung% = ((heading% + 225) \ 450) AND 3
   IF (heading% > 112 AND heading% < 338) OR (heading% > 562 AND heading% < 788) OR (heading% > 1012 AND heading% < 1238) OR (heading% > 1462 AND heading% < 1688) THEN
    winkel45% = 0
   ELSE
    winkel45% = 1
   END IF
  END IF
  IF ei% = 77 THEN
   turn% = 45
   heading% = (heading% + (1800 - turn%)) MOD 1800
   richtung% = ((heading% + 225) \ 450) AND 3
   IF (heading% > 112 AND heading% < 338) OR (heading% > 562 AND heading% < 788) OR (heading% > 1012 AND heading% < 1238) OR (heading% > 1462 AND heading% < 1688) THEN
    winkel45% = 0
   ELSE
    winkel45% = 1
   END IF
  END IF
 IF ei% = 72 THEN
    newPX& = PX& - iSTable%(heading%) * 2
    newPY& = PY& - iCTable%(heading%) * 2
   GOSUB schritt
 END IF
 IF ei% = 80 THEN
    newPX& = PX& + iSTable%(heading%) * 2
    newPY& = PY& + iCTable%(heading%) * 2
    GOSUB schritt
 END IF

 END IF
IF ein$ = "B" THEN bremse% = bremse% XOR 1'toggle slow down to 18f/s
IF ein$ = "s" THEN
'snapshot view to wall3
FOR j% = 16 TO 47
 FOR i% = 0 TO 63
 tx3%(i%, j%) = POINT(camx%(i%), camy%(j%))
 NEXT i%
NEXT j%
END IF
IF ein$ = "F" THEN 'cheat enemy freeze
freeze% = freeze% XOR 1
END IF
IF ein$ = "E" OR ein$ = CHR$(27) THEN
 health% = 126
 GOSUB dieing
END IF
IF ein$ = "H" THEN ' home
 PX& = anfangx&
 PY& = anfangy&
 heading% = 900
END IF
IF ein$ = "K" THEN 'toggle joystick(default:mouse)
 nostick% = nostick% XOR 1
 mousekl% = 0
 mousekr% = 0
END IF
IF ein$ = "0" OR VAL(ein$) > 0 AND VAL(ein$) <= 5 THEN 'set mastervolume
 IF karte% = 1 THEN
  wielaut% = VAL(ein$) * 3
  MasterVolume wielaut%, wielaut%, 0
 END IF
END IF
IF ein$ = " " THEN  ' push door etc.
  nNewPX& = PX& - (iStable2%(heading%) * 2)'*1
  nNewPY& = PY& - (iCtable2%(heading%) * 2)
  druck% = Grid%(nNewPX& \ 10000, nNewPY& \ 10000)
  IF druck% = 5 THEN 'door
   IF tueraktion% = 0 THEN
    tueraktion% = 7
    tuer% = tuer% XOR 1
    tuermerkx% = nNewPX& \ 10000
    tuermerky% = nNewPY& \ 10000
   END IF
  END IF
  IF druck% = 7 THEN ' myst.box, bonus
  score% = score% + 50
  health% = health% - 30
  IF health% < 0 THEN health% = 0
  druckx% = nNewPX& \ 10000
  drucky% = nNewPY& \ 10000
    FOR wichbox% = 0 TO boxes%
     IF boxx%(wichbox%) = druckx% AND boxy%(wichbox%) = drucky% THEN
      IF box$(wichbox%) <> "nothing" THEN
       IF UCASE$(RIGHT$(box$(wichbox%), 4)) = ".GIF" THEN
        DEF SEG = &HA000
        BSAVE "screen.tmp", 0, 64000
        GOSUB fadeout
        gifname$ = box$(wichbox%)
        wichplt% = 1
        GOSUB gifload
        GOSUB fadeinplt2
        ee$ = ""
        WHILE ee$ = ""
         ee$ = INKEY$
        WEND
        GOSUB fadeoutplt2
        'display repaint!
        DEF SEG = &HA000
        BLOAD "screen.tmp", 0
        GOSUB fadein
       END IF
       IF UCASE$(RIGHT$(box$(wichbox%), 4)) = ".EXE" THEN
        DEF SEG = &HA000
        BSAVE "screen.tmp", 0, 64000
        GOSUB fadeout
        CLS
        SHELL box$(wichbox%)
        GOSUB pltblack
        DEF SEG = &HA000
        BLOAD "screen.tmp", 0
        GOSUB fadein
       END IF
      END IF
      Grid%(druckx%, drucky%) = 0
      EXIT FOR
     END IF
    NEXT wichbox%
  END IF
  IF druck% = 8 THEN 'level exit
   OPEN "booalive.dat" FOR OUTPUT AS #1
   CLOSE #1
   GOSUB resultofexit
   GOSUB fadeout
   GOSUB beenden
  END IF
 END IF
END IF

' door
IF tueraktion% > 0 THEN
 IF tuer% = 1 THEN ' open
  FOR jj% = 0 TO 63
  tx5%(31, jj%) = maske%
  tx5%(32, jj%) = maske%
   FOR fast% = 0 TO 3
   FOR ii% = 0 TO 30
   tx5%(ii%, jj%) = tx5%(ii% + 1, jj%)
   tx5%(63 - ii%, jj%) = tx5%((63 - ii%) - 1, jj%)
   NEXT ii%
   NEXT fast%
  NEXT jj%
 ELSE '(tuer%=0 'close)
 FOR jj% = 0 TO 63
  tx5%(0, jj%) = tx5bunker%(tueraktion% * 4, jj%)
  tx5%(63, jj%) = tx5bunker%(tueraktion% * 4, jj%)
  tx5%(1, jj%) = tx5bunker%(tueraktion% * 4 + 1, jj%)
  tx5%(62, jj%) = tx5bunker%(tueraktion% * 4 + 1, jj%)
  tx5%(2, jj%) = tx5bunker%(tueraktion% * 4 + 2, jj%)
  tx5%(61, jj%) = tx5bunker%(tueraktion% * 4 + 2, jj%)
  tx5%(3, jj%) = tx5bunker%(tueraktion% * 4 + 3, jj%)
  tx5%(60, jj%) = tx5bunker%(tueraktion% * 4 + 3, jj%)
  FOR fast% = 0 TO 3
   FOR ii% = 31 TO 1 STEP -1
   tx5%(ii%, jj%) = tx5%(ii% - 1, jj%)
   tx5%(63 - ii%, jj%) = tx5%((63 - ii%) + 1, jj%)
  NEXT ii%
  NEXT fast%
  NEXT jj%
  END IF
  tueraktion% = tueraktion% - 1
  IF tueraktion% = 0 THEN
    IF tuer% = 1 THEN 'door now completly open
     tuerwart% = 20
    ELSE
    END IF
  END IF
 END IF


 IF tuerwart% > 0 THEN
  IF Grid%(xgrid%, ygrid%) <> 5 THEN
   tuerwart% = tuerwart% - 1
   IF tuerwart% = 0 THEN 'time to close door
    tueraktion% = 7 '31 looks cool too!
    tuer% = 0
   END IF
  END IF
 END IF

' check joystick or mouse
IF nostick% = 0 THEN
 joyx% = STICK(0) - scala%
 joyy% = STICK(1) - scala%
 joyx% = joyx% + joyx% + joyx%
 joyy% = joyy% + joyy% + joyy%
ELSE ' mouse cntrl
 MouseStatus mousekl%, mousekr%, mousex%, mousey%
 joyx% = (mousex% - 320) \ 2
 joyy% = (mousey% - 100)
 stride! = .1 + (ABS(joyy%) \ 20)'20
 'LOCATE 4, 4: PRINT mousex%; " "; mousey%; " "; mousekr%; " ";
END IF
IF joyx% < -2 THEN
 turn% = ABS(joyx%) \ 6
 IF turn% > 0 THEN
  heading% = (heading% + turn%) MOD 1800
  richtung% = ((heading% + 225) \ 450) AND 3
  IF (heading% > 112 AND heading% < 338) OR (heading% > 562 AND heading% < 788) OR (heading% > 1012 AND heading% < 1238) OR (heading% > 1462 AND heading% < 1688) THEN
    winkel45% = 0
  ELSE
   winkel45% = 1
  END IF
 END IF
END IF
IF joyx% > 2 THEN
 turn% = ABS(joyx%) \ 6
 IF turn% > 0 THEN
  heading% = (heading% + (1800 - turn%)) MOD 1800
  richtung% = ((heading% + 225) \ 450) AND 3
  IF (heading% > 112 AND heading% < 338) OR (heading% > 562 AND heading% < 788) OR (heading% > 1012 AND heading% < 1238) OR (heading% > 1462 AND heading% < 1688) THEN
   winkel45% = 0
  ELSE
   winkel45% = 1
  END IF
 END IF
END IF
 
 IF joyy% < -5 THEN
    newPX& = PX& - iSTable%(heading%) * stride!
    newPY& = PY& - iCTable%(heading%) * stride!
   GOSUB schritt
 END IF
 IF joyy% > 5 THEN
    newPX& = PX& + iSTable%(heading%) * stride!
    newPY& = PY& + iCTable%(heading%) * stride!
    GOSUB schritt
 END IF

IF nostick% = 0 THEN firebutton% = STRIG(1) ' sound?
IF feuer% = 0 THEN
 IF firebutton% <> 0 OR ein$ = CHR$(13) OR mousekl% <> 0 THEN
  IF karte% = 1 THEN
  IF treffer% = 0 THEN
   DMAPlay VARSEG(WavBuffer1(1)), VARPTR(WavBuffer1(1)), Length&, Freq&
  ELSE
   DMAPlay VARSEG(WavBuffer2(1)), VARPTR(WavBuffer2(1)), Length&, Freq&
   treffer% = 0
  END IF
  END IF
  feuer% = 1
 END IF
ELSE
 feuer% = 0
END IF

'Spiders...
IF freeze% = 0 THEN
spri% = spri% + 1
IF spri% > 1 THEN 'movement every nth frame
 spri% = 0
woichbin% = Grid%(xgrid%, ygrid%)
Grid%(xgrid%, ygrid%) = 250
FOR ii% = 0 TO friends%
 IF (firebutton% <> 0 OR ein$ = CHR$(13) OR mousekl% <> 0) AND korn% = 1 AND friendboden%(ii%) = 0 THEN
  IF friendx%(ii%) = spidx% AND friendy%(ii%) = spidy% THEN
   'under fire
    friendhealth%(ii%) = friendhealth%(ii%) + 1
    'sound 100, .1
    treffer% = 1
    IF friendhealth%(ii%) > 10 THEN
     'spider dies
     Grid%(spidx%, spidy%) = 9
     raushier% = 0
     score% = score% + 10
     IF RND > .49 THEN 'rebirth spider
      FOR jjj% = 1 TO 25
       FOR iii% = 1 TO 12
        IF Grid%(iii%, jjj%) = 0 THEN
         Grid%(iii%, jjj%) = 4
         friendx%(ii%) = iii%
         friendy%(ii%) = jjj%
         friendhealth%(ii%) = 0
         raushier% = 1
         EXIT FOR
        END IF
       NEXT iii%
       IF raushier% = 1 THEN EXIT FOR
      NEXT jjj%
     ELSE
      FOR jjj% = 25 TO 1 STEP -1
       FOR iii% = 12 TO 1 STEP -1
        IF Grid%(iii%, jjj%) = 0 THEN
         Grid%(iii%, jjj%) = 4
         friendx%(ii%) = iii%
         friendy%(ii%) = jjj%
         friendhealth%(ii%) = 0
         raushier% = 1
         EXIT FOR
        END IF
       NEXT iii%
       IF raushier% = 1 THEN EXIT FOR
      NEXT jjj%
     END IF
    END IF
  END IF
 END IF
 dahin% = Grid%(friendx%(ii%) + fxw%(ii%), friendy%(ii%) + fyw%(ii%))
 IF (dahin% = 0 OR (dahin% = 5 AND tuer% = 1)) AND RND < .8 THEN
  Grid%(friendx%(ii%), friendy%(ii%)) = friendboden%(ii%)
  friendx%(ii%) = friendx%(ii%) + fxw%(ii%)
  friendy%(ii%) = friendy%(ii%) + fyw%(ii%)
  friendboden%(ii%) = Grid%(friendx%(ii%), friendy%(ii%))
  Grid%(friendx%(ii%), friendy%(ii%)) = 4
 ELSE
  IF dahin% <> 250 THEN
   fxw%(ii%) = fxwf%(fwz%(ii%))
   fyw%(ii%) = fywf%(fwz%(ii%))
   fwz%(ii%) = fwz%(ii%) + fwz2%(ii%)
   IF fwz%(ii%) > 3 OR fwz%(ii%) < 0 THEN
    fwz%(ii%) = 0
    IF RND > .49 THEN
     fwz2%(ii%) = -1
    ELSE
     fwz2%(ii%) = 1
    END IF
   END IF
  ELSE ' spider touchin YOU!
   health% = health% + 5
   IF health% > 125 THEN
    GOSUB dieing
   END IF
  END IF
 END IF
NEXT ii%
Grid%(xgrid%, ygrid%) = woichbin%
END IF
END IF

GOSUB ComputeView
scor2$ = "0000"
scor$ = STR$(score%)
scor$ = RIGHT$(scor$, LEN(scor$) - 1)
MID$(scor2$, 1 + LEN(scor2$) - LEN(scor$), LEN(scor$)) = scor$
COLOR yellow%: LOCATE 23, 10: PRINT scor2$; " ";
LOCATE 23, 27: PRINT 100 - health%; "% "; : COLOR 50
tz% = tz% + 1
IF tz% >= 30 THEN
tz% = 0
 'COLOR 255
 'LOCATE 1, 12: PRINT (1 / ((TIMER - t#) / 30)); "f/s "; ' show frames/min
 'COLOR 50
 t# = TIMER
 'del all graves frequently
 FOR jj% = 1 TO 25
 FOR ii% = 1 TO 12
  IF Grid%(ii%, jj%) = 9 THEN Grid%(ii%, jj%) = 0
 NEXT ii%
 NEXT jj%
END IF
IF TIMER < t2# THEN 'slow down very fast cpu-s
 WHILE TIMER < t2# AND TIMER > 2
 WEND
END IF
LOOP
END' eo main

schritt:
    neuxgrid% = INT(newPX& \ 10000) ' if walking forward/back: check for
    neuygrid% = INT(newPY& \ 10000)'  no wall or open door
    davorne% = Grid%(neuxgrid%, neuygrid%)
    IF davorne% = 0 OR davorne% = 5 THEN
      IF davorne% = 0 THEN
       PX& = newPX&: PY& = newPY&
       xgrid% = neuxgrid%: ygrid% = neuygrid%
      ELSE 'nicht 0 sondern 5(tÅr)
       IF tuer% = 1 THEN
        PX& = newPX&: PY& = newPY&
        xgrid% = neuxgrid%: ygrid% = neuygrid%
       ELSE 'door closed: slide along
        GOSUB tndschritt
       END IF
      END IF
    ELSE 'tried to walk through a wall
     GOSUB tndschritt
    END IF
RETURN

tndschritt:
     'SOUND 80, .1'slide along wall
     oldxgrid% = INT(PX& \ 10000)
     oldygrid% = INT(PY& \ 10000)
     IF (oldxgrid% <> neuxgrid%) THEN
      IF Grid%(oldxgrid%, neuygrid%) = 0 THEN
       PY& = newPY&
       xgrid% = oldxgrid%: ygrid% = neuygrid%
      END IF
     END IF
     IF (oldygrid% <> neuygrid%) THEN
      IF Grid%(neuxgrid%, oldygrid%) = 0 THEN
       PX& = newPX&
       xgrid% = neuxgrid%: ygrid% = oldygrid%
      END IF
     END IF
RETURN

ComputeView:
'primary raycaster
GOSUB ddoffscreen 'redirect qbasic grafx-output to field fe&()
DEF SEG = feseg%
korn% = 0
X1% = 20
FOR a% = heading% + 140 TO heading% - 139 STEP -1
  GOSUB castboden ' draw floor(much faster without floortextures would be:
  'LINE (20, 0)-(299, 100), 0, BF
  XX& = PX&: YY& = PY&
  l% = 0
weitercasten1:
  'outguard1: cast in steps of 100
   xint% = XX& \ 10000
   yint% = YY& \ 10000
   stepx% = iStable2%(a%): stepy% = iCtable2%(a%)
   DO
    XX& = XX& - stepx%: YY& = YY& - stepy%
     l% = l% + 100
   oldxint% = xint%
   oldyint% = yint%
   xint% = XX& \ 10000
   yint% = YY& \ 10000
 LOOP UNTIL (xint% <> oldxint%) OR (yint% <> oldyint%) 'Grid%(xint%, yint%)
 l% = l% - 100' one step back
 XX& = XX& + stepx%: YY& = YY& + stepy%
 'outguard2: step in steps of 10
   xint% = XX& \ 10000
   yint% = YY& \ 10000
   stepx% = iSTable%(a%): stepy% = iCTable%(a%)
   DO
    XX& = XX& - stepx%: YY& = YY& - stepy%
     l% = l% + 10
   oldxint% = xint%
   oldyint% = yint%
   xint% = XX& \ 10000
   yint% = YY& \ 10000
 LOOP UNTIL (xint% <> oldxint%) OR (yint% <> oldyint%) 'Grid%(xint%, yint%)
 l% = l% - 10' one step back
 XX& = XX& + stepx%: YY& = YY& + stepy%
'caster: cast in steps of 2
   xint% = XX& \ 10000
   yint% = YY& \ 10000
 stepx% = iSTable%(a%) \ 5: stepy% = iCTable%(a%) \ 5
  'XX& = PX&: YY& = PY&
  'l% = 0
   DO
    XX& = XX& - stepx%: YY& = YY& - stepy%
    l% = l% + 2
   oldxint% = xint%
   oldyint% = yint%
   xint% = XX& \ 10000
   yint% = YY& \ 10000
 LOOP UNTIL (xint% <> oldxint%) OR (yint% <> oldyint%) 'Grid%(xint%, yint%)
  k% = Grid%(xint%, yint%)
  IF k% = 0 THEN 'this next grid%(,)-index is only floor, go on casting
   GOTO weitercasten1
  END IF
  xxfrac% = XX& - (10000& * xint%)'ehem
  yyfrac% = YY& - (10000& * yint%)'ehem
  distf% = ((xxfrac% + yyfrac%) \ 156) AND 63' x-position inside 64*64Texture

 IF k% = 1 THEN ' which texture is here?...
  dd% = 22500 \ l%: IF dd% > 600 THEN dd% = 600'dd%=l2250%(l%)  '2250 \ l%
  h% = dd% + dd%' height of the object (calculated by distance l%)
  DT% = 100 - dd%
  IF DT% <= 50 THEN
   tey% = 50 - DT%
  ELSE
   tey% = 0
  END IF
'wÑnde:
teydt% = tey% + DT%
IF (xxfrac% > 100) AND (xxfrac% < 9900) THEN   ' east or west side of object?
DO
farb% = tx1%(distf%, (tey% * h64i%(h%)) \ 500)' y-position inside texture
POKE l320&(teydt%) + X1%, farb%
tey% = tey% + 1
teydt% = teydt% + 1
LOOP UNTIL teydt% > 150 OR tey% > h%
ELSE ' no: its north or south
DO
farb% = tx1dark%(distf%, (tey% * h64i%(h%)) \ 500)
POKE l320&(teydt%) + X1%, farb%
tey% = tey% + 1
teydt% = teydt% + 1
LOOP UNTIL teydt% > 150 OR tey% > h%
END IF
GOTO qqq
END IF

IF k% = 2 THEN
  dd% = 22500 \ l%: IF dd% > 600 THEN dd% = 600
  h% = dd% + dd%
  DT% = 100 - dd%
  IF DT% <= 50 THEN
   tey% = 50 - DT%
  ELSE
   tey% = 0
  END IF
'wÑnde:
teydt% = tey% + DT%
IF (xxfrac% > 100) AND (xxfrac% < 9900) THEN
 DO
  farb% = tx2%(distf%, (tey% * h64i%(h%)) \ 500)
  POKE l320&(teydt%) + X1%, farb%
  tey% = tey% + 1
  teydt% = teydt% + 1
 LOOP UNTIL teydt% > 150 OR tey% > h%
ELSE
 DO
  farb% = tx2dark%(distf%, (tey% * h64i%(h%)) \ 500)
  POKE l320&(teydt%) + X1%, farb%
  tey% = tey% + 1
  teydt% = teydt% + 1
 LOOP UNTIL teydt% > 150 OR tey% > h%
END IF
GOTO qqq
END IF

IF k% = 3 THEN
 dd% = 22500 \ l%: IF dd% > 600 THEN dd% = 600
  h% = dd% + dd%
  DT% = 100 - dd%
  IF DT% <= 50 THEN
   tey% = 50 - DT%
  ELSE
   tey% = 0
  END IF
'wÑnde:
teydt% = tey% + DT%
IF (xxfrac% > 100) AND (xxfrac% < 9900) THEN
 DO
  farb% = tx3%(distf%, (tey% * h64i%(h%)) \ 500)
  POKE l320&(teydt%) + X1%, farb%
  tey% = tey% + 1
  teydt% = teydt% + 1
 LOOP UNTIL teydt% > 150 OR tey% > h%
ELSE
 DO
  farb% = tx3dark%(distf%, (tey% * h64i%(h%)) \ 500)
  POKE l320&(teydt%) + X1%, farb%
  tey% = tey% + 1
  teydt% = teydt% + 1
 LOOP UNTIL teydt% > 150 OR tey% > h%
END IF
GOTO qqq
END IF
   
IF k% = 4 THEN 'spider
mxxfrac% = xxfrac%
myyfrac% = yyfrac%
IF korn% = 0 AND X1% > 150 AND X1% < 170 THEN
 korn% = 1
 spidx% = xint%
 spidy% = yint%
END IF
IF XX& > PX& THEN 'mirroring
 mxxfrac% = 10000& - xxfrac%
END IF
IF YY& < PY& THEN
 myyfrac% = 10000& - yyfrac%
END IF
IF winkel45% = 0 THEN 'paint onto corner
 distf% = ((mxxfrac% + myyfrac%) / 156) AND 127
 dd% = 22500 \ l%: IF dd% > 600 THEN dd% = 600
 h% = dd% + dd%
 DT% = 100 - dd%
 IF DT% <= 50 THEN
  tey% = 50 - DT%
 ELSE
  tey% = 0
 END IF
 'wÑnde:
 teydt% = tey% + DT%
 IF (myyfrac% > 100) AND (myyfrac% < 9900) THEN
 DO
   farb% = tx4%(distf%, (tey% * h64i%(h%)) \ 250)
   IF farb% <> maske% THEN
    POKE l320&(teydt%) + X1%, farb%
    spritecolumn%(teydt%) = 1
   ELSE
    spritecolumn%(teydt%) = 0
   END IF
   tey% = tey% + 1
   teydt% = teydt% + 1
  LOOP UNTIL teydt% > 150 OR tey% > h%
 ELSE
 DO
   farb% = tx4%(distf%, (tey% * h64i%(h%)) \ 250)
   IF farb% <> maske% THEN
    POKE l320&(teydt%) + X1%, farb%
    spritecolumn%(teydt%) = 1
   ELSE
    spritecolumn%(teydt%) = 0
   END IF
    tey% = tey% + 1
    teydt% = teydt% + 1
  LOOP UNTIL teydt% > 150 OR tey% > h%

 END IF
ELSE 'if winkel45...
  distf% = ((xxfrac% + yyfrac%) \ 78) AND 127
  dd% = 22500 \ l%: IF dd% > 600 THEN dd% = 600
  h% = dd% + dd%
  DT% = 100 - dd%
  IF DT% <= 50 THEN
   tey% = 50 - DT%
  ELSE
   tey% = 0
  END IF
  'wÑnde:
  teydt% = tey% + DT%
  IF richtung% = 1 OR richtung% = 3 THEN
    IF (yyfrac% > 100) AND (yyfrac% < 9900) THEN
      DO
        farb% = tx4%(distf%, (tey% * h64i%(h%)) \ 250)
        IF farb% <> maske% THEN
          POKE l320&(teydt%) + X1%, farb%
          spritecolumn%(teydt%) = 1
        ELSE
          spritecolumn%(teydt%) = 0
        END IF
        tey% = tey% + 1
        teydt% = teydt% + 1
      LOOP UNTIL teydt% > 150 OR tey% > h%
    ELSE ' if yyfrac.
      FOR ii% = 50 TO 150 'nîtig?!?
        spritecolumn%(ii%) = 0
      NEXT ii%
    END IF
  ELSE 'if richt
    IF (xxfrac% > 100) AND (xxfrac% < 9900) THEN
      DO
        farb% = tx4%(distf%, (tey% * h64i%(h%)) \ 250)
        IF farb% <> maske% THEN
          POKE l320&(teydt%) + X1%, farb%
          spritecolumn%(teydt%) = 1
        ELSE
          spritecolumn%(teydt%) = 0
        END IF
        tey% = tey% + 1
        teydt% = teydt% + 1
      LOOP UNTIL teydt% > 150 OR tey% > h%
    ELSE ' if yyfrac.
      FOR ii% = 50 TO 150 'nîtig?!?
        spritecolumn%(ii%) = 0
      NEXT ii%
    END IF
  END IF  'if ri
END IF'if winkel45%=0
 '....cast 2. mal....
 xintmerk%(rek%) = xint%
 yintmerk%(rek%) = yint%
 gridmerk%(rek%) = Grid%(xint%, yint%)
 rek% = rek% + 1
 Grid%(xint%, yint%) = 0

 GOSUB cast2
 rek% = rek% - 1
 Grid%(xintmerk%(rek%), yintmerk%(rek%)) = gridmerk%(rek%)
 xint% = xintmerk%(rek%)
 yint% = yintmerk%(rek%)
 GOTO qqq
END IF
  
IF k% = 5 THEN 'door
  dd% = 22500 \ l%: IF dd% > 600 THEN dd% = 600
  h% = dd% + dd%
  DT% = 100 - dd%
  IF DT% <= 50 THEN
   tey% = 50 - DT%
  ELSE
   tey% = 0
  END IF
teydt% = tey% + DT%
DO
 farb% = tx5%(distf%, (tey% * h64i%(h%)) \ 500)
 IF farb% <> maske% THEN
  POKE l320&(teydt%) + X1%, farb%
  spritecolumn%(teydt%) = 1
 ELSE
  spritecolumn%(teydt%) = 0
 END IF
  tey% = tey% + 1
  teydt% = teydt% + 1
 LOOP UNTIL teydt% > 150 OR tey% > h%
 '....cast 2. time....
 xintmerk%(rek%) = xint%
 yintmerk%(rek%) = yint%
 gridmerk%(rek%) = Grid%(xint%, yint%)
 rek% = rek% + 1
 Grid%(xint%, yint%) = 0

 GOSUB cast2
 rek% = rek% - 1
 Grid%(xintmerk%(rek%), yintmerk%(rek%)) = gridmerk%(rek%)
 xint% = xintmerk%(rek%)
 yint% = yintmerk%(rek%)
 GOTO qqq
END IF

IF k% >= 6 AND k% <= 15 THEN ' masked textures 6 to 15
  dd% = 22500 \ l%: IF dd% > 600 THEN dd% = 600
  h% = dd% + dd%
  DT% = 100 - dd%
  IF DT% <= 50 THEN
   tey% = 50 - DT%
  ELSE
   tey% = 0
  END IF
'wÑnde:
teydt% = tey% + DT%
DO
SELECT CASE k%
CASE 6: farb% = tx6%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 7: farb% = tx7%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 8: farb% = tx8%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 9: farb% = tx9%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 10: farb% = tx10%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 11: farb% = tx11%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 12: farb% = tx12%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 13: farb% = tx13%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 14: farb% = tx14%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 15: farb% = tx15%(distf%, (tey% * h64i%(h%)) \ 500)
END SELECT
 IF farb% <> maske% THEN
  POKE l320&(teydt%) + X1%, farb%
  spritecolumn%(teydt%) = 1
 ELSE
  spritecolumn%(teydt%) = 0
 END IF
  tey% = tey% + 1
  teydt% = teydt% + 1
 LOOP UNTIL teydt% > 150 OR tey% > h%
 '....cast 2. mal....
 xintmerk%(rek%) = xint%
 yintmerk%(rek%) = yint%
 gridmerk%(rek%) = Grid%(xint%, yint%)
 rek% = rek% + 1
 Grid%(xint%, yint%) = 0

 GOSUB cast2
 rek% = rek% - 1
 Grid%(xintmerk%(rek%), yintmerk%(rek%)) = gridmerk%(rek%)
 xint% = xintmerk%(rek%)
 yint% = yintmerk%(rek%)
 GOTO qqq
END IF
 


IF k% >= 16 THEN   ' nonmasked textures >=16
  dd% = 22500 \ l%: IF dd% > 600 THEN dd% = 600
  h% = dd% + dd%
  DT% = 100 - dd%
  IF DT% <= 50 THEN
   tey% = 50 - DT%
  ELSE
   tey% = 0
  END IF
'wÑnde:
teydt% = tey% + DT%
DO
SELECT CASE k%
CASE 16: farb% = tx16%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 17: farb% = tx17%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 18: farb% = tx18%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 19: farb% = tx19%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 20: farb% = tx20%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 21: farb% = tx21%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 22: farb% = tx22%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 23: farb% = tx23%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 24: farb% = tx24%(distf%, (tey% * h64i%(h%)) \ 500)
END SELECT
  POKE l320&(teydt%) + X1%, farb%
  tey% = tey% + 1
  teydt% = teydt% + 1
 LOOP UNTIL teydt% > 150 OR tey% > h%
END IF


'.............................................end of k%-check

qqq:

X1% = X1% + 1
NEXT
'other sprites
IF feuer% = 0 THEN
PUT (130, 86), hand1m&, AND
PUT (130, 86), hand1&, OR
ELSE
PUT (130, 77), hand2m&, AND
PUT (130, 77), hand2&, OR
END IF
GOSUB ddonscreen
'PALETTE 0, 0
RETURN'computeview

cast2:
'secundary raycaster, recursive: launching itself if required.
'allmost the same like computeview:
weitercasten2:
'outguard1
   xint% = XX& \ 10000
   yint% = YY& \ 10000
stepx% = iStable2%(a%): stepy% = iCtable2%(a%)
   DO
    XX& = XX& - stepx%: YY& = YY& - stepy%
     l% = l% + 100
   oldxint% = xint%
   oldyint% = yint%
   xint% = XX& \ 10000
   yint% = YY& \ 10000
 LOOP UNTIL (xint% <> oldxint%) OR (yint% <> oldyint%) 'Grid%(xint%, yint%)
 l% = l% - 100
XX& = XX& + stepx%: YY& = YY& + stepy%
'...
'outguard2
   xint% = XX& \ 10000
   yint% = YY& \ 10000
  stepx% = iSTable%(a%): stepy% = iCTable%(a%)
   DO
    XX& = XX& - stepx%: YY& = YY& - stepy%
     l% = l% + 10
   oldxint% = xint%
   oldyint% = yint%
   xint% = XX& \ 10000
   yint% = YY& \ 10000
 LOOP UNTIL (xint% <> oldxint%) OR (yint% <> oldyint%) 'Grid%(xint%, yint%)
 l% = l% - 10
 XX& = XX& + stepx%: YY& = YY& + stepy%
'caster
   xint% = XX& \ 10000
   yint% = YY& \ 10000
 stepx% = iSTable%(a%) \ 5: stepy% = iCTable%(a%) \ 5
  'XX& = PX&: YY& = PY&
  'l% = 0
   DO
    XX& = XX& - stepx%: YY& = YY& - stepy%
    l% = l% + 2
   oldxint% = xint%
   oldyint% = yint%
   xint% = XX& \ 10000
   yint% = YY& \ 10000
 LOOP UNTIL (xint% <> oldxint%) OR (yint% <> oldyint%) 'Grid%(xint%, yint%)
 k% = Grid%(xint%, yint%)
  IF k% = 0 THEN
   GOTO weitercasten2
  END IF
xxfrac% = XX& - (10000& * xint%)
yyfrac% = YY& - (10000& * yint%)
distf% = ((xxfrac% + yyfrac%) \ 156) AND 63
 
IF k% = 1 THEN 'wall 1
  dd% = 22500 \ l%: IF dd% > 600 THEN dd% = 600
  h% = dd% + dd%
  DT% = 100 - dd%
  IF DT% <= 50 THEN
   tey% = 50 - DT%
  ELSE
   tey% = 0
  END IF
'wÑnde:
teydt% = tey% + DT%
IF (xxfrac% > 100) AND (xxfrac% < 9900) THEN
DO
 IF spritecolumn%(teydt%) = 0 THEN
  farb% = tx1%(distf%, (tey% * h64i%(h%)) \ 500)
  POKE l320&(teydt%) + X1%, farb%
 END IF
 tey% = tey% + 1
 teydt% = teydt% + 1
LOOP UNTIL teydt% > 150 OR tey% > h%
ELSE
DO
IF spritecolumn%(teydt%) = 0 THEN
farb% = tx1dark%(distf%, (tey% * h64i%(h%)) \ 500)
POKE l320&(teydt%) + X1%, farb%
END IF
tey% = tey% + 1
teydt% = teydt% + 1
LOOP UNTIL teydt% > 150 OR tey% > h%
END IF
GOTO qqq2
END IF

IF k% = 2 THEN 'wall 2
  dd% = 22500 \ l%: IF dd% > 600 THEN dd% = 600
  h% = dd% + dd%
  DT% = 100 - dd%
  IF DT% <= 50 THEN
   tey% = 50 - DT%
  ELSE
   tey% = 0
  END IF
'wÑnde:
teydt% = tey% + DT%
IF (xxfrac% > 100) AND (xxfrac% < 9900) THEN
 DO
IF spritecolumn%(teydt%) = 0 THEN
  farb% = tx2%(distf%, (tey% * h64i%(h%)) \ 500)
  POKE l320&(teydt%) + X1%, farb%
  END IF
  tey% = tey% + 1
  teydt% = teydt% + 1
 LOOP UNTIL teydt% > 150 OR tey% > h%
ELSE
 DO
IF spritecolumn%(teydt%) = 0 THEN
  farb% = tx2dark%(distf%, (tey% * h64i%(h%)) \ 500)
  POKE l320&(teydt%) + X1%, farb%
  END IF
  tey% = tey% + 1
  teydt% = teydt% + 1
 LOOP UNTIL teydt% > 150 OR tey% > h%
END IF
GOTO qqq2
END IF

IF k% = 3 THEN 'camera-wall
 dd% = 22500 \ l%: IF dd% > 600 THEN dd% = 600
  h% = dd% + dd%
  DT% = 100 - dd%
  IF DT% <= 50 THEN
   tey% = 50 - DT%
  ELSE
   tey% = 0
  END IF
'wÑnde:
teydt% = tey% + DT%
IF (xxfrac% > 100) AND (xxfrac% < 9900) THEN
 DO
IF spritecolumn%(teydt%) = 0 THEN
  farb% = tx3%(distf%, (tey% * h64i%(h%)) \ 500)
  POKE l320&(teydt%) + X1%, farb%
  END IF
  tey% = tey% + 1
  teydt% = teydt% + 1
 LOOP UNTIL teydt% > 150 OR tey% > h%
ELSE
 DO
IF spritecolumn%(teydt%) = 0 THEN
  farb% = tx3dark%(distf%, (tey% * h64i%(h%)) \ 500)
  POKE l320&(teydt%) + X1%, farb%
  END IF
  tey% = tey% + 1
  teydt% = teydt% + 1
 LOOP UNTIL teydt% > 150 OR tey% > h%
END IF
GOTO qqq2
END IF
 
'------
IF k% = 4 THEN 'sprite cast2
 mxxfrac% = xxfrac%
 myyfrac% = yyfrac%
 IF korn% = 0 AND X1% > 150 AND X1% < 170 THEN
  korn% = 1
  spidx% = xint%
  spidy% = yint%
 END IF
 IF XX& > PX& THEN 'spiegeln...
  mxxfrac% = 10000& - xxfrac%
 END IF
 IF YY& < PY& THEN
  myyfrac% = 10000& - yyfrac%
 END IF
 IF winkel45% = 0 THEN 'zeichne Åber ecke
  distf% = ((mxxfrac% + myyfrac%) / 156) AND 127
  dd% = 22500 \ l%: IF dd% > 600 THEN dd% = 600
  h% = dd% + dd%
  DT% = 100 - dd%
  IF DT% <= 50 THEN
   tey% = 50 - DT%
  ELSE
   tey% = 0
  END IF
  'wÑnde:
  teydt% = tey% + DT%
  IF (myyfrac% > 500) AND (myyfrac% < 9500) THEN
   DO
    IF spritecolumn%(teydt%) = 0 THEN
     farb% = tx4%(distf%, (tey% * h64i%(h%)) \ 250)
     IF farb% <> maske% THEN
      POKE l320&(teydt%) + X1%, farb%
      spritecolumn%(teydt%) = 1
     END IF
    END IF
    tey% = tey% + 1
    teydt% = teydt% + 1
   LOOP UNTIL teydt% > 150 OR tey% > h%
  ELSE
   DO
    IF spritecolumn%(teydt%) = 0 THEN
     farb% = tx4%(distf%, (tey% * h64i%(h%)) \ 250)
     IF farb% <> maske% THEN
      POKE l320&(teydt%) + X1%, farb%
      spritecolumn%(teydt%) = 1
     END IF
    END IF
    tey% = tey% + 1
    teydt% = teydt% + 1
   LOOP UNTIL teydt% > 150 OR tey% > h%
  END IF
 ELSE 'if winkel45...
  distf% = ((xxfrac% + yyfrac%) \ 78) AND 127
  dd% = 22500 \ l%: IF dd% > 600 THEN dd% = 600
  h% = dd% + dd%
  DT% = 100 - dd%
  IF DT% <= 50 THEN
   tey% = 50 - DT%
  ELSE
   tey% = 0
  END IF
  'wÑnde:
  teydt% = tey% + DT%
  IF richtung% = 1 OR richtung% = 3 THEN
   IF (yyfrac% > 500) AND (yyfrac% < 9500) THEN
    DO
     IF spritecolumn%(teydt%) = 0 THEN
      farb% = tx4%(distf%, (tey% * h64i%(h%)) \ 250)
      IF farb% <> maske% THEN
       POKE l320&(teydt%) + X1%, farb%
       spritecolumn%(teydt%) = 1
      END IF
     END IF
     tey% = tey% + 1
     teydt% = teydt% + 1
    LOOP UNTIL teydt% > 150 OR tey% > h%
   ELSE ' if yyfrac.
   END IF
  ELSE 'if richt
   IF (xxfrac% > 100) AND (xxfrac% < 9900) THEN
    DO
     IF spritecolumn%(teydt%) = 0 THEN
      farb% = tx4%(distf%, (tey% * h64i%(h%)) \ 250)
      IF farb% <> maske% THEN
       POKE l320&(teydt%) + X1%, farb%
       spritecolumn%(teydt%) = 1
      END IF
     END IF
     tey% = tey% + 1
     teydt% = teydt% + 1
    LOOP UNTIL teydt% > 150 OR tey% > h%
   ELSE ' if yyfrac.
   END IF
  END IF  'if ri
 END IF'if winkel45%=0
 '....cast ?. time....
  
 xintmerk%(rek%) = xint%
 yintmerk%(rek%) = yint%
 gridmerk%(rek%) = Grid%(xint%, yint%)
 rek% = rek% + 1
 Grid%(xint%, yint%) = 0

 GOSUB cast2
 rek% = rek% - 1
 Grid%(xintmerk%(rek%), yintmerk%(rek%)) = gridmerk%(rek%)
 xint% = xintmerk%(rek%)
 yint% = yintmerk%(rek%)
 GOTO qqq2
END IF
  
'------------
IF k% = 5 THEN ' door
  dd% = 22500 \ l%: IF dd% > 600 THEN dd% = 600
  h% = dd% + dd%
  DT% = 100 - dd%
  IF DT% <= 50 THEN
   tey% = 50 - DT%
  ELSE
   tey% = 0
  END IF
'wÑnde:
teydt% = tey% + DT%
'IF (myyfrac% > 100) AND (myyfrac% < 9900) THEN
DO
IF spritecolumn%(teydt%) = 0 THEN
 farb% = tx5%(distf%, (tey% * h64i%(h%)) \ 500)
 IF farb% <> maske% THEN
  POKE l320&(teydt%) + X1%, farb%
  spritecolumn%(teydt%) = 1
 END IF
END IF
  tey% = tey% + 1
  teydt% = teydt% + 1
 LOOP UNTIL teydt% > 150 OR tey% > h%
 'END IF
 '....cast ?. mal....
 xintmerk%(rek%) = xint%
 yintmerk%(rek%) = yint%
 gridmerk%(rek%) = Grid%(xint%, yint%)
 rek% = rek% + 1
 Grid%(xint%, yint%) = 0

 GOSUB cast2
 rek% = rek% - 1
 Grid%(xintmerk%(rek%), yintmerk%(rek%)) = gridmerk%(rek%)
 xint% = xintmerk%(rek%)
 yint% = yintmerk%(rek%)
 GOTO qqq2
END IF

'......................................

IF k% >= 6 AND k% <= 15 THEN 'masked plant etc.
  dd% = 22500 \ l%: IF dd% > 600 THEN dd% = 600'dd%=l2250%(l%)  '2250 \ l%
  h% = dd% + dd%
  DT% = 100 - dd%
  IF DT% <= 50 THEN
   tey% = 50 - DT%
  ELSE
   tey% = 0
  END IF
'wÑnde:
teydt% = tey% + DT%
DO
 IF spritecolumn%(teydt%) = 0 THEN
SELECT CASE k%
CASE 6: farb% = tx6%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 7: farb% = tx7%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 8: farb% = tx8%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 9: farb% = tx9%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 10: farb% = tx10%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 11: farb% = tx11%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 12: farb% = tx12%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 13: farb% = tx13%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 14: farb% = tx14%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 15: farb% = tx15%(distf%, (tey% * h64i%(h%)) \ 500)
END SELECT
  IF farb% <> maske% THEN
   POKE l320&(teydt%) + X1%, farb%
   spritecolumn%(teydt%) = 1
  END IF
 END IF
  tey% = tey% + 1
  teydt% = teydt% + 1
 LOOP UNTIL teydt% > 150 OR tey% > h%
 '....cast rekursiv n. mal....
 xintmerk%(rek%) = xint%
 yintmerk%(rek%) = yint%
 gridmerk%(rek%) = Grid%(xint%, yint%)
 rek% = rek% + 1
 Grid%(xint%, yint%) = 0
 GOSUB cast2
 rek% = rek% - 1
 Grid%(xintmerk%(rek%), yintmerk%(rek%)) = gridmerk%(rek%)
 xint% = xintmerk%(rek%)
 yint% = yintmerk%(rek%)
 GOTO qqq2
END IF

IF k% >= 16 THEN   ' nonmaskable (faster)
  dd% = 22500 \ l%: IF dd% > 600 THEN dd% = 600
  h% = dd% + dd%
  DT% = 100 - dd%
  IF DT% <= 50 THEN
   tey% = 50 - DT%
  ELSE
   tey% = 0
  END IF
'wÑnde:
teydt% = tey% + DT%
DO
 IF spritecolumn%(teydt%) = 0 THEN
SELECT CASE k%
CASE 16: farb% = tx16%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 17: farb% = tx17%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 18: farb% = tx18%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 19: farb% = tx19%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 20: farb% = tx20%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 21: farb% = tx21%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 22: farb% = tx22%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 23: farb% = tx23%(distf%, (tey% * h64i%(h%)) \ 500)
CASE 24: farb% = tx24%(distf%, (tey% * h64i%(h%)) \ 500)
END SELECT
  POKE l320&(teydt%) + X1%, farb%
 END IF
  tey% = tey% + 1
  teydt% = teydt% + 1
 LOOP UNTIL teydt% > 150 OR tey% > h%
END IF



'------------
qqq2:
RETURN'cast2

castboden:
  XX& = PX&: YY& = PY& ' cast floor
  l% = 0
  ddold% = 0
  stepx% = iSTable%(a%): stepy% = iCTable%(a%)
  DO
   XX& = XX& - stepx%: YY& = YY& - stepy%
   l% = l% + 10
   dd% = 22500 \ l%
   IF dd% <> ddold% THEN
    IF dd% > 600 THEN dd% = 600
     xint% = XX& \ 10000
     yint% = YY& \ 10000
     k% = Grid%(xint%, yint%)
     k2% = Grid2%(xint%, yint%)'floor
     k3% = Grid3%(xint%, yint%)'roof
     DT% = 100 - dd%
     IF DT% = 50 THEN
      IF k2% OR k3% THEN
       xxfrac% = XX& - (10000& * xint%)'ehem
       yyfrac% = YY& - (10000& * yint%)'ehem
       distfx% = ((xxfrac%) \ 156) AND 63
       distfy% = ((yyfrac%) \ 156) AND 63
      END IF
     farb% = 0
     SELECT CASE k2%
CASE 1: farb% = tx1floor%(distfx%, distfy%)
CASE 2: farb% = tx2floor%(distfx%, distfy%)
CASE 3: farb% = tx3floor%(distfx%, distfy%)
CASE 4: farb% = tx4floor%(distfx%, distfy%)
CASE 5: farb% = tx5floor%(distfx%, distfy%)
CASE 6: farb% = tx6floor%(distfx%, distfy%)
     END SELECT
     POKE l320r&(DT%) + X1%, farb%
     POKE l320r&(DT% + 1) + X1%, farb%
     farb% = 0
     SELECT CASE k3%
CASE 1: farb% = tx1floor%(distfx%, distfy%)
CASE 2: farb% = tx2floor%(distfx%, distfy%)
CASE 3: farb% = tx3floor%(distfx%, distfy%)
CASE 4: farb% = tx4floor%(distfx%, distfy%)
CASE 5: farb% = tx5floor%(distfx%, distfy%)
CASE 6: farb% = tx6floor%(distfx%, distfy%)
     END SELECT
     POKE l320&(DT%) + X1%, farb%
     POKE l320&(DT% + 1) + X1%, farb%
    END IF
    IF DT% > 50 AND DT% <= 93 THEN
     IF k2% OR k3% THEN
      xxfrac% = XX& - (10000& * xint%)
      yyfrac% = YY& - (10000& * yint%)
      distfx% = ((xxfrac%) \ 156) AND 63
      distfy% = ((yyfrac%) \ 156) AND 63
     END IF
     farb% = 0
     SELECT CASE k2%
CASE 1: farb% = tx1floor%(distfx%, distfy%)
CASE 2: farb% = tx2floor%(distfx%, distfy%)
CASE 3: farb% = tx3floor%(distfx%, distfy%)
CASE 4: farb% = tx4floor%(distfx%, distfy%)
CASE 5: farb% = tx5floor%(distfx%, distfy%)
CASE 6: farb% = tx6floor%(distfx%, distfy%)
     END SELECT
     ' ON k2% GOSUB wf1, wf2, wf3, wf4, wf5, wf6
     POKE l320r&(DT%) + X1%, farb%
     farb% = 0
     SELECT CASE k3%
CASE 1: farb% = tx1floor%(distfx%, distfy%)
CASE 2: farb% = tx2floor%(distfx%, distfy%)
CASE 3: farb% = tx3floor%(distfx%, distfy%)
CASE 4: farb% = tx4floor%(distfx%, distfy%)
CASE 5: farb% = tx5floor%(distfx%, distfy%)
CASE 6: farb% = tx6floor%(distfx%, distfy%)
     END SELECT
     POKE l320&(DT%) + X1%, farb%
    END IF
   END IF
   ddold% = dd%
  LOOP UNTIL (k% > 0 AND k% < 4) OR (k% > 15)
RETURN' castbo

 
ddoffscreen:
 DEF SEG = arefseg%
 POKE m&(einer%), fes1%
 POKE m&(einer%) + 1, fes2%
RETURN
ddonscreen:
 DEF SEG = arefseg%
 POKE m&(einer%), ha0001%
 POKE m&(einer%) + 1, ha0002%
 fe&(0) = fe0&
 fe&(1) = fe1&
 PUT (0, 50), fe&, PSET
RETURN

texture:
GOSUB plt1toplt2 ' load all textures
GOSUB finddarks
 FOR j% = 0 TO 63
   FOR i% = 0 TO 63
    tx1%(i%, j%) = POINT(i%, j%)
   NEXT i%
  NEXT j%
  darkx% = 0: darky% = 0
  GOSUB darken
 FOR j% = 0 TO 63
   FOR i% = 0 TO 63
    tx1dark%(i%, j%) = POINT(i%, j%)
   NEXT i%
  NEXT j%
  '..................
 FOR j% = 0 TO 63
   FOR i% = 0 TO 63
    tx2%(i%, j%) = POINT(i% + 64, j%)
   NEXT i%
  NEXT j%
  darkx% = 64: darky% = 0
  GOSUB darken
 FOR j% = 0 TO 63
   FOR i% = 0 TO 63
    tx2dark%(i%, j%) = POINT(i% + 64, j%)
   NEXT i%
  NEXT j%
  '..................
 FOR j% = 0 TO 63
   FOR i% = 0 TO 63
    tx3%(i%, j%) = POINT(i% + 192, j%)
   NEXT i%
  NEXT j%
  darkx% = 192: darky% = 0
  GOSUB darken
 FOR j% = 0 TO 63
   FOR i% = 0 TO 63
    tx3dark%(i%, j%) = POINT(i% + 192, j%)
    tx5%(i%, j%) = POINT(128 + i%, j%)
    tx5bunker%(i%, j%) = POINT(128 + i%, j%)
   NEXT i%
  NEXT j%
'------------------
'load sprites 2 from boom2.gif
gifname$ = "boom2.gif"
wichplt% = 1
GOSUB gifload
GOSUB finddarks
FOR j% = 0 TO 199
FOR i% = 0 TO 319
a% = POINT(i%, j%)
PSET (i%, j%), aehn%(a%)
NEXT i%
NEXT j%
'---
 FOR j% = 0 TO 127
   FOR i% = 0 TO 127
    tx4%(i%, j%) = POINT(i%, j%)
   NEXT i%
  NEXT j%

 FOR j% = 0 TO 63
   FOR i% = 0 TO 63
    tx6%(i%, j%) = POINT(i% + 128, j%)
    tx7%(i%, j%) = POINT(i% + 128, j% + 64)
    tx8%(i%, j%) = POINT(i% + 256, j%)
    tx9%(i%, j%) = POINT(i% + 256, j% + 64)
    tx1floor%(i%, j%) = POINT(i% + 192, j% + 64)
    tx2floor%(i%, j%) = POINT(i%, j% + 128)
    tx3floor%(i%, j%) = POINT(i% + 64, j% + 128)
    tx4floor%(i%, j%) = POINT(i% + 128, j% + 128)
    tx5floor%(i%, j%) = POINT(i% + 192, j% + 128)
    tx6floor%(i%, j%) = POINT(i% + 256, j% + 128)
   NEXT i%
  NEXT j%
'-----
'------------------
'load sprites 3 from boom3.gif
gifname$ = "boom3.gif"
wichplt% = 1
GOSUB gifload
GOSUB finddarks
FOR j% = 0 TO 199
FOR i% = 0 TO 319
a% = POINT(i%, j%)
PSET (i%, j%), aehn%(a%)
NEXT i%
NEXT j%
'---
'
 FOR j% = 0 TO 63
   FOR i% = 0 TO 63
    tx10%(i%, j%) = POINT(i%, j%)
    tx11%(i%, j%) = POINT(i% + 64, j%)
    tx12%(i%, j%) = POINT(i% + 128, j%)
    tx13%(i%, j%) = POINT(i% + 192, j%)
    tx14%(i%, j%) = POINT(i% + 256, j%)
    tx15%(i%, j%) = POINT(i%, j% + 64)
    tx16%(i%, j%) = POINT(i% + 64, j% + 64)
    tx17%(i%, j%) = POINT(i% + 128, j% + 64)
    tx18%(i%, j%) = POINT(i% + 192, j% + 64)
    tx19%(i%, j%) = POINT(i% + 256, j% + 64)
    tx20%(i%, j%) = POINT(i%, j% + 128)
    tx21%(i%, j%) = POINT(i% + 64, j% + 128)
    tx22%(i%, j%) = POINT(i% + 128, j% + 128)
    tx23%(i%, j%) = POINT(i% + 192, j% + 128)
    tx24%(i%, j%) = POINT(i% + 256, j% + 128)
   NEXT i%
  NEXT j%
RETURN

cockpit:
LINE (18, 48)-(301, 152), 15, B
LINE (0, 0)-(319, 199), 15, B
gifname$ = "display2.gif"
wichplt% = 1
GOSUB gifload
GOSUB finddarks
FOR j% = 0 TO 199
FOR i% = 0 TO 319
a% = POINT(i%, j%)
PSET (i%, j%), aehn%(a%)
NEXT i%
NEXT j%
LINE (18, 48)-(301, 152), 0, BF
GOSUB fadein
RETURN

plt1set:
    OUT &H3C7, 0: OUT &H3C8, 0
    FOR a = 0 TO 255
     OUT &H3C9, plt1%(0, a)
     OUT &H3C9, plt1%(1, a)
     OUT &H3C9, plt1%(2, a)
    NEXT a
RETURN

fadein:
FOR dunkel% = 63 TO 0 STEP -1
    PALETTE 0, 0
    'OUT &H3C7, 0
    OUT &H3C8, 0
    FOR fade% = 0 TO 255
     red% = plt1%(0, fade%) - dunkel%
     IF red% < 0 THEN red% = 0
     green% = plt1%(1, fade%) - dunkel%
     IF green% < 0 THEN green% = 0
     blue% = plt1%(2, fade%) - dunkel%
     IF blue% < 0 THEN blue% = 0
     OUT &H3C9, red%
     OUT &H3C9, green%
     OUT &H3C9, blue%
    NEXT fade%
    NEXT dunkel%
RETURN

fadeinplt2:
FOR dunkel% = 63 TO 0 STEP -1
    PALETTE 0, 0
    OUT &H3C8, 0
    FOR fade% = 0 TO 255
     red% = plt2%(0, fade%) - dunkel%
     IF red% < 0 THEN red% = 0
     green% = plt2%(1, fade%) - dunkel%
     IF green% < 0 THEN green% = 0
     blue% = plt2%(2, fade%) - dunkel%
     IF blue% < 0 THEN blue% = 0
     OUT &H3C9, red%
     OUT &H3C9, green%
     OUT &H3C9, blue%
    NEXT fade%
    NEXT dunkel%
RETURN

fadeout:
OUT &H3C7, 0 '! plt regs auslesen
FOR i% = 0 TO 255
 plt1%(0, i%) = INP(&H3C9) AND 63
 plt1%(1, i%) = INP(&H3C9) AND 63
 plt1%(2, i%) = INP(&H3C9) AND 63
NEXT i%
FOR dunkel% = 0 TO 63
    PALETTE 0, 0
    'OUT &H3C7, 0
    OUT &H3C8, 0
    FOR fade% = 0 TO 255
     red% = plt1%(0, fade%) - dunkel%
     IF red% < 0 THEN red% = 0
     green% = plt1%(1, fade%) - dunkel%
     IF green% < 0 THEN green% = 0
     blue% = plt1%(2, fade%) - dunkel%
     IF blue% < 0 THEN blue% = 0
     OUT &H3C9, red%
     OUT &H3C9, green%
     OUT &H3C9, blue%
    NEXT fade%
    NEXT dunkel%
RETURN

fadeoutplt2:
FOR dunkel% = 0 TO 63
    PALETTE 0, 0
    OUT &H3C8, 0
    FOR fade% = 0 TO 255
     red% = plt2%(0, fade%) - dunkel%
     IF red% < 0 THEN red% = 0
     green% = plt2%(1, fade%) - dunkel%
     IF green% < 0 THEN green% = 0
     blue% = plt2%(2, fade%) - dunkel%
     IF blue% < 0 THEN blue% = 0
     OUT &H3C9, red%
     OUT &H3C9, green%
     OUT &H3C9, blue%
    NEXT fade%
    NEXT dunkel%
RETURN


'anfang procedures v gifloader

END
 
GetByte: a$ = " ": GET #1, , a$: a = ASC(a$): RETURN
 
NextScanLine:
    IF Interlaced THEN
        y = y + PassStep
        IF y >= YEnd THEN
            PassNumber = PassNumber + 1
            SELECT CASE PassNumber
            CASE 1: y = 4: PassStep = 8
            CASE 2: y = 2: PassStep = 4
            CASE 3: y = 1: PassStep = 2
            END SELECT
        END IF
    ELSE
        y = y + 1
    END IF
    x = XStart: Ybase = y * 320&: DoneFlag = y > 199
RETURN
GetCode:
    IF BitsIn = 0 THEN GOSUB ReadBufferedByte: LastChar = a: BitsIn = 8
    WorkCode = LastChar \ ShiftOut(BitsIn)
    DO WHILE CodeSize > BitsIn
        GOSUB ReadBufferedByte: LastChar = a
        WorkCode = WorkCode OR LastChar * Powersof2(BitsIn)
        BitsIn = BitsIn + 8
    LOOP
    BitsIn = BitsIn - CodeSize
    Code = WorkCode AND MaxCode
RETURN
ReadBufferedByte:
    IF BlockPointer > BlockSize THEN
        GOSUB GetByte: BlockSize = a
        a$ = SPACE$(BlockSize): GET #1, , a$
        BlockPointer = 1
    END IF
    a = ASC(MID$(a$, BlockPointer, 1)): BlockPointer = BlockPointer + 1
RETURN

gifload:

FOR a = 0 TO 7: ShiftOut(8 - a) = 2 ^ a: NEXT
FOR a = 0 TO 11: Powersof2(a) = 2 ^ a: NEXT
'A$ = COMMAND$
a$ = gifname$
'a$ = "qbstdplt.gif"
OPEN a$ FOR BINARY AS #1
a$ = "      ": GET #1, , a$
IF a$ <> "GIF87a" THEN PRINT "Not a GIF87a file.": END
GET #1, , TotalX: GET #1, , TotalY: GOSUB GetByte
NumColors = 2 ^ ((a AND 7) + 1): NoPalette = (a AND 128) = 0
GOSUB GetByte: Background = a
GOSUB GetByte: IF a <> 0 THEN PRINT "Bad screen descriptor.": END
IF NoPalette = 0 THEN P$ = SPACE$(NumColors * 3): GET #1, , P$
DO
    GOSUB GetByte
    IF a = 44 THEN
        EXIT DO
    ELSEIF a <> 33 THEN
        PRINT "Unknown extension type.": END
    END IF
    GOSUB GetByte
    DO: GOSUB GetByte: a$ = SPACE$(a): GET #1, , a$: LOOP UNTIL a = 0
LOOP
GET #1, , XStart: GET #1, , YStart: GET #1, , XLength: GET #1, , YLength
XEnd = XStart + XLength: YEnd = YStart + YLength: GOSUB GetByte
IF a AND 128 THEN PRINT "Can't handle local colormaps.": END
Interlaced = a AND 64: PassNumber = 0: PassStep = 8
GOSUB GetByte
ClearCode = 2 ^ a
EOSCode = ClearCode + 1
FirstCode = ClearCode + 2: NextCode = FirstCode
StartCodeSize = a + 1: CodeSize = StartCodeSize
StartMaxCode = 2 ^ (a + 1) - 1: MaxCode = StartMaxCode

BitsIn = 0: BlockSize = 0: BlockPointer = 1
x = XStart: y = YStart: Ybase = y * 320&

SCREEN 13: DEF SEG = &HA000
IF NoPalette = 0 THEN
     'OUT &H3C7, 0: OUT &H3C8, 0
        zzz1% = 0
        zzz2% = 0
    FOR a = 1 TO NumColors * 3
     'OUT &H3C9, ASC(MID$(P$, A, 1)) \ 4
       IF wichplt% = 0 THEN
       plt1%(zzz1%, zzz2%) = ASC(MID$(P$, a, 1)) \ 4
       ELSE
       plt2%(zzz1%, zzz2%) = ASC(MID$(P$, a, 1)) \ 4
       END IF
       zzz1% = zzz1% + 1
       IF zzz1% > 2 THEN
        zzz1% = 0
        zzz2% = zzz2% + 1
       END IF
     NEXT a
END IF
LINE (0, 0)-(319, 199), Background, BF
DO
    GOSUB GetCode
    IF Code <> EOSCode THEN
        IF Code = ClearCode THEN
            NextCode = FirstCode
            CodeSize = StartCodeSize
            MaxCode = StartMaxCode
            GOSUB GetCode
            CurCode = Code: LastCode = Code: LastPixel = Code
            IF x < 320 THEN POKE x + Ybase, LastPixel
            x = x + 1: IF x = XEnd THEN GOSUB NextScanLine
        ELSE
            CurCode = Code: StackPointer = 0
            IF Code > NextCode THEN EXIT DO 'bad GIF if this happens
            IF Code = NextCode THEN
                CurCode = LastCode
                OutStack(StackPointer) = LastPixel
                StackPointer = StackPointer + 1
            END IF

            DO WHILE CurCode >= FirstCode
                OutStack(StackPointer) = Suffix(CurCode)
                StackPointer = StackPointer + 1
                CurCode = Prefix(CurCode)
            LOOP

            LastPixel = CurCode
            IF x < 320 THEN POKE x + Ybase, LastPixel
            x = x + 1: IF x = XEnd THEN GOSUB NextScanLine

            FOR a = StackPointer - 1 TO 0 STEP -1
                IF x < 320 THEN POKE x + Ybase, OutStack(a)
                 x = x + 1: IF x = XEnd THEN GOSUB NextScanLine
            NEXT

            IF NextCode < 4096 THEN
                Prefix(NextCode) = LastCode
                Suffix(NextCode) = LastPixel
                NextCode = NextCode + 1
                IF NextCode > MaxCode AND CodeSize < 12 THEN
                    CodeSize = CodeSize + 1
                    MaxCode = MaxCode * 2 + 1
                END IF
            END IF
            LastCode = Code
        END IF
    END IF
LOOP UNTIL DoneFlag OR Code = EOSCode
CLOSE #1
DoneFlag = 0: EOSCode = 0
'end of gif loader
RETURN

plt1toplt2:
FOR j% = 0 TO 2
FOR i% = 0 TO 255
plt2%(j%, i%) = plt1%(j%, i%) - 15
IF plt2%(j%, i%) < 0 THEN plt2%(j%, i%) = 0
NEXT i%
NEXT j%
RETURN

finddarks:
FOR i% = 0 TO 255
 gefunden% = 0
 r2% = plt2%(0, i%)
 g2% = plt2%(1, i%)
 b2% = plt2%(2, i%)
 abwr% = 0   'abweichungen r g b (abfrage)
 abwg% = 0
 abwb% = 0
 FOR j% = 0 TO 255
  r% = plt1%(0, j%)
  g% = plt1%(1, j%)
  B% = plt1%(2, j%)
  IF r% = (r2%) AND g% = (g2%) AND B% = (b2%) THEN
   aehn%(i%) = j%  'Ñhnlichkeitsliste
   'genau gleiche farbe gefunden!
   gefunden% = 1
   EXIT FOR
 
  END IF
 NEXT j%
 IF gefunden% = 1 THEN GOTO habes

 FOR abw% = 0 TO 15
 abwr% = abwr% + 1
 FOR j% = 0 TO 255
  r% = plt1%(0, j%)
  g% = plt1%(1, j%)
  B% = plt1%(2, j%)
   IF ((r2% <= r% + abwr%) AND (r2% >= r% - abwr%)) AND ((g2% <= g% + abwg%) AND (g2% >= g% - abwg%)) AND ((b2% <= B% + abwb%) AND (b2% >= B% - abwb%)) THEN
    gefunden% = 1
    aehn%(i%) = j%  'Ñhnlichkeitsliste
   'Ñhnliche farbe gefunden!
   EXIT FOR

  END IF
 NEXT j%
 IF gefunden% = 1 THEN GOTO habes
 abwb% = abwb% + 1
 FOR j% = 0 TO 255
  r% = plt1%(0, j%)
  g% = plt1%(1, j%)
  B% = plt1%(2, j%)
    IF ((r2% <= r% + abwr%) AND (r2% >= r% - abwr%)) AND ((g2% <= g% + abwg%) AND (g2% >= g% - abwg%)) AND ((b2% <= B% + abwb%) AND (b2% >= B% - abwb%)) THEN
   gefunden% = 1
   aehn%(i%) = j%  'Ñhnlichkeitsliste
   'Ñhnliche farbe gefunden!
   EXIT FOR
   END IF

 NEXT j%
 IF gefunden% = 1 THEN GOTO habes
 abwg% = abwg% + 1
 FOR j% = 0 TO 255
  r% = plt1%(0, j%)
  g% = plt1%(1, j%)
  B% = plt1%(2, j%)
    IF ((r2% <= r% + abwr%) AND (r2% >= r% - abwr%)) AND ((g2% <= g% + abwg%) AND (g2% >= g% - abwg%)) AND ((b2% <= B% + abwb%) AND (b2% >= B% - abwb%)) THEN
   gefunden% = 1
   aehn%(i%) = j%  'Ñhnlichkeitsliste
   'Ñhnliche farbe gefunden!
   EXIT FOR
   END IF
 NEXT j%
 NEXT abw%
 'none found
 aehn%(i%) = i%
habes:
NEXT i%
RETURN

darken:
FOR j% = 0 TO 63
FOR i% = 0 TO 63
a% = POINT(i% + darkx%, j% + darky%)
PSET (i% + darkx%, j% + darky%), aehn%(a%)
NEXT i%
NEXT j%
RETURN

gload:
IF COMMAND$ = "" THEN
sname$ = "boo1.map"
ELSE
sname$ = COMMAND$
END IF
OPEN sname$ FOR INPUT AS #1
INPUT #1, id$
IF id$ <> "KABOOM Mapfile" THEN
 CLOSE #1
 SCREEN 0: WIDTH 80, 25
 CLS : PRINT "Error: Tried to load a Non-Kaboom-Levelmap"
 END
END IF
  FOR j% = 1 TO 25
   FOR i% = 1 TO 12
    INPUT #1, doedel$
    g% = VAL(doedel$)
    IF g% >= 0 AND g% <= 8 THEN
    Grid%(i%, j%) = VAL(doedel$)
    IF g% = 8 THEN
     levelexitx% = i%
     levelexity% = i%
    END IF
    ELSE
    IF g% = 9 THEN
     Grid%(i%, j%) = 0
     PX& = 5000 + (10000& * i%)
     PY& = 5000 + (10000& * j%)
     anfangx& = PX&
     anfangy& = PY&
    END IF
    IF g% > 9 THEN
     Grid%(i%, j%) = VAL(doedel$)
    END IF
    END IF
   NEXT i%
  NEXT j%
INPUT #1, dumm$ ' floor
  FOR j% = 1 TO 25
   FOR i% = 1 TO 12
    INPUT #1, doedel$
    g% = VAL(doedel$)
    Grid2%(i%, j%) = VAL(doedel$)
   NEXT i%
  NEXT j%
INPUT #1, dumm$
  FOR j% = 1 TO 25
   FOR i% = 1 TO 12
    INPUT #1, doedel$
    g% = VAL(doedel$)
    Grid3%(i%, j%) = VAL(doedel$)
   NEXT i%
  NEXT j%

  INPUT #1, heading$
  heading% = VAL(heading$)
 CLOSE #1
RETURN

bonusload:
 OPEN "boobonus.dat" FOR INPUT AS #1
  FOR i% = 0 TO boxes%
   INPUT #1, box$(i%)
  NEXT i%
 CLOSE #1
RETURN

beenden:
 'SCREEN 0: WIDTH 80, 25
 REDIM SHARED fe&(0)
 REDIM SHARED backgo&(0)
 REDIM SHARED backgu&(0)
 REDIM SHARED backg2o&(0)
 REDIM SHARED backg2u&(0)
 REDIM SHARED schw%(0)
 REDIM SHARED tx1%(0, 0), tx2%(0, 0), tx3%(0, 0), tx1floor%(0, 0), tx2floor%(0, 0)
 REDIM SHARED tx4%(0, 0), tx5%(0, 0), tx6%(0, 0), tx5bunker%(0, 0)
 REDIM SHARED tx1dark%(0, 0), tx2dark%(0, 0), tx3dark%(0, 0)
 REDIM SHARED plt1%(0, 0), plt2%(0, 0), aehn%(0)
 REDIM SHARED m&(0)
 REDIM SHARED l2250%(0)
 REDIM SHARED l320&(0)
 REDIM SHARED l320r&(0)
 REDIM SHARED h64!(0)
 REDIM SHARED h64i%(0)
 REDIM SHARED STable!(0), CTable!(0)
 REDIM SHARED iSTable%(0), iCTable%(0)
 REDIM SHARED camx%(0), camy%(0)
 REDIM SHARED spritecolumn%(0)
 REDIM SHARED txmerk1%(0, 0)
 REDIM SHARED xintmerk%(50)
 REDIM SHARED yintmerk%(50)
 REDIM SHARED gridmerk%(50)

 END
RETURN

resultofexit:
OPEN "booreslt.dat" FOR OUTPUT AS #2
CLOSE #2
OPEN "booreslt.dat" FOR APPEND AS #2
PRINT #2, STR$(score%)
PRINT #2, STR$(health%)
CLOSE #2
RETURN

dieing:
OPEN "booalive.dat" FOR OUTPUT AS #2
CLOSE #2
KILL "booalive.dat"
GOSUB resultofexit
GOSUB fadetored
GOSUB fadeout
GOSUB beenden
RETURN

fadetored:
OUT &H3C7, 0 '! plt regs auslesen
 FOR dunkel% = 0 TO 63
    PALETTE 0, 0
    PALETTE 0, 0
    'OUT &H3C7, 0
    OUT &H3C8, 0
    FOR fade% = 0 TO 255
     red% = plt1%(0, fade%)   '- dunkel%
     IF red% < 0 THEN red% = 0
     green% = plt1%(1, fade%) - dunkel%
     IF green% < 0 THEN green% = 0
     blue% = plt1%(2, fade%) - dunkel%
     IF blue% < 0 THEN blue% = 0
     OUT &H3C9, red%
     OUT &H3C9, green%
     OUT &H3C9, blue%
    NEXT fade%
    NEXT dunkel%
RETURN

pltblack:
 FOR fade% = 0 TO 255
  OUT &H3C9, 0
  OUT &H3C9, 0
  OUT &H3C9, 0
 NEXT fade%
RETURN

wichlevel:
OPEN "booreslt.dat" FOR INPUT AS #2
INPUT #2, score$
score% = VAL(score$)
INPUT #2, health$
health% = VAL(health$)
IF health% > 125 THEN
health% = 0
score% = 0
END IF
CLOSE #2
RETURN

DEFSNG A-Z
FUNCTION DMADone%
Count% = INP(LenPort%)
Count2% = INP(LenPort%)
Count& = CLNG(Count% + 1) * CLNG(Count2% + 1)
IF (Count& - 1) >= &HFFFF& THEN junk% = INP(DSPDataAvail%): DMADone% = -1
END FUNCTION

SUB DMAPlay (Segment&, Offset&, Length&, Freq&)
' Transfers and plays the contents of the buffer.
Length& = Length& - 1
Page% = 0
MemLoc& = Segment& * 16 + Offset&
SELECT CASE Channel%
    CASE 0
       PgPort% = &H87
       AddPort% = &H0
       LenPort% = &H1
       ModeReg% = &H48
    CASE 1
       PgPort% = &H83
       AddPort% = &H2
       LenPort% = &H3
       ModeReg% = &H49
    CASE 2
       PgPort% = &H81
       AddPort% = &H4
       LenPort% = &H5
       ModeReg% = &H4A
    CASE 3
       PgPort% = &H82
       AddPort% = &H6
       LenPort% = &H7
       ModeReg% = &H4B
    CASE ELSE
       PRINT "DMA channels 0-3 only are supported."
       EXIT SUB
END SELECT

OUT &HA, &H4 + Channel%
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
OUT &HA, Channel%

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
      PRINT "You need a Sound Blaster with a DSP v3.x+ to play at high speed."
      EXIT SUB
   END IF
END IF
END SUB

SUB DMAState (StopGo%)
' Stops or continues DMA play.
IF StopGo% THEN WriteDSP &HD4 ELSE WriteDSP &HD0

END SUB

FUNCTION DSPVersion!
' Gets the DSP version.
WriteDSP &HE1
Temp% = ReadDSP%
Temp2% = ReadDSP%
DSPVersion! = VAL(STR$(Temp%) + "." + STR$(Temp2%))
END FUNCTION

SUB GetBLASTER (DMA%, BasePort%, IRQ%)
' This subroutine parses the BLASTER environment string and returns settings.
karte% = 1
IF LEN(ENVIRON$("BLASTER")) = 0 THEN
 ' PRINT "BLASTER environment variable not set."
 karte% = 0
 EXIT SUB
END IF
FOR Length% = 1 TO LEN(ENVIRON$("BLASTER"))
   SELECT CASE MID$(ENVIRON$("BLASTER"), Length%, 1)
      CASE "A"
        BasePort% = &H220    'VAL("&H" + MID$(ENVIRON$("BLASTER"), Length% + 1, 3))
      CASE "I"
        IRQ% = 5  'VAL(MID$(ENVIRON$("BLASTER"), Length% + 1, 1))
      CASE "D"
        DMA% = 1       ' VAL(MID$(ENVIRON$("BLASTER"), Length% + 1, 1))
   END SELECT
NEXT

END SUB

SUB MasterVolume (Right%, Left%, Getvol%)
OUT BasePort% + 4, &H22
'PRINT BasePort%
IF Getvol% THEN
   Left% = INP(BasePort% + 5) \ 16
   Right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (Right% + Left% * 16) AND &HFF
END IF
END SUB

' DEFLNG A-Z............................................
SUB MouseDriver (ax%, bx%, cx%, dx%)
  DEF SEG = VARSEG(mouse$)
  mouse% = SADD(mouse$)
  CALL Absolute(ax%, bx%, cx%, dx%, mouse%)
END SUB

SUB MouseHide
 ax% = 2
 MouseDriver ax%, 0, 0, 0
END SUB

FUNCTION MouseInit%
  ax% = 0
  MouseDriver ax%, 0, 0, 0
  MouseInit% = ax%
END FUNCTION

SUB MousePut (x%, y%)
  ax% = 4
  cx% = x%
  dx% = y%
  MouseDriver ax%, 0, cx%, dx%
END SUB

SUB MouseRange (X1%, Y1%, x2%, Y2%)
  ax% = 7
  cx% = X1%
  dx% = x2%
MouseDriver ax%, 0, cx%, dx%
  ax% = 8
  cx% = Y1%
  dx% = Y2%
  MouseDriver ax%, 0, cx%, dx%
END SUB

SUB MouseShow
  ax% = 1
  MouseDriver ax%, 0, 0, 0
END SUB

SUB MouseStatus (Lb%, Rb%, xMouse%, yMouse%)
  ax% = 3
  MouseDriver ax%, bx%, cx%, dx%
  Lb% = ((bx% AND 1) <> 0)
  Rb% = ((bx% AND 2) <> 0)
  xMouse% = cx%
  yMouse% = dx%
END SUB

FUNCTION ReadDSP%
' Reads a byte from the DSP
DO
LOOP UNTIL INP(BasePort% + 14) AND &H80
ReadDSP% = INP(BasePort% + 10)
END FUNCTION

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

SUB SetStereo (OnOff%)
OUT BasePort% + 4, &HE
IF OnOff% THEN OUT BasePort% + 5, 2 ELSE OUT BasePort% + 5, 0
END SUB

SUB SpeakerState (OnOff%)
' Turns speaker on or off.
IF OnOff% THEN WriteDSP &HD1 ELSE WriteDSP &HD3
END SUB

FUNCTION SpeakerStatus%
OUT BasePort% + 4, &HD8
IF INP(BasePort% + 5) = &HFF THEN SpeakerStatus% = -1 ELSE SpeakerStatus% = 0
END FUNCTION

SUB WriteDSP (byte%)
' Writes a byte to the DSP
DO
LOOP WHILE INP(BasePort% + 12) AND &H80
OUT BasePort% + 12, byte%
END SUB

