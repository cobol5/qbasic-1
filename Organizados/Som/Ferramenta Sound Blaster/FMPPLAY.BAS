DECLARE SUB FMPinit ()
DECLARE SUB PLAYSONG (pfilename$)
DECLARE SUB STOPPLAYSONG ()
DECLARE SUB CONTINUEPLAYSONG ()
DECLARE SUB FMPplaystart (pfilename$)
DECLARE SUB FMPadlport (adressreg%, bdata%)

'Fm pipes player main section
CONST TOTALVOICES = 9
TYPE keybtype: flow AS INTEGER: fhi AS INTEGER: foct AS INTEGER: location AS INTEGER: END TYPE: TYPE voicetype: scancode AS INTEGER: inuse AS INTEGER: delay AS INTEGER: regB0 AS INTEGER: END TYPE
DIM SHARED adlportasm%(512), keyb(128) AS keybtype, totalkeys%, voice(9) AS voicetype, setoldinstrumentsflag%, currentsong$
DIM SHARED kbmatrix%(257), currentkbmatrix%(257), oldkbmatrix%(257), transpose%, playbuffer&(8192), recordbuffer&(8192), playbufferindex&, recordbufferindex&, recorddelaycounter&, playdelaycounter&, keycount&, keyindex&, recordmode%, playmode%
keyDATA:
DATA 0,0,0,0,0,0,0,0,0,0,0,0,21,1,5,27,55,1,5,25,0,0,0,0,114,1,5,22,159,1,5,20,210,1,5,18,0,0,0,0,21,1,6,15,55,1,6,13,0,0,0,0,114,1,6,10,210,1,6,6,238,1,4,29,6,1,5,28,38,1,5,26,74,1,5,24,93,1,5,23,136,1,5,21,184,1,5,19,238,1,5,17,6,1,6,16
DATA 38,1,6,14,74,1,6,12,93,1,6,11,136,1,6,9,184,1,6,7,0,0,0,0,0,0,0,0,21,1,4,39,55,1,4,37,0,0,0,0,114,1,4,34,159,1,4,32,210,1,4,30,0,0,0,0,21,1,5,27,55,1,5,25,93,1,5,23,210,1,4,30,238,1,3,41,159,1,6,8,6,1,4,40,38,1,4,38,74,1,4,36,93,1,4,35
DATA 136,1,4,33,184,1,4,31,238,1,4,29,6,1,5,28,38,1,5,26,74,1,5,24,93,1,5,23,0,0,0,0,0,0,0,0,0,0,0,0,210,1,3,42,-1,-1,-1,-1
adlportDATA:
DATA 85,137,229,80,83,81,82,30,86,6,87,190,6,0,177,4,54,139,26,139,7,54,137,2,70,70,254,201,117,242,22,31,139,86,10,138,70,6,238,185,6,0,236,73,117,252,139,86,12,138,70,8,238,139,86,10,185,35,0,236
DATA 73,117,252,95,7,94,31,90,89,91,88,137,236,93,202,8,-1
FMPinit

PLAYSONG "wingame.fmp"
DO WHILE INKEY$ = ""
  WAIT &H3DA, 8
  WAIT &H3DA, 8, 8
  CONTINUEPLAYSONG
LOOP
STOPPLAYSONG
SYSTEM

SUB CONTINUEPLAYSONG
IF playmode% = 0 THEN EXIT SUB
FOR n% = 0 TO TOTALVOICES - 1
voice(n%).delay = voice(n%).delay + 1
IF voice(n%).delay > 32000 THEN voice(n%).delay = 32000
NEXT
DO WHILE kbmatrix%(128) > 0
saveflag% = 0
keycode% = kbmatrix%(128 + kbmatrix%(128))
originalkeycode% = keycode%
kbmatrix%(128) = kbmatrix%(128) - 1
IF keycode% > 127 THEN
keycode% = keycode% - 128
IF keycode% < totalkeys% THEN
saveflag% = 1
voicenumber% = -1
FOR n% = 0 TO TOTALVOICES - 1
IF voice(n%).scancode = keycode% THEN voicenumber% = n%
NEXT
IF voicenumber% = -1 THEN GOTO FMPskip2
byte% = voice(voicenumber%).regB0
byte% = byte% AND (255 - 32)
FMPadlport &HB0 + voicenumber%, byte%
flag% = 1
FOR n% = 0 TO TOTALVOICES - 1
IF n% <> voicenumber% THEN
IF voice(n%).inuse = 1 AND (keyb(voice(n%).scancode).location = keyb(voice(voicenumber%).scancode).location) THEN flag% = 0
END IF
NEXT
voice(voicenumber%).delay = 0
voice(voicenumber%).inuse = 0
FMPskip2:
END IF
GOTO keyrespondSKIP0
ELSE
IF keycode% < totalkeys% THEN
flag% = 1
FOR n% = 0 TO TOTALVOICES - 1
IF voice(n%).inuse = 1 THEN
IF voice(n%).scancode = keycode% THEN flag% = 0
END IF
NEXT
IF flag% = 1 THEN
saveflag% = 1
unused% = -1
FOR n% = 0 TO TOTALVOICES - 1
IF voice(n%).inuse = 0 THEN unused% = n%
NEXT
largest% = 0
IF unused% = -1 THEN
voicenumber% = -1
FOR n% = 0 TO TOTALVOICES - 1
IF voice(n%).delay > largest% THEN
largest% = voice(n%).delay
voicenumber% = n%
END IF
NEXT
ELSE
voicenumber% = -1
FOR n% = 0 TO TOTALVOICES - 1
IF voice(n%).inuse = 0 THEN
IF voice(n%).delay > largest% THEN
largest% = voice(n%).delay
voicenumber% = n%
END IF
END IF
NEXT
END IF
voice(voicenumber%).scancode = keycode%
voice(voicenumber%).delay = 0
voice(voicenumber%).inuse = 1
scan% = voice(voicenumber%).scancode
lobyte% = keyb(scan%).flow
hibyte% = keyb(scan%).fhi
fr = lobyte% + (hibyte% * 256)
FOR n% = 0 TO 4
fr = fr * 1.05946
NEXT
ffr% = INT(fr)
lobyte% = ffr% AND 255
hibyte% = INT(ffr% / 256)
hibyte% = hibyte% OR ((keyb(scan%).foct + transpose%) * 4)
hibyte% = hibyte% OR 32
FMPadlport &HA0 + voicenumber%, lobyte%
FMPadlport &HB0 + voicenumber%, hibyte%
voice(voicenumber%).regB0 = hibyte%
END IF
GOTO keyrespondSKIP0
END IF
END IF
SELECT CASE keycode%
CASE 74
saveflag% = 1
transpose% = transpose% - 1
IF transpose% < -4 THEN transpose% = -4
CASE 78
saveflag% = 1
transpose% = transpose% + 1
IF transpose% > 1 THEN transpose% = 1
END SELECT
keyrespondSKIP0:
IF recordmode% = 1 AND saveflag% = 1 THEN
DEF SEG = VARSEG(recordbuffer&(0))
POKE recordbufferindex&, recorddelaycounter&
POKE recordbufferindex& + 1, originalkeycode%
DEF SEG
recordbufferindex& = recordbufferindex& + 2
keycount& = keycount& + 1
recorddelaycounter& = 0
END IF
keyrespondSKIP1:
LOOP
FOR n% = 0 TO TOTALVOICES - 1
IF voice(n%).inuse = 1 THEN
IF kbmatrix%(voice(n%).scancode) = 0 THEN
byte% = voice(n%).regB0
byte% = byte% AND (255 - 32)
FMPadlport &HB0 + n%, byte%
flag% = 1
FOR n2% = 0 TO TOTALVOICES - 1
IF n2% <> n% THEN
IF voice(n2%).inuse = 1 AND (keyb(voice(n2%).scancode).location = keyb(voice(n%).scancode).location) THEN flag% = 0
END IF
NEXT
voice(n%).delay = 0
voice(n%).inuse = 0
END IF
END IF
NEXT
GOSUB fmkeysDOPLAY
EXIT SUB

fmkeysDOPLAY:
playdelaycounter& = playdelaycounter& + 1
BACK0:
DEF SEG = VARSEG(playbuffer&(0))
IF playdelaycounter& >= PEEK(playbufferindex&) THEN
i% = PEEK(playbufferindex& + 1)
n% = kbmatrix%(128)
kbmatrix%(n% + 129) = i%
kbmatrix%(128) = n% + 1
IF i% < 128 THEN
kbmatrix%(i%) = 1
ELSE
kbmatrix%(i% - 128) = 0
END IF
DEF SEG
keyindex& = keyindex& + 1
IF keyindex& >= playbuffer&(3) THEN PLAYSONG currentsong$
playbufferindex& = playbufferindex& + 2
playdelaycounter& = 0
IF playmode% = 1 THEN GOTO BACK0
END IF
DEF SEG
RETURN

END SUB

SUB FMPadlport (adressreg%, bdata%)
DEF SEG = VARSEG(adlportasm%(0))
p1% = &H389
p2% = &H388
p3% = bdata%
p4% = adressreg%
CALL ABSOLUTE(p1%, p2%, p3%, p4%, 0)
DEF SEG
END SUB

SUB FMPinit
RESTORE keyDATA
index% = 0
GOTO FMPINITskip0
DO
keyb(index%).flow = a%
keyb(index%).fhi = b%
keyb(index%).foct = c%
keyb(index%).location = d%
index% = index% + 1
FMPINITskip0:
READ a%, b%, c%, d%
LOOP UNTIL a% = -1
totalkeys% = index%
RESTORE adlportDATA
DEF SEG = VARSEG(adlportasm%(0))
i% = VARPTR(adlportasm%(0))
GOTO FMPINITskip1
DO
POKE i%, q%
i% = i% + 1
FMPINITskip1:
READ q%
LOOP UNTIL q% = -1
DEF SEG
END SUB

SUB FMPplaystart (pfilename$)
DEF SEG = VARSEG(playbuffer&(0))
BLOAD pfilename$, 0
DEF SEG
poffset& = 0
psegment& = VARSEG(playbuffer&(0))
FOR n% = 0 TO 244: FMPadlport n%, 0: NEXT
FMPadlport 1, 32
DEF SEG = psegment&
byte% = PEEK(0 + poffset&)
basead% = &H20
FMPadlport basead% + 0, byte%
FMPadlport basead% + 1, byte%
FMPadlport basead% + 2, byte%
FMPadlport basead% + 8, byte%
FMPadlport basead% + 9, byte%
FMPadlport basead% + 10, byte%
FMPadlport basead% + 16, byte%
FMPadlport basead% + 17, byte%
FMPadlport basead% + 18, byte%
DEF SEG = psegment&
byte% = PEEK(1 + poffset&)
basead% = &H20
FMPadlport basead% + 3, byte%
FMPadlport basead% + 4, byte%
FMPadlport basead% + 5, byte%
FMPadlport basead% + 11, byte%
FMPadlport basead% + 12, byte%
FMPadlport basead% + 13, byte%
FMPadlport basead% + 19, byte%
FMPadlport basead% + 20, byte%
FMPadlport basead% + 21, byte%
DEF SEG = psegment&
byte% = PEEK(2 + poffset&)
basead% = &H40
FMPadlport basead% + 0, byte%
FMPadlport basead% + 1, byte%
FMPadlport basead% + 2, byte%
FMPadlport basead% + 8, byte%
FMPadlport basead% + 9, byte%
FMPadlport basead% + 10, byte%
FMPadlport basead% + 16, byte%
FMPadlport basead% + 17, byte%
FMPadlport basead% + 18, byte%
DEF SEG = psegment&
byte% = PEEK(3 + poffset&)
basead% = &H40
FMPadlport basead% + 3, byte%
FMPadlport basead% + 4, byte%
FMPadlport basead% + 5, byte%
FMPadlport basead% + 11, byte%
FMPadlport basead% + 12, byte%
FMPadlport basead% + 13, byte%
FMPadlport basead% + 19, byte%
FMPadlport basead% + 20, byte%
FMPadlport basead% + 21, byte%
DEF SEG = psegment&
byte% = PEEK(4 + poffset&)
basead% = &H60
FMPadlport basead% + 0, byte%
FMPadlport basead% + 1, byte%
FMPadlport basead% + 2, byte%
FMPadlport basead% + 8, byte%
FMPadlport basead% + 9, byte%
FMPadlport basead% + 10, byte%
FMPadlport basead% + 16, byte%
FMPadlport basead% + 17, byte%
FMPadlport basead% + 18, byte%
DEF SEG = psegment&
byte% = PEEK(5 + poffset&)
basead% = &H60
FMPadlport basead% + 3, byte%
FMPadlport basead% + 4, byte%
FMPadlport basead% + 5, byte%
FMPadlport basead% + 11, byte%
FMPadlport basead% + 12, byte%
FMPadlport basead% + 13, byte%
FMPadlport basead% + 19, byte%
FMPadlport basead% + 20, byte%
FMPadlport basead% + 21, byte%
DEF SEG = psegment&
byte% = PEEK(6 + poffset&)
basead% = &H80
FMPadlport basead% + 0, byte%
FMPadlport basead% + 1, byte%
FMPadlport basead% + 2, byte%
FMPadlport basead% + 8, byte%
FMPadlport basead% + 9, byte%
FMPadlport basead% + 10, byte%
FMPadlport basead% + 16, byte%
FMPadlport basead% + 17, byte%
FMPadlport basead% + 18, byte%
DEF SEG = psegment&
byte% = PEEK(7 + poffset&)
basead% = &H80
FMPadlport basead% + 3, byte%
FMPadlport basead% + 4, byte%
FMPadlport basead% + 5, byte%
FMPadlport basead% + 11, byte%
FMPadlport basead% + 12, byte%
FMPadlport basead% + 13, byte%
FMPadlport basead% + 19, byte%
FMPadlport basead% + 20, byte%
FMPadlport basead% + 21, byte%
DEF SEG = psegment&
byte% = PEEK(8 + poffset&)
basead% = &HE0
FMPadlport basead% + 0, byte%
FMPadlport basead% + 1, byte%
FMPadlport basead% + 2, byte%
FMPadlport basead% + 8, byte%
FMPadlport basead% + 9, byte%
FMPadlport basead% + 10, byte%
FMPadlport basead% + 16, byte%
FMPadlport basead% + 17, byte%
FMPadlport basead% + 18, byte%
DEF SEG = psegment&
byte% = PEEK(9 + poffset&)
basead% = &HE0
FMPadlport basead% + 3, byte%
FMPadlport basead% + 4, byte%
FMPadlport basead% + 5, byte%
FMPadlport basead% + 11, byte%
FMPadlport basead% + 12, byte%
FMPadlport basead% + 13, byte%
FMPadlport basead% + 19, byte%
FMPadlport basead% + 20, byte%
FMPadlport basead% + 21, byte%
DEF SEG = psegment&
byte% = PEEK(10 + poffset&)
basead% = &HC0
FMPadlport basead%, byte%
FMPadlport basead% + 1, byte%
FMPadlport basead% + 2, byte%
FMPadlport basead% + 3, byte%
FMPadlport basead% + 4, byte%
FMPadlport basead% + 5, byte%
FMPadlport basead% + 6, byte%
FMPadlport basead% + 7, byte%
FMPadlport basead% + 8, byte%
DEF SEG
DEF SEG = VARSEG(playbuffer&(0))
transpose% = PEEK(11) - 4
DEF SEG
IF transpose% < -4 THEN transpose% = -4
IF transpose% > 1 THEN transpose% = 1
playmode% = 1
keyindex& = 0
playdelaycounter& = 0
playbufferindex& = 16
IF playbuffer&(3) = 0 THEN STOPPLAYSONG
END SUB

SUB PLAYSONG (pfilename$)
currentsong$ = pfilename$
FOR n% = 0 TO 244: FMPadlport n%, 0: NEXT
FOR n% = 0 TO TOTALVOICES - 1: voice(n%).inuse = 0: voice(n%).delay = 0: NEXT
FOR i% = 0 TO 127: kbmatrix%(i%) = 0: NEXT
FMPplaystart (pfilename$)
END SUB

SUB STOPPLAYSONG
playmode% = 0
FOR n% = 0 TO 244: FMPadlport n%, 0: NEXT
END SUB

