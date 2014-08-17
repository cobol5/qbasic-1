DECLARE SUB clearrecordmessage ()
DECLARE SUB clearplaymessage ()
DECLARE SUB clearkbbuffer ()
DECLARE SUB setnewoldinst (psegment&, poffset&)
DECLARE SUB setnewinstruments ()
DECLARE SUB setoldinstruments ()
DECLARE SUB playstart ()
DECLARE SUB playstop ()
DECLARE SUB saverecording ()
DECLARE SUB recordstop ()
DECLARE SUB recordstart ()
DECLARE SUB updatenotesdown ()
DECLARE SUB updatescreen ()
DECLARE SUB showtitle ()
DECLARE SUB testreset ()
DECLARE SUB voiceon (vnumber%)
DECLARE SUB voiceoff (vnumber%)
DECLARE SUB letgokey (scode%)
DECLARE SUB presskey (scode%)
DECLARE SUB keytrackdelays ()
DECLARE SUB keyrespond ()
DECLARE SUB onkb ()
DECLARE SUB offkb ()
DECLARE SUB fmkeys ()
DECLARE SUB resetvoicearray ()
DECLARE SUB modulatorouts (basead%, byte%)
DECLARE SUB carrierouts (basead%, byte%)
DECLARE SUB setinstruments ()
DECLARE SUB readkeydata ()
DECLARE SUB adlport (adressreg%, bdata%)
DECLARE SUB adlport (adressreg%, bdata%)
DECLARE SUB resetfmcard ()
DECLARE SUB loadassembly ()
CLS
SCREEN 0
WIDTH 80, 50

DIM SHARED recordfilename$, playfilename$

recordfilename$ = "inship.fmp"
playfilename$ = "inship.fmp"








'*****************************************************************************
DIM SHARED asmfile1$, asmfile2$
asmfile1$ = "keyb.prg"
asmfile2$ = "adlport.prg"
'*****************************************************************************

'Define constants
CONST ADRESSPORT = &H388
CONST DATAPORT = &H389
CONST TOTALVOICES = 9

'Define types
TYPE keybtype
  flow AS INTEGER
  fhi AS INTEGER
  foct AS INTEGER
  location AS INTEGER
END TYPE

TYPE voicetype
    scancode AS INTEGER             'The scancode of the key using this voice
    inuse AS INTEGER
    delay AS INTEGER             'The number of loop cycles since last used
                                     'Clamp this to 32000
    regB0 AS INTEGER
END TYPE

'Define arrays
'Assembly language
DIM SHARED asm%(512)
'Key management
DIM SHARED keyb(128) AS keybtype
DIM SHARED keyname$(128)
DIM SHARED totalkeys%
DIM SHARED voice(9) AS voicetype
'Keyboard interrupt management
DIM SHARED kbcontrol%(1024)
DIM SHARED kbmatrix%(257)
DIM SHARED currentkbmatrix%(257)
DIM SHARED oldkbmatrix%(257)
'Transposing variables
DIM SHARED transpose%
'Play/Record buffer
DIM SHARED playbuffer&(8192)
DIM SHARED recordbuffer&(8192)
DIM SHARED playbufferindex&, recordbufferindex&
DIM SHARED recorddelaycounter&, playdelaycounter&
DIM SHARED keycount&, keyindex&
DIM SHARED recordmode%, playmode%
DIM SHARED instrument%(8)
DIM SHARED setoldinstrumentsflag%

showtitle

loadassembly
resetfmcard

readkeydata
setinstruments
resetvoicearray
onkb

fmkeys

offkb
resetfmcard
clearrecordmessage
clearplaymessage

IF recordmode% = 1 THEN recordstop
IF playmode% = 1 THEN playstop

SYSTEM

keyDATA:
'Data for keys
'     flow      fhigh     octative     screenlocation
DATA 0,0,0,0
DATA 0,0,0,0  : 'ESC
DATA 0,0,0,0  : '1
DATA 21,1,5,27  : '2
DATA 55,1,5,25  : '3
DATA 0,0,0,0  : '4
DATA 114,1,5,22  : '5
DATA 159,1,5,20  : '6
DATA 210,1,5,18  : '7
DATA 0,0,0,0  : '8
DATA 21,1,6,15  : '9
DATA 55,1,6,13  : '0
DATA 0,0,0,0  : '-
DATA 114,1,6,10  : '=
DATA 210,1,6,6  : 'backspace
DATA 238,1,4,29  : 'tab
DATA 6,1,5,28  : 'q
DATA 38,1,5,26  : 'w
DATA 74,1,5,24  : 'e
DATA 93,1,5,23  : 'r
DATA 136,1,5,21  : 't
DATA 184,1,5,19  : 'y
DATA 238,1,5,17  : 'u
DATA 6,1,6,16  : 'i
DATA 38,1,6,14  : 'o
DATA 74,1,6,12  : 'p
DATA 93,1,6,11  : '[
DATA 136,1,6,9  : ']
DATA 184,1,6,7  : 'enter
DATA 0,0,0,0  : '
DATA 0,0,0,0  : 'a
DATA 21,1,4,39 : 's
DATA 55,1,4,37  : 'd
DATA 0,0,0,0  : 'f
DATA 114,1,4,34  : 'g
DATA 159,1,4,32  : 'h
DATA 210,1,4,30  : 'j
DATA 0,0,0,0 : 'k
DATA 21,1,5,27  : 'l
DATA 55,1,5,25 : ';
DATA 93,1,5,23 : ''
DATA 210,1,4,30  : '`
DATA 238,1,3,41  : 'left shift
DATA 159,1,6,8  : '\
DATA 6,1,4,40   : 'z
DATA 38,1,4,38  : 'x
DATA 74,1,4,36  : 'c
DATA 93,1,4,35  : 'v
DATA 136,1,4,33 : 'b
DATA 184,1,4,31 : 'n
DATA 238,1,4,29 : 'm
DATA 6,1,5,28  : ',
DATA 38,1,5,26  : '.
DATA 74,1,5,24  : '?
DATA 93,1,5,23 : 'right shift - scanm: 54
DATA 0,0,0,0  : 'scan: 55
DATA 0,0,0,0  : 'scan: 56
DATA 0,0,0,0  : 'scan: 57
DATA 210,1,3,42  : 'scan: 58
DATA -1,-1,-1,-1
   
keynameDATA:
'Data for names of keys
DATA ""
DATA ""
DATA ""  : '1
DATA "mid c#"  : '2
DATA "mid d#"  : '3
DATA ""  : '4
DATA "mid f#"  : '5
DATA "mid g#"  : '6
DATA "mid a#"  : '7
DATA ""  : '8
DATA "hi c#"  : '9
DATA "hi d#"  : '0
DATA ""  : '-
DATA "hi f#"  : '=
DATA "hi a#"  : 'backspace
DATA "low b"  : 'tab
DATA "mid c"  : 'q
DATA "mid d" : 'w
DATA "mid e"  : 'e
DATA "mid f"  : 'r
DATA "mid g"  : 't
DATA "mid a"  : 'y
DATA "mid b"  : 'u
DATA "hi c"  : 'i
DATA "hi d"  : 'o
DATA "hi e"  : 'p
DATA "hi f"  : '[
DATA "hi g"  : ']
DATA "hi a"  : 'enter
DATA ""  : '
DATA ""  : 'a
DATA "low c#" : 's
DATA "low d#"  : 'd
DATA ""  : 'f
DATA "low f#"  : 'g
DATA "low g#"  : 'h
DATA "low a#"  : 'j
DATA "" : 'k
DATA "mid c#"  : 'l
DATA "mid d#"  : ';
DATA "mid f"  : ''
DATA "low a#"  : '`
DATA "deep b"  : 'left shift
DATA "hi g#"  : '\
DATA "low c"   : 'z
DATA "low d"  : 'x
DATA "low e"  : 'c
DATA "low f"  : 'v
DATA "low g" : 'b
DATA "low a" : 'n
DATA "low b" : 'm
DATA "mid c"  : ',
DATA "mid d"  : '.
DATA "mid e"  : '?
DATA "mid f"  : 'right shift
DATA "" : 'scan: 55
DATA "" : 'scan: 56
DATA "" : 'scan: 57
DATA "deep a#" : 'scan: 58
DATA "XXXXXXXX.XXX"

instrumentDATA:
'Data for instrument
'Modulator Sound Characteristic
DATA &He1
'Carrier Sound Characteristic
DATA &He0
'Modulator Scaling/Output Level
DATA 20
'Carrier Scaling/Output Level
DATA 0
'Modulator Attack/Decay
DATA &H21
'Carrier Attack/Decay
DATA &Ha2
'Modulator Sustain/Release
DATA &Hc4
'Carrier Sustain/Release
DATA &Hf3
'Modulator Wave Select
DATA 0
'Carrier Wave Select
DATA 2
'Feedback/Connection
DATA 0

'Writes out to an Adlib FM port.  Calls assembly language to do this.
SUB adlport (adressreg%, bdata%)

DEF SEG = VARSEG(asm%(0))
p1% = DATAPORT
p2% = ADRESSPORT
p3% = bdata%
p4% = adressreg%
CALL ABSOLUTE(p1%, p2%, p3%, p4%, 0)
DEF SEG

END SUB

SUB carrierouts (basead%, byte%)

adlport basead% + 3, byte%
adlport basead% + 4, byte%
adlport basead% + 5, byte%
adlport basead% + 11, byte%
adlport basead% + 12, byte%
adlport basead% + 13, byte%
adlport basead% + 19, byte%
adlport basead% + 20, byte%
adlport basead% + 21, byte%

END SUB

SUB clearkbbuffer
FOR i% = 0 TO 127: kbmatrix%(i%) = 0: NEXT
END SUB

SUB clearplaymessage
LOCATE 19, 32
COLOR 7, 0
PRINT "                "
END SUB

SUB clearrecordmessage
LOCATE 18, 32
COLOR 7, 0
PRINT "                "
END SUB

'The sub with the synthesiser loop
SUB fmkeys

'Do some initializing

GOTO fmkeysSKIP0
DO
    keytrackdelays
    keyrespond

    IF recordmode% = 1 THEN GOSUB fmkeysDORECORD
    IF playmode% = 1 THEN GOSUB fmkeysDOPLAY
   
    WAIT &H3DA, 8
    WAIT &H3DA, 8, 8
fmkeysSKIP0:
LOOP UNTIL kbmatrix%(1) = 1     'Loop until ESC is pressed

EXIT SUB


fmkeysDORECORD:
recorddelaycounter& = recorddelaycounter& + 1
recorddelaycounter& = recorddelaycounter& AND 255
IF recorddelaycounter& = 0 THEN
  DEF SEG = VARSEG(recordbuffer&(0))
  POKE recordbufferindex&, 255
  POKE recordbufferindex& + 1, 0
  recordbufferindex& = recordbufferindex& + 2
  keycount& = keycount& + 1
  DEF SEG
END IF
RETURN

fmkeysDOPLAY:
playdelaycounter& = playdelaycounter& + 1
BACK0:
DEF SEG = VARSEG(playbuffer&(0))
IF playdelaycounter& >= PEEK(playbufferindex&) THEN

  'Adjust keyboard buffer appropriately
  i% = PEEK(playbufferindex& + 1)
  n% = kbmatrix%(128)
  kbmatrix%(n% + 129) = i%
  kbmatrix%(128) = n% + 1
 
  'Adjust keyboard matrix appropriately
  IF i% < 128 THEN
    kbmatrix%(i%) = 1
  ELSE
    kbmatrix%(i% - 128) = 0
  END IF
  DEF SEG

  'Stop if all keys have been run through
  keyindex& = keyindex& + 1
  IF keyindex& >= playbuffer&(3) THEN playstop

  'Go to next entry in index
  playbufferindex& = playbufferindex& + 2
 
  'Reset delay counter flag
  playdelaycounter& = 0
 
  'See if any other key has to be processed
  IF playmode% = 1 THEN GOTO BACK0
END IF

'Restore basic segment
DEF SEG

RETURN

END SUB

'Responds to key presses and releases
SUB keyrespond

'Respond to key presses and releases
DO WHILE kbmatrix%(128) > 0
    saveflag% = 0
    keycode% = kbmatrix%(128 + kbmatrix%(128))
    originalkeycode% = keycode%
    kbmatrix%(128) = kbmatrix%(128) - 1
    
    'Set old instruments after play is complete
    'IF setoldinstrumentsflag% = 1 AND keycode% < 128 THEN
    '  setoldinstruments
    '  setoldinstrumentsflag% = 0
    'END IF

    'A valid key was pressed
    IF keycode% > 127 THEN
        keycode% = keycode% - 128
        IF keycode% < totalkeys% THEN letgokey keycode%: saveflag% = 1
        GOTO keyrespondSKIP0
    ELSE
        IF keycode% < totalkeys% THEN
            flag% = 1
            FOR n% = 0 TO TOTALVOICES - 1
                IF voice(n%).inuse = 1 THEN
                    IF voice(n%).scancode = keycode% THEN flag% = 0
                END IF
            NEXT
            IF flag% = 1 THEN presskey keycode%: saveflag% = 1
            GOTO keyrespondSKIP0
        END IF
    END IF
    SELECT CASE keycode%
    CASE 74   '-
        saveflag% = 1
        transpose% = transpose% - 1
        IF transpose% < -4 THEN transpose% = -4
        updatescreen
    CASE 78   '+
        saveflag% = 1
        transpose% = transpose% + 1
        IF transpose% > 1 THEN transpose% = 1
        updatescreen
    CASE 73   'page up
        IF recordmode% = 0 THEN
          recordstart
          GOTO keyrespondSKIP1
        ELSE
          recordstop
          GOTO keyrespondSKIP1
        END IF
    CASE 77   '6 (numeric keypad)
        IF playmode% = 0 THEN
          playstart
        ELSE
          playstop
        END IF
    CASE 81   '3 (numeric keypad)
        IF recordmode% = 0 AND playmode% = 0 THEN
          resetfmcard
          setoldinstruments
        END IF
        IF recordmode% = 1 THEN recordstop
        IF playmode% = 1 THEN playstop
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

'Clear up all voices that are allocated with no keys down
FOR n% = 0 TO TOTALVOICES - 1
    IF voice(n%).inuse = 1 THEN
        IF kbmatrix%(voice(n%).scancode) = 0 THEN
            'Turn the voice off
            voiceoff n%
           
            'Reset the data structure of the voice
            voice(n%).delay = 0
            voice(n%).inuse = 0
        END IF
    END IF
NEXT

END SUB

'Runs through the keys and adds a delay to the unused ones
SUB keytrackdelays

FOR n% = 0 TO TOTALVOICES - 1
    voice(n%).delay = voice(n%).delay + 1
    IF voice(n%).delay > 32000 THEN voice(n%).delay = 32000
NEXT

END SUB

'This is called when a key is released
SUB letgokey (scode%)

voicenumber% = -1
'Find the voice to be turned off
FOR n% = 0 TO TOTALVOICES - 1
    IF voice(n%).scancode = scode% THEN voicenumber% = n%
NEXT

'Leave if there is no voice using that key
IF voicenumber% = -1 THEN EXIT SUB

'Turn the voice off
voiceoff voicenumber%

'Reset the data structure of the voice
voice(voicenumber%).delay = 0
voice(voicenumber%).inuse = 0

END SUB

SUB loadassembly

'Load assembly language
    'Load adlib port interface
DEF SEG = VARSEG(asm%(0))
BLOAD "adlport.rtn", 0
DEF SEG

'Load keyboard interrupt
DEF SEG = VARSEG(kbcontrol%(0))
BLOAD asmfile1$, 0
kbcontrol%(128) = VARSEG(kbmatrix%(0))
kbcontrol%(129) = VARPTR(kbmatrix%(0))
DEF SEG

END SUB

SUB modulatorouts (basead%, byte%)

adlport basead% + 0, byte%
adlport basead% + 1, byte%
adlport basead% + 2, byte%
adlport basead% + 8, byte%
adlport basead% + 9, byte%
adlport basead% + 10, byte%
adlport basead% + 16, byte%
adlport basead% + 17, byte%
adlport basead% + 18, byte%

END SUB

'Re-Installs the DOS keyboard handler
SUB offkb

DEF SEG = VARSEG(kbcontrol%(0))
CALL ABSOLUTE(&H1B0)
DEF SEG

END SUB

'Installs the custom keyboard handler
SUB onkb

DEF SEG = VARSEG(kbcontrol%(0))
CALL ABSOLUTE(&H180)
DEF SEG

END SUB

SUB playstart

'Display "Playing" message on screen
LOCATE 19, 32
COLOR 10
PRINT "Playing"

'Load up FMP track and instruments
DEF SEG = VARSEG(playbuffer&(0))
BLOAD playfilename$, 0
DEF SEG

'Set new instruments
setnewinstruments

'Set transpose level from play file
DEF SEG = VARSEG(playbuffer&(0))
transpose% = PEEK(11) - 4
DEF SEG
IF transpose% < -4 THEN transpose% = -4
IF transpose% > 1 THEN transpose% = 1
updatescreen

'Set flags and index variables
playmode% = 1
keyindex& = 0
playdelaycounter& = 0
playbufferindex& = 16

'Stop playing if there are 0 keys
IF playbuffer&(3) = 0 THEN playstop

END SUB

SUB playstop
playmode% = 0
clearplaymessage
END SUB

'Allocates a voice for use with the key
SUB presskey (scode%)

unused% = -1
'Look for an unused channel
FOR n% = 0 TO TOTALVOICES - 1
    IF voice(n%).inuse = 0 THEN unused% = n%
NEXT

largest% = 0
'If an unused channel wasn't found, use the channel with the longest delay.
IF unused% = -1 THEN
    voicenumber% = -1
    FOR n% = 0 TO TOTALVOICES - 1
        IF voice(n%).delay > largest% THEN
            largest% = voice(n%).delay
            voicenumber% = n%
        END IF
    NEXT
ELSE
    'If an unused channel was found, use the channel with the longest delay
    '   anyway, on the condition that it's not being used.
    voicenumber% = -1
    FOR n% = 0 TO TOTALVOICES - 1
        IF voice(n%).inuse = 0 THEN
            IF voice(n%).delay > largest% THEN
                largest% = voice(n%).delay
                voicenumber% = n%
            END IF
            'If the unused channel has the same scan code, use it.
            'IF voice(n%).scancode = scode% THEN voicenumber% = n%: EXIT FOR
        END IF
    NEXT
END IF

'Tell voice which key is using it
voice(voicenumber%).scancode = scode%

'Reset the voice's delay
voice(voicenumber%).delay = 0

'Tell system the voice is being used
voice(voicenumber%).inuse = 1

'Start the note on the voice
    'Turn on the voice
voiceon voicenumber%

END SUB

'Reads in information about each key on the keyboard.
SUB readkeydata

'Setup loop:
    'Make sure we're reading the key data
RESTORE keyDATA
    'Reset array index
index% = 0
GOTO readkeydataSKIP0
'Loop until a -1,-1,-1 is reached
DO
    'Write the information into an array
    keyb(index%).flow = a%
    keyb(index%).fhi = b%
    keyb(index%).foct = c%
    keyb(index%).location = d%
    'Increment array index
    index% = index% + 1
readkeydataSKIP0:
    'Read in some more data
    READ a%, b%, c%, d%
LOOP UNTIL a% = -1

totalkeys% = index%


'Read key descriptions
'Setup loop:
    'Make sure we're reading the key data
RESTORE keynameDATA
    'Reset array index
index% = 0
GOTO readkeydataSKIP1
'Loop until a$ = "XXXXXXXX.XXX" is reached
DO
    'Write the information into an array
    keyname$(index%) = a$
    'Increment array index
    index% = index% + 1
readkeydataSKIP1:
    'Read in some more data
    READ a$
LOOP UNTIL a$ = "XXXXXXXX.XXX"

END SUB

SUB recordstart

'Display "Recording" message on screen
LOCATE 18, 32
COLOR 10
'PRINT "Recording"

'Set flags and index variables
recordmode% = 1
playmode% = 0
keycount& = 0
recorddelaycounter& = 0
recordbufferindex& = 16

'Save instrument data in header
FOR i% = 0 TO 10
  DEF SEG = VARSEG(instrument%(0))
  byte% = PEEK(VARPTR(instrument%(0)) + i%)
  DEF SEG = VARSEG(recordbuffer&(0))
  POKE i%, byte%
NEXT
DEF SEG

'Save transpose level in header
DEF SEG = VARSEG(recordbuffer&(0))
POKE 11, transpose% + 4
DEF SEG

END SUB

SUB recordstop
recordmode% = 0
saverecording
clearrecordmessage
END SUB

SUB resetfmcard

'Reset fm card
FOR n% = 0 TO 244
  adlport n%, 0
NEXT

END SUB

SUB resetvoicearray

FOR n% = 0 TO TOTALVOICES - 1
    voice(n%).inuse = 0
    voice(n%).delay = 0
NEXT

END SUB

SUB saverecording
recordbuffer&(3) = keycount&
DEF SEG = VARSEG(recordbuffer&(0))
BSAVE recordfilename$, 0, 16 + (keycount& * 2)
DEF SEG
END SUB

'Sets up all nine music channels with a single instrument (for now)
SUB setinstruments

'Enable waveform switching
adlport 1, 32

'Make sure we're reading instrument data
RESTORE instrumentDATA

'Set Modulator Sound Characteristic
READ byte%
basead% = &H20
modulatorouts basead%, (byte%)
DEF SEG = VARSEG(instrument%(0))
POKE VARPTR(instrument%(0)) + 0, byte%
DEF SEG

'Set Carrier Sound Characteristic
READ byte%
basead% = &H20
carrierouts basead%, (byte%)
DEF SEG = VARSEG(instrument%(0))
POKE VARPTR(instrument%(0)) + 1, byte%
DEF SEG

'Set Modulator Scaling/Output Level
READ byte%
basead% = &H40
modulatorouts basead%, (byte%)
DEF SEG = VARSEG(instrument%(0))
POKE VARPTR(instrument%(0)) + 2, byte%
DEF SEG

'Set Carrier Scaling/Output Level
READ byte%
basead% = &H40
carrierouts basead%, (byte%)
DEF SEG = VARSEG(instrument%(0))
POKE VARPTR(instrument%(0)) + 3, byte%
DEF SEG

'Set Modulator Attack/Decay
READ byte%
basead% = &H60
modulatorouts basead%, (byte%)
DEF SEG = VARSEG(instrument%(0))
POKE VARPTR(instrument%(0)) + 4, byte%
DEF SEG

'Set Carrier Attack/Decay
READ byte%
basead% = &H60
carrierouts basead%, (byte%)
DEF SEG = VARSEG(instrument%(0))
POKE VARPTR(instrument%(0)) + 5, byte%
DEF SEG

'Set Modulator Sustain/Release
READ byte%
basead% = &H80
modulatorouts basead%, (byte%)
DEF SEG = VARSEG(instrument%(0))
POKE VARPTR(instrument%(0)) + 6, byte%
DEF SEG

'Set Carrier Sustain/Release
READ byte%
basead% = &H80
carrierouts basead%, (byte%)
DEF SEG = VARSEG(instrument%(0))
POKE VARPTR(instrument%(0)) + 7, byte%
DEF SEG

'Set Modulator Wave Select
READ byte%
basead% = &HE0
modulatorouts basead%, (byte%)
DEF SEG = VARSEG(instrument%(0))
POKE VARPTR(instrument%(0)) + 8, byte%
DEF SEG

'Set Carrier Wave Select
READ byte%
basead% = &HE0
carrierouts basead%, (byte%)
DEF SEG = VARSEG(instrument%(0))
POKE VARPTR(instrument%(0)) + 9, byte%
DEF SEG

'Set Feedback/Connection
READ byte%
basead% = &HC0
adlport basead%, byte%
adlport basead% + 1, byte%
adlport basead% + 2, byte%
adlport basead% + 3, byte%
adlport basead% + 4, byte%
adlport basead% + 5, byte%
adlport basead% + 6, byte%
adlport basead% + 7, byte%
adlport basead% + 8, byte%
DEF SEG = VARSEG(instrument%(0))
POKE VARPTR(instrument%(0)) + 10, byte%
DEF SEG

END SUB

SUB setnewinstruments
'Access play buffer memory
setnewoldinst VARSEG(playbuffer&(0)), 0
END SUB

SUB setnewoldinst (psegment&, poffset&)

resetfmcard
adlport 1, 32

'Set Modulator Sound Characteristic
DEF SEG = psegment&
byte% = PEEK(0 + poffset&)
basead% = &H20
modulatorouts basead%, (byte%)

'Set Carrier Sound Characteristic
DEF SEG = psegment&
byte% = PEEK(1 + poffset&)
basead% = &H20
carrierouts basead%, (byte%)

'Set Modulator Scaling/Output Level
DEF SEG = psegment&
byte% = PEEK(2 + poffset&)
basead% = &H40
modulatorouts basead%, (byte%)

'Set Carrier Scaling/Output Level
DEF SEG = psegment&
byte% = PEEK(3 + poffset&)
basead% = &H40
carrierouts basead%, (byte%)

'Set Modulator Attack/Decay
DEF SEG = psegment&
byte% = PEEK(4 + poffset&)
basead% = &H60
modulatorouts basead%, (byte%)

'Set Carrier Attack/Decay
DEF SEG = psegment&
byte% = PEEK(5 + poffset&)
basead% = &H60
carrierouts basead%, (byte%)

'Set Modulator Sustain/Release
DEF SEG = psegment&
byte% = PEEK(6 + poffset&)
basead% = &H80
modulatorouts basead%, (byte%)

'Set Carrier Sustain/Release
DEF SEG = psegment&
byte% = PEEK(7 + poffset&)
basead% = &H80
carrierouts basead%, (byte%)

'Set Modulator Wave Select
DEF SEG = psegment&
byte% = PEEK(8 + poffset&)
basead% = &HE0
modulatorouts basead%, (byte%)

'Set Carrier Wave Select
DEF SEG = psegment&
byte% = PEEK(9 + poffset&)
basead% = &HE0
carrierouts basead%, (byte%)

'Set Feedback/Connection
DEF SEG = psegment&
byte% = PEEK(10 + poffset&)
basead% = &HC0
adlport basead%, byte%
adlport basead% + 1, byte%
adlport basead% + 2, byte%
adlport basead% + 3, byte%
adlport basead% + 4, byte%
adlport basead% + 5, byte%
adlport basead% + 6, byte%
adlport basead% + 7, byte%
adlport basead% + 8, byte%

'Restore basic segment
DEF SEG

END SUB

SUB setoldinstruments
'Access play buffer memory
setnewoldinst VARSEG(instrument%(0)), VARPTR(instrument%(0))
END SUB

SUB showtitle

CLS
LOCATE 1, 30
COLOR 7
PRINT "FM PIPES by Milo Sedlacek"

LOCATE 12, 32
PRINT "+ for octative up"
LOCATE 13, 32
PRINT "- for octative down"
LOCATE 14, 32
PRINT "9 = record in: "; UCASE$(recordfilename$)
LOCATE 15, 32
PRINT "6 = play from: "; UCASE$(playfilename$)
LOCATE 16, 32
PRINT "3 = stop record or play"
LOCATE 17, 32
PRINT "ESC = exit"
updatescreen
END SUB

SUB testreset

adlport 1, 32   'wave select on
adlport &H20, 32    'modulator sound type
adlport &H23, 32    'carrier sound type
adlport &H40, 0     'modulator total level
adlport &H43, 0     'carrier total level
adlport &H60, 1     'modulator attack/decay
adlport &H63, 1     'carrier attack/decay
adlport &H80, 3     'modulator sustain/release
adlport &H83, 4     'carrier sustain/release
'adlport &HE0, 0     'modulator wave select
'adlport &HE3, 0     'carrier wave select
'adlport &HC0, 0     'feedback/connection
'adlport &HA0, 0     'low frequency number
'adlport &HB0, 32 + 3 + 16 'high frequency number/octative/note on

END SUB

SUB updatescreen

LOCATE 23, 65
COLOR 7
PRINT "octative: "; 4 + transpose%

END SUB

'Turns an FM voice off
SUB voiceoff (vnumber%)

'Turn off the "Key on" bit in the appropriate register
    'Find out which byte to write
byte% = voice(vnumber%).regB0
byte% = byte% AND (255 - 32)

'Write to the hardware
adlport &HB0 + vnumber%, byte%

'Display the voice note on the screen
COLOR 7
LOCATE 1 + keyb(voice(vnumber%).scancode).location, 1

'Search all the voices to make sure the voice is really off
flag% = 1
FOR n% = 0 TO TOTALVOICES - 1
  IF n% <> vnumber% THEN
    IF voice(n%).inuse = 1 AND (keyb(voice(n%).scancode).location = keyb(voice(vnumber%).scancode).location) THEN flag% = 0
  END IF
NEXT
IF flag% = 1 THEN PRINT "       ";

END SUB

'Turns on an FM voice
SUB voiceon (vnumber%)

'Determine which key is using the voice
scan% = voice(vnumber%).scancode

'Determine low byte of frequency
lobyte% = keyb(scan%).flow

'Determine high byte of frequency
hibyte% = keyb(scan%).fhi

fr = lobyte% + (hibyte% * 256)
FOR n% = 0 TO 4
fr = fr * 1.05946
NEXT
ffr% = INT(fr)
lobyte% = ffr% AND 255
hibyte% = INT(ffr% / 256)

'Include octative in high byte of frequency
hibyte% = hibyte% OR ((keyb(scan%).foct + transpose%) * 4)

'Include the "key on" bit in high byte of frequency
hibyte% = hibyte% OR 32

'Write to the hardware
adlport &HA0 + vnumber%, lobyte%    'write low byte
adlport &HB0 + vnumber%, hibyte%    'write high byte, octative and note on

'Save hi-byte for when voice needs to be shut off
voice(vnumber%).regB0 = hibyte%

'Display the voice note on the screen
COLOR 7
LOCATE 1 + keyb(voice(vnumber%).scancode).location, 1
PRINT keyname$(voice(vnumber%).scancode);

END SUB

