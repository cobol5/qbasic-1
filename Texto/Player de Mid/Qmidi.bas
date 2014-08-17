'QMIDI v4.1
'Source Code
'By Jesse Dorland (jessedorland@hotmail.com)
'
'Source Code and Documentation Released to the Public Domain, October 1998
'SBMIDI and SBSIM Drivers Copyrighted by Creative Technology.
'
'DISCLAIMER
'----------
'If your computer suffers any mishaps as a result of using QMIDI, I'm not
'responsible for it.  Reading the documentation should ensure safe
'operation, and I don't think that it's even possible for QMIDI to do any
'permanent damage, but I'm not making any guarantees.  You have been
'warned....
'
'-----{ START OF CODE }-----
'
'---[ SUBROUTINE DECLARATIONS ]---
'
DECLARE SUB Delay (Repetitions%)
DECLARE SUB DetectSettings (BasePort%, IRQ%, LoDMA%, HiDMA%, CardType%, MPU401%)
DECLARE SUB DriversLoaded (SBMIDI%, SBSIM%)
DECLARE SUB GetBass (LeftChannel%, RightChannel%)
DECLARE SUB GetTreble (LeftChannel%, RightChannel%)
DECLARE SUB GetMaster (LeftChannel%, RightChannel%)
DECLARE SUB GetMIDI (LeftChannel%, RightChannel%)
DECLARE SUB GetVoice (LeftChannel%, RightChannel%)
DECLARE FUNCTION GetSynth% ()
DECLARE FUNCTION LoadMIDI% (Filename$)
DECLARE SUB LoopMIDI ()
DECLARE FUNCTION MIDIError$ ()
DECLARE FUNCTION MixerChip$ ()
DECLARE FUNCTION MusicDone% ()
DECLARE SUB PauseMIDI ()
DECLARE SUB ResumeMIDI ()
DECLARE SUB PlayMIDI (Handle%)
DECLARE SUB SetBass (LeftChannel%, RightChannel%)
DECLARE SUB SetTreble (LeftChannel%, RightChannel%)
DECLARE SUB SetMaster (LeftChannel%, RightChannel%)
DECLARE SUB SetMIDI (LeftChannel%, RightChannel%)
DECLARE SUB SetVoice (LeftChannel%, RightChannel%)
DECLARE SUB SetCard (CardType%)
DECLARE FUNCTION SoundCard$ (CardType%)
DECLARE SUB StopMIDI ()
DECLARE FUNCTION TimeMIDI! ()
DECLARE FUNCTION InternalBitRead% (Variable%, BitNum%)
DECLARE SUB InternalBitSet (Variable%, BitNum%, OnOff%)
DECLARE SUB InternalBitToggle (Variable%, BitNum%)
DECLARE SUB InternalGetIntVector (IntNum%, Segment&, Offset&)
DECLARE SUB InternalSetIntVector (IntNum%, Segment&, Offset&)
DECLARE SUB InternalGetVol (LeftChannel%, RightChannel%, Index%)
DECLARE SUB InternalSetVol (LeftChannel%, RightChannel%, Index%)
DECLARE SUB InternalWriteMixer (Index%, Value%)
DECLARE FUNCTION InternalReadMixer% (Index%)
DECLARE SUB IntX (IntNum AS INTEGER, Regs AS ANY)
DECLARE SUB UnloadMIDI (Handle%)
DECLARE SUB CleanUpMIDI ()
DECLARE FUNCTION MemUsed& (Handle%)

'
'---[ REQUIRED CODE ]---
'THE FOLLOWING CODE MUST BE INSERTED AT THE BEGINNING OF
'ANY PROGRAMS THAT USE QMIDI, IN ORDER FOR IT TO WORK PROPERLY.
'
'$DYNAMIC
TYPE Registers
	 AX    AS INTEGER
	 BX    AS INTEGER
	 CX    AS INTEGER
	 DX    AS INTEGER
	 BP    AS INTEGER
	 SI    AS INTEGER
	 DI    AS INTEGER
	 FLAGS AS INTEGER
	 DS    AS INTEGER
	 ES    AS INTEGER
END TYPE
IntXCodeData:
DATA  &H55, &H8B, &HEC, &H83, &HEC, &H08, &H56, &H57, &H1E, &H55, &H8B, &H5E
DATA  &H06, &H8B, &H47, &H10, &H3D, &HFF, &HFF, &H75, &H04, &H1E, &H8F, &H47
DATA  &H10, &H8B, &H47, &H12, &H3D, &HFF, &HFF, &H75, &H04, &H1E, &H8F, &H47
DATA  &H12, &H8B, &H47, &H08, &H89, &H46, &HF8, &H8B, &H07, &H8B, &H4F, &H04
DATA  &H8B, &H57, &H06, &H8B, &H77, &H0A, &H8B, &H7F, &H0C, &HFF, &H77, &H12
DATA  &H07, &HFF, &H77, &H02, &H1E, &H8F, &H46, &HFA, &HFF, &H77, &H10, &H1F
DATA  &H8B, &H6E, &HF8, &H5B, &HCD, &H21, &H55, &H8B, &HEC, &H8B, &H6E, &H02
DATA  &H89, &H5E, &HFC, &H8B, &H5E, &H06, &H1E, &H8F, &H46, &HFE, &HFF, &H76
DATA  &HFA, &H1F, &H89, &H07, &H8B, &H46, &HFC, &H89, &H47, &H02, &H89, &H4F
DATA  &H04, &H89, &H57, &H06, &H58, &H89, &H47, &H08, &H89, &H77, &H0A, &H89
DATA  &H7F, &H0C, &H9C, &H8F, &H47, &H0E, &H06, &H8F, &H47, &H12, &H8B, &H46
DATA  &HFE, &H89, &H47, &H10, &H5A, &H1F, &H5F, &H5E, &H8B, &HE5, &H5D, &HCA
DATA  &H02, &H00
DIM SHARED QMIDIRegs AS Registers, MEM.SEGMENT(0 TO 255) AS INTEGER
DIM SHARED MIDI.PLAYTIME AS SINGLE, MIDI.ERROR AS INTEGER, PAUSED AS SINGLE
DIM SHARED SBMIDI.INTERRUPT AS INTEGER, MEM.ALLOCATED(0 TO 255) AS LONG
DIM SHARED SBSIM.INTERRUPT AS INTEGER, MIXER.CHIP AS INTEGER
DIM SHARED SB.BASEPORT AS INTEGER, SB.IRQ AS INTEGER
DIM SHARED SB.LODMA AS INTEGER, SB.HIDMA AS INTEGER, SB.CARDTYPE AS INTEGER
DIM SHARED SB.MPU401 AS INTEGER, BIT.STORAGE(0 TO 7) AS INTEGER
DIM SHARED SENSITIVE AS INTEGER, REVERSE.STEREO AS INTEGER
DIM SHARED SOUND.DISABLED AS INTEGER, CURRENTHANDLE AS INTEGER

DriversLoaded SBMIDI.INTERRUPT, SBSIM.INTERRUPT
IF SBMIDI.INTERRUPT = 0 THEN SBMIDI.INTERRUPT = &H80
IF SBSIM.INTERRUPT = 0 THEN SBSIM.INTERRUPT = &H81
DetectSettings SB.BASEPORT, SB.IRQ, SB.LODMA, SB.HIDMA, SB.CARDTYPE, SB.MPU401
IF SB.CARDTYPE = 0 THEN SetCard 2
IF SB.BASEPORT = 0 THEN SB.BASEPORT = &H220
IF SB.IRQ = 0 THEN SB.IRQ = 5
IF SB.LODMA = 0 THEN SB.LODMA = 1
IF SB.HIDMA = 0 AND SB.CARDTYPE = 6 THEN SB.HIDMA = 5
'
'---[ END OF REQUIRED CODE ]---
'INSERT YOUR OWN CODE BEGINNING HERE.
'

REM $STATIC
SUB CleanUpMIDI
FOR I% = 0 TO 255
	IF MEM.SEGMENT(I%) THEN UnloadMIDI I%
NEXT I%
MIDI.ERROR = 0
END SUB

'A machine-independent delay
SUB Delay (Repetitions%)
FOR I% = 1 TO Repetitions%
	WAIT &H3DA, 8, 8
	WAIT &H3DA, 8
NEXT I%
END SUB

REM $DYNAMIC
'DetectSettings - Attempt to detect Sound Blaster settings
SUB DetectSettings (BasePort%, IRQ%, LoDMA%, HiDMA%, CardType%, MPU401%)

'Reset all of the variables
BasePort% = 0
IRQ% = 0
LoDMA% = 0
HiDMA% = 0
CardType% = 0
MPU401% = 0

'Read the BLASTER environment variable
Settings$ = ENVIRON$("BLASTER")

'Attempt to extract the base port, High DMA, Low DMA, IRQ, and card type
'from the BLASTER enironment variable.
FOR I% = 1 TO LEN(Settings$) - 1
	'If the type of sound card was found, get it and exit the loop.
	SELECT CASE UCASE$(MID$(Settings$, I%, 1))
		'If the card type was found...
		CASE "T"
			CardType% = VAL(MID$(Settings$, I% + 1, 1))
			'If the base port address was found...
		CASE "A"
			BasePort% = VAL("&H" + LTRIM$(STR$(VAL(MID$(Settings$, I% + 1, 3)))))
		'If the IRQ was found...
		CASE "I"
			IRQ% = VAL(MID$(Settings$, I% + 1, 2))
		'If the low DMA channel was found...
		CASE "D"
			LoDMA% = VAL(MID$(Settings$, I% + 1, 1))
		'If the high DMA channel was found...
		CASE "H"
			HiDMA% = VAL(MID$(Settings$, I% + 1, 1))
		'If the MPU401 port was found...
		CASE "P"
			MPU401% = VAL("&H" + LTRIM$(STR$(VAL(MID$(Settings$, I% + 1, 3)))))
	END SELECT
NEXT I%

'If the card type wasn't found in the BLASTER variable, try to figure
'out the type using another method.

IF CardType% = 0 THEN
	'Examine the card's DMA channel.
	SELECT CASE LoDMA%
		'If the DMA is 210h or 230h, the card is an SB1.0/1.5.
		CASE &H210, &H230
			CardType% = 1
		'If the DMA is 250h or 260h, the card is either an SB2CD or a
		'Sound Blaster 16.  It could also be a Sound Blaster 1.0/1.5,
		'but it probably isn't.  Actually, it's also unlikely that the card
		'is an SB16, but I check for it anyway, because there's an easy way
		'to tell if it is - the High DMA channel will be greater than
		'0.
		'
		'On the other hand, there's no way that I know of to
		'distinguish an SB 1.0 from an SB 2.0, except by looking at the
		'BLASTER environment variable.  And since this code is executing
		'that method obviously failed.
		CASE &H250, &H260
			'Examining the High DMA channel will narrow it down.
			'If the High DMA is greater than 0, the card is an SB16.
			IF HiDMA% THEN
				CardType% = 6
			'Otherwise, define the card as a Sound Blaster 2.0.
			ELSE
				CardType% = 3
			END IF
		'If the DMA channel is any other value....
		CASE ELSE
			'Check the High DMA channel.  If it's a non-zero value,
			'we've got an SB16.
			IF HiDMA% THEN
				CardType% = 6
			'Otherwise....
			ELSE
				'If sensitive error checking is on, define the card as
				'a Sound Blaster 1.0/1.5.
				IF SENSITIVE THEN
					CardType% = 1
				'Otherwise, assume it's a Sound Blaster Pro.
				ELSE
					CardType% = 4
				END IF
			END IF
	END SELECT
END IF

'Determine the sound card's mixer chip
SELECT CASE CardType%
	'If the card could not be detected....
	CASE 0
		MIDI.ERROR = 7
		'If sensitive error checking is on, disable mixer operations
		IF SENSITIVE THEN
			MIXER.CHIP = 0
		'Otherwise, assume the default mixer chip.
		ELSE
			MIXER.CHIP = 2
		END IF
	'If the card is a Sound Blaster 1.0/1.5 or equivalent....
	CASE 1
		'Return an error.
		MIDI.ERROR = 6
		'If sensitive error checking is on, disable mixer operations and
		'exit.
		IF SENSITIVE THEN
			MIXER.CHIP = 0
			EXIT SUB
		'Otherwise, set the earliest mixer chip and continue.
		ELSE
			MIXER.CHIP = 1
		END IF
	'If the card is a Sound Blaster 2.0/2.5 or equivalent....
	CASE 3
		'There are two different kinds of SB 2.0 cards: the regular SB2,
		'and the SB2CD.  The SB2CD has a mixer chip (the CT1335), whereas
		'the SB 2.0 does not.  The way to tell them apart is that the
		'Sound Blaster 2.0 uses Base Ports 220h and 240h, and the SB2CD
		'uses ports 250h and 260h.
		'
		'Assume the sound card is an SB2CD for now...
		MIXER.CHIP = 1
		'If the card is defined as an SB 2.0, not an SB 2.0 CD, and
		'sensitive error checking is on, disable mixer operations.
		IF (BasePort% = &H220 OR BasePort% = &H240) AND SENSITIVE <> 0 THEN
			MIXER.CHIP = 0
		END IF
		MIDI.ERROR = 0
	'If the card is a Sound Blaster Pro, assume chip CT1345
	CASE 2, 4, 5
		MIXER.CHIP = 2
		MIDI.ERROR = 0
	'If the card is a Sound Blaster 16 or later, assume chip CT1745
	CASE IS >= 6
		MIXER.CHIP = 3
		MIDI.ERROR = 0
END SELECT
END SUB

'DriversLoaded - Attempt to detect if sound drivers are loaded
SUB DriversLoaded (SBMIDI%, SBSIM%)
'Open the data file.
FF% = FREEFILE
OPEN "DRIVERS.DAT" FOR BINARY AS #FF%
FileSize& = LOF(FF%)
NoExist% = 0
'If the file is empty, return an error.
IF FileSize& = 0 THEN
	CLOSE FF%
	KILL "DRIVERS.DAT"
	MIDI.ERROR = 1
	NoExist% = 1
'If the file is not exactly 1,024 bytes in size, return an error.
ELSEIF FileSize& <> 1024 THEN
	CLOSE FF%
	MIDI.ERROR = 9
	NoExist% = 1
END IF

'If DRIVERS.DAT exists, and is 1 kilobyte in size, read the driver
'data from it.
IF NoExist% = 0 THEN
REDIM DRIVERDATA$(1 TO 5)
FOR I% = 1 TO 4
	DRIVERDATA$(I%) = INPUT$(256, #FF%)
NEXT I%
END IF

'Close the data file.
CLOSE #FF%

'Check the interrupt handlers for int 80h-FFh, to see if they are occupied
'by either SBMIDI or SBSIM.
SBMIDI% = 0
SBSIM% = 0
FOR I% = &H80 TO &HFF
	'Get the address of the interrupt handler.
	InternalGetIntVector I%, Segment&, Offset&
	'If the segment returned is 0, that means that the current interrupt
	'is not in use.
	IF Segment& = 0 THEN GOTO Skip:

	'The following code checks for the drivers by looking for the text
	'"SBMIDI" and "SBSIM" at certain locations in the driver code.
	'If it doesn't work, a different method is used.
	IF SBMIDI% = 0 THEN
	  DEF SEG = Segment& - 17
	  TEMP$ = ""
	  FOR J% = 1 TO 6
		TEMP$ = TEMP$ + CHR$(PEEK(271 + J%))
	  NEXT
	  IF TEMP$ = "SBMIDI" THEN SBMIDI% = I%
	END IF
	IF SBSIM% = 0 AND Segment& <> 0 THEN
		DEF SEG = Segment& - 1
		TEMP$ = ""
		FOR J% = 1 TO 5
			TEMP$ = TEMP$ + CHR$(PEEK(274 + J%))
		NEXT
		IF TEMP$ = "SBSIM" THEN SBSIM% = I%
	END IF
 
	'This is the second detection method.  It's more complex than the first
	'method, but not really any more accurate.
	IF NoExist% = 0 THEN
	'Point to the segment of the interrupt handler.
	DEF SEG = Segment&
	'Read 256 bytes of code from the interrupt handler.
	DRIVERDATA$(5) = ""
	FOR J% = 0 TO 255
		Byte% = PEEK(Offset& + J%)
		DRIVERDATA$(5) = DRIVERDATA$(5) + CHR$(Byte%)
	NEXT J%
	'Check to see if the code matches any of the data from DRIVERS.DAT.
	FOR J% = 1 TO 4
		MATCH% = 1
		FOR k% = 0 TO 255
			IF MID$(DRIVERDATA$(J%), k% + 1, 1) <> MID$(DRIVERDATA$(5), k% + 1, 1) THEN
				SELECT CASE k%
					CASE IS = 14, 15, 113, 114, 235, 236
					CASE ELSE
						MATCH% = 0
						EXIT FOR
				END SELECT
			END IF
		NEXT k%
		'If there was a match, find out which driver is using the interrupt.
		IF MATCH% THEN
			IF J% = 1 THEN SBSIM% = I%
			IF J% <> 1 THEN SBMIDI% = I%
		END IF
		'If both SBMIDI and SBSIM have been found, exit the loop.
		IF SBSIM% <> 0 AND SBMIDI% <> 0 THEN EXIT FOR
	NEXT J%
   
	'If both SBMIDI and SBSIM have been found, exit the loop.
	IF SBSIM% <> 0 AND SBMIDI% <> 0 THEN EXIT FOR
	END IF
Skip:
NEXT I%
IF NoExist% = 0 THEN MIDI.ERROR = 0
END SUB

'GetBass - Returns the current Bass level
SUB GetBass (LeftChannel%, RightChannel%)
SELECT CASE MIXER.CHIP
	'If no mixer chip is present, return an error
	CASE 0
		MIDI.ERROR = 6
		EXIT SUB
	'If the mixer chip is CT1335 or CT1345, return an error, becuase
	'these chips do not support Treble and Bass control.
	CASE 1, 2
		MIDI.ERROR = 8
		EXIT SUB
	'If the mixer chip is CT1745....
	CASE 3
		'Get the bass settings.
		LBass% = InternalReadMixer%(&H46)
		RBass% = InternalReadMixer%(&H47)
		LeftChannel% = 0
		RighChannel% = 0
		'Extract the settings
		FOR I% = 4 TO 7
			BIT.STORAGE(I%) = InternalBitRead%(LBass%, I%)
			IF BIT.STORAGE(I%) THEN InternalBitToggle LeftChannel%, I% - 4
			BIT.STORAGE(I%) = InternalBitRead%(RBass%, I%)
			IF BIT.STORAGE(I%) THEN InternalBitToggle RightChannel%, I% - 4
		NEXT I%
		'Convert the 4-bit settings to 5-bit values.
		LeftChannel% = LeftChannel% * 2
		IF LeftChannel% = 30 THEN LeftChannel% = 31
		RightChannel% = RightChannel% * 2
		IF RightChannel% = 30 THEN RightChannel% = 31
		MIDI.ERROR = 0
END SELECT
END SUB

'GetMaster - Returns the current Master volume level
SUB GetMaster (LeftChannel%, RightChannel%)
SELECT CASE MIXER.CHIP
	'If no mixer chip is present, return an error
	CASE 0: MIDI.ERROR = 6: EXIT SUB
	'If the mixer chip is CT1335....
	CASE 1
		InternalGetVol LeftChannel%, RightChannel%, 2
		MIDI.ERROR = 0
	'If the mixer chip is CT1345....
	CASE 2
		InternalGetVol LeftChannel%, RightChannel%, &H22
		MIDI.ERROR = 0
	'If the mixer chip is CT1745....
	CASE 3
		InternalGetVol LeftChannel%, RightChannel%, &H30
		MIDI.ERROR = 0
END SELECT
END SUB

'GetMIDI - Returns the current MIDI volume level
SUB GetMIDI (LeftChannel%, RightChannel%)
SELECT CASE MIXER.CHIP
	'If no mixer chip is present, return an error
	CASE 0: MIDI.ERROR = 6: EXIT SUB
	'If the mixer chip is CT1335....
	CASE 1
		InternalGetVol LeftChannel%, RightChannel%, 6
		MIDI.ERROR = 0
	'If the mixer chip is CT1345....
	CASE 2
		InternalGetVol LeftChannel%, RightChannel%, &H26
		MIDI.ERROR = 0
	'If the mixer chip is CT1745....
	CASE 3
		InternalGetVol LeftChannel%, RightChannel%, &H34
		MIDI.ERROR = 0
END SELECT
END SUB

REM $STATIC
FUNCTION GetSynth%
QMIDIRegs.BX = 10
CALL IntX(SBMIDI.INTERRUPT, QMIDIRegs)
GetSynth% = QMIDIRegs.AX
END FUNCTION

REM $DYNAMIC
'GetTreble - Returns the current Treble level
SUB GetTreble (LeftChannel%, RightChannel%)
SELECT CASE MIXER.CHIP
	'If no mixer chip is present, return an error
	CASE 0
		MIDI.ERROR = 6
		EXIT SUB
	'If the mixer chip is CT1335 or CT1345, return an error, becuase
	'these chips do not support Treble and Bass control.
	CASE 1, 2
		MIDI.ERROR = 8
		EXIT SUB
	'If the mixer chip is CT1745....
	CASE 3
		'Get the treble settings.
		LTreble% = InternalReadMixer%(&H44)
		RTreble% = InternalReadMixer%(&H45)
		LeftChannel% = 0
		RighChannel% = 0
		'Extract the settings
		FOR I% = 4 TO 7
			BIT.STORAGE(I%) = InternalBitRead%(LTreble%, I%)
			IF BIT.STORAGE(I%) THEN InternalBitToggle LeftChannel%, I% - 4
			BIT.STORAGE(I%) = InternalBitRead%(RTreble%, I%)
			IF BIT.STORAGE(I%) THEN InternalBitToggle RightChannel%, I% - 4
		NEXT I%
		'Convert the 4-bit settings to 5-bit values.
		LeftChannel% = LeftChannel% * 2
		IF LeftChannel% = 30 THEN LeftChannel% = 31
		RightChannel% = RightChannel% * 2
		IF RightChannel% = 30 THEN RightChannel% = 31
		MIDI.ERROR = 0
END SELECT
END SUB

'GetVoice - Returns the current Voice volume level
SUB GetVoice (LeftChannel%, RightChannel%)
SELECT CASE MIXER.CHIP
	'If no mixer chip is present, return an error
	CASE 0: MIDI.ERROR = 6: EXIT SUB
	'If the mixer chip is CT1335....
	CASE 1
		'Reset the Left Channel variable
		LeftChannel% = 0
		'Read the byte containing the Voice volume.
		MasterVolume% = InternalReadMixer%(&HA)
		'Extract the actual volume setting from the byte.
		FOR I% = 1 TO 2
			BIT.STORAGE(I%) = InternalBitRead%(MasterVolume%, I%)
			IF BIT.STORAGE(I%) = 1 THEN
				InternalBitToggle LeftChannel%, I% - 1
			END IF
		NEXT I%
		'Convert the 2-bit value returned to a 5-bit value.
		LeftChannel% = LeftChannel% * 8
		IF LeftChannel% = 24 THEN LeftChannel% = 31
		'The CT1335 mixer chip only allows monaural volume control, so copy
		'the value of the Left Channel volume into the Right Channel variable.
		RightChannel% = LeftChannel%
	'If the mixer chip is CT1345....
	CASE 2
		InternalGetVol LeftChannel%, RightChannel%, &H4
	'If the mixer chip is CT1745....
	CASE 3
		InternalGetVol LeftChannel%, RightChannel%, &H32
END SELECT
END SUB

FUNCTION InternalBitRead% (Variable%, BitNum%)
VarSegment% = VARSEG(Variable%)
Offset% = VARPTR(Variable%)
DEF SEG = VarSegment%
InternalBitRead% = -((PEEK(Offset% + BitNum% \ 8) AND 2 ^ (BitNum% MOD 8)) > 0)
DEF SEG
END FUNCTION

SUB InternalBitSet (Variable%, BitNum%, OnOff%)
VarSegment% = VARSEG(Variable%)
Offset% = VARPTR(Variable%)
DEF SEG = VarSegment%
IF OnOff% THEN
	POKE Offset% + BitNum% \ 8, PEEK(Offset% + BitNum% \ 8) OR 2 ^ (BitNum% MOD 8)
ELSE
	POKE Offset% + BitNum% \ 8, PEEK(Offset% + BitNum% \ 8) AND 255 - 2 ^ (BitNum% MOD 8)
END IF
DEF SEG
END SUB

SUB InternalBitToggle (Variable%, BitNum%)
VarSegment% = VARSEG(Variable%)
Offset% = VARPTR(Variable%)
DEF SEG = VarSegment%
POKE Offset% + BitNum% \ 8, PEEK(Offset% + BitNum% \ 8) XOR 2 ^ (BitNum% MOD 8)
DEF SEG
END SUB

SUB InternalGetIntVector (IntNum%, Segment&, Offset&)
QMIDIRegs.AX = IntNum% + 13568
CALL IntX(&H21, QMIDIRegs)
Segment& = QMIDIRegs.ES
Offset& = QMIDIRegs.BX
END SUB

SUB InternalGetVol (LeftChannel%, RightChannel%, Index%)
SELECT CASE MIXER.CHIP
	'If no mixer chip is present, exit
	CASE 0: EXIT SUB
	'If the mixer chip is CT1335....
	CASE 1
		'Reset the Left Channel variable
		LeftChannel% = 0
		'Read the byte containing the Volume.
		MasterVolume% = InternalReadMixer%(Index%)
		'Extract the actual volume setting from the byte.
		FOR I% = 1 TO 3
			BIT.STORAGE(I%) = InternalBitRead%(MasterVolume%, I%)
			IF BIT.STORAGE(I%) = 1 THEN
				InternalBitToggle LeftChannel%, I% - 1
			END IF
		NEXT I%
		'Convert the 3-bit value returned to a 5-bit value.
		LeftChannel% = LeftChannel% * 4
		IF LeftChannel% = 28 THEN LeftChannel% = 31
		'The CT1335 mixer chip only allows monaural volume control, so copy
		'the value of the Left Channel volume into the Right Channel variable.
		RightChannel% = LeftChannel%
	'If the mixer chip is CT1345....
	CASE 2
		'Reset the Left Channel variable
		LeftChannel% = 0
		'Reset the Right Channel variable
		RightChannel% = 0
		'Read the byte containing the Volume.
		MasterVolume% = InternalReadMixer%(Index%)
		'Extract the volume settings from the byte.
		FOR I% = 1 TO 3
			BIT.STORAGE(I%) = InternalBitRead%(MasterVolume%, I%)
			IF BIT.STORAGE(I%) THEN
				InternalBitToggle RightChannel%, I% - 1
			END IF
			BIT.STORAGE(I% + 4) = InternalBitRead%(MasterVolume%, I% + 4)
			IF BIT.STORAGE(I% + 4) THEN
				InternalBitToggle LeftChannel%, I% - 1
			END IF
		NEXT I%
		'Convert the 3-bit values returned to 5-bit values.
		LeftChannel% = LeftChannel% * 4
		IF LeftChannel% = 28 THEN LeftChannel% = 31
		RightChannel% = RightChannel% * 4
		IF LeftChannel% = 28 THEN LeftChannel% = 31
	'If the mixer chip is CT1745....
	CASE 3
		'Reset the Left Channel variable
		LeftChannel% = 0
		'Reset the Right Channel variable
		RightChannel% = 0
		'Read the byte containing the Left Volume.
		LMasterVolume% = InternalReadMixer%(Index%)
		'Read the byte containing the Right Volume.
		RMasterVolume% = InternalReadMixer%(Index% + 1)
		'Extract the left channel volume settings from the byte.
		FOR I% = 3 TO 7
			BIT.STORAGE(I%) = InternalBitRead%(LMasterVolume%, I%)
			IF BIT.STORAGE(I%) THEN
				InternalBitToggle LeftChannel%, I% - 3
			END IF
		NEXT I%
		'Extract the right channel volume settings from the byte.
		FOR I% = 3 TO 7
			BIT.STORAGE(I%) = InternalBitRead%(RMasterVolume%, I%)
			IF BIT.STORAGE(I%) THEN
				InternalBitToggle RightChannel%, I% - 3
			END IF
		NEXT I%
END SELECT
END SUB

FUNCTION InternalReadMixer% (Index%)
OUT SB.BASEPORT + 4, Index%
InternalReadMixer% = INP(SB.BASEPORT + 5)
END FUNCTION

SUB InternalSetVol (LeftChannel%, RightChannel%, Index%)
SELECT CASE MIXER.CHIP
	'If no mixer chip is present, exit
	CASE 0: EXIT SUB
	'If the mixer chip is CT1335....
	CASE 1
		'If the left channel volume is not in the proper range, don't
		'change any settings.  NOTE: Since cards with the CT1335 mixer chip
		'only support monaural sound, the right channel volume setting is
		'ignored.
		IF LeftChannel% > -1 AND LeftChannel% < 32 THEN
			'Convert the 5-bit Left Channel value to a 3-bit value.
			LeftChannel% = LeftChannel% \ 4
			IF LeftChannel% > 7 THEN LeftChannel% = 7
			'Get the current volume setting.
			Volume% = InternalReadMixer%(2)
			'Extract the volume setting.
			FOR I% = 0 TO 2
				BIT.STORAGE(I%) = InternalBitRead%(LeftChannel%, I%)
				IF BIT.STORAGE(I%) THEN
					InternalBitSet Volume%, I% + 1, 1
				END IF
			NEXT I%
			'Write the new volume setting
			InternalWriteMixer Index%, Volume%
		END IF
	'If the mixer chip is CT1345....
	CASE 2
		'Get the current volume setting.
		Volume% = InternalReadMixer%(Index%)
		'Convert the 5-bit Left Channel value to a 3-bit value.
		LeftChannel% = LeftChannel% \ 4
		IF LeftChannel% > 7 THEN LeftChannel% = 7
		'Convert the 5-bit Right Channel value to a 3-bit value.
		RightChannel% = RightChannel% \ 4
		IF RightChannel% > 7 THEN RightChannel% = 7
		'If Reverse Stereo is enabled, swap the variables.
		IF REVERSE.STEREO THEN SWAP LeftChannel%, RightChannel%
		'Modify the volume settings as necessary.
		FOR I% = 0 TO 2
			'First, the right channel....
			BIT.STORAGE(I%) = InternalBitRead%(RightChannel%, I%)
			IF RightChannel% > -1 AND RightChannel% < 32 THEN
				IF BIT.STORAGE(I%) THEN BitVal% = 1 ELSE BitVal% = 0
				InternalBitSet Volume%, I% + 1, BitVal%
			END IF
			'Now, the left channel....
			BIT.STORAGE(I%) = InternalBitRead%(LeftChannel%, I%)
			IF LeftChannel% > -1 AND LeftChannel% < 32 THEN
				IF BIT.STORAGE(I%) THEN BitVal% = 1 ELSE BitVal% = 0
				InternalBitSet Volume%, I% + 5, BitVal%
			END IF
		NEXT I%
		'Write the new volume settings
		InternalWriteMixer Index%, Volume%
	'If the mixer chip is CT1745....
	CASE 3
		'Get the current left channel volume setting.
		LVolume% = InternalReadMixer%(Index%)
		'Get the current right channel volume setting.
		RVolume% = InternalReadMixer%(Index% + 1)
		'If Reverse Stereo is enabled, swap the variables.
		IF REVERSE.STEREO THEN SWAP LeftChannel%, RightChannel%
		'Modify the volume settings as necessary.
		FOR I% = 0 TO 4
			'First, the right channel....
			BIT.STORAGE(I%) = InternalBitRead%(RightChannel%, I%)
			IF RightChannel% > -1 AND RightChannel% < 32 THEN
				IF BIT.STORAGE(I%) THEN BitVal% = 1 ELSE BitVal% = 0
				InternalBitSet RVolume%, I% + 3, BitVal%
			END IF
			'Now, the left channel....
			BIT.STORAGE(I%) = InternalBitRead%(LeftChannel%, I%)
			IF LeftChannel% > -1 AND LeftChannel% < 32 THEN
				IF BIT.STORAGE(I%) THEN BitVal% = 1 ELSE BitVal% = 0
				InternalBitSet LVolume%, I% + 3, BitVal%
			END IF
		NEXT I%
		'Write the new volume settings
		InternalWriteMixer Index%, LVolume%
		InternalWriteMixer Index% + 1, RVolume%
END SELECT

END SUB

SUB InternalWriteMixer (Index%, Value%)
OUT SB.BASEPORT + 4, Index%
OUT SB.BASEPORT + 5, Value%
END SUB

REM $STATIC
SUB IntX (IntNum AS INTEGER, Regs AS Registers) STATIC

STATIC filenum AS INTEGER, IntOffset AS INTEGER, Loaded AS INTEGER
		   
	' use fixed-length string to fix its position in memory
	' and so we don't mess up string pool before routine
	' gets its pointers from caller

DIM IntCode AS STRING * 200
IF NOT Loaded THEN                     ' loaded will be 0 first time
	RESTORE IntXCodeData:
   
	FOR k% = 1 TO 145
		READ h%
		MID$(IntCode, k%, 1) = CHR$(h%)
	NEXT

	'  determine address of interrupt no. offset in IntCode
  
	IntOffset% = INSTR(IntCode$, CHR$(&HCD) + CHR$(&H21)) + 1
	Loaded% = -1
END IF

SELECT CASE IntNum
  
	CASE &H25, &H26, IS > 255               ' ignore these interrupts
  
	CASE ELSE
		DEF SEG = VARSEG(IntCode)             ' poke interrupt number into
		POKE VARPTR(IntCode) * 1& + IntOffset - 1, IntNum     ' code block
		CALL ABSOLUTE(Regs, VARPTR(IntCode$))               ' call routine
END SELECT

END SUB

REM $DYNAMIC
'LoadMIDI - loads a MIDI file into memory
FUNCTION LoadMIDI% (Filename$)
LoadMIDI% = -1
'See if an extension was supplied, and if not, add one.
IF INSTR(Filename$, ".") = 0 THEN Filename$ = Filename$ + ".MID"
'Open the file
FF% = FREEFILE
OPEN Filename$ FOR BINARY AS #FF%
FileLen& = LOF(FF%)
CLOSE #FF%
'If the file is empty, delete it and exit now.
IF FileLen& = 0 THEN KILL Filename$: MIDI.ERROR = 1: EXIT FUNCTION
'Make the filename an ASCIIZ string.
Filename$ = Filename$ + CHR$(0)

'Find an empty MIDI handle
NewHandle% = -1
FOR I% = 0 TO 255
	IF MEM.SEGMENT(I%) = 0 THEN NewHandle% = I%: EXIT FOR
NEXT I%
'If there are no empty handles, return an error.
IF NewHandle% = -1 THEN MIDI.ERROR = 12: EXIT FUNCTION
'Attempt to allocate a block of conventional memory.
QMIDIRegs.AX = &H4800
QMIDIRegs.BX = (FileLen& \ 16) + 1
CALL IntX(&H21, QMIDIRegs)
'If the block couldn't be allocated, it means there's not enough free
'memory.  To fix this, we need to ask BASIC to release some of the memory
'it's using:
IF QMIDIRegs.AX = 7 OR QMIDIRegs.AX = 8 THEN
	'Find out how much memory is available, in kilobytes.
	LargestBlock& = QMIDIRegs.BX
	LargestBlock& = LargestBlock& * 16
	'Calculate the amount of memory that BASIC needs to release for us.
	MEM.ALLOCATED(NewHandle%) = (FileLen& + 2048) - LargestBlock&
	'Attempt to release the memory.
	A& = SETMEM(-MEM.ALLOCATED(NewHandle%))
	'Try again to allocate a block of memory
	QMIDIRegs.AX = &H4800
	QMIDIRegs.BX = (FileLen& \ 16) + 1
	CALL IntX(&H21, QMIDIRegs)
	'If the second attempt was unsuccessful, then there just isn't
	'enough memory, and an error needs to be returned.
	IF QMIDIRegs.AX = 7 OR QMIDIRegs.AX = 8 THEN
		'Give any memory we took back to BASIC.
		A& = SETMEM(650000)
		'Return an error.
		MIDI.ERROR = 2
		MEM.SEGMENT(NewHandle%) = 0
		'Abort.
		EXIT FUNCTION
	END IF
END IF
'If the memory was allocated successfully, store the segment
'of the memory block.
MEM.SEGMENT(NewHandle%) = QMIDIRegs.AX
MIDISegment& = QMIDIRegs.AX

'Open the MIDI file using a DOS interrupt.
QMIDIRegs.AX = &H3D00
QMIDIRegs.DX = SADD(Filename$)
QMIDIRegs.DS = VARSEG(Filename$)
CALL IntX(&H21, QMIDIRegs)
'Store the file handle.
Handle% = QMIDIRegs.AX
'Read the data from the file in 16 kilobyte increments.
FOR I& = 1 TO FileLen& STEP 16384
	QMIDIRegs.AX = &H3F00
	QMIDIRegs.CX = 16384
	QMIDIRegs.DX = 0
	QMIDIRegs.DS = VAL("&H" + HEX$(MIDISegment&))
	QMIDIRegs.BX = Handle%
	CALL IntX(&H21, QMIDIRegs)
	MIDISegment& = MIDISegment& + 1024
NEXT I&

'Close the file
QMIDIRegs.AX = &H3E00
QMIDIRegs.BX = Handle%
CALL IntX(&H21, QMIDIRegs)

MIDI.ERROR = 0
LoadMIDI% = NewHandle%
END FUNCTION

REM $STATIC
SUB LoopMIDI
IF SBMIDI.INTERRUPT < &H80 AND SENSITIVE <> 0 THEN MIDI.ERROR = 4: EXIT SUB
QMIDIRegs.BX = 11
CALL IntX(SBMIDI.INTERRUPT, QMIDIRegs)
IF QMIDIRegs.AX = 0 THEN PlayMIDI CURRENTHANDLE
END SUB

FUNCTION MemUsed& (Handle%)
MemUsed& = MEM.ALLOCATED(Handle%)
MIDI.ERROR = 0
END FUNCTION

REM $DYNAMIC
'MIDIError - Translates a QMIDI error code into text
FUNCTION MIDIError$
SELECT CASE MIDI.ERROR
		CASE 0: MIDIError$ = "NO ERROR"
		CASE 1: MIDIError$ = "FILE DOES NOT EXIST"
		CASE 2: MIDIError$ = "OUT OF MEMORY"
		CASE 3: MIDIError$ = "NO MIDI FILE PLAYING"
		CASE 4: MIDIError$ = "INVALID SBMIDI INTERRUPT"
		CASE 5: MIDIError$ = "INVALID SBSIM INTERRUPT"
		CASE 6: MIDIError$ = "NO MIXER CHIP"
		CASE 7: MIDIError$ = "COULD NOT DETECT SOUND CARD"
		CASE 8: MIDIError$ = "FEATURE UNAVAILABLE"
		CASE 9: MIDIError$ = "FILE IS CORRUPT"
		CASE 10: MIDIError$ = "INVALID SOUND CARD TYPE"
		CASE 11: MIDIError$ = "COULD NOT PLAY MUSIC"
		CASE 12: MIDIError$ = "ALL HANDLES IN USE"
		CASE 13: MIDIError$ = "INVALID HANDLE NUMBER"
		CASE ELSE: MIDIError$ = "UNKNOWN ERROR"
END SELECT
END FUNCTION

'MixerChip - Returns name of mixer chip used by sound card
FUNCTION MixerChip$
SELECT CASE MIXER.CHIP
	CASE 0: MixerChip$ = "No Mixer Chip Detected"
	CASE 1: MixerChip$ = "CT1335"
	CASE 2: MixerChip$ = "CT1345"
	CASE 3: MixerChip$ = "CT1745"
	CASE ELSE: MixerChip$ = "Unknown"
END SELECT
END FUNCTION

REM $STATIC
FUNCTION MusicDone%
IF SBMIDI.INTERRUPT < &H80 AND SENSITIVE <> 0 THEN MIDI.ERROR = 4: EXIT FUNCTION
IF MIDI.PLAYTIME = 0 THEN MIDI.ERROR = 3: EXIT FUNCTION
QMIDIRegs.BX = 11
CALL IntX(SBMIDI.INTERRUPT, QMIDIRegs)
IF QMIDIRegs.AX = 0 THEN QMIDIRegs.AX = -1 ELSE QMIDIRegs.AX = 0
MusicDone% = QMIDIRegs.AX
MIDI.ERROR = 0
END FUNCTION

REM $DYNAMIC
'PauseMIDI - Pauses a MIDI file that is currently playing
SUB PauseMIDI
IF SBMIDI.INTERRUPT < &H80 AND SENSITIVE <> 0 THEN MIDI.ERROR = 4: EXIT SUB
'If no MIDI file is playing, exit now
IF MIDI.PLAYTIME = 0 THEN
	MIDI.ERROR = 3
	EXIT SUB
END IF
'Call the SBSIM driver to pause the music.
QMIDIRegs.BX = 7
CALL IntX(SBMIDI.INTERRUPT, QMIDIRegs)
'Save the number of seconds that the MIDI file has been playing.
PAUSED = TimeMIDI!
'If the music hasn't been playing long enough for TimeMIDI! to return
'a value greater than 0, change PAUSED to a tiny positive value.
IF PAUSED = 0! THEN PAUSED = .00001
'Indicate that the file has stopped playing.
MIDI.PLAYTIME = 0
MIDI.ERROR = 0
END SUB

'PlayMIDI - Begins playing a MIDI file in the background.
SUB PlayMIDI (Handle%)
IF Handle% < 0 OR Handle% > 255 THEN MIDI.ERROR = 13: EXIT SUB
IF SBMIDI.INTERRUPT < &H80 AND SENSITIVE <> 0 THEN MIDI.ERROR = 4: EXIT SUB
'If sound is not disabled....
IF SOUND.DISABLED = 0 THEN
	'Call the SBMIDI driver to begin playing the MIDI file.
	QMIDIRegs.BX = 4
	QMIDIRegs.DX = MEM.SEGMENT(Handle%)
	QMIDIRegs.AX = 0
	CALL IntX(SBMIDI.INTERRUPT, QMIDIRegs)
	QMIDIRegs.BX = 5
	CALL IntX(SBMIDI.INTERRUPT, QMIDIRegs)
	'If the music could not be started, return an error.
	IF QMIDIRegs.AX <> 0 THEN MIDI.ERROR = 11: EXIT SUB
	'Start the MIDI timer.
	MIDI.PLAYTIME = TIMER
	'Set the current handle.
	CURRENTHANDLE = Handle%
END IF
MIDI.ERROR = 0
END SUB

'ResumeMIDI - Starts playing a MIDI file after it has been paused
SUB ResumeMIDI
IF SBMIDI.INTERRUPT < &H80 AND SENSITIVE <> 0 THEN MIDI.ERROR = 4: EXIT SUB
'If the MIDI file is not paused, exit now
IF PAUSED = 0! THEN EXIT SUB
'Call the SBSIM driver to resume playing.
QMIDIRegs.BX = 8
CALL IntX(SBMIDI.INTERRUPT, QMIDIRegs)
'Update the MIDI timer.
MIDI.PLAYTIME = TIMER - PAUSED
PAUSED = 0!
MIDI.ERROR = 0
END SUB

'SetBass - Set the Bass level
SUB SetBass (LeftChannel%, RightChannel%)
LC% = LeftChannel%
RC% = RightChannel%
SELECT CASE MIXER.CHIP
	'If no mixer chip is present, return an error
	CASE 0
		MIDI.ERROR = 6
		EXIT SUB
	'If the mixer chip is CT1335 or CT1345, return an error, becuase
	'these chips do not support Treble and Bass control.
	CASE 1, 2
		MIDI.ERROR = 8
		EXIT SUB
	'If the mixer chip is CT1745....
	CASE 3
		'Get the current bass values.
		LBass% = InternalReadMixer%(&H46)
		RBass% = InternalReadMixer%(&H47)
		'Convert the 5-bit settings to 4-bit values.
		LC% = LC% \ 2
		IF LC% > 15 THEN LC% = 15
		RC% = RC% \ 2
		IF RC% > 15 THEN RC% = 15
		'Place the settings into new variables.
		FOR I% = 0 TO 3
			BIT.STORAGE(I%) = InternalBitRead%(LC%, I%)
			IF BIT.STORAGE(I%) THEN BitVal% = 1 ELSE BitVal% = 0
			InternalBitSet LBass%, I% + 4, BitVal%
			BIT.STORAGE(I%) = InternalBitRead%(RC%, I%)
			IF BIT.STORAGE(I%) THEN BitVal% = 1 ELSE BitVal% = 0
			InternalBitSet RBass%, I% + 4, BitVal%
		NEXT I%
		'Write the new settings.
		InternalWriteMixer &H46, LBass%
		InternalWriteMixer &H47, RBass%
		MIDI.ERROR = 0
END SELECT
END SUB

REM $STATIC
'SetCard - Properly Sets the Sound Card Type
SUB SetCard (CardType%)
'If the card type is invalid, return an error
IF CardType% < 1 OR CardType% > 8 THEN
	MIDI.ERROR = 10
	EXIT SUB
END IF
'Otherwise, set the new card type.
SB.CARDTYPE = CardType%
'Then, set the mixer chip accordingly.
SELECT CASE CardType%
	'If the card is a Sound Blaster 1.0/1.5 or equivalent....
	CASE 1
		'Return an error.
		MIDI.ERROR = 6
		'If sensitive error checking is on, disable mixer operations and
		'exit.
		IF SENSITIVE THEN
			MIXER.CHIP = 0
			EXIT SUB
		'Otherwise, set the earliest mixer chip and continue.
		ELSE
			MIXER.CHIP = 1
		END IF
	'If the card is a Sound Blaster 2.0/2.5 or equivalent....
	CASE 3
		'There are two different kinds of SB 2.0 cards: the regular SB2,
		'and the SB2CD.  The SB2CD has a mixer chip (the CT1335), whereas
		'the SB 2.0 does not.  The way to tell them apart is that the
		'Sound Blaster 2.0 uses Base Ports 220h and 240h, and the SB2CD
		'uses ports 250h and 260h.
		'
		'Assume the sound card is an SB2CD for now...
		MIXER.CHIP = 1
		'If the card is defined as an SB 2.0, not an SB 2.0 CD, and
		'sensitive error checking is on, disable mixer operations.
		IF (BasePort% = &H220 OR BasePort% = &H240) AND SENSITIVE <> 0 THEN
			MIXER.CHIP = 0
		END IF
		MIDI.ERROR = 0
	'If the card is a Sound Blaster Pro, assume chip CT1345
	CASE 2, 4, 5
		MIXER.CHIP = 2
		MIDI.ERROR = 0
	'If the card is a Sound Blaster 16 or later, assume chip CT1745
	CASE ELSE
		MIXER.CHIP = 3
		MIDI.ERROR = 0
END SELECT
END SUB

REM $DYNAMIC
'SetMaster - Changes the Master volume level
SUB SetMaster (LeftChannel%, RightChannel%)
SELECT CASE MIXER.CHIP
	'If no mixer chip is present, return an error
	CASE 0: MIDI.ERROR = 6: EXIT SUB
	'If the mixer chip is CT1335....
	CASE 1
		LC% = LeftChannel%: RC% = RightChannel%
		InternalSetVol LC%, RC%, 2
		MIDI.ERROR = 0
	'If the mixer chip is CT1345....
	CASE 2
		LC% = LeftChannel%: RC% = RightChannel%
		InternalSetVol LC%, RC%, &H22
		MIDI.ERROR = 0
	'If the mixer chip is CT1745....
	CASE 3
		LC% = LeftChannel%: RC% = RightChannel%
		InternalSetVol LC%, RC%, &H30
		MIDI.ERROR = 0
END SELECT
END SUB

SUB SetMIDI (LeftChannel%, RightChannel%)
SELECT CASE MIXER.CHIP
	'If no mixer chip is present, return an error
	CASE 0: MIDI.ERROR = 6: EXIT SUB
	'If the mixer chip is CT1335....
	CASE 1
		LC% = LeftChannel%: RC% = RightChannel%
		InternalSetVol LC%, RC%, 6
		MIDI.ERROR = 0
	'If the mixer chip is CT1345....
	CASE 2
		LC% = LeftChannel%: RC% = RightChannel%
		InternalSetVol LC%, RC%, &H26
		MIDI.ERROR = 0
	'If the mixer chip is CT1745....
	CASE 3
		LC% = LeftChannel%: RC% = RightChannel%
		InternalSetVol LC%, RC%, &H34
		MIDI.ERROR = 0
END SELECT
END SUB

'SetTreble - Set the Treble level
SUB SetTreble (LeftChannel%, RightChannel%)
LC% = LeftChannel%
RC% = RightChannel%
SELECT CASE MIXER.CHIP
	'If no mixer chip is present, return an error
	CASE 0
		MIDI.ERROR = 6
		EXIT SUB
	'If the mixer chip is CT1335 or CT1345, return an error, becuase
	'these chips do not support Treble and Bass control.
	CASE 1, 2
		MIDI.ERROR = 8
		EXIT SUB
	'If the mixer chip is CT1745....
	CASE 3
		'Get the current treble values.
		LTreble% = InternalReadMixer%(&H44)
		RTreble% = InternalReadMixer%(&H45)
		'Convert the 5-bit settings to 4-bit values.
		LC% = LC% \ 2
		IF LC% > 15 THEN LC% = 15
		RC% = RC% \ 2
		IF RC% > 15 THEN RC% = 15
		'Place the settings into new variables.
		FOR I% = 0 TO 3
			BIT.STORAGE(I%) = InternalBitRead%(LC%, I%)
			IF BIT.STORAGE(I%) THEN BitVal% = 1 ELSE BitVal% = 0
			InternalBitSet LTreble%, I% + 4, BitVal%
			BIT.STORAGE(I%) = InternalBitRead%(RC%, I%)
			IF BIT.STORAGE(I%) THEN BitVal% = 1 ELSE BitVal% = 0
			InternalBitSet RTreble%, I% + 4, BitVal%
		NEXT I%
		'Write the new settings.
		InternalWriteMixer &H44, LTreble%
		InternalWriteMixer &H45, RTreble%
		MIDI.ERROR = 0
END SELECT
END SUB

SUB SetVoice (LeftChannel%, RightChannel%)
LC% = LeftChannel%
RC% = RightChannel%
SELECT CASE MIXER.CHIP
	'If no mixer chip is present, return an error
	CASE 0: MIDI.ERROR = 6: EXIT SUB
	'If the mixer chip is CT1335....
	CASE 1
		'If the left channel volume is not in the proper range, don't
		'change any settings.  NOTE: Since cards with the CT1335 mixer chip
		'only support monaural sound, the right channel volume setting is
		'ignored.
		IF LC% > -1 AND LC% < 32 THEN
			'Convert the 5-bit Left Channel value to a 2-bit value.
			LC% = LC% \ 8
			IF LC% > 3 THEN LC% = 3
			'Get the current volume setting.
			Volume% = InternalReadMixer%(&HA)
			'Extract the volume setting.
			FOR I% = 0 TO 1
				BIT.STORAGE(I%) = InternalBitRead%(LC%, I%)
				IF BIT.STORAGE(I%) THEN BitVal% = 1 ELSE BitVal% = 0
				InternalBitSet Volume%, I% + 1, 0
			NEXT I%
			'Write the new volume setting
			InternalWriteMixer &HA, Volume%
		END IF
	'If the mixer chip is CT1345....
	CASE 2
		InternalSetVol LC%, RC%, &H4
	'If the mixer chip is CT1745....
	CASE 3
		InternalSetVol LC%, RC%, &H32
END SELECT
END SUB

'SoundCard - Translates card type into text
FUNCTION SoundCard$ (CardType%)
SELECT CASE CardType%
	CASE 1: SoundCard$ = "Sound Blaster 1.0/1.5"
	CASE 2: SoundCard$ = "Sound Blaster Pro"
	CASE 3: SoundCard$ = "Sound Blaster 2.0"
	CASE 4, 5: SoundCard$ = "Sound Blaster Pro 2"
	CASE 6: SoundCard$ = "Sound Blaster 16/32/AWE32/AWE64"
	CASE ELSE: SoundCard$ = "Unknown"
END SELECT
END FUNCTION

'StopMIDI - Stops playing MIDI file
SUB StopMIDI
IF SBMIDI.INTERRUPT < &H80 AND SENSITIVE <> 0 THEN MIDI.ERROR = 4: EXIT SUB
'Call the SBMIDI driver to stop the music.
IF MIDI.PLAYTIME THEN
	QMIDIRegs.BX = 4
	QMIDIRegs.DX = MEM.SEGMENT(CURRENTHANDLE)
	QMIDIRegs.AX = 0
	CALL IntX(SBMIDI.INTERRUPT, QMIDIRegs)
	MIDI.ERROR = 0
ELSE
	MIDI.ERROR = 3
END IF
MIDI.PLAYTIME = 0
END SUB

FUNCTION TimeMIDI!
'If a MIDI file is paused, lock the current playing time
IF PAUSED > 0! THEN
	TimeMIDI! = PAUSED
	MIDI.ERROR = 0
'If a MIDI file is playing, carry out the timing routine
ELSEIF MIDI.PLAYTIME THEN
	'Get the current time
	CurrentTime! = TIMER
	'If midnight has come since the MIDI file started playing, change
	'CurrentTime! accordingly
	IF CurrentTime! - MIDI.PLAYTIME < 0 THEN
		CurrentTime! = 86400 + CurrentTime!
	END IF
	'Get the final result
	TimeMIDI! = CurrentTime! - MIDI.PLAYTIME
	MIDI.ERROR = 0
ELSE
	MIDI.ERROR = 3
END IF
END FUNCTION

REM $STATIC
SUB UnloadMIDI (Handle%)
IF Handle% < 0 OR Handle% > 255 THEN MIDI.ERROR = 13: EXIT SUB
'If a block of memory was allocated to hold the MIDI file....
IF MEM.SEGMENT(Handle%) THEN
	'Release the block of memory.
	QMIDIRegs.ES = MEM.SEGMENT(Handle%)
	QMIDIRegs.AX = &H4900
	CALL IntX(&H21, QMIDIRegs)
	'Give back all the memory we took from BASIC.
	A& = SETMEM(650000)
END IF
MEM.SEGMENT(Handle%) = 0
MEM.ALLOCATED(Handle%) = 0
MIDI.ERROR = 0
END SUB

