'QMIDI v4.1
'Demo Program
'By Jesse Dorland (jessedorland@hotmail.com)
'
'-----{ START OF CODE }-----
'
'---[ SUBROUTINE DECLARATIONS ]---
'
DECLARE SUB Center (Text$)
DECLARE SUB FillArea (X1%, Y1%, X2%, Y2%, FC%, BC%, Char%)
DECLARE SUB MakeBox (X1%, Y1%, X2%, Y2%, Col%)
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

CLS
FillArea 1, 1, 80, 1, 1, 1, 32
FillArea 1, 25, 80, 25, 1, 1, 32
COLOR 14, 1
Center "QMIDI v4.0 Demonstration"
LOCATE 25, 1
Center "ENTER: Continue     ESC: Quit Demo;;"

'INTRODUCTION MESSAGE
FillArea 1, 2, 80, 24, 11, 1, 177
MakeBox 15, 7, 65, 17, 4
LOCATE 7, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ WELCOME! ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÙ"
Center "This program will show you some of QMIDI's"
Center "features.  At any time during the demo,"
Center "you can press the Enter key to proceed to the"
Center "next screen, and the Escape key to exit the"
Center "program.  There may be other keys you can"
Center "press during certain demonstrations.  These"
Center "keys will be noted on the screen."
DO
A$ = UCASE$(INPUT$(1))
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): COLOR 7, 0: CLS : END
END SELECT
LOOP


'DRIVERSLOADED DEMONSTRATION
FillArea 1, 2, 80, 24, 11, 1, 177
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ SUB DRIVERSLOADED ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "SYNTAX:"
Center "DriversLoaded SBMIDI%, SBSIM%"
PRINT
Center "PURPOSE:"
Center "Detects whether the SBMIDI and SBSIM sound drivers"
Center "are loaded."
PRINT
Center "SBMIDI is required for all QMIDI features.  SBSIM is"
Center "not required at all, but may improve wavetable sound"
Center "quality."
Center "NOTE: DRIVERS.DAT *MUST* be in the current directory."
PRINT
Center "Press Enter to execute the DriversLoaded routine."
DO
A$ = UCASE$(INPUT$(1))
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): COLOR 7, 0: CLS : END
END SELECT
LOOP
DriversLoaded SBMIDI.INTERRUPT, SBSIM.INTERRUPT
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ SUB DRIVERSLOADED ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
IF SBMIDI.INTERRUPT THEN
	Center "SBMIDI found at Interrupt " + HEX$(SBMIDI.INTERRUPT) + "h"
	PRINT
ELSE
	Center "SBMIDI not found!"
	Center "The SBMIDI driver could not be detected.  If you have"
	Center "not loaded SBMIDI.EXE, exit this program and do so."
	Center "If you are sure that SBMIDI is already loaded, you can"
	Center "press Enter to continue anyway.  However, if you choose"
	Center "to continue and the driver is not loaded, this program"
	Center "will not work properly."
	PRINT
	Center "Press Enter to continue, or Escape to quit."
	PRINT
	DO
		A$ = UCASE$(INPUT$(1))
		SELECT CASE A$
			CASE CHR$(13): EXIT DO
			CASE CHR$(27): COLOR 7, 0: CLS : END
		END SELECT
	LOOP
	SBMIDI.INTERRUPT = &H80
	MakeBox 10, 4, 70, 20, 4
	LOCATE 4, 1
	COLOR 15, 4
	Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	Center "³ SUB DRIVERSLOADED ³"
	Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
END IF
IF SBSIM.INTERRUPT THEN
	Center "SBSIM found at Interrupt " + HEX$(SBSIM.INTERRUPT) + "h"
	PRINT
	Center "Press Enter to continue."
ELSE
	Center "SBSIM not found!"
	Center "The SBSIM driver could not be detected.  If you have"
	Center "not loaded SBSIM.COM, exit this program and do so."
	Center "If you are sure that SBSIM is already loaded, you can"
	Center "press Enter to continue anyway.  However, if you choose"
	Center "to continue and the driver is not loaded, some parts"
	Center "of this program will not work properly."
	PRINT
	Center "Press Enter to continue, or Escape to quit."
	SBSIM.INTERRUPT = &H81
END IF
DO
	A$ = UCASE$(INPUT$(1))
	SELECT CASE A$
		CASE CHR$(13): EXIT DO
		CASE CHR$(27): COLOR 7, 0: CLS : END
	END SELECT
LOOP


'DETECTSETTINGS DEMONSTRATION
MakeBox 10, 3, 70, 21, 4
LOCATE 3, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ SUB DETECTSETTINGS ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "SYNTAX:"
Center "DetectSettings BasePort%, IRQ%, LoDMA%, HiDMA%, CardType%,"
Center "MPU401%"
PRINT
Center "PURPOSE:"
Center "Returns detailed information about the user's sound card."
PRINT
Center "DetectSettings attempts to detect various sound card"
Center "settings, including card type, I/O port, IRQ, and DMA."
Center "Currently, only the card type and I/O port affect how"
Center "QMIDI functions.  However, this may change in future"
Center "versions."
PRINT
Center "Press Enter to execute the DetectSettings routine."
DO
A$ = UCASE$(INPUT$(1))
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): COLOR 7, 0: CLS : END
END SELECT
LOOP
DetectSettings SB.BASEPORT, SB.IRQ, SB.LODMA, SB.HIDMA, SB.CARDTYPE, SB.MPU401
SC$ = SoundCard$(SB.CARDTYPE)
MakeBox 10, 3, 70, 21, 4
LOCATE 3, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ SUB DETECTSETTINGS ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "Sound Card: " + SC$
PRINT
IF SC$ = "Unknown" THEN
	Center "The sound card could not be detected!  Without knowing"
	Center "what type of card is installed, QMIDI cannot properly"
	Center "perform mixer operations.  You may either manually"
	Center "select your sound card, or continue the demonstration,"
	Center "skipping the mixer control section of the demo."
	PRINT
	Center "Press 1 if you have an SB2.0 CD or compatible card."
	Center "Press 2 if you have an SB Pro or compatible card."
	Center "Press 3 if you have an SB16 or compatible card."
	PRINT
	Center "To skip the mixer control section, press Enter."
	DO
	A$ = UCASE$(INPUT$(1))
	SELECT CASE A$
		CASE CHR$(13): MIXER.CHIP = 0: EXIT DO
		CASE "1": MIXER.CHIP = 1: EXIT DO
		CASE "2": MIXER.CHIP = 2: EXIT DO
		CASE "3": MIXER.CHIP = 3: EXIT DO
		CASE CHR$(27): COLOR 7, 0: CLS : END
	END SELECT
	LOOP
ELSE
	Center "Please confirm that the above card setting is correct"
	Center "by pressing Enter.  If the above setting is incorrect,"
	Center "please choose your card from the list below.  If you're"
	Center "not sure what sound card you have, press Enter.  Please"
	Center "note that, if the sound card setting is incorrect, the"
	Center "mixer control routines may behave strangely."
	PRINT
	Center "Press 1 if you have an SB 1.0, 1.5, 2.0 or compatible card."
	Center "Press 2 if you have an SB 2.0CD or compatible card."
	Center "Press 3 if you have an SB Pro, Pro 2 or compatible card."
	Center "Press 4 if you have an SB 16, AWE32 or compatible card."
	PRINT
	Center "Press Enter to use the current setting."
	DO
	A$ = UCASE$(INPUT$(1))
	SELECT CASE A$
		CASE CHR$(13): EXIT DO
		CASE "1": MIXER.CHIP = 0: EXIT DO
		CASE "2": MIXER.CHIP = 1: EXIT DO
		CASE "3": MIXER.CHIP = 2: EXIT DO
		CASE "4": MIXER.CHIP = 3: EXIT DO
		CASE CHR$(27): COLOR 7, 0: CLS : END
	END SELECT
	LOOP
END IF
MakeBox 10, 3, 70, 21, 4
LOCATE 3, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ SUB DETECTSETTINGS ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "I/O Port: " + LTRIM$(RTRIM$(HEX$(SB.BASEPORT))) + "h"
PRINT
Center "Please confirm that the above setting is correct"
Center "by pressing Enter.  If it is incorrect, you may specify"
Center "a different I/O port from the list below.  If you're not"
Center "sure which I/O port your sound card uses, press Enter."
Center "NOTE: If the I/O port setting is incorrect, the mixer"
Center "control routines may behave strangely."
PRINT
Center "Press 1 if your sound card's I/O port is 210h.  If it's"
Center "220h, press 2.  Press 3 if it's 230h.  Press 4 if it's"
Center "240h.  Press 5 for 250h.  Or, press 6 for 260h."
PRINT
Center "Press Enter to use the current setting."
DO
A$ = UCASE$(INPUT$(1))
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE "1": SB.BASEPORT = &H210: EXIT DO
	CASE "2": SB.BASEPORT = &H220: EXIT DO
	CASE "3": SB.BASEPORT = &H230: EXIT DO
	CASE "4": SB.BASEPORT = &H240: EXIT DO
	CASE "5": SB.BASEPORT = &H250: EXIT DO
	CASE "6": SB.BASEPORT = &H260: EXIT DO
	CASE CHR$(27): COLOR 7, 0: CLS : END
END SELECT
LOOP
MakeBox 10, 3, 70, 21, 4
LOCATE 3, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ SUB DETECTSETTINGS ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "SOUND CARD SETTINGS"
PRINT
Center "Sound Card: " + SoundCard$(SB.CARDTYPE)
Center "Mixer Chip: " + MixerChip$
PRINT
Center "Base Port: " + LTRIM$(RTRIM$(HEX$(SB.BASEPORT))) + "h"
Center "MPU-401 Port: " + LTRIM$(RTRIM$(HEX$(SB.MPU401))) + "h"
Center "IRQ:" + STR$(SB.IRQ)
Center "8-bit DMA:" + STR$(SB.LODMA)
Center "16-bit DMA:" + STR$(SB.HIDMA)
PRINT
Center "Press Enter to continue."
DO
A$ = UCASE$(INPUT$(1))
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): COLOR 7, 0: CLS : END
END SELECT
LOOP

'LOADMIDI DEMONSTRATION
FillArea 1, 2, 80, 24, 11, 1, 177
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ FUNCTION LOADMIDI ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "SYNTAX:"
Center "Handle% = LoadMIDI(Filename$)"
PRINT
Center "PURPOSE:"
Center "Loads a MIDI file into memory."
PRINT
Center "Before a MIDI file can be played, it must be loaded"
Center "into memory.  LoadMIDI will return a MIDI HANDLE, a"
Center "number that represents the MIDI file."
PRINT
Center "Press Enter to load a MIDI file."
DO
A$ = UCASE$(INPUT$(1))
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): COLOR 7, 0: CLS : END
END SELECT
LOOP
MIDIHandle% = LoadMIDI("SAMPLE1.MID")
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
IF MIDI.ERROR THEN
	LOCATE 4, 1
	COLOR 15, 4
	Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	Center "³ SUB LOADMIDI ³"
	Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
	SELECT CASE MIDI.ERROR
		CASE 1
			Center "ERROR: The MIDI file could not be found.  Make sure that"
			Center "SAMPLE1.MID is present in the current directory."
	END SELECT
	PRINT
	Center "Press a key to quit."
	DO UNTIL LEN(INKEY$): LOOP
	COLOR 7, 0: CLS : END
END IF

'PLAYMIDI DEMONSTRATION
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ SUB PLAYMIDI ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "SYNTAX:"
Center "PlayMIDI Handle%"
PRINT
Center "PURPOSE:"
Center "Begins playing a MIDI file."
PRINT
Center "Before calling PlayMIDI, you must load a MIDI"
Center "file using the LoadMIDI routine."
PRINT
Center "Press Enter to begin playing music."
DO
A$ = UCASE$(INPUT$(1))
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): COLOR 7, 0: CLS : StopMIDI: UnloadMIDI MIDIHandle%:  END
END SELECT
LOOP
PlayMIDI MIDIHandle%
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ SUB PLAYMIDI ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "You should now be listening to music!"
PRINT
Center "Press Enter if you hear music."
Center "Press 1 if you don't."
PRINT
DO
A$ = UCASE$(INPUT$(1))
SELECT CASE A$
	CASE CHR$(13): HeardMusic% = 1: EXIT DO
	CASE "1": HeardMusic% = 0: EXIT DO
	CASE CHR$(27): StopMIDI: UnloadMIDI MIDIHandle%: COLOR 7, 0: CLS : END
END SELECT
LOOP
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ SUB PLAYMIDI ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
IF HeardMusic% = 0 THEN
	StopMIDI
	Center "If you didn't hear sound, here are a few suggestions:"
	PRINT
	Center "1) Make sure the SBMIDI driver is properly loaded."
	Center "2) Try loading SB122OLD.EXE instead of SBMIDI.EXE."
	Center "3) If you have an ESS AudioDrive sound card, boot your"
	Center "computer in DOS mode - don't exit to DOS from inside"
	Center "Windows, but boot directly to DOS.  See the QMIDI"
	Center "documentation for details."
	Center "4) If you have an ESS AudioDrive sound card with Software"
	Center "Wavetable, and wish to use QMIDI in Windows, load SBMIDI"
	Center "with the /3 switch (NOTE: This is VERY slow)."
	PRINT
	Center "Press Enter to continue the demo, or Escape to quit."
	DO
	A$ = UCASE$(INPUT$(1))
	SELECT CASE A$
		CASE CHR$(13): EXIT DO
		CASE CHR$(27): COLOR 7, 0: CLS : END
	END SELECT
	LOOP
ELSE
	Center "The music is playing in the background.  The program"
	Center "can continue to execute normally.  Don't believe it?"
	Center "Look below:"
	PRINT
	X% = 11
	XINC% = 1
	ORIGY% = CSRLIN + 1
	Y% = ORIGY%
	YINC% = 1
	LOCATE Y%, X%: PRINT "Press Enter to Continue!"
	A$ = ""
	DO
		A$ = INKEY$
		SELECT CASE A$
			CASE CHR$(13): EXIT DO
			CASE CHR$(27): StopMIDI: UnloadMIDI MIDIHandle%: COLOR 7, 0: CLS : END
		END SELECT
		FOR I% = 1 TO 6: WAIT &H3DA, 8, 8: WAIT &H3DA, 8: NEXT I%
		LOCATE Y%, X%: PRINT SPACE$(24)
		X% = X% + XINC%
		IF X% < 12 OR X% > 45 THEN XINC% = -XINC%
		Y% = Y% + YINC%
		IF Y% < ORIGY% OR Y% > 18 THEN YINC% = -YINC%
		LOCATE Y%, X%: PRINT "Press Enter to Continue!"
	LOOP
END IF

'TIMEMIDI! DEMONSTRATION
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ FUNCTION TIMEMIDI ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "SYNTAX:"
Center "A! = TimeMIDI!"
PRINT
Center "PURPOSE:"
Center "Returns the amount of time music has been playing."
PRINT
Center "According to the TimeMIDI function, music has been"
Center "playing for this many seconds:"
PRINT
ORIGY% = CSRLIN
LOCATE ORIGY% + 2
Center "Press Enter to continue."
DO
A$ = INKEY$
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): StopMIDI: UnloadMIDI MIDIHandle%: COLOR 7, 0: CLS : END
END SELECT
LOCATE ORIGY%, 1
Center "  " + LTRIM$(RTRIM$(STR$(INT(TimeMIDI!)))) + "  "
LOOP
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ SUB PAUSEMIDI ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "SYNTAX:"
Center "PauseMIDI"
PRINT
Center "PURPOSE:"
Center "Pauses any music that is currently playing."
PRINT
Center "PauseMIDI allows you to stop the music, then"
Center "resume playing at the exact point where the"
Center "music was stopped.  Pausing may not work"
Center "properly when using wavetable synthesis."
PRINT
Center "Press Enter to pause the music."
DO
A$ = INKEY$
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): StopMIDI: UnloadMIDI MIDIHandle%: COLOR 7, 0: CLS : END
END SELECT
LOOP
PauseMIDI
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ SUB RESUMEMIDI ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "SYNTAX:"
Center "ResumeMIDI"
PRINT
Center "PURPOSE:"
Center "Resumes music playing after it has been paused."
PRINT
Center "ResumeMIDI is used to start playing music again after"
Center "it has been paused with PauseMIDI."
PRINT
Center "Press Enter to resume playing."
DO
A$ = INKEY$
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): StopMIDI: UnloadMIDI MIDIHandle%: COLOR 7, 0: CLS : END
END SELECT
LOOP
ResumeMIDI
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ SUB RESUMEMIDI ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "You should be listening to music again."
PRINT
Center "Press Enter to continue."
DO
A$ = INKEY$
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): StopMIDI: UnloadMIDI MIDIHandle%: COLOR 7, 0: CLS : END
END SELECT
LOOP
StopMIDI

'STOPMIDI DEMONSTRATION
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ SUB STOPMIDI ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "SYNTAX:"
Center "StopMIDI"
PRINT
Center "PURPOSE:"
Center "Stops any music that is currently playing."
PRINT
PRINT
PRINT
PRINT
Center "Press Enter to stop the music."
DO
A$ = UCASE$(INPUT$(1))
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): COLOR 7, 0: CLS : END
END SELECT
LOOP
StopMIDI

'UNLOADMIDI DEMONSTRATION
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ SUB UNLOADMIDI ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "SYNTAX:"
Center "UnloadMIDI Handle%"
PRINT
Center "PURPOSE:"
Center "Removes a MIDI file from memory."
PRINT
Center "MIDI files MUST be unloaded before your program"
Center "ends.  Otherwise, the files can't be unloaded at"
Center "all, and you'll have to reboot your computer to"
Center "get the memory back."
PRINT
Center "Press Enter to unload the MIDI file."
DO
A$ = UCASE$(INPUT$(1))
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): COLOR 7, 0: CLS : END
END SELECT
LOOP
UnloadMIDI MIDIHandle%

'LOOPMIDI DEMONSTRATION
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ SUB LOOPMIDI ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "SYNTAX:"
Center "LoopMIDI"
PRINT
Center "PURPOSE:"
Center "Restarts the music if it has finished playing."
PRINT
Center "LoopMIDI checks to see if the current MIDI file"
Center "has finished playing.  If it has, the music loops"
Center "back to the beginning."
PRINT
Center "Press Enter to hear an example."
DO
A$ = UCASE$(INPUT$(1))
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): COLOR 7, 0: CLS : END
END SELECT
LOOP
MIDIHandle% = LoadMIDI("SAMPLE5.MID")
IF MIDI.ERROR THEN
	LOCATE 4, 1
	COLOR 15, 4
	Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
	Center "³ SUB LOOPMIDI ³"
	Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
	SELECT CASE MIDI.ERROR
		CASE 1
			Center "ERROR: The MIDI file could not be found.  Make sure that"
			Center "SAMPLE5.MID is present in the current directory."
	END SELECT
	PRINT
	Center "Press a key to quit."
	DO UNTIL LEN(INKEY$): LOOP
	COLOR 7, 0: CLS : END
END IF

PlayMIDI MIDIHandle%
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ SUB LOOPMIDI ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
PRINT
Center "You should now be listening to a short melody,"
Center "which will loop continuously."
PRINT
Center "Press Enter to continue."
DO
A$ = INKEY$
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): StopMIDI: UnloadMIDI MIDIHandle%: COLOR 7, 0: CLS : END
END SELECT
LoopMIDI
LOOP
StopMIDI
UnloadMIDI MIDIHandle%

IF MIXER.CHIP = 0 THEN GOTO SkipMixer
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ MIXER CONTROL ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "QMIDI's mixer control routines allow you to control"
Center "Master, MIDI and Voice volume settings, as well as"
Center "Bass and Treble settings on certain sound cards."
PRINT
Center "To begin the mixer control demonstration, the current"
Center "volume levels (and, if supported, the Bass and Treble"
Center "levels) will be printed on the screen."
PRINT
Center "Press Enter to continue."
DO
A$ = INKEY$
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): StopMIDI: UnloadMIDI MIDIHandle%: COLOR 7, 0: CLS : END
END SELECT
LOOP
GetMaster MasterL%, MasterR%
GetMIDI MIDIL%, MIDIR%
GetVoice VoiceL%, VoiceR%
IF MIXER.CHIP = 3 THEN
	GetBass BassL%, BassR%
	GetTreble TrebleL%, TrebleR%
END IF
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ MIXER CONTROL ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "CURRENT MIXER SETTINGS:"
PRINT
Center "Master Volume: Left" + STR$(MasterL%) + ", Right" + STR$(MasterR%)
Center "MIDI Volume: Left" + STR$(MIDIL%) + ", Right" + STR$(MIDIR%)
Center "Voice Volume: Left" + STR$(VoiceL%) + ", Right" + STR$(VoiceR%)
IF MIXER.CHIP = 3 THEN
	Center "Bass Level: Left" + STR$(BassL%) + ", Right" + STR$(BassR%)
	Center "Treble Level: Left" + STR$(TrebleL%) + ", Right" + STR$(BassR%)
END IF
PRINT
Center "On the following screens, you'll be shown some of the"
Center "things you can do with mixer control, like music fading"
Center "and panning."
PRINT
Center "Press Enter to continue."
DO
A$ = INKEY$
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): StopMIDI: UnloadMIDI MIDIHandle%: COLOR 7, 0: CLS : END
END SELECT
LOOP
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ MUSIC FADING ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "Press Enter to being the fading demonstration.  Music"
Center "will begin playing.  It will play for a few seconds,"
Center "and then the volume will gradually decrease.  When"
Center "there is no sound at all, the volume will return to"
Center "its original level, and the music will play for a few"
Center "more seconds before stopping."
DO
A$ = INKEY$
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): StopMIDI: UnloadMIDI MIDIHandle%: COLOR 7, 0: CLS : END
END SELECT
LOOP
PRINT
GetMIDI OrigL%, OrigR%
LeftChan% = OrigL%
RightChan% = OrigL%
SetMIDI LeftChan%, RightChan%
MIDIHandle% = LoadMIDI("SAMPLE3.MID")
PlayMIDI MIDIHandle%
Center "Playing...;;"
DO UNTIL TimeMIDI! > 5: LOOP
Center "Fading Out...;;"
FOR I% = LeftChan% TO 0 STEP -1
	FOR J% = 1 TO 10: WAIT &H3DA, 8, 8: WAIT &H3DA, 8: NEXT J%
	SetMIDI I%, I%
NEXT I%
Center "  Fading In...  ;;"
FOR I% = 0 TO LeftChan%
	FOR J% = 1 TO 10: WAIT &H3DA, 8, 8: WAIT &H3DA, 8: NEXT J%
	SetMIDI I%, I%
NEXT I%
Center "    Playing...    ;;"
TM! = TimeMIDI!
DO UNTIL TimeMIDI! > TM! + 5: LOOP
StopMIDI
UnloadMIDI MIDIHandle%
SetMIDI OrigL%, OrigR%
Center "        Done!        "
PRINT
Center "Press Enter if the music faded out, then back in."
Center "Press 1 if there was no change in volume, or if the"
Center "music cut out completely."
DO
A$ = INKEY$
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE "1"
		MakeBox 10, 4, 70, 20, 4
		LOCATE 4, 1
		COLOR 15, 4
		Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
		Center "³ MUSIC FADING ³"
		Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		Center "If the volume did not change at all, or if the music stopped"
		Center "abruptly without fading:"
		PRINT
		Center "1) Make sure that the sound card type and I/O port settings"
		Center "are correctly detected when the DetectSettings routine is run."
		Center "2) Volume control requires a Sound Blaster 2.0CD, SB Pro,"
		Center "SB16, or compatible sound card.  If your sound card is not"
		Center "compatible with any of these cards, volume control will"
		Center "probably not work."
		PRINT
		Center "Press Enter to continue or Escape to exit."
		DO
		A$ = INKEY$
		SELECT CASE A$
			CASE CHR$(13): EXIT DO
			CASE CHR$(27): StopMIDI: UnloadMIDI MIDIHandle%: COLOR 7, 0: CLS : END
		END SELECT
		LOOP
	CASE CHR$(27): StopMIDI: UnloadMIDI MIDIHandle%: COLOR 7, 0: CLS : END
END SELECT
LOOP
PanningDemo:
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ MUSIC PANNING ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "Press Enter to being the panning demonstration.  Music"
Center "will begin playing.  You should only hear music coming"
Center "from the left speaker.  After a few seconds, the music"
Center "should pan to the right, until you only hear music from"
Center "the right speaker.  Then, it will pan back to the left,"
Center "play for a few more seconds, and stop."
DO
A$ = INKEY$
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): StopMIDI: UnloadMIDI MIDIHandle%: COLOR 7, 0: CLS : END
END SELECT
LOOP
PRINT
GetMIDI OrigL%, OrigR%
SetMIDI OrigL%, 0
MIDIHandle% = LoadMIDI("SAMPLE4.MID")
PlayMIDI MIDIHandle%
Center "Playing...;;"
DO UNTIL TimeMIDI! > 5: LOOP
Center "Panning Right...;;"
FOR I% = 0 TO OrigL%
	FOR J% = 1 TO 10: WAIT &H3DA, 8, 8: WAIT &H3DA, 8: NEXT J%
	SetMIDI OrigL%, I%
NEXT I%
FOR I% = OrigL% TO 0 STEP -1
	FOR J% = 1 TO 10: WAIT &H3DA, 8, 8: WAIT &H3DA, 8: NEXT J%
	SetMIDI I%, OrigL%
NEXT I%
Center "  Panning Left...  ;;"
FOR I% = 0 TO OrigL%
	FOR J% = 1 TO 10: WAIT &H3DA, 8, 8: WAIT &H3DA, 8: NEXT J%
	SetMIDI I%, OrigL%
NEXT I%
FOR I% = OrigL% TO 0 STEP -1
	FOR J% = 1 TO 10: WAIT &H3DA, 8, 8: WAIT &H3DA, 8: NEXT J%
	SetMIDI OrigL%, I%
NEXT I%
Center "      Playing...      ;;"
TM! = TimeMIDI!
DO UNTIL TimeMIDI! > TM! + 5: LOOP
StopMIDI
UnloadMIDI MIDIHandle%
SetMIDI OrigL%, OrigR%
Center "        Done!        "
PRINT
Center "Press Enter if the music panned correctly."
Center "Press 1 if there was no change in the position of the"
Center "music, or if the music cut out completely."
Center "Press 2 if the music panned from right to left and back."
DO
A$ = INKEY$
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE "1"
		MakeBox 10, 4, 70, 20, 4
		LOCATE 4, 1
		COLOR 15, 4
		Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
		Center "³ MUSIC PANNING ³"
		Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		Center "If the music position did not change at all, or if the music"
		Center "stopped abruptly without fading:"
		PRINT
		Center "1) Make sure that the sound card type and I/O port settings"
		Center "are correctly detected when the DetectSettings routine is run."
		Center "2) Volume control requires a Sound Blaster 2.0CD, SB Pro,"
		Center "SB16, or compatible sound card.  If your sound card is not"
		Center "compatible with any of these cards, volume control will"
		Center "probably not work."
		PRINT
		Center "Press Enter to continue or Escape to exit."
		DO
		A$ = INKEY$
		SELECT CASE A$
			CASE CHR$(13): EXIT DO
			CASE CHR$(27): StopMIDI: UnloadMIDI MIDIHandle%: COLOR 7, 0: CLS : END
		END SELECT
		LOOP
	CASE "2"
		MakeBox 10, 4, 70, 20, 4
		LOCATE 4, 1
		COLOR 15, 4
		Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
		Center "³ MUSIC PANNING ³"
		Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
		Center "Some sound cards and speakers seem to reverse the left and"
		Center "right audio channels for no apparent reason.  This will"
		Center "cause music panning to be reversed.  To correct this, your"
		Center "programs can place a value of 1 in the REVERSE.STEREO"
		Center "variable, like this:"
		PRINT
		Center "REVERSE.STEREO = 1"
		PRINT
		Center "Press Enter to continue."
		Center "Press 1 to restart the music panning demo, with Reverse"
		Center "Stereo enabled."
		DO
		A$ = INKEY$
		SELECT CASE A$
			CASE CHR$(13): EXIT DO
			CASE "1": REVERSE.STEREO = 1: GOTO PanningDemo
			CASE CHR$(27): StopMIDI: UnloadMIDI MIDIHandle%: COLOR 7, 0: CLS : END
		END SELECT
		LOOP
	CASE CHR$(27): StopMIDI: UnloadMIDI MIDIHandle%: COLOR 7, 0: CLS : END
END SELECT
LOOP
SkipMixer:

'CLEANUPMIDI DEMONSTRATION
MakeBox 10, 4, 70, 20, 4
LOCATE 4, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ SUB CLEANUPMIDI ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
Center "SYNTAX:"
Center "CleanUpMIDI"
PRINT
Center "PURPOSE:"
Center "Removes all MIDI files from memory."
PRINT
Center "It's a good idea to call this routine at the end"
Center "of your program to make sure that all MIDI files"
Center "are unloaded."
PRINT
PRINT
Center "Press Enter to continue."
DO
A$ = UCASE$(INPUT$(1))
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): COLOR 7, 0: CLS : END
END SELECT
LOOP
CleanUpMIDI

'ENDING MESSAGE
FillArea 1, 2, 80, 24, 11, 1, 177
MakeBox 15, 7, 65, 17, 4
LOCATE 7, 1
COLOR 15, 4
Center "ÚÄÄÄÄÄÄÄÄÄÄ¿"
Center "³ BYE BYE! ³"
Center "ÀÄÄÄÄÄÄÄÄÄÄÙ"
Center "That's it for the demonstration.  I hope you"
Center "have a good idea of what QMIDI is capable of."
Center "There's a lot more information in the"
Center "documentation (QMIDI.DOC), so be sure to take"
Center "a look at it before you start using QMIDI."
PRINT
Center "Press Enter to quit."
DO
A$ = UCASE$(INPUT$(1))
SELECT CASE A$
	CASE CHR$(13): EXIT DO
	CASE CHR$(27): COLOR 7, 0: CLS : END
END SELECT
LOOP
COLOR 7, 0
CLS
END

REM $STATIC
SUB Center (Text$)
IF RIGHT$(Text$, 2) = ";;" THEN
	NoCR% = 1
	Text$ = LEFT$(Text$, LEN(Text$) - 2)
ELSE
	NoCR% = 0
END IF
LOCATE , 40 - (LEN(Text$) \ 2)
PRINT Text$;
IF NoCR% = 0 THEN PRINT
END SUB

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

REM $STATIC
SUB FillArea (X1%, Y1%, X2%, Y2%, FC%, BC%, Char%)
Row% = CSRLIN
Col% = POS(0)
FOR I% = Y1% TO Y2%
	LOCATE I%, X1%
	COLOR FC%, BC%
	PRINT STRING$((X2% - X1%) + 1, Char%);
NEXT I%
LOCATE Row%, Col%
COLOR 7, 0
END SUB

REM $DYNAMIC
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

SUB MakeBox (X1%, Y1%, X2%, Y2%, Col%)
FillArea X1%, Y1%, X2%, Y2%, Col%, Col%, 32
FillArea X2% + 1, Y1% + 1, X2% + 1, Y2% + 1, 7, 0, 177
FillArea X1% + 1, Y2% + 1, X2% + 1, Y2% + 1, 7, 0, 177
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

