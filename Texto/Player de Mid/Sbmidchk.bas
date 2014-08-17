'SBMIDCHK
'Stand-alone SBMIDI driver detection utility
'By Jesse Dorland (jessedorland@hotmail.com)
'Released to the Public Domain, October 1998.
'
'NOTE: SBMIDCHK.BAS will not run in source form with any flavor of
'BASIC.  It must be compiled to work properly.  A compiled version
'(SBMIDCHK.EXE) is provided.
'
'SBMIDCHK is a stand-alone utility for detecting the SBMIDI sound
'driver.  The source code is of no use, but when compiled, SBMIDCHK
'provides a way to detect the driver (and load it, if necessary)
'before your program starts, using batch files.  This is handy because
'the SBMIDI and SBSIM drivers cannot be loaded properly from any running
'BASIC program.
'
'SBMIDCHK returns an errorlevel.  If it returns 0, it didn't locate SBMIDI.
'If it did locate the driver, it returns a non-zero value, equal to the
'interrupt number that the driver is using.
'
'The SBSIMCHK utility works just like SBMIDCHK, except that it detects the
'SBSIM driver.

DECLARE FUNCTION SBMIDILoaded% ()
DECLARE SUB GetIntVector (IntNum%, Segment&, Offset&)
DECLARE SUB EndWithErrorLevel ALIAS "_Exit" (BYVAL ErrorLevel%)

'Check if the drivers are loaded
ErrorLevel% = SBMIDILoaded%

'Exit with the proper errorlevel
EndWithErrorLevel ErrorLevel%

DEFINT A-Z
SUB GetIntVector (IntNum%, Segment&, Offset&) STATIC
'If the code hasn't been loaded already, do it now.
IF GetIntVCodeLoaded% = 0 THEN
    asm$ = asm$ + CHR$(&H55)
    asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)
    asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&HA)
    asm$ = asm$ + CHR$(&H8A) + CHR$(&H7)
    asm$ = asm$ + CHR$(&HB4) + CHR$(&H35)
    asm$ = asm$ + CHR$(&HCD) + CHR$(&H21)
    asm$ = asm$ + CHR$(&H8C) + CHR$(&HC1)
    asm$ = asm$ + CHR$(&H89) + CHR$(&HDA)
    asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8)
    asm$ = asm$ + CHR$(&H89) + CHR$(&HF)
    asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)
    asm$ = asm$ + CHR$(&H89) + CHR$(&H17)
    asm$ = asm$ + CHR$(&H5D)
    asm$ = asm$ + CHR$(&HCB)
    asm$ = asm$ + CHR$(&H34) + CHR$(&H0)
    asm$ = asm$ + CHR$(&H60)
    asm$ = asm$ + CHR$(&H23) + CHR$(&H0)
    GetIntVCodeLoaded% = 1
END IF
'Execute the code
DEF SEG = VARSEG(asm$)
CALL ABSOLUTE(IntNum%, Segment%, Offset%, SADD(asm$))
Segment& = Segment%
Offset& = Offset%
END SUB

REM $DYNAMIC
DEFSNG A-Z
'DriversLoaded - Attempt to detect if sound drivers are loaded
FUNCTION SBMIDILoaded%
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

SBMIDI% = 0
FOR I% = &H80 TO &HFF
    'Get the address of the interrupt handler.
    GetIntVector I%, Segment&, Offset&
    'If the segment returned is 0, that means that the current interrupt
    'is not in use.
    IF Segment& = 0 THEN GOTO Skip:

    IF SBMIDI% = 0 THEN
      DEF SEG = Segment& - 17
      TEMP$ = ""
      FOR J% = 1 TO 6
        TEMP$ = TEMP$ + CHR$(PEEK(271 + J%))
      NEXT
      IF TEMP$ = "SBMIDI" THEN SBMIDI% = I%
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
    FOR J% = 2 TO 4
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
        IF MATCH% THEN SBMIDI% = I%: EXIT FOR
        
    NEXT J%
  
    IF SBMIDI% <> 0 THEN EXIT FOR
    END IF
Skip:
NEXT I%
SBMIDILoaded% = SBMIDI%

END FUNCTION

