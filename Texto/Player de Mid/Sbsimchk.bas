'SBSIMCHK v1.0
'Stand-alone SBSIM driver detection utility
'By Jesse Dorland (jessedorland@hotmail.com)
'Released to the Public Domain, October 1998.
'
'NOTE: SBSIMCHK.BAS will not run in source form with any flavor of
'BASIC.  It must be compiled to work properly.  A compiled version
'(SBSIMCHK.EXE) is provided.
'
'SBSIMCHK is a stand-alone utility for detecting the SBSIM sound
'driver.  The source code is of no use, but when compiled, SBSIMCHK
'provides a way to detect the driver (and load it, if necessary)
'before your program starts, using batch files.  This is handy because,
'while you can check to see if the drivers are loaded while your program
'is running, you can't actually load the drivers.
'
'SBSIMCHK returns an errorlevel; if it returns a non-zero value, it found
'driver.  If it returns 0, it didn't locate SBSIM.
'
'The SBMIDCHK utility works just like SBSIMCHK, except that it detects the
'SBMIDI driver.

DECLARE FUNCTION SBSIMLoaded% ()
DECLARE SUB GetIntVector (IntNum%, Segment&, Offset&)
DECLARE SUB EndWithErrorLevel ALIAS "_Exit" (BYVAL ErrorLevel%)

'Check if the drivers are loaded
ErrorLevel% = SBSIMLoaded%

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

FUNCTION SBSIMLoaded% STATIC
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
DRIVERDATA$(1) = INPUT$(256, #FF%)
END IF

'Close the data file.
CLOSE #FF%

'Check the interrupt handlers for int 80h-FFh, to see if they are occupied
'by either SBMIDI or SBSIM.
SBSIM% = 0
FOR I% = &H80 TO &HFF
    'Get the address of the interrupt handler.
    GetIntVector I%, Segment&, Offset&
    'If the segment returned is 0, that means that the current interrupt
    'is not in use.
    IF Segment& = 0 THEN GOTO Skip:

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
    MATCH% = 1
    FOR k% = 0 TO 255
        IF MID$(DRIVERDATA$(1), k% + 1, 1) <> MID$(DRIVERDATA$(5), k% + 1, 1) THEN
            SELECT CASE k%
                CASE IS = 14, 15, 113, 114, 235, 236
                CASE ELSE
                    MATCH% = 0
                    EXIT FOR
                END SELECT
            END IF
        NEXT k%
    IF MATCH% THEN SBSIM% = I%: EXIT FOR
    END IF
Skip:
NEXT I%
SBSIMLoaded% = SBSIM%
END FUNCTION

