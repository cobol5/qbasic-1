'Copyright (C) HiTech Labs, Denis Andrianov
'Properties for WinDos.
'============================================================================

'$INCLUDE: 'mouse.bi'
'$INCLUDE: 'dos.bi'


DECLARE SUB Interface ()
DECLARE SUB SetPal (Attribute%, Red%, Green%, Blue%)

DECLARE FUNCTION FileOpen% (File$, Handle%)
DECLARE FUNCTION FileClose% (BYVAL Handle%)
DECLARE FUNCTION FileADateTimeGet% (BYVAL Handle%, Years%, Months%, Days%)
DECLARE FUNCTION FileADateTimeSet% (BYVAL Handle%, BYVAL Years%, BYVAL Months%, BYVAL Days%)
DECLARE FUNCTION FileMDateTimeGet% (BYVAL Handle%, Years%, Months%, Days%, Hours%, Minutes%, Seconds%)
DECLARE FUNCTION FileMDateTimeSet% (BYVAL Handle%, BYVAL Years%, BYVAL Months%, BYVAL Days%, BYVAL Hours%, BYVAL Minutes%, BYVAL Seconds%)
DECLARE FUNCTION FileAttrSet% (File$, BYVAL Attr%)
'DECLARE FUNCTION FileAttrGet% (File$)
DECLARE SUB File2Video (Filename$)
DECLARE SUB DiskInfoGet (Drive$, Serial$, Volume$)
DECLARE SUB DiskSpaceGet (Drive$)

DECLARE FUNCTION CalcDirSize% (Directory$)
DECLARE FUNCTION ReadDir% (DirSpec$, BYVAL SearchType%, BYVAL Filter%)
DECLARE SUB ReadINI ()

DECLARE SUB TickButton (BYVAL x%, BYVAL y%, BYVAL ButtonType%, BYVAL Size%)
DECLARE SUB TinyButton (BYVAL x%, BYVAL y%, BYVAL x2%, BYVAL y2%, BYVAL Colour%, BYVAL TopCol%, BYVAL BotCol%)
DECLARE SUB LoadFont (Text$, BYVAL Textx%, BYVAL Texty%, BYVAL Size%, BYVAL Bold%, BYVAL Colour%)
DECLARE SUB InputLine (BYVAL LocateX%, BYVAL LocateY%, BYVAL Init%, BYVAL Limit%, BYVAL StartScroll%)
DECLARE SUB UpdateTicks ()

DECLARE FUNCTION AddSlash$ (Strng$)
DECLARE FUNCTION RoundOff& (Number!)
DECLARE FUNCTION InStr2Num% (Text$, Separator$, BYVAL ResetFunc%)

DECLARE FUNCTION ConventionalSize% ()
DECLARE FUNCTION DOSinHMA% ()

DECLARE FUNCTION DosVerGet$ ()
DECLARE FUNCTION BiosDateGet$ ()
DECLARE FUNCTION VerifyGet% ()
DECLARE SUB EmptyKeyb ()

DECLARE FUNCTION Assign2Lng& (Number%)


CONST MainINI = "windos.ini"
CONST MainFNT = "windos.fnt"
CONST FindFirst = &H4E00         'Functions of interrupt 21h.
CONST FindNext = &H4F00
CONST ProgramSize = 89800

DIM SHARED GraphicsPath$
DIM SHARED GraphicsPathSlash$
DIM SHARED PalPath$
DIM SHARED PalFile$
DIM SHARED FontsPath$
DIM SHARED TempPath$
DIM SHARED XmsSpd%              'X mouse speed.
DIM SHARED YmsSpd%              'Y mouse speed.

DIM SHARED WindosFont$(255)

CONST MaxFileArray = 2500
DIM SHARED FileArray$(1 TO MaxFileArray)

DIM SHARED DriveInfoMode%
DIM SHARED TotalDiskSpace@
DIM SHARED FreeDiskSpace@
DIM SHARED UsedDiskSpace@
DIM SHARED SizeOfAllFiles@      'Sum of files in subdirectories.
DIM SHARED NumOfAllFiles@       'Total number of files in subdirectories.

DIM SHARED FileMDate$                       'File modification date.
DIM SHARED FileMTime$                       'File modification time.
DIM SHARED FileADate$                       'File access date.

DIM SHARED CommandLine$
DIM SHARED FileCount&
DIM SHARED Attributes%

DIM SHARED CurrencyPattern$
DIM SHARED LongPattern$

DIM SHARED DiskNotReady%
DIM SHARED FileOpened%
DIM SHARED Word$

DIM SHARED Disk AS DiskBlockStructure
DIM SHARED DiskProperty AS ExtendedDiskStructure
DIM SHARED DTA AS DTAStructure
DIM SHARED Regs AS RegTypeX

'============================================================================

'**********************
'Defaults.
'**********************

id$ = "Copyright (C) HiTech Labs, Denis Andrianov"
Version$ = "2.01"
Esc$ = CHR$(27)
F10$ = CHR$(0) + CHR$(68)

CurrencyPattern$ = "###,###,###,###,###"
LongPattern$ = "#,###,###,###"


ReadINI

CommandLine$ = COMMAND$
CommandLen% = LEN(CommandLine$)

'**********************************************
'Determine the mode for the program to work in.
'**********************************************

  IF CommandLen% = 0 THEN
    PRINT "This program is automatically used by WinDos."
    END

'********************************
'Mode 1 = show drive information.
'********************************

  ELSEIF CommandLen% = 1 THEN
    DriveInfoMode% = 1

'********************************
'Mode 0 = show file information.
'Collect file information.
'********************************

  ELSE

    DriveInfoMode% = 0



      IF LEFT$(CommandLine$, 1) <> "@" THEN

        FileCount& = 1
        FileArray$(1) = CommandLine$

      ELSE

        File% = FREEFILE                    'Reading list file into array...
        OPEN AddSlash$(TempPath$) + MID$(CommandLine$, 2) FOR INPUT AS #File%

          DO UNTIL EOF(File%)
            IF FileCount& >= MaxFileArray THEN
            EXIT DO
            END IF
          FileCount& = FileCount& + 1
          LINE INPUT #File%, Temp$
          Temp$ = LTRIM$(RTRIM$(Temp$))
          FileArray$(FileCount&) = Temp$
          LOOP

        CLOSE #File%
      END IF

  END IF


'*******************************
'Check for the mouse.
'*******************************

  IF NOT MouseInit% THEN
  PRINT "Mouse driver not found."
  END
  END IF


'*******************************
'Load the font.
'*******************************

File% = FREEFILE
OPEN AddSlash$(FontsPath$) + MainFNT FOR INPUT AS #File%
  FOR Counter% = 1 TO 255
  LINE INPUT #File%, WindosFont$(Counter%)
  NEXT
CLOSE #File%

'============================================================================

Interface

'************************
'Set up the mouse...
'************************

MouseSpeed XmsSpd%, YmsSpd%
MouseRange 0, 0, 638, 478
MouseShow

'============================================================================


  DO
  KH$ = INKEY$
  MouseStatus Lb%, Rb%, xMouse%, yMouse%
  'LOCATE 1, 1: PRINT xMouse%, yMouse%

    IF KH$ = Esc$ OR KH$ = F10$ THEN
    MouseHide
    EXIT DO
    END IF



    SELECT CASE Lb%

    CASE -1

      IF ButDown% = 0 THEN
      ButDown% = 1
      MouseHide

        IF xMouse% > 190 AND xMouse% < 310 AND yMouse% > 420 AND yMouse% < 460 AND DriveInfoMode% = 0 THEN
        ButPrs% = 1       'Apply is pressed...
        TinyButton 190, 420, 310, 460, 4, 0, 15
        LoadFont "Apply", 236, 435, 4, 1, 13

        ELSEIF xMouse% > 330 AND xMouse% < 450 AND yMouse% > 420 AND yMouse% < 460 AND DriveInfoMode% = 0 THEN
        ButPrs% = 2       'Cancel is pressed...
        TinyButton 330, 420, 450, 460, 4, 0, 15
        LoadFont "Cancel", 371, 435, 4, 1, 13

        ELSEIF xMouse% > 270 AND xMouse% < 370 AND yMouse% > 420 AND yMouse% < 460 AND DriveInfoMode% = 1 THEN
        ButPrs% = 3       'Exit is pressed...
        TinyButton 270, 420, 370, 460, 4, 0, 15
        LoadFont "Exit", 311, 435, 4, 1, 13

        ELSEIF xMouse% > 90 AND xMouse% < 230 AND yMouse% > 175 AND yMouse% < 195 AND DriveInfoMode% = 0 THEN
        Attributes% = Attributes% XOR 1      'Read-Only
        UpdateTicks

        ELSEIF xMouse% > 90 AND xMouse% < 230 AND yMouse% > 205 AND yMouse% < 225 AND DriveInfoMode% = 0 THEN
        Attributes% = Attributes% XOR 2      'Hidden
        UpdateTicks

        ELSEIF xMouse% > 90 AND xMouse% < 230 AND yMouse% > 235 AND yMouse% < 255 AND DriveInfoMode% = 0 THEN
        Attributes% = Attributes% XOR 4      'System
        UpdateTicks

        ELSEIF xMouse% > 90 AND xMouse% < 230 AND yMouse% > 265 AND yMouse% < 285 AND DriveInfoMode% = 0 THEN
        Attributes% = Attributes% XOR 32     'Archive
        UpdateTicks

        ELSEIF xMouse% > 270 AND xMouse% < 360 AND yMouse% > 175 AND yMouse% < 192 AND DriveInfoMode% = 0 AND FileOpened% = -1 AND FileCount& = 1 THEN
        Word$ = FileMDate$                   'File Modification Date
        InputLine 35, 12, 0, 10, 10
        FileMDate$ = Word$

        ELSEIF xMouse% > 270 AND xMouse% < 360 AND yMouse% > 192 AND yMouse% < 208 AND DriveInfoMode% = 0 AND FileOpened% = -1 AND FileCount& = 1 THEN
        Word$ = FileMTime$                   'File Modification Time
        InputLine 35, 13, 0, 10, 10
        FileMTime$ = Word$

        ELSEIF xMouse% > 270 AND xMouse% < 360 AND yMouse% > 224 AND yMouse% < 240 AND DriveInfoMode% = 0 AND FileOpened% = -1 AND FileCount& = 1 THEN
        Word$ = FileADate$                   'File Access Date
        InputLine 35, 15, 0, 10, 10
        FileADate$ = Word$
        END IF

      MouseShow
      END IF


    CASE 0
    ButDown% = 0

      IF ButPrs% > 0 THEN
      MouseHide

        IF ButPrs% = 1 THEN                      'Apply is released

'**********************
'Apply attributes.
'**********************


          IF FileCount& = 1 THEN
            IF FileOpened% THEN
              IF FileOpen%(FileArray$(1), Handle%) THEN

              Days% = InStr2Num%(FileMDate$, "/", -1)
              Months% = InStr2Num%(FileMDate$, "/", 0)
              Years% = InStr2Num%(FileMDate$, "/", 0)
              Hours% = InStr2Num%(FileMTime$, ":", -1)
              Minutes% = InStr2Num%(FileMTime$, ":", 0)
              Seconds% = InStr2Num%(FileMTime$, ":", 0)
              Temp% = FileMDateTimeSet%(Handle%, Years%, Months%, Days%, Hours%, Minutes%, Seconds%)

              Days% = InStr2Num%(FileADate$, "/", -1)
              Months% = InStr2Num%(FileADate$, "/", 0)
              Years% = InStr2Num%(FileADate$, "/", 0)
              Temp% = FileADateTimeSet%(Handle%, Years%, Months%, Days%)
              Temp% = FileClose%(Handle%)

              END IF
            END IF
          END IF



          FOR Counter& = 1 TO FileCount&
          Filename$ = FileArray$(Counter&)
            IF NOT (FileAttrSet%(Filename$, Attributes%)) THEN
            EXIT FOR
            END IF
          NEXT


        EXIT DO


'************************
'Other buttons.
'************************

        ELSEIF ButPrs% = 2 THEN                  'Cancel is released
        EXIT DO

        ELSEIF ButPrs% = 3 THEN                  'Exit is released
        EXIT DO
        END IF

      ButPrs% = 0
      MouseShow
      END IF

    END SELECT

  LOOP



CLS


'*************************************
'REMOVE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
'CHDRIVE "c:\program\projects\windos"
'CHDIR "c:\program\projects\windos"
'**************************************

'Function returns:   -1 if successful
'                     0 if not successful
'
'FUNCTION FileAttrGet% (File$)
'
'File0$ = File$ + CHR$(0)
'
'Regs.ax = &H4300
'Regs.ds = SSEG(File0$)
'Regs.dx = SADD(File0$)
'CALL INTERRUPTX(&H21, Regs, Regs)
'
'  IF (Regs.Flags AND 1) = 0 THEN
'  FileAttrGet% = -1
'  END IF
'
'END FUNCTION

FUNCTION AddSlash$ (Strng$)

  IF RIGHT$(Strng$, 1) <> "\" THEN
    AddSlash$ = Strng$ + "\"
  ELSE
    AddSlash$ = Strng$
  END IF

END FUNCTION

'The function assigns an unsigned integer (Number%) to a long integer, which
'it returns.
'
FUNCTION Assign2Lng& (Number%)

NextBit& = 1


  FOR BitNum% = 0 TO 15

    IF Number% AND NextBit& THEN
    LongNumber& = LongNumber& OR NextBit&
    END IF

  NextBit& = NextBit& * 2
  NEXT


Assign2Lng& = LongNumber&

END FUNCTION

FUNCTION BiosDateGet$

DEF SEG = &HF000

  FOR Offset& = &HFFF5 TO &HFFFC
  BiosDate$ = BiosDate$ + CHR$(PEEK(Offset&))
  NEXT

DEF SEG

BiosDateGet$ = BiosDate$

END FUNCTION

FUNCTION CalcDirSize% (Directory$)

STATIC StopKey$
STATIC TempMask$
TempMask$ = "*.*"


CalcDirSizeStart:

  IF StopKey$ = CHR$(27) THEN
    CalcDirSize% = 0
    EXIT FUNCTION
  ELSE
    CalcDirSize% = 1
    StopKey$ = INKEY$
  END IF

CHDIR Directory$

  IF ReadDir(TempMask$, FindFirst, 55) THEN

    FOR Counter% = 1 TO FileDirCount%
      IF ReadDir(TempMask$, FindNext, 55) = 0 THEN EXIT FUNCTION
    NEXT

    DO
    File$ = UCASE$(RTRIM$(DTA.Filename))
    FileDirCount% = FileDirCount% + 1

      IF ASC(DTA.Attributes) AND 16 THEN               'This is a directory.
          IF LEFT$(File$, 1) <> "." THEN
          'PRINT AddSlash$(Directory$) + File$
          Temp% = CalcDirSize(AddSlash(Directory$) + File$)
          GOTO CalcDirSizeStart
          END IF
      ELSE                                             'This is a file.
        SizeOfAllFiles@ = SizeOfAllFiles@ + DTA.FileSize
        NumOfAllFiles@ = NumOfAllFiles@ + 1
      END IF

      IF ReadDir(TempMask$, FindNext, 55) = 0 THEN
      EXIT FUNCTION
      END IF

    LOOP

  END IF


END FUNCTION

FUNCTION ConventionalSize%

CALL INTERRUPT(&H12, Regs, Regs)
ConventionalSize% = Regs.ax

END FUNCTION

SUB DiskInfoGet (Drive$, Serial$, Volume$)

Disk.Info = 0

Regs.ax = &H440D

  IF LEN(Drive$) = 0 THEN
    Regs.bx = 0
  ELSE
    Regs.bx = ASC(UCASE$(Drive$)) - 64
  END IF

Regs.cx = &H866
Regs.ds = VARSEG(Disk)
Regs.dx = VARPTR(Disk)

CALL INTERRUPT(&H21, Regs, Regs)

Serial$ = "        "
LSET Serial$ = HEX$(Disk.SerialNumber)
Volume$ = "           "
LSET Volume$ = Disk.Label

END SUB

SUB DiskSpaceGet (Drive$)

'***************************************************
'Try extended interrupt for DOS with FAT32 support.
'***************************************************

Drive0$ = Drive$ + CHR$(0)

Regs.ax = &H7303
Regs.ds = SSEG(Drive0$)
Regs.dx = SADD(Drive0$)
Regs.es = VARSEG(DiskProperty)
Regs.di = VARPTR(DiskProperty)
Regs.cx = 44

ON LOCAL ERROR GOTO DiskGetSpaceError
CALL Interruptx(&H21, Regs, Regs)
ON LOCAL ERROR GOTO 0


'****************************************
'If unavailable, use standard interrupt.
'****************************************

  IF (Regs.ax AND &HFF) = 0 THEN
    'IF (Regs.flags AND 1) = 0 THEN

    Regs.ax = &H3600
    Regs.dx = ASC(UCASE$(Drive$)) - 64

    ON LOCAL ERROR GOTO DiskGetSpaceError
    CALL Interruptx(&H21, Regs, Regs)
    ON LOCAL ERROR GOTO 0

    DiskProperty.SectorsPerCluster = Assign2Lng&(Regs.ax)
    DiskProperty.FreeClusters = Assign2Lng&(Regs.bx)
    DiskProperty.BytesPerSector = Assign2Lng&(Regs.cx)
    DiskProperty.TotalClusters = Assign2Lng&(Regs.dx)

    DiskProperty.TotalSectors = DiskProperty.TotalClusters * DiskProperty.SectorsPerCluster
    DiskProperty.FreeSectors = DiskProperty.FreeClusters * DiskProperty.SectorsPerCluster

    'END IF
  END IF


'********************
'Compute disk space.
'********************

FreeDiskSpace@ = DiskProperty.BytesPerSector * DiskProperty.SectorsPerCluster
TotalDiskSpace@ = FreeDiskSpace@ * DiskProperty.TotalClusters
FreeDiskSpace@ = FreeDiskSpace@ * DiskProperty.FreeClusters
UsedDiskSpace@ = TotalDiskSpace@ - FreeDiskSpace@

AfterDiskGetSpaceError:
EXIT SUB

'============================================================================
DiskGetSpaceError:
RESUME AfterDiskGetSpaceError

END SUB

FUNCTION DOSinHMA%

Regs.ax = &H3306
CALL INTERRUPT(&H21, Regs, Regs)

  IF Regs.dx AND &H1000 THEN
  DOSinHMA% = -1
  END IF

END FUNCTION

FUNCTION DosVerGet$

Regs.ax = &H3306
CALL INTERRUPT(&H21, Regs, Regs)

g$ = LTRIM$(STR$(Regs.bx AND &HFF))
d$ = LTRIM$(STR$(Regs.bx \ 256))
r$ = LTRIM$(STR$(Regs.dx AND 7))

DosVerGet$ = g$ + "." + d$ + "." + r$

END FUNCTION

SUB EmptyKeyb

DEF SEG = 0
POKE &H41A, PEEK(&H41C)
DEF SEG

END SUB

SUB File2Video (Filename$)

Buffer$ = SPACE$(19200)
BufSeg% = SSEG(Buffer$)
BufOff% = SADD(Buffer$)


File% = FREEFILE
OPEN Filename$ FOR BINARY AS #File%

SEEK #File%, 8      'Skip the BSAVE header.


GET #File%, , Buffer$

  FOR ByteNum% = 0 TO 19200 - 1
  DEF SEG = BufSeg%
  Byte% = PEEK(BufOff% + ByteNum%)
  DEF SEG = &HA000
  POKE ByteNum%, Byte%
  NEXT

GET #File%, , Buffer$

  FOR ByteNum% = 0 TO 19200 - 1
  DEF SEG = BufSeg%
  Byte% = PEEK(BufOff% + ByteNum%)
  DEF SEG = &HA4B0
  POKE ByteNum%, Byte%
  NEXT


CLOSE #File%
DEF SEG

END SUB

'Function returns:   -1 if successful
'                     0 if not successful
'
FUNCTION FileADateTimeGet% (BYVAL Handle%, Years%, Months%, Days%)

Regs.ax = &H5704
Regs.bx = Handle%

CALL Interruptx(&H21, Regs, Regs)

  IF Regs.flags AND 1 THEN
    EXIT FUNCTION
  ELSE
    FileADateTimeGet% = -1
  END IF

Years% = ((Regs.dx AND &HFE00&) \ 512) + 1980
Months% = (Regs.dx AND &H1E0&) \ 32
Days% = Regs.dx AND &H1F&

END FUNCTION

'Function returns:   -1 if successful
'                     0 if not successful
'
FUNCTION FileADateTimeSet% (BYVAL Handle%, BYVAL Years%, BYVAL Months%, BYVAL Days%)

FileADateTimeSet% = -1
Regs.ax = &H5705
Regs.bx = Handle%
Regs.cx = 0
Regs.dx = (Years% - 1980) * 512 + Months% * 32 + Days%

ON LOCAL ERROR GOTO FileADateTimeSetError
CALL Interruptx(&H21, Regs, Regs)
ON LOCAL ERROR GOTO 0

  IF Regs.flags AND 1 THEN
  FileADateTimeSet% = 0
  END IF

AfterFileADateTimeSetError:
EXIT FUNCTION

'============================================================================
FileADateTimeSetError:
FileADateTimeSet% = 0
RESUME AfterFileADateTimeSetError

END FUNCTION

'Function returns:   -1 if successful
'                     0 if not successful
'
FUNCTION FileAttrSet% (File$, BYVAL Attr%)

FileAttrSet% = -1
File0$ = File$ + CHR$(0)

Attr% = Attr% AND &HFFEF            'Clear directory bit.


Regs.ax = &H4301
Regs.cx = Attr%
Regs.ds = SSEG(File0$)
Regs.dx = SADD(File0$)

ON LOCAL ERROR GOTO FileAttrSetError
CALL Interruptx(&H21, Regs, Regs)
ON LOCAL ERROR GOTO 0

  IF Regs.flags AND 1 THEN
  FileAttrSet% = 0
  END IF

AfterFileAttrSetError:
EXIT FUNCTION

'============================================================================
FileAttrSetError:
FileAttrSet% = 0
RESUME AfterFileAttrSetError

END FUNCTION

'Function returns:   -1 if successful
'                     0 if not successful
'
FUNCTION FileClose% (BYVAL Handle%)

FileClose% = -1
Regs.ax = &H3E00
Regs.bx = Handle%

ON LOCAL ERROR GOTO FileCloseError
CALL Interruptx(&H21, Regs, Regs)
ON LOCAL ERROR GOTO 0

  IF Regs.flags AND 1 THEN
  FileClose% = 0
  END IF

AfterFileCloseError:
EXIT FUNCTION

'============================================================================
FileCloseError:
FileClose% = 0
RESUME AfterFileCloseError

END FUNCTION

'Function returns:   -1 if successful
'                     0 if not successful
'
FUNCTION FileMDateTimeGet% (BYVAL Handle%, Years%, Months%, Days%, Hours%, Minutes%, Seconds%)

Regs.ax = &H5700
Regs.bx = Handle%

CALL Interruptx(&H21, Regs, Regs)

  IF Regs.flags AND 1 THEN
    EXIT FUNCTION
  ELSE
    FileMDateTimeGet% = -1
  END IF

Years% = ((Regs.dx AND &HFE00&) \ 512) + 1980
Months% = (Regs.dx AND &H1E0&) \ 32
Days% = Regs.dx AND &H1F&
Hours% = (Regs.cx AND &HF800&) \ 2048
Minutes% = (Regs.cx AND &H7E0&) \ 32
Seconds% = (Regs.cx AND &H1F&) * 2

END FUNCTION

'Function returns:   -1 if successful
'                     0 if not successful
'
FUNCTION FileMDateTimeSet% (BYVAL Handle%, BYVAL Years%, BYVAL Months%, BYVAL Days%, BYVAL Hours%, BYVAL Minutes%, BYVAL Seconds%)

FileMDateTimeSet% = -1
Regs.ax = &H5701
Regs.bx = Handle%
Regs.cx = Hours% * 2048 + Minutes% * 32 + Seconds% \ 2
Regs.dx = (Years% - 1980) * 512 + Months% * 32 + Days%

ON LOCAL ERROR GOTO FileMDateTimeSetError
CALL Interruptx(&H21, Regs, Regs)
ON LOCAL ERROR GOTO 0

  IF Regs.flags AND 1 THEN
  FileMDateTimeSet% = 0
  END IF

AfterFileMDateTimeSetError:
EXIT FUNCTION

'============================================================================
FileMDateTimeSetError:
FileMDateTimeSet% = 0
RESUME AfterFileMDateTimeSetError

END FUNCTION

'Function returns:   -1 if successful, file handle in Handle%
'                     0 if not successful, error code in Handle%
'
FUNCTION FileOpen% (File$, Handle%)

'INT 21 - DOS 2+ - "OPEN" - OPEN EXISTING FILE
'        AH = 3Dh
'        AL = access and sharing modes (see #01402)
'        DS:DX -> ASCIZ filename
'        CL = attribute mask of files to look for (server call only)
'Return: CF clear if successful
'            AX = file handle
'        CF set on error
'            AX = error code (01h,02h,03h,04h,05h,0Ch,56h) (see #01680 at AH=59h)
'               0 = OK
'               2 = File not found
'               3 = Path not found
'               4 = No more handles available
'               5 = Access denied
'               0Ch = Invalid access code
'
'Notes:  file pointer is set to start of file
'        if SHARE or a network is loaded, the file open may fail if the file
'          is already open, depending on the combination of sharing modes
'          (see #01403,#01404)
'        file handles which are inherited from a parent also inherit sharing
'          and access restrictions
'        files may be opened even if given the hidden or system attributes
'        under the FlashTek X-32 DOS extender, the pointer is in DS:EDX
'        DR DOS checks the system password or explicitly supplied password at
'          the end of the filename (following a semicolon) against the reserved
'          field in the directory entry before allowing access
'        sharing modes are only effective on local drives if SHARE is loaded
'
'Bitfields for access and sharing modes:
'Bit(s)  Description     (Table 01402)
' 2-0    access mode
'        000 READ only
'        001 WRITE only
'        010 read/write
'        011 (DOS 5+ internal) passed to redirector on EXEC to allow
'                                            case-sensitive filenames
' 3      reserved (0)
' 6-4    sharing mode (DOS 3.0+) (see #01403)
'        000 compatibility mode
'        001 "DENYALL" prohibit both read and write access by others
'        010 "DENYWRITE" prohibit write access by others
'        011 "DENYREAD" prohibit read access by others
'        100 "DENYNONE" allow full access by others
'        111 network FCB (only available during server call)
' 7      inheritance
'        if set, file is private to current process and will not be inherited
'         by child processes


FileOpen% = -1
File0$ = File$ + CHR$(0)

Regs.ax = &H3D00
Regs.cx = 0
Regs.ds = SSEG(File0$)
Regs.dx = SADD(File0$)

ON LOCAL ERROR GOTO FileOpenError
CALL Interruptx(&H21, Regs, Regs)
ON LOCAL ERROR GOTO 0

Handle% = Regs.ax

  IF Regs.flags AND 1 THEN
  FileOpen% = 0
  END IF

AfterFileOpenError:
EXIT FUNCTION


'============================================================================
FileOpenError:
FileOpen% = 0
RESUME AfterFileOpenError

END FUNCTION

SUB InputLine (BYVAL LocateX%, BYVAL LocateY%, BYVAL Init%, BYVAL Limit%, BYVAL StartScroll%)

  IF Init% THEN
  GOSUB RemoveCursor
  EXIT SUB
  END IF

EmptyKeyb

COLOR 15
GOSUB PutCursor

'----------------------------------------------------------------------------
  DO

  KH$ = INKEY$
  KeyLength% = LEN(KH$)

    IF KeyLength% THEN
    Ascii% = ASC(RIGHT$(KH$, 1))

      IF Ascii% <> 34 AND Ascii% <> 13 AND Ascii% <> 9 AND KeyLength% = 1 THEN

        IF Ascii% = 27 THEN
        Word$ = ""

        ELSEIF Ascii% = 8 THEN
          IF LenWord% THEN
          Word$ = LEFT$(Word$, LEN(Word$) - 1)
          END IF

        ELSEIF LenWord% < Limit% THEN
        Word$ = Word$ + KH$
        END IF

      GOSUB PutCursor
      END IF

    END IF

  LOOP UNTIL KH$ = CHR$(13)

GOSUB RemoveCursor
EXIT SUB

'============================================================================

UpdateCursor:
LenWord% = LEN(Word$)
LOCATE LocateY%, LocateX%
  IF LenWord% < StartScroll% THEN
    PRINT Word$ + Cursor$ + SPACE$(StartScroll% - LenWord%)
  ELSE
    PRINT RIGHT$(Word$, StartScroll%) + Cursor$
  END IF
RETURN

RemoveCursor:
Cursor$ = " "
GOSUB UpdateCursor
RETURN

PutCursor:
Cursor$ = "_"
GOSUB UpdateCursor
RETURN

END SUB

FUNCTION InStr2Num% (Text$, Separator$, BYVAL ResetFunc%)

STATIC Position%

  IF ResetFunc% THEN
  Position% = 0
  END IF

Position% = Position% + 1
OldPosition% = Position%
Position% = INSTR(Position%, Text$, Separator$)

  IF Position% THEN
    Temp$ = MID$(Text$, OldPosition%, Position% - OldPosition%)
  ELSE
    Temp$ = MID$(Text$, OldPosition%)
  END IF

InStr2Num% = VAL(Temp$)

END FUNCTION

SUB Interface

SCREEN 12
CLS

'********************************
'Set black palette.
'********************************

  FOR i% = 0 TO 15
  SetPal i%, 0, 0, 0
  NEXT


'********************************
'Load the interface.
'********************************

OUT &H3C4, 2
OUT &H3C5, 1
File2Video GraphicsPathSlash$ + "property.im1"

OUT &H3C4, 2
OUT &H3C5, 2
File2Video GraphicsPathSlash$ + "property.im2"

OUT &H3C4, 2
OUT &H3C5, 4
File2Video GraphicsPathSlash$ + "property.im3"

OUT &H3C4, 2
OUT &H3C5, 8
File2Video GraphicsPathSlash$ + "property.im4"


'********************************
'Initialize the palette.
'********************************

Red$ = " "
Green$ = Red$
Blue$ = Red$

File% = FREEFILE
OPEN AddSlash$(PalPath$) + PalFile$ FOR BINARY AS #File%

SEEK #File%, 9&
GET #File%, , TextLen%
SEEK #File%, 9 + 2 + TextLen%

  FOR Colour% = 0 TO 15
  GET #File%, , Red$
  GET #File%, , Green$
  GET #File%, , Blue$
  SetPal Colour%, ASC(Red$), ASC(Green$), ASC(Blue$)
  NEXT

CLOSE #File%


'*********************************
'Display drive/system information?
'*********************************

  IF DriveInfoMode% THEN

    TinyButton 270, 420, 370, 460, 4, 15, 0
    LoadFont "Exit", 310, 435, 4, 1, 13

      FOR Counter% = 0 TO 4
      LINE (48 + Counter%, 98 + Counter%)-(102 - Counter%, 402 - Counter%), Counter% * 2 + 5, B
      NEXT

    LoadFont "0 Kb", 7, 400, 4, 1, 14

    MemSizeKb% = ConventionalSize%
    MemSizeB& = MemSizeKb% * 1024&
    FreeMemB& = FRE(-1) + ProgramSize
    FreeMemKb% = RoundOff&(FreeMemB& / 1024)
    UsedMemB& = MemSizeB& - FreeMemB&
    UsedMemKb% = RoundOff&(UsedMemB& / 1024)
    UsedMemKb$ = LTRIM$(STR$(UsedMemKb%))

    LoadFont "Conventional Memory", 7, 465, 4, 1, 14
    LoadFont LTRIM$(STR$(MemSizeKb%)) + " Kb", 7, 90, 4, 1, 14
    ypos% = 398 - (UsedMemB& / MemSizeB&) * 296
    LINE (52, 398)-(98, ypos%), 13, BF
    LoadFont UsedMemKb$ + " Kb", 7, ypos%, 4, 1, 14
    LoadFont "Used:        Kb", 7, 435, 4, 1, 14
    LOCATE 28, 7
    PRINT "   ";
    LOCATE 28, 7
    PRINT UsedMemKb$;
    LOCATE 29, 7
    PRINT "   ";
    LOCATE 29, 7
    LoadFont "Free:         Kb", 7, 450, 4, 1, 14
    PRINT LTRIM$(STR$(FreeMemKb%));

    DiskInfoGet CommandLine$, Serial$, Volume$
    LoadFont "Volume Label:", 130, 98, 4, 1, 14
    LOCATE 7, 30
    PRINT Volume$
    LoadFont "Serial Number:", 130, 114, 4, 1, 14
    LOCATE 8, 30
    PRINT Serial$

    DiskSpaceGet CommandLine$ + ":\"

    LoadFont "Total Space:", 127, 146, 4, 1, 14
    LoadFont "bytes", 400, 146, 4, 1, 14
    LOCATE 10, 30
    PRINT USING CurrencyPattern$; TotalDiskSpace@

    LoadFont "Used Space:", 127, 162, 4, 1, 14
    LoadFont "bytes", 400, 162, 4, 1, 14
    LOCATE 11, 30
    PRINT USING CurrencyPattern$; UsedDiskSpace@

    LoadFont "Free Space:", 127, 178, 4, 1, 14
    LoadFont "bytes", 400, 178, 4, 1, 14
    LOCATE 12, 30
    PRINT USING CurrencyPattern$; FreeDiskSpace@

    LoadFont "Sectors Per Cluster:", 127, 210, 4, 1, 14
    LOCATE 14, 36
    PRINT USING LongPattern$; DiskProperty.SectorsPerCluster

    LoadFont "Bytes Per Sector:", 127, 226, 4, 1, 14
    LOCATE 15, 36
    PRINT USING LongPattern$; DiskProperty.BytesPerSector

    BytesPerCluster@ = DiskProperty.BytesPerSector * DiskProperty.SectorsPerCluster
    LoadFont "Bytes Per Cluster:", 127, 242, 4, 1, 14
    LOCATE 16, 30
    PRINT USING CurrencyPattern$; BytesPerCluster@

    LoadFont "Total Sectors:", 127, 278, 4, 1, 14
    LOCATE 18, 36
    PRINT USING LongPattern$; DiskProperty.TotalSectors

    LoadFont "Free Sectors:", 127, 294, 4, 1, 14
    LOCATE 19, 36
    PRINT USING LongPattern$; DiskProperty.FreeSectors

    LINE (177, 360)-(207, 380), 12, BF
    LoadFont "-- Used Space", 212, 365, 4, 1, 14
    LINE (177, 380)-(207, 400), 1, BF
    LoadFont "-- Free Space", 212, 385, 4, 1, 14

    LoadFont "BIOS Date:", 470, 98, 4, 1, 14
    LOCATE 7, 70
    PRINT BiosDateGet$;

    LoadFont "DOS Version:", 470, 130, 4, 1, 14
    LOCATE 9, 70
    PRINT DosVerGet$;

    LoadFont "DOS in HMA:", 470, 146, 4, 1, 14
      IF DOSinHMA% THEN
        Temp% = -1
      ELSE
        Temp% = 0
      END IF
    TickButton 570, 151, Temp%, 7

    LoadFont "Verify Flag:", 470, 162, 4, 1, 14
      IF VerifyGet% THEN
        Temp% = -1
      ELSE
        Temp% = 0
      END IF
    TickButton 570, 167, Temp%, 7


    CIRCLE (500, 360), 100, 13

      IF FreeDiskSpace@ > 0 THEN
        Radians! = UsedDiskSpace@ / TotalDiskSpace@ * 2 * 3.14
        LINE (500, 360)-(600, 360), 13
        LINE (500, 360)-(COS(Radians!) * 100 + 500, SIN(Radians!) * 100 + 360), 13
        Temp! = Radians! / 2
        PAINT (COS(Temp!) * 95 + 500, SIN(Temp!) * 95 + 360), 12, 13
        Temp! = (Radians! + 2 * 3.14) / 2
        PAINT (COS(Temp!) * 95 + 500, SIN(Temp!) * 95 + 360), 0, 13
      ELSE
        PAINT (500, 360), 12, 13
      END IF


'============================================================================

'*********************************
'Display attribute information.
'*********************************

  ELSE

      LoadFont "Filename / Directory :", 50, 96, 4, 1, 14
      LOCATE 8, 7
      PRINT SPACE$(66);
      LOCATE , 7

        IF FileCount& > 1 THEN
          PRINT "Multiple"
        ELSE
          PRINT FileArray$(1)
        END IF


'********************************
'Calculate file/directory sizes.
'********************************

      SizeOfAllFiles@ = 0
      NumOfAllFiles@ = 0

      LoadFont "File / Directory Sizes:", 50, 350, 4, 1, 14
      LoadFont "bytes in                                   file(s).", 210, 370, 4, 1, 14

      LOCATE 24, 7
      PRINT "<ESC> to cancel... ";
      LOCATE 24, 35
      PRINT SPACE$(19);


        FOR Counter& = 1 TO FileCount&
        Filename$ = FileArray$(Counter&)
          IF ReadDir(Filename$, FindFirst, 55) THEN
          NewAttribute% = ASC(DTA.Attributes)
          Attributes% = Attributes% OR NewAttribute%

            IF NewAttribute% AND 16 THEN
              CHDRIVE Filename$
                IF CalcDirSize%(Filename$) = 0 THEN
                LoadFont ">", 30, 370, 4, 1, 14
                EXIT FOR
                END IF
            ELSE
              SizeOfAllFiles@ = SizeOfAllFiles@ + DTA.FileSize
              NumOfAllFiles@ = NumOfAllFiles@ + 1
            END IF

          END IF
        NEXT


      LOCATE 24, 7
      PRINT USING CurrencyPattern$; SizeOfAllFiles@
      LOCATE 24, 35
      PRINT USING CurrencyPattern$; NumOfAllFiles@



'********************************
'Draw the ticks.
'********************************

      LoadFont "Attributes :", 50, 146, 4, 1, 14
      UpdateTicks

      TickButton 100, 315, Attributes% AND 16, 10
      LoadFont "Sub-Directory", 120, 311, 4, 1, 14


'********************************
'Calculate date/time attributes.
'********************************

      FileOpened% = 0
      FileMDate$ = "          "
      FileMTime$ = FileMDate$
      FileADate$ = FileMDate$



        IF FileCount& = 1 THEN
          FileOpened% = FileOpen%(Filename$, Handle%)


            IF FileOpened% THEN

              IF FileMDateTimeGet%(Handle%, Years%, Months%, Days%, Hours%, Minutes%, Seconds%) THEN
              RSET FileMDate$ = LTRIM$(STR$(Days%)) + "/" + LTRIM$(STR$(Months%)) + "/" + LTRIM$(STR$(Years%))
              RSET FileMTime$ = LTRIM$(STR$(Hours%)) + ":" + LTRIM$(STR$(Minutes%)) + ":" + LTRIM$(STR$(Seconds%))
              END IF

              IF FileADateTimeGet%(Handle%, Years%, Months%, Days%) THEN
              RSET FileADate$ = LTRIM$(STR$(Days%)) + "/" + LTRIM$(STR$(Months%)) + "/" + LTRIM$(STR$(Years%))
              Temp% = FileADateTimeSet%(Handle%, Years%, Months%, Days%)
              END IF

            Temp% = FileClose%(Handle%)
            END IF

        END IF


'***********************************
'Display time/date attributes.
'***********************************

      LOCATE 12, 35
      PRINT FileMDate$; " ";
      LoadFont "File Modification Date (dd/mm/yyyy)", 370, 179, 4, 1, 14

      LOCATE 13, 35
      PRINT FileMTime$; " ";
      LoadFont "File Modification Time (hh:mm:ss)", 370, 195, 4, 1, 14

      LOCATE 15, 35
      PRINT FileADate$; " ";
      LoadFont "File Access Date (dd/mm/yyyy)", 370, 227, 4, 1, 14


'***********************************
'Display the buttons.
'***********************************

      TinyButton 330, 420, 450, 460, 4, 15, 0
      LoadFont "Cancel", 370, 435, 4, 1, 13
      TinyButton 190, 420, 310, 460, 4, 15, 0
      LoadFont "Apply", 235, 435, 4, 1, 13


  END IF

MousePut 320, 240

END SUB

SUB LoadFont (Text$, BYVAL Textx%, BYVAL Texty%, BYVAL Size%, BYVAL Bold%, BYVAL Colour%)

DRAW "S" + STR$(Size%)

RealBold% = Bold%

  WHILE Bold% > -1
  DRAW "BM" + STR$(Textx%) + "," + STR$(Texty%) + " C" + STR$(Colour%)
    FOR k% = 1 TO LEN(Text$)
    char$ = MID$(Text$, k%, 1)
    DRAW WindosFont$(ASC(char$))
      IF RealBold% > 0 THEN DRAW "BR1"
    NEXT
  Bold% = Bold% - 1
  Textx% = Textx% + 1
  WEND

END SUB


FUNCTION ReadDir% (DirSpec$, BYVAL SearchType%, BYVAL Filter%)

  IF SearchType% <> FindFirst AND SearchType% <> FindNext THEN
  EXIT FUNCTION
  END IF


'*********************************
'Setting up DTA...
'*********************************

  IF SearchType% = FindFirst THEN
  Regs.ax = &H1A00
  Regs.ds = VARSEG(DTA)
  Regs.dx = VARPTR(DTA)
  CALL Interruptx(&H21, Regs, Regs)
  END IF


'*********************************
'Finding FIRST or NEXT entries...
'*********************************
ChrZero$ = CHR$(0)

  IF RIGHT$(DirSpec$, 1) = ChrZero$ THEN
    Mask$ = DirSpec$
  ELSE
    Mask$ = DirSpec$ + ChrZero$
  END IF

Regs.ax = SearchType%
Regs.cx = Filter%
Regs.ds = SSEG(Mask$)
Regs.dx = SADD(Mask$)

ON LOCAL ERROR GOTO ReadDirError
CALL Interruptx(&H21, Regs, Regs)
ON LOCAL ERROR GOTO 0


'*********************************
'Check for success.
'*********************************

  IF Regs.flags AND 1 THEN
    ReadDir% = 0
  ELSE
    NullByte% = INSTR(DTA.Filename, CHR$(0))
      IF NullByte% THEN
      DTA.Filename = LEFT$(DTA.Filename, NullByte% - 1)
      END IF
    ReadDir% = -1
  END IF

ReadDirEnd:
EXIT FUNCTION

'============================================================================
ReadDirError:
ReadDir% = 0
RESUME ReadDirEnd

END FUNCTION

SUB ReadINI

File% = FREEFILE
OPEN MainINI FOR INPUT AS #File%


  DO UNTIL EOF(File%)

  LINE INPUT #File%, Something$
  Something$ = UCASE$(LTRIM$(RTRIM$(Something$)))
  Equal% = INSTR(Something$, "=")

    IF Equal% THEN
    Temp$ = LTRIM$(MID$(Something$, Equal% + 1))
    Temp% = VAL(Temp$)
    Something$ = RTRIM$(LEFT$(Something$, Equal% - 1))
    END IF

'----------------------------------------------------------------------------

    IF Something$ = "TEMP" THEN
    TempPath$ = Temp$

    ELSEIF Something$ = "GRAPHICS" THEN
    GraphicsPath$ = Temp$
    GraphicsPathSlash$ = AddSlash$(GraphicsPath$)

    ELSEIF Something$ = "PALETTE" THEN
    PalPath$ = Temp$

    ELSEIF Something$ = "PROPERTYCOLOURSET" THEN
    PalFile$ = Temp$

    ELSEIF Something$ = "FONTS" THEN
    FontsPath$ = Temp$

    ELSEIF Something$ = "XMOUSE" THEN
    XmsSpd% = Temp%

    ELSEIF Something$ = "YMOUSE" THEN
    YmsSpd% = Temp%
    END IF
'----------------------------------------------------------------------------

  LOOP

CLOSE #File%

END SUB

FUNCTION RoundOff& (Number!)

Temp% = INT(Number!)

  IF Number! - Temp% >= .5 THEN
    RoundOff& = Temp% + 1
  ELSE
    RoundOff& = Temp%
  END IF

END FUNCTION

SUB SetPal (Attribute%, Red%, Green%, Blue%)

OUT &H3C8, Attribute%
OUT &H3C9, Red%
OUT &H3C9, Green%
OUT &H3C9, Blue%

END SUB

SUB TickButton (BYVAL x%, BYVAL y%, BYVAL ButtonType%, BYVAL Size%)

  IF ButtonType% THEN

      FOR Counter% = 13 TO 3 STEP -2
      LINE (x% - Size%, y% - Size%)-(x% + Size%, y% + Size%), Counter%, BF
      Size% = Size% - 1
      NEXT

    Tick$ = "f3e7"
    PSET (x% - 5, y%), 13
    DRAW Tick$
    PSET (x% - 5, y% + 1), 14
    DRAW Tick$

  ELSE

    FOR Counter% = 6 TO 13 STEP 2
    LINE (x% - Size%, y% - Size%)-(x% + Size%, y% + Size%), Counter%, BF
    Size% = Size% - 1
    NEXT

  END IF


END SUB

SUB TinyButton (BYVAL x%, BYVAL y%, BYVAL x2%, BYVAL y2%, BYVAL Colour%, BYVAL TopCol%, BYVAL BotCol%)

LINE (x%, y%)-(x2%, y2%), Colour%, BF      'Drawing main rectangle...

LINE (x%, y%)-(x2%, y%), TopCol%
LINE (x%, y%)-(x%, y2%), TopCol%

LINE (x2%, y%)-(x2%, y2%), BotCol%
LINE (x%, y2%)-(x2%, y2%), BotCol%

END SUB

SUB UpdateTicks

TickButton 100, 185, Attributes% AND 1, 10
LoadFont "Read-Only", 120, 181, 4, 1, 14

TickButton 100, 215, Attributes% AND 2, 10
LoadFont "Hidden", 120, 211, 4, 1, 14

TickButton 100, 245, Attributes% AND 4, 10
LoadFont "System", 120, 241, 4, 1, 14

TickButton 100, 275, Attributes% AND 32, 10
LoadFont "Archive", 120, 271, 4, 1, 14

END SUB

FUNCTION VerifyGet%

Regs.ax = &H5400
CALL INTERRUPT(&H21, Regs, Regs)

VerifyGet% = Regs.ax AND &HFF

END FUNCTION

