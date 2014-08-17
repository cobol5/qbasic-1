'Copyright (C) HiTech Labs, Denis Andrianov
'WinDos Setup
'============================================================================

DECLARE SUB Interface ()
DECLARE SUB Finish ()
DECLARE SUB Pal (BYVAL C%, BYVAL R%, BYVAL G%, BYVAL B%)
DECLARE SUB InfoBox ()
DECLARE SUB LoadFont (Text$, BYVAL TextX%, BYVAL TextY%, ColourX%, ColourY%, BYVAL StepColourX%, BYVAL StepColourY%)
DECLARE SUB MoveText (BYVAL Fast%, BYVAL LoopNum%, Text$)
DECLARE SUB InputLine (BYVAL LocateX%, BYVAL LocateY%, BYVAL Init%, BYVAL Limit%, BYVAL StartScroll%)

DECLARE FUNCTION CopyFile% (Source$, Destination$)

DECLARE FUNCTION RemoveSlash$ (DirString$)
DECLARE FUNCTION YN% ()
DECLARE SUB Delay (BYVAL Time%)


CONST DefDir = "C:\WINDOS"

CONST LogoPrgm = "HITECH.EXE"
CONST DesktopPrgm = "DESKTOP.BAT"
CONST DisktopPrgm = "DISKTOP.BAT"
CONST ConfigPrgm = "OPTIONS.EXE"
CONST WindosINI = "WINDOS.INI"
CONST HistoryINI = "HISTORY.INI"
CONST ExtensionsINI = "EXT.INI"
CONST QuickINI = "QUICK.INI"

CONST FrameX1% = 50
CONST FrameY1% = 70
CONST FrameX2% = 270
CONST FrameY2% = 112

DIM SHARED Word$
DIM SHARED Scroll(1595) AS INTEGER
DIM SHARED ScreenBuff(4753) AS INTEGER

'============================================================================
'Defaults...

SourceDir$ = CURDIR$

PkZipParam$ = " -o -d >nul"

WinDosDAT$ = "MAIN.DAT"
WinDosEXE$ = "MAIN.EXE"


'============================================================================
SHELL LogoPrgm
Interface

'SLEEP 1
'MoveText 0, 123, "HiTech Labs proudly present `WinDos 265` (by Denis Andrianov). All rights reserved.  "

MoveText 1, 38, "Install WinDos (Y/N)?"
  IF NOT (YN%) THEN Finish

MoveText 1, 38, "Install to default directory (Y/N)?"
InfoBox
LoadFont "WinDos Directory:", 92, 76, 0, 95, 0, 20

Word$ = DefDir
InputLine 12, 12, 1, 63, 17
Result% = YN%

'----------------------------------------------------------------------------

  DO

    IF Result% THEN
      Result% = 0
    ELSE
      MoveText 1, 38, "Input destination directory."
      Word$ = ""
      InputLine 12, 12, 0, 63, 17
    END IF


  Word$ = LTRIM$(RTRIM$(Word$))
    IF RIGHT$(Word$, 1) <> "\" THEN
    Word$ = Word$ + "\"
    END IF


    IF LEN(Word$) > 3 AND MID$(Word$, 2, 2) = ":\" THEN
      ON ERROR GOTO DriveErrors
      CHDRIVE Word$
      ON ERROR GOTO 0

        IF DriveChangeError% = 0 THEN
          NextBackSlash% = 3

            DO
            BackSlash% = NextBackSlash%
            NextBackSlash% = INSTR(BackSlash% + 1, Word$, "\")

              IF NextBackSlash% - BackSlash% = 1 THEN
              MoveText 1, 38, "Idiotic path!"
              SLEEP 3
              EXIT DO
              END IF

            DirectoryExists% = 0
            DirName$ = LEFT$(Word$, NextBackSlash% - 1)
            ON ERROR GOTO DirErrors
            MKDIR DirName$
            ON ERROR GOTO 0

              IF DirCreateError% = 1 THEN
              DirCreateError% = 0
              EXIT DO
              END IF

              IF NextBackSlash% = LEN(Word$) THEN
              InstallDir$ = Word$
              GOTO BeginToCopy
              END IF

            LOOP

        ELSE
          DriveChangeError% = 0
        END IF


    ELSEIF LEN(Word$) = 3 AND RIGHT$(Word$, 2) = ":\" THEN
      MoveText 1, 38, "Can't install to \ dir!"
      SLEEP 3
    ELSE
      MoveText 1, 38, "Must be full path!"
      SLEEP 3
    END IF


  LOOP

'----------------------------------------------------------------------------

BeginToCopy:

InstallDir$ = UCASE$(InstallDir$)

ON ERROR GOTO NoSource
CHDIR SourceDir$
CHDRIVE SourceDir$
ON ERROR GOTO 0

  IF DirectoryExists% THEN
  MoveText 1, 38, "Directory exists. Overwrite (Y/N)?"
  InfoBox
  LoadFont "If overwriting old WinDos", 60, 78, 0, 95, 0, 20
  LoadFont "directory, all config", 60, 86, 0, 95, 0, 20
  LoadFont "files will be lost !!!", 60, 94, 0, 95, 0, 20
    IF NOT (YN%) THEN Finish
  END IF

PUT (FrameX1%, FrameY1%), ScreenBuff, PSET
MoveText 1, 38, "Installing WinDos . . ."
Success% = CopyFile%(WinDosDAT$, InstallDir$ + WinDosEXE$)

  IF NOT Success% THEN
  MoveText 1, 38, "Error while copying. Setup aborted!"
  SLEEP
  Finish
  END IF



ON ERROR GOTO ErrInst

CHDRIVE InstallDir$
CHDIR RemoveSlash$(InstallDir$)

SHELL WinDosEXE$ + PkZipParam$
KILL WinDosEXE$

  IF LEN(ENVIRON$("TEMP")) = 0 THEN
  MKDIR "TEMP"
  END IF
MKDIR "ARCHIVES"
MKDIR "KEYS"
MKDIR "PROGRAMS"
MKDIR "DESKTOP"
MKDIR "GRAPHICS\ICONS"

ON ERROR GOTO 0



InstallDir$ = RemoveSlash$(InstallDir$)
ON ERROR GOTO ErrInst

OPEN WindosINI FOR OUTPUT AS #1
PRINT #1, "[Custom]"
PRINT #1, "AnyKey = 0"
PRINT #1, "UseCommandCom = -1"
PRINT #1, "FlushBuffers = -1"
PRINT #1,
PRINT #1, "[Common Interface]"
PRINT #1, "xMouse = 6"
PRINT #1, "yMouse = 4"
PRINT #1, "DoubleClick = .5"
PRINT #1, "ScreenSaver = SQUARES.COM"
PRINT #1, "Delay = 60"
PRINT #1,
PRINT #1, "[Desktop]"
PRINT #1, "Resolution = 640x480"
PRINT #1, "Background = HITECH.BMP"
PRINT #1, "DefaultShortcut = DISK2.BMP"
PRINT #1,
PRINT #1, "[Disktop]"
PRINT #1, "DisktopColourSet = DI_SEA.PAL"
PRINT #1,
PRINT #1, "[Compression Tools]"
PRINT #1, "CompressColourSet = CO_SEA.PAL"
PRINT #1,
PRINT #1, "[Properties]"
PRINT #1, "PropertyColourSet = PR_SEA.PAL"
PRINT #1,
PRINT #1, "[Print]"
PRINT #1, "PrintColourSet = PN_SEA.PAL"
PRINT #1,
PRINT #1, "[Paths]"
Temp$ = ENVIRON$("COMSPEC")
  IF LEN(Temp$) = 0 THEN
    Temp$ = LEFT$(InstallDir$, 2)
  ELSE
    Temp$ = LEFT$(Temp$, 2)
  END IF
PRINT #1, "DefaultDrive = " + Temp$
PRINT #1, "WinDos = " + InstallDir$
PRINT #1, "Shortcuts = " + InstallDir$ + "\PROGRAMS"
PRINT #1, "Desktop = " + InstallDir$ + "\DESKTOP"
Temp$ = ENVIRON$("TEMP")
  IF LEN(Temp$) = 0 THEN
  Temp$ = InstallDir$ + "\TEMP"
  END IF
PRINT #1, "Temp = " + Temp$
PRINT #1, "ScrSavers = " + InstallDir$ + "\SCREEN"
PRINT #1, "Fonts = " + InstallDir$ + "\FONTS"
PRINT #1, "ArchTools = " + InstallDir$ + "\ARCHIVES"
PRINT #1, "Graphics = " + InstallDir$ + "\GRAPHICS"
PRINT #1, "Icons = " + InstallDir$ + "\GRAPHICS\ICONS"
PRINT #1, "Keys = " + InstallDir$ + "\KEYS"
PRINT #1, "Palette = " + InstallDir$ + "\GRAPHICS\PALETTE"
PRINT #1,
PRINT #1, "[Default Viewers]"
PRINT #1, "Viewer = HTEDIT.EXE"
PRINT #1,
PRINT #1, "[Last Save]"
PRINT #1, "CurrentDir = " + LEFT$(InstallDir$, 3)
PRINT #1, "FileScroll = 1"
PRINT #1, "ViewType = 1"
PRINT #1, "FileSort = 0"
PRINT #1, "Filter = 48"
PRINT #1, "ExpCall = 0"
CLOSE


OPEN HistoryINI FOR OUTPUT AS #1
  FOR Counter% = 0 TO 50
  PRINT #1, InstallDir$
  NEXT
PRINT #1, " 1"
PRINT #1, " 0"
CLOSE


OPEN ExtensionsINI FOR OUTPUT AS #1
PRINT #1, "[TXT="
PRINT #1, "(HiTech Editor)"
PRINT #1, "{+" + InstallDir$ + "\HTEDIT.EXE <DRIVE>:<DIR\><FILE.EXT><SWITCH_TO_DIR><NO_PAUSE>}"
PRINT #1, "]"
CLOSE


OPEN QuickINI FOR OUTPUT AS #1
CLOSE


'OPEN IconsINI FOR APPEND AS #1
'PRINT #1, "README.BAT = DOC1.BMP"
'PRINT #1, "FASTKEYB.BAT = SHUTTLE.BMP"
'PRINT #1, "VD.BAT = RADIAT.BMP"
'CLOSE


'OPEN "DESKTOP\README.BAT" FOR OUTPUT AS #1
'PRINT #1, "@echo off"
'PRINT #1, LEFT$(InstallDir$, 2)
'PRINT #1, "cd " + InstallDir$
'PRINT #1, "htedit.exe " + InstallDir$ + "\README.TXT"
'CLOSE


'OPEN "DESKTOP\FASTKEYB.BAT" FOR OUTPUT AS #1
'PRINT #1, "@" + InstallDir$ + "\UTILS\FASTKEYB\FASTKEYB.COM"
'CLOSE


'OPEN "DESKTOP\VD.BAT" FOR OUTPUT AS #1
'PRINT #1, "@echo off"
'PRINT #1, LEFT$(InstallDir$, 2)
'PRINT #1, "cd " + InstallDir$ + "\UTILS\VD202"
'PRINT #1, "vd.exe"
'CLOSE


ON ERROR GOTO 0

'----------------------------------------------------------------------------

MoveText 1, 38, "Setup complete."
SLEEP 1

MoveText 1, 38, "Configure WinDos now (Y/N)?"

  IF YN% THEN
  SCREEN 0
  WIDTH 80, 25
  SHELL ConfigPrgm
  END IF

Finish

'============================================================================
NoSource:
MoveText 1, 38, "Source files unavailable!"
SLEEP
Finish

'----------------------------------------------------------------------------
DirErrors:
SELECT CASE ERR

  CASE 75
    DirectoryExists% = 1
    DirCreateError% = 0
    RESUME NEXT

  CASE ELSE
    DirCreateError% = 1
    MoveText 1, 38, "Cannot create directory!"
    SLEEP 3
    RESUME NEXT

END SELECT

'----------------------------------------------------------------------------
DriveErrors:
SELECT CASE ERR

  CASE 68
    DriveChangeError% = 1
    MoveText 1, 38, "Drive unavailable!"
    SLEEP 3
    RESUME NEXT

  CASE ELSE
    DriveChangeError% = 1
    MoveText 1, 38, "What kind of drive is this?"
    SLEEP 3
    RESUME NEXT

END SELECT

'----------------------------------------------------------------------------
ErrInst:
  IF ERR = 75 THEN        'The directory has been already created.
  RESUME NEXT
  END IF
MoveText 1, 38, "Error while installing!"
SLEEP
Finish

'Source$ file MUST exist, otherwise it would be created!
'The function returns: -1 if successful.
'                       0 if not successful.
'
FUNCTION CopyFile% (Source$, Destination$)

STATIC Buffer$
CONST MaxBuffSize = 4096 '32256
CopyFile% = -1

'============================================================================
File1% = FREEFILE

ON LOCAL ERROR GOTO OpenFileError1
OPEN Source$ FOR BINARY AS #File1%
ON LOCAL ERROR GOTO 0

Remaining& = LOF(File1%)
File2% = FREEFILE

ON LOCAL ERROR GOTO OpenFileError2
OPEN Destination$ FOR BINARY AS #File2%
ON LOCAL ERROR GOTO 0


  DO

    IF Remaining& <= MaxBuffSize THEN
      Buffer$ = SPACE$(Remaining&)
      Remaining& = 0
    ELSE
      Buffer$ = SPACE$(MaxBuffSize)
      Remaining& = Remaining& - MaxBuffSize
    END IF

  ON LOCAL ERROR GOTO FileAccessError
  GET #File1%, , Buffer$
  PUT #File2%, , Buffer$
  ON LOCAL ERROR GOTO 0

  LOOP WHILE Remaining&


CLOSE #File1%, #File2%

'----------------------------------------------------------------------------
AfterFileErrors:

  IF ErrorCode% = 67 OR ErrorCode% = 61 THEN   '67=Too many files error
  ON LOCAL ERROR GOTO KillDestError            '61=Disk full error
  KILL Destination$
  ON LOCAL ERROR GOTO 0
  END IF

AfterKillDestError:
EXIT FUNCTION

'============================================================================
'============================================================================
FileAccessError:
CLOSE #File2%
ErrorCode% = ERR

OpenFileError2:
CLOSE #File1%

OpenFileError1:
CopyFile% = 0
RESUME AfterFileErrors

'----------------------------------------------------------------------------
KillDestError:
RESUME AfterKillDestError

END FUNCTION

SUB Delay (BYVAL Time%)

'time%=67 (1 second)

  FOR i% = 1 TO Time%
  WAIT &H3DA, 8, 8
  WAIT &H3DA, 8
  NEXT i%

END SUB

SUB Finish

SCREEN 0
CLS
END

END SUB

SUB InfoBox

x1% = FrameX1%
y1% = FrameY1%
x2% = FrameX2%
y2% = FrameY2%

'----------------------------------------------------------------------------
accel! = 1.5                          'Zooming box

xmean% = (x1% + x2%) / 2
ymean% = (y1% + y2%) / 2

xcoord% = xmean%
ycoord% = ymean%

Counter! = 1


 DO
 Counter! = Counter! * accel!
 xcoord% = xcoord% + Counter!
 ycoord% = ycoord% + Counter!

   IF xcoord% > x2% THEN
   xcoord% = xcoord% - Counter!
     IF ycoord% > y2% THEN EXIT DO
   END IF

   IF ycoord% > y2% THEN
   ycoord% = ycoord% - Counter!
     IF xcoord% > x2% THEN EXIT DO
   END IF

 LINE (xmean% - (xcoord% - xmean%), ymean% - (ycoord% - ymean%))-(xcoord%, ycoord%), 0, BF
 LINE (xmean% - (xcoord% - xmean%) + 1, ymean% - (ycoord% - ymean%) + 1)-(xcoord% - 1, ycoord% - 1), 255, B

 Delay 1
 LOOP


LINE (x1%, y1%)-(x2%, y2%), 0, BF
LINE (x1% + 1, y1% + 1)-(x2% - 1, y2% - 1), 255, B

END SUB

SUB InputLine (BYVAL LocateX%, BYVAL LocateY%, BYVAL Init%, BYVAL Limit%, BYVAL StartScroll%)

'limit% = 63
'StartScroll% = 5
'locatex% = 25
'locatey% = 12
'word$ = ""

DEF SEG = &H40
POKE &H1A, PEEK(&H1C)
DEF SEG

COLOR 9

  IF Init% THEN
  GOSUB RemoveCursor
  EXIT SUB
  END IF

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

SUB Interface

SCREEN 13
CLS

  FOR i% = 20 TO 255
  Temp% = i% - 20
  Pal i%, Temp% \ 9, Temp% \ 4, Temp% \ 8         'Setting up the palette...
  NEXT

  FOR i% = 20 TO 255                     'Drawing lines...
  LINE (0, i%)-(320, i%), i%
  NEXT

'----------------------------------------------------------------------------
'Writing `WinDos Setup` in big digital letters...
Text$ = "WinDos Setup"

LOCATE 1, 1
COLOR 16
PRINT Text$

length% = LEN(Text$)

  FOR X% = 0 TO length% * 8
    FOR Y% = 0 TO 10
    Colour% = POINT(X%, Y%)
      IF Colour% = 16 THEN
      XPos% = X% * 3 + 20
      YPos% = Y% * 3 + 10
      LINE (XPos%, YPos%)-(XPos% + 1, YPos% + 1), 2, BF
      END IF
    NEXT
  NEXT

'----------------------------------------------------------------------------

LINE (0, 185)-(319, 194), 8, BF

GET (FrameX1%, FrameY1%)-(FrameX2%, FrameY2%), ScreenBuff

END SUB

SUB LoadFont (Text$, BYVAL TextX%, BYVAL TextY%, ColourX%, ColourY%, BYVAL StepColourX%, BYVAL StepColourY%)


ColourYSaved% = ColourY%
DEF SEG = &HF000


  FOR LetterNum% = 1 TO LEN(Text$)
  LetterOffset% = &HFA6E + ASC(MID$(Text$, LetterNum%, 1)) * 8
  xPosition% = TextX% + (LetterNum% - 1) * 8
  yPosition% = TextY%
  ColourX% = ColourX% + 8 * StepColourX%
  ColourY% = ColourYSaved%


    FOR LineNum% = 0 TO 7
    ScanLine% = PEEK(LetterOffset% + LineNum%)
    ColourY% = ColourY% + StepColourY%

    TempColour% = ColourX% + ColourY%

      IF ScanLine% AND 128 THEN
      PSET (xPosition%, yPosition%), TempColour%
      END IF

      IF ScanLine% AND 64 THEN
      PSET (xPosition% + 1, yPosition%), TempColour% + StepColourX%
      END IF

      IF ScanLine% AND 32 THEN
      PSET (xPosition% + 2, yPosition%), TempColour% + 2 * StepColourX%
      END IF

      IF ScanLine% AND 16 THEN
      PSET (xPosition% + 3, yPosition%), TempColour% + 3 * StepColourX%
      END IF

      IF ScanLine% AND 8 THEN
      PSET (xPosition% + 4, yPosition%), TempColour% + 4 * StepColourX%
      END IF

      IF ScanLine% AND 4 THEN
      PSET (xPosition% + 5, yPosition%), TempColour% + 5 * StepColourX%
      END IF

      IF ScanLine% AND 2 THEN
      PSET (xPosition% + 6, yPosition%), TempColour% + 6 * StepColourX%
      END IF

      IF ScanLine% AND 1 THEN
      PSET (xPosition% + 7, yPosition%), TempColour% + 7 * StepColourX%
      END IF

    yPosition% = yPosition% + 1
    NEXT

  NEXT



DEF SEG


END SUB

SUB MoveText (BYVAL Fast%, BYVAL LoopNum%, Text$)

Text$ = Text$ + SPACE$(40)


  FOR Position% = 1 TO LoopNum%
  LOCATE 1, 1
  COLOR 16
  Symbol$ = MID$(Text$, Position%, 1)
  PRINT Symbol$


    FOR xCounter% = 0 TO 7

      IF INKEY$ = CHR$(0) + CHR$(1) THEN
      Fast% = -1
      END IF

      FOR yCounter% = 0 TO 7
        IF POINT(xCounter%, yCounter%) THEN
          IF Fast% THEN
            PSET (310 + xCounter%, 187 + yCounter%), 14
            PSET (311 + xCounter%, 186 + yCounter%), 0
          ELSE
            PSET (317, 187 + yCounter%), 14
            PSET (318, 186 + yCounter%), 0
          END IF
        END IF
      NEXT

      IF Fast% = 0 THEN
      GET (2, 185)-(319, 194), Scroll
      PUT (1, 185), Scroll, PSET
      Delay 1
      END IF

    NEXT


    IF Fast% THEN
    GET (9, 185)-(319, 194), Scroll
    PUT (1, 185), Scroll, PSET
    LINE (310, 185)-(319, 194), 8, BF
    Delay 1
    END IF

  NEXT



END SUB

SUB Pal (BYVAL C%, BYVAL R%, BYVAL G%, BYVAL B%)

OUT &H3C8, C%
OUT &H3C9, R%
OUT &H3C9, G%
OUT &H3C9, B%

END SUB

FUNCTION RemoveSlash$ (DirString$)

  IF LEN(DirString$) > 3 AND RIGHT$(DirString$, 1) = "\" THEN
    RemoveSlash$ = LEFT$(DirString$, LEN(DirString$) - 1)
  ELSE
    RemoveSlash$ = DirString$
  END IF

END FUNCTION

FUNCTION YN%

  DO

  KH$ = UCASE$(INKEY$)

    IF KH$ = "Y" THEN
      YN% = -1
      EXIT DO
    ELSEIF KH$ = "N" THEN
      YN% = 0
      EXIT DO
    END IF

  LOOP

END FUNCTION

