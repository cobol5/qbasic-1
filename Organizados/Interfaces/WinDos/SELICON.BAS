'(C) HiTech Labs, by Denis Andrianov.
'============================================================================
'$INCLUDE: 'future.bi'

DECLARE SUB MouseSpeed (xSpeed%, ySpeed%)

DECLARE SUB Interface ()
DECLARE SUB Finish ()
DECLARE SUB ShowFiles ()
DECLARE SUB Button (x AS INTEGER, y AS INTEGER, x2 AS INTEGER, y2 AS INTEGER, Colour AS LONG, TopCol AS LONG, BotCol AS LONG)
DECLARE SUB ButtonsPress (x%, y%, ButNum%)
DECLARE SUB ButtonsRelease (ButNum%)

DECLARE SUB EmptyKeyb ()

DECLARE SUB ReadMainINI ()
DECLARE SUB ReadIconsINI ()
DECLARE FUNCTION ReadDir% (DirSpec$, Filter%, SearchType%)
DECLARE SUB FindNeededFiles ()
DECLARE SUB WhatFile (Number%, File$, Extension$, Directory%)
DECLARE SUB CalcFile (x%, y%, nFile%)

DECLARE SUB AddSlash (Strng$)
DECLARE FUNCTION RemoveExtension$ (FileName$)
DECLARE FUNCTION RemoveDirSlash$ (Directory$)
DECLARE FUNCTION CenterText% (XCoord1%, XCoord2%, Length%)


CONST Version = "1.1"                   'Program version number.
CONST MainINI = "windos.ini"
CONST MainTMP = "windos.tmp"
CONST IconsINI = "icons.ini"
CONST IconsTMP = "icons.tmp"
CONST IconSize = 2518                   'Size of icons in bytes.
CONST LeftButton = "lsmlbut.bmp"
CONST RightButton = "rsmlbut.bmp"
CONST FrameXSize = 300
CONST FrameYSize = 80
CONST FindFirst = &H4E00                'Functions of interrupt 21h.
CONST FindNext = &H4F00
CONST EntryNotFound = 0
CONST EntryFound = -1
CONST MaxFileNum = 999                  '0 is included!!!
CONST MaxDirNum = 499                   '0 is included!!!
CONST MaxFilesOnScreen = 279            '0 is included!!!

DIM SHARED id$                          'Program ID.
DIM SHARED IconsPath$                   'Icons directory.
DIM SHARED DesktopPath$                 'Desktop directory.
DIM SHARED GraphicsPath$                'Graphics directory.
DIM SHARED CurrentDir$                  'Current directory.
DIM SHARED Resolution$                  'Resolution.
DIM SHARED DefShortcut$                 'Default shortcut.
DIM SHARED SelectShortcut$              'Selected shortcut.
DIM SHARED SelectIcon$                  'Selected icon.
DIM SHARED XmsSpd%                      'Mouse speed on x.
DIM SHARED YmsSpd%                      'Mouse speed on y.
DIM SHARED FrameX1%                     'Top frame's coordinates.
DIM SHARED FrameY1%
DIM SHARED FrameX2%
DIM SHARED FrameY2%
DIM SHARED MaxX%                        'Max X resolution.
DIM SHARED MaxY%                        'Max Y resolution.
DIM SHARED MaxFilesPerColumn%           'On desktop.
DIM SHARED MaxFilesPerLine%             'On desktop.
DIM SHARED StepX%                       'Distance between files on desktop.
DIM SHARED StepY%
DIM SHARED StartX%                      'First coord. for icons on desktop.
DIM SHARED StartY%
DIM SHARED LastPointX%                  'Last x coord. for icons on desktop.
DIM SHARED LastPointY%                  'Last y coord. for icons on desktop.
DIM SHARED Files$(MaxFileNum)           'Array with file names.
DIM SHARED Directories$(MaxDirNum)      'Array with directory names.
DIM SHARED Icons$(MaxFilesOnScreen, 1)  'Array with icons (BMP).
'num,0 = *.*          'Shortcut
'num,1 = *.BMP        'Icon
DIM SHARED ObjNum%                      'Total amount of files + directories.
DIM SHARED FileScroll%                  'Scrolling position in file list.
'0=shortcuts
'1=icons only
DIM SHARED IconsStep%
'0=before continue is pressed
'1=after continue is pressed
'2=ready to store new settings

TYPE DTAStructure
  DOS            AS STRING * 19
  CreateTime     AS INTEGER
  Attributes     AS STRING * 1
  AccessTime     AS INTEGER
  AccessDate     AS INTEGER
  FileSize       AS LONG
  FileName       AS STRING * 13
END TYPE

TYPE RegTypeX
     ax    AS INTEGER
     bx    AS INTEGER
     cx    AS INTEGER
     dx    AS INTEGER
     bp    AS INTEGER
     si    AS INTEGER
     di    AS INTEGER
     Flags AS INTEGER
     ds    AS INTEGER
     es    AS INTEGER
END TYPE

DIM SHARED DTA AS DTAStructure
DIM SHARED Regs AS RegTypeX
'============================================================================
'Defaults

ON ERROR GOTO ErrHandler

  IF NOT IsVESA% THEN
  PRINT "Not a VESA compatible card."
  END
  END IF

id$ = "(C) HiTech Labs, by Denis Andrianov"
IconsStep% = 0

ReadMainINI
ReadIconsINI
Interface

Future.MouseOn

'============================================================================
 
  DO UNTIL KH$ = CHR$(27) OR KH$ = CHR$(0) + CHR$(68)
  Future.UpdateMouse
  x% = Future.MouseX%
  y% = Future.MouseY%
  b% = Future.MouseB%
  'Future.Print 0, 0, STR$(x%) + STR$(y%) + STR$(b%), RGB2Color&(0, 0, 255), 0

  KH$ = INKEY$

  SELECT CASE b%
   CASE 1
     IF Pressed% = 0 THEN
     Future.MouseOff
     ButtonsPress x%, y%, ButNum%
     Future.MouseOn
     Pressed% = 1
     END IF
   CASE 0
     IF Pressed% = 1 THEN
     Future.MouseOff
     Pressed% = 0
       IF ButNum% <> 0 THEN
       ButtonsRelease ButNum%
       END IF
     Future.MouseOn
     END IF
  END SELECT


  LOOP

Finish

'================================SUB ROUTINES================================

ErrHandler:
ResetScreen

SELECT CASE ERR
  CASE 7            'Out of memory.
  PRINT "Not enough conventional memory!!!"
  PRINT "Remove some TSR programs or device drivers and then try running"
  PRINT "this program again."

  CASE 9            'Out of range.
  PRINT "Subscript out of range. Please report this error."

  CASE 5            'Syntax Error.
  PRINT "Cannot proceed. Syntax error has occured."
 
  CASE 14
  PRINT "Out of stack space."
 
END SELECT

PRINT "Error #"; ERR; "has occured."
PRINT "Press any key to return to DOS."
SLEEP
END

SUB AddSlash (Strng$)

  IF RIGHT$(Strng$, 1) <> "\" THEN
  Strng$ = Strng$ + "\"
  END IF

END SUB

SUB Button (x AS INTEGER, y AS INTEGER, x2 AS INTEGER, y2 AS INTEGER, Colour AS LONG, TopCol AS LONG, BotCol AS LONG)

Future.FillBox x, y, x2, y2, Colour

Future.Line x, y, x2, y, TopCol, -1
Future.Line x, y, x, y2, TopCol, -1

Future.Line x2, y, x2, y2, BotCol, -1
Future.Line x, y2, x2, y2, BotCol, -1

END SUB

SUB ButtonsPress (x%, y%, ButNum%)

Colour& = RGB2Color&(200, 200, 200)
WhiteCol& = RGB2Color&(255, 255, 255)
BlackCol& = RGB2Color&(0, 0, 0)

'----------------------------------------------------------------------------
'Change Icon button
  IF x% > 20 AND y% > MaxY% - 35 AND x% < 330 AND y% < MaxY% - 5 THEN
 
  ButNum% = 1
  Button 20, MaxY% - 35, 330, MaxY% - 5, Colour&, BlackCol&, WhiteCol&
 
    SELECT CASE IconsStep%
      CASE 0
        Future.Print 131, MaxY% - 27, "Change icon", RGB2Color&(0, 0, 255), -1
      CASE ELSE
        Future.Print 101, MaxY% - 27, "Change to this icon", RGB2Color&(0, 0, 255), -1
    END SELECT

'----------------------------------------------------------------------------
'Exit button
  ELSEIF x% > 350 AND y% > MaxY% - 35 AND x% < 450 AND y% < MaxY% - 5 THEN
 
  ButNum% = 2
  Button 350, MaxY% - 35, 450, MaxY% - 5, Colour&, BlackCol&, WhiteCol&
  Future.Print 386, MaxY% - 27, "Exit", RGB2Color&(0, 0, 255), -1

'----------------------------------------------------------------------------
'Scrolling left button
  ELSEIF x% > MaxX% - 100 AND y% > MaxY% - 30 AND x% < MaxX% - 78 AND y% < MaxY% - 10 THEN
 
  ButNum% = 3
  Future.FillBox MaxX% - 100, MaxY% - 30, MaxX% - 78, MaxY% - 10, RGB2Color&(200, 200, 200)
  Future.LoadBMP GraphicsPath$ + LeftButton, MaxX% - 99, MaxY% - 29

'----------------------------------------------------------------------------
'Scrolling right button
  ELSEIF x% > MaxX% - 50 AND y% > MaxY% - 30 AND x% < MaxX% - 28 AND y% < MaxY% - 10 THEN

  ButNum% = 4
  Future.FillBox MaxX% - 50, MaxY% - 30, MaxX% - 28, MaxY% - 10, RGB2Color&(200, 200, 200)
  Future.LoadBMP GraphicsPath$ + RightButton, MaxX% - 49, MaxY% - 29

'----------------------------------------------------------------------------
'File name field
  ELSEIF x% < LastPointX% AND y% > LastPointY% AND y% < StartY% + StepY% - 10 THEN
 
  CalcFile x%, y%, nFile%
  WhatFile nFile%, File$, Extension$, Directory%
   
    IF Extension$ = "" THEN
      FileAndExt$ = File$
    ELSE
      FileAndExt$ = File$ + "." + Extension$
    END IF


    IF File$ <> "" THEN

      SELECT CASE IconsStep%
        CASE 0
          SelectShortcut$ = FileAndExt$
          Future.FillBox FrameX1% + 170, FrameY1% + 5, FrameX1% + 285, FrameY1% + 23, RGB2Color&(200, 200, 200)
          Future.Print FrameX1% + 180, FrameY1% + 7, SelectShortcut$, RGB2Color&(255, 0, 0), -1
        CASE 1
          SelectIcon$ = FileAndExt$
          Future.LoadBMP IconsPath$ + SelectIcon$, FrameX1% + 180, FrameY1% + 30
      END SELECT
   
    END IF
'----------------------------------------------------------------------------
  END IF


END SUB

SUB ButtonsRelease (ButNum%)

Colour& = RGB2Color&(200, 200, 200)
WhiteCol& = RGB2Color&(255, 255, 255)
BlackCol& = RGB2Color&(0, 0, 0)

'----------------------------------------------------------------------------
  IF ButNum% = 1 THEN         'Change Icon button

  Button 20, MaxY% - 35, 330, MaxY% - 5, Colour&, WhiteCol&, BlackCol&
 
    SELECT CASE IconsStep%
     
      CASE 0
        IF SelectShortcut$ <> "" THEN
          Future.Print 100, MaxY% - 28, "Change to this icon", RGB2Color&(0, 0, 255), -1
          IconsStep% = 1
          FileScroll% = 0
          CurrentDir$ = IconsPath$
          FindNeededFiles
          ShowFiles
       ELSE
          Future.Print 130, MaxY% - 28, "Change icon", RGB2Color&(0, 0, 255), -1
        END IF


      CASE 1
        IF SelectIcon$ <> "" THEN
          Future.Print 130, MaxY% - 28, "Change icon", RGB2Color&(0, 0, 255), -1
         
          Position% = 0
            DO
            BatchFile$ = Icons$(Position%, 0)
              IF (UCASE$(SelectShortcut$) = UCASE$(BatchFile$)) OR BatchFile$ = "" THEN
              Icons$(Position%, 0) = SelectShortcut$
              Icons$(Position%, 1) = SelectIcon$
              EXIT DO
              END IF
            Position% = Position% + 1
            LOOP UNTIL Position% > MaxFilesOnScreen

          File% = 1
          OPEN IconsINI FOR OUTPUT AS #File%
            FOR Counter% = 0 TO MaxFilesOnScreen
              IF Icons$(Counter%, 0) <> "" THEN
              PRINT #File%, Icons$(Counter%, 0) + " = " + Icons$(Counter%, 1)
              END IF
            NEXT
          CLOSE #File%

          Future.FillBox FrameX1% + 170, FrameY1% + 5, FrameX1% + 285, FrameY1% + 70, RGB2Color&(200, 200, 200)
          SelectShortcut$ = ""
          SelectIcon$ = ""
          IconsStep% = 0
          FileScroll% = 0
          CurrentDir$ = DesktopPath$
          FindNeededFiles
          ShowFiles
        ELSE
          Future.Print 100, MaxY% - 28, "Change to this icon", RGB2Color&(0, 0, 255), -1
        END IF

    END SELECT

'----------------------------------------------------------------------------
  ELSEIF ButNum% = 2 THEN     'Exit button
 
  Button 350, MaxY% - 35, 450, MaxY% - 5, Colour&, WhiteCol&, BlackCol&
  Future.Print 385, MaxY% - 28, "Exit", RGB2Color&(0, 0, 255), -1
  Finish

'----------------------------------------------------------------------------
  ELSEIF ButNum% = 3 THEN     'Left scrolling button
 
  Future.FillBox MaxX% - 99, MaxY% - 29, MaxX% - 77, MaxY% - 9, RGB2Color&(200, 200, 200)
  Future.LoadBMP GraphicsPath$ + LeftButton, MaxX% - 100, MaxY% - 30

  FileScroll% = FileScroll% - MaxFilesPerLine% * MaxFilesPerColumn%
   
    IF FileScroll% < 0 THEN
      FileScroll% = 0
    ELSE
      ShowFiles
    END IF

'----------------------------------------------------------------------------
  ELSEIF ButNum% = 4 THEN     'Right scrolling button

  Future.FillBox MaxX% - 49, MaxY% - 29, MaxX% - 27, MaxY% - 9, RGB2Color&(200, 200, 200)
  Future.LoadBMP GraphicsPath$ + RightButton, MaxX% - 50, MaxY% - 30

  OldFileScroll% = FileScroll%
  FileScroll% = FileScroll% + MaxFilesPerLine% * MaxFilesPerColumn%
   
    IF FileScroll% >= ObjNum% THEN
      FileScroll% = OldFileScroll%
    ELSE
      ShowFiles
    END IF

'----------------------------------------------------------------------------
  END IF


ButNum% = 0

END SUB

SUB CalcFile (x%, y%, nFile%)
 
xFile% = (x% - 15 + 22) \ 80 + 1
yFile% = MaxFilesPerColumn% - (y% - LastPointY% + 10) \ 76
nFile% = (yFile% - 1) * MaxFilesPerLine% + xFile% + FileScroll%

'Future.Print 0, 0, STR$(nFile%), RGB2Color&(255, 255, 255), 0

END SUB

FUNCTION CenterText% (XCoord1%, XCoord2%, Length%)

CenterText% = (XCoord2% - XCoord1% - Length% * 8) \ 2 + XCoord1%

END FUNCTION

SUB EmptyKeyb

DEF SEG = 0
POKE &H41A, PEEK(&H41C)
DEF SEG

END SUB

SUB FindNeededFiles

ERASE Files$
ERASE Directories$

  IF ReadDir(CurrentDir$ + "*.*", 32, FindFirst) = EntryFound THEN
    DO
      IF (ASC(DTA.Attributes) AND 16) = 16 THEN        'This is a directory.
          IF RTRIM$(DTA.FileName) <> "." AND RTRIM$(DTA.FileName) <> ".." AND DirCount% <= MaxDirNum THEN
          Directories$(DirCount%) = DTA.FileName
          DirCount% = DirCount% + 1
          END IF
      ELSE                                             'This is a file.
          IF FileCount% <= MaxFileNum THEN
            IF (IconsStep% = 1 AND DTA.FileSize = IconSize) OR IconsStep% = 0 THEN
            Files$(FileCount%) = DTA.FileName
            FileCount% = FileCount% + 1
            END IF
          END IF
      END IF
      IF ReadDir%(ShortcutsPath$ + "*.*", 32, FindNext) <> EntryFound THEN
      EXIT DO
      END IF
    LOOP
  END IF

ObjNum% = DirCount% + FileCount%

END SUB

SUB Finish

ResetScreen
END

END SUB

SUB Interface

SELECT CASE Resolution$
  CASE "1600x1200"
    MaxX% = 1600
    MaxY% = 1200
    Set1600x1200 16
  CASE "1280x1024"
    MaxX% = 1280
    MaxY% = 1024
    Set1280x1024 16
  CASE "1024x768"
    MaxX% = 1024
    MaxY% = 768
    Set1024x768 16
  CASE "800x600"
    MaxX% = 800
    MaxY% = 600
    Set800x600 16
  CASE ELSE
    MaxX% = 640
    MaxY% = 480
    Set640x480 16
    'MaxX% = 800
    'MaxY% = 600
    'Set800x600 16
END SELECT


FrameX1% = (MaxX% - FrameXSize) \ 2         'Top frame's coordinates.
FrameY1% = 1
FrameX2% = FrameX1% + FrameXSize
FrameY2% = FrameY1% + FrameYSize

StepX% = 80
StepY% = 76
StartX% = 15
StartY% = MaxY% - 30 - StepY%
MaxFilesPerLine% = (MaxX% - StartX% + (43 - StartX%)) \ StepX%
LastPointX% = MaxFilesPerLine% * StepX% + StartX% - (43 - StartX%)
MaxFilesPerColumn% = (MaxY% - 30 - 120) \ StepY%
LastPointY% = MaxY% - 30 - MaxFilesPerColumn% * StepY%
 
'Bottom bar...
Future.FillBox 0, MaxY% - 40, MaxX% - 1, MaxY% - 1, RGB2Color&(200, 200, 200)
Future.Box 1, MaxY% - 39, MaxX% - 2, MaxY% - 2, RGB2Color&(250, 250, 250)

'Buttons...
Future.LoadBMP GraphicsPath$ + LeftButton, MaxX% - 100, MaxY% - 30
Future.LoadBMP GraphicsPath$ + RightButton, MaxX% - 50, MaxY% - 30
Colour& = RGB2Color&(200, 200, 200)
TopCol& = RGB2Color&(255, 255, 255)
BotCol& = RGB2Color&(0, 0, 0)
Button 20, MaxY% - 35, 330, MaxY% - 5, Colour&, TopCol&, BotCol&
Future.Print 130, MaxY% - 28, "Change icon", RGB2Color&(0, 0, 255), -1
Button 350, MaxY% - 35, 450, MaxY% - 5, Colour&, TopCol&, BotCol&
Future.Print 385, MaxY% - 28, "Exit", RGB2Color&(0, 0, 255), -1

'Top menu...
Future.FillBox FrameX1%, FrameY1%, FrameX2%, FrameY2%, RGB2Color&(200, 200, 200)
Future.Box FrameX1% + 1, FrameY1% + 1, FrameX2% - 1, FrameY2% - 1, RGB2Color&(255, 255, 255)
Future.Print FrameX1% + 10, FrameY1% + 7, "Selected shortcut:", RGB2Color&(0, 0, 200), -1
Future.Print FrameX1% + 10, FrameY1% + 40, "Selected icon:", RGB2Color&(0, 0, 200), -1

'Printing program information...
Future.Print CenterText(0, MaxX%, LEN(id$)), 90, id$, RGB2Color&(255, 0, 0), 0
VerText$ = "Version " + Version
Future.Print 0, 45, VerText$, RGB2Color&(255, 255, 0), 0
Future.Print MaxX% - LEN(VerText$) * 8, 45, VerText$, RGB2Color&(255, 255, 0), 0

FileScroll% = 0
CurrentDir$ = DesktopPath$
FindNeededFiles
ShowFiles

SetLocation MaxX% \ 2, MaxY% \ 2
SetMouseRange 0, 0, MaxX% - 1, MaxY% - 1
MouseSpeed XmsSpd%, YmsSpd%

END SUB

SUB MouseSpeed (xSpeed%, ySpeed%)

Regs.ax = 15
Regs.cx = xSpeed%
Regs.dx = ySpeed%
CALL INTERRUPTX(&H33, Regs, Regs)

END SUB

FUNCTION ReadDir% (DirSpec$, Filter%, SearchType%)
 
  IF SearchType% <> FindFirst AND SearchType% <> FindNext THEN
  EXIT FUNCTION
  END IF

  IF SearchType% = FindFirst THEN
  'Setting up DTA...
  Regs.ax = &H1A00
  Regs.ds = VARSEG(DTA)
  Regs.dx = VARPTR(DTA)
  CALL INTERRUPTX(&H21, Regs, Regs)
  END IF
     
'Finding FIRST or NEXT entries...
DirSpec$ = DirSpec$ + CHR$(0)
Regs.ax = SearchType%
Regs.cx = Filter%
Regs.ds = VARSEG(DirSpec$)
Regs.dx = SADD(DirSpec$)
CALL INTERRUPTX(&H21, Regs, Regs)

  IF Regs.Flags AND 1 THEN
    ReadDir% = EntryNotFound
  ELSE
    NullByte% = INSTR(DTA.FileName, CHR$(0))
      IF NullByte% > 0 THEN
      DTA.FileName = LEFT$(DTA.FileName, NullByte% - 1)
      END IF
    ReadDir% = EntryFound
  END IF

END FUNCTION

SUB ReadIconsINI

'Reading icons' INI file.

Position% = 0
File% = 1

OPEN IconsINI FOR INPUT AS #File%
 
  DO UNTIL EOF(File%)
  LINE INPUT #File%, junk$
  Equals% = INSTR(junk$, "=")
   
    IF Equals% > 1 THEN
      IF Position% > MaxFilesOnScreen THEN
      EXIT DO
      END IF
    BatchFile$ = LTRIM$(RTRIM$(LEFT$(junk$, Equals% - 1)))
    IconFile$ = LTRIM$(RTRIM$(RIGHT$(junk$, LEN(junk$) - Equals%)))
      IF ReadDir(DesktopPath$ + BatchFile$, 32, FindFirst) AND ReadDir(IconsPath$ + IconFile$, 32, FindFirst) THEN
        IF DTA.FileSize = IconSize THEN
        Icons$(Position%, 0) = BatchFile$
        Icons$(Position%, 1) = IconFile$
        END IF
      Position% = Position% + 1
      END IF
    END IF
 
  LOOP

CLOSE #File%

END SUB

SUB ReadMainINI

File% = 1
OPEN MainINI FOR INPUT AS #File%

  DO UNTIL EOF(File%) = -1
  LINE INPUT #File%, Something$
  Something$ = LTRIM$(RTRIM$(Something$))
'----------------------------------------------------------------------------
    IF LEFT$(UCASE$(Something$), 10) = "RESOLUTION" THEN
    GOSUB LookSpace
    Resolution$ = MID$(Something$, equal%, LEN(Something$) - equal% + 1)

    ELSEIF LEFT$(UCASE$(Something$), 15) = "DEFAULTSHORTCUT" THEN
    GOSUB LookSpace
    DefShortcut$ = MID$(Something$, equal%, LEN(Something$) - equal% + 1)

'----------------------------------------------------------------------------
    ELSEIF LEFT$(UCASE$(Something$), 8) = "GRAPHICS" THEN
    GOSUB LookSpace
    GraphicsPath$ = MID$(Something$, equal%, LEN(Something$) - equal% + 1)
    AddSlash GraphicsPath$
   
    ELSEIF LEFT$(UCASE$(Something$), 5) = "ICONS" THEN
    GOSUB LookSpace
    IconsPath$ = MID$(Something$, equal%, LEN(Something$) - equal% + 1)
    AddSlash IconsPath$

    ELSEIF LEFT$(UCASE$(Something$), 7) = "DESKTOP" THEN
    GOSUB LookSpace
    DesktopPath$ = MID$(Something$, equal%, LEN(Something$) - equal% + 1)
    AddSlash DesktopPath$

'----------------------------------------------------------------------------
    ELSEIF LEFT$(UCASE$(Something$), 6) = "XMOUSE" THEN
    GOSUB LookSpace
    XmsSpd% = VAL(MID$(Something$, equal%, LEN(Something$) - equal% + 1))

    ELSEIF LEFT$(UCASE$(Something$), 6) = "YMOUSE" THEN
    GOSUB LookSpace
    YmsSpd% = VAL(MID$(Something$, equal%, LEN(Something$) - equal% + 1))
   
    END IF

  LOOP

CLOSE #File%
EXIT SUB

'----------------------------------------------------------------------------
LookSpace:
equal% = INSTR(Something$, "=")
  DO
  equal% = equal% + 1
  Character$ = MID$(Something$, equal%, 1)
  LOOP WHILE Character$ = " "
RETURN

'----------------------------------------------------------------------------

END SUB

FUNCTION RemoveDirSlash$ (Directory$)
 
  IF LEN(Directory$) > 3 AND RIGHT$(Directory$, 1) = "\" THEN
    RemoveDirSlash$ = LEFT$(Directory$, LEN(Directory$) - 1)
  ELSE
    RemoveDirSlash$ = Directory$
  END IF

END FUNCTION

FUNCTION RemoveExtension$ (FileName$)

Dot% = INSTR(FileName$, ".")
  IF Dot% > 0 THEN
    RemoveExtension$ = LEFT$(FileName$, Dot% - 1)
  ELSE
    RemoveExtension$ = FileName$
  END IF

END FUNCTION

SUB ShowFiles

Future.FillBox 0, LastPointY% - 2, MaxX%, StartY% + StepY% - 15, RGB2Color&(0, 0, 0)

CurrentX% = StartX%
CurrentY% = StartY%

 
  FOR Counter% = FileScroll% TO FileScroll% + (MaxFilesPerLine% * MaxFilesPerColumn% - 1)
   
    IF Counter% > MaxFileNum THEN
      EXIT SUB
    ELSE
      IF Files$(Counter%) = "" THEN
      EXIT SUB
      END IF
    END IF
 
    IF IconsStep% = 0 THEN
      Shortcut$ = DefShortcut$
      Position% = 0
        DO
        BatchFile$ = Icons$(Position%, 0)
          IF UCASE$(RTRIM$(Files$(Counter%))) = UCASE$(BatchFile$) THEN
          Shortcut$ = Icons$(Position%, 1)
          EXIT DO
          END IF
        Position% = Position% + 1
        LOOP UNTIL BatchFile$ = "" OR Position% > MaxFilesOnScreen
    ELSE
      Shortcut$ = Files$(Counter%)
    END IF

  Future.LoadBMP IconsPath$ + Shortcut$, CurrentX%, CurrentY%
  FileWithoutExtension$ = RTRIM$(RemoveExtension$(Files$(Counter%)))
  Future.Print CenterText(CurrentX% - 20, CurrentX% + 57, LEN(FileWithoutExtension$)), CurrentY% + 40, FileWithoutExtension$, RGB2Color&(0, 255, 0), 0
 
  CurrentX% = CurrentX% + StepX%
    IF CurrentX% > LastPointX% THEN
    CurrentX% = StartX%
    CurrentY% = CurrentY% - StepY%
    END IF
 
  NEXT

END SUB

SUB WhatFile (Number%, File$, Extension$, Directory%)

  FOR Counter% = 1 TO Number%
    IF Counter% > MaxDirNum + 1 THEN GOTO CheckFiles
  Something$ = Directories$(Counter% - 1)
    IF Something$ = "" THEN GOTO CheckFiles
  NEXT Counter%

File$ = RTRIM$(Something$)            'This is a directory !!!
Extension$ = ""
Directory% = 1
EXIT SUB

'----------------------------------------------------------------------------
CheckFiles:

  FOR Counter2% = 1 TO Number% - (Counter% - 1)
    IF Counter2% > MaxFileNum + 1 THEN
    File$ = ""
    Extension$ = ""
    EXIT SUB
    END IF
  Something$ = Files$(Counter2% - 1)
    IF Something$ = "" THEN
    File$ = ""
    Extension$ = ""
    EXIT SUB
    END IF
  NEXT Counter2%

Something$ = LTRIM$(RTRIM$(Something$))
Dot% = INSTR(1, Something$, ".")
  IF Dot% > 0 THEN
    Extension$ = RIGHT$(Something$, LEN(Something$) - Dot%)
    File$ = LEFT$(Something$, Dot% - 1)
  ELSE
    Extension$ = ""
    File$ = Something$
  END IF
Directory% = 0

END SUB

