'Copyright (C) HiTech Laboratories, Denis Andrianov.
'WinDos Desktop
'============================================================================
'$INCLUDE: 'future.bi'

DECLARE SUB MouseSpeed (xspeed%, yspeed%)

DECLARE SUB Interface ()
DECLARE SUB ShowFiles ()
DECLARE SUB ButtonsPress (x%, y%, ButNum%)
DECLARE SUB ButtonsRelease (ButNum%)

DECLARE SUB EmptyKeyb ()
DECLARE SUB DosScr (Save%)
'0=load
'1=save
DECLARE SUB SaveCfg (ExpCall%)
DECLARE SUB ReadMainINI ()
DECLARE SUB ReadIconsINI ()
DECLARE FUNCTION ReadDir% (Mask$, Filter%, SearchType%)
DECLARE SUB FindNeededFiles ()
DECLARE SUB WhatFile (Number%, File$, Extension$, Directory%)
DECLARE SUB FullMemEXE (Program$, PrgDir$)
'DECLARE SUB RunDisktop ()

DECLARE FUNCTION AddSlash$ (Path$)
DECLARE SUB ExtractFileExt (File$, Extension$)
DECLARE FUNCTION CenterText% (XCoord1%, XCoord2%, Length%)

DECLARE SUB BeforeFinish ()


CONST id = "(C) HiTech Laboratories, Denis Andrianov"
CONST Version = "WinDos 265 Desktop"
CONST Date = "29 December, 2002"
CONST MainINI = "windos.ini"
CONST MainTMP = "windos.tmp"
CONST IconsINI = "icons.ini"
CONST IconsTMP = "icons.tmp"
CONST Disktop = "disktop.bat"
CONST SelIcon = "selicon.exe"
CONST Options = "options.exe"
CONST About = "about.bat"
CONST Batch = "runprg.tmp"
CONST DosScrFile = "dos.scr"
CONST ProgramsFramePic = "shrtcuts.bmp"
CONST IconsFramePic = "icons.bmp"
CONST LongButPic = "lbut256.bmp"
CONST NormalButPic = "but256.bmp"
CONST FindFirst = &H4E00               'Functions of interrupt 21h.
CONST FindNext = &H4F00
CONST MaxMenuFiles = 14
CONST MaxFileNum = 999                  '0 is included!!!
CONST MaxDirNum = 499                   '0 is included!!!
CONST MaxFilesOnScreen = 279            '0 is included!!!
CONST IconSize = 2518

DIM SHARED WindosPath$                  'Windos's main path.
DIM SHARED WindosPathSlash$
DIM SHARED ScrPath$
DIM SHARED TempPathSlash$
DIM SHARED GraphicsPathSlash$
DIM SHARED IconsPathSlash$
DIM SHARED ShortcutsPath$
DIM SHARED DesktopPath$
DIM SHARED DesktopPathSlash$
DIM SHARED DefaultDrive$                'Drive with MS-DOS.
DIM SHARED CurrentDir$                  'Current directory.

DIM SHARED CMDLine$                     'Command line for other programs.
DIM SHARED ScreenSaver$                 'Screen saver executable file.
DIM SHARED ScrDelay%                    'Screen saver delay.
DIM SHARED DblClickDelay!               'Double click delay.
DIM SHARED DefShortcut$                 'Default shortcut.
DIM SHARED Background$                  'Background filename.
DIM SHARED Resolution$                  'Resolution.
DIM SHARED XmsSpd%                      'Mouse speed on x.
DIM SHARED YmsSpd%                      'Mouse speed on y.
DIM SHARED MaxX%                        'Max X resolution.
DIM SHARED MaxY%                        'Max Y resolution.
DIM SHARED DirCount%                    'Num of dirs read.
DIM SHARED FileCount%                   'Num of files read.
DIM SHARED MaxFilesPerColumn%           'On desktop.
DIM SHARED MaxFilesPerLine%             'On desktop.
DIM SHARED StepX%                       'Distance between files on desktop.
DIM SHARED StepY%
DIM SHARED StartX%                      'First coord. for icons on desktop.
DIM SHARED StartY%
DIM SHARED LastPointX%                  'Last x coord. for icons on desktop.
DIM SHARED LastPointY%                  'Last y coord. for icons on desktop.
DIM SHARED FrameX1%                     'Main menu's coordinates.
DIM SHARED FrameY1%
DIM SHARED FrameX2%
DIM SHARED FrameY2%
DIM SHARED Files$(MaxFileNum)           'Array with file names.
DIM SHARED Directories$(MaxDirNum)      'Array with directory names.
DIM SHARED Icons$(MaxFilesOnScreen, 1)  'Array with icons (BMP).
'num,0 = *.*          'Shortcut
'num,1 = *.BMP        'Icon
DIM SHARED ObjNum%                      'Total amount of files + directories.
DIM SHARED FileScroll%                  'Scrolling position in file list.
DIM SHARED MenuState%                   'State of main menu.
'0=nothing is displayed
'1=main menu is displayed
DIM SHARED ProgramsMenuFirstTime%
'0=if the menu was turned on more than once.
'1=if the menu was turned on the first time.
DIM SHARED ButXMSHandle%                'XMS handle for buttons.
DIM SHARED DesktopXMSHandle%            'XMS handle for desktop wallpaper.
DIM SHARED ProgramsMenuXMSHandle%       'XMS handle for main menu.

TYPE DTAStructure
  DOS            AS STRING * 19
  CreateTime     AS STRING * 2
  Attributes     AS STRING * 1
  AccessTime     AS STRING * 2
  AccessDate     AS STRING * 2
  FileSize       AS LONG
  Filename       AS STRING * 13
END TYPE

TYPE RegTypeX
     ax    AS INTEGER
     bx    AS INTEGER
     cx    AS INTEGER
     dx    AS INTEGER
     bp    AS INTEGER
     si    AS INTEGER
     di    AS INTEGER
     flags AS INTEGER
     ds    AS INTEGER
     es    AS INTEGER
END TYPE

DIM SHARED DTA AS DTAStructure
DIM SHARED Regs AS RegTypeX
'============================================================================
'Defaults

  SELECT CASE UCASE$(COMMAND$)
  CASE "/?", "/H"
    COLOR 14
    PRINT id
    PRINT Version
    PRINT
    COLOR 2
    PRINT "Date of completion: " + Date
    PRINT
    COLOR 12
    PRINT "Thank you for using our program."
    END
  END SELECT

  IF NOT IsVESA% THEN
  PRINT "VESA mode not supported."
  END
  END IF

  IF NOT IsXMSInstalled% THEN
  PRINT "XMS driver is not installed."
  END
  END IF

Temp& = FreeXMSmemory&
  IF Temp& < 700000 AND Temp& >= 0 THEN
  PRINT "Not enough XMS. At least 700Kb is needed."
  END
  END IF

ButXMSHandle% = AllocateXMS%(17000)
DesktopXMSHandle% = AllocateXMS%(340000)
ProgramsMenuXMSHandle% = AllocateXMS%(340000)

ReadMainINI
SaveCfg 0
DosScr 1
ReadIconsINI
Interface

Future.MouseOn

ProgramsMenuFirstTime% = 1

'============================================================================
 
  DO
  Future.UpdateMouse
  x% = Future.MouseX%
  y% = Future.MouseY%
  b% = Future.MouseB%
  'Future.Print 0, 0, STR$(x%) + STR$(y%) + STR$(b%), RGB2Color&(0, 0, 255), 0

  IF CurrentTime$ <> TIME$ THEN
  CurrentTime$ = TIME$
    IF x% > MaxX% - 120 AND y% > MaxY% - 50 AND x% < MaxX% AND y% < MaxY% THEN
      Future.MouseOff
      GOSUB ChangeTime
      Future.MouseOn
    ELSE
      GOSUB ChangeTime
    END IF
  END IF

  IF oldx% <> x% OR oldy% <> y% THEN        'Checking for any mouse movement
  oldx% = x%                                'for the screensaver.
  oldy% = y%
  ScrCount% = 0                             'Reset screensaver counter.
  END IF

KH$ = INKEY$
  IF LEN(KH$) > 0 AND b% = 0 THEN
  ScrCount% = 0                             'Reset screensaver counter.
  Future.MouseOff
    SELECT CASE KH$                         'Keyboard control.
      CASE CHR$(27)                                  '<ESC> show DOS screen
        DosScr 0
        SLEEP
        EmptyKeyb
        Interface
      CASE CHR$(0) + CHR$(68)                        'F10 (exit)
        BeforeFinish
        END
    END SELECT
  Future.MouseOn
  END IF

 SELECT CASE b%
   CASE 1
     IF Pressed% = 0 THEN
     Future.MouseOff
     ButtonsPress x%, y%, ButNum%
     Future.MouseOn
     LeftClick% = LeftClick% + 1       'Double click detection
       IF LeftClick% = 1 THEN OldTime! = TIMER
     Pressed% = 1
     END IF

   CASE 0
     IF LeftClick% = 2 AND ((TIMER - OldTime!) < DblClickDelay!) THEN
       LeftClick% = 0                  'Double click detection
       DoubleClick% = 1
       GOSUB OpenIt
     ELSEIF ((TIMER - OldTime!) > DblClickDelay!) THEN
       LeftClick% = 0
     END IF
     IF Pressed% THEN
     Future.MouseOff
     Pressed% = 0
       IF ButNum% <> 0 THEN
       ButtonsRelease ButNum%
       END IF
     Future.MouseOn
     END IF
 END SELECT


  LOOP


'================================SUB ROUTINES================================
OpenIt:
  IF DoubleClick% = 1 AND y% < MaxY% - 30 THEN

  SELECT CASE MenuState%
    CASE 0

      IF x% < LastPointX% AND y% > LastPointY% AND y% < StartY% + StepY% - 10 THEN
      GOSUB CalcFile
      WhatFile nFile%, File$, Extension$, Directory%
        IF File$ <> "" THEN
          IF Extension$ <> "" THEN
            FullMemEXE File$ + "." + Extension$, CurrentDir$
          ELSE
            FullMemEXE File$, CurrentDir$
          END IF
        END IF
      END IF

      IF y% < 56 THEN
      GOSUB CalcFile
        IF xFile% = 1 THEN                  'Disktop
          FullMemEXE Disktop, WindosPath$
        ELSEIF xFile% = 2 THEN              'Options
          FullMemEXE Options, WindosPath$
        ELSEIF xFile% = 3 THEN              'Icons
          FullMemEXE SelIcon, WindosPath$
        ELSEIF xFile% = 4 THEN              'DOS Shell
          FullMemEXE "COMMAND.COM", DefaultDrive$ + "\"
        ELSEIF xFile% = MaxFilesPerLine% - 1 THEN
          FullMemEXE About, WindosPath$
        ELSEIF xFile% = MaxFilesPerLine% THEN
          BeforeFinish
          END
        END IF
      END IF


    CASE 1
      IF x% > FrameX1% + 44 AND y% > FrameY1% + 66 AND x% < FrameX1% + 271 AND y% < FrameY1% + 294 THEN
      GOSUB CalcFile
      WhatFile nFile%, File$, Extension$, Directory%
        IF LEN(File$) THEN
          IF Directory% = 1 THEN
            FileScroll% = 0
            CurrentDir$ = AddSlash$(CurrentDir$) + File$
            FindNeededFiles
            Future.MouseOff
            ShowFiles
            Future.MouseOn
          ELSE
            IF LEN(Extension$) THEN
            FullMemEXE File$ + "." + Extension$, CurrentDir$
            END IF
          END IF
        END IF
      'Future.Print 0, 0, File$ + "." + Extension$ + STR$(Directory%), RGB2Color&(255, 255, 255), 0
      END IF
 
 
  END SELECT

  END IF

DoubleClick% = 0
RETURN

'----------------------------------------------------------------------------
CalcFile:

SELECT CASE MenuState%

  CASE 0
  xFile% = (x% - 15 + 22) \ 80 + 1
  yFile% = MaxFilesPerColumn% - (y% - LastPointY% + 10) \ 76
  nFile% = (yFile% - 1) * MaxFilesPerLine% + xFile%

  CASE 1
  nFile% = (y% - FrameY1% - 70) \ 16 + 1 + FileScroll%

END SELECT

'Future.Print 0, 0, STR$(nFile%), RGB2Color&(255, 255, 255), 0
RETURN

'----------------------------------------------------------------------------
ChangeTime:
Future.FillBox MaxX% - 90, MaxY% - 22, MaxX% - 26, MaxY% - 6, RGB2Color&(200, 200, 200)
Future.Print MaxX% - 90, MaxY% - 22, CurrentTime$, RGB2Color&(0, 0, 255), -1
ScrCount% = ScrCount% + 1
  IF ScrCount% >= ScrDelay% THEN
  ScrCount% = 0
    IF ScreenSaver$ <> "" THEN
    FullMemEXE ScreenSaver$, ScrPath$
    END IF
  END IF
RETURN

'SUB RunDisktop
'
'File% = FREEFILE
'OPEN MainINI FOR APPEND AS #File%
'PRINT #File%, "RunDesktop"
'CLOSE #File%
'
'BeforeFinish
'RUN Disktop
'
'END SUB

FUNCTION AddSlash$ (Path$)

  IF RIGHT$(Path$, 1) <> "\" THEN
    AddSlash$ = Path$ + "\"
  ELSE
    AddSlash$ = Path$
  END IF

END FUNCTION

SUB BeforeFinish

DeallocateXMS ProgramsMenuXMSHandle%
DeallocateXMS DesktopXMSHandle%
DeallocateXMS ButXMSHandle%

ResetScreen
EmptyKeyb

END SUB

SUB ButtonsPress (x%, y%, ButNum%)

  IF x% > 5 AND y% > MaxY% - 26 AND x% < 368 AND y% < MaxY% THEN
  ButNum% = 1            'Programs Menu button
  Future.XMSget 5, MaxY% - 26, 368, MaxY% - 4, ButXMSHandle%
  Future.XMSput 6, MaxY% - 25, ButXMSHandle%
  END IF

  IF x% > 380 AND y% > MaxY% - 26 AND x% < 496 AND y% < MaxY% THEN
  ButNum% = 2            'Edit button
  Future.XMSget 380, MaxY% - 26, 496, MaxY% - 4, ButXMSHandle%
  Future.XMSput 381, MaxY% - 25, ButXMSHandle%
  END IF

  IF x% > FrameX1% + 352 AND y% > FrameY1% + 94 AND x% < FrameX1% + 404 AND y% < FrameY1% + 133 AND MenuState% = 1 THEN
  ButNum% = 3            'Home button
  Future.LINE FrameX1% + 352, FrameY1% + 94, FrameX1% + 404, FrameY1% + 94, RGB2Color&(0, 0, 0), -1
  Future.LINE FrameX1% + 405, FrameY1% + 94, FrameX1% + 405, FrameY1% + 133, RGB2Color&(255, 255, 255), -1
  Future.LINE FrameX1% + 405, FrameY1% + 133, FrameX1% + 353, FrameY1% + 133, RGB2Color&(255, 255, 255), -1
  Future.LINE FrameX1% + 352, FrameY1% + 133, FrameX1% + 352, FrameY1% + 94, RGB2Color&(0, 0, 0), -1
  END IF

  IF x% > FrameX1% + 434 AND y% > FrameY1% + 94 AND x% < FrameX1% + 486 AND y% < FrameY1% + 133 AND MenuState% = 1 THEN
  ButNum% = 4            'Up Dir button
  Future.LINE FrameX1% + 434, FrameY1% + 94, FrameX1% + 486, FrameY1% + 94, RGB2Color&(0, 0, 0), -1
  Future.LINE FrameX1% + 487, FrameY1% + 94, FrameX1% + 487, FrameY1% + 133, RGB2Color&(255, 255, 255), -1
  Future.LINE FrameX1% + 487, FrameY1% + 133, FrameX1% + 435, FrameY1% + 133, RGB2Color&(255, 255, 255), -1
  Future.LINE FrameX1% + 434, FrameY1% + 133, FrameX1% + 434, FrameY1% + 94, RGB2Color&(0, 0, 0), -1
  END IF

  IF x% > FrameX1% + 283 AND y% > FrameY1% + 61 AND x% < FrameX1% + 308 AND y% < FrameY1% + 81 AND MenuState% = 1 THEN
  ButNum% = 5            'Up arrow
    IF FileScroll% > 0 THEN
    FileScroll% = FileScroll% - 1
    ShowFiles
    END IF
  END IF

  IF x% > FrameX1% + 283 AND y% > FrameY1% + 283 AND x% < FrameX1% + 308 AND y% < FrameY1% + 303 AND MenuState% = 1 THEN
  ButNum% = 6            'Down arrow
    IF FileScroll% < ObjNum% - MaxMenuFiles THEN
    FileScroll% = FileScroll% + 1
    ShowFiles
    END IF
  END IF

  IF x% > FrameX1% + 352 AND y% > FrameY1% + 181 AND x% < FrameX1% + 486 AND y% < FrameY1% + 220 AND MenuState% = 1 THEN
  ButNum% = 7            'Exit Menu button
  Future.LINE FrameX1% + 352, FrameY1% + 181, FrameX1% + 486, FrameY1% + 181, RGB2Color&(0, 0, 0), -1
  Future.LINE FrameX1% + 487, FrameY1% + 181, FrameX1% + 487, FrameY1% + 220, RGB2Color&(255, 255, 255), -1
  Future.LINE FrameX1% + 487, FrameY1% + 220, FrameX1% + 353, FrameY1% + 220, RGB2Color&(255, 255, 255), -1
  Future.LINE FrameX1% + 352, FrameY1% + 220, FrameX1% + 352, FrameY1% + 181, RGB2Color&(0, 0, 0), -1
  END IF

END SUB

SUB ButtonsRelease (ButNum%)

  IF ButNum% = 1 OR ButNum% = 7 THEN       'Programs menu/Exit Menu button
 
    SELECT CASE ButNum%
    CASE 1
      Future.XMSput 5, MaxY% - 26, ButXMSHandle%
    CASE ELSE
      Future.LINE FrameX1% + 352, FrameY1% + 181, FrameX1% + 486, FrameY1% + 181, RGB2Color&(255, 255, 255), -1
      Future.LINE FrameX1% + 487, FrameY1% + 181, FrameX1% + 487, FrameY1% + 220, RGB2Color&(0, 0, 0), -1
      Future.LINE FrameX1% + 487, FrameY1% + 220, FrameX1% + 353, FrameY1% + 220, RGB2Color&(0, 0, 0), -1
      Future.LINE FrameX1% + 352, FrameY1% + 220, FrameX1% + 352, FrameY1% + 181, RGB2Color&(255, 255, 255), -1
    END SELECT

  MenuState% = 1 - MenuState%
   
    IF MenuState% = 1 THEN
      Future.XMSget FrameX1%, FrameY1%, FrameX2%, FrameY2%, DesktopXMSHandle%
        IF ProgramsMenuFirstTime% = 1 THEN
          ProgramsMenuFirstTime% = 0
          Future.LoadBMP GraphicsPathSlash$ + ProgramsFramePic, FrameX1%, FrameY1%
          Future.XMSget FrameX1%, FrameY1%, FrameX2%, FrameY2%, ProgramsMenuXMSHandle%
        ELSE
          Future.XMSput FrameX1%, FrameY1%, ProgramsMenuXMSHandle%
        END IF
      FileScroll% = 0
      CurrentDir$ = ShortcutsPath$
      FindNeededFiles
      ShowFiles
    ELSE
      FileScroll% = 0
      CurrentDir$ = DesktopPath$
      FindNeededFiles
      Future.XMSput FrameX1%, FrameY1%, DesktopXMSHandle%
    END IF


'----------------------------------------------------------------------------
  ELSEIF ButNum% = 2 AND MenuState% < 2 THEN       'Edit button
  Future.XMSput 380, MaxY% - 26, ButXMSHandle%

  SELECT CASE MenuState%
  CASE 0
    NewCurDir$ = DesktopPath$
  CASE 1
    NewCurDir$ = ShortcutsPath$
  END SELECT

  File1% = FREEFILE
  File1$ = WindosPathSlash$ + MainINI
  OPEN File1$ FOR INPUT AS #File1%
 
  File2% = FREEFILE
  File2$ = WindosPathSlash$ + MainTMP
  OPEN File2$ FOR OUTPUT AS #File2%

     DO UNTIL EOF(File1%)
     LINE INPUT #File1%, junk$
     junk$ = LTRIM$(RTRIM$(junk$))
       IF UCASE$(LEFT$(junk$, 10)) <> "CURRENTDIR" THEN
       PRINT #File2%, junk$
       END IF
     LOOP

  PRINT #File2%, "CurrentDir = " + NewCurDir$

  CLOSE #File1%, #File2%

  KILL File1$
  NAME File2$ AS File1$

  FullMemEXE Disktop, WindosPath$

'----------------------------------------------------------------------------
  ELSEIF ButNum% = 3 THEN                          'Home button
  Future.LINE FrameX1% + 352, FrameY1% + 94, FrameX1% + 404, FrameY1% + 94, RGB2Color&(255, 255, 255), -1
  Future.LINE FrameX1% + 405, FrameY1% + 94, FrameX1% + 405, FrameY1% + 133, RGB2Color&(0, 0, 0), -1
  Future.LINE FrameX1% + 405, FrameY1% + 133, FrameX1% + 353, FrameY1% + 133, RGB2Color&(0, 0, 0), -1
  Future.LINE FrameX1% + 352, FrameY1% + 133, FrameX1% + 352, FrameY1% + 94, RGB2Color&(255, 255, 255), -1

  FileScroll% = 0
  CurrentDir$ = ShortcutsPath$
  FindNeededFiles
  ShowFiles

'----------------------------------------------------------------------------
  ELSEIF ButNum% = 4 THEN                          'Up Dir button
  Future.LINE FrameX1% + 434, FrameY1% + 94, FrameX1% + 486, FrameY1% + 94, RGB2Color&(255, 255, 255), -1
  Future.LINE FrameX1% + 487, FrameY1% + 94, FrameX1% + 487, FrameY1% + 133, RGB2Color&(0, 0, 0), -1
  Future.LINE FrameX1% + 487, FrameY1% + 133, FrameX1% + 435, FrameY1% + 133, RGB2Color&(0, 0, 0), -1
  Future.LINE FrameX1% + 434, FrameY1% + 133, FrameX1% + 434, FrameY1% + 94, RGB2Color&(255, 255, 255), -1
   
    IF LEN(ShortcutsPath$) < LEN(CurrentDir$) AND LEN(CurrentDir$) > 3 THEN
     
      FOR Counter% = LEN(CurrentDir$) - 1 TO 1 STEP -1
      Character$ = MID$(CurrentDir$, Counter%, 1)
        IF Character$ = "\" THEN EXIT FOR
      NEXT Counter%
   
    CurrentDir$ = LEFT$(CurrentDir$, Counter% - 1)
      IF LEN(CurrentDir$) = 2 THEN
      CurrentDir$ = CurrentDir$ + "\"
      END IF

    FileScroll% = 0
    FindNeededFiles
    ShowFiles
   
    END IF

'----------------------------------------------------------------------------
  END IF

ButNum% = 0

END SUB

FUNCTION CenterText% (XCoord1%, XCoord2%, Length%)

CenterText% = (XCoord2% - XCoord1% - Length% * 8) \ 2 + XCoord1%

END FUNCTION

SUB DosScr (Save%)

DEF SEG = &HB800


  SELECT CASE Save%

  CASE 1
  BSAVE TempPathSlash$ + DosScrFile, 0, &HFA0

  CASE ELSE
  ResetScreen
  BLOAD TempPathSlash$ + DosScrFile

  END SELECT


DEF SEG

END SUB

SUB EmptyKeyb

DEF SEG = 0
POKE &H41A, PEEK(&H41C)
DEF SEG

END SUB

SUB ExtractFileExt (File$, Extension$)

DotPos% = INSTR(File$, ".")

  IF DotPos% THEN
    Extension$ = MID$(File$, DotPos% + 1)
    File$ = LEFT$(File$, DotPos% - 1)
  ELSE
    Extension$ = ""
  END IF

END SUB

SUB FindNeededFiles

  SELECT CASE MenuState%
  CASE 0
    Filter% = 32
  CASE 1
    Filter% = 48
  END SELECT

ERASE Files$
ERASE Directories$

FileCount% = 0
DirCount% = 0
Mask$ = AddSlash$(CurrentDir$) + "*.*"
 
 
  IF ReadDir(Mask$, Filter%, FindFirst) THEN
    DO
      IF ASC(DTA.Attributes) AND 16 THEN               'This is a directory.
          IF RTRIM$(DTA.Filename) <> "." AND RTRIM$(DTA.Filename) <> ".." AND DirCount% <= MaxDirNum THEN
          Directories$(DirCount%) = DTA.Filename
          DirCount% = DirCount% + 1
          END IF
      ELSE                                             'This is a file.
          IF FileCount% <= MaxFileNum THEN
          Files$(FileCount%) = DTA.Filename
          FileCount% = FileCount% + 1
          END IF
      END IF
      IF NOT (ReadDir(Mask$, Filter%, FindNext)) THEN
      EXIT DO
      END IF
    LOOP
  END IF


ObjNum% = DirCount% + FileCount%

END SUB

SUB FullMemEXE (Program$, PrgDir$)

Extension$ = UCASE$(RIGHT$(Program$, 3))
  IF Extension$ <> "EXE" THEN
    IF Extension$ <> "COM" THEN
      IF Extension$ <> "BAT" THEN
      EXIT SUB
      END IF
    END IF
  END IF


File% = FREEFILE
OPEN TempPathSlash$ + Batch FOR OUTPUT AS File%

PRINT #File%, PrgDir$
PRINT #File%, Program$
PRINT #File%, ""                'Command line.
PRINT #File%, 0                 'Get back without a pause.
PRINT #File%, 0                 'No graphics mode.

CLOSE #File%


SaveCfg 2
BeforeFinish
END

END SUB

SUB Interface

SELECT CASE Resolution$
  CASE "1600X1200"
    MaxX% = 1600
    MaxY% = 1200
    Set1600x1200 16
  CASE "1280X1024"
    MaxX% = 1280
    MaxY% = 1024
    Set1280x1024 16
  CASE "1024X768"
    MaxX% = 1024
    MaxY% = 768
    Set1024x768 16
  CASE "800X600"
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

FrameX1% = (MaxX% - 522) \ 2         'Main menu's coordinates.
FrameY1% = (MaxY% - 322) \ 2
FrameX2% = (MaxX% - 522) \ 2 + 522
FrameY2% = (MaxY% - 322) \ 2 + 322
'x1=59 in 640x480
'y1=79

StepX% = 80
StepY% = 76
StartX% = 15
StartY% = MaxY% - 30 - StepY%
MaxFilesPerLine% = (MaxX% - StartX% + (43 - StartX%)) \ StepX%
LastPointX% = MaxFilesPerLine% * StepX% + StartX% - (43 - StartX%)
MaxFilesPerColumn% = (MaxY% - 30 - 86) \ StepY%
LastPointY% = MaxY% - 30 - MaxFilesPerColumn% * StepY%
 
  IF LEN(Background$) THEN
    IF INSTR(Background$, "\") THEN
      Future.LoadBMP Background$, 0, 0
    ELSE
      Future.LoadBMP GraphicsPathSlash$ + Background$, 0, 0
    END IF
  END IF

'Bottom bar
Future.FillBox 0, MaxY% - 30, MaxX% - 1, MaxY% - 1, RGB2Color&(200, 200, 200)
Future.Box 1, MaxY% - 29, MaxX% - 2, MaxY% - 2, RGB2Color&(250, 250, 250)

'Double separator
Future.LINE MaxX% - 121, MaxY% - 25, MaxX% - 121, MaxY% - 6, RGB2Color&(0, 0, 0), -1
Future.LINE MaxX% - 120, MaxY% - 25, MaxX% - 120, MaxY% - 6, RGB2Color&(250, 250, 250), -1

'Bottom bar buttons
Future.LoadBMP GraphicsPathSlash$ + LongButPic, 5, MaxY% - 26
Future.Print 143, MaxY% - 22, "Program Menu", RGB2Color&(0, 255, 0), -1
Future.LoadBMP GraphicsPathSlash$ + NormalButPic, 380, MaxY% - 26
Future.Print 425, MaxY% - 22, "Edit", RGB2Color&(0, 255, 0), -1

'Default icons
Future.LoadBMP GraphicsPathSlash$ + "disktop.bmp", 15, 10
Future.Print 6, 50, "DiskTop", RGB2Color&(255, 240, 0), 0
Future.LoadBMP GraphicsPathSlash$ + "options.bmp", 95, 10
Future.Print 86, 50, "Options", RGB2Color&(255, 240, 0), 0
Future.LoadBMP GraphicsPathSlash$ + "icons.bmp", 175, 10
Future.Print 174, 50, "Icons", RGB2Color&(255, 240, 0), 0
Future.LoadBMP GraphicsPathSlash$ + "dosshell.bmp", 255, 10
Future.Print 254, 50, "Shell", RGB2Color&(255, 240, 0), 0
Temp% = (MaxFilesPerLine% - 2) * StepX% + StartX%
Future.LoadBMP GraphicsPathSlash$ + "help.bmp", Temp%, 10
Future.Print Temp% + 3, 50, "Help", RGB2Color&(255, 240, 0), 0
Temp% = Temp% + StepX%
Future.LoadBMP GraphicsPathSlash$ + "exit.bmp", Temp%, 10
Future.Print Temp% + 3, 50, "Exit", RGB2Color&(255, 240, 0), 0


CMDLine$ = ""
MenuState% = 0
FileScroll% = 0
CurrentDir$ = DesktopPath$

FindNeededFiles
ShowFiles

SetLocation MaxX% \ 2, MaxY% \ 2
SetMouseRange 0, 0, MaxX% - 1, MaxY% - 1
MouseSpeed XmsSpd%, YmsSpd%

END SUB

SUB MouseSpeed (xspeed%, yspeed%)

Regs.ax = 15
Regs.cx = xspeed%
Regs.dx = yspeed%
CALL INTERRUPTX(&H33, Regs, Regs)

END SUB

FUNCTION ReadDir% (Mask$, Filter%, SearchType%)
 
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
DirSpec$ = Mask$ + CHR$(0)
Regs.ax = SearchType%
Regs.cx = Filter%
Regs.ds = VARSEG(DirSpec$)
Regs.dx = SADD(DirSpec$)
CALL INTERRUPTX(&H21, Regs, Regs)

  IF Regs.flags AND 1 THEN
    ReadDir% = 0
  ELSE
    NullByte% = INSTR(DTA.Filename, CHR$(0))
      IF NullByte% > 0 THEN
      DTA.Filename = LEFT$(DTA.Filename, NullByte% - 1)
      END IF
    ReadDir% = -1
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
      IF ReadDir(DesktopPathSlash$ + BatchFile$, 32, FindFirst) AND ReadDir(IconsPathSlash$ + IconFile$, 32, FindFirst) THEN
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

File% = FREEFILE
OPEN MainINI FOR INPUT AS #File%


  DO UNTIL EOF(File%)

  LINE INPUT #File%, Something$
  Something$ = UCASE$(LTRIM$(RTRIM$(Something$)))
  Equal% = INSTR(Something$, "=")

    IF Equal% > 1 THEN
    Value$ = LTRIM$(MID$(Something$, Equal% + 1))
    Value! = VAL(Value$)
    Value% = Value!
    Something$ = RTRIM$(LEFT$(Something$, Equal% - 1))
    END IF

'----------------------------------------------------------------------------
    IF Something$ = "SCREENSAVER" THEN
    ScreenSaver$ = Value$
    
    ELSEIF Something$ = "DELAY" THEN
    ScrDelay% = Value%
   
    ELSEIF Something$ = "BACKGROUND" THEN
    Background$ = Value$

    ELSEIF Something$ = "RESOLUTION" THEN
    Resolution$ = Value$

    ELSEIF Something$ = "DEFAULTSHORTCUT" THEN
    DefShortcut$ = Value$

'----------------------------------------------------------------------------
    ELSEIF Something$ = "DEFAULTDRIVE" THEN
    DefaultDrive$ = Value$
   
    ELSEIF Something$ = "WINDOS" THEN
    WindosPath$ = Value$
    WindosPathSlash$ = AddSlash$(WindosPath$)

    ELSEIF Something$ = "GRAPHICS" THEN
    GraphicsPathSlash$ = AddSlash$(Value$)
  
    ELSEIF Something$ = "SCRSAVERS" THEN
    ScrPath$ = Value$
  
    ELSEIF Something$ = "TEMP" THEN
    TempPathSlash$ = AddSlash$(Value$)
  
    ELSEIF Something$ = "ICONS" THEN
    IconsPathSlash$ = AddSlash$(Value$)
   
    ELSEIF Something$ = "SHORTCUTS" THEN
    ShortcutsPath$ = Value$

    ELSEIF Something$ = "DESKTOP" THEN
    DesktopPath$ = Value$
    DesktopPathSlash$ = AddSlash$(DesktopPath$)

'----------------------------------------------------------------------------
    ELSEIF Something$ = "XMOUSE" THEN
    XmsSpd% = Value%

    ELSEIF Something$ = "YMOUSE" THEN
    YmsSpd% = Value%
  
    ELSEIF Something$ = "DOUBLECLICK" THEN
    DblClickDelay! = Value!

'----------------------------------------------------------------------------
    END IF
  LOOP

CLOSE #File%

END SUB

SUB SaveCfg (ExpCall%)

File1$ = WindosPathSlash$ + MainINI
File1% = FREEFILE
OPEN File1$ FOR INPUT AS File1%

File2$ = WindosPathSlash$ + MainTMP
File2% = FREEFILE
OPEN File2$ FOR OUTPUT AS File2%


  DO UNTIL EOF(File1%)
  LINE INPUT #File1%, Something$
  Temp% = INSTR(Something$, "=")

    IF Temp% THEN
    Temp$ = LCASE$(RTRIM$(LTRIM$(LEFT$(Something$, Temp% - 1))))
    END IF

    IF Temp$ <> "expcall" AND Temp$ <> "batch" AND Temp$ <> "rundesktop" THEN
    PRINT #File2%, Something$
    END IF

  LOOP


PRINT #File2%, "ExpCall =" + STR$(ExpCall%)
 
  IF ExpCall% THEN
  PRINT #File2%, "Batch = " + TempPathSlash$ + Batch
  'PRINT #File2%, "RunDesktop"
  END IF

CLOSE #File1%, #File2%

KILL File1$
NAME File2$ AS File1$

END SUB

SUB ShowFiles

SELECT CASE MenuState%

CASE 0

CurrentX% = StartX%
CurrentY% = StartY%

 
  FOR Counter% = 0 TO MaxFilesPerLine% * MaxFilesPerColumn% - 1
   
    IF Counter% > MaxFileNum THEN
      EXIT SUB
    ELSE
      IF Files$(Counter%) = "" THEN
      EXIT SUB
      END IF
    END IF
 
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

  Future.LoadBMP IconsPathSlash$ + Shortcut$, CurrentX%, CurrentY%
 
  FileWithoutExtension$ = Files$(Counter%)
  ExtractFileExt FileWithoutExtension$, Temp$
 
  Future.Print CenterText(CurrentX% - 20, CurrentX% + 57, LEN(FileWithoutExtension$)), CurrentY% + 40, FileWithoutExtension$, RGB2Color&(0, 255, 0), 0
 
  CurrentX% = CurrentX% + StepX%
    IF CurrentX% > LastPointX% THEN
    CurrentX% = StartX%
    CurrentY% = CurrentY% - StepY%
    END IF
  NEXT


'----------------------------------------------------------------------------
CASE 1
                                     
Future.FillBox FrameX1% + 44, FrameY1% + 66, FrameX1% + 271, FrameY1% + 298, RGB2Color&(100, 100, 100)


  FOR Counter% = FileScroll% TO FileScroll% + MaxMenuFiles - 1
   
   
    IF Counter% < DirCount% THEN           'Directory
     
      Future.Print FrameX1% + 50, FrameY1% + 70 + ObjShown% * 16, Directories$(Counter%) + " <DIRECTORY>", RGB2Color&(255, 255, 0), -1
   
    ELSE                                   'File
       
        IF Counter% - DirCount% > MaxFileNum THEN
          EXIT SUB
        ELSE
          IF LEN(Files$(Counter% - DirCount%)) = 0 THEN
          EXIT SUB
          END IF
        END IF
       
      TempExt$ = UCASE$(RIGHT$(RTRIM$(Files$(Counter% - DirCount%)), 3))
      Text$ = UCASE$(Files$(Counter% - DirCount%))
        IF TempExt$ = "BAT" THEN
          Text$ = Text$ + "  <SHORTCUT>"
        ELSEIF TempExt$ = "BMP" THEN
          Text$ = Text$ + "    <BITMAP>"
        END IF
      Future.Print FrameX1% + 50, FrameY1% + 70 + ObjShown% * 16, Text$, RGB2Color&(0, 255, 0), -1
    END IF
 
  ObjShown% = ObjShown% + 1
  NEXT


'----------------------------------------------------------------------------

END SELECT

END SUB

SUB WhatFile (Number%, File$, Extension$, Directory%)


  IF Number% <= DirCount% THEN

    File$ = Directories$(Number% - 1)          'This is a directory !!!
    Extension$ = ""
    Directory% = 1

  ELSE

    File$ = Files$(Number% - DirCount% - 1)    'This is a file !!!
    ExtractFileExt File$, Extension$
    Extension$ = RTRIM$(Extension$)
    Directory% = 0

  END IF


END SUB

