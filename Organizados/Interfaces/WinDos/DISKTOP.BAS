'Copyright (C) HiTech Labs, Denis Andrianov.
'WinDos Disktop
'============================================================================
DEFINT A-Z

'$INCLUDE: 'mouse.bi'
'$INCLUDE: 'fileio.bi'
'$INCLUDE: 'graphics.bi'
'$INCLUDE: 'reg.bi'
'$INCLUDE: 'dos.bi'

'WinDos library declarations.
DECLARE FUNCTION Execute% (BYVAL PathSeg%, BYVAL PathOff%, BYVAL CommandSeg%, BYVAL CommandOff%)
DECLARE SUB Get12 (BYVAL Seg1%, BYVAL Seg2%, BYVAL Seg3%, BYVAL Seg4%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%)
DECLARE SUB Put12 (BYVAL Seg1%, BYVAL Seg2%, BYVAL Seg3%, BYVAL Seg4%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%)
'For best results:
'x1% = 8n             n >= 0
'x2% = 8m - 1         m >= 1


'Disktop declarations.
DECLARE SUB ButtonsPress (BYVAL x%, BYVAL y%, ButNum%)
DECLARE SUB ButtonsRelease (ButNum%)
DECLARE SUB ButtonsExecute (BYVAL ItemNum%)

DECLARE SUB PullMenuShow (BYVAL Active%, BYVAL OldActive%, BYVAL xOldActive%, BYVAL ShowAll%, BYVAL Part%)
DECLARE SUB PullMenuPress ()
DECLARE SUB PullMenuExecute (BYVAL Menu%, BYVAL ItemNum%)

DECLARE SUB ShowDrives ()
DECLARE SUB DrvButtonPress (BYVAL x%, BYVAL y%, DrvPrs%)

DECLARE SUB DirList ()

DECLARE SUB ShowFiles (BYVAL ScrollType%)
DECLARE SUB SelectObject (File$, Extension$, BYVAL Directory%, BYVAL xFile%, BYVAL yFile%, BYVAL Sel%)
DECLARE FUNCTION FirstSelObject% (File$, Extension$, Directory%)
DECLARE FUNCTION NumOfClipObjects% ()
DECLARE SUB GetPutScreen (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL PutImage%)

DECLARE SUB ReadINI ()
DECLARE FUNCTION ZipRead% (File$, ZipDir$)
DECLARE SUB FindNeededFiles (BYVAL ShowFl%, BYVAL Remember%)
DECLARE SUB LoadFont (BYVAL FontType%, Text$, BYVAL Textx%, BYVAL Texty%, BYVAL Colour%)
DECLARE SUB SaveCfg (BYVAL ExpCall%)
DECLARE FUNCTION UnZipFile% (Filename$, Destination$)
DECLARE FUNCTION ZipExists% ()

DECLARE SUB WhatFile (BYVAL Number%, File$, Extension$, Directory%)
DECLARE SUB Delay (BYVAL Time%)       'time%=67 (1 second)
DECLARE SUB EmptyKeyb ()

DECLARE SUB RunItem (File$, Extension$, BYVAL Directory%, BYVAL ProgramNum%, BYVAL PressKey%)
DECLARE SUB QuickEXE (Program$, PrgDir$, SwitchTo$, BYVAL PressKey%, BYVAL ApplyText%)
DECLARE SUB FullMemEXE (Program$, PrgDir$, SwitchTo$, BYVAL PressKey%, BYVAL GFXMode%, BYVAL LoadCommand%)
DECLARE SUB RunSelEXE (BYVAL Quick%, BYVAL PressKey%)
DECLARE SUB PrintFileSize (BYVAL Number%, BYVAL Directory%, BYVAL Sel%, BYVAL ResetSize%)
DECLARE SUB WDChangeDir (Directory$, BYVAL Directory%)
DECLARE SUB RememberDir (Directory$)
DECLARE SUB ExtractFileExt (File$, Extension$)
DECLARE FUNCTION CMDLineOk% ()
DECLARE SUB SetupCopyObj ()
DECLARE FUNCTION AddSlash$ (Path$)
DECLARE FUNCTION ExtractPrgmDir% (Program$, Path$)
DECLARE SUB Sort (BYVAL SortType%)
'SortType%=-1  Sort directories only in alpha order.
'SortType%=0   Sort based on file names.
'SortType%=1   Sort based on file extensions.
'SortType%=2   Sort based on file sizes.

DECLARE SUB MouseDefaults ()

DECLARE SUB Interface ()
DECLARE SUB Finish ()


CONST id = "Copyright (C) HiTech Labs, Denis Andrianov"
CONST Version = "WinDOS 270 Disktop"
CONST Date = "8 January, 2005"
CONST MainFNT = "windos.fnt"        'Main font.
CONST System1 = "autoexec.bat"      'System files.
CONST System2 = "config.sys"
CONST ReadMe = "readme.txt"         'ReadMe document.
CONST RegEgFile = "extini.txt"      'Registry example file.
CONST VerHisFile = "history.txt"    'WinDos version history file.
CONST Desktop = "desktop.exe"       'Desktop program.
CONST About = "hitech.exe"          'About WinDos program.
CONST RunnerTmpFile = "runprg.tmp"  'Info file for RUNNER.COM.
CONST BatchFile = "user.bat"        'Batch file for exec programs.
CONST MainINI = "windos.ini"        'Name of the main "ini" file.
CONST MainTMP = "windos.tmp"        'Used when rewriting MainINI.
CONST ExtINI = "ext.ini"            'Extensions.
CONST QuickINI = "quick.ini"        'Quick menu programs/directories.
CONST HistoryINI = "history.ini"    'List of n last visited directories.
CONST MenuFile = "pullmenu."        'Pulldown menu text files.
CONST StatusBarFile = "status.bin"
CONST ToolBarFile = "tools.bin"
CONST DriveBarFile = "drives.bin"
CONST ExtEdit = "ee.exe"            'Extension Editor.
CONST CompTool = "compress.exe"     'Compression/decompression program.
CONST CompList = "c"                'Compression list file.
CONST Options = "options.exe"       'Configuration program.
CONST PrintText = "print.exe"       'Printing utility program.
CONST Property = "property.exe"     'Property utility program.
CONST PropertyList = "property.lst" 'Property file/directory listing.
CONST RegDirID = "\\\"              'Ext used to indicate a dir in registry.
CONST XFileSize = 330
CONST YFileSize = 458               'Y position of PrintFileSize text.
CONST Tick = "f4e9l1g9h4"
CONST FindFirst = &H4E00        'Functions of interrupt 21h.
CONST FindNext = &H4F00
CONST FilesFull = 2000         '1000
CONST DirectoriesFull = 500    '300
DIM SHARED Files$(1 TO FilesFull)              'File array.
DIM SHARED Extensions$(1 TO FilesFull)         'Extensions array.
DIM SHARED Directories$(1 TO DirectoriesFull)  'Directory array.
DIM SHARED FileSizes&(1 TO FilesFull)          'File sizes.
DIM SHARED SelObj%(1 TO 2500)                  'Selected objects' array.
DIM SHARED CopyObj$(1 TO 2500)                 'Objects to copy.
DIM SHARED CopyObjIndex%(1 TO 2500)            'Indicates whether an object is a
'0=File                                        'file or directory.
'1=Directory
DIM SHARED Drives$              'All available drives.
DIM SHARED CurrentDir$          'Current directory.
DIM SHARED TMPPath$             'Temp dir written to MainINI.
CONST MaxMemDir = 50            'Maximum number of remembered directories.
DIM SHARED MemDir$(MaxMemDir)   'Directory array.
DIM SHARED PosMemDir%           'Current position in MemDir$ array.
DIM SHARED MemIndex%            'MEM button index/switch.
DIM SHARED FileScroll%          'Current scrolling file.
DIM SHARED FileNum%             'Number of files+directories.
DIM SHARED DirCount%            'Number of directories.
DIM SHARED FileCount%           'Number of files.
DIM SHARED Filter%              'Filter for READDIR.
'(0000 0001)=Read-Only
'(0000 0010)=Hidden
'(0000 0100)=System
'(0001 0000)=Sub-directory
'(0010 0000)=Archive
CONST ReadOnlyAtt = 1           'File attributes.
CONST HiddenAtt = 2
CONST SystemAtt = 4
CONST DirectoryAtt = 16
CONST ArchiveAtt = 32
CONST AllFilesAtt = 55
DIM SHARED SourceDirCopy$       'Dir from which files are copied.
DIM SHARED CutCopy%
'0=do not cut or copy
'1=cut
'2=copy
DIM SHARED FileSum&             'Sum of file sizes.
DIM SHARED CMDLine$             'Command Line.
DIM SHARED KeysFile$            'File with comb. of keys for passing to prgs.
DIM SHARED AnyKey%              'Show "Prs any key to..." message?
DIM SHARED UseCommand%          'Use command.com to run programs?
DIM SHARED FlushBuffers%        'Flush buffers before running progs?
DIM SHARED DblClickDelay!       'Delay between two clicks.
DIM SHARED WindosFont$(255)     'Main font.
DIM SHARED Editor$              'WinDos's default file editor.
DIM SHARED ScreenSaver$         'ScreenSaver filename.
DIM SHARED ScrDelay%            'ScreenSaver delay.
DIM SHARED PalFile$             'Disktop colour set file.
DIM SHARED Something$           'Temp string variable.
DIM SHARED ChrZero$             'CHR$(0).
DIM SHARED FileSort%            'Sorts files.
'0=by Name
'1=by Extension
'2=by Size
'3=by Data/Time
DIM SHARED ViewType%            'List or Large view.
'0=Large
'1=List


CONST MouseDefX = 320           'Default mouse position.
CONST MouseDefY = 240


CONST AbortZipReadKey$ = ""                 'ESC
CONST ZipComp = "PKZIP.EXE"                  'ZIP compression program.
CONST ZipDecomp = "PKUNZIP.EXE"              'ZIP decompression program.
DIM SHARED ZipFile AS ZipCentralFileHeader
DIM SHARED ZipFileEnd AS ZipCentralDirEndHeader
DIM SHARED ActiveZipFile$                    'Current open ZIP file.
DIM SHARED LenActiveZipFile%                 'Length of the above string.
DIM SHARED CurrentZipDir$                    'Current dir inside a ZIP file.
DIM SHARED SourceZipFile$                    'Source ZIP file for copying.
DIM SHARED SourceZipDir$                     'Stored ZIP dir for copying.
DIM SHARED CopyFromZip%                      'Flag indicator.
DIM SHARED PkZip0$                           'Full path to archivers
DIM SHARED PkUnzip0$                         '                     + CHR$(0).
DIM SHARED ZipListFile$                      'Full path to ZIP list file.
DIM SHARED ZipListFile0$                     '" @" + ZipListFile$ + CHR$(0).


DIM SHARED WindosPath$          'WinDos's working path.
DIM SHARED ArchivesPath$        'Path to directory with archive tools.
DIM SHARED ScrPath$             'Path to ScreenSaver directory.
DIM SHARED KeysPath$            'Path to combination key files.
DIM SHARED PalPath$             'Path to palette files.

COMMON SHARED /MouseSubs/ XmsSpd%            'Speed x of a mouse.
COMMON SHARED /MouseSubs/ YmsSpd%            'Speed y of a mouse.

COMMON SHARED /FileIOSubs/ TempPath$           'Path to Temp directory.
COMMON SHARED /FileIOSubs/ DefaultDrive$       'Default drive.
COMMON SHARED /FileIOSubs/ DTA AS DTAStructure

COMMON SHARED /GraphicsSubs/ GraphicsPath$     'Path to GRAPHICS directory.
COMMON SHARED /GraphicsSubs/ GraphicsPathSlash$
COMMON SHARED /GraphicsSubs/ FontsPath$        'Path to Fonts directory.
COMMON SHARED /GraphicsSubs/ Word$             'InputLine's input string.
COMMON SHARED /GraphicsSubs/ NewName$          'Temp input string.
COMMON SHARED /GraphicsSubs/ ScrArray1() AS INTEGER

DIM SHARED Regs AS RegTypeX

CONST MenuOpen = 5

DIM SHARED PullMenuX1%
CONST PullMenuY1 = 20
DIM SHARED PullMenuX2%
DIM SHARED PullMenuY2%
CONST PullMenuNum = 7                        'Total number of menus.
CONST PullMenuItems = 29                     'Max number of pull menu items.
CONST PullMenuWidth = 180                    'Width of each pull menu.
CONST PullMenuTopStep = 70                   'Distance between the headings.
CONST MaxNumOfCharsPerPullMenu = 21          'Max number of letters per menu.
REDIM SHARED MenuText$(1 TO PullMenuNum, PullMenuItems) 'Menu text data.
REDIM SHARED MenuItems%(1 TO PullMenuNum)'Number of text lines for each menu.

REDIM SHARED ScrArray1(7953) AS INTEGER      'Arrays for storing
REDIM SHARED ScrArray2(7953) AS INTEGER      'portions of screen.
REDIM SHARED ScrArray3(7953) AS INTEGER
REDIM SHARED ScrArray4(7953) AS INTEGER


REDIM SHARED FileSizeBackRestore(969) AS INTEGER     'To restore the
                                                     'background behind the
                                                     'displayed file size.

'The following are the codes for key combinations corresponding to ALT+A,
'ALT+B, ALT+C, ALT+D, ALT+E, ALT+F, etc.

DriveKeys$ = CHR$(30) + CHR$(48) + CHR$(46) + CHR$(32) + CHR$(18) + CHR$(33) + CHR$(34) + CHR$(35) + CHR$(23) + CHR$(36)
DriveKeys$ = DriveKeys$ + CHR$(37) + CHR$(38) + CHR$(50) + CHR$(49) + CHR$(24) + CHR$(25) + CHR$(16) + CHR$(19) + CHR$(31)
DriveKeys$ = DriveKeys$ + CHR$(20) + CHR$(22) + CHR$(47) + CHR$(17) + CHR$(45) + CHR$(21) + CHR$(44)


'============================================================================
'Defaults here.

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


ChrZero$ = CHR$(0)

'----------------------------------------------------------------------------
'Checking for mouse...

  IF NOT MouseInit% THEN
  PRINT "Mouse driver not present."
  END
  END IF

'----------------------------------------------------------------------------
'Reading configuration...

ReadINI
  IF MID$(WindosPath$, 2, 1) <> ":" THEN
  PRINT "WinDos path incorrect."
  END
  END IF

'----------------------------------------------------------------------------
'Init variables for use with ZIP...

Something$ = AddSlash$(ArchivesPath$)
PkZip0$ = Something$ + ZipComp + ChrZero$
PkUnzip0$ = Something$ + ZipDecomp + ChrZero$
ZipListFile$ = AddSlash$(TempPath$) + CompList
ZipListFile0$ = " @" + ZipListFile$ + ChrZero$

'----------------------------------------------------------------------------
'Loading main font...

File% = FREEFILE
OPEN AddSlash$(FontsPath$) + MainFNT FOR INPUT AS #File%
  FOR Counter% = 1 TO 255
  LINE INPUT #File%, WindosFont$(Counter%)
  NEXT
CLOSE #File%

'----------------------------------------------------------------------------
'Loading history...

OPEN HistoryINI FOR INPUT AS #File%
  FOR Counter% = 0 TO MaxMemDir
  LINE INPUT #File%, MemDir$(Counter%)
  NEXT
INPUT #File%, PosMemDir%
INPUT #File%, MemIndex%
CLOSE #File%

'----------------------------------------------------------------------------
'Loading pulldown menus...

  FOR Counter% = 1 TO PullMenuNum
  OPEN MenuFile + LTRIM$(STR$(Counter%)) FOR BINARY AS #File%

  Something$ = " "
  GET #File%, , Something$
  Temp% = ASC(Something$)
  MenuItems%(Counter%) = Temp%

    FOR LineNum% = 0 TO Temp%
    Something$ = " "
    GET #File%, , Something$
    Length% = ASC(Something$)

      IF Length% THEN
        Something$ = SPACE$(Length%)
        GET #File%, , Something$
      ELSE
        Something$ = ""
      END IF

    MenuText$(Counter%, LineNum%) = Something$
    NEXT

  CLOSE #File%
  NEXT

'----------------------------------------------------------------------------
'Loading graphics buttons.

REDIM SHARED LeftArrPrs(385) AS INTEGER
REDIM SHARED LeftArrRls(385) AS INTEGER
REDIM SHARED RightArrPrs(385) AS INTEGER
REDIM SHARED RightArrRls(385) AS INTEGER
REDIM SHARED UpArrPrs(385) AS INTEGER
REDIM SHARED UpArrRls(385) AS INTEGER
REDIM SHARED DownArrPrs(385) AS INTEGER
REDIM SHARED DownArrRls(385) AS INTEGER

Something$ = AddSlash$(GraphicsPath$)

DEF SEG = VARSEG(LeftArrRls(0))
BLOAD Something$ + "LtArrRls.bin", 0

DEF SEG = VARSEG(LeftArrPrs(0))
BLOAD Something$ + "LtArrPrs.bin", 0

DEF SEG = VARSEG(RightArrRls(0))
BLOAD Something$ + "RtArrRls.bin", 0

DEF SEG = VARSEG(RightArrPrs(0))
BLOAD Something$ + "RtArrPrs.bin", 0

DEF SEG = VARSEG(UpArrRls(0))
BLOAD Something$ + "UpArrRls.bin", 0

DEF SEG = VARSEG(UpArrPrs(0))
BLOAD Something$ + "UpArrPrs.bin", 0

DEF SEG = VARSEG(DownArrRls(0))
BLOAD Something$ + "DnArrRls.bin", 0

DEF SEG = VARSEG(DownArrPrs(0))
BLOAD Something$ + "DnArrPrs.bin", 0
DEF SEG

'----------------------------------------------------------------------------
Drives$ = GetDrives$
SaveCfg 0

SCREEN 12
CLS

MouseDefaults

Interface
FindNeededFiles 0, 1 - MemIndex%


'If contents have changed, scroll back.
Temp% = FileNum% - FileScroll%
  IF ViewType% THEN
    IF Temp% < 44 THEN
    FileScroll% = 1
    END IF
  ELSE
    IF Temp% < 15 THEN
    FileScroll% = 1
    END IF
  END IF


ShowFiles 0
MouseShow

CurrentTime$ = TIME$


'============================================================================
'============================================================================


DO                                  'Main Loop

  MouseStatus Lb%, Rb%, x%, y%
  'LOCATE 1, 70: PRINT x%; y%
  'LOCATE 1, 1: PRINT PosMemDir%: PRINT MemDir$(0): PRINT MemDir$(1): PRINT MemDir$(2)

  IF CurrentTime$ <> TIME$ THEN
  CurrentTime$ = TIME$
    IF x% > 545 AND y% > 437 AND x% < 638 AND y% < 478 THEN
      MouseHide
      GOSUB ChangeTime
      MouseShow
    ELSE
      GOSUB ChangeTime
    END IF
  END IF

  IF OldX% <> x% OR OldY% <> y% THEN        'Checking for any mouse movement
  OldX% = x%                                'for the screensaver.
  OldY% = y%
  ScrCount% = 0                             'Reset screensaver counter.
  END IF



KH$ = INKEY$
KeyLength% = LEN(KH$)

  IF (KeyLength% > 0) AND ((Lb% OR Rb%) = 0) THEN
  KeyChar$ = RIGHT$(KH$, 1)
  KeyCode% = ASC(KeyChar$)
  ScrCount% = 0                            'Reset screensaver counter.
  MouseHide

    IF KeyLength% = 1 THEN

      SELECT CASE KeyCode%
        CASE 3                             'CTRL+C (Copy)
          ButtonsRelease 3
        CASE 4                             'CTRL+D (Duplicate)
          PullMenuExecute 1, 9
        CASE 8                             'Up Dir
          ButtonsExecute 11
        CASE 16                            'CTRL+P (Print)
          PullMenuExecute 1, 14
        CASE 18                            'CTRL+R (Rename)
          PullMenuExecute 1, 8
        CASE 22                            'CTRL+V (Paste)
          ButtonsRelease 4
        CASE 24                            'CTRL+X (Cut)
          ButtonsRelease 2
        CASE 27                            'ESC (DOS Screen)
          PullMenuExecute 3, 15
        CASE 42                            '* (Select All)
          PullMenuExecute 2, 7
        CASE 43                            '+ (Select)
          PullMenuExecute 2, 5
        CASE 45                            '- (Invert Selection)
          PullMenuExecute 2, 6
        CASE 47                            '/ (Deselect All)
          PullMenuExecute 2, 8
        CASE 52                            'Left (numeric), Back history
          ButtonsExecute 7
        CASE 54                            'Right (numeric), Forward history
          ButtonsExecute 8
      END SELECT

    ELSE

      SELECT CASE KeyCode%
        CASE 60                            'F2 (Command Line)
          PullMenuExecute 1, 2
        CASE 61                            'F3 (QView)
          ButtonsExecute 12
        CASE 63                            'F5 (Refresh)
          ButtonsExecute 6
        CASE 64                            'F6 (Properties)
          PullMenuExecute 3, 12
        CASE 65                            'F7 (Compress)
          PullMenuExecute 1, 11
        CASE 66                            'F8 (Decompress)
          PullMenuExecute 1, 12
        CASE 67                            'F9 (Quick Execute)
          PullMenuExecute 1, 1
        CASE 68                            'F10 (Exit)
          Finish
        CASE 71                            'Home key
          ShowFiles 255
        CASE 72                            'Up arrow
          ButtonsExecute 13
        CASE 75                            'Left arrow
          ButtonsExecute 15
        CASE 77                            'Right arrow
          ButtonsExecute 16
        CASE 79                            'End key
          ButtonsExecute 18
        CASE 80                            'Down arrow
          ButtonsExecute 14
        CASE 83                            '<DEL>
          PullMenuExecute 1, 7
        CASE ELSE

          Temp% = INSTR(DriveKeys$, KeyChar$)

            IF Temp% THEN
            CurrentDir$ = CHR$(Temp% + 64) + ":\"
            LenActiveZipFile% = 0
            FindNeededFiles -1, 1 - MemIndex%
            PrintFileSize 0, 0, 0, 1
            END IF

      END SELECT

    END IF

  MouseShow
  END IF






 SELECT CASE Rb%
   CASE -1

     GOSUB CalcFile

       IF SelectMethod% = 0 THEN
         IF SelObj%(nFile%) THEN
           SelectMethod% = 1             'Dehighlight
         ELSE
           SelectMethod% = 2             'Highlight
         END IF
       END IF

     GOSUB SelectIt

       IF OldNFile% = nFile% THEN
         RightClick% = 0
       ELSE
         RightClick% = 1
       END IF

   CASE 0
     RightClick% = 1                     'To highlight first-pointed file.
     SelectMethod% = 0
 END SELECT






 SELECT CASE Lb%
   CASE -1
     IF NOT (Pressed%) THEN
     MouseHide
       DrvButtonPress x%, y%, DrvPrs%
       ButtonsPress x%, y%, ButNum%
     MouseShow
     LeftClick% = LeftClick% + 1       'Double click detection
       IF LeftClick% = 1 THEN OldTime! = TIMER
     Pressed% = -1
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
       MouseHide
       Pressed% = 0

         IF DrvPrs% THEN ShowDrives
       PullMenuPress

       ButtonsRelease ButNum%
       MouseShow
       END IF

 END SELECT


LOOP

'================================SUB ROUTINES================================

OpenIt:
  IF DoubleClick% THEN

    IF ViewType% = 0 THEN
      IF x% > 18 AND x% < 538 AND y% > 65 AND y% < 385 THEN
      GOSUB ExecuteFile
      END IF
    ELSE
      IF x% >= 0 AND x% < 550 AND y% > 58 AND y% < 406 THEN
      GOSUB ExecuteFile
      END IF
    END IF

  DoubleClick% = 0
  END IF

RETURN

'----------------------------------------------------------------------------
SelectIt:
  IF RightClick% THEN

    IF ViewType% = 0 THEN
      IF x% > 18 AND x% < 538 AND y% > 65 AND y% < 385 THEN
      GOSUB DisplayFile
      END IF
    ELSE
      IF x% >= 0 AND x% < 550 AND y% > 58 AND y% < 406 THEN
      GOSUB DisplayFile
      END IF
    END IF

  END IF

RETURN

'----------------------------------------------------------------------------
CalcFile:

SELECT CASE ViewType%

  CASE 0
  xFile% = (x% - 18) \ 104 + 1
  yFile% = (y% - 65) \ 80 + 1
  xFilePos% = xFile% * 104 - 64
  yFilePos% = yFile% * 80 + 10
  nFile% = 5 * (yFile% - 1) + xFile% + FileScroll% - 1

  CASE 1
  xFile% = x% \ 112 + 1
  yFile% = (y% - 58) \ 32 + 1
  xFilePos% = (xFile% - 1) * 112 + 8
  yFilePos% = (yFile% - 1) * 32 + 80
  nFile% = 11 * (xFile% - 1) + yFile% + FileScroll% - 1

END SELECT

WhatFile nFile%, File$, Extension$, Directory%
RETURN

'----------------------------------------------------------------------------
DisplayFile:

GOSUB CalcFile
OldNFile% = nFile%

  IF LEN(File$) = 0 THEN RETURN
  IF Directory% = -1 AND File$ = ".." THEN RETURN


  IF SelectMethod% = 2 THEN
      IF SelObj%(nFile%) THEN RETURN
    Temp% = 1
  ELSE
      IF SelObj%(nFile%) = 0 THEN RETURN
    Temp% = 0
  END IF


MouseHide

PrintFileSize nFile% - DirCount%, Directory%, Temp%, 0
SelObj%(nFile%) = Temp%
SelectObject File$, Extension$, Directory%, xFilePos%, yFilePos%, Temp%

MouseShow
RETURN

'----------------------------------------------------------------------------
ExecuteFile:

GOSUB CalcFile

  IF LEN(File$) THEN
  MouseHide
  RunItem File$, Extension$, Directory%, 0, AnyKey%
  MouseShow
  END IF

RETURN

'----------------------------------------------------------------------------
ChangeTime:
LINE (561, 460)-(634, 475), 5, BF
LoadFont 255, CurrentTime$, 574, 458, 14
ScrCount% = ScrCount% + 1
  IF ScrDelay% = ScrCount% THEN
  ScrCount% = 0
    IF LEN(ScreenSaver$) THEN
    FullMemEXE ScreenSaver$, ScrPath$, CurrentDir$, 0, 0, UseCommand%
    END IF
  END IF
RETURN

FUNCTION AddSlash$ (Path$)

  IF RIGHT$(Path$, 1) <> "\" THEN
    AddSlash$ = Path$ + "\"
  ELSE
    AddSlash$ = Path$
  END IF

END FUNCTION

SUB ButtonsExecute (BYVAL ItemNum%)


  IF ItemNum% = 2 OR ItemNum% = 3 THEN   'Cut or copy
  SourceDirCopy$ = CurrentDir$
  SetupCopyObj

    IF LenActiveZipFile% THEN
      SourceZipFile$ = AddSlash$(CurrentDir$) + ActiveZipFile$
      SourceZipDir$ = CurrentZipDir$
      CopyFromZip% = -1
    ELSE
      CopyFromZip% = 0
    END IF

  END IF


'лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
'лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

  IF ItemNum% = 1 THEN                   'Exit
  Finish

'лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

  ELSEIF ItemNum% = 2 THEN               'Cut
  CutCopy% = 1

'лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

  ELSEIF ItemNum% = 3 THEN               'Copy
  CutCopy% = 2

'лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

  ELSEIF ItemNum% = 4 THEN               'Paste


'**************************
'Conduct primitive checks.
'**************************

    IF CutCopy% = 0 THEN EXIT SUB

  ObjNum% = NumOfClipObjects%

    IF ObjNum% = 0 THEN EXIT SUB

    IF CopyFromZip% THEN
      IF LenActiveZipFile% THEN
      InfoBox 6, "Pasting from ZIP to ZIP is not allowed."
      Temp% = CaptureInf%(6)
      ShowFiles 0
      EXIT SUB
      END IF
    END IF


'**************************
'Copying / Moving from ZIP?
'**************************

    IF CopyFromZip% THEN

      IF NOT (ZipExists%) THEN
      ShowFiles 0
      EXIT SUB
      END IF

    CMDLine$ = "-d " + SourceZipFile$ + ZipListFile0$
      IF NOT (CMDLineOk%) THEN
      ShowFiles 0
      EXIT SUB
      END IF


    File% = FREEFILE
    OPEN ZipListFile$ FOR OUTPUT AS #File%

      FOR Counter% = 1 TO FilesFull + DirectoriesFull
      Something$ = CopyObj$(Counter%)

        IF LEN(Something$) THEN

          IF LEN(SourceZipDir$) THEN
          Something$ = SourceZipDir$ + "\" + Something$
          END IF

          IF CopyObjIndex%(Counter%) THEN
          Something$ = Something$ + "\*.*"
          END IF

        PRINT #File%, Something$
        END IF

      NEXT

    CLOSE #File%


    GOSUB PrepareToExec
    Temp% = Execute%(SSEG(PkUnzip0$), SADD(PkUnzip0$), SSEG(CMDLine$), SADD(CMDLine$))
      IF CutCopy% = 1 THEN
      Temp% = Execute%(SSEG(PkZip0$), SADD(PkZip0$), SSEG(CMDLine$), SADD(CMDLine$))
      END IF
    GOSUB RestoreAfterExec

    FindNeededFiles 0, 0
    Interface
    GOTO ExitPaste
    END IF


'**************************
'Copying / Moving to ZIP?
'**************************

    IF LenActiveZipFile% THEN

      IF NOT (ZipExists%) THEN
      ShowFiles 0
      EXIT SUB
      END IF

    CMDLine$ = "-a -ex -Pr " + AddSlash$(CurrentDir$) + ActiveZipFile$ + ZipListFile0$
      IF CutCopy% = 1 THEN CMDLine$ = "-m " + CMDLine$

      IF NOT (CMDLineOk%) THEN
      ShowFiles 0
      EXIT SUB
      END IF


    'Change to source directory.
      IF NOT (ChangeDirDrive%(SourceDirCopy$)) THEN
      InfoBox 6, "Source directory cannot be accessed."
      Temp% = CaptureInf%(6)
      LenActiveZipFile% = 0
      FindNeededFiles -1, 0
      GOTO ExitPaste
      END IF


    File% = FREEFILE
    OPEN ZipListFile$ FOR OUTPUT AS #File%

      FOR Counter% = 1 TO FilesFull + DirectoriesFull
      Something$ = CopyObj$(Counter%)

        IF LEN(Something$) THEN
          IF CopyObjIndex%(Counter%) THEN
          Something$ = Something$ + "\*.*"
          END IF
        PRINT #File%, Something$
        END IF

      NEXT

    CLOSE #File%



    GOSUB PrepareToExec
    Temp% = Execute%(SSEG(PkZip0$), SADD(PkZip0$), SSEG(CMDLine$), SADD(CMDLine$))
    GOSUB RestoreAfterExec

    'Change back to current directory.
      IF NOT (ChangeDirDrive%(CurrentDir$)) THEN
      LenActiveZipFile% = 0
      END IF

    FindNeededFiles 0, 0
    Interface
    GOTO ExitPaste
    END IF


'******************************************
'Perform more checks, initialize variables.
'******************************************

  SaveSourcePath$ = SourceDirCopy$

    IF SaveSourcePath$ = CurrentDir$ THEN
    InfoBox 6, "Can not copy files onto themselves!"
    Temp% = CaptureInf%(6)
    ShowFiles 0
    EXIT SUB
    END IF

  CopySuccess% = -1                        'Assume it is a success.
  TempViewType% = ViewType%
  ViewType% = 0
  GStep! = 300 / ObjNum%                   'Progress bar step.
  CurX! = 170

  GOSUB RefreshBox3


'***************************************
'Main loop starts here.
'***************************************

    FOR Counter% = 1 TO FilesFull + DirectoriesFull
    File$ = CopyObj$(Counter%)
    FileWithExt$ = File$                   'Save for error messages.

      IF LEN(File$) THEN
      CurX! = CurX! + GStep!
      Button 170, 275, CurX!, 295, 8, 0, 2, 0, 2

      SourcePath$ = AddSlash$(SaveSourcePath$) + File$
      DestinationPath$ = AddSlash$(CurrentDir$) + File$

      Directory% = CopyObjIndex%(Counter%)

        IF Directory% = 0 THEN
        ExtractFileExt File$, Extension$
        END IF

      SelectObject File$, Extension$, Directory%, 290, 172, 0


'***************************************
'Check if files with the same name exist
'in destination directory.
'***************************************

        IF ReadDir%(DestinationPath$, FindFirst, AllFilesAtt) THEN

          IF Overwrite% <> 1 THEN              'If All don't go here again.
          InfoBox 7, FileWithExt$ + " exists. Overwrite?"
          Overwrite% = CaptureInf%(7)
          GOSUB RefreshBox3
          END IF

          IF Overwrite% <> 3 THEN              'All or Yes
            SELECT CASE KillFileDir%(File$, Extension$, CurrentDir$, Directory%)
            CASE 0
              CopySuccess% = 0
              GOTO CopyErrorMsg
            CASE 1
              GOSUB RefreshBox3
            END SELECT
          END IF

        END IF



'************************************************
'Copy files and directories and check for errors.
'************************************************

        IF Overwrite% <> 3 THEN
            IF CutCopy% = 1 THEN


'***************************
'Move files / directories.
'***************************

              CopySuccess% = MoveFileDir%(SourcePath$, DestinationPath$, Directory%)
                IF CopySuccess% = 1 THEN
                GOSUB RefreshBox3
                CopySuccess% = -1
                END IF


'***************************
'Copy files / directories.
'***************************
            ELSE


              IF Directory% THEN
                CopySuccess% = CopyDir%(SourcePath$, DestinationPath$)
              ELSE
                CopySuccess% = CopyFile%(SourcePath$, DestinationPath$)
              END IF


            END IF
        ELSE

          Overwrite% = 0
        END IF


CopyErrorMsg:
        IF NOT CopySuccess% THEN
        InfoBox 6, "Aborted! Error while copying or cutting."
        Temp% = CaptureInf%(6)
        EXIT FOR
        END IF

      END IF

    NEXT



'**********************************
'Prepare to exit routine.
'**********************************

  ViewType% = TempViewType%
  FindNeededFiles -1, 0

ExitPaste:
  ERASE CopyObj$
  ERASE CopyObjIndex%
  PrintFileSize 0, 0, 0, 1
  CutCopy% = 0
  CopyFromZip% = 0
  Button 90, 458, 140, 477, 8, 0, 0, 4, 1     'Cut button
  Button 143, 458, 193, 477, 8, 0, 0, 4, 1    'Copy button


'лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

  ELSEIF ItemNum% = 6 THEN             'Refresh

  CLS
  OldFileNum% = FileNum%
  FindNeededFiles 0, 0

    IF FileNum% <> OldFileNum% THEN
    FileScroll% = 1
    END IF

  Interface

'лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
  ELSEIF ItemNum% = 7 THEN             'Back history

    IF LenActiveZipFile% = 0 THEN

      IF PosMemDir% >= 1 THEN
        PrintFileSize 0, 0, 0, 1
          IF MemIndex% = 1 AND MemDir$(PosMemDir% - 1) <> CurrentDir$ THEN
            CurrentDir$ = MemDir$(PosMemDir% - 1)
            FindNeededFiles 1, 0
          ELSEIF PosMemDir% > 1 THEN
            PosMemDir% = PosMemDir% - 2
            CurrentDir$ = MemDir$(PosMemDir%)
            PosMemDir% = PosMemDir% + 1
            FindNeededFiles 1, 0
          END IF
      END IF

    END IF

'лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
  ELSEIF ItemNum% = 8 THEN             'Forward history

    IF LenActiveZipFile% = 0 THEN

      IF PosMemDir% <= MaxMemDir THEN
        IF LEN(MemDir$(PosMemDir%)) THEN
        PrintFileSize 0, 0, 0, 1
        CurrentDir$ = MemDir$(PosMemDir%)
        FindNeededFiles 1, 0
        PosMemDir% = PosMemDir% + 1
        END IF
      END IF

    END IF

'лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл

  ELSEIF ItemNum% = 9 THEN             'MEM button

    IF LenActiveZipFile% = 0 THEN

      IF MemIndex% THEN
        RememberDir CurrentDir$
      ELSE
        MemIndex% = 1
      END IF

    END IF

'лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
  ELSEIF ItemNum% = 10 THEN            'RST button
  MemIndex% = 0

'лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
  ELSEIF ItemNum% = 11 THEN            'Up dir button

    IF LEN(CurrentDir$) > 3 OR LenActiveZipFile% > 0 THEN
    WDChangeDir "..", -1
    END IF

'лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
  ELSEIF ItemNum% = 12 THEN            'QView

  OldCurrentDir$ = CurrentDir$


    IF FirstSelObject%(File$, Extension$, Directory%) THEN
      IF NOT (Directory%) THEN
      File$ = File$ + "." + Extension$

        IF LenActiveZipFile% THEN
        Destination$ = TempPath$              'TempPath$ must never change!
          IF NOT (UnZipFile%(File$, Destination$)) THEN
          ShowFiles 0
          EXIT SUB
          END IF
        END IF

      CMDLine$ = AddSlash$(CurrentDir$) + File$
      END IF
    END IF


  FullMemEXE Editor$, WindosPath$, OldCurrentDir$, 0, 0, UseCommand%


'лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
  ELSEIF ItemNum% = 13 OR ItemNum% = 15 THEN         'Scroll up, left

  ShowFiles -1

'лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
  ELSEIF ItemNum% = 14 OR ItemNum% = 16 THEN         'Scroll down, right

  ShowFiles 1

'лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
  ELSEIF ItemNum% = 17 THEN             'Dos shell

  Temp$ = ENVIRON$("COMSPEC")

    IF LEN(Temp$) = 0 THEN
    Temp$ = DefaultDrive$ + "\COMMAND.COM"
    END IF

  FullMemEXE Temp$, CurrentDir$, CurrentDir$, 0, 0, 0


'лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
  ELSEIF ItemNum% = 18 THEN       'End (this button doesn't physically exist)

    IF ViewType% THEN
      Remainder% = FileNum% MOD 11
        IF Remainder% = 0 THEN Remainder% = 11
        IF FileNum% > 55 THEN FileScroll% = FileNum% - Remainder% - 44 + 1
    ELSE
      Remainder% = FileNum% MOD 5
        IF Remainder% = 0 THEN Remainder% = 5
        IF FileNum% > 20 THEN FileScroll% = FileNum% - Remainder% - 15 + 1
    END IF

  ShowFiles 0
  END IF

EXIT SUB


'лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
'лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
RefreshBox3:
InfoBox 3, Something$
Button 170, 275, CurX!, 295, 8, 0, 2, 0, 2
SelectObject File$, Extension$, Directory%, 290, 172, 0
RETURN

'----------------------------------------------------------------------------
PrepareToExec:

SCREEN 0
Temp% = SETMEM(-655360)

RETURN

'----------------------------------------------------------------------------
RestoreAfterExec:

Temp% = SETMEM(655360)

FileScroll% = 1
CMDLine$ = ""
DosScr 1
SCREEN 12
MouseDefaults

RETURN

END SUB

SUB ButtonsPress (BYVAL x%, BYVAL y%, ButNum%)

'Task Bar buttons

  IF y% > 458 AND y% < 477 THEN

    IF x% > 5 AND x% < 70 THEN                       'Exit button
    ButNum% = 1
    Button 5, 458, 70, 477, 0, 8, 0, 0, 3
    END IF

    IF x% > 90 AND x% < 140 THEN                     'Cut button
    ButNum% = 2
    Button 90, 458, 140, 477, 0, 8, 0, 4, 1
    END IF

    IF x% > 143 AND x% < 193 THEN                    'Copy button
    ButNum% = 3
    Button 143, 458, 193, 477, 0, 8, 0, 4, 1
    END IF

    IF x% > 196 AND x% < 246 THEN                    'Paste button
    ButNum% = 4
    Button 196, 458, 246, 477, 0, 8, 0, 0, 0
    END IF

    IF x% > 260 AND x% < 310 THEN                    'Refresh button
    ButNum% = 6
    Button 260, 458, 310, 477, 0, 8, 0, 0, 0
    END IF

  END IF
'----------------------------------------------------------------------------

'Explorer buttons


  IF x% > 581 AND x% < 638 THEN

    IF y% > 60 AND y% < 90 THEN                      'Back history arrow
    ButNum% = 7
    Button 581, 60, 638, 90, 0, 8, 0, 0, 0
    END IF

    IF y% > 92 AND y% < 122 THEN                     'Forward history arrow
    ButNum% = 8
    Button 581, 92, 638, 122, 0, 8, 0, 0, 0
    END IF

    IF y% > 124 AND y% < 154 THEN                    'MEM button
    ButNum% = 9
    Button 581, 124, 638, 154, 0, 8, 0, 4, 1
    END IF

    IF y% > 156 AND y% < 186 THEN                    'RST button
    ButNum% = 10
    Button 581, 156, 638, 186, 0, 8, 0, 0, 0
    END IF

    IF y% > 210 AND y% < 240 THEN                    'Up Dir arrow
    ButNum% = 11
    Button 581, 210, 638, 240, 0, 8, 0, 0, 0
    END IF

    IF y% > 263 AND y% < 293 THEN                    'QView button
    ButNum% = 12
    Button 581, 263, 638, 293, 0, 8, 0, 0, 0
    END IF

  END IF

'----------------------------------------------------------------------------

'File Scrolling buttons

  IF x% > 575 AND x% < 638 THEN

    IF y% > 310 AND y% < 335 THEN
    ButNum% = 13
    PUT (575, 310), UpArrPrs, PSET

    ELSEIF y% > 340 AND y% < 365 THEN
    ButNum% = 14
    PUT (575, 340), DownArrPrs, PSET
    END IF

  END IF



  IF y% > 370 AND y% < 418 THEN

    IF x% > 572 AND x% < 602 THEN
    ButNum% = 15
    PUT (572, 370), LeftArrPrs, PSET

    ELSEIF x% > 608 AND x% < 638 THEN
    ButNum% = 16
    PUT (608, 370), RightArrPrs, PSET
    END IF

  END IF

'----------------------------------------------------------------------------

    IF y% > 434 AND y% < 450 AND x% > 541 AND x% < 638 THEN
    ButNum% = 17
    Button 541, 434, 638, 450, 0, 8, 2, 0, 2
    LoadFont 255, "MS-DOS SHELL", 550, 433, 0
    END IF

'============================================================================


'Only scrolling arrows.
  IF ButNum% >= 13 AND ButNum% <= 16 THEN
  ButtonsExecute ButNum%
  END IF

END SUB

SUB ButtonsRelease (ButNum%)

  IF ButNum% = 1 THEN                        'Exit button
  Button 5, 458, 70, 477, 8, 0, 0, 0, 3

'----------------------------------------------------------------------------
  ELSEIF ButNum% = 2 OR ButNum% = 3 THEN    'Cut or Copy buttons
  Button 90, 458, 140, 477, 8, 0, 0, 4, 1
  Button 143, 458, 193, 477, 8, 0, 0, 4, 1

'----------------------------------------------------------------------------
  ELSEIF ButNum% = 4 THEN                   'Paste button
  Button 196, 458, 246, 477, 8, 0, 0, 0, 0

'----------------------------------------------------------------------------
'Toolbar buttons

  ELSEIF ButNum% = 7 THEN                   'Back history arrow
  Button 581, 60, 638, 90, 8, 0, 0, 0, 0

'----------------------------------------------------------------------------
  ELSEIF ButNum% = 8 THEN                   'Forward history arrow
  Button 581, 92, 638, 122, 8, 0, 0, 0, 0

'----------------------------------------------------------------------------
  ELSEIF ButNum% = 10 THEN                  'RST button
  Button 581, 156, 638, 186, 8, 0, 0, 0, 0
  Button 581, 124, 638, 154, 8, 0, 0, 4, 1

'----------------------------------------------------------------------------
  ELSEIF ButNum% = 11 THEN                  'Up Dir arrow
  Button 581, 210, 638, 240, 8, 0, 0, 0, 0

'----------------------------------------------------------------------------
  ELSEIF ButNum% = 12 THEN                  'QView button
  Button 581, 263, 638, 293, 8, 0, 0, 0, 0

'----------------------------------------------------------------------------
  ELSEIF ButNum% = 13 THEN
  PUT (575, 310), UpArrRls, PSET

'----------------------------------------------------------------------------
  ELSEIF ButNum% = 14 THEN
  PUT (575, 340), DownArrRls, PSET

'----------------------------------------------------------------------------
  ELSEIF ButNum% = 15 THEN
  PUT (572, 370), LeftArrRls, PSET

'----------------------------------------------------------------------------
  ELSEIF ButNum% = 16 THEN
  PUT (608, 370), RightArrRls, PSET

'----------------------------------------------------------------------------
  ELSEIF ButNum% = 17 THEN
  Button 541, 434, 638, 450, 8, 0, 2, 0, 2
  LoadFont 255, "MS-DOS SHELL", 549, 433, 0
  END IF

'============================================================================


'Everything except for scrolling arrows.
  IF ButNum% < 13 OR ButNum% > 16 THEN
  ButtonsExecute ButNum%
  END IF


'============================================================================
  IF ButNum% = 2 THEN                       'Cut button

    IF CutCopy% = 1 AND NumOfClipObjects% > 0 THEN
      Temp% = 10
    ELSE
      Temp% = 4
    END IF

  Button 90, 458, 140, 477, 8, 0, 0, Temp%, 1

'----------------------------------------------------------------------------
  ELSEIF ButNum% = 3 THEN                   'Copy button

    IF CutCopy% = 2 AND NumOfClipObjects% > 0 THEN
      Temp% = 10
    ELSE
      Temp% = 4
    END IF

  Button 143, 458, 193, 477, 8, 0, 0, Temp%, 1

'----------------------------------------------------------------------------
  ELSEIF ButNum% = 9 THEN                   'MEM button

    IF MemIndex% THEN
      Temp% = 10
    ELSE
      Temp% = 4
    END IF

  Button 581, 124, 638, 154, 8, 0, 0, Temp%, 1
  END IF

'----------------------------------------------------------------------------


ButNum% = 0

END SUB

'The function returns: -1 if successful.
'                       0 if not successful.
'
FUNCTION CMDLineOk%

  IF LEN(CMDLine$) > 64 THEN
    CMDLine$ = ""
    InfoBox 6, "Command line too long to pass to program."
    Temp% = CaptureInf%(6)
  ELSE
    CMDLineOk% = -1
  END IF

END FUNCTION

SUB Delay (BYVAL Time%)

'time%=67 (1 second)

  FOR Counter% = 1 TO Time%
  WAIT &H3DA, 8, 8
  WAIT &H3DA, 8
  NEXT

END SUB

SUB DirList

LINE (0, 430)-(540, 450), 0, BF

CharNum% = 66

LOCATE 28, 1
COLOR 15


  IF LenActiveZipFile% THEN
    CharNum% = CharNum% - (LenActiveZipFile% + LEN(CurrentZipDir$) + 1)

      IF CharNum% > 0 THEN
      PRINT RIGHT$(AddSlash$(CurrentDir$), CharNum%);
      END IF

    COLOR 12
    PRINT RIGHT$(ActiveZipFile$ + "\" + CurrentZipDir$, 66);

  ELSE

    PRINT RIGHT$(CurrentDir$, 66);
  END IF


END SUB

SUB DrvButtonPress (BYVAL x%, BYVAL y%, DrvPrs%)

DrvPrs% = 0           'Assume the drives weren't pressed.


  IF y% > 20 AND y% < 40 THEN

  TotalDiskNum% = LEN(Drives$)
  LastX% = 640 \ TotalDiskNum%

    FOR Counter% = 1 TO TotalDiskNum%
    xLeft% = LastX% * Counter% - LastX%
    xRight% = LastX% * Counter% - 1


      IF x% > xLeft% AND x% < xRight% THEN

      Button xLeft%, 20, xRight%, 40, 0, 8, 2, 0, 0
      CurrentDir$ = MID$(Drives$, Counter%, 1) + ":\"
      LenActiveZipFile% = 0

      FindNeededFiles -1, 1 - MemIndex%
      PrintFileSize 0, 0, 0, 1

      DrvPrs% = -1            'The drive button was pressed.
      EXIT SUB

      END IF


    NEXT

  END IF

END SUB

SUB EmptyKeyb

'Empty keyboard buffer.

DEF SEG = &H40
POKE &H1A, PEEK(&H1C)
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

'Function returns: -1 when successful.
'                   0 when failed.
'
FUNCTION ExtractPrgmDir% (Program$, Path$)

Slash$ = "\"


  IF INSTR(Program$, Slash$) = 3 THEN
    IF INSTR(Program$, ":") = 2 THEN
    LenProgram% = LEN(Program$)

      FOR Counter% = LenProgram% TO 1 STEP -1
      Character$ = MID$(Program$, Counter%, 1)
        IF Character$ = Slash$ THEN EXIT FOR
      NEXT

    Path$ = LEFT$(Program$, Counter% - 1)
    Program$ = MID$(Program$, Counter% + 1)

      IF Counter% = 3 THEN
      Path$ = Path$ + Slash$
      END IF

    ExtractPrgmDir% = -1
    END IF
  END IF


END FUNCTION

SUB FindNeededFiles (BYVAL ShowFl%, BYVAL Remember%)

ERASE SelObj%
ERASE Files$
ERASE Extensions$
ERASE Directories$

'============================================================================

  IF LenActiveZipFile% THEN

      IF LEN(CurrentZipDir$) THEN
        Path$ = AddSlash$(CurrentZipDir$)
      ELSE
        Path$ = CurrentZipDir$
      END IF

    IF NOT (ZipRead%(AddSlash$(CurrentDir$) + ActiveZipFile$, Path$)) THEN
    LenActiveZipFile% = 0
    InfoBox 6, "Error while trying to access archive."
    Temp% = CaptureInf%(6)
    GOTO BeginReadDir
    END IF

'----------------------------------------------------------------------------
  ELSE

BeginReadDir:

  Temp% = ChangeDirDrive%(CurrentDir$)

  TempMask$ = "*.*"
  DirCount% = 1
  FileCount% = 1

    IF ReadDir(TempMask$, FindFirst, Filter%) THEN
      DO
      Filename$ = UCASE$(RTRIM$(DTA.Filename))
        IF ASC(DTA.Attributes) AND 16 THEN               'This is a directory.
            IF Filename$ <> "." AND DirCount% <= DirectoriesFull THEN
            Directories$(DirCount%) = Filename$
            DirCount% = DirCount% + 1
            END IF
        ELSE                                             'This is a file.
            IF FileCount% <= FilesFull THEN
            ExtractFileExt Filename$, Extension$
            Files$(FileCount%) = Filename$
            Extensions$(FileCount%) = Extension$
            FileSizes&(FileCount%) = DTA.FileSize
            FileCount% = FileCount% + 1
            END IF
        END IF
        IF NOT (ReadDir(TempMask$, FindNext, Filter%)) THEN EXIT DO
      LOOP
    END IF


  DirCount% = DirCount% - 1
  FileCount% = FileCount% - 1
  FileNum% = DirCount% + FileCount%

  END IF


'============================================================================

  IF FileSort% <> 3 THEN Sort -1
Sort FileSort%

DirList

  IF ShowFl% THEN
  ShowFiles 255
  END IF

  IF Remember% THEN
    IF LenActiveZipFile% = 0 THEN
    RememberDir CurrentDir$
    END IF
  END IF

END SUB

SUB Finish

SaveCfg 0
SCREEN 0
CLS

CHDRIVE WindosPath$
CHDIR WindosPath$

END

END SUB

'Function returns: -1 if file/dir is selected.
'                   0 if no files/dirs are selected.
'
FUNCTION FirstSelObject% (File$, Extension$, Directory%)


  FOR Counter% = 1 TO FileNum%

    IF SelObj%(Counter%) THEN
    WhatFile Counter%, File$, Extension$, Directory%
    FirstSelObject% = -1
    EXIT FUNCTION
    END IF

  NEXT


END FUNCTION

SUB FullMemEXE (Program$, PrgDir$, SwitchTo$, BYVAL PressKey%, BYVAL GFXMode%, BYVAL LoadCommand%)


File% = FREEFILE

'*****************************************
'Write a batch file if using COMMAND.COM.
'*****************************************

  IF LoadCommand% THEN
    IF UCASE$(RIGHT$(Program$, 3)) <> "BAT" THEN

    Destination$ = AddSlash$(TempPath$) + BatchFile

    OPEN Destination$ FOR OUTPUT AS #File%
    BatchLine$ = "@" + Program$

      IF LEN(CMDLine$) THEN
      BatchLine$ = BatchLine$ + " " + CMDLine$
      END IF

    PRINT #File%, BatchLine$
    CLOSE #File%

    CMDLine$ = ""
    Program$ = Destination$

    END IF
  END IF


'****************************
'Write a RunnerTmpFile file.
'****************************

OPEN AddSlash$(TempPath$) + RunnerTmpFile FOR OUTPUT AS #File%
PRINT #File%, PrgDir$
PRINT #File%, Program$
PRINT #File%, CMDLine$
PRINT #File%, PressKey%
PRINT #File%, GFXMode%
CLOSE #File%


'*************************************
'Finish off the rest.
'*************************************

  IF LEN(SwitchTo$) THEN
  CurrentDir$ = SwitchTo$
  END IF

SaveCfg 1

CHDIR WindosPath$
CHDRIVE WindosPath$


'*************************************
'Flush buffers before exit?
'*************************************

  IF FlushBuffers% THEN
  FlushDOSBuffers
  END IF

CLS
END

END SUB

'If get, PutImage% = 0
'If put, PutImage% = -1
'
SUB GetPutScreen (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL PutImage%)

SegScrArray1% = VARSEG(ScrArray1(0))
SegScrArray2% = VARSEG(ScrArray2(0))
SegScrArray3% = VARSEG(ScrArray3(0))
SegScrArray4% = VARSEG(ScrArray4(0))

  IF PutImage% THEN
    Put12 SegScrArray1%, SegScrArray2%, SegScrArray3%, SegScrArray4%, x1%, y1%, x2%, y2%
  ELSE
    Get12 SegScrArray1%, SegScrArray2%, SegScrArray3%, SegScrArray4%, x1%, y1%, x2%, y2%
  END IF

END SUB

SUB Interface

ERASE SelObj%


'*************************************
'Initialize the palette.
'*************************************

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


'********************************
'Draw status bar.
'********************************

DEF SEG = VARSEG(ScrArray1(0))
BLOAD GraphicsPathSlash$ + StatusBarFile, 0
DEF SEG

PUT (0, 455), ScrArray1, PSET


ObjNum% = NumOfClipObjects%

  IF CutCopy% = 1 AND ObjNum% > 0 THEN
    Temp% = 10
  ELSE
    Temp% = 4
  END IF

Button 90, 458, 140, 477, 8, 0, 0, Temp%, 1     'Cut button

  IF CutCopy% = 2 AND ObjNum% > 0 THEN
    Temp% = 10
  ELSE
    Temp% = 4
  END IF

Button 143, 458, 193, 477, 8, 0, 0, Temp%, 1    'Copy button


'********************************
'Draw tools bar.
'********************************

DEF SEG = VARSEG(ScrArray1(0))
BLOAD GraphicsPathSlash$ + ToolBarFile, 0
DEF SEG

PUT (570, 54), ScrArray1, PSET


  IF MemIndex% THEN
  Button 581, 124, 638, 154, 8, 0, 0, 10, 1     'MEM button
  END IF


'********************************
'Draw drive bar.
'********************************

DEF SEG = VARSEG(ScrArray1(0))
BLOAD GraphicsPathSlash$ + DriveBarFile, 0
DEF SEG

PUT (0, 20), ScrArray1, PSET


'********************************
'Draw 3D separator lines.
'********************************

'LINE (565, 53)-(568, 425), 2, B
'LINE (566, 53)-(567, 425), 8, B

LINE (0, 425)-(640, 428), 2, B
LINE (0, 426)-(640, 427), 8, B


'********************************
'Draw 4 scroll buttons.
'********************************

PUT (575, 310), UpArrRls, PSET
PUT (575, 340), DownArrRls, PSET
PUT (572, 370), LeftArrRls, PSET
PUT (608, 370), RightArrRls, PSET


'********************************
'Draw the rest.
'********************************

GET (XFileSize - 5, YFileSize - 1)-(XFileSize + 170, YFileSize + 20), FileSizeBackRestore

PullMenuShow 0, 0, 0, -1, 0
ShowDrives
DirList

Button 541, 434, 638, 450, 8, 0, 2, 0, 2
LoadFont 255, "MS-DOS SHELL", 549, 433, 0

PrintFileSize 0, 0, 0, 1
ShowFiles 0


END SUB

SUB LoadFont (BYVAL FontType%, Text$, BYVAL Textx%, BYVAL Texty%, BYVAL Colour%)

  SELECT CASE FontType%

  CASE 0
  COLOR Colour%
  Text$ = LTRIM$(RTRIM$(Text$))
    IF ViewType% = 0 THEN
      Length% = LEN(Text$)
        IF Length% / 2 <> Length% \ 2 THEN Length% = Length% + 1
      xFilePrint% = Textx% \ 8 - (Length% \ 2) + 5
      yFilePrint% = Texty% \ 16 + 2
    ELSE
      xFilePrint% = Textx% \ 8
      yFilePrint% = Texty% \ 16
    END IF
  LOCATE yFilePrint%, xFilePrint%
  PRINT Text$;

'----------------------------------------------------------------------------
  CASE 3

  'INT 10 - VIDEO - GET FONT INFORMATION (EGA, MCGA, VGA)
  ' AX = 1130h
  ' BH = pointer specifier
  '     00h INT 1Fh pointer
  '     01h INT 43h pointer
  '     02h ROM 8x14 character font pointer
  '     03h ROM 8x8 double dot font pointer
  '     04h ROM 8x8 double dot font (high 128 characters)
  '     05h ROM alpha alternate (9 by 14) pointer (EGA,VGA)
  '     06h ROM 8x16 font (MCGA, VGA)
  '     07h ROM alternate 9x16 font (VGA only) (see #00021)
  '     11h (UltraVision v2+) 8x20 font (VGA) or 8x19 font (autosync EGA)
  '     12h (UltraVision v2+) 8x10 font (VGA) or 8x11 font (autosync EGA)
  'Return: ES:BP = specified pointer
  ' CX    = bytes/character of on-screen font (not the requested font!)
  ' DL    = highest character row on screen
  'Note:  for UltraVision v2+, the 9xN alternate fonts follow the corresponding
  '   8xN font at ES:BP+256N
  'BUG: the IBM EGA and some other EGA cards return in DL the number of rows on
  '   screen rather than the highest row number (which is one less).
  'SeeAlso: AX=1100h,AX=1103h,AX=1120h,INT 1F"SYSTEM DATA",INT 43"VIDEO DATA"
  '
  'Format of alternate font table [array]:
  'Offset Size  Description (Table 00021)
  ' 00h BYTE  character to be replaced (00h = end of table)
  ' 01h  N BYTEs  graphics data for character, one byte per scan line

  Regs.ax = &H1130
  Regs.bx = &H300
  CALL INTERRUPTX(&H10, Regs, Regs)

  x% = Textx%
  y% = Texty% + 6
  DEF SEG = Regs.es


    FOR LetterNum% = 1 TO LEN(Text$)
    LetterOffset% = Regs.bp + ASC(MID$(Text$, LetterNum%, 1)) * 8
    xPosition% = x% + (LetterNum% - 1) * 8
    yPosition% = y%

      FOR LineNum% = 0 TO 7
      ScanLine% = PEEK(LetterOffset% + LineNum%)

        IF ScanLine% AND 128 THEN
        PSET (xPosition%, yPosition%), Colour%
        END IF

        IF ScanLine% AND 64 THEN
        PSET (xPosition% + 1, yPosition%), Colour%
        END IF

        IF ScanLine% AND 32 THEN
        PSET (xPosition% + 2, yPosition%), Colour%
        END IF

        IF ScanLine% AND 16 THEN
        PSET (xPosition% + 3, yPosition%), Colour%
        END IF

        IF ScanLine% AND 8 THEN
        PSET (xPosition% + 4, yPosition%), Colour%
        END IF

        IF ScanLine% AND 4 THEN
        PSET (xPosition% + 5, yPosition%), Colour%
        END IF

        IF ScanLine% AND 2 THEN
        PSET (xPosition% + 6, yPosition%), Colour%
        END IF

        IF ScanLine% AND 1 THEN
        PSET (xPosition% + 7, yPosition%), Colour%
        END IF

      yPosition% = yPosition% + 1
      NEXT

    NEXT



  DEF SEG


'----------------------------------------------------------------------------
  CASE ELSE

  CONST Bold% = 1
  TempBold% = Bold%
  x% = Textx%
  y% = Texty% + 5
  Clr% = Colour%
  DRAW "s4"

  DO WHILE TempBold% > -1
  DRAW "BM" + STR$(x%) + "," + STR$(y%) + "C" + STR$(Clr%)
    FOR Counter% = 1 TO LEN(Text$)
    Char$ = MID$(Text$, Counter%, 1)
    DRAW WindosFont$(ASC(Char$))
      IF Bold% > 0 THEN DRAW "BR1"
    NEXT
  TempBold% = TempBold% - 1
  x% = x% + 1
  LOOP


  END SELECT

END SUB

SUB MouseDefaults

MousePut MouseDefX, MouseDefY
MouseSpeed XmsSpd%, YmsSpd%
MouseRange 0, 0, 638, 478

END SUB

FUNCTION NumOfClipObjects%

  FOR Counter% = 1 TO FilesFull + DirectoriesFull
    IF LEN(CopyObj$(Counter%)) THEN ObjNum% = ObjNum% + 1
  NEXT

NumOfClipObjects% = ObjNum%

END FUNCTION

SUB PrintFileSize (BYVAL Number%, BYVAL Directory%, BYVAL Sel%, BYVAL ResetSize%)

  IF ResetSize% = 1 THEN
  FileSum& = 0
  PUT (XFileSize - 5, YFileSize - 1), FileSizeBackRestore, PSET
  EXIT SUB
  END IF


  IF Directory% = 0 THEN

    SELECT CASE Sel%
    CASE 0
      FileSum& = FileSum& - FileSizes&(Number%)
    CASE 1
      FileSum& = FileSum& + FileSizes&(Number%)
    CASE ELSE
      FileSum& = 0
        FOR Counter% = 1 TO FileCount%
          IF SelObj%(DirCount% + Counter%) THEN
          FileSum& = FileSum& + FileSizes&(Counter%)
          END IF
        NEXT
    END SELECT


    IF FileSum& >= 0 THEN
      Temp$ = LTRIM$(STR$(FileSum&))
      ThreeTimes% = 0
        FOR Counter% = LEN(Temp$) TO 1 STEP -1      'Adding commas...
        ThreeTimes% = ThreeTimes% + 1
        FileSumStr$ = MID$(Temp$, Counter%, 1) + FileSumStr$
          IF ThreeTimes% = 3 AND Counter% > 1 THEN
          ThreeTimes% = 0
          FileSumStr$ = "," + FileSumStr$
          END IF
        NEXT
      PUT (XFileSize - 5, YFileSize - 1), FileSizeBackRestore, PSET
        IF FileSumStr$ <> "0" THEN
        LoadFont 3, FileSumStr$ + " bytes", XFileSize, YFileSize, 15
        END IF
    ELSE
      FileSum& = 0
    END IF


  END IF

END SUB

SUB PullMenuExecute (BYVAL Menu%, BYVAL ItemNum%)

SELECT CASE Menu%

  CASE 1                                 'File Menu

'----------------------------------------------------------------------------

    IF ItemNum% = 1 THEN                 'Quick Execute

      IF LenActiveZipFile% THEN EXIT SUB


    RunSelEXE -1, AnyKey%

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 2 THEN             'Command Line

      IF LenActiveZipFile% THEN EXIT SUB


      IF FirstSelObject%(File$, Extension$, Directory%) THEN
      InfoBox 2, "Command line of selected program:"
      ButPrs% = CaptureInf%(2)

        IF ButPrs% = 1 OR ButPrs% = 2 THEN
        CMDLine$ = NewName$
        RunSelEXE ButPrs% - 1, AnyKey%
        END IF

      ShowFiles 0
      END IF

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 4 THEN             'Create Directory

      IF LenActiveZipFile% THEN EXIT SUB


    InfoBox 5, "Create directory:"

      IF CaptureInf%(5) = 1 THEN
      ON LOCAL ERROR GOTO MkDirError
      MKDIR NewName$
      ON LOCAL ERROR GOTO 0
      FindNeededFiles 0, 0
      PrintFileSize 0, 0, 0, 1
      END IF


AfterMkDirError:
ShowFiles 255

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 5 THEN             'Create Empty File

      IF LenActiveZipFile% THEN EXIT SUB


    InfoBox 5, "Create an empty file:"

      IF CaptureInf%(5) = 1 THEN

        IF ReadDir%(NewName$, FindFirst, AllFilesAtt) THEN
          InfoBox 6, "File already exists."
          PrsBut% = CaptureInf(6)
        ELSE
          ON LOCAL ERROR GOTO MkFileError
          File% = FREEFILE
          OPEN NewName$ FOR BINARY AS #File%
          CLOSE #File%
          ON LOCAL ERROR GOTO 0
          FindNeededFiles 0, 0
          PrintFileSize 0, 0, 0, 1
        END IF

      END IF


AfterMkFileError:
ShowFiles 255

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 7 THEN             'Delete

      IF FirstSelObject%(File$, Extension$, Directory%) THEN
      InfoBox 1, "Delete selected files?"


        IF CaptureInf%(1) = 1 THEN


          IF LenActiveZipFile% THEN

'******************************
'Remove files with an archiver.
'******************************

            File% = FREEFILE
            OPEN ZipListFile$ FOR OUTPUT AS #File%

              FOR Counter% = 1 TO FileNum%
                IF SelObj%(Counter%) THEN
                WhatFile Counter%, File$, Extension$, Directory%

                  IF LEN(CurrentZipDir$) THEN
                    Something$ = CurrentZipDir$ + "\" + File$
                  ELSE
                    Something$ = File$
                  END IF

                  IF Directory% THEN
                    PRINT #File%, Something$ + "\*.*"
                  ELSE
                    PRINT #File%, Something$ + "." + Extension$
                  END IF

                END IF
              NEXT

            CLOSE #File%
            CMDLine$ = "-d " + ActiveZipFile$ + ZipListFile0$
            QuickEXE AddSlash$(ArchivesPath$) + ZipComp, CurrentDir$, CurrentDir$, 0, -1


'******************************
'Remove files normally.
'******************************

          ELSE

            InfoBox 4, Something$

              FOR Counter% = 1 TO FileNum%

                IF SelObj%(Counter%) THEN
                WhatFile Counter%, File$, Extension$, Directory%

                'Clear old file names from screen...
                LINE (250, 180)-(390, 200), 2, BF

                  IF Directory% THEN
                    Something$ = File$
                  ELSE
                    Something$ = File$ + "." + Extension$
                  END IF

                LoadFont 255, Something$, 275, 180, 15

                  SELECT CASE KillFileDir(File$, Extension$, CurrentDir$, Directory%)
                  CASE 0
                    EXIT FOR
                  CASE 1
                    InfoBox 4, Something$
                  END SELECT

                END IF
              NEXT

            PrintFileSize 0, 0, 0, 1
            FindNeededFiles -1, 0

          END IF


'*********************************
'If cancelled then restore screen.
'*********************************
        ELSE

          ShowFiles 255

        END IF

      END IF


'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 8 THEN             'Rename

      IF LenActiveZipFile% THEN EXIT SUB


      IF FirstSelObject%(File$, Extension$, Directory%) THEN
      InfoBox 5, "Rename file/directory to:"
      PrsBut% = CaptureInf%(5)

        IF PrsBut% = 1 THEN
        ON LOCAL ERROR GOTO RenameError
          IF Directory% THEN
            NAME File$ AS NewName$
          ELSE
            NAME File$ + "." + Extension$ AS NewName$
          END IF
        ON LOCAL ERROR GOTO 0
        PrintFileSize 0, 0, 0, 1
        FindNeededFiles 0, 0
        END IF

AfterRenameError:
      ShowFiles 255
      END IF


'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 9 THEN             'Duplicate

      IF LenActiveZipFile% THEN EXIT SUB


      IF FirstSelObject%(File$, Extension$, Directory%) THEN
      InfoBox 5, "Duplicate file/directory as:"
      PrsBut% = CaptureInf(5)

        IF PrsBut% = 1 THEN

          IF LEN(Extension$) THEN
          File$ = File$ + "." + Extension$
          END IF

        Source$ = AddSlash$(CurrentDir$)
        Destination$ = Source$ + NewName$
        Source$ = Source$ + File$

          IF Directory% THEN
            Success% = CopyDir%(Source$, Destination$)
          ELSE
            Success% = CopyFile%(Source$, Destination$)
          END IF

          IF NOT (Success%) THEN
          InfoBox 6, "Error while duplicating."
          Temp% = CaptureInf%(6)
          END IF

        PrintFileSize 0, 0, 0, 1
        FindNeededFiles 0, 0
        END IF

      ShowFiles 255
      END IF


'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 11 THEN             'Compress


      IF LenActiveZipFile% > 0 OR CopyFromZip% = -1 THEN
      EXIT SUB
      END IF


      IF CutCopy% = 2 THEN
        TMPPath$ = SourceDirCopy$
      ELSE
        SetupCopyObj
        TMPPath$ = CurrentDir$
      END IF



    File% = FREEFILE
    OPEN AddSlash$(TempPath$) + CompList FOR OUTPUT AS #File%

      FOR Counter% = 1 TO FilesFull + DirectoriesFull
      Something$ = CopyObj$(Counter%)

        IF LEN(Something$) THEN
          IF CopyObjIndex%(Counter%) THEN
          Something$ = Something$ + "\*.*"
          END IF
        PRINT #File%, Something$
        END IF

      NEXT Counter%

    CLOSE #File%


    CMDLine$ = AddSlash$(CurrentDir$) + "default " + CompList + " /C"
    FullMemEXE CompTool, WindosPath$, CurrentDir$, 0, -1, -1

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 12 THEN            'Decompress

      IF LenActiveZipFile% > 0 OR CopyFromZip% = -1 THEN
      EXIT SUB
      END IF


    NewCurDir$ = CurrentDir$

      IF CutCopy% = 2 THEN
        CurrentDir$ = SourceDirCopy$
      ELSE
        SetupCopyObj
      END IF

      FOR Counter% = 1 TO FilesFull + DirectoriesFull
        IF LEN(CopyObj$(Counter%)) > 0 AND CopyObjIndex%(Counter%) = 0 THEN EXIT FOR
      NEXT Counter%

      IF Counter% >= FilesFull + DirectoriesFull THEN EXIT SUB

    ArchFile$ = AddSlash$(CurrentDir$) + CopyObj$(Counter%)

    TMPPath$ = CurrentDir$
    'PRINT CompTool + " " + ArchFile$ + " " + NewCurDir$ + " /D"
    CMDLine$ = ArchFile$ + " " + NewCurDir$ + " /D"
    FullMemEXE CompTool, WindosPath$, NewCurDir$, 0, -1, -1

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 14 THEN            'Print


      IF FirstSelObject%(File$, Extension$, Directory%) THEN
        IF NOT (Directory%) THEN
        OldCurrentDir$ = CurrentDir$
        File$ = File$ + "." + Extension$

          IF LenActiveZipFile% THEN
          Destination$ = TempPath$              'TempPath$ must never change!
            IF NOT (UnZipFile%(File$, Destination$)) THEN
            ShowFiles 0
            EXIT SUB
            END IF
          END IF

        CMDLine$ = AddSlash$(CurrentDir$) + File$
        FullMemEXE PrintText, WindosPath$, OldCurrentDir$, 0, -1, -1
        END IF
      END IF


    'Enable when the problem with "File Error" in PRINT.EXE is fixed.
    'QuickEXE PrintText, WindosPath$, CurrentDir$, 0, 0

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 16 THEN            'Exit

    Finish

    END IF


'============================================================================

  CASE 2                                   'Edit Menu


    IF ItemNum% = 1 THEN                   'Cut
    ButtonsExecute 2

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 2 THEN               'Copy
    ButtonsExecute 3

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 3 THEN               'Paste
    ButtonsExecute 4

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 5 THEN               'Select

    InfoBox 5, "Input mask:"

      IF CaptureInf(5) = 1 THEN
        ERASE SelObj%
        PrintFileSize 0, 0, 0, 1
        TempMask$ = UCASE$(NewName$)


          IF ReadDir(TempMask$, FindFirst, Filter%) THEN

            DO
            Filename$ = RTRIM$(DTA.Filename)

              FOR i% = 1 TO DirCount%
                IF Filename$ = Directories$(i%) AND LEFT$(Filename$, 1) <> "." THEN
                SelObj%(i%) = 1
                END IF
              NEXT

              FOR i% = 1 TO FileCount%
                IF Extensions$(i%) = "" THEN
                  Temp$ = Files$(i%)
                ELSE
                  Temp$ = Files$(i%) + "." + Extensions$(i%)
                END IF
                IF Filename$ = Temp$ THEN
                SelObj%(DirCount% + i%) = 1
                PrintFileSize i%, 0, 1, 0
                END IF
              NEXT

              IF NOT (ReadDir(TempMask$, FindNext, Filter%)) THEN
              EXIT DO
              END IF

            LOOP

          END IF

      END IF

    ShowFiles 0

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 6 THEN               'Invert Selection

      FOR Counter% = 1 TO DirCount%
        IF Directories$(Counter%) <> ".." THEN
        SelObj%(Counter%) = 1 - SelObj%(Counter%)
        END IF
      NEXT

      FOR Counter% = 1 TO FileCount%
      Temp% = SelObj%(DirCount% + Counter%)
      SelObj%(DirCount% + Counter%) = 1 - Temp%
      NEXT

    PrintFileSize 0, 0, 2, 0
    ShowFiles 0

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 7 THEN               'Select All

      FOR Counter% = 1 TO DirCount%
        IF Directories$(Counter%) <> ".." THEN
        SelObj%(Counter%) = 1
        END IF
      NEXT

      FOR Counter% = 1 TO FileCount%
      SelObj%(DirCount% + Counter%) = 1
      NEXT

    PrintFileSize 0, 0, 2, 0
    ShowFiles 0

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 8 THEN               'Deselect All

    ERASE SelObj%
    PrintFileSize 0, 0, 0, 1
    ShowFiles 0

    END IF

'============================================================================


  CASE 3                                   'View Menu



    IF ItemNum% = 1 THEN                   'AS Icons

    ViewType% = 0
    ShowFiles 255

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 2 THEN               'As Text

    ViewType% = 1
    ShowFiles 255

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 4 THEN               'By Name
    FileSort% = 0
    GOSUB ViewSame

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 5 THEN               'By Extension
    FileSort% = 1
    GOSUB ViewSame

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 6 THEN               'By Size
    FileSort% = 2
    GOSUB ViewSame

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 7 THEN               'By Time/Date
    FileSort% = 3
    GOSUB ViewSame

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 9 THEN               'Options

    FullMemEXE Options, WindosPath$, CurrentDir$, 0, 0, 0


'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 10 THEN              'System Files

    CMDLine$ = DefaultDrive$ + "\" + System1 + " " + DefaultDrive$ + "\" + System2
    FullMemEXE Editor$, WindosPath$, CurrentDir$, 0, 0, UseCommand%

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 12 THEN              'Properties
    Flag% = 0


      IF LenActiveZipFile% = 0 THEN
      CurrentDirSlash$ = AddSlash$(CurrentDir$)
      File% = FREEFILE
      OPEN AddSlash$(TempPath$) + PropertyList FOR OUTPUT AS #File%

        FOR Counter% = 1 TO FileNum%
          IF SelObj%(Counter%) THEN

            Flag% = -1
            WhatFile Counter%, File$, Extension$, Directory%
            Temp$ = CurrentDirSlash$ + File$
              IF LEN(Extension$) THEN
              Temp$ = Temp$ + "." + Extension$
              END IF
            PRINT #File%, Temp$

          END IF
        NEXT

      CLOSE #File%
      END IF



      IF Flag% THEN
        CMDLine$ = "@" + PropertyList
      ELSE
        CMDLine$ = LEFT$(CurrentDir$, 1)
      END IF

    FullMemEXE Property, WindosPath$, CurrentDir$, 0, -1, -1

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 14 THEN              'QuickView (more mem)

    ButtonsExecute 12

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 15 THEN              'DOS Screen

    SCREEN 0
    DosScr 0
    SLEEP
    EmptyKeyb
    SCREEN 12
    Interface
    MouseDefaults

    END IF

'============================================================================

  CASE 4                                   'Filter Menu


    IF ItemNum% = 1 THEN                   'Directory
    Filter% = Filter% XOR DirectoryAtt

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 2 THEN               'Archive
    Filter% = Filter% XOR ArchiveAtt

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 3 THEN               'Read Only
    Filter% = Filter% XOR ReadOnlyAtt

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 4 THEN               'Hidden
    Filter% = Filter% XOR HiddenAtt

'----------------------------------------------------------------------------

    ELSEIF ItemNum% = 5 THEN               'System
    Filter% = Filter% XOR SystemAtt
    END IF

  GOSUB ViewSame

'============================================================================

  CASE 5                                   'Open Menu

  Temp% = FirstSelObject%(File$, Extension$, Directory%)
  RunItem File$, Extension$, Directory%, ItemNum%, AnyKey%


'============================================================================

  CASE 6                                   'Quick Menu

  File% = FREEFILE
  OPEN AddSlash$(WindosPath$) + QuickINI FOR INPUT AS #File%

    DO UNTIL EOF(File%)
    LINE INPUT #File%, Something$
    Something$ = UCASE$(LTRIM$(RTRIM$(Something$)))
    Counter% = Counter% + 1
      IF LEN(Something$) THEN

        IF Counter% >= ItemNum% THEN
          Extension$ = RIGHT$(Something$, 4)
            IF Extension$ = ".EXE" OR Extension$ = ".COM" OR Extension$ = ".BAT" THEN

                IF INSTR(Something$, "!") = 1 THEN
                  Something$ = MID$(Something$, 2)
                    IF ExtractPrgmDir%(Something$, Path$) = 0 THEN
                    EXIT DO
                    END IF
                ELSE
                  Path$ = CurrentDir$
                END IF

              FullMemEXE Something$, Path$, CurrentDir$, AnyKey%, 0, UseCommand%

            ELSE
              LenActiveZipFile% = 0
              CurrentDir$ = Something$
              FindNeededFiles 1, 1 - MemIndex%
              PrintFileSize 0, 0, 0, 1
              EXIT DO
            END IF
        END IF

      END IF
    LOOP

  CLOSE #File%

'============================================================================

  CASE 7                                   'Help Menu


    IF ItemNum% = 1 THEN                   'ReadMe.TXT

    CMDLine$ = AddSlash$(WindosPath$) + ReadMe
    FullMemEXE Editor$, WindosPath$, CurrentDir$, 0, 0, UseCommand%

'----------------------------------------------------------------------------
    ELSEIF ItemNum% = 2 THEN               'View registration file
    CMDLine$ = AddSlash$(WindosPath$) + RegEgFile
    FullMemEXE Editor$, WindosPath$, CurrentDir$, 0, 0, UseCommand%

'----------------------------------------------------------------------------
    ELSEIF ItemNum% = 3 THEN               'View WinDos history file
    CMDLine$ = AddSlash$(WindosPath$) + VerHisFile
    FullMemEXE Editor$, WindosPath$, CurrentDir$, 0, 0, UseCommand%

'----------------------------------------------------------------------------
    ELSEIF ItemNum% = 5 THEN               'About
    FullMemEXE About, WindosPath$, CurrentDir$, 0, 0, -1

'----------------------------------------------------------------------------
    ELSEIF ItemNum% = 7 THEN               'Help on program

      IF LenActiveZipFile% THEN EXIT SUB

    CMDLine$ = "/?"
    RunSelEXE 0, -1

    END IF

'============================================================================

END SELECT

EXIT SUB



'============================================================================
'===================================SUBS=====================================
'============================================================================
ViewSame:
FindNeededFiles 1, 0
PrintFileSize 0, 0, 0, 1
RETURN

'----------------------------------------------------------------------------
MkDirError:
InfoBox 6, "Cannot create directory."
Temp% = CaptureInf%(6)
RESUME AfterMkDirError

'----------------------------------------------------------------------------
MkFileError:
InfoBox 6, "Cannot create file."
Temp% = CaptureInf%(6)
RESUME AfterMkFileError

'----------------------------------------------------------------------------
RenameError:
InfoBox 6, "Unable to rename."
Temp% = CaptureInf(6)
RESUME AfterRenameError

END SUB

SUB PullMenuPress

MouseStatus Lb%, Rb%, x%, y%
  IF y% > 19 OR x% > 490 THEN
  EXIT SUB
  END IF

'----------------------------------------------------------------------------
'Read registry file and update array for OPEN menu.

MenuText$(MenuOpen, 1) = ""
MenuItems%(MenuOpen) = 0
Selected% = FirstSelObject%(File$, Extension$, Directory%)


  IF Selected% THEN

  REDIM Labels$(PullMenuItems - 1)

    IF Directory% THEN
    Extension$ = RegDirID
    END IF

  MenuItems%(MenuOpen) = Reg.GetLabels%(AddSlash$(WindosPath$) + ExtINI, Extension$, Labels$())

    FOR i% = 0 TO MenuItems%(MenuOpen) - 1
    MenuText$(MenuOpen, i% + 1) = LEFT$(Labels$(i%), MaxNumOfCharsPerPullMenu)
    NEXT

  END IF


'----------------------------------------------------------------------------
MouseShow


  DO
  OldActive% = Active%


    IF y% <= 24 THEN

      ItemNum% = 0                          'Cursor off, nothing is selected.
      LenText% = 255                        'Safe to exit loop.

        IF Lb% THEN                         'Turn menu off?
          DO                                'Wait until he lets go of button.
          MouseStatus Lb%, Rb%, x%, y%
          LOOP WHILE Lb%
        EXIT DO
        END IF

      Active% = x% \ PullMenuTopStep + 1
        IF Active% > PullMenuNum THEN
        Active% = PullMenuNum
        END IF

    ELSE

        IF (x% > PullMenuX1%) AND (x% < PullMenuX2%) AND (y% < PullMenuY2% - 7) THEN
          ItemNum% = (y% - 25 - 4) \ 15 + 1            'Item line number.
          Text$ = MenuText$(Active%, ItemNum%)
          LenText% = LEN(Text$)
          y% = 25 + (ItemNum% - 1) * 15                'Text y position.
          x% = PullMenuX1% + 7                         'Text x position.

            IF (OldY% <> y%) OR (OldX% <> x%) THEN
            MouseHide

              IF OldX% > -1 THEN          'Check if anything WAS selected.
              LoadFont 3, OldText$, OldX%, OldY%, 0
              END IF

              IF LenText% THEN            'New selection.
              LoadFont 3, Text$, x%, y%, 15
              END IF

            MouseShow
            OldY% = y%
            OldX% = x%
            OldText$ = Text$
            END IF

        ELSE

        'When mouse leaves the menu the cursor is turned off.
            IF ItemNum% THEN
            MouseHide
            LoadFont 3, OldText$, OldX%, OldY%, 0
            MouseShow
            OldX% = -1                     'Reset selection within the menu.
            ItemNum% = 0                   'Cursor off, nothing is selected.
            LenText% = 255
            END IF

        END IF

    END IF


    IF OldActive% <> Active% THEN
    OldX% = -1                             'Reset selection within the menu.
    MouseHide
      IF OldActive% > 0 THEN
      xOldActive% = (OldActive% - 1) * PullMenuTopStep
      GetPutScreen xOldActive%, PullMenuY1, xOldActive% + PullMenuWidth, PullMenuY2%, -1
      END IF
    'Show new menu, update menu text.
      FOR Counter% = 0 TO 1
      PullMenuShow Active%, OldActive%, xOldActive%, 0, Counter%
      NEXT
    MouseShow
    END IF


    IF (y% > 24) AND (Lb% = -1) AND (LenText% > 0) THEN
    EXIT DO
    END IF

  MouseStatus Lb%, Rb%, x%, y%
  LOOP

'----------------------------------------------------------------------------


MouseHide

  IF ItemNum% THEN
  x1% = x% - 3
  y1% = y% + 2
  x2% = x1% + PullMenuWidth - 8
  y2% = y% + 18

    FOR Counter% = 1 TO 3
    LINE (x1%, y1%)-(x2%, y2%), 2, BF
    LoadFont 3, Text$, x%, y%, 15
    Delay 3
    LINE (x1%, y1%)-(x2%, y2%), 7, BF
    LoadFont 3, Text$, x%, y%, 0
    Delay 3
    NEXT

  END IF


'Menus off...
PullMenuShow 0, 0, 0, -1, 0
'Restoring the background...
GetPutScreen PullMenuX1%, PullMenuY1, PullMenuX2%, PullMenuY2%, -1

'----------------------------------------------------------------------------
'Executing whatever was clicked on...

  IF ItemNum% THEN
  PullMenuExecute Active%, ItemNum%
  END IF

EXIT SUB

'============================================================================
ReadFileIntoSomething:
LINE INPUT #File%, Something$
FirstChar$ = LEFT$(LTRIM$(Something$), 1)
RETURN

END SUB

SUB PullMenuShow (BYVAL Active%, BYVAL OldActive%, BYVAL xOldActive%, BYVAL ShowAll%, BYVAL Part%)

Temp% = (Active% - 1) * PullMenuTopStep

'============================================================================

SELECT CASE Part%

  CASE 0


    IF ShowAll% THEN

      LINE (0, 1)-(639, 17), 2, BF
      LINE (1, 2)-(638, 16), 8, B
        FOR Counter% = 1 TO PullMenuNum
        LoadFont 255, MenuText$(Counter%, 0), PullMenuTopStep \ 3 + PullMenuTopStep * (Counter% - 1), 0, 0
        NEXT

    ELSE

      IF OldActive% THEN
      LINE (xOldActive%, 0)-(xOldActive% + PullMenuTopStep, 15), 2, BF
      LINE (1, 2)-(638, 16), 8, B
      LoadFont 255, MenuText$(OldActive%, 0), PullMenuTopStep \ 3 + PullMenuTopStep * (OldActive% - 1), 0, 0
      END IF

      IF Active% THEN
      Button Temp%, 2, Temp% + PullMenuTopStep, 16, 0, 8, 5, 0, 2
      LoadFont 255, MenuText$(Active%, 0), PullMenuTopStep \ 3 + PullMenuTopStep * (Active% - 1), 0, 0
      END IF

    END IF


'----------------------------------------------------------------------------
  CASE 1

  PullMenuX1% = Temp%
  PullMenuX2% = PullMenuX1% + PullMenuWidth
  PullMenuY2% = PullMenuY1 + MenuItems%(Active%) * 15 + 15


  'Drawing the menu and saving the background...
  GetPutScreen PullMenuX1%, PullMenuY1, PullMenuX2%, PullMenuY2%, 0
  LINE (PullMenuX1%, PullMenuY1)-(PullMenuX2%, PullMenuY2%), 2, BF
  LINE (PullMenuX1% + 1, PullMenuY1 + 1)-(PullMenuX2% - 1, PullMenuY2% - 1), 8, B

    FOR Counter% = 1 TO MenuItems%(Active%)
    Temp$ = MenuText$(Active%, Counter%)
      IF LEN(Temp$) THEN
        LoadFont 3, Temp$, PullMenuX1% + 7, (25 - 15) + Counter% * 15, 0
      ELSE
        Temp% = (35 - 15) + Counter% * 15
        CrackLine 0, PullMenuX1% + 7, Temp%, PullMenuX2% - 7, Temp%
      END IF
    NEXT


  'Updating menus...
    IF Active% = 3 THEN
      CIRCLE (300, 34 + 15 * ViewType%), 4, 15
      PAINT (300, 34 + 15 * ViewType%), 15
      CIRCLE (300, 79 + 15 * FileSort%), 4, 15
      PAINT (300, 79 + 15 * FileSort%), 15

    ELSEIF Active% = 4 THEN
      IF Filter% AND DirectoryAtt THEN PSET (360, PullMenuY1 + 15 * 1), 14: DRAW Tick
      IF Filter% AND ArchiveAtt THEN PSET (360, PullMenuY1 + 15 * 2), 14: DRAW Tick
      IF Filter% AND ReadOnlyAtt THEN PSET (360, PullMenuY1 + 15 * 3), 14: DRAW Tick
      IF Filter% AND HiddenAtt THEN PSET (360, PullMenuY1 + 15 * 4), 14: DRAW Tick
      IF Filter% AND SystemAtt THEN PSET (360, PullMenuY1 + 15 * 5), 14: DRAW Tick

    END IF


'----------------------------------------------------------------------------
END SELECT

END SUB

'Note:       PrgDir$ and SwitchTo$ are subject to change!
'
'
SUB QuickEXE (Program$, PrgDir$, SwitchTo$, BYVAL PressKey%, BYVAL ApplyText%)

Temp% = ChangeDirDrive%(PrgDir$)

  IF ApplyText% THEN
  SCREEN 0
  END IF

  IF FlushBuffers% THEN
  FlushDOSBuffers
  END IF

Program0$ = Program$ + ChrZero$
CMDLine0$ = CMDLine$ + ChrZero$
Temp% = SETMEM(-655360)
Temp% = Execute%(SSEG(Program0$), SADD(Program0$), SSEG(CMDLine0$), SADD(CMDLine0$))
Temp% = SETMEM(655360)
'SHELL Program$ + " " + CMDLine$

  IF ApplyText% THEN
  DosScr 1
  END IF

  IF PressKey% THEN
  SLEEP
  EmptyKeyb
  END IF

  IF ApplyText% THEN
    SCREEN 12
  ELSE
    CLS
  END IF

  IF LEN(SwitchTo$) THEN
  Temp% = ChangeDirDrive%(SwitchTo$)
  CurrentDir$ = SwitchTo$
  END IF

ERASE SelObj%
CMDLine$ = ""

ButtonsExecute 6
MouseDefaults

END SUB

SUB ReadINI

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

    ELSEIF Something$ = "DISKTOPCOLOURSET" THEN
    PalFile$ = Value$

'----------------------------------------------------------------------------
    ELSEIF Something$ = "WINDOS" THEN
    WindosPath$ = Value$

    ELSEIF Something$ = "GRAPHICS" THEN
    GraphicsPath$ = Value$
    GraphicsPathSlash$ = AddSlash$(GraphicsPath$)

    ELSEIF Something$ = "ARCHTOOLS" THEN
    ArchivesPath$ = Value$

    ELSEIF Something$ = "SCRSAVERS" THEN
    ScrPath$ = Value$

    ELSEIF Something$ = "TEMP" THEN
    TempPath$ = Value$

    ELSEIF Something$ = "FONTS" THEN
    FontsPath$ = Value$

    ELSEIF Something$ = "KEYS" THEN
    KeysPath$ = Value$

    ELSEIF Something$ = "PALETTE" THEN
    PalPath$ = Value$

'----------------------------------------------------------------------------
    ELSEIF Something$ = "CURRENTDIR" THEN
    CurrentDir$ = Value$

    ELSEIF Something$ = "FILESCROLL" THEN
    FileScroll% = Value%

    ELSEIF Something$ = "VIEWTYPE" THEN
    ViewType% = Value%

    ELSEIF Something$ = "FILESORT" THEN
    FileSort% = Value%

    ELSEIF Something$ = "FILTER" THEN
    Filter% = Value%

    ELSEIF Something$ = "DEFAULTDRIVE" THEN
    DefaultDrive$ = Value$
'----------------------------------------------------------------------------
    ELSEIF Something$ = "ANYKEY" THEN
    AnyKey% = Value%

    ELSEIF Something$ = "USECOMMANDCOM" THEN
    UseCommand% = Value%

    ELSEIF Something$ = "FLUSHBUFFERS" THEN
    FlushBuffers% = Value%
'----------------------------------------------------------------------------
    ELSEIF Something$ = "XMOUSE" THEN
    XmsSpd% = Value%

    ELSEIF Something$ = "YMOUSE" THEN
    YmsSpd% = Value%

    ELSEIF Something$ = "DOUBLECLICK" THEN
    DblClickDelay! = Value!

'----------------------------------------------------------------------------
    ELSEIF Something$ = "VIEWER" THEN
    Editor$ = Value$
'----------------------------------------------------------------------------
    END IF
  LOOP

CLOSE #File%


END SUB

SUB RememberDir (Directory$)

  IF PosMemDir% = 0 THEN
  MemDir$(0) = Directory$
  PosMemDir% = 1
  EXIT SUB
  END IF
'----------------------------------------------------------------------------

  IF PosMemDir% <= MaxMemDir THEN

    IF MemDir$(PosMemDir% - 1) <> Directory$ THEN
      MemDir$(PosMemDir%) = Directory$
        IF PosMemDir% < MaxMemDir THEN
          FOR Counter% = PosMemDir% + 1 TO MaxMemDir
          MemDir$(Counter%) = ""
          NEXT
        END IF
    PosMemDir% = PosMemDir% + 1
    END IF
'----------------------------------------------------------------------------
  ELSE

    IF MemDir$(MaxMemDir) <> Directory$ THEN
        FOR Counter% = 0 TO MaxMemDir - 1
        MemDir$(Counter%) = MemDir$(Counter% + 1)
        NEXT
      MemDir$(MaxMemDir) = Directory$
      PosMemDir% = MaxMemDir + 1
    END IF

  END IF

END SUB

SUB RunItem (File$, Extension$, BYVAL Directory%, BYVAL ProgramNum%, BYVAL PressKey%)

FileExtension$ = File$ + "." + Extension$

'============================================================================

  IF ProgramNum% = 0 THEN

      '************************
      'Is it directory or ZIP?
      '************************

      IF (Directory% = -1) OR (Extension$ = "ZIP" AND Directory% = 0 AND LenActiveZipFile% = 0) THEN
      WDChangeDir File$, Directory%
      EXIT SUB
      END IF

      '************************
      'Is it a program file?
      '************************

      IF (Extension$ = "EXE" OR Extension$ = "COM" OR Extension$ = "BAT") AND LenActiveZipFile% = 0 THEN
      FullMemEXE FileExtension$, CurrentDir$, "", PressKey%, 0, UseCommand%
      END IF

    '****************************************************
    'Otherwise skip separators, determine program index.
    '****************************************************

    REDIM Labels$(PullMenuItems - 1)

    LabelNum% = Reg.GetLabels%(AddSlash$(WindosPath$) + ExtINI, Extension$, Labels$())

      FOR ProgramNum% = 1 TO LabelNum%
        IF LEN(Labels$(ProgramNum% - 1)) THEN
        Flag% = -1
        EXIT FOR
        END IF
      NEXT

      IF Flag% = 0 THEN
      EXIT SUB
      END IF

  END IF

'============================================================================

OldCurrentDir$ = CurrentDir$


DIM State AS FMStateStruc
DIM RegFlag AS RegFlagsStruc

  IF LenActiveZipFile% THEN
    State.drive = TempPath$
    State.Dir = MID$(TempPath$, 3)
    State.DirSlash = AddSlash$(MID$(TempPath$, 3))
  ELSE
    State.drive = CurrentDir$
    State.Dir = MID$(CurrentDir$, 3)
    State.DirSlash = AddSlash$(MID$(CurrentDir$, 3))
  END IF

  IF LEN(Extension$) THEN
    State.FileExt = FileExtension$
  ELSE
    State.FileExt = File$
  END IF

State.File = File$


  IF Directory% THEN
  Extension$ = RegDirID
  END IF

  IF NOT (Reg.MakeBatch%(AddSlash$(WindosPath$) + ExtINI, Extension$, ProgramNum%, AddSlash$(TempPath$) + BatchFile, State, RegFlag)) THEN
  EXIT SUB
  END IF


'**********************************
'Decompress if file(s) are in ZIP.
'**********************************

  IF LenActiveZipFile% THEN             'CurrentDir$ may change!
  Destination$ = TempPath$              'TempPath$ must never change!
    IF NOT (UnZipFile%(FileExtension$, Destination$)) THEN
    ShowFiles 0
    EXIT SUB
    END IF
  END IF


'*******************************
'Write a list file if needed.
'*******************************

  IF RegFlag.ListOn THEN
  ListDirSlash$ = State.drive + ":" + RTRIM$(State.DirSlash)

  File% = FREEFILE

  ON LOCAL ERROR GOTO ListFileOpenError
  OPEN RTRIM$(RegFlag.ListFile) FOR OUTPUT AS #File%
  ON LOCAL ERROR GOTO 0



    IF FirstSelObject(Temp$, Temp$, Temp%) THEN

      FOR Counter% = 1 TO DirCount%
        IF SelObj%(Counter%) THEN
        ON LOCAL ERROR GOTO ListFileWriteError
        PRINT #File%, ListDirSlash$ + Directories$(Counter%)
        ON LOCAL ERROR GOTO 0
        END IF
      NEXT

      FOR Counter% = 1 TO FileCount%
        IF SelObj%(DirCount% + Counter%) THEN
        ON LOCAL ERROR GOTO ListFileWriteError
        PRINT #File%, ListDirSlash$ + Files$(Counter%) + "." + Extensions$(Counter%)
        ON LOCAL ERROR GOTO 0
        END IF
      NEXT

    ELSE

      ON LOCAL ERROR GOTO ListFileWriteError
      PRINT #File%, ListDirSlash$ + FileExtension$
      ON LOCAL ERROR GOTO 0

    END IF



  ON LOCAL ERROR GOTO ListFileWriteError
  CLOSE #File%
  ON LOCAL ERROR GOTO 0
  END IF


'*******************************
'Load a .KEY file if needed.
'*******************************

  IF RegFlag.KeysOn THEN

  KeyFile$ = RTRIM$(RegFlag.KeyFile)

    IF INSTR(KeyFile$, "\") = 0 THEN
    KeyFile$ = AddSlash$(KeysPath$) + KeyFile$
    END IF

    IF ReadDir%(KeyFile$, FindFirst, AllFilesAtt) THEN
    BLOAD KeyFile$
    END IF

  END IF


'******************************
'Execute.
'******************************

  IF RegFlag.Pause THEN
    PressKey% = -1
  ELSEIF RegFlag.DontPause THEN
    PressKey% = 0
  END IF

RunFromDir$ = State.drive + ":" + RTRIM$(State.Dir)

  '**************
  'Old format.
  '**************

  IF RegFlag.Format = RegOldFormat% THEN

    RunProg$ = RTRIM$(RegFlag.Executable)

      IF RegFlag.SwitchDir THEN
        IF ExtractPrgmDir%(RunProg$, RunFromDir$) = 0 THEN
        GOTO RunItemExit
        END IF
      END IF

    CMDLine$ = LEFT$(LTRIM$(RTRIM$(RegFlag.CommandLine)), 63)
    FullMemEXE RunProg$, RunFromDir$, OldCurrentDir$, PressKey%, 0, 0


  '**************
  'New format.
  '**************

  ELSE

    CMDLine$ = ""
    FullMemEXE AddSlash$(TempPath$) + BatchFile, RunFromDir$, OldCurrentDir$, PressKey%, 0, 0

  END IF




RunItemExit:
EXIT SUB


'****************************************************************************
'===================================SUBS=====================================
'****************************************************************************

ListFileWriteError:
CLOSE #File%        'If error on close, it WILL close here on second attempt.

ListFileOpenError:
InfoBox 6, "Error writing list file."
Temp% = CaptureInf(6)
ShowFiles 0
RESUME RunItemExit

END SUB

SUB RunSelEXE (BYVAL Quick%, BYVAL PressKey%)

  IF FirstSelObject%(File$, Extension$, Directory%) THEN
    IF Extension$ = "EXE" OR Extension$ = "COM" OR Extension$ = "BAT" THEN
    Something$ = File$ + "." + Extension$

      IF Quick% THEN
        IF Extension$ <> "BAT" THEN
        QuickEXE Something$, CurrentDir$, CurrentDir$, PressKey%, -1
        EXIT SUB
        END IF
      ELSE
        FullMemEXE Something$, CurrentDir$, CurrentDir$, PressKey%, 0, UseCommand%
      END IF

    END IF
  END IF


CMDLine$ = ""

END SUB

SUB SaveCfg (BYVAL ExpCall%)

WindosPathSlash$ = AddSlash$(WindosPath$)

'----------------------------------------------------------------------------
File1% = FREEFILE
OPEN WindosPathSlash$ + HistoryINI FOR OUTPUT AS #File1%

  FOR Counter% = 0 TO MaxMemDir
  PRINT #File1%, MemDir$(Counter%)
  NEXT

PRINT #File1%, PosMemDir%
PRINT #File1%, MemIndex%

CLOSE #File1%

'----------------------------------------------------------------------------
File1$ = WindosPathSlash$ + MainINI
OPEN File1$ FOR INPUT AS #File1%

File2% = FREEFILE
File2$ = WindosPathSlash$ + MainTMP
OPEN File2$ FOR OUTPUT AS #File2%

  DO UNTIL EOF(File1%)
  LINE INPUT #File1%, Something$
  Temp% = INSTR(Something$, "=")

    IF Temp% THEN
    Temp$ = LCASE$(RTRIM$(LTRIM$(LEFT$(Something$, Temp% - 1))))
    END IF

    IF Temp$ = "expcall" THEN EXIT DO
    IF Temp$ = "currentdir" THEN EXIT DO
    IF Temp$ = "filescroll" THEN EXIT DO
    IF Temp$ = "batch" THEN EXIT DO
    IF Temp$ = "tmppath" THEN EXIT DO
    IF Temp$ = "viewtype" THEN EXIT DO
    IF Temp$ = "filesort" THEN EXIT DO
    IF Temp$ = "filter" THEN EXIT DO
    IF Temp$ = "rundesktop" THEN EXIT DO

  PRINT #File2%, Something$
  LOOP

PRINT #File2%, "CurrentDir = "; CurrentDir$
PRINT #File2%, "FileScroll ="; FileScroll%
PRINT #File2%, "ViewType ="; ViewType%
PRINT #File2%, "FileSort ="; FileSort%
PRINT #File2%, "Filter ="; Filter%

PRINT #File2%, "ExpCall ="; ExpCall%

  IF ExpCall% THEN
  PRINT #File2%, "Batch = "; AddSlash$(TempPath$) + RunnerTmpFile
  PRINT #File2%, "TMPPath = "; TMPPath$
  END IF

CLOSE #File1%, #File2%

KILL File1$
NAME File2$ AS File1$

END SUB

SUB SelectObject (File$, Extension$, BYVAL Directory%, BYVAL xFile%, BYVAL yFile%, BYVAL Sel%)

  IF LEN(File$) = 0 THEN EXIT SUB


  IF ViewType% THEN
    LINE (xFile% - 8, yFile% - 20)-(xFile% + 94, yFile% + 3), Sel%, BF
  ELSE
    LINE (xFile% - 20, yFile% - 20)-(xFile% + 80, yFile% + 51), Sel%, BF
  END IF

'LEFT is used to eliminate long file names.
Filename$ = LEFT$(LEFT$(File$, 1) + LCASE$(MID$(File$, 2)), 12)
'----------------------------------------------------------------------------

  IF Directory% THEN

    IF ViewType% THEN
      LoadFont 0, Filename$, xFile%, yFile%, 14
    ELSE
      Icon xFile%, yFile%, 0
      LoadFont 0, Filename$, xFile%, yFile% + 35, 15
    END IF

  ELSE
'----------------------------------------------------------------------------

  Extension$ = LCASE$(Extension$)
  Filename$ = LEFT$(Filename$, 8) + "." + LEFT$(Extension$, 3)

    IF ViewType% THEN

        IF Extension$ = "exe" OR Extension$ = "com" OR Extension$ = "bat" THEN
          Colour% = 13
        ELSEIF Extension$ = "zip" THEN
          Colour% = 12
        ELSE
          Colour% = 15
        END IF
      LoadFont 0, Filename$, xFile%, yFile%, Colour%

    ELSE

        IF Extension$ = "exe" OR Extension$ = "com" OR Extension$ = "bat" THEN
          Icon xFile%, yFile%, 1
        ELSEIF Extension$ = "txt" OR Extension$ = "doc" OR Extension$ = "ini" THEN
          Icon xFile%, yFile%, 2
        ELSEIF Extension$ = "zip" OR Extension$ = "rar" OR Extension$ = "pak" OR Extension$ = "arj" OR Extension$ = "lzh" OR Extension$ = "lha" THEN
          Icon xFile%, yFile%, 3
        ELSEIF Extension$ = "mid" OR Extension$ = "rmi" OR Extension$ = "mod" OR Extension$ = "it " OR Extension$ = "s3m" OR Extension$ = "wav" OR Extension$ = "mp3" THEN
          Icon xFile%, yFile%, 4
        ELSEIF Extension$ = "bas" OR Extension$ = "pas" OR Extension$ = "asm" OR Extension$ = "c  " OR Extension$ = "cpp" THEN
          Icon xFile%, yFile%, 6
        ELSE
          Icon xFile%, yFile%, 100
        END IF
      LoadFont 0, Filename$, xFile%, yFile% + 35, 15

    END IF

  END IF

END SUB

SUB SetupCopyObj

ERASE CopyObj$
ERASE CopyObjIndex%

  FOR Counter% = 1 TO FileNum%

    IF SelObj%(Counter%) THEN
    WhatFile Counter%, File$, Extension$, Directory%
    CopyObjIndex%(Counter%) = Directory%

      IF Directory% THEN
        CopyObj$(Counter%) = File$
      ELSE
        CopyObj$(Counter%) = File$ + "." + Extension$
      END IF

    END IF

  NEXT Counter%

END SUB

SUB ShowDrives

TotalDiskNum% = LEN(Drives$)
LastX% = 640 \ TotalDiskNum%

  FOR Counter% = 1 TO TotalDiskNum%
  Button LastX% * Counter% - LastX%, 20, LastX% * Counter% - 1, 40, 8, 0, 2, 0, 0
  LoadFont 255, MID$(Drives$, Counter%, 1) + ":", (Counter% * LastX% + (Counter% - 1) * LastX% + 1) \ 2 - 7, 20, 8
  NEXT

END SUB

'ScrollType% = -1   to scroll left or up
'            =  0   to update files without scrolling
'            =  1   to scroll right or down
'            =  255 to show all files from the beginning (reset)
'
SUB ShowFiles (BYVAL ScrollType%)

  IF ScrollType% = -1 THEN
    IF FileScroll% <= 1 THEN EXIT SUB
    IF ViewType% THEN
      FileScroll% = FileScroll% - 11
    ELSE
      FileScroll% = FileScroll% - 5
    END IF

  ELSEIF ScrollType% = 1 THEN
    IF ViewType% THEN
      IF FileNum% <= FileScroll% + 54 THEN EXIT SUB
      FileScroll% = FileScroll% + 11
    ELSE
      IF FileNum% <= FileScroll% + 19 THEN EXIT SUB
      FileScroll% = FileScroll% + 5
    END IF

  ELSEIF ScrollType% = 255 THEN
    ScrollType% = 0
    FileScroll% = 1
  END IF

'============================================================================
SELECT CASE ViewType%

CASE 0       'Large icons

StartX% = 40
StartY% = 90
StepX% = 104
StepY% = 80


  IF ScrollType% = 0 THEN

    LINE (0, 55)-(564, 420), 0, BF        'Erasing old files...
      FOR Counter% = FileScroll% TO FileScroll% + 19
        IF StartX% >= 540 THEN
        StartX% = 40
        StartY% = StartY% + StepY%
        END IF
      WhatFile Counter%, Filename$, Extension$, Directory%
      SelectObject Filename$, Extension$, Directory%, StartX%, StartY%, SelObj%(Counter%)
      StartX% = StartX% + StepX%
      NEXT

  ELSEIF ScrollType% = -1 THEN

    GetPutScreen 16, 65, 543, 305, 0
    GetPutScreen 16, 145, 543, 385, -1
    LINE (16, 65)-(543, 145), 0, BF        'Erasing old files...
      FOR Counter% = FileScroll% TO FileScroll% + 4
      WhatFile Counter%, Filename$, Extension$, Directory%
      SelectObject Filename$, Extension$, Directory%, StartX%, StartY%, SelObj%(Counter%)
      StartX% = StartX% + StepX%
      NEXT

  ELSEIF ScrollType% = 1 THEN

    GetPutScreen 16, 145, 543, 385, 0
    GetPutScreen 16, 65, 543, 305, -1
    LINE (16, 305)-(543, 385), 0, BF        'Erasing old files...
      FOR Counter% = FileScroll% + 15 TO FileScroll% + 19
      WhatFile Counter%, Filename$, Extension$, Directory%
      SelectObject Filename$, Extension$, Directory%, StartX%, 90 + 3 * 80, SelObj%(Counter%)
      StartX% = StartX% + StepX%
      NEXT

  END IF

'----------------------------------------------------------------------------
CASE 1

StartX% = 8
StartY% = 80
StepX% = 112
StepY% = 32

LINE (0, 55)-(564, 420), 0, BF        'Erasing old files...

  FOR Counter% = FileScroll% TO FileScroll% + 54
    IF StartY% >= 432 THEN
    StartY% = 80
    StartX% = StartX% + StepX%
    END IF
  WhatFile Counter%, Filename$, Extension$, Directory%
  SelectObject Filename$, Extension$, Directory%, StartX%, StartY%, SelObj%(Counter%)
  StartY% = StartY% + StepY%
  NEXT

END SELECT

END SUB

SUB Sort (BYVAL SortType%)

  SELECT CASE SortType%
  CASE -1
    FirstElement% = 1
    LastElement% = DirCount%
  CASE 0, 1, 2
    FirstElement% = 1
    LastElement% = FileCount%
  CASE ELSE
    EXIT SUB
  END SELECT

'----------------------------------------------------------------------------

Span% = LastElement% \ 2

  DO WHILE Span% > 0
  Boundary% = LastElement% - Span%

    DO
    Flag% = 0

      FOR i% = FirstElement% TO Boundary%
      Temp% = i% + Span%


        SELECT CASE SortType%

        CASE -1
          IF Directories$(i%) > Directories$(Temp%) THEN
          SWAP Directories$(i%), Directories$(Temp%)
          Flag% = i%
          END IF

        CASE 0
          IF Files$(i%) > Files$(Temp%) THEN
          SWAP Files$(i%), Files$(Temp%)
          SWAP Extensions$(i%), Extensions$(Temp%)
          SWAP FileSizes&(i%), FileSizes&(Temp%)
          Flag% = i%
          END IF

        CASE 1
          IF Extensions$(i%) > Extensions$(Temp%) THEN
          SWAP Files$(i%), Files$(Temp%)
          SWAP Extensions$(i%), Extensions$(Temp%)
          SWAP FileSizes&(i%), FileSizes&(Temp%)
          Flag% = i%
          END IF

        CASE 2
          IF FileSizes&(i%) > FileSizes&(Temp%) THEN
          SWAP Files$(i%), Files$(Temp%)
          SWAP Extensions$(i%), Extensions$(Temp%)
          SWAP FileSizes&(i%), FileSizes&(Temp%)
          Flag% = i%
          END IF

        END SELECT


      NEXT

    Boundary% = Flag% - Span%
    LOOP WHILE Flag%

  Span% = Span% \ 2
  LOOP

END SUB

'Must be in graphics mode before this function is called!
'If successful then will switch to text mode.
'Current directory MAY BE CHANGED!!!
'Destination$ may be altered.
'
'The function returns: -1 if successful.
'                       0 if not successful.
'
FUNCTION UnZipFile% (Filename$, Destination$)


'***********************************
'Check for existence of the archiver.
'***********************************

  IF NOT (ZipExists%) THEN
  EXIT FUNCTION
  END IF


'***********************************
'Create a command tail for archiver.
'***********************************

CMDLine$ = "-o " + AddSlash$(CurrentDir$) + ActiveZipFile$ + ZipListFile0$


'*********************************
'Check the length of command line.
'*********************************

  IF NOT (CMDLineOk%) THEN
  EXIT FUNCTION
  END IF


'*********************************
'Change to destination directory.
'*********************************

  IF NOT (ChangeDirDrive%(Destination$)) THEN
  GOTO EndUnZipFile
  END IF

CurrentDir$ = Destination$


'*************************
'Write a ZIP list file.
'*************************

File% = FREEFILE
OPEN ZipListFile$ FOR OUTPUT AS #File%

  IF LEN(CurrentZipDir$) THEN
    ZipPath$ = CurrentZipDir$ + "\" + Filename$
  ELSE
    ZipPath$ = Filename$
  END IF

PRINT #File%, ZipPath$
CLOSE #File%


'***************************
'Run the archiver.
'***************************

SCREEN 0
Temp% = SETMEM(-655360)
Temp% = Execute%(SSEG(PkUnzip0$), SADD(PkUnzip0$), SSEG(CMDLine$), SADD(CMDLine$))
Temp% = SETMEM(655360)
UnZipFile% = -1


EndUnZipFile:
CMDLine$ = ""


END FUNCTION

SUB WDChangeDir (Directory$, BYVAL Directory%)


    IF LenActiveZipFile% THEN
    LenCurZipDir% = LEN(CurrentZipDir$)



        IF Directory$ = ".." THEN

            IF LenCurZipDir% THEN
              Path$ = CurrentZipDir$
              GOSUB PreviousDir
              CurrentZipDir$ = Path$
            ELSE
              LenActiveZipFile% = 0
            END IF

        ELSE

            IF LenCurZipDir% THEN
              CurrentZipDir$ = AddSlash$(CurrentZipDir$) + Directory$
            ELSE
              CurrentZipDir$ = Directory$
            END IF

        END IF




    ELSE




      IF Directory% THEN

        IF Directory$ = ".." THEN

          Path$ = CurrentDir$
          GOSUB PreviousDir
          CurrentDir$ = Path$

            IF LEN(CurrentDir$) = 2 THEN
            CurrentDir$ = CurrentDir$ + "\"
            END IF

        ELSE
          CurrentDir$ = AddSlash$(CurrentDir$) + Directory$
        END IF


      ELSE
        ActiveZipFile$ = Directory$ + ".ZIP"
        CurrentZipDir$ = ""
        LenActiveZipFile% = LEN(ActiveZipFile$)
      END IF




    END IF


'----------------------------------------------------------------------------

FindNeededFiles -1, 1 - MemIndex%
PrintFileSize 0, 0, 0, 1
EXIT SUB



'============================================================================
'============================================================================

PreviousDir:

  FOR Counter% = LEN(Path$) TO 1 STEP -1
  Character$ = MID$(Path$, Counter%, 1)
    IF Character$ = "\" THEN EXIT FOR
  NEXT

  IF Counter% < 1 THEN Counter% = 1

Path$ = LEFT$(Path$, Counter% - 1)
RETURN


END SUB

SUB WhatFile (BYVAL Number%, File$, Extension$, Directory%)

  IF Number% <= DirCount% THEN

  Extension$ = ""
    IF Number% >= 1 THEN
      File$ = Directories$(Number%)              'This is a directory !!!
      Directory% = -1
    ELSE
      File$ = Extension$
    END IF

  ELSE

    Number% = Number% - DirCount%
    Directory% = 0

      IF Number% <= FilesFull THEN
        File$ = Files$(Number%)                  'This is a file !!!
        Extension$ = Extensions$(Number%)
      ELSE
        File$ = ""
        Extension$ = File$
      END IF

  END IF

END SUB

FUNCTION ZipExists%

  IF ReadDir%(PkZip0$, FindFirst, AllFilesAtt) AND ReadDir%(PkUnzip0$, FindFirst, AllFilesAtt) THEN
    ZipExists% = -1
  ELSE
    InfoBox 6, "Can't find PKZIP.EXE and PKUNZIP.EXE."
    Temp% = CaptureInf%(6)
  END IF

END FUNCTION

'File$ = a ZIP file.
'ZipDir$ = directory inside a ZIP file, blank if first directory.
'
'Returns:
'           -1 if successful
'            0 if failed
'
FUNCTION ZipRead% (File$, ZipDir$)


'*********************************
'Initialize variables.
'*********************************

CONST ZipMaxBufferSize = 512

ERASE SelObj%
ERASE Files$
ERASE Extensions$
ERASE Directories$

CDHeader$ = "PK" + CHR$(1) + CHR$(2)
EndCDHeader$ = "PK"

ZipRead% = -1
FileCount% = 0
DirCount% = 1
Directories$(DirCount%) = ".."


'*********************************
'Replace "\" with "/" if found for
'compatibility with ZIP.
'*********************************

Directory$ = UCASE$(ZipDir$)

  DO

    IF BackSlash% THEN
    MID$(Directory$, BackSlash%) = "/"
    END IF

  BackSlash% = INSTR(Directory$, "\")
  LOOP WHILE BackSlash%


'*********************************
'Read the end of central directory.
'*********************************

File% = FREEFILE

ON LOCAL ERROR GOTO ZipOpenError
OPEN File$ FOR BINARY AS #File%
ON LOCAL ERROR GOTO 0

FileSize& = LOF(File%)

  IF FileSize& >= ZipMaxBufferSize THEN
    BufferSize% = ZipMaxBufferSize
  ELSE
    BufferSize% = FileSize&
  END IF

Location& = FileSize& - BufferSize% + 1



  DO

    IF Location& <= 0 OR INKEY$ = AbortZipReadKey$ THEN
    GOTO ZipReadEnd
    END IF

  Buffer$ = SPACE$(BufferSize%)

  ON LOCAL ERROR GOTO ZipGetError
  GET #File%, Location&, Buffer$
  ON LOCAL ERROR GOTO 0


  'Search the buffer backwards.
    FOR Find% = BufferSize% - 3 TO 1 STEP -1
      IF MID$(Buffer$, Find%, 4) = EndCDHeader$ THEN EXIT FOR
      IF Find% = 1 THEN Find% = 0: EXIT FOR
    NEXT


    IF Find% THEN
    Location& = Location& + Find% - 1 + 4
    ON LOCAL ERROR GOTO ZipGetError
    GET #File%, Location&, ZipFileEnd
    ON LOCAL ERROR GOTO 0
    EXIT DO
    END IF

  Location& = Location& - BufferSize% + 3
  LOOP


'PRINT ZipFileEnd.Disk
'PRINT ZipFileEnd.CentrDirDisk
'PRINT ZipFileEnd.Entries
'PRINT ZipFileEnd.TotalEntries
'PRINT ZipFileEnd.CentrDirSize
'PRINT ZipFileEnd.CentrDirOff
'PRINT ZipFileEnd.CommentLen


'*********************************
'Run some tests.
'*********************************

  IF ZipFileEnd.Disk <> ZipFileEnd.CentrDirDisk THEN
  GOTO ZipReadEnd
  END IF


'*********************************
'Read the central directory.
'*********************************

Location& = ZipFileEnd.CentrDirOff + 1


  DO UNTIL EOF(File%)

  Filename$ = SPACE$(4)
  ON LOCAL ERROR GOTO ZipGetError
  GET #File%, Location&, Filename$
  ON LOCAL ERROR GOTO 0

    IF INSTR(Filename$, CDHeader$) = 0 THEN
    EXIT DO
    END IF

  ON LOCAL ERROR GOTO ZipGetError
  GET #File%, , ZipFile
  ON LOCAL ERROR GOTO 0

    IF ZipFile.FileLen < 0 THEN
    GOTO ZipGetError
    END IF

  Filename$ = SPACE$(ZipFile.FileLen)
  ON LOCAL ERROR GOTO ZipGetError
  GET #File%, , Filename$
  ON LOCAL ERROR GOTO 0
  Filename$ = UCASE$(Filename$)


    IF INSTR(Filename$, Directory$) = 1 THEN
    Depth% = LEN(Directory$) + 1
    Slash% = INSTR(Depth%, Filename$, "/")

      IF Slash% THEN

          IF DirCount% < DirectoriesFull THEN
          Filename$ = MID$(Filename$, Depth%, Slash% - Depth%)
          Exists% = 0

            FOR Counter% = DirCount% TO 1 STEP -1
              IF Filename$ = Directories$(Counter%) THEN
              Exists% = -1
              END IF
            NEXT

            IF NOT (Exists%) THEN
            DirCount% = DirCount% + 1
            Directories$(DirCount%) = Filename$
            END IF

          END IF

      ELSE

          IF FileCount% < FilesFull THEN

            IF RIGHT$(Filename$, 1) <> "/" THEN
            FileCount% = FileCount% + 1
            Dot% = INSTR(Depth%, Filename$, ".")

              IF Dot% THEN
                Files$(FileCount%) = MID$(Filename$, Depth%, Dot% - Depth%)
                Extensions$(FileCount%) = MID$(Filename$, Dot% + 1)
              ELSE
                Files$(FileCount%) = MID$(Filename$, Depth%)
              END IF

            FileSizes&(FileCount%) = ZipFile.UncompressedSize
            END IF

          END IF

      END IF

    END IF


  Location& = Location& + 42 + ZipFile.FileLen + ZipFile.ExtraLen + ZipFile.CommentLen + 4
  LOOP



'*********************************
'Finish the job.
'*********************************

ZipReadEnd:

CLOSE #File%
FileNum% = DirCount% + FileCount%
EXIT FUNCTION


'============================================================================
'============================================================================
'============================================================================
ZipGetError:
CLOSE #File%

ZipOpenError:
ZipRead% = 0
EXIT FUNCTION

END FUNCTION

