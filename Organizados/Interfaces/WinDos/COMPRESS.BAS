'Compression Tool for WinDos.
'(C) HiTech Laboratories, Denis Andrianov.
'============================================================================

'$INCLUDE: 'mouse.bi'


DECLARE SUB MouseDefaults ()

DECLARE FUNCTION AddSlash$ (Path$)
DECLARE SUB SetPal (Attribute%, Red%, Green%, Blue%)

DECLARE SUB File2Video (Filename$)
DECLARE SUB LoadFont (Text$, BYVAL TextX%, BYVAL TextY%, BYVAL Size%, BYVAL Colour%, BYVAL Bold%)
DECLARE SUB ReadINI ()

DECLARE SUB EmptyKeyb ()

DECLARE SUB InputLine (BYVAL LocateX%, BYVAL LocateY%, BYVAL Init%, BYVAL Limit%, BYVAL StartScroll%)
DECLARE SUB ButtonShaded (BYVAL x%, BYVAL y%, BYVAL ButtonType%)
DECLARE SUB TinyButton (BYVAL x%, BYVAL y%, BYVAL x2%, BYVAL y2%, BYVAL Colour%, BYVAL TopCol%, BYVAL BotCol%, BYVAL SurfaceOn%)
DECLARE SUB TickButton (BYVAL x%, BYVAL y%, BYVAL ButtonType%)

DECLARE SUB Interface ()
DECLARE SUB TxtScrEnd ()
DECLARE SUB CButtonPressed (BYVAL XMouse%, BYVAL YMouse%)

DECLARE SUB FrameRightUpdate ()

DECLARE SUB MakeSFX ()
DECLARE SUB TestArch ()
DECLARE SUB FixArch ()
DECLARE SUB CheckArch ()
DECLARE SUB Begin ()

DECLARE SUB Defaults ()

CONST MainINI = "windos.ini"
CONST MainFnt = "windos.fnt"

CONST DOSScrFile = "dos.scr"

CONST MouseDefX = 320                'Default mouse position.
CONST MouseDefY = 240

DIM SHARED WindosPath$               'Path to WinDos directory.
DIM SHARED GraphicsPath$             'Path to directory with interface gfx.
DIM SHARED GraphicsPathSlash$
DIM SHARED FontsPath$                'Path to fonts.
DIM SHARED PalPath$                  'Palette files path.
DIM SHARED ArchPath$                 'Path to archive tools.
DIM SHARED ArchPathSlash$
DIM SHARED TempPath$                 'Path to temporary directory.
DIM SHARED Archive$                     'Compression/Decompression path.
DIM SHARED CurrentDir$               'Current directory.
DIM SHARED ListFile$                 'List file or uncompression directory.
DIM SHARED PalFile$                  'Palette file.
DIM SHARED Compress%                 'Comp/Decomp switch.
'Compress%=-1  compress
'Compress%=0   decompress
DIM SHARED Word$                     'InputLine variable.
DIM SHARED Mouse$                    'Mouse data.
DIM SHARED XmsSpd%                   'X mouse speed.
DIM SHARED YmsSpd%                   'Y mouse speed.
DIM SHARED WindosFont$(255)          'Fonts array.


DIM SHARED ScreenSlice%(5800)        'For saving portions of screen.

CONST MaxArch = 5
DIM SHARED ArchActive%
DIM SHARED ArchExist%(1 TO MaxArch) 'Indicates the presence of archive tools.
DIM SHARED ArchText$(1 TO MaxArch)  'Names of archives.
DIM SHARED ArchPrgm$(1 TO MaxArch, 3) 'Programs for compression.
'zip=1
'rar=2
'arj=3
'pak=4
'lha=5

DIM SHARED PasswordTick%
DIM SHARED DiskCompTick%
DIM SHARED SFXTick%
DIM SHARED PasswordOn%
DIM SHARED DiskCompOn%
DIM SHARED SFXOn%
DIM SHARED Password$
DIM SHARED DiskComp%

DIM SHARED SpaceChr$               'Space character


TYPE RegType
  ax    AS INTEGER
  bx    AS INTEGER
  cx    AS INTEGER
  dx    AS INTEGER
  bp    AS INTEGER
  si    AS INTEGER
  di    AS INTEGER
  flags AS INTEGER
END TYPE

DIM SHARED Regs AS RegType

'============================================================================
'Defaults

id$ = "Copyright (C) HiTech Laboratories, Denis Andrianov"

Defaults

ArchText$(1) = "ZIP"
ArchText$(2) = "RAR"
ArchText$(3) = "ARJ"
ArchText$(4) = "PAK"
ArchText$(5) = "LZH"

ArchPrgm$(1, 0) = "PKZIP.EXE"
ArchPrgm$(1, 1) = "PKUNZIP.EXE"
ArchPrgm$(1, 2) = "PKZIPFIX.EXE"
ArchPrgm$(1, 3) = "ZIP2EXE.EXE"
ArchPrgm$(2, 0) = "RAR.EXE"
ArchPrgm$(3, 0) = "ARJ.EXE"
ArchPrgm$(4, 0) = "PAK.EXE"
ArchPrgm$(5, 0) = "LHA.EXE"

ESC$ = CHR$(27)
F10$ = CHR$(0) + CHR$(68)
SpaceChr$ = " "


'============================================================================
'Initializing....

'Syntax:     COMPRESS.EXE [Drive:][Path]<Archive> [Drive:][Path]<ListFile> /C
'            COMPRESS.EXE [Drive:][Path]<Archive> [Drive:]<DecompressPath> /D
'/C = Compress
'/D = Decompress

CommandLine$ = UCASE$(LTRIM$(RTRIM$(COMMAND$)))

'REMOVE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
'CommandLine$ = "C:\MYCOP\DEFAULT C. /C"
'CommandLine$ = "A:\DEATH.ZIP D:\ /D"
'CommandLine$ = "C:\PROGRAM\PROJECTS\WINDOS\MYTEMP\CODE\INTRFACE\SEARCH\default /D"
'CHDIR "c:\program\projects\windos"
'REMOVE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  IF LEN(CommandLine$) = 0 OR CommandLine$ = "/?" OR CommandLine$ = "/H" THEN
  COLOR 2
  PRINT id$
  PRINT "Compression Tool v2.02"
  PRINT
  COLOR 12
  PRINT "This program is automatically used by WinDos."
  COLOR 7
  END
  END IF

Slash% = INSTR(CommandLine$, "/")
Space% = INSTR(CommandLine$, " ")


  IF Space% > 0 AND Slash% > Space% THEN

      SELECT CASE MID$(CommandLine$, Slash%, 2)
      CASE "/C"
        Compress% = -1
      CASE "/D"
        Compress% = 0
      CASE ELSE
        CmdError% = -1
      END SELECT

    Archive$ = LEFT$(CommandLine$, Space% - 1)
    ListFile$ = LTRIM$(RTRIM$(MID$(CommandLine$, Space% + 1, Slash% - Space% - 1)))

  ELSE
    CmdError% = -1
  END IF


  IF CmdError% THEN
  PRINT "Path is too long."
  PRINT "Press any key to continue."
  SLEEP
  END
  END IF


ReadINI                    'Loading critical information...


  IF INSTR(ListFile$, "\") = 0 THEN
  ListFile$ = AddSlash$(TempPath$) + ListFile$
  END IF


  IF NOT MouseInit% THEN
  PRINT "Mouse driver not found."
  END
  END IF

File% = FREEFILE                  'Loading main font...
OPEN AddSlash$(FontsPath$) + MainFnt FOR INPUT AS File%
  FOR Counter% = 1 TO 255
  LINE INPUT #File%, WindosFont$(Counter%)
  NEXT
CLOSE File%

CheckArch

SCREEN 12
CLS

'----------------------------------------------------------------------------
Interface

'Setting up the mouse...
MouseSpeed XmsSpd%, YmsSpd%
MouseRange 0, 0, 638, 478
MousePut 320, 240
MouseShow

'----------------------------------------------------------------------------
  DO UNTIL KH$ = ESC$ OR KH$ = F10$
  KH$ = INKEY$
  MouseStatus Lb%, Rb%, XMouse%, YMouse%
  'COLOR 15: LOCATE 1, 1: PRINT xMouse%, yMouse%


    SELECT CASE Lb%

    CASE -1

      IF ButDown% = 0 THEN
      ButDown% = 1
      MouseHide

        IF XMouse% > 20 AND XMouse% < 120 AND YMouse% > 136 AND YMouse% < 286 THEN
        CButtonPressed XMouse%, YMouse%        'CButton is pressed...

        ELSEIF XMouse% > 450 AND XMouse% < 615 AND YMouse% > 144 AND YMouse% < 160 AND PasswordTick% = 1 AND PasswordOn% = 1 THEN
        Word$ = Password$
        InputLine 57, 10, 0, 19, 19
        Password$ = Word$
        Word$ = ""

        ELSEIF XMouse% > 350 AND XMouse% < 615 AND YMouse% > 138 AND YMouse% < 162 AND PasswordOn% = 1 THEN
        PasswordTick% = 1 - PasswordTick%     'Password is ticked...
          IF PasswordTick% THEN
            Temp% = 13
          ELSE
            Temp% = 9
          END IF
        TickButton 360, 150, PasswordTick%
        LoadFont "Password:", 383, 146, 4, Temp%, 1

        ELSEIF XMouse% > 520 AND XMouse% < 560 AND YMouse% > 170 AND YMouse% < 194 AND DiskCompTick% = 1 AND DiskCompOn% = 1 AND ArchActive% = 2 THEN
        Word$ = LTRIM$(STR$(DiskComp%))
        InputLine 66, 12, 0, 4, 4
        DiskComp% = VAL(Word$)
        Word$ = ""

        ELSEIF XMouse% > 350 AND XMouse% < 615 AND YMouse% > 170 AND YMouse% < 194 AND DiskCompOn% = 1 THEN
        DiskCompTick% = 1 - DiskCompTick%    'Compress to 1.44MB is ticked...
          IF DiskCompTick% THEN
            Temp% = 13
          ELSE
            Temp% = 9
          END IF
        TickButton 360, 182, DiskCompTick%
        LoadFont "Compression using", 383, 178, 4, Temp%, 1
        LoadFont "KB files.", 568, 178, 4, Temp%, 1

        ELSEIF XMouse% > 350 AND XMouse% < 615 AND YMouse% > 202 AND YMouse% < 226 AND SFXOn% = 1 THEN
        SFXTick% = 1 - SFXTick%               'Convert to SFX is ticked...
          IF SFXTick% THEN
            Temp% = 13
          ELSE
            Temp% = 9
          END IF
        TickButton 360, 214, SFXTick%
        LoadFont "Create EXE when finished.", 383, 210, 4, Temp%, 1

        ELSEIF XMouse% > 190 AND XMouse% < 310 AND YMouse% > 420 AND YMouse% < 460 THEN
        ButPrs% = 1                        'Begin is pressed...
        TinyButton 190, 420, 310, 460, 7, 0, 15, 0

        ELSEIF XMouse% > 330 AND XMouse% < 450 AND YMouse% > 420 AND YMouse% < 460 THEN
        ButPrs% = 2                         'Done is pressed...
        TinyButton 330, 420, 450, 460, 7, 0, 15, 0

        ELSEIF XMouse% > 300 AND XMouse% < 400 AND YMouse% > 310 AND YMouse% < 350 AND Compress% = 0 THEN
        ButPrs% = 3                          'Test Archive is pressed...
        TinyButton 300, 310, 400, 350, 4, 0, 15, -1
        LoadFont "Test Archive", 314, 326, 4, 13, 1

        ELSEIF XMouse% > 405 AND XMouse% < 505 AND YMouse% > 310 AND YMouse% < 350 AND Compress% = 0 AND ArchActive% < 4 THEN
        ButPrs% = 4                           'Fix Archive is pressed...
        TinyButton 405, 310, 505, 350, 4, 0, 15, -1
        LoadFont "Fix Archive", 422, 326, 4, 13, 1

        ELSEIF XMouse% > 520 AND XMouse% < 620 AND YMouse% > 310 AND YMouse% < 350 AND Compress% = 0 THEN
        ButPrs% = 5                            'Create EXE is pressed...
        TinyButton 520, 310, 620, 350, 4, 0, 15, -1
        LoadFont "Create EXE now", 527, 326, 4, 13, 1

        ELSEIF XMouse% > 63 AND XMouse% < 575 AND YMouse% > 384 AND YMouse% < 400 THEN
          IF Compress% THEN
            Word$ = Archive$
            InputLine 9, 25, 0, 63, 63
            Archive$ = Word$
          ELSE
            Word$ = ListFile$
            InputLine 9, 25, 0, 63, 63
            ListFile$ = Word$
          END IF
        Word$ = ""
        END IF

      MouseShow
      END IF


    CASE 0
    ButDown% = 0

      IF ButPrs% > 0 THEN
      MouseHide

        IF ButPrs% = 1 THEN                      'Begin is released
        Begin

        ELSEIF ButPrs% = 2 THEN                  'Done is released
        MouseShow
        EXIT DO

        ELSEIF ButPrs% = 3 THEN                  'Test Archive is released
        TestArch

        ELSEIF ButPrs% = 4 THEN                  'Fix Archive is released
        FixArch

        ELSEIF ButPrs% = 5 THEN                  'Create EXE is released
        SCREEN 0
        MakeSFX
        TxtScrEnd
        Defaults
        END IF

      ButPrs% = 0
      MouseShow
      END IF

    END SELECT

  LOOP


MouseHide
CLS

FUNCTION AddSlash$ (Path$)

  IF RIGHT$(Path$, 1) <> "\" THEN
    AddSlash$ = Path$ + "\"
  ELSE
    AddSlash$ = Path$
  END IF

END FUNCTION

SUB Begin

SCREEN 0

'============================================================================

  IF ArchActive% = 1 THEN           'ZIP

    IF PasswordTick% THEN
    AddOns$ = "-s" + Password$ + SpaceChr$
    END IF

    IF Compress% THEN
      SHELL ArchPathSlash$ + ArchPrgm$(1, 0) + " -ex -Pr " + AddOns$ + Archive$ + " @" + ListFile$
    ELSE
      SHELL ArchPathSlash$ + ArchPrgm$(1, 1) + " -d " + AddOns$ + Archive$ + SpaceChr$ + ListFile$
    END IF

    IF SFXTick% THEN
    MakeSFX
    END IF

'----------------------------------------------------------------------------
  ELSEIF ArchActive% = 2 THEN       'RAR

    IF PasswordTick% THEN
    AddOns$ = "-p" + Password$ + SpaceChr$
    END IF

    IF DiskCompTick% THEN
      IF Compress% THEN
      AddOns$ = AddOns$ + "-v" + LTRIM$(STR$(DiskComp%)) + SpaceChr$
      END IF
    END IF

    IF Compress% THEN
      SHELL ArchPathSlash$ + ArchPrgm$(2, 0) + " a -m5 -r " + AddOns$ + Archive$ + " @" + ListFile$
    ELSE
      SHELL ArchPathSlash$ + ArchPrgm$(2, 0) + " x " + AddOns$ + Archive$ + SpaceChr$ + ListFile$
    END IF

    IF SFXTick% THEN
    MakeSFX
    END IF

'----------------------------------------------------------------------------
  ELSEIF ArchActive% = 3 THEN       'ARJ

    IF PasswordTick% THEN
    AddOns$ = "-g" + Password$ + SpaceChr$
    END IF

    IF DiskCompTick% THEN
      IF Compress% THEN
        AddOns$ = AddOns$ + "-va "
      ELSE
        AddOns$ = AddOns$ + "-v "
      END IF
    END IF

    IF Compress% THEN
      SHELL ArchPathSlash$ + ArchPrgm$(3, 0) + " a -r -jm " + AddOns$ + Archive$ + " !" + ListFile$
    ELSE
      SHELL ArchPathSlash$ + ArchPrgm$(3, 0) + " x -y " + AddOns$ + Archive$ + SpaceChr$ + ListFile$
    END IF

    IF SFXTick% THEN
    MakeSFX
    END IF

'----------------------------------------------------------------------------
  ELSEIF ArchActive% = 4 THEN       'PAK

    IF Compress% THEN
      SHELL ArchPathSlash$ + ArchPrgm$(4, 0) + " a /cr /i " + Archive$ + " @" + ListFile$
    ELSE
      SHELL ArchPathSlash$ + ArchPrgm$(4, 0) + " e " + Archive$ + SpaceChr$ + ListFile$
    END IF

    IF SFXTick% THEN
    MakeSFX
    END IF

'----------------------------------------------------------------------------
  ELSEIF ArchActive% = 5 THEN       'LHA

    IF Compress% THEN
      SHELL ArchPathSlash$ + ArchPrgm$(5, 0) + " a -r -x " + Archive$ + " @" + ListFile$
    ELSE
      SHELL ArchPathSlash$ + ArchPrgm$(5, 0) + " x " + Archive$ + SpaceChr$ + AddSlash$(ListFile$)
    END IF

    IF SFXTick% THEN
    MakeSFX
    END IF

'----------------------------------------------------------------------------

  END IF


'***************************
'Save DOS screen and exit.
'***************************

DEF SEG = &HB800
BSAVE AddSlash$(TempPath$) + DOSScrFile, 0, &HFA0
DEF SEG

END


END SUB

'ButtonType% = 0 to create a released button
'            = 1 to create a pressed button
'
SUB ButtonShaded (BYVAL x%, BYVAL y%, BYVAL ButtonType%)

Size% = 10


  IF ButtonType% THEN

    FOR Counter% = 13 TO 3 STEP -2
    LINE (x% - Size%, y% - Size%)-(x% + Size%, y% + Size%), Counter%, BF
    Size% = Size% - 1
    NEXT

  ELSE

    FOR Counter% = 3 TO 13 STEP 2
    LINE (x% - Size%, y% - Size%)-(x% + Size%, y% + Size%), Counter%, BF
    Size% = Size% - 1
    NEXT

  END IF


END SUB

SUB CButtonPressed (BYVAL XMouse%, BYVAL YMouse%)

CButNum% = (YMouse% - 136) \ 30 + 1  'Calculating which CButton was pressed...

  IF ArchActive% = CButNum% THEN EXIT SUB
  IF ArchExist%(CButNum%) = 0 THEN EXIT SUB

ArchActive% = CButNum%

'----------------------------------------------------------------------------
yCButPos% = 30 * (CButNum% - 1) + 150

  FOR yCounter% = 150 TO 270 STEP 30
  ButtonShaded 50, yCounter%, 0
  NEXT

ButtonShaded 50, yCButPos%, 1

  FOR i% = 1 TO 5
  LoadFont ArchText$(i%), 90, 146 + 30 * (i% - 1), 4, 9, 1
  NEXT

LoadFont ArchText$(CButNum%), 90, 146 + 30 * (CButNum% - 1), 4, 13, 1

FrameRightUpdate

PasswordTick% = 0
DiskCompTick% = 0
SFXTick% = 0
Password$ = ""

END SUB

SUB CheckArch

ERASE ArchExist%

'----------------------------------------------------------------------------
'If there is no ARCHIVES path, assume that the archivers are in DOS PATH.

  IF LEN(ArchPath$) = 0 THEN
    FOR ArchNum% = 1 TO 5
    ArchExist%(ArchNum%) = 1
    NEXT
  EXIT SUB
  END IF

'----------------------------------------------------------------------------

PkZip% = LEN(DIR$(ArchPathSlash$ + ArchPrgm$(1, 0)))
PkUnzip% = LEN(DIR$(ArchPathSlash$ + ArchPrgm$(1, 1)))

  IF PkZip% THEN
    IF PkUnzip% THEN
    ArchExist%(1) = 1
    END IF
  END IF

'----------------------------------------------------------------------------

  FOR ArchNum% = 2 TO 5

    IF LEN(DIR$(ArchPathSlash$ + ArchPrgm$(ArchNum%, 0))) THEN
    ArchExist%(ArchNum%) = 1
    END IF

  NEXT


END SUB

SUB Defaults

Password$ = ""
PasswordTick% = 0

DiskComp% = 1400
DiskCompTick% = 0

SFXTick% = 0

END SUB

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

SUB FixArch

  IF ArchActive% < 4 THEN
  SCREEN 0
  SHELL LEFT$(ListFile$, 2)
  SHELL "CD " + RIGHT$(ListFile$, LEN(ListFile$) - 2)

    IF ArchActive% = 1 THEN      'ZIP
    SHELL ArchPathSlash$ + ArchPrgm$(1, 2) + SpaceChr$ + Archive$

    ELSEIF ArchActive% = 2 THEN  'RAR
    SHELL ArchPathSlash$ + ArchPrgm$(2, 0) + " r " + Archive$

    ELSEIF ArchActive% = 3 THEN  'ARJ
    SHELL ArchPathSlash$ + ArchPrgm$(3, 0) + " y -jr " + Archive$
    END IF

  CHDIR CurrentDir$
  CHDRIVE CurrentDir$
  TxtScrEnd
  Defaults
  END IF

END SUB

SUB FrameRightUpdate

PUT (350, 140), ScreenSlice%, PSET


  IF ArchActive% <> 5 AND ArchActive% <> 4 THEN
    TickButton 360, 150, 0
    LoadFont "Password:", 383, 146, 4, 9, 1
    InputLine 57, 10, 1, 0, 19
    PasswordOn% = 1
  ELSE
    PasswordOn% = 0
  END IF


  IF ArchActive% = 2 OR ArchActive% = 3 THEN
    TickButton 360, 182, 0
    LoadFont "Compression using", 383, 178, 4, 9, 1
    LOCATE 12, 66
      IF ArchActive% = 2 THEN
        PRINT LTRIM$(STR$(DiskComp%)) + " "
      ELSE
        PRINT "AUTO "
      END IF
    LoadFont "KB files.", 568, 178, 4, 9, 1
    DiskCompOn% = 1
  ELSE
    DiskCompOn% = 0
  END IF


TickButton 360, 214, 0
LoadFont "Create EXE when finished.", 383, 210, 4, 9, 1
SFXOn% = 1

END SUB

SUB InputLine (BYVAL LocateX%, BYVAL LocateY%, BYVAL Init%, BYVAL Limit%, BYVAL StartScroll%)

'limit% = 63
'StartScroll% = 5
'locatex% = 25
'locatey% = 12
'word$ = ""

  IF Init% = 1 THEN
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

SUB Interface

'****************************
'Set up black palette.
'****************************

FOR i% = 0 TO 15
SetPal i%, 0, 0, 0
NEXT


'****************************
'Load interface.
'****************************

'DEF SEG = &HA000

OUT &H3C4, 2
OUT &H3C5, 1
'BLOAD GraphicsPathSlash$ + "compress.im1", 0
File2Video GraphicsPathSlash$ + "compress.im1"

OUT &H3C4, 2
OUT &H3C5, 2
'BLOAD GraphicsPathSlash$ + "compress.im2", 0
File2Video GraphicsPathSlash$ + "compress.im2"

OUT &H3C4, 2
OUT &H3C5, 4
'BLOAD GraphicsPathSlash$ + "compress.im3", 0
File2Video GraphicsPathSlash$ + "compress.im3"

OUT &H3C4, 2
OUT &H3C5, 8
'BLOAD GraphicsPathSlash$ + "compress.im4", 0
File2Video GraphicsPathSlash$ + "compress.im4"

'DEF SEG


'****************************
'Set up palette.
'****************************

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


'****************************
'Save empty right frame.
'****************************

GET (350, 140)-(620, 224), ScreenSlice%


'****************************
'Draw in left frame.
'****************************

  FOR i% = 1 TO MaxArch               'Choose a default archiver.
    IF ArchExist%(i%) THEN
    ArchActive% = i%
    EXIT FOR
    END IF
  NEXT


  IF NOT Compress% THEN               'Apply correct archiver for extension.
    FOR i% = 1 TO MaxArch
      IF "." + ArchText$(i%) = RIGHT$(Archive$, 4) THEN
        IF ArchExist%(i%) THEN
        ArchActive% = i%
        END IF
      END IF
    NEXT
  END IF



LoadFont "Choose compression method:", 70, 110, 4, 14, 1


  FOR i% = 1 TO MaxArch
  yPos% = (i% - 1) * 30
  yText% = yPos% + 146
  yBut% = yPos% + 150

    IF ArchActive% = i% THEN
      Temp% = 13
      ButtonShaded 50, yBut%, 1
    ELSE
      Temp% = 9
      ButtonShaded 50, yBut%, 0
    END IF

  LoadFont ArchText$(i%), 90, yText%, 4, Temp%, 1
  NEXT



'****************************
'Draw in right frame.
'****************************

LoadFont "Options:", 460, 110, 4, 14, 1
FrameRightUpdate



'****************************
'Finish off the rest.
'****************************


  IF NOT Compress% THEN
  TinyButton 300, 310, 400, 350, 4, 15, 0, -1
  LoadFont "Test Archive", 313, 325, 4, 13, 1
  TinyButton 405, 310, 505, 350, 4, 15, 0, -1
  LoadFont "Fix Archive", 421, 325, 4, 13, 1
  TinyButton 520, 310, 620, 350, 4, 15, 0, -1
  LoadFont "Create EXE now", 526, 325, 4, 13, 1
  END IF


LoadFont "Compress / Decompress to (full path):", 205, 360, 4, 14, 1
InputLine 9, 25, 1, 63, 63
LOCATE 25, 9

  IF Compress% THEN
    PRINT Archive$
  ELSE
    PRINT ListFile$
  END IF

END SUB

SUB LoadFont (Text$, BYVAL TextX%, BYVAL TextY%, BYVAL Size%, BYVAL Colour%, BYVAL Bold%)

DRAW "S" + LTRIM$(STR$(Size%))

real.bold% = Bold%

  DO WHILE Bold% > -1
  DRAW "BM " + LTRIM$(STR$(TextX%)) + "," + LTRIM$(STR$(TextY%)) + " C" + LTRIM$(STR$(Colour%))
    FOR k% = 1 TO LEN(Text$)
    Char$ = MID$(Text$, k%, 1)
    DRAW WindosFont$(ASC(Char$))
      IF real.bold% > 0 THEN DRAW "BR1"
    NEXT k%
  Bold% = Bold% - 1
  TextX% = TextX% + 1
  LOOP

END SUB

SUB MakeSFX

  IF ((INSTR(Archive$, "\") = 0 OR INSTR(Archive$, ":") = 0) AND Compress% = -1) OR ((INSTR(ListFile$, "\") = 0 OR INSTR(ListFile$, ":") = 0) AND Compress% = 0) THEN
  PRINT
  PRINT "Full path is needed to create an EXE!"
  EXIT SUB
  END IF



  IF Compress% THEN

      FOR Counter% = LEN(Archive$) TO 1 STEP -1
      Char$ = MID$(Archive$, Counter%, 1)
        IF Char$ = "\" THEN EXIT FOR
      NEXT

    SFXPath$ = LEFT$(Archive$, Counter% - 1)

      IF LEN(SFXPath$) = 2 THEN
      SFXPath$ = SFXPath$ + "\"
      END IF

    SHELL LEFT$(SFXPath$, 2)
    SHELL "CD " + RIGHT$(SFXPath$, LEN(SFXPath$) - 2)


  ELSE

    SHELL LEFT$(ListFile$, 2)
    SHELL "CD " + RIGHT$(ListFile$, LEN(ListFile$) - 2)

  END IF

'----------------------------------------------------------------------------
  IF ArchActive% = 1 THEN       'ZIP
  SHELL ArchPathSlash$ + ArchPrgm$(1, 3) + SpaceChr$ + Archive$

  ELSEIF ArchActive% = 2 THEN   'RAR
  SHELL ArchPathSlash$ + ArchPrgm$(2, 0) + " s " + Archive$

  ELSEIF ArchActive% = 3 THEN   'ARJ
  SHELL ArchPathSlash$ + ArchPrgm$(3, 0) + " y -je " + Archive$

  ELSEIF ArchActive% = 4 THEN   'PAK
  SHELL ArchPathSlash$ + ArchPrgm$(4, 0) + " a /EXE " + Archive$

  ELSEIF ArchActive% = 5 THEN   'LHA
  SHELL ArchPathSlash$ + ArchPrgm$(5, 0) + " s -r -x " + Archive$
  END IF
'----------------------------------------------------------------------------

CHDIR CurrentDir$
CHDRIVE CurrentDir$

END SUB

SUB MouseDefaults

MousePut MouseDefX, MouseDefY
MouseSpeed XmsSpd%, YmsSpd%
MouseRange 0, 0, 638, 478

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
    IF Something$ = "WINDOS" THEN
    WindosPath$ = Value$

    ELSEIF Something$ = "GRAPHICS" THEN
    GraphicsPath$ = Value$
    GraphicsPathSlash$ = AddSlash$(GraphicsPath$)

    ELSEIF Something$ = "ARCHTOOLS" THEN
    ArchPath$ = Value$
    ArchPathSlash$ = AddSlash$(ArchPath$)

    ELSEIF Something$ = "TEMP" THEN
    TempPath$ = Value$

    ELSEIF Something$ = "FONTS" THEN
    FontsPath$ = Value$

    ELSEIF Something$ = "PALETTE" THEN
    PalPath$ = Value$

'----------------------------------------------------------------------------
    ELSEIF Something$ = "XMOUSE" THEN
    XmsSpd% = Value%

    ELSEIF Something$ = "YMOUSE" THEN
    YmsSpd% = Value%

    ELSEIF Something$ = "COMPRESSCOLOURSET" THEN
    PalFile$ = Value$

'----------------------------------------------------------------------------
    ELSEIF Something$ = "TMPPATH" THEN
    CurrentDir$ = Value$
    CHDIR CurrentDir$
    CHDRIVE CurrentDir$
'----------------------------------------------------------------------------
    END IF
  LOOP

CLOSE #File%


END SUB

SUB SetPal (Attribute%, Red%, Green%, Blue%)

OUT &H3C8, Attribute%
OUT &H3C9, Red%
OUT &H3C9, Green%
OUT &H3C9, Blue%

END SUB

SUB TestArch

  IF ArchActive% > 0 THEN
  SCREEN 0

    IF ArchActive% = 1 THEN                'ZIP
        IF PasswordTick% THEN
        AddOns$ = "-s" + Password$ + SpaceChr$
        END IF
      SHELL ArchPathSlash$ + ArchPrgm$(1, 1) + " -t " + AddOns$ + Archive$

    ELSEIF ArchActive% = 2 THEN            'RAR
        IF PasswordTick% THEN
        AddOns$ = "-p" + Password$ + SpaceChr$
        END IF
      SHELL ArchPathSlash$ + ArchPrgm$(2, 0) + " t " + AddOns$ + Archive$

    ELSEIF ArchActive% = 3 THEN            'ARJ
        IF PasswordTick% THEN
        AddOns$ = "-g" + Password$ + SpaceChr$
        END IF
      SHELL ArchPathSlash$ + ArchPrgm$(3, 0) + " t " + AddOns$ + Archive$

    ELSEIF ArchActive% = 4 THEN            'PAK
      SHELL ArchPathSlash$ + ArchPrgm$(4, 0) + " t " + Archive$

    ELSEIF ArchActive% = 5 THEN            'LZH
      SHELL ArchPathSlash$ + ArchPrgm$(5, 0) + " t " + Archive$
    END IF

  TxtScrEnd
  Defaults
  END IF

END SUB

SUB TickButton (BYVAL x%, BYVAL y%, BYVAL ButtonType%)

Size% = 10


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

SUB TinyButton (BYVAL x%, BYVAL y%, BYVAL x2%, BYVAL y2%, BYVAL Colour%, BYVAL TopCol%, BYVAL BotCol%, BYVAL SurfaceOn%)

  IF SurfaceOn% THEN
  LINE (x%, y%)-(x2%, y2%), Colour%, BF      'Drawing main rectangle...
  END IF

LINE (x%, y%)-(x2%, y%), TopCol%
LINE (x%, y%)-(x%, y2%), TopCol%

LINE (x2%, y%)-(x2%, y2%), BotCol%
LINE (x%, y2%)-(x2%, y2%), BotCol%

END SUB

SUB TxtScrEnd

PRINT
COLOR 15
PRINT "Press any key to continue . . ."
SLEEP
SCREEN 12
Interface
MouseSpeed XmsSpd%, YmsSpd%
MousePut 320, 240

END SUB

