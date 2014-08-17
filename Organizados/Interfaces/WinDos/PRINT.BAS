'Copyright (C) HiTech Laboratories, Denis Andrianov
'Print Utility for WinDos
'============================================================================

'$INCLUDE: 'mouse.bi'
'$INCLUDE: 'dos.bi'


DECLARE SUB ReadINI ()

DECLARE SUB Interface ()
DECLARE SUB UpdatePortWindow (BYVAL CButNum%, BYVAL Rst%)
DECLARE SUB UpdatePrnWindow (BYVAL CButNum%, BYVAL Rst%)

DECLARE SUB LoadFont (Text$, Textx%, Texty%, Size%, Bold%, Colour%)
DECLARE SUB ButtonShaded (BYVAL x%, BYVAL y%, BYVAL ButtonType%)
DECLARE SUB TinyButton (BYVAL x%, BYVAL y%, BYVAL x2%, BYVAL y2%, BYVAL TopCol%, BYVAL BotCol%)
DECLARE SUB CButtonPressed (XMouse%, YMouse%)
DECLARE SUB InputLine (BYVAL LocateX%, BYVAL LocateY%, BYVAL Init%, BYVAL Limit%, BYVAL StartScroll%)

DECLARE SUB SetPal (Attribute%, Red%, Green%, Blue%)
DECLARE SUB File2Video (Filename$)

DECLARE FUNCTION AddSlash$ (Path$)
DECLARE SUB EmptyKeyb ()
DECLARE SUB PrintText (Source%)




CONST MainINI = "windos.ini"
CONST MainFNT = "windos.fnt"


DIM SHARED GraphicsPath$
DIM SHARED GraphicsPathSlash$
DIM SHARED FontsPath$           'Fonts path.
DIM SHARED PalPath$
DIM SHARED PalFile$             'Colour set file.
DIM SHARED XmsSpd%              'X mouse speed.
DIM SHARED YmsSpd%              'Y mouse speed.

DIM SHARED WindosFont$(255)

DIM SHARED Word$                'Used in InputLine.
DIM SHARED WhichCButton%

DIM SHARED Filename$
DIM SHARED DirName$
DIM SHARED DirOptions$
DIM SHARED Port$
DIM SHARED LPT1$
DIM SHARED LPT2$
DIM SHARED LPT3$


DIM SHARED Regs AS RegType


'============================================================================
'Defaults...

id$ = "Copyright (C) HiTech Laboratories, Denis Andrianov"
Version$ = "2.11"

ESC$ = CHR$(27)
F10$ = CHR$(0) + CHR$(68)

Port$ = "lpt1:"
LPT1$ = "LPT1 port"
LPT2$ = "LPT2 port"
LPT3$ = "LPT3 port"
Filename$ = COMMAND$



'*******************************
'Initialize the mouse.
'*******************************

  IF NOT MouseInit% THEN
  PRINT "Mouse driver not found."
  END
  END IF


'*******************************
'Load configuration and font.
'*******************************

ReadINI

File% = FREEFILE
OPEN AddSlash$(FontsPath$) + MainFNT FOR INPUT AS File%
  FOR Counter% = 1 TO 255
  LINE INPUT #File%, WindosFont$(Counter%)
  NEXT Counter%
CLOSE File%


'*********************************
'Load the interface, set up mouse.
'*********************************

Interface

MouseSpeed XmsSpd%, YmsSpd%
MouseRange 0, 0, 638, 478
MouseShow


'============================================================================

  DO
  KH$ = INKEY$
  MouseStatus Lb%, Rb%, XMouse%, YMouse%
  'LOCATE 1, 1: PRINT xMouse%, yMouse%

    IF KH$ = ESC$ OR KH$ = F10$ THEN
    MouseHide
    EXIT DO
    END IF



    SELECT CASE Lb%

    CASE -1

      IF ButDown% = 0 THEN
      ButDown% = 1
      MouseHide

        IF XMouse% > 352 AND XMouse% < 608 AND YMouse% > 176 AND YMouse% < 192 THEN
        Word$ = Filename$  'Filename box clicked...
        InputLine 45, 12, 0, 76, 31
        Filename$ = Word$

        ELSEIF XMouse% > 352 AND XMouse% < 608 AND YMouse% > 256 AND YMouse% < 272 THEN
        Word$ = DirName$   'Directory box clicked...
        InputLine 45, 17, 0, 63, 31
        DirName$ = Word$

        ELSEIF XMouse% > 350 AND XMouse% < 608 AND YMouse% > 320 AND YMouse% < 336 THEN
        Word$ = DirOptions$ 'Dir Options box clicked...
        InputLine 45, 21, 0, 31, 31
        DirOptions$ = Word$

        ELSEIF XMouse% > 30 AND XMouse% < 610 AND YMouse% > 130 AND YMouse% < 336 THEN
        CButtonPressed XMouse%, YMouse%     'CButton is pressed...

        ELSEIF XMouse% > 190 AND XMouse% < 310 AND YMouse% > 420 AND YMouse% < 460 THEN
        ButPrs% = 1       'Print is pressed...
        TinyButton 190, 420, 310, 460, 0, 15

        ELSEIF XMouse% > 330 AND XMouse% < 450 AND YMouse% > 420 AND YMouse% < 460 THEN
        ButPrs% = 2       'Exit is pressed...
        TinyButton 330, 420, 450, 460, 0, 15
        END IF

      MouseShow
      END IF


    CASE 0
    ButDown% = 0

      IF ButPrs% > 0 THEN
      MouseHide

        IF ButPrs% = 1 THEN                      'Print is released
        SCREEN 0
        CLS
        COLOR 2
        PRINT "Printing . . ."
        PRINT STRING$(80, 205)
        PrintText WhichCButton%
        EXIT DO

        ELSEIF ButPrs% = 2 THEN                  'Exit is released
        EXIT DO
        END IF

      ButPrs% = 0
      MouseShow
      END IF

    END SELECT

  LOOP


CLS

FUNCTION AddSlash$ (Path$)

  IF RIGHT$(Path$, 1) <> "\" THEN
    AddSlash$ = Path$ + "\"
  ELSE
    AddSlash$ = Path$
  END IF

END FUNCTION

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

SUB CButtonPressed (XMouse%, YMouse%)


SELECT CASE XMouse%
'---------------------------------------------------------------------------
CASE IS < 200

CButNum% = (YMouse% - 132) \ 40     'Calculating which CButton was pressed...

  IF CButNum% > 2 THEN EXIT SUB

UpdatePortWindow CButNum%, 0

'---------------------------------------------------------------------------
CASE IS > 280

  IF YMouse% < 210 THEN
    WhichCButton% = 0
  ELSE
    WhichCButton% = 1
  END IF

UpdatePrnWindow WhichCButton%, 0

'---------------------------------------------------------------------------

END SELECT

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

SCREEN 12
CLS

'****************************
'Set up black palette.
'****************************

  FOR i% = 0 TO 15
  SetPal i%, 0, 0, 0
  NEXT


'****************************
'Load interface.
'****************************

OUT &H3C4, 2
OUT &H3C5, 1
File2Video GraphicsPathSlash$ + "print.im1"

OUT &H3C4, 2
OUT &H3C5, 2
File2Video GraphicsPathSlash$ + "print.im2"

OUT &H3C4, 2
OUT &H3C5, 4
File2Video GraphicsPathSlash$ + "print.im3"

OUT &H3C4, 2
OUT &H3C5, 8
File2Video GraphicsPathSlash$ + "print.im4"


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


'*****************************
'Set up the rest.
'*****************************

LoadFont "Select printer port:", 53, 100, 4, 1, 14
UpdatePortWindow 2, -1
UpdatePortWindow 1, 0
UpdatePortWindow 0, 0

UpdatePrnWindow 1, -1
UpdatePrnWindow 0, 0

  IF LEN(Filename$) = 0 THEN
  UpdatePrnWindow 1, 0
  END IF


Word$ = Filename$
InputLine 45, 12, 1, 0, 31
Word$ = DirName$
InputLine 45, 17, 1, 0, 31
Word$ = DirOptions$
InputLine 45, 21, 1, 0, 31



'********************************
'Estimate the number of A4 pages.
'********************************

File% = FREEFILE
ON LOCAL ERROR GOTO OpenError
OPEN Filename$ FOR INPUT AS #File% LEN = 4096
ON LOCAL ERROR GOTO 0


  DO UNTIL EOF(File%)
  LineCounter& = LineCounter& + 1

  ON LOCAL ERROR GOTO ReadError
  LINE INPUT #File%, junk$
  ON LOCAL ERROR GOTO 0

    IF INKEY$ = CHR$(27) THEN EXIT DO
  LOOP


CLOSE #File%


Pages& = LineCounter& \ 65

  IF LineCounter& MOD 65 THEN
  Pages& = Pages& + 1
  END IF

LoadFont "Document info:", 68, 298, 4, 1, 14
LoadFont "A4 pages:", 30, 335, 4, 1, 13
LoadFont LTRIM$(STR$(Pages&)), 140, 335, 4, 1, 13
LoadFont "Number of lines:", 30, 350, 4, 1, 13
LoadFont LTRIM$(STR$(LineCounter&)), 140, 350, 4, 1, 13


'*****************************
'Set up mouse.
'*****************************

AtMouse:

MousePut 320, 240
EXIT SUB



'============================================================================
ReadError:
CLOSE #File%

OpenError:
RESUME AtMouse

END SUB

SUB LoadFont (Text$, Textx%, Texty%, Size%, Bold%, Colour%)

DRAW "S" + STR$(Size%)

RealBold% = Bold%

  WHILE Bold% > -1
  DRAW "BM " + LTRIM$(STR$(Textx%)) + "," + LTRIM$(STR$(Texty%)) + " C" + LTRIM$(STR$(Colour%))
    FOR k% = 1 TO LEN(Text$)
    char$ = MID$(Text$, k%, 1)
    DRAW WindosFont$(ASC(char$))
      IF RealBold% > 0 THEN DRAW "BR1"
    NEXT k%
  Bold% = Bold% - 1
  Textx% = Textx% + 1
  WEND

END SUB

SUB PrintText (Source%)

COLOR 7


  SELECT CASE Source%

'----------------------------------------------------------------------------

  CASE 0

    Printer% = FREEFILE
    ON LOCAL ERROR GOTO PortOpenError
    OPEN Port$ FOR OUTPUT AS #Printer%
    ON ERROR GOTO 0

    File% = FREEFILE
    ON LOCAL ERROR GOTO FileOpenError
    OPEN Filename$ FOR INPUT AS #File%
    ON LOCAL ERROR GOTO 0


      DO

        FOR Lines% = 1 TO 65
        ON LOCAL ERROR GOTO PrintFileError
        LINE INPUT #File%, junk$
          IF EOF(File%) THEN EXIT DO
        PRINT #Printer%, junk$
        ON LOCAL ERROR GOTO 0
        PRINT junk$
        NEXT

      PRINT
      PRINT STRING$(80, 196)
      PRINT

      ON LOCAL ERROR GOTO PrintFileError
      PRINT #Printer%, CHR$(12)
      ON LOCAL ERROR GOTO 0

      LOOP WHILE INKEY$ <> CHR$(27)


'----------------------------------------------------------------------------
  CASE ELSE

    Temp$ = "dir " + DirOptions$ + " " + DirName$
    SHELL Temp$
    SHELL Temp$ + " >" + Port$

'----------------------------------------------------------------------------
  END SELECT



PrintTextEnd:
EXIT SUB


'============================================================================
PrintFileError:
CLOSE #Printer%, #File%
RESUME PrintTextEnd

FileOpenError:
CLOSE #Printer%

PortOpenError:
RESUME PrintTextEnd

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
    Value% = VAL(Value$)
    Something$ = RTRIM$(LEFT$(Something$, Equal% - 1))
    END IF

'----------------------------------------------------------------------------
    IF Something$ = "FONTS" THEN
    FontsPath$ = Value$

    ELSEIF Something$ = "GRAPHICS" THEN
    GraphicsPath$ = Value$
    GraphicsPathSlash$ = AddSlash$(Value$)

    ELSEIF Something$ = "PALETTE" THEN
    PalPath$ = Value$

    ELSEIF Something$ = "PRINTCOLOURSET" THEN
    PalFile$ = Value$

'----------------------------------------------------------------------------
    ELSEIF Something$ = "XMOUSE" THEN
    XmsSpd% = Value%

    ELSEIF Something$ = "YMOUSE" THEN
    YmsSpd% = Value%

'----------------------------------------------------------------------------
    ELSEIF Something$ = "CURRENTDIR" THEN
    DirName$ = Value$
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

SUB TinyButton (BYVAL x%, BYVAL y%, BYVAL x2%, BYVAL y2%, BYVAL TopCol%, BYVAL BotCol%)

LINE (x%, y%)-(x2%, y%), TopCol%
LINE (x%, y%)-(x%, y2%), TopCol%

LINE (x2%, y%)-(x2%, y2%), BotCol%
LINE (x%, y2%)-(x2%, y2%), BotCol%

END SUB

SUB UpdatePortWindow (BYVAL CButNum%, BYVAL Rst%)

STATIC OldCButNum%

'============================================================================

  IF Rst% THEN
  OldCButNum% = 0
  END IF

Port$ = LCASE$(Port$)


  IF CButNum% <> OldCButNum% THEN

    SELECT CASE CButNum%
    CASE 0
      Text$ = LPT1$
      yText% = 146
      Port$ = "lpt1:"
    CASE 1
      Text$ = LPT2$
      yText% = 186
      Port$ = "lpt2:"
    CASE 2
      Text$ = LPT3$
      yText% = 226
      Port$ = "lpt3:"
    END SELECT

  LoadFont Text$, 100, yText%, 4, 1, 13

    SELECT CASE OldCButNum%
    CASE 0
      Text$ = LPT1$
      yText% = 146
    CASE 1
      Text$ = LPT2$
      yText% = 186
    CASE 2
      Text$ = LPT3$
      yText% = 226
    END SELECT

  LoadFont Text$, 100, yText%, 4, 1, 9

  ButtonShaded 50, 150 + CButNum% * 40, 1
  ButtonShaded 50, 150 + OldCButNum% * 40, 0
  END IF


OldCButNum% = CButNum%
END SUB

SUB UpdatePrnWindow (BYVAL CButNum%, BYVAL Rst%)

STATIC OldCButNum%
'============================================================================

  IF Rst% THEN
  OldCButNum% = 0
  END IF


  IF CButNum% <> OldCButNum% THEN

  ButtonShaded 300, 150, 1 - CButNum%
  ButtonShaded 300, 230, CButNum%

    IF CButNum% = 0 THEN
      Col1% = 13
      Col2% = 9
    ELSE
      Col1% = 9
      Col2% = 13
    END IF

  LoadFont "Print selected file:", 350, 146, 4, 1, Col1%
  LoadFont "Print directory contents:", 350, 226, 4, 1, Col2%
  LoadFont "DOS <DIR> command options:", 350, 290, 4, 1, Col2%
  END IF


OldCButNum% = CButNum%

END SUB

