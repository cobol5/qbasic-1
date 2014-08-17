'Copyright (C) HiTech Labs, Denis Andrianov
'Graphics Procedures
'============================================================================

'$INCLUDE: 'graphics.bi'
'$INCLUDE: 'mouse.bi'
'$INCLUDE: 'dos.bi'


DECLARE SUB Delay (BYVAL Time%)
DECLARE SUB KeybBuffer ()
DECLARE SUB LoadFont (BYVAL FontType%, Text$, BYVAL Textx%, BYVAL Texty%, BYVAL Colour%)


CONST BoxInfoFile = "boxinfo.bin"
CONST BoxInputFile = "boxinput.bin"
CONST BoxCopyFile = "boxcopy.bin"
CONST BoxDelFile = "boxdel.bin"


COMMON SHARED /GraphicsSubs/ GraphicsPath$     'Path to GRAPHICS directory.
COMMON SHARED /GraphicsSubs/ GraphicsPathSlash$
COMMON SHARED /GraphicsSubs/ FontsPath$        'Path to Fonts directory.
COMMON SHARED /GraphicsSubs/ Word$             'InputLine's input string.
COMMON SHARED /GraphicsSubs/ NewName$          'Temp input string.
COMMON SHARED /GraphicsSubs/ ScrArray1() AS INTEGER

DEFINT A-Z
'ButType% = 0  transparent thin button
'         = 1  transparent thin button with a light indicator
'         = 2  standard non-transparent thin button
'         = 3  transparent fat button
'
SUB Button (BYVAL x%, BYVAL y%, BYVAL x2%, BYVAL y2%, BYVAL TopCol%, BYVAL BotCol%, BYVAL Colour%, BYVAL LightCol%, BYVAL ButType%)

  IF ButType% = 3 THEN
  LINE (x%, y%)-(x2%, y2%), 0, B
  x% = x% + 1
  y% = y% + 1
  x2% = x2% - 1
  y2% = y2% - 1
  END IF


LINE (x%, y%)-(x2%, y2%), BotCol%, B
LINE (x%, y%)-(x2%, y%), TopCol%
LINE (x%, y%)-(x%, y2%), TopCol%


  IF ButType% = 1 THEN

    CONST LightLen = 5
    x3% = x% + LightLen
    y3% = y% + LightLen
    LINE (x%, y%)-(x3%, y3%), BotCol%, B
    LINE (x%, y3%)-(x3%, y3%), TopCol%
    LINE (x3%, y%)-(x3%, y3%), TopCol%
    LINE (x% + 1, y% + 1)-(x3% - 1, y3% - 1), LightCol%, BF

  ELSEIF ButType% = 2 THEN

    LINE (x% + 1, y% + 1)-(x2% - 1, y2% - 1), Colour%, BF

  END IF



END SUB

FUNCTION CaptureInf% (BYVAL BoxType%)

Word$ = ""
MouseShow



  DO
  MouseStatus Lb%, Rb%, x%, y%
  KH$ = INKEY$

    IF LEN(KH$) THEN
    KH% = ASC(KH$)
    END IF


    SELECT CASE Lb%

      CASE -1
        IF NOT (Pressed%) THEN
        MouseHide
        ButtonNum% = InfButPress(x%, y%, BoxType%)
        MouseShow
        Pressed% = -1
        END IF

      CASE 0
        IF ButtonNum% > 0 THEN
        MouseHide
        InfButRelease ButtonNum%, BoxType%
        MouseShow
        EXIT DO
        END IF
      Pressed% = 0

    END SELECT



    SELECT CASE BoxType%

    CASE 1
        IF KH% = 27 THEN
          ButtonNum% = 2
        ELSEIF KH% = 13 THEN
          ButtonNum% = 1
        END IF

    CASE 2
      InputLine 25, 12, 0, 63, 31, KH$
        IF KH% = 27 THEN
          ButtonNum% = 3
        ELSEIF KH% = 13 THEN
          ButtonNum% = 1
        END IF

    CASE 5
      InputLine 34, 12, 0, 12, 12, KH$
        IF KH% = 27 THEN
          ButtonNum% = 2
        ELSEIF KH% = 13 THEN
          ButtonNum% = 1
        END IF

    CASE 6
      IF KH% = 27 OR KH% = 13 THEN
      ButtonNum% = 1
      END IF

    CASE 7
        IF KH% = 27 THEN
          ButtonNum% = 3
        ELSEIF KH% = 13 THEN
          ButtonNum% = 2
        END IF
    END SELECT

  LOOP UNTIL KH% = 27 OR KH% = 13

'----------------------------------------------------------------------------
CaptureInf% = ButtonNum%
NewName$ = Word$
MouseHide

END FUNCTION

SUB CrackLine (BYVAL Vertical%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%)

  IF Vertical% = 1 THEN
    LINE (x1%, y1%)-(x2%, y2%), 8
    LINE (x1% - 1, y1%)-(x2% - 1, y2%), 0
  ELSE
    LINE (x1%, y1%)-(x2%, y2%), 8
    LINE (x1%, y1% - 1)-(x2%, y2% - 1), 0
  END IF

END SUB

SUB Icon (BYVAL x%, BYVAL y%, BYVAL IconType%)

CONST Leng% = 60
CONST Wid% = 30

SELECT CASE IconType%
'============================================================================
'Directories
  CASE 0

  PSET (x%, y% + 10), 14
  DRAW "e10r20f10r20d20l60u20h3d20nf3u20e10nf3r20f13r20h3l20"
  PAINT (x% + 10, y% + 10), 14
'============================================================================
'Executables
  CASE 1

  CONST CtrlLen% = 5
  CONST CtrlWid% = 4

  'Drawing the border...
  LINE (x%, y%)-(x% + Leng%, y% + Wid%), 13, B
  LINE (x% + 1, y% + 1)-(x% + Leng% - 1, y% + Wid% - 1), 13, B

  'Drawing the control box...
  LINE (x% + 3, y% + 3)-(x% + 3 + CtrlLen%, y% + 3 + CtrlWid%), 11, BF
  LINE (x% + 4, (y% + 3 + y% + 3 + CtrlWid%) \ 2)-(x% + 3 + CtrlLen% - 1, (y% + 3 + y% + 3 + CtrlWid%) \ 2), 15

  'Drawing the title bar...
  LINE (x% + 3 + CtrlLen% + 2, y% + 3)-(x% + Leng% - 3, y% + 3 + CtrlWid%), 11, BF


'============================================================================
'Text
  CASE 2

  'Drawing circles...
    FOR CounterX% = x% + 5 TO x% + Leng% - 5 STEP 5
    CIRCLE (CounterX%, y% + 5), 5, 13
    NEXT CounterX%

  'Drawing paper...
  PSET (x%, y% + 5), 15
  DRAW "d" + STR$(Wid% - 5) + "r" + STR$(Leng%) + "u" + STR$(Wid% - 5)

  LINE (x% + 2, y% + 10)-(x% + Leng% - 2, y% + Wid% - 2), 15, BF
'============================================================================
'Archives
  CASE 3

  LINE (x% + Leng% \ 2 - 3, y%)-(x% + Leng% \ 2 + 3, y% + Wid% \ 5 - 1), 15, BF
  LINE (x%, y% + Wid% \ 5)-(x% + Leng%, y% + 2 * Wid% \ 5), 12, BF
  LINE (x%, y% + 3 * Wid% \ 5)-(x% + Leng%, y% + 4 * Wid% \ 5), 12, BF
  LINE (x% + Leng% \ 2 - 3, y% + 4 * Wid% \ 5 + 1)-(x% + Leng% \ 2 + 3, y% + Wid%), 15, BF

  PSET (x% + 2, y% + 3 * Wid% \ 5 - 1), 14

    FOR Counter% = 1 TO 7
    DRAW "e4f4"
    NEXT Counter%
'============================================================================
'Music
  CASE 4

    FOR Counter% = y% TO y% + Wid% - 10 STEP 10
    LINE (x%, Counter%)-(x% + Leng%, Counter% + 10), 13
    NEXT

  CIRCLE (x% + Leng% \ 3, y% + 9), 4, 15
  PAINT (x% + Leng% \ 3, y% + 9), 15
  LINE (x% + Leng% \ 3 - 4, y% + 9)-(x% + Leng% \ 3 - 4, y% + Wid%), 15

  CIRCLE (x% + Leng% * 3 \ 4, y% + 23), 4, 15
  PAINT (x% + Leng% * 3 \ 4, y% + 23), 15
  LINE (x% + Leng% * 3 \ 4 + 4, y% + 23)-(x% + Leng% * 3 \ 4 + 4, y%), 15
'============================================================================
'Programming
  CASE 6

  LINE (x% + Leng% \ 3, y% + Wid% \ 3)-(x% + Leng%, y% + Wid%), 10, BF
  LINE (x% + Leng% \ 3 - 10, y% + Wid% \ 3 - 5)-(x% + Leng% - 10, y% + Wid% - 5), 11, BF
  LINE (x%, y%)-(x% + Leng% - 20, y% + Wid% - 10), 15, BF
'============================================================================
'Unknown
  CASE 100

  PSET (x%, y%), 15
  DRAW "d" + STR$(Wid%) + "r" + STR$(Leng%) + "u" + STR$(Wid% - 10) + "h10 d10 r10 h10 l" + STR$(Leng% - 10)

'============================================================================
END SELECT

END SUB

FUNCTION InfButPress% (BYVAL x%, BYVAL y%, BYVAL BoxType%)

  IF BoxType% = 1 THEN              'OK, Cancel' box

    IF x% > 190 AND x% < 310 AND y% > 200 AND y% < 220 THEN
    Button 190, 200, 310, 220, 0, 8, 2, 0, 2
    LoadFont 255, "OK", 244, 200, 0
    InfButPress% = 1      'Button that is being pressed.

    ELSEIF x% > 330 AND x% < 450 AND y% > 200 AND y% < 220 THEN
    Button 330, 200, 450, 220, 0, 8, 2, 0, 2
    LoadFont 255, "Cancel", 371, 200, 0
    InfButPress% = 2       'Button that is being pressed.
    END IF

'----------------------------------------------------------------------------

  ELSEIF BoxType% = 2 THEN       'Run box

    IF x% > 190 AND x% < 270 AND y% > 208 AND y% < 228 THEN
    Button 190, 208, 270, 228, 0, 8, 2, 0, 2
    LoadFont 255, "Run", 221, 208, 0
    InfButPress% = 1      'Button that is being pressed.

    ELSEIF x% > 280 AND x% < 360 AND y% > 208 AND y% < 228 THEN
    Button 280, 208, 360, 228, 0, 8, 2, 0, 2
    LoadFont 255, "Quick", 304, 208, 0
    InfButPress% = 2       'Button that is being pressed.

    ELSEIF x% > 370 AND x% < 450 AND y% > 208 AND y% < 228 THEN
    Button 370, 208, 450, 228, 0, 8, 2, 0, 2
    LoadFont 255, "Cancel", 391, 208, 0
    InfButPress% = 3       'Button that is being pressed.
    END IF

'----------------------------------------------------------------------------
  ELSEIF BoxType% = 5 THEN              'Filename Input box

    IF y% > 200 AND y% < 220 THEN

      IF x% > 190 AND x% < 310 THEN
      Button 190, 200, 310, 220, 0, 8, 2, 0, 2
      LoadFont 255, "OK", 244, 200, 0
      InfButPress% = 1      'Button that is being pressed.

      ELSEIF x% > 330 AND x% < 450 THEN
      Button 330, 200, 450, 220, 0, 8, 2, 0, 2
      LoadFont 255, "Cancel", 371, 200, 0
      InfButPress% = 2       'Button that is being pressed.
      END IF

    END IF

'----------------------------------------------------------------------------

  ELSEIF BoxType% = 6 THEN              'OK only box

    IF x% > 270 AND x% < 370 AND y% > 200 AND y% < 220 THEN
    Button 270, 200, 370, 220, 0, 8, 2, 0, 2
    LoadFont 255, "OK", 314, 200, 0
    InfButPress% = 1      'Button that is being pressed.
    END IF

'----------------------------------------------------------------------------

  ELSEIF BoxType% = 7 THEN       'All,Yes,No box

    IF y% > 200 AND y% < 220 THEN

      IF x% > 190 AND x% < 270 THEN
      Button 190, 200, 270, 220, 0, 8, 2, 0, 2
      LoadFont 255, "Yes to all", 201, 200, 0
      InfButPress% = 1      'Button that is being pressed.

      ELSEIF x% > 280 AND x% < 360 THEN
      Button 280, 200, 360, 220, 0, 8, 2, 0, 2
      LoadFont 255, "Yes", 311, 200, 0
      InfButPress% = 2       'Button that is being pressed.

      ELSEIF x% > 370 AND x% < 450 THEN
      Button 370, 200, 450, 220, 0, 8, 2, 0, 2
      LoadFont 255, "No", 405, 200, 0
      InfButPress% = 3       'Button that is being pressed.
      END IF

   END IF

'----------------------------------------------------------------------------

  END IF

END FUNCTION

SUB InfButRelease (BYVAL ButtonNum%, BYVAL BoxType%)

  IF BoxType% = 1 OR BoxType% = 5 THEN

      IF ButtonNum% = 1 THEN
      Button 190, 200, 310, 220, 8, 0, 2, 0, 2
      LoadFont 255, "OK", 243, 200, 0

      ELSEIF ButtonNum% = 2 THEN
      Button 330, 200, 450, 220, 8, 0, 2, 0, 2
      LoadFont 255, "Cancel", 370, 200, 0
      END IF

'----------------------------------------------------------------------------

  ELSEIF BoxType% = 2 THEN       'Run box

      IF ButtonNum% = 1 THEN
      Button 190, 208, 270, 228, 8, 0, 2, 0, 2
      LoadFont 255, "Run", 220, 208, 0

      ELSEIF ButtonNum% = 2 THEN
      Button 280, 208, 360, 228, 8, 0, 2, 0, 2
      LoadFont 255, "Quick", 303, 208, 0

      ELSEIF ButtonNum% = 3 THEN
      Button 370, 208, 450, 228, 8, 0, 2, 0, 2
      LoadFont 255, "Cancel", 390, 208, 0

      END IF

'----------------------------------------------------------------------------

  ELSEIF BoxType% = 6 THEN

      IF ButtonNum% = 1 THEN
      Button 270, 200, 370, 220, 8, 0, 2, 0, 2
      LoadFont 255, "OK", 313, 200, 0
      END IF

'----------------------------------------------------------------------------

  ELSEIF BoxType% = 7 THEN

    IF ButtonNum% = 1 THEN
    Button 190, 200, 270, 220, 8, 0, 2, 0, 2
    LoadFont 255, "Yes to all", 200, 200, 0

    ELSEIF ButtonNum% = 2 THEN
    Button 280, 200, 360, 220, 8, 0, 2, 0, 2
    LoadFont 255, "Yes", 310, 200, 0

    ELSEIF ButtonNum% = 3 THEN
    Button 370, 200, 450, 220, 8, 0, 2, 0, 2
    LoadFont 255, "No", 404, 200, 0
    END IF

'----------------------------------------------------------------------------

  END IF

END SUB

SUB InfoBox (BYVAL BoxType%, Text$)

Word$ = ""
NewName$ = ""

CONST x1% = 130
CONST y1% = 100
CONST x2% = 510
CONST y2% = 250

'CONST x1% = 150
'CONST y1% = 100
'CONST x2% = 490
'CONST y2% = 250

'----------------------------------------------------------------------------
CONST Accel! = 1.5                          'Zooming box

CONST xMean% = (x1% + x2%) \ 2
CONST yMean% = (y1% + y2%) \ 2

xCoord% = xMean%
yCoord% = yMean%
Counter! = 1


 DO
 Counter! = Counter! * Accel!
 xCoord% = xCoord% + Counter!
 yCoord% = yCoord% + Counter!

   IF xCoord% > x2% THEN
   xCoord% = x2%
   END IF

   IF yCoord% > y2% THEN
   yCoord% = y2%
   END IF

 LINE (xMean% - (xCoord% - xMean%), yMean% - (yCoord% - yMean%))-(xCoord%, yCoord%), 7, BF
 LINE (xMean% - (xCoord% - xMean%) + 1, yMean% - (yCoord% - yMean%) + 1)-(xCoord% - 1, yCoord% - 1), 8, B

   IF yCoord% >= y2% THEN
     IF xCoord% >= x2% THEN
     EXIT DO
     END IF
   END IF

 Delay 1
 LOOP

'----------------------------------------------------------------------------
'More graphics...

'CrackLine 0, x1% + 15, y1% + 40, x2% - 15, y1% + 40
'CrackLine 1, x1% + 15, y1% + 10, x1% + 15, y1% + 39
'CrackLine 1, x2% - 15, y1% + 10, x2% - 15, y1% + 39
'CrackLine 0, x1% + 15, y1% + 10, x2% - 15, y1% + 10

Button x1% + 15, y1% + 50, x2% - 15, y2% - 15, 8, 0, 2, 0, 2

'----------------------------------------------------------------------------
xText% = 320 - (LEN(Text$) * 8 \ 2)


  IF BoxType% = 1 THEN       'OK,Cancel box
    Temp$ = BoxInfoFile
    Button 190, 200, 310, 220, 8, 0, 2, 0, 2
    LoadFont 255, "OK", 243, 200, 0
    Button 330, 200, 450, 220, 8, 0, 2, 0, 2
    LoadFont 255, "Cancel", 370, 200, 0
    LoadFont 3, Text$, xText%, y1% + 60, 0

  ELSEIF BoxType% = 2 THEN       'Run box
    Temp$ = BoxInputFile
    Button 190, 208, 270, 228, 8, 0, 2, 0, 2
    LoadFont 255, "Run", 220, 208, 0
    Button 280, 208, 360, 228, 8, 0, 2, 0, 2
    LoadFont 255, "Quick", 303, 208, 0
    Button 370, 208, 450, 228, 8, 0, 2, 0, 2
    LoadFont 255, "Cancel", 390, 208, 0
    LoadFont 3, Text$, xText%, y1% + 55, 0
    InputLine 25, 12, 1, 63, 31, Temp$

  ELSEIF BoxType% = 3 THEN      'Copying box
    Temp$ = BoxCopyFile
    LINE (260, 152)-(380, 225), 0, BF
    LINE (x1%, 260)-(x2%, 310), 2, BF
    LINE (x1% + 1, 261)-(x2% - 1, 309), 8, B
    Button 170, 275, 470, 295, 0, 8, 2, 0, 2

  ELSEIF BoxType% = 4 THEN      'Deleting box
    Temp$ = BoxDelFile

  ELSEIF BoxType% = 5 THEN       'OK,Cancel,Input box
    Temp$ = BoxInputFile
    Button 190, 200, 310, 220, 8, 0, 2, 0, 2
    LoadFont 255, "OK", 243, 200, 0
    Button 330, 200, 450, 220, 8, 0, 2, 0, 2
    LoadFont 255, "Cancel", 370, 200, 0
    LoadFont 3, Text$, xText%, y1% + 55, 0
    InputLine 34, 12, 1, 12, 12, Temp$

  ELSEIF BoxType% = 6 THEN       'OK only box
    Temp$ = BoxInfoFile
    Button 270, 200, 370, 220, 8, 0, 2, 0, 2
    LoadFont 255, "OK", 313, 200, 0
    LoadFont 3, Text$, xText%, y1% + 60, 0

  ELSEIF BoxType% = 7 THEN       'All,Yes,No box
    Temp$ = BoxDelFile
    Button 190, 200, 270, 220, 8, 0, 2, 0, 2
    LoadFont 255, "Yes to all", 200, 200, 0
    Button 280, 200, 360, 220, 8, 0, 2, 0, 2
    LoadFont 255, "Yes", 310, 200, 0
    Button 370, 200, 450, 220, 8, 0, 2, 0, 2
    LoadFont 255, "No", 404, 200, 0
    LoadFont 3, Text$, xText%, y1% + 65, 0

  END IF



DEF SEG = VARSEG(ScrArray1(0))
BLOAD GraphicsPathSlash$ + Temp$, 0
DEF SEG

PUT (x1%, y1%), ScrArray1, PSET

END SUB

SUB InputLine (BYVAL LocateX%, BYVAL LocateY%, BYVAL Init%, BYVAL Limit%, BYVAL StartScroll%, KH$)

'limit% = 63
'StartScroll% = 5
'locatex% = 25
'locatey% = 12
'word$ = ""

COLOR 15


  IF Init% = 1 THEN
  GOSUB PutCursor
  EXIT SUB
  END IF


'----------------------------------------------------------------------------

KeyLength% = LEN(KH$)


  IF KeyLength% THEN
  GOSUB PutCursor
  Ascii% = ASC(RIGHT$(KH$, 1))

    IF Ascii% <> 34 AND Ascii% <> 13 AND Ascii% <> 9 AND KeyLength% = 1 THEN

      IF Ascii% = 8 THEN
        IF LenWord% THEN
        Word$ = LEFT$(Word$, LEN(Word$) - 1)
        END IF

      ELSEIF LenWord% < Limit% THEN
      Word$ = Word$ + KH$
      END IF

    GOSUB PutCursor
    END IF

  END IF



EXIT SUB

'============================================================================

UpdateCursor:
LenWord% = LEN(Word$)
LOCATE LocateY%, LocateX%
MouseHide
  IF LenWord% < StartScroll% THEN
    PRINT Word$ + Cursor$ + SPACE$(StartScroll% - LenWord%)
  ELSE
    PRINT RIGHT$(Word$, StartScroll%) + Cursor$
  END IF
MouseShow
RETURN

'RemoveCursor:
'Cursor$ = " "
'GOSUB UpdateCursor
'RETURN

PutCursor:
Cursor$ = "_"
GOSUB UpdateCursor
RETURN

END SUB

SUB SetPal (BYVAL Colour%, BYVAL Red%, BYVAL Green%, BYVAL Blue%)

OUT &H3C8, Colour%
OUT &H3C9, Red%
OUT &H3C9, Green%
OUT &H3C9, Blue%

END SUB

