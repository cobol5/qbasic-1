'Copyright (C) HiTech Labs, Denis Andrianov
'HiTech Labs ScreenSaver
'============================================================================

DECLARE FUNCTION MouseInit% ()
DECLARE SUB MouseStatus (Lb%, Rb%, XMouse%, YMouse%)

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

DIM FormulaArray(15751) AS INTEGER
REDIM PictureArray(32200) AS INTEGER

DIM SHARED Regs AS RegType

'============================================================================

'***********************************
'Set video mode.
'***********************************

Regs.ax = &H13
CALL INTERRUPT(&H10, Regs, Regs)


'***********************************
'Test the speed of the computer.
'***********************************

OldTime! = TIMER
DEF SEG = &HA000

  FOR i% = 1 TO 3

    FOR y% = 0 TO 100
    YArrayOffset% = y% * 320
      FOR x% = 0 TO 320
        FOR temp% = 1 TO 4
        POKE YArrayOffset% + x%, 0
        NEXT
      NEXT
    NEXT

  NEXT


'Computer is too fast.
  IF TIMER - OldTime! > .0001 THEN
    FrameRate% = (3 / (TIMER - OldTime!)) \ 2
  ELSE
    FrameRate% = 255
  END IF

'PRINT FrameRate%
'SLEEP



'***********************************
'If too slow for animation then exit.
'***********************************
  
  IF FrameRate% < 15 THEN
  GOSUB TextMode
  PRINT "This program requires a faster computer!"
  PRINT "Please select another screen saver."
  SLEEP
  END
  END IF



'***********************************
'Set up the "magic" array.
'***********************************

FOR y% = 0 TO 125
  FOR x% = 1 TO 125
    nx! = (x% - 125) / 125
    ny! = (y% - 125) / 125
    nz! = 1 - SQR(nx! ^ 2 + ny! ^ 2)
      IF nz! < 0 THEN nz! = 0
    FormulaArray(x% * 125 + y%) = CINT(nz! * 250)
  NEXT x%
NEXT y%



'***********************************
'Load palette and image.
'***********************************

Red$ = " "
Green$ = " "
Blue$ = " "

OPEN "hitech.pal" FOR BINARY AS #1

  FOR ColourAtt% = 0 TO 255
  GET #1, , Red$
  GET #1, , Green$
  GET #1, , Blue$
  OUT &H3C8, ColourAtt%
  OUT &H3C9, ASC(Red$)
  OUT &H3C9, ASC(Green$)
  OUT &H3C9, ASC(Blue$)
  NEXT

CLOSE #1


DEF SEG = VARSEG(PictureArray(0))
BLOAD "hitech.bin", 0



'***********************************
'Check for the existance of the mouse.
'***********************************

  IF MouseInit% THEN
  MouseExists% = -1

  Regs.ax = 4                           'Position mouse at (160,100)
  Regs.cx = 160
  Regs.dx = 100
  CALL INTERRUPT(&H33, Regs, Regs)

  OldXMouse% = 160
  OldYMouse% = 100

  END IF



'***********************************
'Set up variables for speed.
'***********************************

TopPictureArraySegment% = VARSEG(PictureArray(0))
BottomPictureArraySegment% = TopPictureArraySegment% + &H7D0

XLightPosition% = -50
YLightPosition% = 100
Direction% = 4



'***********************************
'Begin the animation.
'***********************************
  
  DO
    
    IF MouseExists% THEN
    MouseStatus Lb%, Rb%, XMouse%, YMouse%
    END IF

    IF XLightPosition% < 10 THEN
      Direction% = Direction% + 1
    ELSEIF XLightPosition% > 310 THEN
      Direction% = Direction% - 1
    END IF
  XLightPosition% = XLightPosition% + Direction%

    FOR y% = 1 TO 199
    ly% = y% - YLightPosition%

      IF y% < 100 THEN
        YArrayOffset% = y% * 320
        PictureArraySegment% = TopPictureArraySegment%
        ScreenSegment% = &HA000
      ELSE
        YArrayOffset% = (y% - 100) * 320
        PictureArraySegment% = BottomPictureArraySegment%
        ScreenSegment% = &HA7D0
      END IF

      FOR x% = 1 TO 319
      lx% = x% - XLightPosition%
      FullArrayOffset% = YArrayOffset% + x%

      DEF SEG = PictureArraySegment%
      temp% = PEEK(FullArrayOffset%) - 125
      ny% = PEEK(FullArrayOffset% + 320) - temp% - ly%
      nx% = PEEK(FullArrayOffset% + 1) - temp% - lx%
        IF ny% < 1 OR ny% > 249 THEN ny% = 1
        IF nx% < 0 OR nx% > 250 THEN nx% = 0
      nx% = 125 - ABS(125 - nx%)
      ny% = 125 - ABS(125 - ny%)

      DEF SEG = ScreenSegment%
      POKE FullArrayOffset%, FormulaArray(nx% * 125 + ny%)
      NEXT

    NEXT

    IF FrameRate% > 33 THEN
    WAIT &H3DA, 8, 8
    WAIT &H3DA, 8
    END IF

  LOOP UNTIL LEN(INKEY$) OR (XMouse% <> OldXMouse% OR YMouse% <> OldYMouse%)



'***********************************
'Exit if a key is pressed or mouse is moved.
'***********************************

GOSUB TextMode
END


'============================================================================
'============================================================================
'============================================================================
TextMode:
Regs.ax = &H3
CALL INTERRUPT(&H10, Regs, Regs)
RETURN

FUNCTION MouseInit%

Regs.ax = 0
CALL INTERRUPT(&H33, Regs, Regs)
MouseInit% = Regs.ax

END FUNCTION

SUB MouseStatus (Lb%, Rb%, XMouse%, YMouse%)

Regs.ax = 3
CALL INTERRUPT(&H33, Regs, Regs)
Lb% = ((Regs.bx AND 1) <> 0)
Rb% = ((Regs.bx AND 2) <> 0)
XMouse% = Regs.cx
YMouse% = Regs.dx

END SUB

