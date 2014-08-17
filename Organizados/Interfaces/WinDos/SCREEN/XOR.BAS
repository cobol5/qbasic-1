'Copyright (C) HiTech Laboratories, Denis Andrianov
'XOR ScreenSaver
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

DIM SHARED Regs AS RegType
DIM Object(335) AS INTEGER

CONST Xmax = 600
CONST Ymax = 440

RANDOMIZE TIMER

'============================================================================

DEF FNRand% (Min%, Max%)
FNRand% = INT((Max% - Min% + 1) * RND + Min%)
END DEF

'============================================================================

XStep% = FNRand%(1, 3)
YStep% = FNRand%(1, 3)

SCREEN 12
CLS


'************************************
'Check for the existance of the mouse.
'************************************

  IF MouseInit% THEN
  MouseExists% = -1
  
  Regs.ax = 4                           'Position mouse at (160,100)
  Regs.cx = 160
  Regs.dx = 100
  CALL INTERRUPT(&H33, Regs, Regs)

  OldXMouse% = 160
  OldYMouse% = 100

  END IF


'************************************
'Draw and get the object into memory.
'************************************

Colour% = FNRand%(1, 15)
CIRCLE (16, 16), FNRand%(8, 16), Colour%
PAINT (16, 16), Colour%

GET (0, 0)-(32, 32), Object


'************************************
'Bounce the object using XOR.
'************************************

  DO
  Xpos% = Xpos% + XStep%
  Ypos% = Ypos% + YStep%

    IF Xpos% < ABS(XStep%) OR Xpos% > Xmax THEN
    XStep% = -XStep%
    END IF

    IF Ypos% < ABS(YStep%) OR Ypos% > Ymax THEN
    YStep% = -YStep%
    END IF

  PUT (Xpos%, Ypos%), Object, XOR

    IF MouseExists% THEN
    MouseStatus Lb%, Rb%, XMouse%, YMouse%
      IF XMouse% <> OldXMouse% OR YMouse% <> OldYMouse% THEN EXIT DO
    END IF

'  WAIT &H3DA, 8, 8
'  WAIT &H3DA, 8
  LOOP UNTIL LEN(INKEY$)

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

