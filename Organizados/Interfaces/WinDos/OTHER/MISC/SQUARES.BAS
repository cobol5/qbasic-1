'Copyright (C) HiTech Laboratories, Denis Andrianov
'Magic Squares ScreenSaver
'============================================================================

DECLARE SUB SetPal (BYVAL Attrib%, BYVAL r%, BYVAL g%, BYVAL b%)
DECLARE SUB GetPal (BYVAL Attrib%, r%, g%, b%)
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

'============================================================================
RANDOMIZE TIMER

SCREEN 13
CLS


'************************************
'Check for the existance of the mouse
'************************************

  IF MouseInit% THEN
  MouseExists% = -1
  MouseStatus Lb%, Rb%, OldXMouse%, OldYMouse%
  END IF


'************************************
'Reset the palette
'************************************

  FOR i% = 0 TO 255
  SetPal i%, 0, 0, 0
  NEXT


'************************************
'Plot one of seven images
'************************************

  FOR y% = 0 TO 199
    FOR x% = 0 TO 319

    Colour% = y% XOR ((x% + y%) XOR (x% + NOT (y%)))
    PSET (x%, y%), Colour%
    
    NEXT
  NEXT



'************************************
'Initialize the palette
'************************************
  
  FOR i% = 0 TO 63
  Temp% = 63 - i%
  SetPal i%, i%, 0, 0
  SetPal i% + 64, Temp%, 0, 0
  SetPal i% + 128, 0, 0, i%
  SetPal i% + 192, 0, 0, Temp%
  NEXT



'************************************
'Rotate the palette.
'************************************
  
  DO
  
  GetPal 255, OldR%, OldG%, OldB%

    FOR i% = 254 TO 0 STEP -1
    GetPal i%, r%, g%, b%
    SetPal i% + 1, r%, g%, b%
    NEXT

  SetPal 0, OldR%, OldG%, OldB%

    FOR i% = 1 TO 2
    WAIT &H3DA, 8, 8
    WAIT &H3DA, 8
    NEXT

    IF MouseExists% THEN
    MouseStatus Lb%, Rb%, XMouse%, YMouse%
      IF XMouse% <> OldXMouse% OR YMouse% <> OldYMouse% THEN EXIT DO
    END IF

  LOOP UNTIL LEN(INKEY$)

SUB GetPal (BYVAL Attrib%, r%, g%, b%)

OUT &H3C7, Attrib%
r% = INP(&H3C9)
g% = INP(&H3C9)
b% = INP(&H3C9)

END SUB

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

SUB SetPal (BYVAL Attrib%, BYVAL r%, BYVAL g%, BYVAL b%)

OUT &H3C8, Attrib%
OUT &H3C9, r%
OUT &H3C9, g%
OUT &H3C9, b%

END SUB

