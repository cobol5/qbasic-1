DECLARE FUNCTION MouseInit% ()
DECLARE SUB MouseStatus (Lb%, Rb%, xmouse%, ymouse%)
DECLARE SUB MouseRange (x1%, y1%, x2%, y2%)
DECLARE SUB MousePut (x%, y%)
DECLARE SUB MouseHide ()
DECLARE SUB MouseShow ()
DECLARE SUB MouseSpeed (xspeed%, yspeed%)

%AX=1
%BX=2
%CX=3
%DX=4

ms% = MouseInit%           'Checking if the mouse exists...
  IF NOT ms% THEN
  CLS
  PRINT "Mouse driver not present."
  END
  END IF

SCREEN 12
CLS

'Setting up the mouse...
MouseRange 0, 0, 638, 478
MousePut 320, 240
MouseShow

'----------------------------------------------------------------------------
  DO UNTIL INKEY$ = CHR$(27)
  LOOP

finish:
SCREEN 0
CLS
END


'===========================================================================
'===========================================================================
'==================================SUBS=====================================
'===========================================================================
'===========================================================================

SUB MouseHide

REG %AX, 2
CALL INTERRUPT &H33

END SUB

FUNCTION MouseInit%

REG %AX, 0
CALL INTERRUPT &H33
MouseInit% = REG(%AX)

END FUNCTION

SUB MousePut (x%, y%)

REG %AX, 4
REG %CX, x%
REG %DX, y%
CALL INTERRUPT &H33

END SUB

SUB MouseRange (x1%, y1%, x2%, y2%)

REG %AX, 7
REG %CX, x1%
REG %DX, x2%
CALL INTERRUPT &H33

REG %AX, 8
REG %CX, y1%
REG %DX, y2%
CALL INTERRUPT &H33

END SUB

SUB MouseShow

REG %AX, 1
CALL INTERRUPT &H33

END SUB

SUB MouseSpeed (xspeed%, yspeed%)

REG %AX, 15
REG %CX, xspeed%
REG %DX, yspeed%

CALL INTERRUPT &H33

END SUB

SUB MouseStatus (Lb%, Rb%, xmouse%, ymouse%)

REG %AX, 3
CALL INTERRUPT &H33

Lb% = ((REG(%BX) AND 1) <> 0)
Rb% = ((REG(%BX) AND 2) <> 0)
xmouse% = REG(%CX)
ymouse% = REG(%DX)

END SUB
