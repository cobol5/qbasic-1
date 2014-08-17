'Copyright (C) HiTech Labs, Denis Andrianov
'Mouse Library.
'============================================================================

'$INCLUDE: 'mouse.bi'
'$INCLUDE: 'dos.bi'


DIM SHARED Regs AS RegType

SUB MouseHide

Regs.ax = 2
CALL Interrupt(&H33, Regs, Regs)

END SUB

FUNCTION MouseInit%

Regs.ax = 0
CALL Interrupt(&H33, Regs, Regs)
MouseInit% = Regs.ax

END FUNCTION

SUB MousePut (BYVAL x%, BYVAL y%)

Regs.ax = 4
Regs.cx = x%
Regs.dx = y%
CALL Interrupt(&H33, Regs, Regs)

END SUB

SUB MouseRange (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%)

Regs.ax = 7
Regs.cx = x1%
Regs.dx = x2%
CALL Interrupt(&H33, Regs, Regs)

Regs.ax = 8
Regs.cx = y1%
Regs.dx = y2%
CALL Interrupt(&H33, Regs, Regs)

END SUB

SUB MouseShow

Regs.ax = 1
CALL Interrupt(&H33, Regs, Regs)

END SUB

SUB MouseSpeed (BYVAL XSpeed%, BYVAL YSpeed%)

Regs.ax = 15
Regs.cx = XSpeed%
Regs.dx = YSpeed%
CALL Interrupt(&H33, Regs, Regs)

END SUB

SUB MouseStatus (Lb%, Rb%, XMouse%, YMouse%)

Regs.ax = 3
CALL Interrupt(&H33, Regs, Regs)
Lb% = ((Regs.bx AND 1) <> 0)
Rb% = ((Regs.bx AND 2) <> 0)
XMouse% = Regs.cx
YMouse% = Regs.dx

END SUB

