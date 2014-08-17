'$INCLUDE: 'qbx.bi'

DIM Regs AS RegType

Regs.ax = &H1600
CALL interrupt(&H2F, Regs, Regs)

PRINT Regs.ax AND &HFF
SLEEP

