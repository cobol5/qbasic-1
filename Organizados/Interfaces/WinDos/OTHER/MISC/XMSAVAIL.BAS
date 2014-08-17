'$INCLUDE: 'qb.bi'

DIM regs AS RegType

regs.ax = &H4300
CALL interrupt(&H2F, regs, regs)

  IF regs.ax = &H4380 THEN
    PRINT "Driver is installed"
  ELSE
    PRINT "Driver is not installed."
  END IF

