TYPE RegTypeX
  AX    AS INTEGER
  BX    AS INTEGER
  cx    AS INTEGER
  dx    AS INTEGER
  bp    AS INTEGER
  si    AS INTEGER
  di    AS INTEGER
  flags AS INTEGER
  ds    AS INTEGER
  es    AS INTEGER
END TYPE

DIM Regs AS RegTypeX


SCREEN 12
CIRCLE (300, 300), 200, 12
COLOR 15

PRINT "Test"
PRINT "We are in screen 12. Let's run another program and see if this shit works."
PRINT "Hit any key."
SLEEP

Regs.AX = &HFFFF
Regs.BX = &HFFFF
CALL INTERRUPTX(&H10, Regs, Regs)

SHELL "child.exe"

Regs.AX = &HFFFE
Regs.BX = &HFFFF
CALL INTERRUPTX(&H10, Regs, Regs)

