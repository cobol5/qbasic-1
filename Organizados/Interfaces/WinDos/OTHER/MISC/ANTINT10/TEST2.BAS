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
PRINT "Test 1"
PRINT "Screen switching = disabled. Let's test. We are in screen 12."
PRINT "Hit any key."

Regs.AX = &HFFFF
Regs.BX = &HFFFF
CALL INTERRUPTX(&H10, Regs, Regs)

SLEEP
SCREEN 0
PRINT "We should have stayed in screen 12."
PRINT "Press any key."
SLEEP

SCREEN 12

Regs.AX = &HFFFE
Regs.BX = &HFFFF
CALL INTERRUPTX(&H10, Regs, Regs)

PRINT "Test 2"
PRINT "Screen switching = enabled. Let's test."
PRINT "Hit any key."
SLEEP
SCREEN 0
PRINT "Switched back to screen 0"
PRINT "Press any key."
SLEEP

