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

Regs.AX = &HFFFF
Regs.BX = &HFFFF
CALL INTERRUPTX(&H10, Regs, Regs)

CLS
PRINT "Test 1"
PRINT "Screen switching = disabled. Let's test."
PRINT "Hit any key."
SLEEP
SCREEN 12
SCREEN 0
PRINT "Should have stayed at screen 0"
PRINT "Press any key."
SLEEP

Regs.AX = &HFFFE
Regs.BX = &HFFFF
CALL INTERRUPTX(&H10, Regs, Regs)

SCREEN 0
PRINT "Test 2"
PRINT "Screen switching = enabled. Let's test."
PRINT "Hit any key."
SLEEP
SCREEN 12
PRINT "Switched to screen 12"
PRINT "Press any key."
SLEEP

