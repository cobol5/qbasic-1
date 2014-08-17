INPUT "Press enter to test your computers graphic drawing speeds ", yes$
CLEAR
SCREEN 13
DEF SEG = &HA000
a = TIMER
FOR c% = 1 TO 255
FOR a% = 0 TO 320
FOR b% = 0 TO 200
POKE a% + (b% * 320&), c%
NEXT b%
NEXT a%
NEXT c%
b = TIMER
DEF SEG
SCREEN 0
CLS
PRINT "Test results(Lower = better): "; b - a
INPUT "Press enter ", yes$

