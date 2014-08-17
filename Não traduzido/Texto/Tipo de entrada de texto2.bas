DECLARE SUB dinput (a, b, b2, c)
COMMON SHARED output$

CLS
COLOR 14
LOCATE 4, 1
PRINT "Please enter your name: (50 chars)"

LOCATE 5, 2
COLOR 1, 1
PRINT SPACE$(51)

COLOR 0, 0
LOCATE 5, 1
dinput 50, 11, 1, 31

PRINT
PRINT "Hello, "; output$; ", thanks for trying DInput!"

SUB dinput (a, b, b2, c)
  SHARED output$

  output$ = ""
  i$ = ""
 
  lin = CSRLIN
  min = POS(0)
  max = a + min
  IF max > 80 THEN PRINT "DInput failed - String line too long"

IF max < 81 THEN
  clr = b
  bckclr = b2
  csrclr = c
  length = a
  current = 0

  WHILE i$ <> CHR$(13)
    COLOR csrclr, bckclr
    LOCATE lin, min + current
    PRINT "_"

    COLOR clr, bckclr
    i$ = ""
    WHILE i$ = ""
      i$ = INKEY$
    WEND
    IF ASC(i$) > 31 AND ASC(i$) < 127 AND current <> length THEN
      LOCATE lin, min + current
      PRINT i$
      output$ = output$ + i$
      current = current + 1
    END IF
    IF ASC(i$) = 8 AND current > 0 THEN
      current = current - 1
      output$ = LEFT$(output$, LEN(output$) - 1)
      LOCATE lin, min + current + 1
      PRINT " "
    END IF

  WEND

LOCATE lin, min + current
PRINT " "
END IF
COLOR clr, 0
END SUB

