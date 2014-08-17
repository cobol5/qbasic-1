'Copyright (C) HiTech Labs, Denis Andrianov.
'Save Keys v1.0
'============================================================================

Filename$ = LTRIM$(RTRIM$(COMMAND$))
  IF LEN(Filename$) = 0 THEN END

CLS
COLOR 14
LOCATE , 15
PRINT "You may begin to press keys or key combinations."
COLOR 12
PRINT "Press ";
COLOR 27
PRINT "<NumLock>";
COLOR 12
PRINT " when you finish or when the beeps are produced by the speaker!"

COLOR 10
LOCATE 10, 25
PRINT "ษออออออKeyboard  Bufferออออออป"
LOCATE , 25
PRINT "บ                            บ"
LOCATE , 25
PRINT "บ                            บ"
LOCATE , 25
PRINT "ศออออออออออออออออออออออออออออผ"

LOCATE 16, 1
COLOR 9
PRINT "Head indicator"
COLOR 14
PRINT "Tail Indicator"

'---------------------------------------------------------------------------
DEF SEG = &H40
GOSUB EmptyKeyb


  DO
    FOR i? = &H1E TO &H1E + 32               'Memory dump to screen.
    DEF SEG = &H40
    byte? = PEEK(i?)
    DEF SEG = &HB800
    POKE 1792 + i?, byte?
    NEXT

  LOCATE 14, 32                              'Erase old arrows.
  PRINT SPACE$(16)
  DEF SEG = &H40                             'Indicate where head and tail
  tail? = PEEK(&H1C)                         'are located with an arrow.
  head? = PEEK(&H1A)
  DEF SEG = &HB800
  POKE 2113 + tail?, 14                      'Yellow colour attribute.
  POKE 2112 + tail?, 24                      'Up arrow.
  POKE 2113 + head?, 9                       'Blue colour attribute.
  POKE 2112 + head?, 24                      'Up arrow.

  WAIT &H3DA, 8, 8
  WAIT &H3DA, 8
  LOOP WHILE INP(&H60) <> 250


DEF SEG = &H40
POKE &H17, 32                          '<NumLock> on, others off...
BSAVE Filename$, &H1A, 36              'Saving buffer...
GOSUB EmptyKeyb

END

'===========================================================================
EmptyKeyb:
POKE &H1A, PEEK(&H1C)                  'Clearing keyboard buffer...
RETURN