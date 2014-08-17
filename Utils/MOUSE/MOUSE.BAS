' The most straightforward QBasic mouse routine ever.  Revised by Andrew
' Morison.  Feel free to rip any of this code.
' xpos% is the x position.
' ypos% is the y position.
' mbut% is the mouse button pressed. 0=none 1=left 2=right 3=both
SCREEN 12
PRINT "Initializing..."
DEF SEG = 0
M.seg& = 256& * PEEK(207) + PEEK(206)
M.info% = 256& * PEEK(205) + PEEK(204) + 2
' see if a Mouse Driver is loaded
DEF SEG = M.seg&
IF (M.seg& OR (M.info% - 2)) AND PEEK(M.info% - 2) = 207 THEN
CLS
PRINT "No mouse driver present."
END IF
'CALL absolute(0, 0, 0, 0, M.info%)  'Set Default (Whatever)
CALL absolute(1, 0, 0, 0, M.info%)
CALL absolute(4, 0, col% * 8, row% * 8, M.info%) ' Stops cursor from smudging
CLS                                              ' the background.
DO                                   'main loop
CALL absolute(3, mbut%, xpos%, ypos%, M.info%)  'check on mouse
LOCATE 1, 1
PRINT "Y"; (ypos%) + 1; "  "; "X"; (xpos%) + 1
LOCATE 2, 1
PRINT "Button"; mbut%
LOOP UNTIL INKEY$ = CHR$(27)

