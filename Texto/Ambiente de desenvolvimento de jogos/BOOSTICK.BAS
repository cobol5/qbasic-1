CLS
COLOR 4
PRINT "KABOOM Joystick Configuration"
PRINT "*****************************"
PRINT
COLOR 3
PRINT "Note: The Middleposition of the manual Adjustment of your Stick"
PRINT "should be exactly the half of its possible Range. Please adjust"
PRINT "manually now."
PRINT "(PS: I've seen that this can be 80 on win95 or 40 on win98.)"
PRINT "Move Joystick to all limits to see Range. Adjust,"
PRINT "then press fire (or Esc to abort)."
COLOR 10
WHILE STRIG(1) = 0 AND i$ <> CHR$(27)
i$ = INKEY$
x% = STICK(0)
y% = STICK(1)
LOCATE 11, 1: PRINT x%, y%, "   "
WEND
IF i$ = CHR$(27) THEN END
LOCATE 11, 1
PRINT "Ok.              "
GOSUB wstick0

COLOR 15
PRINT "Move Joystick to left Maximum and then press fire (or Esc to abort)."
WHILE STRIG(1) = 0 AND i$ <> CHR$(27)
i$ = INKEY$
x% = STICK(0)
y% = STICK(1)
WEND
IF i$ = CHR$(27) THEN END
COLOR 10
PRINT "Ok."
COLOR 15
GOSUB wstick0
PRINT "Now move Stick to right Maximum and then press fire."
WHILE STRIG(1) = 0
x2% = STICK(0)
y2% = STICK(1)
WEND
COLOR 4
stdat$ = "Boostick.dat"
PRINT "Ok. Writing "; stdat$; " ..."
xdif% = (x2% - x%) / 2
OPEN stdat$ FOR OUTPUT AS #1
 PRINT #1, STR$(xdif%)
CLOSE #1
' PRINT xdif%
COLOR 2
PRINT "Thank you. Maybe you will have to adjust your Joystick"
PRINT "manually a little Bit while playing KABOOM."
PRINT
PRINT "Press any key to exit"
GOSUB wstick0
WHILE STRIG(1) = 0 AND i$ = ""
i$ = INKEY$
x% = STICK(0)
y% = STICK(1)
LOCATE 11, 1: PRINT x%, y%, "   "
WEND
IF i$ = CHR$(27) THEN END
LOCATE 11, 1
GOSUB wstick0
END

wstick0:
WHILE STRIG(1) <> 0
dux% = STICK(0)
duy% = STICK(1)
WEND
RETURN

