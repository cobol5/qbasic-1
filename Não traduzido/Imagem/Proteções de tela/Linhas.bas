DO WHILE INKEY$ = ""
CLS
SCREEN 12
WINDOW (-1.333, 1)-(1.333, -1)
PAINT (0, 0), 0

FOR t = .1 TO 360 STEP .1
	FOR angle = 1 TO 90 STEP 3
		x = SIN(t - angle): y = COS(t + angle): z = SIN(t)
		xpos = 1.333 * (x / (z - 1.333))
		ypos = 1.333 * (y / (z - 1.333))
		PSET (xpos, ypos), 10
		'PSET (-xpos, -ypos), 0
		IF INKEY$ <> "" THEN END
	NEXT
NEXT
LOOP

