CLS
RANDOMIZE RND * TIMER
SCREEN 12
DIM x(8000): DIM y(8000)
DIM a(8000): DIM b(8000)
herstart:
c = INT(RND * 10) + 1
FOR l = 1 TO 2
        FOR i = 1 TO 8000
                IF INKEY$ <> "" THEN GOTO eind
                x = RND * 320
                y = RND * 240
                a = RND * 320
                b = RND * 240
                IF INKEY$ <> "" THEN GOTO eind
                PSET (x, y), c
                PSET (640 - x, y), c
                PSET (640 - x, 480 - y), c
                PSET (x, 480 - y), c
                PSET (a, b), c
                PSET (640 - a, b), c
                PSET (640 - a, 480 - b), c
                PSET (a, 480 - b), c
                IF INKEY$ <> "" THEN GOTO eind
        NEXT i
NEXT l
FOR i = 1 TO 6000: NEXT
CLEAR
GOTO herhaal
eind:
FOR i = 1240 TO 0 STEP -1
        LINE (0, i)-(i, 0), 0
        FOR l = 1 TO 10: NEXT
NEXT i
COLOR c
SYSTEM
herhaal:
g = -1
FOR i = 480 TO 240 STEP -1
        IF INKEY$ <> "" THEN GOTO eind
        LINE (0, i)-(640, i), 0
        FOR l = 1 TO 300: NEXT
g = g + 1
IF g = 241 THEN GOTO herstart
LINE (0, g)-(640, g), 0
NEXT i
GOTO herstart

