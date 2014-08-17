SCREEN 13
CLS
cr = 1
IF COMMAND$ = "" THEN
  PRINT "<"; : COLOR 10: PRINT "+"; : COLOR 7: PRINT "> adds bouncers, <"; : COLOR 10: PRINT "-"; : COLOR 7: PRINT "> takes away."
  PRINT "<"; : COLOR 10: PRINT "SPACE"; : COLOR 7: PRINT "> toggles erasing."
  PRINT "<"; : COLOR 10: PRINT "ESC"; : COLOR 7: PRINT "> exits screen saver."
END IF
ms = VAL(COMMAND$)
IF ms = 0 OR ms > 1000 THEN ms = 100
RANDOMIZE TIMER
DIM x(1000)
DIM y(1000)
DIM vx(1000)
DIM vy(1000)
DIM clr(1000)

FOR s = 1 TO ms
  x(s) = 160
  y(s) = 100
  vx(s) = RND * 2 - 1
  vy(s) = RND * 2 - 1
  clr(s) = INT(RND * 256)
NEXT s
DO
10
  FOR s = 1 TO ms
    IF cr = 1 THEN PSET (x(s), y(s)), 0
    IF x(s) > 319 OR x(s) < 2 THEN vx(s) = -vx(s)
    IF y(s) > 199 OR y(s) < 2 THEN vy(s) = -vy(s)
    y(s) = y(s) + vy(s)
    x(s) = x(s) + vx(s)
    PSET (x(s), y(s)), clr(s)
  NEXT
  in$ = INKEY$
  SELECT CASE in$
    CASE ""
    CASE "+", "="
      IF ms < 1000 THEN
        ms = ms + 1
        x(ms) = 160
        y(ms) = 100
        vx(ms) = RND * 2 - 1
        vy(ms) = RND * 2 - 1
        clr(ms) = INT(RND * 256)
      END IF
    CASE "-", "_"
      IF ms > 0 THEN
        PSET (x(ms), y(ms)), 0
        ms = ms - 1
      END IF
    CASE " "
      cr = -cr
      IF cr = 1 THEN CLS
    CASE CHR$(27)
      GOTO nd
  END SELECT
LOOP
nd:
CLS
SCREEN 0
COLOR 10
WIDTH 80
      vain$ = "TwiGGy@Aztechnet.com"
      LOCATE CSRLIN, 1
      l = TIMER
      FOR s = 1 TO LEN(vain$)
        LOCATE CSRLIN, 1
        PRINT RIGHT$(vain$, s) + STRING$(s * 2, 32);
        WHILE TIMER = l: WEND
        l = TIMER
      NEXT
      LOCATE CSRLIN, 1
      PRINT


