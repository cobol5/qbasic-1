RANDOMIZE TIMER
SCREEN 13
TIMER ON
start = TIMER
col = CINT(RND * 60) + 16
c$ = "c" + STR$(col)
DO WHILE INKEY$ = ""
   y = (TIMER - start) * 10
   n = y * 10 MOD 720
   z = INT(n / 2)
  
   DRAW "b m160,100 c0 r50"
   DRAW "u45 l100 d90 r100 u45"
  
   DRAW "ta" + STR$(z)
   DRAW "b m160,100 c0 r50"
   DRAW c$ + "u45 l100 d90 r100 u45"

LOOP

