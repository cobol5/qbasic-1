' if you have a fast P.C, the ball can't be seen
10 CLS
60 FOR i = 0 TO 39
90 IF i > 21 THEN GOTO 120
120 RANDOMIZE TIMER
125 c = INT(RND * 16)
126 COLOR c
127 NEXT i
130 x = 1: y = 1
140 V = 1: h = 1
150 LOCATE x, y: PRINT " "
160 x = x + V: y = y + h
170 IF x < 2 OR x > 22 THEN V = -V
180 IF y < 2 OR y > 77 THEN h = -h
190 LOCATE x, y: PRINT "*"
200 FOR i = 1 TO 50: NEXT i
205 A$ = INKEY$
206 IF A$ = CHR$(32) THEN END
210 GOTO 150

