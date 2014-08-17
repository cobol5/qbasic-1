PLAY "mb"
SCREEN 12
CLS
palm$ =  _
"bd200 bl250 c6 u30 ta-5 u30 ta-10 u30 ta-15 u30 ta360 r20 ta-15 d30 ta-10 d30 ta-5 d30 ta360 d30 l20 br bu p6,6 bd bl bu118 br15 c2 ta20 l20 ta30 l30 ta40 l20 ta60 r20 ta50 r20 ta40 r20 ta30 r25 ta10 l20 ta20 l20 ta30 l20 ta40 l20 ta60 r20 ta50 r20 ta30 r20 ta20 r20 ta10 r20 ta0 r20 ta350 r20 ta340 r20 ta330 r20 ta320 r20 ta340 l20 ta350 l50 ta340 r20 ta330 r20 ta325 ta320 r20 ta310 r10 ta-20 l20 ta-15 l20 ta-0 l50 bu p2,2 bd" _

erniesq$ =  _
"bu250 br250 c14 br250 bd50 ta-55 l100 ta-40 l20 ta-30 l10 ta-20 l10 ta-10 l10 ta0 l10 ta10 l10 ta20 l10 ta40 l10 ta55 l10 c12 ta20 l20 ta0 r20 c14 u7 d14 u8 bl c12 l20 ta-20 r20 c14 ta40 d30 ta30 d30 ta40 d60 ta50 d60 ta60 d50 ta10 u80 ta30 u10 ta35 d50 ta20 d50 bl10 u40 ta35 u20 bd70 bl20 bu30 d50 ta0 bl80 bu68 c12 bd ta-60 bd d20 ta0 g10 e10 d10 u10 f10 h10 bu25 ta-60 d20 ta0 g10 e10 d10 u10 f10 h10 bu25 bh40 bl10 bu65 p14,14 c0 u5 e2 r5 f2 d5 g2 l5 h2 br p0,0 bl bl300 bd150 c15 u100 d200 r300 u200 l300" _

s1$ = "br100 bd75 d50 r100 bu50 l100 r10 bd25 c4 e10 f10 g10 h10 br p4,4 bl"
s2$ = "br100 bd75 bd50 r100 u50 l100 c4 bd25 br70 e10 f10 g10 h10 br p4,4 bl"
s3$ = "br125 bd40 d70 l70 r70 br50 u70 d70 r70 bd50 l190 br85 bu25 c4 e10 f10 g10 h10 br p4,4 bl"
s4$ = "br210 bd5 d190 bl50 u70 l70 bu50 r70 u70 bd95 br15 c4 e10 f10 g10 h10 br p4,4 bl"
s5$ = "br125 bd20 bd70 l70 r70 d70 u70 br50 d70 u70 r70 bu50 l190 br85 bd25 c4 e10 f10 g10 h10 br p4,4 bl"
s6$ = "br125 bd50 d100 u100 r50 d100 u80 bl35 c4 e10 f10 g10 h10 br p4,4 bl"
s7$ = "br100 bd50 d100 u100 r100 bd50 l50 d50 u50 r20 bl55 bu25 c4 e10 f10 g10 h10 br p4,4 bl"
s8$ = "br100 bd50 d100 r100 bu50 l50 u50 bd75 bl35 c4 e10 f10 g10 h10 br p4,4 bl"
s9$ = "br100 bd100 r50 u50 br50 d100 l100 r65 bu25 c4 e10 f10 g10 h10 br p4,4 bl"
s10$ = "br100 bd50 r100 d100 bl50 u50 l50 br65 bu25 c4 e10 f10 g10 h10 br p4,4 bl"
ernie$ =  _
"bl50 bu30 c14 u60 e20 r80 f60 l60 r60 g60 d60 g40 l40 d40 u20 f20 h20 g20 e20 u20 l80 d40 u20 f20 h20 g20 e20 u20 l40 u20 e100 u20 u32 br40 h8 u12 e8 r12 f8 d12 g8 l12 be8 bu8 l4 bd229 c10 r50 l230 d5 r230 u5 d5 l50 g8 d10 f8 r10 e8 u10 h8 l150 g8 d10 f8 r10 e8 u10 h8" _

3 CLS
PRINT "Welcome to The Adventures Of Ernie The Canary!"
PRINT "Press 'p' to play the game, or 'i' to see the instructions."
4
LOCATE 3, 1: INPUT a$
IF a$ = "i" GOTO 5
IF a$ = "p" GOTO 10
SOUND 100, 5
GOTO 4
5 CLS
PRINT "IDEA OF THE GAME-"
PRINT "The idea of the game is to get to the birdseed and the paradise jungle"
PRINT "at the end of the maze."
PRINT "The red diamond represents you (Ernie the Canary)"
PRINT "CONTROLS-"
PRINT "To go north- Press n"
PRINT "To go south- Press s"
PRINT "To go east- Press e"
PRINT "To go west- Press w"
PRINT "To quit- Press q"
PRINT "Good luck!"
PRINT "Press any key to continue..."
6 a$ = INKEY$
IF a$ = "" GOTO 6
GOTO 3
10 CLS
PRINT "You are standing at a T-junction. Exits are north, west and east."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s3$)
15
a$ = INKEY$
IF a$ = "n" GOTO 20
IF a$ = "w" GOTO 180
IF a$ = "e" GOTO 40
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 15
20 CLS
PRINT "You are standing at a two-way junction. Exits are south and east."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s7$)
25
a$ = INKEY$
IF a$ = "s" GOTO 10
IF a$ = "e" GOTO 30
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 25
30 CLS
PRINT "You have come to a dead-SYSTEM. Exit is west."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s2$)
35
a$ = INKEY$
IF nee = 0 THEN IF a$ = "n" THEN nee = 1
IF nee = 1 THEN IF a$ = "e" THEN nee = 2: a$ = ""
IF nee = 2 THEN IF a$ = "e" THEN nee = 3
IF nee = 3 GOTO 170
IF a$ = "w" GOTO 20
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 35
40 CLS
PRINT "You have come to an intersection. Exits are west and south."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s10$)
45
a$ = INKEY$
IF a$ = "w" GOTO 10
IF a$ = "s" GOTO 50
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 45
50 CLS
PRINT "You are on a corner. Exits are north and east."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s8$)
55
a$ = INKEY$
IF a$ = "n" GOTO 40
IF a$ = "e" GOTO 60
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 55
60 CLS
PRINT "You are at a T-junction. Exits are north, west and east."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s3$)
65
a$ = INKEY$
IF a$ = "n" GOTO 70
IF a$ = "w" GOTO 50
IF a$ = "e" GOTO 80
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 65
70 CLS
PRINT "You have come to a dead-SYSTEM. Exit is south."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s6$)
75
a$ = INKEY$
IF a$ = "s" GOTO 60
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 75
80 CLS
PRINT "You have come to a two-way junction. Exits are north and west."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s9$)
85
a$ = INKEY$
IF a$ = "n" GOTO 90
IF a$ = "w" GOTO 60
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 85
90 CLS
PRINT "You are at an intersection. Exits are west and south."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s10$)
95
a$ = INKEY$
IF a$ = "w" GOTO 100
IF a$ = "s" GOTO 80
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 95
100 CLS
PRINT "You are at a t-intersection. Exits are north, west and east."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s3$)
105
a$ = INKEY$
IF a$ = "n" GOTO 120
IF a$ = "w" GOTO 110
IF a$ = "e" GOTO 90
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 105
110 CLS
PRINT "You are at a dead-SYSTEM. Exit is east."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s1$)
115
a$ = INKEY$
IF a$ = "e" GOTO 100
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 115
120 CLS
PRINT "You are at a corner. Exits are south and west."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s10$)
125
a$ = INKEY$
IF a$ = "s" GOTO 100
IF a$ = "w" GOTO 130
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 125
130 CLS
PRINT "You find yourself at a T-junction. Exits are south, west and east."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s5$)
135
a$ = INKEY$
IF a$ = "s" GOTO 150
IF a$ = "w" GOTO 140
IF a$ = "e" GOTO 120
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 135
140 CLS
PRINT "You arrive at a dead-SYSTEM. Exit is east."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s1$)
145
a$ = INKEY$
IF a$ = "e" GOTO 130
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 145
150 CLS
PRINT "You are at a corner. Exits are north and west."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s9$)
155
a$ = INKEY$
IF a$ = "n" GOTO 130
IF a$ = "w" GOTO 160
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 155
160 CLS
PRINT "You come to a corner going north and east."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s8$)
165
a$ = INKEY$
IF a$ = "n" GOTO 170
IF a$ = "e" GOTO 150
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 165
170 CLS
PRINT "Congratulations! You have found the paradise jungle!"
DRAW  _
"c6 bd200 bl250 u50 ta-10 u50 ta-20 u50 ta-30 u50 c2 ta30 l50 ta40 l50 ta-30 u50 ta-50 u50 ta-60 u25 ta20 l50 ta30 l50 ta40 r50 ta-150 l50 ta10 r50 ta360 r20 ta-10 r50 ta-30 r50 ta-10 l50 ta-5 l20 ta360 l20 ta340 r50 ta330 r50 ta350 l50 ta-10 l50 ta340 l10 c6 ta320 bd2 d10 ta330 d50 ta340 d50 ta350 d50 ta360 d50 bd l41 bu189 br50 ta11 r50 bg30 p6,6 be30 c2 ta11 l50 bu10 p2,2 bd10 ta360" _

DRAW  _
"br350 bd190 c6 u30 ta-5 u30 ta-10 u30 ta-15 u30 ta360 r20 ta-15 d30 ta-10 d30 ta-5 d30 ta360 d30 l20 br bu p6,6 bd bl bu118 br15 c2 ta20 l20 ta30 l30 ta40 l20 ta60 r20 ta50 r20 ta40 r20 ta30 r25 ta10 l20 ta20 l20 ta30 l20 ta40 l20 ta60 r20 ta50 r20 ta30 r20 ta20 r20 ta10 r20 ta0 r20 ta350 r20 ta340 r20 ta330 r20 ta320 r20 ta340 l20 ta350 l50 ta340 r20 ta330 r20 ta325 ta320 r20 ta310 r10 ta-20 l20 ta-15 l20 ta-0 l50 bu p2,2 bd" _

DRAW "bl25 bd118 c10 u10 h10 f10 e10 g10 u10 h6 f6 e6 g6 u8 h3 f3 e3"
DRAW "bl170 bu105 c14 u45 e15 r60 f45 l45 r45 g45 d45 g30 l30 d30 u15 f15 h15 g15 e15 u15 l60 d30 u15 f15 h15 g15 e15 u15 l30 u15 e75 u15 u24 br30 h6 u9 e6 r9 f6 d9 g6 l9 be6 bu6 l3"
DRAW  _
"bl150 bd150 c6 l20 ta-30 d24 ta360 r50 ta40 u26 ta360 bd p6,6 bu40 br30 f3 d10 g3 l10 h3 u10 e3 r10 bd p6,6 bu bu16 f3 d10 g3 l10 h3 u10 e3 r10 bd p6,6 bu bu16 f3 d10 g3 l10 h3 u10 e3 r10 bd p6,6 bu bu16 f3 d10 g3 l10 h3 u10 e3 r10 bd p6,6 bu bu16 f3 d10 g3 l10 h3 u10 e3 r10 bd p6,6 bu bd80 bl50 bu16 f3 d10 g3 l10 h3 u10 e3 r10 bd p6,6 bu bu16 f3 d10 g3 l10 h3 u10 e3 r10 bd p6,6 bu bu16 f3 d10 g3 l10 h3 u10 e3 r10 bd p6,6 bu bu16 f3 d10 g3 l10 h3 u10 e3 r10 bd p6,6 bu bu16 f3 d10 g3 l10 h3 u10 e3 r10 bd p6,6 bu" _

DRAW  _
"c9 r40 d75 u75 bl5 c1 d75 u75 bl5 c9 d75 u75 bl5 c1 d75 u75 bl5 c9 d75 u75 bl5 c1 d75 u75 bl5 c9 d75 u75 bl5 c1 d75 u75 bl5 c9 d75 u75 d75 l10 ta30 l10 d10 f10 ta360 r35 ta-30 e10 u10 l10 ta360 l49 bd p9,9 br275 bd48 c10 u10 h15 f15 e15 g15 u10 h10 f10 e10 g10 u8 h5 f5 e5 g5" _

COLOR 14
CIRCLE (520, 100), 50
DRAW "p14,14 l100 r200 l100 g70 e70 d100 u100 f70 h70 e70 g70 u100 d100 h70 f70"
COLOR 15
SOUND 1000, 3
SOUND 0, 1
SOUND 900, 3
SOUND 0, 1
SOUND 800, 3
SOUND 0, 1
SOUND 900, 3
SOUND 0, 1
SOUND 1000, 3
SOUND 0, 1
SOUND 1000, 3
SOUND 0, 1
SOUND 1000, 3
SOUND 0, 5
SOUND 900, 3
SOUND 0, 1
SOUND 900, 3
SOUND 0, 1
SOUND 900, 3
SOUND 0, 5
SOUND 1000, 3
SOUND 0, 1
SOUND 1200, 3
SOUND 0, 1
SOUND 1200, 3
SOUND 0, 5
SOUND 1000, 3
SOUND 0, 1
SOUND 900, 3
SOUND 0, 1
SOUND 800, 3
SOUND 0, 1
SOUND 900, 3
SOUND 0, 1
SOUND 1000, 3
SOUND 0, 1
SOUND 1000, 3
SOUND 0, 1
SOUND 1000, 3
SOUND 0, 1
SOUND 1000, 3
SOUND 0, 1
SOUND 900, 3
SOUND 0, 1
SOUND 900, 3
SOUND 0, 1
SOUND 1000, 3
SOUND 0, 1
SOUND 900, 3
SOUND 0, 1
SOUND 800, 4
PRINT "Would you like to play again? (y/n)"
175
LOCATE 3, 1: INPUT a$
IF a$ = "y" GOTO 10
IF a$ = "n" THEN SYSTEM
SOUND 100, 5
GOTO 175
180 CLS
PRINT "You find yourself at a two-way junction. Exits are north and east."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s8$)
185
a$ = INKEY$
IF a$ = "n" GOTO 200
IF a$ = "e" GOTO 10
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 185
190 CLS
PRINT "You are at a dead-SYSTEM. Exit is south."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s6$)
195
a$ = INKEY$
IF a$ = "s" GOTO 200
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 195
200 CLS
PRINT "You are at a T-junction. Exits are west, south and north."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s4$)
205
a$ = INKEY$
IF a$ = "w" GOTO 210
IF a$ = "n" GOTO 190
IF a$ = "s" GOTO 180
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 205
210 CLS
PRINT "You have come to a dead-SYSTEM. Exit is east."
DRAW "X" + VARPTR$(palm$)
DRAW "X" + VARPTR$(erniesq$)
DRAW "X" + VARPTR$(s1$)
215
a$ = INKEY$
IF a$ = "e" GOTO 200
IF a$ = CHR$(27) OR a$ = "q" THEN SYSTEM
GOTO 215
220 SYSTEM

