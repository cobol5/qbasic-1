'                             ---------------
'                             Û Battlecraft Û
'                             ---------------
'
'         Programmer..................Seah Boon Siew
'         Graphics....................Seah Boon Siew
'         Gameplay....................Seah Boon Siew
'         Missions....................Wong Heng Jian
'                                     Seah Boon Siew
'         Helps & tips................Seah Boon Keong
'         Manual......................Seah Boon Siew
'         Storyline...................Seah Boon Siew
'         Testers.....................Seah Boon Siew
'                                     Wong Heng Jian
'                                     Tang Chee Peng
'                                     Skyter(dunno)
'                                     Desmond(cllee?)
'                                     Raysonc(Rayson Chan)
'                                     ^Bean^(dunno)
'
'
'Hehe my name appears alot  huh:)
'Email me at sbsiew@pl.jaring.my
'
DECLARE SUB drawbox (x!, y!, x2!, y2!)
DECLARE SUB word (seki$, y!, x!)
RANDOMIZE TIMER
DIM SHARED delay
KEY 16, CHR$(0) + CHR$(1)
ON ERROR GOTO errorman
ON KEY(16) GOSUB reestart
KEY(16) ON
DIM level1$(10)
DIM level2$(10)
DIM level3$(10)
DIM level4$(10)
DIM level5$(10)
DIM level6$(10)
DIM level7$(10)
DIM Worde$(10)
DIM wordez$(10)
DIM sek$(7)
terrain$ = "grass"
OPEN "timer.dat" FOR INPUT AS #1
INPUT #1, delay
CLOSE #1
sek$(1) = "Single Player Game"
sek$(2) = "   Continue Game  "
sek$(3) = " Load Custom Game "
sek$(4) = " Enter Map Editor "
sek$(5) = "   Instructions   "
sek$(6) = "    Storyline     "
sek$(7) = "      Exit        "
level1$(1) = "Our first battle plan is to "
level1$(2) = "destroy Anarok's Northen base,"
level1$(3) = "Thor.In order that the invasion"
level1$(4) = "to be smoothly done, you are to"
level1$(5) = "clear the path from any enemy"
level1$(6) = "scouts.Succeed and you will be"
level1$(7) = "promoted."
level1$(10) = "--General--"
level2$(1) = "Congratulations you have cleared"
level2$(2) = "the path well.While our troops "
level2$(3) = "were on the way to the base,"
level2$(4) = "Our scouts has detected an "
level2$(5) = "ambush party west of the base."
level2$(6) = "It might jeopardise our troops."
level2$(7) = "To make our invasion successful,"
level2$(8) = "you must destroy the ambush party."
level2$(9) = "Good luck."
level2$(10) = "--General--"
level3$(1) = "We have captured the base thanks to"
level3$(2) = "you.You are promoted to lieutenant."
level3$(3) = "Three days after capturing Thor,"
level3$(4) = "our opponent Anarok has sent his "
level3$(5) = "troops to counter our attack."
level3$(6) = "You are to ward of the attack."
level3$(10) = "--General--"
level4$(1) = "Now that you have been promoted to"
level4$(2) = "major,you are given a chance to"
level4$(3) = "destroy a base.You're to destroy"
level4$(4) = "Fort Noor.This fort is heavily"
level4$(5) = "guarded by some highly advanced"
level4$(6) = "troops,Ogres.Good luck."
level4$(10) = "--General--"
level5$(1) = "Good job.However, while you are "
level5$(2) = "battling at Fort Noor, our base "
level5$(3) = "in Thor was attacked again and  "
level5$(4) = "have kidnapped our general.You're"
level5$(5) = "to enter Fort Kumbaj and rescue"
level5$(6) = "our general.We will provide you "
level5$(7) = "with an army of ogres to assist "
level5$(8) = "your troop."
level5$(10) = "--Colonel--"
level6$(1) = "Time is running out and we must "
level6$(2) = "continue our invasion.This time"
level6$(3) = "your mission is to take over a "
level6$(4) = "city in Anarok, Guildan.This city"
level6$(5) = "is very important for us and our"
level6$(6) = "opponent.It is heavily guarded by"
level6$(7) = "enemy troops."
level6$(10) = "--General--"
level7$(1) = "Finally you have captured Guildan."
level7$(2) = "This enables us to advance to "
level7$(3) = "Anaroks main base more efficiently."
level7$(4) = "But before that you must destroy a"
level7$(5) = "nearby base in Touk.If you succeed"
level7$(6) = ", we will be able to weaken the main"
level7$(7) = "base's defence."
level7$(10) = "--General--"
level8$(1) = "This will be your most dangerous"
level8$(2) = "battle ,so take care.You are to"
level8$(3) = "invade the southern section of the"
level8$(4) = "main base in Toopaz,Anarok.The "
level8$(5) = "base is heavily guarded and we "
level8$(6) = "are greatly outnumbered.The only "
level8$(7) = "way we could win is to use cunning"
level8$(8) = "stratergies.Good luck."
level8$(10) = "--General--"
level9$(1) = "We should wait no more and begin"
level9$(2) = "attacking the northern section "
level9$(3) = "of the base.This section is even"
level9$(4) = "more heavily guarded than the  "
level9$(5) = "southern section.Thus chances of"
level9$(6) = "victory is slim....but we should "
level9$(7) = "go on, for we will get to enjoy"
level9$(8) = "palams if we succeed."
level9$(10) = "--General--"
wordez$(1) = "          Credits               "
wordez$(2) = "Programmer       Seah Boon Siew "
wordez$(3) = "Graphics         Seah Boon Siew "
wordez$(4) = "Level Design     Seah Boon Siew "
wordez$(5) = "                 Wong Heng Jian "
wordez$(6) = "Missions         Seah Boon Siew "
wordez$(7) = "                 Wong Heng Jian "
wordez$(8) = "Beta Tester      Tang Chee Peng "
wordez$(9) = "                 Raysonc        "
wordez$(10) = "                 Bean & Desmond "
CLS
SCREEN 13
COLOR 12
PRINT "BATTLECRAFT  * 1996 * BY SEAH BOON SIEW ": z = 1.7
FOR x = 0 TO 100
FOR y = 0 TO 11
col = INT(RND * 3) + 64
IF POINT(x, y) = 12 THEN LINE (x * 3.7, y * 5 * z + 12)-(x * 3.7 + 2, y * 5 * z + 12 + 10), col, BF
tt = tt + 1
IF tt > 500 THEN GOTO pompomi
z = z / 1.0008: GOTO guildi
pompomi:
z = z * 1.0008
guildi:
NEXT: NEXT
z = 1.7
tt = 0
drawbox 70, 90, 250, 160
FOR x = 1 TO 7
COLOR 20: LOCATE x + 12, 12: PRINT sek$(x)
NEXT
DO
FOR x = 0 TO 100
FOR y = 0 TO 11
k$ = INKEY$
IF k$ = "1" THEN terrain$ = "grass"
IF k$ = "2" THEN terrain$ = "dry"
IF k$ = "3" THEN terrain$ = "snow"
IF k$ = "4" THEN terrain$ = "night"
IF k$ = CHR$(0) + "P" THEN GOSUB dwn
IF k$ = CHR$(0) + "H" THEN GOSUB up
IF k$ = CHR$(13) THEN
   IF k = 1 THEN GOTO newgame
   IF k = 2 THEN GOTO continuegame
   IF k = 3 THEN GOTO customgame
   IF k = 4 THEN RUN "mapedit.bas"
   IF k = 5 THEN RUN "inst.bas"
   IF k = 6 THEN RUN "story.bas"
   IF k = 7 THEN GOTO ending
END IF
col = INT(RND * 3) + 64
IF POINT(x, y) = 12 THEN LINE (x * 3.7, y * 5 * z + 12)-(x * 3.7 + 2, y * 5 * z + 12 + 10), col, BF
tt = tt + 1
IF tt > 500 THEN GOTO pompom
z = z / 1.0008: GOTO guild
pompom:
z = z * 1.0008
guild:
NEXT: NEXT
tt = 0
z = 1.7
LOOP

k$ = INPUT$(1)
dwn:
SOUND 100, .1
COLOR 22: LOCATE k + 12, 12: PRINT sek$(k)
k = k + 1
IF k > 7 THEN k = 1
COLOR 25: LOCATE k + 12, 11: word sek$(k), k + 12, 11
RETURN
up:
SOUND 100, .1
COLOR 22: LOCATE k + 12, 12: PRINT sek$(k)
k = k - 1
IF k < 1 THEN k = 7
COLOR 25: LOCATE k + 12, 11: word sek$(k), k + 12, 11
RETURN

newgame:
level = 1
DO
LINE (boxclosex, boxclosey)-(320 - boxclosex, 200 - boxclosey), 0, B
boxclosex = boxclosex + 1
boxclosey = boxclosey + 1
FOR lag = 1 TO delay / 30: NEXT
LOOP UNTIL boxclosex > 160 AND boxclosey > 100
GOSUB story
OPEN "game.dat" FOR OUTPUT AS #1
PRINT #1, "level1.lev"
PRINT #1, "1"
PRINT #1, "grass"
CLOSE #1
CLS
FOR x = 1 TO 12
COLOR x + 18: LOCATE x + 1, 15: PRINT "Level 1"
COLOR x + 18: LOCATE 23 - x, 15: PRINT "Level 1"
FOR lag = 1 TO delay / 10: NEXT
NEXT
FOR x = 1 TO 11
COLOR 0: LOCATE x + 1, 15: PRINT "Level 1"
COLOR 0: LOCATE 23 - x, 15: PRINT "Level 1"
FOR lag = 1 TO delay / 10: NEXT
NEXT
COLOR 15: LOCATE 12, 15: PRINT "Level 1"
GOSUB drawarena
GOSUB printstory
k$ = INPUT$(1)
RUN "war2.bas"
story:
RETURN
customgame:
drawbox 70, 90, 250, 160
LOCATE 16, 11: LINE INPUT "Name:"; new$
IF new$ = "" THEN RUN "present.bas"
IF RIGHT$(new$, 3) = "lev" THEN
LOCATE 16, 11: PRINT "Can't load Lev Files"
k$ = INPUT$(1)
RUN "present.bas"
END IF
OPEN win$ FOR INPUT AS #1
CLOSE #1
OPEN "game.dat" FOR OUTPUT AS #1
PRINT #1, new$
PRINT #1, 99
PRINT #1, terrain$
CLOSE #1
RUN "war2.bas"
continuegame:
drawbox 70, 90, 250, 160
LOCATE 16, 11: LINE INPUT "Password:"; pass$
IF pass$ = "" THEN RUN "present.bas"
IF pass$ = "sonc" THEN
      level = 2
      levelname$ = "level2.lev"
      terrain$ = "night"
      GOTO contgame
END IF
IF pass$ = "mole" THEN
      level = 3
      levelname$ = "level3.lev"
      terrain$ = "grass"
      GOTO contgame
END IF
IF pass$ = "goat" THEN
      level = 4
      levelname$ = "level4.lev"
      terrain$ = "dry"
      GOTO contgame
END IF
IF pass$ = "sock" THEN
      level = 5
      levelname$ = "level5.lev"
      terrain$ = "dry"
      GOTO contgame
END IF
IF pass$ = "rune" THEN
      level = 6
      levelname$ = "level6.lev"
      terrain$ = "dry"
      GOTO contgame
END IF
IF pass$ = "bean" THEN
      level = 7
      levelname$ = "level7.lev"
      terrain$ = "snow"
      GOTO contgame
END IF
IF pass$ = "wtts" THEN
      level = 8
      levelname$ = "level8.lev"
      terrain$ = "snow"
      GOTO contgame
END IF
IF pass$ = "quaint" THEN
      level = 9
      levelname$ = "level9.lev"
      terrain$ = "snow"
      GOTO contgame
END IF
SOUND 100, .5: GOTO continuegame
contgame:
DO
LINE (boxclosex, boxclosey)-(320 - boxclosex, 200 - boxclosey), 0, B
boxclosex = boxclosex + 1
boxclosey = boxclosey + 1
FOR lag = 1 TO delay / 30: NEXT
LOOP UNTIL boxclosex > 160 AND boxclosey > 100
GOSUB story
OPEN "game.dat" FOR OUTPUT AS #1
PRINT #1, levelname$
PRINT #1, level
PRINT #1, terrain$
CLOSE #1
CLS
FOR x = 1 TO 12
COLOR x + 18: LOCATE x + 1, 15: PRINT "Level "; level
COLOR x + 18: LOCATE 23 - x, 15: PRINT "Level "; level
FOR lag = 1 TO delay / 10: NEXT
NEXT
FOR x = 1 TO 11
COLOR 0: LOCATE x + 1, 15: PRINT "Level "; level
COLOR 0: LOCATE 23 - x, 15: PRINT "Level "; level
FOR lag = 1 TO delay / 10: NEXT
NEXT
COLOR 15: LOCATE 12, 15: PRINT "Level "; level
k$ = INPUT$(1)
GOSUB drawarena
GOSUB printstory
k$ = INPUT$(1)
RUN "war2.bas"

drawarena:
drawbox 5, 10, 310, 155
FOR x = 1 TO 14
COLOR x + 17: LOCATE 23, 1 + x: PRINT "Û"
COLOR x + 17: LOCATE 23, 38 - x: PRINT "Û"
FOR lag = 1 TO delay / 10: NEXT
NEXT
COLOR 11: LOCATE 23, 15: PRINT " Level"; level
RETURN
printstory:
COLOR 15
FOR x = 1 TO 10
IF level = 1 THEN Worde$(x) = level1$(x)
IF level = 2 THEN Worde$(x) = level2$(x)
IF level = 3 THEN Worde$(x) = level3$(x)
IF level = 4 THEN Worde$(x) = level4$(x)
IF level = 5 THEN Worde$(x) = level5$(x)
IF level = 6 THEN Worde$(x) = level6$(x)
IF level = 7 THEN Worde$(x) = level7$(x)
IF level = 8 THEN Worde$(x) = level8$(x)
IF level = 9 THEN Worde$(x) = level9$(x)
NEXT
LOCATE 4, 6: word Worde$(1), 4, 3
LOCATE 5, 6: word Worde$(2), 5, 3
LOCATE 6, 6: word Worde$(3), 6, 3
LOCATE 7, 6: word Worde$(4), 7, 3
LOCATE 8, 6: word Worde$(5), 8, 3
LOCATE 9, 6: word Worde$(6), 9, 3
LOCATE 10, 6: word Worde$(7), 10, 3
LOCATE 11, 6: word Worde$(8), 11, 3
LOCATE 12, 6: word Worde$(9), 12, 3
LOCATE 20, 16: word Worde$(10), 19, 14
RETURN

reestart:
RUN "present.bas"
ending:
CLS
drawbox 5, 10, 310, 185
FOR x = 1 TO 14
COLOR x + 17: LOCATE 3, 1 + x: PRINT "Û"
COLOR x + 17: LOCATE 3, 38 - x: PRINT "Û"
FOR lag = 1 TO delay / 10: NEXT
NEXT
COLOR 11: LOCATE 3, 15: PRINT " Credits "
LOCATE 5, 6: word wordez$(2), 5, 3
LOCATE 6, 6: word wordez$(3), 7, 3
LOCATE 7, 6: word wordez$(4), 9, 3
LOCATE 8, 6: word wordez$(5), 11, 3
LOCATE 9, 6: word wordez$(6), 13, 3
LOCATE 10, 6: word wordez$(7), 15, 3
LOCATE 11, 6: word wordez$(8), 17, 3
LOCATE 12, 6: word wordez$(9), 19, 3
LOCATE 20, 16: word wordez$(10), 21, 3
k$ = INPUT$(1)
CLS
SYSTEM

errorman:
IF ERR = 52 THEN LOCATE 16, 11: PRINT "  File not found!": k$ = INPUT$(1): RUN "present.bas"
RESUME j
j:
SCREEN 0: WIDTH 80
CLS
PRINT "An Error Has Occured!"
PRINT "Error code:"; ERR
PRINT "Press any key to restart game"
k$ = INPUT$(1)
RUN "present.bas"

SUB drawbox (x, y, x2, y2)
p = 0
LINE (x, y)-(x2, y2), 0, BF
FOR colorx = 20 TO 30 STEP 2
p = p + 1
LINE (x - p, y - p)-(x2 + p, y2 + p), colorx, B
NEXT
p = 0
END SUB

SUB word (seki$, y, x)
kmax = LEN(seki$)
FOR cl = 20 TO 29
FOR lag = 1 TO delay / 15: NEXT lag
FOR k = 1 TO kmax
psent$ = MID$(seki$, k, 1)
COLOR cl: LOCATE y, x + k: PRINT psent$
NEXT
NEXT
END SUB

