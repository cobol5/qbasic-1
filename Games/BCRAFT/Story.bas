DECLARE SUB axe (x!, y!, colo!)
DECLARE SUB initimages ()
DECLARE SUB drawbox (x!, y!, x2!, y2!)
DECLARE SUB word (seki$, y!, x!)
DECLARE SUB mousehide ()
DECLARE FUNCTION person$ (buttonx!)
DECLARE SUB enlarge (x!, y!)
DECLARE SUB MouseDriver (AX%, bx%, CX%, DX%)
DECLARE FUNCTION MouseInit% ()
DECLARE SUB mouseshow ()
DECLARE SUB mousestatus (lb%, RB%, xmouse%, ymouse%)
DECLARE SUB STICKS (Joyx%, JOYY%, BUT1%, BUt2%, BUT3%)
DECLARE SUB bmp (filename$, x$)
DECLARE SUB openpic (filename$)
DIM SHARED tree(1 TO 500)
DIM SHARED man2(1 TO 500)
DIM SHARED man1(1 TO 500)
DIM SHARED grass(1 TO 500)
DIM SHARED ogre(1 TO 500)
DIM SHARED arch(1 TO 500)
DIM SHARED pult(1 TO 500)
DIM SHARED ogre2(1 TO 500)
DIM SHARED arch2(1 TO 500)
DIM SHARED pult2(1 TO 500)
DIM SHARED skirm1(1 TO 500)
DIM SHARED skirm2(1 TO 500)
DIM SHARED rock(1 TO 500)
DIM SHARED mouse$
DIM SHARED choice$(10, 10)
DIM SHARED map(1 TO 1600)
DIM SHARED delay
OPEN "timer.dat" FOR INPUT AS #1
INPUT #1, delay
CLOSE #1
filename$ = "test.lev"
KEY 16, CHR$(0) + CHR$(1)
ON KEY(16) GOSUB reestart
KEY(16) ON
SCREEN 13
bmp "test2.bmp", "Image1"
CLS
word "Once upon a time in Woolar, everyone", 2, 1
word "and their cats were unhappy because", 4, 1
word "there are'nt any Palams in Woolar.", 6, 1
word "Everyday they whined and whined to ", 8, 1
word "the King but to no avail.Finally one", 10, 1
word "day , the King got an idea to obtain", 12, 1
word "those precious Palams.They are going", 14, 1
word "to war with Anarok, a country full ", 16, 1
word "of Palams.You,the Corporal and General", 18, 1
word "Woxi will lead the battle......", 20, 1
k$ = INPUT$(1)
CLS
initimages
PUT (1, 1), map, PSET
LOCATE 3, 25: PRINT "Zooming"
enlarge 1, 1
VIEW PRINT 1 TO 7
CLS 2
word "Hello I am General Woxi.I will tell", 1, 1
word "you some of the battle plans.......", 2, 1
FOR lag = 1 TO delay * 4: NEXT lag
word "We will land our troops at here, near", 3, 1
word "Vombak(West Anarok)............... ", 4, 1
FOR lag = 1 TO delay: NEXT lag
axe 50, 150, 15
word "Our backup troops will be placed abit", 5, 1
word "to the north, you will be leading them", 6, 1
FOR lag = 1 TO delay: NEXT lag
axe 80, 100, 12
FOR lag = 1 TO delay * 4: NEXT lag
CLS 2
word "My troops will be heading towards ", 1, 1
word "base A in the next five days.....", 2, 1
FOR t = 1 TO 80
LINE (50, 150)-(50 + t, 150), 15
FOR lag = 1 TO delay / 15: NEXT lag
NEXT t
word "Your first mission is to look for ", 3, 1
word "enemy scouts around this area....", 4, 1
FOR x = 70 TO 78
CIRCLE (100, 140), 40, x
FOR lag = 1 TO delay / 5: NEXT lag
NEXT
SOUND 200, .5
FOR lag = 1 TO delay * 4: NEXT lag
word "Good Luck.                       ", 5, 1
k$ = INPUT$(1)
reestart: RUN "present.bas"

SUB axe (x, y, colo)
FOR j = 1 TO 5
LINE (x - j, y - j)-(x + j, y + j), colo
LINE (x + j, y - j)-(x - j, y + j), colo
SOUND 100, .1
FOR lag = 1 TO delay / 3: NEXT lag
NEXT
END SUB

SUB bmp (filename$, x$)
CLS
OPEN filename$ FOR BINARY AS #1

header$ = SPACE$(14)
sizing$ = SPACE$(4)
GET #1, 1, header$
GET #1, 15, sizing$
bmpinfosize = CVI(sizing$)
IF bmpinfosize = 12 THEN
   infoheader$ = SPACE$(12)
   GET #1, 15, infoheader$
   nbits = CVI(MID$(infoheader$, 15, 4))

   IF nbits = 1 THEN
      palet$ = SPACE$(6)
      GET #1, bmpinfosize + 15, palet$
   ELSEIF nbits = 4 THEN
      palet$ = SPACE$(48)
      GET #1, bmpinfosize + 15, palet$
   ELSEIF nbits = 8 THEN
      palet$ = SPACE$(768)
      GET #1, bmpinfosize + 15, palet$
   END IF
ELSEIF bmpinfosize = 40 THEN
   infoheader$ = SPACE$(40)
   GET #1, 15, infoheader$
   nbits = CVI(MID$(infoheader$, 15, 4))
   IF nbits = 1 THEN
      palet$ = SPACE$(8)
      GET #1, bmpinfosize + 15, palet$
   ELSEIF nbits = 4 THEN
      palet$ = SPACE$(64)
      GET #1, bmpinfosize + 15, palet$
   ELSEIF nbits = 8 THEN
      palet$ = SPACE$(1024)
      GET #1, bmpinfosize + 15, palet$
   END IF
END IF
  

ft$ = MID$(header$, 1, 2)

filesize = CVL(MID$(header$, 3, 4))

r1 = CVI(MID$(header$, 7, 2))

r2 = CVI(MID$(header$, 9, 2))

offset = CVL(MID$(header$, 11, 4))


headersize = CVL(MID$(infoheader$, 1, 4))

picwidth = CVL(MID$(infoheader$, 5, 4))

picheight = CVL(MID$(infoheader$, 9, 4))

nplanes = CVI(MID$(infoheader$, 13, 4))


IF headersize = 40 THEN
   comptype = CVL(MID$(infoheader$, 17, 4))

   imagesize = CVL(MID$(infoheader$, 21, 4))

   xsize = CVL(MID$(infoheader$, 25, 4))

   ysize = CVL(MID$(infoheader$, 29, 4))

   colorsused = CVL(MID$(infoheader$, 33, 4))

   neededcolours = CVL(MID$(infoheader$, 37, 4))
END IF

IF nbits = 1 THEN
   SCREEN 11
ELSEIF nbits = 4 THEN
   SCREEN 12
ELSEIF nbits = 8 OR nbits = 24 THEN
   SCREEN 13
END IF
LOCATE 1, 40: PRINT "Loading Palette..."
IF bmpinfosize = 40 THEN ngroups = 4
IF bmpinfosize = 12 THEN ngroups = 3

IF nbits = 24 THEN
   IF ngroups = 3 THEN
      FOR c = 0 TO 63
         d = c * 4
         palet$ = palet$ + CHR$(d) + CHR$(d) + CHR$(d)
         palet$ = palet$ + CHR$(d) + CHR$(d) + CHR$(d + 1)
         palet$ = palet$ + CHR$(d) + CHR$(d + 1) + CHR$(d)
         palet$ = palet$ + CHR$(d + 1) + CHR$(d) + CHR$(d)
      NEXT c
   ELSEIF ngroups = 4 THEN
      FOR c = 0 TO 63
         d = c * 4
         palet$ = palet$ + CHR$(d) + CHR$(d) + CHR$(d) + CHR$(0)
         palet$ = palet$ + CHR$(d) + CHR$(d) + CHR$(d + 1) + CHR$(0)
         palet$ = palet$ + CHR$(d) + CHR$(d + 1) + CHR$(d) + CHR$(0)
         palet$ = palet$ + CHR$(d + 1) + CHR$(d) + CHR$(d) + CHR$(0)
      NEXT c
   END IF
END IF

FOR x = 1 TO LEN(palet$) STEP ngroups
   zb# = INT((ASC(MID$(palet$, x, 1))) / 4)
   zg# = INT((ASC(MID$(palet$, x + 1, 1))) / 4)
   zr# = INT((ASC(MID$(palet$, x + 2, 1))) / 4)
   zc# = zb# * 65536# + zg# * 256# + zr#
   cres = ASC(MID$(palet$, x + 3, 1))
   PALETTE ((x - 1) / ngroups), zc#
NEXT x

IF nbits = 24 THEN
   y = picheight - 1
   x = 0
   dat$ = "   "
   WHILE y >= 0
      WHILE x < picwidth
         GET 1, , dat$
         p1 = INT((ASC(MID$(dat$, 1, 1)) + ASC(MID$(dat$, 1, 1)) + ASC(MID$(dat$, 1, 1))) / 3)
         x = x + 1
      WEND
      y = y - 1
      x = 0
   WEND
ELSEIF nbits = 8 THEN
   y = picheight - 1
   x = 0
   dat$ = " "
   WHILE y >= 0
      WHILE x < picwidth
         GET 1, , dat$
         x = x + 1
      WEND
      y = y - 1
      x = 0
   WEND
ELSEIF nbits = 4 THEN
   y = picheight - 1
   x = 0
   dat$ = " "
   WHILE y >= 0
      WHILE x < picwidth
        GET 1, , dat$
        LOCATE 1, 1
        p1 = ASC(dat$) AND 15
        p2 = ASC(dat$) AND 240 / 16
        x = x + 2
      WEND
      y = y - 1
      x = 0
   WEND
ELSEIF nbits = 1 THEN
   y = picheight - 1
   x = 0
   dat$ = " "
   WHILE y >= 0
      WHILE x < picwidth
        GET 1, , dat$
        p1 = ASC(dat$)
        FOR p = 0 TO 7
        NEXT p
        x = x + 8
      WEND
      y = y - 1
      x = 0
   WEND
END IF

CLOSE


END SUB

SUB drawbox (x, y, x2, y2)
p = 0
LINE (x, y)-(x2, y2), 0, BF
FOR colorx = 70 TO 79 STEP 2
p = p + 1
LINE (x - p, y - p)-(x2 + p, y2 + p), colorx, B
NEXT
p = 0

END SUB

SUB enlarge (x, y)
bx = buttonx * 20
by = 170
FOR x = 1 TO 99
FOR y = 1 TO 49
xx = x * 3
yy = y * 3
gcol = POINT(x, y)
LINE (xx, yy + 50)-(xx + 2, yy + 2 + 50), gcol, BF
NEXT: NEXT
END SUB

SUB initimages
DEF SEG = VARSEG(grass(1))
BLOAD "image2.img", VARPTR(grass(1))
DEF SEG = VARSEG(man1(1))
BLOAD "footman.img", VARPTR(man1(1))
DEF SEG = VARSEG(man2(1))
BLOAD "foot2.img", VARPTR(man2(1))
DEF SEG = VARSEG(ogre(1))
BLOAD "ogre.img", VARPTR(ogre(1))
DEF SEG = VARSEG(ogre2(1))
BLOAD "ogre2.img", VARPTR(ogre2(1))
DEF SEG = VARSEG(arch(1))
BLOAD "archer.img", VARPTR(arch(1))
DEF SEG = VARSEG(arch2(1))
BLOAD "archer2.img", VARPTR(arch2(1))
DEF SEG = VARSEG(pult(1))
BLOAD "catapult.img", VARPTR(pult(1))
DEF SEG = VARSEG(pult2(1))
BLOAD "cat2.img", VARPTR(pult2(1))
DEF SEG = VARSEG(skirm1(1))
BLOAD "skirmish.img", VARPTR(skirm1(1))
DEF SEG = VARSEG(skirm2(1))
BLOAD "skirm2.img", VARPTR(skirm2(1))
DEF SEG = VARSEG(tree(1))
BLOAD "tree.img", VARPTR(tree(1))
DEF SEG = VARSEG(map(1))
BLOAD "map.img", VARPTR(map(1))

END SUB

SUB openpic (filename$)
OPEN filename$ FOR INPUT AS #1
FOR x = 1 TO 20
FOR y = 1 TO 30
INPUT #1, j
PSET (x, y), j
NEXT y
NEXT x
CLOSE #1

END SUB

FUNCTION person$ (buttonx)
IF buttonx = 1 THEN person$ = "2pult"
IF buttonx = 2 THEN person$ = "1pult"
IF buttonx = 3 THEN person$ = "1ogre"
IF buttonx = 4 THEN person$ = "2ogre"
IF buttonx = 5 THEN person$ = "1skirm"
IF buttonx = 6 THEN person$ = "2skirm"
IF buttonx = 7 THEN person$ = "1man"
IF buttonx = 8 THEN person$ = "2man"
IF buttonx = 9 THEN person$ = "1arch"
IF buttonx = 10 THEN person$ = "2arch"
IF buttonx = 11 THEN person$ = "grass"
IF buttonx = 12 THEN person$ = "farm"
IF buttonx = 13 THEN person$ = "rock"
END FUNCTION

SUB putimage (select$)
IF select$ = "2pult" THEN PUT (281, 30), pult2, PSET
IF select$ = "1pult" THEN PUT (281, 30), pult, PSET
IF select$ = "1ogre" THEN PUT (281, 30), ogre, PSET
IF select$ = "2ogre" THEN PUT (281, 30), ogre2, PSET
IF select$ = "1skirm" THEN PUT (281, 30), skirm1, PSET
IF select$ = "2skirm" THEN PUT (281, 30), skirm2, PSET
IF select$ = "1man" THEN PUT (281, 30), man1, PSET
IF select$ = "2man" THEN PUT (281, 30), man2, PSET
IF select$ = "1arch" THEN PUT (281, 30), arch, PSET
IF select$ = "2arch" THEN PUT (281, 30), arch2, PSET
IF select$ = "grass" THEN PUT (281, 30), grass, PSET
IF select$ = "farm" THEN PUT (281, 30), tree, PSET
IF select$ = "rock" THEN PUT (281, 30), rock, PSET

END SUB

SUB word (seki$, y, x)
kmax = LEN(seki$)
FOR cl = 70 TO 78
FOR lag = 1 TO delay / 10: NEXT lag
FOR k = 1 TO kmax
psent$ = MID$(seki$, k, 1)
COLOR cl: LOCATE y, x + k: PRINT psent$
NEXT
NEXT

END SUB

