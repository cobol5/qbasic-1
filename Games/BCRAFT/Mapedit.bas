DECLARE SUB initimages ()
DECLARE SUB drawbox (x!, y!, x2!, y2!)
DECLARE SUB Word (seki$, y!, x!)
DECLARE SUB mousehide ()
DECLARE FUNCTION person$ (buttonx!)
DECLARE SUB enlarge (buttonx!)
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
DIM SHARED map$(32, 32)
DIM SHARED delay
ON ERROR GOTO psx
filename$ = "test.lev"
bmp "test2.bmp", "Image1"
initimages
sek$(1) = "  Load Custom Map   "
sek$(2) = " Create Custom Map  "
sek$(3) = "Custom Map Directory"
sek$(4) = " Exit to Main Menu  "
OPEN "timer.dat" FOR INPUT AS #1
INPUT #1, delay
CLOSE #1
restarty:
CLS
drawbox 70, 60, 250, 120
FOR yun = 1 TO 4
COLOR 72: LOCATE 9 + yun, 11: PRINT sek$(yun)
NEXT
DO
k$ = INKEY$
IF k$ = CHR$(0) + "P" THEN GOSUB dwn
IF k$ = CHR$(0) + "H" THEN GOSUB up
IF k$ = CHR$(13) THEN
IF k = 1 THEN GOSUB mvbox: GOSUB loader: GOTO pinch
IF k = 2 THEN GOSUB mvbox: GOSUB maker: GOTO pinch
IF k = 3 THEN CLS : FILES "*.map": k$ = INPUT$(1): GOTO restarty
IF k = 4 THEN RUN "present.bas"
END IF
LOOP
mvbox:
drawbox 70, 50, 250, 120
COLOR 252: LOCATE 9, 15: PRINT "Filename"
COLOR 76: LOCATE 11, 10: LINE INPUT "Name:"; filename$
RETURN
loader:
OPEN filename$ FOR INPUT AS #1
FOR xmap = 1 TO 30
FOR ymap = 1 TO 30
INPUT #1, map$(xmap, ymap)
NEXT: NEXT
CLOSE #1
RETURN
maker:
OPEN filename$ FOR OUTPUT AS #1
FOR xmap = 1 TO 30
FOR ymap = 1 TO 30
PRINT #1, "grass"
map$(xmap, ymap) = "grass"
NEXT: NEXT
CLOSE #1
RETURN
pinch:
CLS
PUT (21, 170), pult2
PUT (41, 170), pult
PUT (61, 170), ogre
PUT (81, 170), ogre2
PUT (101, 170), skirm1
PUT (121, 170), skirm2
PUT (141, 170), man1
PUT (161, 170), man2
PUT (181, 170), arch
PUT (201, 170), arch2
PUT (221, 170), grass
PUT (241, 170), tree
PUT (261, 170), rock

RESTORE
mouse$ = SPACE$(57)
FOR I% = 1 TO 57
  READ A$
  H$ = CHR$(VAL("&H" + A$))
  MID$(mouse$, I%, 1) = H$
NEXT I%
DATA 55,89,E5,8B,5E,0C,8B,07,50,8B,5E,0A,8B,07,50,8B
DATA 5E,08,8B,0F,8B,5E,06,8B,17,5B,58,1E,07,CD,33,53
DATA 8B,5E,0C,89,07,58,8B,5E,0A,89,07,8B,5E,08,89,0F
DATA 8B,5E,06,89,17,5D,CA,08,00

MS% = MouseInit%
IF NOT MS% THEN
  PRINT "Mouse not found": LET AMOUSE$ = "NO":


END IF
LET AMOUSE$ = "YES"
refresh:
FOR xmap = 1 TO 30
FOR ymap = 1 TO 30
IF map$(xmap, ymap) = "1cat" THEN map$(xmap, ymap) = "1pult"
IF map$(xmap, ymap) = "grass" THEN LINE (xmap * 5 + 1, ymap * 5 + 1)-(xmap * 5 - 1, ymap * 5 - 1), 152, BF
IF map$(xmap, ymap) <> "grass" THEN LINE (xmap * 5 + 1, ymap * 5 + 1)-(xmap * 5 - 1, ymap * 5 - 1), 70, BF
IF map$(xmap, ymap) = "1pult" THEN LINE (xmap * 5 + 1, ymap * 5 + 1)-(xmap * 5 - 1, ymap * 5 - 1), 158, BF
IF map$(xmap, ymap) = "2pult" THEN LINE (xmap * 5 + 1, ymap * 5 + 1)-(xmap * 5 - 1, ymap * 5 - 1), 159, BF
IF map$(xmap, ymap) = "1man" THEN LINE (xmap * 5 + 1, ymap * 5 + 1)-(xmap * 5 - 1, ymap * 5 - 1), 171, BF
IF map$(xmap, ymap) = "2man" THEN LINE (xmap * 5 + 1, ymap * 5 + 1)-(xmap * 5 - 1, ymap * 5 - 1), 174, BF
IF map$(xmap, ymap) = "1arch" THEN LINE (xmap * 5 + 1, ymap * 5 + 1)-(xmap * 5 - 1, ymap * 5 - 1), 220, BF
IF map$(xmap, ymap) = "2arch" THEN LINE (xmap * 5 + 1, ymap * 5 + 1)-(xmap * 5 - 1, ymap * 5 - 1), 221, BF
IF map$(xmap, ymap) = "1ogre" THEN LINE (xmap * 5 + 1, ymap * 5 + 1)-(xmap * 5 - 1, ymap * 5 - 1), 34, BF
IF map$(xmap, ymap) = "2ogre" THEN LINE (xmap * 5 + 1, ymap * 5 + 1)-(xmap * 5 - 1, ymap * 5 - 1), 37, BF
IF map$(xmap, ymap) = "1skirm" THEN LINE (xmap * 5 + 1, ymap * 5 + 1)-(xmap * 5 - 1, ymap * 5 - 1), 247, BF
IF map$(xmap, ymap) = "2skirm" THEN LINE (xmap * 5 + 1, ymap * 5 + 1)-(xmap * 5 - 1, ymap * 5 - 1), 248, BF
IF map$(xmap, ymap) = "grass" THEN LINE (xmap * 5 + 1, ymap * 5 + 1)-(xmap * 5 - 1, ymap * 5 - 1), 152, BF
IF map$(xmap, ymap) = "farm" THEN LINE (xmap * 5 + 1, ymap * 5 + 1)-(xmap * 5 - 1, ymap * 5 - 1), 148, BF
IF map$(xmap, ymap) = "rock" THEN LINE (xmap * 5 + 1, ymap * 5 + 1)-(xmap * 5 - 1, ymap * 5 - 1), 98, BF
NEXT: NEXT
mouseshow
select$ = "grass"
LOCATE 16, 23: PRINT "Exit"
LOCATE 16, 30: PRINT "Save"
DO
k$ = INKEY$
IF k$ = "r" THEN mousehide: GOTO refresh
IF k$ = "s" THEN mousehide: GOTO saver
mousestatus lb%, RB%, xmouse%, ymouse%
mxbox = INT(xmouse% / 10)
mybox = INT(ymouse% / 5)
buttonx = INT(xmouse% / 40)
buttony = INT(ymouse% / 28)
LOCATE 20: PRINT mxbox; mybox
IF mxbox > 0 AND mxbox < 31 THEN
IF mybox > 0 AND mybox < 31 THEN
      LOCATE 20, 10: PRINT map$(mxbox, mybox) + "  "
END IF
END IF
IF lb% = -1 THEN
        IF xmouse% > 460 AND xmouse% < 530 THEN
        IF ymouse% > 115 AND ymouse% < 130 THEN
mousehide: GOTO saver
        END IF
        END IF
        IF xmouse% > 340 AND xmouse% < 420 THEN
        IF ymouse% > 115 AND ymouse% < 130 THEN
                RUN "present.bas"
        END IF
        END IF
        IF buttonx > 0 AND buttonx < 14 THEN
        IF buttony > 5 AND buttony < 7 THEN
                  select$ = person$(buttonx)
                  enlarge buttonx
        END IF
        END IF
        IF mxbox > 0 AND mxbox < 31 THEN
        IF mybox > 0 AND mybox < 31 THEN
                  map$(mxbox, mybox) = select$
                  IF select$ = "1pult" THEN LINE (mxbox * 5 + 1, mybox * 5 + 1)-(mxbox * 5 - 1, mybox * 5 - 1), 158, BF
                  IF select$ = "2pult" THEN LINE (mxbox * 5 + 1, mybox * 5 + 1)-(mxbox * 5 - 1, mybox * 5 - 1), 159, BF
                  IF select$ = "1man" THEN LINE (mxbox * 5 + 1, mybox * 5 + 1)-(mxbox * 5 - 1, mybox * 5 - 1), 171, BF
                  IF select$ = "2man" THEN LINE (mxbox * 5 + 1, mybox * 5 + 1)-(mxbox * 5 - 1, mybox * 5 - 1), 174, BF
                  IF select$ = "1arch" THEN LINE (mxbox * 5 + 1, mybox * 5 + 1)-(mxbox * 5 - 1, mybox * 5 - 1), 220, BF
                  IF select$ = "2arch" THEN LINE (mxbox * 5 + 1, mybox * 5 + 1)-(mxbox * 5 - 1, mybox * 5 - 1), 221, BF
                  IF select$ = "1ogre" THEN LINE (mxbox * 5 + 1, mybox * 5 + 1)-(mxbox * 5 - 1, mybox * 5 - 1), 34, BF
                  IF select$ = "2ogre" THEN LINE (mxbox * 5 + 1, mybox * 5 + 1)-(mxbox * 5 - 1, mybox * 5 - 1), 37, BF
                  IF select$ = "1skirm" THEN LINE (mxbox * 5 + 1, mybox * 5 + 1)-(mxbox * 5 - 1, mybox * 5 - 1), 247, BF
                  IF select$ = "2skirm" THEN LINE (mxbox * 5 + 1, mybox * 5 + 1)-(mxbox * 5 - 1, mybox * 5 - 1), 248, BF
                  IF select$ = "grass" THEN LINE (mxbox * 5 + 1, mybox * 5 + 1)-(mxbox * 5 - 1, mybox * 5 - 1), 152, BF
                  IF select$ = "farm" THEN LINE (mxbox * 5 + 1, mybox * 5 + 1)-(mxbox * 5 - 1, mybox * 5 - 1), 148, BF
                  IF select$ = "rock" THEN LINE (mxbox * 5 + 1, mybox * 5 + 1)-(mxbox * 5 - 1, mybox * 5 - 1), 98, BF
        END IF
        END IF
END IF
LOOP
saver:
OPEN filename$ FOR OUTPUT AS #1
FOR x = 1 TO 30
FOR y = 1 TO 30
PRINT #1, map$(x, y)
NEXT: NEXT
CLOSE #1
RUN "present.bas"
dwn:
SOUND 100, .1
COLOR 72: LOCATE k + 9, 11: PRINT sek$(k)
k = k + 1
IF k > 4 THEN k = 1
COLOR 25: LOCATE k + 9, 10: Word sek$(k), k + 9, 10
RETURN
up:
SOUND 100, .1
COLOR 72: LOCATE k + 9, 11: PRINT sek$(k)
k = k - 1
IF k < 1 THEN k = 4
COLOR 25: LOCATE k + 9, 10: Word sek$(k), k + 9, 10
RETURN

psx:
IF ERR = 53 THEN LOCATE 11, 10: PRINT "   File not found!": k$ = INPUT$(1): RUN "present.bas"
RESUME ki
ki:

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

SUB enlarge (buttonx)
mousehide
bx = buttonx * 20
by = 170
FOR x = 1 TO 20
FOR y = 1 TO 29
xx = x * 3
yy = y * 3
gcol = POINT(bx + x, 170 + y)
LINE (xx + 190, yy + 20)-(xx + 3 + 190, yy + 3 + 20), gcol, BF
NEXT: NEXT
mouseshow
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
DEF SEG = VARSEG(rock(1))
BLOAD "rock.img", VARPTR(rock(1))

END SUB

SUB MouseDriver (AX%, bx%, CX%, DX%)
  DEF SEG = VARSEG(mouse$)
  mouse% = SADD(mouse$)
  CALL Absolute(AX%, bx%, CX%, DX%, mouse%)
END SUB

SUB mousehide
 AX% = 2
 MouseDriver AX%, 0, 0, 0
END SUB

FUNCTION MouseInit%
  AX% = 0
  MouseDriver AX%, 0, 0, 0
  MouseInit% = AX%

END FUNCTION

SUB MousePut
  AX% = 4
  CX% = x%
  DX% = y%
  MouseDriver AX%, 0, CX%, DX%
END SUB

SUB mouseshow
  AX% = 1
  MouseDriver AX%, 0, 0, 0
END SUB

SUB mousestatus (lb%, RB%, xmouse%, ymouse%)
  AX% = 3
  MouseDriver AX%, bx%, CX%, DX%
  lb% = ((bx% AND 1) <> 0)
  RB% = ((bx% AND 2) <> 0)
  xmouse% = CX%
  ymouse% = DX%
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

SUB STICKS (Joyx%, JOYY%, BUT1%, BUt2%, BUT3%)
Joyx% = STICK(0)
JOYY% = STICK(1)

BUT1% = STRIG(1)
BUt2% = STRIG(5)
BUT3% = STRIG(7)
END SUB

SUB Word (seki$, y, x)
kmax = LEN(seki$)
FOR cl = 70 TO 78
FOR lag = 1 TO delay / 15: NEXT lag
FOR k = 1 TO kmax
psent$ = MID$(seki$, k, 1)
COLOR cl: LOCATE y, x + k: PRINT psent$
NEXT
NEXT

END SUB

