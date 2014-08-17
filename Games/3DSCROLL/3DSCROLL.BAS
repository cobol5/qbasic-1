DECLARE SUB break (value AS INTEGER)
DECLARE SUB flip ()
DECLARE SUB wind (sz AS INTEGER)
DECLARE SUB box (xp AS INTEGER, yp AS INTEGER, zp AS INTEGER, xsz AS INTEGER, ysz AS INTEGER, zsz AS INTEGER, n1 AS INTEGER, n2 AS INTEGER, n3 AS INTEGER)
DECLARE SUB plan (bg AS INTEGER)
DECLARE SUB pal (bl AS INTEGER, gr AS INTEGER, rd AS INTEGER)
DECLARE SUB loadtyp ()
DECLARE SUB loadp (dname AS STRING)
DECLARE SUB check (xspd AS INTEGER, yspd AS INTEGER, zspd AS INTEGER)
DECLARE SUB move ()
DECLARE SUB wall (xpos AS INTEGER, ypos AS INTEGER, zpos AS INTEGER)
DECLARE SUB pearl ()

CONST true = 1
CONST fgx = 14
CONST fgy = 5
CONST fgz = 14
CONST fsz = 10

TYPE sprite
 x AS INTEGER
 y AS INTEGER
 z AS INTEGER
END TYPE

DIM obj(-fgx TO fgx, -fgy TO fgy, -fgz TO fgz) AS INTEGER
DIM tp(10) AS INTEGER
DIM player AS sprite

loadtyp
loadp "3dp"
SCREEN 7
wind 4

DO

 move
 break 10

LOOP

SUB box (xp AS INTEGER, yp AS INTEGER, zp AS INTEGER, xsz AS INTEGER, ysz AS INTEGER, zsz AS INTEGER, n1 AS INTEGER, n2 AS INTEGER, n3 AS INTEGER)

STATIC col AS INTEGER

IF n1 THEN
 col = 7
 LINE (xp + zp + nsz, yp + zp + ysz)-STEP(xsz, nsz), col
 LINE -STEP(-zsz, -zsz), col
 LINE -STEP(-xsz, -nsz), col
 LINE -STEP(zsz, zsz), col
 PAINT STEP(nsz, -3), col, col
END IF

IF n2 THEN
 LINE (xp + zp - zsz, yp + zp - zsz)-STEP(xsz, ysz), 11, BF
END IF

IF n3 THEN
 col = 9
 LINE (xp + zp + xsz, yp + zp + nsz)-STEP(-zsz, -zsz), col
 LINE -STEP(-nsz, ysz), col
 LINE -STEP(zsz, zsz), col
 LINE -STEP(nsz, -ysz), col
 PAINT STEP(-3, nsz), col, col
END IF

END SUB

SUB break (value AS INTEGER)

DIM i AS INTEGER

FOR i = 1 TO value * 1000
NEXT

END SUB

SUB check (xspd AS INTEGER, yspd AS INTEGER, zspd AS INTEGER)

SHARED player AS sprite
SHARED tp() AS INTEGER
SHARED obj() AS INTEGER

IF player.y = -fgy THEN
 FOR freq = 1000 TO 0 STEP -20
  SOUND freq + 100, .05
 NEXT
 END
END IF

IF ABS(player.x + xspd) <= fgx AND ABS(player.y + yspd) <= fgy AND ABS(player.z + zspd) <= fgz THEN

 SELECT CASE tp(obj(player.x + xspd, player.y + yspd, player.z + zspd))
 
  CASE 1
   player.x = player.x + xspd
   player.y = player.y + yspd
   player.z = player.z + zspd
   plan 4
  
 END SELECT

END IF

END SUB

SUB flip

STATIC vp AS INTEGER
STATIC wp AS INTEGER
STATIC nr AS INTEGER

IF nr = 2 THEN
 nr = 1
 SCREEN , , wp, vp

ELSEIF nr = 1 THEN
 nr = 2
 SWAP vp, wp
 SCREEN , , vp, wp
 CLS

ELSE
 nr = 2
 wp = 1
 SWAP vp, wp
 SCREEN , , vp, wp

END IF

END SUB

SUB loadp (dname AS STRING)

SHARED player AS sprite
SHARED obj() AS INTEGER
DIM xn AS INTEGER
DIM yn AS INTEGER
DIM zn AS INTEGER
DIM pn AS INTEGER
DIM tn AS INTEGER

OPEN "plan.dat" FOR INPUT AS #1
 DO UNTIL EOF(1)
 
  INPUT #1, pn, tn
  
   IF tn = -1 THEN
    xn = pn
   ELSEIF tn = 0 THEN
    yn = pn
   ELSE
    zn = pn
    obj(xn, yn, zn) = tn
   END IF
 
  LOOP
CLOSE #1

player.x = 4
player.y = -3
player.z = 7

END SUB

SUB loadtyp

SHARED tp() AS INTEGER

tp(0) = 1

END SUB

SUB move

SHARED player AS sprite
SHARED obj() AS INTEGER
STATIC jmp AS INTEGER

SELECT CASE INKEY$
 CASE CHR$(0) + "M": check 1, 0, 0
 CASE CHR$(0) + "K": check -1, 0, 0
 CASE CHR$(0) + "P": check 0, 0, -1
 CASE CHR$(0) + "H": check 0, 0, 1
 CASE " ":           IF obj(player.x, player.y - 1, player.z) > 0 THEN jmp = 2
 CASE CHR$(27):      END
 CASE ELSE:          IF jmp = 0 THEN check 0, -1, 0
END SELECT

IF jmp > 0 THEN jmp = jmp - 1: check 0, 1, 0

END SUB

SUB pearl

CIRCLE (0, 0), fsz / 2.5, 4
PAINT STEP(0, 0), 6, 4
CIRCLE (0 - fsz / 8, 0 + fsz / 8), fsz / 10, 12
PAINT STEP(0, 0), 14, 12

END SUB

SUB plan (bg AS INTEGER)

SHARED player AS sprite
SHARED obj() AS INTEGER
DIM xs AS INTEGER
DIM ys AS INTEGER
DIM zs AS INTEGER

flip

FOR xs = -bg TO bg
 FOR ys = -bg TO bg
  FOR zs = bg TO -bg STEP -1
 
   IF ABS(player.x + xs) <= fgx AND ABS(player.y + ys) <= fgy AND ABS(player.z + zs) <= fgz THEN
 
    SELECT CASE obj(player.x + xs, player.y + ys, player.z + zs)
     CASE 9: wall xs, ys, zs
    END SELECT

    IF ABS(xs) + ABS(ys) + ABS(zs) = 0 THEN pearl

   END IF

  NEXT
 NEXT
NEXT

flip

END SUB

SUB wall (xpos AS INTEGER, ypos AS INTEGER, zpos AS INTEGER)

SHARED player AS sprite
SHARED obj() AS INTEGER
DIM n1 AS INTEGER
DIM n2 AS INTEGER
DIM n3 AS INTEGER

IF player.x + xpos + 1 < fgx THEN
 IF obj(player.x + xpos + 1, player.y + ypos, player.z + zpos) = 0 THEN n3 = true
END IF
IF player.y + ypos + 1 < fgy THEN
 IF obj(player.x + xpos, player.y + ypos + 1, player.z + zpos) = 0 THEN n1 = true
END IF
IF player.z + zpos - 1 > -fgz THEN
 IF obj(player.x + xpos, player.y + ypos, player.z + zpos - 1) = 0 THEN n2 = true
END IF

box xpos * fsz, ypos * fsz, zpos * (fsz / 2), fsz, fsz, fsz / 2, n1, n2, n3

END SUB

SUB wind (sz AS INTEGER)

WINDOW (INT(-320 / sz), INT(-200 / sz))-(INT(320 / sz), INT(200 / sz))

END SUB

