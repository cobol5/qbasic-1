DEFINT A-Z
'$INCLUDE: 'rellib.bi'

'Special Thanks to Rich Geldreich for the original Ellipse routines!!!  ;D ;D

DECLARE SUB RelCircle (Destseg%, Xcenter%, Ycenter%, Size%, Col%)
DECLARE SUB RelCircleF (Destseg%, Xcenter%, Ycenter%, Size%, Col%)
DECLARE SUB RelCircleT (Destseg%, Xcenter%, Ycenter%, Size%, Col%)
DECLARE SUB RelCircleFT (Destseg%, Xcenter%, Ycenter%, Size%, Col%)
DECLARE SUB RelCircleI (Destseg%, Xcenter%, Ycenter%, Size%, Col%, Impose%)
DECLARE SUB RelCircleFI (Destseg%, Xcenter%, Ycenter%, Size%, Col%, Impose%)
DECLARE SUB RelEllipse (Destseg%, Xcenter%, Ycenter%, Xrad%, Yrad%, Col%)
DECLARE SUB RelEllipseF (Destseg%, Xcenter%, Ycenter%, Xrad%, Yrad%, Col%)
DECLARE SUB RelEllipseT (Destseg%, Xcenter%, Ycenter%, Xrad%, Yrad%, Col%)
DECLARE SUB RelEllipseFT (Destseg%, Xcenter%, Ycenter%, Xrad%, Yrad%, Col%)
DECLARE SUB RelEllipseI (Destseg%, Xcenter%, Ycenter%, Xrad%, Yrad%, Col%, Impose%)
DECLARE SUB RelEllipseFI (Destseg%, Xcenter%, Ycenter%, Xrad%, Yrad%, Col%, Impose%)

'Created by Richard Eric M. Lope and Adigun Azikiwe Polack!!  ;) !

SUB RelCircle (Destseg%, Xcenter%, Ycenter%, Size%, Col%)
RelEllipse Destseg%, Xcenter%, Ycenter%, Size%, Size%, Col%
END SUB

SUB RelCircleF (Destseg%, Xcenter%, Ycenter%, Size%, Col%)
RelEllipseF Destseg%, Xcenter%, Ycenter%, Size%, Size%, Col%
END SUB

SUB RelCircleFI (Destseg%, Xcenter%, Ycenter%, Size%, Col%, Impose%)
RelEllipseFI Destseg%, Xcenter%, Ycenter%, Size%, Size%, Col%, Impose%
END SUB

SUB RelCircleFT (Destseg%, Xcenter%, Ycenter%, Size%, Col%)
RelEllipseFT Destseg%, Xcenter%, Ycenter%, Size%, Size%, Col%
END SUB

SUB RelCircleI (Destseg%, Xcenter%, Ycenter%, Size%, Col%, Impose%)
RelEllipseI Destseg%, Xcenter%, Ycenter%, Size%, Size%, Col%, Impose%
END SUB

SUB RelCircleT (Destseg%, Xcenter%, Ycenter%, Size%, Col%)
RelEllipseT Destseg%, Xcenter%, Ycenter%, Size%, Size%, Col%
END SUB

SUB RelEllipse (Destseg%, Xcenter, Ycenter, Xrad, Yrad, Col)

  DIM xe AS LONG, ye AS LONG, e AS LONG
  IF Yrad = 0 THEN 'special cases for horizontal & vertical ellipses
    RelLine Destseg%, Xcenter - Xrad, Ycenter, Xcenter + Xrad, Ycenter, Col
    EXIT SUB
  END IF
  IF Xrad = 0 THEN
    RelLine Destseg%, Xcenter, Ycenter - Yrad, Xcenter, Ycenter + Yrad, Col
    EXIT SUB
  END IF
  'work with largest axis to avoid rounding errors
  IF Yrad <= Xrad THEN
    X = 0: Y = Yrad
    xe = 0: ye = CLNG(Xrad) * Xrad
    e = -ye \ 2: C = ye \ Yrad
    DO
      IF e <= 0 THEN
        DO
          RelPset Destseg%, Xcenter + X, Ycenter + Y, Col
          RelPset Destseg%, Xcenter - X, Ycenter + Y, Col
          RelPset Destseg%, Xcenter + X, Ycenter - Y, Col
          RelPset Destseg%, Xcenter - X, Ycenter - Y, Col
          X = X + 1
          xe = xe + Yrad
          e = e + xe
        LOOP WHILE e <= 0
      ELSE
        RelPset Destseg%, Xcenter + X, Ycenter + Y, Col
        RelPset Destseg%, Xcenter - X, Ycenter + Y, Col
        RelPset Destseg%, Xcenter + X, Ycenter - Y, Col
        RelPset Destseg%, Xcenter - X, Ycenter - Y, Col
      END IF
      Y = Y - 1
      ye = ye - C
      e = e - ye
    LOOP UNTIL Y = 0
    RelPset Destseg%, Xcenter + X, Ycenter, Col
    RelPset Destseg%, Xcenter - X, Ycenter, Col
    RelPset Destseg%, Xcenter + X, Ycenter, Col
    RelPset Destseg%, Xcenter - X, Ycenter, Col
  ELSE
    X = 0: Y = Xrad
    xe = 0: ye = CLNG(Yrad) * Yrad
    e = -ye \ 2: C = ye \ Xrad
    DO
      IF e <= 0 THEN
        DO
          RelPset Destseg%, Xcenter + Y, Ycenter + X, Col
          RelPset Destseg%, Xcenter - Y, Ycenter + X, Col
          RelPset Destseg%, Xcenter + Y, Ycenter - X, Col
          RelPset Destseg%, Xcenter - Y, Ycenter - X, Col
          X = X + 1
          xe = xe + Xrad
          e = e + xe
        LOOP WHILE e <= 0
      ELSE
        RelPset Destseg%, Xcenter + Y, Ycenter + X, Col
        RelPset Destseg%, Xcenter - Y, Ycenter + X, Col
        RelPset Destseg%, Xcenter + Y, Ycenter - X, Col
        RelPset Destseg%, Xcenter - Y, Ycenter - X, Col
      END IF
      Y = Y - 1
      ye = ye - C
      e = e - ye
    LOOP UNTIL Y = 0
    RelPset Destseg%, Xcenter, Ycenter + X, Col
    RelPset Destseg%, Xcenter, Ycenter + X, Col
    RelPset Destseg%, Xcenter, Ycenter - X, Col
    RelPset Destseg%, Xcenter, Ycenter - X, Col
  END IF
END SUB

SUB RelEllipseF (Destseg%, Xcenter%, Ycenter%, Xrad%, Yrad%, Col%)

  DIM xe AS LONG, ye AS LONG, e AS LONG
  IF Yrad = 0 THEN 'special cases for horizontal & vertical ellipses
    RelLine Destseg%, Xcenter - Xrad, Ycenter, Xcenter + Xrad, Ycenter, Col
    EXIT SUB
  END IF
  IF Xrad = 0 THEN
    RelLine Destseg%, Xcenter, Ycenter - Yrad, Xcenter, Ycenter + Yrad, Col
    EXIT SUB
  END IF
  'work with largest axis to avoid rounding errors
  IF Yrad <= Xrad THEN
    X = 0: Y = Yrad
    xe = 0: ye = CLNG(Xrad) * Xrad
    e = -ye \ 2: C = ye \ Yrad
    DO
      IF e <= 0 THEN
        DO
          RelLine Destseg%, Xcenter + X, Ycenter + Y, Xcenter - X, Ycenter + Y, Col
          RelLine Destseg%, Xcenter + X, Ycenter - Y, Xcenter - X, Ycenter - Y, Col
          X = X + 1
          xe = xe + Yrad
          e = e + xe
        LOOP WHILE e <= 0
      ELSE
        RelLine Destseg%, Xcenter + X, Ycenter + Y, Xcenter - X, Ycenter + Y, Col
        RelLine Destseg%, Xcenter + X, Ycenter - Y, Xcenter - X, Ycenter - Y, Col
      END IF
      Y = Y - 1
      ye = ye - C
      e = e - ye
    LOOP UNTIL Y = 0
    RelLine Destseg%, Xcenter + X, Ycenter, Xcenter - X, Ycenter, Col
    RelLine Destseg%, Xcenter + X, Ycenter, Xcenter - X, Ycenter, Col
  ELSE
    X = 0: Y = Xrad
    xe = 0: ye = CLNG(Yrad) * Yrad
    e = -ye \ 2: C = ye \ Xrad
    DO
      IF e <= 0 THEN
        DO
          RelLine Destseg%, Xcenter + Y, Ycenter + X, Xcenter - Y, Ycenter + X, Col
          RelLine Destseg%, Xcenter + Y, Ycenter - X, Xcenter - Y, Ycenter - X, Col
          X = X + 1
          xe = xe + Xrad
          e = e + xe
        LOOP WHILE e <= 0
      ELSE
        RelLine Destseg%, Xcenter + Y, Ycenter + X, Xcenter - Y, Ycenter + X, Col
        RelLine Destseg%, Xcenter + Y, Ycenter - X, Xcenter - Y, Ycenter - X, Col
      END IF
      Y = Y - 1
      ye = ye - C
      e = e - ye
    LOOP UNTIL Y = 0
    RelLine Destseg%, Xcenter, Ycenter + X, Xcenter, Ycenter + X, Col
    RelLine Destseg%, Xcenter, Ycenter - X, Xcenter, Ycenter - X, Col
  END IF

END SUB

SUB RelEllipseFI (Destseg%, Xcenter%, Ycenter%, Xrad%, Yrad%, Col%, Impose%)
IF Impose% > 4 THEN Impose% = 4

FOR Rep = 1 TO Impose%
  RelEllipseFT Destseg%, Xcenter%, Ycenter%, Xrad%, Yrad%, Col%
NEXT

END SUB

SUB RelEllipseFT (Destseg%, Xcenter, Ycenter, Xrad, Yrad, Col)

'Draws a filled ellipse with Xcenter and Ycenter as the centerColords
'with radius of Xrad and Yrad,. if xrad=yrad then a circle is drawn
'translucently!!!
 
  DIM xe AS LONG, ye AS LONG, e AS LONG
  IF Yrad = 0 THEN 'special cases for horizontal & vertical ellipses
    RelLineTrans Destseg%, Xcenter - Xrad, Ycenter, Xcenter + Xrad, Ycenter, Col
    EXIT SUB
  END IF
  IF Xrad = 0 THEN
    RelLineTrans Destseg%, Xcenter, Ycenter - Yrad, Xcenter, Ycenter + Yrad, Col
    EXIT SUB
  END IF
  'work with largest axis to avoid rounding errors
  IF Yrad <= Xrad THEN
    X = 0: Y = Yrad
    xe = 0: ye = CLNG(Xrad) * Xrad
    e = -ye \ 2: C = ye \ Yrad
    DO
      IF e <= 0 THEN
        DO
          RelLineTrans Destseg%, Xcenter + X, Ycenter + Y, Xcenter - X, Ycenter + Y, Col
          RelLineTrans Destseg%, Xcenter + X, Ycenter - Y, Xcenter - X, Ycenter - Y, Col
          
          X = X + 1
          xe = xe + Yrad
          e = e + xe
        LOOP WHILE e <= 0
      ELSE
          RelLineTrans Destseg%, Xcenter + X, Ycenter + Y, Xcenter - X, Ycenter + Y, Col
          RelLineTrans Destseg%, Xcenter + X, Ycenter - Y, Xcenter - X, Ycenter - Y, Col
      END IF
      Y = Y - 1
      ye = ye - C
      e = e - ye
    LOOP UNTIL Y = 0
          RelLineTrans Destseg%, Xcenter + X, Ycenter, Xcenter - X, Ycenter, Col
          RelLineTrans Destseg%, Xcenter + X, Ycenter, Xcenter - X, Ycenter, Col
          ELSE
    X = 0: Y = Xrad
    xe = 0: ye = CLNG(Yrad) * Yrad
    e = -ye \ 2: C = ye \ Xrad
    DO
      IF e <= 0 THEN
        DO
          RelLineTrans Destseg%, Xcenter + Y, Ycenter + X, Xcenter - Y, Ycenter + X, Col
          RelLineTrans Destseg%, Xcenter + Y, Ycenter - X, Xcenter - Y, Ycenter - X, Col
          X = X + 1
          xe = xe + Xrad
          e = e + xe
        LOOP WHILE e <= 0
      ELSE
          RelLineTrans Destseg%, Xcenter + Y, Ycenter + X, Xcenter - Y, Ycenter + X, Col
          RelLineTrans Destseg%, Xcenter + Y, Ycenter - X, Xcenter - Y, Ycenter - X, Col
          RelLineTrans Destseg%, Xcenter, Ycenter + X, Xcenter, Ycenter + X, Col
          RelLineTrans Destseg%, Xcenter, Ycenter - X, Xcenter, Ycenter - X, Col
      END IF
      Y = Y - 1
      ye = ye - C
      e = e - ye
    LOOP UNTIL Y = 0
          RelLineTrans Destseg%, Xcenter, Ycenter + X, Xcenter, Ycenter + X, Col
          RelLineTrans Destseg%, Xcenter, Ycenter - X, Xcenter, Ycenter - X, Col
  END IF

END SUB

SUB RelEllipseI (Destseg%, Xcenter%, Ycenter%, Xrad%, Yrad%, Col%, Impose%)
IF Impose% > 4 THEN Impose% = 4

FOR Rep = 1 TO Impose%
  RelEllipseT Destseg%, Xcenter%, Ycenter%, Xrad%, Yrad%, Col%
NEXT

END SUB

SUB RelEllipseT (Destseg%, Xcenter, Ycenter, Xrad, Yrad, Col)

  DIM xe AS LONG, ye AS LONG, e AS LONG
  IF Yrad = 0 THEN 'special cases for horizontal & vertical ellipses
    RelLineTrans Destseg%, Xcenter - Xrad, Ycenter, Xcenter + Xrad, Ycenter, Col
    EXIT SUB
  END IF
  IF Xrad = 0 THEN
    RelLineTrans Destseg%, Xcenter, Ycenter - Yrad, Xcenter, Ycenter + Yrad, Col
    EXIT SUB
  END IF
  'work with largest axis to avoid rounding errors
  IF Yrad <= Xrad THEN
    X = 0: Y = Yrad
    xe = 0: ye = CLNG(Xrad) * Xrad
    e = -ye \ 2: C = ye \ Yrad
    DO
      IF e <= 0 THEN
        DO
          RelPsetTrans Destseg%, Xcenter + X, Ycenter + Y, Col
          RelPsetTrans Destseg%, Xcenter - X, Ycenter + Y, Col
          RelPsetTrans Destseg%, Xcenter + X, Ycenter - Y, Col
          RelPsetTrans Destseg%, Xcenter - X, Ycenter - Y, Col
          X = X + 1
          xe = xe + Yrad
          e = e + xe
        LOOP WHILE e <= 0
      ELSE
        RelPsetTrans Destseg%, Xcenter + X, Ycenter + Y, Col
        RelPsetTrans Destseg%, Xcenter - X, Ycenter + Y, Col
        RelPsetTrans Destseg%, Xcenter + X, Ycenter - Y, Col
        RelPsetTrans Destseg%, Xcenter - X, Ycenter - Y, Col
      END IF
      Y = Y - 1
      ye = ye - C
      e = e - ye
    LOOP UNTIL Y = 0
    RelPsetTrans Destseg%, Xcenter + X, Ycenter, Col
    RelPsetTrans Destseg%, Xcenter - X, Ycenter, Col
    RelPsetTrans Destseg%, Xcenter + X, Ycenter, Col
    RelPsetTrans Destseg%, Xcenter - X, Ycenter, Col
  ELSE
    X = 0: Y = Xrad
    xe = 0: ye = CLNG(Yrad) * Yrad
    e = -ye \ 2: C = ye \ Xrad
    DO
      IF e <= 0 THEN
        DO
          RelPsetTrans Destseg%, Xcenter + Y, Ycenter + X, Col
          RelPsetTrans Destseg%, Xcenter - Y, Ycenter + X, Col
          RelPsetTrans Destseg%, Xcenter + Y, Ycenter - X, Col
          RelPsetTrans Destseg%, Xcenter - Y, Ycenter - X, Col
          X = X + 1
          xe = xe + Xrad
          e = e + xe
        LOOP WHILE e <= 0
      ELSE
        RelPsetTrans Destseg%, Xcenter + Y, Ycenter + X, Col
        RelPsetTrans Destseg%, Xcenter - Y, Ycenter + X, Col
        RelPsetTrans Destseg%, Xcenter + Y, Ycenter - X, Col
        RelPsetTrans Destseg%, Xcenter - Y, Ycenter - X, Col
      END IF
      Y = Y - 1
      ye = ye - C
      e = e - ye
    LOOP UNTIL Y = 0
    RelPsetTrans Destseg%, Xcenter, Ycenter + X, Col
    RelPsetTrans Destseg%, Xcenter, Ycenter + X, Col
    RelPsetTrans Destseg%, Xcenter, Ycenter - X, Col
    RelPsetTrans Destseg%, Xcenter, Ycenter - X, Col
  END IF

END SUB

