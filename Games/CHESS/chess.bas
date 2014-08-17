DECLARE SUB Watch ()
DECLARE SUB GetPoints (px%, py%, Who%, Points%, OutX%, Outy%, Deep%)
DECLARE SUB XorSpot (px%, py%, Fill%)
DECLARE FUNCTION InCheck% (Who%)
DECLARE SUB Getche ()
DECLARE SUB GoodAI (Who%)
DECLARE FUNCTION Value% (p$)
DECLARE FUNCTION InDanger% (px%, py%, Who%)
DECLARE SUB AI (Who%)
DECLARE SUB HideMouse ()
DECLARE SUB getch ()
DECLARE SUB DrawSpot (px%, py%, p$)
DECLARE SUB GetMousePos (BX%, CX%, DX%)
DECLARE SUB Main ()
DECLARE FUNCTION LegalMove% (Fx%, Fy%, Tx%, Ty%)
DECLARE SUB GetUserInput (Who%)
DECLARE SUB DrawBoard ()
DECLARE SUB RedBlink ()
DECLARE SUB GetIMG (File$, Array%())
DECLARE SUB PutIMG (px%, py%, Array%())
DECLARE SUB Init ()
DECLARE SUB ShowMouse ()
DECLARE SUB ResetMouse (NumButtons%, Errlvl%)
DEFINT A-Z

RANDOMIZE TIMER
DEF fnran (n) = RND * n + 1

TYPE RegType
  AX    AS INTEGER
  BX    AS INTEGER
  CX    AS INTEGER
  DX    AS INTEGER
  BP    AS INTEGER
  SI    AS INTEGER
  DI    AS INTEGER
  Flags AS INTEGER
  DS    AS INTEGER
  ES    AS INTEGER
END TYPE

DIM SHARED Registers AS RegType

DIM SHARED Block(-1 TO 9, -1 TO 9) AS STRING * 1
DIM SHARED PMove$(800), NumMoves
DIM SHARED Board(8, 8)
DIM SHARED Xpos(10)
DIM SHARED Xoff, Yoff, ToX, ToY, FromX, FromY, Board0, Board1
DIM SHARED HighPoint, HighX, HighY
'Graphics Arrays

DIM SHARED P1(16, 16)
DIM SHARED P2(16, 16)
DIM SHARED R1(16, 16)
DIM SHARED R2(16, 16)
DIM SHARED B1(16, 16)
DIM SHARED B2(16, 16)
DIM SHARED N1(16, 16)
DIM SHARED N2(16, 16)
DIM SHARED K1(16, 16)
DIM SHARED K2(16, 16)
DIM SHARED Q1(16, 16)
DIM SHARED Q2(16, 16)


SCREEN 13
CONST TRUE = 1
CONST FALSE = 0

Init
'Watch
ResetMouse 1, 1
ShowMouse
'LINE (Xpos(1) + Xoff, Xpos(2) + Yoff)-(Xpos(1) + Xoff + 15, Xpos(2) + Yoff + 15), 254, B
Main




BoardData:
DATA 1,0,1,0,1,0,1,0
DATA 0,1,0,1,0,1,0,1
DATA 1,0,1,0,1,0,1,0
DATA 0,1,0,1,0,1,0,1
DATA 1,0,1,0,1,0,1,0
DATA 0,1,0,1,0,1,0,1
DATA 1,0,1,0,1,0,1,0
DATA 0,1,0,1,0,1,0,1

PieceData:


DATA  R,N,B,Q,K,B,N,R
DATA  P,P,P,P,P,P,P,P
DATA  0,0,0,0,0,0,0,0
DATA  0,0,0,0,0,0,0,0
DATA  0,0,0,0,0,0,0,0
DATA  0,0,0,0,0,0,0,0
DATA  p,p,p,p,p,p,p,p
DATA  r,n,b,q,k,b,n,r

DATA  0,0,0,0,0,0,0,0
DATA  0,0,0,0,0,0,0,0
DATA  0,0,0,0,0,0,0,0
DATA  0,0,0,0,0,0,0,0
DATA  0,0,0,0,0,0,0,0
DATA  0,0,0,0,0,0,0,0
DATA  0,0,0,0,0,0,0,0
DATA  0,0,0,0,0,0,0,0

SUB AI (Who)
  LOCATE 23, 1: PRINT "Random": PLAY "l40n10n10n11n12"
  HideMouse
  DO
    DO
      px = fnran(8)
      py = fnran(8)
      Land = ASC(Block(px, py))
      IF Who = 1 AND Land > 96 THEN EXIT DO
      IF Who = 2 AND Land < 96 AND Land > 50 THEN EXIT DO
    LOOP
    FromX = px
    FromY = py
    FOR n = 1 TO 100
      px = fnran(8)
      py = fnran(8)
      IF LegalMove(FromX, FromY, px, py) AND NOT InDanger(px, py, Who) THEN
        ToX = px
        ToY = py
        getch
        ShowMouse
        EXIT SUB
      END IF
    NEXT
  LOOP

END SUB

SUB DrawBoard

  FOR py = 1 TO 8
    FOR px = 1 TO 8
      CALL DrawSpot(px, py, Block(px, py))
    NEXT
  NEXT

END SUB

SUB DrawSpot (px, py, p$)
     
      SELECT CASE p$
        CASE "p": PutIMG px, py, P1()
        CASE "P": PutIMG px, py, P2()
        CASE "n": PutIMG px, py, N1()
        CASE "N": PutIMG px, py, N2()
        CASE "k": PutIMG px, py, K1()
        CASE "K": PutIMG px, py, K2()
        CASE "b": PutIMG px, py, B1()
        CASE "B": PutIMG px, py, B2()
        CASE "r": PutIMG px, py, R1()
        CASE "R": PutIMG px, py, R2()
        CASE "q": PutIMG px, py, Q1()
        CASE "Q": PutIMG px, py, Q2()
      END SELECT

END SUB

SUB getch
  DO
    CALL GetMousePos(Butt, mx, my)
    RedBlink
  LOOP WHILE Butt = 1

END SUB

SUB Getche
DO
  a$ = INKEY$
LOOP WHILE a$ = ""
IF a$ = CHR$(27) THEN END
END SUB

SUB GetIMG (File$, Array())

   OPEN "\grub\" + File$ FOR INPUT AS #1
   FOR py = 1 TO 16
     FOR px = 1 TO 16
       INPUT #1, col
       Array(px, py) = col
       IF col <> 0 AND INSTR(File$, "3.") THEN Array(px, py) = Array(px, py) - 2
       ' + 20 + 30 through 80
     NEXT
   NEXT
   CLOSE #1
END SUB

SUB GetMousePos (BX, CX, DX)
  
  Registers.AX = &H3
  CALL interrupt(&H33, Registers, Registers)
  BX = Registers.BX AND 7
  CX = Registers.CX \ 2
  DX = Registers.DX


END SUB

SUB GetPoints (px, py, Who, Points, OutX, Outy, Deep)
  'LOCATE 23: PRINT "                  "
  'LOCATE 2, 1
  IF InDanger(px, py, Who) THEN Deff = Value(Block(px, py))
 
  FOR ny = 1 TO 8
    FOR nx = 1 TO 8
      IF LegalMove(px, py, nx, ny) THEN
        p = Value(Block(nx, ny))
        h$ = Block(px, py)
        Block(px, py) = "0"
        Hold$ = Block(nx, ny)
        Block(nx, ny) = h$
        IF InDanger(nx, ny, Who) AND Value(Block(nx, ny)) <> 0 THEN p = p - Value(Block(nx, ny)) * 2: 'PRINT "Bad"; Value(Block(nx, ny))
        HighH = 0
        FOR zy = 1 TO 8
          FOR zx = 1 TO 8
            IF Who = 2 AND LegalMove(nx, ny, zx, zy) = TRUE AND ASC(Block(zx, zy)) >= 96 AND Block(zx, zy) <> "0" THEN h = Value(Block(zx, zy)) / 2
            IF Who = 1 AND LegalMove(nx, ny, zx, zy) = TRUE AND ASC(Block(zx, zy)) < 96 AND Block(zx, zy) <> "0" THEN h = Value(Block(zx, zy)) / 2
            IF h > HighH THEN
              HighH = h
              h = 0
            END IF
          NEXT
        NEXT
        p = p + HighH
        Block(nx, ny) = Hold$
        Block(px, py) = h$
        IF p > AttackHigh THEN
          PRINT Block(px, py); p; ":"; nx, ny
          AttackHigh = p
          OutX = nx
          Outy = ny
        END IF
      END IF
    NEXT
  NEXT
  IF AttackHigh < Deff AND Deff > 0 THEN
    FOR ny = 1 TO 8
      FOR nx = 1 TO 8
        IF LegalMove(px, py, nx, ny) AND NOT InDanger(nx, ny, Who) THEN
          Points = Deff
          SOUND 500, .9
          OutX = nx
          Outy = ny
          EXIT FOR
        END IF
      NEXT
    NEXT
  END IF
 
  'IF Deep = 1 THEN
  '  FOR OuterY = 1 TO 8
  '    FOR OuterX = 1 TO 8
  '      Land = ASC(Block(OuterX, OuterY))
  '      IF Who = 2 AND Land < 96 AND Land > 50 THEN
  '        FOR sy = 1 TO 8
  '          FOR sx = 1 TO 8
  '            IF LegalMove(OuterX, OuterY, sx, sy) THEN
  '              Score = Value(Block(sx, sy))
  '              H$ = Block(sx, sy)
  '              Block$(sx, sy) = Block(OuterX, OuterY)
  '              Block(OuterX, OuterY) = "0"
  '              FOR ny = 1 TO 8
  '                FOR nx = 1 TO 8
  '                  Force = 0
  '                  Land = ASC(Block(px, py))
  '                  IF Who = 2 AND Land < 96 AND Land > 50 THEN GetPoints nx, ny, Who, Points, OutX, Outy, 2
  '                  IF Who = 1 AND Land > 96 THEN GetPoints nx, ny, Who, Force, AttX, AttY, 2
  '                  IF Force > HighForce THEN
  '                    HighForce = Force
  '                    ForceFX = OuterX
  '                    ForceFY = OuterY
  '                    ForceTX = sx
  '                    ForceTY = sy
  '                END IF
  '                NEXT
  '              NEXT
  '              Block(sx, sy) = H$
  '              Block(OuterX, OuterY) = Block$(sx, sy)
  '            END IF
  '          NEXT
  '        NEXT
  '      END IF
  '    NEXT
  '  NEXT
  'END IF
  
  IF Who = 1 AND Land > 96 THEN GetPoints nx, ny, Who, Force, AttX, AttY, 2

  IF AttackHigh > 0 THEN Points = AttackHigh + Deff ELSE Points = 0

END SUB

SUB GetUserInput (Who)
 

ChosePiece:

  DO
    CALL GetMousePos(Butt, mx, my)
    a$ = INKEY$
    IF a$ <> "" THEN
    SELECT CASE UCASE$(a$)
      CASE CHR$(27): END
      CASE "S":
        INPUT "File name to save game:", f$
        IF INSTR(f$, ".") = 0 THEN f$ = f$ + ".CHS"
        OPEN f$ FOR OUTPUT AS #1
          PRINT #1, NumMoves
          FOR n = 1 TO NumMoves
            PRINT #1, PMove$(n)
          NEXT
        CLOSE #1
    END SELECT
    END IF
  LOOP UNTIL Butt = 1
  IF mx < (Xpos(1) + Xoff) OR mx > (Xpos(9) + Xoff) THEN GOTO ChosePiece
  IF mx < (Xpos(1) + Xoff) OR mx > (Xpos(9) + Xoff) THEN GOTO ChosePiece
  IF my < (Xpos(1) + Yoff) OR my > (Xpos(9) + Yoff) THEN GOTO ChosePiece
  IF my < (Xpos(1) + Yoff) OR my > (Xpos(9) + Yoff) THEN GOTO ChosePiece
 
 
  mx = mx \ 16 - (Xoff \ 16)
  my = my \ 16 - (Yoff \ 16)
  Land = ASC(Block(mx, my))
  IF Who = 1 AND Land < 96 THEN GOTO ChosePiece
  IF Who = 2 AND Land > 96 THEN GOTO ChosePiece
  IF Block(mx, my) = "0" THEN GOTO ChosePiece

  HideMouse
  LINE (Xpos(mx) + Xoff, Xpos(my) + Yoff)-(Xpos(mx) + Xoff + 15, Xpos(my) + Yoff + 15), 254, B
  ShowMouse
  FromX = mx
  FromY = my
  getch
Where:
  DO
    CALL GetMousePos(Butt, mx, my)
    RedBlink
    IF INKEY$ <> "" THEN END
  LOOP WHILE Butt = 0
  IF Butt <> 1 THEN
    HideMouse
    XorSpot FromX, FromY, 1
    DrawSpot FromX, FromY, Block(FromX, FromY)
    ShowMouse
    GOTO ChosePiece
  END IF
 
  CALL GetMousePos(Butt, mx, my)
 
  mx = mx \ 16 - (Xoff \ 16)
  my = my \ 16 - (Yoff \ 16)
  IF LegalMove(FromX, FromY, mx, my) = 0 THEN GOTO Where
  ToX = mx
  ToY = my

END SUB

SUB GoodAI (Who)
'-------------- In Check? ---------------
      IF InCheck(Who) THEN
        FOR ny = 1 TO 8
          FOR nx = 1 TO 8
            PC = ASC(Block(nx, ny))
            IF Who = 2 AND PC < 96 AND PC > 50 OR Who = 1 AND PC > 96 THEN
              FOR my = 1 TO 8
                FOR mx = 1 TO 8
                  IF LegalMove(nx, ny, mx, my) THEN
                    Store$ = Block(mx, my)
                    Block(mx, my) = Block(nx, ny): Block(nx, ny) = "0"
                    IF InCheck(Who) = 0 THEN HighPoint = 1000: HighFromX = nx: HighFromY = ny: HighToX = mx: HighToY = my: Block(nx, ny) = Block(mx, my): Block(mx, my) = "0":  GOTO MoveAI
                    Block(nx, ny) = Block(mx, my): Block(mx, my) = Store$
                  END IF
                NEXT
              NEXT
            END IF
          NEXT
        NEXT
        PRINT "Checkmate... Player"; Who; "lost"
        INPUT "File name to save game:", f$
        IF INSTR(f$, ".") = 0 THEN f$ = f$ + ".CHS"
        OPEN f$ FOR OUTPUT AS #1
          PRINT #1, NumMoves
          FOR n = 1 TO NumMoves
            PRINT #1, PMove$(n)
          NEXT
        CLOSE #1
        END
      END IF
'------------ End of In Check -------------
 
  HighPoint = 0
  LOCATE 2, 1
  FOR py = 1 TO 8
    FOR px = 1 TO 8
      Points = 0
      Land = ASC(Block(px, py))
      IF Who = 2 AND Land < 96 AND Land > 50 THEN GetPoints px, py, Who, Points, OutX, Outy, 1
      IF Who = 1 AND Land > 96 THEN GetPoints px, py, Who, Points, OutX, Outy, 1
     
      'Watch it think?
      'LINE (Xpos(px) + Xoff, Xpos(py) + Yoff)-(Xpos(px) + Xoff + 15, Xpos(py) + Yoff + 15), 69, B
      'LOCATE 23, 1: PRINT Points
      'LINE (Xpos(OutX) + Xoff, Xpos(Outy) + Yoff)-(Xpos(OutX) + Xoff + 15, Xpos(Outy) + Yoff + 15), 4, B
      'IF Points THEN getche
      'CALL XorSpot(px, py)
      'CALL DrawSpot(px, py, Block(px, py))
      'IF OutX <> 0 THEN
      '  CALL XorSpot(OutX, Outy)
      '  CALL DrawSpot(OutX, Outy, Block(OutX, Outy))
      'END IF

      IF Points > HighPoint THEN
        HighPoint = Points
        HighToX = OutX
        HighToY = Outy
        HighFromX = px
        HighFromY = py
      END IF
    NEXT
  NEXT
MoveAI:

  ToX = HighToX
  ToY = HighToY
  FromX = HighFromX
  FromY = HighFromY
  IF HighPoint = 0 THEN AI Who

END SUB

SUB HideMouse
 
  Registers.AX = &H2
  CALL interrupt(&H33, Registers, Registers)
  
END SUB

FUNCTION InCheck (Who)
  IF Who = 1 THEN
    FOR py = 1 TO 8
      FOR px = 1 TO 8
        IF Block(px, py) = "k" THEN IF InDanger(px, py, 1) THEN InCheck = TRUE
      NEXT
    NEXT
  ELSE
    FOR py = 1 TO 8
      FOR px = 1 TO 8
        IF Block(px, py) = "K" THEN IF InDanger(px, py, 2) THEN InCheck = TRUE
      NEXT
    NEXT
  END IF

END FUNCTION

FUNCTION InDanger (px, py, Who)

  IF Who = 1 THEN
    FOR ny = 1 TO 8
      FOR nx = 1 TO 8
        Land = ASC(Block(nx, ny))
        IF Land < 96 AND Land <> 48 THEN
          IF LegalMove(nx, ny, px, py) THEN InDanger = TRUE: EXIT FUNCTION
        END IF
      NEXT
    NEXT
  ELSE
    FOR ny = 1 TO 8
      FOR nx = 1 TO 8
        Land = ASC(Block(nx, ny))
        IF Land > 96 THEN
          IF LegalMove(nx, ny, px, py) THEN InDanger = TRUE: EXIT FUNCTION
        END IF
      NEXT
    NEXT
  END IF

END FUNCTION

SUB Init

  Board0 = 2
  Board1 = 69'139
 
  GetIMG "Knight2.img", N1()
  GetIMG "Knight3.img", N2()
  GetIMG "Queen2.img", Q1()
  GetIMG "Queen3.img", Q2()
  GetIMG "rook2.img", R1()
  GetIMG "Rook3.img", R2()
  GetIMG "Pawn2.img", P1()
  GetIMG "Pawn3.img", P2()
  GetIMG "Bishop2.img", B1()
  GetIMG "Bishop3.img", B2()
  GetIMG "King2.img", K1()
  GetIMG "King3.img", K2()


  Xoff = 16 * 5
  Yoff = 16 * 1
  RESTORE BoardData
  FOR py = 1 TO 8
    FOR px = 1 TO 8
      READ Board(px, py)
    NEXT
  NEXT
 
  RESTORE PieceData
  FOR py = 1 TO 8
    FOR px = 1 TO 8
      READ Block(px, py)
    NEXT
  NEXT

  FOR n = 0 TO 10
    Xpos(n) = n * 16
  NEXT
 
  DEF SEG = &HA000
  BLOAD "BckGrnd8.GFX"
  DEF SEG

  FOR py = 1 TO 8
    FOR px = 1 TO 8
     IF Board(px, py) = 0 THEN LINE (Xoff + Xpos(px), Yoff + Xpos(py))-(Xoff + Xpos(px) + 15, Yoff + Xpos(py) + 15), Board0, BF
     IF Board(px, py) = 1 THEN LINE (Xoff + Xpos(px), Yoff + Xpos(py))-(Xoff + Xpos(px) + 15, Yoff + Xpos(py) + 15), Board1, BF
    NEXT
  NEXT
  DrawBoard

END SUB

FUNCTION LegalMove (Fx, Fy, Tx, Ty)

  IF Ty > 8 OR Ty < 1 OR Tx > 8 OR Tx < 1 THEN EXIT FUNCTION
  'Caps = 65-90
  'Lower = 97-122
 
  Land = ASC(Block(Tx, Ty))
  SELECT CASE Block(Fx, Fy)
    CASE "p"
      IF Ty = Fy - 1 AND Tx = Fx AND Block(Tx, Ty) = "0" THEN LegalMove = TRUE
      IF Ty = Fy - 1 AND Tx = Fx - 1 AND Land < 96 AND Block(Tx, Ty) <> "0" THEN LegalMove = TRUE
      IF Ty = Fy - 1 AND Tx = Fx + 1 AND Land < 96 AND Block(Tx, Ty) <> "0" THEN LegalMove = TRUE
      IF Ty = Fy - 2 AND Tx = Fx AND Fy = 7 AND Block(Tx, Ty) = "0" AND Block(Tx, Ty + 1) = "0" THEN LegalMove = TRUE
    CASE "P"
      IF Ty = Fy + 1 AND Tx = Fx AND Block(Tx, Ty) = "0" THEN LegalMove = TRUE
      IF Ty = Fy + 1 AND Tx = Fx - 1 AND Land > 96 AND Block(Tx, Ty) <> "0" THEN LegalMove = TRUE
      IF Ty = Fy + 1 AND Tx = Fx + 1 AND Land > 96 AND Block(Tx, Ty) <> "0" THEN LegalMove = TRUE
      IF Ty = Fy + 2 AND Tx = Fx AND Fy = 2 AND Block(Tx, Ty) = "0" AND Block(Tx, Ty - 1) = "0" THEN LegalMove = TRUE
    CASE "n":
      IF Ty = Fy - 2 AND Tx = Fx - 1 AND Land < 96 THEN LegalMove = TRUE
      IF Ty = Fy - 2 AND Tx = Fx + 1 AND Land < 96 THEN LegalMove = TRUE
      IF Ty = Fy + 2 AND Tx = Fx - 1 AND Land < 96 THEN LegalMove = TRUE
      IF Ty = Fy + 2 AND Tx = Fx + 1 AND Land < 96 THEN LegalMove = TRUE
      IF Ty = Fy - 1 AND Tx = Fx - 2 AND Land < 96 THEN LegalMove = TRUE
      IF Ty = Fy - 1 AND Tx = Fx + 2 AND Land < 96 THEN LegalMove = TRUE
      IF Ty = Fy + 1 AND Tx = Fx - 2 AND Land < 96 THEN LegalMove = TRUE
      IF Ty = Fy + 1 AND Tx = Fx + 2 AND Land < 96 THEN LegalMove = TRUE
    CASE "N":
      IF Ty = Fy - 2 AND Tx = Fx - 1 AND (Land > 96 OR Land = 48) THEN LegalMove = TRUE
      IF Ty = Fy - 2 AND Tx = Fx + 1 AND (Land > 96 OR Land = 48) THEN LegalMove = TRUE
      IF Ty = Fy + 2 AND Tx = Fx - 1 AND (Land > 96 OR Land = 48) THEN LegalMove = TRUE
      IF Ty = Fy + 2 AND Tx = Fx + 1 AND (Land > 96 OR Land = 48) THEN LegalMove = TRUE
      IF Ty = Fy - 1 AND Tx = Fx - 2 AND (Land > 96 OR Land = 48) THEN LegalMove = TRUE
      IF Ty = Fy - 1 AND Tx = Fx + 2 AND (Land > 96 OR Land = 48) THEN LegalMove = TRUE
      IF Ty = Fy + 1 AND Tx = Fx - 2 AND (Land > 96 OR Land = 48) THEN LegalMove = TRUE
      IF Ty = Fy + 1 AND Tx = Fx + 2 AND (Land > 96 OR Land = 48) THEN LegalMove = TRUE
    CASE "b":
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx + c, Fy + c))
        IF Land > 96 THEN EXIT DO
        IF Fx + c = Tx AND Fy + c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx + c, Fy - c))
        IF Land > 96 THEN EXIT DO
        IF Fx + c = Tx AND Fy - c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx - c, Fy + c))
        IF Land > 96 THEN EXIT DO
        IF Fx - c = Tx AND Fy + c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx - c, Fy - c))
        IF Land > 96 THEN EXIT DO
        IF Fx - c = Tx AND Fy - c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
    CASE "B":
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx + c, Fy + c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF Fx + c = Tx AND Fy + c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx + c, Fy - c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF Fx + c = Tx AND Fy - c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx - c, Fy + c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF Fx - c = Tx AND Fy + c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx - c, Fy - c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF Fx - c = Tx AND Fy - c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
    CASE "r":
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx + c, Fy))
        IF Land > 96 THEN EXIT DO
        IF Fx + c = Tx AND Fy = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx - c, Fy))
        IF Land > 96 THEN EXIT DO
        IF Fx - c = Tx AND Fy = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx, Fy + c))
        IF Land > 96 THEN EXIT DO
        IF Fx = Tx AND Fy + c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx, Fy - c))
        IF Land > 96 THEN EXIT DO
        IF Fx = Tx AND Fy - c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
    CASE "R":
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx + c, Fy))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF Fx + c = Tx AND Fy = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx - c, Fy))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF Fx - c = Tx AND Fy = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx, Fy + c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF Fx = Tx AND Fy + c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx, Fy - c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF Fx = Tx AND Fy - c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
    CASE "q":
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx + c, Fy))
        IF Land > 96 THEN EXIT DO
        IF Fx + c = Tx AND Fy = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx - c, Fy))
        IF Land > 96 THEN EXIT DO
        IF Fx - c = Tx AND Fy = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx, Fy + c))
        IF Land > 96 THEN EXIT DO
        IF Fx = Tx AND Fy + c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx, Fy - c))
        IF Land > 96 THEN EXIT DO
        IF Fx = Tx AND Fy - c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx + c, Fy + c))
        IF Land > 96 THEN EXIT DO
        IF Fx + c = Tx AND Fy + c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx + c, Fy - c))
        IF Land > 96 THEN EXIT DO
        IF Fx + c = Tx AND Fy - c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx - c, Fy + c))
        IF Land > 96 THEN EXIT DO
        IF Fx - c = Tx AND Fy + c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx - c, Fy - c))
        IF Land > 96 THEN EXIT DO
        IF Fx - c = Tx AND Fy - c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
    CASE "Q":
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx + c, Fy + c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF Fx + c = Tx AND Fy + c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx + c, Fy - c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF Fx + c = Tx AND Fy - c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx - c, Fy + c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF Fx - c = Tx AND Fy + c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx - c, Fy - c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF Fx - c = Tx AND Fy - c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx + c, Fy))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF Fx + c = Tx AND Fy = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx - c, Fy))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF Fx - c = Tx AND Fy = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx, Fy + c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF Fx = Tx AND Fy + c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(Fx, Fy - c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF Fx = Tx AND Fy - c = Ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
    CASE "k"
      IF Tx > Fx + 1 THEN EXIT FUNCTION
      IF Tx < Fx - 1 THEN EXIT FUNCTION
      IF Ty > Fy + 1 THEN EXIT FUNCTION
      IF Ty < Fy - 1 THEN EXIT FUNCTION
      IF Land < 96 THEN LegalMove = TRUE: EXIT FUNCTION
    CASE "K"
      IF Tx > Fx + 1 THEN EXIT FUNCTION
      IF Tx < Fx - 1 THEN EXIT FUNCTION
      IF Ty > Fy + 1 THEN EXIT FUNCTION
      IF Ty < Fy - 1 THEN EXIT FUNCTION
      IF Land > 96 OR Land = 48 THEN LegalMove = TRUE: EXIT FUNCTION
      
    'CASE ELSE: PRINT "Error! Unknown piece'"; Block(px, py); "' ASC("; ASC(Block(px, py)); ")": END
  END SELECT



END FUNCTION

SUB Main

DO
  IF InCheck(1) THEN BEEP
Player1:
  ShowMouse
  GetUserInput 1
  'GoodAI 1
  Piece$ = Block(FromX, FromY)
  Block(FromX, FromY) = "0"
  Hold$ = Block(ToX, ToY)
  Block(ToX, ToY) = Piece$
  IF InCheck(1) THEN
    Block(FromX, FromY) = Piece$
    Block(ToX, ToY) = Hold$
    HideMouse
    XorSpot FromX, FromY, 2
    ShowMouse
    GOTO Player1
  END IF
  FOR n = 1 TO 8
    IF Block(n, 1) = "p" THEN Block(n, 1) = "q"
  NEXT
  HideMouse
  XorSpot FromX, FromY, 1
  XorSpot ToX, ToY, 1
  DrawSpot ToX, ToY, Piece$
  ShowMouse
  NumMoves = NumMoves + 1
  PMove$(NumMoves) = STR$(FromX) + "," + STR$(FromY) + "," + STR$(ToX) + "," + STR$(ToY)

Player2:
  FOR py = 1 TO 8
    FOR px = 1 TO 8
      IF Block(px, py) = "K" THEN IF InDanger(px, py, 2) THEN BEEP
    NEXT
  NEXT
  'GetUserInput 2
  GoodAI 2
  Piece$ = Block(FromX, FromY)
  HideMouse
  XorSpot FromX, FromY, 1
  Block(FromX, FromY) = "0"
  XorSpot ToX, ToY, 1
  DrawSpot ToX, ToY, Piece$
  Block(ToX, ToY) = Piece$
  IF InCheck(2) THEN
    Block(FromX, FromY) = Piece$
    Block(ToX, ToY) = Hold$
    HideMouse
    XorSpot FromX, FromY, 2
    ShowMouse
    GOTO Player2
  END IF
 
  NumMoves = NumMoves + 1
  PMove$(NumMoves) = STR$(FromX) + "," + STR$(FromY) + "," + STR$(ToX) + "," + STR$(ToY)
  ShowMouse
  FOR n = 1 TO 8
    IF Block(n, 8) = "P" THEN Block(n, 8) = "Q"
  NEXT

LOOP
END SUB

SUB PutIMG (px, py, Array())

  FOR ny = 1 TO 16
    FOR nx = 1 TO 16
      IF Array(nx, ny) THEN PSET (Xpos(px) + Xoff + nx - 1, Xpos(py) + Yoff + ny - 1), Array(nx, ny)
    NEXT
  NEXT

END SUB

SUB RedBlink
  'EXIT SUB
  STATIC red
  STATIC Down
  IF Down = 0 THEN red = red + 1
  IF Down = 1 THEN red = red - 1
  IF red > 60 THEN Down = 1
  IF red < 10 THEN Down = 0
  PALETTE 254, red

END SUB

SUB ResetMouse (NumButtons, Errlvl)
 
  Registers.AX = &H0
  CALL interrupt(&H33, Registers, Registers)
  NumButtons = Registers.BX     ' # of avalible buttons
  Errlvl = Registers.AX         'Stores ERRORLEVEL Values

END SUB

SUB ShowMouse

  Registers.AX = &H1
  CALL interrupt(&H33, Registers, Registers)

END SUB

FUNCTION Value (p$)
  SELECT CASE UCASE$(p$)
    CASE "P": Value = 1
    CASE "N": Value = 2
    CASE "B": Value = 2
    CASE "R": Value = 3
    CASE "Q": Value = 8
    CASE "K": Value = 6
    CASE ELSE: Value = 0
  END SELECT
END FUNCTION

SUB Watch

  INPUT "File name to watch:", f$
  IF INSTR(f$, ".") = 0 THEN f$ = f$ + ".CHS"

  DIM Fx(500)
  DIM Fy(500)
  DIM Tx(500)
  DIM Ty(500)

  OPEN f$ FOR INPUT AS #1
    INPUT #1, NumMoves
    n = 0
    DO
      n = n + 1
      INPUT #1, Fx(n)
      INPUT #1, Fy(n)
      INPUT #1, Tx(n)
      INPUT #1, Ty(n)
    LOOP UNTIL EOF(1)
  CLOSE #1
  NumMoves = n
  FOR n = 1 TO NumMoves
    Block(Tx(n), Ty(n)) = Block(Fx(n), Fy(n))
    Block(Fx(n), Fy(n)) = "0"
    XorSpot Fx(n), Fy(n), 1
    DrawBoard
    IF InCheck(1) OR InCheck(2) THEN BEEP
    Getche
  NEXT
  END



END SUB

SUB XorSpot (px, py, Fill)

  IF Fill = 1 THEN
    IF Board(px, py) = 0 THEN LINE (Xpos(px) + Xoff, Xpos(py) + Yoff)-(Xpos(px) + 15 + Xoff, Xpos(py) + 15 + Yoff), Board0, BF
    IF Board(px, py) = 1 THEN LINE (Xpos(px) + Xoff, Xpos(py) + Yoff)-(Xpos(px) + 15 + Xoff, Xpos(py) + 15 + Yoff), Board1, BF
  ELSE
    IF Board(px, py) = 0 THEN LINE (Xpos(px) + Xoff, Xpos(py) + Yoff)-(Xpos(px) + 15 + Xoff, Xpos(py) + 15 + Yoff), Board0, B
    IF Board(px, py) = 1 THEN LINE (Xpos(px) + Xoff, Xpos(py) + Yoff)-(Xpos(px) + 15 + Xoff, Xpos(py) + 15 + Yoff), Board1, B
  END IF
END SUB

