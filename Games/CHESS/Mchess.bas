'
'  .::::::.   :::::::.        .,-:::::   ::   .: .,:::::: .::::::.  .::::::.
' ,;;'```';;,  ;;;'';;'     ,;;;'````'  ,;;   ;;,;;;;'''';;;`    ` ;;;`    `
' [[[     [[[\ [[[__[[\.    [[[        ,[[[,,,[[[ [[cccc '[==/[[[[,'[==/[[[[,
' "$$c  cc$$$" $$""""Y$$    $$$        "$$$"""$$$ $$""""   '''    $  '''    $
'  "*8bo,Y88b,_88o,,od8P    `88bo,__,o, 888   "88o888oo,__88b    dP 88b    dP
'    "*YP" "M"""YUMMMP"       "YUMMMMMP"MMM    YMM""""YUMMM"YMmMY"   "YMmMY"
'
'
'
'                                QB Chess
'
'                         by Jim MacDonald / Jikg
'         
'
'       Questions, comments, or bugs?  E-mail me: jikg@mychoice.net
'
'
'Special thanks to:
'  Mike Hackett              Piece Artist
'  The guys at M \ K         Modem code


DECLARE SUB PuzzleGame ()
DECLARE SUB Viewgif (File$, MyOffX%, MyOffY%, Transparent%)
DECLARE SUB ModemSetup ()
DECLARE FUNCTION WaitForByte$ (Wait$)
DECLARE SUB IModem (ToDo%)
DECLARE SUB delay (Seconds!)
DECLARE SUB Watch ()
DECLARE SUB GetPoints (px%, py%, Who%, Points%, OutX%, Outy%, Deep%)
DECLARE SUB XorSpot (px%, py%, Fill%)
DECLARE FUNCTION InCheck% (Who%)
DECLARE SUB getche ()
DECLARE SUB GoodAI (Who%)
DECLARE FUNCTION Value% (P$)
DECLARE FUNCTION InDanger% (px%, py%, Who%)
DECLARE SUB AI (Who%)
DECLARE SUB HideMouse ()
DECLARE SUB getch ()
DECLARE SUB DrawSpot (px%, py%, P$)
DECLARE SUB GetMousePos (BX%, CX%, DX%)
DECLARE SUB Main ()
DECLARE FUNCTION LegalMove% (fx%, fy%, tx%, ty%)
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

DIM SHARED Prefix(4095), Suffix(4095), OutStack(4095), shiftout%(8)
DIM SHARED Ybase AS LONG, powersof2(11) AS LONG, WorkCode AS LONG


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


DIM SHARED CPU, PlayMode
DIM SHARED PuzzleMode, MaxMoves, ExitThis

'Modem Data
DIM SHARED YourTurn, ModemGame, Dialing
DIM SHARED ComPort$, Call$

CONST OpenComPort = 1
CONST WaitToConnect = 2
CONST Dial = 3
CONST WaitForCall = 4
CONST ModemPort = 10
ComPort$ = "1"

SCREEN 13
CONST TRUE = 1
CONST FALSE = 0




Init
'Watch
ResetMouse 1, 1
ShowMouse
'LINE (Xpos(1) + Xoff, Xpos(2) + Yoff)-(Xpos(1) + Xoff + 15, Xpos(2) + Yoff + 15), 254, B
IF PuzzleMode = FALSE THEN Main
PuzzleGame



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

SUB delay (Seconds!) STATIC

   Begin! = TIMER
   DO UNTIL (TIMER - Begin! > Seconds!) OR (TIMER - Begin! < 0)
   LOOP

END SUB

SUB DrawBoard

  FOR py = 1 TO 8
    FOR px = 1 TO 8
      CALL DrawSpot(px, py, Block(px, py))
    NEXT
  NEXT

END SUB

SUB DrawSpot (px, py, P$)
     
      SELECT CASE P$
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

SUB getche
DO
  a$ = INKEY$
LOOP WHILE a$ = ""
IF a$ = CHR$(27) THEN END
END SUB

SUB GetIMG (File$, Array())

   
   OPEN File$ FOR INPUT AS #1
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
  deff = 0
  'LOCATE 23: PRINT "                  "
  'LOCATE 2, 1
  IF InDanger(px, py, Who) THEN deff = Value(Block(px, py))
 
  FOR ny = 1 TO 8
    FOR nx = 1 TO 8
      IF LegalMove(px, py, nx, ny) THEN
        P = Value(Block(nx, ny))
        h$ = Block(px, py)
        Block(px, py) = "0"
        Hold$ = Block(nx, ny)
        Block(nx, ny) = h$
        IF InDanger(nx, ny, Who) AND Value(Block(nx, ny)) <> 0 THEN P = P - Value(Block(nx, ny)) * 2: 'PRINT "Bad"; Value(Block(nx, ny))
        IF InCheck(Who) THEN P = P - 1000
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
        P = P + HighH
        P = P + deff
        Block(nx, ny) = Hold$
        Block(px, py) = h$
        IF P > AttackHigh THEN
          AttackHigh = P
          OutX = nx
          Outy = ny
        END IF
      END IF
    NEXT
  NEXT
  IF AttackHigh < deff AND deff > 0 THEN
    FOR ny = 1 TO 8
      FOR nx = 1 TO 8
        IF LegalMove(px, py, nx, ny) AND NOT InDanger(nx, ny, Who) THEN
          'PRINT Block(px, py); deff; ":"; nx; ","; ny; "Deffence"
          Points = deff
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

  IF AttackHigh > 0 THEN Points = AttackHigh + deff ELSE Points = 0

END SUB

SUB GetUserInput (Who)
 
IF ModemGame = TRUE AND Who <> YourTurn THEN GOTO WaitForNetWork
  
ChosePiece:

  DO
    CALL GetMousePos(Butt, mx, my)
    a$ = INKEY$
    IF a$ <> "" THEN
    IF ModemGame = TRUE THEN
      m$ = INPUT$(1, ModemPort)
      IF m$ <> "" THEN
        SELECT CASE m$
          CASE "/":
            Null$ = WaitForByte$(CHR$(13))
            Text$ = WaitForByte(CHR$(1))
            LOCATE 22, 1: PRINT Text$
            PLAY "l25n30n25n30"
        END SELECT
      END IF
    END IF
   
    SELECT CASE UCASE$(a$)
      CASE "T":
        IF ModemGame = TRUE THEN
          LOCATE 1, 1: INPUT ">>"; Text$
          PRINT #ModemPort, "/"
          PRINT #ModemPort, Text$ + CHR$(1)
        END IF
      CASE CHR$(27):
        IF ModemGame = TRUE THEN IModem Disconnect
        END
      CASE "H":
        GoodAI 1
        LOCATE 1, 1: PRINT HighPoint; Block(FromX, FromY); ":"; ToX; ToY
      CASE "R"
        HideMouse
        Viewgif "chess", 0, 0, -1
        'DEF SEG = &HA000
        'BLOAD "BckGrnd8.GFX"
        'DEF SEG
        FOR py = 1 TO 8
        FOR px = 1 TO 8
        IF Board(px, py) = 0 THEN LINE (Xoff + Xpos(px), Yoff + Xpos(py))-(Xoff + Xpos(px) + 15, Yoff + Xpos(py) + 15), Board0, BF
        IF Board(px, py) = 1 THEN LINE (Xoff + Xpos(px), Yoff + Xpos(py))-(Xoff + Xpos(px) + 15, Yoff + Xpos(py) + 15), Board1, BF
        NEXT
        NEXT
        DrawBoard
        ShowMouse
        ShowMouse
        ShowMouse
        ShowMouse
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

  EXIT SUB

WaitForNetWork:
  HideMouse
  DO
    a$ = INKEY$
    'IF LOC(ModemPort) <> 0 THEN m$ = INPUT$(1, ModemPort) ELSE m$ = ""
    m$ = INPUT$(1, ModemPort)
    IF m$ <> "" THEN
      SELECT CASE m$
        CASE "+": IModem Disconnect: END
        CASE "M":
          Null$ = WaitForByte(CHR$(13))
          fx$ = WaitForByte(CHR$(13))
          fy$ = WaitForByte(CHR$(13))
          tx$ = WaitForByte(CHR$(13))
          ty$ = WaitForByte(CHR$(13))
          FromX = ASC(fx$)
          FromY = ASC(fy$)
          ToX = ASC(tx$)
          ToY = ASC(ty$)
          ShowMouse
          EXIT SUB
        CASE "/":
          Null$ = WaitForByte$(CHR$(13))
          Text$ = WaitForByte(CHR$(1))
          LOCATE 22, 1: PRINT Text$
          PLAY "l25n30n25n30"

      END SELECT
    END IF
    IF a$ <> "" THEN
      SELECT CASE UCASE$(a$)
        CASE CHR$(27): IModem Disconnect: END
        CASE "T":
          LOCATE 1, 1: INPUT ">>"; Text$
          PRINT #ModemPort, "/"
          PRINT #ModemPort, Text$ + CHR$(1)
      END SELECT
    END IF
  LOOP


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
        IF PuzzleMode = TRUE THEN ExitThis = TRUE: EXIT SUB
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
      IF Who = 1 AND Land >= 96 THEN GetPoints px, py, Who, Points, OutX, Outy, 1
     
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
  'LOCATE 1, 1: PRINT HighPoint; "Fx:"; FromX; "Fy:"; FromY; "ToX:"; ToX; "ToY:"; ToY
  IF HighPoint = 0 THEN AI Who

END SUB

SUB HideMouse
 
  Registers.AX = &H2
  CALL interrupt(&H33, Registers, Registers)
  
END SUB

SUB IModem (ToDo)

SELECT CASE ToDo
  CASE WaitToConnect
        PRINT "Waiting for Connection..."
        Rot$ = "|"
        Rot% = 0
        DO UNTIL RIGHT$(Con$, 8) = "CONNECT " OR RIGHT$(Con$, 4) = "BUSY"
        LOCATE 1, 26: PRINT Rot$
        SELECT CASE Rot%
        CASE 1: Rot$ = "\"
        CASE 2: Rot$ = "-"
        CASE 3: Rot$ = "/"
        CASE 4: Rot$ = "|": Rot% = 0
        END SELECT
        Rot% = Rot% + 1
        aa$ = INKEY$
        delay .08
        IF aa$ = CHR$(27) THEN PRINT #ModemPort, "+++ATH": EXIT SUB
        IF LOC(ModemPort) <> 0 THEN
        m$ = INPUT$(1, ModemPort)
        Con$ = Con$ + m$
        LOCATE 2, 1: PRINT Con$
        END IF
        LOOP
        IF RIGHT$(Con$, 4) = "BUSY" THEN EXIT SUB
        bps$ = "1"
        DO UNTIL VAL(bps$) = 0
        IF LOC(ModemPort) <> 0 THEN bps$ = INPUT$(1, ModemPort)
        LOOP
        DO UNTIL bps$ <> "0"
        IF LOC(ModemPort) <> 0 THEN bps$ = INPUT$(1, ModemPort)
        LOOP
        PRINT "Successfully connected!"
  CASE OpenComPort
        OPEN "COM" + ComPort$ + ":19200" + ", N,8,1,BIN,RB2048,TB2048" FOR RANDOM AS #ModemPort
        PRINT #ModemPort, "ATZ"
  CASE Dial
    TonePulse$ = "T"
    PRINT #ModemPort, "ATD" + TonePulse$ + Call$
    delay 1
    IModem WaitToConnect
  CASE WaitForCall
    PRINT #ModemPort, "ATS0=1"
    IModem WaitToConnect
  CASE Disconnect
    PRINT #ModemPort, "+++ATH": PRINT #ModemPort, "atz": CLOSE #ModemPort
END SELECT



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
 
 
 
  'DEF SEG = &HA000
  'BLOAD "BckGrnd8.GFX"
  'DEF SEG
  Viewgif "chess", 0, 0, -1
  Viewgif "qbchess", 70, 30, 52
  
   'LOCATE 18, 9: PRINT "1. Player vs. Computer  "
   'LOCATE , 9: PRINT "2. Player vs. Player    "
   'LOCATE , 9: PRINT "3. Modem game           "
   'LOCATE , 9: PRINT "4. Computer vs. Computer"
   Viewgif "options", 96, 130, 52
   DO
     a$ = INPUT$(1)
   LOOP WHILE VAL(a$) < 0 OR VAL(a$) > 5
   PlayMode = VAL(a$)
   IF PlayMode = 1 THEN CPU = TRUE
   IF PlayMode = 3 THEN ModemGame = TRUE
   IF PlayMode = 5 THEN PuzzleMode = TRUE
   IF ModemGame = TRUE THEN ModemSetup


   Viewgif "chess", 0, 0, -1

  FOR py = 1 TO 8
    FOR px = 1 TO 8
     IF Board(px, py) = 0 THEN LINE (Xoff + Xpos(px), Yoff + Xpos(py))-(Xoff + Xpos(px) + 15, Yoff + Xpos(py) + 15), Board0, BF
     IF Board(px, py) = 1 THEN LINE (Xoff + Xpos(px), Yoff + Xpos(py))-(Xoff + Xpos(px) + 15, Yoff + Xpos(py) + 15), Board1, BF
    NEXT
  NEXT

  IF PuzzleMode = FALSE THEN DrawBoard

END SUB

FUNCTION LegalMove (fx, fy, tx, ty)

  IF ty > 8 OR ty < 1 OR tx > 8 OR tx < 1 THEN EXIT FUNCTION
  'Caps = 65-90
  'Lower = 97-122
 
  Land = ASC(Block(tx, ty))
  SELECT CASE Block(fx, fy)
    CASE "p"
      IF ty = fy - 1 AND tx = fx AND Block(tx, ty) = "0" THEN LegalMove = TRUE
      IF ty = fy - 1 AND tx = fx - 1 AND Land < 96 AND Block(tx, ty) <> "0" THEN LegalMove = TRUE
      IF ty = fy - 1 AND tx = fx + 1 AND Land < 96 AND Block(tx, ty) <> "0" THEN LegalMove = TRUE
      IF ty = fy - 2 AND tx = fx AND fy = 7 AND Block(tx, ty) = "0" AND Block(tx, ty + 1) = "0" THEN LegalMove = TRUE
    CASE "P"
      IF ty = fy + 1 AND tx = fx AND Block(tx, ty) = "0" THEN LegalMove = TRUE
      IF ty = fy + 1 AND tx = fx - 1 AND Land > 96 AND Block(tx, ty) <> "0" THEN LegalMove = TRUE
      IF ty = fy + 1 AND tx = fx + 1 AND Land > 96 AND Block(tx, ty) <> "0" THEN LegalMove = TRUE
      IF ty = fy + 2 AND tx = fx AND fy = 2 AND Block(tx, ty) = "0" AND Block(tx, ty - 1) = "0" THEN LegalMove = TRUE
    CASE "n":
      IF ty = fy - 2 AND tx = fx - 1 AND Land < 96 THEN LegalMove = TRUE
      IF ty = fy - 2 AND tx = fx + 1 AND Land < 96 THEN LegalMove = TRUE
      IF ty = fy + 2 AND tx = fx - 1 AND Land < 96 THEN LegalMove = TRUE
      IF ty = fy + 2 AND tx = fx + 1 AND Land < 96 THEN LegalMove = TRUE
      IF ty = fy - 1 AND tx = fx - 2 AND Land < 96 THEN LegalMove = TRUE
      IF ty = fy - 1 AND tx = fx + 2 AND Land < 96 THEN LegalMove = TRUE
      IF ty = fy + 1 AND tx = fx - 2 AND Land < 96 THEN LegalMove = TRUE
      IF ty = fy + 1 AND tx = fx + 2 AND Land < 96 THEN LegalMove = TRUE
    CASE "N":
      IF ty = fy - 2 AND tx = fx - 1 AND (Land > 96 OR Land = 48) THEN LegalMove = TRUE
      IF ty = fy - 2 AND tx = fx + 1 AND (Land > 96 OR Land = 48) THEN LegalMove = TRUE
      IF ty = fy + 2 AND tx = fx - 1 AND (Land > 96 OR Land = 48) THEN LegalMove = TRUE
      IF ty = fy + 2 AND tx = fx + 1 AND (Land > 96 OR Land = 48) THEN LegalMove = TRUE
      IF ty = fy - 1 AND tx = fx - 2 AND (Land > 96 OR Land = 48) THEN LegalMove = TRUE
      IF ty = fy - 1 AND tx = fx + 2 AND (Land > 96 OR Land = 48) THEN LegalMove = TRUE
      IF ty = fy + 1 AND tx = fx - 2 AND (Land > 96 OR Land = 48) THEN LegalMove = TRUE
      IF ty = fy + 1 AND tx = fx + 2 AND (Land > 96 OR Land = 48) THEN LegalMove = TRUE
    CASE "b":
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx + c, fy + c))
        IF Land > 96 THEN EXIT DO
        IF fx + c = tx AND fy + c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx + c, fy - c))
        IF Land > 96 THEN EXIT DO
        IF fx + c = tx AND fy - c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx - c, fy + c))
        IF Land > 96 THEN EXIT DO
        IF fx - c = tx AND fy + c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx - c, fy - c))
        IF Land > 96 THEN EXIT DO
        IF fx - c = tx AND fy - c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
    CASE "B":
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx + c, fy + c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF fx + c = tx AND fy + c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx + c, fy - c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF fx + c = tx AND fy - c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx - c, fy + c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF fx - c = tx AND fy + c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx - c, fy - c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF fx - c = tx AND fy - c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
    CASE "r":
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx + c, fy))
        IF Land > 96 THEN EXIT DO
        IF fx + c = tx AND fy = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx - c, fy))
        IF Land > 96 THEN EXIT DO
        IF fx - c = tx AND fy = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx, fy + c))
        IF Land > 96 THEN EXIT DO
        IF fx = tx AND fy + c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx, fy - c))
        IF Land > 96 THEN EXIT DO
        IF fx = tx AND fy - c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
    CASE "R":
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx + c, fy))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF fx + c = tx AND fy = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx - c, fy))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF fx - c = tx AND fy = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx, fy + c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF fx = tx AND fy + c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx, fy - c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF fx = tx AND fy - c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
    CASE "q":
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx + c, fy))
        IF Land > 96 THEN EXIT DO
        IF fx + c = tx AND fy = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx - c, fy))
        IF Land > 96 THEN EXIT DO
        IF fx - c = tx AND fy = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx, fy + c))
        IF Land > 96 THEN EXIT DO
        IF fx = tx AND fy + c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx, fy - c))
        IF Land > 96 THEN EXIT DO
        IF fx = tx AND fy - c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx + c, fy + c))
        IF Land > 96 THEN EXIT DO
        IF fx + c = tx AND fy + c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx + c, fy - c))
        IF Land > 96 THEN EXIT DO
        IF fx + c = tx AND fy - c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx - c, fy + c))
        IF Land > 96 THEN EXIT DO
        IF fx - c = tx AND fy + c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx - c, fy - c))
        IF Land > 96 THEN EXIT DO
        IF fx - c = tx AND fy - c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land < 96 AND Land <> 48 THEN EXIT DO
      LOOP WHILE c < 8
    CASE "Q":
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx + c, fy + c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF fx + c = tx AND fy + c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx + c, fy - c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF fx + c = tx AND fy - c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx - c, fy + c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF fx - c = tx AND fy + c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx - c, fy - c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF fx - c = tx AND fy - c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx + c, fy))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF fx + c = tx AND fy = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx - c, fy))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF fx - c = tx AND fy = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx, fy + c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF fx = tx AND fy + c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
      c = 0
      DO
        c = c + 1
        Land = ASC(Block(fx, fy - c))
        IF Land < 96 AND Land <> 48 THEN EXIT DO
        IF fx = tx AND fy - c = ty THEN LegalMove = TRUE: EXIT FUNCTION
        IF Land > 96 THEN EXIT DO
      LOOP WHILE c < 8
    CASE "k"
      IF tx > fx + 1 THEN EXIT FUNCTION
      IF tx < fx - 1 THEN EXIT FUNCTION
      IF ty > fy + 1 THEN EXIT FUNCTION
      IF ty < fy - 1 THEN EXIT FUNCTION
      IF Land < 96 THEN LegalMove = TRUE: EXIT FUNCTION
    CASE "K"
      IF tx > fx + 1 THEN EXIT FUNCTION
      IF tx < fx - 1 THEN EXIT FUNCTION
      IF ty > fy + 1 THEN EXIT FUNCTION
      IF ty < fy - 1 THEN EXIT FUNCTION
      IF Land > 96 OR Land = 48 THEN LegalMove = TRUE: EXIT FUNCTION
      
    'CASE ELSE: PRINT "Error! Unknown piece'"; Block(px, py); "' ASC("; ASC(Block(px, py)); ")": END
  END SELECT



END FUNCTION

SUB Main

DO
  IF InCheck(1) THEN BEEP
Player1:
  IF PuzzleMode = TRUE THEN LOCATE 1, 1: PRINT "Move:"; PuzzleMove + 1
  ShowMouse
  IF PlayMode <> 4 THEN GetUserInput 1
  IF PlayMode = 4 THEN GoodAI 1
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
  IF ModemGame = TRUE AND YourTurn = 1 THEN
    PRINT #ModemPort, "M"
    PRINT #ModemPort, CHR$(FromX) '
    PRINT #ModemPort, CHR$(FromY) '+ CHR$(13)
    PRINT #ModemPort, CHR$(ToX) '+ CHR$(13)
    PRINT #ModemPort, CHR$(ToY) '+ CHR$(13)
    delay 1
  END IF
  IF InCheck(2) THEN BEEP
  IF PuzzleMode = TRUE THEN PuzzleMove = PuzzleMove + 1

Player2:
  'FOR py = 1 TO 8
  '  FOR px = 1 TO 8
  '    IF Block(px, py) = "K" THEN IF InDanger(px, py, 2) THEN BEEP
  '  NEXT
  'NEXT
  IF PlayMode <> 4 AND CPU = FALSE THEN GetUserInput 2
  IF PlayMode = 4 OR CPU = TRUE THEN GoodAI 2
  IF ExitThis = TRUE THEN EXIT SUB
  IF PuzzleMode = TRUE AND PuzzleMove >= MaxMoves THEN LOCATE 1, 1: PRINT "Failed to checkmate...": EXIT SUB
  Piece$ = Block(FromX, FromY)
  Block(FromX, FromY) = "0"
  Hold$ = Block(ToX, ToY)
  Block(ToX, ToY) = Piece$
  IF InCheck(2) THEN
    Block(FromX, FromY) = Piece$
    Block(ToX, ToY) = Hold$
    HideMouse
    XorSpot FromX, FromY, 2
    ShowMouse
    GOTO Player2
  END IF
 
  HideMouse
  XorSpot FromX, FromY, 1
  XorSpot ToX, ToY, 1
  DrawSpot ToX, ToY, Piece$
  Block(ToX, ToY) = Piece$
 
  NumMoves = NumMoves + 1
  PMove$(NumMoves) = STR$(FromX) + "," + STR$(FromY) + "," + STR$(ToX) + "," + STR$(ToY)
  IF ModemGame = TRUE AND YourTurn = 2 THEN
    PRINT #ModemPort, "M"
    PRINT #ModemPort, CHR$(FromX) '+ CHR$(1)
    PRINT #ModemPort, CHR$(FromY) '+ CHR$(1)
    PRINT #ModemPort, CHR$(ToX) '+ CHR$(1)
    PRINT #ModemPort, CHR$(ToY) '+ CHR$(1)
  END IF
  ShowMouse
  FOR n = 1 TO 8
    IF Block(n, 8) = "P" THEN Block(n, 8) = "Q"
  NEXT
LOOP
END SUB

SUB ModemSetup
   SCREEN 0
   WIDTH 80
   DO
     LOCATE 1, 1:
     INPUT "Enter COM Port (default is 1):", ComPort$
     IF ComPort$ = "" THEN ComPort$ = "1"
   LOOP WHILE VAL(ComPort$) = 0
  
   PRINT "1. Dial"
   PRINT "2. Wait for call"
   PRINT "3. Allready connected"
   DO
     a$ = INPUT$(1)
   LOOP WHILE VAL(a$) < 0 OR VAL(a$) > 3
   command = VAL(a$)
   SELECT CASE command
     CASE 1:
       YourTurn = 1
       INPUT "Enter phone number to dial (985-3581):", Call$
       PRINT "Press a key to dial..."
       getche
       IModem OpenComPort
       delay 1
       CLS
       IModem Dial
     CASE 2:
       YourTurn = 2
       PRINT "Press a key to begin waiting..."
       getche
       IModem OpenComPort
       delay 1
       CLS
       IModem WaitForCall
     CASE 3:
       YourTurn = 1
       IModem OpenComPort
       delay 1
       CLS
   END SELECT
   SCREEN 13






END SUB

SUB PutIMG (px, py, Array())

  FOR ny = 1 TO 16
    FOR nx = 1 TO 16
      IF Array(nx, ny) THEN PSET (Xpos(px) + Xoff + nx - 1, Xpos(py) + Yoff + ny - 1), Array(nx, ny)
    NEXT
  NEXT

END SUB

SUB PuzzleGame

  CPU = TRUE
  CurP = 1
      FOR py = 1 TO 8
        FOR px = 1 TO 8
         IF Board(px, py) = 0 THEN LINE (Xoff + Xpos(px), Yoff + Xpos(py))-(Xoff + Xpos(px) + 15, Yoff + Xpos(py) + 15), Board0, BF
         IF Board(px, py) = 1 THEN LINE (Xoff + Xpos(px), Yoff + Xpos(py))-(Xoff + Xpos(px) + 15, Yoff + Xpos(py) + 15), Board1, BF
        NEXT
      NEXT
 
  DEF SEG = &HA000
  BSAVE "chess.TMP", 0, 64000
  DEF SEG
 
  GOSUB GetPuzzleData
  DrawBoard
  DO
    ExitThis = FALSE
    LOCATE 1, 1: PRINT "Puzzle #:"; CurP
    LOCATE 2, 1: PRINT "White Checkmates in"; MaxMoves; "moves"
    DO
      a$ = INKEY$
    LOOP WHILE a$ = ""
    SELECT CASE UCASE$(a$)
      CASE CHR$(0) + "M":
        CurP = CurP + 1
      CASE CHR$(0) + "K":
        CurP = CurP - 1
        IF CurP < 1 THEN CurP = 1
      CASE CHR$(27): END
    CASE CHR$(13):
      HideMouse
      DEF SEG = &HA000
      BLOAD "chess.TMP"
      DEF SEG
      DrawBoard
      ShowMouse
      Main
      getche
      a$ = CHR$(0)
      DEF SEG = &HA000
      BLOAD "chess.TMP"
      DEF SEG

    END SELECT
    IF LEFT$(a$, 1) = CHR$(0) THEN
      GOSUB GetPuzzleData
      HideMouse
      FOR py = 1 TO 8
        FOR px = 1 TO 8
         IF Board(px, py) = 0 THEN LINE (Xoff + Xpos(px), Yoff + Xpos(py))-(Xoff + Xpos(px) + 15, Yoff + Xpos(py) + 15), Board0, BF
         IF Board(px, py) = 1 THEN LINE (Xoff + Xpos(px), Yoff + Xpos(py))-(Xoff + Xpos(px) + 15, Yoff + Xpos(py) + 15), Board1, BF
        NEXT
      NEXT
      DrawBoard
      ShowMouse
    END IF
    
  LOOP


GetPuzzleData:
OPEN "qbchess.puz" FOR INPUT AS #8
  PuzzNum = 0
  DO
    PuzzNum = PuzzNum + 1
    IF PuzzNum = CurP THEN
      INPUT #8, MaxMoves
      FOR py = 1 TO 8
        LINE INPUT #8, Row$
        FOR px = 1 TO 8
          Block(px, py) = MID$(Row$, px * 3 - 2, 1)
          Block(1, py) = MID$(Row$, 1, 1)
        NEXT
        
      NEXT
      LINE INPUT #8, Hint$
      CLOSE #8
      RETURN
    ELSE
      FOR py = 1 TO 10
        LINE INPUT #8, a$
      NEXT
    END IF
  LOOP UNTIL EOF(8)
  PRINT "Puzzle Number not found!": END
RETURN


END SUB

SUB RedBlink
  'EXIT SUB
  STATIC Red
  STATIC Down
  IF Down = 0 THEN Red = Red + 1
  IF Down = 1 THEN Red = Red - 1
  IF Red > 60 THEN Down = 1: Red = 60
  IF Red < 10 THEN Down = 0: Red = 10
  PALETTE 254, Red

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

FUNCTION Value (P$)
  SELECT CASE UCASE$(P$)
    CASE "P": Value = 1
    CASE "N": Value = 2
    CASE "B": Value = 2
    CASE "R": Value = 3
    CASE "Q": Value = 8
    CASE "K": Value = 6
    CASE ELSE: Value = 0
  END SELECT
END FUNCTION

SUB Viewgif (File$, MyOffX, MyOffY, Transparent)

OPEN File$ + ".GIF" FOR INPUT AS #1
CLOSE #1
FOR a% = 0 TO 7: shiftout%(8 - a%) = 2 ^ a%: NEXT a%
FOR a% = 0 TO 11: powersof2(a%) = 2 ^ a%: NEXT a%
a$ = File$
IF a$ = "" THEN INPUT "GIF file"; a$: IF a$ = "" THEN END
IF INSTR(a$, ".") = 0 THEN a$ = a$ + ".gif"
OPEN a$ FOR BINARY AS #1
a$ = "      ": GET #1, , a$
'IF a$ <> "GIF87a" THEN PRINT "Not a GIF87a file.": END
GET #1, , TotalX: GET #1, , TotalY: GOSUB GetByte
NumColors = 2 ^ ((a% AND 7) + 1): NoPalette = (a% AND 128) = 0
GOSUB GetByte: Background = a%
GOSUB GetByte: IF a% <> 0 THEN PRINT "Bad screen descriptor.": END
IF NoPalette = 0 THEN P$ = SPACE$(NumColors * 3): GET #1, , P$
DO
    GOSUB GetByte
    IF a% = 44 THEN
        EXIT DO
    ELSEIF a% <> 33 THEN
        PRINT "Unknown extension type.": END
    END IF
    GOSUB GetByte
    DO: GOSUB GetByte: a$ = SPACE$(a%): GET #1, , a$: LOOP UNTIL a% = 0
LOOP
GET #1, , XStart: GET #1, , YStart: GET #1, , XLength: GET #1, , YLength
XEnd = XStart + XLength: YEnd = YStart + YLength: GOSUB GetByte
IF a% AND 128 THEN PRINT "Can't handle local colormaps.": END
Interlaced = a% AND 64: PassNumber = 0: PassStep = 8
GOSUB GetByte
ClearCode = 2 ^ a%
EOSCode = ClearCode + 1
FirstCode = ClearCode + 2: NextCode = FirstCode
StartCodeSize = a% + 1: CodeSize = StartCodeSize
StartMaxCode = 2 ^ (a% + 1) - 1: MaxCode = StartMaxCode

BitsIn = 0: BlockSize = 0: BlockPointer = 1
x% = XStart: y% = YStart: Ybase = y% * 320&

SCREEN 13: DEF SEG = &HA000
IF NoPalette = 0 THEN
    OUT &H3C7, 0: OUT &H3C8, 0
    FOR a% = 1 TO NumColors * 3: OUT &H3C9, ASC(MID$(P$, a%, 1)) \ 4: NEXT a%
END IF
'LINE (0, 0)-(319, 199), Background, BF
DO
    GOSUB GetCode
    IF Code <> EOSCode THEN
        IF Code = ClearCode THEN
            NextCode = FirstCode
            CodeSize = StartCodeSize
            MaxCode = StartMaxCode
            GOSUB GetCode
            CurCode = Code: LastCode = Code: LastPixel = Code
            IF x% + MyOffX < 320 AND LastPixel <> Transparent THEN POKE x% + MyOffX + Ybase, LastPixel
            x% = x% + 1: IF x% = XEnd THEN GOSUB NextScanLine
        ELSE
            CurCode = Code: StackPointer = 0
            IF Code > NextCode THEN EXIT DO
            IF Code = NextCode THEN
                CurCode = LastCode
                OutStack(StackPointer) = LastPixel
                StackPointer = StackPointer + 1
            END IF

            DO WHILE CurCode >= FirstCode
                OutStack(StackPointer) = Suffix(CurCode)
                StackPointer = StackPointer + 1
                CurCode = Prefix(CurCode)
            LOOP

            LastPixel = CurCode
            IF x% + MyOffX < 320 AND LastPixel <> Transparent THEN POKE x% + MyOffX + Ybase, LastPixel
            x% = x% + 1: IF x% = XEnd THEN GOSUB NextScanLine

            FOR a% = StackPointer - 1 TO 0 STEP -1
                IF x% + MyOffX < 320 AND OutStack(a%) <> Transparent THEN POKE x% + MyOffX + Ybase, OutStack(a%)
                x% = x% + 1: IF x% = XEnd THEN GOSUB NextScanLine
            NEXT a%

            IF NextCode < 4096 THEN
                Prefix(NextCode) = LastCode
                Suffix(NextCode) = LastPixel
                NextCode = NextCode + 1
                IF NextCode > MaxCode AND CodeSize < 12 THEN
                    CodeSize = CodeSize + 1
                    MaxCode = MaxCode * 2 + 1
                END IF
            END IF
            LastCode = Code
        END IF
    END IF
LOOP UNTIL DoneFlag OR Code = EOSCode
CLOSE #1
EXIT SUB

GetByte: a$ = " ": GET #1, , a$: a% = ASC(a$): RETURN

NextScanLine:
    IF Interlaced THEN
        y% = y% + PassStep
        IF y% >= YEnd THEN
            PassNumber = PassNumber + 1
            SELECT CASE PassNumber
            CASE 1: y% = 4: PassStep = 8
            CASE 2: y% = 2: PassStep = 4
            CASE 3: y% = 1: PassStep = 2
            END SELECT
        END IF
    ELSE
        y% = y% + 1
    END IF
    x% = XStart: Ybase = (y% + MyOffY) * 320&: DoneFlag = y% > 199
RETURN
GetCode:
    IF BitsIn = 0 THEN GOSUB ReadBufferedByte: LastChar = a%: BitsIn = 8
    WorkCode = LastChar \ shiftout%(BitsIn)
    DO WHILE CodeSize > BitsIn
        GOSUB ReadBufferedByte: LastChar = a%
        WorkCode = WorkCode OR LastChar * powersof2(BitsIn)
        BitsIn = BitsIn + 8
    LOOP
    BitsIn = BitsIn - CodeSize
    Code = WorkCode AND MaxCode
RETURN
ReadBufferedByte:
    IF BlockPointer > BlockSize THEN
        GOSUB GetByte: BlockSize = a%
        a$ = SPACE$(BlockSize): GET #1, , a$
        BlockPointer = 1
    END IF
    a% = ASC(MID$(a$, BlockPointer, 1)): BlockPointer = BlockPointer + 1
RETURN


END SUB

FUNCTION WaitForByte$ (Wait$)

  DO
    IF LOC(ModemPort) <> 0 THEN m$ = INPUT$(1, ModemPort) ELSE m$ = ""
    IF m$ = Wait$ THEN EXIT DO
    IF m$ <> "" THEN Byte$ = Byte$ + m$
  LOOP
  WaitForByte$ = Byte$

END FUNCTION

SUB Watch

  INPUT "File name to watch:", f$
  IF INSTR(f$, ".") = 0 THEN f$ = f$ + ".CHS"

  DIM fx(500)
  DIM fy(500)
  DIM tx(500)
  DIM ty(500)

  OPEN f$ FOR INPUT AS #1
    INPUT #1, NumMoves
    n = 0
    DO
      n = n + 1
      INPUT #1, fx(n)
      INPUT #1, fy(n)
      INPUT #1, tx(n)
      INPUT #1, ty(n)
    LOOP UNTIL EOF(1)
  CLOSE #1
  NumMoves = n
  FOR n = 1 TO NumMoves
    Block(tx(n), ty(n)) = Block(fx(n), fy(n))
    Block(fx(n), fy(n)) = "0"
    XorSpot fx(n), fy(n), 1
    DrawBoard
    IF InCheck(1) OR InCheck(2) THEN BEEP
    getche
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

