DEFINT A-Z
'-----------------------------------------------------------------------------
'                     DIMENSION MAN - 1995 THE S.H. SOFTWORX
'-----------------------------------------------------------------------------

TYPE HUES
 RED AS INTEGER
 GRN AS INTEGER
 BLU AS INTEGER
END TYPE

TYPE HIGHSCORES
 S AS DOUBLE
 D AS INTEGER
 N AS STRING * 10
END TYPE

DECLARE SUB INITIALIZE ()
DECLARE SUB CHKSCREEN ()
DECLARE SUB HRLSQRE ()
DECLARE SUB PSRPLTE ()
DECLARE SUB USEBOMB (BD%)
DECLARE SUB RMTEXPLD ()
DECLARE SUB HITWALL ()
DECLARE SUB D.HGHSCRS ()
DECLARE SUB FLASH (R%, G%, B%)
DECLARE SUB STATS (MD%, PTS%)
DECLARE SUB SOUNDFX (TYP%)
DECLARE SUB TELEPORT (MD%)
DECLARE SUB DELAY (MS%)
DECLARE SUB OPTIONS ()
DECLARE SUB D.TITLE ()
DECLARE SUB NEWLEVEL ()
DECLARE SUB ANIMATE ()
DECLARE SUB DSCREEN ()
DECLARE SUB PPUT (SOURCE%(), IMG%, X%, Y%)
DECLARE SUB FPUT (M$, X%, Y%, LC%, UC%)
DECLARE SUB SETSYSCLRS (PAL() AS HUES, SC%, EC%)

DECLARE FUNCTION ENTNAME$ ()

COMMON SHARED PAL() AS HUES, SPR(), FONT(), SCRN(), CLRDAT(), SPPI
COMMON SHARED DX, DY, DP, SND, AI, SPD, R$, L$, U$, D$, K$, AT, LV
COMMON SHARED SC&, DM, BM, BN, MN, RP, STD, GO, DRK, HRL, BR, WL, TR
COMMON SHARED HS() AS HIGHSCORES, EXT

CLS
SCREEN 13
INITIALIZE

REDIM CLRDAT(1 TO 15)
FOR D = 1 TO 15: READ DAT: CLRDAT(D) = DAT: NEXT
STATS 1, 0: FOR I = 2 TO 7: STATS I, 0: NEXT

DO
 DO: D.TITLE: LOOP UNTIL EXT
 SC& = 0: DM = 1: BM = 5: MN = LV: BN = 1500: AB = 1500: GO = 0: RP = 0
 EXT = 0: FOR I = 2 TO 7: STATS I, 0: NEXT
 DO
  RMTEXPLD
  NEWLEVEL
  DSCREEN
  SOUNDFX 1: TELEPORT 0
  WHILE UCASE$(INKEY$) <> "": WEND
  ST = 29: AL = 0: AD = 0: STD = 1: RP = 0
  DO
   ANIMATE
   CHKSCREEN
   IF GO THEN EXIT DO
   K$ = UCASE$(INKEY$)
   IF RP = 5 THEN EXIT DO
   IF K$ = CHR$(27) THEN EXT = 1: EXIT DO
   IF K$ = L$ THEN XS = -14: YS = 0: DS = -1: ST = 2: AL = 2: STD = 0: BD = 1
   IF K$ = R$ THEN XS = 14: YS = 0: DS = 1: ST = 5: AL = 2: STD = 0: BD = 2
   IF K$ = U$ THEN XS = 0: YS = -14: DS = -22: ST = 10: AL = 1: STD = 0: BD = 3
   IF K$ = D$ THEN XS = 0: YS = 14: DS = 22: ST = 8: AL = 1: STD = 0: BD = 4
   IF K$ = CHR$(13) AND BM <> 0 THEN USEBOMB BD: STATS 4, -1
   IF K$ = CHR$(32) THEN STD = 1
   IF K$ = CHR$(0) + ";" THEN NEWLEVEL: RP = 0: DSCREEN: STD = 1
   IF STD THEN
    AL = AL + 1: IF AL > 1 THEN AL = 0
    IF AL = 1 THEN
     PUT (DX, DY), SPR(SCRN(DP) * SPPI), PSET
    ELSE
     PPUT SPR(), 29, DX, DY
    END IF
   END IF
   IF STD <> 1 THEN
    PUT (DX, DY), SPR(SCRN(DP) * SPPI), PSET
    DX = DX + XS: DY = DY + YS
    AD = AD + 1: IF AD > AL THEN AD = 0
    IF DX < 6 THEN DX = 20: XS = 14: YS = 0: DS = 1: ST = 5: AL = 2
    IF DX > 300 THEN DX = 286: XS = -14: YS = 0: DS = -1: ST = 2: AL = 2
    IF DY < 14 THEN DY = 28: XS = 0: YS = 14: DS = 22: ST = 8: AL = 1
    IF DY > 182 THEN DY = 168: XS = 0: YS = -14: DS = -22: ST = 10: AL = 1
    DP = DP + DS: PPUT SPR(), ST + AD, DX, DY
    STATS 6, -10: IF SND THEN SOUND 37, .03
   END IF
   DELAY SPD
  LOOP
  IF GO OR EXT THEN EXIT DO
  FOR I = 148 TO 150
   PUT (293, 0), SPR(I * SPPI), PSET
   FOR T = 1 TO 2: SOUNDFX 8: NEXT
  NEXT
  SOUNDFX 1: TELEPORT 1: DSCREEN
  IF BN THEN
   FOR I = BN TO 0 STEP -500
    SOUNDFX 3
    IF BN < 500 THEN
     STATS 6, -BN: STATS 2, BN
    ELSE
     STATS 6, -500: STATS 2, 500
    END IF
   NEXT
  END IF
  IF BM THEN
   FOR I = BM TO 1 STEP -1
    SOUNDFX 2
    STATS 4, -1: STATS 2, 100
   NEXT
  END IF
  AB = AB + 500: IF AB > 10000 THEN AB = 10000
  BN = AB: BM = 5: STD = 1: RP = 5: DM = DM + 1: DRK = 0
  IF DM > 999 THEN DM = 999
  SLEEP 1
  FOR I = 3 TO 7: STATS I, 0: NEXT
 LOOP
 IF EXT = 0 THEN
  DSCREEN
  FOR I = 1 TO 8
   FOR T = 50 TO 51
    PUT (DX, DY), SPR(T * SPPI), PSET
    SOUNDFX 7: DELAY SPD
   NEXT
  NEXT
  SOUNDFX 6
  FOR I = 52 TO 58
   PUT (DX, DY), SPR(I * SPPI), PSET: DELAY SPD
  NEXT
  DSCREEN
  SLEEP 1
  LINE (0, 14)-(319, 199), 0, BF
  FOR I = 1 TO 10
   IF SC& > HS(I).S THEN
    HS(I).N = ENTNAME$
    HS(I).D = DM: HS(I).S = SC&
    OPEN "HGHSCRS.DAT" FOR OUTPUT AS #1
     FOR I = 1 TO 10
     WRITE #1, HS(I).D, HS(I).S, HS(I).N
    NEXT
    CLOSE #1
    EXIT FOR
   END IF
  NEXT
  D.HGHSCRS
 ELSE
  LINE (0, 14)-(319, 199), 0, BF
 END IF
LOOP

DATA 24,32,40,48,56,80,88,96,112,160,192,208,224,232,240

SUB ANIMATE

 X = 6: Y = 14: R = 1: AT = 0
 AI = AI + 1: IF AI > 3 THEN AI = 0
 FOR T = 1 TO 286
  SELECT CASE SCRN(T)
   CASE 127: PL = 1: AY = 5
   CASE 78 TO 114: PL = 1: AY = 0
   CASE 118 TO 122: PL = 1: AY = 5
  END SELECT
  IF PL THEN
   PL = 0: AI1 = SCRN(T) + AI
   PUT (X, Y + AY), SPR(AI1 * SPPI), PSET
  END IF
  X = X + 14: R = R + 1
  IF R > 22 THEN R = 1: X = 6: Y = Y + 14
 NEXT

END SUB

SUB CHKSCREEN

 SELECT CASE SCRN(DP)
  CASE 1: HITWALL: HRL = 0
  CASE 68 TO 72   'REMOTE
   STATS 7, SCRN(DP) + 5: SCRN(DP) = 0
   RP = RP + 1: SOUNDFX 8
  CASE 78
   STATS 2, 15: SOUNDFX 3: SCRN(DP) = 0
  CASE 82
   STATS 2, 25: SOUNDFX 3: SCRN(DP) = 0
  CASE 86
   STATS 2, 50: SOUNDFX 3: SCRN(DP) = 0
  CASE 90
   STATS 2, 75: SOUNDFX 3: SCRN(DP) = 0
  CASE 94
   STATS 2, 100: SOUNDFX 3: SCRN(DP) = 0
  CASE 98    'BEER
   DRK = DRK + 2: SCRN(DP) = 0: SOUNDFX 5
  CASE 102
   STATS 5, 1: SCRN(DP) = 0: FOR I = 1 TO 5: SOUNDFX 3: NEXT
  CASE 106
   SCRN(DP) = 0: D = 1
   PUT (DX, DY), SPR(SCRN(DP) * SPPI), PSET
   PPUT SPR(), 12, DX, DY: DELAY SPD
   FOR I = 1 TO 2: SOUNDFX 2: FLASH 55, 25, 0: NEXT
   FOR I = 63 TO 66
    X = 6: Y = 14: R = 1
    FOR T = 1 TO 286
     N = SCRN(T)
     IF N = 118 OR N = 122 OR N = 126 OR N = 127 THEN
      PUT (X, Y + 5), SPR(I * SPPI), PSET
      P = P + 50: IF D > 3 THEN SCRN(T) = 0
     END IF
     X = X + 14: R = R + 1
     IF R > 22 THEN R = 1: X = 6: Y = Y + 14
    NEXT
    DELAY SPD \ 4: D = D + 1
   NEXT
   STATS 2, P: DSCREEN: STD = 1
  CASE 110
   SCRN(DP) = 0: D = 1
   PUT (DX, DY), SPR(SCRN(DP) * SPPI), PSET
   PPUT SPR(), 12, DX, DY: DELAY SPD
   FOR I = 1 TO 2: SOUNDFX 2: FLASH 55, 25, 0: NEXT
   FOR I = 63 TO 66
    X = 6: Y = 14: R = 1
    FOR T = 1 TO 286
     N = SCRN(T)
     IF N = 1 THEN
      PUT (X, Y), SPR(I * SPPI), PSET
      P = P + 10: IF D > 3 THEN SCRN(T) = 0
     END IF
     X = X + 14: R = R + 1
     IF R > 22 THEN R = 1: X = 6: Y = Y + 14
    NEXT
    DELAY SPD \ 4: D = D + 1
   NEXT
   STATS 2, P: DSCREEN: STD = 1
  CASE 114
   SCRN(DP) = 0: STATS 2, 1000
   PUT (DX, DY), SPR(SCRN(DP) * SPPI), PSET
   PPUT SPR(), 29, DX, DY: DELAY SPD
   X = 6: Y = 14: R = 1: RP = 5
   FOR T = 1 TO 286
    N = SCRN(T)
    IF N = 68 OR N = 69 OR N = 70 OR N = 71 OR N = 72 THEN
     PUT (X, Y), SPR(I * SPPI), PSET: SCRN(T) = 0
    END IF
    X = X + 14: R = R + 1
    IF R > 22 THEN R = 1: X = 6: Y = Y + 14
   NEXT
  CASE 118
   PUT (DX, DY), SPR(0 * SPPI), PSET
   PUT (DX, DY + 5), SPR(33 * SPPI), PSET
   SOUNDFX 6: SCRN(DP) = 0
   FOR I = 33 TO 38
    PUT (DX, DY + 5), SPR(I * SPPI), PSET: DELAY SPD
   NEXT
   SOUNDFX 2
   FOR I = 63 TO 66
    PUT (DX, DY + 5), SPR(I * SPPI), PSET: DELAY SPD \ 4
   NEXT
   STATS 5, -1: DSCREEN: HRL = 0
   WHILE UCASE$(INKEY$) <> "": WEND
   IF MN < 1 THEN GO = 1:  ELSE STD = 1
  CASE 122: HRLSQRE
  CASE 126: PSRPLTE: HRL = 0
  CASE 127
   PUT (DX, DY), SPR(0), PSET
   DSCREEN
   SOUNDFX 9
   DP = INT(RND * 286) + 1: DSCREEN
   X = 6: Y = 14: R = 1
   FOR T = 1 TO 286
    IF T = DP THEN EXIT FOR
    X = X + 14: R = R + 1
    IF R > 22 THEN R = 1: X = 6: Y = Y + 14
   NEXT
   DX = X: DY = Y: HRL = 0
 END SELECT

END SUB

SUB D.HGHSCRS
 
 LINE (0, 14)-(319, 199), 0, BF

 FPUT "DIMENSION MAN HIGH SCORES", 84, 30, 144, 149
 
 FPUT "NAME          DIMENSION          SCORE", 46, 50, 184, 189
 Y = 60
 FOR I = 1 TO 9
  FPUT "0" + LTRIM$(STR$(I)), 26, Y, 0, 0
  FPUT HS(I).N, 46, Y, 48, 53
  T$ = "000": N$ = LTRIM$(STR$(HS(I).D))
  T$ = LEFT$(T$, 3 - LEN(N$)) + N$
  FPUT T$, 148, Y, 0, 0
  T$ = "0000000": N$ = LTRIM$(STR$(HS(I).S))
  T$ = LEFT$(T$, 7 - LEN(N$)) + N$
  FPUT T$, 238, Y, 0, 0
  Y = Y + 10
 NEXT

 FPUT "10", 26, Y, 0, 0
 FPUT HS(10).N, 46, Y, 48, 53
 T$ = "000": N$ = LTRIM$(STR$(HS(10).D))
 T$ = LEFT$(T$, 3 - LEN(N$)) + N$
 FPUT T$, 148, Y, 0, 0
 T$ = "0000000": N$ = LTRIM$(STR$(HS(10).S))
 T$ = LEFT$(T$, 7 - LEN(N$)) + N$
 FPUT T$, 238, Y, 0, 0

 SLEEP 4
 LINE (0, 14)-(319, 199), 0, BF

END SUB

SUB D.TITLE

 X = 66
 FOR I = 1 TO 9
  PUT (X, 92), SPR(1 * SPPI), PSET: X = X + 14
 NEXT
 X = X + 14
 FOR I = 1 TO 3
  PUT (X, 92), SPR(1 * SPPI), PSET: X = X + 14
 NEXT

 SLEEP 1
 FOR I = 1 TO 2: FLASH 55, 25, 0: NEXT
 
 FOR T = 165 TO 168
  X = 66
  FOR I = 134 TO 146
   PUT (X, 92), SPR(T * SPPI), PSET: X = X + 14
  NEXT
  DELAY 10
 NEXT
 X = 66
 FOR I = 134 TO 146
  PUT (X, 92), SPR(I * SPPI), PSET: X = X + 14
 NEXT
 
 FOR Y = 92 TO 28 STEP -1
  X = 66
  FOR I = 134 TO 146
   PUT (X, Y), SPR(I * SPPI), PSET
   LINE (X, Y + 14)-(X + 14, Y + 14), 0
   X = X + 14
  NEXT
  DELAY 1
 NEXT
 
 FPUT "1995 THE S:H: SOFTWORX", 92, 50, 146, 150

 REDIM SEL$(3)
 SEL$(0) = "PLAY THE GAME"
 SEL$(1) = "HIGH SCORES"
 SEL$(2) = "OPTIONS"
 SEL$(3) = "EXIT TO DOS"
 Y = 96
 FOR I = 0 TO 3
  FPUT SEL$(I), 120, Y, 40, 44
  Y = Y + 10
 NEXT
 FPUT "CURSOR UP AND DOWN SELECTS: PRESS ENTER TO CONFIRM:", 8, 180, 152, 156

 WHILE UCASE$(INKEY$) <> "": WEND

 Y = 96: D = 1: SS = 0
 DO
  K$ = UCASE$(INKEY$)
  IF K$ = U$ AND Y > 96 THEN YS = -10: D = 1: S = -1
  IF K$ = D$ AND Y < 126 THEN YS = 10: D = 1: S = 1
  IF K$ = CHR$(13) THEN
   SELECT CASE SS
    CASE 0
     EXT = 1: EXIT SUB
    CASE 1
     D.HGHSCRS
     EXIT SUB
    CASE 2
     OPTIONS
     LINE (0, 60)-(319, 170), 0, BF
     Y = 96
     FOR I = 0 TO 3
      FPUT SEL$(I), 120, Y, 40, 44
      Y = Y + 10
     NEXT
     Y = 96: SS = 0: S = 0
     FPUT SEL$(SS), 120, Y, 208, 212
    CASE 3: SYSTEM
   END SELECT
  END IF
  IF D THEN
   D = 0: SOUNDFX 3
   FPUT SEL$(SS), 120, Y, 40, 44
   Y = Y + YS: SS = SS + S
   FPUT SEL$(SS), 120, Y, 208, 212
  END IF
 LOOP

END SUB

SUB DELAY (MS%)

 WT& = MS
 FOR T& = 0 TO WT& * 500: NEXT

END SUB

SUB DSCREEN

X = 6: Y = 14: R = 1
FOR T = 1 TO 286
 IMG = SCRN(T)
 IF IMG = 118 OR IMG = 122 OR IMG = 126 OR IMG = 127 THEN
 ELSE
  PUT (X, Y), SPR(IMG * SPPI), PSET
 END IF
 X = X + 14: R = R + 1
 IF R > 22 THEN R = 1: X = 6: Y = Y + 14
NEXT

X = 6: Y = 14: R = 1
FOR T = 1 TO 286
 IMG = SCRN(T)
 IF IMG = 118 OR IMG = 122 OR IMG = 126 OR IMG = 127 THEN
  PUT (X, Y), SPR(0), PSET
  PUT (X, Y + 5), SPR(IMG * SPPI), PSET
 END IF
 X = X + 14: R = R + 1
 IF R > 22 THEN R = 1: X = 6: Y = Y + 14
NEXT

END SUB

FUNCTION ENTNAME$

 WHILE UCASE$(INKEY$) <> "": WEND

 FPUT "YOUR SCORE IS IN THE TOP TEN!!", 66, 30, 152, 156
 A$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ*%.,"
 FPUT A$, 66, 50, 32, 37
 FPUT "?", 66, 57, 0, 0
 FPUT ",          .", 118, 90, 64, 69
 FPUT "USE CURSOR LEFT AND RIGHT TO SELECT CHARACTER", 20, 140, 0, 0
 FPUT "       PRESS SPACE TO ENTER A CHARACTER", 20, 150, 0, 0
 FPUT "            PRESS ENTER WHEN DONE", 20, 160, 0, 0
 N$ = "A" + SPACE$(9)
 AX = 66: L = 1: L1 = 1: D = 1
 DO
  K$ = UCASE$(INKEY$)
  IF K$ = R$ AND L < 30 THEN
   L = L + 1: D = 1
   LINE (AX, 57)-(AX + 7, 57 + 7), 0, BF
   AX = AX + 6: FPUT "?", AX, 57, 0, 0
   IF SND THEN SOUND 100, .4
   MID$(N$, L1, 1) = MID$(A$, L, 1)
  END IF
  IF K$ = L$ AND L > 1 THEN
   L = L - 1: D = 1
   LINE (AX, 57)-(AX + 7, 57 + 7), 0, BF
   AX = AX - 6: FPUT "?", AX, 57, 0, 0
   IF SND THEN SOUND 100, .4
   MID$(N$, L1, 1) = MID$(A$, L, 1)
  END IF
  IF K$ = CHR$(32) THEN
   IF L = 30 THEN
    MID$(N$, L1, 1) = CHR$(32)
    L1 = L1 + 1: D = 1
    IF L1 > 10 THEN L1 = 10
    MID$(N$, L1, 1) = MID$(A$, L, 1)
   ELSEIF L = 29 THEN
    MID$(N$, L1, 1) = CHR$(32)
    L1 = L1 - 1: D = 1
    IF L1 < 1 THEN L1 = 1
    MID$(N$, L1, 1) = MID$(A$, L, 1)
   ELSE
    L1 = L1 + 1: D = 1
    IF L1 > 10 THEN EXIT DO
    MID$(N$, L1, 1) = MID$(A$, L, 1)
   END IF
  END IF
  IF D THEN FPUT N$, 124, 90, 0, 0: D = 0
 LOOP UNTIL K$ = CHR$(13)

 OPEN "HGHSCRS.DAT" FOR OUTPUT AS #1
  FOR I = 1 TO 10
   WRITE #1, HS(I).D, HS(I).S, HS(I).N
  NEXT
 CLOSE #1

 ENTNAME$ = RTRIM$(N$)

END FUNCTION

SUB FLASH (R, G, B)

 R1 = PAL(0).RED: G1 = PAL(0).GRN: B1 = PAL(0).BLU

 PAL(0).RED = R: PAL(0).GRN = G: PAL(0).BLU = B

 SETSYSCLRS PAL(), 0, 0: DELAY SPD \ 4

 PAL(0).RED = R1: PAL(0).GRN = G1: PAL(0).BLU = B1

 SETSYSCLRS PAL(), 0, 0

END SUB

SUB FPUT (M$, X, Y, LC, UC)

 W = FONT(0) \ 8: H = FONT(1)
 FS = (W * H) \ 2 + 2

 X1 = X: Y1 = Y
 FOR I = 1 TO LEN(M$)
  A$ = MID$(M$, I, 1)
  SELECT CASE A$
   CASE "0" TO "9"
    IMG = VAL(A$)
   CASE "A" TO "Z"
    IMG = ASC(A$) - 55
   CASE ".": IMG = 36: CASE ",": IMG = 37
   CASE "!": IMG = 38: CASE "?": IMG = 39
   CASE ":": IMG = 40: CASE "_": IMG = 41
   CASE "*": IMG = 42: CASE "%": IMG = 43
   CASE ELSE
    SP = 1
  END SELECT
   IF SP THEN
    LINE (X1, Y1)-((X1 + W) - 1, (Y1 + H) - 1), 0, BF
   ELSE
    PUT (X1, Y1), FONT(IMG * FS), PSET
   END IF
   X1 = X1 + W: SP = 0
   IF X1 > 319 THEN X1 = X: Y1 = Y1 + H
 NEXT

 IF LC THEN
  FOR Y2 = Y TO Y + H
  FOR X2 = X TO X1
   IF POINT(X2, Y2) = 15 THEN
    PSET (X2, Y2), LC
   END IF
  NEXT: NEXT
 END IF

 IF UC THEN
  FOR Y2 = Y TO Y + H
  FOR X2 = X TO X1
   IF POINT(X2, Y2) = 8 THEN
    PSET (X2, Y2), UC
   END IF
  NEXT: NEXT
 END IF

END SUB

SUB HITWALL

 SOUNDFX 2
 X = DX: Y = DY: R = 1
 FOR H = 1 TO 14: FOR W = 1 TO 14
  SELECT CASE POINT(X, Y)
   CASE 128 TO 138: PSET (X, Y), 0
  END SELECT
  X = X + 1
 NEXT: X = DX: Y = Y + 1: NEXT

 REDIM TMP(100)
 GET (DX, DY)-(DX + 13, DY + 13), TMP

 IF DRK = 0 THEN
  FOR I = 46 TO 49
   PPUT SPR(), I, DX, DY: DELAY SPD \ 2
   PUT (DX, DY), TMP, PSET: DELAY SPD \ 2
  NEXT
 END IF

 SOUNDFX 2
 FOR I = 63 TO 66
  PUT (DX, DY), SPR(I * SPPI), PSET
  DELAY SPD \ 4
 NEXT

 IF DRK THEN
  SCRN(DP) = 0: DRK = DRK - 1
 ELSE
  STATS 5, -1: IF MN < 1 THEN GO = 1
  DELAY SPD: SCRN(DP) = 0: STD = 1:
  WHILE UCASE$(INKEY$) <> "": WEND
 END IF

END SUB

SUB HRLSQRE

 DSCREEN
 SOUNDFX 4
 IF SPD > 10 THEN DL = 10 ELSE DL = 1

 DR = INT(RND * 4) + 1: HRL = 1

 SELECT CASE DR
  CASE 1: X = -14: Y = 0: D = -1
  CASE 2: X = 14: Y = 0: D = 1
  CASE 3: X = 0: Y = -14: D = -22
  CASE 4: X = 0: Y = 14: D = 22
 END SELECT

 DO
  ANIMATE
  IMG = 29 + RI
  DELAY SPD - DL: T = SCRN(DP)
  IF T = 118 OR T = 122 OR T = 126 OR T = 127 THEN
   PUT (DX, DY), SPR(0), PSET
   PUT (DX, DY + 5), SPR(SCRN(DP) * SPPI), PSET
  ELSE
   PUT (DX, DY), SPR(SCRN(DP) * SPPI), PSET
  END IF
  DX = DX + X: DY = DY + Y
  IF DX < 6 THEN DX = 6: EXIT DO
  IF DX > 300 THEN DX = 300: EXIT DO
  IF DY < 14 THEN DY = 14: EXIT DO
  IF DY > 182 THEN DY = 182: EXIT DO
  DP = DP + D: RI = RI + 1: IF RI > 3 THEN RI = 0
  PPUT SPR(), IMG, DX, DY
  CHKSCREEN
 LOOP WHILE HRL

 STD = 1: DSCREEN

END SUB

SUB INITIALIZE

 REDIM PAL(255) AS HUES
 DEF SEG = VARSEG(PAL(0))
 BLOAD "DM_CLR.DAT", 0
 SETSYSCLRS PAL(), 0, 255

 REDIM SPR(16899)
 DEF SEG = VARSEG(SPR(0))
 BLOAD "DM_SPR.001", 0
 SPPI = (SPR(0) \ 8) * (SPR(1)) \ 2 + 2

 REDIM FONT(1011)
 DEF SEG = VARSEG(FONT(0))
 BLOAD "DM_SPR.002", 0

 REDIM HS(10) AS HIGHSCORES
 OPEN "HGHSCRS.DAT" FOR INPUT AS #1
  FOR I = 1 TO 10
   INPUT #1, HS(I).D, HS(I).S, HS(I).N
  NEXT
 CLOSE #1

 SPD = 25: SND = 1
 LV = 5: WL = 90: BR = 3: TR = 10
 R$ = CHR$(0) + "M": L$ = CHR$(0) + "K"
 U$ = CHR$(0) + "H": D$ = CHR$(0) + "P"
 DM = 1: BM = 5: MN = 5: BN = 1500

END SUB

SUB NEWLEVEL

                   'SET WALLS
REDIM SCRN(286)
FOR I = 1 TO WL
 RANDOMIZE TIMER
 N = INT(RND * 286) + 1
 SCRN(N) = 1
NEXT
                   'SET TREASURE
FOR I = 1 TO 5 + INT(RND * 5) + 1
 RANDOMIZE TIMER
 N1 = INT(RND * 49) + 1
 SELECT CASE N1
  CASE IS < 10: IMG = 78
  CASE IS < 20: IMG = 82
  CASE IS < 30: IMG = 86
  CASE IS < 40: IMG = 90
  CASE IS < 50: IMG = 94
  CASE ELSE: IMG = 78
 END SELECT
 N2 = INT(RND * 286) + 1
 SCRN(N2) = IMG
NEXT
                   'SET HELPFUL ITEMS
RANDOMIZE TIMER
N = INT(RND * 14) + 1
IF N = 7 THEN
 DO
  N = INT(RND * 286) + 1
 LOOP UNTIL SCRN(N) = 0
 SCRN(N) = 114
END IF
RANDOMIZE TIMER
N = INT(RND * 8) + 1
IF N = 5 THEN
 DO
  N = INT(RND * 286) + 1
 LOOP UNTIL SCRN(N) = 0
 SCRN(N) = 102
END IF
RANDOMIZE TIMER
N = INT(RND * BR) + 1
FOR I = 1 TO N
 N1 = INT(RND * 286) + 1
 SCRN(N1) = 98
NEXT
RANDOMIZE TIMER
N = INT(RND * 10) + 1
IF N = 1 THEN
 N1 = INT(RND * 286) + 1
 SCRN(N1) = 110
END IF
RANDOMIZE TIMER
N = INT(RND * 10) + 1
IF N = 1 THEN
 N1 = INT(RND * 286) + 1
 SCRN(N1) = 106
END IF
                   'SET HAZZARDS
TRP = TR - 5
FOR I = 1 TO 5 + INT(RND * TRP) + 1
 RANDOMIZE TIMER
 N1 = INT(RND * 45)
 SELECT CASE N1
  CASE IS < 20: IMG = 118
  CASE IS < 30: IMG = 127
  CASE IS < 40: IMG = 122
  CASE IS < 50: IMG = 126
 END SELECT
 N2 = INT(RND * 264)
 SCRN(N2) = IMG: SCRN(N2 + 22) = 0
NEXT
                   'SET REMOTE PIECES
FOR I = 68 TO 72
 DO
  RANDOMIZE TIMER
  P = INT(RND * 286) + 1
 LOOP UNTIL SCRN(P) = 0
 SCRN(P) = I
NEXT
                   'ASSIGN NEW COLORS
RANDOMIZE TIMER
C = INT(RND * 15) + 1
SC = CLRDAT(C)
FOR T = 16 TO 23
  PAL(T).RED = PAL(SC).RED
  PAL(T).GRN = PAL(SC).GRN
  PAL(T).BLU = PAL(SC).BLU
  SC = SC + 1
NEXT
SETSYSCLRS PAL(), 0, 255
                   'SET DMAN X, Y
RANDOMIZE TIMER
N = INT(RND * 286) + 1
SCRN(N) = 0
X = 6: Y = 14: R = 1
FOR T = 1 TO 286
 IF T = N THEN EXIT FOR
 X = X + 14: R = R + 1
 IF R > 22 THEN R = 1: X = 6: Y = Y + 14
NEXT
DX = X: DY = Y: DP = N: AT = 0: AI = 0

END SUB

SUB OPTIONS

 LINE (0, 60)-(319, 170), 0, BF
 
 REDIM O$(6)

 O$(0) = "SOUND FX   "
 O$(1) = "GAME SPEED "
 O$(2) = "WALLS      "
 O$(3) = "TRAPS 5 TO "
 O$(4) = "BEER  1 TO "
 O$(5) = "LIVES      "
 O$(6) = "EXIT OPTIONS"
 Y = 80: REDIM OF$(1)
 OF$(0) = " OFF": OF$(1) = " ON "
 FPUT O$(0) + OF$(SND), 115, 80, 40, 44
 FPUT O$(1) + STR$(SPD), 115, 90, 40, 44
 FPUT O$(2) + STR$(WL), 115, 100, 40, 44
 FPUT O$(3) + STR$(TR), 115, 110, 40, 44
 FPUT O$(4) + STR$(BR), 115, 120, 40, 44
 FPUT O$(5) + STR$(LV), 115, 130, 40, 44
 FPUT O$(6), 115, 140, 40, 44
 SS = 0: D = 1: Y = 80

  DO
   K$ = UCASE$(INKEY$)
   IF K$ = U$ AND Y > 80 THEN YS = -10: D = 1: S = -1
   IF K$ = D$ AND Y < 140 THEN YS = 10: D = 1: S = 1
   IF D THEN
    D = 0: SOUNDFX 3
    FPUT O$(SS), 115, Y, 40, 44
    Y = Y + YS: SS = SS + S
    IF SS = 0 THEN
     FPUT O$(SS) + OF$(SND), 115, Y, 208, 212
    ELSE
     FPUT O$(SS), 115, Y, 208, 212
    END IF
   END IF
   IF K$ = CHR$(13) THEN
    SELECT CASE SS
     CASE 0
      SND = SND + 1: IF SND > 1 THEN SND = 0
      SOUNDFX 3
      FPUT O$(SS) + OF$(SND), 115, Y, 208, 212
     CASE 1
      SPD = SPD + 1: IF SPD > 99 THEN SPD = 1
      FPUT O$(SS) + STR$(SPD) + " ", 115, Y, 208, 212
     CASE 2
      WL = WL + 1: IF WL > 130 THEN WL = 50
      FPUT O$(SS) + STR$(WL) + " ", 115, Y, 208, 212
     CASE 3
      TR = TR + 1: IF TR > 20 THEN TR = 5
      FPUT O$(SS) + STR$(TR) + " ", 115, Y, 208, 212
     CASE 4
      BR = BR + 1: IF BR > 8 THEN BR = 1
      FPUT O$(SS) + STR$(BR), 115, Y, 208, 212
     CASE 5
      LV = LV + 1: IF LV > 15 THEN LV = 1
      FPUT O$(SS) + STR$(LV) + " ", 115, Y, 208, 212
     CASE 6: EXIT DO
    END SELECT
   END IF
  LOOP

 ERASE O$, OF$

END SUB

SUB PPUT (SOURCE(), IMG, X, Y)

 WTH = SOURCE(0) / 8
 HGT = SOURCE(1)
 SIZ& = WTH * HGT / 2 + 2

 REDIM DESTINATION(SIZ&)
 GET (X, Y)-(X + WTH - 1, Y + HGT - 1), DESTINATION

 DPOS& = 4
 SPOS& = IMG * (SIZ& * 2&) + 4&
 DSEG = VARSEG(DESTINATION(0))
 SSEG = VARSEG(SOURCE(0))

 FOR Y1 = 1 TO HGT
  FOR X1 = 1 TO WTH
   DEF SEG = SSEG
   DAT = PEEK(SPOS&)
   IF DAT > 0 THEN
    DEF SEG = DSEG
    POKE DPOS&, DAT
   END IF
   SPOS& = SPOS& + 1: DPOS& = DPOS& + 1
  NEXT
 NEXT
 PUT (X, Y), DESTINATION, PSET

END SUB

SUB PSRPLTE

 RANDOMIZE TIMER
 SOUNDFX 10: SCRN(DP) = 0
 TT = INT(RND * 4) + 1
 IF TT = 4 AND DY = 14 THEN TT = 1

 SELECT CASE TT
  CASE 1
   REDIM TMP(100)
   FOR X = 6 TO DX STEP 14
    GET (X, DY)-(X + 13, DY + 13), TMP
    PPUT SPR(), 132, X, DY: DELAY SPD \ 8
    PUT (X, DY), TMP, PSET
   NEXT
   SOUNDFX 10
   FOR I = 151 TO 155
    PUT (DX, DY), SPR(0), PSET
    PUT (DX, DY + 5), SPR(126 * SPPI), PSET
    PPUT SPR(), I, DX, DY: DELAY SPD \ 2
   NEXT
  CASE 2
   REDIM TMP(100)
   FOR X = 300 TO DX STEP -14
    GET (X, DY)-(X + 13, DY + 13), TMP
    PPUT SPR(), 131, X, DY: DELAY SPD \ 8
    PUT (X, DY), TMP, PSET
   NEXT
   SOUNDFX 10
   FOR I = 156 TO 160
    PUT (DX, DY), SPR(0), PSET
    PUT (DX, DY + 5), SPR(126 * SPPI), PSET
    PPUT SPR(), I, DX, DY: DELAY SPD \ 2
   NEXT
  CASE 3
   SOUNDFX 5
   FOR I = 39 TO 41
    PUT (DX, DY), SPR(0), PSET
    PUT (DX, DY + 5), SPR(126 * SPPI), PSET
    PPUT SPR(), I, DX, DY: DELAY SPD \ 2
   NEXT
   DSCREEN
  CASE 4
   SOUNDFX 10
   PUT (DX, DY), SPR(0), PSET
   PUT (DX, DY + 5), SPR(126 * SPPI), PSET
   PPUT SPR(), 29, DX, DY
   FOR I = 46 TO 49
    PUT (DX, DY - 14), SPR(SCRN(DP - 22) * SPPI), PSET
    PPUT SPR(), 133, DX, DY - 14
    PPUT SPR(), I, DX, DY - 14: DELAY SPD \ 4
   NEXT
   PUT (DX, DY - 14), SPR(SCRN(DP - 22) * SPPI), PSET
   PPUT SPR(), 164, DX, DY - 14: DELAY SPD \ 4
   PUT (DX, DY - 14), SPR(SCRN(DP - 22) * SPPI), PSET
   SOUNDFX 5
   FOR I = 161 TO 164
    PUT (DX, DY), SPR(0), PSET
    PUT (DX, DY + 5), SPR(126 * SPPI), PSET
    PPUT SPR(), I, DX, DY: DELAY SPD
   NEXT
 END SELECT
 
 DELAY SPD: SOUNDFX 2
 FOR I = 63 TO 66
  PUT (DX, DY + 5), SPR(I * SPPI), PSET
  DELAY SPD \ 4
 NEXT
 STATS 5, -1: DSCREEN: STD = 1
 IF MN < 1 THEN GO = 1

END SUB

SUB RMTEXPLD

 REDIM MX(4), MY(4)
 FOR I = 1 TO 4: MX(I) = 152: MY(I) = 92: NEXT

 FOR I = 1 TO 40
  LINE (0, 14)-(319, 199), 0, BF
  X = INT(RND * -2) + 4: Y = INT(RND * -2) + 4
  PUT (152 + X, 92 + Y), SPR(67 * SPPI), PSET
  IF SND THEN SOUND 37 + INT(RND * 150), .4
 NEXT

 LINE (0, 14)-(319, 199), 0, BF
 FOR I = 1 TO 2: SOUNDFX 2: NEXT

 FOR I = 1 TO 40
  N = 1
  FOR T = 73 TO 76
   PUT (MX(N), MY(N)), SPR(T * SPPI), PSET: N = N + 1
  NEXT
  MX(1) = MX(1) - 2: MY(1) = MY(1) - 2
  MX(2) = MX(2) - 2: MY(2) = MY(2) + 2
  MX(3) = MX(3) + 2: MY(3) = MY(3) - 2
  MX(4) = MX(4) + 2: MY(4) = MY(4) + 2
  DELAY (SPD \ 8)
 NEXT

END SUB

SUB SETSYSCLRS (PAL() AS HUES, SC, EC)

 OUT &H3C8, SC

 FOR T = SC TO EC
   OUT &H3C9, PAL(T).RED
   OUT &H3C9, PAL(T).GRN
   OUT &H3C9, PAL(T).BLU
 NEXT

END SUB

SUB SOUNDFX (TYP)

 IF SND THEN
   SELECT CASE TYP
    CASE 1              'TELEPORT
     FOR SD = 8000 TO 1000 STEP -20
      SOUND SD, .03: SOUND SD - 800, .03
     NEXT
    CASE 2              'CRASH
     SOUND 40, .1
     FOR SD = 1 TO 70
      SOUND 37 + INT(RND * 2000), .03
     NEXT
    CASE 3              'GET TREASURE
     FOR SD = 1000 TO 2000 STEP 30
      SOUND SD, .03
     NEXT
    CASE 4              'HURL
     FOR SD = 100 TO 2000 STEP 30
      SOUND SD, .03: SOUND 100, .03
     NEXT
    CASE 5              'SQUISH
     FOR SD = 100 TO 5000 STEP 90
      SOUND SD, .03: SOUND 37, .04
     NEXT
    CASE 6              'FALL
     FOR SD = 2000 TO 100 STEP -30
      SOUND SD, .08
     NEXT
    CASE 7              'FLAP
     FOR SD = 100 TO 37 STEP -5
      SOUND SD, .03: SOUND 10000, .03
     NEXT
    CASE 8              'REMOTE
     FOR SD = 37 TO 10000 STEP 200
      SOUND SD, .03
     NEXT
    CASE 9              'TELEPORT
     FOR SD = 100 TO 5000 STEP 60
      SOUND SD, .06: SOUND 50, .03
     NEXT
     FOR SD = 5000 TO 100 STEP -60
      SOUND SD, .06: SOUND 50, .03
     NEXT
     FOR SD = 100 TO 10000 STEP 80
      SOUND SD, .06: SOUND 50, .03
     NEXT
    CASE 10             'PRESSURE PLATE
     FOR SD = 500 TO 37 STEP -15
      SOUND SD, .03
     NEXT
   END SELECT
    
 END IF

END SUB

SUB STATS (MD, PTS)

 SELECT CASE MD
  CASE 1            'DISPLAY
   FPUT "    SCORE    DIMENSION    BOMB    MEN    BONUS ", 0, 0, 176, 180
  CASE 2            'SCORE
   P& = PTS: SC& = SC& + P&
   IF SC& > 9999999 THEN SC& = 9999999
   T$ = "0000000": T1$ = LTRIM$(STR$(SC&))
   T$ = LEFT$(T$, 7 - LEN(T1$)) + T1$
   FPUT T$, 18, 7, 0, 0
  CASE 3            'DIMENSION
   DM = DM + PTS
   T$ = "000": T1$ = LTRIM$(STR$(DM))
   T$ = LEFT$(T$, 3 - LEN(T1$)) + T1$
   FPUT T$, 96, 7, 0, 0
  CASE 4            'BOMB
   BM = BM + PTS
   T$ = "00": T1$ = LTRIM$(STR$(BM))
   T$ = LEFT$(T$, 2 - LEN(T1$)) + T1$
   FPUT T$, 162, 7, 0, 0
  CASE 5            'MEN
   MN = MN + PTS: IF MN > 100 THEN MN = 100
   T$ = "000": T1$ = LTRIM$(STR$(MN))
   T$ = LEFT$(T$, 3 - LEN(T1$)) + T1$
   FPUT T$, 204, 7, 0, 0
  CASE 6            'BONUS
   BN = BN + PTS: IF BN < 0 THEN BN = 0
   T$ = "00000": T1$ = LTRIM$(STR$(BN))
   T$ = LEFT$(T$, 5 - LEN(T1$)) + T1$
   FPUT T$, 246, 7, 0, 0
  CASE 7            'REMOTE
   IF PTS THEN
    PPUT SPR(), PTS, 293, 0
   ELSE
    PUT (293, 0), SPR(147 * SPPI), PSET
   END IF
 END SELECT

END SUB

SUB TELEPORT (MD)

 IF MD THEN
  S = 28: E = 16: STP = -1
 ELSE
  S = 16: E = 28: STP = 1
 END IF
 
 FOR I = S TO E STEP STP
  DELAY SPD \ 2
  PUT (DX, DY), SPR(I * SPPI), PSET
 NEXT
 
END SUB

SUB USEBOMB (BD)

 PUT (DX, DY), SPR(SCRN(DP) * SPPI), PSET

  FOR I = 1 TO 3
   FOR T = 59 TO 62
    PPUT SPR(), T, DX, DY
    DELAY SPD \ 4
    PUT (DX, DY), SPR(SCRN(DP + D) * SPPI), PSET
    IF SND THEN SOUND 37, .03: SOUND 10000, .04
   NEXT
   ANIMATE
  NEXT
  PUT (DX, DY), SPR(SCRN(DP) * SPPI), PSET
  PPUT SPR(), 12, DX, DY
  SOUNDFX 2
  FOR I = 63 TO 66
   PUT (DX, DY), SPR(I * SPPI), PSET
   DELAY SPD \ 4
  NEXT
  PUT (DX, DY), SPR(SCRN(DP) * SPPI), PSET
  
  FLASH 57, 25, 0: SOUNDFX 2
 
  FOR I = 63 TO 66
   IF DX - 14 >= 6 THEN
    PUT (DX - 14, DY), SPR(I * SPPI), PSET
    IF SCRN(DP - 1) = 1 THEN SCRN(DP - 1) = 0
   END IF
   IF DX + 14 <= 300 THEN
    PUT (DX + 14, DY), SPR(I * SPPI), PSET
    IF SCRN(DP + 1) = 1 THEN SCRN(DP + 1) = 0
   END IF
   IF DY - 14 >= 14 THEN
    PUT (DX, DY - 14), SPR(I * SPPI), PSET
    IF SCRN(DP - 22) = 1 THEN SCRN(DP - 22) = 0
   END IF
   IF DY + 14 <= 182 THEN
    PUT (DX, DY + 14), SPR(I * SPPI), PSET
    IF SCRN(DP + 22) = 1 THEN SCRN(DP + 22) = 0
   END IF
   DELAY SPD \ 4
  NEXT
  STD = 1: DSCREEN

END SUB

