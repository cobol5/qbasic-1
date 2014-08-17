DECLARE SUB Gtext (XX%, YY%, Text$, FG%, BG%)
DECLARE SUB DrawEgaChar (XX%, YY%, Text$, FG%, BG%)
DEFINT A-Z

SCREEN 12
CLS

  FOR i% = 1 TO 30
  Gtext 100, i% * 10, "1234567890abcdefghijklmnopqrstuvwxyz", 15, 14
  NEXT

SUB DrawEgaChar (XX, YY, Text$, FG, BG)

X = XX
Y = YY
Char$ = Text$
A = ASC(Char$)

DEF SEG = &HF000        'point to rom character set segment
OFS1 = A * 8 + &HFA6D

FOR B = 1 TO 8
  OFS2 = OFS1 + B
  C = PEEK(OFS2)
  IF C = 0 THEN
    X = XX
    Y = Y + 1
  ELSE
    FOR D = 7 TO 0 STEP -1
    E = 2 ^ D
      IF C < E THEN
          IF FG = 1 THEN
          PSET (X, Y), FG
          END IF
        X = X + 1
      ELSE
          IF FG <> 0 THEN
          PSET (X, Y), FG
          END IF
        X = X + 1
        E = 2 ^ D
        C = C - E
      END IF
    NEXT

    Y = Y + 1
    X = XX
  END IF
NEXT

END SUB

SUB Gtext (XX, YY, Text$, FG, BG)

FOR i% = 1 TO LEN(Text$)
X = XX
Y = YY

DEF SEG = &HF000        'point to rom character set segment
OFS1 = &HFA6D + ASC(MID$(Text$, i%, 1)) * 8

FOR B = 1 TO 8
  OFS2 = OFS1 + B
  C = PEEK(OFS2)
  IF C = 0 THEN
    X = XX
    Y = Y + 1
  ELSE
    FOR D = 7 TO 0 STEP -1
    E = 2 ^ D
      IF C < E THEN
          IF FG = 1 THEN
          PSET (X, Y), FG
          END IF
        X = X + 1
      ELSE
          IF FG <> 0 THEN
          PSET (X, Y), FG
          END IF
        X = X + 1
        E = 2 ^ D
        C = C - E
      END IF
    NEXT

    Y = Y + 1
    X = XX
  END IF
NEXT


XX = XX + 8
NEXT


END SUB

