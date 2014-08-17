Text$ = "000000000000000000000000000000000000000000000123456789abcdefghijklmnopqrstuvwxyz"
x% = 0
y% = 20
Colour% = 14

SCREEN 12
CLS
'LINE (0, 0)-(639, 479), 2, BF


DEF SEG = &HF000


  FOR LetterNum% = 1 TO LEN(Text$)
  LetterOffset% = &HFA6E + ASC(MID$(Text$, LetterNum%, 1)) * 8
  xPosition% = x% + (LetterNum% - 1) * 8
  yPosition% = y%

    FOR LineNum% = 0 TO 7
    ScanLine% = PEEK(LetterOffset% + LineNum%)

      IF ScanLine% AND 128 THEN
      PSET (xPosition%, yPosition%), Colour%
      END IF

      IF ScanLine% AND 64 THEN
      PSET (xPosition% + 1, yPosition%), Colour%
      END IF

      IF ScanLine% AND 32 THEN
      PSET (xPosition% + 2, yPosition%), Colour%
      END IF

      IF ScanLine% AND 16 THEN
      PSET (xPosition% + 3, yPosition%), Colour%
      END IF

      IF ScanLine% AND 8 THEN
      PSET (xPosition% + 4, yPosition%), Colour%
      END IF

      IF ScanLine% AND 4 THEN
      PSET (xPosition% + 5, yPosition%), Colour%
      END IF

      IF ScanLine% AND 2 THEN
      PSET (xPosition% + 6, yPosition%), Colour%
      END IF

      IF ScanLine% AND 1 THEN
      PSET (xPosition% + 7, yPosition%), Colour%
      END IF
    
    yPosition% = yPosition% + 1
    NEXT

  NEXT


DEF SEG

