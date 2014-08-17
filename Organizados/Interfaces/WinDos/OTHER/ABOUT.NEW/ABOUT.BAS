DECLARE FUNCTION RGB2Colour% (R%, G%, B%)
DECLARE SUB GetPal (BYVAL Attrib%, R%, G%, B%)
DECLARE SUB SetPal (BYVAL Colour%, BYVAL Red%, BYVAL Green%, BYVAL Blue%)
DECLARE SUB LoadFont (Text$, BYVAL TextX%, BYVAL TextY%, ColourX%, ColourY%, BYVAL StepColourX%, BYVAL StepColourY%)

SCREEN 13
CLS


'*************************************
'Initialize the palette.
'*************************************

Red$ = " "
Green$ = Red$
Blue$ = Red$

OPEN "about.pal" FOR BINARY AS #1

SEEK #1, 9&
GET #1, , TextLen%
SEEK #1, 9 + 2 + TextLen%

  FOR Colour% = 0 TO 255
  GET #1, , Red$
  GET #1, , Green$
  GET #1, , Blue$
  SetPal Colour%, ASC(Red$), ASC(Green$), ASC(Blue$)
  NEXT

CLOSE #1



  FOR i% = 0 TO 200
  LINE (0, i%)-(319, i%), i%
  NEXT

'LoadFont "THIS IS A LONG TEST LINE", 0, 50, 20, 0, 1, 0
'LoadFont "THIS IS A LONG TEST LINE", 0, 100, 0, 40, 0, 5








'************************************
'Rotate the palette.
'************************************

  DO

  GetPal 1, OldR%, OldG%, OldB%

    FOR i% = 2 TO 255
    GetPal i%, R%, G%, B%
    SetPal i% - 1, R%, G%, B%
    NEXT

  SetPal 255, OldR%, OldG%, OldB%

    FOR i% = 1 TO 2
    WAIT &H3DA, 8, 8
    WAIT &H3DA, 8
    NEXT

  LOOP UNTIL LEN(INKEY$)

SUB GetPal (BYVAL Attrib%, R%, G%, B%)

OUT &H3C7, Attrib%
R% = INP(&H3C9)
G% = INP(&H3C9)
B% = INP(&H3C9)

END SUB

SUB LoadFont (Text$, BYVAL TextX%, BYVAL TextY%, ColourX%, ColourY%, BYVAL StepColourX%, BYVAL StepColourY%)


ColourYSaved% = ColourY%
DEF SEG = &HF000


  FOR LetterNum% = 1 TO LEN(Text$)
  LetterOffset% = &HFA6E + ASC(MID$(Text$, LetterNum%, 1)) * 8
  xPosition% = TextX% + (LetterNum% - 1) * 8
  yPosition% = TextY%
  ColourX% = ColourX% + 8 * StepColourX%
  ColourY% = ColourYSaved%


    FOR LineNum% = 0 TO 7
    ScanLine% = PEEK(LetterOffset% + LineNum%)
    ColourY% = ColourY% + StepColourY%

    TempColour% = ColourX% + ColourY%
      
      IF ScanLine% AND 128 THEN
      PSET (xPosition%, yPosition%), TempColour%
      END IF

      IF ScanLine% AND 64 THEN
      PSET (xPosition% + 1, yPosition%), TempColour% + StepColourX%
      END IF

      IF ScanLine% AND 32 THEN
      PSET (xPosition% + 2, yPosition%), TempColour% + 2 * StepColourX%
      END IF

      IF ScanLine% AND 16 THEN
      PSET (xPosition% + 3, yPosition%), TempColour% + 3 * StepColourX%
      END IF

      IF ScanLine% AND 8 THEN
      PSET (xPosition% + 4, yPosition%), TempColour% + 4 * StepColourX%
      END IF

      IF ScanLine% AND 4 THEN
      PSET (xPosition% + 5, yPosition%), TempColour% + 5 * StepColourX%
      END IF

      IF ScanLine% AND 2 THEN
      PSET (xPosition% + 6, yPosition%), TempColour% + 6 * StepColourX%
      END IF

      IF ScanLine% AND 1 THEN
      PSET (xPosition% + 7, yPosition%), TempColour% + 7 * StepColourX%
      END IF

    yPosition% = yPosition% + 1
    NEXT

  NEXT



DEF SEG


END SUB

SUB MoveText (BYVAL Fast%, BYVAL LoopNum%, Text$)

Text$ = Text$ + SPACE$(40)


  FOR Position% = 1 TO LoopNum%
  LOCATE 1, 1
  COLOR 16
  Symbol$ = MID$(Text$, Position%, 1)
  PRINT Symbol$


    FOR xCounter% = 0 TO 7

      IF INKEY$ = CHR$(0) + CHR$(1) THEN
      Fast% = -1
      END IF

      FOR yCounter% = 0 TO 7
        IF POINT(xCounter%, yCounter%) THEN
          IF Fast% THEN
            PSET (310 + xCounter%, 187 + yCounter%), 14
            PSET (311 + xCounter%, 186 + yCounter%), 0
          ELSE
            PSET (317, 187 + yCounter%), 14
            PSET (318, 186 + yCounter%), 0
          END IF
        END IF
      NEXT

      IF Fast% = 0 THEN
'      GET (2, 185)-(319, 194), Scroll
'      PUT (1, 185), Scroll, PSET
'      Delay 1
      END IF

    NEXT


    IF Fast% THEN
'    GET (9, 185)-(319, 194), Scroll
'    PUT (1, 185), Scroll, PSET
    LINE (310, 185)-(319, 194), 8, BF
'    Delay 1
    END IF

  NEXT



END SUB

FUNCTION RGB2Colour% (R%, G%, B%)

SmallestDif% = 3 * 63



  FOR Colour% = 0 TO 255
  GetPal Colour%, CurR%, CurG%, CurB%

  DifR% = INT(R% - CurR%)
  DifG% = INT(G% - CurG%)
  DifB% = INT(B% - CurB%)
  Dif% = DifR% + DifG% + DifB%

    IF Dif% THEN
        IF Dif% < SmallestDif% THEN
        SmallestDif% = Dif%
        ClosestCol% = Colour%
        END IF
    ELSE
      RGB2Color% = Colour%
      EXIT FUNCTION
    END IF

  NEXT



RGB2Color% = ClosestCol%

END FUNCTION

DEFINT A-Z
SUB SetPal (BYVAL Colour%, BYVAL Red%, BYVAL Green%, BYVAL Blue%)

OUT &H3C8, Colour%
OUT &H3C9, Red%
OUT &H3C9, Green%
OUT &H3C9, Blue%

END SUB

