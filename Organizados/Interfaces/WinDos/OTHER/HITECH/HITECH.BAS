'Copyright (C) HiTech Labs, Denis Andrianov
'HiTech Labs Demo Program
'============================================================================

DECLARE SUB SetPal (ColNum%, R%, G%, B%)
DECLARE SUB GetPal (ColNum%, R%, G%, B%)

DECLARE SUB Delay (Number%)

DIM FormulaArray(15751) AS INTEGER
DIM PaletteArray(767) AS INTEGER
REDIM PictureArray(32200) AS INTEGER

CONST PictureData = "logo.bin"
CONST PicturePal = "logo.pal"


SCREEN 13
CLS

'----------------------------------------------------------------------------
'Loading picture palette...

Red$ = " "
Green$ = " "
Blue$ = " "

OPEN PicturePal FOR BINARY AS #1

  FOR ColourAtt% = 0 TO 255
  GET #1, , Red$
  GET #1, , Green$
  GET #1, , Blue$
  Red% = ASC(Red$)
  Green% = ASC(Green$)
  Blue% = ASC(Blue$)
  Temp% = ColourAtt% * 3
  PaletteArray(Temp%) = Red%
  PaletteArray(Temp% + 1) = Green%
  PaletteArray(Temp% + 2) = Blue%
  SetPal ColourAtt%, Red%, Green%, Blue%
  NEXT

CLOSE #1


'Loading picture data from file to array...
DEF SEG = VARSEG(PictureArray(0))
BLOAD PictureData, 0

'Initializing variables...
TopPictureArraySegment% = VARSEG(PictureArray(0))
BottomPictureArraySegment% = TopPictureArraySegment% + &H7D0

'----------------------------------------------------------------------------
'Speed test...

OldTime! = TIMER
DEF SEG = &HA000

  FOR i% = 1 TO 3

    FOR y% = 0 TO 100
    YArrayOffset% = y% * 320
      FOR x% = 0 TO 320
        FOR Temp% = 1 TO 4
        POKE YArrayOffset% + x%, 0
        NEXT
      NEXT
    NEXT

  NEXT


'Stupid SMARTDRIVE problem or computer is too fast.
TimeDiff! = TIMER - OldTime!
  IF TimeDiff! > .0001 THEN
    FrameRate% = (3 / TimeDiff!) \ 2
  ELSE
    FrameRate% = 255
  END IF

'PRINT FrameRate%
'SLEEP

  IF FrameRate% < 15 THEN
  GOTO FadingPart
  END IF

'----------------------------------------------------------------------------
'Setting up a "magic" array...

FOR y% = 0 TO 125
  FOR x% = 0 TO 125
    nx! = (x% - 125) / 125
    ny! = (y% - 125) / 125
    nz! = 1 - SQR(nx! ^ 2 + ny! ^ 2)
      IF nz! < 0 THEN nz! = 0
    FormulaArray(x% * 125 + y%) = CINT(nz! * 250)
  NEXT x%
NEXT y%

'----------------------------------------------------------------------------

XLightPosition% = -130
YLightPosition% = 100
Direction% = 3
  

  FOR i% = 1 TO 93

    IF XLightPosition% < 10 THEN
      Direction% = Direction% + 1
    ELSEIF XLightPosition% > 220 THEN
      Direction% = Direction% - 1
    END IF
  XLightPosition% = XLightPosition% + Direction%

    FOR y% = 1 TO 199
    ly% = y% - YLightPosition%
    GOSUB WhichPart

      FOR x% = 1 TO 319
      lx% = x% - XLightPosition%
      FullArrayOffset% = YArrayOffset% + x%

      DEF SEG = PictureArraySegment%
      Temp% = PEEK(FullArrayOffset%) - 125
      ny% = PEEK(FullArrayOffset% + 320) - Temp% - ly%
      nx% = PEEK(FullArrayOffset% + 1) - Temp% - lx%
        IF ny% < 1 OR ny% > 249 THEN ny% = 1
        IF nx% < 0 OR nx% > 250 THEN nx% = 0
      nx% = 125 - ABS(125 - nx%)
      ny% = 125 - ABS(125 - ny%)

      DEF SEG = ScreenSegment%
      POKE FullArrayOffset%, FormulaArray(nx% * 125 + ny%)
      NEXT

    NEXT

    IF FrameRate% > 33 THEN
    Delay 1
    END IF

  NEXT

'----------------------------------------------------------------------------
FadingPart:

'Setting a black palette...
  FOR ColourAtt% = 0 TO 255
  SetPal ColourAtt%, 0, 0, 0
  NEXT

'Loading full picture to screen...
  FOR y% = 1 TO 199
  GOSUB WhichPart
    FOR x% = 1 TO 319
    FullArrayOffset% = YArrayOffset% + x%
    DEF SEG = PictureArraySegment%
    Pixel% = PEEK(FullArrayOffset%)
    DEF SEG = ScreenSegment%
    POKE FullArrayOffset%, Pixel%
    NEXT
  NEXT

'Fading, cool effects...
  FOR i% = 1 TO 3 * 63 + 130
    FOR ColourAtt% = 0 TO 255
    GetPal ColourAtt%, Red%, Green%, Blue%
    Temp% = ColourAtt% * 3
      IF i% <= 63 + 130 THEN
        IF Red% < PaletteArray(Temp%) THEN Red% = Red% + 1
        IF Green% < PaletteArray(Temp% + 1) THEN Green% = Green% + 1
        IF Blue% < PaletteArray(Temp% + 2) THEN Blue% = Blue% + 1
      ELSEIF i% <= 2 * 63 + 130 THEN
        IF Red% < 63 THEN Red% = Red% + 1
        IF Green% < 63 THEN Green% = Green% + 1
        IF Blue% < 63 THEN Blue% = Blue% + 1
      ELSE
        IF Red% > 0 THEN Red% = Red% - 1
        IF Green% > 0 THEN Green% = Green% - 1
        IF Blue% > 0 THEN Blue% = Blue% - 1
      END IF
    SetPal ColourAtt%, Red%, Green%, Blue%
    NEXT
  Delay 1
  NEXT


SCREEN 0
WIDTH 80, 25

END

'===================================SUBS=====================================
WhichPart:

  IF y% < 100 THEN
    YArrayOffset% = y% * 320
    PictureArraySegment% = TopPictureArraySegment%
    ScreenSegment% = &HA000
  ELSE
    YArrayOffset% = (y% - 100) * 320
    PictureArraySegment% = BottomPictureArraySegment%
    ScreenSegment% = &HA7D0
  END IF

RETURN

SUB Delay (Number%)

  FOR i% = 1 TO Number%
  WAIT &H3DA, 8, 8
  WAIT &H3DA, 8
  NEXT

END SUB

SUB GetPal (ColNum%, R%, G%, B%)

OUT &H3C7, ColNum%
R% = INP(&H3C9)
G% = INP(&H3C9)
B% = INP(&H3C9)

END SUB

SUB SetPal (ColNum%, R%, G%, B%)

OUT &H3C8, ColNum%
OUT &H3C9, R%
OUT &H3C9, G%
OUT &H3C9, B%

END SUB

