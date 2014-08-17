REM $INCLUDE: 'RelLib.BI'
DEFINT A-Z

DECLARE SUB RelScore256 (DestSeg%, X%, Y%, Score&, Centered%, FontArray%(), FontArrayIndex%())
DECLARE SUB RelScoreGamma256 (DestSeg%, X%, Y%, Score&, Centered%, FontArray%(), FontArrayIndex%(), Gamma%)
DECLARE SUB RelScoreTrans256 (DestSeg%, X%, Y%, Score&, Centered%, FontArray%(), FontArrayIndex%())
DECLARE SUB RelSc256X (DestSeg%, X%, Y%, Score#, FontArray%(), FontArrayIndex%(), FontPos%, TransSwitch%)
DECLARE SUB RelScGamma256X (DestSeg%, X%, Y%, Score#, FontArray%(), FontArrayIndex%(), FontPos%, Gamma%)
DECLARE SUB RelFontFixedT256 (DestSeg%, X%, Y%, Text$, Centered%, FontArray%(), FontArrayIndex%())
DECLARE SUB RelFontFixed256 (DestSeg%, X%, Y%, Text$, Centered%, FontArray%(), FontArrayIndex%())
DECLARE SUB RelFontGamma256 (DestSeg%, X%, Y%, Text$, Centered%, FontArray%(), FontArrayIndex%(), Gamma%)
DECLARE SUB RelFontGamma256X (DestSeg%, X%, Y%, Text$, Centered%, FontArray%(), FontArrayIndex%(), Gamma%, Spacing!)
DECLARE SUB RelFontTrans256 (DestSeg%, X%, Y%, Text$, Centered%, FontArray%(), FontArrayIndex%())
DECLARE SUB RelFontTrans256X (DestSeg%, X%, Y%, Text$, Centered%, FontArray%(), FontArrayIndex%(), Spacing!)
DECLARE SUB RelFont256 (DestSeg%, X%, Y%, Text$, Centered%, FontArray%(), FontArrayIndex%())
DECLARE SUB RelFont256X (DestSeg%, X%, Y%, Text$, Centered%, FontArray%(), FontArrayIndex%(), Spacing!)
DECLARE SUB RelScGamma256XZ (DestSeg%, X%, Y%, Score#, FontArray%(), FontArrayIndex%(), FontPos%, Gamma%, GammaZeros%, RemoveExcZeros%)
DECLARE SUB RelSc256XZ (DestSeg%, X%, Y%, Score#, FontArray%(), FontArrayIndex%(), FontPos%, TransSwitch%, TransZeros%, RemoveExcZeros%)
DECLARE SUB RelFontGamma256WX (DestSeg%, X%, Y%, Text$, Centered%, FontArray%(), FontArrayIndex%(), Gamma%, TypeAmount%, Spacing!)
DECLARE SUB RelFontGamma256W (DestSeg%, X%, Y%, Text$, Centered%, FontArray%(), FontArrayIndex%(), Gamma%, TypeAmount%)
DECLARE SUB RelFontTrans256WX (DestSeg%, X%, Y%, Text$, Centered%, FontArray%(), FontArrayIndex%(), TypeAmount%, Spacing!)
DECLARE SUB RelFont256WX (DestSeg%, X%, Y%, Text$, Centered%, FontArray%(), FontArrayIndex%(), TypeAmount%, Spacing!)
DECLARE SUB RelFont256W (DestSeg%, X%, Y%, Text$, Centered%, FontArray%(), FontArrayIndex%(), TypeAmount%)

SUB RelFont256 (DestSeg%, X%, Y%, Text$, Centered%, FontArray(), FontArrayIndex()) STATIC
FontSeg% = VARSEG(FontArray(1))
IF NOT Centered% THEN

        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSprite DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X% = X% + (FontArray(FontArrayIndex(FontChar%)) \ 8)
        NEXT I%

ELSE            'Centered

        StrLen% = 0
        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                StrLen% = StrLen% + (FontArray(FontArrayIndex(FontChar%)))
        NEXT I%
        StrLen% = StrLen% \ 8  'Calculate length
        X% = (320 - StrLen%) \ 2
        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSprite DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X% = X% + (FontArray(FontArrayIndex(FontChar%)) \ 8)
        NEXT I%

END IF

END SUB

SUB RelFont256W (DestSeg%, X%, Y%, Text$, Centered%, FontArray(), FontArrayIndex(), TypeAmount) STATIC

'Coding for "RelFont256W" by Adigun Azikiwe Polack.
'(Based on the original "RelFont256" by Richard Eric M. Lope)

FontSeg% = VARSEG(FontArray(1))
TypeText = LEN(Text$) - (LEN(Text$) - TypeAmount)
IF TypeText > LEN(Text$) THEN TypeText = LEN(Text$)
IF NOT Centered% THEN

        FOR I% = 1 TO TypeText
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSprite DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X% = X% + (FontArray(FontArrayIndex(FontChar%)) \ 8)
        NEXT I%

ELSE            'Centered

        StrLen% = 0
        FOR I% = 1 TO TypeText
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                StrLen% = StrLen% + (FontArray(FontArrayIndex(FontChar%)))
        NEXT I%
        StrLen% = StrLen% \ 8  'Calculate length
        X% = (320 - StrLen%) \ 2
        FOR I% = 1 TO TypeText
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSprite DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X% = X% + (FontArray(FontArrayIndex(FontChar%)) \ 8)
        NEXT I%

END IF

END SUB

SUB RelFont256WX (DestSeg%, X%, Y%, Text$, Centered%, FontArray(), FontArrayIndex(), TypeAmount, Spacing!) STATIC

'Coding for "RelFont256WX" by Adigun Azikiwe Polack.
'(Based on the original "RelFont256" by Richard Eric M. Lope)

X! = X%: Y! = Y%

FontSeg% = VARSEG(FontArray(1))
TypeText = LEN(Text$) - (LEN(Text$) - TypeAmount)
IF TypeText > LEN(Text$) THEN TypeText = LEN(Text$)
IF NOT Centered% THEN

        FOR I% = 1 TO TypeText
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSprite DestSeg%, X!, Y!, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X! = X! + ((FontArray(FontArrayIndex(FontChar%)) \ 8) - Spacing!)
        NEXT I%

ELSE            'Centered

        StrLen% = 0
        FOR I% = 1 TO TypeText
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                StrLen% = StrLen% + (FontArray(FontArrayIndex(FontChar%)))
        NEXT I%
        StrLen% = StrLen% \ 8  'Calculate length
        X! = (320 - StrLen%) \ 2 + (Spacing! * 6)
        FOR I% = 1 TO TypeText
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSprite DestSeg%, X!, Y!, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X! = X! + ((FontArray(FontArrayIndex(FontChar%)) \ 8) - Spacing!)
        NEXT I%

END IF


END SUB

SUB RelFont256X (DestSeg%, X%, Y%, Text$, Centered%, FontArray(), FontArrayIndex(), Spacing!) STATIC

'Coding for "RelFont256X" by Adigun Azikiwe Polack.
'(Based on the original "RelFont256" by Richard Eric M. Lope)

X! = X%: Y! = Y%

FontSeg% = VARSEG(FontArray(1))
IF NOT Centered% THEN

        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSprite DestSeg%, X!, Y!, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X! = X! + ((FontArray(FontArrayIndex(FontChar%)) \ 8) - Spacing!)
        NEXT I%

ELSE            'Centered

        StrLen% = 0
        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                StrLen% = StrLen% + (FontArray(FontArrayIndex(FontChar%)))
        NEXT I%
        StrLen% = StrLen% \ 8  'Calculate length
        X! = (320 - StrLen%) \ 2 + (Spacing! * 6)
        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSprite DestSeg%, X!, Y!, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X! = X! + ((FontArray(FontArrayIndex(FontChar%)) \ 8) - Spacing!)
        NEXT I%

END IF


END SUB

SUB RelFontFixed256 (DestSeg%, X%, Y%, Text$, Centered%, FontArray(), FontArrayIndex()) STATIC
FontSeg% = VARSEG(FontArray(1))
Spacing% = 0   'Calculate fixed spacing
Spacing% = FontArray(FontArrayIndex(1)) \ 8
IF NOT Centered% THEN

        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSprite DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X% = X% + Spacing%
        NEXT I%

ELSE            'Centered
        StrLen% = 0
        StrLen% = Spacing% * LEN(Text$)
        X% = (320 - StrLen%) \ 2
        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSprite DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X% = X% + Spacing%
        NEXT I%

END IF

END SUB

SUB RelFontFixedT256 (DestSeg%, X%, Y%, Text$, Centered%, FontArray(), FontArrayIndex()) STATIC

FontSeg% = VARSEG(FontArray(1))
Spacing% = 0   'Calculate fixed spacing
Spacing% = FontArray(FontArrayIndex(1)) \ 8
IF NOT Centered% THEN

        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteTrans DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X% = X% + Spacing%
        NEXT I%

ELSE            'Centered
        StrLen% = 0
        StrLen% = Spacing% * LEN(Text$)
        X% = (320 - StrLen%) \ 2
        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteTrans DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X% = X% + Spacing%
        NEXT I%

END IF


END SUB

SUB RelFontGamma256 (DestSeg%, X%, Y%, Text$, Centered%, FontArray(), FontArrayIndex(), Gamma%) STATIC

'Coding for "RelFontGamma256" by Richard Eric M. Lope and
'                                             Adigun Azikiwe Polack.

FontSeg% = VARSEG(FontArray(1))
IF NOT Centered% THEN

        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteGamma DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar))), Gamma%
                X% = X% + (FontArray(FontArrayIndex(FontChar%)) \ 8)
        NEXT I%

ELSE            'Centered

        StrLen% = 0
        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                StrLen% = StrLen% + (FontArray(FontArrayIndex(FontChar%)))
        NEXT I%
        StrLen% = StrLen% \ 8  'Calculate length
        X% = (320 - StrLen%) \ 2
        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteGamma DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar))), Gamma%
                X% = X% + (FontArray(FontArrayIndex(FontChar%)) \ 8)
        NEXT I%

END IF

END SUB

SUB RelFontGamma256W (DestSeg%, X%, Y%, Text$, Centered%, FontArray(), FontArrayIndex(), Gamma%, TypeAmount) STATIC

'Coding for "RelFontGamma256W" by Adigun Azikiwe Polack.
'(Based on the original "RelFontGamma256" by the very same coder himself.)

FontSeg% = VARSEG(FontArray(1))
TypeText = LEN(Text$) - (LEN(Text$) - TypeAmount)
IF TypeText > LEN(Text$) THEN TypeText = LEN(Text$)
IF NOT Centered% THEN

        FOR I% = 1 TO TypeText
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteGamma DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar))), Gamma%
                X% = X% + (FontArray(FontArrayIndex(FontChar%)) \ 8)
        NEXT I%

ELSE            'Centered

        StrLen% = 0
        FOR I% = 1 TO TypeText
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                StrLen% = StrLen% + (FontArray(FontArrayIndex(FontChar%)))
        NEXT I%
        StrLen% = StrLen% \ 8  'Calculate length
        X% = (320 - StrLen%) \ 2
        FOR I% = 1 TO TypeText
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteGamma DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar))), Gamma%
                X% = X% + (FontArray(FontArrayIndex(FontChar%)) \ 8)
        NEXT I%

END IF

END SUB

SUB RelFontGamma256WX (DestSeg%, X%, Y%, Text$, Centered%, FontArray(), FontArrayIndex(), Gamma%, TypeAmount, Spacing!) STATIC

'Coding for "RelFontGamma256WX" by Adigun Azikiwe Polack.
'(Based on the original "RelFontGamma256" by the very same coder himself.)

X! = X%: Y! = Y%

FontSeg% = VARSEG(FontArray(1))
TypeText = LEN(Text$) - (LEN(Text$) - TypeAmount)
IF TypeText > LEN(Text$) THEN TypeText = LEN(Text$)
IF NOT Centered% THEN

        FOR I% = 1 TO TypeText
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteGamma DestSeg%, X!, Y!, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar))), Gamma%
                X! = X! + ((FontArray(FontArrayIndex(FontChar%)) \ 8) - Spacing!)
        NEXT I%

ELSE            'Centered

        StrLen% = 0
        FOR I% = 1 TO TypeText
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                StrLen% = StrLen% + (FontArray(FontArrayIndex(FontChar%)))
        NEXT I%
        StrLen% = StrLen% \ 8  'Calculate length
        X! = (320 - StrLen%) \ 2 + (Spacing! * 6)
        FOR I% = 1 TO TypeText
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteGamma DestSeg%, X!, Y!, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar))), Gamma%
                X! = X! + ((FontArray(FontArrayIndex(FontChar%)) \ 8) - Spacing!)
        NEXT I%

END IF


END SUB

SUB RelFontGamma256X (DestSeg%, X%, Y%, Text$, Centered%, FontArray(), FontArrayIndex(), Gamma%, Spacing!) STATIC

'Coding for "RelFontGamma256X" by Richard Eric M. Lope and
'                                             Adigun Azikiwe Polack.

X! = X%: Y! = Y%

FontSeg% = VARSEG(FontArray(1))
IF NOT Centered% THEN

        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteGamma DestSeg%, X!, Y!, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar))), Gamma%
                X! = X! + ((FontArray(FontArrayIndex(FontChar%)) \ 8) - Spacing!)
        NEXT I%

ELSE            'Centered

        StrLen% = 0
        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                StrLen% = StrLen% + (FontArray(FontArrayIndex(FontChar%)))
        NEXT I%
        StrLen% = StrLen% \ 8  'Calculate length
        X! = (320 - StrLen%) \ 2 + (Spacing! * 6)
        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteGamma DestSeg%, X!, Y!, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar))), Gamma%
                X! = X! + ((FontArray(FontArrayIndex(FontChar%)) \ 8) - Spacing!)
        NEXT I%

END IF


END SUB

SUB RelFontTrans256 (DestSeg%, X%, Y%, Text$, Centered%, FontArray(), FontArrayIndex()) STATIC
FontSeg% = VARSEG(FontArray(1))
IF NOT Centered% THEN

        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteTrans DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X% = X% + (FontArray(FontArrayIndex(FontChar%)) \ 8)
        NEXT I%

ELSE            'Centered

        StrLen% = 0
        FOR I% = 1 TO LEN(Text$)   'get font length
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                StrLen% = StrLen% + (FontArray(FontArrayIndex(FontChar%)))
        NEXT I%
        StrLen% = StrLen% \ 8  'Calculate length
        X% = (320 - StrLen%) \ 2
        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteTrans DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X% = X% + (FontArray(FontArrayIndex(FontChar%)) \ 8)
        NEXT I%

END IF

END SUB

SUB RelFontTrans256W (DestSeg%, X%, Y%, Text$, Centered%, FontArray(), FontArrayIndex(), TypeAmount) STATIC

'Coding for "RelFontTrans256W" by Adigun Azikiwe Polack.
'(Based on the original "RelFontTrans256" by Richard Eric M. Lope)

FontSeg% = VARSEG(FontArray(1))
TypeText = LEN(Text$) - (LEN(Text$) - TypeAmount)
IF TypeText > LEN(Text$) THEN TypeText = LEN(Text$)
IF NOT Centered% THEN

        FOR I% = 1 TO TypeText
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteTrans DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X% = X% + (FontArray(FontArrayIndex(FontChar%)) \ 8)
        NEXT I%

ELSE            'Centered

        StrLen% = 0
        FOR I% = 1 TO TypeText
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                StrLen% = StrLen% + (FontArray(FontArrayIndex(FontChar%)))
        NEXT I%
        StrLen% = StrLen% \ 8  'Calculate length
        X% = (320 - StrLen%) \ 2
        FOR I% = 1 TO TypeText
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteTrans DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X% = X% + (FontArray(FontArrayIndex(FontChar%)) \ 8)
        NEXT I%

END IF

END SUB

SUB RelFontTrans256WX (DestSeg%, X%, Y%, Text$, Centered%, FontArray(), FontArrayIndex(), TypeAmount, Spacing!) STATIC

'Coding for "RelFontTrans256WX" by Adigun Azikiwe Polack.
'(Based on the original "RelFontTrans256" by Richard Eric M. Lope)

X! = X%: Y! = Y%

FontSeg% = VARSEG(FontArray(1))
TypeText = LEN(Text$) - (LEN(Text$) - TypeAmount)
IF TypeText > LEN(Text$) THEN TypeText = LEN(Text$)
IF NOT Centered% THEN

        FOR I% = 1 TO TypeText
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteTrans DestSeg%, X!, Y!, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X! = X! + ((FontArray(FontArrayIndex(FontChar%)) \ 8) - Spacing!)
        NEXT I%

ELSE            'Centered

        StrLen% = 0
        FOR I% = 1 TO TypeText
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                StrLen% = StrLen% + (FontArray(FontArrayIndex(FontChar%)))
        NEXT I%
        StrLen% = StrLen% \ 8  'Calculate length
        X! = (320 - StrLen%) \ 2 + (Spacing! * 6)
        FOR I% = 1 TO TypeText
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteTrans DestSeg%, X!, Y!, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X! = X! + ((FontArray(FontArrayIndex(FontChar%)) \ 8) - Spacing!)
        NEXT I%

END IF


END SUB

SUB RelFontTrans256X (DestSeg%, X%, Y%, Text$, Centered%, FontArray(), FontArrayIndex(), Spacing!) STATIC

'Coding for "RelFontTrans256X" by Adigun Azikiwe Polack.
'(Based on the original "RelFontTrans256" by Richard Eric M. Lope)

X! = X%: Y! = Y%

FontSeg% = VARSEG(FontArray(1))
IF NOT Centered% THEN

        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteTrans DestSeg%, X!, Y!, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X! = X! + ((FontArray(FontArrayIndex(FontChar%)) \ 8) - Spacing!)
        NEXT I%

ELSE            'Centered

        StrLen% = 0
        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                StrLen% = StrLen% + (FontArray(FontArrayIndex(FontChar%)))
        NEXT I%
        StrLen% = StrLen% \ 8  'Calculate length
        X! = (320 - StrLen%) \ 2 + (Spacing! * 6)
        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteTrans DestSeg%, X!, Y!, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X! = X! + ((FontArray(FontArrayIndex(FontChar%)) \ 8) - Spacing!)
        NEXT I%

END IF

END SUB

SUB RelSc256X (DestSeg%, X%, Y%, Score#, FontArray%(), FontArrayIndex%(), FontPos%, TransSwitch%) STATIC

'Coding for "RelSc256X" by Adigun Azikiwe Polack.

' 10 digits in the score                         - Justification: RIGHT -

IF Score# < 0# THEN Score# = 0#
IF Score# > 9999999999# THEN Score# = 9999999999#
Scotab# = Score#

IF Scotab# >= 0 AND Scotab# <= 9.9999# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X% + (FontPos% * 9), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X% + (FontPos% * 9), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10 AND Scotab# <= 99.9999# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X% + (FontPos% * 8), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X% + (FontPos% * 8), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100 AND Scotab# <= 999.9999# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X% + (FontPos% * 7), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X% + (FontPos% * 7), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000 AND Scotab# <= 9999.999900000001# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X% + (FontPos% * 6), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X% + (FontPos% * 6), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000 AND Scotab# <= 99999.9999# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X% + (FontPos% * 5), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X% + (FontPos% * 5), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000 AND Scotab# <= 999999.9999000001# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X% + (FontPos% * 4), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X% + (FontPos% * 4), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000 AND Scotab# <= 9999999.9999# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X% + (FontPos% * 3), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X% + (FontPos% * 3), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000000 AND Scotab# <= 99999999.9999# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X% + (FontPos% * 2), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X% + (FontPos% * 2), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000000 AND Scotab# <= 999999999.9999# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X% + FontPos%, Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X% + FontPos%, Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000000 AND Scotab# <= 9999999999.99# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF


END SUB

SUB RelSc256XZ (DestSeg%, X%, Y%, Score#, FontArray%(), FontArrayIndex%(), FontPos%, TransSwitch%, TransZeros%, RemoveExcZeros%) STATIC

'Coding for "RelSc256XZ" by Adigun Azikiwe Polack.

' 10 digits in the score                         - Justification: RIGHT -

IF RemoveExcZeros% <= 0 THEN RemoveExcZeros% = 0
IF RemoveExcZeros% >= 10 THEN RemoveExcZeros% = 10
IF Score# < 0# THEN Score# = 0#
IF Score# > 9999999999# THEN Score# = 9999999999#
Scotab# = Score#

IF Scotab# >= 0 AND Scotab# <= 9.9999# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X% + (FontPos% * 9), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X% + (FontPos% * 9), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Zeros = 9
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10 AND Scotab# <= 99.9999# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X% + (FontPos% * 8), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X% + (FontPos% * 8), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Zeros = 8
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100 AND Scotab# <= 999.9999# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X% + (FontPos% * 7), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X% + (FontPos% * 7), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Scotab# = Scotab# - Scotab#
  Zeros = 7
END IF

IF Scotab# >= 1000 AND Scotab# <= 9999.999900000001# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X% + (FontPos% * 6), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X% + (FontPos% * 6), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Zeros = 6
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000 AND Scotab# <= 99999.9999# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X% + (FontPos% * 5), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X% + (FontPos% * 5), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Zeros = 5
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000 AND Scotab# <= 999999.9999000001# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X% + (FontPos% * 4), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X% + (FontPos% * 4), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Zeros = 4
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000 AND Scotab# <= 9999999.9999# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X% + (FontPos% * 3), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X% + (FontPos% * 3), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Zeros = 3
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000000 AND Scotab# <= 99999999.9999# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X% + (FontPos% * 2), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X% + (FontPos% * 2), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Zeros = 2
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000000 AND Scotab# <= 999999999.9999# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X% + FontPos%, Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X% + FontPos%, Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Zeros = 1
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000000 AND Scotab# <= 9999999999.99# THEN
  SELECT CASE TransSwitch%
    CASE FALSE: RelFont256 DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
    CASE TRUE: RelFontTrans256 DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%()
  END SELECT
  Zeros = 0
  Scotab# = Scotab# - Scotab#
END IF

FOR PosZero% = 0 + (FontPos% * RemoveExcZeros%) TO ((FontPos% * Zeros) - FontPos%) STEP FontPos%
  SELECT CASE TransZeros%
    CASE FALSE:
      RelFont256 DestSeg%, X% + PosZero%, Y%, "0", 0, FontArray%(), FontArrayIndex%()
    CASE TRUE:
      RelFontTrans256 DestSeg%, X% + PosZero%, Y%, "0", 0, FontArray%(), FontArrayIndex%()
  END SELECT
NEXT

END SUB

SUB RelScGamma256X (DestSeg%, X%, Y%, Score#, FontArray%(), FontArrayIndex%(), FontPos%, Gamma%) STATIC

'Coding for "RelScGamma256X" by Adigun Azikiwe Polack.

' 10 digits in the score                         - Justification: RIGHT -

IF Score# < 0# THEN Score# = 0#
IF Score# > 9999999999# THEN Score# = 9999999999#
Scotab# = Score#

IF Scotab# >= 0 AND Scotab# <= 9.9999# THEN
  RelFontGamma256 DestSeg%, X% + (FontPos% * 9), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10 AND Scotab# <= 99.9999# THEN
  RelFontGamma256 DestSeg%, X% + (FontPos% * 8), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100 AND Scotab# <= 999.9999# THEN
  RelFontGamma256 DestSeg%, X% + (FontPos% * 7), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000 AND Scotab# <= 9999.999900000001# THEN
  RelFontGamma256 DestSeg%, X% + (FontPos% * 6), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000 AND Scotab# <= 99999.9999# THEN
  RelFontGamma256 DestSeg%, X% + (FontPos% * 5), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000 AND Scotab# <= 999999.9999000001# THEN
  RelFontGamma256 DestSeg%, X% + (FontPos% * 4), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000 AND Scotab# <= 9999999.9999# THEN
  RelFontGamma256 DestSeg%, X% + (FontPos% * 3), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000000 AND Scotab# <= 99999999.9999# THEN
  RelFontGamma256 DestSeg%, X% + (FontPos% * 2), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000000 AND Scotab# <= 999999999.9999# THEN
  RelFontGamma256 DestSeg%, X% + FontPos%, Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000000 AND Scotab# <= 9999999999.99# THEN
  RelFontGamma256 DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Scotab# = Scotab# - Scotab#
END IF



END SUB

SUB RelScGamma256XZ (DestSeg%, X%, Y%, Score#, FontArray%(), FontArrayIndex%(), FontPos%, Gamma%, GammaZeros%, RemoveExcZeros%) STATIC

'Coding for "RelScGamma256XZ" by Adigun Azikiwe Polack.

' 10 digits in the score                         - Justification: RIGHT -

IF RemoveExcZeros% <= 0 THEN RemoveExcZeros% = 0
IF RemoveExcZeros% >= 10 THEN RemoveExcZeros% = 10
IF Score# < 0# THEN Score# = 0#
IF Score# > 9999999999# THEN Score# = 9999999999#
Scotab# = Score#

IF Scotab# >= 0 AND Scotab# <= 9.9999# THEN
  RelFontGamma256 DestSeg%, X% + (FontPos% * 9), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Zeros = 9
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10 AND Scotab# <= 99.9999# THEN
  RelFontGamma256 DestSeg%, X% + (FontPos% * 8), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Zeros = 8
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100 AND Scotab# <= 999.9999# THEN
  RelFontGamma256 DestSeg%, X% + (FontPos% * 7), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Zeros = 7
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000 AND Scotab# <= 9999.999900000001# THEN
  RelFontGamma256 DestSeg%, X% + (FontPos% * 6), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Zeros = 6
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000 AND Scotab# <= 99999.9999# THEN
  RelFontGamma256 DestSeg%, X% + (FontPos% * 5), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Zeros = 5
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000 AND Scotab# <= 999999.9999000001# THEN
  RelFontGamma256 DestSeg%, X% + (FontPos% * 4), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Zeros = 4
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000 AND Scotab# <= 9999999.9999# THEN
  RelFontGamma256 DestSeg%, X% + (FontPos% * 3), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Zeros = 3
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000000 AND Scotab# <= 99999999.9999# THEN
  RelFontGamma256 DestSeg%, X% + (FontPos% * 2), Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Zeros = 2
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000000 AND Scotab# <= 999999999.9999# THEN
  RelFontGamma256 DestSeg%, X% + FontPos%, Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Zeros = 1
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000000 AND Scotab# <= 9999999999.99# THEN
  RelFontGamma256 DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), 0, FontArray%(), FontArrayIndex%(), Gamma%
  Zeros = 0
  Scotab# = Scotab# - Scotab#
END IF


FOR PosZero% = 0 + (FontPos% * RemoveExcZeros%) TO ((FontPos% * Zeros) - FontPos%) STEP FontPos%
  RelFontGamma256 DestSeg%, X% + PosZero%, Y%, "0", 0, FontArray%(), FontArrayIndex%(), GammaZeros%
NEXT

END SUB

SUB RelScore256 (DestSeg%, X%, Y%, Score&, Centered%, FontArray%(), FontArrayIndex%()) STATIC
FontSeg% = VARSEG(FontArray(1))
Spacing% = 0   'Calculate fixed spacing
Spacing% = FontArray(FontArrayIndex(1)) \ 8

IF Score& >= 1999999999 THEN Score& = 1999999999

'===Add Commas
Text$ = RTRIM$(LTRIM$(STR$(Score&)))

L = LEN(Text$)
Temp$ = ""
II = 0

FOR I = L TO 1 STEP -1
        I$ = MID$(Text$, I, 1)

        IF II = 3 THEN
                Temp$ = Temp$ + "," + I$
        ELSE
                Temp$ = Temp$ + I$
        END IF
        II = (II MOD 3) + 1

NEXT I

'Reverse it

L = LEN(Temp$)
Temp2$ = ""

FOR I = L TO 1 STEP -1
        I$ = MID$(Temp$, I, 1)
        Temp2$ = Temp2$ + I$
NEXT I

Text$ = LTRIM$(RTRIM$(Temp2$))

'Print it
IF NOT Centered% THEN

        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSprite DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X% = X% + Spacing%
        NEXT I%

ELSE            'Centered
        StrLen% = 0
        StrLen% = Spacing% * LEN(Text$)
        X% = (320 - StrLen%) \ 2
        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSprite DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X% = X% + Spacing%
        NEXT I%

END IF

END SUB

SUB RelScoreGamma256 (DestSeg%, X%, Y%, Score&, Centered%, FontArray%(), FontArrayIndex%(), Gamma%) STATIC
FontSeg% = VARSEG(FontArray(1))
Spacing% = 0   'Calculate fixed spacing
Spacing% = FontArray(FontArrayIndex(1)) \ 8

'Coding for "RelScoreGamma256X" by Richard Eric M. Lope and
'                                             Adigun Azikiwe Polack.

IF Score& >= 1999999999 THEN Score& = 1999999999

'===Add Commas
Text$ = RTRIM$(LTRIM$(STR$(Score&)))

L = LEN(Text$)
Temp$ = ""
II = 0

FOR I = L TO 1 STEP -1
        I$ = MID$(Text$, I, 1)

        IF II = 3 THEN
                Temp$ = Temp$ + "," + I$
        ELSE
                Temp$ = Temp$ + I$
        END IF
        II = (II MOD 3) + 1

NEXT I

'Reverse it

L = LEN(Temp$)
Temp2$ = ""

FOR I = L TO 1 STEP -1
        I$ = MID$(Temp$, I, 1)
        Temp2$ = Temp2$ + I$
NEXT I

Text$ = LTRIM$(RTRIM$(Temp2$))

'Print it
IF NOT Centered% THEN

        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteGamma DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar))), Gamma%
                X% = X% + Spacing%
        NEXT I%

ELSE            'Centered
        StrLen% = 0
        StrLen% = Spacing% * LEN(Text$)
        X% = (320 - StrLen%) \ 2
        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteGamma DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar))), Gamma%
                X% = X% + Spacing%
        NEXT I%

END IF



END SUB

SUB RelScoreTrans256 (DestSeg%, X%, Y%, Score&, Centered%, FontArray%(), FontArrayIndex%()) STATIC
FontSeg% = VARSEG(FontArray(1))
Spacing% = 0   'Calculate fixed spacing
Spacing% = FontArray(FontArrayIndex(1)) \ 8

IF Score& >= 1999999999 THEN Score& = 1999999999

'===Add Commas
Text$ = RTRIM$(LTRIM$(STR$(Score&)))

L = LEN(Text$)
Temp$ = ""
II = 0

FOR I = L TO 1 STEP -1
        I$ = MID$(Text$, I, 1)

        IF II = 3 THEN
                Temp$ = Temp$ + "," + I$
        ELSE
                Temp$ = Temp$ + I$
        END IF
        II = (II MOD 3) + 1

NEXT I

'Reverse it

L = LEN(Temp$)
Temp2$ = ""

FOR I = L TO 1 STEP -1
        I$ = MID$(Temp$, I, 1)
        Temp2$ = Temp2$ + I$
NEXT I

Text$ = LTRIM$(RTRIM$(Temp2$))

'Print it
IF NOT Centered% THEN

        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteTrans DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X% = X% + Spacing%
        NEXT I%

ELSE            'Centered
        StrLen% = 0
        StrLen% = Spacing% * LEN(Text$)
        X% = (320 - StrLen%) \ 2
        FOR I% = 1 TO LEN(Text$)
                FontChar% = ASC(MID$(Text$, I%, 1)) - 31
                RelSpriteTrans DestSeg%, X%, Y%, FontSeg%, VARPTR(FontArray(FontArrayIndex(FontChar)))
                X% = X% + Spacing%
        NEXT I%

END IF


END SUB

