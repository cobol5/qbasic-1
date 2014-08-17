DEFINT A-Z
REM $INCLUDE: 'RelLib.BI'

DECLARE SUB RelPrintES (DestSeg%, X%, Y%, Font$, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%)
DECLARE SUB RelPrintESB (DestSeg%, X%, Y%, Font$, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%)
DECLARE SUB RelPrintESC (DestSeg%, X%, Y%, Font$, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%)
DECLARE SUB RelPrintScoreESL (DestSeg%, Score#, X%, Y%, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%, Shade%)
DECLARE SUB RelPrintScoreESR (DestSeg%, Score#, X%, Y%, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%, Shade%)
DECLARE SUB RelPrintScoreSL (DestSeg%, Score#, X%, Y%, Mincolor%, Xscale%, Yscale%, Italic%, Shade%)
DECLARE SUB RelPrintScoreSR (DestSeg%, Score#, X%, Y%, Mincolor%, Xscale%, Yscale%, Italic%, Shade%)
DECLARE SUB RelPrintTrScoreSL (DestSeg%, Score#, X%, Y%, Mincolor%, Xscale%, Yscale%, Italic%, Shade%)
DECLARE SUB RelPrintTrScoreSR (DestSeg%, Score#, X%, Y%, Mincolor%, Xscale%, Yscale%, Italic%, Shade%)
DECLARE SUB RelPrintTrScoreL (DestSeg%, Score#, X%, Y%, Mincolor%, Italic%, Shade%)
DECLARE SUB RelPrintTrScoreR (DestSeg%, Score#, X%, Y%, Mincolor%, Italic%, Shade%)

SUB RelPrintES (DestSeg%, X%, Y%, Font$, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%) STATIC

'Coding for "RelPrintES" by Adigun Azikiwe Polack.

IF ShadTrans THEN
  RelPrintTransS DestSeg%, X% + 1, Y% + 1, Font$, Shadcol%, Xscale%, Yscale%, Italic%
  RelPrintTransS DestSeg%, X%, Y% + 1, Font$, Shadcol%, Xscale%, Yscale%, Italic%
  RelPrintTransS DestSeg%, X% + 1, Y%, Font$, Shadcol%, Xscale%, Yscale%, Italic%
ELSE
  RelPrintS DestSeg%, X% + 1, Y% + 1, Font$, Shadcol%, Xscale%, Yscale%, Italic%
  RelPrintS DestSeg%, X%, Y% + 1, Font$, Shadcol%, Xscale%, Yscale%, Italic%
  RelPrintS DestSeg%, X% + 1, Y%, Font$, Shadcol%, Xscale%, Yscale%, Italic%
END IF
IF Trans THEN
  RelPrintTransS DestSeg%, X, Y, Font$, Col%, Xscale%, Yscale%, Italic%
ELSE
  RelPrintS DestSeg%, X, Y, Font$, Col%, Xscale%, Yscale%, Italic%
END IF

END SUB

SUB RelPrintESB (DestSeg%, X%, Y%, Font$, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%) STATIC

'Coding for "RelPrintESB" by Adigun Azikiwe Polack.

IF ShadTrans THEN
  RelPrintTransSB DestSeg%, X% + 1, Y% + 1, Font$, Shadcol%, Xscale%, Yscale%, Italic%
  RelPrintTransSB DestSeg%, X%, Y% + 1, Font$, Shadcol%, Xscale%, Yscale%, Italic%
  RelPrintTransSB DestSeg%, X% + 1, Y%, Font$, Shadcol%, Xscale%, Yscale%, Italic%
ELSE
  RelPrintSB DestSeg%, X% + 1, Y% + 1, Font$, Shadcol%, Xscale%, Yscale%, Italic%
  RelPrintSB DestSeg%, X%, Y% + 1, Font$, Shadcol%, Xscale%, Yscale%, Italic%
  RelPrintSB DestSeg%, X% + 1, Y%, Font$, Shadcol%, Xscale%, Yscale%, Italic%
END IF
IF Trans THEN
  RelPrintTransSB DestSeg%, X, Y, Font$, Col%, Xscale%, Yscale%, Italic%
ELSE
  RelPrintSB DestSeg%, X, Y, Font$, Col%, Xscale%, Yscale%, Italic%
END IF

END SUB

SUB RelPrintESC (DestSeg%, X%, Y%, Font$, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%) STATIC

'Coding for "RelPrintESC" by Adigun Azikiwe Polack.

IF ShadTrans THEN
  RelPrintTransSC DestSeg%, X% + 1, Y% + 1, Font$, Shadcol%, Xscale%, Yscale%, Italic%
  RelPrintTransSC DestSeg%, X%, Y% + 1, Font$, Shadcol%, Xscale%, Yscale%, Italic%
  RelPrintTransSC DestSeg%, X% + 1, Y%, Font$, Shadcol%, Xscale%, Yscale%, Italic%
ELSE
  RelPrintSC DestSeg%, X% + 1, Y% + 1, Font$, Shadcol%, Xscale%, Yscale%, Italic%
  RelPrintSC DestSeg%, X%, Y% + 1, Font$, Shadcol%, Xscale%, Yscale%, Italic%
  RelPrintSC DestSeg%, X% + 1, Y%, Font$, Shadcol%, Xscale%, Yscale%, Italic%
END IF
IF Trans THEN
  RelPrintTransSC DestSeg%, X, Y, Font$, Col%, Xscale%, Yscale%, Italic%
ELSE
  RelPrintSC DestSeg%, X, Y, Font$, Col%, Xscale%, Yscale%, Italic%
END IF

END SUB

SUB RelPrintScoreESL (DestSeg%, Score#, X%, Y%, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%, Shade%) STATIC

'Coding for "RelPrintScoreESL" by Adigun Azikiwe Polack.

IF ShadTrans THEN
  RelPrintTrScoreSL DestSeg%, Score#, X% + 1, Y% + 1, Shadcol%, Xscale%, Yscale%, Italic%, Shade%
  RelPrintTrScoreSL DestSeg%, Score#, X%, Y% + 1, Shadcol%, Xscale%, Yscale%, Italic%, Shade%
  RelPrintTrScoreSL DestSeg%, Score#, X% + 1, Y%, Shadcol%, Xscale%, Yscale%, Italic%, Shade%
ELSE
  RelPrintScoreSL DestSeg%, Score#, X% + 1, Y% + 1, Shadcol%, Xscale%, Yscale%, Italic%, Shade%
  RelPrintScoreSL DestSeg%, Score#, X%, Y% + 1, Shadcol%, Xscale%, Yscale%, Italic%, Shade%
  RelPrintScoreSL DestSeg%, Score#, X% + 1, Y%, Shadcol%, Xscale%, Yscale%, Italic%, Shade%
END IF
IF Trans THEN
  RelPrintTrScoreSL DestSeg%, Score#, X%, Y%, Col%, Xscale%, Yscale%, Italic%, Shade%
ELSE
  RelPrintScoreSL DestSeg%, Score#, X%, Y%, Col%, Xscale%, Yscale%, Italic%, Shade%
END IF

END SUB

SUB RelPrintScoreESR (DestSeg%, Score#, X%, Y%, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%, Shade%) STATIC

'Coding for "RelPrintScoreESR" by Adigun Azikiwe Polack.

' 10 digits in the score                         - Justification: RIGHT -

IF Score# < 0# THEN Score# = 0#
IF Score# > 9999999999# THEN Score# = 9999999999#
Scotab# = Score#

IF Shade% <= 0 THEN Shade% = 0
IF Shade% >= 2 THEN Shade% = 2

IF Scotab# >= 0 AND Scotab# <= 9.9999# THEN
  RelPrintScoreESL DestSeg%, Score#, (X% + 72) * Xscale%, Y%, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%, Shade%
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10 AND Scotab# <= 99.9999# THEN
  RelPrintScoreESL DestSeg%, Score#, (X% + 64) * Xscale%, Y%, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%, Shade%
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100 AND Scotab# <= 999.9999# THEN
  RelPrintScoreESL DestSeg%, Score#, (X% + 56) * Xscale%, Y%, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%, Shade%
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000 AND Scotab# <= 9999.999900000001# THEN
  RelPrintScoreESL DestSeg%, Score#, (X% + 48) * Xscale%, Y%, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%, Shade%
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000 AND Scotab# <= 99999.9999# THEN
  RelPrintScoreESL DestSeg%, Score#, (X% + 40) * Xscale%, Y%, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%, Shade%
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000 AND Scotab# <= 999999.9999000001# THEN
  RelPrintScoreESL DestSeg%, Score#, (X% + 32) * Xscale%, Y%, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%, Shade%
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000 AND Scotab# <= 9999999.9999# THEN
  RelPrintScoreESL DestSeg%, Score#, (X% + 24) * Xscale%, Y%, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%, Shade%
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000000 AND Scotab# <= 99999999.9999# THEN
  RelPrintScoreESL DestSeg%, Score#, (X% + 16) * Xscale%, Y%, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%, Shade%
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000000 AND Scotab# <= 999999999.9999# THEN
  RelPrintScoreESL DestSeg%, Score#, (X% + 8) * Xscale%, Y%, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%, Shade%
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000000 AND Scotab# <= 9999999999.99# THEN
  RelPrintScoreESL DestSeg%, Score#, X% * Xscale%, Y%, Col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%, Shade%
  Scotab# = Scotab# - Scotab#
END IF

END SUB

SUB RelPrintScoreSL (DestSeg%, Score#, X%, Y%, Mincolor%, Xscale%, Yscale%, Italic%, Shade%)

'Coding for "RelPrintScoreSL" by Adigun Azikiwe Polack.

' 10 digits in the score                         - Justification: LEFT -

IF Shade% <= 0 THEN Shade% = 0
IF Shade% >= 2 THEN Shade% = 2

IF Score# < 0# THEN Score# = 0#
IF Score# > 9999999999# THEN Score# = 9999999999#

SELECT CASE Shade%
  CASE 0: RelPrintSC DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintS DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintSB DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
END SELECT
END SUB

SUB RelPrintScoreSR (DestSeg%, Score#, X%, Y%, Mincolor%, Xscale%, Yscale%, Italic%, Shade%)

'Coding for "RelPrintScoreSR" by Adigun Azikiwe Polack.

' 10 digits in the score                         - Justification: RIGHT -

IF Score# < 0# THEN Score# = 0#
IF Score# > 9999999999# THEN Score# = 9999999999#
Scotab# = Score#

IF Shade% <= 0 THEN Shade% = 0
IF Shade% >= 2 THEN Shade% = 2

IF Scotab# >= 0 AND Scotab# <= 9.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintSC DestSeg%, (X% + 72) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintS DestSeg%, (X% + 72) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintSB DestSeg%, (X% + 72) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10 AND Scotab# <= 99.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintSC DestSeg%, (X% + 64) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintS DestSeg%, (X% + 64) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintSB DestSeg%, (X% + 64) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100 AND Scotab# <= 999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintSC DestSeg%, (X% + 56) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintS DestSeg%, (X% + 56) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintSB DestSeg%, (X% + 56) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000 AND Scotab# <= 9999.999900000001# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintSC DestSeg%, (X% + 48) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintS DestSeg%, (X% + 48) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintSB DestSeg%, (X% + 48) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000 AND Scotab# <= 99999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintSC DestSeg%, (X% + 40) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintS DestSeg%, (X% + 40) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintSB DestSeg%, (X% + 40) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000 AND Scotab# <= 999999.9999000001# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintSC DestSeg%, (X% + 32) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintS DestSeg%, (X% + 32) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintSB DestSeg%, (X% + 32) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000 AND Scotab# <= 9999999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintSC DestSeg%, (X% + 24) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintS DestSeg%, (X% + 24) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintSB DestSeg%, (X% + 24) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000000 AND Scotab# <= 99999999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintSC DestSeg%, (X% + 16) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintS DestSeg%, (X% + 16) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintSB DestSeg%, (X% + 16) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000000 AND Scotab# <= 999999999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintSC DestSeg%, (X% + 8) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintS DestSeg%, (X% + 8) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintSB DestSeg%, (X% + 8) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000000 AND Scotab# <= 9999999999.99# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintSC DestSeg%, X% * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintS DestSeg%, X% * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintSB DestSeg%, X% * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

END SUB

SUB RelPrintTrScoreL (DestSeg%, Score#, X%, Y%, Mincolor%, Italic%, Shade%) STATIC

'Coding for "RelPrintTrScoreL" by Adigun Azikiwe Polack.

' 10 digits in the score                         - Justification: LEFT -

IF Shade% <= 0 THEN Shade% = 0
IF Shade% >= 2 THEN Shade% = 2

IF Score# < 0# THEN Score# = 0#
IF Score# > 9999999999# THEN Score# = 9999999999#

SELECT CASE Shade%
  CASE 0: RelPrintTransC DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrintTrans DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintTransB DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
END SELECT
END SUB

SUB RelPrintTrScoreR (DestSeg%, Score#, X%, Y%, Mincolor%, Italic%, Shade%) STATIC

'Coding for "RelPrintTrScoreR" by Adigun Azikiwe Polack.

' 10 digits in the score                         - Justification: RIGHT -

IF Score# < 0# THEN Score# = 0#
IF Score# > 9999999999# THEN Score# = 9999999999#
Scotab# = Score#

IF Shade% <= 0 THEN Shade% = 0
IF Shade% >= 2 THEN Shade% = 2

IF Scotab# >= 0 AND Scotab# <= 9.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransC DestSeg%, X% + 72, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrintTrans DestSeg%, X% + 72, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintTransB DestSeg%, X% + 72, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10 AND Scotab# <= 99.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransC DestSeg%, X% + 64, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrintTrans DestSeg%, X% + 64, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintTransB DestSeg%, X% + 64, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100 AND Scotab# <= 999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransC DestSeg%, X% + 56, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrintTrans DestSeg%, X% + 56, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintTransB DestSeg%, X% + 56, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000 AND Scotab# <= 9999.999900000001# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransC DestSeg%, X% + 48, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrintTrans DestSeg%, X% + 48, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintTransB DestSeg%, X% + 48, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000 AND Scotab# <= 99999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransC DestSeg%, X% + 40, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrintTrans DestSeg%, X% + 40, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintTransB DestSeg%, X% + 40, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000 AND Scotab# <= 999999.9999000001# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransC DestSeg%, X% + 32, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrintTrans DestSeg%, X% + 32, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintTransB DestSeg%, X% + 32, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000 AND Scotab# <= 9999999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransC DestSeg%, X% + 24, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrintTrans DestSeg%, X% + 24, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintTransB DestSeg%, X% + 24, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000000 AND Scotab# <= 99999999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransC DestSeg%, X% + 16, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrintTrans DestSeg%, X% + 16, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintTransB DestSeg%, X% + 16, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000000 AND Scotab# <= 999999999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransC DestSeg%, X% + 8, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrintTrans DestSeg%, X% + 8, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintTransB DestSeg%, X% + 8, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000000 AND Scotab# <= 9999999999.99# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransC DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 1: RelPrintTrans DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  CASE 2: RelPrintTransB DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

END SUB

SUB RelPrintTrScoreSL (DestSeg%, Score#, X%, Y%, Mincolor%, Xscale%, Yscale%, Italic%, Shade%)

'Coding for "RelPrintTrScoreSL" by Adigun Azikiwe Polack.

' 10 digits in the score                         - Justification: LEFT -

IF Shade% <= 0 THEN Shade% = 0
IF Shade% >= 2 THEN Shade% = 2

IF Score# < 0# THEN Score# = 0#
IF Score# > 9999999999# THEN Score# = 9999999999#

SELECT CASE Shade%
  CASE 0: RelPrintTransSC DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintTransS DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintTransSB DestSeg%, X%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
END SELECT
END SUB

SUB RelPrintTrScoreSR (DestSeg%, Score#, X%, Y%, Mincolor%, Xscale%, Yscale%, Italic%, Shade%)

'Coding for "RelPrintTrScoreSR" by Adigun Azikiwe Polack.

' 10 digits in the score                         - Justification: RIGHT -

IF Score# < 0# THEN Score# = 0#
IF Score# > 9999999999# THEN Score# = 9999999999#
Scotab# = Score#

IF Shade% <= 0 THEN Shade% = 0
IF Shade% >= 2 THEN Shade% = 2

IF Scotab# >= 0 AND Scotab# <= 9.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransSC DestSeg%, (X% + 72) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintTransS DestSeg%, (X% + 72) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintTransSB DestSeg%, (X% + 72) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10 AND Scotab# <= 99.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransSC DestSeg%, (X% + 64) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintTransS DestSeg%, (X% + 64) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintTransSB DestSeg%, (X% + 64) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100 AND Scotab# <= 999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransSC DestSeg%, (X% + 56) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintTransS DestSeg%, (X% + 56) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintTransSB DestSeg%, (X% + 56) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000 AND Scotab# <= 9999.999900000001# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransSC DestSeg%, (X% + 48) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintTransS DestSeg%, (X% + 48) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintTransSB DestSeg%, (X% + 48) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000 AND Scotab# <= 99999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransSC DestSeg%, (X% + 40) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintTransS DestSeg%, (X% + 40) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintTransSB DestSeg%, (X% + 40) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000 AND Scotab# <= 999999.9999000001# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransSC DestSeg%, (X% + 32) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintTransS DestSeg%, (X% + 32) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintTransSB DestSeg%, (X% + 32) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000 AND Scotab# <= 9999999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransSC DestSeg%, (X% + 24) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintTransS DestSeg%, (X% + 24) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintTransSB DestSeg%, (X% + 24) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 10000000 AND Scotab# <= 99999999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransSC DestSeg%, (X% + 16) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintTransS DestSeg%, (X% + 16) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintTransSB DestSeg%, (X% + 16) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 100000000 AND Scotab# <= 999999999.9999# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransSC DestSeg%, (X% + 8) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintTransS DestSeg%, (X% + 8) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintTransSB DestSeg%, (X% + 8) * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

IF Scotab# >= 1000000000 AND Scotab# <= 9999999999.99# THEN
  SELECT CASE Shade%
  CASE 0: RelPrintTransSC DestSeg%, X% * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 1: RelPrintTransS DestSeg%, X% * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  CASE 2: RelPrintTransSB DestSeg%, X% * Xscale%, Y%, LTRIM$(STR$(Score#)), Mincolor%, Xscale%, Yscale%, Italic%
  END SELECT
  Scotab# = Scotab# - Scotab#
END IF

END SUB

