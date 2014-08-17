'$INCLUDE: 'rellib.bi'

DECLARE SUB RelScanlinesH (Layer%, x1%, y1%, x2%, y2%, col%, LineLoc%, Impose%)
DECLARE SUB RelScanlinesV (Layer%, x1%, y1%, x2%, y2%, col%, LineLoc%, Impose%)

SUB RelScanlinesH (Layer%, x1%, y1%, x2, y2%, col%, LineLoc%, Impose%)

SELECT CASE LineType
  CASE 0:
  FOR LinetY% = y1% TO y2% STEP 2
    SELECT CASE Impose%
    CASE 1 TO 3:
      FOR TrLine% = 0 TO Impose%
        RelLineTrans Layer%, x1%, LinetY%, x2, LinetY%, col%
      NEXT
    CASE IS > 3
      RelLine Layer%, x1%, LinetY%, x2, LinetY%, col%
    END SELECT
  NEXT
  CASE 1:
  FOR LinetY% = y1% + 1 TO y2% STEP 2
    SELECT CASE Impose%
    CASE 1 TO 3:
      FOR TrLine% = 0 TO Impose%
        RelLineTrans Layer%, x1%, LinetY%, x2, LinetY%, col%
      NEXT
    CASE IS > 3
      RelLine Layer%, x1%, LinetY%, x2, LinetY%, col%
    END SELECT
  NEXT
END SELECT

END SUB

SUB RelScanlinesV (Layer%, x1%, y1%, x2, y2%, col%, LineLoc%, Impose%)

SELECT CASE LineType
  CASE 0:
  FOR LinetX% = x1% TO x2% STEP 2
    SELECT CASE Impose%
    CASE 1 TO 3:
      FOR TrLine% = 0 TO Impose%
        RelLineTrans Layer%, LinetX%, y1%, LinetX%, y2%, col%
      NEXT
    CASE IS > 3
      RelLine Layer%, LinetX%, y1%, LinetX%, y2%, col%
    END SELECT
  NEXT
  CASE 1:
  FOR LinetX% = x1% + 1 TO x2% STEP 2
    SELECT CASE Impose%
    CASE 1 TO 3:
      FOR TrLine% = 0 TO Impose%
        RelLineTrans Layer%, LinetX%, y1%, LinetX%, y2%, col%
      NEXT
    CASE IS > 3
      RelLine Layer%, LinetX%, y1%, LinetX%, y2%, col%
    END SELECT
  NEXT
END SELECT

END SUB

