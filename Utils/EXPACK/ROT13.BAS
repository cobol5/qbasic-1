'===========================================================================
'You can make .SPC files with this for SpecialEdition Info!!
'===========================================================================
DECLARE SUB ROT13 (test$)
Test$ = "PICSA"

PRINT "Original - "; test$
CALL ROT13(test$)
PRINT "ROT-13   - "; test$

SUB ROT13 (test$)
  FOR tt = 1 TO LEN(test$)
    y% = 0
    x% = ASC(MID$(test$, tt, 1))
    IF (x% > 64 AND x% < 91) OR (x% > 96 AND x% < 123) THEN
      y% = 13
      x% = x% - y%
      IF x% < 97 AND x% > 83 THEN x% = x% + 26 ELSE IF x% < 65 THEN x% = x% + 26
    END IF
    MID$(test$, tt, 1) = CHR$(x%)
  NEXT tt
END SUB

