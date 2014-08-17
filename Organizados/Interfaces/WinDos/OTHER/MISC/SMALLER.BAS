
INPUT "Filename:", FileName$

OPEN FileName$ FOR INPUT AS #1
OPEN "Result.bas" FOR OUTPUT AS #2

  DO UNTIL EOF(1)
  LINE INPUT #1, junk$
    IF LTRIM$(junk$) <> "" AND LEFT$(LTRIM$(junk$), 1) <> "'" THEN
    PRINT #2, junk$
    END IF
  LOOP

CLOSE
END

