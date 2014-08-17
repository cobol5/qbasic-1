DECLARE FUNCTION ExtractDir$ (Path$)


ListFile$ = LTRIM$(RTRIM$(COMMAND$))

OPEN ListFile$ FOR INPUT AS #1

  DO UNTIL EOF(1)
  Index% = Index% + 1
  LINE INPUT #1, Path$
  
  Dot% = INSTR(Path$, ".")

    IF Dot% THEN
      Extension$ = MID$(Path$, Dot%)
    ELSE
      Extension$ = ""
    END IF

  NewFile$ = LTRIM$(STR$(Index%))

    IF LEN(NewFile$) = 1 THEN
    NewFile$ = "0" + NewFile$
    END IF

  NAME Path$ AS ExtractDir$(Path$) + NewFile$ + Extension$
  LOOP

CLOSE #1

'Function returns the directory of the file specified by its full path.
'The function adds a slash on the end of returned directory.
'If only a filename is contained in Path$ a null string is returned.
'
FUNCTION ExtractDir$ (Path$)

Slash$ = "\"

  IF INSTR(Path$, Slash$) = 0 THEN
  EXIT FUNCTION
  END IF

LenPath% = LEN(Path$)

  FOR Counter% = LenPath% TO 1 STEP -1
    IF MID$(Path$, Counter%, 1) = Slash$ THEN
    EXIT FOR
    END IF
  NEXT

ExtractDir$ = LEFT$(Path$, Counter%)

END FUNCTION

