directory$ = "d:"

  FOR k% = 1 TO 4
    FOR i% = 0 TO 9
    directory$ = directory$ + "\" + LTRIM$(STR$(i%))
    MKDIR directory$
    NEXT
  NEXT

