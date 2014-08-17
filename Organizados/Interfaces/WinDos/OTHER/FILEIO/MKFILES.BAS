Directory$ = "c:\mycop\"

  FOR i% = 1 TO 100
  OPEN Directory$ + "file" + LTRIM$(STR$(i%)) FOR BINARY AS #1
  CLOSE
  NEXT

