CONST MenuMax = 7

  FOR i% = 1 TO MenuMax
  Filename$ = "pullmenu." + LTRIM$(STR$(i%))
  OPEN Filename$ + "t" FOR INPUT AS #1
  OPEN Filename$ FOR BINARY AS #2

  INPUT #1, Temp$
  ItemNum% = VAL(Temp$)
  Temp$ = CHR$(ItemNum%)
  PUT #2, , Temp$

    FOR k% = 0 TO ItemNum%
    INPUT #1, Temp$
    Temp$ = LTRIM$(RTRIM$(Temp$))
    Length% = LEN(Temp$)
    Length$ = CHR$(Length%)
    PUT #2, , Length$
    PUT #2, , Temp$
    NEXT

  CLOSE

  NEXT

