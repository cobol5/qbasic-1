'============================================================================
'J.M.K S.F. BatchBASIC 1.0
'(c) 2000 by Jan-Max Meyer Krapohl, owner of J.M.K S.F.
'Programmiersprachen und -tool entwicklung
'http://home.t-online.de/home/cochisecom/jmksf.htm
'jmk.software.factory@web.de
'============================================================================
'Diese (kleine) Programmiersprache wurde in BASIC programmiert. Sie ist Public
'Domain  und darf verÑndert werden, jedoch mÅssen Sie mir Credits geben,  wenn
'Sie  es unter Ihrem Namen weitergeben wollen. (weiteres siehe Read Me)
'============================================================================
'Besuchen Sie J.M.K S.F. auf den WWW:
'http://home.t-online.de/home/cochisecom/jmksf.htm
'============================================================================
'Auf der Homepage von
'J.M.K S.F.
'Jan-Max Meyer Krapohl, owner of J.M.K S.F.
'Programmiersprachen und -tool entwicklung
'finden Sie:
'- Weitere, selbstentwickelte und -erfundene, eigene Programmiersprachen aus
'  der J.M.K S.F. Produktpalette
'- Selbstentwickelte und -erfundene Programmiertools
'- Jaaa, auch ein paar Spiele und andere Programme !
'============================================================================
filename$ = LTRIM$(RTRIM$(COMMAND$))
CLS
COLOR 15, 0
PRINT "J.M.K S.F. BatchBASIC 1.0"
PRINT "(c) 2000 by Jan-Max Meyer Krapohl, owner of J.M.K S.F."
PRINT "Programmiersprachen und -tool entwicklung"
PRINT "http://home.t-online.de/home/cochisecom/jmksf.htm"
PRINT "jmk.software.factory@web.de"
SLEEP 1
COLOR 4, 0
PRINT
PRINT "Interpreter startet..."
SLEEP 2
COLOR 7, 0
CLS
stringvar$ = ""
OPEN filename$ FOR INPUT AS #1
DO
INPUT #1, bbcode$
bbcode2$ = UCASE$(bbcode$)
IF LEFT$(bbcode2$, 3) = "PRI" THEN GOTO printbe
IF LEFT$(bbcode2$, 3) = "PAU" THEN GOTO pausebe
IF LEFT$(bbcode2$, 3) = "WAI" THEN GOTO waitbe
IF LEFT$(bbcode2$, 3) = "SHE" THEN GOTO shellbe
IF LEFT$(bbcode2$, 3) = "INP" THEN GOTO inputbe
IF LEFT$(bbcode2$, 3) = "CLS" THEN GOTO clearbe

IF LEFT$(bbcode2$, 3) = "END" THEN GOTO nextzeile
IF LEFT$(bbcode2$, 1) = "'" THEN GOTO nextzeile
GOTO shellbe

printbe:
'PRINT Hallo !
text$ = MID$(bbcode$, 7)
IF text$ = "string$" THEN PRINT stringvar$ ELSE PRINT text$
GOTO nextzeile

pausebe:
PRINT "Weiter mit beliebiger Taste..."
DO
LOOP WHILE INKEY$ = ""
GOTO nextzeile

waitbe:
'WAIT=1
laenge$ = MID$(bbcode$, 6)
laenge% = VAL(laenge$)
SLEEP laenge%
GOTO nextzeile

shellbe:
'SHELL dir *.exe
makeonshell$ = MID$(bbcode$, 7)
IF makeonshell$ = "string$" THEN SHELL stringvar$ ELSE SHELL makeonshell$
GOTO nextzeile

inputbe:
'INPUT Bitte was eingeben:
text2$ = MID$(bbcode$, 7)
PRINT text2$;
INPUT "", stringvar$
GOTO nextzeile


clearbe:
CLS
GOTO nextzeile

nextzeile:
LOOP UNTIL bbcode2$ = "END"
CLOSE #1
END


