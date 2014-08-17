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
filename$ = "NONAME.BBF"
CLS
PRINT "J.M.K S.F. BatchBASIC 1.0 Codeeditor"
PRINT "(c) 2000 by Jan-Max Meyer Krapohl, owner of J.M.K S.F."
PRINT "Programmiersprachen und -tool entwicklung"
PRINT "http://home.t-online.de/home/cochisecom/jmksf.htm"
PRINT "jmk.software.factory@web.de"
PRINT
PRINT "Dateiname: "; filename$
newline:
INPUT "> ", bbcode$
COLOR 1, 0
bbcode2$ = UCASE$(bbcode$)
IF LEFT$(bbcode2$, 6) = "PRINT " THEN LOCATE CSRLIN - 1, 3: PRINT bbcode$
IF LEFT$(bbcode2$, 6) = "INPUT " THEN LOCATE CSRLIN - 1, 3: PRINT bbcode$
IF LEFT$(bbcode2$, 5) = "PAUSE" THEN LOCATE CSRLIN - 1, 3: PRINT bbcode$
IF LEFT$(bbcode2$, 5) = "WAIT=" THEN LOCATE CSRLIN - 1, 3: PRINT bbcode$
IF LEFT$(bbcode2$, 3) = "END" THEN LOCATE CSRLIN - 1, 3: PRINT bbcode$
IF LEFT$(bbcode2$, 6) = "SHELL " THEN LOCATE CSRLIN - 1, 3: PRINT bbcode$
IF LEFT$(bbcode2$, 3) = "CLS" THEN LOCATE CSRLIN - 1, 3: PRINT bbcode$
COLOR 2, 0
IF LEFT$(bbcode2$, 1) = "'" THEN LOCATE CSRLIN - 1, 3: PRINT bbcode$
COLOR 7, 0
IF bbcode2$ = "+M" THEN GOTO menu
IF bbcode2$ = "+R" THEN GOTO runint
IF bbcode2$ = "+E" THEN GOTO exitprog
OPEN filename$ FOR APPEND AS #1
PRINT #1, bbcode$
CLOSE #1
GOTO newline

menu:
PRINT "MenÅ"
PRINT "===="
PRINT "Speichern unter....1"
PRINT "Starten............2"
PRINT "Beenden............3"
DO
taste1$ = INKEY$
LOOP WHILE taste1$ = ""
IF taste1$ = "1" THEN GOTO saveas
IF taste1$ = "2" THEN GOTO runint
IF taste1$ = "3" THEN GOTO exitprog
GOTO newline
saveas:
INPUT "Dateiname: ", newfile$
IF RIGHT$(newfile$, 4) = ".BBF" THEN dran$ = "" ELSE dran$ = ".BBF"
filename$ = newfile$ + dran$
NAME "NONAME.BBF" AS filename$

runint:
SHELL "bb " + filename$
GOTO newline

exitprog:
CLS
END

