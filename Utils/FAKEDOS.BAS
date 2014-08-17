COLOR 15
CLS
PRINT "Press any key to continue. Where's the any key?"
DO WHILE INKEY$ = ""
LOOP
CLS
PRINT "To quit, type EXIT"
PRINT "Press a key to clear the screen and continue"
DO WHILE INKEY$ = ""
LOOP
CLS
begin:
LINE INPUT "C:\>", c$
SELECT CASE UCASE$(c$)
CASE "CLS"
CLS
GOTO begin:
CASE "EXIT"
GOTO quit:
CASE "TYPE AUTOEXEC.BAT"
PRINT "WIN"
GOTO begin:
CASE ELSE
PRINT "Bad command or file name"
GOTO begin:
END SELECT
quit:
PRINT "Press any key to continue."
DO UNTIL INKEY$ <> ""
LOOP
CLS

