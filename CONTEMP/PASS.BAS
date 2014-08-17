Y = 20
X = 34

b = 1
OPEN "conf.txt" FOR INPUT AS #1
WHILE NOT EOF(1)
LINE INPUT #1, A$
IF b = 6 THEN GOTO 1
b = b + 1
WEND
1 CLOSE #1
CORRECT.PASSWORD$ = A$

CLS
password:
COLOR 1, 1
SCREEN 0
CLS

GOSUB display

password$ = ""
star$ = ""

DO

key.press$ = INPUT$(1)

IF key.press$ = CHR$(13) THEN GOTO check.password
                                                 
                                                 

IF key.press$ = CHR$(8) THEN
star2$ = ""
password2$ = ""
FOR q = 1 TO LEN(star$)
star2$ = star2$ + " "
NEXT q
FOR q = 1 TO LEN(password$)
password2$ = password2$ + " "
NEXT q
IF LEN(password$) > 0 THEN
password$ = LEFT$(password$, LEN(password$) - 1)
star$ = LEFT$(star$, LEN(star$) - 1)
LOCATE Y, X
PRINT star2$
LOCATE Y, X
PRINT star$
ELSE BEEP
END IF
END IF


IF key.press$ <> CHR$(13) THEN
IF key.press$ <> CHR$(8) THEN
IF LEN(password$) < 20 THEN
password$ = password$ + key.press$
star$ = ""
V = LEN(password$)
FOR c = 1 TO V
star$ = star$ + "*"
NEXT c
LOCATE Y, X
PRINT star$
ELSE BEEP
END IF
END IF
END IF

LOOP



check.password:

CORRECT.PASSWORD$ = UCASE$(CORRECT.PASSWORD$)
password$ = UCASE$(password$)
                                              
IF password$ = CORRECT.PASSWORD$ THEN
END
ELSE
LOCATE 19, 22
PRINT "ษออออออออออออออออออออออออออออออออป"
LOCATE 20, 22
PRINT "บ        Senha Incorreta!        บ"
LOCATE 21, 22
PRINT "ศออออออออออออออออออออออออออออออออผ"
BEEP
DO: LOOP WHILE INKEY$ = ""
GOTO password
END IF
END

display:
COLOR 0, 7
PRINT "                         O Programa de Senhas Versฦo 1.0                        "
COLOR 15, 1
PRINT "                                                                                "
PRINT "                    CCCCC  OOO  N   N TTTTT EEEEE M   M PPPPP"
PRINT "                    C     O   O NN  N   T   E     MM MM P   P"
PRINT "                    C     O   O N N N   T   EEEEE M M M PPPPP"
PRINT "                    C     O   O N  NN   T   E     M   M P    "
PRINT "                    CCCCC  OOO  N   N   T   EEEEE M   M P    "
PRINT "                                                                                "
PRINT "                               Por Ygor Lazaro"
PRINT
PRINT "                                                                                "
PRINT "                                                                                "
PRINT "                                                                                "
PRINT "                                                                                "
PRINT "                                                                                "
PRINT "                                                                                "
LOCATE 19, 22
PRINT "ษออออออออออออออออออออออออออออออออป"
LOCATE 20, 22
PRINT "บ    Senha:                      บ"
LOCATE 21, 22
PRINT "ศออออออออออออออออออออออออออออออออผ"

RETURN

