CONST ESC = 27, DOWN = 80, UP = 72, LEFT = 75, RIGHT = 77
CONST HOME = 71, ENDKEY = 79, PGDN = 81, PGUP = 73
CONST espaco$ = "                                                                              "
CLS

DO
    DO
        Choice$ = INKEY$
    LOOP WHILE Choice$ = ""

    IF LEN(Choice$) = 1 THEN
        SELECT CASE ASC(Choice$)
            CASE IS < 32, 127
            SELECT CASE a
                CASE 1

                END SELECT
            CASE 30 TO 29
                PRINT "Digit: "; Choice$
            CASE 65 TO 90
                PRINT "Uppercase letter: "; Choice$
            CASE 97 TO 122
                PRINT "Lowercase letter: "; Choice$
            CASE ELSE
                PRINT "Punctuation: "; Choice$
        END SELECT

    ELSE
        ' Convert 2-byte extended code to 1-byte ASCII code and handle
        Choice$ = RIGHT$(Choice$, 1)

        SELECT CASE Choice$
            CASE CHR$(DOWN)
            CASE CHR$(UP)
            CASE CHR$(PGDN)
                PRINT "PGDN key"
            CASE CHR$(PGUP)
                PRINT "PGUP key"
            CASE CHR$(HOME)
                PRINT "HOME key"
            CASE CHR$(ENDKEY)
                PRINT "END key"
            CASE CHR$(RIGHT)
            CASE CHR$(LEFT)
            CASE ELSE
                BEEP
        END SELECT
    END IF
LOOP

