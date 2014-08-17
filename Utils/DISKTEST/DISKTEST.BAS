IF COMMAND$ = "" THEN
    INPUT "What drive"; drive$
    CHDRIVE drive$
END IF
CHDRIVE COMMAND$
OPEN "test.fil" FOR BINARY AS #1
a$ = STRING$(32767, "a")
ON TIMER(10) GOSUB done
TIMER ON
DO
a! = a! + 32
PUT #1, a!, a$
LOOP

done:
        speed# = a! / 10
        PRINT "Your computer can save "; speed#; " Kilobytes/second"
        CLOSE
        KILL "Test.fil"
        SYSTEM

