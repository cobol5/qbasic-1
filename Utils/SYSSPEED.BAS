CLS
REDIM counts(20)

DEF SEG = &H40                 'point to low memory

PRINT "About to preforming tests"
PRINT "Approximately 1 second per test"
PRINT "Press any key to begin..."
WHILE INKEY$ = "": WEND
PRINT "Begining tests.. ."

' Regular count test
FOR i = 1 TO 18

    t1 = PEEK(&H6C)                'get tick count
    DO
        t2 = PEEK(&H6C)            'get tick count
    LOOP UNTIL t2 <> t1            'wait 'til its a new tick
    DO
        sysfact& = sysfact& + 1
        IF sysfact& = z THEN EXIT DO
    LOOP UNTIL t2 <> PEEK(&H6C)         'keep going 'til its a new tick
    systotal& = systotal& + sysfact&
NEXT i
sysfact& = systotal& / 18
counts(1) = sysfact&

PRINT
PRINT "Simple count test complete"
systotal& = 0

' Sine test
FOR i = 1 TO 18

    t1 = PEEK(&H6C)                'get tick count
    DO
        t2 = PEEK(&H6C)            'get tick count
    LOOP UNTIL t2 <> t1            'wait 'til its a new tick
    DO
        temp = SIN(112.3)
        sysfact& = sysfact& + 1
        IF sysfact& = z THEN EXIT DO
    LOOP UNTIL t2 <> PEEK(&H6C)         'keep going 'til its a new tick
    systotal& = systotal& + sysfact&
NEXT i
sysfact& = systotal& / 18
counts(2) = sysfact&

PRINT "Sine test complete"
systotal& = 0

' Cosine test
FOR i = 1 TO 18

    t1 = PEEK(&H6C)                'get tick count
    DO
        t2 = PEEK(&H6C)            'get tick count
    LOOP UNTIL t2 <> t1            'wait 'til its a new tick
    DO
        temp = COS(112.3)
        sysfact& = sysfact& + 1
        IF sysfact& = z THEN EXIT DO
    LOOP UNTIL t2 <> PEEK(&H6C)         'keep going 'til its a new tick
    systotal& = systotal& + sysfact&
NEXT i
sysfact& = systotal& / 18
counts(3) = sysfact&

PRINT "Cosine test complete"
systotal& = 0

' Tangent test
FOR i = 1 TO 18

    t1 = PEEK(&H6C)                'get tick count
    DO
        t2 = PEEK(&H6C)            'get tick count
    LOOP UNTIL t2 <> t1            'wait 'til its a new tick
    DO
        temp = TAN(112.3)
        sysfact& = sysfact& + 1
        IF sysfact& = z THEN EXIT DO
    LOOP UNTIL t2 <> PEEK(&H6C)         'keep going 'til its a new tick
    systotal& = systotal& + sysfact&
NEXT i
sysfact& = systotal& / 18
counts(4) = sysfact&

PRINT "Tangent test complete"
systotal& = 0

PRINT
PRINT "Test results"

FOR i = 1 TO 4
    PRINT counts(i)
NEXT i

OPEN "C:\sysspeed.dos" FOR OUTPUT AS #1
FOR i = 1 TO 4
PRINT #1, counts(i)
NEXT i
CLOSE #1

