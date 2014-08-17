        DECLARE SUB Init ()
        DECLARE SUB MoveStars ()
        DECLARE SUB NewStar (N!)

        COMMON SHARED Amount
       
        Amount = 50

        DIM SHARED StarsX(Amount, 1), StarsY(Amount, 1) AS SINGLE
        DIM SHARED SXA(Amount, 1), SYA(Amount, 1)
       
        DEF SEG = &HA000

       
        Init
       
        DO WHILE INKEY$ = ""
                MoveStars
        LOOP

        END

SUB Init
   SCREEN 13
   FOR X = 1 TO Amount: NewStar X: NEXT X
  
   FOR X = 1 TO 60: OUT &H3C8, X: OUT &H3C9, X + 10: OUT &H3C9, X + 10: OUT &H3C9, X + 10: NEXT X
END SUB

SUB MoveStars
        FOR X = 1 TO Amount
                POKE StarsX(X, 1) + (INT(StarsY(X, 1)) * 320), 0
                StarsX(X, 1) = StarsX(X, 1) - SXA(X, 1)
                StarsY(X, 1) = StarsY(X, 1) - SYA(X, 1)
                IF StarsX(X, 1) < 4 OR StarsX(X, 1) > 316 THEN NewStar X
                IF StarsY(X, 1) < 4 OR StarsY(X, 1) > 196 THEN NewStar X
                SXA(X, 1) = SXA(X, 1) + (SXA(X, 1) / 32)
                SYA(X, 1) = SYA(X, 1) + (SYA(X, 1) / 32)
               
                'For star shading, use this one:
                'POKE StarsX(X, 1) + (INT(StarsY(X, 1)) * 320), (SXA(X, 1) * SXA(X, 1)) + (SYA(X, 1) * SYA(X, 1)) * 3
               
                POKE StarsX(X, 1) + (INT(StarsY(X, 1)) * 320), (X AND 15) + 15
        NEXT X
        EXIT SUB
END SUB

SUB NewStar (N)
       
        SXA(N, 1) = (RND(1) * 2) - 1
        SYA(N, 1) = (RND(1) * 2) - 1
        StarsX(N, 1) = 160 - (SXA(N, 1) * 32)
        StarsY(N, 1) = 100 - (SYA(N, 1) * 32)

END SUB

