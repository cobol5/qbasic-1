'                            Kedjikeen Presents:
'                                   MAZER
'
'    This is a little program I wrote years ago before I knew anything about
' proper coding.  As a result, it's nearly imposible to follow.  However, I
' did manage to put in some options and a way to actually play the game.
' (Wow Eric, big deal)  :)  Well anyway, as my first real program I thought
' I should put it up, if you hate it, I understand.  It's really not so bad.
' You can screw around with the palette to see how the program works, but
' just digging through the code won't help you much, and that's my fault.
'                          Thanks!
'                                Eric             EricMD@ix.netcom.com
DIM V(4)
DIM SHARED M$(13)
DIM Z(21, 61)
DIM SHARED R(13)
DIM E$(4)
goinagain:
SCREEN 7
SCREEN 13
LINE (10, 10)-(308, 170), 4, B    'DRAWS BORDER
LINE (13, 13)-(305, 167), 4, B
PSET (13, 13), 0
PSET (305, 13), 0
PSET (13, 167), 0
PSET (305, 167), 0
PSET (14, 14), 4
PSET (304, 14), 4
PSET (14, 166), 4
PSET (304, 166), 4
COLOR 121
LOCATE 3, 18
PRINT "MAZER"
COLOR 133
LOCATE 4, 21
PRINT "by Eric Davidson"
LOCATE 15, 4
COLOR 36
PRINT "F2  main menu"
LOCATE 16, 4
PRINT "Q   quit"
COLOR 244
PALETTE 244, 256 * 55
LOCATE 18, 18
PRINT "Q"
COLOR 243
PALETTE 243, 65536 * 55 + 45
LOCATE 18, 19
PRINT "uit"
LOCATE 20, 15
COLOR 202
PALETTE 202, (65536 * 50) + (256 * 30)
PRINT "Press <ENTER>"
PALETTE 5, 0
COLOR 5                                        'KEDJIKEEN LOGO
C = 63 * .4
FOR B = 20 TO 1 STEP -1
PALETTE B + 120, 65536 * (64 - (B * 3))
PALETTE B + 50, 64 - (B * 3)
C = C - .5
CIRCLE (20 + C, 20 + C), B, B + 50
PAINT (20 + C, 20 + C), B + 50
CIRCLE (70 + C, 20 + C), B, B + 120
PAINT (70 + C, 20 + C), B + 120
NEXT B
FOR A = 0 TO 90
FOR B = 0 TO 5
IF POINT(A + 41, B + 43) = 5 THEN
PSET (A + 41, B + 43), 18 + (B * 2)
END IF
NEXT B
NEXT A
E$(1) = "X ALWAYS POSSIBLE"
E$(2) = "X DIFFICULT"
E$(3) = "X PLAY"
E$(4) = "  LARGE"
T = 1
RD:
FOR A = 1 TO 4
COLOR 66
IF T = A THEN COLOR 51
LOCATE 6 + A, 20
PRINT E$(A)
NEXT A
FOR A = 1 TO 80
DO UNTIL INKEY$ = ""
LOOP
NEXT A
GOSUB 2
IF k$ = CHR$(13) THEN GOTO DONE
IF k$ = "Q" THEN SCREEN 0: END
IF k$ = "P" AND T < 4 THEN T = T + 1
IF k$ = "H" AND T > 1 THEN T = T - 1
IF k$ = " " THEN
T$ = LEFT$(E$(T), 1)
IF T$ = "X" THEN T$ = " " ELSE T$ = "X"
MID$(E$(T), 1, 1) = T$
END IF
GOTO RD
DONE:
FOR A = 1 TO 4
T$ = LEFT$(E$(A), 1)
IF T$ = "X" THEN V(A) = 1
NEXT A
CLS
HOMER:
U = 30 * V(4) + 30
RESTORE
RANDOMIZE TIMER
FOR A = 0 TO 21
FOR B = 0 TO U + 1
Z(A, B) = 0
M$(0) = " "
IF A = 0 OR A = 21 THEN Z(A, B) = 8
IF B = 0 OR B = U + 1 THEN Z(A, B) = 1
NEXT B
NEXT A
FOR M = 1 TO 13
READ M$(M)
NEXT M
SCREEN 9
COLOR 1
PALETTE 1, 0
PALETTE 4, 0
DATA º,È,»,É,¼,Ë,Ì,Í,Î,¹,Ê,Ø,×
W = 0
FOR A = 1 TO 20
FOR B = 1 TO U
FOR C = 1 TO 13
R(C) = C
NEXT C
W = (W - 1) * (W - 1)
N = 0: W = 0
T = Z(A - 1, B)
IF T = 1 OR T = 3 OR T = 4 OR T = 6 OR T = 7 OR T = 9 OR T = 10 OR T = 13 THEN N = 1 ELSE N = -1
T = Z(A, B - 1)
IF T = 2 OR T = 4 OR T = 6 OR T = 8 OR T = 9 OR T = 7 OR T = 11 OR T = 12 THEN W = 1 ELSE W = -1
IF A = 20 THEN R(1) = 0: R(3) = 0: R(4) = 0: R(6) = 0: R(7) = 0: R(9) = 0: R(10) = 0
IF B = U THEN R(2) = 0: R(7) = 0: R(6) = 0: R(4) = 0: R(8) = 0: R(10) = 0: R(9) = 0: R(11) = 0
IF N = 1 THEN R(3) = 0: R(4) = 0: R(6) = 0: R(8) = 0: R(12) = 0
IF N = -1 THEN R(1) = 0: R(2) = 0: R(5) = 0: R(7) = 0: R(9) = 0: R(10) = 0: R(11) = 0: R(13) = 0
IF W = 1 THEN R(1) = 0: R(2) = 0: R(4) = 0: R(7) = 0: R(13) = 0
IF W = -1 THEN R(3) = (0): R(5) = 0: R(6) = 0: R(8) = 0: R(9) = 0: R(10) = 0: R(11) = 0: R(12) = 0
IF V(2) = 0 AND A <> 20 AND B <> U THEN
IF W = 1 THEN R(12) = 0
IF W = 0 THEN R(13) = 0
END IF
IF A = 20 AND B = U - 1 THEN
T = Z(19, U)
IF T <> 1 AND T <> 3 AND T <> 4 AND T <> 6 AND T <> 7 AND T <> 9 AND T <> 10 THEN R(5) = 0
END IF
H = 0
E = 0
FOR F = 1 TO 13
E = E + R(F)
NEXT F
IF E > 0 THEN
DO UNTIL H <> 0
G = INT(RND(1) * 13) + 1
H = R(G)
LOOP
ELSE
H = 0
END IF
LOCATE A + 2, B + 2
Z(A, B) = H
PRINT M$(H)
NEXT B
NEXT A
PAINT (20, 34), 4, 1
IF V(4) = 0 THEN S = POINT(249, 300) + POINT(252, 297)
IF V(4) = 1 THEN S = POINT(489, 300) + POINT(492, 297)
IF V(1) = 0 THEN S = 5
IF S < 4 THEN GOTO HOMER
PAINT (20, 34), 0, 1
PALETTE 1, 7
IF V(2) = 0 THEN END
k$ = ""
A = 1
B = 1
T = Z(A, B)
IF T <> 3 AND T <> 4 AND T <> 6 AND T <> 8 AND A > 1 AND Z(A - 1, B) <> 13 THEN N = 1 ELSE N = 0
IF T <> 2 AND T <> 5 AND T <> 8 AND T <> 11 AND A < 20 AND Z(A + 1, B) <> 13 THEN S = 1 ELSE S = 0
IF T <> 1 AND T <> 2 AND T <> 4 AND T <> 7 AND B > 1 AND Z(A, B - 1) <> 12 THEN W = 1 ELSE W = 0
IF T <> 1 AND T <> 3 AND T <> 5 AND T <> 10 AND B < U AND Z(A, B + 1) <> 12 THEN E = 1 ELSE E = 0
IF V(3) <> 1 THEN GOTO DONERS
PALETTE 12, 0
LINE (0, 0)-(7, 13), 12, BF
DIM BACKD(8, 8)
GET (0, 0)-(7, 13), BACKD
X = 1: Y = 1
REPLAY:
N = 0: S = 0: W = 0: E = 0
H = Z(Y, X)
XX = X * 8 + 9
YY = Y * 14 + 14
PUT (XX, YY), BACKD
IF H = 1 OR H = 2 OR H = 5 OR H = 7 OR H = 9 OR H = 10 OR H = 11 THEN
IF Z(Y - 1, X) <> 13 THEN N = 1
END IF
IF H = 1 OR H = 3 OR H = 4 OR H = 6 OR H = 7 OR H = 9 OR H = 10 THEN
IF Z(Y + 1, X) <> 13 THEN S = 1
END IF
IF H = 3 OR H = 5 OR H = 6 OR H = 8 OR H = 9 OR H = 10 OR H = 11 THEN
IF Z(Y, X - 1) <> 12 THEN W = 1
END IF
IF H = 2 OR H = 4 OR H = 6 OR H = 7 OR H = 8 OR H = 9 OR H = 11 THEN
IF Z(Y, X + 1) <> 12 THEN E = 1
END IF
GOSUB 2
PUT (XX, YY), BACKD
IF k$ = "H" AND N = 1 THEN Y = Y - 1
IF k$ = "P" AND S = 1 THEN Y = Y + 1
IF k$ = "M" AND E = 1 THEN X = X + 1
IF k$ = "K" AND W = 1 THEN X = X - 1
IF k$ = "Q" THEN GOTO DONERS
IF k$ = "<" THEN GOTO goinagain
GOTO REPLAY

DONERS:
END
2
k$ = ""
DO UNTIL k$ <> ""
k$ = RIGHT$(UCASE$(INKEY$), 1)
LOOP
RETURN

