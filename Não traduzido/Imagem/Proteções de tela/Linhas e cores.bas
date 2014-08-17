'SCRSAV02.BAS
'Programmer: Davy van Soest, Holland

'This programm is FREEWARE, but if you use any parts of it, please give some
'credit to me. Thanks!

'You can create your own figures by changing the DATA at the end of this file.

'You can change line 23 to change the speed of the programm.

SCREEN 12: CLS : CONST PI = 3.141592653#
XMin = -32: XMax = 32: YMin = -24: YMax = 24
WINDOW SCREEN (XMin, YMax)-(XMax, YMin)
PALETTE 1, 65536 * 42: PALETTE 2, 256 * 42: PALETTE 3, 1 * 42
PALETTE 4, 257 * 42: PALETTE 5, 65792 * 42: PALETTE 6, 65537 * 42
DIM X(1 TO 6), Y(1 TO 6), Z(1 TO 6)
DIM XU(1 TO 6), YU(1 TO 6), ZU(1 TO 6)
DIM XP(1 TO 6), YP(1 TO 6), ZP(1 TO 6)
PSET (0, 0), 0: DO: FOR H = 1 TO 8
READ A: FOR K = 1 TO A: READ XU(K), YU(K), ZU(K): NEXT K
FOR K = 1 TO A: READ P(K): NEXT K
FOR KD = 0 TO 1 STEP 1: FOR K = -1 TO 1 STEP .007
FOR w = 1 TO 200: FOR wt = 1 TO 10: NEXT wt, w  ' <--- Change this line to change speed
FOR KT = 1 TO A
X(KT) = SGN(XU(KT)) * SIN((K + P(KT)) * PI * XU(KT)) * 12
Y(KT) = SGN(YU(KT)) * SIN((K + P(KT)) * PI * YU(KT)) * 12
Z(KT) = SGN(ZU(KT)) * SIN((K + P(KT)) * PI * ZU(KT)) * 12
NEXT KT
FOR KT = 1 TO A - 1
IF KD = 0 THEN COL = KT ELSE COL = 0
LINE (X(KT) + Z(KT), Y(KT) + Z(KT))-(X(KT + 1) + Z(KT + 1), Y(KT + 1) + Z(KT + 1)), COL
IF KD = 0 THEN COL = KT + 1 ELSE COL = 0
NEXT KT: LINE (X(KT) + Z(KT), Y(KT) + Z(KT))-(X(1) + Z(1), Y(1) + Z(1)), COL
IF INKEY$ <> "" THEN END
NEXT K: NEXT KD: NEXT H: RESTORE: LOOP

DATA 4 ,1,2,2 ,1,2,2 ,2,3,2 ,2,1,2 ,1.1,1.1,2.1,1.6
DATA 3 ,1,4,1 ,2,1,1 ,3,3,2 ,2.7,2.2,3.2
DATA 4 ,3,4,1 ,5,3,2 ,1,4,2 ,4,3,5 ,1,1,0,0
DATA 3 ,2,4,3 ,3,4,1 ,1,3,2 ,1,0,0
DATA 2 ,2,3,4 ,3,2,1 ,0,1
DATA 3 ,1,3,2 ,3,4,2 ,3,4,1 ,0,0,0
DATA 4 ,2,1,2 ,2,2,1 ,1,2,2 ,2,2,1 ,1.5,2.5,1.5,2.5
DATA 3 ,3,1,2 ,2,3,2 ,1,2,2 ,0,0,0

