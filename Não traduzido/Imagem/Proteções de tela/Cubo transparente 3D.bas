DECLARE SUB Init ()
DECLARE SUB Vector ()
'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-Ä-Äúú ú
'³ *** Very Simple Floating-Point Vector Cube ***
'³
'³ Programmed by SkurK/b.
'³ E-mail: skurk@multinet.no
'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-Ä-Äúú ú
     
        COMMON SHARED Distance, NumPoints, NumConn, Vx, Vy, Vz
       
        NumPoints = 8
        NumConn = 12

        DIM SHARED Points(NumPoints, 3)
        DIM SHARED Rotated(NumPoints, 2)
        DIM SHARED Connect(NumConn, 2)
     
        Init

        DO WHILE INKEY$ = ""
                PCOPY 3, 2: SCREEN , , 2, 0
       
                WAIT &H3DA, 8
                WAIT &H3DA, 8, 8
       
                Vector
                PCOPY 2, 0: SCREEN , , 2, 0
        LOOP
        END

'---------------------------------------------------------------------------
' Datas for object
' NumPoints=Number of Points (X,Y,Z)
' NumConn=Number of Connects (PointA,PointB)
'---------------------------------------------------------------------------

        REM  points table
        REM  x , y , z
        DATA -50,-50,-50
        DATA 50,-50,-50
        DATA 50,50,-50
        DATA -50,50,-50
        DATA -50,-50,50
        DATA 50,-50,50
        DATA 50,50,50
        DATA -50,50,50

        REM  Points.  (From,To, From,To, ..)
        DATA 1,2, 2,3, 3,4, 4,1
        DATA 5,6, 6,7, 7,8, 8,5
        DATA 5,1, 6,2, 7,3, 8,4

SUB Init
        'copy data into tables
       
        FOR N = 1 TO NumPoints
            READ x, y, z
            Points(N, 1) = x
            Points(N, 2) = y
            Points(N, 3) = z
        NEXT N

        FOR N = 1 TO NumConn
            READ a, b
            Connect(N, 1) = a
            Connect(N, 2) = b
        NEXT N

        Distance = 144
        SCREEN 7
END SUB

SUB Vector
        Vx = Vx + .01
        Vy = Vy + .02
        Vz = Vz + .03

        FOR N = 1 TO NumPoints
        x = Points(N, 1)
        y = Points(N, 2)
        z = Points(N, 3)
        GOSUB Calc
        Rotated(N, 1) = Nx
        Rotated(N, 2) = Ny
        NEXT N

        FOR N = 1 TO NumConn
            LINE (Rotated(Connect(N, 1), 1), Rotated(Connect(N, 1), 2))-(Rotated(Connect(N, 2), 1), Rotated(Connect(N, 2), 2)), 7
        NEXT N
        EXIT SUB

Calc:   Ty = ((y * COS(Vx)) - (z * SIN(Vx)))
        Tz = ((y * SIN(Vx)) + (z * COS(Vx)))
        Tx = ((x * COS(Vy)) - (Tz * SIN(Vy)))
        Tz = ((x * SIN(Vy)) + (Tz * COS(Vy)))
        Ox = Tx
        Tx = ((Tx * COS(Vz)) - (Ty * SIN(Vz)))
        Ty = ((Ox * SIN(Vz)) + (Ty * COS(Vz)))
        Nx = INT(128 * (Tx) / (Distance - (Tz))) + 160
        Ny = INT(100 - (128 * Ty) / (Distance - (Tz)))
        RETURN
END SUB

