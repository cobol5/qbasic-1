DECLARE SUB CheckForClick ()
DECLARE SUB DrawButtons ()
DECLARE SUB InitButtons ()
DECLARE SUB LoadPal ()
DEFINT A-Z

REM $INCLUDE: 'RelLib.BI'

TYPE PalType
        R AS INTEGER
        G AS INTEGER
        B AS INTEGER
END TYPE

TYPE ButtonType
        X       AS INTEGER
        Y       AS INTEGER
        W       AS INTEGER
        H       AS INTEGER
END TYPE

REDIM SHARED Vpage(31999) AS INTEGER

REDIM SHARED Font(1 TO 1) AS INTEGER
REDIM SHARED FontIndex(1 TO 1) AS INTEGER

REDIM SHARED Relpal(255) AS PalType

REDIM SHARED Button(4) AS ButtonType
DIM SHARED RGBpal AS STRING * 768
DIM SHARED Path$

InitButtons

RANDOMIZE TIMER
CLS
RelInitVGA13
RelCls VARSEG(Vpage(0)), 0

Path$ = ""
RelLoadPal Path$ + "RelPal.Pal", RGBpal, TRUE

File$ = Path$ + "SIfntDG.Put"
InitImageData File$, Font()
MakeImageIndex Font(), FontIndex()


RelFont256 VARSEG(Vpage(0)), 10, 50, "Press A Key:", TRUE, Font(), FontIndex()

RelFontTrans256 VARSEG(Vpage(0)), 10, 70, "Fonts by Adigun Polack", TRUE, Font(), FontIndex()

IF RelMouseDetect THEN
        RelFont256 VARSEG(Vpage(0)), 10, 150, "Mouse Detected", TRUE, Font(), FontIndex()
ELSE
        RelFont256 VARSEG(Vpage(0)), 10, 150, "No Mouse Detected", TRUE, Font(), FontIndex()
END IF
RelPcopy VIDEO, VARSEG(Vpage(0))
RelMouseShow
C$ = INPUT$(1)
RelCls VARSEG(Vpage(0)), 0
RelCls VIDEO, 0

DrawButtons

RelMouseShow

CheckForClick

RelInitText
END

SUB CheckForClick STATIC

RelFont256 VIDEO, 10, 150, "Press Right Mouse Button to EXIT", TRUE, Font(), FontIndex()

DO
        IF RelMouseClick = 1 THEN
                FOR I = 0 TO 4
                        IF RelMouseInside(Button(I).X, Button(I).Y, Button(I).X + Button(I).W, Button(I).Y + Button(I).H) THEN
                                RelBoxF VARSEG(Vpage(0)), 10, 180, 319, 199, 0
                                RelFont256 VARSEG(Vpage(0)), 10, 180, "Clicked Button:" + STR$(I), TRUE, Font(), FontIndex()
                                RelCopyBlock VIDEO, VARSEG(Vpage(0)), 10, 180, 319, 199
                        END IF
                NEXT I
        END IF

LOOP UNTIL RelMouseClick = 2



END SUB

SUB DrawButtons STATIC
FOR I = 0 TO 4
        RelBoxF VARSEG(Vpage(0)), Button(I).X, Button(I).Y, Button(I).X + Button(I).W, Button(I).Y + Button(I).H, 25 + I
NEXT I
RelPcopy VIDEO, VARSEG(Vpage(0))
END SUB

SUB InitButtons STATIC
TileH = 20
TileW = 40

Button(0).X = 0
Button(0).Y = 0
Button(0).W = TileW
Button(0).H = TileH

Button(1).X = 120
Button(1).Y = 0
Button(1).W = TileW
Button(1).H = TileH

Button(2).X = 250
Button(2).Y = 0
Button(2).W = TileW
Button(2).H = TileH

Button(3).X = 0
Button(3).Y = 100
Button(3).W = TileW
Button(3).H = TileH

Button(4).X = 250
Button(4).Y = 100
Button(4).W = TileW
Button(4).H = TileH





END SUB

