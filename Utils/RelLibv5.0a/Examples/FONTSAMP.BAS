DECLARE SUB LoadPal ()
DEFINT A-Z

REM $INCLUDE: 'RelLib.BI'

REDIM SHARED Vpage(31999) AS INTEGER

REDIM SHARED Font(1 TO 1) AS INTEGER
REDIM SHARED FontIndex(1 TO 1) AS INTEGER
REDIM SHARED FontPP(1 TO 1) AS INTEGER
REDIM SHARED FontPPIndex(1 TO 1) AS INTEGER
DIM SHARED RGBpal AS STRING * 768
DIM SHARED Path$


CLS
RelInitVGA13
RelCLS VARSEG(Vpage(0)), 0

'Path$ = "C:\Qbasic\RelLib\Examples\"
Path$ = ""

RelLoadPal Path$ + "RelPal.Pal", RGBpal, TRUE

File$ = Path$ + "SIfntDG.Put"
InitImageData File$, Font()
MakeImageIndex Font(), FontIndex()

File$ = Path$ + "EX-Fnt01.Put"
InitImageData File$, FontPP()
MakeImageIndex FontPP(), FontPPIndex()

RelFont256 VARSEG(Vpage(0)), 10, 50, "Press A Key:", TRUE, Font(), FontIndex()

RelFontTrans256 VARSEG(Vpage(0)), 10, 70, "Fonts by Adigun Polack", TRUE, Font(), FontIndex()
RelFontGamma256 VARSEG(Vpage(0)), 10, 80, "Fonts by Adigun Polack", TRUE, Font(), FontIndex(), 6
RelFontGamma256 VARSEG(Vpage(0)), 10, 90, "Fonts by Adigun Polack", TRUE, Font(), FontIndex(), -6

IF RelMouseDetect THEN
        RelFont256 VARSEG(Vpage(0)), 10, 150, "Mouse Detected", TRUE, Font(), FontIndex()
ELSE
        RelFont256 VARSEG(Vpage(0)), 10, 150, "No Mouse Detected", TRUE, Font(), FontIndex()
END IF
RelPCopy VIDEO, VARSEG(Vpage(0))
RelMouseShow
C$ = INPUT$(1)

FOR X = 0 TO 319
        C = C MOD 255 + 1
FOR Y = 0 TO 199
        RelPset VARSEG(Vpage(0)), X, Y, C
NEXT Y
NEXT X



RelFont256 VARSEG(Vpage(0)), 20, 50, "Fonts by Adigun Polack", FALSE, Font(), FontIndex()
RelFont256 VARSEG(Vpage(0)), 20, 60, "Greets to EEBRO and BETA_SS", FALSE, FontPP(), FontPPIndex()
RelFontFixedT256 VARSEG(Vpage(0)), 10, 190, "Anya Therese B. Lope", TRUE, Font(), FontIndex()


Score& = 1999999999
RelScore256 VARSEG(Vpage(0)), 10, 100, Score&, TRUE, Font(), FontIndex()
RelScoreTrans256 VARSEG(Vpage(0)), 0, 110, Score&, TRUE, Font(), FontIndex()
RelScoreGamma256 VARSEG(Vpage(0)), 0, 110, Score&, TRUE, Font(), FontIndex(), 6
RelScoreGamma256 VARSEG(Vpage(0)), 0, 120, Score&, TRUE, Font(), FontIndex(), -6
RelMouseLimitXY 100, 90, 250, 150
RelBox VARSEG(Vpage(0)), 100, 90, 250, 150, 15
RelMouseHide
RelPCopy VIDEO, VARSEG(Vpage(0))
RelMouseShow
DO
        XX = RelMouseX
        YY = RelMouseY
        RelBoxF VARSEG(Vpage(0)), 0, 0, 150, 30, 0
        RelFont256 VARSEG(Vpage(0)), 0, 0, "MouseX=" + STR$(XX), FALSE, Font(), FontIndex()
        RelFont256 VARSEG(Vpage(0)), 0, 10, "MouseY=" + STR$(YY), FALSE, Font(), FontIndex()
        RelFont256 VARSEG(Vpage(0)), 0, 20, "MouseB=" + STR$(RelMouseClick), FALSE, Font(), FontIndex()
        RelCopyBlock VIDEO, VARSEG(Vpage(0)), 0, 0, 150, 30
LOOP UNTIL INKEY$ <> ""

RelMouseHide


RelInitText
END

