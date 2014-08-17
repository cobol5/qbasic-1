'Based on the original "MODE7SMT.bas" by Richard Eric M. Lope, only this
'time, it saves the picture for you as a _wide_ array of picture files as you
'exit!!  Rather cool, huh?  ;*) !!

'Uses the ORIGINAL 320x100 graphics mode created for RelLib by
'  Adigun Azikiwe Polack!!!  :D !

'Joystick/Gamepad Routines and Image Saving Routine for RelLib by
'                                                   Adigun Azikiwe Polack.

'Try compiling this to .EXE format and activating that just-converted file
'  for the *FULL* effect!!  ;) !

'Uses 64*64 tiles


'Controls (Keyboard)...
'    Arrows = move
'    A/Z    = Scale z/height
'    S/X    = scale x
'    D/C    = scale y
'    F/V    = Horizon/Eyeview

'Controls (Joystick/Gamepad)...
'    Direction pad     = move
'    Button 1/Button 2 = Scale z/height
'    Button 3/Button 4 = Horizon/Eyeview

DEFINT A-Z

REM $INCLUDE: 'RelLib.BI'

RelJoyDetect Joy
RelInitText
IF Joy = 1 THEN PRINT "Joystick Detected!!" ELSE PRINT "Sorry, no joystick detected.  Switching to keyboard only......."
COLOR 15
PRINT : PRINT "After the demo begins, press " + CHR$(34) + "ESC" + CHR$(34) + " to save the picture in a WIDE variety of"
PRINT "image files right as you exit!  "; : COLOR 10: : PRINT ";) !"
COLOR 7
SLEEP 5

RANDOMIZE TIMER

TYPE KartType
    x       AS SINGLE
    y       AS SINGLE
    dx      AS SINGLE
    dy      AS SINGLE
    Angle   AS INTEGER
    speed   AS SINGLE
    accel   AS SINGLE
END TYPE


TYPE LevelType
    camx        AS INTEGER
    camy        AS INTEGER
    MaxX        AS INTEGER
    MaxY        AS INTEGER
END TYPE

CONST PI = 3.141593
CONST FRICTION = .015
CONST MAXTILE = 5
CONST TSIZE = 64
CONST TSIZESQUARED = TSIZE * TSIZE
CONST MAPTILEXMAX = 64
CONST MAPMAXPIXELX = MAPTILEXMAX * TSIZE
CONST MAPTILEYMAX = 128
CONST MAPMAXPIXELY = MAPTILEYMAX * TSIZE


'$DYNAMIC

DIM SHARED Vpage%(0 TO 31999)
DIM SHARED Layer%

'$STATIC
DIM SHARED Lcos!(359)
DIM SHARED Lsin!(359)
DIM SHARED Map%(MAPTILEXMAX - 1, MAPTILEYMAX - 1)
DIM SHARED Texture%((((TSIZESQUARED%) + 4) \ 2) * MAXTILE)

Layer% = VARSEG(Vpage%(0))

DIM SHARED Mario AS KartType


Mario.x = 0
Mario.y = 0
Mario.dx = .5
Mario.dy = .5
Mario.Angle = 0
Mario.accel = .1
Mario.speed = 0


Level.camX = 0
Level.camY = 0
Level.MaxX = 2000
Level.MaxY = 1500

RelFFix

FOR y% = 0 TO MAPTILEYMAX - 1
FOR x% = 0 TO MAPTILEXMAX - 1
    Map%(x%, y%) = 2 + INT(RND * (MAXTILE - 1))
    'set to one on bounds so that you see the map bounds
    IF y% = 0 THEN Map%(x%, y%) = 1
    IF x% = 0 THEN Map%(x%, y%) = 1
    IF y% = MAPTILEYMAX - 1 THEN Map%(x%, y%) = 1
    IF x% = MAPTILEXMAX - 1 THEN Map%(x%, y%) = 1
NEXT x%
NEXT y%

FOR i% = 0 TO 359
    RA! = i% * (3.141593 / 180)
    Lcos!(i%) = COS(RA!)
    Lsin!(i%) = SIN(RA!)
NEXT i%


CLS
SCREEN 13


'Textures
FOR y% = 0 TO 63
FOR x% = 0 TO 63
    PSET (x%, y%), 255 - (4 * SQR((x% - 32) ^ 2 + (y% - 32) ^ 2)) AND 255
    PSET (x% + 64, y%), 4 * ABS((x% ^ 2 + y% ^ 2) AND 63) + 31
    PSET (x% + 64 * 2, y%), 64 + (x% * RND) OR (RND * y%)
    PSET (x% + 64 * 3, y%), SIN(y% OR x%) * 127 + COS(y% OR x%) * 127
    PSET (x% + 64 * 4, y%), (4 * SQR((x% - 32) ^ 2 + (y% - 32) ^ 2)) AND 255
NEXT x%
NEXT y%
FOR i% = 0 TO 3
RelSmooth VIDEO, 0, 0, 319, 64
NEXT i%
FOR i% = 0 TO MAXTILE - 1
    GET (i% * 64, 0)-STEP(63, 63), Texture%(i% * (((TSIZESQUARED%) + 4) \ 2))
NEXT i%

x1 = 0
y1 = 35
x2 = 319
y2 = 100

Angle = 0
scalex = 200
scaley = 200
scalez = 15
Horz = 5
camx! = 0
camy! = 0
RelKeyBoardON
Finished = FALSE
F& = 0
T# = TIMER
RelInit320x100

FOR i = 0 TO 255
 OUT &H3C8, i
 OUT &H3C9, i \ 8
 OUT &H3C9, i \ 4
 OUT &H3C9, i \ 8
NEXT i

DO
    Btx% = 65: Bty% = 65 ' <----- use this as a way of "balancing" the
                         '        joystick/gamepad, to avoid problems
                         '        later on!  ;) !

    IF Joy THEN RelJoy Btx%, Bty%
    IF Joy THEN RelJoyTrig Bt1%, Bt2%, Bt3%, Bt4%
   
    F& = (F& + 1) AND &H7FFFFFFF

    IF Btx% <= 31 OR RelKey(KEYLEFT) THEN
       Mario.Angle = (Mario.Angle - 1)
       IF Mario.Angle < 0 THEN Mario.Angle = 360 + Mario.Angle
       KoopaFrame% = 2
    END IF
    IF Btx% >= 86 OR RelKey(KEYRIGHT) THEN
       Mario.Angle = (Mario.Angle + 1)
       IF Mario.Angle > 359 THEN Mario.Angle = 360 - Mario.Angle
       KoopaFrame% = 3
    END IF
    IF Bty% >= 86 OR RelKey(KEYDOWN) THEN
        Mario.speed = Mario.speed - Mario.accel
    END IF
    IF Bty% <= 31 OR RelKey(KEYUP) THEN
        Mario.speed = Mario.speed + Mario.accel
    END IF
    IF RelKey(KEYSPACE) THEN
        'Scalez = Scalez + 1
    END IF
    IF Bt1% = True OR RelKey(&H1E) THEN          'A
        scalez = scalez + 1
    END IF
    IF Bt2% = True OR RelKey(&H2C) THEN          'z
        scalez = scalez - 1
    END IF
    IF RelKey(&H1F) THEN          's
        scalex = scalex + 1
    END IF
    IF RelKey(&H2D) THEN          'x
        scalex = scalex - 1
        IF scalex < 1 THEN scalex = 1
    END IF
    IF RelKey(&H20) THEN            'd
        scaley = scaley + 1
    END IF
    IF RelKey(&H2E) THEN            'c
        scaley = scaley - 1
    END IF

    IF Bt3% = True OR RelKey(&H21) THEN            'F
        Horz = Horz + 1
        y1 = y1 - 1
        IF y1 < 0 THEN y1 = 0
    END IF
    IF Bt4% = True OR RelKey(&H2F) THEN            'v
        Horz = Horz - 1
    END IF

    IF RelKey(KEYESC) THEN
        Finished = True
    END IF

    Mario.speed = Mario.speed - Mario.speed * FRICTION
    dx! = (Lcos!(Mario.Angle)) * Mario.speed
    dy! = (Lsin!(Mario.Angle)) * Mario.speed

    Mario.x = Mario.x + dx!
    IF Mario.x < 0 THEN
        Mario.x = 0
    ELSEIF Mario.x >= MAPMAXPIXELX THEN
        Mario.x = MAPMAXPIXELX
    END IF

    Mario.y = Mario.y + dy!
    IF Mario.y < 0 THEN
        Mario.y = 0
    ELSEIF Mario.y >= MAPMAXPIXELY THEN
        Mario.y = MAPMAXPIXELY
    END IF

    Angle% = Mario.Angle
    px! = Mario.x
    py! = Mario.y
    RelCls Layer, 0
   
    '--------------------------------- Let's add some sky here, shall we?  ;)
    FOR sky = 0 TO 100
      Relline Layer, 0, sky, 319, sky, (sky * 3)
    NEXT
    FOR sky = 86 TO 100
      Relline Layer, 0, sky, 319, sky, 255
    NEXT
    '------------------------------------------------------------------------
   
    RelMode7Map Layer%, x1, y1, x2, y2, Angle, scalex, scaley, scalez, Horz, px!, py!, Texture%(), 0, Map%()
    
    RelPrintShadow Layer, 0, 0, "Camx:" + STR$(INT(px!)), 224, FALSE, 1, 1, 26, 0
    RelPrintShadow Layer, 0, 10, "Camy:" + STR$(INT(py!)), 224, FALSE, 1, 1, 26, 0
    RelPrintShadow Layer, 0, 20, "scalex:" + STR$(scalex), 224, FALSE, 1, 1, 26, 0
    RelPrintShadow Layer, 0, 30, "scaley:" + STR$(scaley), 224, FALSE, 1, 1, 26, 0
    RelPrintShadow Layer, 0, 40, "scalez:" + STR$(scalez), 224, FALSE, 1, 1, 26, 0
    RelPrintShadow Layer, 0, 50, "horz:" + STR$(Horz), 224, FALSE, 1, 1, 26, 0
   
    IF Joy THEN
      RelPrintShadow Layer, 160, 0, "Gamepad Response ---", 224, FALSE, 1, 1, 26, 0
      RelPrintShadow Layer, 168, 10, "X = " + STR$(INT(Btx%)), 224, FALSE, 1, 1, 26, 0
      RelPrintShadow Layer, 168, 20, "Y = " + STR$(INT(Bty%)), 224, FALSE, 1, 1, 26, 0
      RelPrintShadow Layer, 168, 30, "Button 1 = " + STR$(INT(Bt1%)), 224, FALSE, 1, 1, 26, 0
      RelPrintShadow Layer, 168, 40, "Button 2 = " + STR$(INT(Bt2%)), 224, FALSE, 1, 1, 26, 0
      RelPrintShadow Layer, 168, 50, "Button 3 = " + STR$(INT(Bt3%)), 224, FALSE, 1, 1, 26, 0
      RelPrintShadow Layer, 168, 60, "Button 4 = " + STR$(INT(Bt4%)), 224, FALSE, 1, 1, 26, 0
    END IF

    RelPrintShadow Layer, 0, 84, "320x100 graphics mode is created", 224, FALSE, 1, 1, 56, 0
    RelPrintShadow Layer, 16, 92, "for RelLib by Adigun Azikiwe Polack.", 224, FALSE, 1, 1, 56, 0
    'RelWait
    RelPcopy VIDEO, Layer
LOOP UNTIL Finished

'----------------- Now, to save the pic as a whole variety of formats!!  ;) !
RelSavePic VIDEO, "Mde7Sti1.com", COMexe, Gr.320x100
RelPcopy VIDEO, Layer

RelSavePic VIDEO, "Mde7Sti1.gif", GIF, Gr.320x100
RelPcopy VIDEO, Layer

RelSavePic VIDEO, "Mde7Sti1.bmp", BMP, Gr.320x100
RelPcopy VIDEO, Layer

RelSavePic VIDEO, "Mde7Sti1.clp", CLP, Gr.320x100
RelPcopy VIDEO, Layer

RelSavePic VIDEO, "Mde7Sti1.tga", TGA, Gr.320x100
RelPcopy VIDEO, Layer

RelSavePic VIDEO, "Mde7Sti1.ras", RAS, Gr.320x100
RelPcopy VIDEO, Layer

RelSavePic VIDEO, "Mde7Sti1.wmf", WMF, Gr.320x100
RelPcopy VIDEO, Layer

RelSavePic VIDEO, "Mde7Sti1.tif", TIF, Gr.320x100
RelPcopy VIDEO, Layer
'----------------------------------------------------------------------------

RelKeyBoardOFF

Fps% = F& / (TIMER - T#)
CLS
RelInitText
WIDTH 80
PRINT "FPS:"; Fps%
c$ = INPUT$(1)
END

