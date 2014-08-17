'Based on the original "M732x16.bas" by Richard Eric M. Lope, only this time,
'it saves the picture for you as a _wide_ array of picture files as you
'exit!!  Rather cool, huh?  ;*) !!

'Uses the ORIGINAL 320x133 graphics mode created for RelLib by
'  Adigun Azikiwe Polack!!!  :D !


'Joystick/Gamepad Routines and Image Saving Routine for RelLib by
'                                                   Adigun Azikiwe Polack.

'Try compiling this to .EXE format and activating that just-converted file
'  for the *FULL* effect!!  ;) !


'This is the No map example
'Faster than mapmode but only supports one tile
'useful for:
'64*64 Tilesize
'1. Floors in fighting games
'2. eye candies
'3. endings and intros
'4. ????


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

TYPE KartType                   'our camera
    x       AS SINGLE
    y       AS SINGLE
    dx      AS SINGLE
    dy      AS SINGLE
    Angle   AS INTEGER
    speed   AS SINGLE
    accel   AS SINGLE
END TYPE


'not needed but used for easy reference
CONST PI = 3.141593
CONST FRICTION = .016                       'for pseudo physics
CONST TSIZE = 64                            'tilesize
CONST TSIZESQUARED = TSIZE * TSIZE
CONST MAPTILEXMAX = 64
CONST MAPMAXPIXELX = MAPTILEXMAX * TSIZE
CONST MAPTILEYMAX = 64
CONST MAPMAXPIXELY = MAPTILEYMAX * TSIZE


'$DYNAMIC

DIM SHARED Vpage%(0 TO 31999)           'our buffer
DIM SHARED Layer%

'$STATIC
DIM SHARED Lcos!(359)                   'fast Lookups
DIM SHARED Lsin!(359)
DIM SHARED Map%(63, 63)
DIM SHARED Texture%((((TSIZESQUARED%) + 4) \ 2))    'calc array size for
                                                    'GET array

Layer% = VARSEG(Vpage%(0))                          'set layer address
                                                    'for reference

DIM SHARED Mario AS KartType                        'needs a better name


Mario.x = MAPMAXPIXELX \ 2                  'center our camera
Mario.y = MAPMAXPIXELY \ 2
Mario.dx = 0
Mario.dy = 0
Mario.Angle = 0
Mario.accel = .1                        'acceleration
Mario.speed = 0



'calc lookup tables

FOR i% = 0 TO 359
    RA! = i% * (3.141593 / 180)
    Lcos!(i%) = COS(RA!)
    Lsin!(i%) = SIN(RA!)
NEXT i%

RelFFix

CLS
SCREEN 13

'do some nice texture
FOR y% = 0 TO 63
FOR x% = 0 TO 63
    PSET (x%, y%), 4 * ABS((x% ^ 2 + y% ^ 2) AND 127) + 128
NEXT x%
NEXT y%

'smooth it for kicks ;*)
FOR i% = 0 TO 1
    RelSmooth VIDEO, 0, 0, 319, 64
NEXT i%
'duh!
GET (0, 0)-STEP(63, 63), Texture%(0)

x1 = 0              'window dimensions for Mode7
y1 = 46
x2 = 319
y2 = 133

'mode 7 parameters
Angle = 0               'Look forward
scalex = 200            '200 looks good
scaley = 200
scalez = 15             'height
Horz = 10               'eyeview
camx! = 0
camy! = 0
RelKeyBoardON           'activate keyboard handler
Finished = FALSE
T# = TIMER

RelInit320x133          'Let's set the graphics res. to 320x133 mode!!!  ;) !
'                                                       ~~~~~~~~~~~~

'gray scale the pal
FOR i = 0 TO 255
 OUT &H3C8, i
 OUT &H3C9, i \ 4
 OUT &H3C9, i \ 6
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

    'calc physics
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
    FOR sky = 0 TO 133
      Relline Layer, 0, sky, 319, sky, (sky * 3)
    NEXT
    FOR sky = 86 TO 133
      Relline Layer, 0, sky, 319, sky, 255
    NEXT
    '------------------------------------------------------------------------
   
    RelMode7 Layer%, x1, y1, x2, y2, Angle%, scalex, scaley, scalez, Horz, px!, py!, Texture%(), 0

    RelPrintShadow Layer, 0, 0, "Camx:" + STR$(INT(px!)), 224, FALSE, 1, 1, 21, 0
    RelPrintShadow Layer, 0, 10, "Camy:" + STR$(INT(py!)), 224, FALSE, 1, 1, 21, 0
    RelPrintShadow Layer, 0, 20, "scalex:" + STR$(scalex), 224, FALSE, 1, 1, 21, 0
    RelPrintShadow Layer, 0, 30, "scaley:" + STR$(scaley), 224, FALSE, 1, 1, 21, 0
    RelPrintShadow Layer, 0, 40, "scalez:" + STR$(scalez), 224, FALSE, 1, 1, 21, 0
    RelPrintShadow Layer, 0, 50, "horz:" + STR$(Horz), 224, FALSE, 1, 1, 21, 0
   
    IF Joy THEN
      RelPrintShadow Layer, 160, 0, "Gamepad Response ---", 224, FALSE, 1, 1, 26, 0
      RelPrintShadow Layer, 168, 10, "X = " + STR$(INT(Btx%)), 224, FALSE, 1, 1, 26, 0
      RelPrintShadow Layer, 168, 20, "Y = " + STR$(INT(Bty%)), 224, FALSE, 1, 1, 26, 0
      RelPrintShadow Layer, 168, 30, "Button 1 = " + STR$(INT(Bt1%)), 224, FALSE, 1, 1, 26, 0
      RelPrintShadow Layer, 168, 40, "Button 2 = " + STR$(INT(Bt2%)), 224, FALSE, 1, 1, 26, 0
      RelPrintShadow Layer, 168, 50, "Button 3 = " + STR$(INT(Bt3%)), 224, FALSE, 1, 1, 26, 0
      RelPrintShadow Layer, 168, 60, "Button 4 = " + STR$(INT(Bt4%)), 224, FALSE, 1, 1, 26, 0
    END IF
   
    RelPrintShadow Layer, 0, 117, "320x133 graphics mode is created", 224, FALSE, 1, 1, 56, 0
    RelPrintShadow Layer, 16, 125, "for RelLib by Adigun Azikiwe Polack.", 224, FALSE, 1, 1, 56, 0
    'RelWait
    RelPcopy VIDEO, Layer
LOOP UNTIL Finished

'----------------- Now, to save the pic as a whole variety of formats!!  ;) !
RelSavePic VIDEO, "Mde7Sti2.com", COMexe, Gr.320x200
RelPcopy VIDEO, Layer

RelSavePic VIDEO, "Mde7Sti2.gif", GIF, Gr.320x133
RelPcopy VIDEO, Layer

RelSavePic VIDEO, "Mde7Sti2.bmp", BMP, Gr.320x200
RelPcopy VIDEO, Layer

RelSavePic VIDEO, "Mde7Sti2.clp", CLP, Gr.320x200
RelPcopy VIDEO, Layer

RelSavePic VIDEO, "Mde7Sti2.tga", TGA, Gr.320x200
RelPcopy VIDEO, Layer

RelSavePic VIDEO, "Mde7Sti2.ras", RAS, Gr.320x200
RelPcopy VIDEO, Layer

RelSavePic VIDEO, "Mde7Sti2.wmf", WMF, Gr.320x200
RelPcopy VIDEO, Layer

RelSavePic VIDEO, "Mde7Sti2.tif", TIF, Gr.320x200
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

