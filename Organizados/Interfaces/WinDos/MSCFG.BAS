'Mouse configuration program for WinDos.
'by Denis Andrianov
'============================================================================
DECLARE FUNCTION MouseInit% ()
DECLARE SUB MouseStatus (Lb%, Rb%, xmouse%, ymouse%)
DECLARE SUB MouseRange (x1%, y1%, x2%, y2%)
DECLARE SUB MousePut (x%, y%)
DECLARE SUB MouseHide ()
DECLARE SUB MouseDriver (ax%, bx%, cx%, dx%)
DECLARE SUB MouseShow ()
DECLARE SUB MouseSpeed (xspeed%, yspeed%)

DECLARE SUB ReadINI ()
DECLARE SUB WriteINI ()

DECLARE SUB LoadFont (text$, t1extx%, texty%, colour%, size%)
DECLARE SUB Interface ()
DECLARE SUB TinyButton (x%, y%, x2%, y2%, colour%, topcol%, botcol%)

DECLARE SUB UpdateSpeed ()
DECLARE SUB UpdateClick ()

CONST MainINI = "windos.ini"
CONST TempINI = "windos.tmp"
CONST MainFNT = "windos.fnt"

CONST XMsMax = 50
CONST YMsMax = 50
CONST DefaultX = 6
CONST DefaultY = 4
CONST ClickMax = 3000
CONST DefaultClick = 500

DIM SHARED FontsPath$
DIM SHARED Mouse$               'Mouse data.
DIM SHARED WindosFont$(255)
DIM SHARED ClkDelay%
DIM SHARED XmsSpd%
DIM SHARED YmsSpd%
DIM SHARED id$
DIM SHARED FinishDate$
'============================================================================
'Defaults
Mouse$ = SPACE$(57)
id$ = "Mouse Configuration Program V1.1 (by Denis Andrianov)"
FinishDate$ = "30 July, 2000"

LeftClick% = 0
DoubleClick% = 0

'============================================================================
'Initializing....

FOR i% = 1 TO 57             'Reading mouse data...
  READ a$
  H$ = CHR$(VAL("&H" + a$))
  MID$(Mouse$, i%, 1) = H$
  NEXT i%

ms% = MouseInit%           'Checking if the mouse exists...
  IF NOT ms% THEN
  CLS
  PRINT "Mouse driver not present."
  END
  END IF

ReadINI

freefl% = FREEFILE                  'Loading main font...
OPEN FontsPath$ + "\" + MainFNT FOR INPUT AS freefl%
  FOR counter% = 1 TO 255
  LINE INPUT #freefl%, WindosFont$(counter%)
  NEXT counter%
CLOSE freefl%

SCREEN 12
CLS

Interface

'Setting up the mouse...
MouseRange 0, 0, 638, 478
MouseSpeed XmsSpd%, YmsSpd%
MousePut 320, 240
MouseShow

'----------------------------------------------------------------------------
  DO UNTIL INKEY$ = CHR$(27)
  MouseStatus Lb%, Rb%, xmouse%, ymouse%
  'LOCATE 1, 1: PRINT xmouse%, ymouse%
  'LOCATE 1, 1: PRINT LeftClick%

    SELECT CASE Lb%
  
    CASE -1
    
      IF ButDown% = 0 THEN
      
        IF xmouse% > 20 AND xmouse% < 140 AND ymouse% > 130 AND ymouse% < 160 THEN
        MouseHide                  'Top Default is pressed...
        ButPrs% = 1
        TinyButton 20, 130, 140, 160, 7, 0, 15
        LoadFont "Default value", 40, 136, 0, 4
       
        ELSEIF xmouse% > 295 AND xmouse% < 345 AND ymouse% > 80 AND ymouse% < 130 THEN
        MouseHide                  'Top center + is pressed...
        ButPrs% = 2
        TinyButton 295, 80, 345, 130, 7, 8, 15
        LoadFont "+", 314, 90, 10, 8

        ELSEIF xmouse% > 295 AND xmouse% < 345 AND ymouse% > 310 AND ymouse% < 360 THEN
        MouseHide                  'Top center - is pressed...
        ButPrs% = 3
        TinyButton 295, 310, 345, 360, 7, 8, 15
        LoadFont "-", 316, 320, 10, 8
      
        ELSEIF xmouse% > 405 AND xmouse% < 455 AND ymouse% > 195 AND ymouse% < 245 THEN
        MouseHide                  'Top left + is pressed...
        ButPrs% = 4
        TinyButton 405, 195, 455, 245, 7, 8, 15
        LoadFont "+", 424, 205, 10, 8
       
        ELSEIF xmouse% > 185 AND xmouse% < 235 AND ymouse% > 195 AND ymouse% < 245 THEN
        MouseHide                  'Top right - is pressed...
        ButPrs% = 5
        TinyButton 185, 195, 235, 245, 7, 8, 15
        LoadFont "-", 206, 205, 10, 8
            
        ELSEIF xmouse% > 20 AND xmouse% < 140 AND ymouse% > 430 AND ymouse% < 460 THEN
        MouseHide                  'Bottom default button is pressed...
        ButPrs% = 6
        TinyButton 20, 430, 140, 460, 7, 0, 15
        LoadFont "Default value", 40, 436, 0, 4

        ELSEIF xmouse% > 200 AND xmouse% < 240 AND ymouse% > 410 AND ymouse% < 439 THEN
        MouseHide                  'Bottom + button is pressed...
        ButPrs% = 7
        TinyButton 200, 410, 240, 439, 7, 8, 15
        LoadFont "+", 214, 410, 10, 8

        ELSEIF xmouse% > 200 AND xmouse% < 240 AND ymouse% > 450 AND ymouse% < 479 THEN
        MouseHide                  'Bottom - button is pressed...
        ButPrs% = 8
        TinyButton 200, 450, 240, 479, 7, 8, 15
        LoadFont "-", 216, 450, 10, 8

        ELSEIF xmouse% > 550 AND xmouse% < 630 AND ymouse% > 310 AND ymouse% < 360 THEN
        MouseHide                  'Save Config button is pressed...
        ButPrs% = 9
        TinyButton 550, 310, 630, 360, 7, 8, 15
        LoadFont "Save Config", 556, 325, 0, 4

        ELSEIF xmouse% > 550 AND xmouse% < 630 AND ymouse% > 370 AND ymouse% < 420 THEN
        MouseHide                  'Cancel button is pressed...
        ButPrs% = 10
        TinyButton 550, 370, 630, 420, 7, 8, 15
        LoadFont "Cancel", 571, 385, 0, 4
        END IF
    
      MouseShow
      LeftClick% = LeftClick% + 1
        IF LeftClick% = 1 THEN OldTime! = TIMER
      ButDown% = 1
     
      END IF

  
    CASE 0
    
     IF LeftClick% = 2 AND ((TIMER - OldTime!) < ClkDelay% / 1000) THEN
       LeftClick% = 0
       DoubleClick% = 1
       GOSUB OpenIt
     ELSEIF ((TIMER - OldTime!) > ClkDelay% / 1000) THEN
       LeftClick% = 0
     END IF
   
    ButDown% = 0
    
      IF ButPrs% > 0 THEN
      MouseHide
      
        IF ButPrs% = 1 THEN                      'Top Default is released
        TinyButton 20, 130, 140, 160, 7, 15, 0
        LoadFont "Default value", 40, 135, 0, 4
        XmsSpd% = DefaultX
        YmsSpd% = DefaultY
        UpdateSpeed

        ELSEIF ButPrs% = 2 THEN                  'Top center + is released
        TinyButton 295, 80, 345, 130, 7, 15, 8
        LoadFont "+", 313, 90, 10, 8
        YmsSpd% = YmsSpd% - 1
        UpdateSpeed

        ELSEIF ButPrs% = 3 THEN                  'Top center - is released
        TinyButton 295, 310, 345, 360, 7, 15, 8
        LoadFont "-", 315, 320, 10, 8
        YmsSpd% = YmsSpd% + 1
        UpdateSpeed

        ELSEIF ButPrs% = 4 THEN                  'Top left + is released
        TinyButton 405, 195, 455, 245, 7, 15, 8
        LoadFont "+", 423, 205, 10, 8
        XmsSpd% = XmsSpd% - 1
        UpdateSpeed

        ELSEIF ButPrs% = 5 THEN                  'Top left - is released
        TinyButton 185, 195, 235, 245, 7, 15, 8
        LoadFont "-", 205, 205, 10, 8
        XmsSpd% = XmsSpd% + 1
        UpdateSpeed

        ELSEIF ButPrs% = 6 THEN                  'Bottom default button is released
        TinyButton 20, 430, 140, 460, 7, 15, 0
        LoadFont "Default value", 40, 435, 0, 4
        ClkDelay% = DefaultClick
        UpdateClick

        ELSEIF ButPrs% = 7 THEN                  'Bottom + button is released
        TinyButton 200, 410, 240, 439, 7, 15, 8
        LoadFont "+", 213, 410, 10, 8
        ClkDelay% = ClkDelay% + 50
        UpdateClick

        ELSEIF ButPrs% = 8 THEN                  'Bottom - button is released
        TinyButton 200, 450, 240, 479, 7, 15, 8
        LoadFont "-", 215, 450, 10, 8
        ClkDelay% = ClkDelay% - 50
        UpdateClick

        ELSEIF ButPrs% = 9 THEN                  'Save Config is released
        TinyButton 550, 310, 630, 360, 7, 15, 8
        LoadFont "Save Config", 555, 325, 0, 4
        WriteINI
        EXIT DO

        ELSEIF ButPrs% = 10 THEN                  'Cancel is released
        TinyButton 550, 370, 630, 420, 7, 15, 8
        LoadFont "Cancel", 570, 385, 0, 4
        EXIT DO
        END IF
    
      ButPrs% = 0
      MouseShow
      END IF

    END SELECT

  LOOP


finish:
SCREEN 0
CLS
END

'===================================SUBS=====================================
OpenIt:
 
  IF DoubleClick% = 1 THEN
   
    IF xmouse% > 260 AND xmouse% < 380 AND ymouse% > 410 AND ymouse% < 460 THEN
    MouseHide
    LINE (261, 411)-(379, 459), 7, BF
    LoadFont "RULEZ!!!", 295, 425, 13, 4
    SLEEP 1
    LINE (261, 411)-(379, 459), 7, BF
    LoadFont "WinDos", 295, 425, 10, 4
    MouseShow
    END IF
 
  DoubleClick% = 0
  END IF

RETURN
'================================Mouse data==================================

DATA 55,89,E5,8B,5E,0C,8B,07,50,8B,5E,0A,8B,07,50,8B
DATA 5E,08,8B,0F,8B,5E,06,8B,17,5B,58,1E,07,CD,33,53
DATA 8B,5E,0C,89,07,58,8B,5E,0A,89,07,8B,5E,08,89,0F
DATA 8B,5E,06,89,17,5D,CA,08,00
RESTORE

SUB Interface

LINE (0, 0)-(640, 480), 7, BF

LoadFont id$, 145, 10, 14, 4
LoadFont FinishDate$, 270, 25, 14, 4

'Mouse speed text
LoadFont "Mouse speed control:", 20, 80, 0, 4

'Default button
TinyButton 20, 130, 140, 160, 7, 15, 0
LoadFont "Default value", 40, 135, 0, 4

'Top, bottom  buttons
TinyButton 295, 80, 345, 130, 7, 15, 8
LoadFont "+", 313, 90, 10, 8
TinyButton 295, 310, 345, 360, 7, 15, 8
LoadFont "-", 315, 320, 10, 8

'Left, right buttons
TinyButton 405, 195, 455, 245, 7, 15, 8
LoadFont "+", 423, 205, 10, 8
TinyButton 185, 195, 235, 245, 7, 15, 8
LoadFont "-", 205, 205, 10, 8

'Center button
TinyButton 295, 195, 345, 245, 7, 15, 8
LoadFont "X =" + STR$(XmsSpd%), 305, 200, 12, 4
LoadFont "Y =" + STR$(YmsSpd%), 305, 220, 12, 4

'Double click text
LoadFont "Double Click delay (in milliseconds):", 20, 380, 0, 4

'Default button
TinyButton 20, 430, 140, 460, 7, 15, 0
LoadFont "Default value", 40, 435, 0, 4

LoadFont STR$(ClkDelay%), 40, 400, 12, 4

TinyButton 200, 410, 240, 439, 7, 15, 8
LoadFont "+", 213, 410, 10, 8
TinyButton 200, 450, 240, 479, 7, 15, 8
LoadFont "-", 215, 450, 10, 8

LINE (260, 410)-(380, 460), 0, B
LoadFont "WinDos", 295, 425, 10, 4
LoadFont "Test Window", 282, 460, 0, 4

TinyButton 550, 310, 630, 360, 7, 15, 8
LoadFont "Save Config", 555, 325, 0, 4

TinyButton 550, 370, 630, 420, 7, 15, 8
LoadFont "Cancel", 570, 385, 0, 4

END SUB

SUB LoadFont (text$, textx%, texty%, colour%, size%)

x% = textx%
y% = texty% + 5
clr% = colour%
bold% = 1
DRAW "s" + LTRIM$(STR$(size%))

real.bold% = bold%

  WHILE bold% > -1
  DRAW "BM " + LTRIM$(STR$(x%)) + "," + LTRIM$(STR$(y%)) + " C" + LTRIM$(STR$(clr%))
    FOR k% = 1 TO LEN(text$)
    char$ = MID$(text$, k%, 1)
    DRAW WindosFont$(ASC(char$))
      IF real.bold% > 0 THEN DRAW "BR1"
    NEXT k%
  bold% = bold% - 1
  x% = x% + 1
  WEND

END SUB

SUB MouseDriver (ax%, bx%, cx%, dx%)

DEF SEG = VARSEG(Mouse$)
Mouse% = SADD(Mouse$)
CALL Absolute(ax%, bx%, cx%, dx%, Mouse%)

END SUB

SUB MouseHide

ax% = 2
MouseDriver ax%, 0, 0, 0

END SUB

FUNCTION MouseInit%

ax% = 0
MouseDriver ax%, 0, 0, 0
MouseInit% = ax%

END FUNCTION

SUB MousePut (x%, y%)

ax% = 4
cx% = x%
dx% = y%
MouseDriver ax%, 0, cx%, dx%

END SUB

SUB MouseRange (x1%, y1%, x2%, y2%)

ax% = 7
cx% = x1%
dx% = x2%
MouseDriver ax%, 0, cx%, dx%

ax% = 8
cx% = y1%
dx% = y2%
MouseDriver ax%, 0, cx%, dx%

END SUB

SUB MouseShow

ax% = 1
MouseDriver ax%, 0, 0, 0

END SUB

SUB MouseSpeed (xspeed%, yspeed%)

ax% = 15
cx% = xspeed%
dx% = yspeed%

MouseDriver ax%, 0, cx%, dx%

END SUB

SUB MouseStatus (Lb%, Rb%, xmouse%, ymouse%)
 
ax% = 3
MouseDriver ax%, bx%, cx%, dx%
Lb% = ((bx% AND 1) <> 0)
Rb% = ((bx% AND 2) <> 0)
xmouse% = cx%
ymouse% = dx%

END SUB

SUB ReadINI

File% = FREEFILE
OPEN MainINI FOR INPUT AS #File%

  DO UNTIL EOF(File%) = -1
  LINE INPUT #File%, something$
  something$ = LTRIM$(RTRIM$(something$))
   
    IF LEFT$(UCASE$(something$), 5) = "FONTS" THEN
    GOSUB lookspace
    FontsPath$ = MID$(something$, equal%, LEN(something$) - equal% + 1)
   
    ELSEIF LEFT$(UCASE$(something$), 6) = "XMOUSE" THEN
    GOSUB lookspace
    XmsSpd% = VAL(MID$(something$, equal%, LEN(something$) - equal% + 1))

    ELSEIF LEFT$(UCASE$(something$), 6) = "YMOUSE" THEN
    GOSUB lookspace
    YmsSpd% = VAL(MID$(something$, equal%, LEN(something$) - equal% + 1))
   
    ELSEIF LEFT$(UCASE$(something$), 11) = "DOUBLECLICK" THEN
    GOSUB lookspace
    ClkDelay! = VAL(MID$(something$, equal%, LEN(something$) - equal% + 1))
    ClkDelay% = ClkDelay! * 1000
   
    END IF
 
  LOOP

CLOSE #File%
EXIT SUB

'----------------------------------------------------------------------------
lookspace:
equal% = INSTR(something$, "=")
  DO
  equal% = equal% + 1
  character$ = MID$(something$, equal%, 1)
  LOOP WHILE character$ = " "
RETURN

END SUB

SUB TinyButton (x%, y%, x2%, y2%, colour%, topcol%, botcol%)

LINE (x%, y%)-(x2%, y2%), colour%, BF      'Drawing main rectangle...

LINE (x%, y%)-(x2%, y%), topcol%
LINE (x%, y%)-(x%, y2%), topcol%

LINE (x2%, y%)-(x2%, y2%), botcol%
LINE (x%, y2%)-(x2%, y2%), botcol%

END SUB

SUB UpdateClick

  IF ClkDelay% < 0 THEN ClkDelay% = 0
  IF ClkDelay% > ClickMax THEN ClkDelay% = ClickMax

LINE (30, 400)-(80, 420), 7, BF
LoadFont STR$(ClkDelay%), 40, 400, 12, 4

END SUB

SUB UpdateSpeed

  IF XmsSpd% < 0 THEN XmsSpd% = 0
  IF XmsSpd% > XMsMax THEN XmsSpd% = XMsMax
  IF YmsSpd% < 0 THEN YmsSpd% = 0
  IF YmsSpd% > YMsMax THEN YmsSpd% = YMsMax

LINE (296, 196)-(344, 244), 7, BF
LoadFont "X =" + STR$(XmsSpd%), 305, 200, 12, 4
LoadFont "Y =" + STR$(YmsSpd%), 305, 220, 12, 4

MouseSpeed XmsSpd%, YmsSpd%

END SUB

SUB WriteINI

File1% = FREEFILE
OPEN MainINI FOR INPUT AS #File1%

File2% = FREEFILE
OPEN TempINI FOR OUTPUT AS #File2%

  DO UNTIL EOF(File1%)
  LINE INPUT #File1%, something$
  something$ = LTRIM$(RTRIM$(something$))
  
    IF LEFT$(UCASE$(something$), 6) = "XMOUSE" THEN
    PRINT #File2%, "xMouse =" + STR$(XmsSpd%)

    ELSEIF LEFT$(UCASE$(something$), 6) = "YMOUSE" THEN
    PRINT #File2%, "yMouse =" + STR$(YmsSpd%)
  
    ELSEIF LEFT$(UCASE$(something$), 11) = "DOUBLECLICK" THEN
    PRINT #File2%, "DoubleClick =" + STR$(ClkDelay% / 1000)
  
    ELSE
    PRINT #File2%, something$
    END IF

  LOOP


CLOSE #File1%
CLOSE #File2%

KILL MainINI
NAME TempINI AS MainINI

END SUB

