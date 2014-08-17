DEFINT A-Z
DECLARE SUB ShowTime ()
DECLARE SUB mouse (CX, dx, bx)
DECLARE SUB MOUSEPOINTER (SW)
DECLARE SUB DrawClock ()
DECLARE SUB DrawClock2 ()
DECLARE SUB DrawClock3 ()
DECLARE SUB DrawClock3b ()
DECLARE SUB Show ()
DECLARE SUB Box ()

CLEAR , , 9000

DIM ELVIS(40, 40)
DIM SHARED A(9)                 'Set up array for code
DEF SEG = VARSEG(A(0))          'Get array segment (nnnn:    )
                                 '    (two 8 bit)
    FOR I = 0 TO 17                 'length of DATA to
       READ R                       'read
       POKE VARPTR(A(0)) + I, R     'into array/2 (nnnn:iiii) (one 8 bit)
    NEXT I                          'until 17


'**************************** Machine Code *********************************

DATA &HB8,&H00,&H00   :   ' mov  AX,[n]       [Swap code-(L),(H)] in AX
DATA &H55             :   ' push BP           Save BP
DATA &H8B,&HEC        :   ' mov  BP,SP        Get BP to c Seg
DATA &HCD,&H33        :   ' int  33           Interrupt 33
DATA &H92             :   ' xchg AX,[reg]     [Swap code-reg] in AX
DATA &H8B,&H5E,&H06   :   ' mov  BX,[BP+6]    Point to (variable)
DATA &H89,&H07        :   ' mov  [BX],AX      Put AX in (variable)
DATA &H5D             :   ' pop  BP           Restore BP
DATA &HCA,&H02,&H00   :   ' ret  2            Far return

SCREEN 13
'****************************** Mouse set up ******************************
          
                CALL MOUSEPOINTER(0)      'Reset mouse and
                CALL MOUSEPOINTER(1)      'turn pointer on
                CALL MOUSEPOINTER(3)      'Get coordinates

'********************************** TONES **********************************
Cuckoo$ = "T120L16MLO3BF#"
UpScale2$ = "T120L64MSO2E.A.>D.G.>C.F.B."
BobWhite$ = "T120L16O4C#.P16T255L64mlC#DD#EFF#GG#AA#BO5CC#DD#EF"
Whung$ = "T255L32mlO3CD<CD>>CD<<CD>>CD<<CD>CD"
Fweet$ = "T255L64MsO4C#DD#EFF#GG#AA#B>CC#DD#EF"


KEY(2) ON: ON KEY(2) GOSUB MMENU
KEY(3) ON: ON KEY(3) GOSUB APPMENU
CLS
PSET (16, 7)
DRAW "c161nr2dnrlnd2rdc66rc1dc66nrl1drgdc15nenhr3c66rlc15l3"
DRAW "l3c66lrc15r3dnrnldnrnld"
DRAW "c16nrnlc44dc1nrld3c161nluc1u2r2d3c161nr "
GET (9, 5)-(24, 22), ELVIS
CLS
'****************************** P R O G R A M ******************************
START:

SWDC = 0                                   'CLOCKFACE SWITCH
SWCT = 0                                   'CLOCKFACE TIME
SWSNZ = 0                                  'SNOOZE SWITCH
SWBUZ = 0                                  'BUZZER SWITCH
SWELVIS = 0


ONOFF = 0
BUZTONE = 0
ALRMPRT = 0



TOP:                                       'DRAW CLOCKFACK
CLS
CALL MOUSEPOINTER(2)                       'SETUP MOUSE
IF SWDC = 0 THEN CALL DrawClock
IF SWDC = 1 THEN CALL DrawClock2

CALL MOUSEPOINTER(1)
CALL MOUSEPOINTER(3)

BEGIN:                                  'START MAIN LOOP

IF SWDC = 0 THEN X% = 13
IF SWDC = 0 THEN Y% = 24
IF SWDC = 1 THEN X% = 13
IF SWDC = 1 THEN Y% = 24

ALARMSET$ = SET1$ + ":" + SET2$ + " " + AMPM$
COLOR 39: LOCATE X%, Y%: PRINT ALARMSET$


DO
SWELVIS = SWELVIS + 1

GOSUB CALLTIME
CALL mouse(CX, dx, bx)


                         REM ********* CHECK ALARM

IF AMPM$ = "AM" THEN GOTO TWO              'COMPARE TIME TO ALARM SETTINGS
   
ONE:
  
             HOUR% = VAL(LEFT$(ALARMSET$, 2))
              IF HOUR% = 12 THEN GOTO NOON
                HOUR% = HOUR% + 12
                HOUR$ = STR$(HOUR%)
                HOUR$ = RIGHT$(HOUR$, LEN(HOUR$) - 1)
                HOUR$ = RIGHT$("  " + HOUR$, 2)
          ALARMSET$ = HOUR$ + ":" + SET2$

         SET$ = TIME$
         FIRST$ = LEFT$(SET$, 2)
         SECOND$ = MID$(SET$, 4, 2)
         ALAR$ = FIRST$ + ":" + SECOND$
         IF ALARMSET$ = ALAR$ AND SWSNZ = 0 AND BUZTONE = 0 THEN GOSUB WAKEUP
         IF ALARMSET$ = ALAR$ AND SWSNZ = 0 AND BUZTONE = 1 THEN GOSUB WAKEUP2
      
GOTO GETOUT

TWO:
     
             HOUR% = VAL(LEFT$(ALARMSET$, 2))
                IF HOUR% > 9 THEN GOTO CONV
                HOUR$ = STR$(HOUR%)
                HOUR$ = RIGHT$(HOUR$, LEN(HOUR$) - 1)
                HOUR$ = RIGHT$("" + HOUR$, 2)
                ALARMSET$ = "0" + HOUR$ + ":" + SET2$
  
         SET$ = TIME$
         FIRST$ = LEFT$(SET$, 2)
         SECOND$ = MID$(SET$, 4, 2)
         ALAR$ = FIRST$ + ":" + SECOND$
         IF ALARMSET$ = ALAR$ AND SWSNZ = 0 AND BUZTONE = 0 THEN GOSUB WAKEUP
         IF ALARMSET$ = ALAR$ AND SWSNZ = 0 AND BUZTONE = 1 THEN GOSUB WAKEUP2

         GOTO GETOUT
CONV:
    
             HOUR% = VAL(LEFT$(ALARMSET$, 2))
              IF HOUR% = 12 THEN GOTO MIDNIGHT
                HOUR$ = STR$(HOUR%)
                HOUR$ = RIGHT$(HOUR$, LEN(HOUR$) - 1)
                HOUR$ = RIGHT$("" + HOUR$, 2)
                ALARMSET$ = HOUR$ + ":" + SET2$
   
         SET$ = TIME$
         FIRST$ = LEFT$(SET$, 2)
         SECOND$ = MID$(SET$, 4, 2)
         ALAR$ = FIRST$ + ":" + SECOND$
         IF ALARMSET$ = ALAR$ AND SWSNZ = 0 AND BUZTONE = 0 THEN GOSUB WAKEUP
         IF ALARMSET$ = ALAR$ AND SWSNZ = 0 AND BUZTONE = 1 THEN GOSUB WAKEUP2

      GOTO GETOUT

NOON:
         ALARMSET$ = SET1$ + ":" + SET2$
         SET$ = TIME$
         FIRST$ = LEFT$(SET$, 2)
         SECOND$ = MID$(SET$, 4, 2)
         ALAR$ = FIRST$ + ":" + SECOND$
         IF ALARMSET$ = ALAR$ AND SWSNZ = 0 AND BUZTONE = 0 THEN GOSUB WAKEUP
         IF ALARMSET$ = ALAR$ AND SWSNZ = 0 AND BUZTONE = 1 THEN GOSUB WAKEUP2

      GOTO GETOUT

MIDNIGHT:
      
         HOUR$ = "00"
         ALARMSET$ = HOUR$ + ":" + SET2$
         SET$ = TIME$
         FIRST$ = LEFT$(SET$, 2)
         SECOND$ = MID$(SET$, 4, 2)
         ALAR$ = FIRST$ + ":" + SECOND$
         IF ALARMSET$ = ALAR$ AND SWSNZ = 0 AND BUZTONE = 0 THEN GOSUB WAKEUP
         IF ALARMSET$ = ALAR$ AND SWSNZ = 0 AND BUZTONE = 1 THEN GOSUB WAKEUP2

      GOTO GETOUT

GETOUT:




IF bx = 1 THEN
IF dx > 191 AND dx < 239 THEN                  'SET ALARM BUTTON
IF CX > 71 AND CX < 79 THEN
GOSUB SetAlarm
END IF
END IF
END IF

IF bx = 1 THEN
IF dx > 270 AND dx < 295 THEN                 'SHUT OFF ALARM BUTTON
IF CX > 110 AND CX < 120 THEN
GOTO SHUTOFF
END IF
END IF
END IF
                       
IF bx = 1 THEN
IF dx > 143 AND dx < 151 THEN                 'TONE ON
IF CX > 119 AND CX < 127 THEN
BUZTONE = 0
END IF
END IF
END IF

IF bx = 1 THEN
IF dx > 159 AND dx < 167 THEN
IF CX > 119 AND CX < 127 THEN                 'TUNE ON
BUZTONE = 1
END IF
END IF
END IF

IF bx = 1 THEN
IF dx > 135 AND dx < 191 THEN
IF CX > 0 AND CX < 8 THEN                     'GOTO OPTIONS
GOSUB OPTIONS
END IF
END IF
END IF

IF bx = 1 THEN
IF dx > 263 AND dx < 319 THEN
IF CX > 0 AND CX < 8 THEN                     'GRAPHIC
GOSUB GRAPHIC
END IF
END IF
END IF

IF bx = 1 THEN
IF dx > 63 AND dx < 103 THEN
IF CX > 71 AND CX < 79 THEN                     'RADIO
GOSUB RADIO
END IF
END IF
END IF



IF SWDC = 0 THEN GOSUB FaceButtons
IF SWDC = 1 THEN GOSUB FaceButtons
                   
IF SWELVIS = 30000 THEN GOSUB THEKING
LOOP UNTIL INKEY$ = CHR$(27)
END

THEKING:
SWELVIS = 0
YN = INT(RND * 3) + 1
IF YN = 1 GOTO BEGIN
IF YN = 2 GOTO BEGIN
CALL MOUSEPOINTER(2)
FOR VERT = 54 TO 255
PUT (VERT, 100), ELVIS
FOR T = 1 TO 30000: NEXT T
PUT (VERT, 100), ELVIS, XOR
NEXT VERT
CALL MOUSEPOINTER(1)
CALL MOUSEPOINTER(3)
RETURN


                        REM ************ SET ALARM **********

SetAlarm:
ONOFF = 1
SWSNZ = 0
ALRMPRT = 1

IF SWDC = 0 THEN X% = 13
IF SWDC = 0 THEN Y% = 24
IF SWDC = 1 THEN X% = 13
IF SWDC = 1 THEN Y% = 24


LINE (188, 95)-(257, 104), 0, BF
COLOR 15: LOCATE X%, Y%: INPUT "HOUR"; HR$
SET1$ = HR$
LINE (188, 95)-(257, 104), 0, BF
COLOR 15: LOCATE X%, Y%: INPUT " MIN"; MIN$
SET2$ = MIN$
LINE (188, 95)-(257, 104), 0, BF
COLOR 15: LOCATE X%, Y%: INPUT "AM\PM"; AP$
AMPM$ = AP$
LINE (188, 95)-(257, 104), 0, BF
ALARMSET$ = SET1$ + ":" + SET2$ + " " + AMPM$
COLOR 39: LOCATE X%, Y%: PRINT ALARMSET$
RETURN

FaceButtons:                                  'ON/OFF LIGHTS
IF ONOFF = 0 THEN GOTO ALARMOFF
IF ONOFF = 1 THEN GOTO ALARMON
GOTO FIN

ALARMOFF:
COLOR 10: LOCATE 15, 35: PRINT "OFF"              'CHANGE BUTTON COLORS
COLOR 20: LOCATE 11, 35: PRINT "ON"
IF BUZTONE = 0 THEN GOTO BUZ
IF BUZTONE = 1 THEN GOTO TUNE
GOTO FIN

ALARMON:
COLOR 39: LOCATE 15, 35: PRINT "OFF"              'CHANGE BUTTON COLORS
COLOR 10: LOCATE 11, 35: PRINT "ON"
IF BUZTONE = 0 THEN GOTO BUZ
IF BUZTONE = 1 THEN GOTO TUNE
GOTO FIN

BUZ:
COLOR 10: LOCATE 16, 19: PRINT "B"
COLOR 7: LOCATE 16, 21: PRINT "T"
GOTO FIN

TUNE:
COLOR 7: LOCATE 16, 19: PRINT "B"
COLOR 10: LOCATE 16, 21: PRINT "T"

FIN:
RETURN

                      REM ************ SHOW TIME ************

CALLTIME:
IF SWCT = 0 THEN CALL ShowTime
IF SWCT = 1 THEN CALL ShowTime
RETURN




                    REM ************* GRAPHIC ***************
GRAPHIC:
CALL Show
ALARMSET$ = SET1$ + ":" + SET2$ + " " + AMPM$
LOCATE X%, Y%: COLOR 39: PRINT ALARMSET$

PICOVER:
TIMER OFF
WINDOW
CLS
GOTO TOP

                    REM ************ RADIO ***************
RADIO:
CALL Box
GOTO TOP

                    REM ************ OPTIONS **************
OPTIONS:
COLOR 10
LINE (10, 140)-(313, 190), 0, BF
LOCATE 19, 12:  PRINT "1-CHOOSE BUZZER TONE"     'MESSAGE BOX
LOCATE 21, 16:  PRINT "2-CHOOSE SKIN"
LOCATE 23, 16: INPUT "ENTER CHOICE", A$
IF A$ = "1" GOTO PICKB
IF A$ = "2" GOTO PICKS
GOTO OPTIONS

PICKB:
X$ = ""
LINE (10, 140)-(313, 190), 0, BF                  'SELECT TONE
LOCATE 19, 3: PRINT "0-Space"
LOCATE 20, 3: PRINT "1-Cuckoo"
LOCATE 21, 3: PRINT "2-Upscale"
LOCATE 22, 3: PRINT "3-OH"
LOCATE 23, 3: PRINT "4-Siren"
LOCATE 19, 15: PRINT "5-Fweet"
LOCATE 20, 15: PRINT "6-Waver"
LOCATE 21, 15: PRINT "7-Bob White"
LOCATE 22, 15: PRINT "8-Whung"
LOCATE 23, 15: PRINT "9-Kazango"
LOCATE 19, 26: PRINT "ENTER CHOICE"; X$

GetKeyB:
    X$ = INKEY$
    IF X$ = "" THEN GOTO GetKeyB
 
  IF X$ = "1" THEN                                  'HEAR TONE
    SWBUZ = 1
    PLAY "X" + VARPTR$(Cuckoo$)
  ELSEIF X$ = "2" THEN
    SWBUZ = 2
    PLAY "X" + VARPTR$(UpScale2$)
  ELSEIF X$ = "3" THEN
  SWBUZ = 3
    FOR I = 800 TO 2000 STEP 100        'Oh
      SOUND I, .2
    NEXT
    FOR I = 2000 TO 50 STEP -100
      SOUND I, .2
    NEXT
  ELSEIF X$ = "0" THEN
  SWBUZ = 0
    FOR I = 1000 TO 40 STEP -20         'Space 1
      SOUND I, .2
    NEXT
  ELSEIF X$ = "6" THEN
  SWBUZ = 6
    FOR Z = 1 TO 30 STEP .7             'Waver
      SOUND (SIN(Z) + 20) * 30, .2
    NEXT
  ELSEIF X$ = "7" THEN
  SWBUZ = 7
    PLAY "X" + VARPTR$(BobWhite$)
  ELSEIF X$ = "8" THEN
  SWBUZ = 8
    PLAY "X" + VARPTR$(Whung$)
  ELSEIF X$ = "4" THEN
  SWBUZ = 4
    FOR Y = 1 TO 3                      'siren
      SOUND 550, 9
      SOUND 400, 9
    NEXT
  ELSEIF X$ = "5" THEN
  SWBUZ = 5
    FOR I = 1 TO 2
      PLAY "X" + VARPTR$(Fweet$)
    NEXT
  ELSEIF X$ = "9" THEN
  SWBUZ = 9
    FOR I = 10000 TO 500 STEP -500      'Kazango
      SOUND I, .2
    NEXT
  END IF
LOCATE 21, 27: PRINT "SAVE CHOICE"
LOCATE 23, 31: INPUT "Y\N"; A$
IF A$ = "Y" THEN GOTO FINB
IF A$ = "N" THEN GOTO PICKB
GOTO PICKB

                       REM *************** SKINS *************
PICKS:
COLOR 10
LINE (10, 140)-(313, 190), 0, BF
LOCATE 19, 16:  PRINT "1-TABLE TOP 1"                     'MESSAGE BOX
LOCATE 21, 16:  PRINT "2-TABLE TOP 2"
LOCATE 23, 16: INPUT "ENTER CHOICE", A$
IF A$ = "1" THEN SWDC = 0
IF A$ = "2" THEN SWDC = 1
GOTO TOP



FINB:
CLS
GOTO TOP



                        REM ****** ALARM SOUNDS **********
WAKEUP:
SWSNZ = 1
DO
IF SWBUZ = 1 THEN PLAY "X" + VARPTR$(Cuckoo$)
IF SWBUZ = 2 THEN PLAY "X" + VARPTR$(UpScale2$)
IF SWBUZ = 3 THEN
FOR I = 800 TO 2000 STEP 100         'Oh
      SOUND I, .2
    NEXT
    FOR I = 2000 TO 50 STEP -100
      SOUND I, .2
    NEXT
ELSEIF SWBUZ = 0 THEN
    FOR I = 1000 TO 40 STEP -20         'Space 1
      SOUND I, .2
    NEXT
ELSEIF SWBUZ = 6 THEN
    FOR Z = 1 TO 30 STEP .7             'Waver
      SOUND (SIN(Z) + 20) * 30, .2
    NEXT
ELSEIF SWBUZ = 7 THEN
PLAY "X" + VARPTR$(BobWhite$)
ELSEIF SWBUZ = 8 THEN
PLAY "X" + VARPTR$(Whung$)
ELSEIF SWBUZ = 4 THEN
    FOR Y = 1 TO 3                      'siren
      SOUND 550, 9
      SOUND 400, 9
    NEXT
ELSEIF SWBUZ = 5 THEN
    FOR I = 1 TO 2
      PLAY "X" + VARPTR$(Fweet$)
    NEXT
ELSEIF SWBUZ = 9 THEN
    FOR I = 10000 TO 500 STEP -500      'Kazango
      SOUND I, .2
    NEXT
END IF

LOOP UNTIL INKEY$ <> ""
TIMER ON: ON TIMER(430) GOSUB WAKEUP
COLOR 10: LOCATE 16, 25: PRINT "SNOOZE"
RETURN


                    REM ************ ALARM TUNES **********

WAKEUP2:                                    'ALARM SOUNDS "TUNE"
SWSNZ = 1

    DO WHILE INKEY$ = ""
    PLAY ON
    Music$ = "o3L8ED+ED+Eo2Bo3DCL2o2A"
    PLAY Music$
    LOOP

TIMER ON: ON TIMER(430) GOSUB WAKEUP2
COLOR 10: LOCATE 16, 25: PRINT "SNOOZE"
RETURN


MMENU:
CLOSE #1: RUN "MMENU"

APPMENU:
CLOSE #1: RUN "APPMENU"



                      REM ************* SHUTOFF ALARM *************
SHUTOFF:
COLOR 10: LOCATE 15, 35: PRINT "OFF"               'TURN OFF ALARM
COLOR 20: LOCATE 11, 35: PRINT "ON"
COLOR 7: LOCATE 16, 25: PRINT "SNOOZE"
ONOFF = 0

TIMER OFF: GOTO BEGIN

SUB Box
DIM ELVIS2(40, 40)
CLS
PSET (16, 7)
DRAW "c161nr2dnrlnd2rdc66rc1dc66nrl1drgdc15nenhr3c66rlc15l3"
DRAW "l3c66lrc15r3dnrnldnrnld"
DRAW "c16nrnlc44dc1nrld3c161nluc1u2r2d3c161nr "
GET (9, 5)-(24, 22), ELVIS2


CLS
CALL MOUSEPOINTER(2)
PAINT (160, 100), 130                       'DRAW CLOCKFACE
CIRCLE (50, 100), 40, 229, 1.5, 4.8
CIRCLE (270, 100), 40, 229, 4.8, 1.5
LINE (53, 67)-(272, 67), 229
LINE (53, 133)-(272, 133), 229
PAINT (130, 100), 229
LINE (49, 70)-(269, 130), 5, BF
LINE (54, 85)-(264, 119), 0, BF
COLOR 7: LOCATE 13, 3: PRINT "TIME"
COLOR 7: LOCATE 13, 35: PRINT "RADIO"        'BUTTONS AND STUFF
COLOR 10: LOCATE 10, 9: PRINT "RADIO"
 COLOR 10: LOCATE 15, 8: PRINT "88  92  96  100  104  106"


UP:
DO
CALL MOUSEPOINTER(1)
CALL MOUSEPOINTER(3)
CALL mouse(CX, dx, bx)

CALL ShowTime

IF bx = 1 THEN
IF dx > 63 AND dx < 103 THEN
IF CX > 71 AND CX < 79 THEN
GOSUB SCRAM                            'RADIO OFF
END IF
END IF
END IF

IF bx = 1 THEN
IF dx > 55 AND dx < 254 THEN
IF CX > 111 AND CX < 119 THEN
GOSUB 88                            '88
END IF
END IF
END IF
LOOP UNTIL INKEY$ = CHR$(27)
END

88 :

DO
GOSUB SWEET
  X$ = INKEY$
    IF X$ <> "" THEN GOTO UP
    CALL ShowTime
  GOSUB THEKING2
GOSUB MICHAEL
  X$ = INKEY$
    IF X$ <> "" THEN GOTO UP
    CALL ShowTime
    GOSUB THEKING2
GOSUB yesterday
  X$ = INKEY$
    IF X$ <> "" THEN GOTO UP
    CALL ShowTime
    GOSUB THEKING2
GOSUB entertainer
  X$ = INKEY$
    IF X$ <> "" THEN GOTO UP
    CALL ShowTime
    GOSUB THEKING2
GOSUB mission
  X$ = INKEY$
    IF X$ <> "" THEN GOTO UP
    CALL ShowTime
    GOSUB THEKING2
GOSUB rising
  X$ = INKEY$
    IF X$ <> "" THEN GOTO UP
    CALL ShowTime
    GOSUB THEKING2
GOSUB TWILIGHT
  X$ = INKEY$
    IF X$ <> "" THEN GOTO UP
    CALL ShowTime
    GOSUB THEKING2
GOSUB bog
  X$ = INKEY$
    IF X$ <> "" THEN GOTO UP
    CALL ShowTime
    GOSUB THEKING2

LOOP UNTIL INKEY$ <> ""
  GOTO UP


                     REM **************** TUNES ******************

                                  
mission:
PLAY "T190O0L4MSGGB->C<GGFF+GGB->C<GGFF+O2 MN L8B-G MLL2DL4D L8B-G L2D-L4D- MNML L8B-G L2CL4C MN ML <B->C P2 MN T190O0L4GGB->C<GGFF+GGB->C<GGFF+O2 L8B-G MLL2DL4D L8B-G L2D-L4D- MNML L8B-G L2CL4C MN ML <B->C< P2 MS L4GGB->C<GGFF+GGB->C<GGFF+ MSG"
l = 50
PLAY "T190O0L4MSGGB->C<GGFF+GGB->C<GGFF+O2 MN L8B-G MLL2DL4D L8B-G L2D-L4D- MNML L8B-G L2CL4C MN ML <B->C P2 MN T190O0L4GGB->C<GGFF+GGB->C<GGFF+O2 L8B-G MLL2DL4D L8B-G L2D-L4D- MNML L8B-G L2CL4C MN ML <B->C< P2 MS L4GGB->C<GGFF+GGB->C<GGFF+ MSG"
l = 50
PLAY "T190O0L4MSGGB->C<GGFF+GGB->C<GGFF+O2 MN L8B-G MLL2DL4D L8B-G L2D-L4D- MNML L8B-G L2CL4C MN ML <B->C P2 MN T190O0L4GGB->C<GGFF+GGB->C<GGFF+O2 L8B-G MLL2DL4D L8B-G L2D-L4D- MNML L8B-G L2CL4C MN ML <B->C< P2 MS L4GGB->C<GGFF+GGB->C<GGFF+ MSG"
l = 50
RETURN

rising:
PLAY "T100L4O1MNA>L2DL8EDL2FL8GG+L2AL8FEL2DL4A>L2D<L4A>L2C<L8AGL1AP4P8L8A>L2D<L4DL2FL4GL8AAL4AL8FEL2DL4DL2AL4AL8A<L4AP8>L8FEL1D"
l = 160
PLAY "T100L4O1MNA>L2DL8EDL2FL8GG+L2AL8FEL2DL4A>L2D<L4A>L2C<L8AGL1AP4P8L8A>L2D<L4DL2FL4GL8AAL4AL8FEL2DL4DL2AL4AL8A<L4AP8>L8FEL1D"
l = 160
PLAY "T100L4O1MNA>L2DL8EDL2FL8GG+L2AL8FEL2DL4A>L2D<L4A>L2C<L8AGL1AP4P8L8A>L2D<L4DL2FL4GL8AAL4AL8FEL2DL4DL2AL4AL8A<L4AP8>L8FEL1D"
l = 160
RETURN

yesterday:
PLAY "t120mno2l8gfl2fp4p4l8ab>c+def mlel16emndl2d p4p4l8ddc<b-agl4b-l8a mlal4amngfl8a mlgl4gmnd fl8al2ap4 mno2l8gfl2fp4p4l8ab>c+def mlel16emndl2d p4p4l8ddc<b-agl4b-l8a mlal4amngfl8a mlgl4gmnd fl8al2ap4l2aal4>defl8edep8dl4cd<l1al2aa>l4defl8ed l4ep4l8dl4cefc<b-al8gfl2fp4p4l8ab>c+defep16l16dl2dp4p4l8ddc<b-agl4b-l8a mlal4amngfl8a mlgl4gmndfl8al2ap8l4fagdfl8al1a"
l = 270
PLAY "t120mno2l8gfl2fp4p4l8ab>c+def mlel16emndl2d p4p4l8ddc<b-agl4b-l8a mlal4amngfl8a mlgl4gmnd fl8al2ap4 mno2l8gfl2fp4p4l8ab>c+def mlel16emndl2d p4p4l8ddc<b-agl4b-l8a mlal4amngfl8a mlgl4gmnd fl8al2ap4l2aal4>defl8edep8dl4cd<l1al2aa>l4defl8ed l4ep4l8dl4cefc<b-al8gfl2fp4p4l8ab>c+defep16l16dl2dp4p4l8ddc<b-agl4b-l8a mlal4amngfl8a mlgl4gmndfl8al2ap8l4fagdfl8al1a"
l = 270
PLAY "t120mno2l8gfl2fp4p4l8ab>c+def mlel16emndl2d p4p4l8ddc<b-agl4b-l8a mlal4amngfl8a mlgl4gmnd fl8al2ap4 mno2l8gfl2fp4p4l8ab>c+def mlel16emndl2d p4p4l8ddc<b-agl4b-l8a mlal4amngfl8a mlgl4gmnd fl8al2ap4l2aal4>defl8edep8dl4cd<l1al2aa>l4defl8ed l4ep4l8dl4cefc<b-al8gfl2fp4p4l8ab>c+defep16l16dl2dp4p4l8ddc<b-agl4b-l8a mlal4amngfl8a mlgl4gmndfl8al2ap8l4fagdfl8al1a"
l = 270
RETURN


entertainer:
PLAY "t180mno4l8dec<l4al8bl4gl8dec<l4al8bl4gl8dec<l4al8b>c<a-l4gp4>>l4g<l8dd+e>l4c<l8e>l4c<l8e>l2cp4>l8cdd+ecdl4el8<b>l4dl2cp4<<l8dd+e>l4c<l8e>l4c<l8e>l1c <l8agf+a>cl4el8dc<a>l2dp4<l8dd+e>l4c<l8e>l4c<l8e>l2c >p4p8l8cdd+ecdl4el8<b>l4dl2c p4l8cdecdl4el8cdcecdl4el8cdcecdl4el8<b>l4dc"
l = 290
PLAY "t180mno4l8dec<l4al8bl4gl8dec<l4al8bl4gl8dec<l4al8b>c<a-l4gp4>>l4g<l8dd+e>l4c<l8e>l4c<l8e>l2cp4>l8cdd+ecdl4el8<b>l4dl2cp4<<l8dd+e>l4c<l8e>l4c<l8e>l1c <l8agf+a>cl4el8dc<a>l2dp4<l8dd+e>l4c<l8e>l4c<l8e>l2c >p4p8l8cdd+ecdl4el8<b>l4dl2c p4l8cdecdl4el8cdcecdl4el8cdcecdl4el8<b>l4dc"
l = 290
PLAY "t180mno4l8dec<l4al8bl4gl8dec<l4al8bl4gl8dec<l4al8b>c<a-l4gp4>>l4g<l8dd+e>l4c<l8e>l4c<l8e>l2cp4>l8cdd+ecdl4el8<b>l4dl2cp4<<l8dd+e>l4c<l8e>l4c<l8e>l1c <l8agf+a>cl4el8dc<a>l2dp4<l8dd+e>l4c<l8e>l4c<l8e>l2c >p4p8l8cdd+ecdl4el8<b>l4dl2c p4l8cdecdl4el8cdcecdl4el8cdcecdl4el8<b>l4dc"
l = 290
RETURN

bog:
PLAY "t120mfmso1l16ccecgcacb-cacgce>c<ccecgcacb-cacgce>c<ffaf>c<f>d<f>e-<f>d<f>c<d>a<f> mfmso1l16ccecgcacb-cacgce>c mfmso1l16ggbg>d<g>e<g>f<g>e<g>d<g>b>g<<ffaf>c<f>d<f>e-<f>d<f>c<d>a>f< mfmso1l16ccecgcacb-cacgceg>c<gc>gl8c"
l = 350
PLAY "t120mfmso1l16ccecgcacb-cacgce>c<ccecgcacb-cacgce>c<ffaf>c<f>d<f>e-<f>d<f>c<d>a<f> mfmso1l16ccecgcacb-cacgce>c mfmso1l16ggbg>d<g>e<g>f<g>e<g>d<g>b>g<<ffaf>c<f>d<f>e-<f>d<f>c<d>a>f< mfmso1l16ccecgcacb-cacgceg>c<gc>gl8c"
l = 350
PLAY "t120mfmso1l16ccecgcacb-cacgce>c<ccecgcacb-cacgce>c<ffaf>c<f>d<f>e-<f>d<f>c<d>a<f> mfmso1l16ccecgcacb-cacgce>c mfmso1l16ggbg>d<g>e<g>f<g>e<g>d<g>b>g<<ffaf>c<f>d<f>e-<f>d<f>c<d>a>f< mfmso1l16ccecgcacb-cacgceg>c<gc>gl8c"
l = 350
RETURN

MICHAEL:
PLAY "O3 L4FA MS L2ML>C<L8AL8>CDL2CL4<A>CL1DL2CL4<A>CMS L2MLC<L8A> L4C<AL2GL4FGL2AGF"
PLAY "O3 L4 FA MS L2ML>C<L8AL8>CDL2CL4<A>CL1DL2CL4<A>CMS L2MLC<L8A> L4C<AL2GL4FGL2AGF"
PLAY "O3 L4FA MS L2ML>C<L8AL8>CDL2CL4<A>CL1DL2CL4<A>CMS L2MLC<L8A> L4C<AL2GL4FGL2AGF"
PLAY "O3 L4 FA MS L2ML>C<L8AL8>CDL2CL4<A>CL1DL2CL4<A>CMS L2MLC<L8A> L4C<AL2GL4FGL2AGF"
RETURN

TWILIGHT:
FOR ZZ = 1 TO 4
    Count = 0
    FOR Snd = 500 TO 100 STEP -200
        SOUND Snd, 4
    NEXT Snd
    FOR Bloops = 1 TO 42
        Snnd = (RND * 2400)
        IF Snnd < 37 OR Snnd > 3700 THEN Snnd = 250
        Snd = (RND * 66) + 37
        IF Count = 6 THEN SOUND Snnd, 3: Count = 0
        SOUND Snd, 4
        Count = Count + 1
    NEXT Bloops
    FOR Snd = 100 TO 500 STEP 200
        SOUND Snd, 4
    NEXT Snd
    FOR X = 1 TO 3
        FOR Y = 1 TO 3
            SOUND 1050, .13: SOUND 1350, 3.5
            SOUND 1150, .13: SOUND 1450, 3.5
            SOUND 1050, .13: SOUND 1350, 3.5
            SOUND 850, .13: SOUND 1150, 3.5
        NEXT Y
        FOR Z = 1 TO 5
            Svd = (RND * 550) + 37
            SOUND Svd, 1.7
        NEXT Z
    NEXT X
    FOR I = 1 TO 5
        Svd = (RND * 550) + 37
        SOUND Svd, 1.7
    NEXT I
    FOR Snd = 175 TO 37 STEP -10
        SOUND Snd, 1
    NEXT Snd
    SOUND 37, 10
NEXT ZZ
RETURN

SWEET:
FOR I = 1 TO 2
SOUND 284, 3: SOUND 586, 3
SOUND 426, 3: SOUND 379, 3
SOUND 758, 3: SOUND 426, 3
SOUND 716, 3: SOUND 426, 3
SOUND 284, 3: SOUND 568, 3
SOUND 426, 3: SOUND 379, 3
SOUND 758, 3: SOUND 426, 3
SOUND 716, 3: SOUND 426, 3
SOUND 319, 3: SOUND 568, 3
SOUND 426, 3: SOUND 379, 3
SOUND 758, 3: SOUND 426, 3
SOUND 716, 3: SOUND 426, 3
SOUND 319, 3: SOUND 568, 3
SOUND 426, 3: SOUND 379, 3
SOUND 758, 3: SOUND 426, 3
SOUND 716, 3: SOUND 426, 3
SOUND 379, 3: SOUND 568, 3
SOUND 426, 3: SOUND 379, 3
SOUND 758, 3: SOUND 426, 3
SOUND 716, 3: SOUND 426, 3
SOUND 379, 3: SOUND 568, 3
SOUND 426, 3: SOUND 379, 3
SOUND 758, 3: SOUND 426, 3
SOUND 716, 3: SOUND 426, 3
SOUND 284, 3: SOUND 586, 3
SOUND 426, 3: SOUND 379, 3
SOUND 758, 3: SOUND 426, 3
SOUND 716, 3: SOUND 426, 3
SOUND 284, 3: SOUND 568, 3
SOUND 426, 3: SOUND 379, 3
SOUND 758, 3: SOUND 426, 3
SOUND 716, 3: SOUND 426, 3

SOUND 284, 3: SOUND 586, 3
SOUND 426, 3: SOUND 379, 3
SOUND 758, 3: SOUND 426, 3
SOUND 716, 3: SOUND 426, 3
SOUND 284, 3: SOUND 568, 3
SOUND 426, 3: SOUND 379, 3
SOUND 758, 3: SOUND 426, 3
SOUND 716, 3: SOUND 426, 3
SOUND 319, 3: SOUND 568, 3
SOUND 426, 3: SOUND 379, 3
SOUND 758, 3: SOUND 426, 3
SOUND 716, 3: SOUND 426, 3
SOUND 319, 3: SOUND 568, 3
SOUND 426, 3: SOUND 379, 3
SOUND 758, 3: SOUND 426, 3
SOUND 716, 3: SOUND 426, 3
SOUND 379, 3: SOUND 568, 3
SOUND 426, 3: SOUND 379, 3
SOUND 758, 3: SOUND 426, 3
SOUND 716, 3: SOUND 426, 3
SOUND 379, 3: SOUND 568, 3
SOUND 426, 3: SOUND 379, 3
SOUND 758, 3: SOUND 426, 3
SOUND 716, 3: SOUND 426, 3
SOUND 284, 3: SOUND 586, 3
SOUND 426, 3: SOUND 379, 3
SOUND 758, 3: SOUND 426, 3
SOUND 716, 3: SOUND 426, 3
SOUND 284, 3: SOUND 284, 3
SLEEP 1
   NEXT I
RETURN

THEKING2:
CALL MOUSEPOINTER(2)
FOR VERT = 54 TO 255
PUT (VERT, 100), ELVIS2
FOR T = 1 TO 30000: NEXT T
PUT (VERT, 100), ELVIS2, XOR
NEXT VERT
SWELVIS = 0
CALL MOUSEPOINTER(1)
CALL MOUSEPOINTER(3)
RETURN


SCRAM:



END SUB

DEFSNG A-Z
SUB DrawClock
SWCT = 0
PAINT (130, 100), 78                        'DRAW CLOCKFACE
CIRCLE (50, 100), 40, 229, 1.5, 4.8
CIRCLE (270, 100), 40, 229, 4.8, 1.5
LINE (53, 67)-(272, 67), 229
LINE (53, 133)-(272, 133), 229
PAINT (130, 100), 229
LINE (49, 70)-(269, 130), 5, BF
LINE (54, 85)-(264, 117), 0, BF
COLOR 7: LOCATE 13, 3: PRINT "TIME"
COLOR 7: LOCATE 13, 35: PRINT "ALARM"        'BUTTONS AND STUFF
COLOR 15: LOCATE 10, 25: PRINT "TO SET"
COLOR 7: LOCATE 16, 25: PRINT "SNOOZE"
COLOR 7: LOCATE 11, 35: PRINT "ON"
COLOR 7: LOCATE 15, 35: PRINT "OFF"
COLOR 7: LOCATE 15, 35: PRINT "OFF"
COLOR 15: LOCATE 16, 19: PRINT "B"
COLOR 15: LOCATE 16, 21: PRINT "T"
COLOR 39: LOCATE 10, 9: PRINT "RADIO"
COLOR 10: LOCATE 1, 18: PRINT "OPTIONS"
COLOR 10: LOCATE 1, 34: PRINT "GRAPHIC"


END SUB

SUB DrawClock2
SCREEN 13
SWCT = 1
WINDOW (-1, -1)-(1, 1)
RANDOMIZE TIMER
CL = INT(RND * 180)
CL2 = INT(RND * 2.2) + 1.88
CL3 = INT(RND * 7) + 3
CL4 = INT(RND * 18) + 12
CL5 = INT(RND * 111) + 80
CL6 = INT(RND * .8) + .4
CL7 = INT(RND * .02) + .01
ZIP = INT(RND * .003) + CL7
FOR I = CL6 TO CL2 STEP ZIP
FOR J = CL3 TO CL5 STEP CL4
                X = SIN(J) * I + COS(I) + COS(J * CL) - SIN(J)
                Y = COS(I) * JX - SIN(I) + SIN(J * .2) * ZIP - COS(CL)
                Z = TAN(J)
                        LINE (X, -Z)-(-Y, Z), I * J + CL + CL5
                        LINE (-X, -Z)-(Y, Z), I * J + CL + CL5
                        LINE (X, Z)-(-Y, -Z), I * J + CL + CL5
                        LINE (-X, Z)-(Y, -Z), I * J + CL + CL5
                        LINE (-Z, X)-(Z, -Y), I * J + CL + CL5
                        LINE (-Z, -X)-(Z, Y), I * J + CL + CL5
                        LINE (Z, X)-(-Z, -Y), I * J + CL + CL5
                        LINE (Z, -X)-(-Z, Y), I * J + CL + CL5
 NEXT J: NEXT I
        



WINDOW
CIRCLE (50, 100), 40, 229, 1.5, 4.8
CIRCLE (270, 100), 40, 229, 4.8, 1.5
LINE (53, 67)-(272, 67), 229
LINE (53, 133)-(272, 133), 229
PAINT (130, 100), 229
LINE (49, 70)-(269, 130), 5, BF
LINE (54, 85)-(264, 117), 0, BF
COLOR 7: LOCATE 13, 3: PRINT "TIME"
COLOR 7: LOCATE 13, 35: PRINT "ALARM"        'BUTTONS AND STUFF
COLOR 15: LOCATE 10, 25: PRINT "TO SET"
COLOR 7: LOCATE 16, 25: PRINT "SNOOZE"
COLOR 7: LOCATE 11, 35: PRINT "ON"
COLOR 7: LOCATE 15, 35: PRINT "OFF"
COLOR 7: LOCATE 15, 35: PRINT "OFF"
COLOR 15: LOCATE 16, 19: PRINT "B"
COLOR 15: LOCATE 16, 21: PRINT "T"
COLOR 10: LOCATE 1, 34: PRINT "GRAPHIC"
COLOR 10: LOCATE 1, 18: PRINT "OPTIONS"
COLOR 39: LOCATE 10, 9: PRINT "RADIO"
END SUB

DEFINT A-Z
SUB mouse (CX, dx, bx)
           POKE VARPTR(A(4)), &H92           'Swap code,Get CX setup
          CALL absolute(CX, VARPTR(A(0)))     'Run Code
            '  cx = cx / 8                     'Adjust 25x80
           POKE VARPTR(A(4)), &H91           'Swap code,Get DX setup
          CALL absolute(dx, VARPTR(A(0)))     'Run Code
              dx = dx / 2                     'Adjust 25x80
           POKE VARPTR(A(4)), &H93           'Swap code,Get BX setup
          CALL absolute(bx, VARPTR(A(0)))     'Run Code

                                   'Note :
                                   'Remove the /8
                                   'for graphics modes.

END SUB

SUB MOUSEPOINTER (SW)
       
           POKE VARPTR(A(0)) + 1, SW         'Swap code,Set AX = (SW)
          CALL absolute(C, VARPTR(A(0)))     'Run Code

                                          'Note:
                                             'SW = 0-reset
                                             'SW = 1-on
                                             'SW = 2-off
                                             'SW = 3-coordinates


END SUB

DEFSNG A-Z
SUB Show

CALL MOUSEPOINTER(2)
CLS
LOCATE 10, 3: COLOR 10: PRINT "ENTER GRAPHIC RUN TIME IN SECONDS"
LOCATE 12, 20: INPUT "", A
CALL MOUSEPOINTER(1)
CALL MOUSEPOINTER(3)
RT = A
TIMER ON: ON TIMER(RT) GOSUB PICOVER
WINDOW (-1, -1)-(1, 1)

DO
RANDOMIZE TIMER
CL = INT(RND * 180)
CL2 = INT(RND * 2.2) + 1.88
CL3 = INT(RND * 7) + 3
CL4 = INT(RND * 18) + 12
CL5 = INT(RND * 111) + 80
CL6 = INT(RND * .8) + .4
CL7 = INT(RND * .02) + .01
ZIP = INT(RND * .003) + CL7
FOR I = CL6 TO CL2 STEP ZIP
FOR J = CL3 TO CL5 STEP CL4
                X = SIN(J) * I + COS(I) + COS(J * CL) - SIN(J)
                Y = COS(I) * CL6 - SIN(I) + SIN(J * .2) * ZIP - COS(CL)
                Z = TAN(J)
                        LINE (X, -Z)-(-Y, Z), I * J + CL + CL5
                        LINE (-X, -Z)-(Y, Z), I * J + CL + CL5
                        LINE (X, Z)-(-Y, -Z), I * J + CL + CL5
                        LINE (-X, Z)-(Y, -Z), I * J + CL + CL5
                        LINE (-Z, X)-(Z, -Y), I * J + CL + CL5
                        LINE (-Z, -X)-(Z, Y), I * J + CL + CL5
                        LINE (Z, X)-(-Z, -Y), I * J + CL + CL5
                        LINE (Z, -X)-(-Z, Y), I * J + CL + CL5
 NEXT J: NEXT I
    X$ = INKEY$
    IF X$ <> "" THEN GOTO EXT

RANDOMIZE TIMER
CL = INT(RND * 156)
CL2 = INT(RND * 2.83) + 1.68
CL3 = INT(RND * 9) + 4
CL4 = INT(RND * 18) + 10
CL5 = INT(RND * 100) + 71
CL6 = INT(RND * .9) + .2
CL7 = INT(RND * .02) + .01
ZIP = INT(RND * .003) + CL7
FOR I = CL6 TO CL2 STEP ZIP
FOR J = CL3 TO CL5 STEP CL4
                X = SIN(J) * I + COS(I) + COS(J * CL) - SIN(J)
                Y = COS(I) * JX - SIN(I) + SIN(J * .2) * ZIP - COS(CL)
                Z = TAN(J)
                        LINE (X, -Z)-(-Y, Z), I * J + CL + CL5
                        LINE (-X, -Z)-(Y, Z), I * J + CL + CL5
                        LINE (X, Z)-(-Y, -Z), I * J + CL + CL5
                        LINE (-X, Z)-(Y, -Z), I * J + CL + CL5
                        LINE (-Z, X)-(Z, -Y), I * J + CL + CL5
                        LINE (-Z, -X)-(Z, Y), I * J + CL + CL5
                        LINE (Z, X)-(-Z, -Y), I * J + CL + CL5
                        LINE (Z, -X)-(-Z, Y), I * J + CL + CL5
 NEXT J: NEXT I
    X$ = INKEY$
    IF X$ <> "" THEN GOTO EXT

FOR A = 1 TO 5
RANDOMIZE TIMER
CL = INT(RND * 199)
DL = INT(RND * 1.5) + ZIP
EL = INT(RND * .04) + .01
ZIP = INT(RND * I) + .2
FOR I = EL TO .9 STEP .005
FOR J = DL TO .9 STEP ZIP
                X = SIN(EL + (I * J)) + SIN(I) - COS(Z)
                Y = COS(DL + (J * I)) ^ A + COS(I)
                Z = TAN(J) + COS(J ^ I) - SIN(A)
                        LINE (X, -Z)-(-Y, Z), J * CL - 55
                        LINE (-X, -Z)-(Y, Z), J * CL - 55
                        LINE (X, Z)-(-Y, -Z), J * CL - 55
                        LINE (-X, Z)-(Y, -Z), J * CL - 55
                        LINE (-Z, X)-(Z, -Y), J * CL - 55
                        LINE (-Z, -X)-(Z, Y), J * CL - 55
                        LINE (Z, X)-(-Z, -Y), J * CL - 55
                        LINE (Z, -X)-(-Z, Y), J * CL - 55
         NEXT J: NEXT I
         NEXT A
    X$ = INKEY$
    IF X$ <> "" THEN GOTO EXT

FOR A = 1 TO 3
RANDOMIZE TIMER
CL = INT(RND * 213) + I
CL2 = INT(RND * .05) + .01
CL3 = INT(RND * .9) + .6
ZAP = INT(RND * 3) + A
ZIP = INT(RND * .006) + .05
FOR I = CL2 TO CL3 STEP .01
FOR J = 1 TO 2 STEP ZIP
                X = SIN(I * J) / ZAP + SIN(Z) - SIN(I) ^ .2
                Y = COS(J * I) ^ A + COS(X) + COS(J / .01)
                Z = TAN(I)
                        LINE (X, -Z)-(-Y, Z), J * CL - 32
                        LINE (-X, -Z)-(Y, Z), J * CL - 32
                        LINE (X, Z)-(-Y, -Z), J * CL - 32
                        LINE (-X, Z)-(Y, -Z), J * CL - 32
                        LINE (-Z, X)-(Z, -Y), J * CL - 32
                        LINE (-Z, -X)-(Z, Y), J * CL - 32
                        LINE (Z, X)-(-Z, -Y), J * CL - 32
                        LINE (Z, -X)-(-Z, Y), J * CL - 32
         NEXT J: NEXT I
   NEXT A
    X$ = INKEY$
    IF X$ <> "" THEN GOTO EXT


FOR A = 1 TO 4
RANDOMIZE TIMER
CL = INT(RND * 211) + I
CL2 = INT(RND * .09) + .03
CL3 = INT(RND * 1.1) + .6
CL4 = INT(RND * 1.1) + .7
CL5 = INT(RND * 102) + 88
ZAP = INT(RND * 3) + A
ZIP = INT(RND * .009) + .09
FOR I = CL2 TO CL3 STEP .01
FOR J = (.1 + ZIP) TO CL4 STEP ZIP
                X = SIN(X * J) / ZAP + SIN(Z) - SIN(I) ^ .2
                Y = COS(Y * I) ^ A + COS(A) + COS(J / .01)
                Z = TAN(I ^ J) - SIN(A) + COS(A)
                        LINE (X, -Z)-(-Y, Z), J * CL - CL5 ^ ZIP
                        LINE (-X, -Z)-(Y, Z), J * CL - CL5 ^ ZIP
                        LINE (X, Z)-(-Y, -Z), J * CL - CL5 ^ ZIP
                        LINE (-X, Z)-(Y, -Z), J * CL - CL5 ^ ZIP
                        LINE (-Z, X)-(Z, -Y), J * CL - CL5 ^ ZIP
                        LINE (-Z, -X)-(Z, Y), J * CL - CL5 ^ ZIP
                        LINE (Z, X)-(-Z, -Y), J * CL - CL5 ^ ZIP
                        LINE (Z, -X)-(-Z, Y), J * CL - CL5 ^ ZIP
         NEXT J: NEXT I
         NEXT A

    X$ = INKEY$
    IF X$ <> "" THEN GOTO EXT

FOR A = 1 TO 1
RANDOMIZE TIMER
CL = INT(RND * 218) + 4
CL2 = INT(RND * 4.49) + 3.9
CL3 = INT(RND * 182) + 117
CL4 = INT(RND * 36) + 33

ZIP = INT(RND * .003) + .01
FOR I = .1 TO CL2 STEP ZIP
FOR J = 1 TO CL3 STEP CL4
                X = SIN(I) * Y + COS(J)
                Y = COS(I) * Z - SIN(J)
                Z = TAN(J ^ .2)
                        LINE (X, -Z)-(-Y, Z), I * J + CL + 35
                        LINE (-X, -Z)-(Y, Z), I * J + CL + 35
                        LINE (X, Z)-(-Y, -Z), I * J + CL + 35
                        LINE (-X, Z)-(Y, -Z), I * J + CL + 35
                        LINE (-Z, X)-(Z, -Y), I * J + CL + 35
                        LINE (-Z, -X)-(Z, Y), I * J + CL + 35
                        LINE (Z, X)-(-Z, -Y), I * J + CL + 35
                        LINE (Z, -X)-(-Z, Y), I * J + CL + 35
 NEXT J: NEXT I
    NEXT A

    X$ = INKEY$
    IF X$ <> "" THEN GOTO EXT


RANDOMIZE TIMER
CL = INT(RND * 180)
CL2 = INT(RND * 2.2) + 1.88
CL3 = INT(RND * 7) + 3
CL4 = INT(RND * 18) + 12
CL5 = INT(RND * 111) + 80
CL6 = INT(RND * .8) + .4
CL7 = INT(RND * .02) + .01
ZIP = INT(RND * .003) + CL7
FOR I = CL6 TO CL2 STEP ZIP
FOR J = CL3 TO CL5 STEP CL4
                X = SIN(J) * I + COS(I) + COS(J * CL) - SIN(J)
                Y = COS(I) * JX - SIN(I) + SIN(J * .2) * ZIP - COS(CL)
                Z = TAN(J)
                        LINE (X, -Z)-(-Y, Z), I * J + CL + CL5
                        LINE (-X, -Z)-(Y, Z), I * J + CL + CL5
                        LINE (X, Z)-(-Y, -Z), I * J + CL + CL5
                        LINE (-X, Z)-(Y, -Z), I * J + CL + CL5
                        LINE (-Z, X)-(Z, -Y), I * J + CL + CL5
                        LINE (-Z, -X)-(Z, Y), I * J + CL + CL5
                        LINE (Z, X)-(-Z, -Y), I * J + CL + CL5
                        LINE (Z, -X)-(-Z, Y), I * J + CL + CL5
 NEXT J: NEXT I
    X$ = INKEY$
    IF X$ <> "" THEN GOTO EXT

FOR A = 1 TO 2
RANDOMIZE TIMER
CL = INT(RND * 177)
CL2 = INT(RND * 2.5) + 1.8
CL3 = INT(RND * .5) + .2
CL4 = INT(RND * 2.2) + 1.6
DL = INT(RND * 3) + .3
ZAP = INT(RND * 4) + ZIP
ZIP = INT(RND * .003) + .009
         FOR I = CL2 TO CL3 STEP -ZIP
         FOR J = CL4 TO CL3 STEP -.21
      
                X = SIN(I) + SIN(J)
                Y = COS(J) + COS(I) - SIN(ZAP)
                Z = TAN(J) - COS(DL)
                        LINE (X, -Z)-(-Y, Z), J * I * CL - 120
                        LINE (-X, -Z)-(Y, Z), J * I * CL - 120
                        LINE (X, Z)-(-Y, -Z), J * I * CL - 120
                        LINE (-X, Z)-(Y, -Z), J * I * CL - 120
                        LINE (-Z, X)-(Z, -Y), J * I * CL - 120
                        LINE (-Z, -X)-(Z, Y), J * I * CL - 120
                        LINE (Z, X)-(-Z, -Y), J * I * CL - 120
                        LINE (Z, -X)-(-Z, Y), J * I * CL - 120
          NEXT J: NEXT I
            NEXT A
    X$ = INKEY$
    IF X$ <> "" THEN GOTO EXT

FOR A = 1 TO 2
RANDOMIZE TIMER
CL = INT(RND * 255)
CL2 = INT(RND * 3.1) + 2.5
CL3 = INT(RND * 2.5) + 2
CL4 = INT(RND * 75) + 19
DL = INT(RND * 3) + SQR(.3)
ZAP = INT(RND * 4) + ZIP
ZIP = INT(RND * .003) + .009
         FOR I = CL2 TO .1 STEP -(ZIP ^ .9)
         FOR J = .2 TO CL3 STEP ATN(.2)
                X = SIN(J) + SIN(J)
                Y = COS(J) + COS(I) - SIN(ZAP)
                Z = TAN(I) - COS(DL) * (A - DL)
                        LINE (X, -Z)-(-Y, Z), J * I * CL - CL4
                        LINE (-X, -Z)-(Y, Z), J * I * CL - CL4
                        LINE (X, Z)-(-Y, -Z), J * I * CL - CL4
                        LINE (-X, Z)-(Y, -Z), J * I * CL - CL4
                        LINE (-Z, X)-(Z, -Y), J * I * CL - CL4
                        LINE (-Z, -X)-(Z, Y), J * I * CL - CL4
                        LINE (Z, X)-(-Z, -Y), J * I * CL - CL4
                        LINE (Z, -X)-(-Z, Y), J * I * CL - CL4
          NEXT J: NEXT I
            NEXT A
    X$ = INKEY$
    IF X$ <> "" THEN GOTO EXT

FOR A = 1 TO 2
RANDOMIZE TIMER
CL = INT(RND * 237) + 1 ^ A
CL2 = INT(RND * 3) + 2.5
CL3 = INT(RND * .5) + .1
DL = INT(RND * 3) + CDBL(.3)
ZAP = INT(RND * 4) + ZIP
ZIP = INT(RND * .003) + .01
         FOR I = CL2 TO CL3 STEP -(ZIP)
         FOR J = CL3 TO 1.5 STEP ATN(.2)
                X = SIN(J) + SIN(-I)
                Y = COS(J) + COS(I) - SIN(ZAP)
                Z = TAN(J) - COS(DL) * (A - DL)
                        LINE (X, -Z)-(-Y, Z), J * I * CL - 49
                        LINE (-X, -Z)-(Y, Z), J * I * CL - 49
                        LINE (X, Z)-(-Y, -Z), J * I * CL - 49
                        LINE (-X, Z)-(Y, -Z), J * I * CL - 49
                        LINE (-Z, X)-(Z, -Y), J * I * CL - 49
                        LINE (-Z, -X)-(Z, Y), J * I * CL - 49
                        LINE (Z, X)-(-Z, -Y), J * I * CL - 49
                        LINE (Z, -X)-(-Z, Y), J * I * CL - 49
          NEXT J: NEXT I
            NEXT A
    X$ = INKEY$
    IF X$ <> "" THEN GOTO EXT

FOR A = 1 TO 2
RANDOMIZE TIMER
CL = INT(RND * 217) + 5
CL2 = INT(RND * 4.49) + 2.75
CL3 = INT(RND * .5) + .1
CL4 = INT(RND * 38) + 20
ZIP = INT(RND * .003) + .015
FOR I = CL3 TO CL2 STEP ZIP
FOR J = 1 TO 8 STEP 1
                X = SIN(I) * Y + COS(J) + SIN(J * ZIP)
                Y = COS(I) * Z + SIN(I) + COS(I + ZIP)
                Z = TAN(J)
                        LINE (X, -Z)-(-Y, Z), I * J + CL + CL4
                        LINE (-X, -Z)-(Y, Z), I * J + CL + CL4
                        LINE (X, Z)-(-Y, -Z), I * J + CL + CL4
                        LINE (-X, Z)-(Y, -Z), I * J + CL + CL4
                        LINE (-Z, X)-(Z, -Y), I * J + CL + CL4
                        LINE (-Z, -X)-(Z, Y), I * J + CL + CL4
                        LINE (Z, X)-(-Z, -Y), I * J + CL + CL4
                        LINE (Z, -X)-(-Z, Y), I * J + CL + CL4
 NEXT J: NEXT I
 NEXT A
    X$ = INKEY$
    IF X$ <> "" THEN GOTO EXT

FOR A = 1 TO 3
RANDOMIZE TIMER
CL = INT(RND * 213) + I ^ A
CL2 = INT(RND * 2.4) + .4
CL3 = INT(RND * 300) + 250
CL4 = INT(RND * 20) + 5
ZIP = INT(RND * .002) + .03 ^ 1
FOR I = .1 TO CL2 STEP ZIP
FOR J = 10 TO CL3 STEP 42 ^ .9
                X = SIN(J) - SIN(ABS(I))
                Y = COS(J) * Z - LOG(I) + SIN(J / CL) - I
                Z = TAN(J / .02)
                        LINE (X, -Z)-(-Y, Z), SIN(X) * J + CL - 115
                        LINE (-X, -Z)-(Y, Z), SIN(X) * J + CL - 115
                        LINE (X, Z)-(-Y, -Z), SIN(X) * J + CL - 115
                        LINE (-X, Z)-(Y, -Z), SIN(X) * J + CL - 115
                        LINE (-Z, X)-(Z, -Y), SIN(X) * J + CL - 115
                        LINE (-Z, -X)-(Z, Y), SIN(X) * J + CL - 115
                        LINE (Z, X)-(-Z, -Y), SIN(X) * J + CL - 115
                        LINE (Z, -X)-(-Z, Y), SIN(X) * J + CL - 115
 NEXT J: NEXT I
    NEXT A
    X$ = INKEY$
    IF X$ <> "" THEN GOTO EXT


FOR A = 1 TO 1
RANDOMIZE TIMER
CL = INT(RND * 96) + I
CL2 = INT(RND * 2.49) + 2
CL3 = INT(RND * 30) + 25
CL4 = INT(RND * 15) + 12
ZIP = INT(RND * .006) + .003
FOR I = .1 TO CL2 STEP ZIP
FOR J = 5 TO CL3 STEP CL4
                X = SIN(I ^ A) + SIN(J - (ZIP ^ A)) + SGN(I * A)
                Y = COS(J) + COS(I + ZIP) + COS(J * I ^ A)
                Z = TAN(J) - FIX(ZIP) * INT(J + ZIP)
                        LINE (X, -Z)-(-Y, Z), I * 100 - CL * J
                        LINE (-X, -Z)-(Y, Z), I * 100 - CL * J
                        LINE (X, Z)-(-Y, -Z), I * 100 - CL * J
                        LINE (-X, Z)-(Y, -Z), I * 100 - CL * J
                        LINE (-Z, X)-(Z, -Y), I * 100 - CL * J
                        LINE (-Z, -X)-(Z, Y), I * 100 - CL * J
                        LINE (Z, X)-(-Z, -Y), I * 100 - CL * J
                        LINE (Z, -X)-(-Z, Y), I * 100 - CL * J
 NEXT J: NEXT I
NEXT A
    X$ = INKEY$
    IF X$ <> "" THEN GOTO EXT






LOOP UNTIL INKEY$ <> ""

EXT:

END SUB

SUB ShowTime
    SCREEN 13
    COLOR 39
    LOCATE 13, 9

    SELECT CASE Format%

       CASE 0

          HOUR% = VAL(LEFT$(TIME$, 2))

          SELECT CASE HOUR%

             CASE IS > 12
                HOUR% = HOUR% - 12
                HOUR$ = STR$(HOUR%)
                HOUR$ = RIGHT$(HOUR$, LEN(HOUR$) - 1)
                HOUR$ = RIGHT$("  " + HOUR$, 2)
                PRINT HOUR$ + RIGHT$(TIME$, 6) + " PM";

             CASE IS = 12
                PRINT TIME$ + " PM";

             CASE IS = 0
                HOUR% = 12
                PRINT "12" + RIGHT$(TIME$, 6) + " AM";

             CASE IS < 12
                HOUR$ = STR$(HOUR%)
                HOUR$ = RIGHT$(HOUR$, LEN(HOUR$) - 1)
                HOUR$ = RIGHT$("  " + HOUR$, 2)
                PRINT HOUR$ + RIGHT$(TIME$, 6) + " AM";

          END SELECT
       
       CASE 1
         


          PRINT TIME$


    END SELECT
 
 
END SUB

