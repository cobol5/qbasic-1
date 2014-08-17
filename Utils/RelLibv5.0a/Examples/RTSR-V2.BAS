' Real-Time Star Routines v2.0, by Adigun Azikiwe Polack

' (Feel free to add them to your programs.  If you do use ANY of my routines,
'  then please give me credit for them, otherwise DO NOT EVEN USE THEM AT
'  ALL.  *PERIOD*.  Thanks so rather much and **BEST OF SUCH SPLENDID QB
'  PROGRAMMING TO YOU**!!!  :D  ;D  ;) )

' (NOTE: This program requires RelLib and QuickBASIC 4.5 in order for you to
'  use.)                              ~~~

' Hello to you all and GOD BLESS YOU **EVER** SO RICHLY!!!  ;)  In this new
' version of my Real-Time Star Routines, I have made them smaller and more
' flexible for you to use, to save you even MORE programming time than ever!
' Doing them all for your benefit, not my own, but *YOURS* indeed, after all.
' Just take one good whiff at my adjusted routines and find out why.  Plus,
' in the color of the stars that are within a 16-color spread, the stars are
' color-animated now, making them come alive in a more _exciting_ way!!!  ;D
' With all of these adjustments and more, I sure hope you do enjoy this
' latest version of my routines.  Later!!!  ;*)
'
'
'                                      WITH SUCH PRECIOUS LOVE AND REGARDS,
'
'                                      Adigun Azikiwe Polack
'                                      One of the Founders of "Aura Flow"
'                                      May 1, 2003

DEFINT A-Z

'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'Here are the newly improved and compact versions of my Star Routines!!!  ;D

DECLARE SUB STARinit (Strmul%)
DECLARE SUB STARMotion (Lay%, Strmul%, Col%, Trans%)
DECLARE SUB STARmove (Strmul%, Dir%, Spd%)
'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ


'$DYNAMIC
'$INCLUDE: 'rellib.bi'
CLEAR

DIM SHARED Layer%
REDIM SHARED Vpage(31999) AS INTEGER
DIM SHARED Pal AS STRING * 768

'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
'This only command is REQUIRED in order for these updated Star Routines to
'                                                            work properly!!!

REDIM SHARED S!(200), SE!(200), StrCOL(200)
'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

Layer% = VARSEG(Vpage(0))
RelKeyBoardON

RelInitVGA13

RelReadPal Pal
RelLoadPal "RelPal.pal", Pal, 1

RelDelay 60

'ออออออออออออออออออออออออออออออออออออออออออออออ About the "STARinit" command:

'STARinit 200
'          
'          ภฤฤฤฤฤฤฤฤฤThis is the number of stars that you want to use.
'
' **ALWAYS** use this command FIRST before you even use the "STARmotion"
' and "STARmove" commands!!!  ;)
'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ


'ออออออออออออออออออออออออออออออออออออออออออออ About the "STARmotion" command:

'STARMotion Layer%, 200, 1, 0
'                        
'             ณ      ณ   ณ  ภฤฤThis is the transparent/opaque switch.
'             ณ      ณ   ณ     (0 = Opaque; 1 = Transparent)
'             ณ      ณ   ณ
'             ณ      ณ   ภฤฤฤฤฤThis is the number that represents a color of
'             ณ      ณ         the stars within any 16-color spread of a
'             ณ      ณ         256-color palette (beyond the first 15 colors,
'             ณ      ณ         usually!)!
'             ณ      ณ
'             ณ      ภฤฤฤฤฤฤฤฤฤThis is the number of stars that you want
'             ณ                to use.
'             ณ
'             ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤThis is the layer position that you want to
'                              paste your stars in.
'
' T I P S : ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
'
' * It is recommended that you use this command before you use
'   the "STARmove" command.
'
' * While using this command, if you set the color number to -1,
'   then the stars will animate in random colors!
'
' ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
'
'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ


'ออออออออออออออออออออออออออออออออออออออออออออออ About the "STARmove" command:

'STARmove 200, 2, 5
'               
'          ณ   ณ  ภฤฤฤThis is the speed setting for the stars (0 to 300 only).
'          ณ   ณ      (Lower numbers = Lower Speeds;
'          ณ   ณ       higher numbers = Higher Speeds; 300 = MAXIMUM SPEED;
'          ณ   ณ       and 0 = NO speed)
'          ณ   ณ
'          ณ   ภฤฤฤฤฤฤThis is the direction switch for the stars.
'          ณ          (1 = up, 2 = down, 3 = left, 4 = right)
'          ณ
'          ภฤฤฤฤฤฤฤฤฤThis is the number of stars that you want to use.
'
' T I P S : ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
'
' * It is best to use the "STARmove" command AFTER you use the
'   "STARMotion" command, particularly when drawing your layers
'   and then putting your layers to the VIDEO buffer!  ;)
'
' * If you use this command to move in a horizontal direction
'   and IMMEDIATELY use that same command to move in a vertical
'   direction, then you have successfully have the potential of
'   moving the stars diagonally in a wild way!!!
'
' ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
'
'
'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ





'ออออ Now, let's put those routines to some good use with this example!!!  ;)

Stpd = 5
STARinit 200
Col = 1

DO

STARMotion Layer%, 200, Col, 0
STARmove 200, 2, Stpd

IF RelKey%(KEYESC) THEN EXIT DO
IF RelKey%(KEYSPACE) THEN
  IF ColDel <= 0 THEN
    Col = Col + 1: ColDel = 10: IF Col > 9 THEN Col = -1
  END IF
END IF
IF RelKey%(KEYUP) THEN Stpd = Stpd - 1
IF RelKey%(KEYDOWN) THEN Stpd = Stpd + 1
IF RelKey%(KEYLEFT) THEN STARmove 200, 3, Stpd + 25
IF RelKey%(KEYRIGHT) THEN STARmove 200, 4, Stpd + 25

ColDel = ColDel - 1: IF ColDel <= 0 THEN ColDel = 0

RelPrint Layer%, 1, 1, "---- Real-Time Star Routines v2.0 ----", 16, 0
RelPrint Layer%, 0, 0, "---- Real-Time Star Routines v2.0 ----", 24, 0
RelPrint Layer%, 41, 9, "by Adigun Azikiwe POLACK", 16, 1
RelPrint Layer%, 40, 8, "by Adigun Azikiwe POLACK", 24, 1
RelPrint Layer%, 1, 25, "---------- Special Thanks to -----------", 50, 0
RelPrint Layer%, 1, 33, "GOD ALMIGHTY (ESPECIALLY!!!  :*D ;D ;) )", 240, 0
RelPrint Layer%, 145, 41, "and", 16, 0
RelPrint Layer%, 49, 49, "Richard Eric M. Lope!!!  ;D", 16, 0
RelPrint Layer%, 0, 24, "---------- Special Thanks to -----------", 56, 0
RelPrint Layer%, 0, 32, "GOD ALMIGHTY (ESPECIALLY!!!  :*D ;D ;) )", 248, 0
RelPrint Layer%, 144, 40, "and", 24, 0
RelPrint Layer%, 48, 48, "Richard Eric M. Lope!!!  ;D", 24, 0


RelPrint Layer%, 49, 145, "SPACE - change star colors", 96, 0
RelPrint Layer%, 48, 144, "SPACE - change star colors", 104, 0

RelPrint Layer%, 1, 153, " - increase speed   - decrease speed", 96, 0
RelPrint Layer%, 0, 152, " - increase speed   - decrease speed", 104, 0

RelPrint Layer%, 1, 169, "Press LEFT and RIGHT keys to move those", 96, 0
RelPrint Layer%, 1, 177, "stars horizontally!", 96, 0
RelPrint Layer%, 0, 168, "Press LEFT and RIGHT keys to move those", 104, 0
RelPrint Layer%, 0, 176, "stars horizontally!", 104, 0

RelPrint Layer%, 1, 193, "Press [ESC] to exit this test.", 96, 0
RelPrint Layer%, 0, 192, "Press [ESC] to exit this test.", 104, 0

RelPCopy VIDEO, Layer%
RelCLS Layer%, 0
RelWait
LOOP

RelFade 0, 0, 0, 80
RelKeyBoardOFF

EndProgram:  RelInitText
             COLOR 7: END

'ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

REM $STATIC
SUB STARinit (Strmul%)

FOR str% = 0 TO Strmul%
  S!(str%) = INT(RND(1) * 200) + 1: SE!(str%) = INT(RND(1) * 340) + 10
NEXT

END SUB

SUB STARMotion (Lay%, Strmul%, Col%, Trans%)
FOR Rn = 0 TO Strmul%
  IF Col% > 0 THEN
    StrCOL(Rn) = (INT(RND(1) * 14) + 1) + (16 * Col%)
    ELSEIF Col% = -1 THEN
    StrCOL(Rn) = INT(RND(1) * 239) + 16
  END IF
NEXT

FOR str% = 0 TO Strmul%
  IF Trans% = 0 THEN
    RelPset Lay%, SE!(str%), S!(str%), StrCOL(str%)
  END IF
  IF Trans% >= 1 THEN
    RelPsetTrans Lay%, SE!(str%), S!(str%), StrCOL(str%)
  END IF
NEXT

END SUB

SUB STARmove (Strmul%, Dir%, Spd%)

IF Spd% <= 0 THEN Spd% = 0
IF Spd% >= 300 THEN Spd% = 300

FOR str% = 0 TO Strmul%
  StAmt! = (str% * .001)
 
  IF Dir% = 1 THEN
    S!(str%) = S!(str%) - (StAmt! * (Spd%)): IF S!(str%) <= 0 THEN S!(str%) = 220: SE!(str%) = INT(RND(1) * 320) + 1
  END IF

  IF Dir% = 2 THEN
    S!(str%) = S!(str%) + (StAmt! * (Spd%)): IF S!(str%) >= 210 THEN S!(str%) = -0: SE!(str%) = INT(RND(1) * 320) + 1
  END IF

  IF Dir% = 3 THEN
    SE!(str%) = SE!(str%) - (StAmt! * (Spd%)): IF SE!(str%) <= -10 THEN SE!(str%) = 340: S!(str%) = INT(RND(1) * 200) + 1
  END IF

  IF Dir% = 4 THEN
    SE!(str%) = SE!(str%) + (StAmt! * (Spd%)): IF SE!(str%) >= 330 THEN SE!(str%) = -20: S!(str%) = INT(RND(1) * 200) + 1
  END IF
NEXT

END SUB

