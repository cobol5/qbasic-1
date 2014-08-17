'           Microsoft BASIC 7.0, Professional Development System
'              Copyright (C) 1987-1989, Microsoft Corporation
'
'           Microsoft QBX 7.0, Professional Development System
'              Copyright (C) 1987-1989, Microsoft Corporation
'
'                    Raymond E Dixon
'                    5815 Buckley Dr.
'                    Jacksonville, Fl. 32244
'
'                    (904) 778-4048
'                    (904) 772-0329
'
'    I think the only routine that won't work with QB45 is "SLEEP()"(removed)
'      which is a QBX function , replace a loop for QB45.
'    I started all subs with Q so not to conflict with other subs
'      when I need to load and move to my programs.
' ALL the main code is for testing the sub.
'
' UPDATES:   and a few comments from aurthor.
'
' started 05/12/90
' added numeric input 5/30/90 to handle decimal, neg and real numbers
' in numericinput only numbers and decimal allowed in format
' speeded up input routine by removing unessary code.
' removed SLEEP()
' fixed a few bugs 06/03/90
' after many hours work seems to function the way I had hope for.

'*************** Declarations and definitions begin here ********************
   DEFINT A-Z  'Resets the default data type from single precision to integer

   DECLARE FUNCTION Qformateditnum$ (work$, format$, ExitCode%, UPflag%, PGUPflag%, DNflag%, PGDNflag%, RETflag%, TABflag%, ESCflag%)
   DECLARE FUNCTION Qformateditstr$ (work$, format$, caseflag%, ExitCode%, UPflag%, PGUPflag%, DNflag%, PGDNflag%, RETflag%, TABflag%, ESCflag%)
   DECLARE FUNCTION Qremovechar$ (userstring$, skip$)
   DECLARE FUNCTION Qremoveformat$ (instring$, format$)
   DECLARE FUNCTION Quserformat$ (inputstring$, format$)
   DECLARE SUB Qdrawscreen ()
   DECLARE SUB Qmessage (msg$, row%)
   DECLARE SUB Qsglbox (scol1%, srow1%, ecol1%, erow1%)
   DECLARE SUB Qdblbox (leftcol%, leftrow%, rightcol%, rightrow%)
   DECLARE SUB QformatDEC (a$, beforeDEC%, afterdec%)
   DECLARE SUB Qclreol ()
   DECLARE SUB Qclrscrn (startline%, endline%, startcol%, endcol%)

' Define names similar to keyboard names with their equivalent key codes.

   CONST SPACE = 32, ESC = 27, enter = 13, TABKEY = 9
   CONST DOWN = 80, UP = 72, LEFT = 75, RIGHT = 77
   CONST HOME = 71, ENDK = 79, PGDN = 81, PGUP = 73
   CONST INS = 82, DEL = 83, NULL = 0
   CONST CTRLD = 4, CTRLG = 7, CTRLH = 8, CTRLS = 19, CTRLV = 22

' Define English names for color-specification numbers. Add BRIGHT to
' any color to get bright version.

   CONST BLACK = 0, blue = 1, GREEN = 2, CYAN = 3, RED = 4, MAGENTA = 5
   CONST YELLOW = 6, WHITE = 7, BRIGHT = 8

' Assign colors to different kinds of text. By changing the color assigned,
' you can change the color of the display. The initial colors are
' chosen because they work for color or black-and-white displays.
' Codes for normal and highlight

   HILITE = WHITE + BRIGHT
   CONST BACKGROUND = blue
   CONST normal = WHITE + BRIGHT

' Miscellaneous symbolic constants

   CONST False = 0, True = 1
   CONST CURSORON = 1, CURSOROFF = 0

   'set edit colors
   'Editbackground = RED
   'Editforeground = WHITE + BRIGHT

   'set edit to reverse
   editbackground = normal
   editforeground = blue

'*************** Declarations and definitions end here ********************

   COLOR HILITE, blue
   CLS
   Qdrawscreen
   Qclrscrn 4, 20, 2, 78
   msg$ = "ESC restores CTRL-E Clears, all other edit keys function normal"
   Qmessage msg$, 3

start:
'
' comment out the format$ that are not being used and a instring to match
' except for prompt message.
' format$  can not be a null
' string passed maybe null "" or any basic string
' there are so many formats that I only listed a few, just try yours
'GOTO num
'******************************************************************
   instring$ = "887649889"
   msg1$ = ": string returned unformated"
   format$ = "(999)-(99)-(9999) SS number"
   msg2$ = ": enter data at specified position"
   GOSUB teststring

'******************************************************************
   instring$ = "409"
   msg1$ = ": enter at specified area using string input"
   format$ = "before:>999<:after"
   msg2$ = ": before and after prompts"
   GOSUB teststring

'*******************************************************************
   instring$ = "123456789"
   msg1$ = ": numeric input are right justified"
   format$ = "9999999"
   msg2$ = ": if longer than format left characters are lost"
   GOSUB testnumeric

'*******************************************************************
   instring$ = "123.4500"
   msg1$ = ": decimal numbers are aligned"
   format$ = "99999.99"
   msg2$ = ": for numeric input all numbers are input right to left"
   GOSUB testnumeric

'*******************************************************************
   instring$ = "44.00"
   msg1$ = ": instring$ maybe upto 80 char"
   format$ = "99999.999"
   msg2$ = ": format maybe different decimal pos"
   GOSUB testnumeric

'***********************************************
   instring$ = "7770329"
   msg1$ = ": seven digit phone numbers"
   format$ = " 999-9999  seven digit phone"      '  7 digit phone
   msg2$ = ": allmost any format using string input"
   GOSUB teststring

'***********************************************
   instring$ = "9047784048"    ' 10 digit phone
   msg1$ = ": ten digit phone numbers"

   format$ = "(999) 999-9999"
   msg2$ = ": allmost any format"
   GOSUB teststring

   msg1$ = ": ten digit phone numbers"

'           with user prompt
   format$ = "Area Code: (999) Phone: 999-9999"
   msg2$ = ": allmost any format, even user prompt "
   GOSUB teststring

'********************************************************
   instring$ = Qremovechar(LEFT$(DATE$, 6), "-") + RIGHT$(DATE$, 2)
'  instring="040146"       ' date input
   msg1$ = ": date formated input"

   format$ = " 19/39/99 "  'mask for month/day/year
   msg2$ = ": with limited entry"
   GOSUB teststring

'***********************************************
   instring$ = "M"
   msg1$ = ": maybe preset to Male or Female"
   format$ = "Enter Male or Female ? (M/F):|"   '   one char M/F
   msg2$ = ": only MF allowed"
   GOSUB teststring
'********************************************************
   instring$ = "A124444"
   msg1$ = ": account numbers"

   format$ = "ACC NO: @99-9999" 'first char is alpha only ,rest numeric
   msg2$ = ": any format with alpha only first digit"
   GOSUB teststring

'********************************************************
' for fixed length strings or user type

   instring$ = "raymond e dixon"
   msg1$ = ": may force caps, upper, lower or any case "
   'format$ = STRING$(LEN(instring$), "@")
   msg2$ = ": alpha input only, alphanumeric or numeric only"

   format$ = ">@@@@@@@@@@@@@@@@@@@@@@@<"
   GOSUB teststring

'********************************************************
   instring$ = ""
   msg1$ = ": force enterkey or exitkey only, for msg display "
   format$ = " Press ENTER key to Continue ~"   '(~) requires enter to be pressed
   msg2$ = ": any single line message can be displayed"
   GOSUB teststring
'********************************************************

   msg1$ = ""

redosformat:
   msg2$ = " Enter Your Format String (no quotes): "
   format$ = msg2$ + STRING$(25, "#")

Qclrscrn 4, 20, 2, 78
LOCATE 4, 4
PRINT "Formats Allowed:";
LOCATE 5, 5
PRINT CHR$(34) + "99" + CHR$(34) + "             ' numbers only  < (99 max) each digit = to max value";
LOCATE 6, 5
PRINT CHR$(34) + "19" + CHR$(34) + "             ' (19) is max value";
LOCATE 7, 5
PRINT CHR$(34) + "999-99-9999    SS number" + CHR$(34);
LOCATE 8, 5
PRINT CHR$(34) + "999-9999; " + CHR$(34) + "     ' 7 digit phone";
LOCATE 9, 5
PRINT CHR$(34) + "(999) 999-9999" + CHR$(34) + " ' 10 digit phone";
LOCATE 10, 5
PRINT CHR$(34) + "19/39/99" + CHR$(34) + "       ' date format";
LOCATE 11, 5
PRINT CHR$(34) + "########" + CHR$(34) + "       '# alphanumeric set for 8 characters maybe more or less";
LOCATE 12, 5
PRINT CHR$(34) + "@@@@@@@@" + CHR$(34) + "       '@ alpha only   same as above";
LOCATE 13, 5
PRINT CHR$(34) + "Y/N:*" + CHR$(34) + "          '* force YN answer.";
LOCATE 14, 5
PRINT CHR$(34) + "M/F:|" + CHR$(34) + "          '| force MF answer.";
LOCATE 15, 5
PRINT CHR$(34) + "MESSAGE~" + CHR$(34) + "       '~ force enter key or other exitkey for prompts .";
LOCATE 16, 5
PRINT "maybe any format you can create in a basic string except #@~*|0123456789";
LOCATE 17, 5
PRINT "may not be used in prompt,  you can even include a message if you like.";

LOCATE 18, 5
PRINT "      " + CHR$(34) + "Test Data: 99" + CHR$(34) + " <- this format will print";
LOCATE 19, 5
PRINT "       Test Data: your value passed";
LOCATE 20, 5
PRINT "                 in the the length of 2 Setting max value to 99.";
'

    instring$ = ""
    LOCATE 22, 3

    instring$ = Qformateditstr(instring$, format$, 1, ExitCode, 0, 0, 0, 0, 1, 1, 0)

    'test user input
    IF LEN(instring$) THEN
     FOR cpos = 1 TO LEN(instring$)
       ' see if input is valid
       IF INSTR("#@~0123456789*", MID$(instring$, cpos, 1)) THEN
          test$ = MID$(instring$, cpos, 1)
          'get valid char
          EXIT FOR
       END IF
     NEXT cpos

     ELSE
      'user press return
      GOTO redosformat
     END IF

     IF LEN(test$) > 0 THEN ' user format ok
      format$ = instring$
      instring$ = ""
     ELSE
      GOTO redosformat
     END IF
'
' test user format
'
   msg1$ = ""
   instring$ = ""
   msg2$ = ": Test your Format$ "

   GOSUB teststring
'***********************************************
   instring$ = "Y"
   msg1$ = ": maybe preset to Yes or No"
   format$ = "TEST another STRING format? (Y/N):*"   '   one char y/n
   msg2$ = ": only YN allowed"
   GOSUB teststring

   IF instring$ = "Y" THEN
   GOTO redosformat
   END IF
'*****************************************************************************
   msg1$ = ""

redonformat:
   msg2$ = " Enter Your Format String (no quotes): "
   format$ = msg2$ + STRING$(25, "#")

Qclrscrn 4, 20, 2, 78
   LOCATE 2, 28
   PRINT "TEST QFORMATEDITNUM";

LOCATE 6, 5
PRINT "Formats Allowed:";
LOCATE 7, 5
PRINT CHR$(34) + "99" + CHR$(34) + "          ' numbers only  < (99 max) each digit = to max value";
LOCATE 8, 5
PRINT CHR$(34) + "19" + CHR$(34) + "          ' (19) is max value";
LOCATE 9, 5
PRINT CHR$(34) + "9999999.99" + CHR$(34) + "  ' decimal may be any position;"
LOCATE 10, 5
PRINT CHR$(34) + "999.9999" + CHR$(34);
LOCATE 11, 5
PRINT "may not use prompt or messages in numeric input.";
LOCATE 12, 5
PRINT "remember numbers are input right to left ."

    instring$ = ""
    LOCATE 22, 3

    instring$ = Qformateditstr(instring$, format$, 1, ExitCode, 0, 0, 0, 0, 1, 1, 0)

    'test user input
    IF LEN(instring$) THEN
     FOR cpos = 1 TO LEN(instring$)
       ' see if input is valid
       IF INSTR("#@~0123456789*", MID$(instring$, cpos, 1)) THEN
          test$ = MID$(instring$, cpos, 1)
          'get valid char
          EXIT FOR
       END IF
     NEXT cpos

     ELSE
      'user press return
      GOTO redonformat
     END IF

     IF LEN(test$) > 0 THEN ' user format ok
      format$ = instring$
      instring$ = ""
     ELSE
      GOTO redonformat
     END IF
'
' test user format
'
   msg1$ = ""
   instring$ = ""
   msg2$ = ": Test your NUMERIC Format$ "

   GOSUB testnumeric
'***********************************************
   instring$ = "Y"
   msg1$ = ": maybe preset to Yes or No"
   format$ = "TEST another NUMERIC format? (Y/N):*"   '   one char y/n
   msg2$ = ": only YN allowed"
   GOSUB teststring

   IF instring$ = "Y" THEN
   GOTO redonformat
   END IF
   GOTO start
'*****************************************************************************
teststring:    ' this routine test formateditstr sub
'*****************************************************************************
   Qclrscrn 4, 20, 2, 78
   LOCATE 2, 28
   PRINT "TEST QFORMATEDITSTR";
   msg$ = "Press ENTER key to Continue (TAB to exit)"
   Qmessage msg$, 22

   LOCATE 6, 3
   PRINT "User String$ = "; CHR$(34) + instring$ + CHR$(34);
   LOCATE 7, 16
   PRINT msg1$;
   LOCATE 8, 3
   PRINT "User format$ = "; CHR$(34) + format$ + CHR$(34);
   LOCATE 9, 16
   PRINT msg2$;
   LOCATE 11, 3
   PRINT "Test Qformateditstr : ";
   ExitCode = 0  'returns 1 to 7  if flag set   see sub for details
   UPflag = 0  'True OR False  1 set for exitkey
   PUPflag = 0  'True OR False  2  ""
   DNflag = 0  'True OR False  3  ""
   PDNflag = 0  'True OR False  4  ""
   RTflag = 1  'True OR False  5  ""   return key exit program
   TABflag = 1  'True OR False  6  ""   tab key loops agian after pause
   ESCflag = 0  'True OR False  7  ""
   caseflag = 1
   scol1 = POS(0) + 1: srow1 = 10: ecol1 = LEN(format$) + POS(0) + 2: erow1 = 12

   Qsglbox scol1, srow1, ecol1, erow1
'
   LOCATE 11, scol1 + 1

   instring$ = Qformateditstr(instring$, format$, caseflag, ExitCode, UPflag, PUPflag, DNflag, PDNflag, RTflag, TABflag, ESCflag)
'
   LOCATE 13, 8
   PRINT "Length of string: ";
   PRINT LEN(instring$);
   LOCATE 14, 8
   PRINT "String as returned: ";
   PRINT instring$;

'use as statement
'   PRINT qremovechar$(instring$, " ")
'use as function
'   instring$ = qremovechar$(instring$, " ")

' remember if you pass string as parameter userformat modifies the string.
' if you  pass as value it won't change.
'     (string$) passed as value.
'      string$  passed as address.
' !! Quserformat alters string if passed as address !!
'    you can use removeformat to change it back.
'    instring$ = qremoveformat$(instring$, format$)

   LOCATE 16, 8
   PRINT "User formatted string "; Quserformat$((instring$), format$);

   LOCATE 17, 8
   PRINT "ExitCode : ";
   PRINT ExitCode%;

'       set flags TRUE to enable  exit on key
'       FALSE on entry Disables key exit
'       UPflag     = True  ,exitcode =  1
'       PGUPflag   = True  ,exitcode =  2
'       DNflag     = True  ,exitcode =  3
'       PGDNflag   = True  ,exitcode =  4
'       RETflag    = True  ,exitcode =  5
'       TABflag    = True  ,exitcode =  6
'       ESCflag    = True  ,exitcode =  7

   SELECT CASE ExitCode%
      CASE 1  'what to do if uparrow key exit
              'could be
              'GOTO previous entry
      CASE 2  'what to do if pageup key exit
      CASE 3  'what to do if downarrow key exit
              'could be
              'GOTO next entry
      CASE 4  'what to do if pagedown key exit
      CASE 5  'what to do if enter key exit
              'could be accept entry
              msg$ = "Anykey to Continue"
              Qmessage msg$, 22

'  Wait until there's a character
'
         choice$ = ""
         WHILE choice$ = ""
            choice$ = INKEY$
         WEND

'to be changed to a loop.
         
         msg$ = "ESC restores CTRL-E Clears, all other edit keys function normal"
         Qmessage msg$, 3
         RETURN
      CASE 6  'what to do if tab key exit
              'could be return to menu
         GOTO ENDPROG
      CASE 7  'what to do if esc key exit
              'string restored
   END SELECT
ENDPROG:
   COLOR WHITE, BLACK
   CLS
   END
'****************************************************************************
testnumeric:   ' code below is for testing numeric input routine
'****************************************************************************

   Qclrscrn 4, 20, 2, 78
   LOCATE 2, 28
   PRINT "TEST QFORMATEDITNUM"
   
   msg$ = "Press ENTER key to Continue (TAB to exit)"
   Qmessage msg$, 22

   LOCATE 6, 3
   PRINT "User String$ = "; CHR$(34) + instring$ + CHR$(34);

   LOCATE 7, 16
   PRINT msg1$;

   LOCATE 8, 3
   PRINT "User format$ = "; CHR$(34) + format$ + CHR$(34);

   LOCATE 9, 16
   PRINT msg2$;

   LOCATE 11, 3
   PRINT "Test Qformateditnum : ";

   ExitCode% = 0  'returns 1 to 7  if flag set   see sub for details
   UPflag = 0  'True OR False   set for exitkey
   PUPflag = 0  'True OR False    ""
   DNflag = 0  'True OR False    ""
   PDNflag = 0  'True OR False    ""
   RTflag = 1  'True OR False    ""   return key exit program
   TABflag = 1  'True OR False    ""   tab key loops agian after pause
   ESCflag = 0  'True or False    ""
'
   scol1 = POS(0) + 1: srow1 = 10: ecol1 = LEN(format$) + POS(0) + 2: erow1 = 12
   Qsglbox scol1, srow1, ecol1, erow1
    
'
   LOCATE 11, scol1 + 1

   instring$ = Qformateditnum(instring$, format$, ExitCode, UPflag, PUPflag, DNflag, PDNflag, RTflag, TABflag, ESCflag)
'
   LOCATE 13, 8
   PRINT "Length of string: ";
   PRINT LEN(instring$);

   LOCATE 14, 8
   PRINT "String as returned: ";
   PRINT instring$;

   LOCATE 16, 8
   PRINT "Print using #########.##"; USING "#########.##"; VAL(instring$)

   LOCATE 17, 8
   PRINT "ExitCode : ";
   PRINT ExitCode%;

   SELECT CASE ExitCode%
      CASE 1  'what to do if uparrow key exit
              'could be
              'GOTO previous entry

      CASE 2  'what to do if pageup key exit

      CASE 3  'what to do if downarrow key exit
              'could be
              'GOTO next entry

      CASE 4  'what to do if pagedown key exit

      CASE 5  'what to do if enter key exit
              'could be accept entry
         
              msg$ = "Anykey to Continue "
              Qmessage msg$, 22
'  Wait until there's a character
'
         choice$ = ""
         WHILE choice$ = ""
            choice$ = INKEY$
         WEND

'to be changed to a loop.
         
         msg$ = "ESC restores CTRL-E Clears, all other edit keys function normal"
         Qmessage msg$, 3
         RETURN

      CASE 6  'what to do if tab key exit
              'could be return to menu
         GOTO ENDPROG
   END SELECT

   CLS
   END

'DATE: 05/30/90
'
'clear line from cursur to end of line without moving cursor
'
'
   SUB Qclreol
      retpos = POS(0)
      clrlen = 79 - POS(0)
      PRINT SPACE$(clrlen);
      LOCATE , retpos
   END SUB

SUB Qclrscrn (startline, endline, startcol, endcol)

     FOR c = startline TO endline
     LOCATE c, startcol
     PRINT STRING$(endcol - startcol, " ");
     NEXT

END SUB

'
  SUB Qdblbox (leftcol, leftrow, rightcol, rightrow)
' call routine
'    leftcol = 1: leftrow = 1: rightcol = 80: rightrow = 23
'    call Qdblbox(leftcol,leftrow,rightcol,rightrow)
'Qdblbox
      LOCATE leftrow, leftcol
'draw top of box
      PRINT CHR$(201);
      FOR i = (leftcol + 1) TO (rightcol - 1)
         PRINT CHR$(205);
      NEXT i
      PRINT CHR$(187)
'draw side of box
      FOR i = (leftrow + 1) TO (rightrow - 1)
         LOCATE i, leftcol
         PRINT CHR$(186);
         LOCATE i, rightcol
         PRINT CHR$(186);
      NEXT i
'draw bottom of box
      LOCATE rightrow, leftcol
      PRINT CHR$(200);
      FOR i = (leftcol + 1) TO (rightcol - 1)
         PRINT CHR$(205);
      NEXT i
      PRINT CHR$(188);
   END SUB

'
'draws border around screen
'
   SUB Qdrawscreen

      LOCATE 2, 4
      PRINT DATE$;

      LOCATE 2, 65
      PRINT "Version 2.00";

      msg$ = "COPYRIGHT 1990     Formatted Input Routine      BY: RAYMOND E DIXON"
      Qmessage msg$, 24

      Qdblbox 1, 1, 80, 25
      Qsglbox 2, 21, 79, 23

   END SUB

'DATE: 05/30/90
'     sub required with Qformateditnum
'
   SUB QformatDEC (number$, beforeDEC, afterdec)
'
'  Sub Routine to handle the number of decimal characters in a string
'
      length = LEN(number$)
      delimit = INSTR(number$, ".")
      IF delimit = 0 THEN
         beforeDEC = length
         afterdec = 0
      END IF
      IF delimit <> 0 THEN
         IF LEFT$(number$, 1) = "." THEN
            beforeDEC = 0
            afterdec = length - 1
         END IF
         IF RIGHT$(number$, 1) = "." THEN
            afterdec = 0
            beforeDEC = length - 1
         END IF
         IF delimit <> 1 OR delimit <> length THEN
            beforeDEC = delimit - 1
            afterdec = (length - beforeDEC) - 1
         END IF
      END IF
      IF length = 0 THEN
         beforeDEC = 0
         afterdec = 0
      END IF
   END SUB

'DATE: 05/30/90
'           numeric formats allow higest
'           value of format position.
'
'      format$ = "99999.99" decimal   ( any decimal position)
'      format$ = "99" numbers only  < (99 max) each digit = to max value
'      format$ = "19" (19) is max value
'
'      use basic print using "####.##";VAL(instring$) for decimal numbers
'      or integer.            decimal  pos and length optional
'
'       USE LOCATE ROW,COLUMN
'
'       maybe passed by parameters if you like to add to parms
'
'       column = Column pos to start printing
'       Row = Row to start printing
'
'       set editforeground color  before call
'       set editbackgroung color  before call
'
'       ExitCode = VALUE EXIT  1 TO 7
'
'       set flags to enable  to exit on key
'
'       UPflag     = True  ,exitcode =  1
'       PGUPflag   = True  ,exitcode =  2
'       DNflag     = True  ,exitcode =  3
'       PGDNflag   = True  ,exitcode =  4
'       RETflag    = True  ,exitcode =  5
'       TABflag    = True  ,exitcode =  6
'       ESCflag    = True  ,exitcode =  7
'
'       ESC key restores field if True or False
'
'   sample how to handle exitcode after input routine (see program).
'
'   SELECT CASE ExitCode%
'
'       CASE 1 'what to do if uparrow key exit
'               could be
'               GOTO previous entry
'
'       CASE 2 'what to do if pageup key exit
'
'       CASE 3 'what to do if downarrow key exit
'               could be
'               GOTO next entry
'       CASE 4 'what to do if pagedown key exit
'
'       CASE 5 'what to do if enter key exit
'                could be accept entry
'       CASE 6 'what to do if tab key exit
'               'could be return to menu
'
'   END SELECT
'
'
   FUNCTION Qformateditnum$ (work$, format$, ExitCode, UPflag, PGUPflag, DNflag, PGDNflag, RETflag, TABflag, ESCflag)

      SHARED editbackground, editforeground

'
' Define names similar to keyboard names with their equivalent key codes.
'  const maybe moved to main code and used for all routines
'
      CONST SPACE = 32, ESC = 27, enter = 13, TABKEY = 9
      CONST DOWN = 80, UP = 72, LEFT = 75, RIGHT = 77
      CONST HOME = 71, ENDK = 79, PGDN = 81, PGUP = 73
      CONST INS = 82, DEL = 83, NULL = 0, CTRLE = 5
      CONST CTRLD = 4, CTRLG = 7, CTRLH = 8, CTRLS = 19, CTRLV = 22
      CONST True = 1, False = 0
      STATIC curpos  'retain cursor pos.
'
' comment out next two lines and pass row and col as parameters
' if you would too.
'
      row = CSRLIN
      col = POS(0)
      firsttime = 1
      length = LEN(format$)
'
      SELECT CASE LEN(work$)
         CASE IS > length
'
'Make work$ the right length
'
            work$ = RIGHT$(work$, length)
         CASE IS < length
            work$ = STRING$(length - LEN(work$) - 1, " ") + work$
      END SELECT
      
      IF INSTR(format$, ".") THEN
         decflag = 1
         IF INSTR(work$, ".") THEN
            
            QformatDEC (work$), bforeDEC, aftDEC
            QformatDEC (format$), beforeDEC, afterdec
            work$ = Qremovechar$((work$), ".")
            IF afterdec > aftDEC THEN
               work$ = work$ + STRING$(afterdec - (aftDEC - 1), "0")
            END IF
            IF afterdec < aftDEC THEN
               work$ = STRING$(aftDEC - (afterdec - 1), " ") + LEFT$(work$, beforeDEC + (afterdec - 1))
            END IF
         ELSE
            QformatDEC format$, beforeDEC, afterdec
            work$ = work$ + STRING$(afterdec + 1, "0")
         END IF
      ELSE
         QformatDEC (work$), beforeDEC, afterdec
         work$ = LEFT$(work$, beforeDEC)
         afterdec = 0
         work$ = STRING$(length - LEN(work$), " ") + work$
         decflag = 0
      END IF
'
'length of input = to format set by user
'length of format$ is edit length not user length

      
      SELECT CASE LEN(work$)
         CASE IS > length
'
'Make work$ the right length after dec adjust
'
            work$ = RIGHT$(work$, length)
         CASE IS < length
            IF decflag THEN
               work$ = STRING$(length - LEN(work$) - 1, " ") + work$
            ELSE
               work$ = STRING$(length - LEN(work$), " ") + work$
            END IF
      END SELECT
'
'print user data with formated string
'
      temp$ = work$
      work$ = STRING$(length, " ")
'
'step through format$ and insert org characters
'
      k = 1
      FOR j = 1 TO length
         Character$ = MID$(format$, j, 1)
         IF INSTR(".", Character$) THEN
            MID$(work$, j, 1) = Character$
         ELSE
'
'mix with format$
'
            char$ = MID$(temp$, k, 1)
            MID$(work$, j, 1) = char$
            k = k + 1
         END IF
      NEXT j
'
' got formatted string so save for ESC and restore.
'
      org$ = work$
      curpos = 1
      ExitCode = 0
'
' EDIT in reverse video
'
      COLOR editforeground, editbackground
      LOCATE row, col
      PRINT work$;  '
'
' loop until an exit
'
      DO
         
         SELECT CASE curpos
'
'  Cursor position too long
'
            CASE IS > length
               curpos = length
            CASE IS < 1
               curpos = 1
         END SELECT
'
         LOCATE row, col
         PRINT work$;

'
'set cursor to end of field
'
         LOCATE row, col + length - 1, 1, 7, 7
'
'  Wait until there's a character
'
         choice$ = ""
         WHILE choice$ = ""
            choice$ = INKEY$
         WEND
         LOCATE , , 0
'
'  Normal character
'
         IF LEN(choice$) = 1 THEN
            special$ = MID$(format$, curpos, 1)
            keychoice = ASC(choice$)
            SELECT CASE keychoice
               CASE enter
'
'return exitcode is set if flag set
'
                  IF RETflag = True THEN
                     ExitCode = 5
                     EXIT DO
                  END IF
               CASE TABKEY  'TAB is set
                  IF TABflag = True THEN
                     ExitCode = 6
                     EXIT DO
                  END IF
               CASE ESC  ' ESC restores edit string
                  work$ = org$
                  curpos = 1
                  IF ESCflag = True THEN
                     ExitCode = 7
                     EXIT DO
                  END IF
               CASE CTRLE  'erase number

                  work$ = ""

                  IF LEN(work$) = 0 THEN
                     IF afterdec > 0 THEN
                        work$ = STRING$(afterdec, "0")
                         IF LEN(work$) < length THEN
                           IF decflag THEN
                              work$ = STRING$(length - LEN(work$) - 1, " ") + work$
                           ELSE
                              work$ = STRING$(length - LEN(work$), " ") + work$
                           END IF
                           work$ = Quserformat$(work$, format$)
                        END IF
                     ELSE
                        work$ = ""
                        IF LEN(work$) < length THEN
                           IF decflag THEN
                              work$ = STRING$(length - LEN(work$) - 1, " ") + work$
                           ELSE
                              work$ = STRING$(length - LEN(work$), " ") + work$
                           END IF
                           work$ = Quserformat$(work$, format$)
                        END IF
                     END IF
                  END IF
            END SELECT
'
            SELECT CASE special$
               CASE "0" TO "9"  'get numbers only

                  IF choice$ <= special$ THEN  'get pos max value
                     keychoice = ASC(choice$)
                  ELSE
                     keychoice = 0
                  END IF
            END SELECT
'
            SELECT CASE choice$

               CASE "-"  'handle neg numbers
                  temp$ = work$
                  work$ = ""

                  IF LEN(work$) = 0 THEN
                     IF afterdec > 0 THEN
                        work$ = LTRIM$(RTRIM$(choice$)) + STRING$(afterdec, "0")
                         IF LEN(work$) < length THEN
                           IF decflag THEN
                              work$ = STRING$(length - LEN(work$) - 1, " ") + work$
                           ELSE
                              work$ = STRING$(length - LEN(work$), " ") + work$
                           END IF
                           work$ = Quserformat$(work$, format$)
                        END IF
                     ELSE
                        work$ = LTRIM$(RTRIM$(choice$))
                        IF LEN(work$) < length THEN
                           IF decflag THEN
                              work$ = STRING$(length - LEN(work$) - 1, " ") + work$
                           ELSE
                              work$ = STRING$(length - LEN(work$), " ") + work$
                           END IF
                           work$ = Quserformat$(work$, format$)
                        END IF
                     END IF
                  END IF
            END SELECT
'
            SELECT CASE CHR$(keychoice)

               CASE "0" TO "9"  'numbers only

                  FOR j = 1 TO length
                     Character$ = MID$(format$, j, 1)
                     IF INSTR(".", Character$) THEN
                        MID$(work$, j, 1) = CHR$(255)
                        cursor = cursor + 1
                     END IF
                  NEXT j
                     
                     work$ = Qremovechar$(work$, CHR$(255))
                     IF firsttime = 1 THEN
                     work$ = STRING$(afterdec, "0") + LTRIM$(RTRIM$(choice$))

                     firsttime = 0
                     ELSE
                     work$ = LTRIM$(RTRIM$(work$)) + LTRIM$(RTRIM$(choice$))
                     END IF
'
                  IF afterdec > 0 THEN
                     IF LEN(work$) >= afterdec THEN
                        IF LEFT$(work$, 1) = "0" THEN
                           work$ = RIGHT$(work$, LEN(work$) - 1)
                        END IF
                     END IF
                  END IF

                  IF afterdec > 0 THEN
                  IF LEN(work$) >= afterdec + 1 THEN
                     IF MID$(work$, 2, 1) = "0" THEN
                        work$ = "-" + RIGHT$(work$, LEN(work$) - 2)
                     END IF
                  END IF
                  END IF
'
                  IF LEN(work$) < length THEN
                     IF decflag THEN
                        work$ = STRING$(length - LEN(work$) - 1, " ") + work$
                     ELSE
                        work$ = STRING$(length - LEN(work$), " ") + work$
                     END IF
                  END IF
                  work$ = Quserformat$(work$, format$)
                  curpos = curpos + 1
            END SELECT
         ELSE
'
'Extended character
'
            keychoice = ASC(MID$(choice$, 2))
            SELECT CASE keychoice
               CASE DEL  '  Delete
'remove format for delete
                  FOR j = 1 TO length
                     Character$ = MID$(format$, j, 1)
                     IF INSTR(".", Character$) THEN
                        MID$(work$, j, 1) = CHR$(255)
                     ELSE
                     END IF
                  NEXT j
'
'remove dummy blanks
'
                  work$ = Qremovechar$(work$, CHR$(255))
                  work$ = LTRIM$(RTRIM$(work$))
                  IF afterdec > 0 THEN
                     IF LEN(work$) <= afterdec THEN
                        work$ = "0" + work$
                     END IF
                  END IF
                  IF LEN(work$) THEN
                     work$ = LEFT$(work$, LEN(work$) - 1)
                     IF decflag THEN
                        work$ = STRING$(length - LEN(work$) - 1, " ") + work$
                     ELSE
                        work$ = STRING$(length - LEN(work$), " ") + work$
                     END IF
                  END IF
                  work$ = Quserformat$((work$), format$)
                  curpos = curpos - 1
               CASE UP  '  Up arrow
                  IF UPflag = True THEN
                     ExitCode = 1
                     EXIT DO
                  END IF
               CASE PGUP  '  Page up
                  IF PGUPflag = True THEN
                     ExitCode = 2
                     EXIT DO
                  END IF
               CASE PGDN  '  Page down
                  IF PGDNflag = True THEN
                     ExitCode = 4
                     EXIT DO
                  END IF
               CASE DOWN  '  Down arrow
                  IF DNflag = True THEN
                     ExitCode = 3
                     EXIT DO
                  END IF
               CASE ELSE
            END SELECT
         END IF
      firsttime = 0
      LOOP WHILE ExitCode = 0
'
'all done now clean up
'
      COLOR normal, BACKGROUND  'set color to normal

      LOCATE row, col, CURSOROFF
      PRINT work$;

'
' REMOVE format$
'
      FOR j = 1 TO length
         Character$ = MID$(format$, j, 1)
         char$ = MID$(work$, j, 1)
         IF INSTR(".", Character$) THEN
'skip
         ELSE
'
'remove temp blanks
'
            IF char$ = CHR$(255) THEN
'skip
            ELSE
               tmp$ = tmp$ + char$
            END IF
         END IF
      NEXT j
      COLOR normal, BACKGROUND  'set color to normal
'
'remove any spaces
'
      tmp$ = RTRIM$(LTRIM$(tmp$))
      IF LEN(tmp$) - 1 < afterdec THEN
         IF LEFT$(tmp$, 1) = "-" THEN
            tmp$ = "-" + STRING$(afterdec - LEN(tmp$) + 1, "0") + RIGHT$(tmp$, LEN(tmp$) - 1)
         END IF
      END IF
      IF LEN(tmp$) < 2 THEN
         tmp$ = "0" + tmp$
      END IF
'
'reinsert decimal in correct position
'
      IF decflag THEN
         rwork$ = RIGHT$(tmp$, afterdec)
         lwork$ = LEFT$(tmp$, LEN(tmp$) - LEN(rwork$))
         work$ = lwork$ + "." + rwork$
      END IF
'

      Qformateditnum$ = LTRIM$(RTRIM$(work$))
'
   END FUNCTION

'DATE: 05/30/90
'                     Raymond E Dixon
'                     5815 Buckley dr
'                     Jacksonville, Fl 32244
'                     (904) 778-4048
'
'  IF ANYONE MAKES ANY INPROVEMENTS I WOULD LIKE YOU TO RENAME THIS SUB
'  TO A NEW NAME. AND IF YOU WOULD SEND ME A COPY.
'
'       formated input routine with user format
'
'       assign values before calling routine
'
'       work$ =""  or string to edit
'
'           numeric formats allow higest
'           value of format position.
'
'      format$ = "99" numbers only  < (99 max) each digit = to max value
'      format$ = "19" (19) is max value
'      format$ = "999-99-9999" SS number
'      format$ = "999-9999"       7  digit phone
'      format$ = "(999) 999-9999" 10 digit phone
'      format$ = "19/39/99"  date format
'      format$ = "########" alphanumeric set for 8 characters (maybe more or less)
'      format$ = "@@@@@@@@" alpha only   same as above
'      format$ = "Y/N:*"    force YN answer.
'      format$ = "M/F:|"    force MF answer.
'      format$ = "~"       'force enter key for prompts or other exit key.
'      format$ = may be any format you can create in a basic string
'                even you can include the Prompt if you like.
'
'      format$ = "Test Data: 99" 'this format will print
'                 Test Data: your value passed
'                            in the the length of 2
'                            Seting numbers 1 to 99.
'
'       USE LOCATE ROW,COLUMN
'
'       maybe passed by parameters if you like to add to parms
'
'       column = Column pos to start printing
'       Row = Row to start printing
'
'       set foreground color  before call
'
'       set backgroung color  before call
'
'       ExitCode = VALUE EXIT  1 TO 7
'
'       set flags to enable  to exit on key
'
'       UPflag     = True  ,exitcode =  1
'       PGUPflag   = True  ,exitcode =  2
'       DNflag     = True  ,exitcode =  3
'       PGDNflag   = True  ,exitcode =  4
'       RETflag    = True  ,exitcode =  5
'       TABflag    = True  ,exitcode =  6
'       ESCflag    = True  ,exitcode =  7
'
'       ESC key restores field if True or False
'
'        force case if set.
'                        caseflag = 0 any case
'                                 = 1 for upper
'                                 = 2 for lower
'
'   sample how to handle exitcode after input routine (see program).
'
'   SELECT CASE ExitCode%
'
'       CASE 1 'what to do if uparrow key exit
'               could be
'               GOTO previous entry
'
'       CASE 2 'what to do if pageup key exit
'
'       CASE 3 'what to do if downarrow key exit
'               could be
'               GOTO next entry
'       CASE 4 'what to do if pagedown key exit
'
'       CASE 5 'what to do if enter key exit
'                could be accept entry
'       CASE 6 'what to do if tab key exit
'               'could be return to menu
'
'   END SELECT
'
'
   FUNCTION Qformateditstr$ (work$, format$, caseflag, ExitCode, UPflag, PGUPflag, DNflag, PGDNflag, RETflag, TABflag, ESCflag)
'
' Define names similar to keyboard names with their equivalent key codes.
'  const maybe moved to main code and used for all routines
'
      CONST SPACE = 32, ESC = 27, enter = 13, TABKEY = 9
      CONST DOWN = 80, UP = 72, LEFT = 75, RIGHT = 77
      CONST HOME = 71, ENDK = 79, PGDN = 81, PGUP = 73
      CONST INS = 82, DEL = 83, NULL = 0, CTRLE = 5
      CONST CTRLD = 4, CTRLG = 7, CTRLH = 8, CTRLS = 19, CTRLV = 22
      CONST True = 1, False = NOT True
      STATIC insertmode, curpos  'retain insert mode and cursor pos.
'
' comment out next two lines and pass row and col as parameters
' if you would too.
'
      SHARED editbackground, editforeground

      row = CSRLIN
      col = POS(0)
      firsttime = 1
'
'step through format$
'
      length = LEN(format$)
      FOR j = 1 TO length
         FChr$ = MID$(format$, j, 1)
         SELECT CASE FChr$
'
'skip special characters
'
            CASE "~", "@", "0" TO "9", "#", "*", "|"
            CASE ELSE
'
'values to skip over in format
'
               formatVALUES$ = formatVALUES$ + FChr$
         END SELECT
      NEXT j
'
'length of input = to format set by user
'length of format$ is edit length not user length
'

'
'  Insert Mode flag
'
      insertmode = 0

      SELECT CASE LEN(work$)
         CASE IS > length
'
'String too long
'Make work$ the right length
'
            work$ = MID$(work$, 1, length)
         CASE IS < length
            work$ = work$ + STRING$(length - LEN(work$), SPACE)
      END SELECT
'
'print user data with formated string

      temp$ = work$
      work$ = STRING$(length, " ")
      
'
'set to start of org string
'
      k = 1
'
'step through format$ and insert org characters
'
      FOR j = 1 TO length
         Character$ = MID$(format$, j, 1)
         IF INSTR(formatVALUES$, Character$) THEN
            MID$(work$, j, 1) = Character$
         ELSE
'
'mix with format$
'
            char$ = MID$(temp$, k, 1)
            MID$(work$, j, 1) = char$
            k = k + 1
         END IF
      NEXT j
'
' got formatted string so save for ESC and restore.
'
      org$ = work$
      curpos = 1
      ExitCode = 0
'
' EDIT in reverse video

      COLOR editforeground, editbackground
      LOCATE row, col
      PRINT work$;

'
' loop until an exit
'
      DO
         DO
            IF INSTR(formatVALUES$, MID$(format$, curpos, 1)) THEN
               curpos = curpos + 1
            ELSE
               EXIT DO
            END IF
            IF curpos > length THEN
               curpos = length
               DO
                  IF INSTR(formatVALUES$, MID$(format$, curpos, 1)) THEN
                     curpos = curpos - 1
                  ELSE
                     EXIT DO
                  END IF
               LOOP
            END IF
         LOOP
         SELECT CASE curpos
'
'  Cursor position too long
'
            CASE IS > length
               curpos = length
            CASE IS < 1
               curpos = 1
         END SELECT

       LOCATE row, col

         PRINT work$;
'
' change curor for insert mode
'InsertMode is on
'
         IF insertmode = True THEN
            LOCATE row, col + curpos - 1, 1, 0, 15
         ELSE
            LOCATE row, col + curpos - 1, 1, 7, 7
         END IF

      IF INSTR(format$, "~") THEN
      LOCATE row, col + curpos - 1, 0, 7, 7
      END IF
'
'  Wait until there's a character
'
         choice$ = ""
         WHILE choice$ = ""
            choice$ = INKEY$
         WEND
         LOCATE , , 0
'
'  Normal character
'
         IF LEN(choice$) = 1 THEN
            special$ = MID$(format$, curpos, 1)
            keychoice = ASC(choice$)
            SELECT CASE keychoice
               CASE enter
'
'return is set
'
                  IF RETflag = True THEN
                     ExitCode = 5
                     EXIT DO
                  END IF
               CASE TABKEY  'TAB is set
                  IF TABflag = True THEN
                     ExitCode = 6
                     EXIT DO
                  END IF
               CASE CTRLE  ' CTRL E erases edit string
                  work$ = STRING$(length, " ")
                  temp$ = STRING$(length, " ")
'
'set to start of org string
'
      k = 1
'
'step through format$ and insert org characters
'
      FOR j = 1 TO length
         Character$ = MID$(format$, j, 1)
         IF INSTR(formatVALUES$, Character$) THEN
            MID$(work$, j, 1) = Character$
         ELSE
'
'mix with format$
'
            char$ = MID$(temp$, k, 1)
            MID$(work$, j, 1) = char$
            k = k + 1
         END IF
      NEXT j

                  curpos = 1

               CASE ESC  ' ESC restores edit string
                  work$ = org$
                  curpos = 1
                  IF ESCflag = True THEN
                     ExitCode = 7
                     EXIT DO
                  END IF
            END SELECT
            SELECT CASE special$
               CASE "0" TO "9"  'get numbers only
                  IF choice$ <= special$ THEN
                     keychoice = ASC(choice$)
                  ELSE
                     keychoice = 0
                  END IF
               CASE "@"  ' force alpha only
                  IF UCASE$(choice$) >= "A" AND UCASE$(choice$) <= "Z" OR choice$ = " " OR choice$ = CHR$(8) THEN
                     keychoice = ASC(choice$)
                  ELSE
                     keychoice = 0
                  END IF
               CASE "*"  ' force YN only
                  IF UCASE$(choice$) = "Y" OR UCASE$(choice$) = "N" OR choice$ = " " THEN
                     keychoice = ASC(choice$)
                  ELSE
                     keychoice = 0
                  END IF
               CASE "|"  ' force MF only
                  IF UCASE$(choice$) = "M" OR UCASE$(choice$) = "F" OR choice$ = " " THEN
                     keychoice = ASC(choice$)
                  ELSE
                     keychoice = 0
                  END IF

               CASE "~"  'force enter only
                  IF UCASE$(choice$) = "" THEN
                     keychoice = ASC(choice$)
                  ELSE
                     keychoice = 0
                  END IF
            END SELECT

            SELECT CASE keychoice
               CASE SPACE TO 126  '  Normal ascii char
                  SELECT CASE caseflag
                     CASE 1  '  Make it upper
                        choice$ = UCASE$(choice$)
                        keychoice = ASC(choice$)
                     CASE 2  '  Make it lower
                        choice$ = LCASE$(choice$)
                        keychoice = ASC(choice$)
                  END SELECT
                  IF insertmode = 0 THEN

                     MID$(work$, curpos, 1) = CHR$(keychoice)
                     curpos = curpos + 1

                     IF firsttime = 1 THEN

                     work$ = choice$ + STRING$(length - 1, " ")
                     work$ = Quserformat$((work$), format$)

                     firsttime = 0

                     END IF
                  END IF

                  IF insertmode = 1 THEN
'
' REMOVE format$
'
                     FOR j = 1 TO length
                        Character$ = MID$(format$, j, 1)
                        IF INSTR(formatVALUES$, Character$) THEN
                           MID$(work$, j, 1) = CHR$(255)
                           cursor = cursor + 1
                        END IF
                     NEXT j
                     IF curpos < length THEN
                        lwork$ = LTRIM$(LEFT$(work$, curpos - 1))
                        rwork$ = RTRIM$(RIGHT$(work$, length - (curpos - 1)))
                        work$ = LEFT$(lwork$ + choice$ + rwork$, length)
                        curpos = curpos + 1
                     ELSE
                        BEEP
                     END IF
                     
                     work$ = Qremovechar$((work$), CHR$(255))
                     work$ = Quserformat$((work$), format$)

                  END IF
               CASE 8, 127  '  Back space
                  IF curpos% > 1 THEN
                     IF INSTR(formatVALUES$, MID$(format$, curpos%, 1)) = 0 THEN
                        MID$(work$, curpos%, 1) = " "
                        curpos% = curpos% - 1
                     END IF
                     DO
                        IF curpos% > 0 THEN
                           IF INSTR(formatVALUES$, MID$(format$, curpos%, 1)) THEN
                              curpos% = curpos% - 1
                           ELSE
                              EXIT DO
                           END IF
                        ELSE
                           EXIT DO
                        END IF
                     LOOP
                     IF curpos% = 0 THEN
                        DO
                           curpos% = curpos% + 1
                           IF INSTR(formatVALUES$, MID$(format$, curpos%, 1)) = 0 THEN
                              EXIT DO
                           ELSE
                              EXIT DO
                           END IF
                        LOOP
                     END IF
                  END IF
               CASE ELSE
            END SELECT
         ELSE
'
'Extended character
'           firsttime = 0

            keychoice = ASC(MID$(choice$, 2))
            SELECT CASE keychoice
               CASE LEFT  '  Left arrow
                  IF curpos > 1 THEN
                     curpos = curpos - 1
                     DO
                        IF curpos > 0 THEN
                           IF INSTR(formatVALUES$, MID$(format$, curpos, 1)) THEN
                              curpos = curpos - 1
                           ELSE
                              EXIT DO
                           END IF
                        ELSE
                           EXIT DO
                        END IF
                     LOOP
                     IF curpos = 0 THEN
                        DO
                           curpos = curpos + 1
                           IF INSTR(formatVALUES$, MID$(format$, curpos, 1)) = 0 THEN
                              EXIT DO
                           END IF
                        LOOP
                     END IF
                  END IF
               CASE RIGHT  'Right arrow
                  curpos = curpos + 1
               CASE HOME  'Home key
                  curpos = 1
               CASE ENDK  '  End  key
                  curpos = length
               CASE INS  '  InsertMode
'
'toggle insert mode
'
                  insertmode = 1 - insertmode
               CASE DEL  '  Delete
                  MID$(work$, curpos, 1) = CHR$(255)
'
' REMOVE format$
'
                  FOR j = 1 TO length
                     Character$ = MID$(format$, j, 1)
                     IF INSTR(formatVALUES$, Character$) THEN
                        MID$(work$, j, 1) = CHR$(255)
                     END IF
                  NEXT j
                  IF curpos < length THEN
                     FOR j = curpos TO leng
                        IF j < length - 1 THEN
                           char$ = MID$(work$, j + 1, 1)
                           MID$(work$, j, 1) = char$
                           MID$(work$, length, 1) = CHR$(255)
                        END IF
                     NEXT j
                  END IF
                  work$ = Qremovechar$((work$), CHR$(255))
                  work$ = Quserformat$((work$), format$)
               CASE UP  '  Up arrow
                  IF UPflag = True THEN
                     ExitCode = 1
                     EXIT DO
                  END IF
               CASE PGUP  '  Page up
                  IF PGUPflag = True THEN
                     ExitCode = 2
                     EXIT DO
                  END IF
               CASE PGDN  '  Page down
                  IF PGDNflag = True THEN
                     ExitCode = 4
                     EXIT DO
                  END IF
               CASE DOWN  '  Down arrow
                  IF DNflag = True THEN
                     ExitCode = 3
                     EXIT DO
                  END IF
               CASE ELSE
            END SELECT
         END IF
      firsttime = 0
      LOOP WHILE ExitCode = 0
'
'all done now clean up
'
      COLOR normal, BACKGROUND  'set color to normal
      LOCATE row, col, CURSOROFF
      PRINT work$;

'
' REMOVE format$
'
      FOR j = 1 TO length
         Character$ = MID$(format$, j, 1)
         char$ = MID$(work$, j, 1)
         IF INSTR(formatVALUES$, Character$) THEN
'skip
         ELSE
'
'remove temp blanks
'
            IF char$ = CHR$(255) THEN
'skip
            ELSE
               tmp$ = tmp$ + char$
            END IF
         END IF
      NEXT j

'
'remove any spaces
'
      Qformateditstr$ = RTRIM$(LTRIM$(tmp$))
'
   END FUNCTION

'prints msg at row
'
   SUB Qmessage (msg$, row)
      LOCATE row, 3
      PRINT SPACE$(76)
      ml = 80 - LEN(msg$)
      mp = ml \ 2
      LOCATE row, mp
      PRINT msg$;
   END SUB

'DATE: 05/30/90
'
   FUNCTION Qremovechar$ (userstring$, skip$)
'
      length = LEN(userstring$)  'Get length of string.
      Character$ = ""
      FOR k = 1 TO length
'
'Get individual Character from string, from left to right.
'
         char$ = MID$(userstring$, k, 1)
'
'Test for valid chararacter.
'
         IF char$ = skip$ THEN
'
'skip unwanted character
'
         ELSE
'
'add character to string
'
            Character$ = Character$ + char$
         END IF
      NEXT
'
      Qremovechar$ = Character$
'
   END FUNCTION

'DATE: 05/30/90
'  remove user format from string
'  see Quserformat$ for def of format
'
   FUNCTION Qremoveformat$ (work$, format$) STATIC
      IF LEN(work$) < LEN(format$) THEN
         EXIT FUNCTION
      END IF
      length = LEN(format$)
' REMOVE format$
      FOR j = 1 TO length
         Character$ = MID$(format$, j, 1)
         char$ = MID$(work$, j, 1)
         IF INSTR(formatVALUES$, Character$) THEN
'skip
         ELSE
            IF char$ = CHR$(255) THEN
'skip
            ELSE
               tmp$ = tmp$ + char$
            END IF
         END IF
      NEXT j
      Qremoveformat$ = RTRIM$(LTRIM$(tmp$))
   END FUNCTION

'
SUB Qsglbox (scol1, srow1, ecol1, erow1)
'    scol1 = 1: srow1 = 1: ecol1 = 80: erow1 = 23
      LOCATE srow1, scol1
'top
      PRINT CHR$(218);
      FOR i = (scol1 + 1) TO (ecol1 - 1)
         PRINT CHR$(196);
      NEXT i
      PRINT CHR$(191)
'sides
      FOR i = (srow1 + 1) TO (erow1 - 1)
         LOCATE i, scol1
         PRINT CHR$(179);
         LOCATE i, ecol1
         PRINT CHR$(179);
      NEXT i
'bottom
      LOCATE erow1, scol1
      PRINT CHR$(192);
      FOR i = (scol1 + 1) TO (ecol1 - 1)
         PRINT CHR$(196);
      NEXT i
      PRINT CHR$(217)
   END SUB

'DATE: 05/30/90
'    will print string using format$
'    or convert to formated string
'      not for decimal numbers
'
'      format$ = "99" numbers only  < (99 max) each digit = to max value
'      format$ = "19" (19) is max value
'      format$ = "999-99-9999" SS number
'      format$ = "999-9999"       7  digit phone
'      format$ = "(999) 999-9999" 10 digit phone
'      format$ = "19/39/99"  date format
'      format$ = "########" alphanumeric set for 8 characters (maybe more or less)
'      format$ = "@@@@@@@@" alpha only   same as above
'      format$ = "Y/N:*"    force YN answer.
'      format$ = "~"        force enter key for prompts or other exit key.
'      format$ = may be any format you can create in a basic string
'                even you can include the Prompt if you like.
'
'      format$ = "Test Data: 99" 'this format will print
'                 Test Data: your value passed
'                            in the the length of 2
'                            Seting numbers 1 to 99.
'
'    locate row,col
'    print Quserformat$(string$,Format$);
'                  or
'    print Quserformat$("7784048","999-9999");
'                  or
'    a$ = Quserformat$(string$,Format$)
'    print a$;
'
'    output would be:  778-4048
'
' remember if you pass string as parameter userformat modifies the string.
' if you  pass as value it won't change.
'     (string$) passed as value.
'      string$  passed as address.
'
' !! Quserformat alters string if passed as address !!
'    you can use removeformat to change it back.
'    instring$ = qremoveformat$(instring$,format$)
'
'
   FUNCTION Quserformat$ (work$, format$)
'
'step through format$
'
      length = LEN(format$)
      FOR j = 1 TO length
         FChr$ = MID$(format$, j, 1)
         SELECT CASE FChr$
'
'skip special characters
'
            CASE "~", "@", "0" TO "9", "#", "*", "|"
            CASE ELSE
'
'values to skip over in format
'
               formatVALUES$ = formatVALUES$ + FChr$
         END SELECT
      NEXT j
'
'print user data with formated string
'
      temp$ = work$
      work$ = STRING$(length, " ")
'
'set to start of org string
'
      k = 1
'
'step through format$ and insert org characters
'
      FOR j = 1 TO length
         Character$ = MID$(format$, j, 1)
         IF INSTR(formatVALUES$, Character$) THEN
            MID$(work$, j, 1) = Character$
         ELSE
'
'mix with format$
'
            char$ = MID$(temp$, k, 1)
            MID$(work$, j, 1) = char$
            k = k + 1
         END IF
      NEXT j
'
      Quserformat$ = work$
'
   END FUNCTION

