' Name:        GetInput$ Function
' Author:      Kunal Arya (arya@apc.net)
' Usage:
'              Row - The Row for the textbox to be printed
'              Col - The Column for the textbox to be printed
'              PasswordChar$ - Character to be printed instead of
'                              text. It'll print the text if it's
'                              left blank.
'              MaxWidth - The Maximum width that the user can type
'                         NOTE: This will not be affected if there
'                               is a mask.
'              Fore - Foreground color of the text
'              Back - Background color of the text
'              Empty$ - The character to be printed in the empty areas
'              Insert - Determines state of insert (on/off)
'              Default$ - Text that is already in the textbox
'              Mask$ - Determines the mask string
'                      Usage of mask:
'                      You may put any character or number in this string.
'                      To set where numbers are to be put, insert a "#".
'                      To set where all others are to be put, insert a "$".
'                      If you want to put in a "$" to be printed, insert a "~"
'                           example: "~###.##" would turn to "$###.##"
'              Flags - Defines the following flags:
'                      qbSetUp - Used to put the textbox before it is
'                                accessed.
'                      qbShowMask - For showing entire mask
'                      qbForceComplete - Force the user to finish the mask.
'              Req$ - These character(s) are required in the string.
'
'
DECLARE FUNCTION GetInput$ (Row&, Col&, PasswordChar$, MaxWidth&, Fore&, Back&, Empty$, Insert&, Default$, Mask$, Flags&, Req$)
DECLARE SUB PrintHelp (text$)
CONST CURRENTFORE = -1, CURRENTBACK = -2
CONST qbSetUp = 2, qbShowMask = 8, qbForceComplete = 256
CLS
COLOR 15, 0
PRINT " GetInput$ Demonstration Program:"
PRINT "        Form Application         "
PRINT
LOCATE 5, 1: PRINT "Press any key to begin";
DO UNTIL I$ <> "": I$ = INKEY$: LOOP
LOCATE 5, 1: PRINT "                      "
'Name$ = GetInput$(1, 1, "", 40, CURRENTFORE, CURRENTBACK, "ù", 0, "", "Price: ~####.##", qbSetUp + qbForceComplete)
LOCATE 4, 1: PRINT "Name:"
PrintHelp "This is a simple, basic GetInput$ structure."
Name$ = GetInput$(4, 20, "", 40, 15, 1, "ù", 0, "", "", 0, "")
COLOR 15, 0
LOCATE 5, 1: PRINT "Street Address:"
PrintHelp "This, too, has a simple structure."
Street$ = GetInput$(5, 20, "", 50, 15, 1, "ù", 0, "", "", 0, "")
COLOR 15, 0
LOCATE 6, 1: PRINT "City:"
PrintHelp "This is a simple text box."
Name$ = GetInput$(6, 20, "", 50, 15, 1, "ù", 0, "", "", 0, "")
COLOR 15, 0
LOCATE 7, 1: PRINT "State:"
PrintHelp "This has a mask. Enter the 2-character abbreviation."
City$ = GetInput$(7, 20, "", 2, 15, 1, "ù", 0, "", "$$", 0, "")
COLOR 15, 0
LOCATE 7, 30: PRINT "Zip Code:"
PrintHelp "This has a number mask. Enter the 5-digit code."
ZipCode$ = GetInput$(7, 41, "", 5, 15, 1, "ù", 0, "", "#####", 0, "")
COLOR 15, 0
LOCATE 8, 1: PRINT "Phone Number:"
PrintHelp "This has custom mask. You have to finish the number."
Phone$ = GetInput$(8, 20, "", 14, 15, 1, "ù", 0, "", "(###) ###-####", qbForceComplete, "")
COLOR 15, 0
LOCATE 9, 1: PRINT "Fax Number:"
PrintHelp "This has custom mask. You don't have to finish the number."
Fax$ = GetInput$(9, 20, "", 14, 15, 1, "ù", 0, "", "(###) ###-####", 0, "")
COLOR 15, 0
LOCATE 10, 1: PRINT "Email:"
PrintHelp "This requires you to type in a '@' and '.'"
Email$ = GetInput$(10, 20, "", 40, 15, 1, "ù", 0, "", "", 0, "@.")
COLOR 15, 0
LOCATE 11, 1: PRINT "Password:"
PrintHelp "This is hidden to the user."
Password$ = GetInput$(11, 20, "*", 20, 15, 1, "ù", 0, "", "", 0, "")
COLOR 15, 0
LOCATE 12, 1: PRINT "Fake Social Security Number:"
PrintHelp "This has a password mask."
SocialSecurity$ = GetInput$(12, 30, "*", 11, 15, 1, "ù", 0, "", "###-##-####", 0, "")
COLOR 15, 0
CLS
PRINT "     GetInput$ Demonstration Program:   "
PRINT "            Form Application            "
PRINT "                                        "
PRINT "                                        "
PRINT " There is a variety of things you can do"
PRINT " with this function.  You can change the"
PRINT " password  character and  the  character"
PRINT " that is printed in the empty space. See"
PRINT " the instruction comments in the program"
PRINT " or the readme  file included with this."
END

DEFLNG A-Z
                        'Define all unsigned variables to be LONG
FUNCTION GetInput$ (Row, Col, PasswordChar$, MaxWidth, Fore, Back, Empty$, Insert, Default$, Mask$, Flags, Req$)
   SELECT CASE Flags
   CASE qbSetUp: SetUp = -1
   CASE qbShowMask: ShowMask = -1
   CASE qbForceComplete: Force = -1
   CASE qbSetUp + qbShowMask: SetUp = -1: ShowMask = -1
   CASE qbSetUp + qbForceComplete: SetUp = -1: Force = -1
   CASE qbShowMask + qbForceComplete: ShowMask = -1: Force = -1
   CASE qbSetUp + qbShowMask + qbForceComplete: SetUp = -1: ShowMask = -1: Force = -1
   END SELECT
   ' Print text for foreground and background saves
   LOCATE Row, Col: PRINT "A"
   GetForeColor = SCREEN(Row, Col, 1) MOD 16       'Save foreground color
   GetBackColor = SCREEN(Row, Col, 1) / 16         'Save background color
   Mask = 0
   IF Mask$ <> "" THEN
      Mask = -1
      MaxWidth = LEN(Mask$)
      FOR CountLen = 1 TO LEN(Mask$)
         a$ = MID$(Mask$, CountLen, 1)
         IF a$ = "#" OR a$ = "$" THEN
            Ms$ = Ms$ + a$
            MaskCount = MaskCount + 1
         END IF
      NEXT
   END IF
   IF Empty$ = "" THEN Empty$ = " "
   text$ = Default$
   CurrentPos = LEN(text$) + 1
   GOSUB PutText                                   'Put text on screen
   IF SetUp THEN EXIT FUNCTION
   DO
      I$ = INKEY$             'Get keyboard character
      SELECT CASE I$
      CASE CHR$(8)            'Backspace
         IF LEN(text$) > 0 THEN
            text$ = MID$(text$, 1, CurrentPos - 2) + MID$(text$, CurrentPos, LEN(text$))
            CurrentPos = CurrentPos - 1
         END IF
      CASE CHR$(27)           'Escape
         IF Force AND LEN(text$) < MaskCount THEN
         ELSE
            IsThere = 0
            FOR CheckReq = 1 TO LEN(Req$)
               a$ = MID$(Req$, CheckReq, 1)
               IF INSTR(1, text$, a$) = 0 THEN
                  IsThere = -1
               END IF
            NEXT
            IF NOT IsThere THEN
               EXIT FUNCTION
            END IF
         END IF
      CASE CHR$(13)           'Enter
         IF Force AND LEN(text$) < MaskCount THEN
         ELSE
            IsThere = 0
            FOR CheckReq = 1 TO LEN(Req$)
               a$ = MID$(Req$, CheckReq, 1)
               IF INSTR(1, text$, a$) = 0 THEN
                  IsThere = -1
               END IF
            NEXT
            IF NOT IsThere THEN
               EXIT DO
            END IF
         END IF
      CASE CHR$(0) + "M"      'Right
         IF Mask = 0 THEN
            IF CurrentPos < LEN(text$) + 1 THEN
               CurrentPos = CurrentPos + 1
            END IF
         END IF
      CASE CHR$(0) + "K"      'Left
         IF Mask = 0 THEN
            IF CurrentPos > 1 THEN
               CurrentPos = CurrentPos - 1
            END IF
         END IF
      CASE ELSE               'Anything else
         IF Mask = 0 THEN
            IF LEN(I$) = 1 AND LEN(text$) < MaxWidth THEN
               IfInsCurrent = CurrentPos
               IF Insert = 1 THEN IfInsCurrent = CurrentPos + 1
               text$ = MID$(text$, 1, CurrentPos - 1) + I$ + MID$(text$, IfInsCurrent, LEN(text$))
               CurrentPos = CurrentPos + 1
            END IF
         ELSE
            IF LEN(I$) = 1 AND LEN(text$) < MaskCount THEN
               IF MID$(Ms$, LEN(text$) + 1, 1) = "#" THEN
                  IF VAL(I$) > 0 OR I$ = "0" THEN
                     text$ = text$ + I$
                     CurrentPos = CurrentPos + 1
                  END IF
               ELSEIF MID$(Ms$, LEN(text$) + 1, 1) = "$" THEN
                  text$ = text$ + I$
                  CurrentPos = CurrentPos + 1
               END IF
            END IF
         END IF
      END SELECT
SkipIt:
      IF I$ <> "" THEN GOSUB PutText
   LOOP
   ' Set the text to be returned
   GetInput$ = text$
   ' Restore previous colors
   COLOR GetForeColor, GetBackColor
   LOCATE , , 0, 10, 11
   EXIT FUNCTION

PutText:
   IF Fore <> CURRENTFORE THEN
      COLOR Fore
   ELSE
      COLOR GetForeColor
   END IF
   IF Back <> CURRENTBACK THEN
      COLOR , Back
   ELSE
      COLOR , GetBackColor
   END IF
   ' Put the character for empty spaces
   LOCATE Row, Col + LEN(text$), 0: PRINT STRING$(MaxWidth - LEN(text$), Empty$);
   ' Print the text on the screen
   IF Mask = 0 THEN
      IF PasswordChar$ = "" THEN
         LOCATE Row, Col, 0: PRINT text$;
         IF Insert = 0 THEN
            LOCATE Row, Col + CurrentPos - 1, 1, 10, 11: PRINT ;
         ELSE
            LOCATE Row, Col + CurrentPos - 1, 1, 1, 11: PRINT ;
         END IF
      ELSE
         LOCATE Row, Col, 0: PRINT STRING$(LEN(text$), PasswordChar$);
         IF Insert = 0 THEN
            LOCATE Row, Col + CurrentPos - 1, 1, 10, 11: PRINT ;
         ELSE
            LOCATE Row, Col + CurrentPos - 1, 1, 1, 11: PRINT ;
         END IF
      END IF
   ELSE
      IF PasswordChar$ = "" THEN
         Txt$ = Mask$
         B = 0
         AlR = 0
         FOR FillText = 1 TO LEN(Mask$)
            a$ = MID$(Mask$, FillText, 1)
            IF a$ = "$" OR a$ = "#" THEN
               B = B + 1
               IF B = LEN(text$) AND AlR = 0 THEN AlR = 1: NTxt = FillText
               MID$(Txt$, FillText, 1) = MID$(text$, B, 1)
            END IF
         NEXT
         IF LEN(text$) = 0 THEN NTxt = 0
         FOR fillChar = 1 TO LEN(Txt$)
            IF MID$(Txt$, fillChar, 1) = "~" THEN
               MID$(Txt$, fillChar, 1) = "$"
            END IF
         NEXT
         IF NOT ShowMask THEN Txt$ = MID$(Txt$, 1, NTxt)
         LOCATE Row, Col, 0: PRINT Txt$;
         IF Insert = 0 THEN
            LOCATE Row, Col + NTxt, 1, 10, 11: PRINT ;
         ELSE
            LOCATE Row, Col + NTxt, 1, 1, 11: PRINT ;
         END IF
      ELSE
         Txt$ = Mask$
         B = 0
         AlR = 0
         FOR FillText = 1 TO LEN(Mask$)
            a$ = MID$(Mask$, FillText, 1)
            IF a$ = "$" OR a$ = "#" THEN
               B = B + 1
               IF B = LEN(text$) AND AlR = 0 THEN AlR = 1: NTxt = FillText
               MID$(Txt$, FillText, 1) = PasswordChar$
            END IF
         NEXT
         IF LEN(text$) = 0 THEN NTxt = 0
         FOR fillChar = 1 TO LEN(Txt$)
            IF MID$(Txt$, fillChar, 1) = "~" THEN
               MID$(Txt$, fillChar, 1) = "$"
            END IF
         NEXT
         IF NOT ShowMask THEN Txt$ = MID$(Txt$, 1, NTxt)
         LOCATE Row, Col, 0: PRINT Txt$;
         IF Insert = 0 THEN
            LOCATE Row, Col + NTxt, 1, 10, 11: PRINT ;
         ELSE
            LOCATE Row, Col + NTxt, 1, 1, 11: PRINT ;
         END IF
      END IF
   END IF

   RETURN
END FUNCTION

DEFSNG A-Z
SUB PrintHelp (text$)
Txt$ = SPACE$(80)
MID$(Txt$, 41 - LEN(text$) / 2) = text$
COLOR 14, 1
LOCATE 25, 1: PRINT Txt$;
END SUB

