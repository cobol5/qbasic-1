DEFINT A-Z
    DECLARE SUB SaveIt (KeyPress)
    DECLARE SUB Mouse (cx, dx, bx)
    DECLARE SUB MousePointer (SW)
    DECLARE SUB ListFile ()
    DECLARE SUB Delay (amt!)
    DECLARE FUNCTION GetInput$ (Rpos, Cpos, length)
    COMMON SHARED Npath$, NewFile, PageNo, NoOfPages, Title$
    COMMON SHARED FFName$, DrvLtr$, x, OneToOpen$, canceled
    COMMON SHARED FileMode, a$
    CLEAR , , 12000
         DIM SHARED AvlDir$(x)
    DIM SHARED AvlFile$(x)
    DIM SHARED FFpath$(25)
         DIM SHARED AvlDrv(x)
         DIM SHARED TmpDoc$(LineNo)
    DIM SHARED Doc$(LineNo)
    DIM SHARED Text$(row)
        DIM SHARED a(9)
        DEF SEG = VARSEG(a(0))
        FOR i = 0 TO 17
                READ r
                POKE VARPTR(a(0)) + i, r
        NEXT i
'---------------------------- Machine Code -----------------------------------
DATA &HB8,&H00,&H00   :
DATA &H55             :
DATA &H8B,&HEC        :
DATA &HCD,&H33        :
DATA &H92             :
DATA &H8B,&H5E,&H06   :
DATA &H89,&H07        :
DATA &H5D             :
DATA &HCA,&H02,&H00   :
'-------------------------------------------------------------------------------
SCREEN 0: COLOR 9, 1: CLS
BG$ = "UWLabs "
BG$ = BG$ + BG$ + BG$ + BG$ + BG$
BG$ = BG$ + BG$ + BG$ + BG$
BG$ = BG$ + BG$ + BG$ + BG$
b$ = STRING$(40, " ")
c1$ = CHR$(201) + STRING$(38, 205) + CHR$(187)
c2$ = CHR$(186) + STRING$(38, " ") + CHR$(186)
c3$ = CHR$(200) + STRING$(38, 205) + CHR$(188)
LOCATE 3, 1: PRINT BG$: PRINT BG$: PRINT BG$
LOCATE 23, 1, 0: PRINT LEFT$(BG$, 160);
LOCATE 25, 2, 0: PRINT LEFT$(BG$, 79);
LOCATE 1, 1, 0: PRINT MID$(BG$, 2)
COLOR 15: LOCATE 25, 65, 0: PRINT "  Loading...    ";
COLOR 8, 0
    FOR x = 10 TO 17
        LOCATE x, 22: PRINT b$
     NEXT x
    COLOR 11, 3: LOCATE 9, 20: PRINT c1$
    FOR x = 10 TO 15
        LOCATE x, 20: PRINT c2$
     NEXT x
    LOCATE 16, 20: PRINT c3$
    COLOR 11, 3
    FOR x = 10 TO 15
        LOCATE x, 22: PRINT STRING$(36, " ")
     NEXT x
    COLOR 15, 3: LOCATE 11, 30: PRINT "QBWrite version 2.7"
COLOR 8: LOCATE 12, 31: PRINT "(P/S2 Mouse ver.)"
COLOR 0: LOCATE 13, 26: PRINT "This product was created at:"
COLOR 1: LOCATE 14, 29: PRINT "Under"
COLOR 9: LOCATE 14, 34: PRINT "WARE"
COLOR 8: LOCATE 14, 39: PRINT "Labs."
COLOR 11: LOCATE 14, 44: PRINT "(94/97)"
COLOR 8, 0: LOCATE 10, 60: PRINT "ab"
LOCATE 11, 60: PRINT " U": LOCATE 12, 60: PRINT "la"
LOCATE 13, 60: PRINT "s ": LOCATE 14, 60: PRINT "WL"
LOCATE 15, 60: PRINT "bs": LOCATE 16, 60: PRINT "UW"
LOCATE 17, 22, 0: PRINT "UWLabs UWLabs UWLabs UWLabs UWLabs UWLab"
COLOR 31, 1
StartTime = VAL(RIGHT$(TIME$, 2)) + 70
COLOR 15
CALL MousePointer(0): CALL MousePointer(1): CALL MousePointer(3)
q! = TIMER + 5
DO WHILE INKEY$ = ""
        CALL Mouse(cx, dx, bx)
        d = d + 1
        IF d > 0 AND d < 101 THEN LOCATE 25, 79, 0: PRINT " ";
        IF d > 100 AND d < 201 THEN LOCATE 25, 79, 0: PRINT "๚";
        IF d > 200 AND d < 301 THEN LOCATE 25, 79, 0: PRINT "๙";
        IF d > 300 AND d < 401 THEN LOCATE 25, 79, 0: PRINT CHR$(15);
        IF d > 400 AND d < 501 THEN LOCATE 25, 79, 0: PRINT "O";
        IF d > 500 AND d < 601 THEN LOCATE 25, 79, 0: PRINT CHR$(15);
        IF d > 600 AND d < 701 THEN LOCATE 25, 79, 0: PRINT "๙";
        IF d > 700 AND d < 801 THEN LOCATE 25, 79, 0: PRINT "๚";
        IF d > 802 THEN d = 0
        IF bx = 1 THEN EXIT DO
        IF TIMER > q! THEN EXIT DO
LOOP
          LOCATE 25, 79, 0: PRINT " ";
          DO WHILE INKEY$ <> ""
                LOOP
    PageNo = 1: NoOfPages = 1: t5$ = STRING$(31, " ")
    REDIM TmpDoc$(32)
    REDIM Doc$(32)
    ON TIMER(15) GOSUB SSaver
ReNewIt:
    REDIM Text$(25)
    row = 3: col = 1: BackUp = 0: InsertMode = 0: IMode$ = "Overstrike": canceled = 0
    FFName$ = "Untitled.QBW": KHome$ = CHR$(0) + CHR$(71): KEnd$ = CHR$(0) + CHR$(79)
     KDel$ = CHR$(0) + CHR$(83): KPgUp$ = CHR$(0) + CHR$(73)
    KPgDn$ = CHR$(0) + CHR$(81): KUp$ = CHR$(0) + CHR$(72)
      KDn$ = CHR$(0) + CHR$(80): KLft$ = CHR$(0) + CHR$(75)
     KRit$ = CHR$(0) + CHR$(77): KIns$ = CHR$(0) + CHR$(82)
      KF1$ = CHR$(0) + CHR$(59): KF2$ = CHR$(0) + CHR$(60)
      KF3$ = CHR$(0) + CHR$(61): KF4$ = CHR$(0) + CHR$(62)
      KF5$ = CHR$(0) + CHR$(63): KF6$ = CHR$(0) + CHR$(64)
      KF7$ = CHR$(0) + CHR$(65): KF8$ = CHR$(0) + CHR$(66)
      KF9$ = CHR$(0) + CHR$(67): KF10$ = CHR$(0) + CHR$(68)
     KF11$ = CHR$(0) + CHR$(133): KF12$ = CHR$(0) + CHR$(134)
  ON ERROR GOTO Trapper
GOTO InitScreen
'------------------------------------------------
TimeUpdate:
     hour = VAL(LEFT$(TIME$, 2))
        IF hour >= 12 THEN p$ = "pm" ELSE p$ = "am"
        IF hour > 12 THEN hour = hour - 12
        IF hour = 0 THEN hour = 12
        hour$ = STR$(hour)
     minute$ = MID$(TIME$, 4, 2)
     Now$ = hour$ + ":" + minute$ + " " + p$
 COLOR 8, 15: LOCATE 1, 15, 0: PRINT Now$
 COLOR 7, 0: RETURN
'       --- Initialize screen ---
InitScreen:
   CLS : SCREEN 0:  KEY OFF
    PALETTE 0, 63        'These statements change the program's
    PALETTE 7, 0         'colors. They're commented below so you
    PALETTE 8, 15        'refer to then after changes.
    PALETTE 1, 49        ' <----------------------<<<
               'Palette#'s:  0/0   7/63  8/8   1/7       (rev B&W)
               'Palette#'s:  0/63  7/0   8/7   1/56      (Gray)
               'Palette#'s:  0/63  7/0   8/27  1/11      (Blue)
               'Palette#'s   0/63  7/0   8/41  1/43      (Blue2)
               'Palette#'s   0/63  7/0   8/43  1/41      (Blue3)
               'Palette#'s:  0/63  7/0   8/23  1/2       (Green)
               'Palette#'s:  0/63  7/0   8/47  1/21      (Purple)
               'Palette#'s:  0/63  7/0   8/44  1/12      (Red)
               'Palette#'s:  0/63  7/0   8/62  1/14      (Yellow)
               'Palette#'s   0/63  7/0   8/62  1/46      (Orange)
               'Palette#'s   0/63  7/0   8/11  1/25      (Lt.Blu)
               'Palette#'s   0/63  7/0   8/28  1/39      (Peach)
               'Palette#'s   0/63  7/0   8/10  1/51      (Lime)
               'Palette#'s   0/63  7/0   8/10  1/50      (GREEN!)
               'Palette#'s   0/63  7/0   8/15  1/49      (Steel)
               'Palette#'s   0/63  7/0   8/45  1/46      (Pink)
               'Palette#'s   0/63  7/0   8/47  1/45      (Pink2)
    PALETTE 15, 56
   COLOR 7, 0: CLS
     COLOR 7, 1
   LOCATE 25, 1, 0: PRINT STRING$(80, " ");
   LOCATE 24, 1, 0: PRINT STRING$(80, " ");
     GOSUB BtmMenu
     COLOR 0, 7
   LOCATE 1, 1: PRINT STRING$(160, " ")
   COLOR 8, 7: LOCATE 1, 3: PRINT DATE$
GOSUB TimeUpdate
   COLOR 8, 7: LOCATE 1, 60: PRINT " Press [Esc] to Exit "
LOCATE 3, 1, 0: COLOR 7, 0
IF FirstTime = 1 THEN GOTO SkipDoc
FirstTime = 1
PRINT "                      "
PRINT "                        ษออออออออออออออออออออออออออออป"
PRINT "                        บ ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐบÞ"
PRINT "                        บ ฐฐ WELCOME TO QB-WRITE! ฐฐฐบÞ"
PRINT "                        บ ฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐฐบÞ"
PRINT "                        ศออออออออออออออออออออออออออออผÞ"
PRINT "                          ฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿"
PRINT "      Please take time to read the `README1.QBW' file before using this"
PRINT "      program.   To load press [F12] then select the correct directory."
PRINT "     "
PRINT "  ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
PRINT "  บ NOTE: BEFORE saving any files with QBW please note that it uses a random บ"
PRINT "  บ file mode with carriage returns placed every 80 characters, this format  บ"
PRINT "  บ allows QBWrite to IMPORT ASCII files, BUT WILL NOT WRITE THEM!!! Please  บ"
PRINT "  บ REMEMBER to save all files with the *.QBW extension so there will be no  บ"
PRINT "  บ mistaking them for ASCII files. (REMEMBER OPEN ANY, SAVE AS  *.QBW).     บ"
PRINT "  ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
PRINT "                 ษอออออออออออออออออออออออออออออออออออออออออออออป"
PRINT "                 บ Press any key to clear this text and begin. บ"
PRINT "                 ศอออออออออออออออออออออออออออออออออออออออออออออผ"
PRINT "                                                                   UnderWARE(97)"
SkipDoc:
DO WHILE INKEY$ <> "": LOOP
DO WHILE INKEY$ = ""
        CALL Mouse(cx, dx, bx)
        IF bx = 1 THEN EXIT DO
LOOP
 FOR x = 3 TO 23
        LOCATE x, 1, 0: PRINT STRING$(80, " ")
 NEXT x
                TIMER ON       ' <======================<<< here's the timer
 LOCATE 3, 1, 1, 1, 31
'-------------------- Key input loop ---------
 GOSUB StatusBar
EnterDo:
 CALL MousePointer(1)
 CALL MousePointer(3)
 COLOR 7, 0
    DO
        CALL Mouse(cx, dx, bx)
        a$ = INKEY$
        IF a$ = CHR$(0) + CHR$(84) THEN a$ = "": GOSUB UWPopUp
        IF InsertMode = 1 THEN LOCATE row, col, 1, 7, 31
        IF InsertMode = 0 THEN LOCATE row, col, 1, 1, 31
        IF a$ = CHR$(27) THEN EXIT DO
        IF a$ <> "" THEN GOSUB CheckKeys: GOSUB StatusBar: LastTime = 0
        IF bx = 1 THEN
           IF cx = 1 AND dx > 60 AND dx < 80 THEN EXIT DO
           IF cx = 2 AND dx > 41 AND dx < 59 THEN
              DO WHILE bx = 1: CALL Mouse(cx, dx, bx): LOOP
              CALL MousePointer(2): a$ = CHR$(0) + CHR$(82)
              GOSUB CheckKeys: GOSUB StatusBar: LastTime = 0
              CALL MousePointer(1): CALL MousePointer(3)
           END IF
           IF cx > 2 AND cx < 24 THEN
              row = cx: col = dx
              LOCATE row, col, 0
           END IF
           IF cx > 23 THEN
                IF dx > 2 AND dx < 7 THEN a$ = KF1$
                IF dx > 8 AND dx < 14 THEN a$ = KF2$
                IF dx > 15 AND dx < 20 THEN a$ = KF3$
                IF dx > 21 AND dx < 26 THEN a$ = KF4$
                IF dx > 27 AND dx < 32 THEN a$ = KF5$
                IF dx > 33 AND dx < 39 THEN a$ = KF6$
                IF dx > 40 AND dx < 46 THEN a$ = KF7$
                IF dx > 47 AND dx < 53 THEN a$ = KF8$
                IF dx > 54 AND dx < 60 THEN a$ = KF9$
                IF dx > 61 AND dx < 67 THEN a$ = KF10$
                IF dx > 68 AND dx < 73 THEN a$ = KF11$
                IF dx > 74 AND dx < 79 THEN a$ = KF12$
                row = 3: col = 0
                GOSUB CheckKeys: GOSUB StatusBar: LastTime = 0
           END IF
        END IF
    LOOP
 GOTO EndItNow
'------------------------------- UnderWARE Labs Pop-Up ---
UWPopUp:
 COLOR 1, 15: LOCATE 6, 25, 0: PRINT "ษอออออออออออออออออออออออออออออป"
 FOR x = 7 TO 16: LOCATE x, 25, 0: PRINT "บ                             บ": NEXT x
 LOCATE x, 25, 0: PRINT "ศอออออออออออออออออออออออออออออผ"
 COLOR 15, 7
 FOR x = 7 TO 17
        c = SCREEN(x, 56)
        LOCATE x, 56: PRINT CHR$(c);
 NEXT x
 FOR x = 26 TO 56
        c = SCREEN(18, x)
        LOCATE 18, x: PRINT CHR$(c);
 NEXT x
 COLOR 0, 15
 LOCATE 7, 32, 0: PRINT " QBWrite ver 2.7 "
 COLOR 1: LOCATE 9, 28, 0: PRINT "To register this version,"
 LOCATE 10, 35: PRINT "send $4 to:"
 COLOR 8: LOCATE 11, 33: PRINT "UnderWARE Labs"
 LOCATE 12, 33: PRINT "c/o L.Schramm"
 LOCATE 13, 33: PRINT "Rte 1 Box 6-A"
 LOCATE 14, 33: PRINT "Hiddenite, NC 28636"
 COLOR 1: LOCATE 15, 27: PRINT "Send comments & suggestions"
 LOCATE 16, 29: PRINT "to: "
 COLOR 0: LOCATE 16, 33, 0: PRINT "STEELCHARM@AOL.COM"
    DO WHILE INKEY$ = "": LOOP
 COLOR 7, 0
 FOR x = 6 TO 18: LOCATE x, 25, 0: PRINT STRING$(32, " "): NEXT x
 FOR x = 6 TO 18: LOCATE x, 1, 0: PRINT Text$(x): NEXT x
RETURN
'------------------------------- Print Menu ---
PrintMenu:
 COLOR 8, 7
 LOCATE 6, 25, 0: PRINT "ษอออออออออออออออออออออออออออออป"
 FOR x = 7 TO 13
        LOCATE x, 25, 0: PRINT "บ                             บ"
 NEXT x
 LOCATE x, 25, 0: PRINT "ศอออออออออออออออออออออออออออออผ"
 COLOR 15, 7
 FOR x = 7 TO 14
        c = SCREEN(x, 56)
        LOCATE x, 56: PRINT CHR$(c);
 NEXT x
 FOR x = 26 TO 56
        c = SCREEN(15, x)
        LOCATE 15, x: PRINT CHR$(c);
 NEXT x
 COLOR 0, 7: LOCATE 7, 35: PRINT "Print Menu"
 COLOR 8: LOCATE 9, 27: PRINT "[F6] = Set Up Printer"
 LOCATE 10, 27: PRINT "[F7] = Clear Printer Set Up"
 LOCATE 11, 27: PRINT "[F8] = PRINT NOW"
 LOCATE 12, 27: PRINT "[F9] = Print Reg. Form"
 LOCATE 13, 27, 0: PRINT "[Esc]= Clear This Menu"
    DO
        CALL Mouse(cx, dx, bx)
        IF bx = 1 AND dx > 26 AND dx < 53 THEN
            IF cx = 9 THEN LastTime = 0: GOTO SetUpPrn
            IF cx = 10 THEN LastTime = 0: GOTO ClearSetUp
            IF cx = 11 THEN LastTime = 0: GOSUB PrintTheDoc
            IF cx = 12 THEN LastTime = 0: GOTO PrnRegForm
            IF cx = 13 THEN LastTime = 0: EXIT DO
        END IF
        a$ = INKEY$
        IF a$ = KF6$ THEN : LastTime = 0: GOTO SetUpPrn
        IF a$ = KF8$ THEN : LastTime = 0: GOSUB PrintTheDoc
        IF a$ = KF9$ THEN : LastTime = 0: GOTO PrnRegForm
        IF a$ = CHR$(27) THEN : LastTime = 0: EXIT DO
        IF a$ = KF7$ THEN : LastTime = 0: GOTO ClearSetUp
    LOOP
 COLOR 7, 0
 FOR x = 3 TO 20: LOCATE x, 25, 0: PRINT STRING$(32, " "): NEXT x
 FOR x = 3 TO 20: LOCATE x, 1, 0: PRINT Text$(x): NEXT x
RETURN
'-------------------------------- Clear Printer --
ClearSetUp:
  COLOR 7, 0
  FOR x = 3 TO 23: LOCATE x, 1, 0
   PRINT STRING$(80, " ")
   LOCATE x, 1, 0: PRINT Text$(x)
  NEXT x
  COLOR 8, 15
  LOCATE 9, 25, 0: PRINT "ษอออออออออออออออออออออออออออออป"
  FOR x = 10 TO 12
   LOCATE x, 25, 0: PRINT "บ                             บ"
  NEXT x
  LOCATE 13, 25, 0: PRINT "ศอออออออออออออออออออออออออออออผ"
 COLOR 15, 7
 FOR x = 10 TO 13
        c = SCREEN(x, 56)
        LOCATE x, 56: PRINT CHR$(c);
 NEXT x
 FOR x = 26 TO 56
        c = SCREEN(14, x)
        LOCATE 14, x: PRINT CHR$(c);
 NEXT x
  COLOR 0, 15
  LOCATE 11, 31, 0: PRINT "Resetting printer..."
        LPRINT CHR$(27); "W"; CHR$(0);
        LPRINT CHR$(27); CHR$(91); CHR$(64); CHR$(4); CHR$(0); CHR$(0); CHR$(0); CHR$(17); CHR$(1);
        LPRINT CHR$(27); "I"; CHR$(0);
  FOR x = 1 TO 3
    LPRINT CHR$(7);
    SLEEP 1
  NEXT x
  COLOR 7, 0
  FOR x = 3 TO 23
    LOCATE x, 1: PRINT STRING$(80, " ")
    LOCATE x, 1, 0: PRINT Text$(x)
  NEXT x
 LastTime = 0
 DO WHILE INKEY$ <> "": LOOP
RETURN
'---------------------------------------- Set Up IBM-Type Printers --
SetUpPrn:
 SLEEP 1: COLOR 8, 15
   LOCATE 6, 25, 0: PRINT "ษอออออออออออออออออออออออออออออป"
   FOR x = 7 TO 13
     LOCATE x, 25: PRINT "บ                             บ"
   NEXT x
   LOCATE 14, 25, 0: PRINT "ศอออออออออออออออออออออออออออออผ"
 COLOR 15, 7
 FOR x = 7 TO 14
        c = SCREEN(x, 56)
        LOCATE x, 56: PRINT CHR$(c);
 NEXT x
 FOR x = 26 TO 56
        c = SCREEN(15, x)
        LOCATE 15, x: PRINT CHR$(c);
 NEXT x
 COLOR 0, 15: LOCATE 7, 32, 0: PRINT "IBM PRINTER SETUP"
 COLOR 8: LOCATE 9, 27: PRINT " 1 = Near Letter Quality   "
 LOCATE 10, 27: PRINT " 2 = Single High, Dbl wide "
 LOCATE 11, 27: PRINT " 3 = Double High, Sgl wide "
 LOCATE 12, 27: PRINT " 4 = Double high, Dbl wide "
 LOCATE 13, 27, 0: PRINT "  [Esc] = Clear This Menu  "
    DO
        a$ = INKEY$
        CALL Mouse(cx, dx, bx)
        IF bx = 1 AND dx > 26 AND dx < 53 THEN
            IF cx = 9 THEN a$ = "1"
            IF cx = 10 THEN a$ = "2"
            IF cx = 11 THEN a$ = "3"
            IF cx = 12 THEN a$ = "4"
            IF cx = 13 THEN a$ = CHR$(27)
        END IF
        IF a$ = "1" THEN
                LPRINT CHR$(27); CHR$(73); CHR$(2);
                LPRINT CHR$(7);
                PrintMode = 0: LastTime = 0
                EXIT DO
                END IF
        IF a$ = "2" THEN
                LPRINT CHR$(27); CHR$(87); CHR$(1);
                LPRINT CHR$(7);
                PrintMode = 0: LastTime = 0
                EXIT DO
                END IF
        IF a$ = "3" THEN
                LPRINT CHR$(27); CHR$(91); CHR$(64); CHR$(4);
                LPRINT CHR$(0); CHR$(0); CHR$(0); CHR$(34); CHR$(1);
                LPRINT CHR$(7);
                PrintMode = 2: LastTime = 0
                EXIT DO
                END IF
        IF a$ = "4" THEN
                LPRINT CHR$(27); CHR$(91); CHR$(64); CHR$(4);
                LPRINT CHR$(0); CHR$(0); CHR$(0); CHR$(34); CHR$(2);
                LPRINT CHR$(7);
                PrintMode = 2: LastTime = 0
                EXIT DO
                END IF
        IF a$ = CHR$(27) THEN EXIT DO
    LOOP
 COLOR 7, 0
  FOR x = 3 TO 23
    LOCATE x, 1: PRINT STRING$(80, " ")
    LOCATE x, 1, 0: PRINT Text$(x)
  NEXT x
RETURN
'--------------------------------- print reg form --
PrnRegForm:
  COLOR 8, 15: LOCATE 8, 25, 0: PRINT "ษอออออออออออออออออออออออออออออป"
  FOR x = 9 TO 11
        LOCATE x, 25: PRINT "บ                             บ"
  NEXT x
  LOCATE 12, 25, 0: PRINT "ศอออออออออออออออออออออออออออออผ"
 COLOR 15, 7
 FOR x = 9 TO 12
        c = SCREEN(x, 56)
        LOCATE x, 56: PRINT CHR$(c);
 NEXT x
 FOR x = 26 TO 56
        c = SCREEN(13, x)
        LOCATE 13, x: PRINT CHR$(c);
 NEXT x
 COLOR 16, 15: LOCATE 10, 32, 0: PRINT "Printing Form..."
 OPEN "regform1.txt" FOR INPUT AS #1
     DO WHILE NOT EOF(1)
     LINE INPUT #1, p$
     LPRINT p$
     LOOP
 CLOSE : SLEEP 2: COLOR 7, 0
 FOR x = 3 TO 23
        LOCATE x, 1: PRINT STRING$(80, " ")
        LOCATE x, 1, 0: PRINT Text$(x)
 NEXT x
  LastTime = 0
RETURN
'---------------------------------- Print Document --
PrintTheDoc:
 COLOR 8, 15: LOCATE 7, 20, 0: PRINT CHR$(201); STRING$(38, 205); CHR$(187)
        FOR x = 8 TO 10
          LOCATE x, 20: PRINT CHR$(186); STRING$(38, " "); CHR$(186)
        NEXT x
       LOCATE 11, 20: PRINT CHR$(200); STRING$(38, 205); CHR$(188)
 COLOR 15, 7
 FOR x = 8 TO 11
        c = SCREEN(x, 60)
        LOCATE x, 60: PRINT CHR$(c);
 NEXT x
 FOR x = 21 TO 60
        c = SCREEN(12, x)
        LOCATE 12, x: PRINT CHR$(c);
 NEXT x
 COLOR 0, 15: LOCATE 9, 23, 0: PRINT "Printing document, please wait..."
    LastTime = 0: SLEEP 1: LPRINT
        FOR x = 1 TO UBOUND(Doc$, 1)
                LPRINT Doc$(x)
                IF PrintMode = 2 THEN
                 SELECT CASE x
                        CASE 31, 62, 93, 124, 155, 186, 217, 248
                        LPRINT : LPRINT
                        CASE 279, 310, 341, 372, 403, 434, 465, 496, 527
                        LPRINT : LPRINT
                        CASE 558, 589, 620, 651, 682, 713, 744, 775
                        LPRINT : LPRINT
                   END SELECT
                 END IF
                 IF PrintMode <> 2 THEN
                   SELECT CASE x
                        CASE 62, 124, 186, 248
                        LPRINT : LPRINT : LPRINT : LPRINT
                        CASE 310, 372, 434, 496
                        LPRINT : LPRINT : LPRINT : LPRINT
                        CASE 558, 620, 682, 744
                        LPRINT : LPRINT : LPRINT : LPRINT
                   END SELECT
                 END IF
        NEXT x
        COLOR 7, 0
        FOR x = 3 TO 23
          LOCATE x, 20: PRINT STRING$(41, " ")
          LOCATE x, 1, 0: PRINT Text$(x)
        NEXT x
  LastTime = 0
RETURN
'---------------------------------------------------
'               ----- Select keystroke ----
CheckKeys:
  SELECT CASE a$
        CASE KF1$: GOTO HelpBar
        CASE KF2$: GOSUB PrintMenu: RETURN 'Print Menu
        CASE KF6$: GOSUB SetUpPrn: RETURN
        CASE KF7$: GOSUB ClearSetUp: RETURN
        CASE KF8$: GOSUB PrintTheDoc: RETURN
        CASE KF9$: GOSUB PrnRegForm: RETURN
        CASE KF3$
                CALL SaveIt(3)
                FOR x = 3 TO 23
                 LOCATE x, 1, 0: PRINT Text$(x);
                NEXT x
                  IF NewFile = 1 THEN NewFile = 0: GOTO ReNewIt
                GOSUB BtmMenu: RETURN
        CASE KF5$
                CALL SaveIt(5)
                FOR x = 3 TO 23
                 LOCATE x, 1, 0: PRINT Text$(x);
                NEXT x
                IF NewFile = 1 THEN
                        NewFile = 0: PageNo = 1
                        FLNo = 1: FileMode = 0: PrintMode = 0
                        NoOfPages = 1: REDIM Doc$(32): REDIM Text$(32)
                        GOTO ReNewIt
                END IF
                GOSUB BtmMenu: RETURN
        CASE KF10$
                CALL SaveIt(10)
                FOR x = 3 TO 23
                 LOCATE x, 1, 0: PRINT Text$(x);
                NEXT x
                GOSUB BtmMenu: RETURN
        CASE KF11$
                CALL SaveIt(11)
                FOR x = 3 TO 23
                 LOCATE x, 1, 0: PRINT Text$(x);
                NEXT x
                GOSUB BtmMenu: RETURN
        CASE KF12$
                CALL SaveIt(12)
                FOR x = 3 TO 23
                 LOCATE x, 1, 0: PRINT Text$(x);
                NEXT x
                GOSUB BtmMenu: RETURN
        CASE KF4$: GOTO MoreHelp
        CASE KPgUp$
                IF PageNo > 1 THEN
                   LOCATE 3, 1, 0
                   PageNo = PageNo - 1
                   FOR x = 3 TO 23: PRINT STRING$(80, " "); : NEXT x
                   FileLine = ((PageNo - 1) * 21) + 1
                   FOR x = 3 TO 23
                        Text$(x) = Doc$(FileLine)
                        FileLine = FileLine + 1
                        LOCATE x, 1, 0: PRINT Text$(x);
                   NEXT x
                ELSE FOR T = 1800 TO 1000 STEP -600: SOUND T, .5: NEXT T
                END IF
                RETURN
        CASE KPgDn$
                IF PageNo < NoOfPages THEN
                   LOCATE 3, 1, 0
                   FOR x = 3 TO 23: PRINT STRING$(80, " "); : NEXT x
                   FileLine = (PageNo * 21) + 1
                   FOR x = 3 TO 23
                        Text$(x) = Doc$(FileLine)
                        FileLine = FileLine + 1
                        LOCATE x, 1, 0: PRINT Text$(x);
                   NEXT x
                   PageNo = PageNo + 1
                ELSE FOR T = 1800 TO 1000 STEP -600: SOUND T, .5: NEXT T
                END IF
                RETURN
        CASE KIns$
                IF InsertMode = 0 THEN InsertMode = 1: IMode$ = "Insert    ": RETURN
                IF InsertMode = 1 THEN InsertMode = 0: IMode$ = "Overstrike": RETURN
        CASE KDel$
                IF LEN(Text$(row)) < 1 OR col > LEN(Text$(row)) THEN
                   FOR T = 1800 TO 1000 STEP -600: SOUND T, .5: NEXT T: RETURN
                END IF
                IF col <= LEN(Text$(row)) THEN
                  b$ = LEFT$(Text$(row), col - 1)
                  c$ = RIGHT$(Text$(row), LEN(Text$(row)) - col)
                  Text$(row) = b$ + c$
                  LOCATE row, 1, 0: PRINT Text$(row); STRING$(80 - LEN(Text$(row)), " ")
                  LOCATE row, col, 0: RETURN
                END IF
        CASE KLft$
                IF col > 1 THEN
                  col = col - 1
                  LOCATE row, col, 0
                  RETURN
                END IF
        CASE KRit$
                IF col < 80 THEN
                  col = col + 1
                  LOCATE row, col, 0
                  RETURN
                END IF
        CASE KUp$
                IF row > 3 THEN
                  row = row - 1
                  LOCATE row, col, 0
                  RETURN
                END IF
        CASE KDn$
                IF row < 23 THEN
                  row = row + 1
                  LOCATE row, col, 0
                  RETURN
                END IF
        CASE KEnd$
                col = LEN(Text$(row)) + 1
                IF col = 0 THEN col = 1
                IF col > 80 THEN col = 80
                LOCATE row, col, 0
                RETURN
        CASE CHR$(13)
                IF row > 22 AND PageNo < NoOfPages THEN BEEP: RETURN
                IF row > 22 AND PageNo = NoOfPages THEN
                    REDIM TmpDoc$(FLNo + 22)
                    IF PageNo > 1 THEN
                       FOR x = 1 TO FLNo
                         TmpDoc$(x) = Doc$(x)
                       NEXT x
                    ELSE
                       FOR x = 3 TO 23
                         TmpDoc$(x - 2) = Text$(x)
                       NEXT x
                    END IF
                    count = 3
                    REDIM Doc$(FLNo + 22)
                    FOR x = 1 TO UBOUND(TmpDoc$, 1)
                        Doc$(x) = TmpDoc$(x)
                    NEXT x
                  LOCATE 3, 1, 0
                   FOR x = 3 TO 23: PRINT STRING$(80, " "): NEXT x
                   PageNo = PageNo + 1
                   NoOfPages = NoOfPages + 1
                   row = 3: col = 1
                   LOCATE row, col, 0
                     REDIM Text$(25)
                   RETURN
                END IF
                row = row + 1
                col = 1
                LOCATE row, col, 0
        CASE CHR$(9)                                    'tab
                IF col > 74 THEN FOR T = 1800 TO 1000 STEP -600: SOUND T, .5: NEXT T: RETURN
                IF col > LEN(Text$(row)) THEN
                  col = (INT(col / 5) * 5) + 5
                  Text$(row) = Text$(row) + "     "
                  Text$(row) = LEFT$(Text$(row), col - 1)
                  LOCATE row, 1, 0: PRINT Text$(row)
                  LOCATE row, col, 0
                   RETURN
                END IF
                b$ = LEFT$(Text$(row), col - 1)
                c$ = RIGHT$(Text$(row), LEN(Text$(row)) - (col - 1))
                c = 5 - (col MOD 5)
                col = (INT(col / 5) * 5) + 5
                c$ = STRING$(c, " ") + c$
                Text$(row) = b$ + c$
                IF LEN(Text$(row)) > 80 THEN Text$(row) = LEFT$(Text$(row), 80)
                LOCATE row, 1, 0: PRINT Text$(row)
                LOCATE row, col, 0
                RETURN
        CASE CHR$(8)                                    'backspace
                IF col < LEN(Text$(row)) THEN GOTO BKSPC
                col = col - 1
                IF col < 1 THEN
                        row = row - 1
                        col = LEN(Text$(row)) + 1
                        IF col = 0 THEN col = 1
                        IF col >= 80 THEN
                                Text$(row) = LEFT$(Text$(row), 79)
                                col = 80
                        END IF
                        Text$(row + 1) = ""
                        LOCATE row + 1, 1, 0: PRINT " "
                END IF
                IF row = 2 THEN row = 3: col = 1: FOR T = 1800 TO 1000 STEP -600: SOUND T, .5: NEXT T: RETURN
                IF col >= 1 THEN
                        Text$(row) = LEFT$(Text$(row), col - 1)
                END IF
                LOCATE row, 1, 0: PRINT Text$(row); " "
                LOCATE row, col, 0
        CASE KHome$
                StartPos = 0
                FOR q = 1 TO LEN(Text$(row))
                    FirstLtr = INSTR(q, Text$(row), " ")
                    IF FirstLtr <> q THEN EXIT FOR
                    StartPos = FirstLtr
                NEXT
                col = StartPos + 1
                LOCATE row, col, 0
        CASE ELSE
                IF LEN(a$) > 1 THEN
                        FOR T = 1800 TO 1000 STEP -600
                         SOUND T, .5
                        NEXT T
                        RETURN
                END IF
                IF col > LEN(Text$(row)) THEN
                  Text$(row) = Text$(row) + STRING$(col - LEN(Text$(row)) - 1, " ")
                END IF
                IF col < LEN(Text$(row)) THEN GOTO InsOvr
                Text$(row) = Text$(row) + a$
                LOCATE row, 1, 0: PRINT Text$(row)
                col = col + 1
                IF col > 80 AND row < 24 THEN col = 1: row = row + 1
                IF col > 79 AND row > 23 THEN
                        col = 23: FOR T = 1800 TO 1000 STEP -600: SOUND T, .5: NEXT T
                        Text$(row) = LEFT$(Text$(row), 79)
                        RETURN
                  END IF
                LOCATE row, col, 0
  END SELECT
  LastTime = 0
RETURN
'--------------------------------- Insert or Overwrite ---
InsOvr:
  IF InsertMode = 0 THEN
        b$ = LEFT$(Text$(row), col - 1)
        c$ = RIGHT$(Text$(row), LEN(Text$(row)) - (col - 0))
        Text$(row) = b$ + a$ + c$
         LOCATE row, 1, 0: PRINT Text$(row)
         col = col + 1: LOCATE row, col, 0
        RETURN
  END IF
  IF InsertMode = 1 THEN
        b$ = LEFT$(Text$(row), col - 1)
        c$ = RIGHT$(Text$(row), LEN(Text$(row)) - (col - 1))
        Text$(row) = b$ + a$ + c$
         IF LEN(Text$(row)) > 80 THEN Text$(row) = LEFT$(Text$(row), 80)
        LOCATE row, 1, 0: PRINT Text$(row)
        col = col + 1: LOCATE row, col, 0
  END IF
RETURN
'---------------------------------- Backspace from inside line ---
BKSPC:
        IF col = 1 THEN
            FOR T = 1800 TO 1000 STEP -600
               SOUND T, .5
            NEXT T
            RETURN
        END IF
        b$ = LEFT$(Text$(row), col - 2)
        c$ = RIGHT$(Text$(row), LEN(Text$(row)) - (col - 1))
        Text$(row) = b$ + c$
        LOCATE row, 1, 0: PRINT Text$(row) + " "
        col = col - 1
        LOCATE row, col, 0
GOTO EnterDo
'----------------------------------------- Status bar Update ------
StatusBar:
        LastTime = 0
IF Npath$ <> "" THEN FFName$ = Npath$: COLOR 0, 15: LOCATE 1, 25, 0: PRINT "                                          "
COLOR 0, 15: x = INT(40 - ((LEN(FFName$) + 6) / 2))
IF LEN(FFName$) > 35 THEN FFName$ = LEFT$(FFName$, 3) + "~~~" + RIGHT$(FFName$, 29)
LOCATE 1, x, 0: PRINT "File: "; FFName$
FLNo = ((PageNo - 1) * 21) + (row - 2)
COLOR 8: LOCATE 2, 3, 0: PRINT "Page:"; PageNo; "/"; NoOfPages
LOCATE 2, 20, 0: PRINT "Line:"; FLNo: LOCATE 2, 32, 0: PRINT "Col:"; col
LOCATE 2, 42, 0: PRINT "Cursor: "; IMode$
LOCATE 2, 65, 0: PRINT "Line Len:"; LEN(Text$(row))
COLOR 7, 0: Doc$(FLNo) = Text$(row): LOCATE row, col, 0
RETURN
'---------------------------------------------
HelpBar:
       COLOR 8, 15: LOCATE 1, 1, 0: PRINT STRING$(160, " ")
 LOCATE 1, 8, 0: PRINT "= This Menu     [Ins]  = Insert text in line  [Del] = Delete 1 character"
 LOCATE 2, 8, 0: PRINT "= Print Options [End]  = Cursor to text end"
 LOCATE 3, 8, 0: PRINT "= File Options  [Home] = Cursor to line start     [F4] = List More...    "
 COLOR 0: LOCATE 1, 3, 0: PRINT "[F1]": LOCATE 1, 24: PRINT "[Ins]"
 LOCATE 2, 3, 0: PRINT "[F2]": LOCATE 2, 24: PRINT "[End]"
 LOCATE 3, 1, 0: PRINT "  [F3] ": LOCATE 3, 24: PRINT "[Home]"
 LOCATE 1, 53: PRINT " [Del]": LOCATE 3, 56, 0: PRINT "  [F4]"
    DO
        CALL Mouse(cx, dx, bx)
        a$ = INKEY$
        IF a$ <> "" THEN IF a$ = CHR$(0) + CHR$(62) THEN GOTO MoreHelp
        IF a$ = " " THEN EXIT DO
        IF a$ = CHR$(27) THEN EXIT DO
        IF a$ = CHR$(13) THEN EXIT DO
        IF bx = 1 THEN
           IF cx = 3 AND dx > 57 AND dx < 77 THEN GOTO MoreHelp
           IF cx < 4 THEN EXIT DO
        END IF
    LOOP
    LastTime = 0
  GOTO EndHelp
MoreHelp:
  COLOR 0, 15: LOCATE 1, 1, 0: PRINT STRING$(160, " ")
  COLOR 0: LOCATE 1, 3, 0: PRINT "Other supported keys:"
  COLOR 8: LOCATE 2, 3: PRINT "[Backspace] = Backup 1 and delete  [Tab] = Tabs text upto 5 spaces"
  LOCATE 3, 3: PRINT "[Enter] = Carriage return          "
  COLOR 0: LOCATE 2, 3: PRINT "[Backspace]": LOCATE 2, 38: PRINT "[Tab]"
  LOCATE 3, 1: PRINT "  [Enter]": LOCATE 3, 38, 0: PRINT "[Caps], [Shift], ["; CHR$(24);
  PRINT "], ["; CHR$(25); "], ["; CHR$(26); "], ["; CHR$(27); "]        "
    DO
        CALL Mouse(cx, dx, bx)
        IF bx = 1 AND cx > 3 AND cx < 24 THEN EXIT DO
        a$ = INKEY$
        IF a$ = " " THEN EXIT DO
        IF a$ = CHR$(27) THEN EXIT DO
        IF a$ = CHR$(13) THEN EXIT DO
    LOOP
EndHelp:
   COLOR 8, 15: LOCATE 1, 1, 0: PRINT STRING$(160, " ")
   LOCATE 1, 2, 0: PRINT DATE$: GOSUB TimeUpdate
   COLOR 8, 15: LOCATE 1, 60: PRINT " Press [Esc] to Exit "
   GOSUB StatusBar: LOCATE 3, 1, 0: PRINT STRING$(80, " ")
   COLOR 7, 0: LOCATE 3, 1, 0: PRINT Doc$(((PageNo - 1) * 21) + 1)
   LastTime = 0: GOTO EnterDo
'--------------------------------- --- End Program w/ optional return ---
EndItNow:
  COLOR 8, 15: LOCATE 8, 23, 0: PRINT "ษออออออออออออออออออออออออออออออออออป"
   FOR x = 9 TO 11
        LOCATE x, 23: PRINT "บ                                  บ"
   NEXT x
  LOCATE 12, 23, 0: PRINT "ศออออออออออออออออออออออออออออออออออผ"
 COLOR 15, 7
 FOR x = 9 TO 12
        c = SCREEN(x, 59)
        LOCATE x, 59: PRINT CHR$(c);
 NEXT x
 FOR x = 24 TO 59
        c = SCREEN(13, x)
        LOCATE 13, x: PRINT CHR$(c);
 NEXT x
    COLOR 0, 15: LOCATE 9, 26, 0: PRINT "ALL UNSAVED WORK WILL BE LOST!"
    COLOR 8: LOCATE 11, 28, 0: PRINT "Exit Now?  [ Yes ]  [ No  ]"
    COLOR 0: LOCATE 11, 41, 0: PRINT "Y": LOCATE 11, 50, 0: PRINT "N"
    DO
        CALL Mouse(cx, dx, bx)
        a$ = UCASE$(INKEY$)
        IF a$ = "Y" THEN GOTO QuitToDOS
        IF a$ = "N" THEN EXIT DO
        IF a$ = CHR$(27) THEN EXIT DO
        IF bx = 1 AND cx = 11 THEN
           IF dx > 38 AND dx < 46 THEN GOTO QuitToDOS
           IF dx > 47 AND dx < 56 THEN EXIT DO
        END IF
    LOOP
    CALL MousePointer(2): COLOR 7, 0
    FOR x = 3 TO 23
        LOCATE x, 1, 0: PRINT STRING$(80, " ");
        LOCATE x, 1, 0: PRINT Text$(x);
    NEXT x
  GOSUB StatusBar: LastTime = 0: GOTO EnterDo
'------------------------ program ended --------------------------------------
QuitToDOS:
   CALL MousePointer(2) 'Turn mouse off
   PALETTE 0, 0         'Reset palette to default before return.
   PALETTE 7, 7
   PALETTE 8, 8
   PALETTE 15, 15
   PALETTE 1, 1
   WIDTH 80, 25: COLOR 7, 0
   CLS : TIMER OFF
        PRINT "  ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
        PRINT "  บ  Thank you for using UnderWARE Labs!   Please don't forget to register.  บ"
        PRINT "  ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
      SLEEP 3
   LOCATE 4, 1, 1
DEF SEG
 SYSTEM
'------------------------------------------- Error Trapping Routine -----------------
Trapper:
  SELECT CASE ERR
        CASE 55: CLOSE : RESUME EnterDo
        CASE 14
                a1$ = " Out of String space file  "
                a2$ = "    too large to open.     "
                a3$ = "      Press any key...     "
                GOSUB PostIt
                 GOSUB ClikToEsc
                GOSUB ClearBox
                   Npath$ = ""
                RESUME EnterDo
        CASE 25
                a1$ = "Please turn printer on, and"
                a2$ = "         try again.        "
                a3$ = "       Press any key...    "
                GOSUB PostIt
                 GOSUB ClikToEsc
                GOSUB ClearBox
                RESUME PrintMenu
        CASE 27
                a1$ = " There's no paper in the   "
                a2$ = " printer please try again. "
                a3$ = "     Press any key...      "
                GOSUB PostIt
                 GOSUB ClikToEsc
                GOSUB ClearBox
                RESUME PrintMenu
        CASE 52, 64
                a1$ = " Bad file name, please try "
                a2$ = "           again.          "
                a3$ = "       Press any key...    "
                GOSUB PostIt
                 GOSUB ClikToEsc
                GOSUB ClearBox
                   Npath$ = ""
                RESUME EnterDo
        CASE 53, 75, 76
                a1$ = " Cannot find file,  please "
                a2$ = " check path and directory. "
                a3$ = " try again.  [press a key] "
                GOSUB PostIt
                 GOSUB ClikToEsc
                GOSUB ClearBox
                RESUME NEXT
        CASE 71
                a1$ = "Disk Not ready, Insert disk"
                a2$ = "and try again. Press any key"
                a3$ = "to continue,[Esc] to cancel."
                GOSUB PostIt
                 DO
                  CALL Mouse(cx, dx, bx)
                  c$ = INKEY$
                  IF bx = 1 AND cx = 11 AND dx > 38 AND dx < 44 THEN
                        c$ = CHR$(27)
                        ELSE c$ = " "
                  END IF
                  IF c$ = CHR$(27) THEN
                        GOSUB ClearBox
                        RESUME EnterDo
                  END IF
                LOOP UNTIL INKEY$ <> ""
                  IF c$ = CHR$(27) THEN GOSUB ClearBox: RESUME EnterDo
                GOSUB ClearBox
                RESUME
        CASE 5
                IF col < 1 THEN col = 1: RESUME
                IF col > 80 THEN col = 1: RESUME
                a1$ = "This computer won't support"
                a2$ = "one of the functions,Ending"
                a3$ = " Program.   [Press a key]. "
                GOSUB ElsePost
                GOSUB ClikToEsc
                GOTO QuitErr
        CASE ELSE
                a1$ = " Something's wrong, but I  "
                a2$ = " don't know what.  Ending  "
                a3$ = " Program.   [Press a key]. "
                GOSUB ElsePost
                 GOSUB ClikToEsc
                GOTO QuitToDOS
  END SELECT
ClikToEsc:
        LastTime = 0: DO WHILE INKEY$ = "": LOOP: LastTime = 0
RETURN
PostIt:
  COLOR 8, 15: LOCATE 7, 25, 0: PRINT "ษอออออออออออออออออออออออออออออป"
   FOR x = 8 TO 12
        LOCATE x, 25: PRINT "บ                             บ"
   NEXT x
  LOCATE 13, 25, 0: PRINT "ศอออออออออออออออออออออออออออออผ"
 COLOR 15, 7
 FOR x = 8 TO 13
        c = SCREEN(x, 56)
        LOCATE x, 56: PRINT CHR$(c);
 NEXT x
 FOR x = 26 TO 56
        c = SCREEN(14, x)
        LOCATE 14, x: PRINT CHR$(c);
 NEXT x
  COLOR 16, 15: LOCATE 8, 36: PRINT " ERROR ! "
  COLOR 8: LOCATE 9, 27: PRINT a1$
  LOCATE 10, 27: PRINT a2$: LOCATE 11, 27: PRINT a3$
  LOCATE 12, 33, 0: PRINT "Error code:"; ERR
RETURN
ElsePost:
  COLOR 7, 0: SCREEN 0: CLS : WIDTH 80, 25
  LOCATE 9, 25, 0: PRINT "ษอออออออออออออออออออออออออออออป"
    FOR x = 10 TO 14
        LOCATE x, 25: PRINT "บ                             บ"
    NEXT x
  LOCATE 15, 25: PRINT "ศอออออออออออออออออออออออออออออผ"
  COLOR 31, 0: LOCATE 10, 37, 0: PRINT "ERROR !"
  COLOR 7: LOCATE 11, 27: PRINT a1$: LOCATE 12, 27: PRINT a2$
  LOCATE 13, 27: PRINT a3$: LOCATE 14, 33, 0: PRINT "Error code:"; ERR
RETURN
ClearBox:
  COLOR 7, 0
    FOR x = 3 TO 23
      LOCATE x, 1: PRINT STRING$(80, " ");
      LOCATE x, 1, 0: PRINT Text$(x);
    NEXT x
RETURN
'------------------error ending-------------
QuitErr:
   WIDTH 80, 25: COLOR 7, 0: CLS : TIMER OFF
PRINT "ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
PRINT "บ  Thank you for Trying UnderWARE Labs!   Please EMail for error correction.   บ"
PRINT "ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
   LOCATE 4, 1, 1
SYSTEM
'----------------------------- Screen Saver ---------------------------------------
SSaver:
   LOCATE row, col, 0: GOSUB TimeUpdate: LastTime = LastTime + 15
   TimeToWait = 300       '  <----------<<<<  No. of seconds for Screen saver.
   IF LastTime < TimeToWait THEN LOCATE row, col, 1: RETURN
   TIMER OFF: COLOR 0, 7: CLS : a = 0: b = 7: q = 1: w = 1
   DO WHILE INKEY$ = ""
        x = x + q
          IF x > 70 THEN q = -1
          IF x < 2 THEN q = 1
        y = y + w
          IF y > 20 THEN w = -1
          IF y < 2 THEN w = 1
        COLOR a, b: LOCATE y, x, 0:     PRINT "ษออออออออป"
          LOCATE y + 1, x, 0: PRINT "บ        บ"
          LOCATE y + 2, x, 0: PRINT "ศออ1996ออผ"
          COLOR 3: LOCATE y + 1, x + 2, 0: PRINT "UWLABS"
        SLEEP 1: COLOR b, b: LOCATE y, x, 0:    PRINT "          "
          LOCATE y + 1, x, 0: PRINT "          "
          LOCATE y + 2, x, 0: PRINT "          "
        cnt = cnt + 1: dnt = dnt + 1
           IF cnt > 300 THEN cnt = 1: x = x + .1: y = y + .1
           IF dnt > 30 THEN dnt = 1: SWAP a, b: COLOR a, b: CLS
   LOOP
 TIMER ON
   GOSUB BtmMenu: COLOR 0, 7
   LOCATE 1, 1: PRINT STRING$(160, " ")
 COLOR 7, 8
     FOR x = 3 TO 23
        LOCATE x, 1, 0: PRINT STRING$(80, " ");
        LOCATE x, 1, 0: PRINT Text$(x);
     NEXT x
   COLOR 8, 15: LOCATE 1, 3: PRINT DATE$: GOSUB TimeUpdate
   COLOR 8, 15: LOCATE 1, 60: PRINT " Press [Esc] to Exit "
 LOCATE 3, 1, 0: GOSUB StatusBar
RETURN
'----------
NoDrive:
        IF ERR = 76 THEN AvlDrv(x) = 0: RESUME NEXT
        IF ERR = 71 THEN : RESUME NEXT
        'END
ErrCheck:
        LOCATE 23, 2, 0: PRINT "Error:"; ERR
        RESUME NEXT
END
'----------
BtmMenu:
   COLOR 1, 7: LOCATE 24, 1, 0: PRINT STRING$(80, " ");
   COLOR 1, 7: LOCATE 25, 1, 0: PRINT STRING$(80, " ");
   LOCATE 25, 2, 0: PRINT CHR$(222); : LOCATE 25, 8, 0: PRINT CHR$(222); : LOCATE 25, 15, 0: PRINT CHR$(222);
   LOCATE 25, 21, 0: PRINT CHR$(222); : LOCATE 25, 27, 0: PRINT CHR$(222); : LOCATE 25, 33, 0: PRINT CHR$(222);
   LOCATE 25, 40, 0: PRINT CHR$(222); : LOCATE 25, 47, 0: PRINT CHR$(222); : LOCATE 25, 54, 0: PRINT CHR$(222);
   LOCATE 25, 61, 0: PRINT CHR$(222); : LOCATE 25, 68, 0: PRINT CHR$(222); : LOCATE 25, 74, 0: PRINT CHR$(222);
   LOCATE 24, 2, 0: PRINT CHR$(222); : LOCATE 24, 8, 0: PRINT CHR$(222); : LOCATE 24, 15, 0: PRINT CHR$(222);
   LOCATE 24, 21, 0: PRINT CHR$(222); : LOCATE 24, 27, 0: PRINT CHR$(222); : LOCATE 24, 33, 0: PRINT CHR$(222);
   LOCATE 24, 40, 0: PRINT CHR$(222); : LOCATE 24, 47, 0: PRINT CHR$(222); : LOCATE 24, 54, 0: PRINT CHR$(222);
   LOCATE 24, 61, 0: PRINT CHR$(222); : LOCATE 24, 68, 0: PRINT CHR$(222); : LOCATE 24, 74, 0: PRINT CHR$(222);
   LOCATE 25, 7, 0: PRINT CHR$(221); : LOCATE 25, 14, 0: PRINT CHR$(221); : LOCATE 25, 20, 0: PRINT CHR$(221);
   LOCATE 25, 26, 0: PRINT CHR$(221); : LOCATE 25, 32, 0: PRINT CHR$(221); : LOCATE 25, 39, 0: PRINT CHR$(221);
   LOCATE 25, 46, 0: PRINT CHR$(221); : LOCATE 25, 53, 0: PRINT CHR$(221); : LOCATE 25, 60, 0: PRINT CHR$(221);
   LOCATE 25, 67, 0: PRINT CHR$(221); : LOCATE 25, 73, 0: PRINT CHR$(221); : LOCATE 25, 79, 0: PRINT CHR$(221);
   LOCATE 24, 7, 0: PRINT CHR$(221); : LOCATE 24, 14, 0: PRINT CHR$(221); : LOCATE 24, 20, 0: PRINT CHR$(221);
   LOCATE 24, 26, 0: PRINT CHR$(221); : LOCATE 24, 32, 0: PRINT CHR$(221); : LOCATE 24, 39, 0: PRINT CHR$(221);
   LOCATE 24, 46, 0: PRINT CHR$(221); : LOCATE 24, 53, 0: PRINT CHR$(221); : LOCATE 24, 60, 0: PRINT CHR$(221);
   LOCATE 24, 67, 0: PRINT CHR$(221); : LOCATE 24, 73, 0: PRINT CHR$(221); : LOCATE 24, 79, 0: PRINT CHR$(221);
     COLOR 8, 1
   LOCATE 25, 3, 0: PRINT "Bar "; : LOCATE 25, 9, 0: PRINT "Menu "; : LOCATE 25, 16, 0: PRINT "Menu";
   LOCATE 25, 22, 0: PRINT "Help"; : LOCATE 25, 28, 0: PRINT "File"; : LOCATE 25, 34, 0: PRINT "Prntr";
   LOCATE 25, 41, 0: PRINT "Prntr"; : LOCATE 25, 48, 0: PRINT " Now "; : LOCATE 25, 55, 0: PRINT " Reg ";
   LOCATE 25, 62, 0: PRINT "Save "; : LOCATE 25, 69, 0: PRINT " As "; : LOCATE 25, 75, 0: PRINT "File";
   LOCATE 24, 3, 0: PRINT "Help"; : LOCATE 24, 9, 0: PRINT "Print"; : LOCATE 24, 16, 0: PRINT "File";
   LOCATE 24, 22, 0: PRINT "More"; : LOCATE 24, 28, 0: PRINT "New "; : LOCATE 24, 34, 0: PRINT "SetUp";
   LOCATE 24, 41, 0: PRINT "Clear"; : LOCATE 24, 48, 0: PRINT "Print"; : LOCATE 24, 55, 0: PRINT "Print";
   LOCATE 24, 62, 0: PRINT "Quick"; : LOCATE 24, 69, 0: PRINT "Save"; : LOCATE 24, 75, 0: PRINT "Open";
RETURN

SUB Delay (amt!)

        T! = amt! + TIMER
        DO WHILE TIMER < T!
        LOOP
END SUB

FUNCTION GetInput$ (Rpos, Cpos, length)
'
  col = Cpos                         'intitial cursor location
  row = Rpos
  GetIt$ = UCASE$(FFName$)
'
LOCATE row, col: PRINT GetIt$
LOCATE row, col, 1, 0, 31                       'Makes a LARGE cursor

InsertMode = 0                                  'for overwrite mode.
 
     DO
        T$ = INKEY$
        IF T$ = CHR$(13) THEN EXIT DO           'Enter key.
         IF T$ = CHR$(27) THEN                  'Escape key.
              GetIt$ = "": EXIT DO               'Removes Text for a blank
         END IF                                 ' field.
        IF T$ <> "" THEN GOSUB CheckAKey        'Just what it says.
     LOOP

GOTO NearTheBottom                              'To skip to SUB END.
'- - - - - - - - - - - - - - -  - - - - - -
CheckAKey:
   SELECT CASE T$
        CASE CHR$(8)                                    'Backspace...
                IF col >= Cpos + LEN(GetIt$) THEN        'from text end.
                  IF LEN(GetIt$) < 2 THEN
                     GetIt$ = "": col = Cpos
                     GOTO SelEnd
                  END IF
                  IF col = Cpos THEN RETURN
                  GetIt$ = LEFT$(GetIt$, (LEN(GetIt$) - 1))
                  col = col - 1
                END IF
                IF col < Cpos + LEN(GetIt$) THEN         'from mid text.
                  IF col = Cpos THEN RETURN             '(except beginning)
                  LGetIt$ = LEFT$(GetIt$, (col - Cpos) - 1)
                  RGetIt$ = RIGHT$(GetIt$, (LEN(GetIt$) + Cpos) - col)
                  GetIt$ = LGetIt$ + RGetIt$
                  col = col - 1
                END IF
        CASE CHR$(9)                                    'Tab
                BEEP: RETURN                            '(not supported)
        CASE CHR$(0) + CHR$(82)                         'Insert
                IF InsertMode = 1 THEN InsertMode = 0: GOTO SelEnd
                IF InsertMode = 0 THEN InsertMode = 1: GOTO SelEnd
        CASE CHR$(0) + CHR$(71)                         'Home
                FOR q = 1 TO length                     'loops to find
                    FirstLtr = INSTR(q, GetIt$, " ")     'the last occurence
                    IF FirstLtr <> q THEN EXIT FOR      'of the opening spaces
                    StartPos = FirstLtr                 'on the text.
                NEXT
                col = StartPos + Cpos
        CASE CHR$(0) + CHR$(83)                         'Delete...
                IF col >= Cpos + LEN(GetIt$) THEN RETURN    'not from text end.
                IF col = Cpos THEN                         'from beginning.
                    GetIt$ = RIGHT$(GetIt$, LEN(GetIt$) - 1)
                    GOTO SelEnd
                END IF
                LGetIt$ = LEFT$(GetIt$, (col - Cpos))        'from mid text.
                RGetIt$ = RIGHT$(GetIt$, (LEN(GetIt$) + Cpos) - col - 1)
                GetIt$ = LGetIt$ + RGetIt$
                IF LEN(GetIt$) > length THEN GetIt$ = LEFT$(GetIt$, length)
        CASE CHR$(0) + CHR$(79)                             'End
                col = Cpos + LEN(GetIt$)
        CASE CHR$(0) + CHR$(75)                             'Left cursor
                IF col > Cpos THEN col = col - 1
        CASE CHR$(0) + CHR$(77)                             'Right cursor
                IF col < Cpos + length THEN col = col + 1
        CASE ELSE                                           'Add a letter...
           IF LEN(T$) > 1 THEN BEEP: RETURN        '(BEEP on non letter key).
           IF col >= Cpos + LEN(GetIt$) THEN            'at the end.
                col = col + 1
                IF col > Cpos + length THEN col = col - 1: RETURN
                GetIt$ = GetIt$ + T$
           END IF
           IF col < Cpos + LEN(GetIt$) THEN              'mid text, with
                IF InsertMode = 1 THEN                       'insert on.
                     LGetIt$ = LEFT$(GetIt$, (col - Cpos))
                     RGetIt$ = RIGHT$(GetIt$, (LEN(GetIt$) + Cpos) - col)
                     GetIt$ = LGetIt$ + T$ + RGetIt$
                     IF LEN(GetIt$) > length THEN GetIt$ = LEFT$(GetIt$, length)
                     col = col + 1
                END IF
                IF InsertMode = 0 THEN              'mid text with insert off.
                     LGetIt$ = LEFT$(GetIt$, (col - Cpos))
                     RGetIt$ = RIGHT$(GetIt$, (LEN(GetIt$) + Cpos) - col - 1)
                     GetIt$ = LGetIt$ + T$ + RGetIt$
                     IF LEN(GetIt$) > length THEN GetIt$ = LEFT$(GetIt$, length)
                     col = col + 1
                END IF
           END IF
   END SELECT
SelEnd:
   x = length - LEN(GetIt$)
        IF x < 0 THEN x = 0
   LOCATE Rpos, Cpos, 0: PRINT GetIt$; STRING$(x, " ")         'adds blanks.
        IF InsertMode = 0 THEN LOCATE row, col, 1, 1, 31   'Small cursor
        IF InsertMode = 1 THEN LOCATE row, col, 1, 7, 31    'Large cursor
RETURN
'----------------------------------------------------------
NearTheBottom:
GetInput$ = UCASE$(GetIt$)
END FUNCTION

SUB ListFile
TIMER OFF
ON ERROR GOTO NoDrive
REDIM AvlDrv(15)
 FOR x = 1 TO 15
         DrvLtr$ = CHR$(64 + x) + ":\"
         AvlDrv(x) = 1
         IF x > 3 THEN CHDIR DrvLtr$
NEXT x
SHELL "c:": SHELL "cd\"

ON ERROR GOTO Trapper
'----------------- variables ---
FFpath$(1) = "C:\"
Header$ = Title$: FFName$ = "*.QBW": PathFlag = 2: FFpath$(PathFlag) = "C:\"
CurDrv$ = "C:\": CurSearch$ = ".QBW": DirFlag = 1: DirSlider = 12
FFlag = 1: FFslider = 12
'-------------------------------
 COLOR 7, 1
 LOCATE 4, 10: PRINT "ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
 FOR x = 5 TO 21
 LOCATE x, 10: PRINT "บ                                                          บ"
 NEXT x
 LOCATE x, 10: PRINT "ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
 COLOR 15, 7
 FOR x = 5 TO 22
        c = SCREEN(x, 70)
        LOCATE x, 70: PRINT CHR$(c);
 NEXT x
 FOR x = 11 TO 70
        c = SCREEN(23, x)
        LOCATE 23, x: PRINT CHR$(c);
 NEXT x
 COLOR 7, 1
 LOCATE 5, 34: PRINT Title$: LOCATE 7, 13: PRINT "FILE:"
 COLOR 0, 7: LOCATE 7, 19: PRINT FFName$ + STRING$(48 - LEN(FFName$), " ")
        COLOR 7, 1: LOCATE 9, 13: PRINT FFpath$(PathFlag)
 LOCATE 10, 12: PRINT "ฺฤฤฤฤฤฤฟ"
 FOR x = 11 TO 20: LOCATE x, 12: PRINT "ณ      ณ": NEXT x
 LOCATE x, 12: PRINT "ภฤฤฤฤฤฤู"
 FOR x = 1 TO 9
        IF AvlDrv(x) = 1 THEN
                b = b + 1
                IF x = 3 THEN COLOR 0, 7 ELSE COLOR 7, 1
                LOCATE b + 10, 14, 0: PRINT "["; CHR$(64 + x); ":]"
        END IF
 NEXT x
 COLOR 7, 1
 LOCATE 10, 38: PRINT "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ "
 FOR x = 11 TO 20: LOCATE x, 38: PRINT "ณ              ฐณ": NEXT x
 LOCATE x, 38: PRINT "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"
 LOCATE 10, 20: PRINT "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
 FOR x = 11 TO 20: LOCATE x, 20: PRINT "ณ               ฐณ": NEXT x
 LOCATE x, 20: PRINT "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"
        GOSUB CheckDirs: GOSUB CheckFiles
 LOCATE 11, 57: PRINT "ฺฤฤฤฤฤฤฤฤฟ": LOCATE 12, 57: PRINT "ณ   OK   บ"
 LOCATE 13, 57: PRINT "ภออออออออผ": LOCATE 15, 57: PRINT "ฺฤฤฤฤฤฤฤฤฟ"
 LOCATE 16, 57: PRINT "ณ CANCEL บ": LOCATE 17, 57: PRINT "ภออออออออผ"
     COLOR 1, 0: LOCATE 11, 36: PRINT CHR$(24): LOCATE 11, 53: PRINT CHR$(24)
 LOCATE 20, 36: PRINT CHR$(25): LOCATE 20, 53: PRINT CHR$(25)
 COLOR 8, 7: LOCATE 12, 36: PRINT "þ": LOCATE 12, 53: PRINT "þ"
 COLOR 8, 1: LOCATE 20, 56: PRINT "UnderWARE(96)"
'-------------------------------
SkipErr:
        CALL MousePointer(0): CALL MousePointer(1): CALL MousePointer(3)
BackToIt:
    DO WHILE k$ <> CHR$(27)
          k$ = INKEY$
          IF k$ = CHR$(13) THEN GOSUB CheckValid
          CALL Mouse(cx, dx, bx)
          IF bx = 1 THEN
                IF dx > 56 AND dx < 67 THEN
                        IF cx > 10 AND cx < 14 THEN GOTO OKed
                        IF cx > 14 AND cx < 18 THEN GOTO Cancel
                END IF
                IF cx = 7 AND dx > 18 AND dx < 67 THEN GOSUB TypeFileIn
                IF cx > 10 AND cx < 21 THEN
                        IF dx = 36 THEN
                                IF cx = 11 THEN GOSUB ScrollUpDir
                                IF cx = 20 THEN GOSUB ScrollDwnDir
                        END IF
                        IF dx = 53 THEN
                                IF cx = 11 THEN GOSUB ScrollUpFile
                                IF cx = 20 THEN GOSUB ScrollDwnFile
                        END IF
                        IF dx > 14 AND dx < 18 THEN GOSUB PickDrive
                        IF dx > 21 AND dx < 35 THEN GOSUB PickDir
                        IF dx > 39 AND dx < 52 THEN GOSUB PickFile
                END IF
          END IF
    LOOP
    GOTO Cancel
'******************************** Exit *************************************
Done:
    CALL MousePointer(2)              'Turn mouse off
    OneToOpen$ = FFpath$(PathFlag) + FFName$
      COLOR 7, 1
      IF INSTR(OneToOpen$, "*") > 0 THEN
          COLOR 7, 0: LOCATE 12, 58: PRINT "   OK   ": GOTO SkipErr
      END IF
    IF Title$ = "SAVE A FILE" THEN
        LOCATE 9, 13: PRINT "Saving File..."; OneToOpen$
        ELSE LOCATE 9, 13: PRINT "Opening File..."; OneToOpen$
        END IF
GOTO EndListFile
'**************************** Return to Basic ******************************
TypeFileIn:
        COLOR 0, 7: FFName$ = GetInput$(7, 19, 48)
        IF FFName$ = "" THEN RETURN
        GOSUB CheckValid
        d = INSTR(FFName$, "."): CurSearch$ = MID$(FFName$, d)
        GOSUB CheckFiles
        RETURN
ScrollUpDir:
         COLOR 7, 1
         Delay .1
         IF NumDirs < 11 THEN RETURN
         IF DirFlag < 2 THEN RETURN
         LOCATE DirSlider, 36: PRINT "ฐ"
         DirFlag = DirFlag - 1
         DirSlider = INT(((DirFlag + 9) / NumDirs) * 8) + 11
         IF DirFlag = 1 THEN DirSlider = 12
           COLOR 8, 7: LOCATE DirSlider, 36: PRINT "þ"
           COLOR 7, 1: FOR x = 11 TO 20: LOCATE x, 22: PRINT "              ": NEXT x
         FOR x = 11 TO 20
           LOCATE x, 22: PRINT "["; AvlDir$(DirFlag + x - 11); "]"
         NEXT x
RETURN
ScrollDwnDir:
         COLOR 7, 1
         Delay .1
         IF NumDirs < 11 THEN RETURN
         IF AvlDir$(DirFlag + 10) = "" THEN RETURN
         LOCATE DirSlider, 36: PRINT "ฐ"
         DirFlag = DirFlag + 1
         DirSlider = INT(((DirFlag + 9) / NumDirs) * 8) + 11
         IF DirSlider < 12 THEN DirSlider = 12
                COLOR 8, 7: LOCATE DirSlider, 36: PRINT "þ"
                COLOR 7, 1
         FOR x = 11 TO 20: LOCATE x, 22: PRINT "              ": NEXT x
         FOR x = 11 TO 20
           LOCATE x, 22: PRINT "["; AvlDir$(DirFlag + x - 11); "]"
         NEXT x
RETURN
PickDir:
         COLOR 7, 1: DO WHILE bx = 1: CALL Mouse(cx, dx, bx): LOOP
         CALL MousePointer(2)
         FOR x = 11 TO 20: LOCATE x, 22: PRINT "              ": NEXT x
         FOR x = 11 TO 20
           IF AvlDir$(DirFlag + x - 11) <> "" THEN LOCATE x, 22: PRINT "["; AvlDir$(DirFlag + x - 11); "]"
         NEXT x
        COLOR 7, 1: LOCATE cx, 22: PRINT "             "
        LOCATE cx, 22: PRINT "["; AvlDir$(DirFlag + cx - 11); "]"
        COLOR 7, 1: ChangeDir$ = "CD " + AvlDir$(DirFlag + cx - 11)
           IF AvlDir$(DirFlag + cx - 11) <> ".." THEN
                PathFlag = PathFlag + 1
                FFpath$(PathFlag) = FFpath$(PathFlag - 1) + AvlDir$(DirFlag + cx - 11) + "\"
                END IF
           IF AvlDir$(DirFlag + cx - 11) = ".." THEN
                IF PathFlag = 1 THEN CALL MousePointer(1): CALL MousePointer(3): RETURN
                PathFlag = PathFlag - 1
                END IF
        SHELL ChangeDir$
  FFlag = 1: DirFlag = 1: GOSUB CheckDirs: GOSUB CheckFiles: GOSUB ReSetFSlide
        CALL MousePointer(1): CALL MousePointer(3)
RETURN
ScrollUpFile:
         COLOR 7, 1
         Delay .1
         IF NumFiles < 11 THEN RETURN
         IF FFlag < 2 THEN RETURN
         LOCATE FFslider, 53: PRINT "ฐ": FFlag = FFlag - 1
         FFslider = INT(((FFlag + 9) / NumFiles) * 8) + 11
         IF FFlag = 1 THEN FFslider = 12
                COLOR 8, 7: LOCATE FFslider, 53: PRINT "þ": COLOR 7, 1
         FOR x = 11 TO 20: LOCATE x, 40: PRINT "            ": NEXT x
         FOR x = 11 TO 20
           LOCATE x, 40: PRINT AvlFile$(FFlag + x - 11)
         NEXT x
RETURN
ScrollDwnFile:
         COLOR 7, 1
         Delay .1
         IF NumFiles < 11 THEN RETURN
         IF AvlFile$(FFlag + 10) = "" THEN RETURN
         LOCATE FFslider, 53: PRINT "ฐ": FFlag = FFlag + 1
         FFslider = INT(((FFlag + 9) / NumFiles) * 8) + 11
         IF FFslider < 12 THEN FFslider = 12
                COLOR 8, 7: LOCATE FFslider, 53: PRINT "þ": COLOR 7, 1
                FOR x = 11 TO 20: LOCATE x, 40: PRINT "            ": NEXT x
         FOR x = 11 TO 20
           LOCATE x, 40: PRINT AvlFile$(FFlag + x - 11)
         NEXT x
RETURN
PickFile:
         IF AvlFile$(FFlag + cx - 11) = "" THEN RETURN
         COLOR 7, 1: DO WHILE bx = 1: CALL Mouse(cx, dx, bx): LOOP
         CALL MousePointer(2)
         FOR x = 11 TO 20: LOCATE x, 40: PRINT "            ": NEXT x
         FOR x = 11 TO 20
           LOCATE x, 40: PRINT AvlFile$(FFlag + x - 11)
         NEXT x
        COLOR 8, 7: LOCATE cx, 40: PRINT "           "
        FFName$ = AvlFile$(FFlag + cx - 11): LOCATE cx, 40: PRINT FFName$
        LOCATE 7, 19, 0: PRINT FFName$: COLOR 7, 1
        CALL MousePointer(1): CALL MousePointer(3)
RETURN
PickDrive:
         b = 0: DO WHILE bx = 1: CALL Mouse(cx, dx, bx): LOOP
         CALL MousePointer(2)
         FOR x = 1 TO 10
           IF AvlDrv(x) = 1 THEN
                b = b + 1
                IF b = cx - 10 THEN
                        COLOR 8, 7
                        y$ = CHR$(64 + x) + ":"
                        LOCATE b + 10, 14, 0: PRINT "["; CHR$(64 + x); ":]"
                        ELSE
                        COLOR 7, 1
                        LOCATE b + 10, 14, 0: PRINT "["; CHR$(64 + x); ":]"
                END IF
           END IF
         NEXT x
         COLOR 7, 1: SHELL y$: SHELL "CD\": PathFlag = 1: FFpath$(PathFlag) = y$ + "\"
         GOSUB ReSetFSlide: GOSUB ReSetDSlide: GOSUB CheckDirs: GOSUB CheckFiles
        CALL MousePointer(1): CALL MousePointer(3)
RETURN
GOTO BackToIt
OKed:
        COLOR 8, 7: CALL MousePointer(2): LOCATE 12, 58: PRINT "   OK   "
        GOTO Done
Cancel:
  COLOR 8, 7: CALL MousePointer(2): LOCATE 16, 58: PRINT " CANCEL "
 SLEEP 1: COLOR 7, 1
 canceled = 1
 Npath$ = "Untitled.QBW"
 GOTO EndListFile
'------
CheckValid:
      IF Title$ = "OPEN A FILE" THEN d = INSTR(FFName$, "*"): e = INSTR(FFName$, ".")
      IF Title$ = "SAVE A FILE" THEN
                e = INSTR(FFName$, ".")
                IF e < 1 THEN FFName$ = FFName$ + ".QBW"
                d = 0: e = 2
                END IF
      IF d < 1 AND e > 1 THEN GOTO OKed
      LFname$ = LEFT$(FFName$, d - 1): CurSearch$ = MID$(FFName$, d + 1)
      IF e = LEN(FFName$) THEN CurSearch$ = ".*"
      IF d < 3 THEN LFname$ = CurDrv$
      COLOR 7, 1: LOCATE 9, 13, 0: PRINT STRING$(53, " ")
      LOCATE 9, 13, 0: PRINT FFpath$(PathFlag); "\*"; CurSearch$
RETURN
'-------
CheckDirs:
        REDIM AvlDir$(513): AvlDir$(1) = ".."
        SHELL "dir /a:d/o/b > c:\dir.$$$"
        x = 1
        OPEN "c:\dir.$$$" FOR INPUT AS #1
                DO WHILE NOT EOF(1)
                  x = x + 1
                  LINE INPUT #1, AvlDir$(x)
                  AvlDir$(x) = LEFT$(UCASE$(AvlDir$(x)), 12)
                LOOP
        CLOSE
        NumDirs = x
         FOR x = 11 TO 20: LOCATE x, 22: PRINT "              ": NEXT x
         FOR x = 11 TO 20
           IF AvlDir$(x - 10) <> "" THEN LOCATE x, 22: PRINT "["; AvlDir$(x - 10); "]"
         NEXT x
RETURN
'------------------
CheckFiles:
        REDIM AvlFile$(513)
    COLOR 7, 1: LOCATE 9, 13, 0: PRINT STRING$(53, " ")
      LOCATE 9, 13, 0: PRINT FFpath$(PathFlag); "*"; CurSearch$
        FFlag = 1
        FF$ = "dir *" + CurSearch$ + " /a:-d-h-s/o/b > c:\curfiles.$$$"
         SHELL FF$: NumFiles = x - 2: x = 0
        OPEN "c:\curfiles.$$$" FOR INPUT AS #2
                DO WHILE NOT EOF(2)
                  x = x + 1
                  LINE INPUT #2, AvlFile$(x)
                  IF LEN(AvlFile$(x)) > 12 THEN AvlFile$(x) = LEFT$(AvlFile$(x), 8) + RIGHT$(AvlFile$(x), 4)
                  AvlFile$(x) = UCASE$(AvlFile$(x))
                LOOP
        CLOSE
        NumFiles = x
         FOR x = 11 TO 20: LOCATE x, 40: PRINT "             ": NEXT x
         FOR x = 11 TO 20: LOCATE x, 40: PRINT AvlFile$(x - 10): NEXT x
RETURN
'------------------
ReSetFSlide:
                FFlag = 1: LOCATE FFslider, 53: PRINT "ฐ": COLOR 8, 7
                LOCATE 12, 53: PRINT "þ": COLOR 7, 1: FFslider = 12
RETURN
ReSetDSlide:
                DirFlag = 1: LOCATE DirSlider, 36: PRINT "ฐ": COLOR 8, 7
                LOCATE 12, 36: PRINT "þ": COLOR 7, 1: DirSlider = 12
RETURN
'------------------
EndListFile:
TIMER ON
END SUB

SUB Mouse (cx, dx, bx)
        
           POKE VARPTR(a(4)), &H92           'Swap code,Get CX setup
          CALL absolute(cx, VARPTR(a(0)))     'Run Code
              cx = (cx / 8) + 1               'Adjust 25x80
           POKE VARPTR(a(4)), &H91           'Swap code,Get DX setup
          CALL absolute(dx, VARPTR(a(0)))     'Run Code
              dx = (dx / 8) + 1               'Adjust 25x80
           POKE VARPTR(a(4)), &H93           'Swap code,Get BX setup
          CALL absolute(bx, VARPTR(a(0)))     'Run Code

END SUB

SUB MousePointer (SW)
        
           POKE VARPTR(a(0)) + 1, SW         'Swap code,Set AX = (SW)
          CALL absolute(c, VARPTR(a(0)))     'Run Code

END SUB

SUB SaveIt (KeyPress)
 LastTime = 0
 IF KeyPress = 10 AND Npath$ <> "" THEN GOTO SaveIt
  Dpath$ = "c:\qbwrite\": Dfile$ = "SAMPLE01.QBW"
  IF KeyPress = 3 THEN GOTO ShowFileMenu
  IF KeyPress = 5 THEN GOTO OpenNew
  IF KeyPress = 11 THEN GOTO SaveAs
  IF KeyPress = 12 THEN GOTO SaveAs
ShowFileMenu:
  COLOR 8, 15: LOCATE 5, 25, 0: PRINT "ษออออออออออออออออออออออออออออป"
    FOR x = 6 TO 12
       LOCATE x, 25: PRINT "บ                            บ"
    NEXT x
  LOCATE 13, 25: PRINT "ศออออออออออออออออออออออออออออผ"
 COLOR 15, 7
 FOR x = 6 TO 13
        c = SCREEN(x, 55)
        LOCATE x, 55: PRINT CHR$(c);
 NEXT x
 FOR x = 26 TO 55
        c = SCREEN(14, x)
        LOCATE 14, x: PRINT CHR$(c);
 NEXT x
  COLOR 0, 15: LOCATE 6, 35: PRINT "FILE  MENU"
  COLOR 8: LOCATE 8, 27: PRINT " [F 5] = New File          "
  LOCATE 9, 27: PRINT " [F10] = Quick Save        "
  LOCATE 10, 27: PRINT " [F11] = Save As...        "
  LOCATE 11, 27: PRINT " [F12] = Open File...      "
  LOCATE 12, 27: PRINT " [Esc] = Clear This Menu   "
  UU$ = "": LastTime = 0
     DO
      CALL Mouse(cx, dx, bx)
      UU$ = INKEY$
       IF bx = 1 AND dx > 27 AND dx < 50 THEN
          IF cx = 8 THEN UU$ = CHR$(0) + CHR$(63)
          IF cx = 9 THEN UU$ = CHR$(0) + CHR$(68)
          IF cx = 10 THEN UU$ = CHR$(0) + CHR$(133)
          IF cx = 11 THEN UU$ = CHR$(0) + CHR$(134)
          IF cx = 12 THEN UU$ = CHR$(27)
       END IF
       IF UU$ = CHR$(0) + CHR$(63) THEN KeyPress = 5: GOTO OpenNew
       IF UU$ = CHR$(0) + CHR$(68) AND Npath$ <> "" THEN KeyPress = 10: GOTO SaveIt
       IF UU$ = CHR$(0) + CHR$(68) AND Npath$ = "" THEN KeyPress = 10: GOTO SaveAs
       IF UU$ = CHR$(0) + CHR$(133) THEN KeyPress = 11: GOTO SaveAs
       IF UU$ = CHR$(0) + CHR$(134) THEN KeyPress = 12: GOTO SaveAs
       IF UU$ = CHR$(27) THEN GOTO EndOfSub
    LOOP
OpenNew:
   LastTime = 0: COLOR 0, 15: LOCATE 6, 27: PRINT "                           "
  COLOR 8, 15: LOCATE 5, 25, 0: PRINT "ษออออออออออออออออออออออออออออป"
    FOR x = 6 TO 12
       LOCATE x, 25: PRINT "บ                            บ"
    NEXT x
  LOCATE 13, 25: PRINT "ศออออออออออออออออออออออออออออผ"
 COLOR 15, 7
 FOR x = 6 TO 13
        c = SCREEN(x, 55)
        LOCATE x, 55: PRINT CHR$(c);
 NEXT x
 FOR x = 26 TO 55
        c = SCREEN(14, x)
        LOCATE 14, x: PRINT CHR$(c);
 NEXT x
    COLOR 8, 15
    LOCATE 7, 27: PRINT "Any work that has not been "
    LOCATE 8, 27: PRINT "   saved will be lost!     "
    LOCATE 9, 27: PRINT "                           "
    LOCATE 10, 27: PRINT "    Open New anyway?       "
    LOCATE 11, 27: PRINT "                           "
    LOCATE 12, 27: PRINT "                           "
   COLOR 8, 7: LOCATE 12, 29: PRINT "[ Yes ]": LOCATE 12, 44: PRINT "[ No  ]"
   COLOR 0, 15: LOCATE 12, 31: PRINT "Y": LOCATE 12, 46: PRINT "N"
     DO
        CALL Mouse(cx, dx, bx)
        a$ = UCASE$(INKEY$)
        IF bx = 1 AND cx = 12 THEN
           IF dx > 28 AND dx < 36 THEN a$ = "Y"
           IF dx > 43 AND dx < 51 THEN a$ = "N"
        END IF
        IF a$ = "Y" THEN NewFile = 1: Npath$ = "": EXIT SUB
        IF a$ = CHR$(27) THEN NewFile = 0: GOTO EndOfSub
        IF a$ = "N" THEN NewFile = 0: GOTO EndOfSub
     LOOP
OpenAFile:
  LastTime = 0: count = 0: NoOfPages = 0
  Npath$ = OneToOpen$
  IF RIGHT$(Npath$, 3) = "QBW" THEN FileMode = 3
  OPEN Npath$ FOR INPUT AS #1
        DO WHILE NOT EOF(1): LINE INPUT #1, a$: count = count + 1: LOOP
  CLOSE #1
  FullPg = INT(count / 21): ExtraLines = count MOD 21
        IF ExtraLines > 0 THEN NoOfPages = FullPg + 1 ELSE NoOfPages = FullPg
  DocLen = (NoOfPages * 21) + 1
 REDIM Doc$(DocLen): REDIM Text$(25): count = 1
 OPEN Npath$ FOR INPUT AS #1
        DO WHILE NOT EOF(1)
           LINE INPUT #1, a$
           Doc$(count) = a$
           IF count < 23 THEN Text$(count + 2) = a$
           count = count + 1
        LOOP
 CLOSE #1
 count = 1: PageNo = 1: COLOR 7, 0
   FOR x = 3 TO 23
        LOCATE x, 1, 0: PRINT STRING$(80, " ");
        LOCATE x, 1, 0: PRINT Text$(x);
   NEXT x
 GOTO EndOfSub
SaveAs:
  LastTime = 0
  IF KeyPress = 12 THEN Title$ = "OPEN A FILE" ELSE Title$ = "SAVE A FILE"
ListFile
  CALL MousePointer(0): CALL MousePointer(1): CALL MousePointer(3)
ReEnter:
    COLOR 0: LastTime = 0
    IF canceled = 1 THEN canceled = 0: GOTO EndOfSub
AcceptIt:
        IF KeyPress = 12 THEN GOTO OpenAFile
SaveIt:
  LastTime = 0
   IF FFName$ = Npath$ THEN KILL Npath$     'DEL the existing file to rewrite
   FOR x = UBOUND(Doc$, 1) TO 1 STEP -1
        IF LEN(Doc$(x)) > 0 THEN
                DO WHILE w < 80
                 w = w + 1
                 y = INSTR(w, Doc$(x), CHR$(0))
                 IF y <> w THEN EXIT FOR
                LOOP
        END IF
   NEXT x
   IF x < 1 THEN GOTO NoSaveIt
   OPEN OneToOpen$ FOR RANDOM AS #1
        FIELD #1, 81 AS z$
         FOR w = 1 TO x
          NulSpaces = 80 - LEN(Doc$(w))
          New$ = Doc$(w) + CHR$(13) + STRING$(NulSpaces, 0)
          LSET z$ = New$
          PUT #1, w
         NEXT w
    CLOSE
 GOTO EndOfSub
NoSaveIt:
  COLOR 8, 15: LOCATE 7, 25, 0: PRINT "ษออออออออออออออออออออออออออออป"
    FOR x = 8 TO 12
       LOCATE x, 25: PRINT "บ                            บ"
    NEXT x
  LOCATE 13, 25: PRINT "ศออออออออออออออออออออออออออออผ"
 COLOR 15, 7
 FOR x = 8 TO 13
        c = SCREEN(x, 55)
        LOCATE x, 55: PRINT CHR$(c);
 NEXT x
 FOR x = 26 TO 55
        c = SCREEN(14, x)
        LOCATE 14, x: PRINT CHR$(c);
 NEXT x
  COLOR 0, 15: LOCATE 8, 27: PRINT "                           "
  LOCATE 9, 27: PRINT " There is no text to save. ": COLOR 16
  LOCATE 11, 27: PRINT "       Returning...        "
  LOCATE 10, 27: PRINT "                           "
  LOCATE 12, 27: PRINT "                           "
  COLOR 7, 0: SLEEP 2: LastTime = 0
  FOR x = 3 TO 23: LOCATE x, 1: PRINT Text$(x): NEXT x
EndOfSub:
  LastTime = 0: COLOR 7, 0
  FOR x = 3 TO 23: LOCATE x, 25: PRINT STRING$(80, " "); : NEXT x
END SUB

