'----------------------Quick RoadHog 2  Install-------------------------------
'                                       Uninstall
'                                       Copy






'                             Press F5 to Start








DECLARE SUB SETHUES (HUE() AS ANY, SC%, EC%)
CLEAR
ON ERROR GOTO 4
TYPE HUES
RED AS INTEGER
GRN AS INTEGER
BLU AS INTEGER
END TYPE

SCREEN 0: CLS
PRINT "You may Stop this Program at any time by pressing Ctrl+Break."
PRINT "Press"
PRINT "1 To Install Quick RoadHog 2 or Run the Tests."
PRINT ""
PRINT "2 To UnInstall Quick RoadHog 2."
PRINT ""
PRINT "3 To copy Quick RoadHog 2 to a Disk."
PRINT ""
11 wee$ = INKEY$: IF wee$ = "" THEN 11
IF wee$ = "2" THEN 12
IF wee$ = "3" THEN 13
REM--------------------------------Test or Install
PRINT " This Program will test your system to see if it will"
PRINT "be able to run Quick RoadHog 2 at a playable speed."
PRINT , " Press any key to start."
1 IF INKEY$ = "" THEN 1
CLS : PRINT "Switching to screen mode 7"
FOR wee = 5000 TO 0 STEP -5: LOCATE 2: PRINT wee: NEXT wee
CLEAR : SCREEN 7
CLS : PRINT "Screen 7 OK"
PRINT "Redefining Screen Size"
FOR wee = 5000 TO 0 STEP -5: LOCATE 3: PRINT wee: NEXT wee
WINDOW SCREEN (0, 0)-(100, 100)
LOCATE 4: PRINT "Size OK"
PRINT "Checking Graphics"
FOR wee = 0 TO 15
LINE (0, 0)-(100, 100), wee: LINE (100, 0)-(0, 100), wee
CIRCLE (50, 50), wee, wee: LINE (50 - wee, 50 - wee)-(50 + wee, 50 + wee), 16 - wee, B
FOR DLY = 0 TO 1000: NEXT DLY
NEXT wee
PRINT "BackGround Colors"
FOR wee = 15 TO 0 STEP -1
FOR DLY = 0 TO 1000: NEXT DLY
COLOR , wee
NEXT wee
PRINT "Graphics OK"
FOR wee = 100 TO 0 STEP -5
LINE (wee, 0)-(wee + 5, 25), 8, BF: LINE (wee + 5, 5)-(wee + 10, 20), 0, BF
FOR DLY = 0 TO 1000: NEXT DLY
NEXT wee
LINE (95, 5)-(100, 20), 8, BF
LOCATE 3, 4: PRINT "Switching to Screen Mode 13"
FOR wee = 5000 TO 0 STEP -5: LOCATE 4, 3: PRINT wee: NEXT wee
SCREEN 13
WINDOW SCREEN (0, 0)-(100, 100)
LINE (0, 0)-(100, 100), 1: LINE (100, 0)-(0, 100), 1
FOR wee = 255 TO 0 STEP -1: LINE (RND * 100, RND * 100)-(RND * 100, RND * 100), wee: LOCATE 1: PRINT wee: PRINT "Testing 256 Colors": NEXT wee
CLS : PRINT "About to Test Speed": PRINT "This Should Take About 20 Seconds": PRINT "You May Hear Sound . . ."
FOR wee = 5000 TO 0 STEP -2: LOCATE 4: PRINT wee: NEXT wee
TIME1 = TIMER
FOR TIME = 0 TO 70
LOCATE 1: PRINT "Testing Speed": PLAY "mbt255l32n1"
CLS
FOR wee = 5 TO 100 STEP 10
LINE (wee, -500)-(wee + 4, 95), 200, BF
LINE (wee + 6, 5)-(wee + 8, 600), 200, BF
PAINT (50, 50), 25, 200
NEXT wee
NEXT TIME
FOR wee = 1000 TO 0 STEP -1
LOCATE 22, 25: PRINT wee: PLAY "mbt255l64n1"
LOCATE INT(1 + (RND * 25)), INT(1 + (RND * 23))
PRINT CHR$(INT(34 + (RND * 220)))
NEXT wee
TIME2 = TIMER
TIME = TIME2 - TIME1
SCREEN 7
CLS : PRINT "Screen Mode 7 Page 0"
PRINT "Checking Pages"
SCREEN , , 1: PRINT "Screen Mode 7 Page 1"
FOR wee = 0 TO 25
IF PAGE = 0 THEN PAGE = 1 ELSE PAGE = 0
SCREEN , , PAGE, PAGE
LOCATE 22: PRINT CHR$(INT(34 + (RND * 220)))
FOR DLY = 0 TO 1000: NEXT DLY
NEXT wee
SCREEN 7, , 0, 0
TIME2 = TIMER: speed = TIME2 - TIME1
2 '---------------------------------
CLS
PRINT "Speed was"; speed; "Seconds"
PRINT , , "Speed Table:"
PRINT , , " 16   Turbo"
PRINT , , " 18   Very Fast"
PRINT , , " 20   Fast"
PRINT , , " 22   Playable"
PRINT , , " 24   Too Slow"
PRINT , , " 26   No Way"
LOCATE 23: PRINT "Press Q to End Testing, I to Install"
3 wee$ = INKEY$: IF wee$ = "" THEN 3
IF wee$ = "q" OR wee$ = "Q" THEN END
IF wee$ <> "i" AND wee$ <> "I" THEN BEEP: GOTO 2
CLS
PRINT "Did You DownLoad This From AOL ?"
7 wee$ = INKEY$: IF wee$ = "" THEN 7
IF wee$ = "y" OR wee$ = "Y" THEN PAT$ = "C:\AOL\DOWNLOAD\HOG22Q\BASIC\HOG2"
IF wee$ = "n" OR wee$ = "N" THEN PRINT "Enter the Drive Letter Where": PRINT "RoadHog 2 is Now."
9 IF wee$ = "n" OR wee$ = "N" THEN drv$ = INKEY$: IF drv$ = "" THEN 9
IF wee$ = "n" OR wee$ = "N" THEN PRINT "Type in the Path Where it is": PRINT "(dont include the drive Letter)": PRINT "ie. \GAMES\FILES"
IF wee$ = "n" OR wee$ = "N" THEN LINE INPUT PAT$
PRINT "Press any Key to Install RoadHog 2 From"
PRINT drv$; ":"; PAT$; " to C:\BASIC\HOG2"
8 IF INKEY$ = "" THEN 8
REM---------------------------------------------------------Install RoadHog 2

SHELL "C:\"
SHELL "CD\"
SHELL "MD basic"
SHELL "CD Basic"
SHELL "MD Hog2"
SHELL "CD\"
SHELL drv$ + ":"
SHELL "CD" + PAT$
SHELL "copy *.qbi C:\BASIC\Hog2"
SHELL "copy Install.bas C:\BASIC\Hog2"
SHELL "copy ReadMe!.doc C:\BASIC\Hog2"
SHELL "copy RoadHog2.tmp C:\BASIC\Hog2"
SHELL "copy RoadHog2.err C:\BASIC\Hog2"
SHELL "copy hog2.rh2 C:\BASIC\Hog2"
SHELL "copy *.pal C:\BASIC\Hog2"
SHELL "copy RoadHog2.bas C:\BASIC\Hog2"
SHELL "copy Hog2.ico C:\Windows"
PLAY "mbt255l32n20n50"
GOTO 6
4 REM-----------------ERROR
SCREEN 0, 0, 0, 0
CLS
PRINT "Error!"
PRINT "There is a high chance that you"
PRINT "won't be able to play RoadHog 2,"
PRINT "Unless a Path was set wrong."
PRINT "Try running this again, or using"
5 IF INKEY$ = "" THEN 5 ELSE END

6 'End Of Program
CLS
PRINT "Install Complete !"
PRINT , , "If your in Program Manager or Explorer"
PRINT "remember to press F5"
PRINT "to refresh the screen."
PRINT , , "Windows 95 users should make a shortcut for best performance,"
PRINT "and set Idle Sensitivity to Low (all the way LEFT) in the shortcut's"
PRINT "properties (Right Click the shortcut to set properties)."
END

12 REM----------------------------------UnInstall
CLS
PRINT "Removing Quick RoadHog 2 . . ."
SHELL "c:"
SHELL "cd\"
SHELL "cd BASIC"
SHELL "deltree HOG2"
SHELL "cd\"
PRINT "Remove Hog2.ico [yn]"
18 wee$ = INKEY$: IF wee$ = "" THEN 18
IF wee$ = "N" OR wee$ = "n" THEN 14
SHELL "cd windows"
SHELL "del hog2.ico"
GOTO 14

13 REM----------------------------------Copy
CLS : PRINT "You must have installed Quick Roadog 2": PRINT "before doing this."
PRINT "Enter the Drive Letter to copy to."
15 drv$ = INKEY$: IF drv$ = "" THEN 15
PRINT "Press any key to copy to "; drv$; ":\"
16 IF INKEY$ = "" THEN 16
SHELL "c:"
SHELL "cd\"
SHELL "cd c:\basic\hog2"
PRINT "Copying..."
SHELL "copy *.* " + drv$ + ":\"
SHELL "cd\"
SHELL "cd windows"
SHELL "copy hog2.ico " + drv$ + ":\"
14 PRINT "Done"
END

DEFINT A-Z
SUB SETHUES (HUE() AS HUES, SC, EC)

 OUT &H3C8, SC

 FOR I = SC TO EC
   OUT &H3C9, HUE(I).RED: OUT &H3C9, HUE(I).GRN: OUT &H3C9, HUE(I).BLU
 NEXT

END SUB

