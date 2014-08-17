DECLARE SUB display (col1%, col2%, text$, sp1%, ze1%)
DECLARE SUB menubox (ze1%, sp1%, ze2%, sp2%)
DECLARE SUB intro ()
'===========================================================================
' Subject: REAL DIRECTORY                    Date: Year of 1991 (00:00)
'  Author: Rich Geldreich                    Code: QB, PDS  
'    Keys: REAL,DIRECTORY                  Packet: DOS.ABC
'===========================================================================
'RealDir.Bas version 1.00
'By Rich Geldreich December, 1991
'You may use this program in any way as you wish as long as
'you don't make any money off it before I know about it!
'For any questions, comments, write or call at...
'410 Market St.
'Gloucester City, New Jersey 08030
'(609)-456-0721
'===========================================================================
'       modified for using with SVGAQB-Library (Zephyr Soft)
'                     by Dieter Caspary in 1999
'                         Code: QBasic 4.5
'===========================================================================
'$INCLUDE: 'svgabc.bi'

'$DYNAMIC
DEFINT A-Z

DECLARE SUB RestorePath (a$)
DECLARE SUB ChangeDrive (Drive$)
DECLARE SUB Refresh (X, Y, Length)
DECLARE SUB Sort (a$(), Low, High)
DECLARE SUB GetDir (EntryName$(), Extension$(), EntryType(), DirNum, Path$, Status)
DECLARE SUB SelectFile (FileName$, Status)
DECLARE SUB MakeFrame (X1, Y1, X2, Y2)

DECLARE FUNCTION LogicalDrives (Drive$)
DECLARE FUNCTION GetKey ()
DECLARE FUNCTION CurrentPath$ ()
DECLARE FUNCTION CurrentDrive$ ()
DECLARE FUNCTION NumDrives ()
DECLARE FUNCTION RandInt (Lower, Upper)
DECLARE FUNCTION RealPath$ ()


CONST True = -1, False = NOT True
CONST File = 0, Directory = 1
CONST Enter = 13, TabKey = 9
CONST UpArrow = -72, DownArrow = -80
CONST LeftArrow = -75, RightArrow = -77
CONST BackSpace = 8
CONST Home = -71, EndKey = -79, Esc = 27

TYPE FileFindBuf
    DOS            AS STRING * 19
    CreateTime     AS STRING * 1
    Attributes     AS INTEGER
    AccessTime     AS INTEGER
    AccessDate     AS INTEGER
    FileSize       AS LONG
    FileName       AS STRING * 13
END TYPE

TYPE Register
    ax    AS INTEGER
    bx    AS INTEGER
    cx    AS INTEGER
    dx    AS INTEGER
    bp    AS INTEGER
    si    AS INTEGER
    di    AS INTEGER
    flags AS INTEGER
    ds    AS INTEGER
    es    AS INTEGER
END TYPE
DIM SHARED ErrorStatus


'sample usage:

IF WHICHVGA = 0 THEN STOP
dummy = RES640
intro


'The next program line gets a filename from the user.
'Don't overlook that THE CURRENT DIRECTORY IS CHANGED(unless you
'use RealDir and RestoreDir)!!!

'Status will be 2 if the user hits Escape.
'Status will be 1 if a file was selected.

OldPath$ = RealPath$
SelectFile FileName$, Status
RestorePath OldPath$


intro
display 0, 28, "Status returned:" + STR$(Status), 5, 5
IF Status = 2 THEN
	display 0, 28, "Aborted", 5, 7
ELSE
	display 0, 28, "File selected: " + FileName$, 5, 7
END IF

SLEEP
VIDEOMODESET 3
END


ErrorHandler:
    ErrorStatus = True
RESUME NEXT

REM $STATIC
'Changes current drive.
SUB ChangeDrive (Drive$)
    DIM inreg AS Register
    inreg.ax = &HE00
    inreg.dx = ASC(Drive$) - 65
    CALL INTERRUPT(&H21, inreg, inreg)
END SUB

'Returns current drive.
FUNCTION CurrentDrive$
    DIM inreg AS Register
    inreg.ax = &H1900
    CALL INTERRUPT(&H21, inreg, inreg)
    CurrentDrive$ = CHR$(65 + inreg.ax MOD 256)
END FUNCTION

'Returns current path(not a full path- the current drive must be added).
'**********************************************************************
'WARNING: for some reason, if the drive isn't ready this sub will
'HANG UP!!! (the SelectFile sub makes sure the drive is ready)
FUNCTION CurrentPath$
	DIM inreg AS Register
	DIM PathSize AS STRING * 64
	inreg.ax = &H4700
	inreg.dx = ASC(CurrentDrive$) - 64
	inreg.ds = VARSEG(PathSize)
	inreg.si = VARPTR(PathSize)
	CALL INTERRUPTX(&H21, inreg, inreg)
	CurrentPath$ = LEFT$(PathSize, INSTR(PathSize, CHR$(0)) - 1)
END FUNCTION

SUB display (col1, col2, text$, sp1, ze1)
X1 = sp1 * 8
X2 = ze1 * 16
DRWSTRING 1, col1, col2, text$, X1, X2
END SUB

'Generic Getdir subroutine. Status will be -1 if an error occurs.
'EntryName$() ,Extension$() and Entrytype will hold directory.
'EntryType() tells what the entry is(a FILE or DIRECTORY- see constants).
'Path$ must be full path + wildcard. ...like "c:\dos\*.*"
'If Path$ isn't found then Status will be -1.
SUB GetDir (EntryName$(), Extension$(), EntryType(), DirNum, Path$, Status)

    DIM inreg AS Register, outreg AS Register
    DIM Buffer AS FileFindBuf
     
    DirNum = 0


    inreg.ax = &H1A00
    inreg.ds = VARSEG(Buffer)
    inreg.dx = VARPTR(Buffer)
    CALL INTERRUPT(&H21, inreg, outreg)
    inreg.ax = &H4E00
    inreg.cx = 16
    Npath$ = Path$ + CHR$(0)
    inreg.dx = SADD(Npath$)
    CALL INTERRUPTX(&H21, inreg, outreg)
    FirstFM = (outreg.ax AND &HF)
    IF outreg.flags AND 1 THEN
	Status = True
	EXIT SUB
    ELSE
	Status = False
    END IF
  
    IF FirstFM = 0 THEN
	GOSUB MakeFile
	DO
	    inreg.ax = &H4F00
	    inreg.dx = SADD(Npath$)
	    CALL INTERRUPT(&H21, inreg, outreg)
	    NextFM = outreg.ax AND &HF
	    IF NextFM = 0 THEN
		GOSUB MakeFile
	    END IF
	LOOP WHILE NextFM = 0
    END IF
    EXIT SUB

MakeFile:
    IF LEFT$(Buffer.FileName, 1) = "." THEN
	RETURN
    END IF
 
     
    Entry$ = RTRIM$(Buffer.FileName)
    IF Buffer.Attributes = 4096 THEN
	EntryName$ = RTRIM$(LEFT$(Entry$, 8))
	EntryType = Directory
    ELSE
	IF INSTR(Entry$, ".") = 0 THEN
	    EntryName$ = RTRIM$(LEFT$(Entry$, 8))
	    Extension$ = ""
	ELSE
	    EntryName$ = LEFT$(Entry$, INSTR(Entry$, ".") - 1)
	    Extension$ = RTRIM$(LEFT$(MID$(Entry$, INSTR(Entry$, ".") + 1), 3))
	END IF
	EntryType = File
    END IF
    
  
    EntryName$(DirNum) = EntryName$
    Extension$(DirNum) = Extension$
    EntryType(DirNum) = EntryType

    DirNum = DirNum + 1

    Buffer.Attributes = 0
    Buffer.AccessTime = 0
    Buffer.AccessDate = 0
    Buffer.FileSize = 0
    Buffer.FileName = STRING$(13, 32)
RETURN

END SUB

'This sub returns the ascii keycode- extended keycodes(ones that have
'a zero as the first character) return easy to handle negative
'values.
FUNCTION GetKey
    DO
	a$ = INKEY$
    LOOP UNTIL a$ <> ""
    IF LEN(a$) = 2 THEN
	GetKey = -ASC(RIGHT$(a$, 1))
    ELSE
	GetKey = ASC(a$)
    END IF
END FUNCTION

SUB intro
FILLSCREEN 28
FOR i = 16 TO 31
DRWBOX 1, i, 0 + i - 16, 0 + i - 16, 639 - i + 16, 479 - i + 16
NEXT
FONTSYSTEM
a$ = "RealDir & SVGA   1999 by D.Caspary"
b$ = SPACE$(INT((76 - LEN(a$)) / 2))
DRWSTRING 1, 28, 216, b$ + a$ + b$, 16, 448
DRWLINE 1, 25, 16, 444, 623, 444
DRWLINE 1, 30, 16, 445, 623, 445
END SUB

FUNCTION LogicalDrives (Drive$)
    DIM inreg AS Register
    inreg.ax = &H440E
    inreg.bx = ASC(Drive$) - 64
    CALL INTERRUPT(&H21, inreg, inreg)
    IF (inreg.flags AND 1) = 1 THEN
	LogicalDrives = -1
    ELSE
	LogicalDrives = inreg.ax AND 255
    END IF
END FUNCTION

'Makes a frame in text mode. X1,Y1 start X2,Y2 end
SUB MakeFrame (X1, Y1, X2, Y2)

X1 = X1 * 8: Y1 = Y1 * 16: X2 = X2 * 8: Y2 = Y2 * 16

DRWLINE 1, 25, 16, 444, 623, 444
DRWLINE 1, 30, 16, 445, 623, 445
    
    UpLeft$ = "Ú": UpRight$ = "¿": LoLeft$ = "À": LoRight$ = "Ù"
    H$ = "Ä": V$ = "³"
    LOCATE Y1, X1
    PRINT UpLeft$; STRING$(X2 - X1 - 1, H$); UpRight$;
    LOCATE Y2, X1
    PRINT LoLeft$; STRING$(X2 - X1 - 1, H$); LoRight$;
    FOR Y = Y1 + 1 TO Y2 - 1
	LOCATE Y, X1
	PRINT V$;
	LOCATE Y, X2
	PRINT V$;
    NEXT
END SUB

SUB menubox (sp1, ze1, sp2, ze2)
X1 = sp1 * 8 - 2
X2 = ze1 * 16 - 2
Y1 = sp2 * 8 + 2
Y2 = ze2 * 16 + 2
DRWFILLBOX 1, 28, X1, X2, Y1, Y2
DRWLINE 1, 26, X1, X2, Y1, X2
DRWLINE 1, 26, X1, X2, X1, Y2
DRWLINE 1, 30, X1, Y2, Y1, Y2
DRWLINE 1, 30, Y1, X2, Y1, Y2
END SUB

'Returns the number of logical drives. For instance- if 4 is returned
'then the valid drive names are A: B: C: & D:
'Since I only got to check this sub out with my computer's
'drive configuration, this sub checks over it's findings
'to make sure it has the correct number of logical drives.
'(better safe than sorry!)
FUNCTION NumDrives
    DIM inreg AS Register
    inreg.ax = &HE00
    inreg.dx = ASC(CurrentDrive$) - 65
    CALL INTERRUPT(&H21, inreg, inreg)
    Temp = (inreg.ax MOD 256) - 1
    FOR a = 1 TO Temp
	IF LogicalDrives(CHR$(a + 64)) = -1 THEN
	    NumDrives = a - 1
	    EXIT FUNCTION
	END IF
    NEXT
    NumDrives = Temp
END FUNCTION

'This subroutine is for the QuickSort algoritmn only.
FUNCTION RandInt (Lower, Upper)
    RandInt = INT(RND(1) * (Upper - Lower) + .5) + Lower
END FUNCTION

'Returns the current path in a usable form.
'WARNING: if the drive isn't ready this sub will HANG UP!!!
FUNCTION RealPath$
    RealPath$ = CurrentDrive$ + ":\" + CurrentPath$
END FUNCTION

'Highlights an area. Use a COLOR statement before calling.
SUB Refresh (X, Y, Length)
    FOR a = X TO X + Length - 1
	'display a, Y, CHR$(SCREEN(Y, a))
    NEXT
END SUB

'Similar to the CHDIR command except this can also change the current
'drive.
SUB RestorePath (a$)
    ChangeDrive LEFT$(a$, 1)
    CHDIR a$
END SUB

'Allows the user to select a file.
'Valid keys:
'Up, down, left, and right arrow keys move cursor.
'Tab key changes between Files and Directories windows.
'A-Z, a-z, 0-9 goes right to the next name beginning with the pressed letter.
'Home and End keys go to beginning or ending of list.
'Enter selects.
'Esc aborts.
SUB SelectFile (FileName$, Status)
  
    'dim all arrays
    REDIM EntryName$(400), Extension$(400), EntryType(400)
    REDIM SortBuffer$(400)
    REDIM Window$(34, 12), Direct$(100)
  
    
   
    'make screen
    menubox 4, 8, 76, 27                'big frame
    menubox 5, 10, 32, 12               'filename frame
    menubox 5, 13, 58, 26               'files frame
    menubox 60, 13, 75, 26              'directories frame
    DRWLINE 1, 25, 5 * 8, 9 * 16 + 4, 75 * 8 - 1, 9 * 16 + 4
    DRWLINE 1, 30, 5 * 8, 9 * 16 + 5, 75 * 8 - 1, 9 * 16 + 5

    display 28, 216, "File Name                  ", 5, 10
    display 28, 216, "Files" + STRING$(48, 32), 5, 13
    display 28, 216, "Directories    ", 60, 13
   
    'If Position=1 then Cursor is at Files window, if it's 2 then
    'the cursor is at the Directories window
    Position = 1
   
    'set up path at root directory of current drive
    Newpath$ = CurrentDrive$ + ":\"
   
    'set up default wildcard
    WildCard$ = "*.*"
   
    DO
      
	'fast way of clearing arrays
	ERASE Window$, Direct$
	REDIM Window$(34, 12), Direct$(100)
      
	UseLast1 = False
	UseLast2 = False
       
	'get current drive
	Drive$ = CurrentDrive$
       
	'clear flag
	ErrorStatus = 0
		 
	'see if drive is ready
	ON ERROR GOTO ErrorHandler
	CHDIR Newpath$
	ON ERROR GOTO 0
       
	'if drives ready then get current path
	IF ErrorStatus = 0 THEN Path$ = CurrentPath$
       
	'Path$ will be "" if on root directory
	IF Path$ = "" THEN
	    FullPath$ = Drive$ + ":\" + WildCard$
	    Root = True
	ELSE
	    FullPath$ = Drive$ + ":\" + Path$ + "\" + WildCard$
	    Root = False
	END IF
	'FullPath$ now has ready to use path+wildcard

	display 0, 28, FullPath$ + STRING$(68 - LEN(FullPath$), 32), 5, 8
       
	'alert user if drive not ready; otherwise
	'get the directory
	IF ErrorStatus <> 0 THEN
	    SOUND 1000, 3
	    Num = 0
	    display 40, 28, "Drive Error", 5, 5
	ELSE
	    GetDir EntryName$(), Extension$(), EntryType(), Num, FullPath$, Status
	END IF
       
	'set NoFiles to True so it can be proved false later
	NoFiles = True
       
	'if Num<>0 then there are directories or files
	IF Num <> 0 THEN
	    TempNum = 0
	   
	    'put all files in sort buffer
	    FOR a = 0 TO Num - 1
		IF EntryType(a) = File THEN
		    SortBuffer$(TempNum) = EntryName$(a)
		    IF Extension$(a) <> "" THEN
			SortBuffer$(TempNum) = SortBuffer$(TempNum) + "." + Extension$(a)
		    END IF
		    TempNum = TempNum + 1
		END IF
	    NEXT
	    'if there are files then QuickSort them
	    'and put them in a 2 dimensional array
	    IF TempNum <> 0 THEN
	      
		Sort SortBuffer$(), 0, TempNum - 1
		X = 0
		Y = 0
		FOR a = 0 TO TempNum - 1
		    Window$(X, Y) = SortBuffer$(a)
		    LastX = X
		    LastY = Y
		    Y = Y + 1
		    IF Y > 11 THEN
			Y = 0: X = X + 1
		    END IF
		NEXT
		NoFiles = False
	    END IF
	END IF
       
	'set up directory array
	DirectNum = 0
	IF NOT Root THEN
	    Direct$(DirectNum) = ".."
	    DirectNum = DirectNum + 1
	END IF
	FOR a = 0 TO Num - 1
	    IF EntryType(a) = Directory THEN
		Direct$(DirectNum) = EntryName$(a)
		DirectNum = DirectNum + 1
	    END IF
	NEXT
	Sort Direct$(), 0, DirectNum - 1
	FOR a = 1 TO NumDrives
	    Direct$(DirectNum) = "[-" + CHR$(64 + a) + "-]"
	    DirectNum = DirectNum + 1
	NEXT

	'CurrentX and CurrentY hold cursors position in Files window
	CurrentX = 0
	CurrentY = 0
       
	'DirectNum holds maximum position if Direct$() array
	DirectNum = DirectNum - 1
       
	'put lists on screen
	GOSUB Update1
	GOSUB Update2
       
	'clear FileName window at put new name
	'display 19, 3, STRING$(8 + 3 + 1, " ")
	'display 19, 3, Window$(0, 0)

       
	DO
	    'if there are no files then don't let cursor at Files window
	    IF NoFiles THEN Position = 2
	   
	    'depending on Position, go to FileWindow or DirectWindow
	    SELECT CASE Position
		CASE 1
		    GOSUB FileWindow
		CASE 2
		    GOSUB DirectWindow
	    END SELECT
	    'loop while the user just presses the TAB key
	LOOP WHILE Status = 3
       
	'loop until the user presses ENTER or ESC
    LOOP UNTIL Status <> 0
   
    'erase the arrays
    ERASE EntryName$, Extension$, EntryType, SortBuffer$
'return to calling program
EXIT SUB

'updates the Files window with files
Update1:
    'switch to work screen so user gets a clean change
    'SCREEN 0, , 2, 1
    'COLOR 15, 0
    'copy old screen to work screen
    'PCOPY 1, 2
   
    'start printing the file entries
    RealX = 7
    FOR X = CurrentX TO CurrentX + 2
	FOR Y = 0 TO 11
	    display 0, 28, STRING$(12, 32), RealX, Y + 14
	    display 0, 28, Window$(X, Y), RealX, Y + 14
	NEXT
	RealX = RealX + 17
    NEXT
    'print location bar
    'LOCATE 22, 7: PRINT STRING$(50, "°")
    'IF LastX = 0 THEN
    '    X = 7
    'ELSE
    '    X = 7 + (CurrentX / LastX) * 49
    'END IF
    'LOCATE 22, X: PRINT "Û";
    'copy work screen to display screen
    'PCOPY 2, 1
    'SCREEN 0, , 1, 1
    'COLOR 15, 0
RETURN
'updates the directory window
Update2:
    'print directories
    RealY = 14
    FOR Y = CurrentY TO CurrentY + 11
	display 0, 28, STRING$(8, 32), 62, RealY
	display 0, 28, Direct$(Y), 62, RealY
	RealY = RealY + 1
    NEXT
 
    'print location bar
    'FOR Y = 8 TO 8 + 13
    '    LOCATE Y, 75
    '    PRINT "°"
    'NEXT
    'LOCATE 8 + (CurrentY / DirectNum) * 13, 75
    'PRINT "Û"
RETURN
'this sub controls the cursor when it is in the Files window
FileWindow:
    IF UseLast1 THEN
	XPos = LastXPos1
	YPos = LastYPos1
    ELSE
	XPos = 0
	YPos = 0
    END IF
    DO
	'highlight the cursors position
	display 28, 0, Window$(CurrentX + XPos, YPos), XPos * 17 + 7, YPos + 14
	'COLOR 0, 7
	'Refresh 6 + XPos * 17, YPos + 14
	'COLOR 15, 0
       
	'get the filename under the cursor
	FileName$ = Window$(CurrentX + XPos, YPos)
       
	'print the current file at tope
	display 0, 28, STRING$(13, 32), 5, 11
	display 0, 28, FileName$, 5, 11

	'wait for a key
	a = GetKey
       
	'clear cursor
	display 0, 28, Window$(CurrentX + XPos, YPos), XPos * 17 + 7, YPos + 14
	'Refresh XPos * 17 + 7, YPos + 9, 18
       
	'process the key
	SELECT CASE a
	    CASE Esc
		Status = 2
		RETURN
	    CASE Home
		CurrentX = 0
		XPos = 0: YPos = 0
		GOSUB Update1
	    CASE EndKey
		CurrentX = LastX
		YPos = LastY
		XPos = 0
		GOSUB Update1
	    CASE TabKey
		'save the cursors position
		LastXPos1 = XPos
		LastYPos1 = YPos
		UseLast1 = True

		Status = 3
		Position = 2
		RETURN
	    CASE Enter
		IF Path$ <> "" THEN
		    FileName$ = Drive$ + ":\" + Path$ + "\" + FileName$
		ELSE
		    FileName$ = Drive$ + ":\" + FileName$
		END IF
		Status = 1
		RETURN
	    CASE DownArrow
		YPos = YPos + 1
		IF XPos + CurrentX = LastX AND YPos > LastY THEN
		    YPos = LastY
		END IF
		IF YPos > 11 THEN
		    YPos = 0
		    XPos = XPos + 1
		    IF XPos > 2 THEN
			XPos = 2
			CurrentX = CurrentX + 1
			IF CurrentX - 2 > LastX THEN
			    CurrentX = LastX - 2
			END IF
			GOSUB Update1
		    END IF
		END IF
	    CASE UpArrow
		YPos = YPos - 1
		IF YPos < 0 THEN
		    IF XPos + CurrentX = 0 THEN
			YPos = 0
		    ELSE
			YPos = 11
			XPos = XPos - 1
			IF XPos < 0 THEN
			    XPos = 0
			    CurrentX = CurrentX - 1
			    IF CurrentX < 0 THEN
				CurrentX = 0
				XPos = 2
			    END IF
			    GOSUB Update1
			END IF
		    END IF
		END IF
	    CASE LeftArrow
		XPos = XPos - 1
		IF XPos < 0 THEN
		    XPos = 0
		    IF XPos + CurrentX = 0 THEN
			YPos = 0
		    ELSE
			CurrentX = CurrentX - 1
			GOSUB Update1
		    END IF
		END IF
	    CASE RightArrow
		XPos = XPos + 1
		IF XPos + CurrentX > LastX THEN
		    XPos = LastX - CurrentX
		    YPos = LastY
		END IF
		IF XPos + CurrentX = LastX AND YPos > LastY THEN
		    YPos = LastY
		END IF
		IF XPos > 2 THEN
		    XPos = 2
		    CurrentX = CurrentX + 1
		    IF XPos + CurrentX = LastX AND YPos > LastY THEN
			YPos = LastY
		    END IF
		    GOSUB Update1
		END IF
	    CASE 65 TO 90, 97 TO 122, 48 TO 57
		a$ = UCASE$(CHR$(a))
		RealX = CurrentX + XPos
		RealY = CurrentY + YPos
		StopX = RealX
		StopY = RealY
		ScanX = RealX
		ScanY = RealY + 1
		IF ScanY > 11 THEN
		    ScanY = 0
		    ScanX = ScanX + 1
		END IF
		DO UNTIL ScanX > LastX OR LEFT$(Window$(ScanX, ScanY), 1) = a$
		    ScanY = ScanY + 1
		    IF ScanY > 11 THEN
			ScanY = 0
			ScanX = ScanX + 1
		    END IF
		LOOP
		IF NOT ScanX > LastX THEN
		    XPos = 0
		    YPos = ScanY
		    CurrentX = ScanX
		    GOSUB Update1
		ELSE
		    ScanX = 0
		    ScanY = 0
		    DO UNTIL (ScanX = StopX AND ScanY = StopY) OR LEFT$(Window$(ScanX, ScanY), 1) = a$
			ScanY = ScanY + 1
			IF ScanY > 11 THEN
			    ScanY = 0
			    ScanX = ScanX + 1
			END IF
		    LOOP
		    IF NOT (ScanX = StopX AND ScanY = StopY) THEN
			XPos = 0
			YPos = ScanY
			CurrentX = ScanX
			GOSUB Update1
		    END IF
		END IF
	END SELECT
    LOOP
'this sub controls the cursor when it is in the Directories window
DirectWindow:
    IF UseLast2 THEN
	YPos = LastYPos2
    ELSE

	YPos = 0
    END IF
    DO
	'highlight the cursors position
	display 28, 0, Direct$(YPos + CurrentY), 62, YPos + 14
	'COLOR 0, 7
	'Refresh 61, YPos + 14, 14
	'wait for a key
	a = GetKey
       
	'unhighlight the cursors position
	display 0, 28, Direct$(YPos + CurrentY), 62, YPos + 14
	'COLOR 15, 0
	'Refresh 61, YPos + 14, 14
       
	'process key
	SELECT CASE a
	    CASE Esc
		Status = 2
		RETURN
	    CASE TabKey
		'see if it's ok to go to files window
		IF NOT NoFiles THEN
		    'save the cursors position
		    UseLast2 = True
		    LastYPos2 = YPos
		   
		    Position = 1
		    Status = 3
		    RETURN
		END IF
	    CASE Enter
		NewDirect$ = Direct$(YPos + CurrentY)
		IF NewDirect$ = ".." THEN
		    Newpath$ = ".."
		    Status = 0
		    RETURN
		ELSEIF LEFT$(NewDirect$, 1) = "[" THEN
		    'PCOPY 1, 2
		    NewDrive$ = MID$(NewDirect$, 3, 1)
		    ErrorStatus = 0
		   
		    'check drive to see if it is ready
		    ON ERROR GOTO ErrorHandler
		    CHDIR NewDrive$ + ":\"
		    ON ERROR GOTO 0
		   
		    'PCOPY 2, 1
		    IF ErrorStatus <> 0 THEN
			SOUND 1000, 3
			display 40, 28, "Drive Error", 5, 5
			'LOCATE 25, 34
			'COLOR 15 + 16
			'PRINT "Drive Error";
			'COLOR 15
		    ELSE
			display 22, 28, STRING$(11, 32), 5, 5
			'LOCATE 25, 34
			'PRINT STRING$(11, 32);
			Newpath$ = NewDrive$ + ":\"
			ChangeDrive NewDrive$
			Status = 0
			RETURN
		    END IF
		ELSE
		    IF Path$ = "" THEN
			Newpath$ = Drive$ + ":\" + NewDirect$
		    ELSE
			Newpath$ = Drive$ + ":\" + Path$ + "\" + NewDirect$
		    END IF
		    Status = 0
		    RETURN
		END IF
	    CASE DownArrow
		YPos = YPos + 1
		IF YPos + CurrentY > DirectNum THEN
		    YPos = YPos - 1
		END IF
		IF YPos > 11 THEN
		    YPos = 11
		    CurrentY = CurrentY + 1
		    IF CurrentY + 11 > DirectNum THEN
			CurrentY = DirectNum - 11
		    END IF
		    GOSUB Update2
		END IF
	    CASE UpArrow
		YPos = YPos - 1
		IF YPos < 0 THEN
		    YPos = 0
		    CurrentY = CurrentY - 1
		    IF CurrentY < 0 THEN
			CurrentY = 0
		    END IF
		    GOSUB Update2
		END IF
	    CASE Home
		YPos = 0
		CurrentY = 0
		GOSUB Update2
	    CASE EndKey
		YPos = 0
		CurrentY = DirectNum
		GOSUB Update2
	    CASE 65 TO 90, 97 TO 122, 48 TO 57
		a$ = UCASE$(CHR$(a))
		StopScan = YPos + CurrentY
		Scan = StopScan + 1
		DO UNTIL Scan > DirectNum OR LEFT$(Direct$(Scan), 1) = a$
		    Scan = Scan + 1
		LOOP
		IF NOT Scan > DirectNum THEN
		    YPos = 0
		    CurrentY = Scan
		    GOSUB Update2
		ELSE
		    Scan = 0
		    DO UNTIL Scan = StopScan OR LEFT$(Direct$(Scan), 1) = a$
			Scan = Scan + 1
		    LOOP
		    IF NOT Scan = StopScan THEN
			YPos = 0
			CurrentY = Scan
			GOSUB Update2
		    END IF
		END IF
	END SELECT
    LOOP

END SUB

'QuickSorts a string array. Low=first entry High=last entry
SUB Sort (a$(), Low, High)

   IF Low < High THEN
      IF High - Low = 1 THEN
	 IF a$(Low) > a$(High) THEN
	    SWAP a$(Low), a$(High)
	 END IF
      ELSE

	 RandIndex = RandInt(Low, High)
	 SWAP a$(High), a$(RandIndex)
	 Partition$ = a$(High)
	 DO

	    i = Low: J = High
	    DO WHILE (i < J) AND (a$(i) <= Partition$)
	       i = i + 1
	    LOOP
	    DO WHILE (J > i) AND (a$(J) >= Partition$)
	       J = J - 1
	    LOOP

	    IF i < J THEN
	       SWAP a$(i), a$(J)
	    END IF
	 LOOP WHILE i < J

	 SWAP a$(i), a$(High)

	 IF (i - Low) < (High - i) THEN
	    Sort a$(), Low, i - 1
	    Sort a$(), i + 1, High
	 ELSE
	    Sort a$(), i + 1, High
	    Sort a$(), Low, i - 1
	 END IF
      END IF
   END IF
END SUB

