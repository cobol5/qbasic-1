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

'$DYNAMIC
DEFINT A-Z

DECLARE SUB RestorePath (A$)
DECLARE SUB ChangeDrive (Drive$)
DECLARE SUB Refresh (X, Y, Length)
DECLARE SUB Sort (A$(), Low, High)
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
    Ax    AS INTEGER
    Bx    AS INTEGER
    Cx    AS INTEGER
    Dx    AS INTEGER
    Bp    AS INTEGER
    Si    AS INTEGER
    Di    AS INTEGER
    Flags AS INTEGER
    Ds    AS INTEGER
    Es    AS INTEGER
END TYPE
DIM SHARED ErrorStatus


'sample usage:



SCREEN 0, , 1, 1

CLS
'The next program line gets a filename from the user.
'Don't overlook that THE CURRENT DIRECTORY IS CHANGED(unless you
'use RealDir and RestoreDir)!!!

'Status will be 2 if the user hits Escape.
'Status will be 1 if a file was selected.

OldPath$ = RealPath$
SelectFile FileName$, Status
RestorePath OldPath$


CLS
PRINT "Status returned:"; Status
IF Status = 2 THEN
    PRINT "Aborted."
ELSE
    PRINT "File selected: "; FileName$
END IF
END


ErrorHandler:
    ErrorStatus = True
RESUME NEXT

REM $STATIC
'Changes current drive.
SUB ChangeDrive (Drive$)
    DIM InReg AS Register
    InReg.Ax = &HE00
    InReg.Dx = ASC(Drive$) - 65
    CALL interrupt(&H21, InReg, InReg)
END SUB

'Returns current drive.
FUNCTION CurrentDrive$
    DIM InReg AS Register
    InReg.Ax = &H1900
    CALL interrupt(&H21, InReg, InReg)
    CurrentDrive$ = CHR$(65 + InReg.Ax MOD 256)
END FUNCTION

'Returns current path(not a full path- the current drive must be added).
'**********************************************************************
'WARNING: for some reason, if the drive isn't ready this sub will
'HANG UP!!! (the SelectFile sub makes sure the drive is ready)
FUNCTION CurrentPath$
        DIM InReg AS Register
        DIM PathSize AS STRING * 64
        InReg.Ax = &H4700
        InReg.Dx = ASC(CurrentDrive$) - 64
        InReg.Ds = VARSEG(PathSize)
        InReg.Si = VARPTR(PathSize)
        CALL InterruptX(&H21, InReg, InReg)
        CurrentPath$ = LEFT$(PathSize, INSTR(PathSize, CHR$(0)) - 1)
END FUNCTION

'Generic Getdir subroutine. Status will be -1 if an error occurs.
'EntryName$() ,Extension$() and Entrytype will hold directory.
'EntryType() tells what the entry is(a FILE or DIRECTORY- see constants).
'Path$ must be full path + wildcard. ...like "c:\dos\*.*"
'If Path$ isn't found then Status will be -1.
SUB GetDir (EntryName$(), Extension$(), EntryType(), DirNum, Path$, Status)

    DIM InReg AS Register, OutReg AS Register
    DIM Buffer AS FileFindBuf
     
    DirNum = 0


    InReg.Ax = &H1A00
    InReg.Ds = VARSEG(Buffer)
    InReg.Dx = VARPTR(Buffer)
    CALL interrupt(&H21, InReg, OutReg)
    InReg.Ax = &H4E00
    InReg.Cx = 16
    Npath$ = Path$ + CHR$(0)
    InReg.Dx = SADD(Npath$)
    CALL InterruptX(&H21, InReg, OutReg)
    FirstFM = (OutReg.Ax AND &HF)
    IF OutReg.Flags AND 1 THEN
        Status = True
        EXIT SUB
    ELSE
        Status = False
    END IF
  
    IF FirstFM = 0 THEN
        GOSUB MakeFile
        DO
            InReg.Ax = &H4F00
            InReg.Dx = SADD(Npath$)
            CALL interrupt(&H21, InReg, OutReg)
            NextFM = OutReg.Ax AND &HF
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
        A$ = INKEY$
    LOOP UNTIL A$ <> ""
    IF LEN(A$) = 2 THEN
        GetKey = -ASC(RIGHT$(A$, 1))
    ELSE
        GetKey = ASC(A$)
    END IF
END FUNCTION

FUNCTION LogicalDrives (Drive$)
    DIM InReg AS Register
    InReg.Ax = &H440E
    InReg.Bx = ASC(Drive$) - 64
    CALL interrupt(&H21, InReg, InReg)
    IF (InReg.Flags AND 1) = 1 THEN
        LogicalDrives = -1
    ELSE
        LogicalDrives = InReg.Ax AND 255
    END IF
END FUNCTION

'Makes a frame in text mode. X1,Y1 start X2,Y2 end
SUB MakeFrame (X1, Y1, X2, Y2)
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

'Returns the number of logical drives. For instance- if 4 is returned
'then the valid drive names are A: B: C: & D:
'Since I only got to check this sub out with my computer's
'drive configuration, this sub checks over it's findings
'to make sure it has the correct number of logical drives.
'(better safe than sorry!)
FUNCTION NumDrives
    DIM InReg AS Register
    InReg.Ax = &HE00
    InReg.Dx = ASC(CurrentDrive$) - 65
    CALL interrupt(&H21, InReg, InReg)
    Temp = (InReg.Ax MOD 256) - 1
    FOR A = 1 TO Temp
        IF LogicalDrives(CHR$(A + 64)) = -1 THEN
            NumDrives = A - 1
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
    FOR A = X TO X + Length - 1
        LOCATE Y, A
        PRINT CHR$(SCREEN(Y, A));
    NEXT
END SUB

'Similar to the CHDIR command except this can also change the current
'drive.
SUB RestorePath (A$)
    ChangeDrive LEFT$(A$, 1)
    CHDIR A$
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
  
    
    SCREEN 0, , 1, 1
    CLS
    COLOR 14, 0
   
    'make screen
    MakeFrame 1, 1, 80, 24
    MakeFrame 5, 7, 58, 22
    MakeFrame 60, 7, 75, 22
    MakeFrame 17, 2, 32, 4
  
   
    COLOR 15
    LOCATE 1, 33
    PRINT " Choose File "
    LOCATE 3, 5
    PRINT "File Name:"
    LOCATE 6, 29
    PRINT "Files"
    LOCATE 6, 62
    PRINT "Directories"
   
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

        LOCATE 5, 5
        PRINT FullPath$; STRING$(79 - POS(0), 32)
       
        'alert user if drive not ready; otherwise
        'get the directory
        IF ErrorStatus <> 0 THEN
            SOUND 1000, 3
            Num = 0
            LOCATE 25, 34
            COLOR 15 + 16
            PRINT "Drive Error";
            COLOR 15
        ELSE
            GetDir EntryName$(), Extension$(), EntryType(), Num, FullPath$, Status
        END IF
       
        'set NoFiles to True so it can be proved false later
        NoFiles = True
       
        'if Num<>0 then there are directories or files
        IF Num <> 0 THEN
            TempNum = 0
           
            'put all files in sort buffer
            FOR A = 0 TO Num - 1
                IF EntryType(A) = File THEN
                    SortBuffer$(TempNum) = EntryName$(A)
                    IF Extension$(A) <> "" THEN
                        SortBuffer$(TempNum) = SortBuffer$(TempNum) + "." + Extension$(A)
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
                FOR A = 0 TO TempNum - 1
                    Window$(X, Y) = SortBuffer$(A)
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
        FOR A = 1 TO NumDrives
            Direct$(DirectNum) = "[-" + CHR$(64 + A) + "-]"
            DirectNum = DirectNum + 1
        NEXT
        FOR A = 0 TO Num - 1
            IF EntryType(A) = Directory THEN
                Direct$(DirectNum) = EntryName$(A)
                DirectNum = DirectNum + 1
            END IF
        NEXT
        Sort Direct$(), 0, DirectNum - 1

        'CurrentX and CurrentY hold cursors position in Files window
        CurrentX = 0
        CurrentY = 0
       
        'DirectNum holds maximum position if Direct$() array
        DirectNum = DirectNum - 1
       
        'put lists on screen
        GOSUB Update1
        GOSUB Update2
       
        'clear FileName window at put new name
        LOCATE 3, 19: PRINT STRING$(8 + 3 + 1, " ")
        LOCATE 3, 19: PRINT Window$(0, 0)
       
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
    SCREEN 0, , 2, 1
    COLOR 15, 0
    'copy old screen to work screen
    PCOPY 1, 2
   
    'start printing the file entries
    RealX = 7
    FOR X = CurrentX TO CurrentX + 2
        FOR Y = 0 TO 11
            LOCATE Y + 9, RealX
            PRINT STRING$(12, 32)
            LOCATE Y + 9, RealX
            PRINT Window$(X, Y)
        NEXT
        RealX = RealX + 17
    NEXT
    'print location bar
    LOCATE 22, 7: PRINT STRING$(50, "°")
    IF LastX = 0 THEN
        X = 7
    ELSE
        X = 7 + (CurrentX / LastX) * 49
    END IF
    LOCATE 22, X: PRINT "Û";
    'copy work screen to display screen
    PCOPY 2, 1
    SCREEN 0, , 1, 1
    COLOR 15, 0
RETURN
'updates the directory window
Update2:
    'print directories
    RealY = 9
    FOR Y = CurrentY TO CurrentY + 11
        LOCATE RealY, 62
        PRINT STRING$(8, 32);
        LOCATE RealY, 62
        PRINT Direct$(Y)
        RealY = RealY + 1
    NEXT
    'print location bar
    FOR Y = 8 TO 8 + 13
        LOCATE Y, 75
        PRINT "°"
    NEXT
    LOCATE 8 + (CurrentY / DirectNum) * 13, 75
    PRINT "Û"
RETURN
'this sub controls the cursor when it is in the Files window
FileWindow:
    IF UseLast1 THEN
        Xpos = LastXPos1
        YPos = LastYPos1
    ELSE
        Xpos = 0
        YPos = 0
    END IF
    DO
        'highlight the cursors position
        COLOR 0, 7
        Refresh 6 + Xpos * 17, YPos + 9, 18
        COLOR 15, 0
       
        'get the filename under the cursor
        FileName$ = Window$(CurrentX + Xpos, YPos)
       
        'print the current file at tope
        LOCATE 3, 19
        PRINT STRING$(13, 32)
        LOCATE 3, 19
        PRINT FileName$

        'wait for a key
        A = GetKey
       
        'clear cursor
        Refresh 6 + Xpos * 17, YPos + 9, 18
       
        'process the key
        SELECT CASE A
            CASE Esc
                Status = 2
                RETURN
            CASE Home
                CurrentX = 0
                Xpos = 0: YPos = 0
                GOSUB Update1
            CASE EndKey
                CurrentX = LastX
                YPos = LastY
                Xpos = 0
                GOSUB Update1
            CASE TabKey
                'save the cursors position
                LastXPos1 = Xpos
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
                IF Xpos + CurrentX = LastX AND YPos > LastY THEN
                    YPos = LastY
                END IF
                IF YPos > 11 THEN
                    YPos = 0
                    Xpos = Xpos + 1
                    IF Xpos > 2 THEN
                        Xpos = 2
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
                    IF Xpos + CurrentX = 0 THEN
                        YPos = 0
                    ELSE
                        YPos = 11
                        Xpos = Xpos - 1
                        IF Xpos < 0 THEN
                            Xpos = 0
                            CurrentX = CurrentX - 1
                            IF CurrentX < 0 THEN
                                CurrentX = 0
                                Xpos = 2
                            END IF
                            GOSUB Update1
                        END IF
                    END IF
                END IF
            CASE LeftArrow
                Xpos = Xpos - 1
                IF Xpos < 0 THEN
                    Xpos = 0
                    IF Xpos + CurrentX = 0 THEN
                        YPos = 0
                    ELSE
                        CurrentX = CurrentX - 1
                        GOSUB Update1
                    END IF
                END IF
            CASE RightArrow
                Xpos = Xpos + 1
                IF Xpos + CurrentX > LastX THEN
                    Xpos = LastX - CurrentX
                    YPos = LastY
                END IF
                IF Xpos + CurrentX = LastX AND YPos > LastY THEN
                    YPos = LastY
                END IF
                IF Xpos > 2 THEN
                    Xpos = 2
                    CurrentX = CurrentX + 1
                    IF Xpos + CurrentX = LastX AND YPos > LastY THEN
                        YPos = LastY
                    END IF
                    GOSUB Update1
                END IF
            CASE 65 TO 90, 97 TO 122, 48 TO 57
                A$ = UCASE$(CHR$(A))
                RealX = CurrentX + Xpos
                RealY = CurrentY + YPos
                StopX = RealX
                StopY = RealY
                ScanX = RealX
                ScanY = RealY + 1
                IF ScanY > 11 THEN
                    ScanY = 0
                    ScanX = ScanX + 1
                END IF
                DO UNTIL ScanX > LastX OR LEFT$(Window$(ScanX, ScanY), 1) = A$
                    ScanY = ScanY + 1
                    IF ScanY > 11 THEN
                        ScanY = 0
                        ScanX = ScanX + 1
                    END IF
                LOOP
                IF NOT ScanX > LastX THEN
                    Xpos = 0
                    YPos = ScanY
                    CurrentX = ScanX
                    GOSUB Update1
                ELSE
                    ScanX = 0
                    ScanY = 0
                    DO UNTIL (ScanX = StopX AND ScanY = StopY) OR LEFT$(Window$(ScanX, ScanY), 1) = A$
                        ScanY = ScanY + 1
                        IF ScanY > 11 THEN
                            ScanY = 0
                            ScanX = ScanX + 1
                        END IF
                    LOOP
                    IF NOT (ScanX = StopX AND ScanY = StopY) THEN
                        Xpos = 0
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
        COLOR 0, 7
        Refresh 61, YPos + 9, 14
        'wait for a key
        A = GetKey
       
        'unhighlight the cursors position
        COLOR 15, 0
        Refresh 61, YPos + 9, 14
       
        'process key
        SELECT CASE A
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
                    PCOPY 1, 2
                    NewDrive$ = MID$(NewDirect$, 3, 1)
                    ErrorStatus = 0
                   
                    'check drive to see if it is ready
                    ON ERROR GOTO ErrorHandler
                    CHDIR NewDrive$ + ":\"
                    ON ERROR GOTO 0
                   
                    PCOPY 2, 1
                    IF ErrorStatus <> 0 THEN
                        SOUND 1000, 3
                        LOCATE 25, 34
                        COLOR 15 + 16
                        PRINT "Drive Error";
                        COLOR 15
                    ELSE
                        LOCATE 25, 34
                        PRINT STRING$(11, 32);
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
                A$ = UCASE$(CHR$(A))
                StopScan = YPos + CurrentY
                Scan = StopScan + 1
                DO UNTIL Scan > DirectNum OR LEFT$(Direct$(Scan), 1) = A$
                    Scan = Scan + 1
                LOOP
                IF NOT Scan > DirectNum THEN
                    YPos = 0
                    CurrentY = Scan
                    GOSUB Update2
                ELSE
                    Scan = 0
                    DO UNTIL Scan = StopScan OR LEFT$(Direct$(Scan), 1) = A$
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
SUB Sort (A$(), Low, High)

   IF Low < High THEN
      IF High - Low = 1 THEN
         IF A$(Low) > A$(High) THEN
            SWAP A$(Low), A$(High)
         END IF
      ELSE

         RandIndex = RandInt(Low, High)
         SWAP A$(High), A$(RandIndex)
         Partition$ = A$(High)
         DO

            I = Low: J = High
            DO WHILE (I < J) AND (A$(I) <= Partition$)
               I = I + 1
            LOOP
            DO WHILE (J > I) AND (A$(J) >= Partition$)
               J = J - 1
            LOOP

            IF I < J THEN
               SWAP A$(I), A$(J)
            END IF
         LOOP WHILE I < J

         SWAP A$(I), A$(High)

         IF (I - Low) < (High - I) THEN
            Sort A$(), Low, I - 1
            Sort A$(), I + 1, High
         ELSE
            Sort A$(), I + 1, High
            Sort A$(), Low, I - 1
         END IF
      END IF
   END IF
END SUB

