'BASTerm.BAS Version 1.2d
'A terminal example program in QBASIC (Freeware)
'Copyright (C)1995-1996 Mark Kim
'* Supports basic I/O from/to modem.
'* Supports only COM1 and COM2
'* ANSI support (with ANSI.SYS)
'* No upload or download support in any mode.
'* No guarentee, warentee, or any type of -rentees whatsoever.  Use at your
'  own risk.
'* No distribution after modification.  Parts of this file may be used in
'  others' programs provided that credit is given in proper place(s).  Must
'  include in the credit where the original file (this) can be found.
'Comments via Internet E-Mail to: markkkim@aol.com
'Older versions of the program is available.  Contact the author for info.
'RUN SETUP FROM THE MENU FIRST!!!

'Function/Sub declarations
DECLARE SUB opencom ()     'opens com port for communication
DECLARE FUNCTION menu% ()  'displays menu and accepts&returns input
DECLARE SUB waitforcall () 'waits for a call and connects when modem rings
DECLARE SUB callbbs ()     'calls a place and connects
DECLARE SUB setmodemoption ()  'sets modem option and saves it to a file
DECLARE SUB endprog ()     'ends the program
DECLARE SUB connect ()     'connects and handles output/input to/from modem

'Change this value to other values to change the key that cuts communication
CONST HALT = 27    'ASCII value for Escape key

CLS
COLOR 6
DO    'loop until user tells the program to end program
  item% = menu%    'run menu function and store return value to item integer
  SELECT CASE item%      'run subs according to the option chosen
    CASE 1: waitforcall
    CASE 2: callbbs
    CASE 3: setmodemoption
    CASE 4: endprog
  END SELECT
LOOP UNTIL item% = 4     '4 is the value that ends the loop
SYSTEM                   'exit to DOS, if it was called directly from DOS

'VERSION HISTORY:
' Version 1.0a:
' Version 1.0b:
'   Fixed: Now setmodemoption sub creates CFG instead of INI.
'   Updated: More efficient/modifiable input-from-modem process.
' Version 1.1a:
'   Fixed: Some spelling/grammar errors in comments
'   Updated: Option to print the outgoing data to screen, a process
'     needed when connecting to other terminal programs (not BBS program)
'   (never released)
' Version 1.2a:
'   Fixed: Forgot to add local echo option -- oops!  While I'm at it,
'     options menu added.
'   Updated: ANSI support.  Requires ANSI.SYS to be loaded.  Special thanks
'     to Ariel R. Benzakein (He's a guy!) from AOL for the ANSI tip.
'   Bug: Because the program uses PRINT and PRINT to CON, the alignment of
'     the screen does not look that great.  Oh well.
' Version 1.2b:
'   Fixed: Some unnecessary commentted lines of programming were deleted
'   Fixed: "Local echo enabled" message changed to "Local echo toggled"
' Version 1.2c:
'   Fixed: "END IF without IF" error statement corrected
' Version 1.2d:
'   Fixed: Bug with not printing anything to the screen was fixed (oops!)
'   Updated: ASCII 254 "remarks start indicators" changed to asterisk

'A sub that calls another computer via modem...
'I just call it callbbs because I am testing it on a BBS
SUB callbbs
  COLOR 3
  INPUT "Number to call: ", number$
  opencom  'a sub that opens the com port for communication
  PRINT #1, "ATDT" + number$  'call the number in tone (not pulse)
  PRINT "Calling " + number$ + "...."
  connect  'a sub to processes data and sends/receives it via modem
END SUB

'A sub that processes and exchanges data between the modems
SUB connect
  echo% = 0                   'local echo.  0 = off, 1 = on
  PRINT "Start Typing when modems connect... Press <Esc> for options or to quit:"
  PRINT
  OPEN "CON" FOR OUTPUT AS #3 'open screen for output (to get ANSI support)
  DO
    'User enters stuff
    ch$ = INKEY$              'get a character from keyboard, if any is typed
    IF ch$ = CHR$(HALT) THEN  'if ESC pressed
      PRINT "====== OPTIONS ======="
      PRINT "1. Local echo on/off  "
      PRINT "2. Send HALT character (<ESC> by default)"
      PRINT "3. Close port         "
      PRINT "4. Return to terminal "
      SELECT CASE INPUT$(1)   'input a character and select case at same time
        CASE "1":
          echo% = echo% XOR 1            'toggle echo% between 0 and 1
          PRINT "Local echo toggled. Returning to terminal...."
        CASE "2":
          PRINT #1, CHR$(HALT)           'send #1 the HALT character (ESC)
          PRINT "HALT character sent. Returning to terminal...."
        CASE "3": EXIT DO                'exitting DO closes port
        CASE "4":                        'acknowledges pressing 4, but does X
        CASE ELSE:
          PRINT "Invalid option! Returning to terminal...."
      END SELECT
    ELSEIF ch$ <> "" THEN PRINT #1, ch$; 'if something is typed send to modem
    END IF
    IF echo% = 1 THEN         'if local echo enabled
      PRINT #3, ch$;          'print to monitor using ANSI.SYS, if avail
    END IF
    'Messages received
    'input message:
    ' LOC(1) is zero if nothing is in the buffer.  Otherwise it tells
    ' how big the incoming data is.  So if there is anything in the buffer
    ' input 1 character, the line says:
    IF LOC(1) <> 0 THEN
      inchar$ = INPUT$(1, #1)
    ELSE
      inchar$ = ""
    END IF
    PRINT #3, inchar$;    'print the character (using ANSI.SYS if avail)
  LOOP
  CLOSE #1   'close the file once done
  CLOSE #3   'close the screen of ANSI support
END SUB

'This is how the program ends
SUB endprog
  COLOR 7
  PRINT "Ending Program."
END SUB

'Draws menu and returns the option the user typed
FUNCTION menu%
  DO    'a loop to draw the menu over and over until a valid option is typed
    COLOR 5
    PRINT "==== BASTerm Menu ===="
    PRINT " 1. Wait for a call  "
    PRINT " 2. Call somewhere    "
    PRINT " 3. Set Modem options "
    PRINT " 4. End Program       "
    PRINT "     Choose One: ";       'A semicolon is needed to have cursor
                                     'at the end instead of next line
    LOCATE , , 1: ch$ = INPUT$(1)  'LOCATE,,1 is to show the cursor...
                                   'the cursor is otherwise hidden during
                                   'INPUT$(1) operation
    PRINT : PRINT   'go to the next line then skip a line
  LOOP UNTIL VAL(ch$) >= 1 AND VAL(ch$) <= 4  'until 1,2,3, or 4 is entered
  menu% = VAL(ch$)  'returns the typed option to the function's caller
END FUNCTION

'Opens the comport for communication
SUB opencom
  PRINT "Retrieving COM port data.  Please hold."  'it didn't take long
                                                   'before but it does now!
  'open configuration file to input comport data:
  OPEN "BASTerm.CFG" FOR INPUT AS #2    'open config file for data input
  INPUT #2, comport$    'input the line
  CLOSE #2     'close file
  'open comport using data from the file:
  'RB2048 and TB2048 are the buffer sizes.  Bigger the buffer size the
  'more stable they are.  I think they maybe are used up as the data
  'comes in and out but I think maybe they are the sizes of the data
  'that can be held at once.  This can cause trouble if you Ctrl-Brk
  'during connection to fix parts of the program so make them pretty
  'big.
  OPEN comport$ + ",N,8,1,RB2048,TB2048" FOR RANDOM AS #1
END SUB

'Asks and saves new com port information
SUB setmodemoption
  COLOR 2
  INPUT "COM Port Number (1 or 2): ", portno$
  INPUT "Modem Speed (300, 1200, 2400, etc): ", speed$
  PRINT "Are The Settings Right? ";
  ch$ = INPUT$(1)
  IF ch$ = "Y" OR ch$ = "y" THEN    'if Y or y is entered
    PRINT "Yes"   'print Yes... something fancy here....
    comport$ = "COM" + portno$ + ":" + speed$  'put the data into a variable
    OPEN "BASTerm.CFG" FOR OUTPUT AS #2  'open the config file for output
    PRINT #2, comport$   'store data
    CLOSE #2    'close file
    PRINT "Settings saved."
  ELSE   'anything other than Y or y is entered
    PRINT "No"  'print No... something fancy here....
    PRINT "Disregarding New Settings...."
  END IF
  PRINT
END SUB

'A function that sets the modem to detect ringing and then connect
SUB waitforcall
  COLOR 3
  opencom       'open comport
  PRINT #1, "ATS0=1"    'ATS0=1 is used to connect after 1 ring.
  connect       'make communcation between modems available when the
                'connection is made
END SUB

