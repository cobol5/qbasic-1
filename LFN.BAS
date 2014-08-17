'===========================================================================
' Subject: LONG FILENAMES FROM DOS DEMO      Date: 04-18-98 (18:51)       
'  Author: Alex Warren                       Code: QB, PDS                
'  Origin: comp.lang.basic.misc            Packet: DOS.ABC
'===========================================================================
'Quite a few people have asked how long filenames are used from DOS in the last
'few months - below is some DOS BASIC code that will show you how! It's released
'into the public domain so feel free to use for whatever purpose you see fit.

'The program doesn't show you *everything* you can do with long filenames but it
'should be useful enough that you can modify it fairly easily with information
'from Ralf Brown's Interrupts List (address in the program).

'If you're lucky (actually, if *I* have the time) I'll write an article to
'explain the program (though that might not be necessary, the comments should
'explain all you need to know).

'I claim no responsibility for anything this program does - or doesn't - do.

'The code requires QuickBasic 4.5 or better (maybe worse, I dunno) - make sure
'you start it with /L
'---


' LONG FILENAMES FROM DOS DEMONSTRATION v1.0
' by Alex Warren, 18th April 1998
' Released into the Public Domain

' This program is intended to show you how to use the long filename
' functions. It's not particularly neat but it should be good enough to
' show you how to incorporate long filenames in your applications without
' having to use the incredibly messy method of using SHELL.

' If you want to save or open files with long file names, you could save
' using a short filename using OPEN and then rename the file using the
' rename interrupt (int 21h, service 7156h - check Ralf Brown's Interrupts
' List at www.ctyme.com/rbrown.htm), open by retrieving the equivalent
' short filename (see the showdirectory sub)

DECLARE SUB setdirectory ()
DECLARE SUB getfileattribs ()
DECLARE SUB showdirectory ()
DECLARE SUB createfile ()
DECLARE SUB createdirectory ()
DECLARE SUB getvolumeinfo (drive$)
DECLARE FUNCTION truncate$ (s$)
DECLARE FUNCTION getcurrentdir$ ()
DECLARE SUB int21 ()
'$INCLUDE: 'qb.bi'
DIM SHARED inregs AS RegTypeX, outregs AS RegTypeX

DIM SHARED filesystem$, casesensitive%, preservecase%, unicode%, lfnfunctions%, compressed%

CLS

PRINT "Long Filenames from DOS Demonstration, by Alex Warren"
PRINT "version 1.0, 18th April 1998"
PRINT "Released into the Public Domain"
PRINT

getvolumeinfo ("C:\")

PRINT "File system is: "; filesystem$
PRINT
PRINT "Searches are case sensitive:"; casesensitive%
PRINT "Preserves case in directory entries:"; preservecase%
PRINT "Uses unicode chars in file and dir names:"; unicode%
PRINT "Supports DOS LFN functions:"; lfnfunctions%
PRINT "Compressed volume:"; compressed%
DO
PRINT "---"
PRINT
PRINT "Current directory is "; getcurrentdir$
PRINT
PRINT "Choose:"
PRINT "1: Create directory; 2: Set current directory; 3: Show directory;"
PRINT "4: Create file; 5: Change drive; 6: Exit"

' Not included but worth investigating are:
' - Remove directory (service 713Ah)
' - Delete file (7141h)
' - Move/Rename file (7156h)
' - Get/Set file attributes (7143h)
' +++ others on Ralf Brown's Interrupts List

DO: a$ = INPUT$(1): LOOP UNTIL a$ >= "1" AND a$ <= "6"
selected = VAL(a$)

SELECT CASE selected
	CASE 1
		createdirectory
	CASE 2
		setdirectory
	CASE 3
		showdirectory
	CASE 4
		createfile
	CASE 5
		INPUT "Enter drive letter: ", driveletter$
		driveletter$ = driveletter$ + ":"
		SHELL driveletter$
	CASE 6
		END
END SELECT

LOOP

SUB createdirectory

DIM pathname AS STRING * 255

INPUT "Enter FULL path of sub-directory to create: "; p$
pathname = p$

' inregs:
' .ax: service - 7139h
' .ds: segment of pathname
' .dx: offset of pathname

inregs.ax = &H7139
inregs.ds = VARSEG(pathname)
inregs.dx = VARPTR(pathname)
int21

END SUB

SUB createfile
END SUB

FUNCTION getcurrentdir$

DIM thedir AS STRING * 255

' inregs:
' .ax: service - 7147h
' .dx: 0=default, 1=A:, 2=B:, 3=C:, 4=D: etc.
' .ds: segment of buffer
' .si: offset of buffer

inregs.dx = &H0
inregs.ax = &H7147
inregs.ds = VARSEG(thedir)
inregs.si = VARPTR(thedir)
int21

' This line truncates thedir up to the first null character:
d$ = truncate$(thedir)

' This line adds the backslash if not present, as the backslash may or may
' not be present already. If you don't want the backslash there ever, modify
' this line:
IF RIGHT$(d$, 1) <> "\" THEN d$ = d$ + "\"

getcurrentdir$ = d$

END FUNCTION

SUB getvolumeinfo (drive$)

' inregs:
' .ax: service - 71A0h
' .ds: segment of rootname
' .dx: offset of rootname
' .es: segment of buffer for file system name
' .di: offset of buffer for file system name
' .cx: size of file system name buffer

DIM rootname AS STRING * 4
DIM filesysname AS STRING * 255
rootname = drive$ + CHR$(0)

inregs.ax = &H71A0
inregs.ds = VARSEG(rootname)
inregs.dx = VARPTR(rootname)
inregs.es = VARSEG(filesysname)
inregs.di = VARPTR(filesysname)
inregs.cx = 255
int21

IF outregs.ax = &H7100 THEN
	BEEP
	PRINT "LFN functions are not supported; exiting program..."
	END
END IF

filesystem$ = truncate$(filesysname)
IF outregs.bx AND 2 ^ 0 THEN casesensitive% = 1 ELSE casesensitive% = 0
IF outregs.bx AND 2 ^ 1 THEN preservecase% = 1 ELSE preservecase% = 0
IF outregs.bx AND 2 ^ 2 THEN unicode% = 1 ELSE unicode% = 0
IF outregs.bx AND 2 ^ 14 THEN lfnfunctions% = 1 ELSE lfnfunctions% = 0
IF outregs.bx AND 2 ^ 15 THEN compressed% = 1 ELSE compressed% = 0

END SUB

SUB int21

CALL INTERRUPTX(&H21, inregs, outregs)

END SUB

SUB setdirectory

DIM pathname AS STRING * 255

INPUT "Enter FULL path of sub-directory: "; p$
pathname = p$

' inregs:
' .ax: service - 713Bh
' .ds: segment of pathname
' .dx: offset of pathname

inregs.ax = &H713B
inregs.ds = VARSEG(pathname)
inregs.dx = VARPTR(pathname)
int21


END SUB

SUB showdirectory

quitloop = 0

' What's done:
' first, call 714Eh to find first matching file, then call 714Fh to find
' next matching file and so on. Use 71A1h to stop the search.

' Attributes masks:
' Bit: 4 = directory
'      3 = volume label
'      2 = system
'      1 = hidden
'      0 = read-only

' ax=&h714E (service)
' ds:dx = filespec, e.g. *.*
' es:di = finddata record (see below)
' cl = allowable attributes mask, set here to FF so ALL attributes allowed
' ch = required attributes mask, set here to 00 so NO attributes required

DIM finddatarecord AS STRING * 500
DIM filespec AS STRING * 4

filespec = "*.*" + CHR$(0)

inregs.ax = &H714E
inregs.ds = VARSEG(filespec)
inregs.dx = VARPTR(filespec)
inregs.es = VARSEG(finddatarecord)
inregs.di = VARPTR(finddatarecord)
inregs.cx = &HFF
inregs.si = &H0
int21
filefindhandle = outregs.ax

numfiles = 1

DO

' finddatarecord$ is now filled with loads of information about the file
' we've retrieved (info from Ralf Brown's Interrupts List):
' offset 00h bits 0-6 are standard DOS file attribs
' 04h QWORD file creation time
' 0Ch QWORD last access time
' 14h QWORD last modification time
' 1Ch DWORD file size (high 32 bits)
' 20h DWORD file size (low 32 bits)
' 2Ch 260bytes ASCIZ full filename
' 130h 14bytes ASCIZ short filename

' If required, you could interpret all that stuff but we're only interested
' in the filenames for now:

longfilename$ = truncate$(MID$(finddatarecord, &H2D, 260))
shortfilename$ = truncate$(MID$(finddatarecord, &H131, 14))

' NOTE that shortfilename$ will contain nothing if the short filename
' is *exactly* the same as the long file name.

PRINT longfilename$; TAB(30); shortfilename$

' If longfilename$ is null, we've reached the end of the list.
IF longfilename$ = "" THEN PRINT "*** END OF LIST; PRESS A KEY ***": EXIT DO


' reset finddatarecord
finddatarecord = STRING$(500, 0)

inregs.ax = &H714F
inregs.bx = filefindhandle
int21

IF numfiles MOD 20 = 0 THEN
	COLOR 14
	PRINT ">> Press a key <<"
	COLOR 7
	a$ = INPUT$(1)
	IF a$ = CHR$(27) THEN quitloop = 1
END IF

numfiles = numfiles + 1

LOOP UNTIL quitloop = 1

IF quitloop = 0 THEN a$ = INPUT$(1)

' It's important to reset the filefind handle:
inregs.ax = &H71A1
inregs.bx = filefindhandle
int21

END SUB

FUNCTION truncate$ (s$)

' Truncates s$ to first null character

truncate$ = LEFT$(s$, INSTR(s$, CHR$(0)) - 1)
END FUNCTION
