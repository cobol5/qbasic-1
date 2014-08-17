DECLARE FUNCTION FileOpen% (File$, Handle%)
DECLARE FUNCTION FileClose% (BYVAL Handle%)
DECLARE FUNCTION FileMDateTimeGet% (BYVAL Handle%, Years%, Months%, Days%, Hours%, Minutes%, Seconds%)
DECLARE FUNCTION FileMDateTimeSet% (BYVAL Handle%, BYVAL Years%, BYVAL Months%, BYVAL Days%, BYVAL Hours%, BYVAL Minutes%, BYVAL Seconds%)
DECLARE FUNCTION FileADateTimeGet% (BYVAL Handle%, Years%, Months%, Days%)
DECLARE FUNCTION FileADateTimeSet% (BYVAL Handle%, BYVAL Years%, BYVAL Months%, BYVAL Days%)


TYPE RegTypeX
  ax    AS INTEGER
  bx    AS INTEGER
  cx    AS INTEGER
  dx    AS INTEGER
  bp    AS INTEGER
  si    AS INTEGER
  di    AS INTEGER
  Flags AS INTEGER
  ds    AS INTEGER
  es    AS INTEGER
END TYPE


DIM SHARED Regs AS RegTypeX


CLS

File$ = "fileattr.bas"
PRINT File$
PRINT


  IF NOT (FileOpen%(File$, Handle%)) THEN
  PRINT "Error opening file"
  END
  END IF

  IF FileMDateTimeGet%(Handle%, Years%, Months%, Days%, Hours%, Minutes%, Seconds%) THEN
  PRINT "File's creation date  (DD/MM/YYYY):"
  PRINT Days%; "/"; Months%; "/"; Years%
  PRINT "File's creation time  (HH:MM:SS):"
  PRINT Hours%; ":"; Minutes%; ":"; Seconds%
  END IF

  IF FileADateTimeGet%(Handle%, Years%, Months%, Days%) THEN
  PRINT
  PRINT "File's access date  (DD/MM/YYYY):"
  PRINT Days%; "/"; Months%; "/"; Years%
  END IF

Temp% = FileClose%(ThisHandle%)

'Function returns:   -1 if successful
'                     0 if not successful
'
FUNCTION FileADateTimeGet% (BYVAL Handle%, Years%, Months%, Days%)

Regs.ax = &H5704
Regs.bx = Handle%

CALL INTERRUPTX(&H21, Regs, Regs)

  IF Regs.Flags AND 1 THEN
    EXIT FUNCTION
  ELSE
    FileADateTimeGet% = -1
  END IF

Years% = ((Regs.dx AND &HFE00&) \ 512) + 1980
Months% = (Regs.dx AND &H1E0&) \ 32
Days% = Regs.dx AND &H1F&

END FUNCTION

'Function returns:   -1 if successful
'                     0 if not successful
'
FUNCTION FileADateTimeSet% (BYVAL Handle%, BYVAL Years%, BYVAL Months%, BYVAL Days%)

Regs.ax = &H5705
Regs.bx = Handle%
Regs.cx = 0
Regs.dx = (Years% - 1980) * 512 + Months% * 32 + Days%

CALL INTERRUPTX(&H21, Regs, Regs)

  IF (Regs.Flags AND 1) = 0 THEN
  FileADateTimeSet% = -1
  END IF

END FUNCTION

'Function returns:   -1 if successful
'                     0 if not successful
'
FUNCTION FileClose% (BYVAL Handle%)

Regs.ax = &H3E00
Regs.bx = Handle%

CALL INTERRUPTX(&H21, Regs, Regs)

  IF (Regs.Flags AND 1) = 0 THEN
  FileClose% = -1
  END IF

END FUNCTION

'Function returns:   -1 if successful
'                     0 if not successful
'
FUNCTION FileMDateTimeGet% (BYVAL Handle%, Years%, Months%, Days%, Hours%, Minutes%, Seconds%)

Regs.ax = &H5700
Regs.bx = Handle%

CALL INTERRUPTX(&H21, Regs, Regs)

  IF Regs.Flags AND 1 THEN
    EXIT FUNCTION
  ELSE
    FileMDateTimeGet% = -1
  END IF

Years% = ((Regs.dx AND &HFE00&) \ 512) + 1980
Months% = (Regs.dx AND &H1E0&) \ 32
Days% = Regs.dx AND &H1F&
Hours% = (Regs.cx AND &HF800&) \ 2048
Minutes% = (Regs.cx AND &H7E0&) \ 32
Seconds% = (Regs.cx AND &H1F&) * 2

END FUNCTION

'Function returns:   -1 if successful
'                     0 if not successful
'
FUNCTION FileMDateTimeSet% (BYVAL Handle%, BYVAL Years%, BYVAL Months%, BYVAL Days%, BYVAL Hours%, BYVAL Minutes%, BYVAL Seconds%)

Regs.ax = &H5701
Regs.bx = Handle%
Regs.cx = Hours% * 2048 + Minutes% * 32 + Seconds% \ 2
Regs.dx = (Years% - 1980) * 512 + Months% * 32 + Days%

CALL INTERRUPTX(&H21, Regs, Regs)

  IF (Regs.Flags AND 1) = 0 THEN
  FileMDateTimeSet% = -1
  END IF

END FUNCTION

'Function returns:   -1 if successful, file handle in Handle%
'                     0 if not successful, error code in Handle%
'
FUNCTION FileOpen% (File$, Handle%)

'INT 21 - DOS 2+ - "OPEN" - OPEN EXISTING FILE
'        AH = 3Dh
'        AL = access and sharing modes (see #01402)
'        DS:DX -> ASCIZ filename
'        CL = attribute mask of files to look for (server call only)
'Return: CF clear if successful
'            AX = file handle
'        CF set on error
'            AX = error code (01h,02h,03h,04h,05h,0Ch,56h) (see #01680 at AH=59h)
'               0 = OK
'               2 = File not found
'               3 = Path not found
'               4 = No more handles available
'               5 = Access denied
'               0Ch = Invalid access code
'
'Notes:  file pointer is set to start of file
'        if SHARE or a network is loaded, the file open may fail if the file
'          is already open, depending on the combination of sharing modes
'          (see #01403,#01404)
'        file handles which are inherited from a parent also inherit sharing
'          and access restrictions
'        files may be opened even if given the hidden or system attributes
'        under the FlashTek X-32 DOS extender, the pointer is in DS:EDX
'        DR DOS checks the system password or explicitly supplied password at
'          the end of the filename (following a semicolon) against the reserved
'          field in the directory entry before allowing access
'        sharing modes are only effective on local drives if SHARE is loaded
'
'Bitfields for access and sharing modes:
'Bit(s)  Description     (Table 01402)
' 2-0    access mode
'        000 READ only
'        001 WRITE only
'        010 read/write
'        011 (DOS 5+ internal) passed to redirector on EXEC to allow
'                                            case-sensitive filenames
' 3      reserved (0)
' 6-4    sharing mode (DOS 3.0+) (see #01403)
'        000 compatibility mode
'        001 "DENYALL" prohibit both read and write access by others
'        010 "DENYWRITE" prohibit write access by others
'        011 "DENYREAD" prohibit read access by others
'        100 "DENYNONE" allow full access by others
'        111 network FCB (only available during server call)
' 7      inheritance
'        if set, file is private to current process and will not be inherited
'         by child processes



File0$ = File$ + CHR$(0)

Regs.ax = &H3D00
Regs.cx = 0
Regs.ds = SSEG(File0$)
Regs.dx = SADD(File0$)

CALL INTERRUPTX(&H21, Regs, Regs)

Handle% = Regs.ax


  IF (Regs.Flags AND 1) = 0 THEN
  FileOpen% = -1
  END IF

END FUNCTION

