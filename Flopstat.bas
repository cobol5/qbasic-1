'======================================================================
' Quick Basic Forum
'   Date : 17-Jul-91
'   From : Richard Attenborough
'Subject : Floppy Disk Status Check

'I have been playing around with a way to check the status of a floppy
'disk without crashing my machine. I believe that I have created a
'routine to do this but I would appreciate it if those of you out there
'who know more about this than I do would check it over. I got the
'information about the interrupt used from the DOS PROGRAMMERS REFERENCE
'2ND EDITION (HIGHLY recommended, it even has some QB source code in
'it!).
'========================================================================


DECLARE FUNCTION CheckFloppyStatus% (DriveNum%)
'$INCLUDE: 'qb.bi'
CLS
x = CheckFloppyStatus%(0)

PRINT x
END

FUNCTION CheckFloppyStatus% (DriveNum%)
'Interrupt 13h  Function 02h  Read Disk Sectors
'calling registers AH    = 02h (sets function)
'                  AL    = number of sectors to transfer
'                  ES:BX = Pointer to User's disk Buffer
'                  CH    = Track number
'                  CL    = Sector Number
'                  DH    = Head Number
'                  DL    = Drive Number (set bit 7 if checking the HD)
'Return Registers
'               Carry flag is CLEAR if successful
'                  AH    = 0
'                  AL    = Number of Sectors Transferred
'
'               Carry Flag is SET if ERROR
'                  AH    = Status Byte (see Error Table Below) 
'Note: When using this function with a  Hard Disk, the track number is 10
'      rather than 8. The upper two bits are passed to the function in the
'      high two bits of the CL register.
'
'       STATUS BITS CHART                                              error
'                                                                       code
'   bit          meaning                                               ------
' 76543210                                                     decimal
' .......1  Illegal command to driver                          =   1 =>   1
' ......1.  Address mark not located (bad sector)              =   2 =>   2
' ......11  Write Protected Disk                               =   3 =>   3
' .....1..  Requested sector not found                         =   4 =>   4
' .....11.  Diskette Change line active                        =   6 =>   5
' ....1...  DMA overrun                                        =   8 =>   6
' ....1..1  DMA attempt across 64k boundary                    =   9 =>   7
' ....11..  Invalid Media                                      =  12 =>   8
' ...1....  CRC error on disk read                             =  16 =>   9
' ..1.....  Controller Error                                   =  32 =>  10
' .1......  Seek Failure                                       =  64 =>  11
' 1.......  Disk Time out (failure to respond, drive not ready)=-128 =>  12
'
'
' calling convention    FDrivErr% = CheckFloppyStatus% (0)
'
' Routine written by Richard Attenborough, July 1991.
' Placed into the Public Domain, July 1991.
' I make no claim that this routine will do ANYTHING! I believe that it works
' but if you have problems with it then I can't help you.

DIM InRegs AS RegTypeX
DIM OutRegs AS RegTypeX

JunkVar$ = SPACE$(1024)


IF DriveNum% < 0 OR DriveNum% > 1 THEN
   ' I have only created this routine for floppy disk use so I saw no need
   ' to allow drive letters other than A and B. Remember Hard Drive 0 = 128
   ' asking for status of an invalid drive so return invalid drive code..-1
   CheckFloppyStatus% = -1

END IF

InRegs.AX = &H2 * 256 + 0      'function 02 plus read zero sectors of data

InRegs.DX = &H0 * 256 + DriveNum%
                               'Head zero+Drive #0 or 1 (floppy drive A or B)

InRegs.CX = &H1 * 256 + 1      'Set track and Sector to 0

InRegs.BX = SADD(JunkVar$)     'offset  Address of JunkVar (disk Buffer)
InRegs.ES = VARSEG(JunkVar$)   'Segment Address of JunkVar

CALL InterruptX(&H13, InRegs, OutRegs)

CheckFloppyStatus% = 0

IF (OutRegs.FLAGS AND 1) = 1 THEN  ' checks CARRY flag

   AHREG = INT(OutRegs.AX / 256) 'set the value of AHREG to be the AH Register

   SELECT CASE AHREG  ' set the errorcode based on AHREG

          CASE IS < 0
              ' Disk Time Out (failure to respond, drive not ready)
              CheckFloppyStatus% = 12

          CASE 1, 2, 3, 4
              CheckFloppyStatus% = AHREG
    
          CASE 6
              CheckFloppyStatus% = 5
      
          CASE 8
              CheckFloppyStatus% = 6

          CASE 9
              CheckFloppyStatus% = 7

          CASE 12
              CheckFloppyStatus% = 8

          CASE 16
              CheckFloppyStatus% = 9

          CASE 32
              CheckFloppyStatus% = 10

          CASE 64
              CheckFloppyStatus% = 11

          CASE ELSE
              PRINT "panic, a new and unknown error code has been detected"
              PRINT AHREG; " is the errorcode"
        END SELECT

END IF

PRINT "# of sectors read="; OutRegs.AX - (AHREG * 256)
PRINT JunkVar$
PRINT LEN(JunkVar$)

END FUNCTION

