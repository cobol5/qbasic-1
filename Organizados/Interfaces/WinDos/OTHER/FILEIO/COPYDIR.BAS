'Copyright (C) HiTech Labs, Denis Andrianov
'Copy Directory v1.0
'============================================================================

'$INCLUDE: 'windos.bi'

DECLARE FUNCTION CopyFile% (Source$, Destination$)
DECLARE FUNCTION CopyDir% (Source$, Destination$)
DECLARE FUNCTION ReadDir% (DirSpec$, BYVAL SearchType%)
DECLARE FUNCTION AddSlash$ (Strng$)

CONST FindFirst = &H4E00        'Functions of interrupt 21h.
CONST FindNext = &H4F00

DIM SHARED Regs AS RegTypeX
DIM SHARED DTA AS DTAStructure

'============================================================================

OldTime! = TIMER
  IF CopyDir%("d:\program\basprog\windos", "c:\mycop\windos") THEN
    PRINT "Success"
  ELSE
    PRINT "Failed"
  END IF
PRINT "Done in "; TIMER - OldTime!; " seconds."

FUNCTION AddSlash$ (Strng$)

  IF RIGHT$(Strng$, 1) <> "\" THEN
    AddSlash$ = Strng$ + "\"
  ELSE
    AddSlash$ = Strng$
  END IF

END FUNCTION

'The function returns TRUE if successfull.
'
FUNCTION CopyDir% (Source$, Destination$)

CopyDir% = -1
CopyDirSuccess% = -1

STATIC TempMask$
TempMask$ = "*.*"

'============================================================================
ON LOCAL ERROR GOTO MkChDirError
MKDIR Destination$
ON LOCAL ERROR GOTO 0

CopyDirStart:

  IF NOT CopyDirSuccess% THEN
  CopyDir% = 0
  EXIT FUNCTION
  END IF

ON LOCAL ERROR GOTO MkChDirError
CHDIR Source$
CHDRIVE Source$
ON LOCAL ERROR GOTO 0
  
'----------------------------------------------------------------------------
  IF ReadDir(TempMask$, FindFirst) THEN

    FOR Counter% = 1 TO FileDirCount%
      IF NOT ReadDir(TempMask$, FindNext) THEN EXIT FUNCTION
    NEXT

    DO
    Filename$ = UCASE$(RTRIM$(DTA.Filename))
    SourceFile$ = AddSlash$(Source$) + Filename$
    DestFile$ = AddSlash$(Destination$) + Filename$
    FileDirCount% = FileDirCount% + 1

      IF ASC(DTA.Attributes) AND 16 THEN               'This is a directory.
          IF LEFT$(Filename$, 1) <> "." THEN
          PRINT SourceFile$
          CopyDirSuccess% = CopyDir%(SourceFile$, DestFile$)
          GOTO CopyDirStart
          END IF
      ELSE                                             'This is a file.
          IF NOT CopyFile%(SourceFile$, DestFile$) THEN
          CopyDirSuccess% = 0
          GOTO CopyDirStart
          END IF
      END IF

      IF NOT ReadDir(TempMask$, FindNext) THEN
      EXIT FUNCTION
      END IF

    LOOP

  END IF


CopyDirEnd:
EXIT FUNCTION

'============================================================================
'============================================================================
MkChDirError:
CopyDir% = 0
RESUME CopyDirEnd


END FUNCTION

'Source$ file MUST exist, otherwise it would be created!
'The function returns TRUE if successfull.
'
FUNCTION CopyFile% (Source$, Destination$)

CONST MaxBuffSize = 32256
CopyFile% = -1

'============================================================================
File1% = FREEFILE

ON LOCAL ERROR GOTO OpenFileError1
OPEN Source$ FOR BINARY AS #File1%
ON LOCAL ERROR GOTO 0

Remaining& = LOF(File1%)
File2% = FREEFILE

ON LOCAL ERROR GOTO OpenFileError2
OPEN Destination$ FOR BINARY AS #File2%
ON LOCAL ERROR GOTO 0


  DO

    IF Remaining& <= MaxBuffSize THEN
      Buffer$ = SPACE$(Remaining&)
      Remaining& = 0
    ELSE
      Buffer$ = SPACE$(MaxBuffSize)
      Remaining& = Remaining& - MaxBuffSize
    END IF

  ON LOCAL ERROR GOTO FileAccessError
  GET #File1%, , Buffer$
  PUT #File2%, , Buffer$
  ON LOCAL ERROR GOTO 0

  LOOP WHILE Remaining&


CLOSE #File1%, #File2%

'----------------------------------------------------------------------------
AfterFileErrors:

  IF ErrorCode% = 67 OR ErrorCode% = 61 THEN   '67=Too many files error
  ON LOCAL ERROR GOTO KillDestError            '61=Disk full error
  KILL Destination$
  ON LOCAL ERROR GOTO 0
  END IF

AfterKillDestError:
EXIT FUNCTION

'============================================================================
'============================================================================
FileAccessError:
CLOSE #File2%
ErrorCode% = ERR

OpenFileError2:
CLOSE #File1%

OpenFileError1:
CopyFile% = 0
RESUME AfterFileErrors

'----------------------------------------------------------------------------
KillDestError:
RESUME AfterKillDestError

END FUNCTION

FUNCTION ReadDir% (DirSpec$, BYVAL SearchType%)

  IF SearchType% <> FindFirst AND SearchType% <> FindNext THEN
  EXIT FUNCTION
  END IF

  IF SearchType% = FindFirst THEN
  'Setting up DTA...
  Regs.ax = &H1A00
  Regs.ds = VARSEG(DTA)
  Regs.dx = VARPTR(DTA)
  CALL InterruptX(&H21, Regs, Regs)
  END IF

'Finding FIRST or NEXT entries...
DirSpec$ = DirSpec$ + CHR$(0)
Regs.ax = SearchType%
Regs.cx = 55                      'Read all files!!!
Regs.ds = SSEG(DirSpec$)
Regs.dx = SADD(DirSpec$)
CALL InterruptX(&H21, Regs, Regs)

  IF Regs.flags AND 1 THEN
    ReadDir% = 0
  ELSE
    NullByte% = INSTR(DTA.Filename, CHR$(0))
      IF NullByte% > 0 THEN
      DTA.Filename = LEFT$(DTA.Filename, NullByte% - 1)
      END IF
    ReadDir% = -1
  END IF

END FUNCTION

