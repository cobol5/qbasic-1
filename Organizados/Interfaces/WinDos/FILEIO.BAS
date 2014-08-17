'Copyright (C) HiTech Labs, Denis Andrianov
'File I/O Library.
'============================================================================

'$INCLUDE: 'fileio.bi'
'$INCLUDE: 'graphics.bi'
'$INCLUDE: 'dos.bi'


DECLARE SUB LoadFont (BYVAL FontType%, Text$, BYVAL Textx%, BYVAL Texty%, BYVAL Colour%)
DECLARE FUNCTION AddSlash$ (Path$)
DECLARE SUB ExtractFileExt (File$, Extension$)


CONST FindFirst = &H4E00        'Functions of interrupt 21h.
CONST FindNext = &H4F00
CONST AllFilesAtt = 55          'Attribute for reading ALL files.
CONST DosScrFile = "dos.scr"
CONST AllFilesMask$ = "*.*"
CONST UpDirMask$ = ".."

CONST AbortCopyKey$ = ""       'ESC


DIM SHARED Regs AS RegTypeX     'All registers structure.
DIM SHARED DeleteReadOnly%
'255=display info box when read-only found
'  0=do not kill read-only files
' -1=kill read-only files


COMMON SHARED /FileIOSubs/ TempPath$           'Path to Temp directory.
COMMON SHARED /FileIOSubs/ DefaultDrive$       'Default drive.
COMMON SHARED /FileIOSubs/ DTA AS DTAStructure

'The function returns: -1 if successful.
'                       0 if not successful.
'
FUNCTION ChangeDirDrive% (Path$)

ChangeDirDrive% = -1

ON LOCAL ERROR GOTO ChDirDriveError
CHDIR Path$
CHDRIVE Path$
ON LOCAL ERROR GOTO 0

EXIT FUNCTION

'============================================================================

ChDirDriveError:
InfoBox 1, "Cannot access path on drive " + LEFT$(Path$, 1) + ". Retry?"
  IF CaptureInf(1) = 2 THEN
  Path$ = DefaultDrive$ + "\"
  ChangeDirDrive% = 0
  END IF
RESUME

END FUNCTION

'ChangeFileAtt% changes the file's DOS attribute.
'
'The function returns: -1 if successful.
'                       0 if not successful.
'
FUNCTION ChangeFileAtt% (File$, Attribute%)

Temp$ = File$ + CHR$(0)

Regs.ax = &H4301
Regs.cx = Attribute%
Regs.ds = SSEG(Temp$)
Regs.dx = SADD(Temp$)

ON LOCAL ERROR GOTO FileAttError
CALL INTERRUPTX(&H21, Regs, Regs)
ON LOCAL ERROR GOTO 0

  IF Regs.flags AND 1 THEN
    ChangeFileAtt% = 0
  ELSE
    ChangeFileAtt% = -1
  END IF


ChangeFileAttEnd:
EXIT FUNCTION

'============================================================================
FileAttError:
ChangeFileAtt% = 0
RESUME ChangeFileAttEnd

END FUNCTION

'This procedure may CHANGE THE CURRENT DIRECTORY!!!
'
'The function returns: -1 if successful.
'                       0 if not successful.
'
FUNCTION CopyDir% (Source$, Destination$)

CopyDir% = -1
CopyDirSuccess% = -1

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
  IF ReadDir(AllFilesMask$, FindFirst, AllFilesAtt) THEN

    FOR Counter% = 1 TO FileDirCount%
      IF NOT ReadDir(AllFilesMask$, FindNext, AllFilesAtt) THEN EXIT FUNCTION
    NEXT

    DO
    Filename$ = UCASE$(RTRIM$(DTA.Filename))
    SourceFile$ = AddSlash$(Source$) + Filename$
    DestFile$ = AddSlash$(Destination$) + Filename$
    FileDirCount% = FileDirCount% + 1

      IF ASC(DTA.Attributes) AND 16 THEN               'This is a directory.
        IF LEFT$(Filename$, 1) <> "." THEN
        CopyDirSuccess% = CopyDir%(SourceFile$, DestFile$)
        GOTO CopyDirStart
        END IF
      ELSE                                             'This is a file.
        IF NOT CopyFile%(SourceFile$, DestFile$) THEN
        CopyDirSuccess% = 0
        GOTO CopyDirStart
        END IF
      END IF

      IF NOT ReadDir(AllFilesMask$, FindNext, AllFilesAtt) THEN
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
'The function returns: -1 if successful.
'                       0 if not successful.
'
FUNCTION CopyFile% (Source$, Destination$)

STATIC Buffer$
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

    IF INKEY$ = AbortCopyKey$ THEN
    CopyFile% = 0
    EXIT DO
    END IF

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

SUB DosScr (BYVAL Save%)

ImageFile$ = AddSlash$(TempPath$) + DosScrFile

DEF SEG = &HB800

  SELECT CASE Save%
    CASE 1
      BSAVE ImageFile$, 0, &HFA0
    CASE ELSE
      SCREEN 0
      BLOAD ImageFile$
  END SELECT

DEF SEG

END SUB

SUB EmptyDir (Directory$)

ON LOCAL ERROR GOTO KillDirError
CHDIR Directory$
CHDRIVE Directory$
ON LOCAL ERROR GOTO 0

EmptyDirStart:
'----------------------------------------------------------------------------


  IF ReadDir(AllFilesMask$, FindFirst, AllFilesAtt) THEN

    DO
    Filename$ = UCASE$(RTRIM$(DTA.Filename))

      IF LEFT$(Filename$, 1) <> "." THEN

        IF ASC(DTA.Attributes) AND 16 THEN               'This is a directory.
          LowerDir$ = AddSlash$(Directory$) + Filename$
          EmptyDir LowerDir$
          ON LOCAL ERROR GOTO KillDirError
          CHDIR UpDirMask$
          RMDIR LowerDir$
          ON LOCAL ERROR GOTO 0
        ELSE                                             'This is a file.
          ON LOCAL ERROR GOTO KillFileError
          KILL Filename$
          ON LOCAL ERROR GOTO 0
        END IF
      GOTO EmptyDirStart

      ELSE

        IF NOT (ReadDir(AllFilesMask$, FindNext, AllFilesAtt)) THEN
        GOTO EmptyDirEnd
        END IF

      END IF

    LOOP

  END IF


EmptyDirEnd:
EXIT SUB

'============================================================================
'============================================================================
KillFileError:

  IF DeleteReadOnly% = 255 THEN
  InfoBox 1, "Delete read-only files inside directory?"
    IF CaptureInf%(1) = 1 THEN       'OK pressed.
      DeleteReadOnly% = -1
    ELSE                             'Cancel pressed.
      DeleteReadOnly% = 0
    END IF
  END IF

  IF DeleteReadOnly% THEN
    IF ChangeFileAtt(Filename$, 0) THEN
      RESUME
    ELSE
      RESUME EmptyDirEnd
    END IF
  END IF

RESUME EmptyDirEnd

'----------------------------------------------------------------------------
KillDirError:
RESUME EmptyDirEnd

END SUB

SUB FlushDOSBuffers

Regs.ax = &HD00
CALL Interrupt(&H21, Regs, Regs)

END SUB

'The function returns a string of all available drives.
'
FUNCTION GetDrives$

DEF SEG = 0
FloppyDiskNum% = PEEK(&H410) \ 64 + 1
DEF SEG

Drives$ = "A"

  IF FloppyDiskNum% = 2 THEN
  Drives$ = Drives$ + "B"
  END IF

  FOR BL% = 3 TO 26
    Regs.ax = &H4409
    Regs.bx = BL%
    CALL Interrupt(&H21, Regs, Regs)
      IF (Regs.flags AND 1) = 0 THEN
      Drives$ = Drives$ + CHR$(BL% + 64)
      END IF
  NEXT

GetDrives$ = Drives$

END FUNCTION

'If Directory%=TRUE, Directory$ contains the path where File$ (directory
'being deleted) is located.
'If Directory%=FALSE, Directory$ contains the location of the file.
'
'Function returns: -1 when successful and NO info boxes were shown.
'                   1 when successful, but info boxes WERE shown.
'                   0 when not successful.
'
FUNCTION KillFileDir% (File$, Extension$, Directory$, BYVAL Directory%)

' 0=Killing was canceled completely for ALL files.
'-1=Killing has not been canceled.
KillFileDir% = -1

'Directory with a backslash.
FullPath$ = AddSlash$(Directory$)


  IF Directory% THEN

'----------------------------------------------------------------------------
'Attempt to kill a directory.

  'Full location of directory.
  FullPath$ = FullPath$ + File$

  DeleteReadOnly% = 255          '"Delete read-only?" has not been asked yet.

  EmptyDir FullPath$

  ON LOCAL ERROR GOTO DirNotEmptyError
  CHDIR UpDirMask$
  RMDIR FullPath$
  ON LOCAL ERROR GOTO 0

    IF DeleteReadOnly% <> 255 THEN    'Were there any info boxes in EmptyDir?
    KillFileDir% = 1
    END IF

'----------------------------------------------------------------------------
'Attempt to kill a file.

  ELSE

  FileWithExt$ = File$ + "." + Extension$
  FullPath$ = FullPath$ + FileWithExt$

  ON LOCAL ERROR GOTO ReadOnlyFileError
  KILL FullPath$
  ON LOCAL ERROR GOTO 0

'----------------------------------------------------------------------------
  END IF

KillFileDirEnd:
EXIT FUNCTION

'============================================================================
'============================================================================
DirNotEmptyError:
InfoBox 6, "Unable to remove " + File$ + " directory."
Temp% = CaptureInf%(6)
KillFileDir% = 0
RESUME KillFileDirEnd

'----------------------------------------------------------------------------
ReadOnlyFileError:
KillFileDir% = 1
InfoBox 1, FileWithExt$ + " is read-only. Delete?"


  IF CaptureInf%(1) = 1 THEN                        'OK is pressed.

    IF ChangeFileAtt(FullPath$, 0) THEN
      RESUME
    ELSE
      InfoBox 1, "Cannot remove " + FileWithExt$ + ". Retry?"
        IF CaptureInf%(1) = 1 THEN                  'Try to remove again.
          RESUME
        ELSE
          KillFileDir% = 0
          RESUME KillFileDirEnd
        END IF
    END IF

  ELSE
    KillFileDir% = 0
    RESUME KillFileDirEnd
  END IF


END FUNCTION

'MoveDirOnDisk% will move all files and sub-directories inside the source
'directory as long as the destination directory is on the same drive.
'Otherwise the function will not be successful. If the function is successful
'the caller must delete the emptied source directory to complete the
'operation.
'The function can change the current directory!
'
'The function returns: -1 if successful.
'                       0 if not successful.
'
FUNCTION MoveDirOnDisk% (Source$, Destination$)

MoveDirOnDisk% = -1               'Assume success.

ON LOCAL ERROR GOTO MoveDirError
CHDIR Source$
CHDRIVE Source$
MKDIR Destination$
ON LOCAL ERROR GOTO 0

MoveDirStart:

'----------------------------------------------------------------------------

  IF ReadDir%(AllFilesMask$, FindFirst, AllFilesAtt) THEN

    DO
    Filename$ = UCASE$(RTRIM$(DTA.Filename))

      IF LEFT$(Filename$, 1) <> "." THEN
      NewSource$ = AddSlash$(Source$) + Filename$
      NewDest$ = AddSlash$(Destination$) + Filename$

        IF ASC(DTA.Attributes) AND 16 THEN               'This is a directory.
            IF MoveDirOnDisk%(NewSource$, NewDest$) THEN
              ON LOCAL ERROR GOTO MoveDirError
              CHDIR UpDirMask$
              RMDIR NewSource$
              ON LOCAL ERROR GOTO 0
            ELSE
              MoveDirOnDisk% = 0
              GOTO MoveDirEnd
            END IF
        ELSE                                             'This is a file.
          ON LOCAL ERROR GOTO MoveDirError
          NAME NewSource$ AS NewDest$
          ON LOCAL ERROR GOTO 0
        END IF
      GOTO MoveDirStart

      ELSE

        IF NOT (ReadDir(AllFilesMask$, FindNext, AllFilesAtt)) THEN
        GOTO MoveDirEnd
        END IF

      END IF

    LOOP

  END IF


MoveDirEnd:
EXIT FUNCTION

'============================================================================
'============================================================================
MoveDirError:
MoveDirOnDisk% = 0
RESUME MoveDirEnd

END FUNCTION

'Function returns: -1 when successful and NO info boxes were shown.
'                   1 when successful, but info boxes WERE shown.
'                   0 when not successful.
'
FUNCTION MoveFileDir% (Source$, Destination$, BYVAL Directory%)

  IF INSTR(Source$, "\") = 0 OR INSTR(Destination$, "\") = 0 THEN
  EXIT FUNCTION
  END IF

MoveFileDir% = -1           'Assume success.
'============================================================================

  IF LEFT$(Source$, 1) = LEFT$(Destination$, 1) THEN

    IF Directory% THEN
        IF MoveDirOnDisk%(Source$, Destination$) THEN
          ON LOCAL ERROR GOTO MoveFileDirError
          CHDIR UpDirMask$
          RMDIR Source$
          ON LOCAL ERROR GOTO 0
        ELSE
          MoveFileDir% = 0
        END IF
    ELSE
      ON LOCAL ERROR GOTO MoveFileDirError
      NAME Source$ AS Destination$
      ON LOCAL ERROR GOTO 0
    END IF

'----------------------------------------------------------------------------
  ELSE

      IF Directory% THEN
        Success% = CopyDir%(Source$, Destination$)
      ELSE
        Success% = CopyFile%(Source$, Destination$)
      END IF


      IF Success% THEN

      'Search for a backslash.
        FOR Counter% = LEN(Source$) TO 1 STEP -1
        Temp% = ASC(MID$(Source$, Counter%, 1))
          IF Temp% = 92 THEN EXIT FOR                '92 = "\"
        NEXT

      File$ = MID$(Source$, Counter% + 1)

        IF Directory% = 0 THEN
        ExtractFileExt File$, Extension$
        END IF

      Directory$ = AddSlash$(LEFT$(Source$, Counter% - 1))
      Success% = KillFileDir%(File$, Extension$, Directory$, Directory%)
      END IF

    MoveFileDir% = Success%


  END IF


MoveFileDirEnd:
EXIT FUNCTION

'============================================================================
'============================================================================
MoveFileDirError:
MoveFileDir% = 0
RESUME MoveFileDirEnd

END FUNCTION

FUNCTION ReadDir% (DirSpec$, BYVAL SearchType%, BYVAL Filter%)

  IF SearchType% <> FindFirst AND SearchType% <> FindNext THEN
  EXIT FUNCTION
  END IF


'*********************************
'Setting up DTA...
'*********************************

  IF SearchType% = FindFirst THEN
  Regs.ax = &H1A00
  Regs.ds = VARSEG(DTA)
  Regs.dx = VARPTR(DTA)
  CALL INTERRUPTX(&H21, Regs, Regs)
  END IF


'*********************************
'Finding FIRST or NEXT entries...
'*********************************
ChrZero$ = CHR$(0)

  IF RIGHT$(DirSpec$, 1) = ChrZero$ THEN
    Mask$ = DirSpec$
  ELSE
    Mask$ = DirSpec$ + ChrZero$
  END IF

Regs.ax = SearchType%
Regs.cx = Filter%
Regs.ds = SSEG(Mask$)
Regs.dx = SADD(Mask$)

ON LOCAL ERROR GOTO ReadDirError
CALL INTERRUPTX(&H21, Regs, Regs)
ON LOCAL ERROR GOTO 0


'*********************************
'Check for success.
'*********************************

  IF Regs.flags AND 1 THEN
    ReadDir% = 0
  ELSE
    NullByte% = INSTR(DTA.Filename, CHR$(0))
      IF NullByte% THEN
      DTA.Filename = LEFT$(DTA.Filename, NullByte% - 1)
      END IF
    ReadDir% = -1
  END IF

ReadDirEnd:
EXIT FUNCTION

'============================================================================
ReadDirError:
ReadDir% = 0
RESUME ReadDirEnd

END FUNCTION

