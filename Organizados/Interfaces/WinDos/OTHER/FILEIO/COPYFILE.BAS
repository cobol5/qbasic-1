DECLARE FUNCTION CopyFile% (Source$, Destination$)

OldTime! = TIMER
Temp% = CopyFile%("c:\command.com", "a:\temp\command.com")

  IF Temp% THEN
    PRINT "Success"
  ELSE
    PRINT "Error"
  END IF

PRINT TIMER - OldTime!;
PRINT "sec taken"

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

