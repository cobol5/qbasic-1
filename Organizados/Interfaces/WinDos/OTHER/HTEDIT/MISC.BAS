'$FORM HTEdit

DECLARE FUNCTION SaveFile% (SaveType%)
DECLARE FUNCTION FillChar$ (Char$)

CONST TRUE = -1
CONST FALSE = NOT TRUE
CONST MaxBuffSize = 32766

COMMON SHARED MainTxtChanged%
COMMON SHARED Port$
COMMON SHARED FileName$
COMMON SHARED WordWrap%
COMMON SHARED Sep1$
COMMON SHARED Sep2$
COMMON SHARED Sep3$

SUB AddFileToMenu ()

DIM OpenFileList$(3)
STATIC OpenFileCount%

  IF OpenFileCount% < 4 THEN
    OpenFileCount% = OpenFileCount% + 1
      IF OpenFileCount% = 1 THEN
      HTEdit.FileSep3Mnu.Visible = TRUE
      END IF
    HTEdit.FilenamesMnu(OpenFileCount% - 1).Visible = TRUE
  END IF

  FOR i% = 0 TO 3
    IF HTEdit.FilenamesMnu(i%).CAPTION <> "" THEN
      OpenFileList$(i%) = RIGHT$(HTEdit.FilenamesMnu(i%).CAPTION, LEN(HTEdit.FilenamesMnu(i%).CAPTION) - 4)
    ELSE
      OpenFileList$(i%) = ""
    END IF
  NEXT i%

  FOR i% = 0 TO 2
  HTEdit.FilenamesMnu(i% + 1).CAPTION = "&" + LTRIM$(STR$(i% + 2)) + ". " + OpenFileList$(i%)
  NEXT i%

HTEdit.FilenamesMnu(0).CAPTION = "&1. " + FileName$

END SUB

SUB DrawSymLine (Symbols$)

HTEdit.MainTXT(WordWrap%).SelText = Symbols$
HTEdit.MainTXT(WordWrap%).SelStart = HTEdit.MainTXT(WordWrap%).SelStart + LEN(Symbols$)

END SUB

FUNCTION FillChar$ (Char$)

  FOR counter% = 1 TO 75
  Temp$ = Temp$ + Char$
  NEXT

FillChar$ = Temp$ + CHR$(13)

END FUNCTION

SUB LoadFile ()
ON LOCAL ERROR GOTO LoadError

File% = 1
OPEN FileName$ FOR BINARY AS #File%

FileSize& = LOF(File%)

  IF FileSize& <= MaxBuffSize THEN
    BuffSize& = FileSize&
  ELSE
    BuffSize& = MaxBuffSize
    MSGBOX "HTEdit can only display the first 32K of the file."
  END IF

Buffer$ = SPACE$(BuffSize&)
  
GET #File%, , Buffer$
CLOSE #File%

HTEdit.MainTXT(WordWrap%).TEXT = Buffer$

HTEdit.FileNameLbl.ForeColor = 2
HTEdit.FileNameLbl.CAPTION = FileName$

MainTxtChanged% = FALSE

AfterLoadError:
EXIT SUB

'============================================================================
LoadError:
  SELECT CASE ERR
  CASE 7
    MSGBOX "Out of memory.", 0, "Error"
  CASE ELSE
    MSGBOX "Cannot load file.", 0, "Error"
  END SELECT
RESUME AfterLoadError

END SUB

FUNCTION SaveFile% (SaveType%)
ON LOCAL ERROR GOTO CantSave

File% = 1
OPEN FileName$ FOR BINARY AS #File%
Size& = LOF(File%)
CLOSE File%

  IF Size& > 0 AND SaveType% = 1 THEN
    Answer% = MSGBOX("Replace existing file?", 4, "File exists")
  ELSE
    Answer% = 6
  END IF


  IF Answer% = 6 THEN

      IF Size& > 0 THEN
      KILL FileName$
      END IF

    OPEN FileName$ FOR BINARY AS #File%
    junk$ = HTEdit.MainTXT(WordWrap%).TEXT
    PUT #File%, , junk$
    CLOSE #File%

    HTEdit.FileNameLbl.ForeColor = 2
    HTEdit.FileNameLbl.CAPTION = FileName$
    MainTxtChanged% = FALSE
    SaveFile% = TRUE

  ELSE

    MSGBOX "File not saved."
    SaveFile% = FALSE

  END IF

AfterWriteError:
EXIT FUNCTION

'============================================================================
CantSave:
MSGBOX "Cannot save file.", 0, "Error"
CLOSE
SaveFile% = FALSE
RESUME AfterWriteError

END FUNCTION

