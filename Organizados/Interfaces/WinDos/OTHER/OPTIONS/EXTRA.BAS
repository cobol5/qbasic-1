
DECLARE FUNCTION ReadFileUpTo% (BYVAL File%, Text$)
DECLARE FUNCTION AddSlash$ (Strng$)
DECLARE SUB ReadINI ()
DECLARE SUB WriteINI ()
DECLARE SUB QuickEXE (Program$, PrgDir$, SwitchTo$)
DECLARE SUB EmptyKeyb ()
DECLARE FUNCTION CheckPath% (Directory$)
DECLARE FUNCTION ReadFileUpTo% (BYVAL File%, Text$)

CONST MainINI = "windos.ini"
CONST TempINI = "windos.tmp"

'$INCLUDE: 'common.bi'

FUNCTION AddSlash$ (Strng$)

  IF RIGHT$(Strng$, 1) <> "\" THEN
    AddSlash$ = Strng$ + "\"
  ELSE
    AddSlash$ = Strng$
  END IF

END FUNCTION

FUNCTION CheckPath% (Directory$)

CheckPath% = 0


  IF MID$(Directory$, 2, 1) = ":" AND INSTR(Directory$, "\") >= 3 THEN
  CurrentDir$ = CURDIR$
  ON LOCAL ERROR GOTO BadDir
  CHDIR Directory$
  ON LOCAL ERROR GOTO 0

    IF NOT (ErrorDir%) THEN
    CHDIR CurrentDir$
    CheckPath% = -1
    END IF

  END IF


EXIT FUNCTION

'============================================================================
BadDir:
ErrorDir% = -1
RESUME NEXT

END FUNCTION

SUB EmptyKeyb ()

DEF SEG = &H40
POKE &H1A, PEEK(&H1C)
DEF SEG

END SUB

SUB QuickEXE (Program$, PrgDir$, SwitchTo$)

CHDIR PrgDir$
CHDRIVE LEFT$(PrgDir$, 2)

EmptyKeyb

SHELL Program$

  IF LEN(SwitchTo$) THEN
  CHDRIVE LEFT$(SwitchTo$, 2)
  CHDIR SwitchTo$
  END IF

END SUB

FUNCTION ReadFileUpTo% (BYVAL File%, Text$)

  DO UNTIL EOF(File%)
  LINE INPUT #File%, Junk$
    IF UCASE$(RTRIM$(Junk$)) = UCASE$(RTRIM$(Text$)) THEN
    ReadFileUpTo% = -1
    EXIT FUNCTION
    END IF
  LOOP

END FUNCTION

SUB ReadINI ()
ON LOCAL ERROR GOTO NoFile


File% = FREEFILE
OPEN MainINI FOR INPUT AS #File%

  DO UNTIL EOF(File%) = -1
  LINE INPUT #File%, Something$

  Something$ = LTRIM$(RTRIM$(Something$))
  Equals% = INSTR(Something$, "=")

    IF Equals% > 1 THEN
    Value$ = LTRIM$(MID$(Something$, Equals% + 1))
    Value% = VAL(Value$)
    Something$ = UCASE$(RTRIM$(LEFT$(Something$, Equals% - 1)))
    END IF


'----------------------------------------------------------------------------
    IF Something$ = "SCREENSAVER" THEN
    ScreenSaver$ = Value$
   
    ELSEIF Something$ = "DELAY" THEN
    ScrDelay% = Value%

'----------------------------------------------------------------------------
    ELSEIF Something$ = "DEFAULTDRIVE" THEN
    DefaultDrive$ = Value$
   
    ELSEIF Something$ = "WINDOS" THEN
    WindosPath$ = Value$
 
    ELSEIF Something$ = "SCRSAVERS" THEN
    ScrPath$ = Value$
 
    ELSEIF Something$ = "TEMP" THEN
    TempPath$ = Value$
 
    ELSEIF Something$ = "FONTS" THEN
    FontsPath$ = Value$

    ELSEIF Something$ = "ARCHTOOLS" THEN
    ArchPath$ = Value$
 
    ELSEIF Something$ = "GRAPHICS" THEN
    GraphicsPath$ = Value$

    ELSEIF Something$ = "DESKTOP" THEN
    DesktopPath$ = Value$

    ELSEIF Something$ = "SHORTCUTS" THEN
    ShortcutsPath$ = Value$

    ELSEIF Something$ = "ICONS" THEN
    IconsPath$ = Value$

    ELSEIF Something$ = "KEYS" THEN
    KeysPath$ = Value$

    ELSEIF Something$ = "PALETTE" THEN
    PalPath$ = Value$

'----------------------------------------------------------------------------
    ELSEIF Something$ = "DISKTOPCOLOURSET" THEN
    DisktopPal$ = Value$

    ELSEIF Something$ = "COMPRESSCOLOURSET" THEN
    CompressPal$ = Value$

    ELSEIF Something$ = "PROPERTYCOLOURSET" THEN
    PropertiesPal$ = Value$

    ELSEIF Something$ = "PRINTCOLOURSET" THEN
    PrintPal$ = Value$

'----------------------------------------------------------------------------
    ELSEIF Something$ = "BACKGROUND" THEN
    Background$ = Value$

    ELSEIF Something$ = "RESOLUTION" THEN
    Resolution$ = Value$

    ELSEIF Something$ = "DEFAULTSHORTCUT" THEN
    DefIcon$ = Value$

'----------------------------------------------------------------------------
    ELSEIF Something$ = "ANYKEY" THEN
    AnyKey% = Value%

    ELSEIF Something$ = "FLUSHBUFFERS" THEN
    FlushBuffers% = Value%

    ELSEIF Something$ = "USECOMMANDCOM" THEN
    UseCommandCom% = Value%

    ELSEIF Something$ = "VIEWER" THEN
    Editor$ = Value$

'----------------------------------------------------------------------------
    END IF
  LOOP

CLOSE #File%
EXIT SUB

'============================================================================
NoFile:
MSGBOX "File " + MainINI + " doesn't exist. Press OK to quit."
END

END SUB

SUB WriteINI ()

File1% = FREEFILE
OPEN MainINI FOR INPUT AS #File1%

File2% = FREEFILE
OPEN TempINI FOR OUTPUT AS #File2%


  DO UNTIL EOF(File1%)
  LINE INPUT #File1%, Something$
  Equals% = INSTR(Something$, "=")

    IF Equals% > 1 THEN
      Value$ = UCASE$(LTRIM$(RTRIM$(LEFT$(Something$, Equals% - 1))))
    ELSE
      Value$ = ""
    END IF


    IF Value$ = "SCREENSAVER" THEN
    PRINT #File2%, "ScreenSaver = " + UCASE$(ScreenSaver$)
    
    ELSEIF Value$ = "DELAY" THEN
    PRINT #File2%, "Delay = " + LTRIM$(STR$(ScrDelay%))

'----------------------------------------------------------------------------

    ELSEIF Value$ = "DEFAULTDRIVE" THEN
    PRINT #File2%, "DefaultDrive = " + UCASE$(DefaultDrive$)

    ELSEIF Value$ = "WINDOS" THEN
    PRINT #File2%, "WinDos = " + UCASE$(WindosPath$)

    ELSEIF Value$ = "SCRSAVERS" THEN
    PRINT #File2%, "ScrSavers = " + UCASE$(ScrPath$)

    ELSEIF Value$ = "TEMP" THEN
    PRINT #File2%, "Temp = " + UCASE$(TempPath$)

    ELSEIF Value$ = "FONTS" THEN
    PRINT #File2%, "Fonts = " + UCASE$(FontsPath$)

    ELSEIF Value$ = "ARCHTOOLS" THEN
    PRINT #File2%, "ArchTools = " + UCASE$(ArchPath$)

    ELSEIF Value$ = "GRAPHICS" THEN
    PRINT #File2%, "Graphics = " + UCASE$(GraphicsPath$)

    ELSEIF Value$ = "DESKTOP" THEN
    PRINT #File2%, "Desktop = " + UCASE$(DesktopPath$)

    ELSEIF Value$ = "SHORTCUTS" THEN
    PRINT #File2%, "Shortcuts = " + UCASE$(ShortcutsPath$)

    ELSEIF Value$ = "ICONS" THEN
    PRINT #File2%, "Icons = " + UCASE$(IconsPath$)

    ELSEIF Value$ = "KEYS" THEN
    PRINT #File2%, "Keys = " + UCASE$(KeysPath$)

    ELSEIF Value$ = "PALETTE" THEN
    PRINT #File2%, "Palette = " + UCASE$(PalPath$)

'----------------------------------------------------------------------------
    ELSEIF Value$ = "DISKTOPCOLOURSET" THEN
    PRINT #File2%, "DisktopColourSet = " + UCASE$(DisktopPal$)

    ELSEIF Value$ = "COMPRESSCOLOURSET" THEN
    PRINT #File2%, "CompressColourSet = " + UCASE$(CompressPal$)

    ELSEIF Value$ = "PROPERTYCOLOURSET" THEN
    PRINT #File2%, "PropertyColourSet = " + UCASE$(PropertiesPal$)

    ELSEIF Value$ = "PRINTCOLOURSET" THEN
    PRINT #File2%, "PrintColourSet = " + UCASE$(PrintPal$)

'----------------------------------------------------------------------------
    ELSEIF Value$ = "BACKGROUND" THEN
    PRINT #File2%, "Background = " + UCASE$(Background$)

    ELSEIF Value$ = "RESOLUTION" THEN
    PRINT #File2%, "Resolution = " + Resolution$

    ELSEIF Value$ = "DEFAULTSHORTCUT" THEN
    PRINT #File2%, "DefaultShortcut = " + UCASE$(DefIcon$)

'----------------------------------------------------------------------------

    ELSEIF Value$ = "ANYKEY" THEN
    PRINT #File2%, "AnyKey = " + LTRIM$(STR$(AnyKey%))

    ELSEIF Value$ = "USECOMMANDCOM" THEN
    PRINT #File2%, "UseCommandCom = " + LTRIM$(STR$(UseCommandCom%))

    ELSEIF Value$ = "FLUSHBUFFERS" THEN
    PRINT #File2%, "FlushBuffers = " + LTRIM$(STR$(FlushBuffers%))

    ELSEIF Value$ = "VIEWER" THEN
    PRINT #File2%, "Viewer = " + UCASE$(Editor$)

'----------------------------------------------------------------------------

    ELSE
    PRINT #File2%, Something$
    END IF

  LOOP


CLOSE #File1%, #File2%

KILL MainINI
NAME TempINI AS MainINI

END SUB

