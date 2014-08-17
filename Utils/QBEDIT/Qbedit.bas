'Qedit v1.1
'This is a small text editor I wrote in my spare tim. Nothing special.
'In all accutality this appears like the old UNIX editors.
'Function Listing: F1=Memory screen, alt+q=quit, alt+p=print
'alt+r=ruler, shift+arrow keys=block, alt+s=save,alt+x=exit w/o saving, Esc=exit out of dialoge box
'alt+l=load
DECLARE SUB Filter (Dirty$)
DECLARE SUB Edit (Text$(), KeyCode%, FileChanged%)
DECLARE SUB MsgBox (Msg$, ValidKeys$, KeyCode%)
DECLARE SUB InputBox (Prompt$, InputText$, KeyCode%)
DECLARE FUNCTION ColorMonitor% ()
DECLARE SUB DrawBox (TopRow%, NumRows%, LCol%, NumCols%)
DECLARE FUNCTION FileExists% (FileName$)
DECLARE FUNCTION FileLineCount% (FileName$)
DECLARE FUNCTION FileSize& (FileName$)
DECLARE FUNCTION Greater% (Var1%, Var2%)
DECLARE SUB InputLine (Text$, ViewWidth%, KeyCode%)
DECLARE FUNCTION LegalFileName% (FileName$)
DECLARE FUNCTION Lesser% (Var1%, Var2%)
'   These variables control how much of the screen the editor
'covers:
'  TopRow% = 1: BotRow% = 25: LeftCol% = 1: RiteCol% = 80
'these values, of course, make it take the whole screen. Simply
'by changing these values you can edit text on only part of the
'screen if you wish. Note, however, that code assumes that
'dialog boxes can be centered in the editing area of the screen.
'If the screen window for editing is made too small, the dialog
'boxes will be chopped off. So if you want a very small editing
'area, you will have to rework that part of the code. As written,
'the screen window must be at least 72 columns wide and the
'total number of screen rows should not be less than 3.
DEFINT A-Z
'$INCLUDE: 'qb.bi'
CONST False = 0, True = NOT False
CONST Black = 0, Blue = 1, Brown = 6, Grey = 7
CONST AltF = -33, AltL = -38, AltP = -25, AltQ = -16
CONST AltR = -19, AltS = -31, AltX = -45
CONST ESC = 27, ENTER = 13, BkSpc = 8, TabKey = 9
CONST F1Key = -59, F10Key = -68
CONST InsertKey = -82, DeleteKey = -83
CONST HomeKey = -71, CtrlHome = -119
CONST EndKey = -79, CtrlEnd = -117
CONST PgUpKey = -73, PgDnKey = -81
CONST UpArrow = -72, DnArrow = -80, LArrow = -75, RArrow = -77
DIM SHARED TopRow%, BotRow%, LeftCol%, RiteCol%
DIM SHARED ScrnRows%, MidRow%, ScrnWidth%
DIM SHARED FirstLn%, LastLn%, MaxLines%, FirstChar%, MaxChars%
DIM SHARED CursRow%, CursCol%, ActLn%, ActChar%
DIM SHARED FG%, BG%, BoxFG%, BoxBG%
DIM SHARED CursSize%, TabLen%, MemPad%, Null$ 'MemPad is a varible that keeps enough memory for internal functions
TopRow% = 1: BotRow% = 25: LeftCol% = 1: RiteCol% = 80
ScrnRows% = (BotRow% - TopRow%) + 1
MidRow% = TopRow% + (ScrnRows% \ 2)
ScrnWidth% = (RiteCol% - LeftCol%) + 1

FirstLn% = 1: LastLn% = 0: MaxLines% = 800
FirstChar% = 1: MaxChars% = 255

CursRow% = TopRow%: CursCol% = LeftCol%
ActLn% = FirstLn%: ActChar% = FirstChar%

FG% = Grey: BG% = Black: BoxFG% = Grey: BoxBG% = Brown
IF ColorMonitor% THEN
  CursSize% = 12: BG% = Blue
ELSE
  CursSize% = 7
END IF
TabLen% = 8: Null$ = "": MemPad% = 1024

DIM Text$(FirstLn% TO MaxLines% + 1)

 Cmd$ = RTRIM$(COMMAND$)
 Ptr% = INSTR(Cmd$, "/L:")
 IF Ptr% THEN
   Ptr2% = INSTR(Ptr% + 3, Cmd$, " ")
   IF Ptr2% THEN
	L$ = MID$(Cmd$, Ptr% + 3, Ptr2% - (Ptr% + 3))
	IF LEN(L$) THEN MaxLines% = VAL(L$)
	Cmd$ = MID$(Cmd$, 1, Ptr% - 1) + MID$(Cmd$, Ptr2% + 1)
   ELSE
	L$ = MID$(Cmd$, Ptr% + 3)
	IF LEN(L$) THEN MaxLines% = VAL(L$)
	Cmd$ = RTRIM$(MID$(Cmd$, 1, Ptr% - 1))
   END IF
 END IF
 File$ = Cmd$
 IF INSTR(File$, "*") OR INSTR(File$, "?") THEN
   BEEP: PRINT "No wildcards allowed in file specification": END
 END IF

IF LEN(File$) THEN
  IF FileExists(File$) THEN
    File = FREEFILE
    OPEN File$ FOR INPUT AS File
	 IF LOF(File) THEN
	   LnPtr% = 0
	   DO UNTIL EOF(File)
		LnPtr% = LnPtr% + 1
		IF LnPtr% > MaxLines% THEN
		  BEEP: PRINT "File has too many lines"
		  CLOSE File: END
		END IF
		LINE INPUT #File, Text$(LnPtr%)
		CALL Filter(Text$(LnPtr%))
		IF FRE(Null$) < MemPad% THEN
		  BEEP: PRINT "Not enough memory"
		  CLOSE File: END
		END IF
	   LOOP
	   LastLn% = LnPtr%
	 END IF
    CLOSE File
  ELSE
    IF NOT LegalFileName(File$) THEN
	 BEEP: PRINT "Bad file name": END
    END IF
  END IF
END IF

SCREEN 0, 1, 0: WIDTH 80
COLOR FG%, BG%, BG%: CLS

FileChanged% = False
DO
CALL Edit(Text$(), ExitKey%, FileChanged%)
SELECT CASE ExitKey%
  CASE AltS
    IF FileChanged% THEN
	 IF LEN(File$) THEN
	   GOSUB SaveFile
	 ELSE
	   GOSUB GetFileName
	   IF KeyCode% = ENTER THEN GOSUB SaveFile
	 END IF
    END IF
  CASE AltQ
    IF FileChanged% THEN
	 CALL MsgBox("Save file changes? (Y/N) ", "YyNn", KeyCode%)
	 SELECT CASE KeyCode%
	   CASE 89, 121                  '"Y", "y"
		IF LEN(File$) THEN
		  GOSUB SaveFile
		ELSE
		  GOSUB GetFileName
		  IF KeyCode% = ENTER THEN GOSUB SaveFile: EXIT DO
		END IF
	   CASE 78, 110                  '"N", "n"
		EXIT DO
	   CASE ESC
		'do nothing to return to edit mode
	 END SELECT
    ELSE
	 EXIT DO
    END IF
  CASE AltX
    IF FileChanged% THEN
	 GOSUB GetFileName
	 IF KeyCode% = ENTER THEN GOSUB SaveFile: EXIT DO
    ELSE
	 EXIT DO
    END IF
END SELECT
LOOP
CLS : END

GetFileName:
DO
  CALL InputBox("File name: ", File$, KeyCode%)
  IF KeyCode% = ENTER THEN
    IF LegalFileName(File$) THEN EXIT DO ELSE BEEP
  ELSE 'IF KeyCode% = ESC THEN
    EXIT DO
  END IF
LOOP
RETURN

SaveFile:
File = FREEFILE
OPEN File$ FOR OUTPUT AS File
  IF LastLn% > 0 THEN
    FOR LnPtr% = FirstLn% TO LastLn%
	 PRINT #File, Text$(LnPtr%)
    NEXT
  END IF
CLOSE File
FileChanged% = False
RETURN

FUNCTION ColorMonitor%
ColorMonitor% = True
DEF SEG = 0
IF PEEK(&H463) <> &HD4 THEN ColorMonitor% = False
DEF SEG
END FUNCTION

SUB DrawBox (TopRow%, NumRows%, LCol%, NumCols%)

Top$ = STRING$(NumCols%, "Û")
MID$(Top$, 1, 2) = " Û": MID$(Top$, NumCols% - 1, 2) = "Û "
Side$ = SPACE$(NumCols%)
MID$(Side$, 1, 2) = " Û": MID$(Side$, NumCols% - 1, 2) = "Û "
Bot$ = Top$
MID$(Bot$, 2, 1) = "Û": MID$(Bot$, NumCols% - 1, 1) = "Û"

LOCATE TopRow%, LCol%, 0: PRINT Top$
FOR Row% = 3 TO NumRows%
  LOCATE , LCol%: PRINT Side$
NEXT
LOCATE , LCol%: PRINT Bot$;

END SUB

SUB Edit (Text$(), KeyCode%, FileChanged%)

'when marking a block involving multiple lines:
BlkStartLn = 0        'is the starting line of the block
BlkLines = 0          'the number of lines in the block.
				  'If zero, then not in this block marking mode

'when marking a block of characters within a single line:
BlkStartChar = 0      'this is the starting character
BlkChars = 0          'the number of characters in the block.
				  'If zero, then not in this block marking mode

Refresh$ = SPACE$(ScrnWidth)       'scratch space
InsertMode = True
LOCATE , , 0, CursSize - 1, CursSize
GOSUB RefreshScreen
DO
  ActLn$ = Text$(ActLn): ActLnLen = LEN(ActLn$)
  LOCATE CursRow, CursCol, 1
  DO WHILE LEN(INKEY$): LOOP
  DO: Ky$ = INKEY$: LOOP UNTIL LEN(Ky$)
  DEF SEG = 0: ShiftByte = PEEK(&H417): DEF SEG
  ShiftPressed = ((ShiftByte AND 3) <> 0)
  KeyCode = ASC(Ky$): IF KeyCode = 0 THEN KeyCode = -ASC(MID$(Ky$, 2))
  SELECT CASE KeyCode
    CASE 32 TO 255, 1 TO 6, 14 TO 27: GOSUB DoLetterKey
    CASE BkSpc: GOSUB DoBackSpaceKey
    CASE TabKey: GOSUB DoTabKey
    CASE ENTER: GOSUB DoCarriageReturn
    CASE InsertKey: GOSUB DoInsertKey
    CASE DeleteKey: GOSUB DoDeleteKey
    CASE UpArrow: GOSUB DoUpArrow
    CASE DnArrow: GOSUB DoDnArrow
    CASE PgUpKey: GOSUB DoPgUpKey
    CASE PgDnKey: GOSUB DoPgDnKey
    CASE LArrow: GOSUB DoLArrow
    CASE RArrow: GOSUB DoRArrow
    CASE HomeKey: GOSUB DoHomeKey
    CASE EndKey: GOSUB DoEndKey
    CASE CtrlHome: GOSUB DoCtrlHome
    CASE CtrlEnd: GOSUB DoCtrlEnd
    CASE F1Key: GOSUB DoHelp
    CASE F10Key: CLS : SHELL: GOSUB RefreshScreen
    CASE AltF: GOSUB FindText
    CASE AltL: GOSUB LoadFile
    CASE AltP: GOSUB DoPrint
    CASE AltR: GOSUB DoRuler
    CASE AltS: IF BlkLines THEN GOSUB SaveLines ELSE EXIT DO
    CASE AltQ, AltX: EXIT DO
    CASE ELSE: BEEP
  END SELECT
LOOP

EXIT SUB

DoCarriageReturn:
IF BlkLines OR BlkChars THEN BEEP: RETURN
IF LastLn = MaxLines THEN BEEP: RETURN
IF ActLn < LastLn THEN
  FOR LnPtr = LastLn TO ActLn + 1 STEP -1
    SWAP Text$(LnPtr), Text$(LnPtr + 1)
  NEXT
  Text$(ActLn) = LEFT$(ActLn$, ActChar - 1)
  Text$(ActLn + 1) = MID$(ActLn$, ActChar)
END IF
ActLn = ActLn + 1
ActChar = FirstChar
IF CursRow <> BotRow THEN CursRow = CursRow + 1
CursCol = LeftCol
LastLn = LastLn + 1
GOSUB RefreshScreen
FileChanged = True
RETURN

DoTabKey:
IF BlkLines OR BlkChars THEN BEEP: RETURN
IF ActLn > MaxLines THEN BEEP: RETURN
TabChars = TabLen - (ActChar MOD TabLen)
IF ActLnLen + TabChars > MaxChars THEN BEEP: RETURN
IF ActChar - 1 + TabChars > MaxChars THEN BEEP: RETURN
IF ActChar <= ActLnLen THEN
  Text$(ActLn) = LEFT$(ActLn$, ActChar - 1) + SPACE$(TabChars) + MID$(ActLn$, ActChar)
ELSE
  Text$(ActLn) = ActLn$ + SPACE$((ActChar - ActLnLen - 1) + TabChars)
END IF
ActChar = ActChar + TabChars
IF CursCol + TabChars <= RiteCol THEN
  CursCol = CursCol + TabChars
  GOSUB RefreshLine
ELSE
  GOSUB RefreshScreen
END IF
IF ActLn > LastLn THEN LastLn = ActLn
FileChanged = True
RETURN

DoDeleteKey:
IF BlkLines THEN
  IF BlkLines > 0 THEN
    ActLn = BlkStartLn
    CursRow = Greater(CursRow - BlkLines, TopRow)
  END IF
  ClipLines = ABS(BlkLines)
  BytesNeeded& = 0
  FOR LnPtr = ActLn TO ActLn + ClipLines - 1
    BytesNeeded& = BytesNeeded& + LEN(Text$(LnPtr)) + 6
  NEXT
  IF BytesNeeded& + MemPad > FRE(Null$) THEN ClipLines = 0: BEEP: RETURN
  REDIM ClipBoard$(ClipLines - 1)
  FOR LnPtr = 0 TO ClipLines - 1
    SWAP ClipBoard$(LnPtr), Text$(ActLn + LnPtr)
  NEXT
  FOR LnPtr = ActLn TO LastLn - ClipLines
    SWAP Text$(LnPtr), Text$(LnPtr + ClipLines)
  NEXT
  BlkLines = 0: ClipChars = 0
  LastLn = LastLn - ClipLines
  IF NOT ShiftPressed THEN ClipLines = 0: ERASE ClipBoard$
  GOSUB RefreshScreen
ELSEIF BlkChars THEN
  IF BlkChars > 0 THEN
    ActChar = BlkStartChar
    CursCol = Greater(CursCol - BlkChars, LeftCol)
  END IF
  ClipChars = ABS(BlkChars)
  ClipBoard$ = MID$(ActLn$, ActChar, ClipChars)
  Text$(ActLn) = LEFT$(ActLn$, ActChar - 1) + MID$(ActLn$, ActChar + ClipChars)
  BlkChars = 0: ClipLines = 0
  IF NOT ShiftPressed THEN ClipChars = 0: ClipBoard$ = Null$
  GOSUB RefreshScreen
ELSE 'no blocks marked
  IF ShiftPressed THEN BEEP: RETURN
  IF ActChar <= ActLnLen THEN
    Text$(ActLn) = LEFT$(ActLn$, ActChar - 1) + MID$(ActLn$, ActChar + 1)
    GOSUB RefreshLine
  ELSE 'IF ActChar > ActLnLen THEN
    IF ActLn >= LastLn THEN BEEP: RETURN
    IF ActChar > MaxChars THEN BEEP: RETURN
    IF (ActChar - 1) + LEN(Text$(ActLn + 1)) > MaxChars THEN BEEP: RETURN
    Text$(ActLn) = ActLn$ + SPACE$(ActChar - ActLnLen - 1) + Text$(ActLn + 1)
    Text$(ActLn + 1) = Null$
    FOR LnPtr = ActLn + 1 TO LastLn
	 SWAP Text$(LnPtr), Text$(LnPtr + 1)
    NEXT
    LastLn = LastLn - 1
    GOSUB RefreshScreen
  END IF
END IF
FileChanged = True
RETURN

DoInsertKey:
IF ShiftPressed THEN
  IF ClipLines THEN
    IF LastLn + ClipLines > MaxLines THEN BEEP: RETURN
    BytesNeeded& = 0
    FOR LnPtr = 0 TO ClipLines - 1
	 BytesNeeded& = BytesNeeded& + LEN(ClipBoard$(LnPtr))
    NEXT
    IF BytesNeeded& + MemPad > FRE(Null$) THEN BEEP: RETURN
    IF ActLn <= LastLn THEN
	 FOR LnPtr = LastLn TO ActLn STEP -1
	   SWAP Text$(LnPtr), Text$(LnPtr + ClipLines)
	 NEXT
    END IF
    FOR LnPtr = 0 TO ClipLines - 1
	 Text$(ActLn + LnPtr) = ClipBoard$(LnPtr)
    NEXT
    LastLn = LastLn + ClipLines
    GOSUB RefreshScreen
    FileChanged = True
  ELSEIF ClipChars THEN
    IF ActLn > MaxLines THEN BEEP: RETURN
    IF ActLnLen + ClipChars > MaxChars THEN BEEP: RETURN
    IF ActChar + ClipChars - 1 > MaxChars THEN BEEP: RETURN
    IF ActChar <= ActLnLen THEN
	 Text$(ActLn) = LEFT$(ActLn$, ActChar - 1) + ClipBoard$ + MID$(ActLn$, ActChar)
    ELSE
	 Text$(ActLn) = ActLn$ + SPACE$(ActChar - ActLnLen - 1) + ClipBoard$
    END IF
    IF ActLn > LastLn THEN LastLn = ActLn
    GOSUB RefreshLine
    FileChanged = True
  END IF
ELSE 'IF NOT ShiftPressed THEN
  InsertMode = NOT InsertMode
  IF InsertMode THEN
    LOCATE , , , CursSize - 1, CursSize
  ELSE
    LOCATE , , , CursSize \ 2, CursSize
  END IF
END IF
RETURN

DoBackSpaceKey:
IF BlkLines OR BlkChars THEN BEEP: RETURN
IF ActChar = FirstChar THEN
  IF ActLn = FirstLn THEN BEEP: RETURN
  IF ActLnLen + LEN(Text$(ActLn - 1)) > MaxChars THEN BEEP: RETURN
  ActChar = LEN(Text$(ActLn - 1)) + 1
  Text$(ActLn - 1) = Text$(ActLn - 1) + ActLn$
  Text$(ActLn) = Null$
  FOR LnPtr = ActLn TO LastLn
    SWAP Text$(LnPtr), Text$(LnPtr + 1)
  NEXT
  IF LastLn >= ActLn THEN LastLn = LastLn - 1
  ActLn = ActLn - 1
  IF CursRow <> TopRow THEN CursRow = CursRow - 1
  CursCol = Lesser(ActChar, RiteCol)
  GOSUB RefreshScreen
ELSE 'IF ActChar > FirstChar THEN
  IF ActChar <= ActLnLen + 1 THEN
    Text$(ActLn) = LEFT$(ActLn$, ActChar - 2) + MID$(ActLn$, ActChar)
  END IF
  ActChar = ActChar - 1
  IF CursCol <> LeftCol THEN
    CursCol = CursCol - 1
    GOSUB RefreshLine
  ELSE
    GOSUB RefreshScreen
  END IF
END IF
FileChanged = True
RETURN

DoLetterKey:
IF BlkLines OR BlkChars THEN BEEP: RETURN
IF ActLn > MaxLines THEN BEEP: RETURN
IF ActChar > MaxChars THEN BEEP: RETURN
IF ActChar <= ActLnLen THEN
  IF InsertMode THEN
    IF ActLnLen = MaxChars THEN BEEP: RETURN
    Text$(ActLn) = LEFT$(ActLn$, ActChar - 1) + " " + MID$(ActLn$, ActChar)
  END IF
  MID$(Text$(ActLn), ActChar) = Ky$
ELSE 'IF ActChar > ActLnLen THEN
  Text$(ActLn) = ActLn$ + SPACE$(ActChar - ActLnLen - 1) + Ky$
END IF
ActChar = ActChar + 1
IF CursCol <> RiteCol THEN
  CursCol = CursCol + 1
  GOSUB RefreshLine
ELSE
  GOSUB RefreshScreen
END IF
IF ActLn > LastLn THEN LastLn = ActLn
FileChanged = True
RETURN

DoUpArrow:
BlkChars = 0
IF ShiftPressed THEN
  IF BlkLines = 0 THEN BlkStartLn = ActLn
  IF ActLn > FirstLn THEN BlkLines = BlkLines - 1
ELSE
  BlkLines = 0
END IF
IF ActLn <> FirstLn THEN
  ActLn = ActLn - 1
  IF CursRow <> TopRow THEN CursRow = CursRow - 1
END IF
GOSUB RefreshScreen
RETURN

DoDnArrow:
BlkChars = 0
IF ShiftPressed THEN
  IF BlkLines = 0 THEN BlkStartLn = ActLn
  IF ActLn <= LastLn THEN BlkLines = BlkLines + 1
ELSE
  BlkLines = 0
END IF
IF ActLn <= LastLn THEN
  ActLn = ActLn + 1
  IF CursRow <> BotRow THEN CursRow = CursRow + 1
END IF
GOSUB RefreshScreen
RETURN

DoPgUpKey:
BlkChars = 0
IF ShiftPressed THEN
  IF BlkLines = 0 THEN BlkStartLn = ActLn
  BlkLines = Greater(BlkLines - ScrnRows, FirstLn - BlkStartLn)
ELSE
  BlkLines = 0
END IF
ActLn = Greater(ActLn - ScrnRows, FirstLn)
IF ActLn < CursRow THEN CursRow = TopRow + ActLn - 1
GOSUB RefreshScreen
RETURN

DoPgDnKey:
BlkChars = 0
IF ShiftPressed THEN
  IF BlkLines = 0 THEN BlkStartLn = ActLn
  BlkLines = Lesser(BlkLines + ScrnRows, (LastLn + 1) - BlkStartLn)
ELSE
  BlkLines = 0
END IF
ActLn = Lesser(ActLn + ScrnRows, LastLn + 1)
IF (LastLn + 1) - ActLn < BotRow - CursRow THEN
  CursRow = Lesser(BotRow - ((LastLn + 1) - ActLn), TopRow + LastLn)
END IF
GOSUB RefreshScreen
RETURN

DoLArrow:
IF ShiftPressed THEN
  IF BlkLines THEN BEEP: RETURN
  IF BlkChars = 0 THEN BlkStartChar = ActChar
  BlkChars = Greater(BlkChars - 1, FirstChar - BlkStartChar)
ELSE
  BlkLines = 0: BlkChars = 0
END IF
IF ActChar <> FirstChar THEN
  ActChar = ActChar - 1
  IF CursCol <> LeftCol THEN CursCol = CursCol - 1
END IF
GOSUB RefreshScreen
RETURN

DoRArrow:
IF ShiftPressed THEN
  IF BlkLines THEN BEEP: RETURN
  IF BlkChars = 0 THEN BlkStartChar = ActChar
  IF ActChar <= MaxChars THEN BlkChars = BlkChars + 1
ELSE
  BlkLines = 0: BlkChars = 0
END IF
IF ActChar <= MaxChars THEN
  ActChar = ActChar + 1
  IF CursCol <> RiteCol THEN CursCol = CursCol + 1
END IF
GOSUB RefreshScreen
RETURN

DoHomeKey:
IF ShiftPressed THEN
  IF BlkLines THEN BEEP: RETURN
  IF BlkChars = 0 THEN BlkStartChar = ActChar
  BlkChars = FirstChar - BlkStartChar
ELSE
  BlkLines = 0: BlkChars = 0
END IF
ActChar = FirstChar
CursCol = LeftCol
GOSUB RefreshScreen
RETURN

DoEndKey:
IF ShiftPressed THEN
  IF BlkLines THEN BEEP: RETURN
  IF BlkChars = 0 THEN BlkStartChar = ActChar
  BlkChars = (ActLnLen + 1) - BlkStartChar
ELSE
  BlkLines = 0: BlkChars = 0
END IF
LeftChar = ActChar - CursCol + 1
ActChar = ActLnLen + 1
IF ActChar <= LeftChar THEN
  CursCol = Lesser(ActChar, RiteCol)
ELSE
  CursCol = Lesser(ActChar - LeftChar + 1, RiteCol)
END IF
GOSUB RefreshScreen
RETURN

DoCtrlHome:
BlkChars = 0
IF ShiftPressed THEN
  IF BlkLines = 0 THEN BlkStartLn = ActLn
  BlkLines = FirstLn - BlkStartLn
ELSE
  BlkLines = 0
END IF
ActLn = FirstLn
CursRow = TopRow
GOSUB RefreshScreen
RETURN

DoCtrlEnd:
BlkChars = 0
IF ShiftPressed THEN
  IF BlkLines = 0 THEN BlkStartLn = ActLn
  BlkLines = (LastLn + 1) - BlkStartLn
ELSE
  BlkLines = 0
END IF
ActLn = LastLn + 1
CursRow = Lesser(BotRow, TopRow + LastLn)
GOSUB RefreshScreen
RETURN

RefreshScreen:
LOCATE , , 0
LnPtr = ActLn - (CursRow - TopRow)
CharPtr = ActChar - (CursCol - LeftCol)
IF BlkLines THEN
  StartLn = BlkStartLn: StopLn = BlkStartLn + BlkLines
  IF StartLn > StopLn THEN SWAP StartLn, StopLn
  FOR Row = TopRow TO BotRow
    LSET Refresh$ = MID$(Text$(LnPtr), CharPtr, ScrnWidth)
    LOCATE Row, LeftCol
    IF LnPtr >= StartLn AND LnPtr < StopLn THEN
	 COLOR BG, FG: PRINT Refresh$; : COLOR FG, BG
    ELSE
	 PRINT Refresh$;
    END IF
    LnPtr = LnPtr + 1
  NEXT
ELSEIF BlkChars THEN
  StartChar = BlkStartChar: StopChar = BlkStartChar + BlkChars
  IF StartChar > StopChar THEN SWAP StartChar, StopChar
  LCol = Greater(StartChar - CharPtr, 0)
  RCol = StopChar - CharPtr
  FOR Row = TopRow TO BotRow
    LSET Refresh$ = MID$(Text$(LnPtr), CharPtr, ScrnWidth)
    LOCATE Row, LeftCol
    IF LnPtr = ActLn THEN
	 PRINT LEFT$(Refresh$, LCol);
	 COLOR BG, FG
	 PRINT MID$(Refresh$, LCol + 1, RCol - LCol);
	 COLOR FG, BG
	 PRINT MID$(Refresh$, RCol + 1);
    ELSE
	 PRINT Refresh$;
    END IF
    LnPtr = LnPtr + 1
  NEXT
ELSE
  FOR Row = TopRow TO BotRow
    LSET Refresh$ = MID$(Text$(LnPtr), CharPtr, ScrnWidth)
    LOCATE Row, LeftCol
    PRINT Refresh$;
    LnPtr = LnPtr + 1
  NEXT
END IF
RETURN

RefreshLine:
LSET Refresh$ = MID$(Text$(ActLn), ActChar - CursCol + LeftCol, ScrnWidth)
LOCATE CursRow, LeftCol, 0
IF BlkChars THEN
  StartChar = BlkStartChar: StopChar = BlkStartChar + BlkChars
  IF StartChar > StopChar THEN SWAP StartChar, StopChar
  LCol = Greater(StartChar - CharPtr, 0)
  RCol = StopChar - CharPtr
  PRINT LEFT$(Refresh$, LCol);
  COLOR BG, FG
  PRINT MID$(Refresh$, LCol + 1, RCol - LCol);
  COLOR FG, BG
  PRINT MID$(Refresh$, RCol + 1);
ELSE
  PRINT Refresh$;
END IF
RETURN

DoHelp:
Temp$ = STR$(ActChar): MID$(Temp$, 1, 1) = ":"
Msg$ = "Cursor" + STR$(ActLn) + Temp$ + " | Bytes free:" + STR$(FRE(Null$))
D = (ScrnWidth - LEN(Msg$)) \ 2 - 4
COLOR BoxFG, BoxBG
CALL DrawBox(MidRow% - 1, 3, LeftCol + D, LEN(Msg$) + 8)
LOCATE MidRow%, LeftCol + D + 4: PRINT Msg$;
DO UNTIL ASC(INPUT$(1)) = ESC: LOOP
Temp$ = Null$: Msg$ = Null$
COLOR FG, BG
GOSUB RefreshScreen
RETURN

LoadFile:
IF BlkLines OR BlkChars THEN BEEP: RETURN
IF ActLn > MaxLines THEN BEEP: RETURN
CALL InputBox("File name: ", LoadFile$, KeyCode)
GOSUB RefreshScreen
IF KeyCode <> ENTER THEN RETURN
LoadFile$ = LTRIM$(RTRIM$(LoadFile$))
IF LEN(LoadFile$) = 0 THEN RETURN
IF FileExists(LoadFile$) THEN
  FileLines = FileLineCount(LoadFile$)
  IF FileLines = 0 THEN RETURN
  IF LastLn + FileLines > MaxLines THEN
    CALL MsgBox("File has too many lines", Null$, KeyCode)
    GOSUB RefreshScreen: RETURN
  ELSEIF FileSize&(LoadFile$) + MemPad > FRE(Null$) THEN
    CALL MsgBox("Not enough memory", Null$, KeyCode)
    GOSUB RefreshScreen: RETURN
  END IF
ELSE
  BEEP: CALL MsgBox("File not found", Null$, KeyCode)
  GOSUB RefreshScreen: GOTO LoadFile
END IF

IF ActLn <= LastLn THEN
  FOR LnPtr = LastLn TO ActLn STEP -1
    SWAP Text$(LnPtr), Text$(LnPtr + FileLines)
  NEXT
END IF
File = FREEFILE
OPEN LoadFile$ FOR INPUT AS #File
  FOR LnPtr = ActLn TO ActLn + FileLines - 1
    LINE INPUT #File, FileLn$
    CALL Filter(FileLn$)
    Text$(LnPtr) = FileLn$
  NEXT
  FileLn$ = Null$
CLOSE File
LastLn = LastLn + FileLines
FileChanged = True
GOSUB RefreshScreen
RETURN

SaveLines:
CALL InputBox("File name: ", SaveFile$, KeyCode)
GOSUB RefreshScreen
IF KeyCode = ENTER THEN
  SaveFile$ = LTRIM$(RTRIM$(SaveFile$))
  IF LEN(SaveFile$) THEN
    IF FileExists(SaveFile$) THEN
	 CALL MsgBox("Specified file already exists: [A]ppend [O]verwrite [C]ancel? ", "AaOoCc", KeyCode)
	 GOSUB RefreshScreen
	 SELECT CASE KeyCode
	   CASE 65, 97: Mode$ = "A": GOSUB DoFileSave
	   CASE 79, 111: Mode$ = "O": GOSUB DoFileSave
	   CASE 67, 99, ESC    'do nothing to cancel the save
	 END SELECT
    ELSE
     IF LegalFileName(SaveFile$) THEN
	   Mode$ = "O": GOSUB DoFileSave
	 ELSE
	   CALL MsgBox("Bad file name", Null$, KeyCode)
	   GOSUB RefreshScreen: GOTO SaveLines
	 END IF
    END IF
  END IF
END IF
RETURN

DoFileSave:
File = FREEFILE
OPEN Mode$, File, SaveFile$
  StartLn = Lesser(ActLn, BlkStartLn)
  FOR LnPtr = StartLn TO StartLn + ABS(BlkLines) - 1
    PRINT #File, Text$(LnPtr)
  NEXT
CLOSE File
RETURN

FindText:
IF BlkLines THEN BEEP: RETURN
IF BlkChars THEN
  CharPtr = Lesser(BlkStartChar, BlkStartChar + BlkChars)
  Search$ = MID$(ActLn$, CharPtr, ABS(BlkChars))
  GOSUB Search
ELSE
  CALL InputBox("Find: ", Search$, KeyCode)
  GOSUB RefreshScreen
  IF KeyCode = ENTER THEN
    IF LEN(Search$) THEN GOSUB Search
  END IF
END IF
RETURN

Search:
Count = 0
LnPtr = ActLn: CharPtr = ActChar
DO
  CharPtr = INSTR(CharPtr, Text$(LnPtr), Search$)
  IF CharPtr THEN
    BlkStartChar = CharPtr
    BlkChars = LEN(Search$)
    ActChar = BlkStartChar + BlkChars
    CursCol = Lesser(LeftCol + ActChar - 1, RiteCol)
    TopLn = ActLn - (CursRow - TopRow)
    ActLn = LnPtr
    CursRow = MidRow
    IF ActLn >= TopLn AND ActLn < TopLn + ScrnRows THEN
	 CursRow = TopRow + (ActLn - TopLn)
    ELSEIF ActLn <= ScrnRows THEN
	 CursRow = TopRow + ActLn - 1
    ELSEIF (LastLn + 1) - ActLn < BotRow - CursRow THEN
	 CursRow = BotRow - ((LastLn + 1) - ActLn)
    END IF
    GOSUB RefreshScreen: EXIT DO
  ELSE
    Count = Count + 1
    IF Count > LastLn THEN
	 CALL MsgBox("Search string not found", Null$, KeyCode)
	 GOSUB RefreshScreen: EXIT DO
    END IF
    LnPtr = LnPtr + 1: IF LnPtr > LastLn THEN LnPtr = FirstLn
    CharPtr = 1
  END IF
LOOP
RETURN

DoPrint:
IF BlkLines THEN
  StartLn = Lesser(ActLn, BlkStartLn)
  StopLn = StartLn + ABS(BlkLines) - 1
ELSE
  StartLn = FirstLn
  StopLn = LastLn
END IF
FOR LnPtr = StartLn TO StopLn
  LPRINT Text$(LnPtr)
NEXT
BlkLines = 0
GOSUB RefreshScreen
RETURN

DoRuler:
IF BlkLines OR BlkChars THEN RETURN
Ruler$ = SPACE$(MaxChars)
FOR CharPtr = 1 TO MaxChars
  MID$(Ruler$, CharPtr, 1) = "Â"
  IF CharPtr MOD 5 = 0 THEN MID$(Ruler$, CharPtr) = "Å"
    IF CharPtr MOD 10 = 0 THEN
    MID$(Ruler$, CharPtr - 1) = CHR$(((CharPtr \ 10) MOD 10) + 48)
  END IF
NEXT
Ruler$ = MID$(Ruler$, ActChar - CursCol + LeftCol, ScrnWidth)

RulerRow = TopRow
DO
  GOSUB PrintRuler
  DO WHILE LEN(INKEY$): LOOP
  DO: Ky$ = INKEY$: LOOP UNTIL LEN(Ky$)
  KeyCode = ASC(Ky$): IF KeyCode = 0 THEN KeyCode = -ASC(MID$(Ky$, 2))
SELECT CASE KeyCode
    CASE UpArrow
	 IF RulerRow > TopRow THEN RulerRow = RulerRow - 1
    CASE DnArrow
	 IF RulerRow < BotRow THEN RulerRow = RulerRow + 1
    CASE PgUpKey
	 IF RulerRow <> TopRow THEN RulerRow = TopRow
    CASE PgDnKey
	 IF RulerRow <> BotRow THEN RulerRow = BotRow
    CASE ESC
	 EXIT DO
    CASE ELSE
	 BEEP
  END SELECT
LOOP
Ruler$ = Null$
GOSUB RefreshScreen
RETURN

PrintRuler:
GOSUB RefreshScreen
LOCATE RulerRow, LeftCol, 0
COLOR BG, FG: PRINT Ruler$; : COLOR FG, BG
RETURN

END SUB

FUNCTION FileExists% (FileName$)
DIM XRegister AS RegTypeX

'save the current DTA
XRegister.AX = &H2F00
CALL InterruptX(&H21, XRegister, XRegister)
OldDTASeg% = XRegister.ES
OldDTAOff% = XRegister.BX

'set up a new DTA
DTA$ = SPACE$(43)
XRegister.AX = &H1A00
XRegister.DS = VARSEG(DTA$)
XRegister.DX = SADD(DTA$)
CALL InterruptX(&H21, XRegister, XRegister)

'get first matching file
Temp$ = FileName$ + CHR$(0)
XRegister.AX = &H4E00
XRegister.CX = &H6
XRegister.DS = VARSEG(Temp$)
XRegister.DX = SADD(Temp$)
CALL InterruptX(&H21, XRegister, XRegister)
'if the carry flag is clear then the file exists
FileExists% = ((XRegister.Flags AND 1) = 0)
'restore the old DTA
XRegister.AX = &H1A00
XRegister.DS = OldDTASeg%
XRegister.DX = OldDTAOff%
CALL InterruptX(&H21, XRegister, XRegister)

END FUNCTION

FUNCTION FileLineCount% (FileName$)
lines% = 0
File = FREEFILE
OPEN FileName$ FOR INPUT AS File
  DO UNTIL EOF(File)
    lines% = lines% + 1
    LINE INPUT #File, FileLine$
  LOOP
CLOSE File
FileLineCount% = lines%
END FUNCTION

FUNCTION FileSize& (FileName$)
File = FREEFILE
OPEN FileName$ FOR BINARY AS File
  FileSize& = LOF(File)
CLOSE File
END FUNCTION

SUB Filter (Dirty$)

Char$ = CHR$(7): GOSUB FilterIt       'beep
BadChr$ = CHR$(12): GOSUB FilterIt    'Form feed

BadChr$ = CHR$(9)                     'tab
Ptr% = 1
DO
  Ptr% = INSTR(Ptr%, Dirty$, BadChr$)
  IF Ptr% THEN
    TabChars% = TabLen - (Ptr% MOD TabLen%)
    Dirty$ = MID$(Dirty$, 1, Ptr% - 1) + SPACE$(TabChars%) + MID$(Dirty$, Ptr% + 1)
  ELSE
    EXIT DO
  END IF
LOOP
Dirty$ = LEFT$(Dirty$, MaxChars%)
EXIT SUB

FilterIt:
Ptr% = 1
DO
  Ptr% = INSTR(Ptr%, Dirty$, Char$)
  IF Ptr% THEN
    MID$(Dirty$, Ptr%, 1) = " "
  ELSE
    EXIT DO
  END IF
LOOP
RETURN

END SUB

FUNCTION Greater% (Var1%, Var2%) STATIC
Greater% = Var1%: IF Var2% > Var1% THEN Greater% = Var2%
END FUNCTION

SUB InputBox (Prompt$, InputText$, KeyCode%)

COLOR BoxFG%, BoxBG%
CALL DrawBox(MidRow% - 1, 3, LeftCol% + 3, ScrnWidth% - 6)
DO
  LOCATE MidRow%, LeftCol% + 7: PRINT Prompt$;
  CALL InputLine(InputText$, ScrnWidth% - 15 - LEN(Prompt$), KeyCode%)
LOOP UNTIL KeyCode% = ENTER OR KeyCode% = ESC
COLOR FG%, BG%

END SUB

SUB InputLine (Text$, ViewWidth%, KeyCode%)

MaxLnLen% = 128
LCol% = POS(0): RCol% = LCol% + ViewWidth% - 1
View$ = SPACE$(ViewWidth%)
Work$ = LEFT$(Text$, MaxLnLen%)
LnLen% = LEN(Work$)
CCol% = Lesser(LCol% + LnLen%, RCol%)
AChar% = LnLen% + 1

LOCATE , , 0, CursSize% - 1, CursSize%
Insert% = True
DO
  LOCATE , LCol%
  LSET View$ = MID$(Work$, AChar% - (CCol% - LCol%), ViewWidth%)
  PRINT View$;
  LOCATE , CCol%, 1
  LnLen% = LEN(Work$)
  DO
    DO: Ky$ = INKEY$: LOOP UNTIL LEN(Ky$)
    KeyCode% = ASC(Ky$): IF KeyCode% = 0 THEN KeyCode% = -ASC(MID$(Ky$, 2))
  LOOP UNTIL KeyCode% <> BadKey%: BadKey% = 0
  LOCATE , , 0
  SELECT CASE KeyCode%
    CASE BkSpc
	 IF AChar% = 1 THEN
	   BEEP: BadKey% = KeyCode%
	 ELSE
	   IF AChar% <= LnLen% + 1 THEN
		Work$ = LEFT$(Work$, AChar% - 2) + MID$(Work$, AChar%)
	   END IF
	   AChar% = AChar% - 1
	   IF CCol% <> LCol% THEN CCol% = CCol% - 1
	 END IF
    CASE ENTER
	 Text$ = Work$
	 EXIT DO
    CASE ESC
	 EXIT DO
    CASE 32 TO 255      ' letter keys
	 IF AChar% > MaxLnLen% THEN
	   BEEP: BadKey% = KeyCode%
	 ELSE
	   IF AChar% <= LnLen% THEN
		IF Insert% THEN
		  IF LnLen% = MaxLnLen% THEN
		    BEEP: BadKey% = KeyCode%
		  ELSE
		    Work$ = LEFT$(Work$, AChar% - 1) + Ky$ + MID$(Work$, AChar%)
		    AChar% = AChar% + 1
		    IF CCol% <> RCol% THEN CCol% = CCol% + 1
		  END IF
		ELSE 'IF NOT Insert% THEN (in overstrike mode)
		  MID$(Work$, AChar%, 1) = Ky$
		  AChar% = AChar% + 1
		  IF CCol% <> RCol% THEN CCol% = CCol% + 1
		END IF
	   ELSE 'IF AChar% > LnLen% THEN
		Work$ = Work$ + SPACE$(AChar% - LnLen% - 1) + Ky$
		AChar% = AChar% + 1
		IF CCol% <> RCol% THEN CCol% = CCol% + 1
	   END IF
	 END IF
    CASE LArrow
	 IF AChar% = 1 THEN
	   BEEP: BadKey% = KeyCode%
	 ELSE 'IF AChar% > 1 THEN
	   AChar% = AChar% - 1
	   IF CCol% <> LCol% THEN CCol% = CCol% - 1
	 END IF
    CASE RArrow
	 IF AChar% > MaxLnLen% THEN
	   BEEP: BadKey% = KeyCode%
	 ELSE
	   AChar% = AChar% + 1
	   IF CCol% <> RCol% THEN CCol% = CCol% + 1
	 END IF
    CASE HomeKey
	 AChar% = 1
	 CCol% = LCol%
    CASE EndKey
	 AChar% = LnLen% + 1
	 CCol% = Lesser(LCol% + LnLen%, RCol%)
    CASE InsertKey
	 Insert% = NOT Insert%
	 IF Insert% THEN
	   LOCATE , , , CursSize% - 1, CursSize%
	 ELSE
	   LOCATE , , , CursSize% \ 2, CursSize%
	 END IF
    CASE DeleteKey
	 IF AChar% <= LnLen% THEN
	   Work$ = LEFT$(Work$, AChar% - 1) + MID$(Work$, AChar% + 1)
	 END IF
    CASE ELSE
	 BEEP: BadKey% = KeyCode%
  END SELECT
 LOOP
 LOCATE , , , CursSize% - 1, CursSize%

END SUB

 FUNCTION LegalFileName% (FileName$)
 DIM XRegister AS RegTypeX
 LegalFileName% = False
 IF FileExists%(FileName$) THEN
  LegalFileName% = True
 ELSE
  'attempt to create the file
  Temp$ = FileName$ + CHR$(0)
  XRegister.AX = &H3C00
  XRegister.CX = 0
  XRegister.DS = VARSEG(Temp$)
  XRegister.DX = SADD(Temp$)
  CALL InterruptX(&H21, XRegister, XRegister)
  IF (XRegister.Flags AND 1) = 0 THEN
    'if carry flag is clear, then file was created OK
    LegalFileName% = True
    'delete the file
    XRegister.AX = &H4100
    CALL InterruptX(&H21, XRegister, XRegister)
  END IF
 END IF

 END FUNCTION

'Define static varibles
 FUNCTION Lesser% (Var1%, Var2%) STATIC
 Lesser% = Var1%: IF Var2% < Var1% THEN Lesser% = Var2%
 END FUNCTION

SUB MsgBox (Msg$, ValidKeys$, KeyCode%)

 W% = LEN(Msg$) + 8
 M% = ScrnWidth% - 6
 IF W% > M% THEN W% = M% - 4
 D% = (ScrnWidth% - W%) \ 2
 COLOR BoxFG%, BoxBG%
 CALL DrawBox(MidRow% - 1, 3, LeftCol% + D%, ScrnWidth% - (D% * 2))
 LOCATE MidRow%, LeftCol% + D% + 4: PRINT LEFT$(Msg$, W%);
 COLOR FG%, BG%

 DO WHILE LEN(INKEY$): LOOP
 IF LEN(ValidKeys$) THEN
  LOCATE , , 1
  DO
    DO: VK$ = INKEY$: LOOP UNTIL LEN(VK$)
    IF ASC(VK$) = ESC THEN EXIT DO
    IF INSTR(ValidKeys$, VK$) THEN EXIT DO ELSE BEEP
  LOOP
  LOCATE , , 0
  KeyCode% = ASC(VK$)
 ELSE
  DO UNTIL LEN(INKEY$): LOOP
 END IF
END SUB

SUB SetVGABorderColor (Colour%)
 'Colour must be in the range 0 - 255
 DIM Register AS RegType
 Register.AX = &H1001
 Register.BX = Colour% * &H100
 CALL Interrupt(&H10, Register, Register)
END SUB

