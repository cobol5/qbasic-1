'±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
'± Copyright (C) HiTech Laboratories, Denis Andrianov                       ±
'± Search Utility for WinDos                                                ±
'±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

DEFINT A-Z

'$INCLUDE: 'dos.bi'

DECLARE FUNCTION Mouse.Init% ()
DECLARE SUB Mouse.Status (Lb%, Rb%, xMouse%, yMouse%)
DECLARE SUB Mouse.Range (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%)
DECLARE SUB Mouse.Put (BYVAL x%, BYVAL y%)
DECLARE SUB Mouse.Hide ()
DECLARE SUB Mouse.Show ()
DECLARE SUB Mouse.Speed (BYVAL XSpeed%, BYVAL YSpeed%)

DECLARE FUNCTION Button.Create% (Text$, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL TextCol%, BYVAL Colour%, BYVAL TopCol%, BYVAL BotCol%, BYVAL LightCol%, BYVAL ButType%, BYVAL ShowBut%)
DECLARE FUNCTION Button.Down% (BYVAL Handle%, BYVAL xMouse%, BYVAL yMouse%, BYVAL Click%)
DECLARE FUNCTION Button.Pressed% (BYVAL Handle%, BYVAL xMouse%, BYVAL yMouse%, BYVAL Click%)
DECLARE FUNCTION Button.MouseOver% (BYVAL Handle%, BYVAL xMouse%, BYVAL yMouse%)
DECLARE SUB Button.Draw (BYVAL Handle%)

DECLARE FUNCTION Tick.Create% (BYVAL x%, BYVAL y%, BYVAL TickType%, BYVAL State%, BYVAL ShowTick%)
DECLARE FUNCTION Tick.Pressed% (BYVAL Handle%, BYVAL xMouse%, BYVAL yMouse%, BYVAL Click%)
DECLARE FUNCTION Tick.MouseOver% (BYVAL Handle%, BYVAL xMouse%, BYVAL yMouse%)
DECLARE SUB Tick.Draw (BYVAL Handle%)

'NOTE: SLOW! TextPnl object heavily relies on the font routines which are SLOW.
'SOLUTION: Optimise this object or the font routines (or both).

DECLARE FUNCTION TextPnl.Add% (BYVAL Handle%, Text$)
DECLARE FUNCTION TextPnl.Create% (BYVAL x%, BYVAL y%, BYVAL xItems%, BYVAL yItems%, BYVAL HiCol%, BYVAL TextCol%)
DECLARE FUNCTION TextPnl.Items% (BYVAL Handle%)
DECLARE FUNCTION TextPnl.Remove% (BYVAL Handle%, BYVAL Item%)
DECLARE FUNCTION TextPnl.Selected$ (BYVAL Handle%)
DECLARE SUB TextPnl.Show (BYVAL Handle%)
DECLARE SUB TextPnl.Update (BYVAL Handle%, Key$)

DECLARE FUNCTION TextBox.Create% (BYVAL x%, BYVAL y%, Text$, BYVAL WrapPos%, BYVAL TextCol%, BYVAL TextColStep%, BYVAL BackCol%, BYVAL BoxType%, BYVAL Enabled%, BYVAL Active%, BYVAL ShowText%)
DECLARE FUNCTION TextBox.Text$ (BYVAL Handle%, BYVAL xMouse%, BYVAL yMouse%, BYVAL Click%, Key$)
DECLARE FUNCTION TextBox.TextOnly$ (BYVAL Handle%)
DECLARE FUNCTION TextBox.MouseOver% (BYVAL Handle%, BYVAL xMouse%, BYVAL yMouse%)
DECLARE SUB TextBox.Draw (BYVAL Handle%)
DECLARE SUB TextBox.DrawChar (BYVAL Handle%, Char$, BYVAL TextLen%)
DECLARE SUB TextBox.AddChar (BYVAL Handle%, Char$)
DECLARE SUB TextBox.RemChar (BYVAL Handle%)

DECLARE SUB Pal.Set (Attribute%, Red%, Green%, Blue%)

'Font routines are SLOW!!! Please rewrite us in ASM.

DECLARE SUB Font.Text (Text$, BYVAL TextX%, BYVAL TextY%, BYVAL Colour%)
DECLARE SUB Font.TextS (Text$, BYVAL TextX%, BYVAL TextY%, BYVAL Colour%)
DECLARE SUB Font.TextX (Text$, BYVAL TextX%, BYVAL TextY%, ColourX%, ColourY%, BYVAL StepColourX%, BYVAL StepColourY%)
DECLARE FUNCTION Font.xCenter% (Text$, BYVAL xLeft%, BYVAL xRight%)
DECLARE FUNCTION Font.yCenter% (BYVAL yUpper%, BYVAL yLower%)


DECLARE SUB Interface (Config AS ANY)
DECLARE SUB File2Video (Filename$)

DECLARE FUNCTION ReadDir% (DirSpec$, BYVAL SearchType%, BYVAL Filter%)
DECLARE FUNCTION Search% (Directory$, Masks$())

DECLARE SUB INI.Read (Config AS ANY)
DECLARE FUNCTION AddSlash$ (Strng$)


'±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
'Fonts definitions.

CONST CharLength = 8
CONST CharHeight = 8

'   ----------      -
'   --######--       |
'   -#------#-       |
'   -#--------       | Character height
'   -#--------       |
'   -#------#-       |
'   --######--       |
'   ----------      -
'
'   |        |
'    --------
' Character length


'±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
'Buttons definitions.


CONST MaxButtonTextLen = 20
CONST MaxButtonNum = 4

TYPE ButtonsStruc
	x1       AS INTEGER
	y1       AS INTEGER
	x2       AS INTEGER
	y2       AS INTEGER
	Text     AS STRING * MaxButtonTextLen
	TextCol  AS INTEGER
	Colour   AS INTEGER
	TopCol   AS INTEGER
	BotCol   AS INTEGER
	LightCol AS INTEGER
	ButType  AS INTEGER
	State    AS INTEGER         '1 when pressed, 0 when released
END TYPE


'Col stands for colour!
'                      _________ TopCol
'                     
'             + -------- +
' TopCol -->  |          |  <--- BotCol
'             + -------- +
'                     _________ BotCol
'


REDIM SHARED Buttons(1 TO MaxButtonNum) AS ButtonsStruc


'±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
'Tick box definitions.


CONST MaxTickNum = 15
CONST TickLength = 20
CONST TickWidth = 20

TYPE TickStruc
	x        AS INTEGER
	y        AS INTEGER
	TickType AS INTEGER
	State    AS INTEGER
END TYPE


'State bits
' 0 0 0 0 0 0 x x
'               |______ 1 when tick is set, 0 when reset
'             |________ 1 when held down, 0 when released

REDIM SHARED Ticks(1 TO MaxTickNum) AS TickStruc


'±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
'Text box definitions.


CONST MaxTextBoxTextLen = 256         'Actual length = MaxTextBoxTextLen - 1 !
CONST MaxTextBoxNum = 15
CONST TextActiveCol = 15
CONST Cursor = "_"


TYPE TextStruc
	x           AS INTEGER
	y           AS INTEGER
	x2          AS INTEGER
	y2          AS INTEGER
	Text        AS STRING * MaxTextBoxTextLen
' Position    AS INTEGER              'Position of cursor.
	WrapPos     AS INTEGER              'Num of chars to be displayed
	TextCol     AS INTEGER              'Standart text colour
	TextColStep AS INTEGER              'Colour step for transition from TextCol to TextCol2 (gradient effect)
	BackCol     AS INTEGER
	BoxType     AS INTEGER
	Enabled     AS INTEGER
END TYPE

DIM SHARED TextActiveHandle%          'Active text box handle.
DIM SHARED TextBoxHandleCounter%      'Used for automatic handle assignment.

REDIM SHARED Texts(1 TO MaxTextBoxNum) AS TextStruc


'±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
'Text panels definitions.

CONST MaxTextPnlNum = 1
CONST MaxTextPnlLines = 50
																			

TYPE TextPnlStruc
	x           AS INTEGER
	y           AS INTEGER
	xItems      AS INTEGER
	yItems      AS INTEGER
	ScrollPos   AS INTEGER
	Position    AS INTEGER
	HiCol       AS INTEGER
	TextCol     AS INTEGER
END TYPE

REDIM SHARED TextPanel(1 TO MaxTextPnlNum) AS TextPnlStruc
REDIM SHARED TextPanelText$(1 TO MaxTextPnlNum, 1 TO MaxTextPnlLines)

DIM SHARED TextPanelActive%


'±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
'Other

TYPE SearchInfoStructure
	Recursive     AS INTEGER
	Attrib        AS INTEGER
	SizeMinEn     AS INTEGER
	SizeMin       AS LONG
	SizeMaxEn     AS INTEGER
	SizeMax       AS LONG
	AccessDateEn  AS INTEGER
	AccessDate1   AS STRING * 8
	AccessDate2   AS STRING * 8
	CreateDateEn  AS INTEGER
	CreateDate1   AS STRING * 8
	CreateDate2   AS STRING * 8
	CreateTimeEn  AS INTEGER
	CreateTime1   AS STRING * 8
	CreateTime2   AS STRING * 8
	StrSearchEn   AS INTEGER
	CaseSensitive AS INTEGER
	TextStr       AS STRING * 254
	Dirs(20)      AS STRING * 64       'Base directories to search in.
	Masks(20)     AS STRING * 12       'Masks to search for.
END TYPE

TYPE ConfigStruc
	GraphicsPath       AS STRING * 77
	PalPath            AS STRING * 77
	StartDir           AS STRING * 77
	PalFile            AS STRING * 12
	MouseXSpeed        AS INTEGER
	MouseYSpeed        AS INTEGER
END TYPE


DIM SHARED Regs AS RegTypeX
DIM SHARED DTA AS DTAStructure

DIM SearchInfo AS SearchInfoStructure
DIM Config AS ConfigStruc


CONST MainINI = "windos.ini"

CONST FindFirst = &H4E00             'Functions of interrupt 21h.
CONST FindNext = &H4F00

CONST ReadOnlyAtt = 1                '(0000 0001) Read-Only
CONST HiddenAtt = 2                  '(0000 0010) Hidden
CONST SystemAtt = 4                  '(0000 0100) System
CONST DirAtt = 16                    '(0001 0000) Sub-directory
CONST ArchiveAtt = 32                '(0010 0000) Archive
CONST AllFilesAtt = 55


'±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
'±±±±±±±±±±±±±±±±±±±±±±±±±±±±± CODE BEGINS HERE ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
'±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±


'**********************
'Test for mouse.
'**********************

	IF NOT (Mouse.Init) THEN
	PRINT "Mouse driver not loaded."
	END
	END IF


'*************************
'Deal with configuration.
'*************************

INI.Read Config


'*************************
'Initial values...
'*************************

SearchInfo.Recursive = -1

Dirs$ = RTRIM$(Config.StartDir)
Masks$ = "*.*"

SearchInfo.Attrib = DirAtt OR ArchiveAtt

SearchInfo.AccessDateEn = 0
SearchInfo.CreateDateEn = 0
SearchInfo.CreateTimeEn = 0

SearchInfo.StrSearchEn = 0
SearchInfo.CaseSensitive = 0
SearchInfo.TextStr = "String..."


'**********************
'Draw the interface.
'**********************

Interface Config

TextHandle% = TextBox.Create%(30, 380, RTRIM$(SearchInfo.TextStr), 39, 5, 1, 0, 0, -1, 0, -1)
DirsHandle% = TextBox.Create%(30, 113, Dirs$, 60, 5, 1, 0, 0, -1, 0, -1)
MasksHandle% = TextBox.Create%(30, 173, Masks$, 71, 5, 1, 0, 0, -1, -1, -1)
RecursiveHandle% = Tick.Create%(520, 107, 0, SearchInfo.Recursive AND 1, -1)

DAttHandle% = Tick.Create%(28, 232, 0, ((SearchInfo.Attrib AND DirAtt) = DirAtt) AND 1, -1)
AAttHandle% = Tick.Create%(28, 253, 0, ((SearchInfo.Attrib AND ArchiveAtt) = ArchiveAtt) AND 1, -1)
RAttHandle% = Tick.Create%(28, 274, 0, ((SearchInfo.Attrib AND ReadOnlyAtt) = ReadOnlyAtt) AND 1, -1)
HAttHandle% = Tick.Create%(28, 295, 0, ((SearchInfo.Attrib AND HiddenAtt) = HiddenAtt) AND 1, -1)
SAttHandle% = Tick.Create%(28, 316, 0, ((SearchInfo.Attrib AND SystemAtt) = SystemAtt) AND 1, -1)

SmallerHandle% = Tick.Create%(200, 232, 0, SearchInfo.SizeMinEn AND 1, -1)
BiggerHandle% = Tick.Create%(200, 253, 0, SearchInfo.SizeMaxEn AND 1, -1)

AccessDateTickHandle% = Tick.Create%(200, 274, 0, SearchInfo.AccessDateEn AND 1, -1)
CreationDateTickHandle% = Tick.Create%(200, 295, 0, SearchInfo.CreateDateEn AND 1, -1)
CreationTimeTickHandle% = Tick.Create%(200, 316, 0, SearchInfo.CreateTimeEn AND 1, -1)

LowBytesHandle% = TextBox.Create%(300, 237, "0", 11, 5, 1, 0, 0, -1, 0, -1)
HiBytesHandle% = TextBox.Create%(300, 258, "0", 11, 5, 1, 0, 0, -1, 0, -1)

AccessDateFromHandle% = TextBox.Create%(300, 279, "dd:mm:yy", 9, 5, 1, 0, 0, -1, 0, -1)
AccessDateToHandle% = TextBox.Create%(400, 279, "dd:mm:yy", 9, 5, 1, 0, 0, -1, 0, -1)

CreationDateFromHandle% = TextBox.Create%(300, 300, "dd:mm:yy", 9, 5, 1, 0, 0, -1, 0, -1)
CreationDateToHandle% = TextBox.Create%(400, 300, "dd:mm:yy", 9, 5, 1, 0, 0, -1, 0, -1)

CreationTimeFromHandle% = TextBox.Create%(300, 321, "hh:mm:ss", 9, 5, 1, 0, 0, -1, 0, -1)
CreationTimeToHandle% = TextBox.Create%(400, 321, "hh:mm:ss", 9, 5, 1, 0, 0, -1, 0, -1)

EnStrSearchTickHandle% = Tick.Create%(370, 358, 0, SearchInfo.StrSearchEn AND 1, -1)
SensitiveTickHandle% = Tick.Create%(370, 380, 0, SearchInfo.CaseSensitive AND 1, -1)

NewButHandle% = Button.Create%("New", 395, 425, 445, 460, 13, 0, 15, 4, 0, 0, -1)
StartButHandle% = Button.Create%("Start", 450, 425, 500, 460, 13, 0, 15, 4, 0, 0, -1)
GoToButHandle% = Button.Create%("Go to", 505, 425, 555, 460, 13, 0, 15, 4, 0, 0, -1)
ExitButHandle% = Button.Create%("Exit", 560, 425, 610, 460, 13, 0, 15, 4, 0, 0, -1)


FilePnlHandle% = TextPnl.Create%(515, 235, 12, 20, 15, 13)

	FOR i% = 1 TO 30
	Temp% = TextPnl.Add%(FilePnlHandle%, "12345678.123")
	NEXT

TextPnl.Show FilePnlHandle%


'**********************
'Configure the mouse.
'**********************

Mouse.Range 0, 0, 638, 478
Mouse.Speed Config.MouseXSpeed, Config.MouseYSpeed
Mouse.Put 320, 240
Mouse.Show

'±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
'±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±± MAIN LOOP ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
'±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±


	DO
	Mouse.Status Lb%, Rb%, xMouse%, yMouse%

	SearchInfo.TextStr = TextBox.Text(TextHandle%, xMouse%, yMouse%, Lb%, Key$)
	Dirs$ = TextBox.Text(DirsHandle%, xMouse%, yMouse%, Lb%, Key$)
	Masks$ = TextBox.Text(MasksHandle%, xMouse%, yMouse%, Lb%, Key$)
	SearchInfo.Recursive = Tick.Pressed(RecursiveHandle%, xMouse%, yMouse%, Lb%)

'************continue here
	DAtt% = Tick.Pressed(DAttHandle%, xMouse%, yMouse%, Lb%)
	AAtt% = Tick.Pressed(AAttHandle%, xMouse%, yMouse%, Lb%)
	RAtt% = Tick.Pressed(RAttHandle%, xMouse%, yMouse%, Lb%)
	HAtt% = Tick.Pressed(HAttHandle%, xMouse%, yMouse%, Lb%)
	SAtt% = Tick.Pressed(SAttHandle%, xMouse%, yMouse%, Lb%)

	SmallerTick% = Tick.Pressed(SmallerHandle%, xMouse%, yMouse%, Lb%)
	BiggerTick% = Tick.Pressed(BiggerHandle%, xMouse%, yMouse%, Lb%)

	AccessDateTick% = Tick.Pressed(AccessDateTickHandle%, xMouse%, yMouse%, Lb%)
	CreationDateTick% = Tick.Pressed(CreationDateTickHandle%, xMouse%, yMouse%, Lb%)
	CreationTimeTick% = Tick.Pressed(CreationTimeTickHandle%, xMouse%, yMouse%, Lb%)

	LowBytes$ = TextBox.Text(LowBytesHandle%, xMouse%, yMouse%, Lb%, Key$)
	HiBytes$ = TextBox.Text(HiBytesHandle%, xMouse%, yMouse%, Lb%, Key$)

	AccessDateFrom$ = TextBox.Text(AccessDateFromHandle%, xMouse%, yMouse%, Lb%, Key$)
	AccessDateTo$ = TextBox.Text(AccessDateToHandle%, xMouse%, yMouse%, Lb%, Key$)

	CreationDateFrom$ = TextBox.Text(CreationDateFromHandle%, xMouse%, yMouse%, Lb%, Key$)
	CreationDateTo$ = TextBox.Text(CreationDateToHandle%, xMouse%, yMouse%, Lb%, Key$)

	CreationTimeFrom$ = TextBox.Text(CreationTimeFromHandle%, xMouse%, yMouse%, Lb%, Key$)
	CreationTimeTo$ = TextBox.Text(CreationTimeToHandle%, xMouse%, yMouse%, Lb%, Key$)

	EnStrSearchTick% = Tick.Pressed(EnStrSearchTickHandle%, xMouse%, yMouse%, Lb%)
	SensitiveTick% = Tick.Pressed(SensitiveTickHandle%, xMouse%, yMouse%, Lb%)

	NewBut% = Button.Pressed%(NewButHandle%, xMouse%, yMouse%, Lb%)
	StartBut% = Button.Pressed%(StartButHandle%, xMouse%, yMouse%, Lb%)
	GoToBut% = Button.Pressed%(GoToButHandle%, xMouse%, yMouse%, Lb%)
	ExitBut% = Button.Pressed%(ExitButHandle%, xMouse%, yMouse%, Lb%)

	TextPnl.Update FilePnlHandle%, Key$


	'**********************
	'Check buttons...
	'**********************

		IF ExitBut% THEN
		EXIT DO
		END IF


	LOOP UNTIL Key$ = CHR$(27)




Mouse.Hide

DEFSNG A-Z
FUNCTION AddSlash$ (Strng$)

	IF RIGHT$(Strng$, 1) <> "\" THEN
		AddSlash$ = Strng$ + "\"
	ELSE
		AddSlash$ = Strng$
	END IF

END FUNCTION

DEFINT A-Z
'Function returns:  handle of the button     if successful
'                   0                        if error
'
'ButType% = 0  transparent thin button
'         = 1  transparent thin button with a light indicator
'         = 2  standard non-transparent thin button
'         = 3  transparent fat button
'
'
FUNCTION Button.Create% (Text$, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL TextCol%, BYVAL Colour%, BYVAL TopCol%, BYVAL BotCol%, BYVAL LightCol%, BYVAL ButType%, BYVAL ShowBut%)

STATIC HandleCounter%


	IF HandleCounter% < MaxButtonNum THEN
	HandleCounter% = HandleCounter% + 1

	Buttons(HandleCounter%).x1 = x1%
	Buttons(HandleCounter%).y1 = y1%
	Buttons(HandleCounter%).x2 = x2%
	Buttons(HandleCounter%).y2 = y2%
	Buttons(HandleCounter%).Text = LEFT$(Text$, MaxButtonTextLen)
	Buttons(HandleCounter%).TextCol = TextCol%
	Buttons(HandleCounter%).Colour = Colour%
	Buttons(HandleCounter%).TopCol = TopCol%
	Buttons(HandleCounter%).BotCol = BotCol%
	Buttons(HandleCounter%).LightCol = LightCol%
	Buttons(HandleCounter%).ButType = ButType%
	Buttons(HandleCounter%).State = 0

		IF ShowBut% THEN
		Button.Draw HandleCounter%
		END IF

	Button.Create% = HandleCounter%

	END IF


END FUNCTION

'Returns:  0   if the button is being released
'         -1   if the button is being pressed
'
'
FUNCTION Button.Down% (BYVAL Handle%, BYVAL xMouse%, BYVAL yMouse%, BYVAL Click%)

OldState% = Buttons(Handle%).State
Buttons(Handle%).State = 0


	IF Button.MouseOver%(Handle%, xMouse%, yMouse%) THEN
		IF Click% THEN
		Buttons(Handle%).State = 1
		END IF
	END IF

	IF OldState% <> Buttons(Handle%).State THEN
	Button.Draw Handle%
	END IF


Button.Down% = (Buttons(Handle%).State <> 0)

END FUNCTION

SUB Button.Draw (BYVAL Handle%)

x1% = Buttons(Handle%).x1            'Using arrays in calculations requires
y1% = Buttons(Handle%).y1            'more code!
x2% = Buttons(Handle%).x2
y2% = Buttons(Handle%).y2
Text$ = Buttons(Handle%).Text
TextCol% = Buttons(Handle%).TextCol
Colour% = Buttons(Handle%).Colour

	IF Buttons(Handle%).State = 1 THEN
		TopCol% = Buttons(Handle%).BotCol
		BotCol% = Buttons(Handle%).TopCol
	ELSE
		TopCol% = Buttons(Handle%).TopCol
		BotCol% = Buttons(Handle%).BotCol
	END IF

LightCol% = Buttons(Handle%).LightCol
ButType% = Buttons(Handle%).ButType

'----------------------------------------------------------------------------

Mouse.Hide


	IF ButType% = 3 THEN
	LINE (x1%, y1%)-(x2%, y2%), 0, B
	x1% = x1% + 1
	y1% = y1% + 1
	x2% = x2% - 1
	y2% = y2% - 1
	END IF


LINE (x1%, y1%)-(x2%, y2%), BotCol%, B
LINE (x1%, y1%)-(x2%, y1%), TopCol%
LINE (x1%, y1%)-(x1%, y2%), TopCol%


	IF ButType% = 1 THEN

		CONST LightLen = 5
		x3% = x1% + LightLen
		y3% = y1% + LightLen
		LINE (x1%, y1%)-(x3%, y3%), BotCol%, B
		LINE (x1%, y3%)-(x3%, y3%), TopCol%
		LINE (x3%, y1%)-(x3%, y3%), TopCol%
		LINE (x1% + 1, y1% + 1)-(x3% - 1, y3% - 1), LightCol%, BF

	ELSEIF ButType% = 2 THEN

		LINE (x1% + 1, y1% + 1)-(x2% - 1, y2% - 1), Colour%, BF

	END IF


Text$ = LTRIM$(RTRIM$(Text$))
Font.Text Text$, Font.xCenter(Text$, x1%, x2%), Font.yCenter(y1%, y2%), TextCol%


Mouse.Show

END SUB

FUNCTION Button.MouseOver% (BYVAL Handle%, BYVAL xMouse%, BYVAL yMouse%)

	IF xMouse% >= Buttons(Handle%).x1 THEN
		IF xMouse% <= Buttons(Handle%).x2 THEN
			IF yMouse% >= Buttons(Handle%).y1 THEN
				IF yMouse% <= Buttons(Handle%).y2 THEN

				Button.MouseOver% = -1

				END IF
			END IF
		END IF
	END IF

END FUNCTION

'Returns:  -1  if the button was pressed and released
'           0  otherwise
'
'
FUNCTION Button.Pressed% (BYVAL Handle%, BYVAL xMouse%, BYVAL yMouse%, BYVAL Click%)


OldState% = Buttons(Handle%).State


	IF NOT (Button.Down(Handle%, xMouse%, yMouse%, Click%)) THEN
		IF NOT (Click%) THEN
			IF OldState% THEN
			Button.Pressed% = -1
			END IF
		END IF
	END IF


END FUNCTION

'Procedure extracts comma separated strings from Text$ and places them
'into Array$().
SUB ExtractText (Text$, Array$())

StringCount% = 0
Comma% = 0


	DO
	PrevComma% = Comma%
	Comma% = INSTR(PrevComma% + 1, Text$, ",")

		IF Comma% > 0 THEN
			Array$(StringCount%) = RTRIM$(LTRIM$(MID$(Text$, PrevComma% + 1, Comma% - PrevComma% - 1)))
		ELSE
			Array$(StringCount%) = RTRIM$(LTRIM$(MID$(Text$, PrevComma% + 1)))
			EXIT DO
		END IF

	StringCount% = StringCount% + 1
	LOOP

END SUB

SUB File2Video (Filename$)

Buffer$ = SPACE$(19200)
BufSeg% = SSEG(Buffer$)
BufOff% = SADD(Buffer$)


File% = FREEFILE
OPEN Filename$ FOR BINARY AS #File%

SEEK #File%, 8      'Skip the BSAVE header.


GET #File%, , Buffer$

	FOR ByteNum% = 0 TO 19200 - 1
	DEF SEG = BufSeg%
	Byte% = PEEK(BufOff% + ByteNum%)
	DEF SEG = &HA000
	POKE ByteNum%, Byte%
	NEXT

GET #File%, , Buffer$

	FOR ByteNum% = 0 TO 19200 - 1
	DEF SEG = BufSeg%
	Byte% = PEEK(BufOff% + ByteNum%)
	DEF SEG = &HA4B0
	POKE ByteNum%, Byte%
	NEXT


CLOSE #File%
DEF SEG

END SUB

'Standard transparent font routine.
'
'
SUB Font.Text (Text$, BYVAL TextX%, BYVAL TextY%, BYVAL Colour%)

'INT 10 - VIDEO - GET FONT INFORMATION (EGA, MCGA, VGA)
' AX = 1130h
' BH = pointer specifier
'     00h INT 1Fh pointer
'     01h INT 43h pointer
'     02h ROM 8x14 character font pointer
'     03h ROM 8x8 double dot font pointer
'     04h ROM 8x8 double dot font (high 128 characters)
'     05h ROM alpha alternate (9 by 14) pointer (EGA,VGA)
'     06h ROM 8x16 font (MCGA, VGA)
'     07h ROM alternate 9x16 font (VGA only) (see #00021)
'     11h (UltraVision v2+) 8x20 font (VGA) or 8x19 font (autosync EGA)
'     12h (UltraVision v2+) 8x10 font (VGA) or 8x11 font (autosync EGA)
'Return: ES:BP = specified pointer
' CX    = bytes/character of on-screen font (not the requested font!)
' DL    = highest character row on screen
'Note:  for UltraVision v2+, the 9xN alternate fonts follow the corresponding
'   8xN font at ES:BP+256N
'BUG: the IBM EGA and some other EGA cards return in DL the number of rows on
'   screen rather than the highest row number (which is one less).
'SeeAlso: AX=1100h,AX=1103h,AX=1120h,INT 1F"SYSTEM DATA",INT 43"VIDEO DATA"
'
'Format of alternate font table [array]:
'Offset Size  Description (Table 00021)
' 00h BYTE  character to be replaced (00h = end of table)
' 01h  N BYTEs  graphics data for character, one byte per scan line

Regs.ax = &H1130
Regs.bx = &H300
CALL INTERRUPTX(&H10, Regs, Regs)

DEF SEG = Regs.es


	FOR LetterNum% = 1 TO LEN(Text$)
	LetterOffset% = Regs.bp + ASC(MID$(Text$, LetterNum%, 1)) * 8
	xPosition% = TextX% + (LetterNum% - 1) * 8
	yPosition% = TextY%

		FOR LineNum% = 0 TO 7
		ScanLine% = PEEK(LetterOffset% + LineNum%)

			IF ScanLine% AND 128 THEN
			PSET (xPosition%, yPosition%), Colour%
			END IF

			IF ScanLine% AND 64 THEN
			PSET (xPosition% + 1, yPosition%), Colour%
			END IF

			IF ScanLine% AND 32 THEN
			PSET (xPosition% + 2, yPosition%), Colour%
			END IF

			IF ScanLine% AND 16 THEN
			PSET (xPosition% + 3, yPosition%), Colour%
			END IF

			IF ScanLine% AND 8 THEN
			PSET (xPosition% + 4, yPosition%), Colour%
			END IF

			IF ScanLine% AND 4 THEN
			PSET (xPosition% + 5, yPosition%), Colour%
			END IF

			IF ScanLine% AND 2 THEN
			PSET (xPosition% + 6, yPosition%), Colour%
			END IF

			IF ScanLine% AND 1 THEN
			PSET (xPosition% + 7, yPosition%), Colour%
			END IF

		yPosition% = yPosition% + 1
		NEXT

	NEXT


DEF SEG

END SUB

DEFSNG A-Z
'Standard non-transparent font routine (solid).
'
'
SUB Font.TextS (Text$, BYVAL TextX%, BYVAL TextY%, BYVAL Colour%)


DEF SEG = &HF000



		FOR LetterNum% = 1 TO LEN(Text$)
		LetterOffset% = &HFA6E + ASC(MID$(Text$, LetterNum%, 1)) * 8
		xPosition% = TextX% + (LetterNum% - 1) * 8
		yPosition% = TextY%

			FOR LineNum% = 0 TO 7
			ScanLine% = PEEK(LetterOffset% + LineNum%) * 256

			xFinal% = xPosition% + 7
			LINE (xPosition%, yPosition%)-(xFinal%, yPosition%), Colour%, , ScanLine%
			LINE (xPosition%, yPosition%)-(xFinal%, yPosition%), 0, , NOT (ScanLine%)

			yPosition% = yPosition% + 1
			NEXT

		NEXT



DEF SEG


END SUB

DEFINT A-Z
'Extended transparent font routine. Supports gradient fonts.
'This sub is a tiny bit slower than Font.Text.
'
'
SUB Font.TextX (Text$, BYVAL TextX%, BYVAL TextY%, ColourX%, ColourY%, BYVAL StepColourX%, BYVAL StepColourY%)

'***********************
'Get font information.
'***********************

Regs.ax = &H1130
Regs.bx = &H300
CALL INTERRUPTX(&H10, Regs, Regs)

DEF SEG = Regs.es

'***********************
'Draw it.
'***********************

ColourYSaved% = ColourY%


	FOR LetterNum% = 1 TO LEN(Text$)
	LetterOffset% = Regs.bp + ASC(MID$(Text$, LetterNum%, 1)) * 8
	xPosition% = TextX% + (LetterNum% - 1) * 8
	yPosition% = TextY%
	ColourX% = ColourX% + 8 * StepColourX%
	ColourY% = ColourYSaved%


		FOR LineNum% = 0 TO 7
		ScanLine% = PEEK(LetterOffset% + LineNum%)
		ColourY% = ColourY% + StepColourY%

		TempColour% = ColourX% + ColourY%

			IF ScanLine% AND 128 THEN
			PSET (xPosition%, yPosition%), TempColour%
			END IF

			IF ScanLine% AND 64 THEN
			PSET (xPosition% + 1, yPosition%), TempColour% + StepColourX%
			END IF

			IF ScanLine% AND 32 THEN
			PSET (xPosition% + 2, yPosition%), TempColour% + 2 * StepColourX%
			END IF

			IF ScanLine% AND 16 THEN
			PSET (xPosition% + 3, yPosition%), TempColour% + 3 * StepColourX%
			END IF

			IF ScanLine% AND 8 THEN
			PSET (xPosition% + 4, yPosition%), TempColour% + 4 * StepColourX%
			END IF

			IF ScanLine% AND 4 THEN
			PSET (xPosition% + 5, yPosition%), TempColour% + 5 * StepColourX%
			END IF

			IF ScanLine% AND 2 THEN
			PSET (xPosition% + 6, yPosition%), TempColour% + 6 * StepColourX%
			END IF

			IF ScanLine% AND 1 THEN
			PSET (xPosition% + 7, yPosition%), TempColour% + 7 * StepColourX%
			END IF

		yPosition% = yPosition% + 1
		NEXT

	NEXT



DEF SEG


END SUB

'Returns:  an x coordinate for text to be printed at for it to be centered.
'
'
FUNCTION Font.xCenter% (Text$, BYVAL xLeft%, BYVAL xRight%)

Font.xCenter% = (xRight% - xLeft% - LEN(Text$) * CharLength) \ 2 + xLeft% + 1

END FUNCTION

'Returns:  an y coordinate for text to be printed at for it to be centered.
'
'
FUNCTION Font.yCenter% (BYVAL yUpper%, BYVAL yLower%)


Font.yCenter% = (yLower% - yUpper% - CharHeight) \ 2 + yUpper%


END FUNCTION

DEFSNG A-Z
SUB INI.Read (Config AS ConfigStruc)

File% = FREEFILE
OPEN MainINI FOR INPUT AS #File%


	DO UNTIL EOF(File%)

	LINE INPUT #File%, Something$
	Something$ = UCASE$(LTRIM$(RTRIM$(Something$)))
	Equal% = INSTR(Something$, "=")

		IF Equal% > 1 THEN
		Value$ = LTRIM$(MID$(Something$, Equal% + 1))
		Value& = VAL(Value$)
		Something$ = RTRIM$(LEFT$(Something$, Equal% - 1))
		END IF

'----------------------------------------------------------------------------
		IF Something$ = "GRAPHICS" THEN
		Config.GraphicsPath = Value$

		ELSEIF Something$ = "PALETTE" THEN
		Config.PalPath = Value$

		ELSEIF Something$ = "SEARCHCOLOURSET" THEN
		Config.PalFile = Value$

		ELSEIF Something$ = "XMOUSE" THEN
		Config.MouseXSpeed = Value&

		ELSEIF Something$ = "YMOUSE" THEN
		Config.MouseYSpeed = Value&

		ELSEIF Something$ = "CURRENTDIR" THEN
		Config.StartDir = Value$

'----------------------------------------------------------------------------
		END IF
	LOOP

CLOSE #File%


END SUB

DEFINT A-Z
SUB Interface (Config AS ConfigStruc)

SCREEN 12
CLS

'********************************
'Set black palette.
'********************************

	FOR i% = 0 TO 15
	Pal.Set i%, 0, 0, 0
	NEXT


'********************************
'Load the interface.
'********************************

GraphicsPathSlash$ = AddSlash$(RTRIM$(Config.GraphicsPath))

OUT &H3C4, 2
OUT &H3C5, 1
File2Video GraphicsPathSlash$ + "search.im1"

OUT &H3C4, 2
OUT &H3C5, 2
File2Video GraphicsPathSlash$ + "search.im2"

OUT &H3C4, 2
OUT &H3C5, 4
File2Video GraphicsPathSlash$ + "search.im3"

OUT &H3C4, 2
OUT &H3C5, 8
File2Video GraphicsPathSlash$ + "search.im4"


'********************************
'Initialize the palette.
'********************************

Red$ = " "
Green$ = Red$
Blue$ = Red$

File% = FREEFILE
OPEN AddSlash$(RTRIM$(Config.PalPath)) + RTRIM$(Config.PalFile) FOR BINARY AS #File%

SEEK #File%, 9&
GET #File%, , TextLen%
SEEK #File%, 9 + 2 + TextLen%

	FOR Colour% = 0 TO 15
	GET #File%, , Red$
	GET #File%, , Green$
	GET #File%, , Blue$
	Pal.Set Colour%, ASC(Red$), ASC(Green$), ASC(Blue$)
	NEXT

CLOSE #File%


END SUB

SUB Mouse.Hide

Regs.ax = 2
CALL Interrupt(&H33, Regs, Regs)

END SUB

FUNCTION Mouse.Init%

Regs.ax = 0
CALL Interrupt(&H33, Regs, Regs)
Mouse.Init% = Regs.ax

END FUNCTION

SUB Mouse.Put (BYVAL x%, BYVAL y%)

Regs.ax = 4
Regs.cx = x%
Regs.dx = y%
CALL Interrupt(&H33, Regs, Regs)

END SUB

SUB Mouse.Range (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%)

Regs.ax = 7
Regs.cx = x1%
Regs.dx = x2%
CALL Interrupt(&H33, Regs, Regs)

Regs.ax = 8
Regs.cx = y1%
Regs.dx = y2%
CALL Interrupt(&H33, Regs, Regs)

END SUB

SUB Mouse.Show

Regs.ax = 1
CALL Interrupt(&H33, Regs, Regs)

END SUB

SUB Mouse.Speed (BYVAL XSpeed%, BYVAL YSpeed%)

Regs.ax = 15
Regs.cx = XSpeed%
Regs.dx = YSpeed%
CALL Interrupt(&H33, Regs, Regs)

END SUB

SUB Mouse.Status (Lb%, Rb%, xMouse%, yMouse%)

Regs.ax = 3
CALL Interrupt(&H33, Regs, Regs)
Lb% = ((Regs.bx AND 1) <> 0)
Rb% = ((Regs.bx AND 2) <> 0)
xMouse% = Regs.cx
yMouse% = Regs.dx

END SUB

SUB Pal.Set (Attribute%, Red%, Green%, Blue%)

OUT &H3C8, Attribute%
OUT &H3C9, Red%
OUT &H3C9, Green%
OUT &H3C9, Blue%

END SUB

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

FUNCTION Search% (Directory$, Masks$())

STATIC StopKey$
STATIC TempMask$
TempMask$ = "*.*"


CalcDirSizeStart:

	IF StopKey$ = CHR$(27) THEN
		CalcDirSize% = 0
		EXIT FUNCTION
	ELSE
		CalcDirSize% = 1
		StopKey$ = INKEY$
	END IF

CHDIR Directory$

	IF ReadDir(TempMask$, FindFirst, 55) THEN

		FOR Counter% = 1 TO FileDirCount%
			IF ReadDir(TempMask$, FindNext, 55) = 0 THEN EXIT FUNCTION
		NEXT

		DO
		File$ = UCASE$(RTRIM$(DTA.Filename))
		FileDirCount% = FileDirCount% + 1

			IF ASC(DTA.Attributes) AND 16 THEN               'This is a directory.
					IF LEFT$(File$, 1) <> "." THEN
					'PRINT AddSlash$(Directory$) + File$
					'Temp% = CalcDirSize(AddSlash(Directory$) + File$)
					GOTO CalcDirSizeStart
					END IF
			ELSE                                             'This is a file.
				SizeOfAllFiles@ = SizeOfAllFiles@ + DTA.FileSize
				NumOfAllFiles@ = NumOfAllFiles@ + 1
			END IF

			IF ReadDir(TempMask$, FindNext, 55) = 0 THEN
			EXIT FUNCTION
			END IF

		LOOP

	END IF


END FUNCTION

'This function updates the screen and the text associated with the handle.
'
'
SUB TextBox.AddChar (BYVAL Handle%, Char$)


Text$ = TextBox.TextOnly$(Handle%)
OldTextLen% = LEN(Text$)


	IF OldTextLen% < MaxTextBoxTextLen - 1 THEN
		Texts(Handle%).Text = Text$ + Char$ + CHR$(0)
		NewTextLen% = OldTextLen% + 1
	ELSE
		EXIT SUB
	END IF


	IF NewTextLen% < Texts(Handle%).WrapPos THEN
		TextBox.DrawChar Handle%, Char$, OldTextLen%
	ELSE
		TextBox.Draw Handle%
	END IF


END SUB

'Function returns:  handle of the text box   if successful
'                   0                        if error
'
'BoxType%           currently undefined (only 1 is available)
'
'
FUNCTION TextBox.Create% (BYVAL x%, BYVAL y%, Text$, BYVAL WrapPos%, BYVAL TextCol%, BYVAL TextColStep%, BYVAL BackCol%, BYVAL BoxType%, BYVAL Enabled%, BYVAL Active%, BYVAL ShowText%)


	IF TextBoxHandleCounter% < MaxTextBoxNum THEN
	TextBoxHandleCounter% = TextBoxHandleCounter% + 1

	Texts(TextBoxHandleCounter%).x = x%
	Texts(TextBoxHandleCounter%).y = y%
	Texts(TextBoxHandleCounter%).x2 = x% + CharLength% * WrapPos% + 2
	Texts(TextBoxHandleCounter%).y2 = y% + CharHeight% + 2
	Texts(TextBoxHandleCounter%).Text = LEFT$(Text$, MaxTextBoxTextLen - 1) + CHR$(0)
	Texts(TextBoxHandleCounter%).WrapPos = WrapPos%
	Texts(TextBoxHandleCounter%).TextCol = TextCol%
	Texts(TextBoxHandleCounter%).TextColStep = TextColStep%
	Texts(TextBoxHandleCounter%).BackCol = BackCol%
	Texts(TextBoxHandleCounter%).BoxType = BoxType%
	Texts(TextBoxHandleCounter%).Enabled = Enabled%

		IF Active% THEN
		TextActiveHandle% = TextBoxHandleCounter%
		END IF

		IF ShowText% THEN
		TextBox.Draw TextBoxHandleCounter%
		END IF

	TextBox.Create% = TextBoxHandleCounter%

	END IF


END FUNCTION

'Displays the TextBox associated with Handle% on the screen.
'
'
SUB TextBox.Draw (BYVAL Handle%)

x% = Texts(Handle%).x
y% = Texts(Handle%).y
x2% = Texts(Handle%).x2
y2% = Texts(Handle%).y2
WrapPos% = Texts(Handle%).WrapPos

'Skip CHR$(0) when extracting text!
Text$ = RIGHT$(TextBox.TextOnly$(Handle%), WrapPos% - 1)


	IF Handle% = TextActiveHandle% THEN
		TextCol% = TextActiveCol

			IF Texts(Handle%).Enabled THEN
			Text$ = Text$ + Cursor
			END IF

	ELSE
		TextCol% = Texts(Handle%).TextCol
		TextColStep% = Texts(Handle%).TextColStep
	END IF

'----------------------------------------------------------------------------
Mouse.Hide


LINE (x%, y%)-(x2%, y2%), Texts(Handle%).BackCol, BF

x% = x% + 1
y% = y% + 1


	IF TextColStep% THEN
		Font.TextX Text$, x%, y%, 0, TextCol%, 0, TextColStep%
	ELSE
		Font.Text Text$, x%, y%, TextCol%
	END IF



Mouse.Show


END SUB

'Displays a character (or text) relative to the TextBox with Handle%.
'TextLen% is the printed text length and NOT the actual string length.
'The character is placed at TextLen% + 1 position from the beginning of the
'TextBox.
'The function is used internally.
'
'
SUB TextBox.DrawChar (BYVAL Handle%, Char$, BYVAL TextLen%)

x% = Texts(Handle%).x + 1 + CharLength * TextLen%
y% = Texts(Handle%).y + 1


	IF Handle% = TextActiveHandle% THEN
		TextCol% = TextActiveCol

			IF Texts(Handle%).Enabled THEN
			Char$ = Char$ + Cursor
			END IF

	ELSE
		TextCol% = Texts(Handle%).TextCol
		TextColStep% = Texts(Handle%).TextColStep
	END IF



Mouse.Hide

LINE (x%, y%)-(x% + 2 * CharLength, y% + CharHeight), Texts(Handle%).BackCol, BF

	IF TextColStep% THEN
		Font.TextX Char$, x%, y%, 0, TextCol%, 0, TextColStep%
	ELSE
		Font.Text Char$, x%, y%, TextCol%
	END IF

Mouse.Show


END SUB

FUNCTION TextBox.MouseOver% (BYVAL Handle%, BYVAL xMouse%, BYVAL yMouse%)

	IF xMouse% >= Texts(Handle%).x THEN
		IF xMouse% <= Texts(Handle%).x2 THEN
			IF yMouse% >= Texts(Handle%).y THEN
				IF yMouse% <= Texts(Handle%).y2 THEN

				TextBox.MouseOver% = -1

				END IF
			END IF
		END IF
	END IF

END FUNCTION

'This function updates the screen and the text associated with the handle.
'
'
SUB TextBox.RemChar (BYVAL Handle%)

Text$ = TextBox.TextOnly$(Handle%)
OldTextLen% = LEN(Text$)


	IF OldTextLen% >= 1 THEN
		Texts(Handle%).Text = LEFT$(Text$, OldTextLen% - 1) + CHR$(0)
	ELSE
		EXIT SUB
	END IF


	IF OldTextLen% < Texts(Handle%).WrapPos THEN
		TextBox.DrawChar Handle%, "", OldTextLen% - 1
	ELSE
		TextBox.Draw Handle%
	END IF


END SUB

'Returns:   the text associated with the Handle%
'
'The control drives the whole TextBox mechanism. Therefore, this function
'must be called in a loop for TextBoxes to operate. DO NOT use INKEY$
'or anything similar anywhere else if you use this function as it will fail.
'Use the Key$ parameter returned by this function instead!
'
'
FUNCTION TextBox.Text$ (BYVAL Handle%, BYVAL xMouse%, BYVAL yMouse%, BYVAL Click%, Key$)

'******************************
'Check for activation.
'******************************

	IF Click% THEN
		IF Handle% <> TextActiveHandle% THEN
			IF TextBox.MouseOver(Handle%, xMouse%, yMouse%) THEN

			Temp% = TextActiveHandle%
			TextActiveHandle% = Handle%
			TextBox.Draw Temp%              'Deactivate the old text box on screen
			TextBox.Draw Handle%            'Activate the new text box on screen

			END IF
		END IF
	END IF



	IF Handle% <> TextActiveHandle% THEN       'Exit if not active.
	GOTO TextBoxTextEnd
	END IF


'**************************************
'Reaction from keyboard if active.
'**************************************

Key$ = INKEY$
KeyLength% = LEN(Key$)



		IF KeyLength% THEN
		Ascii% = ASC(RIGHT$(Key$, 1))


			IF KeyLength% = 1 THEN

					IF Ascii% <> 13 AND Ascii% <> 9 AND Ascii% <> 27 THEN
						IF Texts(Handle%).Enabled THEN

								IF Ascii% = 8 THEN
									TextBox.RemChar Handle%
								ELSE
									TextBox.AddChar Handle%, Key$
								END IF

						END IF
					END IF

'      ELSE
'
'          IF Ascii% = 72 THEN               'Up arrow
'
'              IF TextActiveHandle% > 1 THEN
'              Temp% = TextActiveHandle%
'              TextActiveHandle% = TextActiveHandle% - 1
'              TextBox.Draw Temp%
'              TextBox.Draw TextActiveHandle%
'              END IF
'
'          ELSEIF Ascii% = 80 THEN           'Down arrow
'
'              IF TextActiveHandle% < TextBoxHandleCounter% THEN
'              Temp% = TextActiveHandle%
'              TextActiveHandle% = TextActiveHandle% + 1
'              TextBox.Draw Temp%
'              TextBox.Draw TextActiveHandle%
'              END IF
'
'          END IF

			END IF


		END IF


TextBoxTextEnd:
TextBox.Text = TextBox.TextOnly$(Handle%)

END FUNCTION

'Returns the text which belongs to the text box with the specified
'handle. This function doesn't drive the text box control and is used
'internally !
'
'
FUNCTION TextBox.TextOnly$ (BYVAL Handle%)

Text$ = RTRIM$(Texts(Handle%).Text)
TextBox.TextOnly$ = LEFT$(Text$, LEN(Text$) - 1)         'Remove CHR$(0)!

END FUNCTION

'Function adds an item Text$ to the bottom of the text panel with Handle%.
'
'Function returns:
'                 -1   if successful
'                  0   otherwise
'
FUNCTION TextPnl.Add% (BYVAL Handle%, Text$)

LastItem% = TextPnl.Items%(Handle%)

	IF LastItem% < MaxTextPnlLines THEN
	TextPanelText$(Handle%, LastItem% + 1) = Text$
	TextPnl.Add% = -1
	END IF

END FUNCTION

'Function creates a text panel object.
'Function returns a handle of the new object.
'
FUNCTION TextPnl.Create% (BYVAL x%, BYVAL y%, BYVAL xItems%, BYVAL yItems%, BYVAL HiCol%, BYVAL TextCol%)

STATIC HandleCounter%


	IF HandleCounter% < MaxTextPnlNum THEN
	HandleCounter% = HandleCounter% + 1

	TextPanel(HandleCounter%).x = x%
	TextPanel(HandleCounter%).y = y%
	TextPanel(HandleCounter%).xItems = xItems%
	TextPanel(HandleCounter%).yItems = yItems%
	TextPanel(HandleCounter%).ScrollPos = 1
	TextPanel(HandleCounter%).Position = 1
	TextPanel(HandleCounter%).HiCol = HiCol%
	TextPanel(HandleCounter%).TextCol = TextCol%

	TextPanelActive% = HandleCounter%
	TextPnl.Create% = HandleCounter%
	END IF

END FUNCTION

'Function returns the total number of items belonging to a text panel
'with Handle%.
'
FUNCTION TextPnl.Items% (BYVAL Handle%)

	FOR Lines% = 1 TO MaxTextPnlLines

		IF LEN(TextPanelText$(Handle%, Lines%)) = 0 THEN
		TextPnl.Items% = Lines% - 1
		EXIT FUNCTION
		END IF

	NEXT


TextPnl.Items% = MaxTextPnlLines

END FUNCTION

SUB TextPnl.KillText (BYVAL Handle%)

	FOR i% = 1 TO TextPnl.Items(Handle%)
	TextPanelText$(Handle%, i%) = ""
	NEXT

TextPanel(Handle%).Position = 1
TextPanel(Handle%).ScrollPos = 1

END SUB

'Function removes an item with index Item% from the text panel with Handle%.
'
'Function returns:
'                 -1   if successful
'                  0   otherwise
'
FUNCTION TextPnl.Remove% (BYVAL Handle%, BYVAL Item%)

'*****************************
'Remove the item if possible.
'*****************************

LastItem% = TextPnl.Items%(Handle%)

	IF LastItem% = 0 THEN
	EXIT FUNCTION
	END IF

	IF Item% < 1 OR Item% > LastItem% THEN
	EXIT FUNCTION
	END IF

TextPanelText$(Handle%, Item%) = ""
LastItem% = LastItem% - 1

	FOR i% = Item% TO LastItem%
	SWAP TextPanelText$(Handle%, i%), TextPanelText$(Handle%, i% + 1)
	NEXT

'*********************
'Position the cursor.
'*********************

	IF LastItem% = 0 THEN

		TextPanel(Handle%).Position = 1
		TextPanel(Handle%).ScrollPos = 1

	ELSE

			IF TextPanel(Handle%).Position > LastItem% THEN
			TextPanel(Handle%).Position = LastItem%
			END IF

			IF TextPanel(Handle%).ScrollPos > LastItem% THEN
			TextPanel(Handle%).ScrollPos = LastItem%
			END IF

	END IF

TextPnl.Remove% = -1

END FUNCTION

'Function returns the selected item in a text panel.
'
FUNCTION TextPnl.Selected$ (BYVAL Handle%)

TextPnl.Selected$ = TextPanelText$(Handle%, TextPanel(Handle%).Position)

END FUNCTION

DEFSNG A-Z
'The procedure displays the text panel with Handle% on the screen.
'
SUB TextPnl.Show (BYVAL Handle%)

x% = TextPanel(Handle%).x
y% = TextPanel(Handle%).y
MaxTextLen% = TextPanel(Handle%).xItems
MaxItems% = TextPanel(Handle%).yItems
ScrollPos% = TextPanel(Handle%).ScrollPos


	FOR LinePos% = ScrollPos% TO ScrollPos% + MaxItems% - 1

	Text$ = SPACE$(MaxTextLen%)
	LSET Text$ = LEFT$(TextPanelText$(Handle%, LinePos%), MaxTextLen%)

		IF LinePos% = TextPanel(Handle%).Position THEN
			Col% = TextPanel(Handle%).HiCol
		ELSE
			Col% = TextPanel(Handle%).TextCol
		END IF

	Mouse.Hide
	'Font.TextS Text$, x%, y%, Col%
	Font.Text Text$, x%, y%, Col%
	Mouse.Show

	y% = y% + CharHeight%

	NEXT


END SUB

DEFINT A-Z
'The sub must be run through the main loop. It checks for any key presses
'and updates the text panel on the screen if needed.
'
SUB TextPnl.Update (BYVAL Handle%, Key$)

'******************************
'Check if the panel is active.
'******************************

	IF TextPanelActive% <> Handle% THEN
	EXIT SUB
	END IF

'*******************************
'Initialize commonly used vars.
'*******************************

ScrollPos% = TextPanel(Handle%).ScrollPos
MaxPos% = TextPanel(Handle%).yItems


SELECT CASE RIGHT$(Key$, 1)

'************************
'Upper arrow is pressed.
'************************

	CASE CHR$(72)

		IF TextPanel(Handle%).Position > 1 THEN
		TextPanel(Handle%).Position = TextPanel(Handle%).Position - 1
		GOSUB TextPnlUpRefresh
		END IF

'************************
'Down arrow is pressed.
'************************

	CASE CHR$(80)

		IF TextPanel(Handle%).Position < TextPnl.Items%(Handle%) THEN
		TextPanel(Handle%).Position = TextPanel(Handle%).Position + 1
		GOSUB TextPnlUpRefresh
		END IF

END SELECT


EXIT SUB

'****************************************************************************
'****************************************************************************

TextPnlUpRefresh:

	IF TextPanel(Handle%).Position < ScrollPos% THEN

		TextPanel(Handle%).ScrollPos = TextPanel(Handle%).Position

	ELSEIF TextPanel(Handle%).Position - TextPanel(Handle%).ScrollPos >= MaxPos% THEN

		TextPanel(Handle%).ScrollPos = TextPanel(Handle%).Position - MaxPos% + 1

	END IF

TextPnl.Show Handle%
RETURN


END SUB

'Function returns:  handle of the tick button   if successful
'                   0                           if error
'
FUNCTION Tick.Create% (BYVAL x%, BYVAL y%, BYVAL TickType%, BYVAL State%, BYVAL ShowTick%)

STATIC HandleCounter%


	IF HandleCounter% < MaxTickNum THEN
	HandleCounter% = HandleCounter% + 1

	Ticks(HandleCounter%).x = x%
	Ticks(HandleCounter%).y = y%
	Ticks(HandleCounter%).TickType = TickType%
	Ticks(HandleCounter%).State = State%

		IF ShowTick% THEN
		Tick.Draw HandleCounter%
		END IF

	Tick.Create% = HandleCounter%

	END IF



END FUNCTION

SUB Tick.Draw (BYVAL Handle%)

x% = Ticks(Handle%).x               'Using arrays in calculations requires
y% = Ticks(Handle%).y               'more code!
TickType% = Ticks(Handle%).TickType
State% = Ticks(Handle%).State

'----------------------------------------------------------------------------
Mouse.Hide



	IF State% AND 1 THEN

			FOR Counter% = 13 TO 3 STEP -2
			LINE (x% - Size%, y% - Size%)-(x% + TickLength + Size%, y% + TickWidth + Size%), Counter%, BF
			Size% = Size% - 1
			NEXT

		Tick$ = "f3e7"
		PSET (x% + TickLength% \ 2 - 5, y% + TickWidth \ 2), 13
		DRAW Tick$
		PSET (x% + TickLength% \ 2 - 5, y% + TickWidth \ 2 + 1), 14
		DRAW Tick$

	ELSE

		FOR Counter% = 6 TO 13 STEP 2
		LINE (x% - Size%, y% - Size%)-(x% + TickLength + Size%, y% + TickWidth + Size%), Counter%, BF
		Size% = Size% - 1
		NEXT

	END IF



Mouse.Show


END SUB

FUNCTION Tick.MouseOver% (BYVAL Handle%, BYVAL xMouse%, BYVAL yMouse%)

	IF xMouse% >= Ticks(Handle%).x THEN
		IF xMouse% <= Ticks(Handle%).x + TickLength THEN
			IF yMouse% >= Ticks(Handle%).y THEN
				IF yMouse% <= Ticks(Handle%).y + TickWidth THEN

				Tick.MouseOver% = -1

				END IF
			END IF
		END IF
	END IF

END FUNCTION

'Returns:  -1  if the tick was set
'           0  if the tick was reset
'
'
FUNCTION Tick.Pressed% (BYVAL Handle%, BYVAL xMouse%, BYVAL yMouse%, BYVAL Click%)

OldState% = Ticks(Handle%).State


	IF Click% THEN
			IF (OldState% AND 2) = 0 THEN
				IF Tick.MouseOver(Handle%, xMouse%, yMouse%) THEN
				Ticks(Handle%).State = (OldState% OR 2) XOR 1
				END IF
			END IF
	ELSE
		Ticks(Handle%).State = OldState% AND &HFD
	END IF

	IF (OldState% AND 1) <> (Ticks(Handle%).State AND 1) THEN
	Tick.Draw Handle%
	END IF


Tick.Pressed = ((Ticks(Handle%).State AND 1) = 1)


END FUNCTION

