DECLARE SUB Command (opt%)
DECLARE SUB Adesktop ()
DECLARE SUB fadeout ()
'GO TO END OF THIS PAGE (ALT + S, F, DOCUMENT: ) TO SEE
'THE WINDOZE (Win.bas) INFO.                        |
'THEN, YOU ARE ALLOWED TO REMOVE THE NEXT LINE.      |
    '                                                |
    '                                                |
    END '<----------<------------<-------------------|
DECLARE SUB Cabinet (Size%, dir$, ext$)
DECLARE SUB ShutDown (opt%)
DECLARE SUB Help ()
DECLARE SUB diagnostic ()
DECLARE SUB NoteBook (file$, opt%)
DECLARE SUB AsciiConvert ()
DECLARE SUB pause (opt%)
DECLARE SUB ScreenSaver (opt%)
DECLARE SUB AccessMenuA ()
DECLARE SUB TaskMan (opt%)
DECLARE SUB About ()
DECLARE SUB MaximizeWindow (num%)
DECLARE SUB RestoreWindow (num%)
DECLARE SUB MinimizeWindow (num%)
'$DYNAMIC
DEFINT A-Z
DECLARE SUB Control (opt)
DECLARE SUB CreateBMP (WindowNum%, num%, x%, y%, filename$)
DECLARE SUB LoadBMP (num%)
DECLARE FUNCTION GetBit% (Byte%, Bit%)
DECLARE SUB CreateIcon (WindowNum%, num%, x%, y%, filename$)
DECLARE SUB LoadIco (num%)
DECLARE FUNCTION ReturnNibble% (Byte%, WhichOne%)
DECLARE SUB DoCopy ()
DECLARE SUB Backroundcopy (filename$, Path$)
DECLARE SUB InputDialog (Topic$, Text$, Password%)
DECLARE SUB Dialog (Topic$, Text$, Button1$, Button2$, Button3$)
DECLARE SUB CreateTextBox (Win%, num%, x%, y%, caption$)
DECLARE SUB DrawTextBox (num%, EraseOld)
DECLARE SUB KillCheckBox (num%)
DECLARE SUB KillRadioButton (num%)
DECLARE SUB KillPBar (num%)
DECLARE SUB KillLabel (num%)
DECLARE SUB killTextField (num%)
DECLARE SUB KillScrollbar (num%)
DECLARE SUB killButton (num%)
DECLARE SUB DisableRadioButton (num%)
DECLARE SUB EnableRadioButton (num%)
DECLARE SUB DisableCheckBox (num%)
DECLARE SUB EnableCheckBox (num%)
DECLARE SUB DisableTextField (num%)
DECLARE SUB EnableTextField (num%)
DECLARE SUB DisableButton (num%)
DECLARE SUB EnableButton (num%)
DECLARE SUB killWindow (num%)
DECLARE FUNCTION Now$ ()
DECLARE SUB CenterText (txt$)
DECLARE FUNCTION Trim$ (Number%)
DECLARE FUNCTION MouseInside% (x%, y%, X2%, y2%)
DECLARE SUB CreatePBar (WindowNum%, num%, x%, y%, XLen%, YLen%, Value%, Max%)
DECLARE SUB DrawPercentBar (num%)
DECLARE SUB splash ()
DECLARE SUB CreateFrame (WindowNum, num, x, y, XLen, YLen, caption$)
DECLARE SUB DrawFrame (num)
DECLARE FUNCTION Exist% (file$)
DECLARE SUB Load640 (file$, StartLine!)
DECLARE SUB Save640 (file$, StartLine!)
DECLARE SUB AddMenuItem (WindowNum, Number, caption$)
DECLARE SUB DrawMenuItems (WindowNum, Number)
DECLARE SUB CreateMenu (WindowNum, Number, caption$)
DECLARE SUB DrawMenu (WindowNum)
DECLARE FUNCTION wrap$ (InputText$, wrap)
DECLARE SUB updatemouse ()
DECLARE SUB main ()
DECLARE SUB SpeakerState (OnOff%)
DECLARE SUB WavPlay (file$, Speed!)
DECLARE SUB ResetSB ()
DECLARE SUB WavPlayDMA (file$, Freq&)
DECLARE SUB DMAPlay (Segment&, Offset&, Length&, Freq&)
DECLARE FUNCTION DMADone% ()
DECLARE SUB WriteDSP (Byte%)
DECLARE FUNCTION DSPVersion! ()
DECLARE SUB GetBLASTER (DMA%, BasePort%, IRQ%)
DECLARE FUNCTION ReadDAC% ()
DECLARE FUNCTION ReadDSP% ()
DECLARE FUNCTION ResetDSP% ()
DECLARE FUNCTION SpeakerStatus% ()
DECLARE SUB CreateRadioButton (WindowNum, num, Group, x, y, caption$, State)
DECLARE SUB DrawRadio (num, DrawText)
DECLARE SUB CreateCheckBox (WindowNum, num, x, y, caption$, State)
DECLARE SUB DrawCheckBox (num, DrawText)
DECLARE SUB Switch (Var, Value1, Value2)
DECLARE SUB SetPal (i&, R&, g&, b&)
DECLARE FUNCTION Error$ (Virhe)
DECLARE SUB GPFRuutu ()
DECLARE SUB CreateTextField (WindowNum, num, x, y, Length, txt$, Passwd)
DECLARE SUB DrawTextField (num)
DECLARE SUB DrawScrollBar (num)
DECLARE SUB CreateScrollBar (WindowNum, num, dir, x, y, XLen, YLen, Max, Value)
DECLARE SUB printF (x&, y&, Textz$, FCol%, BCol%)
DECLARE SUB Defscreen ()
DECLARE SUB LabelText (num, caption$, FCol, BCol)
DECLARE SUB CreateLabel (WindowNum, num, x, y, XLen, YLen, caption$, FCol, BCol, BorderStyle, Align, AutoWrap)
DECLARE SUB DrawLabel (num)
DECLARE SUB Box3D (x, y, X2, y2, Style, Fillcol)
DECLARE SUB UpdateScreen ()
DECLARE SUB ReDrawWindow (num)
DECLARE SUB RedDrawWindow (num)
DECLARE SUB xorline (x, y, X2, y2, box%, Pixels)
DECLARE SUB viewW (num)
DECLARE SUB Initscreen ()
DECLARE SUB CreateButton (WindowNum, num, x, y, X2, y2, caption$)
DECLARE SUB DrawButton (num, Pressed, Hollow)
DECLARE SUB CreateWindow (num, x, y, X2, y2, Topic$, Fillcol, WinDrag)
DECLARE SUB DrawWindow (num)
DECLARE SUB GetFont ()
DECLARE SUB Hiirilue (vasen%, oikea%, keski%, x%, y%)
DECLARE SUB Hiirirajat (X1%, y1%, X2%, y2%)
DECLARE SUB HideMouse ()
DECLARE SUB ShowMouse ()
DECLARE FUNCTION Hiiritarkista% ()
DECLARE SUB Hiiriajuri (ax%, bx%, cx%, dx%)
'OPTION BASE 1
'ON ERROR GOTO Virhe
TIMER ON
RANDOMIZE TIMER
temp$ = INKEY$
IF LCASE$(temp$) = "d" THEN diagnostic

CONST none = 0
CONST DoubleIn = 1
CONST DoubleOut = 2
CONST SingleIn = 3
CONST singleOut = 4
CONST left = 0
CONST center = 1
CONST right = 2
CONST true = 1
CONST false = NOT true

'----------------------------------------------------------------------------
'To minimize memory usage, decrease the maximum amount of objects allowed below
'----------------------------------------------------------------------------
' The SUBS Dialog and InputDialog use the last three button tags, the last
' Window tag and the last TextField tag, so don't use the last index numbers
' in your programs if you use either of those SUBS.
'----------------------------------------------------------------------------


CONST MaxWindows = 15
CONST maxbuttons = 20
CONST MaxLabels = 20
CONST MaxScrollBars = 10
CONST MaxTextFields = 10
CONST MaxCheckBoxes = 10
CONST MaxRadioButtons = 25
CONST MaxFrames = 11
CONST MaxPercentBars = 4
CONST MaxTextBoxes = 30
CONST MaxIcons = 50
CONST MaxBMPs = 5

'----------------------------------------------------------------------------
' How many bytes will be copied at one pass when using Backroundcopy
' The larger the number, the faster the copy process and the slower
' the multitasking system. (recommended: 512)
'----------------------------------------------------------------------------
CONST CopyBytes = 512

TIMER ON
KEY 1, "help"
KEY 15, CHR$(&H8) + CHR$(&H3E)  'alt + f4
KEY 16, CHR$(&H4) + CHR$(&H32)  'ctrl + m
KEY 17, CHR$(&H4) + CHR$(&H18)  'ctrl + o
KEY 18, CHR$(&H4) + CHR$(&H21)  'ctrl + o
ON TIMER(1) GOSUB statusbar
ON KEY(15) GOSUB altf4
ON KEY(16) GOSUB ctrlm
ON KEY(17) GOSUB ctrlo
ON KEY(18) GOSUB ctrlf
ON TIMER(5) GOSUB mousefix
KEY(1) ON
KEY(15) ON
KEY(16) ON
KEY(17) ON
KEY(18) ON

'----------------------------------------------------------------------------
'                                 Define types
'----------------------------------------------------------------------------

TYPE VersionType
  Major AS STRING * 1
  Minor AS STRING * 2
END TYPE

TYPE WindowType
  x AS INTEGER
  y AS INTEGER
  X2 AS INTEGER
  y2 AS INTEGER
  TopicCol AS INTEGER
  TopicBCol AS INTEGER
  Fillcol AS INTEGER
  drag AS INTEGER
END TYPE

TYPE Textfieldtype
  x AS INTEGER
  y AS INTEGER
  Max AS INTEGER
  Win AS INTEGER
  Password AS INTEGER
  Disabled AS INTEGER
END TYPE

TYPE ButtonType
  x AS INTEGER
  y AS INTEGER
  X2 AS INTEGER
  y2 AS INTEGER
  Win AS INTEGER
  Disabled AS INTEGER
END TYPE

TYPE ScrollBarType
  dir AS INTEGER
  x AS INTEGER
  y AS INTEGER
  X2 AS INTEGER
  y2 AS INTEGER
  Value AS SINGLE
  Max AS INTEGER
  Win AS INTEGER
  SStep AS SINGLE
  button AS INTEGER
END TYPE

TYPE Labeltype
  x AS INTEGER
  y AS INTEGER
  X2 AS INTEGER
  y2 AS INTEGER
  FCol AS INTEGER
  BCol AS INTEGER
  Win AS INTEGER
  Style AS INTEGER
  Align AS INTEGER
  wrap AS INTEGER
END TYPE

TYPE CheckBoxtype
  Win AS INTEGER
  x AS INTEGER
  y AS INTEGER
  State AS INTEGER
  Disabled AS INTEGER
END TYPE

TYPE RadioButtonType
  Win AS INTEGER
  x AS INTEGER
  y AS INTEGER
  Group AS INTEGER
  State AS INTEGER
  Disabled AS INTEGER
END TYPE


TYPE Menutype
  caption AS STRING * 10
  Items AS INTEGER
END TYPE

TYPE Frametype
  Win AS INTEGER
  x AS INTEGER
  y AS INTEGER
  X2 AS INTEGER
  y2 AS INTEGER
END TYPE

TYPE PBartype
  Win AS INTEGER
  x AS INTEGER
  y AS INTEGER
  X2 AS INTEGER
  y2 AS INTEGER
  Max AS INTEGER
  Value AS SINGLE
  OldValue AS SINGLE
END TYPE

TYPE TextType
  Win AS INTEGER
  x AS INTEGER
  y AS INTEGER
END TYPE

TYPE Icontype
  Win AS INTEGER
  x AS INTEGER
  y AS INTEGER
END TYPE

TYPE BMPHeader
   Ident AS STRING * 2             'Will be 'BM' if this is a bitmap
   FileSize AS LONG                'Size of the file in bytes
                                   'Not really necessary, use LOF instead
   Reserve AS STRING * 4           'Wasted space
   Offset AS LONG                  'Number of bytes from the end of the
                                   'header to the actual data
   Version AS LONG                 '40 - Windows 3.x
                                   '12 - OS/2
                                   'Actually, this is the size of the
                                   '"BMPInfoHeader", which is the entire
                                   'header minus the first four elements,
                                   'Ident to Offset.  These 4 elements are
                                   'actually part of the "BMPFileHeader"
                                   'according to the convuluted specification
   Cols AS LONG                    'Width of image
   Rows AS LONG                    'Height of image
   Planes AS INTEGER               'Number of planes - should be 1
   BitsPerPixel AS INTEGER         'Pretty obvious
'-- Note, the following is for a Windows BMP.  For OS/2, the
'header ends here and is followed by a colormap of BGR values (not RGB!)
   Compression AS LONG             'Type of compression used,
                                   '0 - None
                                   '1 - 8 bit run length encoding
                                   '2 - 4 bit run length encoding
   CompressedSize AS LONG          'Size of image compressed image
   xScale AS LONG                  'Horizontal scale, in pixels per meter
   yScale AS LONG                  'Vertical scale, in pixels per meter
                                   'Not very useful, why such an odd unit?
   Colors AS LONG                  'Number of colors actually used,
                                   'if this is 0 then all possible colors
                                   'are used
   ImportantColors AS LONG         'Number of colors that are considered
                                   '"important."  If 0 then all colors
                                   'are "important."
END TYPE

TYPE BMPType
  Win AS INTEGER
  x AS INTEGER
  y AS INTEGER
END TYPE




'----------------------------------------------------------------------------
'                            Dimension variables
'----------------------------------------------------------------------------
' NOTE: string variables are defined separately, so they're not fixed length.
'----------------------------------------------------------------------------
'Fade
DIM SHARED col(255, 3), col2(255, 3)

'Window
DIM SHARED Win(MaxWindows) AS WindowType
DIM SHARED RestoreW(MaxWindows) AS WindowType
DIM SHARED WinTopic$(MaxWindows)
DIM SHARED aWIn, AText
DIM SHARED key$, saver, Idle
DIM SHARED wx, wy, wx2, wy2
DIM SHARED topica$, AccessPos, AccessTime, AccessHide, Skin, Skinfile$



'Button
DIM SHARED But(maxbuttons + MaxScrollBars) AS ButtonType
DIM SHARED ButCaption$(maxbuttons + MaxScrollBars)


'Scrollbar
DIM SHARED SBar(MaxScrollBars) AS ScrollBarType
DIM SHARED SBOldValuE(1 TO MaxScrollBars)


'Label
DIM SHARED Label(MaxLabels) AS Labeltype
DIM SHARED LabelCaption$(MaxLabels)


'Textfield
DIM SHARED TextF(MaxTextFields) AS Textfieldtype
DIM SHARED TextText$(MaxTextFields)
DIM SHARED TextTimer!
DIM SHARED Cursor AS STRING * 2
DIM SHARED CursorChar AS STRING * 1


'Checkbox
DIM SHARED CheckBox(MaxCheckBoxes) AS CheckBoxtype
DIM SHARED CheckBoxText$(MaxCheckBoxes)


'Radiobutton
DIM SHARED Radio(MaxRadioButtons) AS RadioButtonType
DIM SHARED RadioText$(MaxRadioButtons)


'WAV-playback
DIM SHARED WavBuffer(0 TO 0) AS STRING * 8000
DIM SHARED WavFreq&
DIM SHARED WavFilehandle&
DIM SHARED WavOffset&
DIM SHARED WavLength&
DIM SHARED PlayingWAV&


'Menu
DIM SHARED MenuBar(MaxWindows, 8) AS Menutype
DIM SHARED menu(MaxWindows, 8, 14) AS STRING * 10
DIM SHARED MenuActive AS INTEGER
DIM SHARED MenuSaved AS INTEGER
DIM SHARED MenuNumber AS INTEGER
DIM SHARED menuselected AS INTEGER
DIM SHARED MenuHeld AS INTEGER



'Frame
DIM SHARED Frame(MaxFrames) AS Frametype
DIM SHARED FrameCaption$(MaxFrames)


'Percent bar
DIM SHARED PBar(MaxPercentBars) AS PBartype


'Textbox
DIM SHARED TextB(MaxTextBoxes) AS TextType
DIM SHARED TextBCaption$(MaxTextBoxes)


'Icon
DIM SHARED Icon(MaxIcons) AS Icontype
DIM SHARED IconFile$(MaxIcons)


'BMP-picture
DIM SHARED Bitmap(MaxBMPs) AS BMPType
DIM SHARED BitmapFile$(MaxBMPs)


'Misc
DIM SHARED FileIcons$(6 TO 30), dir$
DIM SHARED prompt$, a1, a2, DefaultButton, start!
DIM SHARED StatusMSG$, lWin, process$
DIM SHARED Text$(1 TO 1300), LineN, MaxLineN, Position, StartLineN
DIM SHARED Hour$, Minute$, was, Temp1
DIM SHARED Version AS VersionType
DIM SHARED Shadow, Fill, Highlight
DIM SHARED ScreenMode, ScreenXDim, ScreenYDim, ColorDepth, desktop, ActiveTopic, DeActiveTopic, ActiveTopicB, DeActiveTopicB
DIM SHARED clickedButton, HoldButton, MovedBar, MovingBar, Click
DIM SHARED hx, hy, hv, ho, Hk
DIM SHARED DragXOffset, DragYOffset, dragWin, DragXLen, DragYLen, DragLine
DIM SHARED OldHiiriY, OldHiiriX, mousemove, drag
DIM SHARED Virhe, Virhekuvaus$, QBVirhe
DIM SHARED FontX&, FontY&, FontSpace&
DIM SHARED ClickedRadioButton, ClickedCheckBox
DIM SHARED BasePort%, LenPort%, Channel%, IRQ%
DIM SHARED ViewX, ViewY, ViewX2, ViewY2
DIM SHARED Byte AS STRING * 1
DIM SHARED DialogActive AS INTEGER
DIM SHARED DialogClickedButton AS INTEGER
DIM SHARED DialogAWin AS INTEGER
DIM SHARED InputDialogActive AS INTEGER
DIM SHARED InputDialogClickedButton AS INTEGER
DIM SHARED InputDialogAWin AS INTEGER
DIM SHARED Copying
DIM SHARED CopyPos AS SINGLE
DIM SHARED CopyFile
DIM SHARED CopyFile2
DIM SHARED CopyDone
DIM SHARED ClickedIcon

'----------------------------------------------------------------------------
'             Read BLASTER enviroment variable for sound
'----------------------------------------------------------------------------

GetBLASTER Channel%, BasePort%, IRQ%

'Define version

Version.Major = "1"
Version.Minor = "00"

Virhe = 0: Virhekuvaus$ = "": QBVirhe = 0

'----------------------------------------------------------------------------
'                          Define screen parameters
'----------------------------------------------------------------------------
Defscreen

'----------------------------------------------------------------------------
'                               Read font
'----------------------------------------------------------------------------
DIM SHARED Font(48 TO 41 * 127) AS INTEGER
FontX& = 6: FontY& = 8: FontSpace& = 6
GetFont


CLS
viewW -1
PAINT (0, 0), desktop

'----------------------------------------------------------------------------
'                         Initialize mouse driver
'----------------------------------------------------------------------------
DIM SHARED hiiri$
hiiri$ = SPACE$(57)
FOR i% = 1 TO 57
  READ a$
  h$ = CHR$(VAL("&H" + a$))
  MID$(hiiri$, i%, 1) = h$
NEXT i%

DATA 55, 89, E5, 8B, 5E, 0C, 8B, 07, 50, 8B, 5E, 0A, 8B, 07, 50, 8B
DATA 5E, 08, 8B, 0F, 8B, 5E, 06, 8B, 17, 5B, 58, 1E, 07, CD, 33, 53
DATA 8B, 5E, 0C, 89, 07, 58, 8B, 5E, 0A, 89, 07, 8B, 5E, 08, 89, 0F
DATA 8B, 5E, 06, 89, 17, 5D, CA, 08, 00

napit% = Hiiritarkista%
IF (napit% = 0) THEN
  PRINT "Mouse not found!"
  SYSTEM
END IF
Cursor = CursorChar$ + " "
DialogActive = false
InputDialogActive = false

'----------------------------------------------------------------------------
'                         Show splash screen
'----------------------------------------------------------------------------
'CreateIcon 0, 1, 1, 10, "c:\access\icons\windows\desktop\icons\1200.ico"
'SHELL "copy c:\access\icons\windows\desktop\icons\1438.ico c:\access\icons\bmp.ico"
'END
'CreateBMP 0, 1, 200, 100, "c:\cyber\fighter\select.bmp"
ShowMouse
splash
desk:
CreateIcon 0, 1, 10, 10, "c:\access\icons\mycomp.ico"
CreateLabel 0, 6, 5, 45, 42, 11, "My PC", 1, 15, singleOut, center, true
CreateIcon 0, 2, 10, 64 + 10, "c:\access\icons\trash.ico"
CreateLabel 0, 7, 5, 64 + 45, 42, 11, "Trash", 1, 15, singleOut, center, true
CreateIcon 0, 3, 10, 128 + 10, "c:\access\icons\userpro.ico"
CreateLabel 0, 8, 1, 128 + 45, 50, 11, "User Info", 1, 15, singleOut, center, false
CreateIcon 0, 4, 10, 190 + 10, "c:\access\icons\help.ico"
CreateLabel 0, 9, 5, 190 + 45, 42, 11, "Help", 1, 15, singleOut, center, true
CreateIcon 0, 5, 10, 255 + 10, "c:\access\icons\control.ico"
CreateLabel 0, 10, 5, 255 + 45, 42, 11, "Settings", 1, 15, singleOut, center, false

'----------------------------------------------------------------------------
'                       Read objects and draw them
'----------------------------------------------------------------------------
FOR i& = 1 TO MaxWindows
  Win(i&).x = ScreenXDim + 1000
  Win(i&).y = ScreenYDim + 1000
  Win(i&).X2 = ScreenXDim + 1000
  Win(i&).y2 = ScreenYDim + 1000
NEXT
Initscreen
GOSUB refreshdir
IF ScreenMode <> 13 AND ScreenMode <> 7 THEN Hiirirajat 0, 0, ScreenXDim - 1, ScreenYDim - 1 ELSE Hiirirajat 0, 0, ScreenXDim * 2 - 1, ScreenYDim * 2 - 1
PlayingWAV = 0
ShowMouse
MenuActive = false
MenuSaved = false
MenuLoaded = false
Copying = false


'----------------------------------------------------------------------------
'                               Draw screen
'----------------------------------------------------------------------------

UpdateScreen


'////////////////////////////////////////////////////////////////////////////
'----------------------------------------------------------------------------
'                             start MAIN loop
'----------------------------------------------------------------------------
'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
a1 = 1
a2 = 24
GOSUB refreshdir
restart:
TaskMan 1
AccessMenuA
OPEN "C:\access\temp\dir.tmp" FOR OUTPUT AS #3
PRINT #3, "Odis Clemons is the greatest computer hacker ever!! Next to Jose'"
CLOSE
DO
  '----------------------------------------------------------------------------
  '                        Read mouse coords.
  '----------------------------------------------------------------------------
  updatemouse

  main
  Virhe = 0
  Virhekuvaus$ = ""

  '----------------------------------------------------------------------------
  '                        Handle all objects
  '----------------------------------------------------------------------------
startup:
FOR temp = 6 TO 30
IF ClickedIcon = temp THEN NoteBook "", 1: NoteBook FileIcons$(temp), 2: EXIT FOR
NEXT temp

IF DialogPressedbutton > 0 THEN NoteBook TextText$(MaxTextFields), 1: NoteBook TextText$(MaxTextFields), 2: END
IF key$ = "help" THEN END
IF menuselected = 8 AND aWIn = 9 THEN GOSUB endit: menuselected = 0
IF ClickedIcon = 1 THEN END
IF ClickedIcon = 2 THEN NoteBook "c:\access\access.hlp", 1
IF ClickedIcon = 3 THEN About
IF aWIn = 10 THEN
IF key$ = CHR$(0) + "H" AND StartLineN < MaxLineN - 1 THEN StartLineN = StartLineN + 1: NoteBook "", 3
IF key$ = CHR$(0) + "P" AND StartLineN > 1 THEN StartLineN = StartLineN - 1: NoteBook "", 3
IF menuselected = 2 THEN NoteBook TextText$(3), 2: menuselected = 0
END IF
IF key$ = CHR$(27) THEN WavPlayDMA "c:\access\doorslam.wav", 20000: END

IF key$ = CHR$(13) THEN
  END IF

IF aWIn = 12 AND key$ > "" AND key$ <> CHR$(8) AND key$ <> CHR$(13) THEN
   CreateLabel 12, 4, 1, 22, 325, 10, key$, 7, 0, none, left, 1
  temp$ = STR$(ASC(key$))
  CreateLabel 12, 5, 1, 32, 325, 50, temp$, 7, 0, none, left, 1
  pause 0
END IF
lWin = aWIn

GOSUB ButtonCheck

IF SBar(1).Value > SBOldValuE(1) OR SBar(1).Value < SBOldValuE(1) AND MaxLineN > SBar(1).Value AND SBar(1).Value > 1 THEN
temp = 0
printF 1, 1, STR$(SBar(1).Value), 4, 16
LineN = SBar(1).Value
StartLineN = LineN + (SBar(1).Value - SBOldValuE(1))
LineN = StartLineN
DO
temp = temp + 10
CreateLabel 8, 1, 1, temp, 530, 15, Text$(LineN), 0, 15, none, left, false
LineN = LineN + 1
LOOP UNTIL LineN = StartLineN + 27

LineN = 1
StartLineN = LineN
SBOldValuE(1) = SBar(1).Value
CreateScrollBar 8, 1, 1, 527, 13, 15, 285, MaxLineN, 1
CreateScrollBar 8, 2, 0, 1, 284, 524, 15, 100, 1
END IF

IF SBar(1).Value < SBOldValuE(1) AND StartLineN < LineN AND StartLineN > 1 THEN
CreateLabel 8, 1, 1, 10, 530, 200, " ", 0, 15, none, left, false
  temp = 0
  StartLineN = SBar(1).Value
  Temp1 = StartLineN + 27
        FOR LineN = StartLineN TO Temp1
          temp = temp + 10
          CreateLabel 8, 1, 1, temp, 530, 15, Text$(LineN), 0, 15, none, left, false
          LineN = LineN + 1
        NEXT LineN
        'LOOP UNTIL LineN - StartLineN = 27 OR MaxLineN = LineN
  END IF

  SBOldValuE(1) = SBar(1).Value
IF MenuActive = true AND aWIn = 8 OR MenuActive = true AND aWIn = 9 THEN GOTO Skip6
  IF AccessHide = 0 THEN was = 1
  IF AccessHide = 0 AND timea$ <> TIME$ THEN GOSUB Accessmenu
  IF AccessPos = 2 THEN
  IF ho <> 0 AND hy < 20 AND hx > 30 AND hx < 599 AND was = 1 THEN Control 1
  IF hy < 2 AND was = 0 AND AccessHide = 1 THEN was = 1: GOSUB Accessmenu
  IF hy < 20 AND was = 1 AND timea$ <> TIME$ AND AccessHide = 1 THEN GOSUB Accessmenu
  IF hy > 20 AND was = 1 AND AccessHide = 1 THEN HideMouse: was = 0: LINE (0, 1)-(639, 18), desktop, BF: LINE (500, 21)-(638, 42), desktop, BF: ShowMouse: UpdateScreen
  IF was = 1 AND hy < 20 AND hx < 30 AND Click = 1 THEN AccessMenuA
  END IF
  IF AccessPos = 1 THEN
  IF ho <> 0 AND hy > 461 AND hx > 30 AND hx < 599 AND was = 1 THEN Control 1
  IF hy > 477 AND was = 0 AND AccessHide = 1 THEN was = 1: GOSUB Accessmenu
  IF hy > 461 AND was = 1 AND timea$ <> TIME$ AND AccessHide = 1 THEN GOSUB Accessmenu
  IF hy < 461 AND was = 1 AND AccessHide = 1 THEN HideMouse: was = 0: LINE (0, 461)-(639, 479), desktop, BF: LINE (500, 21)-(638, 42), desktop, BF: ShowMouse: UpdateScreen
  END IF
Skip6:

IF Radio(14).State = true AND Click = 1 THEN EnableTextField 3
IF Radio(15).State = true AND Click = 1 THEN DisableTextField 3

  IF topica$ = "Shut down Access" AND DialogClickedButton = 1 THEN
  CLOSE aWIn
  Box3D 200, 200, 400, 250, DoubleOut, 7
  IF Radio(19).State = true THEN WavPlayDMA "c:\access\doorslam.wav", 20000: ShutDown 1
  IF Radio(20).State = true THEN ShutDown 2
  IF Radio(21).State = true THEN ShutDown 3
  END IF
 
  IF DialogClickedButton = 2 THEN PALETTE
  IF clickedButton = 5 THEN
    OPEN "c:\Access\system\system.ini" FOR OUTPUT AS #2
  Desktopa = desktop
    IF Radio(1).State = true THEN Desktopa = 4
    IF Radio(2).State = true THEN Desktopa = 9
    IF Radio(3).State = true THEN Desktopa = 1
    IF Radio(4).State = true THEN Desktopa = 8
    IF Radio(5).State = true THEN Desktopa = 2
    IF Radio(6).State = true THEN Desktopa = 0
    IF Radio(7).State = true THEN Desktopa = 7
 
    IF Radio(8).State = true THEN AccessHide = 1
    IF Radio(9).State = true THEN AccessHide = 0
    IF Radio(10).State = true THEN AccessTime = 1
    IF Radio(11).State = true THEN AccessTime = 0
    IF Radio(12).State = true THEN AccessPos = 2
    IF Radio(13).State = true THEN AccessPos = 1
    IF Radio(14).State = true THEN Skin = 1: Skinfile$ = TextText$(1)
    IF Radio(15).State = true THEN Skin = 0
   
    IF Radio(16).State = true THEN saver = 1
    IF Radio(17).State = true THEN saver = 2
    IF Radio(18).State = true THEN saver = 3
    IF Radio(19).State = true THEN saver = 0
    IF VAL(TextText$(9)) * 60 > 1 THEN
    Idle = VAL(TextText$(9)) * 60
    END IF
    PRINT #2, Desktopa, AccessTime, AccessHide, AccessPos, Skin, saver, Idle, Skinfile$
    CLOSE #2
    OPEN "c:\access\system\user.inf" FOR OUTPUT AS #2
    FOR num = 2 TO 8
    PRINT #2, TextText$(num)
    NEXT num
    PRINT #2, Now$
    CLOSE #2
    killWindow aWIn: StatusMSG$ = ""
    IF Desktopa <> desktop THEN Dialog "Restart", "The Changes you made won't take effect until you restart Access.  Restart?", "Yes", "No", ""
    StatusMSG$ = ""
    END IF
    IF topica$ = "Restart" AND DialogClickedButton = 1 THEN RUN
 
  '----------------------------------------------------------------------------
  '                        End of user code
  '----------------------------------------------------------------------------

LOOP
endit:
Dialog "Shut down Access", "", "Ok", "Cancel", ""
CreateRadioButton MaxWindows, 19, 2, 30, 20, "Shut down Access", true
CreateRadioButton MaxWindows, 20, 2, 30, 30, "Restart PC", false
CreateRadioButton MaxWindows, 21, 2, 30, 40, "Restart Access", false
StatusMSG$ = "Shutting down will delete all temp files and close all progams..."
  RETURN
Accessmenu:
IF MenuActive = true THEN RETURN
IF AccessHide = 1 THEN was = 1
HideMouse
IF AccessPos = 2 THEN
        Box3D -1, -1, 639, 18, DoubleOut, 7
        Box3D 2, 2, 45, 15, DoubleOut, 7
        printF 7, 4, "/u/c04A/c01/uc/c03c/c05e/c06s/c09s", 0, 16
        Box3D 55, 2, 100, 15, DoubleIn, 15
        printF 59, 4, "/u/rTasks", 0, 16
        Box3D 110, 2, 195, 15, DoubleIn, 15
        printF 114, 4, "/u/rControl Panel", 0, 16
        Box3D 205, 2, 250, 15, DoubleIn, 15
        printF 209, 4, "/u/rAbout", 0, 16
IF AccessTime = 1 THEN
        IF Future <> 1 THEN Box3D 590, 2, 635, 15, SingleIn, 7
        IF Future = 1 THEN
        CIRCLE (650, -5), 100, 0, , , 5 / 25
        PAINT (639, 10), Skincolor, 0
        END IF
        IF MID$(timea$, 7, 2) <> MID$(TIME$, 7, 2) THEN
           printF 592, 4, RIGHT$(Now, 7), 0, 16
           timea$ = TIME$
           END IF
        END IF
        END IF
IF AccessPos = 1 THEN
        Box3D -1, 461, 639, 479, DoubleOut, 7
        Box3D 2, 463, 45, 477, DoubleOut, 7
        printF 7, 466, "/u/c04A/c01/uc/c03c/c05e/c06s/c09s", 0, 16
        IF AccessTime = 1 THEN
        Box3D 591, 463, 635, 477, SingleIn, 7
        IF MID$(timea$, 7, 2) <> MID$(TIME$, 7, 2) THEN
           timea$ = TIME$
           printF 592, 466, RIGHT$(Now, 7), 0, 16
           END IF
        END IF
        END IF
ShowMouse
RETURN

statusbar:
IF AccessPos = 2 THEN
           IF StatusMSG$ = Temp1$ AND StatusMSG$ = "" OR StatusMSG$ = "" THEN LINE (1, 461)-(639, 479), desktop, BF: GOTO statusbar2
           Box3D -1, 461, 639, 479, DoubleOut, 7
           printF 7, 466, "                                                ", 0, 16
           printF 7, 466, "/c04" + StatusMSG$, 0, 16
statusbar2:
END IF
IF AccessPos = 1 THEN
           IF StatusMSG$ = Temp1$ AND StatusMSG$ = "" OR StatusMSG$ = "" THEN LINE (1, 461)-(639, 479), desktop, BF: GOTO statusbar3
           Box3D -1, -1, 639, 18, DoubleOut, 7
           printF 7, 4, "                                                ", 0, 16
           printF 7, 4, "/c04" + StatusMSG$, 0, 16
statusbar3:
END IF
Temp1$ = StatusMSG$
RETURN

updatetim:
temp$ = Hour$ + ":" + Minute$
CreateButton 6, 11, 80, 51, 40, 40, temp$
RETURN

Virhe:
Virhe = ERR
IF Virhe = 53 THEN Virhe = 0
QBVirhe = Virhe
RESUME NEXT


mousefix:
HideMouse
ShowMouse
RETURN

ButtonCheck:
SELECT CASE clickedButton
CASE 2
  Control 2
CASE 3
  Control 5
CASE 4
  Control 4
CASE 6
  killWindow aWIn
  StatusMSG$ = ""
CASE 7
  IF VAL(TextText$(1)) > 1 THEN
        temp = VAL(TextText$(1))
        temp = temp - 1
        Hour$ = STR$(temp)
        CreateTextField 6, 1, 7, 10, 2, Hour$, false
  GOSUB updatetim
END IF
CASE 8
  IF VAL(TextText$(1)) < 12 THEN
        temp = VAL(TextText$(1))
        temp = temp + 1
        Hour$ = STR$(temp)
        CreateTextField 6, 1, 7, 10, 2, Hour$, false
  GOSUB updatetim
END IF
CASE 9
  IF VAL(TextText$(2)) > 0 THEN
        temp = VAL(TextText$(2))
        temp = temp - 1
        Minute$ = STR$(temp)
        CreateTextField 6, 2, 7, 60, 2, Minute$, false
  GOSUB updatetim
END IF
CASE 10
  IF VAL(TextText$(2)) < 60 THEN
        temp = VAL(TextText$(2))
        temp = temp + 1
        Minute$ = STR$(temp)
        CreateTextField 6, 2, 7, 60, 2, Minute$, false
  GOSUB updatetim
END IF
CASE 11
  Control 6
CASE 12
  Control 7
CASE 13
killWindow 1
  Command 2
END SELECT
RETURN
refreshdir:
RETURN
IF dir$ = "" THEN dir$ = "c:\access"
IF ext$ = "" THEN ext$ = "TXT,BAS,EXE,NBK,ICO,BAT,COM,HLP,SYS,TMP,TEM,RAP"
OPEN "c:\access\system\filelist.cfg" FOR OUTPUT AS #1
PRINT #1, "SortMethod = 1"
PRINT #1, "-------------------------"
PRINT #1, "All BAS files"
PRINT #1, "C:\access\temp\dir.tmp"
PRINT #1, dir$
PRINT #1, ext$
PRINT #1, "END"
PRINT #1, "-------------------------"
PRINT #1, "END OF LISTING"
CLOSE #1
CHDIR "c:\access\system"
SHELL "c:\access\system\filelist.exe"
CHDIR dir$
RETURN

altf4:
IF aWIn = 8 OR aWIn = 9 OR aWIn = 0 THEN GOSUB endit: RETURN
killWindow aWIn
RETURN
up:
'IF aWin = 10 THEN
'IF StartLineN < MaxLineN - 1 THEN
StartLineN = StartLineN + 1: ReDrawWindow 10': NoteBook "", 3
'END IF
RETURN
down:
'IF aWin = 10 THEN
IF StartLineN > 1 THEN StartLineN = StartLineN - 1: ReDrawWindow 10': NoteBook "", 3
'END IF
RETURN

left:
RETURN

right:
RETURN

ctrlm:
ShowMouse
HideMouse
Save640 "screen", 1
FOR temp = 220 TO 10 STEP -20
CIRCLE (hx, hy), temp, 2
FOR a = 1 TO 31580
FOR b = 1 TO 2
NEXT b
NEXT a
NEXT temp
ShowMouse
DO
updatemouse
IF mousemove = 1 OR Click = 1 OR INKEY$ > "" THEN EXIT DO
LOOP
HideMouse
Load640 "screen", 1
ShowMouse
RETURN

ctrlo:
ScreenSaver 4
RETURN

ctrlf:
HideMouse
Defscreen
CLS
viewW -1
PAINT (0, 0), desktop
UpdateScreen
ShowMouse
RETURN
'----------------------------------------------------------------------------
'                            Quote of the day
'----------------------------------------------------------------------------

Quotes:

'Number of quotes
DATA 5

'Quotes
DATA "Press 'ESC' to ShutDown(alpha version only)."
DATA "WWW.CYBERSHINAKUMA.8K.COM"
DATA "Cybershinakuma1@yahoo.com for tech support."
DATA "Press ALT+F4 to close active window or shut down."
DATA "If you can't find the mouse, press CTRL+M"

DOCUMENT:
' FileList v0.0 source code (c) 2000 m \ k productions etc
' the hardware code was written by Mark K Kim, thanks Mark

'     - DMA Wav player modified from Mike Huff's DMAPLAY.BAS
'           (added 32k+ file support)
'     - Screen saver/loader modified from Hans Lunsing's 640SAVE.BAS
'           (added Start line defining, 320x200x256 support)
'     - Mouse routines modified from Mikrobitti Magazine's article
'     - Programmer's quotes from FUN2_100.TXT collected by Harry Upmal
'     - BMP- and ICO-loader modified from James S. Baughn's Image format FAQ
'       (and associated SUBs)

'----[WIN2.BAS (C) Sami Ky”stil„ 1997]----------------------------------------
'            ÚÄÄÄÄÄÄ¿ÚÄÄ¿ÚÄÄÄÄÄ¿ÚÄÄÄÄ¿  ÚÄÄÄ¿ ÚÄÄÄÄ¿ÚÄÄÄÄ¿
'            ³Û°  Û°³³Û°³³Û° Û°³³ÛÛÛ°À¿ÚÙÛÛ°À¿³ÛÛÛ°³³ÛÛÛ°³
'            ³Û°  Û°³ÃÄÄ´³Û° Û°³³Û° Û°³³Û° Û°³³  Û°³³Û°  ³
'     ú úúÄúÄ´Û°  Û°Ã´Û°Ã´ÛÛ°Û°Ã´Û° Û°Ã´Û° Û°Ã´ Û° Ã´ÛÛ° ÃÄúÄúú ú
'            ³Û°Û°Û°³³Û°³³Û°ÛÛ°³³Û° Û°³³Û° Û°³³Û°  ³³Û°  ³
'            ³ÛÛ°ÛÛ°³³Û°³³Û° Û°³³ÛÛÛ°ÚÙÀ¿ÛÛ°ÚÙ³ÛÛÛ°³³ÛÛÛ°³
'            ÀÄÄÄÄÄÄÙÀÄÄÙÀÄÄÄÄÄÙÀÄÄÄÄÙ  ÀÄÄÄÙ ÀÄÄÄÄÙÀÄÄÄÄÙ
'
'                            EúNúGúIúNúE
'
'                            ::[V2.00]::
'
'                  ÚÄÄÄ¿ ÚÄÄÄÄÄÄÄ¿ ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
'         ú úúÄúÄÄÄ´BúyÃÄ´SúAúMúiÃÄ´KúYú™úSúTúiúLúŽÃÄÄúÄúú ú
'                  ÀÄÄÄÙ ÀÄÄÄÄÄÄÄÙ ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
'
'                       ú úúÄúÄ1Ä9Ä9Ä7ÄúÄúú ú
'
'----------------------------------------------------------------------------
'
' If you have any comments, questions, bug reports, etc. please send them to:
'
'   hiteck@freenet.hut.fi
'
' or:
'
'   kemple.oy@mbnet.fi
'
' or contact hiteck in IRC
'
'----------------------------------------------------------------------------



'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                                                                         *
'* You are granted a permission (and encouraged) to use this code to       *
'* build your own applications, as long as I am credited for the           *
'* engine, after all, it's not that easy to write 4800+ lines of source ;) *
'* Also include the above box somewhere in your program's source           *
'*                                                                         *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'----------------------------------------------------------------------------

REM $STATIC
SUB About
OPEN "c:\access\system\user.inf" FOR INPUT AS #1
INPUT #1, TextText$(2), TextText$(3), TextText$(4), TextText$(5), TextText$(6), TextText$(7), TextText$(8)
CLOSE #1
temp$ = "/bAccess is liscensed to:/n" + "/b" + TextText$(2) + " " + TextText$(4) + ". " + TextText$(3) + "/n" + TextText$(5) + "/n" + TextText$(6) + ", " + TextText$(7) + "/n" + TextText$(8) +  _
"/nWindoze engine v2.00 by Sami Kyostila, 1997. MultiOS by Odis Clemons, 2002."
Dialog "About", temp$, "Ok", "", ""
StatusMSG$ = "Liscense info..."
END SUB

SUB AccessMenuA
CreateWindow 9, 1, -16, 50, 18, "AccessMenu", 8, false
CreateMenu 9, 1, "Access"
AddMenuItem 9, 1, "Help"
AddMenuItem 9, 1, "Settings"
AddMenuItem 9, 1, "Command"
AddMenuItem 9, 1, "Note Book"
AddMenuItem 9, 1, "About"
AddMenuItem 9, 1, "ScreenSaver"
AddMenuItem 9, 1, "File Cabinet"
AddMenuItem 9, 1, "ShutDown"
END SUB

SUB AddMenuItem (WindowNum, Number, caption$)
'----------------------------------------------------------------------------
'              Adds a menu item into a menu
'----------------------------------------------------------------------------
'
' WindowNum - Host window of the menu
'    Number - The number of the menu in the window,
'             counting from left to right (1: First, 2: Second etc.)
'  Caption$ - Caption of the menu item.
'
'----------------------------------------------------------------------------


MenuBar(WindowNum, Number).Items = MenuBar(WindowNum, Number).Items + 1

menu(WindowNum, Number, MenuBar(WindowNum, Number).Items) = caption$

END SUB

SUB Adesktop
ax = 250 + 20
ay = 200
ax2 = 300 + 20
ay2 = 300
HideMouse
LINE (ax - 1, ay)-(ax - 26, ay2), 15
LINE (ax, ay)-(ax - 25, ay2), 1'/

LINE (ax, ay - 1)-(ax2 - 5, ay - 1), 15  '-
LINE (ax, ay)-(ax2, ay), 1

LINE (ax2, ay)-(ax2 + 26, ay2), 1      '\

LINE (ax - 25, ay2)-(ax + 5, ay2), 1    '/_

LINE (ax2 + 26, ay2)-(ax2 - 5, ay2), 1  '_\

LINE (ax + 5, ay2)-(ax + 13, ay2 - 30), 1  '-

LINE (ax2 - 5, ay2)-(ax2 - 13, ay2 - 30), 1'-

LINE (ax + 13, ay2 - 30)-(ax2 - 13, ay2 - 30), 1

PAINT (ax + 1, ay + 1), 1

LINE (ax2, ay)-(ax2 + 26, ay2), 0      '\

LINE (ax - 25, ay2)-(ax + 5, ay2), 0    '/_

LINE (ax2 + 26, ay2)-(ax2 - 5, ay2), 0  '_\

LINE (ax + 5, ay2)-(ax + 13, ay2 - 30), 0  '-

LINE (ax2 - 5, ay2)-(ax2 - 13, ay2 - 30), 0'-

LINE (ax + 13, ay2 - 30)-(ax2 - 13, ay2 - 30), 0
printF ax + 10, ay + 40, "/bAccess", 7, 16
printF ax + 9, ay + 39, "/bAccess", 4, 16
ShowMouse
END SUB

SUB AsciiConvert
CreateWindow 12, 50, 50, 350, 200, "ASCII Converter", 15, true
process$ = "ascii contert"
CreateLabel 12, 3, 1, 12, 252, 10, "Type the character to get its ASC value.", 0, 16, none, center, 1
END SUB

SUB Backroundcopy (filename$, Path$)
'----------------------------------------------------------------------------
'               Copy a file in the backround
'----------------------------------------------------------------------------
'
'    Filename$ - File to copy
'        Path$ - Path to copy the file to
'
'
' Example:
'
'    BackroundCopy "c:\temp\game.zip", "c:\games"
'
' will copy c:\temp\game.zip to the file c:\games\game.zip
'
'----------------------------------------------------------------------------
'Read the state of the copy process from the variable CopyDone (percent done)
'and CopyPos (bytes copied)
'----------------------------------------------------------------------------
' To adjust the copying speed and multitasking, see the main module's CONST
' statements
'----------------------------------------------------------------------------
' the variable Copying indicates if a copying process is active (True: yes
' False: no)
'----------------------------------------------------------------------------


IF NOT Exist(filename$) = true THEN EXIT SUB

IF INSTR(filename$, "\") > 0 THEN
  FOR i = 1 TO LEN(filename$)
    IF MID$(filename$, i, 1) = "\" THEN startPos = i
  NEXT
  Filename2$ = RIGHT$(filename$, LEN(filename$) - INSTR(startPos, filename$, "\"))
END IF

CopyFile = FREEFILE


OPEN filename$ FOR BINARY AS #CopyFile
IF RIGHT$(Path$, 1) <> "\" THEN Path$ = Path$ + "\"

CopyFile2 = FREEFILE
OPEN Path$ + Filename2$ FOR BINARY AS #CopyFile2

SEEK #CopyFile, 1
SEEK #CopyFile2, 1


Copying = true
CopyPos = 0
CopyDone = 0
END SUB

SUB Box3D (x, y, X2, y2, BorderStyle, Fillcol)
'----------------------------------------------------------------------------
' Draws a box with 3D borders
'----------------------------------------------------------------------------
'
' x, y        - Upper left corner
' x2, y2      - Lower right corner
' BorderStyle - Borderstyle, one of the following values:
'               - None
'               - DoubleIN
'               - DoubleOUT
'               - SingleIN
'               - SingleOUT
' Fillcol     - Fill color (16 = Transparent)
'
'----------------------------------------------------------------------------



IF BorderStyle = 1 THEN
  LINE (x, y)-(X2, y), 0
  LINE (x, y)-(x, y2), 0
  LINE (X2, y2)-(X2, y), Fill
  LINE (X2, y2)-(x, y2), Fill

  LINE (x + 1, y + 1)-(X2 - 1, y + 1), Shadow
  LINE (x + 1, y + 1)-(x + 1, y2 - 1), Shadow
  LINE (X2 - 1, y2 - 1)-(X2 - 1, y + 1), Highlight
  LINE (X2 - 1, y2 - 1)-(x + 1, y2 - 1), Highlight
END IF

IF BorderStyle = 2 THEN
  LINE (x, y)-(X2, y), Fill
  LINE (x, y)-(x, y2), Fill
  LINE (X2, y2)-(X2, y), 0
  LINE (X2, y2)-(x, y2), 0

  LINE (x + 1, y + 1)-(X2 - 1, y + 1), Highlight
  LINE (x + 1, y + 1)-(x + 1, y2 - 1), Highlight
  LINE (X2 - 1, y2 - 1)-(X2 - 1, y + 1), Shadow
  LINE (X2 - 1, y2 - 1)-(x + 1, y2 - 1), Shadow
END IF

IF BorderStyle = 3 THEN
  IF Fillcol <> Shadow THEN LINE (x, y)-(X2, y), Shadow ELSE LINE (x, y)-(X2, y), 0
  IF Fillcol <> Shadow THEN LINE (x, y)-(x, y2), Shadow ELSE LINE (x, y)-(x, y2), 0
  LINE (X2, y2)-(X2, y), Highlight
  LINE (X2, y2)-(x, y2), Highlight
END IF

IF BorderStyle = 4 THEN
  LINE (x, y)-(X2, y), Highlight
  LINE (x, y)-(x, y2), Highlight
  IF Fillcol <> Shadow THEN LINE (X2, y2)-(X2, y), Shadow ELSE LINE (X2, y2)-(X2, y), 0
  IF Fillcol <> Shadow THEN LINE (X2, y2)-(x, y2), Shadow ELSE LINE (X2, y2)-(x, y2), 0
END IF

IF NOT Fillcol = 16 THEN
  IF BorderStyle <= 2 THEN
    LINE (x + 2, y + 2)-(X2 - 2, y2 - 2), Fillcol, BF
  ELSE
    LINE (x + 1, y + 1)-(X2 - 1, y2 - 1), Fillcol, BF
  END IF
END IF

END SUB

SUB Cabinet (Size, dir$, ext$)
IF Size = 9 THEN GOTO maximize
HideMouse
Box3D 220, 228, 420, 248, DoubleOut, 0 'text box
Box3D 210, 218, 245, 254, DoubleOut, 0 'Icon box
CreateIcon 0, 1, 205, 198, "c:\access\icons\folder.ico"
printF 247, 235, "Welcome to File Cabinet!", 4, 0
ShowMouse
SLEEP 1
CreateIcon 0, 1, 10, 10, "c:\access\icons\mycomp.ico"
LINE (210, 218)-(420, 255), desktop, BF
IF Size = 0 THEN Size = 4
IF dir$ = "" THEN dir$ = "c:\access"
IF ext$ = "" THEN ext$ = "TXT,BAS,EXE,NBK,ICO,BAT,COM,HLP,SYS,TMP,TEM,RAP"
OPEN "c:\access\system\filelist.cfg" FOR OUTPUT AS #1
PRINT #1, "SortMethod = 1"
PRINT #1, "-------------------------"
PRINT #1, "All BAS files"
PRINT #1, "C:\access\temp\dir.tmp"
PRINT #1, dir$
PRINT #1, ext$
PRINT #1, "END"
PRINT #1, "-------------------------"
PRINT #1, "END OF LISTING"
CLOSE #1
CHDIR "c:\access\system"
SHELL "c:\access\system\filelist.exe"
CHDIR dir$
CreateWindow 14, 250, 25, 540, 225, "File Cabinet", 7, true
OPEN "C:\access\temp\dir.tmp" FOR INPUT AS #3
temp = 0
FOR i = 1 TO 15000
INPUT #3, temp$
IF temp = 1 THEN EXIT FOR
IF LEFT$(temp$, 3) = "---" AND i > 5 THEN temp = 1
NEXT i
TotalFiles = VAL(LEFT$(temp$, 10))
CLOSE
OPEN "C:\access\temp\dir.tmp" FOR INPUT AS #3
FOR i = 1 TO 5
INPUT #3, temp$
NEXT i
maximize:
x = 2
y = 14
x = -60
IF TotalFiles > 30 THEN TotalFiles = 30
temp = -1
FOR i = 6 TO TotalFiles
INPUT #3, FileIcons$(i)
temp = temp + 1
x = x + 70
'IF temp = Size THEN x = 10: y = y + 60: temp = 0
'IF Size = 4 AND y > 180 THEN EXIT FOR
SELECT CASE RIGHT$(FileIcons$(i), 4)
CASE ".TXT"
CreateIcon 14, i, x, y, "c:\access\icons\NoteBk.ico"
CASE ".BAS"
CreateIcon 14, i, x, y, "c:\access\icons\NoteBk.ico"
CASE ".ICO"
CreateIcon 14, i, x, y, "c:\access\icons\" + FileIcons$(i)
CASE ".HLP"
CreateIcon 14, i, x, y, "c:\access\icons\help.ico"
CASE ELSE
CreateIcon 14, i, x, y, "c:\access\icons\disable.ico"
END SELECT
CreateTextBox 14, i, x - 5, y + 34, MID$(FileIcons$(i), 1, LEN(FileIcons$(i)) - 4)
NEXT i
CLOSE
END SUB

SUB CDVolume (right%, left%, Getvol%)
'----------------------------------------------------------------------------
' Sets the CD-volume on the Sound Blaster card
'----------------------------------------------------------------------------
'
'  Right% - Right volume
'   Left% - Left volume
' Getvol% - If true, then volume will be read, not set
'
'----------------------------------------------------------------------------


OUT BasePort% + 4, &H28
IF Getvol% THEN
   left% = INP(BasePort% + 5) \ 16
   right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (right% + left% * 16) AND &HFF
END IF
END SUB

SUB CenterText (txt$)
'----------------------------------------------------------------------------
'                     Centers Txt$ on the screen
'----------------------------------------------------------------------------

LOCATE , 40 - LEN(txt$) \ 2
PRINT txt$
END SUB

SUB Command (opt)
SELECT CASE opt
CASE 1
CreateWindow 1, 50, 50, 400, 105, "Access Command", 7, true
process$ = "command"
prompt$ = ""
CreateTextField 1, 9, 1, 14, 40, "", false
CreateButton 1, 13, 275, 14, 30, 14, "Go!"
CASE 2
TextText$(9) = LCASE$(TextText$(9))

IF LEFT$(TextText$(9), 8) = "notebook" THEN
NoteBook "", 1: NoteBook MID$(TextText$(9), 10, LEN(TextText$(9)) - 9), 2
END IF

IF LEFT$(TextText$(9), 5) = "shell" THEN
HideMouse
Save640 "screen", 1
TIMER OFF
SCREEN 0
SHELL MID$(TextText$(9), 7, LEN(TextText$(9)) - 6)
PRINT "Press any key to continue..."
SLEEP
Defscreen
Load640 "screen", 1
TIMER ON
start! = TIMER
UpdateScreen
ShowMouse
END IF

IF LEFT$(TextText$(9), 7) = "cabinet" THEN
Cabinet 4, MID$(TextText$(9), 9, LEN(TextText$(9)) - 8), ""
END IF

END SELECT
END SUB

SUB Control (opt)
StatusMSG$ = "Loading user info"
OPEN "c:\access\system\user.inf" FOR INPUT AS #1
INPUT #1, TextText$(2), TextText$(3), TextText$(4), TextText$(5), TextText$(6), TextText$(7), TextText$(8)
CLOSE #1
StatusMSG$ = "Control Panel Active..."
SELECT CASE opt
CASE 1
process$ = "control panel"
HideMouse
Box3D 220, 228, 420, 248, DoubleOut, 0 'text box
Box3D 210, 218, 245, 254, DoubleOut, 0 'Icon box
CreateIcon 0, 1, 205, 198, "c:\access\icons\control.ico"
printF 247, 235, "Welcome to Control Panel!", 4, 0
ShowMouse
SLEEP 1
CreateIcon 0, 1, 10, 10, "c:\access\icons\control.ico"
LINE (210, 218)-(420, 255), desktop, BF
CreateWindow 4, 50, 25, 350, 325, "Control Panel", 7, true
CreateButton 4, 2, 2, 14, 85, 40, "Display"
CreateButton 4, 3, 89, 14, 75, 40, "User Info"
CreateButton 4, 4, 166, 14, 85, 40, "Access Menu"
CreateButton 4, 11, 2, 89, 85, 40, "Screen Saver"
CASE 2
killWindow aWIn
process$ = "display"
CreateWindow 2, 25, 25, 340, 225, "Display", 7, false
CreateFrame 2, 1, 1, 10, 100, 100, "Desktop color"
IF desktop = 4 THEN temp = 1 ELSE temp = 0
CreateRadioButton 2, 1, 1, 3, 25, "/c4 Red", temp
IF desktop = 4 THEN temp = 1 ELSE temp = 0
CreateRadioButton 2, 2, 1, 3, 35, "/c9 Light Blue", temp
IF desktop = 1 THEN temp = 1 ELSE temp = 0
CreateRadioButton 2, 3, 1, 3, 45, "/c1 Dark Blue", temp
IF desktop = 8 THEN temp = 1 ELSE temp = 0
CreateRadioButton 2, 4, 1, 3, 55, "/c8 Gray", temp
IF desktop = 2 THEN temp = 1 ELSE temp = 0
CreateRadioButton 2, 5, 1, 3, 65, "/c2 Green", temp
IF desktop = 0 THEN temp = 1 ELSE temp = 0
CreateRadioButton 2, 6, 1, 3, 75, "/c0 Black", temp
IF desktop = 7 THEN temp = 1 ELSE temp = 0
CreateRadioButton 2, 7, 1, 3, 85, "/c15White", temp

CreateFrame 2, 2, 100, 10, 200, 100, "Skins"
IF Skin = 1 THEN temp = 1 ELSE temp = 0
CreateRadioButton 2, 14, 5, 103, 25, "On", temp
IF Skin = 0 THEN temp = 1 ELSE temp = 0
CreateRadioButton 2, 15, 5, 103, 35, "Off", temp
IF Skinfile$ = "" THEN temp = 1 ELSE temp = 0
CreateTextField 2, 1, 103, 55, 29, Skinfile$, false
IF temp = 0 THEN DisableTextField 3
CreateButton 2, 5, 30, 100, 43, 20, "OK"
CreateButton 2, 6, 78, 100, 43, 20, "Cancel"
Temp1 = 2
CASE 4
killWindow aWIn
process$ = "access menu options"
CreateWindow 5, 25, 25, 240, 225, "Access Menu", 7, false
CreateFrame 5, 1, 1, 10, 70, 50, "Auto-Hide"
IF AccessHide = 1 THEN temp = 1 ELSE temp = 0
CreateRadioButton 5, 8, 2, 3, 25, "On", temp
IF AccessHide = 0 THEN temp = 1 ELSE temp = 0
CreateRadioButton 5, 9, 2, 3, 35, "Off", temp

CreateFrame 5, 2, 80, 10, 85, 50, "Time Display"
IF AccessTime = 1 THEN temp = 1 ELSE temp = 0
CreateRadioButton 5, 10, 3, 82, 25, "On", temp
IF AccessTime = 0 THEN temp = 1 ELSE temp = 0
CreateRadioButton 5, 11, 3, 82, 35, "Off", temp

CreateFrame 5, 3, 1, 70, 70, 50, "Access Menu Position"
IF AccessPos = 2 THEN temp = 1 ELSE temp = 0
CreateRadioButton 5, 12, 4, 3, 80, "Top", temp
IF AccessPos = 1 THEN temp = 1 ELSE temp = 0
CreateRadioButton 5, 13, 4, 3, 90, "Bottom", temp
CreateButton 5, 5, 30, 150, 43, 20, "OK"
CreateButton 5, 6, 78, 150, 43, 20, "Cancel"
Temp1 = 4

CASE 5
killWindow aWIn
process$ = ""
CreateWindow 7, 25, 25, 240, 225, "Personal info", 7, true
CreateFrame 7, 1, 1, 10, 200, 55, "Name"
CreateLabel 7, 1, 3, 15, 40, 10, "First Name", 0, 16, none, left, 0
CreateTextField 7, 2, 3, 25, 10, TextText$(2), false
CreateLabel 7, 2, 100, 15, 40, 10, "Last Name", 0, 16, none, left, 0
CreateTextField 7, 3, 100, 25, 10, TextText$(3), false
CreateLabel 7, 3, 105, 47, 40, 10, "Middle Initial", 0, 16, none, left, 0
CreateTextField 7, 4, 80, 45, 1, TextText$(4), false
CreateFrame 7, 2, 1, 70, 200, 60, "Address"
CreateLabel 7, 4, 3, 75, 40, 10, "Number and Street", 0, 16, none, left, 0
CreateTextField 7, 5, 3, 85, 15, TextText$(5), false
CreateLabel 7, 5, 3, 100, 40, 10, "City", 0, 16, none, left, 0
CreateTextField 7, 6, 3, 110, 10, TextText$(6), false
CreateLabel 7, 6, 120, 77, 40, 8, "State", 0, 16, none, left, 0
CreateTextField 7, 7, 120, 87, 2, TextText$(7), false
CreateLabel 7, 7, 120, 102, 40, 8, "Zip Code", 0, 16, none, left, 0
CreateTextField 7, 8, 120, 110, 5, TextText$(8), false
CreateButton 7, 5, 30, 150, 43, 20, "OK"
CreateButton 7, 6, 78, 150, 43, 20, "Cancel"
Temp1 = 4
CASE 6
killWindow aWIn
process$ = "screen saver select"
CreateWindow 11, 25, 25, 340, 225, "Screen Saver Options", 7, false
CreateFrame 11, 1, 1, 10, 100, 100, "Screen saver"
IF saver = 1 THEN temp = 1 ELSE temp = 0
CreateRadioButton 11, 16, 1, 3, 25, "Window Change", temp
IF saver = 2 THEN temp = 1 ELSE temp = 0
CreateRadioButton 11, 17, 1, 3, 35, "Marquee", temp
IF saver = 3 THEN temp = 1 ELSE temp = 0
CreateRadioButton 11, 18, 1, 3, 45, "Matrix", temp
IF saver = 0 THEN temp = 1 ELSE temp = 0
CreateRadioButton 11, 19, 1, 3, 55, "Off", temp
CreateLabel 11, 7, 103, 30, 200, 20, "How many minutes should I wait to activate screen saver?", 0, 16, SingleIn, center, true
CreateTextField 11, 9, 103, 55, 29, STR$(Idle / 60), false

CreateButton 11, 5, 30, 100, 43, 20, "OK"
CreateButton 11, 6, 78, 100, 43, 20, "Cancel"
Temp1 = 6
END SELECT
END SUB

SUB CreateBMP (WindowNum, num, x, y, filename$)
'----------------------------------------------------------------------------
'                    Creates a BMP-picture
'----------------------------------------------------------------------------
'
' WindowNum - Number of host window
'       Num - Index number of the bitmap
'      x, y - Bitmap coordinates
' Filename$ - Filename of the bitmap, must be Windows uncompressed BMP format
'
'----------------------------------------------------------------------------
Bitmap(num).Win = WindowNum
Bitmap(num).x = x
Bitmap(num).y = y
BitmapFile$(num) = filename$

LoadBMP num


END SUB

SUB CreateButton (WindowNum, num, x, y, XLen, YLen, caption$)
'----------------------------------------------------------------------------
'                           Creates a button
'----------------------------------------------------------------------------
'
'  WindowNum  - Host window number
'        Num  - Button index number
'       x, y  - Upper left corner
' XLen, YLen  - X length, Y length
'
'----------------------------------------------------------------------------
'    Check to see if the button is clicked from the Clickedbutton variable
'----------------------------------------------------------------------------
IF num = 0 THEN EXIT SUB
But(num).x = x
But(num).y = y
But(num).X2 = x + XLen
But(num).y2 = y + YLen
ButCaption$(num) = caption$
But(num).Win = WindowNum
But(num).Disabled = false
DrawButton num, 0, 0
IF LCASE$(caption$) = "ok" THEN DefaultButton = num
END SUB

SUB CreateCheckBox (WindowNum, num, x, y, caption$, State)
'----------------------------------------------------------------------------
' Creates a Check Box
'----------------------------------------------------------------------------
'
' WindowNum - Host window
'       Num - Checkbox index number
'      x, y - Upper left corner
'  Caption$ - Checkbox caption
'     State - True: Selected, False: Not selected
'
'----------------------------------------------------------------------------
'   Read the state of the checkbox from the CheckBox(Num).State variable
'----------------------------------------------------------------------------
IF num = 0 THEN EXIT SUB
CheckBox(num).x = x
CheckBox(num).y = y
CheckBox(num).State = State
CheckBox(num).Win = WindowNum
CheckBoxText$(num) = caption$
CheckBox(num).Disabled = false

DrawCheckBox num, true


END SUB

SUB CreateFrame (WindowNum, num, x, y, XLen, YLen, caption$)
'----------------------------------------------------------------------------
'                         Creates a frame
'----------------------------------------------------------------------------
'
'  WindowNum - Host window index number
'        Num - Index of the frame
'       x, y - coords.
' XLen, YLen - X Length, Y Length
'   Caption$ - Caption
'
'----------------------------------------------------------------------------
IF num = 0 THEN EXIT SUB

Frame(num).Win = WindowNum
Frame(num).x = x
Frame(num).y = y
Frame(num).X2 = x + XLen
Frame(num).y2 = y + YLen
FrameCaption$(num) = caption$

DrawFrame num

END SUB

SUB CreateIcon (WindowNum, num, x, y, filename$)
'----------------------------------------------------------------------------
'           Creates an Icon, which the user can click
'----------------------------------------------------------------------------
'
' WindowNum - Number of host window
'       Num - Index number of the Icon
'      x, y - Icon coordinates
' Filename$ - Filename of the icon, must be 32x32 Windows ICO format
'
'----------------------------------------------------------------------------

Icon(num).Win = WindowNum
Icon(num).x = x
Icon(num).y = y
IconFile$(num) = filename$
LoadIco num


END SUB

SUB CreateLabel (WindowNum, num, x, y, XLen, YLen, caption$, FCol, BCol, BorderStyle, Align, AutoWrap)
'----------------------------------------------------------------------------
' Creates a label
'----------------------------------------------------------------------------
'
'  WindowNum - Host window number
'        Num - Label index number
'       x, y - Upper left corner
' XLen, YLen - X Length, Y Length
'   Caption$ - Label text
'       FCol - Foreground color
'       BCol - Backround color
'BorderStyle - See SUB Box3D
'      Align - Text alignment:
'              - Left
'              - Center
'              - Right
'       Wrap - AutoWrap (1: yes, 0: no)
'
'----------------------------------------------------------------------------
'        Change the Label's text and color with the LabelText SUB
'----------------------------------------------------------------------------

Label(num).x = x
Label(num).X2 = XLen + x
Label(num).y = y
Label(num).y2 = YLen + y
LabelCaption$(num) = caption$
Label(num).FCol = FCol
Label(num).BCol = BCol
Label(num).Style = BorderStyle
Label(num).Win = WindowNum
Label(num).Align = Align
Label(num).wrap = AutoWrap

DrawLabel num

END SUB

SUB CreateMenu (WindowNum, Number, caption$)
'----------------------------------------------------------------------------
'                   Creates a menu into a window
'----------------------------------------------------------------------------
'
'  WindowNum - Host window index number
'     Number - The number of the menu in the window,
'              counting from left to right (1: First, 2: Second etc.)
'   Caption$ - Menu caption.
'
'----------------------------------------------------------------------------

MenuBar(WindowNum, Number).caption = caption$
MenuBar(WindowNum, Number).Items = 0

DrawMenu WindowNum

END SUB

SUB CreatePBar (WindowNum, num, x, y, XLen, YLen, Value, Max)
'----------------------------------------------------------------------------
'                   Creates a percent bar
'----------------------------------------------------------------------------
'
'  WindowNum - Host window index number
'        Num - Index of the percent bar
'       x, y - Coords.
' XLen, YLen - X Length, Y Length
'      Value - startup value (not in percents)
'        Max - Max value (not in percents)
'
'----------------------------------------------------------------------------
'  To change a percent bar value, just modify the PBar(Num).Value variable,
'  the percent bar will be redrawn automatically.
'----------------------------------------------------------------------------
IF num = 0 THEN EXIT SUB


PBar(num).Win = WindowNum
PBar(num).x = x
PBar(num).y = y
PBar(num).X2 = x + XLen
PBar(num).y2 = y + YLen
PBar(num).Value = Value
PBar(num).OldValue = Value
PBar(num).Max = Max

DrawPercentBar num

END SUB

SUB CreateRadioButton (WindowNum, num, Group, x, y, caption$, State)
'----------------------------------------------------------------------------
' Creates a Radio button
'----------------------------------------------------------------------------
'
' WindowNum - Host window number
'       Num - Radiobutton index number
'     Group - The number of the group the radio button belongs to
'             Only one radiobutton of each group can be selected at once.
'      x, y - Coords.
'  Caption$ - Caption
'     State - True: Selected, False: Not selected
'
'----------------------------------------------------------------------------
'   Read the state of the radiobutton from the Radio(Num).State variable
'----------------------------------------------------------------------------
IF num = 0 THEN EXIT SUB
Radio(num).Win = WindowNum
Radio(num).Group = Group
Radio(num).x = x
Radio(num).y = y
RadioText$(num) = caption$
Radio(num).State = State
Radio(num).Disabled = false

DrawRadio num, true

END SUB

SUB CreateScrollBar (WindowNum, num, dir, x, y, XLen, YLen, Max, Value)
'----------------------------------------------------------------------------
' Creates a Scrollbar
'----------------------------------------------------------------------------
'
' WindowNum - Host window number
'       Num - Scrollbar index number
'       Dir - Direction:
'             - 0: Vertical
'             - 1: Horizontal
'      x, y - Coords.
'XLen, YLen - X Length, Y Length
'       Max - Max value
'     Value - startup value
'
'----------------------------------------------------------------------------
'     Read the scrollbar's value from the variable SBar(Num).Value
'----------------------------------------------------------------------------
IF num = 0 THEN EXIT SUB
IF Max < Value THEN EXIT SUB
viewW (-1)

IF dir = 0 AND XLen < 44 THEN XLen = 44
IF dir = 1 AND YLen < 44 THEN YLen = 44

SBar(num).dir = dir
SBar(num).x = x
SBar(num).y = y
SBar(num).X2 = x + XLen
SBar(num).y2 = y + YLen
SBar(num).Max = Max
SBar(num).Value = Value
SBar(num).Win = WindowNum
SBar(num).button = (num * 2) + maxbuttons - 1

IF dir = 0 THEN
  SBar(num).SStep = (XLen - 43) / Max
  CreateButton WindowNum, (num * 2) + maxbuttons - 1, x, y, 14, YLen, CHR$(137)
  CreateButton WindowNum, (num * 2) + maxbuttons, x + XLen - 14, y, 14, YLen, CHR$(136)
ELSE
  SBar(num).SStep = (YLen - 43) / Max
  CreateButton WindowNum, (num * 2) + maxbuttons - 1, x, y, XLen, 14, CHR$(140)
  CreateButton WindowNum, (num * 2) + maxbuttons, x, y + YLen - 14, XLen, 14, CHR$(135)
END IF

DrawScrollBar num

END SUB

SUB CreateTextBox (Win, num, x, y, caption$)
'----------------------------------------------------------------------------
'                Creates a textbox (a simplified label)
'----------------------------------------------------------------------------
'
'     Win - Host window index number
'     Num - Textbox index number
'    x, y - coords.
'Caption$ - Textbox text
'
'----------------------------------------------------------------------------


IF TextBCaption$(num) <> "" THEN DrawTextBox num, true


TextB(num).x = x
TextB(num).y = y
TextBCaption$(num) = caption$
TextB(num).Win = Win

DrawTextBox num, false

END SUB

SUB CreateTextField (WindowNum, num, x, y, Length, txt$, Passwd)
'----------------------------------------------------------------------------
' Creates a Text Input Field
'----------------------------------------------------------------------------
'
' WindowNum - Host window number
'       Num - Textfield index number
'      x, y - Coords.
'    Length - Max. characters
'      Txt$ - Startup text
'    Passwd - If true, then "*" will be displayed instead of a character
'
'----------------------------------------------------------------------------
'   Read the text of the textbox from the TextText$(Num) variable
'----------------------------------------------------------------------------
IF num = 0 THEN EXIT SUB
txt$ = LEFT$(txt$, Length)
TextF(num).x = x
TextF(num).y = y
TextText$(num) = txt$
TextF(num).Max = Length
TextF(num).Win = WindowNum
TextF(num).Password = Passwd
TextF(num).Disabled = false

DrawTextField num

END SUB

SUB CreateWindow (num, x, y, X2, y2, Topic$, Fillcol, WinDrag)
'----------------------------------------------------------------------------
' Creates a window
'----------------------------------------------------------------------------
'
'     Num - Window index number
'    x, y - Upper left corner
'  x2, y2 - Lower right corner
'  Topic$ - Topic
' Fillcol - Fill color
' WinDrag - If false, then window cannot be moved
'
'----------------------------------------------------------------------------
IF num = 0 THEN EXIT SUB
Win(num).x = x
Win(num).y = y
Win(num).X2 = X2
Win(num).y2 = y2
WinTopic$(num) = Topic$

Win(num).TopicCol = DeActiveTopic
Win(num).TopicBCol = DeActiveTopicB
Win(num).Fillcol = Fillcol
Win(num).drag = WinDrag

DrawWindow (num)
IF Skin = 1 THEN
  temp = 1: b = 13
  CreateBMP aWIn, 1, temp, b, Skinfile$
  CreateBMP aWIn, 2, temp + 100, b, Skinfile$
  CreateBMP aWIn, 3, temp + 200, b, Skinfile$
  CreateBMP aWIn, 4, temp, b + 100, Skinfile$
  CreateBMP aWIn, 5, temp + 100, b + 100, Skinfile$
  CreateBMP aWIn, 6, temp + 200, b + 100, Skinfile$
END IF
aWIn = num

END SUB

SUB Defscreen
'----------------------------------------------------------------------------
'                      Defines screen parameters
'----------------------------------------------------------------------------
ScreenMode = 12                  'Screenmode to be used
ScreenXDim = 640                 'Max. X
ScreenYDim = 480                 'Max. Y
ColorDepth = 64                  'Number of colors

desktop = 8                      'Desktop color
DragLine = 60                    'Integrity of window outline when moving
ActiveTopic = 0                  'Active topic color
DeActiveTopic = 0                'Deactive topic color
ActiveTopicB = 9                'Active topic backround color
DeActiveTopicB = 1               'Deactive topic backround color
aWIn = 9                         'Active window at startup
AccessSkinColor = 0

Highlight = 15                   '3D-object highlight color
Fill = 7                         '3D-object fill color
Shadow = 8                       '3D-object shadow color
Idle = 60
saver = 1
CursorChar$ = CHR$(179)          'Cursor character
SCREEN ScreenMode: CLS
Win(0).x = 1
Win(0).y = 1
Win(0).X2 = 639
Win(0).y2 = 480
WinTopic$(0) = "Desktop"
Win(0).TopicCol = DeActiveTopic
Win(0).TopicBCol = DeActiveTopicB
Win(0).Fillcol = desktop
Win(0).drag = 0
GOTO selectit
    desktop = 4
    AccessTime = 1
    AccessHide = 1
    AccessPos = 1
    Skin = 0
    Skinfile$ = "c:\ryu.bmp"
    saver = 1
    Idle = 60
    KILL "c:\Access\system\system.ini"
    OPEN "c:\Access\system\system.ini" FOR OUTPUT AS #2
    PRINT #2, desktop, AccessTime, AccessHide, AccessPos, Skin, saver, Idle, Skinfile$
    CLOSE #2
selectit:
'IF exist%("c:\adfas.ad") = false THEN Dialog "Error", "Cannot find file","OK","",""
OPEN "c:\Access\system\system.ini" FOR INPUT AS #2
INPUT #2, desktop, AccessTime, AccessHide, AccessPos, Skin, saver, Idle, Skinfile$
CLOSE #2
Desktopa = desktop
IF Idle = 1 THEN Idle = 60
END SUB

SUB diagnostic
    desktop = 0
    AccessTime = 1
    AccessHide = 1
    AccessPos = 1
    Skin = 0
    Skinfile$ = "c:\ryu.bmp"
    saver = 1
    Idle = 60
CLS
PRINT "Access diagnostic mode(press X to exit)"
PRINT
PRINT "Diagnostic mode is designed to restore the default settings of"
PRINT "Access just in case Access has problems with the settings you"
PRINT "gave it."
PRINT : PRINT "Press 'n' for next"
DO
temp$ = INKEY$
IF temp$ = "x" THEN END
LOOP UNTIL LCASE$(INKEY$) = "n" OR INKEY$ = "x"
IF temp$ = "x" THEN RUN
CLS
PRINT "Access diagnostic mode": PRINT
PRINT "Do you want your Access menu to hide?(y/n)": DO: temp$ = INKEY$: LOOP UNTIL temp$ = "y" OR temp$ = "n" OR temp$ = "X": IF temp$ = "y" THEN AccessHide = 1: IF temp$ = "n" THEN AccessHide = 0: IF temp$ = "X" THEN RUN
PRINT "Do you want to see the time on your Access menu?(y/n)": DO: temp$ = INKEY$: LOOP UNTIL temp$ = "y" OR temp$ = "n" OR temp$ = "X": IF temp$ = "y" THEN AccessTime = 1: IF temp$ = "n" THEN AccessTime = 0: IF temp$ = "X" THEN RUN
PRINT "Do you want the Access bar at the top or bottom of the screen?(t/b)": DO: temp$ = INKEY$:  LOOP UNTIL temp$ = "t" OR temp$ = "b" OR temp$ = "X": IF temp$ = "t" THEN AccessPos = 2: IF temp$ = "b" THEN AccessPos = 1: IF temp$ = "X" THEN RUN
PRINT "Do you want to use a BMP file as a skin for your windows?(y/n)": DO: temp$ = INKEY$:  LOOP UNTIL temp$ = "y" OR temp$ = "n" OR temp$ = "X": IF temp$ = "y" THEN Skin = 1: IF temp$ = "n" THEN Skin = 0: IF temp$ = "X" THEN RUN
PRINT "Which Screen saver, Pallette changer, marquee or none?(1,2 or 0 for none)": DO: temp$ = INKEY$:  LOOP UNTIL temp$ = "1" OR temp$ = "2" OR temp$ = "0" OR temp$ = "X": IF temp$ = "1" THEN saver = 1: IF temp$ = "2" THEN saver = 2: IF temp$ = "0" _
 THEN saver = 0: IF temp$ = "X" THEN RUN
INPUT "How long do you want to wait for the screen saver to activate?(minutes)"; Idle
KILL "c:\Access\system\system.ini"
OPEN "c:\Access\system\system.ini" FOR OUTPUT AS #2
PRINT #2, desktop, AccessTime, AccessHide, AccessPos, Skin, saver, Idle, Skinfile$
CLOSE #2
CLS
PRINT "Access diagnostic mode": PRINT
PRINT "Press any key to restart Access"
DO
LOOP UNTIL INKEY$ > ""
RUN
END SUB

SUB Dialog (Topic$, Text$, Button1$, Button2$, Button3$)
'----------------------------------------------------------------------------
'                      Creates a dialogwindow
'----------------------------------------------------------------------------
'
'  Topic$ - Window topic
'   Text$ - Displayed text
'Button1$ - Button 1 text
'Button2$ - Button 2 text (if blank, then button won't be created)
'Button3$ - Button 3 text (if blank, then button won't be created)
'
'----------------------------------------------------------------------------
'       When a dialog is active, all other objects are disabled
'----------------------------------------------------------------------------
' Read the number of the pressed button in the dialog from the variable
' DialogPressedbutton (1, 2 or 3, counting from left)
'----------------------------------------------------------------------------
topica$ = Topic$
IF Button1$ = "" THEN EXIT SUB
x = ScreenXDim \ 2 - 110
y = ScreenYDim \ 2 - 80
X2 = ScreenXDim \ 2 + 110
y2 = ScreenYDim \ 2 + 80

CreateWindow MaxWindows, x, y, X2, y2, Topic$, Fill, false


DialogActive = true
DialogSelectedButton = 0
DialogAWin = aWIn
aWIn = MaxWindows

IF Button2$ = "" AND Button3$ = "" AND Button1$ <> "" THEN
  CreateButton MaxWindows, maxbuttons, (X2 - x) \ 2 - 30, (y2 - y) \ 2 + 30 - 10, 60, 20, Button1$
END IF

IF Button2$ <> "" AND Button3$ = "" AND Button1$ <> "" THEN
  CreateButton MaxWindows, maxbuttons, (X2 - x) \ 2 - 80, (y2 - y) \ 2 + 30 - 10, 60, 20, Button1$
  CreateButton MaxWindows, maxbuttons - 1, (X2 - x) \ 2 + 10, (y2 - y) \ 2 + 30 - 10, 60, 20, Button2$
END IF

IF Button2$ <> "" AND Button3$ <> "" AND Button1$ <> "" THEN
  CreateButton MaxWindows, maxbuttons, 10, (y2 - y) \ 2 + 30 - 10, 60, 20, Button1$
  CreateButton MaxWindows, maxbuttons - 1, (X2 - x) \ 2 - 35, (y2 - y) \ 2 + 30 - 10, 60, 20, Button2$
  CreateButton MaxWindows, maxbuttons - 2, (X2 - x) - 80, (y2 - y) \ 2 + 30 - 10, 60, 20, Button3$
END IF

Win(MaxWindows).TopicBCol = 1
Win(MaxWindows).TopicCol = 15

ReDrawWindow MaxWindows
HideMouse
printF x + 10, y + 40, wrap$(Text$, 34), 0, 16
ShowMouse
aWIn = MaxWindows
END SUB

SUB DisableButton (num)
'----------------------------------------------------------------------------
'                        Disables a button
'----------------------------------------------------------------------------
'
'  Num - Button number
'
'----------------------------------------------------------------------------



But(num).Disabled = true
DrawButton num, 0, 0

END SUB

SUB DisableCheckBox (num)
'----------------------------------------------------------------------------
'                        Disables a checkbox
'----------------------------------------------------------------------------
'
'  Num - Checkbox number
'
'----------------------------------------------------------------------------

CheckBox(num).Disabled = true

DrawCheckBox num, 0

END SUB

SUB DisableRadioButton (num)
'----------------------------------------------------------------------------
'                        Disables a radiobutton
'----------------------------------------------------------------------------
'
'  Num - Radiobutton number
'
'----------------------------------------------------------------------------

Group = Radio(num).Group

FOR i = 1 TO MaxRadioButtons
  IF Radio(i).Group = Group THEN Radio(i).Disabled = true: DrawRadio i, 0
NEXT

END SUB

SUB DisableTextField (num)
'----------------------------------------------------------------------------
'                        Disables a Textfield
'----------------------------------------------------------------------------
'
'  Num - Textfield number
'
'----------------------------------------------------------------------------

TextF(num).Disabled = true

DrawTextField num

END SUB

FUNCTION DMADone%
'----------------------------------------------------------------------------
'                Use to see if a DMA transfer has been completed
'----------------------------------------------------------------------------

count% = INP(LenPort%)
Count2% = INP(LenPort%)
count& = CLNG(count% + 1) * CLNG(Count2% + 1)
IF (count& - 1) >= &HFFFF& THEN junk% = INP(DSPDataAvail%): DMADone% = -1
END FUNCTION

SUB DMAPlay (Segment&, Offset&, Length&, Freq&)
' Transfers and plays the contents of the buffer.
Length& = Length& - 1
Page% = 0
MemLoc& = Segment& * 16 + Offset&
SELECT CASE Channel%
    CASE 0
       PgPort% = &H87
       AddPort% = &H0
       LenPort% = &H1
       ModeReg% = &H48
    CASE 1
       PgPort% = &H83
       AddPort% = &H2
       LenPort% = &H3
       ModeReg% = &H49
    CASE 2
       PgPort% = &H81
       AddPort% = &H4
       LenPort% = &H5
       ModeReg% = &H4A
    CASE 3
       PgPort% = &H82
       AddPort% = &H6
       LenPort% = &H7
       ModeReg% = &H4B
    CASE ELSE
       PRINT "DMA channels 0-3 only are supported."
       EXIT SUB
END SELECT

OUT &HA, &H4 + Channel%
OUT &HC, &H0
OUT &HB, ModeReg%
OUT AddPort%, MemLoc& AND &HFF
OUT AddPort%, (MemLoc& AND &HFFFF&) \ &H100
IF (MemLoc& AND 65536) THEN Page% = Page% + 1
IF (MemLoc& AND 131072) THEN Page% = Page% + 2
IF (MemLoc& AND 262144) THEN Page% = Page% + 4
IF (MemLoc& AND 524288) THEN Page% = Page% + 8
OUT PgPort%, Page%
OUT LenPort%, Length& AND &HFF
OUT LenPort%, (Length& AND &HFFFF&) \ &H100
OUT &HA, Channel%

IF Freq& < 23000 THEN
   TimeConst% = 256 - 1000000 \ Freq&
   WriteDSP &H40
   WriteDSP TimeConst%
   WriteDSP &H14
   WriteDSP (Length& AND &HFF)
   WriteDSP ((Length& AND &HFFFF&) \ &H100)
ELSE
   IF DSPVersion! >= 3 THEN
      TimeConst% = ((65536 - 256000000 \ Freq&) AND &HFFFF&) \ &H100
      WriteDSP &H40
      WriteDSP TimeConst%
      WriteDSP (Length& AND &HFF)
      WriteDSP ((Length& AND &HFFFF&) \ &H100)
      WriteDSP &H91
   ELSE
      PRINT "You need a Sound Blaster with a DSP v3.x+ to play at high speed."
      EXIT SUB
   END IF
END IF
END SUB

SUB DMAState (StopGo%)
' Stops or continues DMA play.
IF StopGo% THEN WriteDSP &HD4 ELSE WriteDSP &HD0

END SUB

SUB DoCopy
'----------------------------------------------------------------------------
'      Does the copying process for Backroundcopy, called by the system
'----------------------------------------------------------------------------

IF NOT Copying = true THEN EXIT SUB

FOR i = 1 TO CopyBytes
  IF CopyPos = LOF(CopyFile) THEN
    Copying = false
    CLOSE #CopyFile
    CLOSE #CopyFile2
    EXIT FOR
  END IF
  GET #CopyFile, , Byte
  PUT #CopyFile2, , Byte
  CopyPos = CopyPos + 1
  CopyDone = INT(CopyPos / LOF(CopyFile) * 100)
NEXT

END SUB

SUB DrawButton (num, Pressed, Hollow)
'----------------------------------------------------------------------------
'             Draws a button, called by the system
'----------------------------------------------------------------------------

WindowNum = But(num).Win
viewW (WindowNum)
HideMouse


x = But(num).x + Win(WindowNum).x + 5
y = But(num).y + Win(WindowNum).y + 21
X2 = But(num).X2 + Win(WindowNum).x + 5
y2 = But(num).y2 + Win(WindowNum).y + 21
caption$ = ButCaption$(num)


IF Pressed = 1 THEN
  LINE (x, y)-(X2, y), 0
  LINE (x, y)-(x, y2), 0
  LINE (X2, y2)-(X2, y), Fill
  LINE (X2, y2)-(x, y2), Fill

  LINE (x + 1, y + 1)-(X2 - 1, y + 1), Shadow
  LINE (x + 1, y + 1)-(x + 1, y2 - 1), Shadow
  LINE (X2 - 1, y2 - 1)-(X2 - 1, y + 1), Highlight
  LINE (X2 - 1, y2 - 1)-(x + 1, y2 - 1), Highlight
ELSE
  LINE (x, y)-(X2, y), Fill
  LINE (x, y)-(x, y2), Fill
  LINE (X2, y2)-(X2, y), 0
  LINE (X2, y2)-(x, y2), 0

  LINE (x + 1, y + 1)-(X2 - 1, y + 1), Highlight
  LINE (x + 1, y + 1)-(x + 1, y2 - 1), Highlight
  LINE (X2 - 1, y2 - 1)-(X2 - 1, y + 1), Shadow
  LINE (X2 - 1, y2 - 1)-(x + 1, y2 - 1), Shadow
END IF

IF Hollow = 0 THEN LINE (x + 2, y + 2)-(X2 - 2, y2 - 2), Fill, BF


IF Hollow = 0 THEN
  CapLines = 1
  FOR i = 1 TO LEN(caption$)
    IF MID$(caption$, i, 2) = "/n" THEN CapLines = CapLines + 1
  NEXT
  IF INSTR(caption$, "/n") > 0 THEN Capline$ = LEFT$(caption$, INSTR(caption$, "/n")) ELSE Capline$ = caption$

  Textpos% = ((X2 - x) \ 2) - ((LEN(Capline$) * FontX&) \ 2) + x + 1
  IF But(num).Disabled = false THEN
    printF INT(Textpos%), INT((y2 - y)) \ 2 + y - ((CapLines * 8) / 2) + 1, caption$, 0, 16
  ELSE
    printF INT(Textpos%), INT((y2 - y)) \ 2 + y - ((CapLines * 8) / 2) + 1, caption$, 8, 16
  END IF
END IF

ShowMouse
viewW (-1)
END SUB

SUB DrawCheckBox (num, DrawText)
'----------------------------------------------------------------------------
'             Draws a checkbox, called by the system
'----------------------------------------------------------------------------

IF num = 0 THEN EXIT SUB


WindowNum = CheckBox(num).Win
viewW (WindowNum)
HideMouse

x = CheckBox(num).x + Win(WindowNum).x + 5
y = CheckBox(num).y + Win(WindowNum).y + 21
State = CheckBox(num).State
caption$ = CheckBoxText$(num)

'Box3D x + 1, y + 1, x + 10, y + 10, DoubleIN, Highlight
LINE (x + 1, y + 1)-(x + 10, y + 1), 0
LINE (x + 1, y + 1)-(x + 1, y + 10), 0

LINE (x + 2, y + 2)-(x + 9, y + 2), Shadow
LINE (x + 2, y + 2)-(x + 2, y + 9), Shadow

LINE (x + 10, y + 10)-(x + 10, y + 1), Highlight
LINE (x + 10, y + 10)-(x + 1, y + 10), Highlight

LINE (x + 9, y + 9)-(x + 9, y + 2), Fill
LINE (x + 9, y + 9)-(x + 2, y + 9), Fill


IF CheckBox(num).Disabled = true THEN
  LINE (x + 3, y + 3)-(x + 8, y + 8), Fill, BF
ELSE
  LINE (x + 3, y + 3)-(x + 8, y + 8), Highlight, BF
END IF


IF State = true THEN
  LINE (x + 3, y + 3)-(x + 7, y + 7), Shadow
  LINE (x + 7, y + 3)-(x + 3, y + 7), Shadow
  LINE (x + 4, y + 3)-(x + 8, y + 7), 0
  LINE (x + 8, y + 3)-(x + 4, y + 7), 0
END IF


IF DrawText = true THEN printF x + 18, y + 2, caption$, 0, 16


ShowMouse
viewW (-1)
END SUB

SUB DrawFrame (num)
'----------------------------------------------------------------------------
'                 Draws a frame, called by the system
'----------------------------------------------------------------------------

IF num = 0 THEN EXIT SUB
WindowNum = Frame(num).Win

HideMouse
viewW WindowNum

x = Frame(num).x + Win(WindowNum).x + 5
X2 = Frame(num).X2 + Win(WindowNum).x + 5
y = Frame(num).y + Win(WindowNum).y + 21
y2 = Frame(num).y2 + Win(WindowNum).y + 21
caption$ = FrameCaption$(num)

Box3D x, y, X2, y2, SingleIn, 16
Box3D x + 1, y + 1, X2 - 1, y2 - 1, singleOut, 16
LINE (x + 2, y + 2)-(X2 - 2, y2 - 2), Fill, BF

IF caption$ <> "" THEN
  printF x + (FontX&), y - 3, caption$, 0, INT(Fill)
END IF

viewW -1
ShowMouse

END SUB

SUB DrawLabel (num)
'----------------------------------------------------------------------------
'             Draws a label, called by the system
'----------------------------------------------------------------------------

IF num = 0 THEN EXIT SUB
WindowNum = Label(num).Win


viewW (WindowNum)
HideMouse

x = Label(num).x + Win(WindowNum).x + 5
X2 = Label(num).X2 + Win(WindowNum).x + 5
y = Label(num).y + Win(WindowNum).y + 21
y2 = Label(num).y2 + Win(WindowNum).y + 21
caption$ = LabelCaption$(num)
FCol = Label(num).FCol
BCol = Label(num).BCol
BorderStyle = Label(num).Style
Align = Label(num).Align

Box3D x, y, X2, y2, BorderStyle, BCol

IF Label(num).wrap = 1 THEN caption$ = wrap$(caption$, (X2 - x) \ FontSpace& - 2)

CapLines = 1
FOR i = 1 TO LEN(caption$)
  IF MID$(caption$, i, 2) = "/n" THEN CapLines = CapLines + 1
NEXT
IF INSTR(caption$, "/n") > 0 THEN Capline$ = LEFT$(caption$, INSTR(caption$, "/n")) ELSE Capline$ = caption$

IF Align = 0 THEN Textpos = x + FontSpace& \ 2
IF Align = 1 THEN
  Textpos = ((X2 - x) \ 2) - (((LEN(Capline$)) * FontX&) \ 2) + x + 1
END IF

IF Align = 2 THEN Textpos = X2 - ((LEN(Capline$))) - FontSpace& \ 2

IF FCol <> 0 AND BCol = 0 THEN
  printF INT(Textpos), (y2 - y) \ 2 + y - ((CapLines * FontY&) / 2), caption$, INT(FCol), 0
ELSE
  printF INT(Textpos), (y2 - y) \ 2 + y - ((CapLines * FontY&) / 2), caption$, INT(FCol), 16
END IF

ShowMouse
viewW (-1)


END SUB

SUB DrawMenu (WindowNum)
'----------------------------------------------------------------------------
'             Draws a menu, called by the system
'----------------------------------------------------------------------------

'IF num = 0 THEN EXIT SUB
HideMouse

x = Win(WindowNum).x + 3
y = Win(WindowNum).y + 21
X2 = Win(WindowNum).X2 - 3


LINE (x, y - 2)-(X2, y + 10), Fill, BF
LINE (x, y + 11)-(X2, y + 11), Shadow, BF
LINE (x, y + 12)-(X2, y + 12), 0, BF

FOR i& = 1 TO 8
  IF LEN(RTRIM$(MenuBar(WindowNum, i&).caption)) <> 10 AND MenuBar(WindowNum, i&).Items > 0 THEN
    printF x + ((i& - 1) * 8 * 10) + 2, INT(y), MenuBar(WindowNum, i&).caption, 0, 16
  END IF
NEXT


ShowMouse
END SUB

SUB DrawMenuItems (WindowNum, Number)
'----------------------------------------------------------------------------
'             Draws a menu's items, called by the system
'----------------------------------------------------------------------------
'IF num = 0 THEN EXIT SUB
HideMouse

x = Win(WindowNum).x + 3 + ((Number - 1) * 8 * 10) + 2
y = Win(WindowNum).y + 21 + 11
X2 = Win(WindowNum).x + 3 + ((Number - 1) * 8 * 10) + 2 + 8 + (FontX& * 10)
y2 = Win(WindowNum).y + 21 + 14 + MenuBar(WindowNum, Number).Items * 12

Box3D x, y, X2, y2, DoubleOut, Fill

FOR i& = 1 TO MenuBar(WindowNum, Number).Items
  printF x + 5, y + i& * 12 - 8, menu(WindowNum, Number, i&), 0, 16
NEXT

ShowMouse

END SUB

SUB DrawPercentBar (num)
'----------------------------------------------------------------------------
'             Draws a percent bar, called by the system
'----------------------------------------------------------------------------
EXIT SUB
IF num = 0 THEN EXIT SUB

WindowNum = PBar(num).Win

x = PBar(num).x + Win(WindowNum).x + 5
X2 = PBar(num).X2 + Win(WindowNum).x + 5
y = PBar(num).y + Win(WindowNum).y + 21
y2 = PBar(num).y2 + Win(WindowNum).y + 21

IF MouseInside(x, y, X2, y2) = true THEN HideMouse
viewW WindowNum

Value = PBar(num).Value
Max = PBar(num).Max

XPos = ((X2 - x - 4) / 100) * (Value / Max * 100)

Box3D x, y, X2, y2, DoubleIn, 16

IF XPos < 3 THEN
  LINE (x + 2, y2 - 2)-(X2 - 2, y + 2), Fill, BF
  CALL printF(x + (X2 - x) \ 2 - FontSpace& * 2, y + 1 + (y2 - y) \ 2 - FontY& \ 2, LTRIM$(RTRIM$(STR$(INT(Value / Max * 100)))) + "%", 0, 16)
  ShowMouse
  viewW -1
  EXIT SUB
END IF


IF XPos >= (X2 - x) - 3 THEN XPos = (X2 - x) - 3





LINE (x + 2, y + 2)-(x + XPos, y + 2), 15
LINE (x + 2, y + 2)-(x + 2, y2 - 2), 15

LINE (x + 3, y + 3)-(x + XPos - 1, y + 3), 11
LINE (x + 3, y + 3)-(x + 3, y2 - 3), 11

LINE (x + 4, y + 4)-(x + XPos - 2, y2 - 4), 3, BF

LINE (x + XPos - 1, y2 - 3)-(x + XPos - 1, y + 4), 8
LINE (x + XPos - 1, y2 - 3)-(x + 4, y2 - 3), 8

LINE (x + XPos, y2 - 2)-(x + XPos, y + 3), 0
LINE (x + XPos, y2 - 2)-(x + 3, y2 - 2), 0

LINE (x + XPos + 1, y2 - 2)-(X2 - 2, y + 2), Fill, BF

CALL printF(x + (X2 - x) \ 2 - FontSpace& * 2, y + 1 + (y2 - y) \ 2 - FontY& \ 2, LTRIM$(RTRIM$(STR$(INT(Value / Max * 100)))) + "%", 0, 16)

IF MouseInside(x, y, X2, y2) = true THEN ShowMouse
viewW -1

END SUB

SUB DrawRadio (num, DrawText)
'----------------------------------------------------------------------------
'             Draws a radio button, called by the system
'----------------------------------------------------------------------------

IF num = 0 THEN EXIT SUB
WindowNum = Radio(num).Win
HideMouse
viewW (WindowNum)


x = Radio(num).x + Win(WindowNum).x + 5
y = Radio(num).y + Win(WindowNum).y + 21
Check% = Radio(num).State
caption$ = RadioText$(num)

IF Radio(num).Disabled = true THEN
  LINE (x + 2, y + 2)-(x + 6, y + 6), Fill, BF
ELSE
  LINE (x + 2, y + 2)-(x + 6, y + 6), Highlight, BF
END IF

PSET (x + 2, y + 2), Fill
PSET (x + 2, y + 6), Fill
PSET (x + 6, y + 6), Fill
PSET (x + 6, y + 2), Fill


PSET (x + 1, y + 1), 0
PSET (x + 2, y), Shadow
PSET (x, y + 2), Shadow
PSET (x, y + 3), Shadow
PSET (x + 3, y), Shadow
PSET (x + 4, y), Shadow
PSET (x, y + 4), Shadow
PSET (x + 5, y + 1), Shadow
PSET (x + 1, y + 5), Shadow

PSET (x + 5, y), Shadow
PSET (x, y + 5), Shadow

PSET (x + 2, y + 1), 0
PSET (x + 1, y + 2), 0


PSET (x + 7, y + 7), Highlight
PSET (x + 6, y + 7), Highlight
PSET (x + 7, y + 6), Highlight
PSET (x + 8, y + 5), Highlight
PSET (x + 5, y + 8), Highlight
PSET (x + 4, y + 8), Highlight
PSET (x + 8, y + 4), Highlight
PSET (x + 8, y + 3), Highlight
PSET (x + 3, y + 8), Highlight



PSET (x + 6, y + 1), Shadow
PSET (x + 1, y + 6), Shadow

IF Check% = true THEN
  LINE (x + 3, y + 3)-(x + 5, y + 5), 0, BF
  PSET (x + 3, y + 3), Fill
  PSET (x + 5, y + 5), Fill
  PSET (x + 3, y + 5), Fill
  PSET (x + 5, y + 3), Fill
END IF


IF DrawText = true THEN printF x + 18, y + 1, caption$, 0, 16


ShowMouse
viewW -1
END SUB

SUB DrawScrollBar (num)
'----------------------------------------------------------------------------
'             Draws a scrollbar, called by the system
'----------------------------------------------------------------------------
IF num = 0 THEN EXIT SUB
WindowNum = SBar(num).Win
viewW (WindowNum)
HideMouse


dir = SBar(num).dir

x = SBar(num).x + Win(WindowNum).x + 5
y = SBar(num).y + Win(WindowNum).y + 21
X2 = SBar(num).X2 + Win(WindowNum).x + 5
y2 = SBar(num).y2 + Win(WindowNum).y + 21
Max = SBar(num).Max
Value = SBar(num).Value
SStep! = SBar(num).SStep

IF dir = 0 THEN
  LINE (x + 16, y + 2)-(x + 16 + (Value * SStep!) - 1, y2 - 3), Shadow, BF
  LINE (X2 - 16, y2 - 3)-(x + 16 + (Value * SStep!) + 11, y + 2), Shadow, BF
  Box3D x + 15, y + 1, X2 - 15, y2 - 2, 3, 16
  Box3D x + 16 + (Value * SStep!), y + 2, x + 16 + (Value * SStep!) + 10, y2 - 3, 2, Fill
ELSE
  LINE (x + 2, y + 16)-(X2 - 3, y + 16 + (Value * SStep!) - 1), Shadow, BF
  LINE (X2 - 3, y2 - 16)-(x + 3, y + 27 + (Value * SStep!) - 1), Shadow, BF
  Box3D x + 2, y + 15, X2 - 2, y2 - 15, 3, 16
  Box3D x + 2, y + 16 + (Value * SStep!), X2 - 3, y + 16 + (Value * SStep!) + 10, 2, Fill
END IF


viewW (-1)
ShowMouse

END SUB

SUB DrawTextBox (num, EraseOld)
'----------------------------------------------------------------------------
'                          Draws a checkbox
'----------------------------------------------------------------------------

IF num = 0 THEN EXIT SUB
HideMouse
viewW TextB(num).Win

IF EraseOld = true THEN
  TempFill = Fill
  printF TextB(num).x + Win(TextB(num).Win).x + 5, TextB(num).y + Win(TextB(num).Win).y + 21, TextBCaption$(num), TempFill, TempFill
ELSE
  printF TextB(num).x + Win(TextB(num).Win).x + 5, TextB(num).y + Win(TextB(num).Win).y + 21, TextBCaption$(num), 0, Fill
END IF


ShowMouse
viewW -1
END SUB

SUB DrawTextField (num)
'----------------------------------------------------------------------------
'             Draws a textfield, called by the system
'----------------------------------------------------------------------------

WindowNum = TextF(num).Win
viewW (WindowNum)
HideMouse

x = TextF(num).x + Win(WindowNum).x + 5
y = TextF(num).y + Win(WindowNum).y + 21
txt$ = TextText$(num)
Length = TextF(num).Max
Passwd = TextF(num).Password


IF TextF(num).Disabled = true THEN
  Box3D x, y, x + (Length * FontSpace&) + 19, y + 14, 1, Fill
ELSE
  Box3D x, y, x + (Length * FontSpace&) + 19, y + 14, 1, 15
END IF

IF Passwd = false THEN printF x + 6, y + 4, txt$, 0, 16 ELSE printF x + 6, y + 4, STRING$(LEN(txt$), "*"), 0, 16

viewW (-1)
ShowMouse

END SUB

SUB DrawWindow (num)
'----------------------------------------------------------------------------
'             Draws a window, called by the system
'----------------------------------------------------------------------------

TaskMan 0
IF num = 0 THEN EXIT SUB
HideMouse
viewW -1
x = Win(num).x
y = Win(num).y
X2 = Win(num).X2
y2 = Win(num).y2

Topic$ = WinTopic$(num)
TopicCol = Win(num).TopicCol
TopicBCol = Win(num).TopicBCol
Fillcol = Win(num).Fillcol

IF x = 0 AND y = 0 AND X2 = 0 AND y2 = 0 THEN ShowMouse: EXIT SUB


IF Fillcol = Fill THEN
  LINE (x + 2, y + 2)-(X2 - 2, y2 - 2), Fill, BF
ELSE
  LINE (x + 2, y + 2)-(X2 - 1, y + 3), Fill, BF
  LINE (x + 2, y + 17)-(X2 - 1, y + 18), Fill, BF
  LINE (x + 2, y + 2)-(x + 2, y2 - 2), Fill, BF
  LINE (X2 - 1, y2 - 1)-(x + 1, y2 - 4), Fill, BF
  LINE (X2 - 1, y2 - 1)-(X2 - 4, y + 2), Fill, BF
END IF

LINE (X2 - 1, y2 - 1)-(x, y2 - 1), Shadow
LINE (X2 - 1, y2 - 1)-(X2 - 1, y), Shadow
LINE (X2, y2)-(x, y2), 0
LINE (X2, y2)-(X2, y), 0



LINE (x + 1, y + 1)-(x + 1, y2 - 3), Highlight
LINE (x + 1, y + 1)-(X2 - 1, y + 1), Highlight
PSET (x + 1, y2 - 2), Fill
LINE (x, y)-(x, y2), Fill
LINE (x, y)-(X2, y), Fill

LINE (x + 4, y + 20)-(x + 4, y2 - 5), 0
LINE (x + 4, y + 20)-(X2 - 5, y + 20), 0
LINE (x + 3, y + 19)-(x + 3, y2 - 5), Shadow
LINE (x + 3, y + 19)-(X2 - 5, y + 19), Shadow

LINE (X2 - 3, y2 - 3)-(x + 2, y2 - 3), Highlight
LINE (X2 - 3, y2 - 3)-(X2 - 3, y + 19), Highlight
LINE (x + 5, y + 21)-(X2 - 5, y2 - 5), Fillcol, BF

LINE (x + 3, y + 4)-(X2 - 4, y + 16), TopicBCol, BF
PSET (x, y), desktop
PSET (X2, y), desktop
PSET (x, y2), desktop



printF x + 6, y + 6, Topic$, INT(TopicCol), 16


FOR i& = 1 TO 8
  IF LEN(RTRIM$(MenuBar(num, i&).caption)) <> 10 THEN
    DrawMenu num
  END IF
NEXT
IF num = 9 OR num = 8 THEN GOTO skip1
wx = X2 - 17
wy = y + 4
wx2 = wx + 13
wy2 = wy + 12
Box3D wx - 15, wy, wx2 - 15, wy2, DoubleOut, 7 'maximize
printF wx - 15 + 1, wy + 3, "[]", 0, 16
Box3D wx, wy, wx2, wy2, DoubleOut, 7 'close
printF wx + 4, wy + 3, "X", 0, 16
Box3D wx - 30, wy, wx2 - 30, wy2, DoubleOut, 7 'minimize
printF wx - 30 + 4, wy + 2, "_", 0, 16
Box3D wx - 45, wy, wx2 - 45, wy2, DoubleOut, 7 'Help
printF wx - 45 + 4, wy + 3, "?", 0, 16
  IF num = 8 OR num = 9 OR aWIn = 8 OR aWIn = 9 THEN GOTO skip1
CreateMenu aWIn, 1, "Access"
AddMenuItem aWIn, 1, "/c04Help"
AddMenuItem aWIn, 1, "Minimize"
AddMenuItem aWIn, 1, "Maximize"
AddMenuItem aWIn, 1, "Restore"
AddMenuItem aWIn, 1, "Close"
skip1:
IF num = 9 THEN y2 = 25
aWIn = num
ShowMouse
END SUB

FUNCTION DSPVersion!
' Gets the DSP version.
WriteDSP &HE1
temp% = ReadDSP%
temp2% = ReadDSP%
DSPVersion! = VAL(STR$(temp%) + "." + STR$(temp2%))
END FUNCTION

SUB EnableButton (num)
'----------------------------------------------------------------------------
'                        Enables a disabled button
'----------------------------------------------------------------------------
'
' Num - Button number
'
'----------------------------------------------------------------------------

But(num).Disabled = false
DrawButton num, 0, 0

END SUB

SUB EnableCheckBox (num)
'----------------------------------------------------------------------------
'                        Enables a disabled checkbox
'----------------------------------------------------------------------------
'
' Num - Checkbox number
'
'----------------------------------------------------------------------------

CheckBox(num).Disabled = false

DrawCheckBox num, 0


END SUB

SUB EnableRadioButton (num)
'----------------------------------------------------------------------------
'                        Enables a disabled radio
'----------------------------------------------------------------------------
'
' Num - Radiobutton number
'
'----------------------------------------------------------------------------

Group = Radio(num).Group

FOR i = 1 TO MaxRadioButtons
  IF Radio(i).Group = Group THEN Radio(i).Disabled = false: DrawRadio i, 0
NEXT

END SUB

SUB EnableTextField (num)
'----------------------------------------------------------------------------
'                        Enables a disabled textfield
'----------------------------------------------------------------------------
'
' Num - Textfield number
'
'----------------------------------------------------------------------------

TextF(num).Disabled = false

DrawTextField num

END SUB

FUNCTION Error$ (Virhe)
'----------------------------------------------------------------------------
'             Returns error code description
'----------------------------------------------------------------------------
' Virhe - Number of error
'----------------------------------------------------------------------------

SELECT CASE Virhe
CASE IS = 1
Error$ = "NEXT without FOR"
CASE IS = 2
Error$ = "Syntax error"
CASE IS = 3
Error$ = "RETURN without GOSUB"
CASE IS = 4
Error$ = "Out of DATA"
CASE IS = 5
Error$ = "Illegal function call"
CASE IS = 6
Error$ = "Overflow"
CASE IS = 7
Error$ = "Out of memory"
CASE IS = 8
Error$ = "Label not defined"
CASE IS = 9
Error$ = "Subscript out of range"
CASE IS = 10
Error$ = "Duplicate definition"
CASE IS = 11
Error$ = "Division by zero"
CASE IS = 12
Error$ = "Illegal in direct mode"
CASE IS = 13
Error$ = "Type mismatch"
CASE IS = 14
Error$ = "Out of string space"
CASE IS = 16
Error$ = "String formula too complex"
CASE IS = 17
Error$ = "Cannot continue"
CASE IS = 18
Error$ = "Function not defined"
CASE IS = 19
Error$ = "No RESUME"
CASE IS = 20
Error$ = "RESUME without error"
CASE IS = 24
Error$ = "Device timeout"
CASE IS = 26
Error$ = "Device fault"
CASE IS = 27
Error$ = "Out of paper"
CASE IS = 29
Error$ = "WHILE without WEND"
CASE IS = 30
Error$ = "WEND without WHILE"
CASE IS = 33
Error$ = "Duplicate label"
CASE IS = 35
Error$ = "Subprogram not defined"
CASE IS = 37
Error$ = "Argument-count mismatch"
CASE IS = 38
Error$ = "Array not defined"
CASE IS = 40
Error$ = "Variable required"
CASE IS = 50
Error$ = "FIELD overflow"
CASE IS = 51
Error$ = "Internal error"
CASE IS = 52
Error$ = "Bad file name of number"
CASE IS = 53
Error$ = "File not found"
CASE IS = 54
Error$ = "Bad file mode"
CASE IS = 55
Error$ = "File already open"
CASE IS = 56
Error$ = "FIELD statement active"
CASE IS = 57
Error$ = "Device I/O error"
CASE IS = 58
Error$ = "File already exists"
CASE IS = 59
Error$ = "Bad record length"
CASE IS = 61
Error$ = "Disk full"
CASE IS = 62
Error$ = "Input past end of file"
CASE IS = 63
Error$ = "Bad record number"
CASE IS = 64
Error$ = "Bad file name"
CASE IS = 67
Error$ = "Too many files"
CASE IS = 68
Error$ = "Device unavailable"
CASE IS = 69
Error$ = "Communication-buffer overflow"
CASE IS = 70
Error$ = "Permission denied"
CASE IS = 71
Error$ = "Disk not ready"
CASE IS = 72
Error$ = "Disk-media error"
CASE IS = 73
Error$ = "Feature unavailable"
CASE IS = 74
Error$ = "Rename across disks"
CASE IS = 75
Error$ = "Path/File access error"
CASE IS = 76
Error$ = "Path not found"
CASE ELSE
Error$ = ""
END SELECT
END FUNCTION

FUNCTION Exist% (file$)
'----------------------------------------------------------------------------
'                       Checks if a file exists
'----------------------------------------------------------------------------
'
'    File$ - File to check
'
'----------------------------------------------------------------------------
'  Returns True if file exists, else False
'----------------------------------------------------------------------------


IF file$ = "" THEN Exist = false: EXIT FUNCTION
Checkfile = FREEFILE
OPEN file$ FOR BINARY AS #Checkfile
IF LOF(Checkfile) = 0 THEN
  Exist = false
  CLOSE #Checkfile
  KILL file$
ELSE
  Exist = true
  CLOSE #Checkfile
END IF
END FUNCTION

SUB fadeout
FOR i = 0 TO 255
OUT &H3C7, i
col2(i, 1) = INP(&H3C9)
col2(i, 2) = INP(&H3C9)
col2(i, 3) = INP(&H3C9)
NEXT i
FOR j = 0 TO 63
FOR i = 0 TO 255
OUT &H3C8, i
OUT &H3C9, INT(((63 - j) * col2(i, 1)) / 63 + (j * col(i, 1)) / 63)
OUT &H3C9, INT(((63 - j) * col2(i, 2)) / 63 + (j * col(i, 2)) / 63)
OUT &H3C9, INT(((63 - j) * col2(i, 3)) / 63 + (j * col(i, 3)) / 63)
NEXT i
NEXT j


END SUB

SUB FMVolume (right%, left%, Getvol%)
'----------------------------------------------------------------------------
' Sets the FM-volume on the Sound Blaster card
'----------------------------------------------------------------------------
'
'  Right% - Right volume
'   Left% - Left volume
' Getvol% - If true, then volume will be read, not set
'
'----------------------------------------------------------------------------

OUT BasePort% + 4, &H26
IF Getvol% THEN
   left% = INP(BasePort% + 5) \ 16
   right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (right% + left% * 16) AND &HFF
END IF
END SUB

FUNCTION GetBit (Byte, Bit)
'----------------------------------------------------------------------------
'         Returns the state of the bit number 'Bit' in integer 'Byte'
'----------------------------------------------------------------------------
   SELECT CASE Bit
      CASE 1: IF (Byte AND 128) THEN GetBit = 1
      CASE 2: IF (Byte AND 64) THEN GetBit = 1
      CASE 3: IF (Byte AND 32) THEN GetBit = 1
      CASE 4: IF (Byte AND 16) THEN GetBit = 1
      CASE 5: IF (Byte AND 8) THEN GetBit = 1
      CASE 6: IF (Byte AND 4) THEN GetBit = 1
      CASE 7: IF (Byte AND 2) THEN GetBit = 1
      CASE 8: IF (Byte AND 1) THEN GetBit = 1
   END SELECT


END FUNCTION

SUB GetBLASTER (DMA%, BasePort%, IRQ%)
'----------------------------------------------------------------------------
' This subroutine parses the BLASTER environment string and returns settings.
'----------------------------------------------------------------------------

IF LEN(ENVIRON$("BLASTER")) = 0 THEN PRINT "BLASTER environment variable not set.": EXIT SUB
FOR Length% = 1 TO LEN(ENVIRON$("BLASTER"))
   SELECT CASE MID$(ENVIRON$("BLASTER"), Length%, 1)
      CASE "A"
        BasePort% = VAL("&H" + MID$(ENVIRON$("BLASTER"), Length% + 1, 3))
      CASE "I"
        IRQ% = VAL(MID$(ENVIRON$("BLASTER"), Length% + 1, 1))
      CASE "D"
        DMA% = VAL(MID$(ENVIRON$("BLASTER"), Length% + 1, 1))
   END SELECT
NEXT

END SUB

SUB GetFont
'----------------------------------------------------------------------------
'                              Loads the font
'----------------------------------------------------------------------------

SetPal 15, 0, 0, 0

DIM pic AS STRING * 1
OPEN "c:\Access\Winfont.dat" FOR BINARY AS #3
FOR xx& = 0 TO 318
  FOR yy& = 0 TO 16
    GET #3, , pic
    PSET (xx&, yy&), ASC(pic)
  NEXT
NEXT
DEF SEG
CLOSE #3

x = 0
y = 0
FOR i = 33 TO 138
  IF x > 312 THEN x = 0: y = y + 8
  GET (x, y)-(x + FontX& - 1, y + FontY& - 1), Font((i - 32) * 48)
  x = x + FontX&
NEXT

SetPal 15, 63, 63, 63
END SUB

SUB GPFRuutu
'----------------------------------------------------------------------------
' If an error occurs, then this screen is printed.
'----------------------------------------------------------------------------
'
' Variables:
'
'        Virhe - A custom error number
'      QBVirhe - A QBasic error number
' Virhekuvaus$ - Description of the error (QBasic error descriptions will
'                be printed automatically)
'
'----------------------------------------------------------------------------


SCREEN 0: CLS
WIDTH 80, 25
COLOR 15, 0: CLS
COLOR 0, 7
PRINT STRING$(80, " ")
LOCATE 1
CenterText "Error detected"
COLOR 14, 0
PRINT
PRINT " þ Access has detected error #";
COLOR 11
PRINT Trim$(Virhe);
COLOR 14
PRINT " at";
COLOR 11
PRINT TIMER;
COLOR 14
PRINT "(";
COLOR 11
PRINT TIME$;
COLOR 14
PRINT ")"
IF Virhekuvaus$ <> "" THEN
  LOCATE 5
  COLOR 8
  PRINT STRING$(80, "Ä")
  COLOR 7
  LOCATE 5
  CenterText "[Error description]"
  COLOR 15

  PRINT Virhekuvaus$
  COLOR 8
  PRINT STRING$(80, "Ä")
  COLOR 14
END IF
PRINT
PRINT " þ Program can no longer run.  If you would like to restart, press 'r'."
PRINT "   Otherwise, press any key to end Access shell."
PRINT
COLOR 14
PRINT " þ Memory:"
COLOR 7
PRINT "     String:",
COLOR 11
PRINT FRE("")
COLOR 7
PRINT "     Stack:",
COLOR 11
PRINT FRE(-2)
COLOR 7
PRINT "     Array:",
COLOR 11
PRINT FRE(-1)
PRINT
COLOR 14
PRINT " þ System:"
COLOR 7
PRINT "     Version:", ,
COLOR 11
PRINT " "; Version.Major; "."; Version.Minor
COLOR 7
PRINT "     Active window:",
COLOR 11
PRINT aWIn
COLOR 7
PRINT "     Clicked button:",
COLOR 11
PRINT clickedButton
COLOR 7
PRINT "     QBasic error:",
COLOR 11
PRINT QBVirhe; "("; Error$(QBVirhe); ")"
COLOR 7
PRINT "     Mouse coordinates:",
COLOR 11
PRINT " x:"; hx, "y:"; hy
COLOR 7
DO
temp$ = INKEY$
LOOP UNTIL temp$ > ""
IF LCASE$(temp$) = "r" THEN RUN
END
END SUB

SUB Help
IF aWIn = 0 OR aWIn = 9 THEN NoteBook "c:\access\access.hlp", 1: NoteBook "c:\access\access.hlp", 2
IF aWIn = 2 OR aWIn = 4 OR aWIn = 5 OR aWIn = 7 OR aWIn = 11 THEN NoteBook "c:\access\control.hlp", 1: NoteBook "c:\access\control.hlp", 2
IF aWIn = 1 THEN NoteBook "c:\access\command.hlp", 1: NoteBook "c:\access\command.hlp", 2
END SUB

SUB HideMouse
'----------------------------------------------------------------------------
'                   Hides the mouse cursor
'----------------------------------------------------------------------------

  ax% = 2                   'Funktio 2
  Hiiriajuri ax%, 0, 0, 0   'Kutsutaan hiiriajuria
END SUB

SUB Hiiriajuri (ax%, bx%, cx%, dx%)
  DEF SEG = VARSEG(hiiri$)                  'Segmentti talteen
  hiiri% = SADD(hiiri$)                     'Offsetti talteen
  CALL absolute(ax%, bx%, cx%, dx%, hiiri%) 'Kutsu
END SUB

SUB Hiirilue (vasen%, oikea%, keski%, x%, y%)
'----------------------------------------------------------------------------
'                      Returns mouse status
'----------------------------------------------------------------------------
 
  ax% = 3                         'Funktio 3
  Hiiriajuri ax%, bx%, cx%, dx%   'Kutsutaan hiiriajuria
  vasen% = ((bx% AND 1) <> 0)     'Luetaan nappien asennot
  oikea% = ((bx% AND 2) <> 0)     'bx:st„
  keski% = ((bx% AND 4) <> 0)
  x% = cx%                        'ja hiiren koordinaatit
  y% = dx%                        'cx:st„ ja dx:st„
END SUB

SUB Hiirirajat (X1%, y1%, X2%, y2%)
  ax% = 7                         'Funktio 7
  cx% = X1%                       'Rajat cx:„„n ja dx:„„n
  dx% = X2%
  Hiiriajuri ax%, 0, cx%, dx%     'Kutsutaan hiiriajuria
  ax% = 8                         'Funktio 8
  cx% = y1%                       'Rajat cx:„„n ja dx:„„n
  dx% = y2%
  Hiiriajuri ax%, 0, cx%, dx%     'Kutsutaan hiiriajuria
END SUB

FUNCTION Hiiritarkista%
  ax% = 0                        'Funktio 0
  Hiiriajuri ax%, bx%, 0, 0      'Kutsutaan hiiriajuria
  IF (ax% = 0) THEN              'Onko ajuri k„yt”ss„?
    Hiiritarkista% = 0           'jos ei, palautetaan 0
  ELSEIF (bx% = 3) THEN          'Kolminappinen hiiri?
    Hiiritarkista% = 3
  ELSEIF (bx% = 0) THEN          'Ep„standardi hiiri?
    Hiiritarkista% = 1
  ELSE
    Hiiritarkista% = 2           'Tavallinen kaksinappinen hiiri?
  END IF
END FUNCTION

SUB Initscreen
'----------------------------------------------------------------------------
' Defines the objects
'----------------------------------------------------------------------------
' This routine is called at startup
'----------------------------------------------------------------------------
temp$ = INKEY$
IF temp$ = "s" THEN EXIT SUB
'END
' $INCLUDE: 'c:\access\autold.sct'
END SUB

SUB InputDialog (Topic$, Text$, Password)
'----------------------------------------------------------------------------
'                  Creates an inputdialog, similar to dialog
'----------------------------------------------------------------------------
'
'  Topic$ - Window topic
'   Text$ - Displayed text
'Password - If true, then a password textfield will be created
'           ("*" instead of chars)
'
'----------------------------------------------------------------------------
'    When an inputdialog is active, all other objects are disabled
'----------------------------------------------------------------------------
' Read the number of the pressed button in the inputdialog from the variable
' InputDialogPressedbutton (1: OK, 2: Cancel)
'----------------------------------------------------------------------------
' Read the textfield's text from the variable TextText$(MaxTextFields)
'----------------------------------------------------------------------------

x = ScreenXDim \ 2 - 160
y = ScreenYDim \ 2 - 80
X2 = ScreenXDim \ 2 + 160
y2 = ScreenYDim \ 2 + 80

CreateWindow MaxWindows, x, y, X2, y2, Topic$, Fill, false


InputDialogActive = true
InputDialogClickedButton = 0
InputDialogAWin = aWIn
aWIn = MaxWindows

CreateTextField MaxWindows, MaxTextFields, 6, (y2 - y) \ 2 - 10, 47, "", Password
Win(MaxWindows).TopicBCol = 1
Win(MaxWindows).TopicCol = 15

CreateButton MaxWindows, maxbuttons, (X2 - x) \ 2 - 80, (y2 - y) \ 2 + 30 - 10, 60, 20, "OK"
CreateButton MaxWindows, maxbuttons - 1, (X2 - x) \ 2 + 10, (y2 - y) \ 2 + 30 - 10, 60, 20, "Cancel"

ReDrawWindow MaxWindows
HideMouse
printF x + 10, y + 40, wrap$(Text$, 52), 0, Fill
ShowMouse

END SUB

SUB killButton (num)
'----------------------------------------------------------------------------
'                          Removes a button
'----------------------------------------------------------------------------
'
' Num - Button number
'
'----------------------------------------------------------------------------


IF But(num).x = ScreenXDim + 1000 THEN EXIT SUB

IF aWIn <> But(num).Win THEN
 aWIn = But(num).Win
 UpdateScreen
END IF


But(num).x = ScreenXDim + 1000
But(num).y = ScreenYDim + 1000
But(num).X2 = ScreenXDim + 1000
But(num).y2 = ScreenYDim + 1000

ReDrawWindow But(num).Win


END SUB

SUB KillCheckBox (num)
'----------------------------------------------------------------------------
'                          Removes a checkbox
'----------------------------------------------------------------------------
'
' Num - Checkbox number
'
'----------------------------------------------------------------------------

IF CheckBox(num).x = ScreenXDim + 1000 THEN EXIT SUB

IF aWIn <> CheckBox(num).Win THEN
 aWIn = CheckBox(num).Win
 UpdateScreen
END IF


CheckBox(num).x = ScreenXDim + 1000
CheckBox(num).y = ScreenYDim + 1000

ReDrawWindow CheckBox(num).Win



END SUB

SUB KillLabel (num)
'----------------------------------------------------------------------------
'                          Removes a label
'----------------------------------------------------------------------------
'
' Num - Label number
'
'----------------------------------------------------------------------------


IF Label(num).x = ScreenXDim + 1000 THEN EXIT SUB

IF aWIn <> Label(num).Win THEN
 aWIn = But(num).Win
 UpdateScreen
END IF


Label(num).x = ScreenXDim + 1000
Label(num).y = ScreenYDim + 1000
Label(num).X2 = ScreenXDim + 1000
Label(num).y2 = ScreenYDim + 1000

ReDrawWindow Label(num).Win


END SUB

SUB KillPBar (num)

IF PBar(num).x = ScreenXDim + 1000 THEN EXIT SUB

IF aWIn <> PBar(num).Win THEN
 aWIn = PBar(num).Win
 UpdateScreen
END IF


PBar(num).x = ScreenXDim + 1000
PBar(num).y = ScreenYDim + 1000
PBar(num).X2 = ScreenXDim + 1000
PBar(num).y2 = ScreenYDim + 1000

ReDrawWindow PBar(num).Win


END SUB

SUB KillRadioButton (num)
'----------------------------------------------------------------------------
'                          Removes a radiobutton
'----------------------------------------------------------------------------
'
' Num - Radiobutton number
'
'----------------------------------------------------------------------------
IF Radio(num).x = ScreenXDim + 1000 THEN EXIT SUB

IF aWIn <> Radio(num).Win THEN
 aWIn = Radio(num).Win
 UpdateScreen
END IF


Radio(num).x = ScreenXDim + 1000
Radio(num).y = ScreenYDim + 1000

ReDrawWindow Radio(num).Win



END SUB

SUB KillScrollbar (num)
'----------------------------------------------------------------------------
'                          Removes a scrollbar
'----------------------------------------------------------------------------
'
' Num - Scrollbar number
'
'----------------------------------------------------------------------------

IF SBar(num).x = ScreenXDim + 1000 THEN EXIT SUB


IF aWIn <> SBar(num).Win THEN
 aWIn = SBar(num).Win
 UpdateScreen
END IF

SBar(num).x = ScreenXDim + 1000
SBar(num).y = ScreenYDim + 1000
SBar(num).X2 = ScreenXDim + 1000
SBar(num).y2 = ScreenYDim + 1000



killButton SBar(num).button
killButton SBar(num).button + 1

ReDrawWindow aWIn

END SUB

SUB killTextField (num)
'----------------------------------------------------------------------------
'                          Removes a textfield
'----------------------------------------------------------------------------
'
' Num - Textfield number
'
'----------------------------------------------------------------------------

IF TextF(num).x = ScreenXDim + 1000 THEN EXIT SUB


IF aWIn <> TextF(num).Win THEN
 aWIn = TextF(num).Win
 UpdateScreen
END IF


TextF(num).x = ScreenXDim + 1000
TextF(num).y = ScreenYDim + 1000

DrawTextField num

END SUB

SUB killWindow (num)
'----------------------------------------------------------------------------
'                          Removes a button
'----------------------------------------------------------------------------
'
' Num - Removes a window
'
'----------------------------------------------------------------------------
' All the window's objects must be deleted separately before the window is
' deleted. Otherwise some objects may not function right.
'----------------------------------------------------------------------------

IF Win(num).x = ScreenXDim + 1000 THEN EXIT SUB
IF num = 9 OR num = 8 THEN EXIT SUB
StatusMSG$ = ""
x = Win(num).x
y = Win(num).y
X2 = Win(num).X2
y2 = Win(num).y2
RestoreW(num).x = 0
RestoreW(num).y = 0
RestoreW(num).X2 = 0
RestoreW(num).y2 = 0

HideMouse
LINE (x, y)-(X2, y2), desktop, BF
ShowMouse

Win(num).x = ScreenXDim + 1000
Win(num).y = ScreenYDim + 1000
Win(num).X2 = ScreenXDim + 1000
Win(num).y2 = ScreenYDim + 1000
WinTopic$(num) = ""
UpdateScreen
TaskMan 1
END SUB

SUB LabelText (num, caption$, FCol, BCol)
'----------------------------------------------------------------------------
'               Changes the label text
'----------------------------------------------------------------------------
'
'      Num - Label number
' Caption$ - Label caption
'     FCol - Front color (if -1, then it will not be modified)
'     BCol - Back color (if -1, then it will not be modified)
'
'----------------------------------------------------------------------------

IF NOT caption$ = "" THEN LabelCaption$(num) = caption$
IF NOT FCol = -1 THEN Label(num).FCol = FCol
IF NOT BCol = -1 THEN Label(num).BCol = BCol

DrawLabel num

END SUB

SUB LineVolume (right%, left%, Getvol%)
'----------------------------------------------------------------------------
' Sets the Line in-volume on the Sound Blaster card
'----------------------------------------------------------------------------
'
'  Right% - Right volume
'   Left% - Left volume
' Getvol% - If true, then volume will be read, not set
'
'----------------------------------------------------------------------------

OUT BasePort% + 4, &H2E
IF Getvol% THEN
   left% = INP(BasePort% + 5) \ 16
   right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (right% + left% * 16) AND &HFF
END IF
END SUB

SUB Load640 (file$, StartLine!)
'----------------------------------------------------------------------------
'                Loads a screen saved by Save640
'----------------------------------------------------------------------------
'
'      File$ - Filename to load the screen from (NO EXTENSION!)
' startLine! - start line
'
'----------------------------------------------------------------------------


StartLine! = (StartLine! * ScreenXDim) \ 8

DEF SEG = &HA000
IF ScreenMode = 13 THEN
  BLOAD file$ + ".INT", 0
ELSE
3  OUT &H3C4, 2: OUT &H3C5, 1: BLOAD file$ + ".BLU", StartLine!
  OUT &H3C4, 2: OUT &H3C5, 2: BLOAD file$ + ".GRN", StartLine!
  OUT &H3C4, 2: OUT &H3C5, 4: BLOAD file$ + ".RED", StartLine!
  OUT &H3C4, 2: OUT &H3C5, 8: BLOAD file$ + ".INT", StartLine!
  OUT &H3C4, 2: OUT &H3C5, 16
END IF

DEF SEG
END SUB

SUB LoadBMP (num)
'----------------------------------------------------------------------------
'              Loads a BMP-file, called by the system
'----------------------------------------------------------------------------




BMPFile = FREEFILE
xx = Bitmap(num).x + Win(Bitmap(num).Win).x + 5
yy = Bitmap(num).y + Win(Bitmap(num).Win).y + 21

filename$ = BitmapFile$(num)

IF Exist(filename$) = false THEN EXIT SUB
HideMouse
viewW Bitmap(num).Win




DIM BMP AS BMPHeader               'Define header

OPEN filename$ FOR BINARY AS BMPFile     'Open file
GET #BMPFile, , BMP                      'Load header quickly

'-- Load palette
'The next part of the file is the colormap.  Its length is:
Length = 4 * 2 ^ BMP.BitsPerPixel

'Each color is represented by four values: blue, green, red, and an
'unused value.  (Note that the OS/2 colormap doesn't have the
'wasted unused value)

'First we need to determine how many bytes are in each scan line.
Length = (LOF(1) - BMP.Offset) \ BMP.Rows
          '|                   ^ This trancutes any remainders
          '+- You could also use BMP.FileSize
'The above line is a quick way to determine the size of each row.  However,
'it may not work if there are excess bits at the end of each scan line.
'Files with 2 or 4 bits per pixel and odd widths sometimes have to be
'padded with extra bits at the end of each scan line.  This may fool this
'simple routine.

Foo$ = STRING$(Length, 32)         'Allocate space in memory for each row

SEEK #BMPFile, BMP.Offset + 1
FOR y& = BMP.Rows TO 1 STEP -1      'Rows are stored bottom to top in BMPs
   GET #BMPFile, , Foo$             'Load row into memory
   IF y& + yy > ViewY2 THEN GOTO SkipRow
   IF y& + yy < ViewY THEN GOTO SkipRow
   Ptr = 1                         'Pointer into memory
   FOR x& = 1 TO BMP.Cols STEP (8 \ BMP.BitsPerPixel)  'Loop through columns
                                   '^ Number of columns per byte
      IF x& + xx > ViewX2 THEN EXIT FOR
      Bar = ASC(MID$(Foo$, Ptr, 1))'Extract byte
      Ptr = Ptr + 1                'Increment pointer
      IF BMP.BitsPerPixel = 8 THEN     'If one byte per pixel...
         PSET (x& + xx - 1, y& + yy - 1), Bar'...simply output that byte
      ELSEIF BMP.BitsPerPixel = 4 THEN 'If one nibble (4 bits) per pixel...
         PSET (x& + xx - 1, y& + yy - 1), ReturnNibble(Bar, 1)'...Extract nibbles...
         PSET (x& + xx, y& + yy - 1), ReturnNibble(Bar, 2)  '...and output both
      ELSEIF BMP.BitsPerPixel = 1 THEN 'If only one bit per pixel...
         FOR i = 1 TO 8            '...loop through the 8 bits and...
            PSET (x& + xx - 1 + i, y& + yy - 1), GetBit(Bar, i)'...extract each...
         NEXT                      '...and output them
      END IF
   NEXT                            'Loop to next column
SkipRow:
NEXT                               'Done with that row, move to next
CLOSE BMPFile                      'Done!

ShowMouse
viewW -1

END SUB

SUB LoadIco (num)
'----------------------------------------------------------------------------
'              Draws an icon, called by the system
'----------------------------------------------------------------------------


xx = Icon(num).x + Win(Icon(num).Win).x + 5
yy = Icon(num).y + Win(Icon(num).Win).y + 21

IconFile = FREEFILE
filename$ = IconFile$(num)
IF Exist(filename$) = false THEN EXIT SUB

viewW Icon(num).Win
HideMouse
  

OPEN filename$ FOR BINARY AS #IconFile
SEEK #IconFile, 127
Foo$ = STRING$(512, 32)
GET #IconFile, , Foo$
CLOSE #IconFile
Ptr = 1

FOR y& = 32 TO 1 STEP -1
   FOR x& = 1 TO 32 STEP 2
   Bar = ASC(MID$(Foo$, Ptr, 1))
      Ptr = Ptr + 1
      'IF Bar = 0 THEN Bar = desktop
      PSET (x& + xx, y& + yy), ReturnNibble(Bar, 1)
      PSET (x& + xx + 1, y& + yy), ReturnNibble(Bar, 2)
   NEXT
NEXT


viewW -1
ShowMouse

END SUB

SUB main
printF 1, 50, TextText$(MaxTextFields), 0, 4
IF InputDialogPressedbutton > 0 THEN printF 1, 50, TextText$(MaxTextFields), 0, 4
IF Click = 1 THEN SOUND 6000, 1000 / 1000
IF InputDialogPressedbutton > 0 THEN NoteBook TextText$(MaxTextFields), 1: NoteBook TextText$(MaxTextFields), 2: END
IF key$ = CHR$(13) THEN clickedButton = 1: DialogPressedbutton = 1: InputDialogPressedbutton = 1
IF MenuActive = false THEN TIMER ON
IF MenuActive = true THEN TIMER STOP: AutoHide = 0
IF key$ = "help" THEN
  IF aWIn = 10 THEN NoteBook "c:\access\notebk.hlp", 1
END IF

IF Click > 0 OR key$ <> "" OR mousemove = 1 THEN start! = TIMER
temp! = TIMER
IF INT(TIMER) - INT(start!) = Idle THEN start! = TIMER: ScreenSaver saver
IF DialogActive = 1 THEN aWIn = MaxWindows
'IF DialogActive = 1 AND click = 1 THEN ReDrawWindow MaxWindows

IF MenuNumber = 1 AND menuselected = 1 AND aWIn = 9 THEN Help: menuselected = 0
IF MenuNumber = 1 AND menuselected = 2 AND aWIn = 9 THEN Control 1: menuselected = 0
IF MenuNumber = 1 AND menuselected = 3 AND aWIn = 9 THEN Command 1: menuselected = 0
IF MenuNumber = 1 AND menuselected = 4 AND aWIn = 9 THEN NoteBook "", 4: menuselected = 0
IF MenuNumber = 1 AND menuselected = 5 AND aWIn = 9 THEN About: menuselected = 0
IF MenuNumber = 1 AND menuselected = 6 AND aWIn = 9 THEN ScreenSaver saver: menuselected = 0
IF MenuNumber = 1 AND menuselected = 7 AND aWIn = 9 THEN Cabinet 0, "c:\access", "": menuselected = 0

IF MenuNumber = 1 AND menuselected = 1 AND aWIn <> 9 AND aWIn <> 8 AND DialogActive <> true OR DialogActive <> true AND Click = 1 AND hx > wx - 45 AND hy > wy AND hx < wx2 - 45 AND hy < wy2 AND aWIn < MaxWindows THEN menuselected = 0: Help
IF MenuNumber = 1 AND menuselected = 2 AND aWIn <> 9 AND aWIn <> 8 AND DialogActive <> true OR DialogActive <> true AND Click = 1 AND hx > wx - 30 AND hy > wy AND hx < wx2 - 30 AND hy < wy2 AND aWIn < MaxWindows THEN MinimizeWindow aWIn:      _
menuselected = 0
IF MenuNumber = 1 AND menuselected = 3 AND aWIn <> 9 AND aWIn <> 8 AND DialogActive <> true OR DialogActive <> true AND Click = 1 AND hx > wx - 15 AND hy > wy AND hx < wx2 - 15 AND hy < wy2 AND aWIn < MaxWindows THEN MaximizeWindow aWIn:      _
menuselected = 0
IF MenuNumber = 1 AND menuselected = 4 AND aWIn <> 9 AND aWIn <> 8 AND DialogActive <> true THEN RestoreWindow aWIn: StatusMSG$ = "": menuselected = 0: menuselected = 0
IF MenuNumber = 1 AND menuselected = 5 AND aWIn <> 9 AND aWIn <> 8 AND DialogActive <> true OR DialogActive <> true AND Click = 1 AND hx > wx AND hy > wy AND hx < wx2 AND hy < wy2 THEN killWindow aWIn: StatusMSG$ = "": menuselected = 0: aWIn = 2: _
     menuselected = 0

IF aWIn = 8 AND menuselected > 0 AND aWIn < 15 THEN aWIn = menuselected: RestoreWindow menuselected: : aWIn = menuselected: UpdateScreen



IF hv <> -1 AND aWIn = 0 THEN aWIn = 1
'IF click = 0 AND hv <> -1 THEN aWin = maxWindows
IF Click = 1 AND hx > 55 AND hy > 2 AND hx < 100 AND hy < 15 AND aWIn < MaxWindows THEN TaskMan 1
IF Click = 1 AND hx > 110 AND hy > 2 AND hx < 195 AND hy < 15 AND aWIn < MaxWindows THEN Control 1
IF Click = 1 AND hx > 205 AND hy > 2 AND hx < 250 AND hy < 15 AND aWIn < MaxWindows THEN About

IF ho = -1 AND hx >= 600 AND hy < 18 AND AccessPos = 2 AND was = 1 THEN
Box3D 480, 21, 638, 42, DoubleOut, 7
printF 484, 23, Now$, 4, 7
timea$ = TIME$
END IF


IF Click = 1 AND MenuSaved = true THEN
    i& = MenuNumber
      IF LEN(RTRIM$(MenuBar(aWIn, i&).caption)) <> 10 AND MenuBar(aWIn, i&).Items > 0 THEN
        IF NOT (hx > Win(aWIn).x + 3 + ((i& - 1) * 8 * 10) + 2 AND hx < Win(aWIn).x + 3 + ((i&) * 8 * 10) + 2 AND hy > Win(aWIn).y + 21 + 12 AND hy < Win(aWIn).y + 21 + 12 + (MenuBar(aWIn, i&).Items * 12)) THEN
          HideMouse
          Load640 "~temp", Win(aWIn).y + 17
          ShowMouse
          MenuSaved = false
          MenuActive = false
          menuselected = 0
          MenuHeld = 2
        ELSE
          HideMouse
          menuselected = MenuBar(aWIn, i&).Items - ((Win(aWIn).y + 21 + 12 + (MenuBar(aWIn, i&).Items * 12)) - hy) \ 12
          printF Win(aWIn).x + 4 + 4 + ((i& - 1) * 8 * 10) + 2, Win(aWIn).y + 20 + 12 + menuselected * 12 - 8, menu(aWIn, MenuNumber, menuselected), 15, 1
          Load640 "~temp", Win(aWIn).y + 17
          MenuSaved = false
          MenuActive = false
          MenuHeld = 0
          ShowMouse
          Click = 0
          hv = 0
        END IF
      END IF
  END IF


  IF hv = -1 AND MenuSaved = true THEN
    i& = MenuNumber
      IF LEN(RTRIM$(MenuBar(aWIn, i&).caption)) <> 10 AND MenuBar(aWIn, i&).Items > 0 THEN
        IF (hx > Win(aWIn).x + 3 + ((i& - 1) * 8 * 10) + 2 AND hx < Win(aWIn).x + 3 + ((i&) * 8 * 10) + 2 AND hy > Win(aWIn).y + 21 + 12 AND hy < Win(aWIn).y + 21 + 12 + (MenuBar(aWIn, i&).Items * 12)) THEN
          OldMenuHeld = MenuHeld
          MenuHeld = MenuBar(aWIn, i&).Items - ((Win(aWIn).y + 21 + 12 + (MenuBar(aWIn, i&).Items * 12)) - hy) \ 12
          IF OldMenuHeld <> MenuHeld AND OldMenuHeld <> 0 THEN
            HideMouse
            printF Win(aWIn).x + 4 + 4 + ((i& - 1) * 8 * 10) + 2, Win(aWIn).y + 20 + 12 + OldMenuHeld * 12 - 8, menu(aWIn, MenuNumber, OldMenuHeld), 0, Fill
            printF Win(aWIn).x + 4 + 4 + ((i& - 1) * 8 * 10) + 2, Win(aWIn).y + 20 + 12 + MenuHeld * 12 - 8, menu(aWIn, MenuNumber, MenuHeld), 15, 1
            ShowMouse
          END IF
        END IF
      END IF
  END IF


  IF Click = 1 AND MenuSaved = false THEN
  FOR i& = 1 TO 8
      IF LEN(RTRIM$(MenuBar(aWIn, i&).caption)) <> 10 AND MenuBar(aWIn, i&).Items > 0 THEN
        IF hx > Win(aWIn).x + 3 + ((i& - 1) * 8 * 10) + 2 AND hx < Win(aWIn).x + 3 + ((i&) * 8 * 10) + 2 AND hy > Win(aWIn).y + 21 AND hy < Win(aWIn).y + 21 + 12 THEN
          HideMouse
          Save640 "~temp", Win(aWIn).y + 17
          LINE (Win(aWIn).x + 3 + ((i& - 1) * 8 * 10) + 2, Win(aWIn).y + 17)-(Win(aWIn).x + 3 + ((i& - 1) * 8 * 10) + 2 + (LEN(RTRIM$(MenuBar(aWIn, i&).caption)) * FontSpace&), Win(aWIn).y + 21 + 10), 1, BF
          printF Win(aWIn).x + 3 + ((i& - 1) * 8 * 10) + 2, Win(aWIn).y + 21, RTRIM$(MenuBar(aWIn, i&).caption), 15, 1
          DrawMenuItems aWIn, INT(i&)
          ShowMouse
          MenuSaved = true
          MenuActive = true
          MenuNumber = i&
        END IF
      END IF
    NEXT
  END IF

  IF MenuActive = false THEN
  IF hv = 0 AND dragWin <> 0 THEN
    xorline Win(dragWin).x, Win(dragWin).y, Win(dragWin).X2, Win(dragWin).y2, 1, DragLine
    UpdateScreen
    dragWin = 0
  END IF

 
  IF hv = -1 AND dragWin = 0 AND drag = 0 AND Win(aWIn).drag = true THEN
    i = aWIn
    IF Win(i).X2 <> 0 AND Win(i).y2 <> 0 THEN
      IF hx >= Win(i).x + 5 AND hx <= Win(i).X2 - 5 AND hy >= Win(i).y + 5 AND hy <= Win(i).y + 16 THEN
        HideMouse
        DragXOffset = hx - Win(i).x
        DragYOffset = hy - Win(i).y
        DragXLen = Win(i).X2 - Win(i).x
        DragYLen = Win(i).y2 - Win(i).y
        dragWin = i
        LINE (Win(dragWin).x, Win(dragWin).y - 1)-(Win(dragWin).X2, Win(dragWin).y2), desktop, BF
        ShowMouse
        xorline Win(dragWin).x, Win(dragWin).y, Win(dragWin).X2, Win(dragWin).y2, 1, DragLine
      END IF
    END IF
  END IF

  IF dragWin <> 0 AND mousemove = 1 THEN
    xorline Win(dragWin).x, Win(dragWin).y, Win(dragWin).X2, Win(dragWin).y2, 1, DragLine
    Win(dragWin).x = hx - DragXOffset
    Win(dragWin).y = hy - DragYOffset
    Win(dragWin).X2 = hx - DragXOffset + DragXLen
    Win(dragWin).y2 = hy - DragYOffset + DragYLen
    xorline Win(dragWin).x, Win(dragWin).y, Win(dragWin).X2, Win(dragWin).y2, 1, DragLine
  END IF



  IF dragWin = 0 THEN

  IF hv = -1 THEN
    FOR i = 1 TO MaxScrollBars
      IF SBar(i).dir = 0 AND SBar(i).Win = aWIn THEN
        IF hx > SBar(i).x + 16 + Win(SBar(i).Win).x + 5 AND hx < SBar(i).X2 - 16 + Win(SBar(i).Win).x + 5 AND hy > SBar(i).y + Win(SBar(i).Win).y + 21 AND hy < SBar(i).y2 + Win(SBar(i).Win).y + 21 THEN
          SBar(i).Value = (hx - (SBar(i).x + 16 + Win(SBar(i).Win).x + 5)) / SBar(i).SStep - (5 / SBar(i).SStep)
          IF SBar(i).Value <= 0 THEN SBar(i).Value = 0
          IF SBar(i).Value >= SBar(i).Max THEN SBar(i).Value = SBar(i).Max
          MovingBar = 1
          MovedBar = i
          DrawScrollBar i
        END IF
      END IF
      IF SBar(i).dir = 1 AND SBar(i).Win = aWIn THEN
        IF hx > SBar(i).x + Win(SBar(i).Win).x + 5 AND hx < SBar(i).X2 + Win(SBar(i).Win).x + 5 AND hy > SBar(i).y + 16 + Win(SBar(i).Win).y + 21 AND hy < SBar(i).y2 - 16 + Win(SBar(i).Win).y + 21 THEN
          SBar(i).Value = (hy - (SBar(i).y + 16 + Win(SBar(i).Win).y + 21)) / SBar(i).SStep - (5 / SBar(i).SStep)
          IF SBar(i).Value <= 0 THEN SBar(i).Value = 0
          IF SBar(i).Value >= SBar(i).Max THEN SBar(i).Value = SBar(i).Max
          MovingBar = 1
          MovedBar = i
          DrawScrollBar i
        END IF
      END IF
    NEXT
  END IF


  IF hv = -1 THEN
    FOR i = 1 TO MaxTextFields
      IF TextF(i).Win = aWIn AND i <> AText THEN
        IF hx > TextF(i).x + Win(aWIn).x + 5 AND hx < TextF(i).x + (TextF(i).Max * FontSpace&) + 24 + Win(aWIn).x + 5 AND hy > TextF(i).y + Win(aWIn).y + 21 AND hy < TextF(i).y + 14 + Win(aWIn).y + 21 AND TextF(i).Disabled = false THEN
          HideMouse
          IF AText <> 0 THEN
            IF TextF(AText).Win = aWIn AND TextF(AText).Disabled = false THEN LINE (TextF(AText).x + Win(aWIn).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + Win(aWIn).y + 21)-(TextF(AText).x + Win(aWIn).x + 5 + 8 +  _
(LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + 8 + Win(aWIn).y + 21), 15, BF
          END IF
          AText = i
          DrawTextField i
          printF TextF(AText).x + Win(aWIn).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + Win(aWIn).y + 21, CursorChar, 0, 16
          ShowMouse
        END IF
      END IF
    NEXT
  END IF



  IF Click = 1 AND NOT drag = 1 THEN
    FOR i = 1 TO MaxCheckBoxes
      IF CheckBox(i).Win = aWIn AND CheckBox(i).Disabled = false THEN
        IF hx > CheckBox(i).x + Win(aWIn).x + 5 AND hx < CheckBox(i).x + (LEN(CheckBoxText$(i)) * FontSpace&) + 18 + Win(aWIn).x + 5 AND hy > CheckBox(i).y + Win(aWIn).y + 21 AND hy < CheckBox(i).y + 11 + Win(aWIn).y + 21 THEN
          Switch CheckBox(i).State, true, false
          DrawCheckBox i, false
          ClickedCheckBox = i
        END IF
      END IF
    NEXT
  END IF
       

  IF Click = 1 AND NOT drag = 1 THEN
    FOR i = 1 TO MaxRadioButtons
      IF Radio(i).Win = aWIn AND Radio(i).Disabled = false THEN
        IF hx > Radio(i).x + Win(aWIn).x + 5 AND hx < Radio(i).x + (LEN(RadioText$(i)) * FontSpace&) + 18 + Win(aWIn).x + 5 AND hy > Radio(i).y + Win(aWIn).y + 21 AND hy < Radio(i).y + 11 + Win(aWIn).y + 21 THEN
          Radio(i).State = true
          FOR i2 = 1 TO MaxRadioButtons
            IF Radio(i2).Group = Radio(i).Group AND i <> i2 AND Radio(i2).Win = aWIn THEN
              Radio(i2).State = false
              DrawRadio i2, false
            END IF
          NEXT

          DrawRadio i, false
          ClickedRadioButton = i
        END IF
      END IF
    NEXT
  END IF



  IF hv = -1 AND NOT drag = 1 AND DialogActive = false AND InputDialogActive = false THEN
    IF NOT (hx >= Win(aWIn).x AND hx <= Win(aWIn).X2 AND hy >= Win(aWIn).y AND hy <= Win(aWIn).y2) THEN
      FOR i = 1 TO MaxWindows
        IF hx >= Win(i).x AND hx <= Win(i).X2 AND hy >= Win(i).y AND hy <= Win(i).y2 THEN
          Win(aWIn).TopicCol = DeActiveTopic
          Win(aWIn).TopicBCol = DeActiveTopicB
          ReDrawWindow (aWIn)
          aWIn = i
          Win(aWIn).TopicCol = ActiveTopic
          Win(aWIn).TopicBCol = ActiveTopicB
          ReDrawWindow (aWIn)
        END IF
      NEXT
    END IF
  END IF



  IF hv = -1 AND HoldButton = 0 AND MovingBar = 0 THEN
    FOR i = 1 TO maxbuttons + MaxScrollBars
      IF But(i).Win = aWIn THEN
        IF hx >= But(i).x + Win(aWIn).x + 5 AND hx <= But(i).X2 + Win(aWIn).x + 5 AND hy >= But(i).y + Win(aWIn).y + 21 AND hy <= But(i).y2 + Win(aWIn).y + 21 AND NOT But(i).Disabled = true THEN
         DrawButton i, 1, 1
         HoldButton = i
        END IF
      END IF
    NEXT
  END IF


  IF HoldButton <> 0 AND hv = 0 AND MovingBar = 0 THEN
    IF hx >= But(HoldButton).x + Win(aWIn).x + 5 AND hx <= But(HoldButton).X2 + Win(aWIn).x + 5 AND hy >= But(HoldButton).y + Win(aWIn).y + 21 AND hy <= But(HoldButton).y2 + Win(aWIn).y + 21 THEN
      clickedButton = HoldButton
      HoldButton = 0
      DrawButton clickedButton, 0, 1
    ELSE
      DrawButton HoldButton, 0, 1
      HoldButton = 0
      clickedButton = 0
    END IF
  END IF

  IF HoldButton > maxbuttons THEN
    FOR i = 1 TO MaxScrollBars
      IF SBar(i).button = HoldButton THEN
        SBar(i).Value = SBar(i).Value - SBar(i).SStep
        IF SBar(i).Value <= 0 THEN SBar(i).Value = 0
        MovedBar = i
        DrawScrollBar i
      END IF
      IF SBar(i).button + 1 = HoldButton THEN
        SBar(i).Value = SBar(i).Value + SBar(i).SStep
        IF SBar(i).Value >= SBar(i).Max THEN SBar(i).Value = SBar(i).Max
        MovedBar = i
        DrawScrollBar i
      END IF
    NEXT
  END IF


  IF HoldButton <> 0 AND hv = -1 THEN
    IF NOT (hx >= But(HoldButton).x + Win(aWIn).x + 5 AND hx <= But(HoldButton).X2 + Win(aWIn).x + 5 AND hy >= But(HoldButton).y + Win(aWIn).y + 21 AND hy <= But(HoldButton).y2 + Win(aWIn).y + 21) THEN
      DrawButton HoldButton, 0, 1
      HoldButton = 0
    END IF
  END IF


  FOR i = 1 TO MaxPercentBars
    IF aWIn = PBar(i).Win THEN
      IF PBar(i).OldValue <> PBar(i).Value THEN PBar(i).OldValue = PBar(i).Value: DrawPercentBar i
    END IF
  NEXT

  IF Click = 1 THEN
    FOR i = 0 TO MaxIcons
      IF aWIn = Icon(i).Win THEN
        IF hx >= Icon(i).x + Win(aWIn).x + 5 AND hx <= Icon(i).x + Win(aWIn).x + 5 + 32 AND hy >= Icon(i).y + Win(aWIn).y + 21 AND hy <= Icon(i).y + Win(aWIn).y + 21 + 32 THEN
          ClickedIcon = i
        END IF
      END IF
    NEXT
  END IF


  IF AText <> 0 THEN
  IF TextF(AText).Disabled = false THEN
  IF key$ <> "" AND TextF(AText).Win = aWIn THEN

    IF key$ = CHR$(0) + CHR$(15) THEN
      TextSearch = AText
      DO
        TextSearch = TextSearch - 1
        IF TextSearch < 1 THEN TextSearch = MaxTextFields
        IF TextSearch = AText THEN EXIT DO
        IF TextF(TextSearch).Win = aWIn AND TextSearch <> AText AND TextF(TextSearch).Disabled = false AND TextF(TextSearch).x <> ScreenXDim + 1000 THEN
          printF TextF(AText).x + Win(aWIn).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + Win(aWIn).y + 21, "  ", 0, 15
          AText = TextSearch
          EXIT DO
        END IF
      LOOP
      key$ = ""
    END IF


    IF LEFT$(key$, 1) = CHR$(0) THEN key$ = ""
    IF key$ = CHR$(9) THEN
      TextSearch = AText
      DO
        TextSearch = TextSearch + 1
        IF TextSearch > MaxTextFields THEN TextSearch = 1
        IF TextSearch = AText THEN EXIT DO
        IF TextF(TextSearch).Win = aWIn AND TextSearch <> AText AND TextF(TextSearch).Disabled = false AND TextF(TextSearch).x <> ScreenXDim + 1000 THEN
          printF TextF(AText).x + Win(aWIn).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + Win(aWIn).y + 21, "  ", 0, 15
          AText = TextSearch
          EXIT DO
        END IF
      LOOP
      key$ = ""
    END IF

    IF key$ = CHR$(13) THEN key$ = ""

    IF key$ = CHR$(8) THEN
      IF LEN(TextText$(AText)) > 0 THEN
        TextText$(AText) = LEFT$(TextText$(AText), LEN(TextText$(AText)) - 1)
        key$ = ""
      ELSE
        key$ = ""
      END IF
    END IF
    IF LEN(TextText$(AText)) = TextF(AText).Max THEN key$ = ""

    TextText$(AText) = TextText$(AText) + key$
    TextText$(AText) = LEFT$(TextText$(AText), TextF(AText).Max)

    IF TextF(AText).Password = true AND key$ <> "" THEN key$ = "*"

    IF MouseInside(TextF(AText).x + Win(aWIn).x + 5, TextF(AText).y + Win(aWIn).y + 21, TextF(AText).x + Win(aWIn).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + Win(aWIn).y + 21 + 14) = true THEN HideMouse
    printF TextF(AText).x + Win(aWIn).x + 5 + (LEN(TextText$(AText)) * FontSpace&), TextF(AText).y + 4 + Win(aWIn).y + 21, key$, 0, 15
    printF TextF(AText).x + Win(aWIn).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + Win(aWIn).y + 21, Cursor, 0, 15


    IF MouseInside(TextF(AText).x + Win(aWIn).x + 5, TextF(AText).y + Win(aWIn).y + 21, TextF(AText).x + Win(aWIn).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + Win(aWIn).y + 21 + 14) = true THEN ShowMouse
  END IF
    IF TIMER > TextTimer! + .1 THEN
      IF Cursor = "  " THEN Cursor = CursorChar + " ": GOTO DrawCursor
      IF Cursor = CursorChar + " " THEN Cursor = "  "

DrawCursor:
      IF TextF(AText).Win = aWIn AND TextF(AText).Disabled = false THEN
        IF MouseInside(TextF(AText).x + Win(aWIn).x + 5, TextF(AText).y + Win(aWIn).y + 21, TextF(AText).x + Win(aWIn).x + 5 + ((TextF(AText).Max) * FontSpace&) + 24 + FontSpace&, TextF(AText).y + Win(aWIn).y + 21 + 14) = true THEN HideMouse
        printF TextF(AText).x + Win(aWIn).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + Win(aWIn).y + 21, Cursor, 0, 15
        IF MouseInside(TextF(AText).x + Win(aWIn).x + 5, TextF(AText).y + Win(aWIn).y + 21, TextF(AText).x + Win(aWIn).x + 5 + ((TextF(AText).Max) * FontSpace&) + 24 + FontSpace&, TextF(AText).y + Win(aWIn).y + 21 + 14) = true THEN ShowMouse
      END IF
      TextTimer! = TIMER
    END IF
  END IF

  END IF
  END IF
  END IF


  IF PlayingWAV& AND DMADone% = -1 THEN
    WavOffset& = WavOffset& + 8000
    IF WavLength& - WavOffset& < 8000 THEN PlayLength& = WavLength& - WavOffset& ELSE PlayLength& = 8000
    IF PlayLength& <= 0 THEN
      CLOSE #WavFilehandle&
      PlayingWAV& = false
      WavLength& = 0
      WavOffset& = 0
      WavFreq& = 0
    ELSE
      GET #WavFilehandle&, , WavBuffer(0)
      DMAPlay VARSEG(WavBuffer(0)), VARPTR(WavBuffer(0)), PlayLength& - 1, WavFreq&
    END IF
  END IF

  OldHiiriX = hx
  OldHiiriY = hy
  IF Virhe <> 0 THEN GPFRuutu

  IF DialogActive = true AND clickedButton <> 0 THEN
    aWIn = DialogAWin
    IF clickedButton = maxbuttons THEN DialogClickedButton = 1: killWindow MaxWindows
    IF clickedButton = maxbuttons - 1 THEN DialogClickedButton = 2: killWindow MaxWindows
    IF clickedButton = maxbuttons - 2 THEN DialogClickedButton = 3: killWindow MaxWindows
    clickedButton = 0
    DialogActive = false
  END IF

  IF InputDialogActive = true AND clickedButton <> 0 THEN
    aWIn = InputDialogAWin
    killTextField MaxTextFields
    IF clickedButton = maxbuttons THEN InputDialogClickedButton = 1: killWindow MaxWindows
    IF clickedButton = maxbuttons - 1 THEN InputDialogClickedButton = 2: killWindow MaxWindows
    IF clickedButton = maxbuttons - 2 THEN InputDialogClickedButton = 3: killWindow MaxWindows
    clickedButton = 0
    InputDialogActive = false
  END IF
  IF Copying = true THEN DoCopy
IF MenuActive = true THEN TIMER STOP: AutoHide = 0
END SUB

SUB MasterVolume (right%, left%, Getvol%)
'----------------------------------------------------------------------------
' Sets the master volume on the Sound Blaster card
'----------------------------------------------------------------------------
'
'  Right% - Right volume
'   Left% - Left volume
' Getvol% - If true, then volume will be read, not set
'
'----------------------------------------------------------------------------

OUT BasePort% + 4, &H22
'PRINT BasePort%
IF Getvol% THEN
   left% = INP(BasePort% + 5) \ 16
   right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (right% + left% * 16) AND &HFF
END IF
END SUB

SUB MaximizeWindow (num)
IF num = 9 THEN EXIT SUB
IF num = 14 THEN Cabinet 9, "", ""
RestoreW(num).x = Win(num).x
RestoreW(num).y = Win(num).y
RestoreW(num).X2 = Win(num).X2
RestoreW(num).y2 = Win(num).y2
Win(num).x = 1
Win(num).X2 = 640
Win(num).y = 1
Win(num).y2 = 480
IF AccessHide = 1 THEN Win(num).y = 1: Win(num).y2 = 480

aWIn = num
ReDrawWindow num
UpdateScreen
END SUB

SUB MicVolume (Volume%, Getvol%)
'----------------------------------------------------------------------------
' Sets the mic gain on the Sound Blaster card
'----------------------------------------------------------------------------
'
' Volume% - Volume
' Getvol% - If true, then volume will be read, not set
'
'----------------------------------------------------------------------------

OUT BasePort% + 4, &HA
IF Getvol% THEN
   Volume% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, Volume% AND &HF
END IF
END SUB

SUB MinimizeWindow (num)
IF num = 9 OR num = 8 OR num = MaxWindows THEN EXIT SUB
temp = num
x = Win(num).x
y = Win(num).y
X2 = Win(num).X2
y2 = Win(num).y2
RestoreW(num).x = x
RestoreW(num).y = y
RestoreW(num).X2 = X2
RestoreW(num).y2 = y2
HideMouse
LINE (x, y)-(X2, y2), desktop, BF
FOR Temp1 = y2 TO y + 20 STEP -20
CreateWindow 13, x, y, X2, Temp1, " ", 8, false
LINE (x, y)-(X2, y2), desktop, BF
NEXT Temp1
FOR Temp1 = X2 TO x + 40 STEP -20
CreateWindow 13, x, y, Temp1, y + 20, " ", 8, false
LINE (x, y)-(X2, y2), desktop, BF
NEXT Temp1
killWindow 13
num = temp
LINE (x - 40, y)-(X2, y2), desktop, BF

ShowMouse
Win(num).y = Win(num).y + 5000
Win(num).y2 = Win(num).y2 + 5000
aWIn = num
DrawWindow num
UpdateScreen
END SUB

FUNCTION MouseInside (x, y, X2, y2)
'----------------------------------------------------------------------------
' Checks is mouse is inside an area (the mouse cursor (15x15),
'                                    not just the mouse pointer)
'----------------------------------------------------------------------------
'
' x,y,x2,y2 - Area
'
'----------------------------------------------------------------------------




IF hx >= x - 15 AND hx <= X2 AND hy >= y - 15 AND hy <= y2 THEN
  MouseInside = true
ELSE
  MouseInside = false
END IF


END FUNCTION

SUB NoteBook (file$, opt)
SELECT CASE opt
CASE 4
Cabinet Size, dir$, "TXT"
opt = 1
  CASE 1
LineN = 0
StatusMSG$ = "Starting text editor"
HideMouse
Box3D 220, 228, 420, 248, DoubleOut, 0 'text box
Box3D 210, 218, 245, 254, DoubleOut, 0 'Icon box
CreateIcon 0, 1, 205, 198, "c:\access\icons\notebk.ico"
printF 247, 235, "Welcome to Access NoteBook!", 4, 0
ShowMouse
SLEEP 1
CreateIcon 0, 1, 10, 10, "c:\access\icons\mycomp.ico"
CreateWindow 10, 50, 50, 600, 400, "Note Book", 15, true
CreateMenu 10, 2, "File"
AddMenuItem 10, 2, "/uN/uew"
AddMenuItem 10, 2, "/uO/upen"
AddMenuItem 10, 2, "/uS/uave"
AddMenuItem 10, 2, "Save /uA/us"
AddMenuItem 10, 2, "E/ux/uit"
IF file$ = "" THEN CreateTextField 10, 3, 1, 20, 20, "File to open", false
IF file$ > "" THEN opt = 2
CASE 2
killTextField 3
ReDrawWindow 10
StatusMSG$ = "Viewing: " + file$
IF file$ = "" THEN file$ = "c:\access\access.hlp": StatusMSG$ = "Help"
OPEN file$ FOR INPUT AS #1
load:
DO
LineN = LineN + 1
LINE INPUT #1, Text$(LineN)
CreateLabel 10, 1, 1, LineN * 10 + 6, 550, 10, Text$(LineN), 0, 16, 0, left, true
LOOP UNTIL (EOF(1))
CLOSE #1
MaxLineN = LineN
x = Win(10).x
y = Win(10).y
X2 = Win(10).X2
y2 = Win(10).y2
SBar(10).Value = 1
StartLineN = 1
StatusMSG$ = ""
CASE 3
ReDrawWindow 10
END SELECT
END SUB

FUNCTION Now$
'----------------------------------------------------------------------------
'                          Returns current date and time
'----------------------------------------------------------------------------
'
' Example output:
'
'                      July 3, 1997 - 18:30:23
'
'----------------------------------------------------------------------------


Month = VAL(LEFT$(DATE$, 2))
Day = VAL(MID$(DATE$, 4, 2))
Year = VAL(RIGHT$(DATE$, 4))

SELECT CASE Month
CASE IS = 1
  Month$ = "January"
CASE IS = 2
  Month$ = "Febryary"
CASE IS = 3
  Month$ = "March"
CASE IS = 4
  Month$ = "April"
CASE IS = 5
  Month$ = "May"
CASE IS = 6
  Month$ = "June"
CASE IS = 7
  Month$ = "July"
CASE IS = 8
  Month$ = "August"
CASE IS = 9
  Month$ = "September"
CASE IS = 10
  Month$ = "October"
CASE IS = 11
  Month$ = "November"
CASE IS = 12
  Month$ = "December"
END SELECT
ampm$ = "AM"
Hour = VAL(LEFT$(TIME$, 2))
Minute$ = (MID$(TIME$, 4, 2))
Hour$ = (MID$(TIME$, 1, 2))
IF LEFT$(Hour$, 1) = "0" THEN Hour$ = (MID$(TIME$, 2, 1))
SELECT CASE Hour
CASE IS = 13
  Hour$ = "1"
  ampm$ = "PM"
CASE IS = 14
  Hour$ = "2"
  ampm$ = "PM"
CASE IS = 15
  Hour$ = "3"
  ampm$ = "PM"
CASE IS = 16
  Hour$ = "4"
  ampm$ = "PM"
CASE IS = 17
  Hour$ = "5"
  ampm$ = "PM"
CASE IS = 18
  Hour$ = "6"
  ampm$ = "PM"
CASE IS = 19
  Hour$ = "7"
  ampm$ = "PM"
CASE IS = 20
  Hour$ = "8"
  ampm$ = "PM"
CASE IS = 21
  Hour$ = "9"
  ampm$ = "PM"
CASE IS = 22
  Hour$ = "10"
  ampm$ = "PM"
CASE IS = 23
  Hour$ = "11"
  ampm$ = "PM"
CASE IS = 0
  Hour$ = "12"
  ampm$ = "AM"
END SELECT
timea$ = Hour$ + ":" + Minute$
Now$ = Month$ + " " + Trim$(Day) + "," + Trim$(Year) + " - " + timea$ + ampm$
END FUNCTION

SUB pause (opt)
SELECT CASE opt
CASE 1 TO 60
start1! = TIMER
DO
Now1! = TIMER
LOOP UNTIL Now1! - start1! = opt
CASE ELSE
DO
LOOP UNTIL INKEY$ > ""
END SELECT
END SUB

SUB printF (x&, y&, Textz$, FCol%, BCol%)
'----------------------------------------------------------------------------
' Prints text
'----------------------------------------------------------------------------
'
' x&, y& - Text coords.
' Textz$ - The text to print
'  FCol% - Front color (0: default & fastest)
'  BCol% - Front color (16: Transparent)
'
'----------------------------------------------------------------------------
'                          Text control codes
'----------------------------------------------------------------------------
'
' /n     - Linefeed
'            Example:
'
'               "Line 1/nLine 2"
'
'            will print:
'
'              "Line 1
'               Line 2"
'
'----------------------------------------------------------------------------
'
' /c##   - Changes front color to ##, must always be a two character number
'            Example:
'
'               "/c00I have a /c04red/c00apple."
'
'            will print:
'
'               "I have a red apple."
'                ^        ^   ^
'            black      red   black
'
'----------------------------------------------------------------------------
'
' /b     - Toggles bold on/off
'            Example:
'
'               "/bBold/b example text"
'
'            will print:
'
'               "Bold example text"
'                ^    ^
'             bold    normal
'
'----------------------------------------------------------------------------
'
' /u     - Toggles underline on/off
'            Example:
'
'               "/uUnderlined/u example text"
'
'            will print:
'
'               "Underlined example text"
'                ^          ^
'       underlined     normal
'
'----------------------------------------------------------------------------
'
' /r     - Toggles "carving" on/off
'            Example:
'
'               "/rCarved/r example text"
'
'            will print:
'
'               "Underlined example text"
'                ^          ^
'           carved     normal
'
'----------------------------------------------------------------------------


Bold = 0
Underline = 0
startX& = x&
Carved = 0

VIEW SCREEN (0, 0)-(ScreenXDim - 1, ScreenYDim - 1)

starti& = 1
i& = 0
IF MID$(UCASE$(Textz$), i& + 1, 2) = "/N" THEN
  x& = startX&
  y& = y& + 8
  starti& = starti& + 2
END IF

IF MID$(UCASE$(Textz$), i& + 1, 2) = "/C" THEN
  FCol% = VAL(MID$(Textz$, i& + 3, 2))
  starti& = starti& + 4
END IF

IF MID$(UCASE$(Textz$), i& + 1, 2) = "/B" THEN
  Switch Bold, 1, 0
  starti& = starti& + 2
END IF

IF MID$(UCASE$(Textz$), i& + 1, 2) = "/R" THEN
  Switch Carved, 1, 0
  starti& = starti& + 2
END IF


IF MID$(UCASE$(Textz$), i& + 1, 2) = "/U" THEN
  Switch Underline, 1, 0
  starti& = starti& + 2
END IF

FOR i& = starti& TO LEN(Textz$)
 
  IF MID$(UCASE$(Textz$), i&, 2) = "/N" THEN
    x& = startX&
    y& = y& + 8
    i& = i& + 2
  END IF

  IF MID$(UCASE$(Textz$), i&, 2) = "/C" THEN
    FCol% = VAL(MID$(Textz$, i& + 2, 2))
    i& = i& + 4
  END IF

  IF MID$(UCASE$(Textz$), i&, 2) = "/B" THEN
    Switch Bold, 1, 0
    i& = i& + 2
  END IF

  IF MID$(UCASE$(Textz$), i&, 2) = "/U" THEN
    Switch Underline, 1, 0
    i& = i& + 2
  END IF

  IF MID$(UCASE$(Textz$), i&, 2) = "/R" THEN
    Switch Carved, 1, 0
    i& = i& + 2
  END IF


  IF BCol% <> 16 THEN
    LINE (x&, y&)-(x& + FontX&, y& + FontY& - 1), BCol%, BF
  END IF

  IF MID$(Textz$, i&, 1) = CHR$(179) THEN
    LINE (x&, y&)-(x&, y& + FontY& - 1), FCol%
    GOTO DontDraw
  END IF

  IF i& > LEN(Textz$) THEN EXIT SUB
  IF MID$(Textz$, i&, 1) = " " THEN GOTO DontDraw

  IF MID$(Textz$, i&, 1) = "†" THEN MID$(Textz$, i&, 1) = CHR$(131)
  IF MID$(Textz$, i&, 1) = CHR$(140) THEN MID$(Textz$, i&, 1) = CHR$(134)
  IF MID$(Textz$, i&, 1) = "„" THEN MID$(Textz$, i&, 1) = CHR$(132)
  IF MID$(Textz$, i&, 1) = "”" THEN MID$(Textz$, i&, 1) = CHR$(133)
  IF MID$(Textz$, i&, 1) = "" THEN MID$(Textz$, i&, 1) = CHR$(128)
  IF MID$(Textz$, i&, 1) = "Ž" THEN MID$(Textz$, i&, 1) = CHR$(129)
  IF MID$(Textz$, i&, 1) = "™" THEN MID$(Textz$, i&, 1) = CHR$(130)
  IF ASC(MID$(Textz$, i&, 1)) > 138 THEN MID$(Textz$, i&, 1) = CHR$(138)


  IF (x& < ScreenXDim - 8 AND y& < ScreenYDim - FontX& AND x& >= 0 AND y& >= 0) AND (x& < ViewX2 - FontX& AND y& < ViewY2 - FontY& AND x& >= ViewX AND y& >= ViewY) AND ASC(MID$(Textz$, i&, 1)) > 32 AND Carved = 0 THEN
    IF FCol% <> 0 AND BCol% = 0 THEN
      PUT (x&, y&), Font((ASC(MID$(Textz$, i&, 1)) - 32) * 48), PSET
      IF Bold = 1 THEN PUT (x& + 1, y&), Font((ASC(MID$(Textz$, i&, 1)) - 32) * 48), PSET
    ELSE
      PUT (x&, y&), Font((ASC(MID$(Textz$, i&, 1)) - 32) * 48), AND
      IF Bold = 1 THEN PUT (x& + 1, y&), Font((ASC(MID$(Textz$, i&, 1)) - 32) * 48), AND
    END IF
  END IF

 
  IF (x& < ScreenXDim - 8 AND y& < ScreenYDim - FontX& AND x& >= 1 AND y& >= 0) AND (x& < ViewX2 - FontX& AND y& < ViewY2 - FontY& AND x& >= ViewX AND y& >= ViewY) AND ASC(MID$(Textz$, i&, 1)) > 32 AND Carved = 1 THEN
    PUT (x&, y&), Font((ASC(MID$(Textz$, i&, 1)) - 32) * 48), AND
    FCol% = 15

    FOR yy& = y& TO y& + FontY& - 1
      FOR xx& = x& TO x& + FontX& - 1
        IF POINT(xx&, yy&) = 0 THEN PSET (xx&, yy&), FCol% 'ELSE PSET (xx&, yy&), 0
      NEXT
    NEXT

    PUT (x& - 1, y&), Font((ASC(MID$(Textz$, i&, 1)) - 32) * 48), AND
    FCol% = 8

    FOR yy& = y& TO y& + FontY& - 1
      FOR xx& = x& - 1 TO x& + FontX& - 2
        IF POINT(xx&, yy&) = 0 THEN PSET (xx&, yy&), FCol% ' ELSE PSET (xx&, yy&), 0
      NEXT
    NEXT
  ELSE
    IF FCol% <> 0 AND BCol% = 0 THEN
      FOR yy& = y& TO y& + FontY& - 1
        FOR xx& = x& TO x& + FontX& - 1
          IF POINT(xx&, yy&) = 0 THEN PSET (xx&, yy&), FCol% ELSE PSET (xx&, yy&), 0
        NEXT
      NEXT
      GOTO DontDraw
    END IF
 
 
    IF FCol% <> 0 THEN
      FOR yy& = y& TO y& + FontY& - 1
        FOR xx& = x& TO x& + FontX& - 1
          IF POINT(xx&, yy&) = 0 THEN PSET (xx&, yy&), FCol%
        NEXT
      NEXT
    END IF
  END IF

  IF Underline = 1 THEN
    LINE (x&, y& + FontY&)-(x& + FontX&, y& + FontY&), FCol%
  END IF
DontDraw:


  x& = x& + FontSpace&

NEXT
  

END SUB

SUB Program1
' $INCLUDE: 'c:\access\program.bas'
END SUB

FUNCTION ReadDAC%
' Reads a byte from the DAC.
WriteDSP &H20
ReadDAC% = ReadDSP%

END FUNCTION

FUNCTION ReadDSP%
' Reads a byte from the DSP
DO
LOOP UNTIL INP(BasePort% + 14) AND &H80
ReadDSP% = INP(BasePort% + 10)
END FUNCTION

SUB ReDrawWindow (num)
'----------------------------------------------------------------------------
'               Draws a window, called by the system
'----------------------------------------------------------------------------

DrawWindow (num)
FOR i = 1 TO MaxBMPs
  IF Bitmap(i).Win = num THEN
    LoadBMP i
  END IF
NEXT



FOR i = 1 TO MaxFrames
  IF Frame(i).Win = num THEN
    DrawFrame i
  END IF
NEXT

FOR i = 1 TO MaxPercentBars
  IF PBar(i).Win = num THEN
    DrawPercentBar i
  END IF
NEXT


FOR i = 1 TO MaxLabels
  IF Label(i).Win = num THEN
    DrawLabel i
  END IF
NEXT

FOR i = 1 TO MaxScrollBars
  IF SBar(i).Win = num THEN
    DrawScrollBar i
  END IF
NEXT

FOR i = 1 TO MaxTextFields
  IF TextF(i).Win = num THEN
    DrawTextField i
  END IF
NEXT

FOR i = 1 TO MaxCheckBoxes
  IF CheckBox(i).Win = num THEN
    DrawCheckBox i, true
  END IF
NEXT

FOR i = 1 TO MaxRadioButtons
  IF Radio(i).Win = num THEN
    DrawRadio i, true
  END IF
NEXT

FOR i = 1 TO maxbuttons + MaxScrollBars
  IF But(i).Win = num THEN
    DrawButton i, 0, 0
  END IF
NEXT

FOR i = 1 TO MaxTextBoxes
  IF TextB(i).Win = num THEN
    DrawTextBox i, false
  END IF
NEXT

FOR i = 0 TO MaxIcons
  IF Icon(i).Win = num THEN
    LoadIco i
  END IF
NEXT
IF aWIn = 10 AND MaxLineN > 0 AND StatusMSG$ <> "Screen Saver" THEN
Temp1 = 0
FOR temp = StartLineN TO MaxLineN
Temp1 = Temp1 + 1
CreateLabel 10, 1, 1, Temp1 * 10 + 6, 550, 10, Text$(temp), 0, 16, 0, left, true
NEXT temp
StatusMSG$ = "Help"
END IF
                                
END SUB

FUNCTION ResetDSP%
' Resets the DSP
OUT BasePort% + 6, 1
FOR count% = 1 TO 4
   junk% = INP(BasePort% + 6)
NEXT
OUT BasePort% + 6, 0
IF INP(BasePort% + 14) AND &H80 = &H80 AND INP(BasePort% + 10) = &HAA THEN
   ResetDSP% = -1
ELSE
   ResetDSP% = 0
END IF
END FUNCTION

SUB ResetSB
'----------------------------------------------------------------------------
'                 Resets the Sound Blaster
'----------------------------------------------------------------------------

OUT &H226, 1: OUT &H226, 0
DO
  x% = INP(&H22E)
  IF x% AND 128 THEN
    x% = INP(&H22A)
    IF x% = &HAA THEN

    EXIT DO
  END IF
END IF
LOOP


DO
x% = INP(&H22C)
LOOP WHILE x% AND 128


OUT &H22, &HD1




END SUB

SUB RestoreWindow (num)
IF num = 9 THEN EXIT SUB
IF num = 8 THEN
  FOR i = 1 TO MaxWindows - 1
   IF i = 8 THEN i = i + 2
   IF Win(i).X2 <> 0 AND Win(i).x <> 0 THEN MinimizeWindow i
  NEXT i
aWIn = 0
EXIT SUB
END IF

IF Win(num).x = RestoreW(num).x AND Win(num).y2 = RestoreW(num).y2 THEN aWIn = num: ReDrawWindow num: UpdateScreen: EXIT SUB
x = Win(num).x
y = Win(num).y
X2 = Win(num).X2
y2 = Win(num).y2

HideMouse
LINE (x, y)-(X2, y2), desktop, BF
ShowMouse

Win(num).x = RestoreW(num).x
Win(num).y = RestoreW(num).y
Win(num).X2 = RestoreW(num).X2
Win(num).y2 = RestoreW(num).y2
aWIn = num
DrawWindow num
UpdateScreen
END SUB

FUNCTION ReturnNibble (Byte, WhichOne)
'----------------------------------------------------------------------------
' Returns the low (Whichone = 1) and high (Whichone = 2) values of an integer
'----------------------------------------------------------------------------
   IF WhichOne = 1 THEN
      IF Byte AND 128 THEN Total = Total + 8
      IF Byte AND 64 THEN Total = Total + 4
      IF Byte AND 32 THEN Total = Total + 2
      IF Byte AND 16 THEN Total = Total + 1
   ELSE
      IF Byte AND 8 THEN Total = Total + 8
      IF Byte AND 4 THEN Total = Total + 4
      IF Byte AND 2 THEN Total = Total + 2
      IF Byte AND 1 THEN Total = Total + 1
   END IF
   ReturnNibble = Total
END FUNCTION

SUB Save640 (file$, StartLine!)
'----------------------------------------------------------------------------
'                     Saves the entire screen
'----------------------------------------------------------------------------
'
'      File$ - Filename to save the screen to (NO EXTENSION!)
' StartLine! - Start line
'
'----------------------------------------------------------------------------

StartLine! = (ScreenXDim * StartLine!) \ 8

DEF SEG = &HA000

IF ScreenMode = 13 THEN
  BSAVE file$ + ".INT", 0, 65535
ELSE
  OUT &H3CE, 4: OUT &H3CF, 0: BSAVE file$ + ".BLU", StartLine!, 38400 - StartLine!
  OUT &H3CE, 4: OUT &H3CF, 1: BSAVE file$ + ".GRN", StartLine!, 38400 - StartLine!
  OUT &H3CE, 4: OUT &H3CF, 2: BSAVE file$ + ".RED", StartLine!, 38400 - StartLine!
  OUT &H3CE, 4: OUT &H3CF, 3: BSAVE file$ + ".INT", StartLine!, 38400 - StartLine!
  OUT &H3CE, 4: OUT &H3CF, 0
END IF
DEF SEG

END SUB

SUB ScreenSaver (opt)
TIMER OFF
HideMouse
SELECT CASE opt
CASE 1
StatusMSG$ = "Screen Saver"
DO
Temp1& = (INT((100 - 1 + 1) * RND + 1))
temp2& = (INT((100 - 1 + 1) * RND + 1))
temp3& = (INT((100 - 1 + 1) * RND + 1))
temp& = desktop
FOR temp4& = temp2& TO 100
SetPal 15, Temp1&, temp2&, temp3&
SetPal 7, Temp1&, temp2&, temp3&
UpdateScreen
FOR temp = 1 TO 50
IF INKEY$ > "" THEN GOTO Skip4
updatemouse
IF mousemove = 1 OR Click = 1 THEN EXIT DO
NEXT temp
NEXT temp4&


FOR Temp1& = Temp1& TO 100
SetPal temp&, Temp1&, temp2&, temp3&
UpdateScreen
FOR temp = 1 TO 50
IF INKEY$ > "" THEN GOTO Skip4
updatemouse
IF mousemove = 1 OR Click = 1 THEN EXIT DO
NEXT temp
NEXT Temp1&

FOR temp2& = temp2& TO 100
SetPal 7, Temp1&, temp2&, temp3&
UpdateScreen
FOR temp = 1 TO 50
IF INKEY$ > "" THEN GOTO Skip4
updatemouse
IF mousemove = 1 OR Click = 1 THEN EXIT DO
NEXT temp
NEXT temp2&

FOR temp3& = temp3& TO 100
SetPal 8, Temp1&, temp2&, temp3&
UpdateScreen
FOR temp = 1 TO 50
IF INKEY$ > "" THEN GOTO Skip4
updatemouse
IF mousemove = 1 OR Click = 1 THEN EXIT DO
NEXT temp
NEXT temp3&

FOR temp3& = temp3& TO 100
SetPal 0, Temp1&, temp2&, temp3&
UpdateScreen
FOR temp = 1 TO 50
IF INKEY$ > "" THEN GOTO Skip4
updatemouse
IF mousemove = 1 OR Click = 1 THEN EXIT DO
NEXT temp
NEXT temp3&
LOOP UNTIL INKEY$ > ""
Skip4:
StatusMSG$ = ""
TIMER ON
killWindow MaxWindows
PALETTE
UpdateScreen
ShowMouse
EXIT SUB

CASE 2
StatusMSG$ = "Screen Saver"
Save640 "screen", 1
SCREEN 13
'temp3 = (INT((15 - 1 + 1) * RND + 1))
DO
CLS
updatemouse
IF mousemove = 1 OR Click = 1 THEN EXIT DO
Temp1 = (INT((25 - 1 + 1) * RND + 1))
temp2 = (INT((25 - 1 + 1) * RND + 1))
temp = temp + 1
IF temp = 256 THEN temp = 1
COLOR temp
LOCATE Temp1, temp2: PRINT "Access"
SLEEP 1
COLOR 0
LOCATE Temp1, temp2: PRINT "Access"
LOOP UNTIL INKEY$ > ""

Defscreen
Load640 "screen", 1
TIMER ON
start! = TIMER
StatusMSG$ = ""
UpdateScreen
ShowMouse

CASE 3
StatusMSG$ = "Screen Saver"
Save640 "screen", 1
SCREEN 0
FOR x = 1 TO 3000
COLOR 4, 0
CLS
LOCATE 11
PRINT TAB(24); "ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ"
PRINT TAB(24); "ÛÛÛÛÛÛÛÛÛÛÛPROGRAM BYÛÛÛÛÛÛÛÛÛÛÛ"
PRINT TAB(24); "ÛÛÛÛÛÛÛÛÛÛÛ   MiKE   ÛÛÛÛÛÛÛÛÛÛÛ"
PRINT TAB(24); "ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ"
PRINT : PRINT : PRINT
COLOR 7, 0
NEXT x



DIM num(80)


COLOR 2
CLS

FOR x = 1 TO 2
      
        FOR t = 1 TO 80
                num(t) = INT(RND * 9)
        NEXT t
        x = 1
      
        FOR s = 1 TO 80
                PRINT num(s);
IF INKEY$ > "" THEN GOTO finish
updatemouse
IF mousemove = 1 OR Click = 1 THEN GOTO finish
        NEXT s
FOR temp = 1 TO 25
        FOR c = 1 TO 20000
        NEXT c
NEXT temp

        IF INKEY$ <> "" THEN GOTO finish
NEXT x
finish:
Defscreen
Load640 "screen", 1
TIMER ON
start! = TIMER
StatusMSG$ = ""
UpdateScreen
ShowMouse
EXIT SUB

CASE 4
StatusMSG$ = "Screen Saver"
Save640 "screen", 1
CLS
DO
IF INKEY$ > "" THEN GOTO finish
updatemouse
IF mousemove = 1 OR Click = 1 THEN GOTO finish
IF INKEY$ <> "" THEN GOTO finish
LOOP
END SELECT
END SUB

SUB SetPal (i&, R&, g&, b&)
'----------------------------------------------------------------------------
'                        Sets a palette RGB-value
'----------------------------------------------------------------------------
'
'  i& - Color number
'  r& - Red color component
'  g& - Green color component
'  b& - Blue color component
'
'----------------------------------------------------------------------------

OUT &H3C8, i&
OUT &H3C9, R&
OUT &H3C9, g&
OUT &H3C9, b&
END SUB

SUB SetStereo (OnOff%)
'----------------------------------------------------------------------------
'                     Sets stereo on/off
'----------------------------------------------------------------------------

OUT BasePort% + 4, &HE
IF OnOff% THEN OUT BasePort% + 5, 2 ELSE OUT BasePort% + 5, 0
END SUB

SUB ShowMouse
'----------------------------------------------------------------------------
'                      Shows the mouse cursor
'----------------------------------------------------------------------------
 
  ax% = 1                   'Funktio 1
  Hiiriajuri ax%, 0, 0, 0   'Kutsutaan hiiriajuria
END SUB

SUB ShutDown (opt)
SELECT CASE opt
CASE 1
CLS
viewW -1
PAINT (0, 0), desktop
Box3D 220, 218, 405, 245, DoubleOut, 0
LOCATE 15, 29: PRINT "Shuting down Access..."
PLAY "mf O2 l24 bagfedcbagfedcbagfedc l10 n25" 'ShutDown
KILL "c:\access\temp\*.*"
Box3D 220, 218, 405, 245, DoubleOut, 0
LOCATE 15, 29: PRINT "       Done!          "
fadeout
CLS
PALETTE
SYSTEM
CASE 2
CLS
viewW -1
PAINT (0, 0), desktop
Box3D 220, 218, 405, 245, DoubleOut, 0
LOCATE 15, 29: PRINT "    Restarting PC...  "
PLAY "mf O2 l24 bagfedcbagfedcbagfedc l10 n25" 'ShutDown
KILL "c:\access\temp\*.*"
Box3D 220, 218, 405, 245, DoubleOut, 0
LOCATE 15, 29: PRINT "       Done!          "
fadeout
CLS
PALETTE
SYSTEM
OUT &H64, &HFE
CASE 3
CLS
viewW -1
PAINT (0, 0), desktop
Box3D 220, 218, 405, 245, DoubleOut, 0
LOCATE 15, 29: PRINT "  Restarting Access   "
PLAY "mf O2 l24 bagfedcbagfedcbagfedc l10 n25" 'ShutDown
KILL "c:\access\temp\*.*"
Box3D 220, 218, 405, 245, DoubleOut, 0
LOCATE 15, 29: PRINT "       Done!          "
fadeout
CLS
PALETTE
SYSTEM
RUN
END SELECT
END SUB

SUB SpeakerState (OnOff%)
' Turns speaker on or off.
IF OnOff% THEN WriteDSP &HD1 ELSE WriteDSP &HD3
END SUB

FUNCTION SpeakerStatus%
OUT BasePort% + 4, &HD8
IF INP(BasePort% + 5) = &HFF THEN SpeakerStatus% = -1 ELSE SpeakerStatus% = 0
END FUNCTION

SUB splash
'----------------------------------------------------------------------------
'                      Displays the startup splash screen
'----------------------------------------------------------------------------

HideMouse

x = ScreenXDim \ 2 - 110
y = ScreenYDim \ 2 - 80
X2 = ScreenXDim \ 2 + 110
y2 = ScreenYDim \ 2 + 80
OPEN "c:\access\system\user.inf" FOR INPUT AS #1
INPUT #1, TextText$(2)
CLOSE #1

Topic$ = "Whassup " + TextText$(2) + "!"
TopicCol = 4
TopicBCol = 9
Fillcol = Fill

IF Fillcol = Fill THEN
  LINE (x + 2, y + 2)-(X2 - 2, y2 - 2), Fill, BF
ELSE
  LINE (x + 2, y + 2)-(X2 - 1, y + 3), Fill, BF
  LINE (x + 2, y + 17)-(X2 - 1, y + 18), Fill, BF
  LINE (x + 2, y + 2)-(x + 2, y2 - 2), Fill, BF
  LINE (X2 - 1, y2 - 1)-(x + 1, y2 - 4), Fill, BF
  LINE (X2 - 1, y2 - 1)-(X2 - 4, y + 2), Fill, BF
END IF

LINE (X2 - 1, y2 - 1)-(x, y2 - 1), Shadow
LINE (X2 - 1, y2 - 1)-(X2 - 1, y), Shadow
LINE (X2, y2)-(x, y2), 0
LINE (X2, y2)-(X2, y), 0



LINE (x + 1, y + 1)-(x + 1, y2 - 3), Highlight
LINE (x + 1, y + 1)-(X2 - 1, y + 1), Highlight
PSET (x + 1, y2 - 2), Fill
LINE (x, y)-(x, y2), Fill
LINE (x, y)-(X2, y), Fill

LINE (x + 4, y + 20)-(x + 4, y2 - 5), 0
LINE (x + 4, y + 20)-(X2 - 5, y + 20), 0
LINE (x + 3, y + 19)-(x + 3, y2 - 5), Shadow
LINE (x + 3, y + 19)-(X2 - 5, y + 19), Shadow

LINE (X2 - 3, y2 - 3)-(x + 2, y2 - 3), Highlight
LINE (X2 - 3, y2 - 3)-(X2 - 3, y + 19), Highlight
LINE (x + 5, y + 21)-(X2 - 5, y2 - 5), Fillcol, BF

LINE (x + 3, y + 4)-(X2 - 4, y + 16), TopicBCol, BF
PSET (x, y), desktop
PSET (X2, y), desktop
PSET (x, y2), desktop
printF x + 6, y + 6, Topic$, INT(TopicCol), 16

printF x + 39, y + 31, "Welcome to Access", Highlight, 16
printF x + 38, y + 30, "Welcome to Access", 0, 16

printF x + 39 + LEN("Welcome to Access ") * FontSpace&, y + 31, Version.Major + "." + Version.Minor, 8, 16
printF x + 38 + LEN("Welcome to Access ") * FontSpace&, y + 30, Version.Major + "." + Version.Minor, 15, 16

printF x + 6, y + 41, "By Odis Clemons and ", Highlight, 16
printF x + 7, y + 40, "By Odis Clemons and ", 0, 16


printF x + 88 + LEN("   By ") * FontSpace&, y + 41, "Sami Ky”stil„", 8, 16
printF x + 89 + LEN("   By ") * FontSpace&, y + 40, "Sami Ky”stil„", 15, 16


Box3D x + 12, y + 55, X2 - 12, y2 - 10, SingleIn, 16
Box3D x + 13, y + 56, X2 - 13, y2 - 11, singleOut, 16

Box3D x + 20, y + 85, X2 - 20, y2 - 30, SingleIn, 16
Box3D x + 22, y + 87, X2 - 22, y2 - 32, singleOut, 3


RESTORE Quotes
READ Max

Quote = INT(RND * Max) + 1

FOR i = 1 TO Quote
  READ Quote$
NEXT

printF x + 18, y + 60, "It is /c4" + Now$, 0, 16
printF x + 18, y + 70, "Programmer's quote of the day:", 0, 16

printF x + 28, y + 100, wrap$(Quote$, 26), 0, 16


ShowMouse
PLAY "mbO2l24cdefgabacdefgabgcdefgab l1n37" 'Startup music
DO
  updatemouse
  IF Click = 1 THEN EXIT DO
LOOP UNTIL key$ <> ""
HideMouse

LINE (x, y)-(X2, y2), desktop, BF

ShowMouse
END SUB

SUB Switch (Var, Value1, Value2)
'----------------------------------------------------------------------------
'                           Switches values
'----------------------------------------------------------------------------
'
'     Var - Variable to be changed
'  Value1 - Value 1
'  Value2 - Value 2
'
'----------------------------------------------------------------------------
'
' if Var = Value1 then Value2 will be assigned to Var
'
' if Var = Value2 then Value1 will be assigned to Var
'
' This SUB is used with Checkboxes and Radiobuttons
'
'----------------------------------------------------------------------------


IF Var = Value1 THEN Var = Value2: EXIT SUB
IF Var = Value2 THEN Var = Value1
END SUB

SUB TaskMan (opt)
IF opt = 1 THEN CreateWindow 8, 545, 19, 640, 52, "TasksMan", 8, true
CreateMenu 8, 1, "Tasks"
Skip5:
FOR i = 1 TO MaxWindows - 1
   IF Win(i).X2 <> 0 AND Win(i).X2 <> 0 THEN AddMenuItem 8, 1, WinTopic$(i)
  NEXT i

END SUB

FUNCTION Trim$ (Number)
'----------------------------------------------------------------------------
'             Converts numbers to strings and removes null padding
'----------------------------------------------------------------------------
'
'  Number - Number to convert
'
'----------------------------------------------------------------------------


Trim$ = LTRIM$(RTRIM$(STR$(Number)))
END FUNCTION

SUB updatemouse
'----------------------------------------------------------------------------
'                    Reads mouse position and status
'----------------------------------------------------------------------------
  DialogClickedButton = 0
  InputDialogClickedButton = 0
  clickedButton = 0
  key$ = INKEY$
  mousemove = 0
  MovedBar = 0
  ClickedRadioButton = 0
  ClickedCheckBox = 0
  ClickedIcon = 0
  Hiirilue Hiiriv%, Hiirio%, Hiirik%, HiiriX%, HiiriY%
  IF ScreenMode <> 13 AND ScreenMode <> 7 THEN hx = HiiriX% ELSE hx = HiiriX% \ 2
  hy = HiiriY%
  hv = Hiiriv%
  ho = Hiirio%
  Hk = Hiirik%

  IF hv = 0 AND Click = -1 THEN Click = 1 ELSE IF hv = 0 THEN Click = 0
  IF hv = 0 THEN drag = 0
  IF hv = 0 THEN MovingBar = 0
  IF hv = -1 AND Click = 0 THEN Click = -1

  IF hx <> OldHiiriX THEN mousemove = 1
  IF hy <> OldHiiriY THEN mousemove = 1
  IF hx <> OldHiiriX AND hv = -1 THEN drag = 1
  IF hy <> OldHiiriY AND hv = -1 THEN drag = 1


END SUB

SUB UpdateScreen
'----------------------------------------------------------------------------
'                        Redraws the screen
'----------------------------------------------------------------------------
Adesktop
FOR i = 0 TO MaxWindows
  IF Win(i).X2 <> 0 AND Win(i).X2 <> 0 THEN
    IF i = aWIn THEN Win(i).TopicCol = ActiveTopic ELSE Win(i).TopicCol = DeActiveTopic
    IF i = aWIn THEN Win(i).TopicBCol = ActiveTopicB ELSE Win(i).TopicBCol = DeActiveTopicB
    IF i <> aWIn THEN ReDrawWindow (i)
  END IF
NEXT

ReDrawWindow (aWIn)
END SUB

SUB viewW (num)
'----------------------------------------------------------------------------
'                      Sets the viewport
'----------------------------------------------------------------------------
'
' Num - If -1, then the whole screen is used as a viewport, else
'       the window with an index number of Num will be used as the
'       viewport.
'
'----------------------------------------------------------------------------
'  ViewX, ViewY, ViewX2, ViewY2 are set to the current viewport coordinates.
'----------------------------------------------------------------------------
IF num > 0 THEN
  x = Win(num).x + 5
  X2 = Win(num).X2 - 5
  y = Win(num).y + 21
  y2 = Win(num).y2 - 5
  IF X2 >= ScreenXDim THEN X2 = ScreenXDim - 1
  IF y2 >= ScreenYDim THEN y2 = ScreenYDim - 1
  IF x <= 0 THEN x = 0
  IF y <= 0 THEN y = 0
  IF x >= ScreenXDim THEN x = ScreenXDim - 1
  IF y >= ScreenYDim THEN y = ScreenYDim - 1
  IF y = y2 THEN EXIT SUB
  IF x = X2 THEN EXIT SUB
  VIEW SCREEN (x, y)-(X2, y2)
  ViewX = x
  ViewY = y
  ViewX2 = X2
  ViewY2 = y2
ELSE
  VIEW SCREEN (0, 0)-(ScreenXDim - 1, ScreenYDim - 1)
  ViewX = 0
  ViewY = 0
  ViewX2 = ScreenXDim - 1
  ViewY2 = ScreenYDim - 1
END IF
END SUB

SUB VocVolume (right%, left%, Getvol%)
'----------------------------------------------------------------------------
' Sets the VOC-volume on the Sound Blaster card
'----------------------------------------------------------------------------
'
'  Right% - Right volume
'   Left% - Left volume
' Getvol% - If true, then volume will be read, not set
'
'----------------------------------------------------------------------------

OUT BasePort% + 4, &H4
IF Getvol% THEN
   left% = INP(BasePort% + 5) \ 16
   right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (right% + left% * 16) AND &HFF
END IF
END SUB

SUB WavPlay (file$, Speed!)
'----------------------------------------------------------------------------
'  Plays a WAV, VOC, SND sound file  (use only if WavPlayDMA doesn't work)
'----------------------------------------------------------------------------
'
'                 File$ - File to be played
'                Speed! - Playback speed (1: normal)
'
'----------------------------------------------------------------------------
'
' Comparance to WavPlayDMA:
'
'   - Doesn't play in the backround
'   - Playback speed relative to disk speed
'   + More compatible
'
'----------------------------------------------------------------------------
'                   Set volume levels with *Volume subs
'----------------------------------------------------------------------------


ResetSB

IF Exist(file$) = false THEN EXIT SUB

OPEN file$ FOR BINARY AS #255

Bytes& = LOF(255)

IF Bytes& > 32000 THEN delay% = 40 ELSE delay% = 60


bytesremaining& = Bytes&
buffermax% = 100
buffer$ = SPACE$(buffermax%)

SpeakerState true

DO
bytesremaining& = bytesremaining& - bufferlen%
IF bytesremaining& = 0 THEN CLOSE #255: EXIT SUB
IF bytesremaining& > buffermax% THEN
  bufferlen% = buffermax%
ELSE
  bufferlen% = bytesremaining&
  buffer$ = SPACE$(bufferlen%)
END IF

GET #255, , buffer$
DEF SEG = VARSEG(buffer$)

voff& = SADD(buffer$)
ps! = voff&

FOR t% = 1 TO bufferlen% / Speed!
  FOR qq% = 1 TO delay%: NEXT qq%
    WAIT &H22C, &H80, &HFF
    OUT &H22C, &H10
    WAIT &H22C, &H80, &HFF
    OUT &H22C, PEEK(voff&)
    ps! = ps! + Speed!
    voff& = ps!
NEXT t%
LOOP

SpeakerState false
CLOSE #255


END SUB

SUB WavPlayDMA (file$, Freq&)
'----------------------------------------------------------------------------
'  Plays a WAV, VOC, SND sound file in the backround
'----------------------------------------------------------------------------
'
'                 File$ - File to be played
'                 Freq& - Playback frequency
'
'----------------------------------------------------------------------------
'
' Comparance to WavPlay:
'
'   + Plays in the backround
'   + Better sound quality
'   + Correct playback speed
'   - Requires more memory
'   - Sometimes may lock up Sound Blaster
'
'----------------------------------------------------------------------------
'                   Set volume levels with *Volume subs
'----------------------------------------------------------------------------


IF PlayingWAV& = true THEN CLOSE #WavFilehandle&: PlayingWAV& = false

Wavfile = FREEFILE
IF Exist(file$) = false THEN EXIT SUB

OPEN file$ FOR BINARY AS #Wavfile


IF RIGHT$(UCASE$(file$), 3) = "WAV" THEN
  GET #Wavfile, 44, WavBuffer(0)
  Length& = LOF(1) - 44
ELSE
  GET #Wavfile, , WavBuffer(0)
  Length& = LOF(1)
END IF


IF Length& > 8000 THEN
  WavLength& = Length&
  WavFilehandle& = Wavfile
  WavOffset& = 0
  WavFreq& = Freq&
  Length& = 8000
  PlayingWAV& = true
  DMAPlay VARSEG(WavBuffer(0)), VARPTR(WavBuffer(0)), Length&, Freq&
ELSE
  DMAPlay VARSEG(WavBuffer(0)), VARPTR(WavBuffer(0)), Length&, Freq&
  CLOSE #Wavfile
END IF


END SUB

FUNCTION wrap$ (InputText$, wrap)

'----------------------------------------------------------------------------
'                 Wraps long strings at position Wrap
'----------------------------------------------------------------------------

IF InputText$ = "" THEN EXIT FUNCTION
IF wrap <= 1 THEN EXIT FUNCTION
IF LEN(InputText$) <= wrap THEN wrap$ = InputText$: EXIT FUNCTION
TPos = 0
VPos = TPos
last = 0
Line$ = ""

DO
  TPos = TPos + 1
  VPos = VPos + 1
  IF TPos > LEN(InputText$) THEN EXIT DO
  IF VPos > wrap THEN
    IF MID$(InputText$, TPos, 1) <> " " THEN
      TPos2 = TPos
      DO
        TPos2 = TPos2 - 1
        IF TPos2 < 1 THEN
          InputText$ = LEFT$(InputText$, TPos - 1) + "/n" + RIGHT$(InputText$, LEN(InputText$) - TPos)
          VPos = 0
          EXIT DO
        END IF
      LOOP UNTIL MID$(InputText$, TPos2, 1) = " "
      IF TPos2 > 0 THEN
        TPos = TPos2
        InputText$ = LEFT$(InputText$, TPos - 1) + "/n" + RIGHT$(InputText$, LEN(InputText$) - TPos)
        VPos = 0
      END IF
    ELSE
      InputText$ = LEFT$(InputText$, TPos - 1) + "/n" + RIGHT$(InputText$, LEN(InputText$) - TPos)
      VPos = 0
    END IF
  END IF
LOOP UNTIL TPos = LEN(InputText$)

wrap$ = InputText$

END FUNCTION

SUB WriteDAC (Byte%)
' Writes a byte to the DAC.
WriteDSP &H10
WriteDSP Byte%
END SUB

SUB WriteDSP (Byte%)
' Writes a byte to the DSP
DO
LOOP WHILE INP(BasePort% + 12) AND &H80
OUT BasePort% + 12, Byte%

END SUB

SUB xorline (x, y, X2, y2, box%, Pixels)
'----------------------------------------------------------------------------
' Draws a line or a rectangle without erasing the backround
'----------------------------------------------------------------------------
'
'                 x, y - start point
'               x2, y2 - End point
'                 Box% - If 1, then draws a rectangle, else a line
'               Pixels - The integrity of the line, the larger, the slower
'
'----------------------------------------------------------------------------
HideMouse

StepX = (X2 - x) / Pixels
StepY = (y2 - y) / Pixels

PsetX = x
PsetY = y

IF box% = 0 THEN
  FOR i = 0 TO Pixels
    PSET (PsetX, PsetY), 15 XOR POINT(PsetX, PsetY)
    PsetX = PsetX + StepX
    PsetY = PsetY + StepY
  NEXT
ELSE
  PsetX = x + StepX
  FOR i = 0 TO Pixels - 2
    PSET (PsetX, PsetY), 15 XOR POINT(PsetX, PsetY)
    PsetX = PsetX + StepX
  NEXT

  PsetX = x
  PsetY = y
  FOR i = 0 TO Pixels - 1
    PSET (PsetX, PsetY), 15 XOR POINT(PsetX, PsetY)
    PsetY = PsetY + StepY
  NEXT

  PsetX = x
  PsetY = y2
  FOR i = 0 TO Pixels
    PSET (PsetX, PsetY), 15 XOR POINT(PsetX, PsetY)
    PsetX = PsetX + StepX
  NEXT

  PsetX = X2
  PsetY = y
  FOR i = 0 TO Pixels - 1
    PSET (PsetX, PsetY), 15 XOR POINT(PsetX, PsetY)
    PsetY = PsetY + StepY
  NEXT
END IF

ShowMouse
END SUB

