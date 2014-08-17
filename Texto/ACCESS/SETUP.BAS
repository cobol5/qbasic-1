'$DYNAMIC
DEFINT A-Z
DECLARE SUB work (num%)
DECLARE SUB Choice ()
DECLARE SUB control (opt)
DECLARE SUB CreateBMP (WindowNum%, num%, x%, y%, Filename$)
DECLARE SUB LoadBMP (num%)
DECLARE FUNCTION GetBit% (Byte%, Bit%)
DECLARE SUB CreateIcon (WindowNum%, num%, x%, y%, Filename$)
DECLARE SUB LoadIco (num%)
DECLARE FUNCTION ReturnNibble% (Byte%, WhichOne%)
DECLARE SUB DOCopy ()
DECLARE SUB BackroundCopy (Filename$, Path$)
DECLARE SUB InputDialog (topic$, Text$, Password%)
DECLARE SUB Dialog (topic$, Text$, Button1$, Button2$, Button3$)
DECLARE SUB CreateTextBox (Win%, num%, x%, y%, Caption$)
DECLARE SUB DrawTextBox (num%, EraseOld)
DECLARE SUB KillCheckBox (num%)
DECLARE SUB KillRadioButton (num%)
DECLARE SUB KillPBar (num%)
DECLARE SUB KillLabel (num%)
DECLARE SUB KillTextfield (num%)
DECLARE SUB KillScrollbar (num%)
DECLARE SUB KillButton (num%)
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
DECLARE SUB CenterText (Txt$)
DECLARE FUNCTION Trim$ (Number%)
DECLARE FUNCTION MouseInside% (x%, y%, x2%, y2%)
DECLARE SUB CreatePbar (WindowNum%, num%, x%, y%, XLen%, YLen%, Value%, max%)
DECLARE SUB DrawPercentBar (num%)
DECLARE SUB splash ()
DECLARE SUB CreateFrame (WindowNum, num, x, y, XLen, YLen, Caption$)
DECLARE SUB DrawFrame (num)
DECLARE FUNCTION Exist% (file$)
DECLARE SUB Load640 (file$, startLine!)
DECLARE SUB Save640 (file$, startLine!)
DECLARE SUB AddMenuItem (WindowNum, Number, Caption$)
DECLARE SUB DrawMenuItems (WindowNum, Number)
DECLARE SUB CreateMenu (WindowNum, Number, Caption$)
DECLARE SUB DrawMenu (WindowNum)
DECLARE FUNCTION wrap$ (InputText$, wrap)
DECLARE SUB Updatemouse ()
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
DECLARE SUB CreateRadioButton (WindowNum, num, Group, x, y, Caption$, State)
DECLARE SUB DrawRadio (num, DrawText)
DECLARE SUB CreateCheckBox (WindowNum, num, x, y, Caption$, State)
DECLARE SUB DrawCheckBox (num, DrawText)
DECLARE SUB Switch (Var, Value1, Value2)
DECLARE SUB SetPAL (I&, r&, g&, b&)
DECLARE FUNCTION Error$ (Virhe)
DECLARE SUB GPFRuutu ()
DECLARE SUB CreateTextField (WindowNum, num, x, y, Length, Txt$, Passwd)
DECLARE SUB DrawTextField (num)
DECLARE SUB DrawScrollBar (num)
DECLARE SUB CreateScrollBar (WindowNum, num, dir, x, y, XLen, YLen, max, Value)
DECLARE SUB printF (x&, y&, Textz$, FCol%, BCol%)
DECLARE SUB DefScreen ()
DECLARE SUB LabelText (num, Caption$, FCol, BCol)
DECLARE SUB CreateLabel (WindowNum, num, x, y, XLen, YLen, Caption$, FCol, BCol, BorderStyle, Align, AutoWrap)
DECLARE SUB DrawLabel (num)
DECLARE SUB Box3D (x, y, x2, y2, Style, Fillcol)
DECLARE SUB updateScreen ()
DECLARE SUB ReDrawWindow (num)
DECLARE SUB RedDrawWindow (num)
DECLARE SUB xorline (x, y, x2, y2, box%, Pixels)
DECLARE SUB viewW (num)
DECLARE SUB Initscreen ()
DECLARE SUB CreateButton (WindowNum, num, x, y, x2, y2, Caption$)
DECLARE SUB DrawButton (num, Pressed, Hollow)
DECLARE SUB CreateWindow (num, x, y, x2, y2, topic$, Fillcol, WinDrag)
DECLARE SUB DrawWindow (num)
DECLARE SUB GetFont ()
DECLARE SUB Hiirilue (vasen%, oikea%, keski%, x%, y%)
DECLARE SUB Hiirirajat (x1%, y1%, x2%, y2%)
DECLARE SUB Hiiripiiloon ()
DECLARE SUB Hiiriesiin ()
DECLARE FUNCTION Hiiritarkista% ()
DECLARE SUB Hiiriajuri (ax%, bx%, cx%, dx%)
'OPTION BASE 1
'ON ERROR GOTO Virhe
TIMER ON
'ON TIMER(1) GOSUB iconMove

RANDOMIZE TIMER
CONST none = 0
CONST DoubleIN = 1
CONST DoubleOut = 2
CONST SingleIN = 3
CONST Singleout = 4
CONST left = 0
CONST Center = 1
CONST Right = 2
CONST TRUE = 1
CONST False = NOT TRUE



'----------------------------------------------------------------------------
'To minimize memory usage, decrease the maximum amount of objects allowed below
'----------------------------------------------------------------------------
' The SUBS Dialog and InputDialog use the last three button tags, the last
' Window tag and the last TextField tag, so don't use the last index numbers
' in your programs if you use either of those SUBS.
'----------------------------------------------------------------------------


CONST MaxWindows = 15
CONST maxbuttons = 40
CONST MaxLabels = 20
CONST MaxScrollBars = 20
CONST MaxTextFields = 20
CONST MaxCheckBoxes = 10
CONST MaxRadioButtons = 15
CONST MaxFrames = 11
CONST MaxPercentBars = 4
CONST MaxTextboxes = 10
CONST MaxIcons = 5
CONST MaxBMPs = 5

'----------------------------------------------------------------------------
' How many bytes will be copied at one pass when using Backroundcopy
' The larger the number, the faster the copy process and the slower
' the multitasking system. (recommended: 512)
'----------------------------------------------------------------------------
CONST CopyBytes = 512
TIMER ON
'ON TIMER(1) GOSUB statusbar
'ON TIMER(5) GOSUB mousefix
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
  x2 AS INTEGER
  y2 AS INTEGER
  TopicCol AS INTEGER
  TopicBCol AS INTEGER
  Fillcol AS INTEGER
  drag AS INTEGER
END TYPE

TYPE Textfieldtype
  x AS INTEGER
  y AS INTEGER
  max AS INTEGER
  Win AS INTEGER
  Password AS INTEGER
  Disabled AS INTEGER
END TYPE

TYPE ButtonType
  x AS INTEGER
  y AS INTEGER
  x2 AS INTEGER
  y2 AS INTEGER
  Win AS INTEGER
  Disabled AS INTEGER
END TYPE

TYPE ScrollBarType
  dir AS INTEGER
  x AS INTEGER
  y AS INTEGER
  x2 AS INTEGER
  y2 AS INTEGER
  Value AS SINGLE
  max AS INTEGER
  Win AS INTEGER
  SStep AS SINGLE
  Button AS INTEGER
END TYPE

TYPE Labeltype
  x AS INTEGER
  y AS INTEGER
  x2 AS INTEGER
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
  Caption AS STRING * 10
  Items AS INTEGER
END TYPE

TYPE Frametype
  Win AS INTEGER
  x AS INTEGER
  y AS INTEGER
  x2 AS INTEGER
  y2 AS INTEGER
END TYPE

TYPE PBartype
  Win AS INTEGER
  x AS INTEGER
  y AS INTEGER
  x2 AS INTEGER
  y2 AS INTEGER
  max AS INTEGER
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
DIM SHARED SuDir$

'Window
DIM SHARED Win(MaxWindows) AS WindowType
DIM SHARED WinTopic$(MaxWindows)
DIM SHARED aWin, AText
DIM SHARED key$
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
DIM SHARED Menu(MaxWindows, 8, 14) AS STRING * 10
DIM SHARED MenuActive AS INTEGER
DIM SHARED MenuSaved AS INTEGER
DIM SHARED MenuNumber AS INTEGER
DIM SHARED Menuselected AS INTEGER
DIM SHARED MenuHeld AS INTEGER



'Frame
DIM SHARED Frame(MaxFrames) AS Frametype
DIM SHARED FrameCaption$(MaxFrames)


'Percent bar
DIM SHARED PBar(MaxPercentBars) AS PBartype


'Textbox
DIM SHARED TextB(MaxTextboxes) AS TextType
DIM SHARED TextBCaption$(MaxTextboxes)


'Icon
DIM SHARED Icon(MaxIcons) AS Icontype
DIM SHARED IconFile$(MaxIcons)


'BMP-picture
DIM SHARED Bitmap(MaxBMPs) AS BMPType
DIM SHARED BitmapFile$(MaxBMPs)


'Misc
DIM SHARED StatusMSG$, lWin, AccessSkinColor, Future, Process$, part
DIM SHARED Text$(1 TO 1300), Position, MaxPosition
DIM SHARED Version AS VersionType
DIM SHARED Shadow, Fill, Highlight
DIM SHARED Screenmode, ScreenXDim, ScreenYDim, ColorDepth, desktop, ActiveTopic, DeActiveTopic, ActiveTopicB, DeActiveTopicB
DIM SHARED ClickedButton, HoldButton, MovedBar, MovingBar, click
DIM SHARED hx, hy, hv, ho, Hk
DIM SHARED DragXOffset, DragYOffset, dragWin, DragXLen, DragYLen, DragLine
DIM SHARED OldHiiriY, OldHiiriX, MouseMove, drag
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
Choice

DefScreen

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
FOR I% = 1 TO 57
  READ a$
  h$ = CHR$(VAL("&H" + a$))
  MID$(hiiri$, I%, 1) = h$
NEXT I%

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
DialogActive = False
InputDialogActive = False

'----------------------------------------------------------------------------
'                         Show splash screen
'----------------------------------------------------------------------------

Hiiriesiin
splash

'----------------------------------------------------------------------------
'                       Read objects and draw them
'----------------------------------------------------------------------------
FOR I& = 1 TO MaxWindows
  Win(I&).x = ScreenXDim + 1000
  Win(I&).y = ScreenYDim + 1000
  Win(I&).x2 = ScreenXDim + 1000
  Win(I&).y2 = ScreenYDim + 1000
NEXT


Initscreen

IF Screenmode <> 13 AND Screenmode <> 7 THEN Hiirirajat 0, 0, ScreenXDim - 1, ScreenYDim - 1 ELSE Hiirirajat 0, 0, ScreenXDim * 2 - 1, ScreenYDim * 2 - 1
PlayingWAV = 0
Hiiriesiin
MenuActive = False
MenuSaved = False
MenuLoaded = False
Copying = False


'----------------------------------------------------------------------------
'                               Draw screen
'----------------------------------------------------------------------------

updateScreen


'////////////////////////////////////////////////////////////////////////////
'----------------------------------------------------------------------------
'                             start MAIN loop
'----------------------------------------------------------------------------
'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


DO
  '----------------------------------------------------------------------------
  '                        Read mouse coords.
  '----------------------------------------------------------------------------
  Updatemouse


  '----------------------------------------------------------------------------
  '                        Handle all objects
  '----------------------------------------------------------------------------
  main



  Virhe = 0
  Virhekuvaus$ = ""
startup:
'----------------------------------------------------------------------------
  '                     start of user code
  '----------------------------------------------------------------------------
GOSUB ButtonCheck

'----------------------------------------------------------------------------
  '                        End of user code
  '----------------------------------------------------------------------------


LOOP
endit:
Dialog "Shutdown Access", "Do you really want to Shut Down Access?", "Yes", "No", ""
StatusMSG$ = "Shutting down will delete all temp files and close all progams..."
  RETURN

statusbar:
IF StatusMSG$ = temp1$ AND StatusMSG$ = "" OR StatusMSG$ = "" THEN LINE (1, 461)-(639, 479), desktop, BF
           Box3D -1, 461, 639, 479, DoubleOut, 7
           printF 7, 466, "                                                ", 0, 16
           printF 7, 466, "/c04" + StatusMSG$, 0, 16
temp1$ = StatusMSG$
RETURN


Virhe:
Virhe = ERR
IF Virhe = 53 THEN Virhe = 0
QBVirhe = Virhe
RESUME NEXT

mousefix:
Hiiripiiloon
Hiiriesiin
RETURN

ButtonCheck:
SELECT CASE ClickedButton
CASE 1
        'killwindow 1
        part = part + 1
        work part
CASE 2
        part = part - 1
        work part

END SELECT
RETURN
iconMove:
IF aWin <> 1 THEN RETURN
Box3D IconMove1, 284, IconMove1 + 32, 300, 0, 7
IconMove1 = IconMove1 + 20
IF IconMove1 = 225 THEN IconMove1 = 26
CreateIcon 1, 3, IconMove1, 160, "c:\access\icons\desktop.ico"
RETURN
'----------------------------------------------------------------------------
'                            Quote of the day
'----------------------------------------------------------------------------

Quotes:
'Got rid of those damn quotes!

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

REM $STATIC
SUB AddMenuItem (WindowNum, Number, Caption$)
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

Menu(WindowNum, Number, MenuBar(WindowNum, Number).Items) = Caption$

END SUB

SUB BackroundCopy (Filename$, Path$)
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


IF NOT Exist(Filename$) = TRUE THEN EXIT SUB

IF INSTR(Filename$, "\") > 0 THEN
  FOR I = 1 TO LEN(Filename$)
    IF MID$(Filename$, I, 1) = "\" THEN startPos = I
  NEXT
  Filename2$ = RIGHT$(Filename$, LEN(Filename$) - INSTR(startPos, Filename$, "\"))
END IF

CopyFile = FREEFILE


OPEN Filename$ FOR BINARY AS #CopyFile
IF RIGHT$(Path$, 1) <> "\" THEN Path$ = Path$ + "\"

CopyFile2 = FREEFILE
OPEN Path$ + Filename2$ FOR BINARY AS #CopyFile2

SEEK #CopyFile, 1
SEEK #CopyFile2, 1


Copying = TRUE
CopyPos = 0
CopyDone = 0
END SUB

SUB Box3D (x, y, x2, y2, BorderStyle, Fillcol)
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
  LINE (x, y)-(x2, y), 0
  LINE (x, y)-(x, y2), 0
  LINE (x2, y2)-(x2, y), Fill
  LINE (x2, y2)-(x, y2), Fill

  LINE (x + 1, y + 1)-(x2 - 1, y + 1), Shadow
  LINE (x + 1, y + 1)-(x + 1, y2 - 1), Shadow
  LINE (x2 - 1, y2 - 1)-(x2 - 1, y + 1), Highlight
  LINE (x2 - 1, y2 - 1)-(x + 1, y2 - 1), Highlight
END IF

IF BorderStyle = 2 THEN
  LINE (x, y)-(x2, y), Fill
  LINE (x, y)-(x, y2), Fill
  LINE (x2, y2)-(x2, y), 0
  LINE (x2, y2)-(x, y2), 0

  LINE (x + 1, y + 1)-(x2 - 1, y + 1), Highlight
  LINE (x + 1, y + 1)-(x + 1, y2 - 1), Highlight
  LINE (x2 - 1, y2 - 1)-(x2 - 1, y + 1), Shadow
  LINE (x2 - 1, y2 - 1)-(x + 1, y2 - 1), Shadow
END IF

IF BorderStyle = 3 THEN
  IF Fillcol <> Shadow THEN LINE (x, y)-(x2, y), Shadow ELSE LINE (x, y)-(x2, y), 0
  IF Fillcol <> Shadow THEN LINE (x, y)-(x, y2), Shadow ELSE LINE (x, y)-(x, y2), 0
  LINE (x2, y2)-(x2, y), Highlight
  LINE (x2, y2)-(x, y2), Highlight
END IF

IF BorderStyle = 4 THEN
  LINE (x, y)-(x2, y), Highlight
  LINE (x, y)-(x, y2), Highlight
  IF Fillcol <> Shadow THEN LINE (x2, y2)-(x2, y), Shadow ELSE LINE (x2, y2)-(x2, y), 0
  IF Fillcol <> Shadow THEN LINE (x2, y2)-(x, y2), Shadow ELSE LINE (x2, y2)-(x, y2), 0
END IF

IF NOT Fillcol = 16 THEN
  IF BorderStyle <= 2 THEN
    LINE (x + 2, y + 2)-(x2 - 2, y2 - 2), Fillcol, BF
  ELSE
    LINE (x + 1, y + 1)-(x2 - 1, y2 - 1), Fillcol, BF
  END IF
END IF

END SUB

SUB CenterText (Txt$)
'----------------------------------------------------------------------------
'                     Centers Txt$ on the screen
'----------------------------------------------------------------------------

LOCATE , 40 - LEN(Txt$) \ 2
PRINT Txt$
END SUB

SUB Choice
PRINT "Access a1.1 setup": PRINT : PRINT
PRINT "Hello.  I am the Access Genie! "
PRINT "Before we begin setup of Access a1.1, I need to know something."
SLEEP 3
PRINT
INPUT "What directory am I being installed from (e.g. a:\directory\)", SuDir$
CLS
PRINT "Access a1.1 setup": PRINT : PRINT
PRINT : PRINT "Do you want to run setup as:"
PRINT " 1) A graphic interface"
PRINT " 2) Text mode?"
PRINT : PRINT "Press 1 or 2 to make your choice."
PRINT : PRINT "NOTE: Text mode not finished.  Look for it in the Access update a1.2"
SLEEP 3
PRINT "So that actually means you only have one choice.  Press '1' !"
loopit:
DO
temp$ = INKEY$
LOOP WHILE INKEY$ < ""
IF temp$ = "1" THEN mode = 1: EXIT SUB
GOTO loopit
setup:
DIM file(1 TO 145)
DIM SuFile$(1 TO 145)
CLS
PRINT "Access version a1.1 setup"
LOCATE 5, 1:
PRINT "Making directories..."
'MKDIR "c:\access"
'MKDIR "c:\access\icons"
'MKDIR "c:\access\system"
'MKDIR "c:\access\temp"
SuFile$(1) = "c:\ACCESS\ACCESS.BAS"
SuFile$(2) = "c:\ACCESS\WINFONT.DAT"
SuFile$(3) = "c:\ACCESS\ICONS\FOLDER.ICO"
SuFile$(4) = "c:\ACCESS\ICONS\COMPUTER.ICO"
SuFile$(5) = "c:\ACCESS\ICONS\OFOLDER.ICO"
SuFile$(6) = "c:\ACCESS\ICONS\ERROR.ICO"
SuFile$(7) = "c:\ACCESS\ICONS\ENVELOP.ICO"
SuFile$(8) = "c:\ACCESS\ICONS\MAGNIFY.ICO"
SuFile$(9) = "c:\ACCESS\ICONS\IMAGE.ICO"
SuFile$(10) = "c:\ACCESS\ICONS\HELP.ICO"
SuFile$(11) = "c:\ACCESS\ICONS\CD.ICO"
SuFile$(12) = "c:\ACCESS\ICONS\FLOPPY.ICO"
SuFile$(13) = "c:\ACCESS\ICONS\ENCRIPT.ICO"
SuFile$(14) = "c:\ACCESS\ICONS\LOCKED.ICO"
SuFile$(15) = "c:\ACCESS\ICONS\CALENDR.ICO"
SuFile$(16) = "c:\ACCESS\ICONS\SOUNDST.ICO"
SuFile$(17) = "c:\ACCESS\ICONS\VOLUME.ICO"
SuFile$(18) = "c:\ACCESS\ICONS\CDVOL.ICO"
SuFile$(19) = "c:\ACCESS\ICONS\ACCESS.ICO"
SuFile$(20) = "c:\ACCESS\ICONS\MYCOMP.ICO"
SuFile$(21) = "c:\ACCESS\ICONS\DISABLE.ICO"
SuFile$(22) = "c:\ACCESS\ICONS\FOLDERO.ICO"
SuFile$(23) = "c:\ACCESS\ICONS\USERPRO.ICO"
SuFile$(24) = "c:\ACCESS\ICONS\MOUSE.ICO"
SuFile$(25) = "c:\ACCESS\ICONS\SLEEP.ICO"
SuFile$(26) = "c:\ACCESS\ICONS\CHECFDR.ICO"
SuFile$(27) = "c:\ACCESS\ICONS\CONTROL.ICO"
SuFile$(28) = "c:\ACCESS\ICONS\COPYDSK.ICO"
SuFile$(29) = "c:\ACCESS\ICONS\DESKTOP.ICO"
SuFile$(30) = "c:\access\access.hlp"
SuFile$(31) = "c:\ACCESS\ICONS\CURSOR.ICO"
SuFile$(32) = "c:\ACCESS\ICONS\BCURSOR.ICO"
SuFile$(33) = "c:\ACCESS\ICONS\FIND.ICO"
SuFile$(34) = "c:\ACCESS\ICONS\TRASH.ICO"
SuFile$(35) = "c:\ACCESS\ICONS\NOTEBK.ICO"
SuFile$(36) = "c:\access\control.hlp"
SuFile$(37) = "c:\ACCESS\ICONS\TOOLTIP.ICO"
SuFile$(38) = "c:\ACCESS\ICONS\COPY.ICO"
SuFile$(39) = "c:\ACCESS\ICONS\COPYFDR.ICO"
SuFile$(40) = "c:\ACCESS\ICONS\HARDDRV.ICO"
SuFile$(41) = "c:\ACCESS\ICONS\QUESTION.ICO"
SuFile$(42) = "c:\ACCESS\ICONS\GIF.ICO"
SuFile$(43) = "c:\ACCESS\ICONS\BMP.ICO"
SuFile$(44) = "c:\ACCESS\ICONS\THUNDER.ICO"
SuFile$(45) = "c:\ACCESS\ICONS\CLOCK.ICO"
FOR b = 1 TO 45
OPEN SuFile$(b) FOR RANDOM AS #1
CLS
PRINT LOF(1)
SLEEP
file(b) = INT(LOF(1))
max = max + file(b)
PRINT file(b)
CLOSE #1
NEXT b
FOR b = 1 TO 45
file(b) = file(b) * 100 / max
CreateLabel 1, 2, 100, 57, 325, 50, SuFile$(b - 1), 0, 16, none, left, 1
CreateLabel 1, 2, 100, 57, 325, 50, SuFile$(b), 0, 16, none, left, 1
BackroundCopy SuFile$(b), "a:\"
DO
DOCopy
aDone = aDone + file(b)
CreatePbar 1, 1, 80, 90, 150, 20, b, 100
CreatePbar 1, 2, 80, 120, 150, 20, CopyDone, 100
LOOP UNTIL CopyDone = 100
NEXT b
END
END SUB

SUB control (opt)
CreateButton 1, 1, 230, 140, 40, 20, "Next >"
CreateButton 1, 2, 30, 140, 40, 20, "< Back"
StatusMSG$ = "Control Panel Active..."
SELECT CASE opt
CASE 5
killWindow 2
Process$ = ""
'CreateWindow 7, 25, 25, 240, 225, "Personal info", 7, TRUE
CreateWindow 3, 100, 100, 450, 350, "Access a1.1 setup", 7, False
CreateFrame 3, 1, 1, 10, 200, 55, "Name"
CreateLabel 3, 1, 3, 15, 40, 10, "First Name", 0, 16, none, left, 0
CreateTextField 3, 2, 3, 25, 10, TextText$(2), False
CreateLabel 3, 2, 100, 15, 40, 10, "Last Name", 0, 16, none, left, 0
CreateTextField 3, 3, 100, 25, 10, TextText$(3), False
CreateLabel 3, 3, 105, 47, 40, 10, "Middle Initial", 0, 16, none, left, 0
CreateTextField 3, 4, 80, 45, 1, TextText$(4), False
CreateFrame 3, 2, 1, 70, 200, 60, "Address"
CreateLabel 3, 4, 3, 75, 40, 10, "Number and Street", 0, 16, none, left, 0
CreateTextField 3, 5, 3, 85, 15, TextText$(5), False
CreateLabel 3, 5, 3, 100, 40, 10, "City", 0, 16, none, left, 0
CreateTextField 3, 6, 3, 110, 10, TextText$(6), False
CreateLabel 3, 6, 120, 77, 40, 8, "State", 0, 16, none, left, 0
CreateTextField 3, 7, 120, 87, 2, TextText$(7), False
CreateLabel 3, 7, 120, 102, 40, 8, "Zip Code", 0, 16, none, left, 0
CreateTextField 3, 8, 120, 110, 5, TextText$(8), False
CreateButton 3, 1, 230, 140, 40, 20, "Next >"
CreateButton 3, 2, 30, 140, 40, 20, "< Back"
temp1 = 4
END SELECT
END SUB

SUB CreateBMP (WindowNum, num, x, y, Filename$)
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
BitmapFile$(num) = Filename$

LoadBMP num


END SUB

SUB CreateButton (WindowNum, num, x, y, XLen, YLen, Caption$)
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
But(num).x = x
But(num).y = y
But(num).x2 = x + XLen
But(num).y2 = y + YLen
ButCaption$(num) = Caption$
But(num).Win = WindowNum
But(num).Disabled = False
DrawButton num, 0, 0

END SUB

SUB CreateCheckBox (WindowNum, num, x, y, Caption$, State)
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

CheckBox(num).x = x
CheckBox(num).y = y
CheckBox(num).State = State
CheckBox(num).Win = WindowNum
CheckBoxText$(num) = Caption$
CheckBox(num).Disabled = False

DrawCheckBox num, TRUE


END SUB

SUB CreateFrame (WindowNum, num, x, y, XLen, YLen, Caption$)
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


Frame(num).Win = WindowNum
Frame(num).x = x
Frame(num).y = y
Frame(num).x2 = x + XLen
Frame(num).y2 = y + YLen
FrameCaption$(num) = Caption$

DrawFrame num

END SUB

SUB CreateIcon (WindowNum, num, x, y, Filename$)
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
IconFile$(num) = Filename$
LoadIco num


END SUB

SUB CreateLabel (WindowNum, num, x, y, XLen, YLen, Caption$, FCol, BCol, BorderStyle, Align, AutoWrap)
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
Label(num).x2 = XLen + x
Label(num).y = y
Label(num).y2 = YLen + y
LabelCaption$(num) = Caption$
Label(num).FCol = FCol
Label(num).BCol = BCol
Label(num).Style = BorderStyle
Label(num).Win = WindowNum
Label(num).Align = Align
Label(num).wrap = AutoWrap

DrawLabel num

END SUB

SUB CreateMenu (WindowNum, Number, Caption$)
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

MenuBar(WindowNum, Number).Caption = Caption$
MenuBar(WindowNum, Number).Items = 0

DrawMenu WindowNum

END SUB

SUB CreatePbar (WindowNum, num, x, y, XLen, YLen, Value, max)
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



PBar(num).Win = WindowNum
PBar(num).x = x
PBar(num).y = y
PBar(num).x2 = x + XLen
PBar(num).y2 = y + YLen
PBar(num).Value = Value
PBar(num).OldValue = Value
PBar(num).max = max

DrawPercentBar num

END SUB

SUB CreateRadioButton (WindowNum, num, Group, x, y, Caption$, State)
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

Radio(num).Win = WindowNum
Radio(num).Group = Group
Radio(num).x = x
Radio(num).y = y
RadioText$(num) = Caption$
Radio(num).State = State
Radio(num).Disabled = False

DrawRadio num, TRUE

END SUB

SUB CreateScrollBar (WindowNum, num, dir, x, y, XLen, YLen, max, Value)
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

IF max < Value THEN EXIT SUB
viewW (-1)

IF dir = 0 AND XLen < 44 THEN XLen = 44
IF dir = 1 AND YLen < 44 THEN YLen = 44

SBar(num).dir = dir
SBar(num).x = x
SBar(num).y = y
SBar(num).x2 = x + XLen
SBar(num).y2 = y + YLen
SBar(num).max = max
SBar(num).Value = Value
SBar(num).Win = WindowNum
SBar(num).Button = (num * 2) + maxbuttons - 1

IF dir = 0 THEN
  SBar(num).SStep = (XLen - 43) / max
  CreateButton WindowNum, (num * 2) + maxbuttons - 1, x, y, 14, YLen, CHR$(137)
  CreateButton WindowNum, (num * 2) + maxbuttons, x + XLen - 14, y, 14, YLen, CHR$(136)
ELSE
  SBar(num).SStep = (YLen - 43) / max
  CreateButton WindowNum, (num * 2) + maxbuttons - 1, x, y, XLen, 14, CHR$(140)
  CreateButton WindowNum, (num * 2) + maxbuttons, x, y + YLen - 14, XLen, 14, CHR$(135)
END IF

DrawScrollBar num

END SUB

SUB CreateTextBox (Win, num, x, y, Caption$)
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


IF TextBCaption$(num) <> "" THEN DrawTextBox num, TRUE


TextB(num).x = x
TextB(num).y = y
TextBCaption$(num) = Caption$
TextB(num).Win = Win

DrawTextBox num, False

END SUB

SUB CreateTextField (WindowNum, num, x, y, Length, Txt$, Passwd)
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

Txt$ = LEFT$(Txt$, Length)
TextF(num).x = x
TextF(num).y = y
TextText$(num) = Txt$
TextF(num).max = Length
TextF(num).Win = WindowNum
TextF(num).Password = Passwd
TextF(num).Disabled = False

DrawTextField num

END SUB

SUB CreateWindow (num, x, y, x2, y2, topic$, Fillcol, WinDrag)
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
Win(num).x = x
Win(num).y = y
Win(num).x2 = x2
Win(num).y2 = y2
WinTopic$(num) = topic$
Win(num).TopicCol = DeActiveTopic
Win(num).TopicBCol = DeActiveTopicB
Win(num).Fillcol = Fillcol
Win(num).drag = WinDrag
DrawWindow (num)
IF Skin = 1 THEN
  temp = 1: b = 13
  CreateBMP aWin, 1, temp, 13, Skinfile$
  CreateBMP aWin, 2, temp + 100, 13, Skinfile$
  CreateBMP aWin, 3, temp + 200, 13, Skinfile$
END IF
aWin = num
END SUB

SUB DefScreen
'----------------------------------------------------------------------------
'                      Defines screen parameters
'----------------------------------------------------------------------------
Screenmode = 12                  'Screenmode to be used
ScreenXDim = 640                 'Max. X
ScreenYDim = 480                 'Max. Y
ColorDepth = 64                  'Number of colors

desktop = 8                      'Desktop color
DragLine = 60                    'Integrity of window outline when moving
ActiveTopic = 0                  'Active topic color
DeActiveTopic = 0                'Deactive topic color
ActiveTopicB = 9                'Active topic backround color
DeActiveTopicB = 1               'Deactive topic backround color
aWin = 1                         'Active window at startup
AccessSkinColor = 0

Highlight = 15                   '3D-object highlight color
Fill = 7                         '3D-object fill color
Shadow = 8                       '3D-object shadow color

CursorChar$ = CHR$(179)          'Cursor character
SCREEN Screenmode: CLS
Win(0).x = 0
Win(0).y = 1
Win(0).x2 = 639
Win(0).y2 = 18
WinTopic$(0) = "Start"
Win(0).TopicCol = DeActiveTopic
Win(0).TopicBCol = DeActiveTopicB
Win(0).Fillcol = 8
Win(0).drag = 1
desktop = 1
    Desktopa = desktop
END SUB

SUB Dialog (topic$, Text$, Button1$, Button2$, Button3$)
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
topica$ = topic$
IF Button1$ = "" THEN EXIT SUB
x = ScreenXDim \ 2 - 110
y = ScreenYDim \ 2 - 80
x2 = ScreenXDim \ 2 + 110
y2 = ScreenYDim \ 2 + 80

CreateWindow MaxWindows, x, y, x2, y2, topic$, Fill, False


DialogActive = TRUE
DialogSelectedButton = 0
DialogAWin = aWin
aWin = MaxWindows

IF Button2$ = "" AND Button3$ = "" AND Button1$ <> "" THEN
  CreateButton MaxWindows, maxbuttons, (x2 - x) \ 2 - 30, (y2 - y) \ 2 + 30 - 10, 60, 20, Button1$
END IF

IF Button2$ <> "" AND Button3$ = "" AND Button1$ <> "" THEN
  CreateButton MaxWindows, maxbuttons, (x2 - x) \ 2 - 80, (y2 - y) \ 2 + 30 - 10, 60, 20, Button1$
  CreateButton MaxWindows, maxbuttons - 1, (x2 - x) \ 2 + 10, (y2 - y) \ 2 + 30 - 10, 60, 20, Button2$
END IF

IF Button2$ <> "" AND Button3$ <> "" AND Button1$ <> "" THEN
  CreateButton MaxWindows, maxbuttons, 10, (y2 - y) \ 2 + 30 - 10, 60, 20, Button1$
  CreateButton MaxWindows, maxbuttons - 1, (x2 - x) \ 2 - 35, (y2 - y) \ 2 + 30 - 10, 60, 20, Button2$
  CreateButton MaxWindows, maxbuttons - 2, (x2 - x) - 80, (y2 - y) \ 2 + 30 - 10, 60, 20, Button3$
END IF

Win(MaxWindows).TopicBCol = 1
Win(MaxWindows).TopicCol = 15

ReDrawWindow MaxWindows
Hiiripiiloon
printF x + 10, y + 40, wrap$(Text$, 34), 0, 16
Hiiriesiin
aWin = MaxWindows
END SUB

SUB DisableButton (num)
'----------------------------------------------------------------------------
'                        Disables a button
'----------------------------------------------------------------------------
'
'  Num - Button number
'
'----------------------------------------------------------------------------




But(num).Disabled = TRUE
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

CheckBox(num).Disabled = TRUE

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

FOR I = 1 TO MaxRadioButtons
  IF Radio(I).Group = Group THEN Radio(I).Disabled = TRUE: DrawRadio I, 0
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

TextF(num).Disabled = TRUE

DrawTextField num

END SUB

FUNCTION DMADone%
'----------------------------------------------------------------------------
'                Use to see if a DMA transfer has been completed
'----------------------------------------------------------------------------

Count% = INP(LenPort%)
Count2% = INP(LenPort%)
Count& = CLNG(Count% + 1) * CLNG(Count2% + 1)
IF (Count& - 1) >= &HFFFF& THEN junk% = INP(DSPDataAvail%): DMADone% = -1
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

SUB DOCopy
'----------------------------------------------------------------------------
'      Does the copying process for Backroundcopy, called by the system
'----------------------------------------------------------------------------

IF NOT Copying = TRUE THEN EXIT SUB

FOR I = 1 TO CopyBytes
  IF CopyPos = LOF(CopyFile) THEN
    Copying = False
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
Hiiripiiloon


x = But(num).x + Win(WindowNum).x + 5
y = But(num).y + Win(WindowNum).y + 21
x2 = But(num).x2 + Win(WindowNum).x + 5
y2 = But(num).y2 + Win(WindowNum).y + 21
Caption$ = ButCaption$(num)


IF Pressed = 1 THEN
  LINE (x, y)-(x2, y), 0
  LINE (x, y)-(x, y2), 0
  LINE (x2, y2)-(x2, y), Fill
  LINE (x2, y2)-(x, y2), Fill

  LINE (x + 1, y + 1)-(x2 - 1, y + 1), Shadow
  LINE (x + 1, y + 1)-(x + 1, y2 - 1), Shadow
  LINE (x2 - 1, y2 - 1)-(x2 - 1, y + 1), Highlight
  LINE (x2 - 1, y2 - 1)-(x + 1, y2 - 1), Highlight
ELSE
  LINE (x, y)-(x2, y), Fill
  LINE (x, y)-(x, y2), Fill
  LINE (x2, y2)-(x2, y), 0
  LINE (x2, y2)-(x, y2), 0

  LINE (x + 1, y + 1)-(x2 - 1, y + 1), Highlight
  LINE (x + 1, y + 1)-(x + 1, y2 - 1), Highlight
  LINE (x2 - 1, y2 - 1)-(x2 - 1, y + 1), Shadow
  LINE (x2 - 1, y2 - 1)-(x + 1, y2 - 1), Shadow
END IF

IF Hollow = 0 THEN LINE (x + 2, y + 2)-(x2 - 2, y2 - 2), Fill, BF


IF Hollow = 0 THEN
  CapLines = 1
  FOR I = 1 TO LEN(Caption$)
    IF MID$(Caption$, I, 2) = "/n" THEN CapLines = CapLines + 1
  NEXT
  IF INSTR(Caption$, "/n") > 0 THEN Capline$ = LEFT$(Caption$, INSTR(Caption$, "/n")) ELSE Capline$ = Caption$

  Textpos% = ((x2 - x) \ 2) - ((LEN(Capline$) * FontX&) \ 2) + x + 1
  IF But(num).Disabled = False THEN
    printF INT(Textpos%), INT((y2 - y)) \ 2 + y - ((CapLines * 8) / 2) + 1, Caption$, 0, 16
  ELSE
    printF INT(Textpos%), INT((y2 - y)) \ 2 + y - ((CapLines * 8) / 2) + 1, Caption$, 8, 16
  END IF
END IF

Hiiriesiin
viewW (-1)
END SUB

SUB DrawCheckBox (num, DrawText)
'----------------------------------------------------------------------------
'             Draws a checkbox, called by the system
'----------------------------------------------------------------------------



WindowNum = CheckBox(num).Win
viewW (WindowNum)
Hiiripiiloon

x = CheckBox(num).x + Win(WindowNum).x + 5
y = CheckBox(num).y + Win(WindowNum).y + 21
State = CheckBox(num).State
Caption$ = CheckBoxText$(num)

'Box3D x + 1, y + 1, x + 10, y + 10, DoubleIN, Highlight
LINE (x + 1, y + 1)-(x + 10, y + 1), 0
LINE (x + 1, y + 1)-(x + 1, y + 10), 0

LINE (x + 2, y + 2)-(x + 9, y + 2), Shadow
LINE (x + 2, y + 2)-(x + 2, y + 9), Shadow

LINE (x + 10, y + 10)-(x + 10, y + 1), Highlight
LINE (x + 10, y + 10)-(x + 1, y + 10), Highlight

LINE (x + 9, y + 9)-(x + 9, y + 2), Fill
LINE (x + 9, y + 9)-(x + 2, y + 9), Fill


IF CheckBox(num).Disabled = TRUE THEN
  LINE (x + 3, y + 3)-(x + 8, y + 8), Fill, BF
ELSE
  LINE (x + 3, y + 3)-(x + 8, y + 8), Highlight, BF
END IF


IF State = TRUE THEN
  LINE (x + 3, y + 3)-(x + 7, y + 7), Shadow
  LINE (x + 7, y + 3)-(x + 3, y + 7), Shadow
  LINE (x + 4, y + 3)-(x + 8, y + 7), 0
  LINE (x + 8, y + 3)-(x + 4, y + 7), 0
END IF


IF DrawText = TRUE THEN printF x + 18, y + 2, Caption$, 0, 16


Hiiriesiin
viewW (-1)
END SUB

SUB DrawFrame (num)
'----------------------------------------------------------------------------
'                 Draws a frame, called by the system
'----------------------------------------------------------------------------

WindowNum = Frame(num).Win

Hiiripiiloon
viewW WindowNum

x = Frame(num).x + Win(WindowNum).x + 5
x2 = Frame(num).x2 + Win(WindowNum).x + 5
y = Frame(num).y + Win(WindowNum).y + 21
y2 = Frame(num).y2 + Win(WindowNum).y + 21
Caption$ = FrameCaption$(num)

Box3D x, y, x2, y2, SingleIN, 16
Box3D x + 1, y + 1, x2 - 1, y2 - 1, Singleout, 16
LINE (x + 2, y + 2)-(x2 - 2, y2 - 2), Fill, BF

IF Caption$ <> "" THEN
  printF x + (FontX&), y - 3, Caption$, 0, INT(Fill)
END IF

viewW -1
Hiiriesiin

END SUB

SUB DrawLabel (num)
'----------------------------------------------------------------------------
'             Draws a label, called by the system
'----------------------------------------------------------------------------

WindowNum = Label(num).Win


viewW (WindowNum)
Hiiripiiloon

x = Label(num).x + Win(WindowNum).x + 5
x2 = Label(num).x2 + Win(WindowNum).x + 5
y = Label(num).y + Win(WindowNum).y + 21
y2 = Label(num).y2 + Win(WindowNum).y + 21
Caption$ = LabelCaption$(num)
FCol = Label(num).FCol
BCol = Label(num).BCol
BorderStyle = Label(num).Style
Align = Label(num).Align

Box3D x, y, x2, y2, BorderStyle, BCol

IF Label(num).wrap = 1 THEN Caption$ = wrap$(Caption$, (x2 - x) \ FontSpace& - 2)

CapLines = 1
FOR I = 1 TO LEN(Caption$)
  IF MID$(Caption$, I, 2) = "/n" THEN CapLines = CapLines + 1
NEXT
IF INSTR(Caption$, "/n") > 0 THEN Capline$ = LEFT$(Caption$, INSTR(Caption$, "/n")) ELSE Capline$ = Caption$

IF Align = 0 THEN Textpos = x + FontSpace& \ 2
IF Align = 1 THEN
  Textpos = ((x2 - x) \ 2) - (((LEN(Capline$)) * FontX&) \ 2) + x + 1
END IF

IF Align = 2 THEN Textpos = x2 - ((LEN(Capline$))) - FontSpace& \ 2

IF FCol <> 0 AND BCol = 0 THEN
  printF INT(Textpos), (y2 - y) \ 2 + y - ((CapLines * FontY&) / 2), Caption$, INT(FCol), 0
ELSE
  printF INT(Textpos), (y2 - y) \ 2 + y - ((CapLines * FontY&) / 2), Caption$, INT(FCol), 16
END IF

Hiiriesiin
viewW (-1)


END SUB

SUB DrawMenu (WindowNum)
'----------------------------------------------------------------------------
'             Draws a menu, called by the system
'----------------------------------------------------------------------------

Hiiripiiloon

x = Win(WindowNum).x + 3
y = Win(WindowNum).y + 21
x2 = Win(WindowNum).x2 - 3


LINE (x, y - 2)-(x2, y + 10), Fill, BF
LINE (x, y + 11)-(x2, y + 11), Shadow, BF
LINE (x, y + 12)-(x2, y + 12), 0, BF

FOR I& = 1 TO 8
  IF LEN(RTRIM$(MenuBar(WindowNum, I&).Caption)) <> 10 AND MenuBar(WindowNum, I&).Items > 0 THEN
    printF x + ((I& - 1) * 8 * 10) + 2, INT(y), MenuBar(WindowNum, I&).Caption, 0, 16
  END IF
NEXT


Hiiriesiin
END SUB

SUB DrawMenuItems (WindowNum, Number)
'----------------------------------------------------------------------------
'             Draws a menu's items, called by the system
'----------------------------------------------------------------------------

Hiiripiiloon

x = Win(WindowNum).x + 3 + ((Number - 1) * 8 * 10) + 2
y = Win(WindowNum).y + 21 + 11
x2 = Win(WindowNum).x + 3 + ((Number - 1) * 8 * 10) + 2 + 8 + (FontX& * 10)
y2 = Win(WindowNum).y + 21 + 14 + MenuBar(WindowNum, Number).Items * 12

Box3D x, y, x2, y2, DoubleOut, Fill

FOR I& = 1 TO MenuBar(WindowNum, Number).Items
  printF x + 5, y + I& * 12 - 8, Menu(WindowNum, Number, I&), 0, 16
NEXT

Hiiriesiin

END SUB

SUB DrawPercentBar (num)
'----------------------------------------------------------------------------
'             Draws a percent bar, called by the system
'----------------------------------------------------------------------------


WindowNum = PBar(num).Win

x = PBar(num).x + Win(WindowNum).x + 5
x2 = PBar(num).x2 + Win(WindowNum).x + 5
y = PBar(num).y + Win(WindowNum).y + 21
y2 = PBar(num).y2 + Win(WindowNum).y + 21

IF MouseInside(x, y, x2, y2) = TRUE THEN Hiiripiiloon
viewW WindowNum

Value = PBar(num).Value
max = PBar(num).max

XPos = ((x2 - x - 4) / 100) * (Value / max * 100)

Box3D x, y, x2, y2, DoubleIN, 16

IF XPos < 3 THEN
  LINE (x + 2, y2 - 2)-(x2 - 2, y + 2), Fill, BF
  CALL printF(x + (x2 - x) \ 2 - FontSpace& * 2, y + 1 + (y2 - y) \ 2 - FontY& \ 2, "/c01" + LTRIM$(RTRIM$(STR$(INT(Value / max * 100)))) + "%", 0, 16)
  Hiiriesiin
  viewW -1
  EXIT SUB
END IF


IF XPos >= (x2 - x) - 3 THEN XPos = (x2 - x) - 3



    

LINE (x + 2, y + 2)-(x + XPos, y + 2), 15
LINE (x + 2, y + 2)-(x + 2, y2 - 2), 15
    
LINE (x + 3, y + 3)-(x + XPos - 1, y + 3), 12 '11
LINE (x + 3, y + 3)-(x + 3, y2 - 3), 12
    
LINE (x + 4, y + 4)-(x + XPos - 2, y2 - 4), 4, BF '3
    
LINE (x + XPos - 1, y2 - 3)-(x + XPos - 1, y + 4), 8
LINE (x + XPos - 1, y2 - 3)-(x + 4, y2 - 3), 8

LINE (x + XPos, y2 - 2)-(x + XPos, y + 3), 0
LINE (x + XPos, y2 - 2)-(x + 3, y2 - 2), 0

LINE (x + XPos + 1, y2 - 2)-(x2 - 2, y + 2), Fill, BF
tem:
CALL printF(x + (x2 - x) \ 2 - FontSpace& * 2, y + 1 + (y2 - y) \ 2 - FontY& \ 2, "/c01" + LTRIM$(RTRIM$(STR$(INT(Value / max * 100)))) + "%", 0, 16)

IF MouseInside(x, y, x2, y2) = TRUE THEN Hiiriesiin
viewW -1

END SUB

SUB DrawRadio (num, DrawText)
'----------------------------------------------------------------------------
'             Draws a radio button, called by the system
'----------------------------------------------------------------------------

WindowNum = Radio(num).Win
Hiiripiiloon
viewW (WindowNum)


x = Radio(num).x + Win(WindowNum).x + 5
y = Radio(num).y + Win(WindowNum).y + 21
Check% = Radio(num).State
Caption$ = RadioText$(num)

IF Radio(num).Disabled = TRUE THEN
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

IF Check% = TRUE THEN
  LINE (x + 3, y + 3)-(x + 5, y + 5), 0, BF
  PSET (x + 3, y + 3), Fill
  PSET (x + 5, y + 5), Fill
  PSET (x + 3, y + 5), Fill
  PSET (x + 5, y + 3), Fill
END IF


IF DrawText = TRUE THEN printF x + 18, y + 1, Caption$, 0, 16


Hiiriesiin
viewW -1
END SUB

SUB DrawScrollBar (num)
'----------------------------------------------------------------------------
'             Draws a scrollbar, called by the system
'----------------------------------------------------------------------------

WindowNum = SBar(num).Win
viewW (WindowNum)
Hiiripiiloon


dir = SBar(num).dir

x = SBar(num).x + Win(WindowNum).x + 5
y = SBar(num).y + Win(WindowNum).y + 21
x2 = SBar(num).x2 + Win(WindowNum).x + 5
y2 = SBar(num).y2 + Win(WindowNum).y + 21
max = SBar(num).max
Value = SBar(num).Value
SStep! = SBar(num).SStep

IF dir = 0 THEN
  LINE (x + 16, y + 2)-(x + 16 + (Value * SStep!) - 1, y2 - 3), Shadow, BF
  LINE (x2 - 16, y2 - 3)-(x + 16 + (Value * SStep!) + 11, y + 2), Shadow, BF
  Box3D x + 15, y + 1, x2 - 15, y2 - 2, 3, 16
  Box3D x + 16 + (Value * SStep!), y + 2, x + 16 + (Value * SStep!) + 10, y2 - 3, 2, Fill
ELSE
  LINE (x + 2, y + 16)-(x2 - 3, y + 16 + (Value * SStep!) - 1), Shadow, BF
  LINE (x2 - 3, y2 - 16)-(x + 3, y + 27 + (Value * SStep!) - 1), Shadow, BF
  Box3D x + 2, y + 15, x2 - 2, y2 - 15, 3, 16
  Box3D x + 2, y + 16 + (Value * SStep!), x2 - 3, y + 16 + (Value * SStep!) + 10, 2, Fill
END IF


viewW (-1)
Hiiriesiin

END SUB

SUB DrawTextBox (num, EraseOld)
'----------------------------------------------------------------------------
'                          Draws a checkbox
'----------------------------------------------------------------------------

Hiiripiiloon
viewW TextB(num).Win

IF EraseOld = TRUE THEN
  TempFill = Fill
  printF TextB(num).x + Win(TextB(num).Win).x + 5, TextB(num).y + Win(TextB(num).Win).y + 21, TextBCaption$(num), TempFill, TempFill
ELSE
  printF TextB(num).x + Win(TextB(num).Win).x + 5, TextB(num).y + Win(TextB(num).Win).y + 21, TextBCaption$(num), 0, Fill
END IF


Hiiriesiin
viewW -1
END SUB

SUB DrawTextField (num)
'----------------------------------------------------------------------------
'             Draws a textfield, called by the system
'----------------------------------------------------------------------------

WindowNum = TextF(num).Win
viewW (WindowNum)
Hiiripiiloon

x = TextF(num).x + Win(WindowNum).x + 5
y = TextF(num).y + Win(WindowNum).y + 21
Txt$ = TextText$(num)
Length = TextF(num).max
Passwd = TextF(num).Password


IF TextF(num).Disabled = TRUE THEN
  Box3D x, y, x + (Length * FontSpace&) + 19, y + 14, 1, Fill
ELSE
  Box3D x, y, x + (Length * FontSpace&) + 19, y + 14, 1, 15
END IF

IF Passwd = False THEN printF x + 6, y + 4, Txt$, 0, 16 ELSE printF x + 6, y + 4, STRING$(LEN(Txt$), "*"), 0, 16

viewW (-1)
Hiiriesiin

END SUB

SUB DrawWindow (num)
'----------------------------------------------------------------------------
'             Draws a window, called by the system
'----------------------------------------------------------------------------

Hiiripiiloon
viewW -1
x = Win(num).x
y = Win(num).y
x2 = Win(num).x2
y2 = Win(num).y2
topic$ = WinTopic$(num)
TopicCol = Win(num).TopicCol
TopicBCol = Win(num).TopicBCol
Fillcol = Win(num).Fillcol
IF x = 0 AND y = 0 AND x2 = 0 AND y2 = 0 THEN Hiiriesiin: EXIT SUB


IF Fillcol = Fill THEN
  LINE (x + 2, y + 2)-(x2 - 2, y2 - 2), Fill, BF
ELSE
  LINE (x + 2, y + 2)-(x2 - 1, y + 3), Fill, BF
  LINE (x + 2, y + 17)-(x2 - 1, y + 18), Fill, BF
  LINE (x + 2, y + 2)-(x + 2, y2 - 2), Fill, BF
  LINE (x2 - 1, y2 - 1)-(x + 1, y2 - 4), Fill, BF
  LINE (x2 - 1, y2 - 1)-(x2 - 4, y + 2), Fill, BF
END IF

LINE (x2 - 1, y2 - 1)-(x, y2 - 1), Shadow
LINE (x2 - 1, y2 - 1)-(x2 - 1, y), Shadow
LINE (x2, y2)-(x, y2), 0
LINE (x2, y2)-(x2, y), 0



LINE (x + 1, y + 1)-(x + 1, y2 - 3), Highlight
LINE (x + 1, y + 1)-(x2 - 1, y + 1), Highlight
PSET (x + 1, y2 - 2), Fill
LINE (x, y)-(x, y2), Fill
LINE (x, y)-(x2, y), Fill

LINE (x + 4, y + 20)-(x + 4, y2 - 5), 0
LINE (x + 4, y + 20)-(x2 - 5, y + 20), 0
LINE (x + 3, y + 19)-(x + 3, y2 - 5), Shadow
LINE (x + 3, y + 19)-(x2 - 5, y + 19), Shadow

LINE (x2 - 3, y2 - 3)-(x + 2, y2 - 3), Highlight
LINE (x2 - 3, y2 - 3)-(x2 - 3, y + 19), Highlight
LINE (x + 5, y + 21)-(x2 - 5, y2 - 5), Fillcol, BF

LINE (x + 3, y + 4)-(x2 - 4, y + 16), TopicBCol, BF
PSET (x, y), desktop
PSET (x2, y), desktop
PSET (x, y2), desktop



printF x + 6, y + 6, topic$, INT(TopicCol), 16


FOR I& = 1 TO 8
  IF LEN(RTRIM$(MenuBar(num, I&).Caption)) <> 10 THEN
    DrawMenu num
  END IF
NEXT
wx = x2 - 17
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
IF aWin <> 0 THEN
CreateMenu aWin, 1, "Access"
AddMenuItem aWin, 1, "/rHelp"
AddMenuItem aWin, 1, "/rMinimize"
AddMenuItem aWin, 1, "Maximize"
AddMenuItem aWin, 1, "Close"
END IF
Hiiriesiin
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

But(num).Disabled = False
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

CheckBox(num).Disabled = False

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

FOR I = 1 TO MaxRadioButtons
  IF Radio(I).Group = Group THEN Radio(I).Disabled = False: DrawRadio I, 0
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

TextF(num).Disabled = False

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


IF file$ = "" THEN Exist = False: EXIT FUNCTION
Checkfile = FREEFILE

OPEN file$ FOR BINARY AS #Checkfile
IF LOF(Checkfile) = 0 THEN
  Exist = False
  CLOSE #Checkfile
  KILL file$
ELSE
  Exist = TRUE
  CLOSE #Checkfile
END IF
END FUNCTION

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

SetPAL 15, 0, 0, 0

DIM pic AS STRING * 1
OPEN SuDir$ + "Winfont.dat" FOR BINARY AS #3
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
FOR I = 33 TO 138
  IF x > 312 THEN x = 0: y = y + 8
  GET (x, y)-(x + FontX& - 1, y + FontY& - 1), Font((I - 32) * 48)
  x = x + FontX&
NEXT

SetPAL 15, 63, 63, 63
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
PRINT " þ Windoze has detected error #";
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
PRINT " þ Program execution will be terminated."
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
PRINT aWin
COLOR 7
PRINT "     Clicked button:",
COLOR 11
PRINT ClickedButton
COLOR 7
PRINT "     QBasic error:",
COLOR 11
PRINT QBVirhe; "("; Error$(QBVirhe); ")"
COLOR 7
PRINT "     Mouse coordinates:",
COLOR 11
PRINT " x:"; hx, "y:"; hy
COLOR 7

END
END SUB

SUB Hiiriajuri (ax%, bx%, cx%, dx%)
  DEF SEG = VARSEG(hiiri$)                  'Segmentti talteen
  hiiri% = SADD(hiiri$)                     'Offsetti talteen
  CALL absolute(ax%, bx%, cx%, dx%, hiiri%) 'Kutsu
END SUB

SUB Hiiriesiin
'----------------------------------------------------------------------------
'                      Shows the mouse cursor
'----------------------------------------------------------------------------
 
  ax% = 1                   'Funktio 1
  Hiiriajuri ax%, 0, 0, 0   'Kutsutaan hiiriajuria
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

SUB Hiiripiiloon
'----------------------------------------------------------------------------
'                   Hides the mouse cursor
'----------------------------------------------------------------------------

  ax% = 2                   'Funktio 2
  Hiiriajuri ax%, 0, 0, 0   'Kutsutaan hiiriajuria
END SUB

SUB Hiirirajat (x1%, y1%, x2%, y2%)
  ax% = 7                         'Funktio 7
  cx% = x1%                       'Rajat cx:„„n ja dx:„„n
  dx% = x2%
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
part = 1
printF 1, 1, "Access version a1.1 setup", 6, 0
CreateWindow 1, 100, 100, 450, 350, "Access a1.1 setup", 7, False
CreateFrame 1, 4, 10, 20, 325, 50, "Hello!"
Text$ = "     Welcome to Access v1.1. This is my first operating system. I designed it with the Windoz engine."
CreateLabel 1, 1, 10, 20, 325, 50, Text$, 0, 16, none, left, 1
CreateButton 1, 1, 130, 140, 40, 20, "Ok"
END SUB

SUB InputDialog (topic$, Text$, Password)
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
x2 = ScreenXDim \ 2 + 160
y2 = ScreenYDim \ 2 + 80

CreateWindow MaxWindows, x, y, x2, y2, topic$, Fill, False


InputDialogActive = TRUE
InputDialogClickedButton = 0
InputDialogAWin = aWin
aWin = MaxWindows

CreateTextField MaxWindows, MaxTextFields, 6, (y2 - y) \ 2 - 10, 47, "", Password
Win(MaxWindows).TopicBCol = 1
Win(MaxWindows).TopicCol = 15

CreateButton MaxWindows, maxbuttons, (x2 - x) \ 2 - 80, (y2 - y) \ 2 + 30 - 10, 60, 20, "OK"
CreateButton MaxWindows, maxbuttons - 1, (x2 - x) \ 2 + 10, (y2 - y) \ 2 + 30 - 10, 60, 20, "Cancel"

ReDrawWindow MaxWindows
Hiiripiiloon
printF x + 10, y + 40, wrap$(Text$, 52), 0, Fill
Hiiriesiin


END SUB

SUB KillButton (num)
'----------------------------------------------------------------------------
'                          Removes a button
'----------------------------------------------------------------------------
'
' Num - Button number
'
'----------------------------------------------------------------------------


IF But(num).x = ScreenXDim + 1000 THEN EXIT SUB

IF aWin <> But(num).Win THEN
 aWin = But(num).Win
 updateScreen
END IF


But(num).x = ScreenXDim + 1000
But(num).y = ScreenYDim + 1000
But(num).x2 = ScreenXDim + 1000
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

IF aWin <> CheckBox(num).Win THEN
 aWin = CheckBox(num).Win
 updateScreen
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

IF aWin <> Label(num).Win THEN
 aWin = But(num).Win
 updateScreen
END IF


Label(num).x = ScreenXDim + 1000
Label(num).y = ScreenYDim + 1000
Label(num).x2 = ScreenXDim + 1000
Label(num).y2 = ScreenYDim + 1000

ReDrawWindow Label(num).Win


END SUB

SUB KillPBar (num)

IF PBar(num).x = ScreenXDim + 1000 THEN EXIT SUB

IF aWin <> PBar(num).Win THEN
 aWin = PBar(num).Win
 updateScreen
END IF


PBar(num).x = ScreenXDim + 1000
PBar(num).y = ScreenYDim + 1000
PBar(num).x2 = ScreenXDim + 1000
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

IF aWin <> Radio(num).Win THEN
 aWin = Radio(num).Win
 updateScreen
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


IF aWin <> SBar(num).Win THEN
 aWin = SBar(num).Win
 updateScreen
END IF

SBar(num).x = ScreenXDim + 1000
SBar(num).y = ScreenYDim + 1000
SBar(num).x2 = ScreenXDim + 1000
SBar(num).y2 = ScreenYDim + 1000



KillButton SBar(num).Button
KillButton SBar(num).Button + 1

ReDrawWindow aWin

END SUB

SUB KillTextfield (num)
'----------------------------------------------------------------------------
'                          Removes a textfield
'----------------------------------------------------------------------------
'
' Num - Textfield number
'
'----------------------------------------------------------------------------

IF TextF(num).x = ScreenXDim + 1000 THEN EXIT SUB


IF aWin <> TextF(num).Win THEN
 aWin = TextF(num).Win
 updateScreen
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
StatusMSG$ = ""
x = Win(num).x
y = Win(num).y
x2 = Win(num).x2
y2 = Win(num).y2

Hiiripiiloon
LINE (x, y)-(x2, y2), desktop, BF
Hiiriesiin

Win(num).x = ScreenXDim + 1000
Win(num).y = ScreenYDim + 1000
Win(num).x2 = ScreenXDim + 1000
Win(num).y2 = ScreenYDim + 1000


updateScreen

END SUB

SUB LabelText (num, Caption$, FCol, BCol)
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

IF NOT Caption$ = "" THEN LabelCaption$(num) = Caption$
IF NOT FCol = -1 THEN Label(num).FCol = FCol
IF NOT BCol = -1 THEN Label(num).BCol = BCol

DrawLabel num

END SUB

SUB LineVolume (Right%, left%, Getvol%)
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
   Right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (Right% + left% * 16) AND &HFF
END IF
END SUB

SUB Load640 (file$, startLine!)
'----------------------------------------------------------------------------
'                Loads a screen saved by Save640
'----------------------------------------------------------------------------
'
'      File$ - Filename to load the screen from (NO EXTENSION!)
' startLine! - start line
'
'----------------------------------------------------------------------------


startLine! = (startLine! * ScreenXDim) \ 8

DEF SEG = &HA000
IF Screenmode = 13 THEN
  BLOAD file$ + ".INT", 0
ELSE
  OUT &H3C4, 2: OUT &H3C5, 1: BLOAD file$ + ".BLU", startLine!
  OUT &H3C4, 2: OUT &H3C5, 2: BLOAD file$ + ".GRN", startLine!
  OUT &H3C4, 2: OUT &H3C5, 4: BLOAD file$ + ".RED", startLine!
  OUT &H3C4, 2: OUT &H3C5, 8: BLOAD file$ + ".INT", startLine!
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

Filename$ = BitmapFile$(num)

IF Exist(Filename$) = False THEN EXIT SUB
Hiiripiiloon
viewW Bitmap(num).Win




DIM BMP AS BMPHeader               'Define header

OPEN Filename$ FOR BINARY AS BMPFile     'Open file
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
         FOR I = 1 TO 8            '...loop through the 8 bits and...
            PSET (x& + xx - 1 + I, y& + yy - 1), GetBit(Bar, I)'...extract each...
         NEXT                      '...and output them
      END IF
   NEXT                            'Loop to next column
SkipRow:
NEXT                               'Done with that row, move to next
CLOSE BMPFile                      'Done!

Hiiriesiin
viewW -1

END SUB

SUB LoadIco (num)
'----------------------------------------------------------------------------
'              Draws an icon, called by the system
'----------------------------------------------------------------------------


xx = Icon(num).x + Win(Icon(num).Win).x + 5
yy = Icon(num).y + Win(Icon(num).Win).y + 21

IconFile = FREEFILE
Filename$ = IconFile$(num)
IF Exist(Filename$) = False THEN EXIT SUB

viewW Icon(num).Win
Hiiripiiloon
  

OPEN Filename$ FOR BINARY AS #IconFile
SEEK #IconFile, 127
Foo$ = STRING$(512, 32)
GET #IconFile, , Foo$
CLOSE #IconFile
Ptr = 1

FOR y& = 32 TO 1 STEP -1
   FOR x& = 1 TO 32 STEP 2
      Bar = ASC(MID$(Foo$, Ptr, 1))
      Ptr = Ptr + 1
      IF Bar = 0 THEN Bar = 7
      PSET (x& + xx, y& + yy), ReturnNibble(Bar, 1)
      PSET (x& + xx + 1, y& + yy), ReturnNibble(Bar, 2)
   NEXT
NEXT


viewW -1
Hiiriesiin

END SUB

SUB main
IF DialogActive = 1 THEN aWin = MaxWindows
IF DialogActive = 1 AND click = 1 THEN ReDrawWindow MaxWindows
IF hv <> -1 AND aWin = 0 THEN aWin = 1

IF click = 1 AND MenuSaved = TRUE THEN
    I& = MenuNumber
      IF LEN(RTRIM$(MenuBar(aWin, I&).Caption)) <> 10 AND MenuBar(aWin, I&).Items > 0 THEN
        IF NOT (hx > Win(aWin).x + 3 + ((I& - 1) * 8 * 10) + 2 AND hx < Win(aWin).x + 3 + ((I&) * 8 * 10) + 2 AND hy > Win(aWin).y + 21 + 12 AND hy < Win(aWin).y + 21 + 12 + (MenuBar(aWin, I&).Items * 12)) THEN
          Hiiripiiloon
          Load640 "~temp", Win(aWin).y + 17
          Hiiriesiin
          MenuSaved = False
          MenuActive = False
          Menuselected = 0
          MenuHeld = 2
        ELSE
          Hiiripiiloon
          Menuselected = MenuBar(aWin, I&).Items - ((Win(aWin).y + 21 + 12 + (MenuBar(aWin, I&).Items * 12)) - hy) \ 12
          printF Win(aWin).x + 4 + 4 + ((I& - 1) * 8 * 10) + 2, Win(aWin).y + 20 + 12 + Menuselected * 12 - 8, Menu(aWin, MenuNumber, Menuselected), 15, 1
          Load640 "~temp", Win(aWin).y + 17
          MenuSaved = False
          MenuActive = False
          MenuHeld = 0
          Hiiriesiin
          click = 0
          hv = 0
        END IF
      END IF
  END IF


  IF hv = -1 AND MenuSaved = TRUE THEN
    I& = MenuNumber
      IF LEN(RTRIM$(MenuBar(aWin, I&).Caption)) <> 10 AND MenuBar(aWin, I&).Items > 0 THEN
        IF (hx > Win(aWin).x + 3 + ((I& - 1) * 8 * 10) + 2 AND hx < Win(aWin).x + 3 + ((I&) * 8 * 10) + 2 AND hy > Win(aWin).y + 21 + 12 AND hy < Win(aWin).y + 21 + 12 + (MenuBar(aWin, I&).Items * 12)) THEN
          OldMenuHeld = MenuHeld
          MenuHeld = MenuBar(aWin, I&).Items - ((Win(aWin).y + 21 + 12 + (MenuBar(aWin, I&).Items * 12)) - hy) \ 12
          IF OldMenuHeld <> MenuHeld AND OldMenuHeld <> 0 THEN
            Hiiripiiloon
            printF Win(aWin).x + 4 + 4 + ((I& - 1) * 8 * 10) + 2, Win(aWin).y + 20 + 12 + OldMenuHeld * 12 - 8, Menu(aWin, MenuNumber, OldMenuHeld), 0, Fill
            printF Win(aWin).x + 4 + 4 + ((I& - 1) * 8 * 10) + 2, Win(aWin).y + 20 + 12 + MenuHeld * 12 - 8, Menu(aWin, MenuNumber, MenuHeld), 15, 1
            Hiiriesiin
          END IF
        END IF
      END IF
  END IF


  IF click = 1 AND MenuSaved = False THEN
  FOR I& = 1 TO 8
      IF LEN(RTRIM$(MenuBar(aWin, I&).Caption)) <> 10 AND MenuBar(aWin, I&).Items > 0 THEN
        IF hx > Win(aWin).x + 3 + ((I& - 1) * 8 * 10) + 2 AND hx < Win(aWin).x + 3 + ((I&) * 8 * 10) + 2 AND hy > Win(aWin).y + 21 AND hy < Win(aWin).y + 21 + 12 THEN
          Hiiripiiloon
          Save640 "~temp", Win(aWin).y + 17
          LINE (Win(aWin).x + 3 + ((I& - 1) * 8 * 10) + 2, Win(aWin).y + 17)-(Win(aWin).x + 3 + ((I& - 1) * 8 * 10) + 2 + (LEN(RTRIM$(MenuBar(aWin, I&).Caption)) * FontSpace&), Win(aWin).y + 21 + 10), 1, BF
          printF Win(aWin).x + 3 + ((I& - 1) * 8 * 10) + 2, Win(aWin).y + 21, RTRIM$(MenuBar(aWin, I&).Caption), 15, 1
          DrawMenuItems aWin, INT(I&)
          Hiiriesiin
          MenuSaved = TRUE
          MenuActive = TRUE
          MenuNumber = I&
        END IF
      END IF
    NEXT
  END IF

  IF MenuActive = False THEN
  IF hv = 0 AND dragWin <> 0 THEN
    xorline Win(dragWin).x, Win(dragWin).y, Win(dragWin).x2, Win(dragWin).y2, 1, DragLine
    updateScreen
    dragWin = 0
  END IF

IF hv = -1 AND dragWin = 0 AND drag = 0 AND Win(aWin).drag = TRUE THEN
    I = aWin
    IF Win(I).x2 <> 0 AND Win(I).y2 <> 0 THEN
      IF hx >= Win(I).x + 5 AND hx <= Win(I).x2 - 5 AND hy >= Win(I).y + 5 AND hy <= Win(I).y + 16 THEN
        Hiiripiiloon
        DragXOffset = hx - Win(I).x
        DragYOffset = hy - Win(I).y
        DragXLen = Win(I).x2 - Win(I).x
        DragYLen = Win(I).y2 - Win(I).y
        dragWin = I
        LINE (Win(dragWin).x, Win(dragWin).y - 1)-(Win(dragWin).x2, Win(dragWin).y2), desktop, BF
        Hiiriesiin
        xorline Win(dragWin).x, Win(dragWin).y, Win(dragWin).x2, Win(dragWin).y2, 1, DragLine
      END IF
    END IF
  END IF

  IF dragWin <> 0 AND MouseMove = 1 THEN
    xorline Win(dragWin).x, Win(dragWin).y, Win(dragWin).x2, Win(dragWin).y2, 1, DragLine
    Win(dragWin).x = hx - DragXOffset
    Win(dragWin).y = hy - DragYOffset
    Win(dragWin).x2 = hx - DragXOffset + DragXLen
    Win(dragWin).y2 = hy - DragYOffset + DragYLen
    xorline Win(dragWin).x, Win(dragWin).y, Win(dragWin).x2, Win(dragWin).y2, 1, DragLine
  END IF



  IF dragWin = 0 THEN

  IF hv = -1 THEN
    FOR I = 1 TO MaxScrollBars
      IF SBar(I).dir = 0 AND SBar(I).Win = aWin THEN
        IF hx > SBar(I).x + 16 + Win(SBar(I).Win).x + 5 AND hx < SBar(I).x2 - 16 + Win(SBar(I).Win).x + 5 AND hy > SBar(I).y + Win(SBar(I).Win).y + 21 AND hy < SBar(I).y2 + Win(SBar(I).Win).y + 21 THEN
          SBar(I).Value = (hx - (SBar(I).x + 16 + Win(SBar(I).Win).x + 5)) / SBar(I).SStep - (5 / SBar(I).SStep)
          IF SBar(I).Value <= 0 THEN SBar(I).Value = 0
          IF SBar(I).Value >= SBar(I).max THEN SBar(I).Value = SBar(I).max
          MovingBar = 1
          MovedBar = I
          DrawScrollBar I
        END IF
      END IF
      IF SBar(I).dir = 1 AND SBar(I).Win = aWin THEN
        IF hx > SBar(I).x + Win(SBar(I).Win).x + 5 AND hx < SBar(I).x2 + Win(SBar(I).Win).x + 5 AND hy > SBar(I).y + 16 + Win(SBar(I).Win).y + 21 AND hy < SBar(I).y2 - 16 + Win(SBar(I).Win).y + 21 THEN
          SBar(I).Value = (hy - (SBar(I).y + 16 + Win(SBar(I).Win).y + 21)) / SBar(I).SStep - (5 / SBar(I).SStep)
          IF SBar(I).Value <= 0 THEN SBar(I).Value = 0
          IF SBar(I).Value >= SBar(I).max THEN SBar(I).Value = SBar(I).max
          MovingBar = 1
          MovedBar = I
          DrawScrollBar I
        END IF
      END IF
    NEXT
  END IF


  IF hv = -1 THEN
    FOR I = 1 TO MaxTextFields
      IF TextF(I).Win = aWin AND I <> AText THEN
        IF hx > TextF(I).x + Win(aWin).x + 5 AND hx < TextF(I).x + (TextF(I).max * FontSpace&) + 24 + Win(aWin).x + 5 AND hy > TextF(I).y + Win(aWin).y + 21 AND hy < TextF(I).y + 14 + Win(aWin).y + 21 AND TextF(I).Disabled = False THEN
          Hiiripiiloon
          IF AText <> 0 THEN
            IF TextF(AText).Win = aWin AND TextF(AText).Disabled = False THEN LINE (TextF(AText).x + Win(aWin).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + Win(aWin).y + 21)-(TextF(AText).x + Win(aWin).x + 5 + 8 +  _
(LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + 8 + Win(aWin).y + 21), 15, BF
          END IF
          AText = I
          DrawTextField I
          printF TextF(AText).x + Win(aWin).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + Win(aWin).y + 21, CursorChar, 0, 16
          Hiiriesiin
        END IF
      END IF
    NEXT
  END IF



  IF click = 1 AND NOT drag = 1 THEN
    FOR I = 1 TO MaxCheckBoxes
      IF CheckBox(I).Win = aWin AND CheckBox(I).Disabled = False THEN
        IF hx > CheckBox(I).x + Win(aWin).x + 5 AND hx < CheckBox(I).x + (LEN(CheckBoxText$(I)) * FontSpace&) + 18 + Win(aWin).x + 5 AND hy > CheckBox(I).y + Win(aWin).y + 21 AND hy < CheckBox(I).y + 11 + Win(aWin).y + 21 THEN
          Switch CheckBox(I).State, TRUE, False
          DrawCheckBox I, False
          ClickedCheckBox = I
        END IF
      END IF
    NEXT
  END IF
       

  IF click = 1 AND NOT drag = 1 THEN
    FOR I = 1 TO MaxRadioButtons
      IF Radio(I).Win = aWin AND Radio(I).Disabled = False THEN
        IF hx > Radio(I).x + Win(aWin).x + 5 AND hx < Radio(I).x + (LEN(RadioText$(I)) * FontSpace&) + 18 + Win(aWin).x + 5 AND hy > Radio(I).y + Win(aWin).y + 21 AND hy < Radio(I).y + 11 + Win(aWin).y + 21 THEN
          Radio(I).State = TRUE
          FOR i2 = 1 TO MaxRadioButtons
            IF Radio(i2).Group = Radio(I).Group AND I <> i2 AND Radio(i2).Win = aWin THEN
              Radio(i2).State = False
              DrawRadio i2, False
            END IF
          NEXT

          DrawRadio I, False
          ClickedRadioButton = I
        END IF
      END IF
    NEXT
  END IF



  IF hv = -1 AND NOT drag = 1 AND DialogActive = False AND InputDialogActive = False THEN
    IF NOT (hx >= Win(aWin).x AND hx <= Win(aWin).x2 AND hy >= Win(aWin).y AND hy <= Win(aWin).y2) THEN
      FOR I = 1 TO MaxWindows
        IF hx >= Win(I).x AND hx <= Win(I).x2 AND hy >= Win(I).y AND hy <= Win(I).y2 THEN
          Win(aWin).TopicCol = DeActiveTopic
          Win(aWin).TopicBCol = DeActiveTopicB
          ReDrawWindow (aWin)
          aWin = I
          Win(aWin).TopicCol = ActiveTopic
          Win(aWin).TopicBCol = ActiveTopicB
          ReDrawWindow (aWin)
        END IF
      NEXT
    END IF
  END IF



  IF hv = -1 AND HoldButton = 0 AND MovingBar = 0 THEN
    FOR I = 1 TO maxbuttons + MaxScrollBars
      IF But(I).Win = aWin THEN
        IF hx >= But(I).x + Win(aWin).x + 5 AND hx <= But(I).x2 + Win(aWin).x + 5 AND hy >= But(I).y + Win(aWin).y + 21 AND hy <= But(I).y2 + Win(aWin).y + 21 AND NOT But(I).Disabled = TRUE THEN
         DrawButton I, 1, 1
         HoldButton = I
        END IF
      END IF
    NEXT
  END IF


  IF HoldButton <> 0 AND hv = 0 AND MovingBar = 0 THEN
    IF hx >= But(HoldButton).x + Win(aWin).x + 5 AND hx <= But(HoldButton).x2 + Win(aWin).x + 5 AND hy >= But(HoldButton).y + Win(aWin).y + 21 AND hy <= But(HoldButton).y2 + Win(aWin).y + 21 THEN
      ClickedButton = HoldButton
      HoldButton = 0
      DrawButton ClickedButton, 0, 1
    ELSE
      DrawButton HoldButton, 0, 1
      HoldButton = 0
      ClickedButton = 0
    END IF
  END IF

  IF HoldButton > maxbuttons THEN
    FOR I = 1 TO MaxScrollBars
      IF SBar(I).Button = HoldButton THEN
        SBar(I).Value = SBar(I).Value - SBar(I).SStep
        IF SBar(I).Value <= 0 THEN SBar(I).Value = 0
        MovedBar = I
        DrawScrollBar I
      END IF
      IF SBar(I).Button + 1 = HoldButton THEN
        SBar(I).Value = SBar(I).Value + SBar(I).SStep
        IF SBar(I).Value >= SBar(I).max THEN SBar(I).Value = SBar(I).max
        MovedBar = I
        DrawScrollBar I
      END IF
    NEXT
  END IF


  IF HoldButton <> 0 AND hv = -1 THEN
    IF NOT (hx >= But(HoldButton).x + Win(aWin).x + 5 AND hx <= But(HoldButton).x2 + Win(aWin).x + 5 AND hy >= But(HoldButton).y + Win(aWin).y + 21 AND hy <= But(HoldButton).y2 + Win(aWin).y + 21) THEN
      DrawButton HoldButton, 0, 1
      HoldButton = 0
    END IF
  END IF


  FOR I = 1 TO MaxPercentBars
    IF aWin = PBar(I).Win THEN
      IF PBar(I).OldValue <> PBar(I).Value THEN PBar(I).OldValue = PBar(I).Value: DrawPercentBar I
    END IF
  NEXT

  IF click = 1 THEN
    FOR I = 1 TO MaxIcons
      IF aWin = Icon(I).Win THEN
        IF hx >= Icon(I).x + Win(aWin).x + 5 AND hx <= Icon(I).x + Win(aWin).x + 5 + 32 AND hy >= Icon(I).y + Win(aWin).y + 21 AND hy <= Icon(I).y + Win(aWin).y + 21 + 32 THEN
          ClickedIcon = I
        END IF
      END IF
    NEXT
  END IF


  IF AText <> 0 THEN
  IF TextF(AText).Disabled = False THEN
  IF key$ <> "" AND TextF(AText).Win = aWin THEN

    IF key$ = CHR$(0) + CHR$(15) THEN
      TextSearch = AText
      DO
        TextSearch = TextSearch - 1
        IF TextSearch < 1 THEN TextSearch = MaxTextFields
        IF TextSearch = AText THEN EXIT DO
        IF TextF(TextSearch).Win = aWin AND TextSearch <> AText AND TextF(TextSearch).Disabled = False AND TextF(TextSearch).x <> ScreenXDim + 1000 THEN
          printF TextF(AText).x + Win(aWin).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + Win(aWin).y + 21, "  ", 0, 15
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
        IF TextF(TextSearch).Win = aWin AND TextSearch <> AText AND TextF(TextSearch).Disabled = False AND TextF(TextSearch).x <> ScreenXDim + 1000 THEN
          printF TextF(AText).x + Win(aWin).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + Win(aWin).y + 21, "  ", 0, 15
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
    IF LEN(TextText$(AText)) = TextF(AText).max THEN key$ = ""

    TextText$(AText) = TextText$(AText) + key$
    TextText$(AText) = LEFT$(TextText$(AText), TextF(AText).max)

    IF TextF(AText).Password = TRUE AND key$ <> "" THEN key$ = "*"

    IF MouseInside(TextF(AText).x + Win(aWin).x + 5, TextF(AText).y + Win(aWin).y + 21, TextF(AText).x + Win(aWin).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + Win(aWin).y + 21 + 14) = TRUE THEN Hiiripiiloon
    printF TextF(AText).x + Win(aWin).x + 5 + (LEN(TextText$(AText)) * FontSpace&), TextF(AText).y + 4 + Win(aWin).y + 21, key$, 0, 15
    printF TextF(AText).x + Win(aWin).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + Win(aWin).y + 21, Cursor, 0, 15


    IF MouseInside(TextF(AText).x + Win(aWin).x + 5, TextF(AText).y + Win(aWin).y + 21, TextF(AText).x + Win(aWin).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + Win(aWin).y + 21 + 14) = TRUE THEN Hiiriesiin
  END IF
    IF TIMER > TextTimer! + .1 THEN
      IF Cursor = "  " THEN Cursor = CursorChar + " ": GOTO DrawCursor
      IF Cursor = CursorChar + " " THEN Cursor = "  "

DrawCursor:
      IF TextF(AText).Win = aWin AND TextF(AText).Disabled = False THEN
        IF MouseInside(TextF(AText).x + Win(aWin).x + 5, TextF(AText).y + Win(aWin).y + 21, TextF(AText).x + Win(aWin).x + 5 + ((TextF(AText).max) * FontSpace&) + 24 + FontSpace&, TextF(AText).y + Win(aWin).y + 21 + 14) = TRUE THEN Hiiripiiloon
        printF TextF(AText).x + Win(aWin).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + Win(aWin).y + 21, Cursor, 0, 15
        IF MouseInside(TextF(AText).x + Win(aWin).x + 5, TextF(AText).y + Win(aWin).y + 21, TextF(AText).x + Win(aWin).x + 5 + ((TextF(AText).max) * FontSpace&) + 24 + FontSpace&, TextF(AText).y + Win(aWin).y + 21 + 14) = TRUE THEN Hiiriesiin
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
      PlayingWAV& = False
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

  IF DialogActive = TRUE AND ClickedButton <> 0 THEN
    aWin = DialogAWin
    IF ClickedButton = maxbuttons THEN DialogClickedButton = 1: killWindow MaxWindows
    IF ClickedButton = maxbuttons - 1 THEN DialogClickedButton = 2: killWindow MaxWindows
    IF ClickedButton = maxbuttons - 2 THEN DialogClickedButton = 3: killWindow MaxWindows
    ClickedButton = 0
    DialogActive = False
  END IF

  IF InputDialogActive = TRUE AND ClickedButton <> 0 THEN
    aWin = InputDialogAWin
    KillTextfield MaxTextFields
    IF ClickedButton = maxbuttons THEN InputDialogClickedButton = 1: killWindow MaxWindows
    IF ClickedButton = maxbuttons - 1 THEN InputDialogClickedButton = 2: killWindow MaxWindows
    IF ClickedButton = maxbuttons - 2 THEN InputDialogClickedButton = 3: killWindow MaxWindows
    ClickedButton = 0
    InputDialogActive = False
  END IF

  IF Copying = TRUE THEN DOCopy

END SUB

FUNCTION MouseInside (x, y, x2, y2)
'----------------------------------------------------------------------------
' Checks is mouse is inside an area (the mouse cursor (15x15),
'                                    not just the mouse pointer)
'----------------------------------------------------------------------------
'
' x,y,x2,y2 - Area
'
'----------------------------------------------------------------------------




IF hx >= x - 15 AND hx <= x2 AND hy >= y - 15 AND hy <= y2 THEN
  MouseInside = TRUE
ELSE
  MouseInside = False
END IF


END FUNCTION

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
I& = 0
IF MID$(UCASE$(Textz$), I& + 1, 2) = "/N" THEN
  x& = startX&
  y& = y& + 8
  starti& = starti& + 2
END IF

IF MID$(UCASE$(Textz$), I& + 1, 2) = "/C" THEN
  FCol% = VAL(MID$(Textz$, I& + 3, 2))
  starti& = starti& + 4
END IF

IF MID$(UCASE$(Textz$), I& + 1, 2) = "/B" THEN
  Switch Bold, 1, 0
  starti& = starti& + 2
END IF

IF MID$(UCASE$(Textz$), I& + 1, 2) = "/R" THEN
  Switch Carved, 1, 0
  starti& = starti& + 2
END IF


IF MID$(UCASE$(Textz$), I& + 1, 2) = "/U" THEN
  Switch Underline, 1, 0
  starti& = starti& + 2
END IF

FOR I& = starti& TO LEN(Textz$)
 
  IF MID$(UCASE$(Textz$), I&, 2) = "/N" THEN
    x& = startX&
    y& = y& + 8
    I& = I& + 2
  END IF

  IF MID$(UCASE$(Textz$), I&, 2) = "/C" THEN
    FCol% = VAL(MID$(Textz$, I& + 2, 2))
    I& = I& + 4
  END IF

  IF MID$(UCASE$(Textz$), I&, 2) = "/B" THEN
    Switch Bold, 1, 0
    I& = I& + 2
  END IF

  IF MID$(UCASE$(Textz$), I&, 2) = "/U" THEN
    Switch Underline, 1, 0
    I& = I& + 2
  END IF

  IF MID$(UCASE$(Textz$), I&, 2) = "/R" THEN
    Switch Carved, 1, 0
    I& = I& + 2
  END IF


  IF BCol% <> 16 THEN
    LINE (x&, y&)-(x& + FontX&, y& + FontY& - 1), BCol%, BF
  END IF

  IF MID$(Textz$, I&, 1) = CHR$(179) THEN
    LINE (x&, y&)-(x&, y& + FontY& - 1), FCol%
    GOTO DontDraw
  END IF

  IF I& > LEN(Textz$) THEN EXIT SUB
  IF MID$(Textz$, I&, 1) = " " THEN GOTO DontDraw

  IF MID$(Textz$, I&, 1) = "†" THEN MID$(Textz$, I&, 1) = CHR$(131)
  IF MID$(Textz$, I&, 1) = CHR$(140) THEN MID$(Textz$, I&, 1) = CHR$(134)
  IF MID$(Textz$, I&, 1) = "„" THEN MID$(Textz$, I&, 1) = CHR$(132)
  IF MID$(Textz$, I&, 1) = "”" THEN MID$(Textz$, I&, 1) = CHR$(133)
  IF MID$(Textz$, I&, 1) = "" THEN MID$(Textz$, I&, 1) = CHR$(128)
  IF MID$(Textz$, I&, 1) = "Ž" THEN MID$(Textz$, I&, 1) = CHR$(129)
  IF MID$(Textz$, I&, 1) = "™" THEN MID$(Textz$, I&, 1) = CHR$(130)
  IF ASC(MID$(Textz$, I&, 1)) > 138 THEN MID$(Textz$, I&, 1) = CHR$(138)


  IF (x& < ScreenXDim - 8 AND y& < ScreenYDim - FontX& AND x& >= 0 AND y& >= 0) AND (x& < ViewX2 - FontX& AND y& < ViewY2 - FontY& AND x& >= ViewX AND y& >= ViewY) AND ASC(MID$(Textz$, I&, 1)) > 32 AND Carved = 0 THEN
    IF FCol% <> 0 AND BCol% = 0 THEN
      PUT (x&, y&), Font((ASC(MID$(Textz$, I&, 1)) - 32) * 48), PSET
      IF Bold = 1 THEN PUT (x& + 1, y&), Font((ASC(MID$(Textz$, I&, 1)) - 32) * 48), PSET
    ELSE
      PUT (x&, y&), Font((ASC(MID$(Textz$, I&, 1)) - 32) * 48), AND
      IF Bold = 1 THEN PUT (x& + 1, y&), Font((ASC(MID$(Textz$, I&, 1)) - 32) * 48), AND
    END IF
  END IF

 
  IF (x& < ScreenXDim - 8 AND y& < ScreenYDim - FontX& AND x& >= 1 AND y& >= 0) AND (x& < ViewX2 - FontX& AND y& < ViewY2 - FontY& AND x& >= ViewX AND y& >= ViewY) AND ASC(MID$(Textz$, I&, 1)) > 32 AND Carved = 1 THEN
    PUT (x&, y&), Font((ASC(MID$(Textz$, I&, 1)) - 32) * 48), AND
    FCol% = 15

    FOR yy& = y& TO y& + FontY& - 1
      FOR xx& = x& TO x& + FontX& - 1
        IF POINT(xx&, yy&) = 0 THEN PSET (xx&, yy&), FCol% 'ELSE PSET (xx&, yy&), 0
      NEXT
    NEXT

    PUT (x& - 1, y&), Font((ASC(MID$(Textz$, I&, 1)) - 32) * 48), AND
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
FOR I = 1 TO MaxBMPs
  IF Bitmap(I).Win = num THEN
    LoadBMP I
  END IF
NEXT



FOR I = 1 TO MaxFrames
  IF Frame(I).Win = num THEN
    DrawFrame I
  END IF
NEXT

FOR I = 1 TO MaxPercentBars
  IF PBar(I).Win = num THEN
    DrawPercentBar I
  END IF
NEXT


FOR I = 1 TO MaxLabels
  IF Label(I).Win = num THEN
    DrawLabel I
  END IF
NEXT

FOR I = 1 TO MaxScrollBars
  IF SBar(I).Win = num THEN
    DrawScrollBar I
  END IF
NEXT

FOR I = 1 TO MaxTextFields
  IF TextF(I).Win = num THEN
    DrawTextField I
  END IF
NEXT

FOR I = 1 TO MaxCheckBoxes
  IF CheckBox(I).Win = num THEN
    DrawCheckBox I, TRUE
  END IF
NEXT

FOR I = 1 TO MaxRadioButtons
  IF Radio(I).Win = num THEN
    DrawRadio I, TRUE
  END IF
NEXT

FOR I = 1 TO maxbuttons + MaxScrollBars
  IF But(I).Win = num THEN
    DrawButton I, 0, 0
  END IF
NEXT

FOR I = 1 TO MaxTextboxes
  IF TextB(I).Win = num THEN
    DrawTextBox I, False
  END IF
NEXT

FOR I = 1 TO MaxIcons
  IF Icon(I).Win = num THEN
    LoadIco I
  END IF
NEXT

END SUB

FUNCTION ResetDSP%
' Resets the DSP
OUT BasePort% + 6, 1
FOR Count% = 1 TO 4
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

SUB Save640 (file$, startLine!)
'----------------------------------------------------------------------------
'                     Saves the entire screen
'----------------------------------------------------------------------------
'
'      File$ - Filename to save the screen to (NO EXTENSION!)
' StartLine! - Start line
'
'----------------------------------------------------------------------------

startLine! = (ScreenXDim * startLine!) \ 8

DEF SEG = &HA000

IF Screenmode = 13 THEN
  BSAVE file$ + ".INT", 0, 65535
ELSE
  OUT &H3CE, 4: OUT &H3CF, 0: BSAVE file$ + ".BLU", startLine!, 38400 - startLine!
  OUT &H3CE, 4: OUT &H3CF, 1: BSAVE file$ + ".GRN", startLine!, 38400 - startLine!
  OUT &H3CE, 4: OUT &H3CF, 2: BSAVE file$ + ".RED", startLine!, 38400 - startLine!
  OUT &H3CE, 4: OUT &H3CF, 3: BSAVE file$ + ".INT", startLine!, 38400 - startLine!
  OUT &H3CE, 4: OUT &H3CF, 0
END IF
DEF SEG

END SUB

SUB SetPAL (I&, r&, g&, b&)
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

OUT &H3C8, I&
OUT &H3C9, r&
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

SUB SpeakerState (OnOff%)
' Turns speaker on or off.
IF OnOff% THEN WriteDSP &HD1 ELSE WriteDSP &HD3
END SUB

FUNCTION SpeakerStatus%
OUT BasePort% + 4, &HD8
IF INP(BasePort% + 5) = &HFF THEN SpeakerStatus% = -1 ELSE SpeakerStatus% = 0
END FUNCTION

SUB splash
EXIT SUB
IF AccessPos = 2 THEN
        Box3D -1, -1, 639, 18, DoubleOut, 7
        FOR temp = 600 TO 5 STEP -10
        Box3D temp, 2, temp + 43, 15, DoubleOut, 7
        printF temp + 5, 4, "/u/c04Access", 0, 16
FOR tempb = 1 TO 7
        FOR tempa = 1 TO 30000
        NEXT tempa
NEXT tempb
        LINE (temp, 2)-(temp + 43, 15), 7, BF
        NEXT temp
        LINE (1, 1)-(639, 18), desktop, BF
END IF
IF AccessPos = 1 THEN
        Box3D -1, 461, 639, 479, DoubleOut, 7
        FOR temp = 600 TO 5 STEP -10
        Box3D temp, 463, temp + 43, 477, DoubleOut, 7
        printF temp + 5, 466, "/u/c00Access", 0, 16
FOR tempb = 1 TO 7
        FOR tempa = 1 TO 30000
        NEXT tempa
NEXT tempb
        LINE (temp, 463)-(temp + 43, 477), 7, BF
        NEXT temp
        LINE (1, 461)-(639, 479), desktop, BF
END IF

EXIT SUB
'----------------------------------------------------------------------------
'                      Displays the startup splash screen
'----------------------------------------------------------------------------

Hiiripiiloon

x = ScreenXDim \ 2 - 110
y = ScreenYDim \ 2 - 80
x2 = ScreenXDim \ 2 + 110
y2 = ScreenYDim \ 2 + 80

topic$ = "Welcome"
TopicCol = 15
TopicBCol = 1
Fillcol = Fill

IF Fillcol = Fill THEN
  LINE (x + 2, y + 2)-(x2 - 2, y2 - 2), Fill, BF
ELSE
  LINE (x + 2, y + 2)-(x2 - 1, y + 3), Fill, BF
  LINE (x + 2, y + 17)-(x2 - 1, y + 18), Fill, BF
  LINE (x + 2, y + 2)-(x + 2, y2 - 2), Fill, BF
  LINE (x2 - 1, y2 - 1)-(x + 1, y2 - 4), Fill, BF
  LINE (x2 - 1, y2 - 1)-(x2 - 4, y + 2), Fill, BF
END IF

LINE (x2 - 1, y2 - 1)-(x, y2 - 1), Shadow
LINE (x2 - 1, y2 - 1)-(x2 - 1, y), Shadow
LINE (x2, y2)-(x, y2), 0
LINE (x2, y2)-(x2, y), 0



LINE (x + 1, y + 1)-(x + 1, y2 - 3), Highlight
LINE (x + 1, y + 1)-(x2 - 1, y + 1), Highlight
PSET (x + 1, y2 - 2), Fill
LINE (x, y)-(x, y2), Fill
LINE (x, y)-(x2, y), Fill

LINE (x + 4, y + 20)-(x + 4, y2 - 5), 0
LINE (x + 4, y + 20)-(x2 - 5, y + 20), 0
LINE (x + 3, y + 19)-(x + 3, y2 - 5), Shadow
LINE (x + 3, y + 19)-(x2 - 5, y + 19), Shadow

LINE (x2 - 3, y2 - 3)-(x + 2, y2 - 3), Highlight
LINE (x2 - 3, y2 - 3)-(x2 - 3, y + 19), Highlight
LINE (x + 5, y + 21)-(x2 - 5, y2 - 5), Fillcol, BF

LINE (x + 3, y + 4)-(x2 - 4, y + 16), TopicBCol, BF
PSET (x, y), desktop
PSET (x2, y), desktop
PSET (x, y2), desktop
printF x + 6, y + 6, topic$, INT(TopicCol), 16

printF x + 39, y + 31, "Welcome to Windoze", Highlight, 16
printF x + 38, y + 30, "Welcome to Windoze", 0, 16

printF x + 39 + LEN("Welcome to Windows ") * FontSpace&, y + 31, Version.Major + "." + Version.Minor, 8, 16
printF x + 38 + LEN("Welcome to Windows ") * FontSpace&, y + 30, Version.Major + "." + Version.Minor, 15, 16

printF x + 39, y + 41, "   By ", Highlight, 16
printF x + 38, y + 40, "   By ", 0, 16


printF x + 39 + LEN("   By ") * FontSpace&, y + 41, "Sami Ky”stil„", 8, 16
printF x + 38 + LEN("   By ") * FontSpace&, y + 40, "Sami Ky”stil„", 15, 16


Box3D x + 12, y + 55, x2 - 12, y2 - 10, SingleIN, 16
Box3D x + 13, y + 56, x2 - 13, y2 - 11, Singleout, 16

Box3D x + 20, y + 85, x2 - 20, y2 - 30, SingleIN, 16
Box3D x + 22, y + 87, x2 - 22, y2 - 32, Singleout, 3


RESTORE Quotes
READ max

Quote = INT(RND * max) + 1

FOR I = 1 TO Quote
  READ Quote$
NEXT

printF x + 18, y + 60, "It is /c14" + Now$, 0, 16
printF x + 18, y + 70, "Programmer's quote of the day:", 0, 16

printF x + 28, y + 100, wrap$(Quote$, 26), 0, 16


Hiiriesiin

DO
  Updatemouse
  IF click = 1 THEN EXIT DO
LOOP UNTIL key$ <> ""
Hiiripiiloon

LINE (x, y)-(x2, y2), desktop, BF

Hiiriesiin
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

SUB Updatemouse
'----------------------------------------------------------------------------
'                    Reads mouse position and status
'----------------------------------------------------------------------------
  DialogClickedButton = 0
  InputDialogClickedButton = 0
  ClickedButton = 0
  key$ = INKEY$
  MouseMove = 0
  MovedBar = 0
  ClickedRadioButton = 0
  ClickedCheckBox = 0
  ClickedIcon = 0
  Hiirilue Hiiriv%, Hiirio%, Hiirik%, HiiriX%, HiiriY%
  IF Screenmode <> 13 AND Screenmode <> 7 THEN hx = HiiriX% ELSE hx = HiiriX% \ 2
  hy = HiiriY%
  hv = Hiiriv%
  ho = Hiirio%
  Hk = Hiirik%

  IF hv = 0 AND click = -1 THEN click = 1 ELSE IF hv = 0 THEN click = 0
  IF hv = 0 THEN drag = 0
  IF hv = 0 THEN MovingBar = 0
  IF hv = -1 AND click = 0 THEN click = -1

  IF hx <> OldHiiriX THEN MouseMove = 1
  IF hy <> OldHiiriY THEN MouseMove = 1
  IF hx <> OldHiiriX AND hv = -1 THEN drag = 1
  IF hy <> OldHiiriY AND hv = -1 THEN drag = 1


END SUB

SUB updateScreen
'----------------------------------------------------------------------------
'                        Redraws the screen
'----------------------------------------------------------------------------
FOR I = 1 TO MaxWindows
  IF I = 8 AND Win(I).x2 <> 0 AND Win(I).x2 <> 0 THEN GOSUB textdisplay: GOTO skip
  IF Win(I).x2 <> 0 AND Win(I).x2 <> 0 THEN
    IF I = aWin THEN Win(I).TopicCol = ActiveTopic ELSE Win(I).TopicCol = DeActiveTopic
    IF I = aWin THEN Win(I).TopicBCol = ActiveTopicB ELSE Win(I).TopicBCol = DeActiveTopicB
    IF I <> aWin THEN ReDrawWindow (I)
  END IF
skip:
NEXT

ReDrawWindow (aWin)
EXIT SUB

textdisplay:
RETURN
Position = SBar(1).Value
StartPosition = Position + (SBar(1).Value - SBOldValuE(1))
Position = StartPosition
temp = 0
DO
temp = temp + 10
CreateLabel 8, 1, 1, temp, 530, 15, Text$(Position), 0, 15, none, left, False
Position = Position + 1
LOOP UNTIL Position = StartPosition + 27

Position = 1
StartPosition = Position
SBOldValuE(1) = SBar(1).Value
CreateScrollBar 8, 1, 1, 527, 13, 15, 285, INT(MaxPosition / 27), 1
CreateScrollBar 8, 2, 0, 1, 284, 524, 15, 100, 1
RETURN
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
  x2 = Win(num).x2 - 5
  y = Win(num).y + 21
  y2 = Win(num).y2 - 5
  IF x2 >= ScreenXDim THEN x2 = ScreenXDim - 1
  IF y2 >= ScreenYDim THEN y2 = ScreenYDim - 1
  IF x <= 0 THEN x = 0
  IF y <= 0 THEN y = 0
  IF x >= ScreenXDim THEN x = ScreenXDim - 1
  IF y >= ScreenYDim THEN y = ScreenYDim - 1
  IF y = y2 THEN EXIT SUB
  IF x = x2 THEN EXIT SUB
  VIEW SCREEN (x, y)-(x2, y2)
  ViewX = x
  ViewY = y
  ViewX2 = x2
  ViewY2 = y2
ELSE
  VIEW SCREEN (0, 0)-(ScreenXDim - 1, ScreenYDim - 1)
  ViewX = 0
  ViewY = 0
  ViewX2 = ScreenXDim - 1
  ViewY2 = ScreenYDim - 1
END IF
END SUB

SUB VocVolume (Right%, left%, Getvol%)
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
   Right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (Right% + left% * 16) AND &HFF
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

IF Exist(file$) = False THEN EXIT SUB

OPEN file$ FOR BINARY AS #255

Bytes& = LOF(255)

IF Bytes& > 32000 THEN delay% = 40 ELSE delay% = 60


bytesremaining& = Bytes&
buffermax% = 100
buffer$ = SPACE$(buffermax%)

SpeakerState TRUE

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

SpeakerState False
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


IF PlayingWAV& = TRUE THEN CLOSE #WavFilehandle&: PlayingWAV& = False

Wavfile = FREEFILE
IF Exist(file$) = False THEN EXIT SUB

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
  PlayingWAV& = TRUE
  DMAPlay VARSEG(WavBuffer(0)), VARPTR(WavBuffer(0)), Length&, Freq&
ELSE
  DMAPlay VARSEG(WavBuffer(0)), VARPTR(WavBuffer(0)), Length&, Freq&
  CLOSE #Wavfile
END IF


END SUB

SUB work (num)
SELECT CASE num
CASE 2
'printF 1, 1, "Access version a1.1 setup", 6, 0
CreateWindow 1, 100, 100, 450, 350, "Access a1.1 setup", 7, False
CreateFrame 1, 4, 10, 20, 325, 50, "Hello!"
Text$ = "     Access setup is quick and simple. Just wait till you see all the features I have!!!"
CreateLabel 1, 1, 10, 20, 325, 50, Text$, 0, 16, none, left, 1
CreateButton 1, 1, 230, 140, 40, 20, "Next >"
CreateButton 1, 2, 30, 140, 40, 20, "< Back"
  CASE 3
DIM SuFile&(1 TO 50)
DIM SuFile$(1 TO 50)
DIM dir$(1 TO 50)
CreateWindow 1, 100, 100, 450, 350, "Access a1.1 setup", 7, False
CreateFrame 1, 4, 10, 20, 325, 50, "Making directories..."
Text$ = "     Just be patient.  This will be over in a second..."
CreateLabel 1, 1, 40, 20, 325, 50, Text$, 0, 16, none, left, 1
CreateLabel 1, 2, 100, 57, 325, 50, "Overall Progress", 0, 16, none, left, 1
CreatePbar 1, 1, 80, 90, 150, 20, 0, 100
CreatePbar 1, 2, 80, 120, 150, 20, 0, 100
'MKDIR "c:\access"
'MKDIR "c:\access\icons"
'MKDIR "c:\access\system"
'MKDIR "c:\access\temp"
'MKDIR "c:\access\trash"
CreateIcon 1, 1, 25, 160, SuDir$ + "floppy.ico"
CreateIcon 1, 2, 250, 160, SuDir$ + "foldero.ico"
SuFile$(1) = SuDir$ + "ACCESS.BAS": dir$(1) = "c:\access"
SuFile$(2) = SuDir$ + "WINFONT.DAT": dir$(2) = "c:\access"
SuFile$(3) = SuDir$ + "FOLDER.ICO": dir$(3) = "c:\access\icons"
SuFile$(4) = SuDir$ + "COMPUTER.ICO": dir$(4) = "c:\access\icons"
SuFile$(5) = SuDir$ + "OFOLDER.ICO": dir$(5) = "c:\access\icons"
SuFile$(6) = SuDir$ + "ERROR.ICO": dir$(6) = "c:\access\icons"
SuFile$(7) = SuDir$ + "ENVELOP.ICO": dir$(7) = "c:\access\icons"
SuFile$(8) = SuDir$ + "MAGNIFY.ICO": dir$(8) = "c:\access\icons"
SuFile$(9) = SuDir$ + "IMAGE.ICO": dir$(9) = "c:\access\icons"
SuFile$(10) = SuDir$ + "HELP.ICO": dir$(10) = "c:\access\icons"
SuFile$(11) = SuDir$ + "CD.ICO": dir$(11) = "c:\access\icons"
SuFile$(12) = SuDir$ + "FLOPPY.ICO": dir$(12) = "c:\access\icons"
SuFile$(13) = SuDir$ + "ENCRIPT.ICO": dir$(13) = "c:\access\icons"
SuFile$(14) = SuDir$ + "LOCKED.ICO": dir$(14) = "c:\access\icons"
SuFile$(15) = SuDir$ + "CALENDR.ICO": dir$(15) = "c:\access\icons"
SuFile$(16) = SuDir$ + "SOUNDST.ICO": dir$(16) = "c:\access\icons"
SuFile$(17) = SuDir$ + "VOLUME.ICO": dir$(17) = "c:\access\icons"
SuFile$(18) = SuDir$ + "CDVOL.ICO": dir$(18) = "c:\access\icons"
SuFile$(19) = SuDir$ + "ACCESS.ICO": dir$(19) = "c:\access\icons"
SuFile$(20) = SuDir$ + "MYCOMP.ICO": dir$(20) = "c:\access\icons"
SuFile$(21) = SuDir$ + "DISABLE.ICO": dir$(21) = "c:\access\icons"
SuFile$(22) = SuDir$ + "FOLDERO.ICO": dir$(22) = "c:\access\icons"
SuFile$(23) = SuDir$ + "USERPRO.ICO": dir$(23) = "c:\access\icons"
SuFile$(24) = SuDir$ + "MOUSE.ICO": dir$(24) = "c:\access\icons"
SuFile$(25) = SuDir$ + "SLEEP.ICO": dir$(25) = "c:\access\icons"
SuFile$(26) = SuDir$ + "CHECFDR.ICO": dir$(26) = "c:\access\icons"
SuFile$(27) = SuDir$ + "CONTROL.ICO": dir$(27) = "c:\access\icons"
SuFile$(28) = SuDir$ + "COPYDSK.ICO": dir$(28) = "c:\access\icons"
SuFile$(29) = SuDir$ + "DESKTOP.ICO": dir$(29) = "c:\access\icons"
SuFile$(30) = SuDir$ + "access.hlp": dir$(30) = "c:\access"
SuFile$(31) = SuDir$ + "CURSOR.ICO": dir$(31) = "c:\access\icons"
SuFile$(32) = SuDir$ + "BCURSOR.ICO": dir$(32) = "c:\access\icons"
SuFile$(33) = SuDir$ + "FIND.ICO": dir$(33) = "c:\access\icons"
SuFile$(34) = SuDir$ + "TRASH.ICO": dir$(34) = "c:\access\icons"
SuFile$(35) = SuDir$ + "NOTEBK.ICO": dir$(35) = "c:\access\icons"
SuFile$(36) = SuDir$ + "control.hlp": dir$(36) = "c:\access"
SuFile$(37) = SuDir$ + "TOOLTIP.ICO": dir$(37) = "c:\access\icons"
SuFile$(38) = SuDir$ + "COPY.ICO": dir$(38) = "c:\access\icons"
SuFile$(39) = SuDir$ + "COPYFDR.ICO": dir$(39) = "c:\access\icons"
SuFile$(40) = SuDir$ + "HARDDRV.ICO": dir$(40) = "c:\access\icons"
SuFile$(41) = SuDir$ + "QUESTION.ICO": dir$(41) = "c:\access\icons"
SuFile$(42) = SuDir$ + "GIF.ICO": dir$(42) = "c:\access\icons"
SuFile$(43) = SuDir$ + "BMP.ICO": dir$(43) = "c:\access\icons"
SuFile$(44) = SuDir$ + "THUNDER.ICO": dir$(44) = "c:\access\icons"
SuFile$(45) = SuDir$ + "CLOCK.ICO": dir$(45) = "c:\access\icons"
SuFile$(46) = SuDir$ + "FILELIST.EXE": dir$(46) = "c:\access\system"
SuFile$(47) = SuDir$ + "WIN.BAS": dir$(47) = "c:\access\system"
SuFile$(48) = SuDir$ + "AUTOLD.SCT": dir$(48) = "c:\access"
SuFile$(49) = SuDir$ + "COMMAND.HLP": dir$(49) = "c:\access"
SuFile$(50) = SuDir$ + "SETUP.BAS": dir$(50) = "c:\access"
FOR b = 1 TO 50
OPEN SuFile$(b) FOR RANDOM AS #5
SuFile&(b) = LOF(5)
max& = max& + SuFile&(b)
printF 1, 100, STR$(SuFile&(b)), 7, 0
CLOSE #5
NEXT b
FOR b = 1 TO 50
IF b > 1 THEN CreateLabel 1, 2, 100, 141, 325, 11, SuFile$(b - 1), 7, 16, none, left, 1
CreateLabel 1, 2, 100, 141, 325, 11, SuFile$(b), 0, 16, none, left, 1
BackroundCopy SuFile$(b), dir$(b)
temp = aDone
DO
DOCopy
IF b = 1 THEN aDone = CopyDone / 2 + 5
'IF b = 47 THEN aDone = aDone + (CopyDone / 180)
CreatePbar 1, 1, 80, 90, 150, 20, INT(aDone), 82
CreatePbar 1, 2, 80, 120, 150, 20, CopyDone, 100
LOOP UNTIL CopyDone = 100
aDone = temp
aDone = aDone + SuFile&(b) * 100 / max&
NEXT b
BEEP
CreatePbar 1, 1, 80, 90, 150, 20, 100, 100
part = 7
IF b > 1 THEN CreateLabel 1, 2, 100, 141, 325, 11, SuFile$(48), 7, 16, none, left, 1
    desktop = 4
    AccessTime = 1
    AccessHide = 1
    AccessPos = 1
    Skin = 0
    Skinfile$ = "c:\ryu.bmp"
    saver = 1
    Idle = 60
OPEN "c:\access\system\system.ini" FOR OUTPUT AS #2
    PRINT #2, desktop, AccessTime, AccessHide, AccessPos, Skin, saver, Idle, Skinfile$
    CLOSE #2
CreateLabel 1, 2, 100, 141, 325, 11, "Finished!", 0, 16, none, left, 1
CreateButton 1, 1, 230, 140, 40, 20, "Next >"
CASE 8
control 5
part = 9
CASE 10
CreateWindow 1, 100, 100, 450, 350, "Access a1.1 setup", 7, False
CreateFrame 1, 4, 10, 20, 325, 150, "Congratulations!"
Text$ = "     You have just finished setting up Access alpha 1.1!  Now that you have Access on your PC, let me give out my shoutouts:/n Antonio/n Jose /n Charlie /n Sonji /n Engine made mostly by Sami Kyostila, designer of Win.bas(1997)"
CreateLabel 1, 1, 10, 45, 325, 50, Text$, 0, 16, none, left, 1
CreateButton 1, 1, 130, 140, 40, 20, "< Finish >"
CASE 11
OPEN "c:\access\system\filelist.cfg" FOR OUTPUT AS #1
PRINT #1, "SortMethod = 1"
PRINT #1, "-------------------------"
PRINT #1, "All BAS files"
PRINT #1, "C:\access\temp\dir.tmp"
PRINT #1, "C:\ACCESS"
PRINT #1, "TXT"
PRINT #1, "END"
PRINT #1, "-------------------------"
PRINT #1, "END OF LISTING"
CLOSE #1
OPEN "c:\access\system\user.inf" FOR OUTPUT AS #2
    FOR num = 2 TO 8
    PRINT #2, TextText$(num)
    NEXT num
    PRINT #2, Now$
    CLOSE #2
OPEN "c:\access\program.bas" FOR OUTPUT AS #2
    PRINT #2, "END"
CLOSE #2
SLEEP
CHAIN "c:\access\access.bas"
END SELECT
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

SUB xorline (x, y, x2, y2, box%, Pixels)
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
Hiiripiiloon

StepX = (x2 - x) / Pixels
StepY = (y2 - y) / Pixels

PsetX = x
PsetY = y

IF box% = 0 THEN
  FOR I = 0 TO Pixels
    PSET (PsetX, PsetY), 15 XOR POINT(PsetX, PsetY)
    PsetX = PsetX + StepX
    PsetY = PsetY + StepY
  NEXT
ELSE
  PsetX = x + StepX
  FOR I = 0 TO Pixels - 2
    PSET (PsetX, PsetY), 15 XOR POINT(PsetX, PsetY)
    PsetX = PsetX + StepX
  NEXT

  PsetX = x
  PsetY = y
  FOR I = 0 TO Pixels - 1
    PSET (PsetX, PsetY), 15 XOR POINT(PsetX, PsetY)
    PsetY = PsetY + StepY
  NEXT

  PsetX = x
  PsetY = y2
  FOR I = 0 TO Pixels
    PSET (PsetX, PsetY), 15 XOR POINT(PsetX, PsetY)
    PsetX = PsetX + StepX
  NEXT

  PsetX = x2
  PsetY = y
  FOR I = 0 TO Pixels - 1
    PSET (PsetX, PsetY), 15 XOR POINT(PsetX, PsetY)
    PsetY = PsetY + StepY
  NEXT
END IF

Hiiriesiin
END SUB

