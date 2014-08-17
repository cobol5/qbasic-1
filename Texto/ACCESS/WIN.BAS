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
'
' Required files:          WIN2.BAS - Main source (about 147Kt/4800 lines)
'                       WINFONT.BAS - 6x8 Font file (about 6k)
'
' Programming language: Microsoft QBasic v1.1
'
'----------------------------------------------------------------------------





'----------------------------------------------------------------------------
'
'  This program has been completely written by me, Sami Ky”stil„, except
'  for the following routines, which I have modified from other peoples'
'  programs:
'
'     - DMA Wav player modified from Mike Huff's DMAPLAY.BAS
'           (added 32k+ file support)
'     - Screen saver/loader modified from Hans Lunsing's 640SAVE.BAS
'           (added Start line defining, 320x200x256 support)
'     - Mouse routines modified from Mikrobitti Magazine's article
'     - Programmer's quotes from FUN2_100.TXT collected by Harry Upmal
'     - BMP- and ICO-loader modified from James S. Baughn's Image format FAQ
'       (and associated SUBs)
'
'  Every SUB in this program is commented to help you master this engine.
'  If a SUB's description says "Called by the system", that means that
'  you don't have to worry about that one, it is handled by the engine.
'
'  Here is a list of some of the SUBS:
'  (???? is a wildcard)
'
'          Main - The engine's inmost core, handles all objects
'     DefScreen - defines screen parameters, like colors, screenmode etc.
'    Initscreen - Place your object creation calls in here
'    Create???? - These subs create objects, read the description to
'                 find out their parameters etc.
'   Disable???? - These subs temporarily disable objects
'    Enable???? - These subs re-enable disabled objects
'      Kill???? - These subs remove objects and windows
'    WAVPlayDMA - Plays WAV/VOC files in the backround
'       WavPlay - Play WAV/VOC files, use only if WAVPlayDMA doesn't work
'      GPFRuutu - Error handler screen
'         Exist - Checks if a file exists
'         Trim$ - Converts integers to strings and removes null padding
'    ????Volume - Sound Blaster volume settings
'  Hiiripiiloon - Hides the mouse cursor, so the screen won't mess up when
'                 drawing (see sub MouseInside)
'    Hiiriesiin - Shows the mouse cursor
'        Dialog - Displays a message box, or prompt the user for information
'   InputDialog - Queries user for a text string (Name, Filename, etc.)
' BackroundCopy - Copy files in the backround (see CONST statements below)
'
'  There are lots of more useful SUBS/FUNCTIONS, so please browse through the
'  SUB/FUNCTION list (press F2 to view it).
'
'----------------------------------------------------------------------------





'----------------------------------------------------------------------------
'                               Features
'----------------------------------------------------------------------------
'
' - DMA backround Wav player, which supports unlimited file lenghts
' - Windows 95 style interface
' - Support for screen modes 1, 7, 8, 9, 10, 11, 12, and 13
' - Backround copying
' - User definable colors
' - Customisable memory usage
' - An extended error handler
' - Quotes of the day
' - A real font (no CGA-ROM stuff)
'
' ...and the list goes on
'
'----------------------------------------------------------------------------





'----------------------------------------------------------------------------
'                      What is this thing anyway?
'----------------------------------------------------------------------------
'
' This program is a GUI (Graphical user interface) that you can use to write
' programs without having to make a new user interface for every program.
' It has may useful objects, for example a button, a checkbox, an text input
' field etc. The objects are similar to ones in Microsoft Windows 95, so
' users won't have trouble adjusting to them. It's great for installation
' programs (see SUB BackroundCopy).
'
'----------------------------------------------------------------------------




'----------------------------------------------------------------------------
'                      Your first Windoze program.
'----------------------------------------------------------------------------
'
' To write a Windoze program, you need to do a couple of things first:
'   First you must delete every executable statement from SUB Initscreen,
' so the objects of this demo are deleted and you get a clean table to start
' from. Then delete all user event handlers between the lines "Start of user
' code" and "End user code" in the main module. SUB Initscreen is called at
' the start of the program, so place all your object creation commands there.
' But before that, you need to make a window where you can place your
' objects, since they can't be placed on a plain desktop. So insert the
' following line to SUB Initscreen:
'
'      CreateWindow 1, 0, 0, 320, 200, "Test window", 7, True
'                   ^  ^  ^  ^    ^    ^              ^  ^
'               index  coords          topic          |  movable? True/False
'                                                     fill color
'
' This line calls SUB Createwindow, which creates and draws a window at
' (0,0) - (320, 200). The window's index number is 1 and it's topic is
' "Test window", and it is filled, with color 7 (light gray), and it is
' movable. To change any of these properties, refer to SUB CreateWindow
' for parameter descriptions. The index number is the handle of the window.
' It will be used to refer to the window when creating objects into it.
'   Now you should add an object to the window. Lets start with the button.
' Add this line to SUB Initscreen:
'
'      CreateButton 1, 1, 10, 10, 100, 40, "Exit"
'                   ^  ^  ^   ^   ^   ^    ^
'  host window handle  |  |   |   |   |    Caption
'                  index  coords (x,y,xlen, ylen)
'
' This line call SUB Createbutton, which creates and draws a button in window
' 1, with an handle (index) number of 1, placed at 10x, 10y, 100 pixels wide,
' 40 pixels high, and with the text "Exit" printed on it.
'   Now, try to run your program. It should display a light gray window in
' the upper left corner of the screen, with an "Exit" button in it. When
' you click the button, nothing happens, so lets fix it. When a button is
' clicked, the variable Clickedbutton is assigned the value of the clicked
' button's index number, in this case 1. So lets make an event handler for
' the button, so when it is clicked, the program is ended. Add these lines
' after the line "Start user code" in the main module:
'
' IF ClickedButton = 1 THEN     'Test if button 1 is clicked
'   END                         'End program
' END IF
'
'                   C O N G R A T U L A T i O N S !
'
' You've just completed your first Windoze program! It's just a small
' demonstration of Windoze's cababilities. And it's also quite boring.
' You can add more complex objects into you program, and multiple windows
' Check out SUBS Create?????? to learn how to make objects like radiobuttons,
' checkboxes, scrollbars and menus. See SUB Printf, to find out how to print
' underlined, bold, colored, etc. text.
'
'----------------------------------------------------------------------------



'----------------------------------------------------------------------------
'                        Useful variables
'----------------------------------------------------------------------------
'
'                   AWin - Which window is active
'                  AText - Active textfield
'                   Key$ - Pressed keyboard key (INKEY$)
'              MouseMove - If 1, then mouse was moved
' ScreenXDim, ScreenYDim - Screen dimensions
'          Clickedbutton - The index of the button which was clicked
'             Holdbutton - The index of the button which is being held
'                          down
'               MovedBar - The index of the scrollbar which was moved
'                  Click - If 1, then mouse was clicked
'                 Hx, Hy - Mouse coordinates
'                     Hv - Mouse left button (if -1, then being held down)
'                     Hk - Mouse center button (if -1, then being held down)
'                     Ho - Mouse right button (if -1, then being held down)
'                  Virhe - A custom error number, if <> 0, then GPFRuutu will
'                          be called. Virhekuvaus$ is the displayed error
'                          description.
'         FontX&, FontY& - Font dimensions
'             FontSpace& - Space between letters
'     ClickedRadiobutton - The index of the radiobutton which was clicked
'        ClickedCheckBox - The index of the checkbox which was clicked
'          ViewX, ViewY,
'         ViewX2, ViewY2 - Current viewport coords.
'           Menuselected - Number of selected menu item, counting from top
'             MenuActive - Number of active menu, counting from left (1..2..)
'          Version.Major - Major version (1 char string)
'          Version.Minor - Minor version (2 char string)
'                Copying - If True, then BackroundCopy is active
'               CopyDone - A percentage of the copy process
'                CopyPos - Bytes copied
'            ClickedIcon - The index of the clicked icon
'
' Most objects have variables which you can read the objects value from
' (or set it). See SUB Create???? to find out the variables for an object.
'
'----------------------------------------------------------------------------


'----------------------------------------------------------------------------
'                              Known bugs
'----------------------------------------------------------------------------
'
' - Objects (Labels for example) cannot be updated from other windows than
'   they are in. This is actually not a bug, I've disabled it, because it
'   causes screen redraw errors. I have no idea how to go about this problem.
'   It would require lots of calculation to find out where to draw and where
'   not to draw.
' - Some icons are not loaded with right colors
' - On some mouse drivers, a percent bar messes up the screen when updated.
'   To fix this, add the following line to SUB MouseInside
'   (after the last comment):
'
'      MouseInside = True: EXIT FUNCTION
'
'   This will cause lots of flickering of the mouse cursor when a PBar is
'   updated, but it should not garble the screen anymore.
' - If you abort a program (Ctrl-C) and press F4 to view the program screen,
'   sometimes it is not displayed. This is not a bug with Windoze, it is
'   caused by memory limitations. Since this program is so huge, QBasic
'   doesn't have any memory where to store the program screen.
' - Some text control codes don't work well together, insert spaces
'   between commands so they are recognized better.
'
'----------------------------------------------------------------------------



'$DYNAMIC
DEFINT A-Z
DECLARE SUB CreateBMP (WindowNum%, num%, x%, y%, Filename$)
DECLARE SUB LoadBMP (num%)
DECLARE FUNCTION GetBit% (Byte%, Bit%)
DECLARE SUB CreateIcon (WindowNum%, num%, x%, y%, Filename$)
DECLARE SUB LoadICO (num%)
DECLARE FUNCTION ReturnNibble% (Byte%, WhichOne%)
DECLARE SUB DoCopy ()
DECLARE SUB BackroundCopy (Filename$, Path$)
DECLARE SUB InputDialog (Topic$, Text$, Password%)
DECLARE SUB Dialog (Topic$, Text$, Button1$, Button2$, Button3$)
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
DECLARE SUB KillWindow (num%)
DECLARE FUNCTION Now$ ()
DECLARE SUB CenterText (Txt$)
DECLARE FUNCTION Trim$ (Number%)
DECLARE FUNCTION MouseInside% (x%, y%, x2%, y2%)
DECLARE SUB CreatePBar (WindowNum%, num%, x%, y%, XLen%, YLen%, Value%, Max%)
DECLARE SUB DrawPercentBar (num%)
DECLARE SUB Splash ()
DECLARE SUB CreateFrame (WindowNum, num, x, y, XLen, YLen, Caption$)
DECLARE SUB DrawFrame (num)
DECLARE FUNCTION Exist% (File$)
DECLARE SUB Load640 (File$, StartLine!)
DECLARE SUB Save640 (File$, StartLine!)
DECLARE SUB AddMenuItem (WindowNum, Number, Caption$)
DECLARE SUB DrawMenuItems (WindowNum, Number)
DECLARE SUB CreateMenu (WindowNum, Number, Caption$)
DECLARE SUB DrawMenu (WindowNum)
DECLARE FUNCTION Wrap$ (InputText$, Wrap)
DECLARE SUB Updatemouse ()
DECLARE SUB Main ()
DECLARE SUB SpeakerState (OnOff%)
DECLARE SUB WavPlay (File$, Speed!)
DECLARE SUB ResetSB ()
DECLARE SUB WavPlayDMA (File$, Freq&)
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
DECLARE SUB SetPAL (i&, r&, g&, b&)
DECLARE FUNCTION Error$ (Virhe)
DECLARE SUB GPFRuutu ()
DECLARE SUB CreateTextField (WindowNum, num, x, y, Length, Txt$, Passwd)
DECLARE SUB DrawTextField (num)
DECLARE SUB DrawScrollBar (num)
DECLARE SUB CreateScrollbar (WindowNum, num, Dir, x, y, XLen, YLen, Max, Value)
DECLARE SUB PrintF (x&, y&, Textz$, FCol%, BCol%)
DECLARE SUB DefScreen ()
DECLARE SUB LabelText (num, Caption$, FCol, BCol)
DECLARE SUB CreateLabel (WindowNum, num, x, y, XLen, YLen, Caption$, FCol, BCol, BorderStyle, Align, AutoWrap)
DECLARE SUB DrawLabel (num)
DECLARE SUB Box3D (x, y, x2, y2, Style, Fillcol)
DECLARE SUB UpdateScreen ()
DECLARE SUB ReDrawWindow (num)
DECLARE SUB RedDrawWindow (num)
DECLARE SUB XORLine (x, y, x2, y2, Box%, Pixels)
DECLARE SUB ViewW (num)
DECLARE SUB Initscreen ()
DECLARE SUB CreateButton (WindowNum, num, x, y, x2, y2, Caption$)
DECLARE SUB DrawButton (num, Pressed, Hollow)
DECLARE SUB CreateWindow (num, x, y, x2, y2, Topic$, Fillcol, WinDrag)
DECLARE SUB DrawWindow (num)
DECLARE SUB GetFont ()
DECLARE SUB Hiirilue (vasen%, oikea%, keski%, x%, y%)
DECLARE SUB Hiirirajat (x1%, y1%, x2%, y2%)
DECLARE SUB Hiiripiiloon ()
DECLARE SUB Hiiriesiin ()
DECLARE FUNCTION Hiiritarkista% ()
DECLARE SUB Hiiriajuri (ax%, bx%, cx%, dx%)
OPTION BASE 1
'ON ERROR GOTO Virhe


RANDOMIZE TIMER
CONST None = 0
CONST DoubleIN = 1
CONST DoubleOUT = 2
CONST SingleIN = 3
CONST SingleOUT = 4
CONST Left = 0
CONST Center = 1
CONST Right = 2
CONST True = 1
CONST False = NOT True



'----------------------------------------------------------------------------
'To minimize memory usage, decrease the maximum amount of objects allowed below
'----------------------------------------------------------------------------
' The SUBS Dialog and InputDialog use the last three button tags, the last
' Window tag and the last TextField tag, so don't use the last index numbers
' in your programs if you use either of those SUBS.
'----------------------------------------------------------------------------


CONST MaxWindows = 10
CONST MaxButtons = 40
CONST MaxLabels = 20
CONST MaxScrollBars = 20
CONST MaxTextFields = 20
CONST MaxCheckBoxes = 10
CONST MaxRadioButtons = 10
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
  Drag AS INTEGER
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
  x2 AS INTEGER
  y2 AS INTEGER
  Win AS INTEGER
  Disabled AS INTEGER
END TYPE

TYPE ScrollBarType
  Dir AS INTEGER
  x AS INTEGER
  y AS INTEGER
  x2 AS INTEGER
  y2 AS INTEGER
  Value AS SINGLE
  Max AS INTEGER
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
  Wrap AS INTEGER
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



'Window
DIM SHARED Win(MaxWindows) AS WindowType
DIM SHARED WinTopic$(MaxWindows)
DIM SHARED AWin, AText
DIM SHARED Key$



'Button
DIM SHARED But(MaxButtons + MaxScrollBars) AS ButtonType
DIM SHARED ButCaption$(MaxButtons + MaxScrollBars)



'Scrollbar
DIM SHARED SBar(MaxScrollBars) AS ScrollBarType



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
DIM SHARED MenuSelected AS INTEGER
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
DIM SHARED Version AS VersionType
DIM SHARED Shadow, Fill, Highlight
DIM SHARED Screenmode, ScreenXDim, ScreenYDim, ColorDepth, Desktop, ActiveTopic, DeActiveTopic, ActiveTopicB, DeActiveTopicB
DIM SHARED ClickedButton, HoldButton, MovedBar, MovingBar, Click
DIM SHARED Hx, Hy, Hv, Ho, Hk
DIM SHARED DragXOffset, DragYOffset, DragWin, DragXLen, DragYLen, DragLine
DIM SHARED OldHiiriY, OldHiiriX, MouseMove, Drag
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

Version.Major = "2"
Version.Minor = "00"








Virhe = 0: Virhekuvaus$ = "": QBVirhe = 0

'----------------------------------------------------------------------------
'                          Define screen parameters
'----------------------------------------------------------------------------
DefScreen



'----------------------------------------------------------------------------
'                               Read font
'----------------------------------------------------------------------------
DIM SHARED Font(48 TO 41 * 127) AS INTEGER
FontX& = 6: FontY& = 8: FontSpace& = 6
GetFont


CLS
ViewW -1
PAINT (0, 0), Desktop


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
  END
END IF
Cursor = CursorChar$ + " "
DialogActive = False
InputDialogActive = False

'----------------------------------------------------------------------------
'                         Show splash screen
'----------------------------------------------------------------------------

Hiiriesiin
Splash

'----------------------------------------------------------------------------
'                       Read objects and draw them
'----------------------------------------------------------------------------
FOR i& = 1 TO MaxWindows
  Win(i&).x = ScreenXDim + 1000
  Win(i&).y = ScreenYDim + 1000
  Win(i&).x2 = ScreenXDim + 1000
  Win(i&).y2 = ScreenYDim + 1000
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
UpdateScreen


'////////////////////////////////////////////////////////////////////////////
'----------------------------------------------------------------------------
'                             Start MAIN loop
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
  Main



  Virhe = 0
  Virhekuvaus$ = ""

  '----------------------------------------------------------------------------
  '                       Start of user code
  '----------------------------------------------------------------------------

  '----------------------------------------------------------------------------
  'Place your event handlers here (What to do when a button is pressed etc.)
  '----------------------------------------------------------------------------

  IF ClickedButton = 3 THEN
    ButCaption$(3) = "Disabled"
    ButCaption$(2) = "Enable"
    EnableButton 2
    DisableButton 3
  END IF

  IF ClickedButton = 2 THEN
    ButCaption$(3) = "Enable"
    ButCaption$(2) = "Disabled"
    DisableButton 2
    EnableButton 3
  END IF

  IF MenuSelected <> 0 AND AWin = 2 THEN Virhe = 1: Virhekuvaus$ = "User selected " + CHR$(34) + RTRIM$(Menu(2, 1, MenuSelected)) + CHR$(34) + " from the File menu in window #2"
 
  PBar(2).Value = Hy
  PBar(1).Value = Hx

  IF MovedBar = 1 THEN LabelText 3, STR$(INT(SBar(1).Value)), 0, 14
  IF MovedBar = 2 THEN LabelText 4, STR$(INT(SBar(2).Value)), 0, 14

  IF ClickedButton = 4 THEN
    CreateTextBox 3, 2, 120, 80, "Password is: " + TextText$(2)
  END IF

  IF ClickedButton = 5 THEN
    EnableTextField 1
  END IF

  IF ClickedButton = 6 THEN
    DisableTextField 1
  END IF

  IF ClickedButton = 7 THEN
    Hiiripiiloon
    SCREEN 0: CLS
    COLOR 14, 0
    PRINT " þ Exit from Windoze " + Version.Major + "." + Version.Minor + " (C) Sami Ky”stil„ 1997"
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
    COLOR 7
    PRINT " Please read the tutorial at the start of the program to learn"
    PRINT " how to make your own Windoze programs easily!"
    END
  END IF

  IF ClickedButton = 8 THEN
    WavPlayDMA TextText$(3), VAL(TextText$(4))
  END IF

  IF ClickedButton = 9 THEN
    Dialog "Example Dialog window",  _
"This is an example of the SUB Dialog, which can be used to display information or prompt the user for an action. It can have a maximum of 3 buttons, but this demo uses only one. There's also a SUB InputDialog, that can be used to prompt the user for a filename etc." _
, "OK", "", ""
  END IF


  '----------------------------------------------------------------------------
  '                        End of user code
  '----------------------------------------------------------------------------


LOOP

Virhe:
Virhe = ERR
IF Virhe = 53 THEN Virhe = 0
QBVirhe = Virhe
RESUME NEXT




'----------------------------------------------------------------------------
'                            Quote of the day
'----------------------------------------------------------------------------

Quotes:

'Number of quotes
DATA 22

'Quotes
DATA "Strange."
DATA "I've never heard of that."
DATA "It used to work."
DATA "It just needs a few minor changes..."
DATA "How could that have happened?"
DATA "It's got to be a hardware failure."
DATA "Perhaps they've changed release."
DATA "You've probably made a mistake."
DATA "You've got to have errors in your material."
DATA "But I've changed everything in that module."
DATA "Yes, it will be finished until then."
DATA "You've probably got hold of an old version."
DATA "I'm almost finished."
DATA "Of course. I just need to make these last changes."
DATA "It'll take no time at all."
DATA "It's probably because of some unfortunate coincident."
DATA "You can't test everything."
DATA "THIS can't effect THAT."
DATA "But I thought I fixed that."
DATA "It's there, it's just not tested."
DATA "It actually works, it just doesn't look that way..."
DATA "Except from the fact that it doesn't work, how do You think it seems?"

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


IF NOT Exist(Filename$) = True THEN EXIT SUB

IF INSTR(Filename$, "\") > 0 THEN
  FOR i = 1 TO LEN(Filename$)
    IF MID$(Filename$, i, 1) = "\" THEN StartPos = i
  NEXT
  Filename2$ = RIGHT$(Filename$, LEN(Filename$) - INSTR(StartPos, Filename$, "\"))
END IF

CopyFile = FREEFILE


OPEN Filename$ FOR BINARY AS #CopyFile
IF RIGHT$(Path$, 1) <> "\" THEN Path$ = Path$ + "\"

CopyFile2 = FREEFILE
OPEN Path$ + Filename2$ FOR BINARY AS #CopyFile2

SEEK #CopyFile, 1
SEEK #CopyFile2, 1


Copying = True
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

SUB CDVolume (Right%, Left%, Getvol%)
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
   Left% = INP(BasePort% + 5) \ 16
   Right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (Right% + Left% * 16) AND &HFF
END IF
END SUB

SUB CenterText (Txt$)
'----------------------------------------------------------------------------
'                     Centers Txt$ on the screen
'----------------------------------------------------------------------------

LOCATE , 40 - LEN(Txt$) \ 2
PRINT Txt$
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

DrawCheckBox num, True


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

LoadICO num


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
Label(num).Wrap = AutoWrap

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

SUB CreatePBar (WindowNum, num, x, y, XLen, YLen, Value, Max)
'----------------------------------------------------------------------------
'                   Creates a percent bar
'----------------------------------------------------------------------------
'
'  WindowNum - Host window index number
'        Num - Index of the percent bar
'       x, y - Coords.
' XLen, YLen - X Length, Y Length
'      Value - Startup value (not in percents)
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
PBar(num).Max = Max

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

DrawRadio num, True

END SUB

SUB CreateScrollbar (WindowNum, num, Dir, x, y, XLen, YLen, Max, Value)
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
'     Value - Startup value
'
'----------------------------------------------------------------------------
'     Read the scrollbar's value from the variable SBar(Num).Value
'----------------------------------------------------------------------------

IF Max < Value THEN EXIT SUB
ViewW (-1)

IF Dir = 0 AND XLen < 44 THEN XLen = 44
IF Dir = 1 AND YLen < 44 THEN YLen = 44

SBar(num).Dir = Dir
SBar(num).x = x
SBar(num).y = y
SBar(num).x2 = x + XLen
SBar(num).y2 = y + YLen
SBar(num).Max = Max
SBar(num).Value = Value
SBar(num).Win = WindowNum
SBar(num).Button = (num * 2) + MaxButtons - 1

IF Dir = 0 THEN
  SBar(num).SStep = (XLen - 43) / Max
  CreateButton WindowNum, (num * 2) + MaxButtons - 1, x, y, 14, YLen, CHR$(137)
  CreateButton WindowNum, (num * 2) + MaxButtons, x + XLen - 14, y, 14, YLen, CHR$(136)
ELSE
  SBar(num).SStep = (YLen - 43) / Max
  CreateButton WindowNum, (num * 2) + MaxButtons - 1, x, y, XLen, 14, CHR$(140)
  CreateButton WindowNum, (num * 2) + MaxButtons, x, y + YLen - 14, XLen, 14, CHR$(135)
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


IF TextBCaption$(num) <> "" THEN DrawTextBox num, True


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
TextF(num).Max = Length
TextF(num).Win = WindowNum
TextF(num).Password = Passwd
TextF(num).Disabled = False

DrawTextField num

END SUB

SUB CreateWindow (num, x, y, x2, y2, Topic$, Fillcol, WinDrag)
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
WinTopic$(num) = Topic$
Win(num).TopicCol = DeActiveTopic
Win(num).TopicBCol = DeActiveTopicB
Win(num).Fillcol = Fillcol
Win(num).Drag = WinDrag
DrawWindow (num)
END SUB

SUB DefScreen
'----------------------------------------------------------------------------
'                      Defines screen parameters
'----------------------------------------------------------------------------

Screenmode = 12                  'Screenmode to be used
ScreenXDim = 640                 'Max. X
ScreenYDim = 480                 'Max. Y
ColorDepth = 16                  'Number of colors

Desktop = 8                      'Desktop color
DragLine = 20                    'Integrity of window outline when moving
ActiveTopic = 0                  'Active topic color
DeActiveTopic = 0                'Deactive topic color
ActiveTopicB = 12                'Active topic backround color
DeActiveTopicB = 4               'Deactive topic backround color
AWin = 1                         'Active window at startup

Highlight = 15                   '3D-object highlight color
Fill = 7                         '3D-object fill color
Shadow = 8                       '3D-object shadow color

CursorChar$ = CHR$(179)          'Cursor character

SCREEN Screenmode: CLS
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

IF Button1$ = "" THEN EXIT SUB
x = ScreenXDim \ 2 - 110
y = ScreenYDim \ 2 - 80
x2 = ScreenXDim \ 2 + 110
y2 = ScreenYDim \ 2 + 80

CreateWindow MaxWindows, x, y, x2, y2, Topic$, Fill, False


DialogActive = True
DialogSelectedButton = 0
DialogAWin = AWin
AWin = MaxWindows

IF Button2$ = "" AND Button3$ = "" AND Button1$ <> "" THEN
  CreateButton MaxWindows, MaxButtons, (x2 - x) \ 2 - 30, (y2 - y) \ 2 + 30 - 10, 60, 20, Button1$
END IF

IF Button2$ <> "" AND Button3$ = "" AND Button1$ <> "" THEN
  CreateButton MaxWindows, MaxButtons, (x2 - x) \ 2 - 80, (y2 - y) \ 2 + 30 - 10, 60, 20, Button1$
  CreateButton MaxWindows, MaxButtons - 1, (x2 - x) \ 2 + 10, (y2 - y) \ 2 + 30 - 10, 60, 20, Button2$
END IF

IF Button2$ <> "" AND Button3$ <> "" AND Button1$ <> "" THEN
  CreateButton MaxWindows, MaxButtons, 10, (y2 - y) \ 2 + 30 - 10, 60, 20, Button1$
  CreateButton MaxWindows, MaxButtons - 1, (x2 - x) \ 2 - 35, (y2 - y) \ 2 + 30 - 10, 60, 20, Button2$
  CreateButton MaxWindows, MaxButtons - 2, (x2 - x) - 80, (y2 - y) \ 2 + 30 - 10, 60, 20, Button3$
END IF

Win(MaxWindows).TopicBCol = 1
Win(MaxWindows).TopicCol = 15

ReDrawWindow MaxWindows
Hiiripiiloon
PrintF x + 10, y + 40, Wrap$(Text$, 34), 0, 16
Hiiriesiin

END SUB

SUB DisableButton (num)
'----------------------------------------------------------------------------
'                        Disables a button
'----------------------------------------------------------------------------
'
'  Num - Button number
'
'----------------------------------------------------------------------------




But(num).Disabled = True
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

CheckBox(num).Disabled = True

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
  IF Radio(i).Group = Group THEN Radio(i).Disabled = True: DrawRadio i, 0
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

TextF(num).Disabled = True

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

SUB DoCopy
'----------------------------------------------------------------------------
'      Does the copying process for Backroundcopy, called by the system
'----------------------------------------------------------------------------

IF NOT Copying = True THEN EXIT SUB

FOR i = 1 TO CopyBytes
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
ViewW (WindowNum)
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
  FOR i = 1 TO LEN(Caption$)
    IF MID$(Caption$, i, 2) = "/n" THEN CapLines = CapLines + 1
  NEXT
  IF INSTR(Caption$, "/n") > 0 THEN Capline$ = LEFT$(Caption$, INSTR(Caption$, "/n")) ELSE Capline$ = Caption$

  Textpos% = ((x2 - x) \ 2) - ((LEN(Capline$) * FontX&) \ 2) + x + 1
  IF But(num).Disabled = False THEN
    PrintF INT(Textpos%), INT((y2 - y)) \ 2 + y - ((CapLines * 8) / 2) + 1, Caption$, 0, 16
  ELSE
    PrintF INT(Textpos%), INT((y2 - y)) \ 2 + y - ((CapLines * 8) / 2) + 1, Caption$, 8, 16
  END IF
END IF

Hiiriesiin
ViewW (-1)
END SUB

SUB DrawCheckBox (num, DrawText)
'----------------------------------------------------------------------------
'             Draws a checkbox, called by the system
'----------------------------------------------------------------------------



WindowNum = CheckBox(num).Win
ViewW (WindowNum)
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


IF CheckBox(num).Disabled = True THEN
  LINE (x + 3, y + 3)-(x + 8, y + 8), Fill, BF
ELSE
  LINE (x + 3, y + 3)-(x + 8, y + 8), Highlight, BF
END IF


IF State = True THEN
  LINE (x + 3, y + 3)-(x + 7, y + 7), Shadow
  LINE (x + 7, y + 3)-(x + 3, y + 7), Shadow
  LINE (x + 4, y + 3)-(x + 8, y + 7), 0
  LINE (x + 8, y + 3)-(x + 4, y + 7), 0
END IF


IF DrawText = True THEN PrintF x + 18, y + 2, Caption$, 0, 16


Hiiriesiin
ViewW (-1)
END SUB

SUB DrawFrame (num)
'----------------------------------------------------------------------------
'                 Draws a frame, called by the system
'----------------------------------------------------------------------------

WindowNum = Frame(num).Win

Hiiripiiloon
ViewW WindowNum

x = Frame(num).x + Win(WindowNum).x + 5
x2 = Frame(num).x2 + Win(WindowNum).x + 5
y = Frame(num).y + Win(WindowNum).y + 21
y2 = Frame(num).y2 + Win(WindowNum).y + 21
Caption$ = FrameCaption$(num)

Box3D x, y, x2, y2, SingleIN, 16
Box3D x + 1, y + 1, x2 - 1, y2 - 1, SingleOUT, 16
LINE (x + 2, y + 2)-(x2 - 2, y2 - 2), Fill, BF

IF Caption$ <> "" THEN
  PrintF x + (FontX&), y - 3, Caption$, 0, INT(Fill)
END IF

ViewW -1
Hiiriesiin

END SUB

SUB DrawLabel (num)
'----------------------------------------------------------------------------
'             Draws a label, called by the system
'----------------------------------------------------------------------------

WindowNum = Label(num).Win


ViewW (WindowNum)
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

IF Label(num).Wrap = 1 THEN Caption$ = Wrap$(Caption$, (x2 - x) \ FontSpace& - 2)

CapLines = 1
FOR i = 1 TO LEN(Caption$)
  IF MID$(Caption$, i, 2) = "/n" THEN CapLines = CapLines + 1
NEXT
IF INSTR(Caption$, "/n") > 0 THEN Capline$ = LEFT$(Caption$, INSTR(Caption$, "/n")) ELSE Capline$ = Caption$

IF Align = 0 THEN Textpos = x + FontSpace& \ 2
IF Align = 1 THEN
  Textpos = ((x2 - x) \ 2) - (((LEN(Capline$)) * FontX&) \ 2) + x + 1
END IF

IF Align = 2 THEN Textpos = x2 - ((LEN(Capline$))) - FontSpace& \ 2

IF FCol <> 0 AND BCol = 0 THEN
  PrintF INT(Textpos), (y2 - y) \ 2 + y - ((CapLines * FontY&) / 2), Caption$, INT(FCol), 0
ELSE
  PrintF INT(Textpos), (y2 - y) \ 2 + y - ((CapLines * FontY&) / 2), Caption$, INT(FCol), 16
END IF

Hiiriesiin
ViewW (-1)


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

FOR i& = 1 TO 8
  IF LEN(RTRIM$(MenuBar(WindowNum, i&).Caption)) <> 10 AND MenuBar(WindowNum, i&).Items > 0 THEN
    PrintF x + ((i& - 1) * 8 * 10) + 2, INT(y), MenuBar(WindowNum, i&).Caption, 0, 16
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

Box3D x, y, x2, y2, DoubleOUT, Fill

FOR i& = 1 TO MenuBar(WindowNum, Number).Items
  PrintF x + 5, y + i& * 12 - 8, Menu(WindowNum, Number, i&), 0, 16
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

IF MouseInside(x, y, x2, y2) = True THEN Hiiripiiloon
ViewW WindowNum

Value = PBar(num).Value
Max = PBar(num).Max

XPos = ((x2 - x - 4) / 100) * (Value / Max * 100)

Box3D x, y, x2, y2, DoubleIN, 16

IF XPos < 3 THEN
  LINE (x + 2, y2 - 2)-(x2 - 2, y + 2), Fill, BF
  CALL PrintF(x + (x2 - x) \ 2 - FontSpace& * 2, y + 1 + (y2 - y) \ 2 - FontY& \ 2, LTRIM$(RTRIM$(STR$(INT(Value / Max * 100)))) + "%", 0, 16)
  Hiiriesiin
  ViewW -1
  EXIT SUB
END IF


IF XPos >= (x2 - x) - 3 THEN XPos = (x2 - x) - 3





LINE (x + 2, y + 2)-(x + XPos, y + 2), 15
LINE (x + 2, y + 2)-(x + 2, y2 - 2), 15

LINE (x + 3, y + 3)-(x + XPos - 1, y + 3), 11
LINE (x + 3, y + 3)-(x + 3, y2 - 3), 11

LINE (x + 4, y + 4)-(x + XPos - 2, y2 - 4), 3, BF

LINE (x + XPos - 1, y2 - 3)-(x + XPos - 1, y + 4), 8
LINE (x + XPos - 1, y2 - 3)-(x + 4, y2 - 3), 8

LINE (x + XPos, y2 - 2)-(x + XPos, y + 3), 0
LINE (x + XPos, y2 - 2)-(x + 3, y2 - 2), 0

LINE (x + XPos + 1, y2 - 2)-(x2 - 2, y + 2), Fill, BF

CALL PrintF(x + (x2 - x) \ 2 - FontSpace& * 2, y + 1 + (y2 - y) \ 2 - FontY& \ 2, LTRIM$(RTRIM$(STR$(INT(Value / Max * 100)))) + "%", 0, 16)

IF MouseInside(x, y, x2, y2) = True THEN Hiiriesiin
ViewW -1

END SUB

SUB DrawRadio (num, DrawText)
'----------------------------------------------------------------------------
'             Draws a radio button, called by the system
'----------------------------------------------------------------------------

WindowNum = Radio(num).Win
Hiiripiiloon
ViewW (WindowNum)


x = Radio(num).x + Win(WindowNum).x + 5
y = Radio(num).y + Win(WindowNum).y + 21
Check% = Radio(num).State
Caption$ = RadioText$(num)

IF Radio(num).Disabled = True THEN
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

IF Check% = True THEN
  LINE (x + 3, y + 3)-(x + 5, y + 5), 0, BF
  PSET (x + 3, y + 3), Fill
  PSET (x + 5, y + 5), Fill
  PSET (x + 3, y + 5), Fill
  PSET (x + 5, y + 3), Fill
END IF


IF DrawText = True THEN PrintF x + 18, y + 1, Caption$, 0, 16


Hiiriesiin
ViewW -1
END SUB

SUB DrawScrollBar (num)
'----------------------------------------------------------------------------
'             Draws a scrollbar, called by the system
'----------------------------------------------------------------------------

WindowNum = SBar(num).Win
ViewW (WindowNum)
Hiiripiiloon


Dir = SBar(num).Dir

x = SBar(num).x + Win(WindowNum).x + 5
y = SBar(num).y + Win(WindowNum).y + 21
x2 = SBar(num).x2 + Win(WindowNum).x + 5
y2 = SBar(num).y2 + Win(WindowNum).y + 21
Max = SBar(num).Max
Value = SBar(num).Value
SStep! = SBar(num).SStep

IF Dir = 0 THEN
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


ViewW (-1)
Hiiriesiin

END SUB

SUB DrawTextBox (num, EraseOld)
'----------------------------------------------------------------------------
'                          Draws a checkbox
'----------------------------------------------------------------------------

Hiiripiiloon
ViewW TextB(num).Win

IF EraseOld = True THEN
  TempFill = Fill
  PrintF TextB(num).x + Win(TextB(num).Win).x + 5, TextB(num).y + Win(TextB(num).Win).y + 21, TextBCaption$(num), TempFill, TempFill
ELSE
  PrintF TextB(num).x + Win(TextB(num).Win).x + 5, TextB(num).y + Win(TextB(num).Win).y + 21, TextBCaption$(num), 0, Fill
END IF


Hiiriesiin
ViewW -1
END SUB

SUB DrawTextField (num)
'----------------------------------------------------------------------------
'             Draws a textfield, called by the system
'----------------------------------------------------------------------------

WindowNum = TextF(num).Win
ViewW (WindowNum)
Hiiripiiloon

x = TextF(num).x + Win(WindowNum).x + 5
y = TextF(num).y + Win(WindowNum).y + 21
Txt$ = TextText$(num)
Length = TextF(num).Max
Passwd = TextF(num).Password


IF TextF(num).Disabled = True THEN
  Box3D x, y, x + (Length * FontSpace&) + 19, y + 14, 1, Fill
ELSE
  Box3D x, y, x + (Length * FontSpace&) + 19, y + 14, 1, 15
END IF

IF Passwd = False THEN PrintF x + 6, y + 4, Txt$, 0, 16 ELSE PrintF x + 6, y + 4, STRING$(LEN(Txt$), "*"), 0, 16

ViewW (-1)
Hiiriesiin

END SUB

SUB DrawWindow (num)
'----------------------------------------------------------------------------
'             Draws a window, called by the system
'----------------------------------------------------------------------------


Hiiripiiloon
ViewW -1
x = Win(num).x
y = Win(num).y
x2 = Win(num).x2
y2 = Win(num).y2
Topic$ = WinTopic$(num)
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
PSET (x, y), Desktop
PSET (x2, y), Desktop
PSET (x, y2), Desktop



PrintF x + 6, y + 6, Topic$, INT(TopicCol), 16
Hiiriesiin

FOR i& = 1 TO 8
  IF LEN(RTRIM$(MenuBar(num, i&).Caption)) <> 10 THEN
    DrawMenu num
  END IF
NEXT

END SUB

FUNCTION DSPVersion!
' Gets the DSP version.
WriteDSP &HE1
Temp% = ReadDSP%
Temp2% = ReadDSP%
DSPVersion! = VAL(STR$(Temp%) + "." + STR$(Temp2%))
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

FOR i = 1 TO MaxRadioButtons
  IF Radio(i).Group = Group THEN Radio(i).Disabled = False: DrawRadio i, 0
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

FUNCTION Exist% (File$)
'----------------------------------------------------------------------------
'                       Checks if a file exists
'----------------------------------------------------------------------------
'
'    File$ - File to check
'
'----------------------------------------------------------------------------
'  Returns True if file exists, else False
'----------------------------------------------------------------------------


IF File$ = "" THEN Exist = False: EXIT FUNCTION
Checkfile = FREEFILE
OPEN File$ FOR BINARY AS #Checkfile
IF LOF(Checkfile) = 0 THEN
  Exist = False
  CLOSE #Checkfile
  KILL File$
ELSE
  Exist = True
  CLOSE #Checkfile
END IF
END FUNCTION

SUB FMVolume (Right%, Left%, Getvol%)
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
   Left% = INP(BasePort% + 5) \ 16
   Right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (Right% + Left% * 16) AND &HFF
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

SetPAL 15, 0, 0, 0

DIM pic AS STRING * 1
OPEN "C:\access\WINFONT.DAT" FOR BINARY AS #1
FOR xx& = 0 TO 318
  FOR yy& = 0 TO 16
    GET #1, , pic
    PSET (xx&, yy&), ASC(pic)
  NEXT
NEXT
DEF SEG
CLOSE #1

x = 0
y = 0
FOR i = 33 TO 138
  IF x > 312 THEN x = 0: y = y + 8
  GET (x, y)-(x + FontX& - 1, y + FontY& - 1), Font((i - 32) * 48)
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
PRINT AWin
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
PRINT " x:"; Hx, "y:"; Hy
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

CreateWindow 1, 0, 0, 290, 200, "Windoze " + Version.Major + "." + Version.Minor + " Demowindow 1/c15 (C) Sami Ky”stil„", Fill, True
CreateWindow 2, 250, 160, 580, 430, "Windoze " + Version.Major + "." + Version.Minor + " Demowindow 2/c15 (C) Sami Ky”stil„", Fill, True
CreateWindow 3, 0, 240, 320, 386, "Windoze " + Version.Major + "." + Version.Minor + " Demowindow 3/c15 (C) Sami Ky”stil„", Fill, True
CreateWindow 4, 330, 10, 600, 150, "Windoze " + Version.Major + "." + Version.Minor + " Demowindow 4/c15 (C) Sami Ky”stil„", Fill, True
CreateWindow 5, 0, 390, 240, 470, "Windoze " + Version.Major + "." + Version.Minor + "/c15 (C) Sami Ky”stil„", 1, True

CreateFrame 1, 1, 10, 10, 150, 100, "Text"
Text$ = "/c091./c00 Normal text /n /n"
Text$ = Text$ + "/c092./c00 /bBold text/b /n /n"
Text$ = Text$ + "/c093./c00 /uUnderlined text/u /n /n"
Text$ = Text$ + "/c094./c00 /c01C/c09o/c03l/c11o/c15r/c11f/c03u/c09l /c04t/c12e/c14x/c15t /n /n"
Text$ = Text$ + "/c095./c00 /rCarved text"
CreateLabel 1, 1, 20, 20, 120, 80, Text$, 0, 16, None, Left, 0

CreateFrame 1, 2, 170, 10, 100, 100, "Buttons"
CreateButton 1, 1, 180, 20, 80, 20, "Normal"
CreateButton 1, 2, 180, 50, 80, 20, "Disable"
But(2).Disabled = True
CreateButton 1, 3, 180, 80, 80, 20, "Enable"

CreateFrame 1, 3, 10, 120, 260, 50, "Percent bars"

CreateLabel 1, 2, 60, 130, 140, 9, "X                   Y", 0, 16, None, Left, 0

CreatePBar 1, 1, 20, 140, 100, 20, 0, ScreenXDim - 1
CreatePBar 1, 2, 140, 140, 100, 20, 0, ScreenYDim - 1


CreateFrame 2, 4, 10, 20, 160, 100, "Scrollbars"
CreateScrollbar 2, 1, 1, 20, 30, 16, 80, 100, 50
CreateScrollbar 2, 2, 0, 50, 30, 100, 16, 100, 25
CreateMenu 2, 1, "File"
AddMenuItem 2, 1, "Exit"
AddMenuItem 2, 1, "Quit"
AddMenuItem 2, 1, "Abort"
AddMenuItem 2, 1, "Terminate"

CreateLabel 2, 3, 50, 90, 30, 20, STR$(SBar(1).Value), 0, 14, DoubleIN, Left, 0
CreateLabel 2, 4, 120, 50, 30, 20, STR$(SBar(2).Value), 0, 14, DoubleIN, Left, 0

CreateFrame 2, 5, 180, 20, 130, 130, "Borders"
CreateLabel 2, 5, 200, 30, 90, 20, "Single in", 0, 16, SingleIN, Center, 0
CreateLabel 2, 6, 200, 60, 90, 20, "Single out", 0, 16, SingleOUT, Center, 0
CreateLabel 2, 7, 200, 90, 90, 20, "Double in", 0, 16, DoubleIN, Center, 0
CreateLabel 2, 8, 200, 120, 90, 20, "Double out", 0, 16, DoubleOUT, Center, 0

CreateFrame 2, 6, 10, 130, 160, 100, "Radiobuttons"
CreateFrame 2, 7, 20, 145, 70, 75, ""
CreateFrame 2, 8, 100, 145, 60, 75, ""
CreateRadioButton 2, 1, 1, 30, 160, "Radio", False
CreateRadioButton 2, 2, 1, 30, 180, "Radio", True
CreateRadioButton 2, 3, 1, 30, 200, "Radio", False

CreateRadioButton 2, 4, 2, 110, 160, "Yes", False
CreateRadioButton 2, 5, 2, 110, 180, "No", True
CreateRadioButton 2, 6, 2, 110, 200, "Maybe", False
Radio(6).Disabled = True

CreateFrame 2, 9, 180, 160, 130, 70, "Checkboxes"
CreateCheckBox 2, 1, 200, 170, "Checkbox 1", True
CreateCheckBox 2, 2, 200, 190, "Checkbox 2", False
CreateCheckBox 2, 3, 200, 210, "Disabled", False
CheckBox(3).Disabled = True


CreateFrame 3, 10, 10, 10, 290, 100, "Textfields"
CreateTextField 3, 1, 20, 20, 25, "Textfield #1", False
CreateTextBox 3, 1, 20, 40, "Password:"
CreateTextField 3, 2, 20, 50, 20, "Secret", True
CreateButton 3, 4, 20, 70, 80, 30, "  Show/nPassword"
CreateButton 3, 5, 200, 20, 40, 14, "Enable"
CreateButton 3, 6, 245, 20, 50, 14, "Disable"

CreateFrame 4, 11, 10, 10, 240, 94, "WAV Player"
CreateTextField 4, 3, 20, 40, 34, "", False
CreateTextBox 4, 3, 20, 30, "Enter a WAV or VOC file to play:"
CreateTextBox 4, 4, 20, 70, "Playback rate:"
CreateTextField 4, 4, 20, 80, 5, "22050", False
CreateButton 4, 8, 140, 70, 50, 20, "Play"

CreateButton 5, 7, 10, 10, 100, 36, "Exit Windoze/n  " + Version.Major + "." + Version.Minor + " demo"
CreateButton 5, 9, 120, 10, 100, 36, "About Windoze/n  " + Version.Major + "." + Version.Minor + " demo"

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
x2 = ScreenXDim \ 2 + 160
y2 = ScreenYDim \ 2 + 80

CreateWindow MaxWindows, x, y, x2, y2, Topic$, Fill, False


InputDialogActive = True
InputDialogClickedButton = 0
InputDialogAWin = AWin
AWin = MaxWindows

CreateTextField MaxWindows, MaxTextFields, 6, (y2 - y) \ 2 - 10, 47, "", Password
Win(MaxWindows).TopicBCol = 1
Win(MaxWindows).TopicCol = 15

CreateButton MaxWindows, MaxButtons, (x2 - x) \ 2 - 80, (y2 - y) \ 2 + 30 - 10, 60, 20, "OK"
CreateButton MaxWindows, MaxButtons - 1, (x2 - x) \ 2 + 10, (y2 - y) \ 2 + 30 - 10, 60, 20, "Cancel"

ReDrawWindow MaxWindows
Hiiripiiloon
PrintF x + 10, y + 40, Wrap$(Text$, 52), 0, Fill
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

IF AWin <> But(num).Win THEN
 AWin = But(num).Win
 UpdateScreen
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

IF AWin <> CheckBox(num).Win THEN
 AWin = CheckBox(num).Win
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

IF AWin <> Label(num).Win THEN
 AWin = But(num).Win
 UpdateScreen
END IF


Label(num).x = ScreenXDim + 1000
Label(num).y = ScreenYDim + 1000
Label(num).x2 = ScreenXDim + 1000
Label(num).y2 = ScreenYDim + 1000

ReDrawWindow Label(num).Win


END SUB

SUB KillPBar (num)

IF PBar(num).x = ScreenXDim + 1000 THEN EXIT SUB

IF AWin <> PBar(num).Win THEN
 AWin = PBar(num).Win
 UpdateScreen
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

IF AWin <> Radio(num).Win THEN
 AWin = Radio(num).Win
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


IF AWin <> SBar(num).Win THEN
 AWin = SBar(num).Win
 UpdateScreen
END IF

SBar(num).x = ScreenXDim + 1000
SBar(num).y = ScreenYDim + 1000
SBar(num).x2 = ScreenXDim + 1000
SBar(num).y2 = ScreenYDim + 1000



KillButton SBar(num).Button
KillButton SBar(num).Button + 1

ReDrawWindow AWin

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


IF AWin <> TextF(num).Win THEN
 AWin = TextF(num).Win
 UpdateScreen
END IF


TextF(num).x = ScreenXDim + 1000
TextF(num).y = ScreenYDim + 1000

DrawTextField num

END SUB

SUB KillWindow (num)
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

x = Win(num).x
y = Win(num).y
x2 = Win(num).x2
y2 = Win(num).y2

Hiiripiiloon
LINE (x, y)-(x2, y2), Desktop, BF
Hiiriesiin

Win(num).x = ScreenXDim + 1000
Win(num).y = ScreenYDim + 1000
Win(num).x2 = ScreenXDim + 1000
Win(num).y2 = ScreenYDim + 1000


UpdateScreen

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

SUB LineVolume (Right%, Left%, Getvol%)
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
   Left% = INP(BasePort% + 5) \ 16
   Right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (Right% + Left% * 16) AND &HFF
END IF
END SUB

SUB Load640 (File$, StartLine!)
'----------------------------------------------------------------------------
'                Loads a screen saved by Save640
'----------------------------------------------------------------------------
'
'      File$ - Filename to load the screen from (NO EXTENSION!)
' StartLine! - Start line
'
'----------------------------------------------------------------------------


StartLine! = (StartLine! * ScreenXDim) \ 8

DEF SEG = &HA000
IF Screenmode = 13 THEN
  BLOAD File$ + ".INT", 0
ELSE
  OUT &H3C4, 2: OUT &H3C5, 1: BLOAD File$ + ".BLU", StartLine!
  OUT &H3C4, 2: OUT &H3C5, 2: BLOAD File$ + ".GRN", StartLine!
  OUT &H3C4, 2: OUT &H3C5, 4: BLOAD File$ + ".RED", StartLine!
  OUT &H3C4, 2: OUT &H3C5, 8: BLOAD File$ + ".INT", StartLine!
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
ViewW Bitmap(num).Win




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
         FOR i = 1 TO 8            '...loop through the 8 bits and...
            PSET (x& + xx - 1 + i, y& + yy - 1), GetBit(Bar, i)'...extract each...
         NEXT                      '...and output them
      END IF
   NEXT                            'Loop to next column
SkipRow:
NEXT                               'Done with that row, move to next
CLOSE BMPFile                      'Done!

Hiiriesiin
ViewW -1

END SUB

SUB LoadICO (num)
'----------------------------------------------------------------------------
'              Draws an icon, called by the system
'----------------------------------------------------------------------------


xx = Icon(num).x + Win(Icon(num).Win).x + 5
yy = Icon(num).y + Win(Icon(num).Win).y + 21

IconFile = FREEFILE
Filename$ = IconFile$(num)
IF Exist(Filename$) = False THEN EXIT SUB

ViewW Icon(num).Win
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
      PSET (x& + xx, y& + yy), ReturnNibble(Bar, 1)
      PSET (x& + xx + 1, y& + yy), ReturnNibble(Bar, 2)
   NEXT
NEXT


ViewW -1
Hiiriesiin

END SUB

SUB Main
'----------------------------------------------------------------------------
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
'----------------------------------------------------------------------------
'
' This is the core of the engine, which handles all object events.
'
' If you wish to make your own objects, add the handler code here.
'
'----------------------------------------------------------------------------



 
  IF Click = 1 AND MenuSaved = True THEN
    i& = MenuNumber
      IF LEN(RTRIM$(MenuBar(AWin, i&).Caption)) <> 10 AND MenuBar(AWin, i&).Items > 0 THEN
        IF NOT (Hx > Win(AWin).x + 3 + ((i& - 1) * 8 * 10) + 2 AND Hx < Win(AWin).x + 3 + ((i&) * 8 * 10) + 2 AND Hy > Win(AWin).y + 21 + 12 AND Hy < Win(AWin).y + 21 + 12 + (MenuBar(AWin, i&).Items * 12)) THEN
          Hiiripiiloon
          Load640 "~temp", Win(AWin).y + 17
          Hiiriesiin
          MenuSaved = False
          MenuActive = False
          MenuSelected = 0
          MenuHeld = 2
        ELSE
          Hiiripiiloon
          MenuSelected = MenuBar(AWin, i&).Items - ((Win(AWin).y + 21 + 12 + (MenuBar(AWin, i&).Items * 12)) - Hy) \ 12
          PrintF Win(AWin).x + 4 + 4 + ((i& - 1) * 8 * 10) + 2, Win(AWin).y + 20 + 12 + MenuSelected * 12 - 8, Menu(AWin, MenuNumber, MenuSelected), 15, 1
          Load640 "~temp", Win(AWin).y + 17
          MenuSaved = False
          MenuActive = False
          MenuHeld = 0
          Hiiriesiin
          Click = 0
          Hv = 0
        END IF
      END IF
  END IF


  IF Hv = -1 AND MenuSaved = True THEN
    i& = MenuNumber
      IF LEN(RTRIM$(MenuBar(AWin, i&).Caption)) <> 10 AND MenuBar(AWin, i&).Items > 0 THEN
        IF (Hx > Win(AWin).x + 3 + ((i& - 1) * 8 * 10) + 2 AND Hx < Win(AWin).x + 3 + ((i&) * 8 * 10) + 2 AND Hy > Win(AWin).y + 21 + 12 AND Hy < Win(AWin).y + 21 + 12 + (MenuBar(AWin, i&).Items * 12)) THEN
          OldMenuHeld = MenuHeld
          MenuHeld = MenuBar(AWin, i&).Items - ((Win(AWin).y + 21 + 12 + (MenuBar(AWin, i&).Items * 12)) - Hy) \ 12
          IF OldMenuHeld <> MenuHeld AND OldMenuHeld <> 0 THEN
            Hiiripiiloon
            PrintF Win(AWin).x + 4 + 4 + ((i& - 1) * 8 * 10) + 2, Win(AWin).y + 20 + 12 + OldMenuHeld * 12 - 8, Menu(AWin, MenuNumber, OldMenuHeld), 0, Fill
            PrintF Win(AWin).x + 4 + 4 + ((i& - 1) * 8 * 10) + 2, Win(AWin).y + 20 + 12 + MenuHeld * 12 - 8, Menu(AWin, MenuNumber, MenuHeld), 15, 1
            Hiiriesiin
          END IF
        END IF
      END IF
  END IF


  IF Click = 1 AND MenuSaved = False THEN
    FOR i& = 1 TO 8
      IF LEN(RTRIM$(MenuBar(AWin, i&).Caption)) <> 10 AND MenuBar(AWin, i&).Items > 0 THEN
        IF Hx > Win(AWin).x + 3 + ((i& - 1) * 8 * 10) + 2 AND Hx < Win(AWin).x + 3 + ((i&) * 8 * 10) + 2 AND Hy > Win(AWin).y + 21 AND Hy < Win(AWin).y + 21 + 12 THEN
          Hiiripiiloon
          Save640 "~temp", Win(AWin).y + 17
          LINE (Win(AWin).x + 3 + ((i& - 1) * 8 * 10) + 2, Win(AWin).y + 17)-(Win(AWin).x + 3 + ((i& - 1) * 8 * 10) + 2 + (LEN(RTRIM$(MenuBar(AWin, i&).Caption)) * FontSpace&), Win(AWin).y + 21 + 10), 1, BF
          PrintF Win(AWin).x + 3 + ((i& - 1) * 8 * 10) + 2, Win(AWin).y + 21, RTRIM$(MenuBar(AWin, i&).Caption), 15, 1
          DrawMenuItems AWin, INT(i&)
          Hiiriesiin
          MenuSaved = True
          MenuActive = True
          MenuNumber = i&
        END IF
      END IF
    NEXT
  END IF

  IF MenuActive = False THEN
  IF Hv = 0 AND DragWin <> 0 THEN
    XORLine Win(DragWin).x, Win(DragWin).y, Win(DragWin).x2, Win(DragWin).y2, 1, DragLine
    UpdateScreen
    DragWin = 0
  END IF

  IF Hv = -1 AND DragWin = 0 AND Drag = 0 AND Win(AWin).Drag = True THEN
    i = AWin
    IF Win(i).x2 <> 0 AND Win(i).y2 <> 0 THEN
      IF Hx >= Win(i).x + 5 AND Hx <= Win(i).x2 - 5 AND Hy >= Win(i).y + 5 AND Hy <= Win(i).y + 16 THEN
        Hiiripiiloon
        DragXOffset = Hx - Win(i).x
        DragYOffset = Hy - Win(i).y
        DragXLen = Win(i).x2 - Win(i).x
        DragYLen = Win(i).y2 - Win(i).y
        DragWin = i
        LINE (Win(DragWin).x, Win(DragWin).y - 1)-(Win(DragWin).x2, Win(DragWin).y2), Desktop, BF
        Hiiriesiin
        XORLine Win(DragWin).x, Win(DragWin).y, Win(DragWin).x2, Win(DragWin).y2, 1, DragLine
      END IF
    END IF
  END IF

  IF DragWin <> 0 AND MouseMove = 1 THEN
    XORLine Win(DragWin).x, Win(DragWin).y, Win(DragWin).x2, Win(DragWin).y2, 1, DragLine
    Win(DragWin).x = Hx - DragXOffset
    Win(DragWin).y = Hy - DragYOffset
    Win(DragWin).x2 = Hx - DragXOffset + DragXLen
    Win(DragWin).y2 = Hy - DragYOffset + DragYLen
    XORLine Win(DragWin).x, Win(DragWin).y, Win(DragWin).x2, Win(DragWin).y2, 1, DragLine
  END IF



  IF DragWin = 0 THEN

  IF Hv = -1 THEN
    FOR i = 1 TO MaxScrollBars
      IF SBar(i).Dir = 0 AND SBar(i).Win = AWin THEN
        IF Hx > SBar(i).x + 16 + Win(SBar(i).Win).x + 5 AND Hx < SBar(i).x2 - 16 + Win(SBar(i).Win).x + 5 AND Hy > SBar(i).y + Win(SBar(i).Win).y + 21 AND Hy < SBar(i).y2 + Win(SBar(i).Win).y + 21 THEN
          SBar(i).Value = (Hx - (SBar(i).x + 16 + Win(SBar(i).Win).x + 5)) / SBar(i).SStep - (5 / SBar(i).SStep)
          IF SBar(i).Value <= 0 THEN SBar(i).Value = 0
          IF SBar(i).Value >= SBar(i).Max THEN SBar(i).Value = SBar(i).Max
          MovingBar = 1
          MovedBar = i
          DrawScrollBar i
        END IF
      END IF
      IF SBar(i).Dir = 1 AND SBar(i).Win = AWin THEN
        IF Hx > SBar(i).x + Win(SBar(i).Win).x + 5 AND Hx < SBar(i).x2 + Win(SBar(i).Win).x + 5 AND Hy > SBar(i).y + 16 + Win(SBar(i).Win).y + 21 AND Hy < SBar(i).y2 - 16 + Win(SBar(i).Win).y + 21 THEN
          SBar(i).Value = (Hy - (SBar(i).y + 16 + Win(SBar(i).Win).y + 21)) / SBar(i).SStep - (5 / SBar(i).SStep)
          IF SBar(i).Value <= 0 THEN SBar(i).Value = 0
          IF SBar(i).Value >= SBar(i).Max THEN SBar(i).Value = SBar(i).Max
          MovingBar = 1
          MovedBar = i
          DrawScrollBar i
        END IF
      END IF
    NEXT
  END IF


  IF Hv = -1 THEN
    FOR i = 1 TO MaxTextFields
      IF TextF(i).Win = AWin AND i <> AText THEN
        IF Hx > TextF(i).x + Win(AWin).x + 5 AND Hx < TextF(i).x + (TextF(i).Max * FontSpace&) + 24 + Win(AWin).x + 5 AND Hy > TextF(i).y + Win(AWin).y + 21 AND Hy < TextF(i).y + 14 + Win(AWin).y + 21 AND TextF(i).Disabled = False THEN
          Hiiripiiloon
          IF AText <> 0 THEN
            IF TextF(AText).Win = AWin AND TextF(AText).Disabled = False THEN LINE (TextF(AText).x + Win(AWin).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + Win(AWin).y + 21)-(TextF(AText).x + Win(AWin).x + 5 + 8 +  _
(LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + 8 + Win(AWin).y + 21), 15, BF
          END IF
          AText = i
          DrawTextField i
          PrintF TextF(AText).x + Win(AWin).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + Win(AWin).y + 21, CursorChar, 0, 16
          Hiiriesiin
        END IF
      END IF
    NEXT
  END IF



  IF Click = 1 AND NOT Drag = 1 THEN
    FOR i = 1 TO MaxCheckBoxes
      IF CheckBox(i).Win = AWin AND CheckBox(i).Disabled = False THEN
        IF Hx > CheckBox(i).x + Win(AWin).x + 5 AND Hx < CheckBox(i).x + (LEN(CheckBoxText$(i)) * FontSpace&) + 18 + Win(AWin).x + 5 AND Hy > CheckBox(i).y + Win(AWin).y + 21 AND Hy < CheckBox(i).y + 11 + Win(AWin).y + 21 THEN
          Switch CheckBox(i).State, True, False
          DrawCheckBox i, False
          ClickedCheckBox = i
        END IF
      END IF
    NEXT
  END IF
       

  IF Click = 1 AND NOT Drag = 1 THEN
    FOR i = 1 TO MaxRadioButtons
      IF Radio(i).Win = AWin AND Radio(i).Disabled = False THEN
        IF Hx > Radio(i).x + Win(AWin).x + 5 AND Hx < Radio(i).x + (LEN(RadioText$(i)) * FontSpace&) + 18 + Win(AWin).x + 5 AND Hy > Radio(i).y + Win(AWin).y + 21 AND Hy < Radio(i).y + 11 + Win(AWin).y + 21 THEN
          Radio(i).State = True
          FOR i2 = 1 TO MaxRadioButtons
            IF Radio(i2).Group = Radio(i).Group AND i <> i2 AND Radio(i2).Win = AWin THEN
              Radio(i2).State = False
              DrawRadio i2, False
            END IF
          NEXT

          DrawRadio i, False
          ClickedRadioButton = i
        END IF
      END IF
    NEXT
  END IF



  IF Hv = -1 AND NOT Drag = 1 AND DialogActive = False AND InputDialogActive = False THEN
    IF NOT (Hx >= Win(AWin).x AND Hx <= Win(AWin).x2 AND Hy >= Win(AWin).y AND Hy <= Win(AWin).y2) THEN
      FOR i = 1 TO MaxWindows
        IF Hx >= Win(i).x AND Hx <= Win(i).x2 AND Hy >= Win(i).y AND Hy <= Win(i).y2 THEN
          Win(AWin).TopicCol = DeActiveTopic
          Win(AWin).TopicBCol = DeActiveTopicB
          ReDrawWindow (AWin)
          AWin = i
          Win(AWin).TopicCol = ActiveTopic
          Win(AWin).TopicBCol = ActiveTopicB
          ReDrawWindow (AWin)
        END IF
      NEXT
    END IF
  END IF



  IF Hv = -1 AND HoldButton = 0 AND MovingBar = 0 THEN
    FOR i = 1 TO MaxButtons + MaxScrollBars
      IF But(i).Win = AWin THEN
        IF Hx >= But(i).x + Win(AWin).x + 5 AND Hx <= But(i).x2 + Win(AWin).x + 5 AND Hy >= But(i).y + Win(AWin).y + 21 AND Hy <= But(i).y2 + Win(AWin).y + 21 AND NOT But(i).Disabled = True THEN
         DrawButton i, 1, 1
         HoldButton = i
        END IF
      END IF
    NEXT
  END IF


  IF HoldButton <> 0 AND Hv = 0 AND MovingBar = 0 THEN
    IF Hx >= But(HoldButton).x + Win(AWin).x + 5 AND Hx <= But(HoldButton).x2 + Win(AWin).x + 5 AND Hy >= But(HoldButton).y + Win(AWin).y + 21 AND Hy <= But(HoldButton).y2 + Win(AWin).y + 21 THEN
      ClickedButton = HoldButton
      HoldButton = 0
      DrawButton ClickedButton, 0, 1
    ELSE
      DrawButton HoldButton, 0, 1
      HoldButton = 0
      ClickedButton = 0
    END IF
  END IF

  IF HoldButton > MaxButtons THEN
    FOR i = 1 TO MaxScrollBars
      IF SBar(i).Button = HoldButton THEN
        SBar(i).Value = SBar(i).Value - SBar(i).SStep
        IF SBar(i).Value <= 0 THEN SBar(i).Value = 0
        MovedBar = i
        DrawScrollBar i
      END IF
      IF SBar(i).Button + 1 = HoldButton THEN
        SBar(i).Value = SBar(i).Value + SBar(i).SStep
        IF SBar(i).Value >= SBar(i).Max THEN SBar(i).Value = SBar(i).Max
        MovedBar = i
        DrawScrollBar i
      END IF
    NEXT
  END IF


  IF HoldButton <> 0 AND Hv = -1 THEN
    IF NOT (Hx >= But(HoldButton).x + Win(AWin).x + 5 AND Hx <= But(HoldButton).x2 + Win(AWin).x + 5 AND Hy >= But(HoldButton).y + Win(AWin).y + 21 AND Hy <= But(HoldButton).y2 + Win(AWin).y + 21) THEN
      DrawButton HoldButton, 0, 1
      HoldButton = 0
    END IF
  END IF


  FOR i = 1 TO MaxPercentBars
    IF AWin = PBar(i).Win THEN
      IF PBar(i).OldValue <> PBar(i).Value THEN PBar(i).OldValue = PBar(i).Value: DrawPercentBar i
    END IF
  NEXT

  IF Click = 1 THEN
    FOR i = 1 TO MaxIcons
      IF AWin = Icon(i).Win THEN
        IF Hx >= Icon(i).x + Win(AWin).x + 5 AND Hx <= Icon(i).x + Win(AWin).x + 5 + 32 AND Hy >= Icon(i).y + Win(AWin).y + 21 AND Hy <= Icon(i).y + Win(AWin).y + 21 + 32 THEN
          ClickedIcon = i
        END IF
      END IF
    NEXT
  END IF


  IF AText <> 0 THEN
  IF TextF(AText).Disabled = False THEN
  IF Key$ <> "" AND TextF(AText).Win = AWin THEN

    IF Key$ = CHR$(0) + CHR$(15) THEN
      TextSearch = AText
      DO
        TextSearch = TextSearch - 1
        IF TextSearch < 1 THEN TextSearch = MaxTextFields
        IF TextSearch = AText THEN EXIT DO
        IF TextF(TextSearch).Win = AWin AND TextSearch <> AText AND TextF(TextSearch).Disabled = False AND TextF(TextSearch).x <> ScreenXDim + 1000 THEN
          PrintF TextF(AText).x + Win(AWin).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + Win(AWin).y + 21, "  ", 0, 15
          AText = TextSearch
          EXIT DO
        END IF
      LOOP
      Key$ = ""
    END IF


    IF LEFT$(Key$, 1) = CHR$(0) THEN Key$ = ""
    IF Key$ = CHR$(9) THEN
      TextSearch = AText
      DO
        TextSearch = TextSearch + 1
        IF TextSearch > MaxTextFields THEN TextSearch = 1
        IF TextSearch = AText THEN EXIT DO
        IF TextF(TextSearch).Win = AWin AND TextSearch <> AText AND TextF(TextSearch).Disabled = False AND TextF(TextSearch).x <> ScreenXDim + 1000 THEN
          PrintF TextF(AText).x + Win(AWin).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + Win(AWin).y + 21, "  ", 0, 15
          AText = TextSearch
          EXIT DO
        END IF
      LOOP
      Key$ = ""
    END IF

    IF Key$ = CHR$(13) THEN Key$ = ""

    IF Key$ = CHR$(8) THEN
      IF LEN(TextText$(AText)) > 0 THEN
        TextText$(AText) = LEFT$(TextText$(AText), LEN(TextText$(AText)) - 1)
        Key$ = ""
      ELSE
        Key$ = ""
      END IF
    END IF
    IF LEN(TextText$(AText)) = TextF(AText).Max THEN Key$ = ""

    TextText$(AText) = TextText$(AText) + Key$
    TextText$(AText) = LEFT$(TextText$(AText), TextF(AText).Max)

    IF TextF(AText).Password = True AND Key$ <> "" THEN Key$ = "*"

    IF MouseInside(TextF(AText).x + Win(AWin).x + 5, TextF(AText).y + Win(AWin).y + 21, TextF(AText).x + Win(AWin).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + Win(AWin).y + 21 + 14) = True THEN Hiiripiiloon
    PrintF TextF(AText).x + Win(AWin).x + 5 + (LEN(TextText$(AText)) * FontSpace&), TextF(AText).y + 4 + Win(AWin).y + 21, Key$, 0, 15
    PrintF TextF(AText).x + Win(AWin).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + Win(AWin).y + 21, Cursor, 0, 15


    IF MouseInside(TextF(AText).x + Win(AWin).x + 5, TextF(AText).y + Win(AWin).y + 21, TextF(AText).x + Win(AWin).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + Win(AWin).y + 21 + 14) = True THEN Hiiriesiin
  END IF
    IF TIMER > TextTimer! + .1 THEN
      IF Cursor = "  " THEN Cursor = CursorChar + " ": GOTO DrawCursor
      IF Cursor = CursorChar + " " THEN Cursor = "  "

DrawCursor:
      IF TextF(AText).Win = AWin AND TextF(AText).Disabled = False THEN
        IF MouseInside(TextF(AText).x + Win(AWin).x + 5, TextF(AText).y + Win(AWin).y + 21, TextF(AText).x + Win(AWin).x + 5 + ((TextF(AText).Max) * FontSpace&) + 24 + FontSpace&, TextF(AText).y + Win(AWin).y + 21 + 14) = True THEN Hiiripiiloon
        PrintF TextF(AText).x + Win(AWin).x + 5 + (LEN(TextText$(AText)) * FontSpace&) + FontSpace&, TextF(AText).y + 4 + Win(AWin).y + 21, Cursor, 0, 15
        IF MouseInside(TextF(AText).x + Win(AWin).x + 5, TextF(AText).y + Win(AWin).y + 21, TextF(AText).x + Win(AWin).x + 5 + ((TextF(AText).Max) * FontSpace&) + 24 + FontSpace&, TextF(AText).y + Win(AWin).y + 21 + 14) = True THEN Hiiriesiin
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

  OldHiiriX = Hx
  OldHiiriY = Hy
  IF Virhe <> 0 THEN GPFRuutu

  IF DialogActive = True AND ClickedButton <> 0 THEN
    AWin = DialogAWin
    IF ClickedButton = MaxButtons THEN DialogClickedButton = 1: KillWindow MaxWindows
    IF ClickedButton = MaxButtons - 1 THEN DialogClickedButton = 2: KillWindow MaxWindows
    IF ClickedButton = MaxButtons - 2 THEN DialogClickedButton = 3: KillWindow MaxWindows
    ClickedButton = 0
    DialogActive = False
  END IF

  IF InputDialogActive = True AND ClickedButton <> 0 THEN
    AWin = InputDialogAWin
    KillTextfield MaxTextFields
    IF ClickedButton = MaxButtons THEN InputDialogClickedButton = 1: KillWindow MaxWindows
    IF ClickedButton = MaxButtons - 1 THEN InputDialogClickedButton = 2: KillWindow MaxWindows
    IF ClickedButton = MaxButtons - 2 THEN InputDialogClickedButton = 3: KillWindow MaxWindows
    ClickedButton = 0
    InputDialogActive = False
  END IF

  IF Copying = True THEN DoCopy

END SUB

SUB MasterVolume (Right%, Left%, Getvol%)
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
   Left% = INP(BasePort% + 5) \ 16
   Right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (Right% + Left% * 16) AND &HFF
END IF
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

FUNCTION MouseInside (x, y, x2, y2)
'----------------------------------------------------------------------------
' Checks is mouse is inside an area (the mouse cursor (15x15),
'                                    not just the mouse pointer)
'----------------------------------------------------------------------------
'
' x,y,x2,y2 - Area
'
'----------------------------------------------------------------------------




IF Hx >= x - 15 AND Hx <= x2 AND Hy >= y - 15 AND Hy <= y2 THEN
  MouseInside = True
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

Now$ = Month$ + " " + Trim$(Day) + "," + Trim$(Year) + " - " + TIME$
END FUNCTION

SUB PrintF (x&, y&, Textz$, FCol%, BCol%)
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
StartX& = x&
Carved = 0

VIEW SCREEN (0, 0)-(ScreenXDim - 1, ScreenYDim - 1)

Starti& = 1
i& = 0
IF MID$(UCASE$(Textz$), i& + 1, 2) = "/N" THEN
  x& = StartX&
  y& = y& + 8
  Starti& = Starti& + 2
END IF

IF MID$(UCASE$(Textz$), i& + 1, 2) = "/C" THEN
  FCol% = VAL(MID$(Textz$, i& + 3, 2))
  Starti& = Starti& + 4
END IF

IF MID$(UCASE$(Textz$), i& + 1, 2) = "/B" THEN
  Switch Bold, 1, 0
  Starti& = Starti& + 2
END IF

IF MID$(UCASE$(Textz$), i& + 1, 2) = "/R" THEN
  Switch Carved, 1, 0
  Starti& = Starti& + 2
END IF


IF MID$(UCASE$(Textz$), i& + 1, 2) = "/U" THEN
  Switch Underline, 1, 0
  Starti& = Starti& + 2
END IF

FOR i& = Starti& TO LEN(Textz$)
 
  IF MID$(UCASE$(Textz$), i&, 2) = "/N" THEN
    x& = StartX&
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
    DrawCheckBox i, True
  END IF
NEXT

FOR i = 1 TO MaxRadioButtons
  IF Radio(i).Win = num THEN
    DrawRadio i, True
  END IF
NEXT

FOR i = 1 TO MaxButtons + MaxScrollBars
  IF But(i).Win = num THEN
    DrawButton i, 0, 0
  END IF
NEXT

FOR i = 1 TO MaxTextboxes
  IF TextB(i).Win = num THEN
    DrawTextBox i, False
  END IF
NEXT

FOR i = 1 TO MaxIcons
  IF Icon(i).Win = num THEN
    LoadICO i
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

SUB Save640 (File$, StartLine!)
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

IF Screenmode = 13 THEN
  BSAVE File$ + ".INT", 0, 65535
ELSE
  OUT &H3CE, 4: OUT &H3CF, 0: BSAVE File$ + ".BLU", StartLine!, 38400 - StartLine!
  OUT &H3CE, 4: OUT &H3CF, 1: BSAVE File$ + ".GRN", StartLine!, 38400 - StartLine!
  OUT &H3CE, 4: OUT &H3CF, 2: BSAVE File$ + ".RED", StartLine!, 38400 - StartLine!
  OUT &H3CE, 4: OUT &H3CF, 3: BSAVE File$ + ".INT", StartLine!, 38400 - StartLine!
  OUT &H3CE, 4: OUT &H3CF, 0
END IF
DEF SEG

END SUB

SUB SetPAL (i&, r&, g&, b&)
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

SUB Splash
'----------------------------------------------------------------------------
'                      Displays the startup splash screen
'----------------------------------------------------------------------------

Hiiripiiloon

x = ScreenXDim \ 2 - 110
y = ScreenYDim \ 2 - 80
x2 = ScreenXDim \ 2 + 110
y2 = ScreenYDim \ 2 + 80

Topic$ = "Welcome"
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
PSET (x, y), Desktop
PSET (x2, y), Desktop
PSET (x, y2), Desktop
PrintF x + 6, y + 6, Topic$, INT(TopicCol), 16

PrintF x + 39, y + 31, "Welcome to Windoze", Highlight, 16
PrintF x + 38, y + 30, "Welcome to Windoze", 0, 16

PrintF x + 39 + LEN("Welcome to Windows ") * FontSpace&, y + 31, Version.Major + "." + Version.Minor, 8, 16
PrintF x + 38 + LEN("Welcome to Windows ") * FontSpace&, y + 30, Version.Major + "." + Version.Minor, 15, 16

PrintF x + 39, y + 41, "   By ", Highlight, 16
PrintF x + 38, y + 40, "   By ", 0, 16


PrintF x + 39 + LEN("   By ") * FontSpace&, y + 41, "Sami Ky”stil„", 8, 16
PrintF x + 38 + LEN("   By ") * FontSpace&, y + 40, "Sami Ky”stil„", 15, 16


Box3D x + 12, y + 55, x2 - 12, y2 - 10, SingleIN, 16
Box3D x + 13, y + 56, x2 - 13, y2 - 11, SingleOUT, 16

Box3D x + 20, y + 85, x2 - 20, y2 - 30, SingleIN, 16
Box3D x + 22, y + 87, x2 - 22, y2 - 32, SingleOUT, 3


RESTORE Quotes
READ Max

Quote = INT(RND * Max) + 1

FOR i = 1 TO Quote
  READ Quote$
NEXT

PrintF x + 18, y + 60, "It is /c14" + Now$, 0, 16
PrintF x + 18, y + 70, "Programmer's quote of the day:", 0, 16

PrintF x + 28, y + 100, Wrap$(Quote$, 26), 0, 16


Hiiriesiin

DO
  Updatemouse
  IF Click = 1 THEN EXIT DO
LOOP UNTIL Key$ <> ""
Hiiripiiloon

LINE (x, y)-(x2, y2), Desktop, BF

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

SUB Tooltips
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
  Key$ = INKEY$
  MouseMove = 0
  MovedBar = 0
  ClickedRadioButton = 0
  ClickedCheckBox = 0
  ClickedIcon = 0
  Hiirilue Hiiriv%, Hiirio%, Hiirik%, HiiriX%, HiiriY%
  IF Screenmode <> 13 AND Screenmode <> 7 THEN Hx = HiiriX% ELSE Hx = HiiriX% \ 2
  Hy = HiiriY%
  Hv = Hiiriv%
  Ho = Hiirio%
  Hk = Hiirik%

  IF Hv = 0 AND Click = -1 THEN Click = 1 ELSE IF Hv = 0 THEN Click = 0
  IF Hv = 0 THEN Drag = 0
  IF Hv = 0 THEN MovingBar = 0
  IF Hv = -1 AND Click = 0 THEN Click = -1

  IF Hx <> OldHiiriX THEN MouseMove = 1
  IF Hy <> OldHiiriY THEN MouseMove = 1
  IF Hx <> OldHiiriX AND Hv = -1 THEN Drag = 1
  IF Hy <> OldHiiriY AND Hv = -1 THEN Drag = 1


END SUB

SUB UpdateScreen
'----------------------------------------------------------------------------
'                        Redraws the screen
'----------------------------------------------------------------------------

FOR i = 1 TO MaxWindows
  IF Win(i).x2 <> 0 AND Win(i).x2 <> 0 THEN
    IF i = AWin THEN Win(i).TopicCol = ActiveTopic ELSE Win(i).TopicCol = DeActiveTopic
    IF i = AWin THEN Win(i).TopicBCol = ActiveTopicB ELSE Win(i).TopicBCol = DeActiveTopicB
    IF i <> AWin THEN ReDrawWindow (i)
  END IF
NEXT

ReDrawWindow (AWin)

END SUB

SUB ViewW (num)
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

SUB VocVolume (Right%, Left%, Getvol%)
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
   Left% = INP(BasePort% + 5) \ 16
   Right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (Right% + Left% * 16) AND &HFF
END IF
END SUB

SUB WavPlay (File$, Speed!)
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

IF Exist(File$) = False THEN EXIT SUB

OPEN File$ FOR BINARY AS #255

Bytes& = LOF(255)

IF Bytes& > 32000 THEN delay% = 40 ELSE delay% = 60


bytesremaining& = Bytes&
buffermax% = 100
buffer$ = SPACE$(buffermax%)

SpeakerState True

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

SUB WavPlayDMA (File$, Freq&)
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


IF PlayingWAV& = True THEN CLOSE #WavFilehandle&: PlayingWAV& = False

Wavfile = FREEFILE
IF Exist(File$) = False THEN EXIT SUB

OPEN File$ FOR BINARY AS #Wavfile


IF RIGHT$(UCASE$(File$), 3) = "WAV" THEN
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
  PlayingWAV& = True
  DMAPlay VARSEG(WavBuffer(0)), VARPTR(WavBuffer(0)), Length&, Freq&
ELSE
  DMAPlay VARSEG(WavBuffer(0)), VARPTR(WavBuffer(0)), Length&, Freq&
  CLOSE #Wavfile
END IF


END SUB

FUNCTION Wrap$ (InputText$, Wrap)

'----------------------------------------------------------------------------
'                 Wraps long strings at position Wrap
'----------------------------------------------------------------------------

IF InputText$ = "" THEN EXIT FUNCTION
IF Wrap <= 1 THEN EXIT FUNCTION
IF LEN(InputText$) <= Wrap THEN Wrap$ = InputText$: EXIT FUNCTION
TPos = 0
VPos = TPos
last = 0
Line$ = ""

DO
  TPos = TPos + 1
  VPos = VPos + 1
  IF TPos > LEN(InputText$) THEN EXIT DO
  IF VPos > Wrap THEN
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

Wrap$ = InputText$

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

SUB XORLine (x, y, x2, y2, Box%, Pixels)
'----------------------------------------------------------------------------
' Draws a line or a rectangle without erasing the backround
'----------------------------------------------------------------------------
'
'                 x, y - Start point
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

IF Box% = 0 THEN
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

  PsetX = x2
  PsetY = y
  FOR i = 0 TO Pixels - 1
    PSET (PsetX, PsetY), 15 XOR POINT(PsetX, PsetY)
    PsetY = PsetY + StepY
  NEXT
END IF

Hiiriesiin
END SUB

