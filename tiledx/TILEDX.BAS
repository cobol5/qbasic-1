DECLARE SUB Palette.Save.Load ()
'Filename: TiledX.BAS
'Dimension SOFT August 13, 1998
'Programmed by: Chris Rennie
'Quality Assurance by: Jay McCarthy
'-----------------------------------------------------------------------------
'
'Before diving right into the program understand that this does not work in
'QBasic 4.5 due to the Absolute Statement. A revised version will enable it to
'work when released. Read the text document before emailing me with any prob-
'lems. Email me with any bugs you encounter and I'll get it fixed by the next
'release.                              
'
'Change pathname$ to where the files are located, then press F5 to start.
'
'
'
'
'
'
'
DECLARE SUB Icon.Routine ()
DECLARE SUB Print.Help ()
DECLARE SUB BMP ()
DECLARE SUB Icon.Normal ()
DECLARE SUB Icon.Pressed ()
DECLARE FUNCTION MouseInit% ()
DECLARE SUB mousehide ()
DECLARE SUB mouseshow ()
DECLARE SUB mousestatus (lb%, rb%, xmouse%, ymouse%)
DECLARE SUB MouseDriver (AX%, bx%, CX%, DX%)
DECLARE SUB Help ()
DECLARE SUB Filename.Text ()
DECLARE SUB Mirror.Up.Down ()
DECLARE SUB Mirror.Left.Right ()
DECLARE SUB Save.Load.Text1 ()
DECLARE SUB Save.Load.Text2 ()
DECLARE SUB Update ()
DECLARE SUB Text.Line3 ()
DECLARE SUB Text.Line4 ()
DECLARE SUB Erase.Windows ()
DECLARE SUB Get.Input ()
DECLARE SUB Text.Line1 ()
DECLARE SUB Text.Line2 ()
DECLARE SUB Mode ()
DECLARE SUB Coordinates ()
DECLARE SUB Screen.Redraw ()
DECLARE SUB Change.Color.Sub ()
DECLARE SUB Exit.Tiled ()
DECLARE SUB Grid.Movement ()
DECLARE SUB TEXT ()
DECLARE SUB Screen.Setup ()
DECLARE SUB Rotate.Right ()
DECLARE SUB Rotate.Left ()

DIM SHARED mouse$
DIM SHARED tiled%(1600)
DIM SHARED tile%(1600)
DIM SHARED Background%(1600)
DIM SHARED foreground%(1600)
DIM SHARED icons%(900)
DIM SHARED letter(42)
DIM SHARED name$(150)
DIM SHARED windowtext(350)
DIM SHARED combinedtile%(1600)
DIM SHARED pal(16)
DIM SHARED temp(16)

COMMON SHARED letter.place.x, letter.place.y, graphic.text$, Color$, keyword$
COMMON SHARED x, y, x1, y1, xcor, ycor, xlower, ylower, xupper, yupper, blank$
COMMON SHARED draw.mode$, key$, tcby, Exit$, temp.mode$, i%, help.option$
COMMON SHARED pathname$, tcolour, a%, x.position, y.position, minimum, max.length
COMMON SHARED save.load$, graphic.name$, memory.name$, text.length, temp.tile%
COMMON SHARED save.or.load$, icon.number, mode.route, data$, help.bypass$, cby
COMMON SHARED oxmouse%, oymouse%, icon.x, icon.y, pal.color, limit$, c%

'*****************************************************************************
pathname$ = "c:\windows\desktop\tiledx\"
'*****************************************************************************


RESTORE
mouse$ = SPACE$(57)
FOR z% = 1 TO 57
  READ a$
  H$ = CHR$(VAL("&H" + a$))
  MID$(mouse$, z%, 1) = H$
NEXT z%
DATA 55,89,E5,8B,5E,0C,8B,07,50,8B,5E,0A,8B,07,50,8B
DATA 5E,08,8B,0F,8B,5E,06,8B,17,5B,58,1E,07,CD,33,53
DATA 8B,5E,0C,89,07,58,8B,5E,0A,89,07,8B,5E,08,89,0F
DATA 8B,5E,06,89,17,5D,CA,08,00

'data$ = pathname$ + "dimsoft.bmp"
'BMP
'LINE (0, 0)-(640, 480), 15, BF
'data$ = pathname$ + "tiledx.bmp"
'BMP

Screen.Setup

FOR a% = 0 TO 1600
   tile%(a%) = 16
NEXT a%

x = 29
y = 28
x1 = 324
y1 = 357
xcor = 1
ycor = 1
xlower = 29
ylower = 28
xupper = 340
yupper = 339
xup = 348
yup = 348
xlow = 21
ylow = 20
draw.mode$ = "penup"
cby = 35
a% = 0
filename$ = "untitled"
blank$ = "                                                          "
save.or.load$ = "save"
icon.number = 1
c% = 0

Erase.Windows

LINE (352, cby)-(370, cby + 18), 0, B
LINE (x, y)-(x + 8, y + 8), 0, B

graphic.text$ = filename$: Filename.Text
graphic.text$ = "Use the number pad or the arrow keys to move the cursor   "
Text.Line1
graphic.text$ = "around the grid. Press space to start drawing.            "
Text.Line2

Icon.Normal
icon.number = 1
Icon.Pressed

'*****************************************************************************
'                           Keyboard Main Module
'*****************************************************************************

Pen.Up:
LINE (x, y)-(x + 8, y + 8), 0, B
graphic.text$ = "pen up               "
Mode
Coordinates
Grid.Movement
GOTO mode.route

Pen.Down:
LINE (x, y)-(x + 8, y + 8), 0, BF
IF pal(c%) <> 16 THEN
  LINE (x + 1, y + 1)-(x + 7, y + 7), pal(c%), BF
  PSET (x1, y1), pal(c%)
  GOTO Color.16.Bypass
  ELSE LINE (x + 1, y + 1)-(x + 7, y + 7), 0, BF
    LINE (x + 2, y + 2)-(x + 6, y + 6), 15
    LINE (x + 2, y + 6)-(x + 6, y + 2), 15
    PSET (x1, y1), 0
    GOTO Color.16.Bypass
END IF
Color.16.Bypass:
tile%(a%) = pal(c%)
graphic.text$ = "pen down             "
Mode
Coordinates
Grid.Movement
GOTO mode.route

Single.Pixel:
LINE (x, y)-(x + 8, y + 8), 0, B
graphic.text$ = "single pixel         "
IF key$ = CHR$(32) THEN
  LINE (x + 1, y + 1)-(x + 7, y + 7), pal(c%), BF
  tile%(a%) = colour
  PSET (x1, y1), colour
END IF
Mode
Coordinates
Grid.Movement
GOTO mode.route

mode.route:
IF temp.mode$ = "changecolor" THEN GOTO Change.Color
IF temp.mode$ = "switchcolor" THEN GOTO Switch.Color
IF temp.mode$ = "paintbackground" THEN GOTO Paint.Background
IF temp.mode$ = "paintscreen" THEN GOTO Paint.Screen
IF temp.mode$ = "converge" THEN GOTO Converge
IF temp.mode$ = "newfile" THEN GOTO New.File
IF temp.mode$ = "savefile" THEN GOTO Save.File
IF temp.mode$ = "loadfile" THEN GOTO Load.File
IF temp.mode$ = "left/right" THEN GOTO Left.Right
IF temp.mode$ = "up/down" THEN GOTO Up.Down
IF temp.mode$ = "rotate.left" THEN GOTO rrotate.left
IF temp.mode$ = "rotate.right" THEN GOTO rrotate.right
IF temp.mode$ = "save.palette" THEN Palette.Save.Load
IF temp.mode$ = "load.palette" THEN Palette.Save.Load
IF draw.mode$ = "penup" THEN GOTO Pen.Up
IF draw.mode$ = "pendown" THEN GOTO Pen.Down
IF draw.mode$ = "singlepixel" THEN GOTO Single.Pixel
IF draw.mode$ = "mouse" THEN GOTO Mouse.Module

Change.Color:
Icon.Normal
icon.number = 4
Icon.Pressed
graphic.text$ = "Use the number pad or arrow keys to change the color.     "
Text.Line1
graphic.text$ = "Press space to select the new color.                      "
Text.Line2
tcby = cby
temp% = c%
graphic.text$ = "change color         "
Mode
Change.Color.Sub
IF Exit$ = "yes" THEN
  LINE (352, tcby)-(370, tcby + 18), 15, B
  LINE (352, cby)-(370, cby + 18), 0, B
  c% = temp%
  temp.mode$ = ""
  Exit$ = ""
  Update
  GOTO mode.route
  ELSE cby = tcby
    temp.mode$ = ""
    Update
    GOTO mode.route
END IF

Switch.Color:
Icon.Normal
icon.number = 5
Icon.Pressed
graphic.text$ = "Use the number pad or arrow keys to change the color.     "
Text.Line1
graphic.text$ = "Press space to select the color to be changed.            "
Text.Line2
tcby = cby
temp% = c%
graphic.text$ = "switch color         "
Mode
Change.Color.Sub
IF Exit$ = "yes" THEN
  LINE (352, tcby)-(370, tcby + 18), 15, B
  LINE (352, cby)-(370, cby + 18), 0, B
  c% = temp%
  temp.mode$ = ""
  Exit$ = ""
  Update
  GOTO mode.route
  ELSE old.color = pal(c%)
    GOTO new.color
END IF
new.color:
graphic.text$ = "Use the number pad or arrow keys to change the color.     "
Text.Line1
graphic.text$ = "Press space to select the new color.                      "
Text.Line2
Change.Color.Sub
IF Exit$ = "yes" THEN
  LINE (352, tcby)-(370, tcby + 18), 15, B
  LINE (352, cby)-(370, cby + 18), 0, B
  c% = temp%
  temp.mode$ = ""
  Exit$ = ""
  Update
  GOTO mode.route
  ELSE new.color = pal(c%)
    GOTO Switch.Color.Routine
END IF

Switch.Color.Routine:

old.a% = a%

FOR a% = 0 TO 1600
   IF tile%(a%) = old.color THEN tile%(a%) = new.color
NEXT a%

a% = 0
FOR colory = 29 TO 341 STEP 8
  FOR colorx = 30 TO 342 STEP 8
     IF tile%(a%) = 16 THEN LINE (colorx, colory)-(colorx + 6, colory + 6), 0, BF: LINE (colorx + 1, colory + 1)-(colorx + 5, colory + 5), 15: LINE (colorx + 1, colory + 5)-(colorx + 5, colory + 1), 15
     IF tile%(a%) <> 16 THEN LINE (colorx, colory)-(colorx + 6, colory + 6), tile%(a%), BF
     a% = a% + 1
  NEXT colorx
NEXT colory

a% = 0
FOR boxy = 357 TO 396
  FOR boxx = 324 TO 363
     IF tile%(a%) = 16 THEN PSET (boxx, boxy), 0
     IF tile%(a%) <> 16 THEN PSET (boxx, boxy), tile%(a%)
     a% = a% + 1
  NEXT boxx
NEXT boxy

a% = old.a%
temp.mode$ = ""
LINE (352, tcby)-(370, tcby + 18), 15, B
LINE (352, cby)-(370, cby + 18), 0, B
c% = temp%
Update
GOTO mode.route

Paint.Background:
Icon.Normal
icon.number = 6
Icon.Pressed
graphic.text$ = "Use the number pad or arrow keys to change the color.     "
Text.Line1
graphic.text$ = "Press space to select the background color.               "
Text.Line2
tcby = cby
temp% = c%
graphic.text$ = "paint background     "
Mode
Change.Color.Sub
IF Exit$ = "yes" THEN
  LINE (352, tcby)-(370, tcby + 18), 15, B
  LINE (352, cby)-(370, cby + 18), 0, B
  c% = temp%
  temp.mode$ = ""
  Update
  Exit$ = ""
  GOTO mode.route
  ELSE background.color = pal(c%)
    GOTO Background.Color.Routine
END IF

Background.Color.Routine:

old.a% = a%

FOR a% = 0 TO 1600
   IF tile%(a%) = 16 THEN tile%(a%) = background.color
NEXT a%

a% = 0
FOR colory = 29 TO 341 STEP 8
  FOR colorx = 30 TO 342 STEP 8
     IF tile%(a%) = 16 THEN LINE (colorx, colory)-(colorx + 6, colory + 6), 0, BF: LINE (colorx + 1, colory + 1)-(colorx + 5, colory + 5), 15: LINE (colorx + 1, colory + 5)-(colorx + 5, colory + 1), 15
     IF tile%(a%) <> 16 THEN LINE (colorx, colory)-(colorx + 6, colory + 6), tile%(a%), BF
     a% = a% + 1
  NEXT colorx
NEXT colory

a% = 0
FOR boxy = 357 TO 396
  FOR boxx = 324 TO 363
     IF tile%(a%) = 16 THEN PSET (boxx, boxy), 0
     IF tile%(a%) <> 16 THEN PSET (boxx, boxy), tile%(a%)
     a% = a% + 1
  NEXT boxx
NEXT boxy

a% = old.a%
temp.mode$ = ""
c% = temp%
LINE (352, tcby)-(370, tcby + 18), 15, B
LINE (352, cby)-(370, cby + 18), 0, B
Update
GOTO mode.route

Paint.Screen:
Icon.Normal
icon.number = 7
Icon.Pressed
graphic.text$ = "Use the number pad or arrow keys to change the color.     "
Text.Line1
graphic.text$ = "Press space to paint the grid with the color selected.    "
Text.Line2
tcby = cby
temp% = c%
graphic.text$ = "paint screen         "
Mode
Change.Color.Sub
IF Exit$ = "yes" THEN
  LINE (352, tcby)-(370, tcby + 18), 15, B
  LINE (352, cby)-(370, cby + 18), 0, B
  c% = temp%
  temp.mode$ = ""
  Update
  Exit$ = ""
  GOTO mode.route
  ELSE screen.color = pal(c%)
    GOTO Screen.Color.Routine
END IF

Screen.Color.Routine:
old.a% = a%
FOR a% = 0 TO 1600
   tile%(a%) = screen.color
NEXT a%
a% = old.a%

Screen.Redraw

temp.mode$ = ""
draw.mode$ = "penup"
c% = temp%
LINE (352, tcby)-(370, tcby + 18), 15, B
LINE (352, cby)-(370, cby + 18), 0, B
Update
GOTO mode.route

Converge:
Icon.Normal
icon.number = 12
Icon.Pressed
graphic.text$ = "converge"
Mode

Get.Foreground.Tile:
graphic.text$ = "Type in the location of the Foreground tile without the   "
Text.Line1
graphic.text$ = "extension. Press enter when finished.                     "
Text.Line2
x.position = 430: y.position = 419: minimum = 430: max.length = 32
Get.Input

IF Exit$ = "yes" THEN GOTO Exit.Converge

foreground.memory.name$ = memory.name$
foreground.graphic.name$ = graphic.name$

DEF SEG = VARSEG(foreground%(0))
BLOAD foreground.graphic.name$, 0
DEF SEG

fg = 385: PUT (fg, 371), foreground%

graphic.text$ = "Is this the correct tile?                                 "
Text.Line1
graphic.text$ = "Press Y for Yes or N for No                               "
Text.Line2

Yes.or.No.1:
DO: key$ = INKEY$: LOOP WHILE key$ = ""
key$ = LCASE$(key$)
IF key$ = CHR$(27) THEN GOTO Exit.Converge
IF key$ = "y" THEN
   GOTO Get.Background.Tile
   ELSEIF key$ = "n" THEN
      LINE (355, 360)-(394, 399), 0, BF
      GOTO Get.Foreground.Tile
      ELSE GOTO Yes.or.No.1
END IF

Get.Background.Tile:
graphic.text$ = "Type in the location of the background tile               "
Text.Line1
graphic.text$ = "without the extension. Press enter when finished.         "
Text.Line2

x.position = 430: y.position = 437
Get.Input
IF Exit$ = "yes" THEN GOTO Exit.Converge
background.memory.name$ = memory.name$: background.picture.name$ = graphic.name$

DEF SEG = VARSEG(Background%(0))
BLOAD background.picture.name$, 0
DEF SEG

bg = 580: PUT (bg, 371), Background%

graphic.text$ = "Is this the correct tile?                                 "
Text.Line1
graphic.text$ = "Press Y for Yes or N for No                               "
Text.Line2

Yes.or.No.2:
DO: key$ = INKEY$: LOOP WHILE key$ = ""
key$ = LCASE$(key$)
IF key$ = CHR$(27) THEN GOTO Exit.Converge
IF key$ = "y" THEN
   GOTO Animation
   ELSEIF key$ = "n" THEN
      LINE (565, 360)-(604, 399), 0, BF
      GOTO Get.Background.Tile
      ELSE GOTO Yes.or.No.2
END IF

Animation:

graphic.text$ = "Coverging the foreground and background tiles.            "
Text.Line1
graphic.text$ = blank$
Text.Line2

DO
   LINE (bg + 34, 371)-(bg + 39, 410), 7, BF
   LINE (fg, 371)-(fg + 5, 410), 7, BF
   bg = bg - 5: fg = fg + 5
   PUT (bg, 371), Background%, PSET
   PUT (fg, 371), foreground%, PSET
   FOR a = 1 TO 1000: NEXT a
LOOP UNTIL fg = 460
LINE (fg, 370)-(bg + 40, 410), 7, BF
PUT (bg - 2, 371), Background%, PSET
PUT (fg + 3, 371), foreground%, PSET

pixel.number1% = 0
OPEN background.memory.name$ FOR INPUT AS #1
   FOR i% = 0 TO 1600
      INPUT #1, tiled%(i%)
      Background%(pixel.number1%) = tiled%(i%)
      pixel.number1% = pixel.number1% + 1
   NEXT i%
CLOSE #1

pixel.number2% = 0
OPEN foreground.memory.name$ FOR INPUT AS #1
   FOR i% = 0 TO 1600
      INPUT #1, tiled%(i%)
      foreground%(pixel.number2%) = tiled%(i%)
      pixel.number2% = pixel.number2% + 1
   NEXT i%
CLOSE #1
LINE (bg - 2, 371)-(bg + 37, 410), 7, BF
LINE (fg + 3, 371)-(fg + 42, 410), 7, BF

pixel.number1% = 0
pixel.number3% = 0
FOR pixel.number2% = 0 TO 1600
   IF foreground%(pixel.number2%) = 16 THEN combinedtile%(pixel.number3%) = Background%(pixel.number1%)
   IF foreground%(pixel.number2%) <> 16 THEN combinedtile%(pixel.number3%) = foreground%(pixel.number2%)
   pixel.number1% = pixel.number1% + 1
   pixel.number3% = pixel.number3% + 1
NEXT pixel.number2%
pixel.number3% = 0

graphic.text$ = "Tile is coverged. Type in the Filename for the tile in the"
Text.Line1
graphic.text$ = "Save\Load window.                                         "
Text.Line2
graphic.text$ = "Type in the Filename of the tile."
Save.Load.Text1
graphic.text$ = "press enter when finished.       "
Save.Load.Text2

FOR pixel.y = 371 TO 410
   FOR pixel.x = 483 TO 522
      PSET (pixel.x, pixel.y), combinedtile%(pixel.number3%)
      pixel.number3% = pixel.number3% + 1
   NEXT pixel.x
NEXT pixel.y

GET (483, 371)-(522, 410), tiled%
PUT (588, 50), tiled%, PSET
x.position = 432: y.position = 55: minimum = 432: max.length = 24: Get.Input
IF Exit$ = "yes" THEN Erase.Windows: GOTO Exit.Converge
converged.memory.name$ = memory.name$: converged.graphic.name$ = graphic.name$

pixel.number3% = 0
FOR i% = 0 TO 1600
  tiled%(i%) = combinedtile%(pixel.number3%)
  pixel.number3% = pixel.number3% + 1
NEXT i%

OPEN converged.memory.name$ FOR OUTPUT AS #1
   FOR i% = 0 TO 1600
      PRINT #1, tiled%(i%)
   NEXT i%
CLOSE #1

GET (460, 360)-(499, 399), tiled%
DEF SEG = VARSEG(tiled%(0))
BSAVE converged.graphic.name$, 0, 1600
DEF SEG

Exit.Converge:
Erase.Windows
temp.mode$ = ""
GOTO mode.route

New.File:
Icon.Normal
icon.number = 13
Icon.Pressed

IF draw.mode$ <> "mouse" THEN LINE (x, y)-(x + 8, y + 8), 15, B

FOR y = 28 TO 340 STEP 8
  FOR x = 29 TO 341 STEP 8
    LINE (x + 1, y + 1)-(x + 7, y + 7), 0, BF
    LINE (x + 2, y + 2)-(x + 6, y + 6), 15
    LINE (x + 2, y + 6)-(x + 6, y + 2), 15
  NEXT x
NEXT y

FOR a% = 0 TO 1600: tile%(a%) = 16: NEXT a%
LINE (363, 357)-(324, 396), 0, BF
LINE (352, cby)-(370, cby + 18), 15, B

x = 29
y = 28
x1 = 324
y1 = 357
xcor = 1
ycor = 1
c% = 0
cby = 35
a% = 0
graphic.text$ = "untitled                                ": Filename.Text

LINE (352, cby)-(370, cby + 18), 0, B

Erase.Windows
temp.mode$ = ""
Icon.Normal
icon.number = 1
Icon.Pressed
graphic.text$ = "Use the number pad or the arrow keys to move the cursor   "
Text.Line1
graphic.text$ = "around the grid. Press space to start drawing.            "
Text.Line2
IF draw.mode$ = "mouse" THEN
  mouseshow
  GOTO mouse.loop
  ELSE draw.mode$ = "penup"
    LINE (x, y)-(x + 8, y + 8), 0, B
    GOTO mode.route
END IF

Load.File:
Icon.Normal
icon.number = 14
Icon.Pressed
LINE (502, 44)-(632, 44), 7
LINE (502, 44)-(374, 44), 15
graphic.text$ = "Load tile                              "
Mode
graphic.text$ = "Type the location of the tile.   "
Save.Load.Text1
graphic.text$ = "Press enter when finished.       "
Save.Load.Text2
graphic.text$ = "location example:                                         "
Text.Line1
graphic.text$ = "c:\windows\desktop\tiled-x\tiled                          "
Text.Line2

x.position = 432: y.position = 55: minimum = 432: max.length = 24
Get.Input
IF Exit$ = "yes" THEN GOTO Exit.Load

old.a% = a%
a% = 0
OPEN memory.name$ FOR INPUT AS #1
FOR i% = 0 TO 1600
  INPUT #1, tiled%(i%)
  tile%(a%) = tiled%(i%)
  a% = a% + 1
NEXT i%
CLOSE #1
a% = old.a%

Screen.Redraw

IF draw.mode$ <> "mouse" THEN draw.mode$ = "penup"
LINE (71, 357)-(316, 366), 15, BF
length = LEN(graphic.name$)
IF length < 41 THEN filename$ = LEFT$(graphic.name$, length)
IF length >= 41 THEN filename$ = LEFT$(graphic.name$, 37) + "...": length = 40
graphic.text$ = filename$: Filename.Text
Exit.Load:
LINE (502, 44)-(633, 44), 7
LINE (502, 44)-(374, 44), 15
LINE (430, 54)-(581, 63), 15, BF
LINE (378, 71)-(581, 90), 15, BF
LINE (588, 50)-(627, 89), 0, BF
temp.mode$ = ""
save.or.load$ = "load"
Update
GOTO mode.route

Save.File:
Icon.Normal
icon.number = 15
Icon.Pressed
LINE (502, 44)-(374, 44), 7
LINE (502, 44)-(632, 44), 15
graphic.text$ = "Type in the Filename of the tile."
Save.Load.Text1
graphic.text$ = "press enter when finished.       "
Save.Load.Text2
graphic.text$ = "Filename example:                                         "
Text.Line1
graphic.text$ = "c:\windows\desktop\tiled-x\tiled                          "
Text.Line2
graphic.text$ = "save tile            "
Mode
x.position = 432: y.position = 55: minimum = 432: max.length = 24
GET (324, 357)-(363, 396), tiled%
PUT (588, 50), tiled%, PSET
Get.Input
IF Exit$ = "yes" THEN GOTO Exit.Save

old.a% = a%
i% = 0
OPEN memory.name$ FOR OUTPUT AS #1
  FOR a% = 0 TO 1600
    tiled%(i%) = tile%(a%)
    PRINT #1, tiled%(i%)
    i% = i% + 1
  NEXT a%
CLOSE #1
a% = old.a%

GET (324, 357)-(363, 396), tiled%
DEF SEG = VARSEG(tiled%(0))
BSAVE graphic.name$, 0, 1600
DEF SEG

LINE (71, 357)-(316, 366), 15, BF
length = LEN(graphic.name$)
IF length < 41 THEN filename$ = LEFT$(graphic.name$, length)
IF length >= 41 THEN filename$ = LEFT$(graphic.name$, 37) + "...": length = 40
text.length = length
graphic.text$ = graphic.name$
save.load$ = "yes"
LINE (71, 373)-(316, 382), 15, BF
Filename.Text
Exit.Save:
LINE (430, 54)-(581, 63), 15, BF
LINE (378, 71)-(581, 90), 15, BF
LINE (588, 50)-(627, 89), 0, BF
temp.mode$ = ""
save.or.load$ = "save"
Update
GOTO mode.route

Left.Right:

Mirror.Left.Right
Icon.Normal
icon.number = 9
Icon.Pressed
Screen.Redraw
temp.mode$ = ""
Update
GOTO mode.route

Up.Down:

Icon.Normal
icon.number = 8
Icon.Pressed
Mirror.Up.Down
Screen.Redraw
temp.mode$ = ""
Update
GOTO mode.route

rrotate.left:

Icon.Normal
icon.number = 10
Icon.Pressed
Rotate.Left
Screen.Redraw
temp.mode$ = ""
Update
GOTO mode.route

rrotate.right:

Icon.Normal
icon.number = 11
Icon.Pressed
Rotate.Right
Screen.Redraw
temp.mode$ = ""
Update
GOTO mode.route

'*****************************************************************************
'                             Mouse Main Module
'*****************************************************************************

Mouse.Module:
Update
mouseshow

mouse.loop:
TIMER ON
start.time = TIMER
mouseshow
DO
  key$ = INKEY$
  mousestatus lb%, rb%, xmouse%, ymouse%
  x.multiple = INT((xmouse% - 29) / 8)
  y.multiple = INT((ymouse% - 28) / 8)
  a% = x.multiple + (y.multiple * 40)
  xcor = x.multiple + 1
  ycor = y.multiple + 1
  IF xcor > 0 AND xcor < 41 AND ycor > 0 AND ycor < 41 THEN x = ((x.multiple + 1) * 8) + 21
  IF xcor > 0 AND xcor < 41 AND ycor > 0 AND ycor < 41 THEN y = ((y.multiple + 1) * 8) + 20
  x1 = (x.multiple + 1) + 323
  y1 = (y.multiple + 1) + 356
  IF xcor > 0 AND xcor < 41 AND ycor > 0 AND ycor < 41 THEN Coordinates
  IF lb% = -1 THEN GOTO left.button.click
  IF rb% = -1 THEN GOTO Icon.Help
  IF key$ = "m" AND x > xlower AND x < xup AND y > ylower AND y < yup THEN mousehide: LINE (x, y)-(x + 8, y + 8), 0, B: draw.mode$ = "penup": GOTO mode.route
  time.past = TIMER - start.time
  LOOP WHILE time.past < 1
TIMER OFF
GOTO Icon.Identify

left.button.click:
'Mouse Mode Route
IF xmouse% > 625 AND xmouse% < 635 AND ymouse% > 3 AND ymouse% < 13 THEN GOTO mouse.exit.tiled
IF xmouse% > 428 AND xmouse% < 582 AND ymouse% > 53 AND ymouse% < 64 AND save.or.load$ = "load" OR xmouse% > 502 AND xmouse% < 633 AND ymouse% > 28 AND ymouse% < 44 THEN GOTO mouse.load
IF xmouse% > 428 AND xmouse% < 582 AND ymouse% > 53 AND ymouse% < 64 AND save.or.load$ = "save" OR xmouse% > 373 AND xmouse% < 501 AND ymouse% > 28 AND ymouse% < 44 THEN GOTO mouse.save
IF xmouse% > 372 AND xmouse% < 634 AND ymouse% > 97 AND ymouse% < 348 THEN GOTO mouse.help
IF xmouse% > 372 AND xmouse% < 634 AND ymouse% > 350 AND ymouse% < 453 THEN GOTO mouse.converge
IF xmouse% > 5 AND xmouse% < 26 AND ymouse% > 27 AND ymouse% < 349 THEN
  mode.route = INT((ymouse% - 8) / 20)
  IF mode.route > 4 AND mode.route < 17 AND mode.route <> 12 THEN
    Icon.Normal
    icon.number = mode.route
    Icon.Pressed
  END IF
  mouseshow
  IF mode.route = 5 THEN GOTO mouse.switch.color
  IF mode.route = 6 THEN GOTO mouse.paint.background
  IF mode.route = 7 THEN GOTO mouse.paint.screen
  IF mode.route = 8 THEN GOTO mouse.up.down
  IF mode.route = 9 THEN GOTO mouse.left.right
  IF mode.route = 10 THEN GOTO mouse.rotate.left
  IF mode.route = 11 THEN GOTO mouse.rotate.right
  IF mode.route = 12 THEN GOTO mouse.converge
  IF mode.route = 13 THEN GOTO mouse.new.tile
  IF mode.route = 14 THEN GOTO mouse.load
  IF mode.route = 15 THEN GOTO mouse.save
  IF mode.route = 16 THEN GOTO mouse.help
END IF

'Pen Up, Pen Down, and Single Pixel
IF xcor > 0 AND xcor < 41 AND ycor > 0 AND ycor < 41 THEN
  Icon.Normal
  icon.number = 2
  Icon.Pressed
  graphic.text$ = "pen down             "
  Mode
  DO
  mousestatus lb%, rb%, xmouse%, ymouse%
  x.multiple = INT((xmouse% - 29) / 8)
  y.multiple = INT((ymouse% - 28) / 8)
  a% = x.multiple + (y.multiple * 40)
  x1 = (x.multiple + 1) + 323
  y1 = (y.multiple + 1) + 356
  xcor = x.multiple + 1
  ycor = y.multiple + 1
  x = ((x.multiple + 1) * 8) + 21
  y = ((y.multiple + 1) * 8) + 20
  IF xcor > 0 AND xcor < 41 AND ycor > 0 AND ycor < 41 THEN Coordinates
  mousehide
  IF x > xlow AND x < xup AND y > ylow AND y < yup AND pal(c%) <> 16 THEN
    LINE (x + 1, y + 1)-(x + 7, y + 7), pal(c%), BF
    PSET (x1, y1), pal(c%)
    tile%(a%) = pal(c%)
  END IF
  IF x > xlow AND x < xup AND y > ylow AND y < yup AND pal(c%) = 16 THEN
    LINE (x + 1, y + 1)-(x + 7, y + 7), 0, BF
    LINE (x + 2, y + 2)-(x + 6, y + 6), 15
    LINE (x + 2, y + 6)-(x + 6, y + 2), 15
    PSET (x1, y1), 0
    tile%(a%) = pal(c%)
  END IF
  mouseshow
  LOOP WHILE lb% = -1
  graphic.text$ = "pen up               "
  Mode
  Icon.Normal
  icon.number = 1
  Icon.Pressed
  GOTO mouse.loop
END IF

'Color Change
IF xmouse% > 351 AND xmouse% < 371 AND ymouse% > 35 AND ymouse% < 341 THEN
  Icon.Normal
  icon.number = 4
  Icon.Pressed
  mousehide
  graphic.text$ = "color change         "
  Mode
  LINE (352, cby)-(370, cby + 18), 15, B
  c% = INT((ymouse% - 35) / 18)
  cby = (c% * 18) + 35
  LINE (352, cby)-(370, cby + 18), 0, B
  graphic.text$ = "pen up               "
  Mode
  mouseshow
  Icon.Normal
  icon.number = 1
  Icon.Pressed
  GOTO mouse.loop
END IF
GOTO mouse.loop

'Switch Color
mouse.switch.color:
  mousehide
  Icon.Normal
  icon.number = 5
  Icon.Pressed
  mouseshow
  graphic.text$ = "left click on the color you wish to change.               "
  Text.Line1
  graphic.text$ = blank$
  Text.Line2
  graphic.text$ = "switch color         "
  temp% = c%
  Mode
  DO
    mousestatus lb%, rb%, xmouse%, ymouse%
    IF lb% = -1 AND xmouse% > 351 AND xmouse% < 371 AND ymouse% > 35 AND ymouse% < 341 THEN proceed$ = "yes": GOTO New.Color.Loop
    IF rb% = -1 THEN Update: GOTO mouse.loop
  LOOP

New.Color.Loop:
IF proceed$ <> "yes" THEN GOTO mouse.loop
  mousehide
  LINE (352, cby)-(370, cby + 18), 15, B
  c% = INT((ymouse% - 35) / 18)
  tcby = (c% * 18) + 35
  old.color = pal(c%)
  LINE (352, tcby)-(370, tcby + 18), 0, B
  mouseshow
  graphic.text$ = "left click on the color that you wish to change the       "
  Text.Line1
  graphic.text$ = "previously selected color to.                             "
  Text.Line2
  DO
    mousestatus lb%, rb%, xmouse%, ymouse%
    IF lb% = -1 AND xmouse% > 351 AND xmouse% < 371 AND ymouse% > 35 AND ymouse% < 341 THEN GOTO Color.Selected
    IF rb% = -1 THEN
      mousehide
      LINE (352, tcby)-(370, tcby + 18), 15, B
      LINE (352, cby)-(370, cby), 0, B
      c% = temp%
      mouseshow
      Update
      GOTO mouse.loop
    END IF
  LOOP

Color.Selected:

mousehide
LINE (352, tcby)-(370, tcby + 18), 15, B
c% = INT((ymouse% - 35) / 18)
tcby = (c% * 18) + 35
new.color = pal(c%)
LINE (352, tcby)-(370, tcby + 18), 0, B
 
old.a% = a%

FOR a% = 0 TO 1600
   IF tile%(a%) = old.color THEN tile%(a%) = new.color
NEXT a%

Screen.Redraw

a% = old.a%
temp.mode$ = ""
c% = temp%
LINE (352, tcby)-(370, tcby + 18), 15, B
LINE (352, cby)-(370, cby + 18), 0, B
proceed$ = "no"
Update
mouseshow
GOTO mouse.loop

mouse.paint.background:
  mousehide
  Icon.Normal
  icon.number = 6
  Icon.Pressed
  mouseshow
  graphic.text$ = "left click on the color you wish to paint the background. "
  Text.Line1
  graphic.text$ = blank$
  Text.Line2
  temp% = c%
  graphic.text$ = "paint background     "
  Mode
  SLEEP 1
  DO
    mousestatus lb%, rb%, xmouse%, ymouse%
    IF lb% = -1 AND xmouse% > 351 AND xmouse% < 371 AND ymouse% > 35 AND ymouse% < 341 THEN GOTO Background.Color.Selected
    IF rb% = -1 THEN Update: GOTO mouse.loop
  LOOP

Background.Color.Selected:
  mousehide
  LINE (352, cby)-(370, cby + 18), 15, B
  c% = INT((ymouse% - 35) / 18)
  tcby = (c% * 18) + 35
  background.color = pal(c%)
  LINE (352, tcby)-(370, tcby + 18), 0, B

old.a% = a%

FOR a% = 0 TO 1600
   IF tile%(a%) = 16 THEN tile%(a%) = background.color
NEXT a%

Screen.Redraw

a% = old.a%
temp.mode$ = ""
c% = temp%
LINE (352, tcby)-(370, tcby + 18), 15, B
LINE (352, cby)-(370, cby + 18), 0, B
mouseshow
Update
GOTO mouse.loop

mouse.paint.screen:
  mousehide
  Icon.Normal
  icon.number = 7
  Icon.Pressed
  mouseshow
  graphic.text$ = "left click on the color you wish to paint the screen.     "
  Text.Line1
  graphic.text$ = blank$
  Text.Line2
  temp% = c%
  graphic.text$ = "paint screen         "
  Mode
  DO
    mousestatus lb%, rb%, xmouse%, ymouse%
    IF lb% = -1 AND xmouse% > 351 AND xmouse% < 371 AND ymouse% > 35 AND ymouse% < 341 THEN GOTO Paint.Color.Selected
    IF rb% = -1 THEN Update: GOTO mouse.loop
  LOOP

Paint.Color.Selected:
  mousehide
  LINE (352, cby)-(370, cby + 18), 15, B
  c% = INT((ymouse% - 35) / 18)
  tcby = (c% * 18) + 35
  paint.color = pal(c%)
  LINE (352, tcby)-(370, tcby + 18), 0, B

old.a% = a%
FOR a% = 0 TO 1600
   tile%(a%) = paint.color
NEXT a%
a% = old.a%

Screen.Redraw

temp.mode$ = ""
c% = temp%
LINE (352, tcby)-(370, tcby + 18), 15, B
LINE (352, cby)-(370, cby + 18), 0, B
mouseshow
Update
GOTO mouse.loop

mouse.up.down:
  mousehide
  GOTO Up.Down

mouse.left.right:
  mousehide
  GOTO Left.Right

mouse.rotate.left:
  mousehide
  GOTO rrotate.left

mouse.rotate.right:
  mousehide
  GOTO rrotate.right

mouse.converge:
  mousehide
  GOTO Converge

mouse.new.tile:
  mousehide
  GOTO New.File
 
mouse.help:
  mousehide
  Icon.Normal
  icon.number = 16
  Icon.Pressed
  mouseshow
  x.position = 427: y.position = 116
  minimum = 427: max.length = 39
  help.option$ = "yes"
  graphic.text$ = "help                 "
  Mode
  graphic.text$ = "type in the keyword or keywords that you wish to learn    "
  Text.Line1
  graphic.text$ = "more about. Press enter when finished.                    "
  Text.Line2
  Get.Input
  Help
  Erase.Windows
  Update
  GOTO mouse.loop

'Load
mouse.load:
  mousehide
  GOTO Load.File

'Save
mouse.save:
  mousehide
  GOTO Save.File

'Exit TiledX
mouse.exit.tiled:
  mousehide
  LINE (626, 4)-(634, 4), 8
  LINE (626, 4)-(626, 12), 8
  LINE (627, 12)-(634, 12), 15
  LINE (634, 12)-(634, 5), 15
  FOR time.delay = 1 TO 30000
  NEXT time.delay
  Exit.Tiled

'This section displays what the mode is when next to the icon
Icon.Identify:
IF xmouse% > 5 AND xmouse% < 26 AND ymouse% > 27 AND ymouse% < 349 THEN
  icon.identity = INT((ymouse% - 8) / 20)
  oxmouse% = xmouse%
  oymouse% = ymouse%
  icon.y = (icon.identity * 20) + 8
  IF icon.identity = 1 THEN
    graphic.text$ = "pen up"
    Icon.Routine
    GOTO mouse.loop
  END IF
  IF icon.identity = 2 THEN
    graphic.text$ = "pen down"
    Icon.Routine
    GOTO mouse.loop
  END IF
  IF icon.identity = 3 THEN
    graphic.text$ = "single pixel"
    Icon.Routine
    GOTO mouse.loop
  END IF
  IF icon.identity = 4 THEN
    graphic.text$ = "change color"
    Icon.Routine
    GOTO mouse.loop
  END IF
  IF icon.identity = 5 THEN
    graphic.text$ = "switch color"
    Icon.Routine
    GOTO mouse.loop
  END IF
  IF icon.identity = 6 THEN
    graphic.text$ = "paint background"
    Icon.Routine
    GOTO mouse.loop
  END IF
  IF icon.identity = 7 THEN
    graphic.text$ = "paint screen"
    Icon.Routine
    GOTO mouse.loop
  END IF
  IF icon.identity = 8 THEN
    graphic.text$ = "flip up\down"
    Icon.Routine
    GOTO mouse.loop
  END IF
  IF icon.identity = 9 THEN
    graphic.text$ = "flip left\right"
    Icon.Routine
    GOTO mouse.loop
  END IF
  IF icon.identity = 10 THEN
    graphic.text$ = "rotate left"
    Icon.Routine
    GOTO mouse.loop
  END IF
  IF icon.identity = 11 THEN
    graphic.text$ = "rotate right"
    Icon.Routine
    GOTO mouse.loop
  END IF
  IF icon.identity = 12 THEN
    graphic.text$ = "converge"
    Icon.Routine
    GOTO mouse.loop
  END IF
  IF icon.identity = 13 THEN
    graphic.text$ = "new"
    Icon.Routine
    GOTO mouse.loop
  END IF
  IF icon.identity = 14 THEN
    graphic.text$ = "load"
    Icon.Routine
    GOTO mouse.loop
  END IF
  IF icon.identity = 15 THEN
    graphic.text$ = "save"
    Icon.Routine
    GOTO mouse.loop
  END IF
  IF icon.identity = 16 THEN
    graphic.text$ = "help"
    Icon.Routine
    GOTO mouse.loop
  END IF
END IF

'This section displays the help for the selected icon
Icon.Help:
IF xmouse% > 5 AND xmouse% < 26 AND ymouse% > 27 AND ymouse% < 349 THEN
  icon.identity = INT((ymouse% - 8) / 20)
  IF icon.identity = 1 THEN
    keyword$ = "F2"
    Help
    GOTO mouse.loop
  END IF
  IF icon.identity = 2 THEN
    keyword$ = "F3"
    Help
    GOTO mouse.loop
  END IF
  IF icon.identity = 3 THEN
    keyword$ = "F4"
    Help
    GOTO mouse.loop
  END IF
  IF icon.identity = 4 THEN
    keyword$ = "F5"
    Help
    GOTO mouse.loop
  END IF
  IF icon.identity = 5 THEN
    keyword$ = "F6"
    Help
    GOTO mouse.loop
  END IF
  IF icon.identity = 6 THEN
    keyword$ = "F7"
    Help
    GOTO mouse.loop
  END IF
  IF icon.identity = 7 THEN
    keyword$ = "F8"
    Help
    GOTO mouse.loop
  END IF
  IF icon.identity = 12 THEN
    keyword$ = "CONVERGE"
    Help
    GOTO mouse.loop
  END IF
  IF icon.identity = 13 THEN
    keyword$ = "NEW"
    Help
    GOTO mouse.loop
  END IF
  IF icon.identity = 14 THEN
    keyword$ = "LOAD"
    Help
    GOTO mouse.loop
  END IF
  IF icon.identity = 15 THEN
    keyword$ = "SAVE"
    Help
    GOTO mouse.loop
  END IF
  IF icon.identity = 16 THEN
    keyword$ = "HELP"
    Help
    GOTO mouse.loop
  END IF
END IF
GOTO mouse.loop

SUB BMP
DIM byte AS STRING * 1
OPEN data$ FOR BINARY AS #1
table$ = INPUT$(54, #1)
DIM table&(30)
DEF SEG = VARSEG(table&(1))
pointer% = VARPTR(table&(1))
FOR x% = 0 TO 51
 POKE pointer% + x%, ASC(MID$(table$, x% + 3, 1))
NEXT
DEF SEG
SCREEN 12
thecolors$ = INPUT$(table&(3) - 54, #1)
DEF SEG = VARSEG(pal&)
pointer% = VARPTR(pal&)
FOR x% = 0 TO 63 STEP 4
 POKE pointer%, (ASC(MID$(thecolors$, x% + 3, 1))) \ 4
 POKE pointer% + 1, (ASC(MID$(thecolors$, x% + 2, 1))) \ 4
 POKE pointer% + 2, (ASC(MID$(thecolors$, x% + 1, 1))) \ 4
 POKE pointer% + 3, 0
NEXT
DEF SEG
y% = table&(6)
DO
  data$ = INPUT$((((table&(5) - 1) OR 7) + 1) \ 2, #1)
    IF (table&(5) \ 2) < LEN(data$) THEN
       linelength% = table&(5) \ 2
    ELSE
       linelength% = LEN(data$)
    END IF
    FOR x% = 1 TO linelength%
      pixel% = ASC(MID$(data$, x%, 1))
      PSET (x% * 2 + 1, y%), pixel% AND 15
      PSET (x% * 2, y%), pixel% \ 16
     NEXT
    y% = y% - 1
LOOP UNTIL EOF(1)
CLOSE
SLEEP 1
END SUB

SUB Change.Color.Sub
Color.Do.Loop:

DO: Color$ = INKEY$: LOOP WHILE Color$ = ""

IF Color$ = "8" AND tcby = 35 OR Color$ = CHR$(0) + "H" AND tcby = 35 THEN
  LINE (352, tcby)-(370, tcby + 18), 15, B
  c% = 16
  tcby = 323
  LINE (352, tcby)-(370, tcby + 18), 0, B
  GOTO Color.Do.Loop

ELSEIF Color$ = "2" AND tcby = 323 OR Color$ = CHR$(0) + "P" AND tcby = 323 THEN
  LINE (352, tcby)-(370, tcby + 18), 15, B
  c% = 0
  tcby = 35
  LINE (352, tcby)-(370, tcby + 18), 0, B
  GOTO Color.Do.Loop

ELSEIF Color$ = "8" OR Color$ = CHR$(0) + "H" THEN
  LINE (352, tcby)-(370, tcby + 18), 15, B
  c% = c% - 1
  tcby = tcby - 18
  LINE (352, tcby)-(370, tcby + 18), 0, B
  GOTO Color.Do.Loop

ELSEIF Color$ = "2" OR Color$ = CHR$(0) + "P" THEN
  LINE (352, tcby)-(370, tcby + 18), 15, B
  c% = c% + 1
  tcby = tcby + 18
  LINE (352, tcby)-(370, tcby + 18), 0, B
  GOTO Color.Do.Loop

ELSEIF Color$ = CHR$(27) THEN
  Exit$ = "yes"
  GOTO Exit.Color.Do.Loop

ELSEIF Color$ = CHR$(32) THEN
  GOTO Exit.Color.Do.Loop
 
ELSE GOTO Color.Do.Loop
END IF

Exit.Color.Do.Loop:
END SUB

SUB Coordinates
graphic.text$ = LTRIM$(STR$(xcor)) + "," + LTRIM$(STR$(ycor)) + "   "
letter.place.x = 72
letter.place.y = 390
Color$ = ""
TEXT
END SUB

SUB Erase.Windows
IF draw.mode$ = "mouse" THEN mousehide
LINE (430, 54)-(581, 63), 15, BF
LINE (378, 71)-(581, 90), 15, BF
LINE (426, 115)-(595, 124), 15, BF
LINE (379, 128)-(630, 343), 15, BF
LINE (429, 418)-(628, 427), 15, BF
LINE (429, 436)-(628, 445), 15, BF
LINE (12, 405)-(365, 448), 15, BF

LINE (385, 371)-(424, 410), 0, BF
LINE (580, 371)-(619, 410), 0, BF

LINE (483, 371)-(522, 410), 7, BF
LINE (588, 50)-(627, 89), 0, BF

letter.place.x = 382
letter.place.y = 131
help.bypass$ = "yes"

graphic.text$ = "keys -                                   ": TEXT
graphic.text$ = "       help.....................f1       ": TEXT
graphic.text$ = "       pen up...................f2       ": TEXT
graphic.text$ = "       pen down.................f3       ": TEXT
graphic.text$ = "       single pixel.............f4       ": TEXT
graphic.text$ = "       change color.............f5       ": TEXT
graphic.text$ = "       switch color.............f6       ": TEXT
graphic.text$ = "       paint background.........f7       ": TEXT
graphic.text$ = "       paint screen.............f8       ": TEXT
graphic.text$ = "       converge.................f9       ": TEXT
graphic.text$ = "       new tile................f10       ": TEXT
graphic.text$ = "       save tile...............f11       ": TEXT
graphic.text$ = "       load tile...............f12       ": TEXT
graphic.text$ = "       quit....................esc       ": TEXT
graphic.text$ = "       flip up\down..............c       ": TEXT
graphic.text$ = "       flip left\right...........v       ": TEXT
graphic.text$ = "       rotate left...............b       ": TEXT
graphic.text$ = "       rotate right..............n       ": TEXT
graphic.text$ = "       mouse toggle..............m       ": TEXT
graphic.text$ = "       save palette..............s       ": TEXT
graphic.text$ = "       load palette..............l       ": TEXT
IF draw.mode$ = "mouse" THEN mouseshow
help.bypass$ = "no"
END SUB

SUB Exit.Tiled
LINE (0, 460)-(639, 479), 0, BF
FOR a = 1 TO 30
LOCATE 30, 1: PRINT ""
NEXT a
CLS
SCREEN 12
DEF SEG = VARSEG(tile%(0))
BLOAD pathname$ + "dimsoft.pic", 0
DEF SEG
FOR y = 0 TO 440 STEP 40
   FOR x = 0 TO 600 STEP 40
      PUT (x, y), tile%, PSET
    NEXT x
NEXT y
LINE (190, 187)-(440, 290), 0, BF
LINE (189, 186)-(441, 291), 8, B
LINE (188, 185)-(442, 292), 7, B
LINE (187, 184)-(443, 293), 15, B
LINE (186, 183)-(444, 294), 15, B
LINE (185, 182)-(445, 295), 7, B
LINE (184, 181)-(446, 296), 8, B
LINE (183, 180)-(447, 297), 0, B

LOCATE 13, 26: PRINT "  Thanks for using TiledX!!  "
LOCATE 14, 26: PRINT "     Dimension SOFT 1998     "
LOCATE 16, 26: PRINT "    email: rropin@aol.com    "
LOCATE 17, 26: PRINT " Programmed by: Chris Rennie "
LOCATE 18, 26: PRINT " Debugging by: Jay McCarthy  "
END
END SUB

SUB Filename.Text
letter.place.x = 72
letter.place.y = 358
Color$ = ""
TEXT
END SUB

SUB Get.Input
begin.x.position = x.position
begin.y.position = y.position
graphic.text$ = ""
keyword$ = ""
graphic.name$ = ""
memory.name$ = ""
i% = 0
do.loop:

DO
   DEF SEG = VARSEG(letter(0))
   BLOAD pathname$ + "unscr.gtx", letter
   DEF SEG
   PUT (x.position, y.position), letter, PSET
   key$ = INKEY$
LOOP WHILE key$ = ""

IF key$ = CHR$(13) THEN DEF SEG = VARSEG(letter(0)): BLOAD pathname$ + "space.gtx", 0: DEF SEG : PUT (x.position, y.position), letter, PSET: GOTO Exit.Do.Loop
IF key$ = CHR$(27) THEN Exit$ = "yes": GOTO Exit.Get.Input
IF key$ = CHR$(8) AND x.position = minimum THEN
   GOTO do.loop
   ELSEIF key$ = CHR$(8) AND i% > max.length THEN
      i% = i% - 1: name$(i%) = ""
      begin.i% = i% - max.length: end.i% = i%:
      FOR i% = begin.i% TO end.i%: graphic.text$ = graphic.text$ + name$(i%): NEXT i%
      letter.place.x = begin.x.position: letter.place.y = begin.y.position: text.length = LEN(graphic.text$): TEXT
      i% = end.i%: graphic.text$ = ""
      GOTO do.loop
      ELSEIF key$ = CHR$(8) THEN
         i% = i% - 1: name$(i%) = ""
         DEF SEG = VARSEG(letter(0)): BLOAD pathname$ + "space.gtx", 0: DEF SEG : PUT (x.position, y.position), letter, PSET
         x.position = x.position - 6
         GOTO do.loop
         ELSEIF i% = max.length AND limit$ = "yes" THEN
           GOTO do.loop
         ELSEIF i% >= max.length THEN
            begin.i% = i% - (max.length - 1): end.i% = i%: name$(i%) = key$
            FOR i% = begin.i% TO end.i%: graphic.text$ = graphic.text$ + name$(i%): NEXT i%
            letter.place.x = begin.x.position: letter.place.y = begin.y.position: text.length = LEN(graphic.text$): TEXT
            i% = end.i%: i% = i% + 1
            GOTO do.loop
            ELSE name$(i%) = key$
               IF key$ = ":" THEN key$ = "colon"
               IF key$ = "\" THEN key$ = "slash"
               IF key$ = " " AND help.option$ = "yes" THEN key$ = "space"
               IF key$ = " " AND help.option$ = "no" THEN GOTO do.loop
               DEF SEG = VARSEG(letter(0))
               graphic.name$ = pathname$ + key$ + ".gtx"
               BLOAD graphic.name$, letter
               DEF SEG
               PUT (x.position, y.position), letter, PSET
               x.position = x.position + 6: i% = i% + 1
               GOTO do.loop
END IF

Exit.Do.Loop:
FOR i% = 0 TO 150: filename$ = filename$ + name$(i%): NEXT i%
filename$ = UCASE$(LTRIM$(RTRIM$(filename$)))
keyword$ = filename$
memory.name$ = filename$ + ".mem": graphic.name$ = filename$ + ".pic"
FOR i% = 0 TO 150: name$(i%) = "": NEXT i%: i% = 0
limit$ = "no"
Exit.Get.Input:
END SUB

SUB Grid.Movement
Grid.Movement.Loop:

DO: key$ = INKEY$: LOOP WHILE key$ = ""

IF key$ = "8" AND y > ylower OR key$ = CHR$(0) + "H" AND y > ylower THEN
 LINE (x, y)-(x + 8, y + 8), 15, B
 y = y - 8: y1 = y1 - 1
 a% = a% - 40
 ycor = ycor - 1
 GOTO exit.grid.movement

ELSEIF key$ = "2" AND y < yupper OR key$ = CHR$(0) + "P" AND y < yupper THEN
  LINE (x, y)-(x + 8, y + 8), 15, B
  y = y + 8: y1 = y1 + 1
  a% = a% + 40
  ycor = ycor + 1
  GOTO exit.grid.movement

ELSEIF key$ = "4" AND x > xlower OR key$ = CHR$(0) + "K" AND x > xlower THEN
  LINE (x, y)-(x + 8, y + 8), 15, B
  x = x - 8: x1 = x1 - 1
  a% = a% - 1
  xcor = xcor - 1
  GOTO exit.grid.movement

ELSEIF key$ = "6" AND x < xupper OR key$ = CHR$(0) + "M" AND x < xupper THEN
  LINE (x, y)-(x + 8, y + 8), 15, B
  x = x + 8: x1 = x1 + 1
  a% = a% + 1
  xcor = xcor + 1
  GOTO exit.grid.movement

ELSEIF key$ = "7" AND x > xlower AND y > ylower THEN
  LINE (x, y)-(x + 8, y + 8), 15, B
  x = x - 8: x1 = x1 - 1
  y = y - 8: y1 = y1 - 1
  a% = a% - 41
  xcor = xcor - 1
  ycor = ycor - 1
  GOTO exit.grid.movement

ELSEIF key$ = "9" AND x < xupper AND y > ylower THEN
  LINE (x, y)-(x + 8, y + 8), 15, B
  x = x + 8: x1 = x1 + 1
  y = y - 8: y1 = y1 - 1
  a% = a% - 39
  xcor = xcor + 1
  ycor = ycor - 1
  GOTO exit.grid.movement

ELSEIF key$ = "1" AND x > xlower AND y < yupper THEN
  LINE (x, y)-(x + 8, y + 8), 15, B
  x = x - 8: x1 = x1 - 1
  y = y + 8: y1 = y1 + 1
  a% = a% + 39
  xcor = xcor - 1
  ycor = ycor + 1
  GOTO exit.grid.movement

ELSEIF key$ = "3" AND x < xupper AND y < yupper THEN
  LINE (x, y)-(x + 8, y + 8), 15, B
  x = x + 8: x1 = x1 + 1
  y = y + 8: y1 = y1 + 1
  a% = a% + 41
  xcor = xcor + 1
  ycor = ycor + 1
  GOTO exit.grid.movement

ELSEIF key$ = CHR$(0) + ";" THEN
  Icon.Normal
  icon.number = 16
  Icon.Pressed
  x.position = 427: y.position = 116
  minimum = 427: max.length = 27
  help.option$ = "yes"
  graphic.text$ = "help                 "
  Mode
  graphic.text$ = "type in the keyword or keywords that you wish to learn    "
  Text.Line1
  graphic.text$ = "more about. Press enter when finished.                    "
  Text.Line2
  Get.Input
  Help
  Erase.Windows
  Update
  GOTO exit.grid.movement

ELSEIF key$ = CHR$(0) + "<" OR draw.mode$ = "pendown" AND key$ = CHR$(32) THEN
  Icon.Normal
  icon.number = 1
  Icon.Pressed
  graphic.text$ = "Use the number pad or the arrow keys to move the cursor   "
  Text.Line1
  graphic.text$ = "around the grid. Press space to start drawing.            "
  Text.Line2
  draw.mode$ = "penup"
  GOTO exit.grid.movement

ELSEIF key$ = CHR$(0) + "=" OR draw.mode$ = "penup" AND key$ = CHR$(32) THEN
  Icon.Normal
  icon.number = 2
  Icon.Pressed
  draw.mode$ = "pendown"
  graphic.text$ = "Use the number pad or the arrow keys to draw. Press space "
  Text.Line1
  graphic.text$ = "to go to pen up.                                          "
  Text.Line2
  GOTO exit.grid.movement

ELSEIF key$ = CHR$(0) + ">" OR draw.mode$ = "singlepixel" AND key$ = CHR$(32) THEN
  Icon.Normal
  icon.number = 3
  Icon.Pressed
  graphic.text$ = "Use the number pad or the arrow keys to move the cursor   "
  Text.Line1
  graphic.text$ = "around the grid. Press space to draw a single pixel.      "
  Text.Line2
  draw.mode$ = "singlepixel"
  GOTO exit.grid.movement

ELSEIF key$ = CHR$(0) + "?" THEN
  temp.mode$ = "changecolor": GOTO exit.grid.movement
 
ELSEIF key$ = CHR$(0) + "@" THEN
  temp.mode$ = "switchcolor": GOTO exit.grid.movement

ELSEIF key$ = CHR$(0) + "A" THEN
  temp.mode$ = "paintbackground": GOTO exit.grid.movement

ELSEIF key$ = CHR$(0) + "B" THEN
  temp.mode$ = "paintscreen": GOTO exit.grid.movement

ELSEIF key$ = CHR$(0) + "C" THEN
  temp.mode$ = "converge": GOTO exit.grid.movement

ELSEIF key$ = CHR$(0) + "D" THEN
  temp.mode$ = "newfile": GOTO exit.grid.movement

ELSEIF key$ = CHR$(0) + CHR$(133) THEN
  temp.mode$ = "savefile": GOTO exit.grid.movement

ELSEIF key$ = CHR$(0) + CHR$(134) THEN
  temp.mode$ = "loadfile": GOTO exit.grid.movement

ELSEIF key$ = CHR$(27) THEN
  Exit.Tiled

ELSEIF key$ = "c" THEN
  temp.mode$ = "up/down": GOTO exit.grid.movement

ELSEIF key$ = "v" THEN
  temp.mode$ = "left/right": GOTO exit.grid.movement

ELSEIF key$ = "b" THEN
  temp.mode$ = "rotate.left": GOTO exit.grid.movement

ELSEIF key$ = "n" THEN
  temp.mode$ = "rotate.right": GOTO exit.grid.movement

ELSEIF key$ = "m" THEN
  LINE (x, y)-(x + 8, y + 8), 15, B
  draw.mode$ = "mouse": GOTO exit.grid.movement
 
ELSEIF key$ = "s" THEN
  temp.mode$ = "save.palette": GOTO exit.grid.movement

ELSEIF key$ = "l" THEN
  temp.mode$ = "load.palette": GOTO exit.grid.movement

ELSE BEEP: GOTO exit.grid.movement
END IF

exit.grid.movement:
END SUB

SUB Help
mousehide
LINE (379, 128)-(630, 343), 15, BF
letter.place.x = 382
letter.place.y = 131
help.bypass$ = "yes"

IF keyword$ = "SAVE PALETTE" THEN
  graphic.text$ = "SAVE PALETTE -                          ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "Move left and right using the arrow keys ": TEXT
  graphic.text$ = "or 6 and 4 to change the slot. Press up  ": TEXT
  graphic.text$ = "and down using the arrow keys or 8 and 2 ": TEXT
  graphic.text$ = "to change the color for that slot. Press ": TEXT
  graphic.text$ = "enter when finished and type out a max-  ": TEXT
  graphic.text$ = "imum 8 character filename to save the    ": TEXT
  graphic.text$ = "palette. Saving it as default will change": TEXT
  graphic.text$ = "the standard palette.                    ": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "LOAD PALETTE" THEN
  graphic.text$ = "LOAD PALETTE -                          ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "Type in the filename of the palette you  ": TEXT
  graphic.text$ = "wish to load and press enter when done.  ": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "HELP" OR keyword$ = "F1" THEN
  graphic.text$ = "help -                                   ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "to find more detailed information on any ": TEXT
  graphic.text$ = "subject press f1 while in the keyboard   ": TEXT
  graphic.text$ = "mode or left click on the help icon while": TEXT
  graphic.text$ = "in the mouse mode. you then type in the  ": TEXT
  graphic.text$ = "keyword of the topic you wish to learn   ": TEXT
  graphic.text$ = "more about and press enter when finished.": TEXT
  graphic.text$ = "if there is no information on that topic ": TEXT
  graphic.text$ = "a list of keywords will appear to aid you": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "PEN UP" OR keyword$ = "F2" THEN
  graphic.text$ = "pen up -                                 ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "keyboard:                                ": TEXT
  graphic.text$ = "to access this mode press f2 or while in ": TEXT
  graphic.text$ = "the pen down mode press the space bar.   ": TEXT
  graphic.text$ = "use the number pad or the arrow keys to  ": TEXT
  graphic.text$ = "move the square cursor around the screen.": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "mouse:                                   ": TEXT
  graphic.text$ = "while using the mouse the draw mode auto-": TEXT
  graphic.text$ = "matically defaults back to this mode     ": TEXT
  graphic.text$ = "after exiting another mode.              ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "note: left clicking on the pen up icon   ": TEXT
  graphic.text$ = "does nothing because the mouse defaults  ": TEXT
  graphic.text$ = "back to this mode.                       ": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "PEN DOWN" OR keyword$ = "F3" THEN
  graphic.text$ = "pen down -                               ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "keyboard:                                ": TEXT
  graphic.text$ = "to access this mode press f3 or while in ": TEXT
  graphic.text$ = "the pen up mode press the space bar. Use ": TEXT
  graphic.text$ = "the number pad or the arrow keys to draw.": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "mouse:                                   ": TEXT
  graphic.text$ = "while on the grid left click to draw.    ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "note: left clicking on the pen down icon ": TEXT
  graphic.text$ = "does nothing because the mouse will go   ": TEXT
  graphic.text$ = "into this mode when left clicking any-   ": TEXT
  graphic.text$ = "where on the grid.                       ": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "SINGLE PIXEL" OR keyword$ = "F4" THEN
  graphic.text$ = "single pixel -                           ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "keyboard:                                ": TEXT
  graphic.text$ = "to access this mode press f4. Use the    ": TEXT
  graphic.text$ = "number pad or the arrow keys to move the ": TEXT
  graphic.text$ = "square cursor around the screen. press   ": TEXT
  graphic.text$ = "the space bar to draw a single pixel.    ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "note: this drawing mode is only available": TEXT
  graphic.text$ = "while using the keyboard.                ": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "CHANGE COLOR" OR keyword$ = "F5" THEN
  graphic.text$ = "change color -                           ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "keyboard:                                ": TEXT
  graphic.text$ = "to access this mode press f5. Use the    ": TEXT
  graphic.text$ = "number pad or the arrow keys to change   ": TEXT
  graphic.text$ = "the color. Press the space bar to select ": TEXT
  graphic.text$ = "the color.                               ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "mouse:                                   ": TEXT
  graphic.text$ = "to change the color left click on any of ": TEXT
  graphic.text$ = "the colors to the right of the grid.     ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "note: left clicking on the color change  ": TEXT
  graphic.text$ = "icon will do nothing since you are able  ": TEXT
  graphic.text$ = "to change the color by doing the above. ": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "SWITCH COLOR" OR keyword$ = "F6" THEN
  graphic.text$ = "switch color -                           ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "keyboard:                                ": TEXT
  graphic.text$ = "to access this mode press f6. use the    ": TEXT
  graphic.text$ = "number pad or the arrow keys to change   ": TEXT
  graphic.text$ = "the color. press the space bar to select ": TEXT
  graphic.text$ = "the color to be changed. follow the same ": TEXT
  graphic.text$ = "process to switch the original color to  ": TEXT
  graphic.text$ = "newly selected color.                    ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "mouse:                                   ": TEXT
  graphic.text$ = "first left click on the switch color icon": TEXT
  graphic.text$ = "to the left of the grid. then left click ": TEXT
  graphic.text$ = "on the color you wish to change. Finally ": TEXT
  graphic.text$ = "left click on the color you wish to      ": TEXT
  graphic.text$ = "change the previously selected color to. ": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "PAINT BACKGROUND" OR keyword$ = "F7" THEN
  graphic.text$ = "paint background -                       ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "keyboard:                                ": TEXT
  graphic.text$ = "to access this mode press f7. use the    ": TEXT
  graphic.text$ = "number pad or the arrow keys to change   ": TEXT
  graphic.text$ = "the color. press space to select the     ": TEXT
  graphic.text$ = "background color.                        ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "mouse:                                   ": TEXT
  graphic.text$ = "first left click on the paint background ": TEXT
  graphic.text$ = "icon to the left of the grid. Next left  ": TEXT
  graphic.text$ = "click on the color you wish to paint the ": TEXT
  graphic.text$ = "background with.                         ": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "PAINT SCREEN" OR keyword$ = "F8" THEN
  graphic.text$ = "paint screen -                           ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "keyboard:                                ": TEXT
  graphic.text$ = "to access this mode press f8. use the    ": TEXT
  graphic.text$ = "number pad or the arrow keys to change   ": TEXT
  graphic.text$ = "the color. press space to select the     ": TEXT
  graphic.text$ = "color to paint the screen with.          ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "mouse:                                   ": TEXT
  graphic.text$ = "first left click on the paint screen icon": TEXT
  graphic.text$ = "to the left of the grid. left click on   ": TEXT
  graphic.text$ = "the color you wish to paint the entire   ": TEXT
  graphic.text$ = "screen with.                             ": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "CONVERGE" OR keyword$ = "F9" THEN
  graphic.text$ = "converge -                               ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "keyboard or mouse:                       ": TEXT
  graphic.text$ = "to access this mode press f9 or left     ": TEXT
  graphic.text$ = "click on the converge icon. First type   ": TEXT
  graphic.text$ = "in the filename of the foreground tile.  ": TEXT
  graphic.text$ = "confirm that it is correct. Next type in ": TEXT
  graphic.text$ = "the filename of the background tile. con-": TEXT
  graphic.text$ = "firm that it is correct. After the two   ": TEXT
  graphic.text$ = "tiles are converged type in the filename ": TEXT
  graphic.text$ = "of the newly converged tile in the save- ": TEXT
  graphic.text$ = "load window.                             ": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "NEW TILE" OR keyword$ = "NEW" OR keyword$ = "F10" THEN
  graphic.text$ = "new tile -                               ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "keyboard:                                ": TEXT
  graphic.text$ = "to access this mode press f10. this mode ": TEXT
  graphic.text$ = "resets the variables values to their     ": TEXT
  graphic.text$ = "defaults. then the screen is restored to ": TEXT
  graphic.text$ = "it's initial setup.                      ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "mouse:                                   ": TEXT
  graphic.text$ = "left click on the new tile icon to the   ": TEXT
  graphic.text$ = "left of the grid. this mode resets var-  ": TEXT
  graphic.text$ = "iables values to their defaults. then the": TEXT
  graphic.text$ = "screen is restored to it's initial setup.": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "SAVE TILE" OR keyword$ = "SAVE" OR keyword$ = "F11" THEN
  graphic.text$ = "save tile -                              ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "keyboard or mouse:                       ": TEXT
  graphic.text$ = "left click on the save icon to the left  ": TEXT
  graphic.text$ = "of the grid or press f11. type in the    ": TEXT
  graphic.text$ = "filename of what you wish to save the    ": TEXT
  graphic.text$ = "tile as. Confirm that it is correct.     ": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "LOAD TILE" OR keyword$ = "LOAD" OR keyword$ = "OPEN" OR keyword$ = "F12" THEN
  graphic.text$ = "load tile -                              ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "keyboard or mouse:                       ": TEXT
  graphic.text$ = "left click on the load icon to the left  ": TEXT
  graphic.text$ = "of the grid or press f12. type in the    ": TEXT
  graphic.text$ = "filename of the tile you wish to load.   ": TEXT
  graphic.text$ = "confirm that it is correct.              ": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "QUIT" OR keyword$ = "ESC" OR keyword$ = "ESCAPE" OR keyword$ = "EXIT" THEN
  graphic.text$ = "quit -                                   ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "keyboard:                                ": TEXT
  graphic.text$ = "to exit tiled X alpha version press the  ": TEXT
  graphic.text$ = "esc key while in any drawing mode        ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "mouse:                                   ": TEXT
  graphic.text$ = "to exit tiled X alpha version left click ": TEXT
  graphic.text$ = "on the close button.                     ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "note: while in the mouse mode you will be": TEXT
  graphic.text$ = "sent to certain sections of the keyboard ": TEXT
  graphic.text$ = "mode like save, load, help, and converge ": TEXT
  graphic.text$ = "which will require you to press esc to   ": TEXT
  graphic.text$ = "quit.                                    ": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "ABOUT CONVERGE" THEN
  graphic.text$ = "about converge -                         ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "converge was initially designed as a tot-": TEXT
  graphic.text$ = "ally seperate program that was to become ": TEXT
  graphic.text$ = "part of the gamers programming package.  ": TEXT
  graphic.text$ = "however due to lack of commitment on all ": TEXT
  graphic.text$ = "of us the vision had never seen the light": TEXT
  graphic.text$ = "of day. now converge has been built dir- ": TEXT
  graphic.text$ = "ectly into Tiled itself. converge's main ": TEXT
  graphic.text$ = "reason of being programmed is just that  ": TEXT
  graphic.text$ = "of simplisity. back in the day when I    ": TEXT
  graphic.text$ = "first started programming I didn't want  ": TEXT
  graphic.text$ = "to mess around with that whole mask deal ": TEXT
  graphic.text$ = "mainly because I had no clue as to why it": TEXT
  graphic.text$ = "would be used. so one day I was thinking ": TEXT
  graphic.text$ = "transparency and new colors had just been": TEXT
  graphic.text$ = "born. each mode now has an additional    ": TEXT
  graphic.text$ = "color. this makes it easier for copying  ": TEXT
  graphic.text$ = "one picture on top of various others very": TEXT
  graphic.text$ = "easily. so there you have it - converge  ": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "ABOUT TILEDX" OR keyword$ = "ABOUT TILED" THEN
  graphic.text$ = "about tiledX -                           ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "tiledX is the new generation of Dimension": TEXT
  graphic.text$ = "SOFT tile editors. It is the very first  ": TEXT
  graphic.text$ = "tile editor to use both a mouse and a key": TEXT
  graphic.text$ = "board. It is the very first to use the   ": TEXT
  graphic.text$ = "transparent color, color 16. this version": TEXT
  graphic.text$ = "strives to set a new standard in qbasic. ": TEXT
  graphic.text$ = "user friendly in design. versatility was ": TEXT
  graphic.text$ = "another key point made with this program.": TEXT
  graphic.text$ = "for instance leave the mouse on an icon  ": TEXT
  graphic.text$ = "or right click on that exact icon to see ": TEXT
  graphic.text$ = "what happens. You can now print out the  ": TEXT
  graphic.text$ = "help for the selected keyword. Also just ": TEXT
  graphic.text$ = "about every graphical glitch has been    ": TEXT
  graphic.text$ = "fixed. Newer releases will be released   ": TEXT
  graphic.text$ = "when significant changes have been added.": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "DIMENSION SOFT" THEN
  graphic.text$ = "dimension soft -                         ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "now a two man operation since the con-   ": TEXT
  graphic.text$ = "coneption of Chronogeddon. We will try to": TEXT
  graphic.text$ = "continue to bring utility programs along ": TEXT
  graphic.text$ = "with origianl games to the qb community. ": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "COLOR 16" THEN
  graphic.text$ = "color 16 -                               ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "an idea brought to you first by Dimension": TEXT
  graphic.text$ = "soft. it's sole purpose is for combining ": TEXT
  graphic.text$ = "tiles in converge. I got sick of typing  ": TEXT
  graphic.text$ = "the same code over and over so I just    ": TEXT
  graphic.text$ = "programmed an additional color that will ": TEXT
  graphic.text$ = "appear black but when used in converge   ": TEXT
  graphic.text$ = "takes on the color of the tile below it. ": TEXT
  graphic.text$ = "tiles can only be converged in tiledx.   ": TEXT
  graphic.text$ = "tiles with color 16 will be seen as being": TEXT
  graphic.text$ = "black outside of this program.           ": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "QUITING MODES" THEN
  graphic.text$ = "quiting modes -                          ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "to exit most modes just press esc or     ": TEXT
  graphic.text$ = "right click.                             ": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "FUTURE PROJECTS" THEN
  graphic.text$ = "future projects -                        ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "presently the most important project is  ": TEXT
  graphic.text$ = "what you see before you. after this is   ": TEXT
  graphic.text$ = "totally complete I will be moving on to  ": TEXT
  graphic.text$ = "either one of two things. I will either  ": TEXT
  graphic.text$ = "start my rpg that I have sitting on the  ": TEXT
  graphic.text$ = "shelves or begin working on windows for  ": TEXT
  graphic.text$ = "qbasic. I haven't decided yet. the rpg as": TEXT
  graphic.text$ = "of right now is based a lot on chrono-   ": TEXT
  graphic.text$ = "trigger for the snes and shows promise.  ": TEXT
  graphic.text$ = "another game that I have been thinking of": TEXT
  graphic.text$ = "starting is based on the movie hackers   ": TEXT
  graphic.text$ = "but will have a neat twist to it - mainly": TEXT
  graphic.text$ = "the idea that the game progresses depend-": TEXT
  graphic.text$ = "ing on what time of the day you are play-": TEXT
  graphic.text$ = "ing. that is something totally new to qb.": TEXT
  graphic.text$ = "the other is windows for qbasic. I'm not ": TEXT
  graphic.text$ = "going to try to copy it exactly. it isn't": TEXT
  graphic.text$ = "what is needed. I plan on making it work ": TEXT
  graphic.text$ = "for qbasic not work for itself.          ": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "RPG" THEN
  graphic.text$ = "Role Playing Game -                      ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "tentatively called chronogeddon. it's a  ": TEXT
  graphic.text$ = "save the world alter time theme and will ": TEXT
  graphic.text$ = "have a very involved story while trying  ": TEXT
  graphic.text$ = "to stay away from it being cheesy. it has": TEXT
  graphic.text$ = "a totally new type of format in compar-  ": TEXT
  graphic.text$ = "ison to other qb rpg's. it advances      ": TEXT
  graphic.text$ = "screen by screen and is multi-tiered much": TEXT
  graphic.text$ = "like that of zelda. it's battle scenes   ": TEXT
  graphic.text$ = "are that of final fantasy. it should have": TEXT
  graphic.text$ = "all the wonderful spells and special     ": TEXT
  graphic.text$ = "effects like everything else. it will be ": TEXT
  graphic.text$ = "screen 12 but I think we can offset the  ": TEXT
  graphic.text$ = "lack of colors with crisper graphics. the": TEXT
  graphic.text$ = "tiles are totally drawn in Tiledx so no  ": TEXT
  graphic.text$ = "suprises with bmp's or pcx's. I'll keep  ": TEXT
  graphic.text$ = "you posted as more developes. Also       ": TEXT
  graphic.text$ = "Dimension soft would like to welcome it's": TEXT
  graphic.text$ = "second member jay mccarthy. he helped    ": TEXT
  graphic.text$ = "make tiledx possible. give thanks to jay.": TEXT
  GOTO Exit.Help
END IF
IF keyword$ = "SCREEN SAVER" THEN
CLS
DO:
  RANDOMIZE TIMER
  a = INT(RND * 640) + 1
  b = INT(RND * 370) + 1
  c = INT(RND * 100) + 1
  d = INT(RND * 16) + 1
  CIRCLE (a, b), c, d
  LOCATE 29, 1: PRINT ""
  sleep$ = INKEY$
LOOP WHILE sleep$ = ""
Exit.Tiled
END IF
  graphic.text$ = "keywords -                               ": TEXT
  graphic.text$ = "                                         ": TEXT
  graphic.text$ = "pen up                  load             ": TEXT
  graphic.text$ = "pen down                quit             ": TEXT
  graphic.text$ = "single pixel            quiting modes    ": TEXT
  graphic.text$ = "change color            about tiledx     ": TEXT
  graphic.text$ = "switch color            about converge   ": TEXT
  graphic.text$ = "paint background        color 16         ": TEXT
  graphic.text$ = "paint screen            dimension soft   ": TEXT
  graphic.text$ = "new                     future projects  ": TEXT
  graphic.text$ = "save                    rpg              ": TEXT
  graphic.text$ = "save palette            load palette     ": TEXT
Exit.Help:
help.bypass$ = "no"
LINE (x, y)-(x + 8, y + 8), 15, B
mouseshow
oxmouse% = xmouse%
oymouse% = ymouse%
graphic.text$ = "press p or left click on the print icon to print out the  "
Text.Line1
graphic.text$ = "current help query.                                       "
Text.Line2
IF draw.mode$ = "mouse" THEN mouseshow
DO
  key$ = INKEY$
  mousestatus lb%, rb%, xmouse%, ymouse%
  IF key$ = "p" OR key$ = "P" OR lb% = -1 AND xmouse% > 597 AND xmouse% < 631 AND ymouse% > 113 AND ymouse% < 126 THEN
    mousehide
    LINE (598, 114)-(598, 124), 8
    LINE (598, 114)-(630, 114), 8
    LINE (599, 124)-(630, 124), 15
    LINE (630, 115)-(630, 124), 15
    Print.Help
  END IF
LOOP UNTIL key$ <> "" OR lb% = -1 OR rb% = -1
IF draw.mode$ = "mouse" THEN
  mouseshow
  draw.mode$ = "mouse"
  ELSE mousehide
    draw.mode$ = "penup"
    LINE (x, y)-(x + 8, y + 8), 0, B
END IF
'Erase.Windows
END SUB

SUB Icon.Normal
icon.y = (icon.number * 20) + 8
LINE (6, icon.y)-(25, icon.y), 15
LINE (6, icon.y)-(6, icon.y + 19), 15
LINE (7, icon.y + 19)-(25, icon.y + 19), 8
LINE (25, icon.y + 19)-(25, icon.y + 1), 8
END SUB

SUB Icon.Pressed
icon.y = (icon.number * 20) + 8
LINE (6, icon.y)-(25, icon.y), 8
LINE (6, icon.y)-(6, icon.y + 19), 8
LINE (7, icon.y + 19)-(25, icon.y + 19), 15
LINE (25, icon.y + 19)-(25, icon.y + 1), 15
END SUB

SUB Icon.Routine
text.length = LEN(graphic.text$)
icon.x = (text.length * 6) + 28
mousehide
GET (26, icon.y)-(166, icon.y + 10), tiled%
LINE (26, icon.y)-(icon.x, icon.y + 10), 15, BF
LINE (26, icon.y)-(icon.x, icon.y + 10), 0, B
letter.place.x = 28
letter.place.y = icon.y + 2
TEXT
DO
  mousestatus lb%, rb%, xmouse%, ymouse%
LOOP WHILE xmouse% = oxmouse% AND ymouse% = oymouse% AND lb% <> -1 AND rb% <> -1
PUT (26, icon.y), tiled%, PSET
mouseshow
END SUB

SUB Mirror.Left.Right
old.a% = a%
startrow = 39
i% = 0

DO
   FOR a% = startrow TO (startrow - 19) STEP -1
      tiled%(i%) = tile%(a%)
      i% = i% + 1
   NEXT a%
   startrow = startrow + 40
   i% = i% + 20
LOOP UNTIL startrow = 1639

startrow = 0
i% = 39

DO
   FOR a% = startrow TO startrow + 19
      tiled%(i%) = tile%(a%)
      i% = i% - 1
   NEXT a%
   startrow = startrow + 40
   i% = i% + 60
LOOP UNTIL startrow = 1600

i% = 0
FOR a% = 0 TO 1600
  tile%(a%) = tiled%(i%)
  i% = i% + 1
NEXT a%

a% = old.a%
END SUB

SUB Mirror.Up.Down
old.a% = a%
startcol = 1560
i% = 0

DO
   FOR a% = startcol TO (startcol - 760) STEP -40
      tiled%(i%) = tile%(a%)
      i% = i% + 40
   NEXT a%
   startcol = startcol + 1
   i% = i% - 799
LOOP UNTIL startcol = 1600

startcol = 0
i% = 1560

DO
   FOR a% = startcol TO (startcol + 760) STEP 40
      tiled%(i%) = tile%(a%)
      i% = i% - 40
   NEXT a%
   startcol = startcol + 1
   i% = i% + 801
LOOP UNTIL startcol = 40

i% = 0
FOR a% = 0 TO 1600
  tile%(a%) = tiled%(i%)
  i% = i% + 1
NEXT a%

a% = old.a%
END SUB

SUB Mode
letter.place.x = 72
letter.place.y = 374
Color$ = ""
TEXT
END SUB

SUB MouseDriver (AX%, bx%, CX%, DX%)
  DEF SEG = VARSEG(mouse$)
  mouse% = SADD(mouse$)
  CALL Absolute(AX%, bx%, CX%, DX%, mouse%)
END SUB

SUB mousehide
 AX% = 2
 MouseDriver AX%, 0, 0, 0
END SUB

FUNCTION MouseInit%
  AX% = 0
  MouseDriver AX%, 0, 0, 0
  MouseInit% = AX%
END FUNCTION

SUB MousePut
  AX% = 4
  CX% = x%
  DX% = y%
  MouseDriver AX%, 0, CX%, DX%
END SUB

SUB mouseshow
  AX% = 1
  MouseDriver AX%, 0, 0, 0
END SUB

SUB mousestatus (lb%, rb%, xmouse%, ymouse%)
  AX% = 3
  MouseDriver AX%, bx%, CX%, DX%
  lb% = ((bx% AND 1) <> 0)
  rb% = ((bx% AND 2) <> 0)
  xmouse% = CX%
  ymouse% = DX%
END SUB

SUB Palette.Save.Load
number = 0
t% = c%
temp% = 0
FOR c% = 0 TO 16
   temp(temp%) = pal(c%)
   temp% = temp% + 1
NEXT c%

FOR temp.y = 100 TO 205 STEP 40
  FOR temp.x = 50 TO 300 STEP 40
     number$ = STR$(number)
     GET (temp.x, temp.y)-(temp.x + 39, temp.y + 39), tiled%
     DEF SEG = VARSEG(tiled%(0))
     BSAVE pathname$ + "temp" + number$ + ".tmp", 0, 1600
     DEF SEG
     number = number + 1
  NEXT temp.x
NEXT temp.y

LINE (50, 100)-(300, 205), 7, BF
LINE (50, 100)-(50, 205), 15
LINE (50, 100)-(300, 100), 15
LINE (51, 205)-(300, 205), 8
LINE (300, 205)-(300, 101), 8

LINE (53, 103)-(297, 113), 1, BF

LINE (53, 119)-(53, 201), 15
LINE (54, 118)-(174, 118), 15
LINE (175, 119)-(175, 129), 8
LINE (176, 119)-(176, 129), 15
LINE (177, 118)-(296, 118), 15
LINE (297, 119)-(297, 201), 8
LINE (296, 202)-(54, 202), 8
LINE (176, 130)-(296, 130), 15

DEF SEG = VARSEG(windowtext(0))
BLOAD pathname$ + "save.wtx", 0
DEF SEG
PUT (102, 121), windowtext, PSET

DEF SEG = VARSEG(windowtext(0))
BLOAD pathname$ + "load.wtx", 0
DEF SEG
PUT (223, 121), windowtext, PSET

DEF SEG = VARSEG(windowtext(0))
BLOAD pathname$ + "palette.wtx", 0
DEF SEG
PUT (55, 105), windowtext, PSET

FOR box = 56 TO 285 STEP 14
  LINE (box, 135)-(box + 14, 149), 15, B
  LINE (box + 1, 136)-(box + 13, 148), 0, BF
NEXT box

c% = 0
FOR paint.box = 57 TO 286 STEP 14
  PAINT (paint.box, 140), pal(c%), 15
  c% = c% + 1
NEXT paint.box

DEF SEG = VARSEG(windowtext(0))
BLOAD pathname$ + "filename.wtx", 0
DEF SEG
PUT (56, 158), windowtext, PSET

LINE (108, 156)-(293, 166), 15, BF
LINE (108, 156)-(108, 165), 8
LINE (108, 156)-(292, 156), 8

LINE (56, 170)-(293, 198), 15, BF
LINE (56, 170)-(56, 197), 8
LINE (56, 170)-(292, 170), 8
IF temp.mode$ = "save.palette" THEN GOTO save.palette
IF temp.mode$ = "load.palette" THEN GOTO Load.Palette

'*****************************************************************************
'This section handles all of the Save feature for keyboard
'*****************************************************************************
save.palette:

LINE (176, 130)-(54, 130), 7
LINE (176, 130)-(296, 130), 15
graphic.text$ = "to change the slot press left or right."
letter.place.x = 58
letter.place.y = 172
TEXT
graphic.text$ = "to change the color press up or down.  "
letter.place.x = 58
letter.place.y = 181
TEXT
graphic.text$ = "press enter or space when finished.    "
letter.place.x = 58
letter.place.y = 190
TEXT
c% = 0
box = 56

Palette.Loop:
LINE (box, 135)-(box + 14, 149), 0, B
pal.color = pal(c%)
IF pal.color <> 16 THEN
  LINE (box + 1, 136)-(box + 13, 148), pal.color, BF
  ELSEIF pal.color = 16 THEN
    LINE (box + 1, 136)-(box + 13, 148), 0, BF
    LINE (box + 2, 137)-(box + 12, 147), 15
    LINE (box + 2, 147)-(box + 12, 137), 15
END IF

DO
key$ = INKEY$
LOOP WHILE key$ = ""

LINE (box, 135)-(box + 14, 149), 15, B
IF key$ = CHR$(0) + "K" AND c% = 0 OR key$ = "4" AND c% = 0 THEN
  box = 280
  c% = 16
  GOTO Palette.Loop
ELSEIF key$ = CHR$(0) + "M" AND c% = 16 OR key$ = "6" AND c% = 16 THEN
  box = 56
  c% = 0
  GOTO Palette.Loop
ELSEIF key$ = CHR$(0) + "K" OR key$ = "4" THEN
  box = box - 14
  c% = c% - 1
  GOTO Palette.Loop
ELSEIF key$ = CHR$(0) + "M" OR key$ = "6" THEN
  box = box + 14
  c% = c% + 1
  GOTO Palette.Loop
ELSEIF key$ = CHR$(0) + "P" AND pal.color = 0 OR key$ = "2" AND pal.color = 0 THEN
  pal.color = 16
  pal(c%) = pal.color
  GOTO Palette.Loop
ELSEIF key$ = CHR$(0) + "H" AND pal.color = 16 OR key$ = "8" AND pal.color = 16 THEN
  pal.color = 0
  pal(c%) = pal.color
  GOTO Palette.Loop
ELSEIF key$ = CHR$(0) + "P" OR key$ = "2" THEN
  pal.color = pal.color - 1
  pal(c%) = pal.color
  GOTO Palette.Loop
ELSEIF key$ = CHR$(0) + "H" OR key$ = "8" THEN
  pal.color = pal.color + 1
  pal(c%) = pal.color
  GOTO Palette.Loop
ELSEIF key$ = CHR$(13) OR key$ = CHR$(32) THEN
  GOTO Palette.Save
ELSEIF key$ = CHR$(27) THEN
  Exit$ = "yes"
  GOTO exit.palette
ELSE GOTO Palette.Loop
END IF

Palette.Save:
graphic.text$ = "type in the eight character filename of"
letter.place.x = 58
letter.place.y = 172
TEXT
graphic.text$ = "the palette to be saved.               "
letter.place.x = 58
letter.place.y = 181
TEXT
graphic.text$ = "                                       "
letter.place.x = 58
letter.place.y = 190
TEXT
x.position = 110
y.position = 158
minimum = 110
max.length = 8
limit$ = "yes"
Get.Input
palette.name$ = pathname$ + keyword$ + ".pal"
OPEN palette.name$ FOR OUTPUT AS #1
  FOR c% = 0 TO 16
    PRINT #1, pal(c%)
  NEXT c%
CLOSE #1
c% = 0
FOR y.pos = 35 TO 335 STEP 18
   LINE (352, y.pos)-(370, y.pos + 18), 15, B
   IF pal(c%) = 16 THEN
     LINE (353, y.pos + 1)-(369, y.pos + 17), 0, BF
     LINE (354, y.pos + 2)-(368, y.pos + 16), 15
     LINE (354, y.pos + 16)-(368, y.pos + 2), 15
   END IF
   IF pal(c%) < 16 THEN LINE (353, y.pos + 1)-(369, y.pos + 17), pal(c%), BF
   c% = c% + 1
NEXT y.pos
GOTO exit.palette

'*****************************************************************************
'This section loads the palette
'*****************************************************************************

Load.Palette:
LINE (176, 130)-(296, 130), 7
LINE (176, 130)-(54, 130), 15
graphic.text$ = "type in the eight character filename of"
letter.place.x = 58
letter.place.y = 172
TEXT
graphic.text$ = "the palette to be loaded.              "
letter.place.x = 58
letter.place.y = 181
TEXT
graphic.text$ = "                                       "
letter.place.x = 58
letter.place.y = 190
TEXT
x.position = 110
y.position = 158
minimum = 110
max.length = 8
limit$ = "yes"
Get.Input
palette.name$ = pathname$ + keyword$ + ".pal"
IF Exit$ = "yes" THEN GOTO exit.palette

OPEN palette.name$ FOR INPUT AS #1
  FOR c% = 0 TO 16
    INPUT #1, pal(c%)
  NEXT c%
CLOSE #1

c% = 0
FOR y.pos = 35 TO 335 STEP 18
   LINE (352, y.pos)-(370, y.pos + 18), 15, B
   IF pal(c%) = 16 THEN
     LINE (353, y.pos + 1)-(369, y.pos + 17), 0, BF
     LINE (354, y.pos + 2)-(368, y.pos + 16), 15
     LINE (354, y.pos + 16)-(368, y.pos + 2), 15
   END IF
   IF pal(c%) < 16 THEN LINE (353, y.pos + 1)-(369, y.pos + 17), pal(c%), BF
   c% = c% + 1
NEXT y.pos
GOTO exit.palette

'*****************************************************************************
'This section erases the window
'*****************************************************************************

exit.palette:

IF Exit$ = "yes" THEN
  FOR c% = 0 TO 16
    pal(c%) = temp(temp%)
    temp% = temp% + 1
  NEXT c%
END IF

number = 0
FOR temp.y = 100 TO 205 STEP 40
  FOR temp.x = 50 TO 300 STEP 40
     number$ = STR$(number)
     DEF SEG = VARSEG(tiled%(0))
     BLOAD pathname$ + "temp" + number$ + ".tmp", 0
     DEF SEG
     PUT (temp.x, temp.y), tiled%, PSET
     KILL pathname$ + "temp" + number$ + ".tmp"
     number = number + 1
  NEXT temp.x
NEXT temp.y
temp.mode$ = ""
c% = t%
Exit$ = "no"
LINE (352, cby)-(370, cby + 18), 0, B
END SUB

SUB Print.Help
FOR pause = 1 TO 10000
NEXT pause
LINE (598, 114)-(598, 124), 15
LINE (598, 114)-(630, 114), 15
LINE (599, 124)-(630, 124), 8
LINE (630, 115)-(630, 124), 8
mouseshow
  LPRINT "*******************************************"
  LPRINT "*Dimension SOFT 1998                      *"
  LPRINT "*Chris Rennie                             *"
  LPRINT "*******************************************"
  LPRINT "*                                         *"
IF keyword$ = "SAVE PALETTE" THEN
  LPRINT "* SAVE PALETTE -                          *"
  LPRINT "*                                         *"
  LPRINT "*Move left and right using the arrow keys *"
  LPRINT "*or 6 and 4 to change the slot. Press up  *"
  LPRINT "*and down using the arrow keys or 8 and 2 *"
  LPRINT "*to change the color for that slot. Press *"
  LPRINT "*enter when finished and type out a max-  *"
  LPRINT "*imum 8 character filename to save the    *"
  LPRINT "*palette. Saving it as default will change*"
  LPRINT "*the standard palette.                    *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "LOAD PALETTE" THEN
  LPRINT "* LOAD PALETTE -                          *"
  LPRINT "*                                         *"
  LPRINT "*Type in the filename of the palette you  *"
  LPRINT "*wish to load and press enter when done.  *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "HELP" OR keyword$ = "F1" THEN
  LPRINT "* HELP -                                  *"
  LPRINT "*                                         *"
  LPRINT "*To find more detailed information on any *"
  LPRINT "*subject press F1 while in the keyboard   *"
  LPRINT "*mode or left click on the help icon while*"
  LPRINT "*in the mouse mode. You then type in the  *"
  LPRINT "*keyword of the topic you wish to learn   *"
  LPRINT "*more about and press enter when finished.*"
  LPRINT "*If there is no information on that topic *"
  LPRINT "*a list of keywords will appear to aid    *"
  LPRINT "*you.                                     *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "PEN UP" OR keyword$ = "F2" THEN
  LPRINT "* PEN UP -                                *"
  LPRINT "*                                         *"
  LPRINT "*Keyboard:                                *"
  LPRINT "*To access this mode press F2 or while in *"
  LPRINT "*the pen down mode press the space bar.   *"
  LPRINT "*Use the number pad or the arrow keys to  *"
  LPRINT "*move the square cursor around the screen.*"
  LPRINT "*                                         *"
  LPRINT "*Mouse:                                   *"
  LPRINT "*While using the mouse the draw mode auto-*"
  LPRINT "*matically defaults back to this mode     *"
  LPRINT "*after exiting another mode.              *"
  LPRINT "*                                         *"
  LPRINT "*Note: Left clicking on the pen up icon   *"
  LPRINT "*does nothing because the mouse defaults  *"
  LPRINT "*back to this mode.                       *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "PEN DOWN" OR keyword$ = "F3" THEN
  LPRINT "* PEN DOWN -                              *"
  LPRINT "*                                         *"
  LPRINT "*Keyboard:                                *"
  LPRINT "*To access this mode press F3 or while in *"
  LPRINT "*the pen up mode press the space bar. Use *"
  LPRINT "*the number pad or the arrow keys to draw.*"
  LPRINT "*                                         *"
  LPRINT "*Mouse:                                   *"
  LPRINT "*While on the grid left click to draw.    *"
  LPRINT "*                                         *"
  LPRINT "*Note: Left clicking on the pen down icon *"
  LPRINT "*does nothing because the mouse will go   *"
  LPRINT "*into this mode when left clicking any-   *"
  LPRINT "*where on the grid.                       *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "SINGLE PIXEL" OR keyword$ = "F4" THEN
  LPRINT "* SINGLE PIXEL -                          *"
  LPRINT "*                                         *"
  LPRINT "*Keyboard:                                *"
  LPRINT "*To access this mode press F4. Use the    *"
  LPRINT "*number pad or the arrow keys to move the *"
  LPRINT "*square cursor around the screen. Press   *"
  LPRINT "*the space bar to draw a single pixel.    *"
  LPRINT "*                                         *"
  LPRINT "*Note: This drawing mode is only available*"
  LPRINT "*while using the keyboard.                *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "CHANGE COLOR" OR keyword$ = "F5" THEN
  LPRINT "* CHANGE COLOR -                          *"
  LPRINT "*                                         *"
  LPRINT "*Keyboard:                                *"
  LPRINT "*To access this mode press F5. Use the    *"
  LPRINT "*number pad or the arrow keys to change   *"
  LPRINT "*the color. Press the space bar to select *"
  LPRINT "*the color.                               *"
  LPRINT "*                                         *"
  LPRINT "*Mouse:                                   *"
  LPRINT "*To change the color left click on any of *"
  LPRINT "*the colors to the right of the grid.     *"
  LPRINT "*                                         *"
  LPRINT "*Note: Left clicking on the color change  *"
  LPRINT "*icon will do nothing since you are able  *"
  LPRINT "*to change the color by doing the above.  *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "SWITCH COLOR" OR keyword$ = "F6" THEN
  LPRINT "* SWITCH COLOR -                          *"
  LPRINT "*                                         *"
  LPRINT "*Keyboard:                                *"
  LPRINT "*To access this mode press F6. Use the    *"
  LPRINT "*number pad or the arrow keys to change   *"
  LPRINT "*the color. Press the space bar to select *"
  LPRINT "*the color to be changed. Follow the same *"
  LPRINT "*process to switch the original color to  *"
  LPRINT "*newly selected color.                    *"
  LPRINT "*                                         *"
  LPRINT "*Mouse:                                   *"
  LPRINT "*First left click on the switch color icon*"
  LPRINT "*to the left of the grid. Then left click *"
  LPRINT "*on the color you wish to change. Finally *"
  LPRINT "*left click on the color you wish to      *"
  LPRINT "*change the previously selected color to. *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "PAINT BACKGROUND" OR keyword$ = "F7" THEN
  LPRINT "* PAINT BACKGROUND -                      *"
  LPRINT "*                                         *"
  LPRINT "*Keyboard:                                *"
  LPRINT "*To access this mode press F7. Use the    *"
  LPRINT "*number pad or the arrow keys to change   *"
  LPRINT "*the color. Press space to select the     *"
  LPRINT "*background color.                        *"
  LPRINT "*                                         *"
  LPRINT "*Mouse:                                   *"
  LPRINT "*First left click on the paint background *"
  LPRINT "*icon to the left of the grid. Next left  *"
  LPRINT "*click on the color you wish to paint the *"
  LPRINT "*background with.                         *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "PAINT SCREEN" OR keyword$ = "F8" THEN
  LPRINT "* PAINT SCREEN -                          *"
  LPRINT "*                                         *"
  LPRINT "*Keyboard:                                *"
  LPRINT "*To access this mode press F8. Use the    *"
  LPRINT "*number pad or the arrow keys to change   *"
  LPRINT "*the color. Press space to select the     *"
  LPRINT "*color to paint the screen with.          *"
  LPRINT "*                                         *"
  LPRINT "*Mouse:                                   *"
  LPRINT "*First left click on the paint screen icon*"
  LPRINT "*to the left of the grid. Left click on   *"
  LPRINT "*the color you wish to paint the entire   *"
  LPRINT "*screen with.                             *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "CONVERGE" OR keyword$ = "F9" THEN
  LPRINT "* CONVERGE -                              *"
  LPRINT "*                                         *"
  LPRINT "*Keyboard or Mouse:                       *"
  LPRINT "*To access this mode press F9 or left     *"
  LPRINT "*click on the converge icon. First type   *"
  LPRINT "*in the filename of the foreground tile.  *"
  LPRINT "*confirm that it is correct. Next type in *"
  LPRINT "*the filename of the background tile. con-*"
  LPRINT "*firm that it is correct. After the two   *"
  LPRINT "*tiles are converged type in the filename *"
  LPRINT "*of the newly converged tile in the save- *"
  LPRINT "*load window.                             *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "NEW TILE" OR keyword$ = "NEW" OR keyword$ = "F10" THEN
  LPRINT "* NEW TILE -                              *"
  LPRINT "*                                         *"
  LPRINT "*Keyboard:                                *"
  LPRINT "*To access this mode press F10. This mode *"
  LPRINT "*resets the variables values to their     *"
  LPRINT "*defaults. Then the screen is restored to *"
  LPRINT "*it's initial setup.                      *"
  LPRINT "*                                         *"
  LPRINT "*Mouse:                                   *"
  LPRINT "*Left click on the new tile icon to the   *"
  LPRINT "*left of the grid. This mode resets var-  *"
  LPRINT "*iables values to their defaults. Then the*"
  LPRINT "*screen is restored to it's initial setup.*"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "SAVE TILE" OR keyword$ = "SAVE" OR keyword$ = "F11" THEN
  LPRINT "* SAVE TILE -                             *"
  LPRINT "*                                         *"
  LPRINT "*Keyboard or Mouse:                       *"
  LPRINT "*Left click on the save icon to the left  *"
  LPRINT "*of the grid or press F11. Type in the    *"
  LPRINT "*filename of what you wish to save the    *"
  LPRINT "*tile as. Confirm that it is correct.     *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "LOAD TILE" OR keyword$ = "LOAD" OR keyword$ = "OPEN" OR keyword$ = "F12" THEN
  LPRINT "* LOAD TILE -                             *"
  LPRINT "*                                         *"
  LPRINT "*Keyboard or Mouse:                       *"
  LPRINT "*Left click on the load icon to the left  *"
  LPRINT "*of the grid or press F12. Type in the    *"
  LPRINT "*filename of the tile you wish to load.   *"
  LPRINT "*confirm that it is correct.              *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "QUIT" OR keyword$ = "ESC" OR keyword$ = "ESCAPE" OR keyword$ = "EXIT" THEN
  LPRINT "* QUIT -                                  *"
  LPRINT "*                                         *"
  LPRINT "*Keyboard:                                *"
  LPRINT "*To exit tiled X alpha version press the  *"
  LPRINT "*esc key while in any drawing mode.       *"
  LPRINT "*                                         *"
  LPRINT "*Mouse:                                   *"
  LPRINT "*To exit tiled X alpha version left click *"
  LPRINT "*on the close button.                     *"
  LPRINT "*                                         *"
  LPRINT "*Note: While in the mouse mode you will be*"
  LPRINT "*sent to certain sections of the keyboard *"
  LPRINT "*mode like save, load, help, and converge *"
  LPRINT "*which will require you to press esc to   *"
  LPRINT "*quit.                                    *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "ABOUT CONVERGE" THEN
  LPRINT "* ABOUT CONVERGE -                        *"
  LPRINT "*                                         *"
  LPRINT "*Converge was initially designed as a tot-*"
  LPRINT "*ally seperate program that was to become *"
  LPRINT "*part of the gamers programming package.  *"
  LPRINT "*However due to lack of commitment on all *"
  LPRINT "*of us the vision had never seen the light*"
  LPRINT "*of day. Now converge has been built dir- *"
  LPRINT "*ectly into Tiled itself. Converge's main *"
  LPRINT "*reason of being programmed is just that  *"
  LPRINT "*of simplisity. Back in the day when I    *"
  LPRINT "*first started programming I didn't want  *"
  LPRINT "*to mess around with that whole mask deal *"
  LPRINT "*mainly because I had no clue as to why it*"
  LPRINT "*would be used. So one day I was thinking *"
  LPRINT "*transparency and new colors had just been*"
  LPRINT "*born. Each mode now has an additional    *"
  LPRINT "*color. This makes it easier for copying  *"
  LPRINT "*one picture on top of various others very*"
  LPRINT "*easily. So there you have it - converge  *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "ABOUT TILEDX" OR keyword$ = "ABOUT TILED" THEN
  LPRINT "* ABOUT TILEDX -                          *"
  LPRINT "*                                         *"
  LPRINT "*TiledX is the new generation of Dimension*"
  LPRINT "*SOFT tile editors. It is the very first  *"
  LPRINT "*tile editor to use both a mouse and a key*"
  LPRINT "*board. It is the very first to use the   *"
  LPRINT "*transparent color, color 16. This version*"
  LPRINT "*strives to set a new standard in qbasic. *"
  LPRINT "*User friendly in design. Versatility was *"
  LPRINT "*another key point made with this program.*"
  LPRINT "*For instance leave the mouse on an icon  *"
  LPRINT "*or right click on that exact icon to see *"
  LPRINT "*what happens. You can now print out the  *"
  LPRINT "*help for the selected keyword. Also just *"
  LPRINT "*about every graphicl glitch has been     *"
  LPRINT "*fixed. Newer releases will be released   *"
  LPRINT "*when significant changes have been added.*"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "DIMENSION SOFT" THEN
  LPRINT "* DIMENSION SOFT -                        *"
  LPRINT "*                                         *"
  LPRINT "*Now a two man operation since the concep-*"
  LPRINT "*tion of Chronogeddon. We will try to con-*"
  LPRINT "*tinue to bring utility programs along    *"
  LPRINT "*with original games to the qb community. *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "COLOR 16" THEN
  LPRINT "* Color 16 -                              *"
  LPRINT "*                                         *"
  LPRINT "*An idea brought to you first by Dimension*"
  LPRINT "*soft. It's sole purpose is for combining *"
  LPRINT "*tiles in converge. I got sick of typing  *"
  LPRINT "*the same code over and over so I just    *"
  LPRINT "*programmed an additional color that will *"
  LPRINT "*appear black but when used in converge   *"
  LPRINT "*takes on the color of the tile below it. *"
  LPRINT "*Tiles can only be converged in tiledx.   *"
  LPRINT "*Tiles with color 16 will be seen as being*"
  LPRINT "*black outside of this program.           *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "QUITING MODES" THEN
  LPRINT "* QUITING MODES -                         *"
  LPRINT "*                                         *"
  LPRINT "*To exit most modes just press esc or     *"
  LPRINT "*right click.                             *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "FUTURE PROJECTS" THEN
  LPRINT "* FUTURE PROJECTS -                       *"
  LPRINT "*                                         *"
  LPRINT "*Presently the most important project is  *"
  LPRINT "*what you see before you. After this is   *"
  LPRINT "*totally complete I will be moving on to  *"
  LPRINT "*either one of two things. I will either  *"
  LPRINT "*start my rpg that I have sitting on the  *"
  LPRINT "*shelves or begin working on windows for  *"
  LPRINT "*qbasic. I haven't decided yet. The rpg as*"
  LPRINT "*of right now is based a lot on Chrono-   *"
  LPRINT "*trigger for the SNES and shows promise.  *"
  LPRINT "*Another game that I have been thinking of*"
  LPRINT "*starting is based on the movie Hackers   *"
  LPRINT "*but will have a neat twist to it - mainly*"
  LPRINT "*the idea that the game progresses depend-*"
  LPRINT "*ing on what time of the day you are play-*"
  LPRINT "*ing. That is something totally new to qb.*"
  LPRINT "*the other is windows for qbasic. I'm not *"
  LPRINT "*going to try to copy it exactly. It isn't*"
  LPRINT "*what is needed. I plan on making it work *"
  LPRINT "*for qbasic not work for itself.          *"
  GOTO Exit.Print.Help
END IF
IF keyword$ = "RPG" THEN
  LPRINT "* Role Playing Game -                     *"
  LPRINT "*                                         *"
  LPRINT "*Tentatively called Chronogeddon. It's a  *"
  LPRINT "*save the world alter time theme and will *"
  LPRINT "*have a very involved story while trying  *"
  LPRINT "*to stay away from it being cheesy. It has*"
  LPRINT "*a totally new type of format in compar-  *"
  LPRINT "*ison to other qb rpg's. It advances      *"
  LPRINT "*screen by screen and is multi-tiered much*"
  LPRINT "*like that of Zelda. It's battle scenes   *"
  LPRINT "*are that of final fantasy. It should have*"
  LPRINT "*all the wonderful spells and special     *"
  LPRINT "*effects like everything else. It will be *"
  LPRINT "*screen 12 but I think we can offset the  *"
  LPRINT "*lack of colors with crisper graphics. The*"
  LPRINT "*tiles are totally drawn in TiledX so no  *"
  LPRINT "*suprises with bmp's or pcx's. I'll keep  *"
  LPRINT "*you posted as more developes. Also       *"
  LPRINT "*Dimension soft would like to welcome it's*"
  LPRINT "*second member Jay McCarthy. He helped    *"
  LPRINT "*make TiledX possible. Give thanks to Jay.*"
  GOTO Exit.Print.Help
END IF
  LPRINT "* Keywords -                              *"
  LPRINT "*                                         *"
  LPRINT "*PEN UP                  SAVE             *"
  LPRINT "*PEN DOWN                QUIT             *"
  LPRINT "*SINGLE PIXEL            QUITING MODES    *"
  LPRINT "*CHANGE COLOR            ABOUT TILEDX     *"
  LPRINT "*SWITCH COLOR            ABOUT CONVERGE   *"
  LPRINT "*PAINT BACKGROUND        COLOR 16         *"
  LPRINT "*PAINT SCREEN            DIMENSION SOFT   *"
  LPRINT "*NEW                     FUTURE PROJECTS  *"
  LPRINT "*LOAD                    RPG              *"
  LPRINT "*SAVE PALETTE            LOAD PALETTE     *"
Exit.Print.Help:
help.bypass$ = "no"
LPRINT "*******************************************"
END SUB

SUB Rotate.Left
old.a% = a%
startcol = 39
i% = 0
endcol = startcol - 39

DO
   FOR a% = startcol TO endcol STEP -1
      tiled%(i%) = tile%(a%)
      i% = i% + 40
   NEXT a%
   startcol = startcol + 40
   i% = i% - 1599
   endcol = startcol - 39
LOOP UNTIL startcol = 1639

i% = 0
FOR a% = 0 TO 1600
  tile%(a%) = tiled%(i%)
  i% = i% + 1
NEXT a%

a% = old.a%
END SUB

SUB Rotate.Right
old.a% = a%
startcol = 1560
i% = 0
endcol = startcol - 1560

DO
   FOR a% = startcol TO endcol STEP -40
      tiled%(i%) = tile%(a%)
      i% = i% + 1
   NEXT a%
   startcol = startcol + 1
   endcol = startcol - 1560
LOOP UNTIL startcol = 1600

i% = 0
FOR a% = 0 TO 1600
  tile%(a%) = tiled%(i%)
  i% = i% + 1
NEXT a%

a% = old.a%
END SUB

SUB Save.Load.Text1
letter.place.x = 379
letter.place.y = 73
TEXT
END SUB

SUB Save.Load.Text2
letter.place.x = 379
letter.place.y = 82
TEXT
END SUB

SUB Screen.Redraw

old.a% = a%
a% = 0
FOR colory = 29 TO 341 STEP 8
  FOR colorx = 30 TO 342 STEP 8
     IF tile%(a%) = 16 THEN LINE (colorx, colory)-(colorx + 6, colory + 6), 0, BF: LINE (colorx + 1, colory + 1)-(colorx + 5, colory + 5), 15: LINE (colorx + 1, colory + 5)-(colorx + 5, colory + 1), 15
     IF tile%(a%) <> 16 THEN LINE (colorx, colory)-(colorx + 6, colory + 6), tile%(a%), BF
     a% = a% + 1
  NEXT colorx
NEXT colory

a% = 0
FOR boxy = 357 TO 396
  FOR boxx = 324 TO 363
     IF tile%(a%) = 16 THEN PSET (boxx, boxy), 0
     IF tile%(a%) <> 16 THEN PSET (boxx, boxy), tile%(a%)
     a% = a% + 1
  NEXT boxx
NEXT boxy
a% = old.a%
END SUB

SUB Screen.Setup
CLS : SCREEN 12

'Main Window

LINE (0, 0)-(639, 459), 7, BF
LINE (0, 0)-(0, 459), 15
LINE (0, 0)-(639, 0), 15
LINE (1, 459)-(639, 459), 8
LINE (639, 1)-(639, 459), 8

LINE (3, 3)-(636, 13), 1, BF
LINE (3, 25)-(635, 25), 8
LINE (3, 25)-(3, 456), 8
LINE (4, 456)-(636, 456), 15
LINE (636, 456)-(636, 26), 15

LINE (29, 28)-(349, 348), 0, BF

FOR y = 28 TO 340 STEP 8
  FOR x = 29 TO 341 STEP 8
    LINE (x, y)-(x + 8, y + 8), 15, B
    LINE (x + 2, y + 2)-(x + 6, y + 6), 15
    LINE (x + 2, y + 6)-(x + 6, y + 2), 15
  NEXT x
NEXT y

OPEN pathname$ + "default.pal" FOR INPUT AS #1
  FOR c% = 0 TO 16
    INPUT #1, pal(c%)
  NEXT
CLOSE #1

c% = 0
FOR y = 35 TO 335 STEP 18
   LINE (352, y)-(370, y + 18), 15, B
   IF pal(c%) = 16 THEN
     PAINT (353, y + 9), 0, 15
     LINE (354, y + 2)-(368, y + 16), 15
     LINE (354, y + 16)-(368, y + 2), 15
   END IF
   IF pal(c%) < 16 THEN PAINT (353, y + 1), pal(c%), 15
   c% = c% + 1
NEXT y

'Draws the update box
LINE (6, 351)-(6, 453), 15
LINE (6, 351)-(370, 351), 15
LINE (7, 453)-(370, 453), 8
LINE (370, 453)-(370, 352), 8
LINE (363, 357)-(324, 396), 0, BF

'This section draws the converge window

LINE (373, 351)-(373, 453), 15
LINE (373, 351)-(633, 351), 15
LINE (374, 453)-(633, 453), 8
LINE (633, 453)-(633, 352), 8
LINE (376, 354)-(630, 364), 1, BF

LINE (385, 371)-(424, 410), 0, BF
LINE (580, 371)-(619, 410), 0, BF

'This section draws the save load
LINE (373, 29)-(373, 94), 15:  LINE (374, 28)-(501, 28), 15
LINE (502, 29)-(502, 44), 8: LINE (502, 44)-(633, 44), 15
LINE (503, 43)-(503, 29), 15:
LINE (504, 28)-(632, 28), 15
LINE (374, 95)-(633, 95), 8: LINE (633, 94)-(633, 29), 8
LINE (588, 50)-(627, 89), 0, BF
LINE (587, 50)-(587, 89), 8: LINE (587, 49)-(627, 49), 8
LINE (587, 90)-(628, 90), 15: LINE (628, 50)-(628, 89), 15
LINE (430, 54)-(581, 63), 15, BF
LINE (429, 54)-(429, 62), 8: LINE (429, 53)-(580, 53), 8
LINE (377, 70)-(581, 90), 15, BF
LINE (377, 70)-(377, 89), 8: LINE (377, 70)-(580, 70), 8

'This section draws the help window
DEF SEG = VARSEG(windowtext(0))
BLOAD pathname$ + "keyword.wtx", 0
DEF SEG
PUT (377, 116), windowtext, PSET
PSET (404, 121), 7
DEF SEG = VARSEG(windowtext(0))
BLOAD pathname$ + "print.wtx", 0
DEF SEG
PUT (598, 114), windowtext, PSET

LINE (374, 98)-(632, 98), 15
LINE (373, 98)-(373, 347), 15
LINE (374, 348)-(632, 348), 8
LINE (633, 98)-(633, 348), 8
LINE (425, 114)-(595, 124), 15, BF
LINE (425, 114)-(425, 123), 8
LINE (425, 114)-(594, 114), 8
LINE (377, 127)-(630, 345), 15, BF
LINE (377, 127)-(377, 344), 8
LINE (377, 127)-(629, 127), 8
LINE (377, 101)-(630, 111), 1, BF

DEF SEG = VARSEG(tile%(0))
BLOAD pathname$ + "converge.ico", 0
DEF SEG
PUT (378, 356), tile%, PSET

DEF SEG = VARSEG(windowtext(0))
BLOAD pathname$ + "foretile.wtx", 0
DEF SEG
PUT (376, 419), windowtext, PSET
LINE (428, 417)-(628, 427), 15, BF
LINE (428, 417)-(428, 426), 8
LINE (428, 417)-(627, 417), 8

DEF SEG = VARSEG(windowtext(0))
BLOAD pathname$ + "backtile.wtx", 0
DEF SEG
PUT (376, 437), windowtext, PSET
LINE (428, 435)-(628, 445), 15, BF
LINE (428, 435)-(428, 444), 8
LINE (428, 435)-(627, 435), 8

'This section loads the icons
DEF SEG = VARSEG(tile%(0))
BLOAD pathname$ + "icons1.ico", 0
DEF SEG
PUT (6, 28), tile%, PSET
DEF SEG = VARSEG(tile%(0))
BLOAD pathname$ + "icons2.ico", 0
DEF SEG
PUT (6, 108), tile%, PSET
DEF SEG = VARSEG(tile%(0))
BLOAD pathname$ + "icons3.ico", 0
DEF SEG
PUT (6, 188), tile%, PSET
DEF SEG = VARSEG(tile%(0))
BLOAD pathname$ + "icons4.ico", 0
DEF SEG
PUT (6, 268), tile%, PSET

DEF SEG = VARSEG(icons%(0))
BLOAD pathname$ + "tiledx.ico", 0
DEF SEG
PUT (6, 5), icons%, PSET

DEF SEG = VARSEG(icons%(0))
BLOAD pathname$ + "close.ico", 0
DEF SEG
PUT (626, 4), icons%, PSET
'LINE (615, 4)-(615, 12), 15

DEF SEG = VARSEG(icons%(0))
BLOAD pathname$ + "help.ico", 0
DEF SEG
PUT (379, 103), icons%, PSET

'This section loads all of the text

DEF SEG = VARSEG(windowtext(0))
BLOAD pathname$ + "filename.wtx", 0
DEF SEG
PUT (14, 358), windowtext, PSET
PUT (377, 55), windowtext, PSET
LINE (70, 356)-(316, 366), 15, BF
LINE (70, 356)-(70, 365), 8
LINE (70, 356)-(315, 356), 8

DEF SEG = VARSEG(windowtext(0))
BLOAD pathname$ + "mode.wtx", 0
DEF SEG
PUT (14, 374), windowtext, PSET
LINE (70, 372)-(316, 382), 15, BF
LINE (70, 372)-(70, 381), 8
LINE (70, 372)-(315, 372), 8

DEF SEG = VARSEG(windowtext(0))
BLOAD pathname$ + "position.wtx", 0
DEF SEG
PUT (14, 390), windowtext, PSET
LINE (70, 388)-(316, 398), 15, BF
LINE (70, 388)-(70, 397), 8
LINE (70, 388)-(315, 388), 8

LINE (11, 404)-(365, 448), 15, BF
LINE (11, 404)-(11, 447), 8
LINE (11, 404)-(364, 404), 8

DEF SEG = VARSEG(windowtext(0))
BLOAD pathname$ + "save.wtx", 0
DEF SEG
PUT (428, 33), windowtext, PSET

DEF SEG = VARSEG(windowtext(0))
BLOAD pathname$ + "load.wtx", 0
DEF SEG
PUT (553, 33), windowtext, PSET

END SUB

SUB TEXT
oletter.place.x = letter.place.x

IF save.load$ = "yes" THEN
  GOTO text.routine
  ELSE text.length = LEN(graphic.text$)
END IF

text.routine:

FOR letter.position = 1 TO text.length
   a$ = LEFT$(graphic.text$, letter.position)
   b$ = RIGHT$(a$, 1)
   IF b$ = " " THEN b$ = "space"
   IF b$ = "." THEN b$ = "per"
   IF b$ = "'" THEN b$ = "apost"
   IF b$ = "," THEN b$ = "comma"
   IF b$ = "\" THEN b$ = "slash"
   IF b$ = ":" THEN b$ = "colon"
   IF b$ = "?" THEN b$ = "quest"
   IF b$ = "-" THEN b$ = "dash"
   IF b$ = "_" THEN b$ = "unscr"
   IF b$ = "'" THEN b$ = "apost"
   c$ = pathname$ + "" + b$ + ".gtx"
         DEF SEG = VARSEG(letter(0))
         BLOAD c$, letter
         DEF SEG
         PUT (letter.place.x, letter.place.y), letter, PSET
      letter.place.x = letter.place.x + 6
NEXT letter.position
IF help.bypass$ = "yes" THEN
   letter.place.x = oletter.place.x
   letter.place.y = letter.place.y + 10
   graphic.text$ = ""
END IF
save.load$ = ""
graphic.text$ = ""
END SUB

SUB Text.Line1
letter.place.x = 15
letter.place.y = 408
Color$ = ""
TEXT
END SUB

SUB Text.Line2
letter.place.x = 15
letter.place.y = 418
Color$ = ""
TEXT
END SUB

SUB Text.Line3
letter.place.x = 15
letter.place.y = 428
Color$ = ""
TEXT
END SUB

SUB Text.Line4
letter.place.x = 15
letter.place.y = 438
Color$ = ""
TEXT
END SUB

SUB Update
Icon.Normal
IF draw.mode$ = "penup" THEN
  graphic.text$ = "Use the number pad or the arrow keys to move the cursor   "
  Text.Line1
  graphic.text$ = "around the grid. Press space to start drawing.            "
  Text.Line2
  icon.number = 1
  Icon.Pressed
ELSEIF draw.mode$ = "pendown" THEN
  graphic.text$ = "Use the number pad or the arrow keys to draw. Press space "
  Text.Line1
  graphic.text$ = "to go to pen up.                                          "
  Text.Line2
  icon.number = 2
  Icon.Pressed
ELSEIF draw.mode$ = "singlepixel" THEN
  graphic.text$ = "Use the number pad or the arrow keys to move the cursor   "
  Text.Line1
  graphic.text$ = "around the grid. Press space to draw a single pixel.      "
  Text.Line2
  icon.number = 3
  Icon.Pressed
ELSE graphic.text$ = "Move the mouse around the screen. Left click to select or "
  Text.Line1
  graphic.text$ = "draw.                                                     "
  Text.Line2
  graphic.text$ = "pen up               "
  Mode
  icon.number = 1
  Icon.Pressed
  mode.route = 0
END IF
END SUB

