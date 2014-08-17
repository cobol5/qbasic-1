'ANDE Corporation. WinDos 100 (by Denis Andrianov).
'============================================================================
DECLARE FUNCTION MouseInit% ()
DECLARE SUB MouseStatus (Lb%, Rb%, xMouse%, yMouse%)
DECLARE SUB MouseRange (x1%, y1%, x2%, y2%)
DECLARE SUB MousePut (x%, y%)
DECLARE SUB MouseHide ()
DECLARE SUB MouseDriver (ax%, bx%, cx%, dx%)
DECLARE SUB MouseShow ()

DECLARE SUB taskbar (zoom%)
DECLARE SUB setdesktop ()
DECLARE SUB menu1 ()
DECLARE SUB pulldown ()
DECLARE SUB smallbutton (x%, y%, x2%, y2%, colour%, topcol%, botcol%)
DECLARE SUB tinybutton (x%, y%, x2%, y2%, colour%, topcol%, botcol%)
DECLARE SUB pullbutt (x%, y%, pressed%, active%, pullpress%)
DECLARE SUB infobox (x%, y%, x2%, y2%, boxtype%, text$)
DECLARE SUB raisedframe (x%, y%, x2%, y2%)
DECLARE SUB crackline (vertical%, x1%, y1%, x2%, y2%)
DECLARE SUB showfiles (startfile%)

DECLARE SUB mainbarpress (x%, y%, pressed%, buttons%)
DECLARE SUB mainbarrelease (x%, y%, pressed%, buttons%)
DECLARE SUB mainmenu1press (x%, y%, pressed%, buttons%)
DECLARE SUB mainmenu1release (x%, y%, pressed%, buttons%)
DECLARE SUB drvbuttonpress (x%, y%, pressed%)
DECLARE SUB drvbuttonrelease ()
DECLARE FUNCTION infbutpress% (x%, y%, boxtype%)
DECLARE FUNCTION infbutrelease% (buttonnum%, boxtype%)
DECLARE FUNCTION captureinf% (boxtype%)
DECLARE SUB expbuttons ()
DECLARE SUB expbuttonspress (x%, y%, pressed%, explorebut%)
DECLARE SUB expbuttonsrelease (explorebut%)
DECLARE SUB pullmenu (x%, y%, pressed%, active%)

DECLARE SUB pullrefresh (refresh1%)

DECLARE SUB dir (x%, y%)
DECLARE SUB shortcut (x%, y%)
DECLARE SUB prg (x%, y%, extension$)
DECLARE SUB arch (x%, y%)
DECLARE SUB unknown (x%, y%)
DECLARE SUB txt (x%, y%)

DECLARE SUB readini ()
DECLARE SUB readdir (file$, file%)
DECLARE SUB loadbgd (file%, x%, y%, x2%, y2%, timeerase%)
DECLARE SUB loadfont (filename$, text$, textx%, texty%, colour%, file%)
DECLARE SUB fprint (text$, textx%, texty%, colour%, file%)
DECLARE SUB whatfile (number%, file$, extension$, directory%)
DECLARE SUB runitem (file$, extension$, directory%)
DECLARE SUB writebatch (file$, extension$, wtype%)  'wtype=1(exe,com,bat)
DECLARE SUB savecfg (file$, extension$)
DECLARE SUB drives ()
DECLARE SUB setdrives ()
DECLARE SUB dirlist (directory$)
DECLARE SUB updir ()
DECLARE SUB changedir (directory$)
DECLARE SUB delay (time%)       'time%=67 (1 second)
DECLARE SUB selectobject (file$, extension$, directory%, xfile%, yfile%, sel%)
DECLARE FUNCTION inputline$ (limit%, startscroll%)

DECLARE SUB quickexe (program$, switchto$)
DECLARE SUB erasefile (file$)
DECLARE SUB editfile ()

DECLARE SUB explorer ()
DECLARE SUB trash ()

DECLARE SUB finish ()

CONST system1 = "Autoexec.bat"  'System files.
CONST system2 = "Config.sys"
CONST system3 = "Msdos.sys"
CONST editor = "edit.com"       'WinDos's default file editor.
CONST about = "about.exe"
CONST batch = "runprg.bat"      'Runs executable files.
CONST runner = "runner.exe"     'Runs executable files.
CONST ininame = "windos.ini"    'Name of "ini" file.
DIM SHARED windospath$          'WinDos's working path.
DIM SHARED trashpath$           'Path to Trash directory.
DIM SHARED temppath$            'Path to Temp directory.
DIM SHARED logspath$            'Path to Logs directory.
DIM SHARED graphicspath$        'Path to Graphics directory.
DIM SHARED deskback%            'Desktop's background colour.
DIM SHARED fontspath$           'Path to Fonts directory.
DIM SHARED archpath$            'Path to Archive tools.
DIM SHARED runpath$             'Path to the file to execute.
DIM SHARED bgdfile$             'Background file.
DIM SHARED mouse$               'Mouse data.
DIM SHARED menu1press%          'Allows to use the first menu.
DIM SHARED drivespress%         'Allows to use the drive buttons.
'CONST FilesFull = 3000
DIM SHARED files$(3000)                         'File array.
'CONST DirectoriesFull = 1000
DIM SHARED directories$(1000)                   'Directory array.
DIM SHARED selobj%(4000)                        'Selected objects' array.
DIM SHARED nextdrive$           'Next drive.
DIM SHARED drive$(26)           'Array contains drives.
DIM SHARED drivenum%            'Number of drives.
DIM SHARED defaultdrive$        'Default drive.
DIM SHARED currentdir$          'Current directory.
DIM SHARED filescroll%          'Current scrolling file.
DIM SHARED filenum%             'Number of files.
DIM SHARED expcall%             'Should I turn on explorer ?
DIM SHARED expon%               'Is equal to 1 when explorer is on.
DIM SHARED expbuttonson%        'Is equal to 1 when right explorer buttons are on.
DIM SHARED driveson%            'Is equal to 1 when drives are on.
DIM SHARED taskbarpress%        'Is equal to 1 when taskbar is on.
DIM SHARED pulldownon%          'Is equal to 1 when pull down menu is on.
CONST mainfnt = "windos.fnt"
DIM SHARED windosfont$(255)     'Main font.
'============================================================================

'Defaults here.
mouse$ = SPACE$(57)
id$ = "(C) ANDE Corporation (by Denis Andrianov)"
currentdir$ = "C:\"
pressed% = 0                  'Pressed button
taskbarpress% = 1             'Taskbar enabled
expbuttonson% = 0             'Explorer buttons off
expon% = 0                    'Explorer is off
active% = 0                   'No menus are on
boxtype% = 0                  'Currently no information boxes
menu% = 0                     'Current menu
doubleclick% = 0              'No double click

  FOR i% = 1 TO 57             'Reading mouse data...
  READ a$
  H$ = CHR$(VAL("&H" + a$))
  MID$(mouse$, i%, 1) = H$
  NEXT i%

ON KEY(10) GOSUB F10
KEY(10) ON

ON TIMER(1) GOSUB changetime

ON ERROR GOTO errhandler:
'============================================================================
'DEFINT A-Z
DEF SEG = &HA000

ms% = MouseInit%           'Checking if the mouse exists...
  IF NOT ms% THEN
  CLS
  PRINT "Mouse not found."
  END
  END IF

readini
  IF MID$(windospath$, 2, 1) <> ":" THEN
  CLS
  PRINT "Error in WINDOS.INI file."
  END
  END IF

freefl% = FREEFILE
OPEN fontspath$ + "\" + mainfnt FOR INPUT AS freefl%
  FOR counter% = 1 TO 255
  LINE INPUT #1, windosfont$(counter%)
  NEXT counter%
CLOSE freefl%


SCREEN 12
CLS
 
  IF expcall% <> 1 THEN
  'logo
  CLS
  loadbgd 1, 0, 0, 640, 480, 0
  taskbar 1
  setdesktop
  readdir "current.dir", 1
    ELSE
    pulldownon% = 1
    driveson% = 1
    expon% = 1
    explorer
    setdrives
    taskbar 0
    savecfg "", ""
    TIMER ON
  END IF

MouseShow
MouseRange 0, 0, 638, 478
MousePut 320, 240
'----------------------------------------------------------------------------

DO                                  'Main Loop

 MouseStatus Lb%, Rb%, x%, y%
' LOCATE 1, 1: PRINT Lb%, Rb%

 SELECT CASE Rb%
   CASE -1
   GOSUB selectit
   GOSUB calcfile
     IF oldnfile% = nfile% THEN rightclick% = 0 ELSE rightclick% = 1
 
   CASE 0
   rightclick% = 1
   GOSUB openit
 END SELECT


 SELECT CASE Lb%
   CASE -1
     IF pressed% = 1 THEN GOTO exitcase
     IF taskbarpress% = 1 THEN mainbarpress x%, y%, pressed%, buttons%
     IF menu1press% = 1 THEN mainmenu1press x%, y%, pressed%, buttons%
     IF expon% = 1 AND driveson% = 1 THEN drvbuttonpress x%, y%, pressed%
     IF expon% = 1 AND expbuttonson% = 1 THEN expbuttonspress x%, y%, pressed%, explorebut%
     IF expon% = 1 AND pulldownon% = 1 AND pullpress% = 0 THEN pullbutt x%, y%, pressed%, active%, pullpress%

   CASE 0
   GOSUB openit
     IF expon% = 1 AND pulldownon% = 1 THEN pullpress% = 0
     IF pressed% = 0 THEN GOTO exitcase
     MouseHide
     pressed% = 0
       IF taskbarpress% = 1 THEN mainbarrelease x%, y%, pressed%, buttons%
       IF menu1press% = 1 THEN mainmenu1release x%, y%, pressed%, buttons%
       IF expon% = 1 AND driveson% = 1 THEN drvbuttonrelease
       IF expon% = 1 AND expbuttonson% = 1 THEN expbuttonsrelease explorebut%
 END SELECT


 SELECT CASE Rb% AND Lb%
   CASE -1
   doubleclick% = 1
 END SELECT


exitcase:
  IF dosexit% = 1 THEN GOSUB F10
LOOP

'================================SUB ROUTINES================================

errhandler:
SELECT CASE ERR
  CASE 9            'Too many files.
 
  CASE 5            'Syntax Error.
  SCREEN 0
  CLS
  PRINT "Cannot proceed. Syntax error has occured."
  END
   
  CASE 57           'Device I/O error.
  currentdir$ = defaultdrive$ + "\"
  RESUME NEXT

  CASE 71           'Disk is not in the drive.
  currentdir$ = defaultdrive$ + "\"
  RESUME NEXT

  CASE 75           'Access denied
  RESUME NEXT

    CASE ELSE
    SCREEN 0
    CLS
    PRINT "Error number"; ERR; "has occured."
    END

END SELECT

'----------------------------------------------------------------------------
openit:
  IF expon% = 1 AND doubleclick% = 1 AND active% = 0 THEN
    IF x% > 18 AND x% < 538 AND y% > 65 AND y% < 385 THEN
    GOSUB calcfile
      IF file$ = "" AND extension$ = "" THEN doubleclick% = 0: RETURN
    runitem file$, extension$, directory%
    END IF
  doubleclick% = 0
  END IF
RETURN

'----------------------------------------------------------------------------
selectit:
  IF expon% = 1 AND rightclick% = 1 AND active% = 0 THEN
    IF x% > 18 AND x% < 538 AND y% > 65 AND y% < 385 THEN
    GOSUB calcfile
    oldnfile% = nfile%
      IF file$ = "" AND extension$ = "" THEN RETURN
    MouseHide
      IF selobj%(nfile%) = 1 THEN
      selobj%(nfile%) = 0
      selectobject file$, extension$, directory%, xfilepos%, yfilepos%, 0
        ELSE
         selobj%(nfile%) = 1
         selectobject file$, extension$, directory%, xfilepos%, yfilepos%, 1
      END IF
    MouseShow
    END IF
  END IF
RETURN

'----------------------------------------------------------------------------
calcfile:
xfile% = (x% - 18) \ 104 + 1
yfile% = (y% - 65) \ 80 + 1
xfilepos% = xfile% * 104 - 64
yfilepos% = yfile% * 80 + 10
nfile% = 5 * (yfile% - 1) + xfile% + filescroll% - 1
whatfile nfile%, file$, extension$, directory%
'   LOCATE 1, 1: PRINT file$, extension$
RETURN

'----------------------------------------------------------------------------
changedrv:
  SELECT CASE ERR
   
    CASE 57
    RESUME NEXT

    CASE 76
    ascii% = ASC(nextdrive$)
    nextdrive$ = CHR$((ascii%) - 1)
    RESUME NEXT

    CASE 71
    RESUME NEXT
   
  END SELECT
PRINT ERR
'----------------------------------------------------------------------------

changetime:
LINE (558, 457)-(638, 478), 7, BF
loadfont "winfont", TIME$, 570, 459, 14, FREEFILE
RETURN
'----------------------------------------------------------------------------

F10:                      'Exit key
finish
RETURN


'================================Mouse data==================================

DATA 55,89,E5,8B,5E,0C,8B,07,50,8B,5E,0A,8B,07,50,8B
DATA 5E,08,8B,0F,8B,5E,06,8B,17,5B,58,1E,07,CD,33,53
DATA 8B,5E,0C,89,07,58,8B,5E,0A,89,07,8B,5E,08,89,0F
DATA 8B,5E,06,89,17,5D,CA,08,00
RESTORE

SUB arch (x%, y%)

leng% = 60
wid% = 30

LINE (x% + leng% / 2 - 3, y%)-(x% + leng% / 2 + 3, y% + wid% / 5 - 1), 15, BF
LINE (x%, y% + wid% / 5)-(x% + leng%, y% + 2 * wid% / 5), 7, BF
LINE (x%, y% + 3 * wid% / 5)-(x% + leng%, y% + 4 * wid% / 5), 7, BF
LINE (x% + leng% / 2 - 3, y% + 4 * wid% / 5 + 1)-(x% + leng% / 2 + 3, y% + wid%), 15, BF

PSET (x% + 2, y% + 3 * wid% / 5 - 1), 14

  FOR counter% = 1 TO 7
  DRAW "e4f4"
  NEXT counter%

END SUB

FUNCTION captureinf% (boxtype%)

MouseShow
KEY(10) OFF

  DO
  MouseStatus Lb%, Rb%, x%, y%
   
    SELECT CASE Lb%
      CASE -1
        IF buttonnum% = 0 THEN
        buttonnum% = infbutpress(x%, y%, boxtype%)
        END IF
      CASE 0
        IF buttonnum% > 0 THEN
        temp% = infbutrelease(buttonnum%, boxtype%)
        EXIT DO
        END IF
    END SELECT
 
  LOOP

'----------------------------------------------------------------------------
captureinf% = buttonnum%
taskbarpress% = 1
driveson% = 1
pulldownon% = 1
  IF expon% = 1 THEN
  expbuttonson% = 1
  END IF
KEY(10) ON

END FUNCTION

SUB changedir (directory$)

  IF LEN(currentdir$) = 3 THEN
  currentdir$ = currentdir$ + directory$
    ELSE
    currentdir$ = currentdir$ + "\" + directory$
  END IF

END SUB

SUB crackline (vertical%, x1%, y1%, x2%, y2%)

  IF vertical% = 1 THEN
  LINE (x1%, y1%)-(x2%, y2%), 15
  LINE (x1% - 1, y1%)-(x2% - 1, y2%), 0
    ELSE
    LINE (x1%, y1%)-(x2%, y2%), 15
    LINE (x1%, y1% - 1)-(x2%, y2% - 1), 0
  END IF

END SUB

SUB delay (time%)

'time%=67 (1 second)

  FOR i% = 1 TO time%
  WAIT &H3DA, 8, 8
  WAIT &H3DA, 8
  NEXT i%

END SUB

SUB dir (x%, y%)

leng% = 60
wid% = 30

'LINE (x% - 3, y%)-(x% + leng%, y% + wid%), 0, BF

PSET (x%, y% + 10), 14
DRAW "e10r20f10r20d20l60u20h3d20nf3u20e10nf3r20f13r20h3l20"
PAINT (x% + 10, y% + 10), 14

END SUB

SUB dirlist (directory$)

LINE (0, 430)-(640, 450), 0, BF
loadfont "winfont", UCASE$(directory$), 10, 430, 7, FREEFILE

END SUB

SUB drives

ON ERROR GOTO changedrv
nextdrive$ = "c"
lastdrive$ = "c"

  FOR drivenum% = 1 TO 26
  CHDIR nextdrive$ + ":\"
  ascii% = ASC(nextdrive$)
  nextdrive$ = CHR$((ascii%) + 1)
    IF lastdrive$ = nextdrive$ THEN EXIT FOR
  lastdrive$ = nextdrive$
  drive$(drivenum%) = CHR$(ascii%)
  NEXT drivenum%

'Please remove!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CHDIR windospath$

ON ERROR GOTO errhandler

END SUB

SUB drvbuttonpress (x%, y%, pressed%)

lastx% = INT(640 / drivenum%)
 
  FOR counter% = 1 TO drivenum%
    IF x% > lastx% * counter% - lastx% AND x% < lastx% * counter% - 1 AND y% > 20 AND y% < 40 THEN
    GOTO pressdrv
    END IF
  NEXT counter%

EXIT SUB


pressdrv:
ERASE selobj%
pressed% = 1
MouseHide
letter$ = CHR$(counter% + 65)
  IF letter$ = "B" THEN letter$ = "A"
smallbutton lastx% * counter% - lastx%, 20, lastx% * counter% - 1, 40, 6, 8, 7
loadfont "winfont", letter$ + ":", (counter% * lastx% + (counter% - 1) * lastx% + 1) / 2 - 5, 20, 2, FREEFILE
currentdir$ = letter$ + ":\"
CHDIR currentdir$
readdir "current.dir", FREEFILE
dirlist currentdir$
filescroll% = 1
showfiles filescroll%
MouseShow

END SUB

SUB drvbuttonrelease
MouseHide
setdrives
MouseShow
END SUB

SUB editfile

  FOR counter% = 1 TO 4000
    IF selobj%(counter%) <> 0 THEN
    whatfile counter%, file$, extension$, directory%
      IF directory% = 0 THEN
      quickexe editor + " " + file$ + "." + extension$, ""
      EXIT FOR
      END IF
    END IF
  NEXT counter%

END SUB

SUB erasefile (file$)

SELECT CASE file$
'----------------------------------------------------------------------------
 CASE ""

   FOR counter% = 1 TO 4000
     IF selobj%(counter%) <> 0 THEN
     whatfile counter%, file$, extension$, directory%
       IF directory% = 0 THEN
         IF LEN(currentdir$) = 3 THEN
         KILL currentdir$ + file$ + "." + extension$
           ELSE
           KILL currentdir$ + "\" + file$ + "." + extension$
         END IF
       END IF
     END IF
   NEXT counter%

 ERASE selobj%
 MouseHide
 explorer
 setdrives
 MouseShow
'----------------------------------------------------------------------------
 
 CASE ELSE

 END SELECT

END SUB

SUB expbuttons

MouseHide

'----------------------------------------------------------------------------

LINE (565, 53)-(568, 425), 10, B              'Drawing 3d lines...
LINE (566, 53)-(567, 425), 2, B

LINE (0, 425)-(640, 428), 2, B
LINE (0, 426)-(640, 427), 10, B

'----------------------------------------------------------------------------
'Drawing 5 main buttons...

  FOR counter% = 60 TO 168 STEP 32
  smallbutton 581, counter%, 638, counter% + 30, 15, 10, 2
  NEXT counter%
           
smallbutton 581, 210, 638, 240, 15, 10, 2
smallbutton 581, 263, 638, 293, 15, 10, 2

PSET (625, 74)
ForwardArrow$ = "c9 h12 d5 l20 d15 r20 d5 e12 bl10 p9,9"
DRAW ForwardArrow$

PSET (625, 107)
BackwardArrow$ = "c9 u7 l20 u5 g12 f12 u5 r20 u8 bl10 p9,9"
DRAW BackwardArrow$

loadfont "winfont", "MEM", 593, 129, 13, FREEFILE

loadfont "winfont", "RST", 593, 161, 13, FREEFILE

PSET (609, 215)
updirarrow$ = "c9g10r20h10bd5p9,9d15r20l40"
DRAW updirarrow$

loadfont "winfont", "QView", 585, 268, 0, FREEFILE
'----------------------------------------------------------------------------

'Drawing 4 small scrolling buttons...

PSET (605, 340), 15
UpArrow$ = "f15l30e15" 'bd5p4,4"
DRAW UpArrow$

PSET (605, 410), 15
DownArrow$ = "h15r30g15"  'bu5p4,4"
DRAW DownArrow$

PSET (573, 375), 15
LeftArrow$ = "e15d30h15"  'br5p4,4"
DRAW LeftArrow$

PSET (637, 375), 15
RightArrow$ = "h15d30e15"   'bl5p4,4"
DRAW RightArrow$

'----------------------------------------------------------------------------

MouseShow

END SUB

SUB expbuttonspress (x%, y%, pressed%, explorebut%)

  IF x% > 581 AND x% < 638 THEN
    
    IF y% > 60 AND y% < 90 THEN
    MouseHide
    pressed% = 1
    explorebut% = 1
    smallbutton 581, 60, 638, 90, 15, 2, 10
    PSET (627, 74)
    ForwardArrow$ = "c9 h12 d5 l20 d15 r20 d5 e12 bl10 p9,9"
    DRAW ForwardArrow$
    END IF

    IF y% > 92 AND y% < 122 THEN
    MouseHide
    pressed% = 1
    explorebut% = 2
    smallbutton 581, 92, 638, 122, 15, 2, 10
    PSET (627, 107)
    BackwardArrow$ = "c9 u7 l20 u5 g12 f12 u5 r20 u8 bl10 p9,9"
    DRAW BackwardArrow$
    END IF

    IF y% > 124 AND y% < 154 THEN
    MouseHide
    pressed% = 1
    explorebut% = 3
    smallbutton 581, 124, 638, 154, 15, 2, 10
    loadfont "winfont", "MEM", 595, 129, 13, FREEFILE
    END IF

    IF y% > 156 AND y% < 186 THEN
    MouseHide
    pressed% = 1
    explorebut% = 4
    smallbutton 581, 156, 638, 186, 15, 2, 10
    loadfont "winfont", "RST", 595, 161, 13, FREEFILE
    END IF

    IF y% > 210 AND y% < 240 THEN
    MouseHide
    pressed% = 1
    explorebut% = 5
    smallbutton 581, 210, 638, 240, 15, 2, 10
    PSET (611, 215)
    updirarrow$ = "c9g10r20h10bd5p9,9d15r20l40"
    DRAW updirarrow$
    END IF

    IF y% > 263 AND y% < 293 THEN
    MouseHide
    pressed% = 1
    explorebut% = 10
    smallbutton 581, 263, 638, 293, 15, 2, 10
    loadfont "winfont", "QView", 587, 268, 0, FREEFILE
    END IF

  END IF
   

    IF y% > 340 AND y% < 355 AND x% > 590 AND x% < 620 THEN
    MouseHide
    pressed% = 1
    explorebut% = 6
    PSET (605, 340), 15
    UpArrow$ = "f15l30e15bd5p4,15"
    DRAW UpArrow$
      IF filescroll% = 1 THEN MouseShow: EXIT SUB
    filescroll% = filescroll% - 5
    showfiles filescroll%
    END IF

    IF y% > 395 AND y% < 410 AND x% > 590 AND x% < 620 THEN
    MouseHide
    pressed% = 1
    explorebut% = 7
    PSET (605, 410), 15
    DownArrow$ = "h15r30g15bu5p4,15"
    DRAW DownArrow$
      IF filenum% <= filescroll% + 19 THEN MouseShow: EXIT SUB
    filescroll% = filescroll% + 5
    showfiles filescroll%
    END IF

    IF y% > 360 AND y% < 390 AND x% > 573 AND x% < 588 THEN
    MouseHide
    pressed% = 1
    explorebut% = 8
    PSET (573, 375), 15
    LeftArrow$ = "e15d30h15br5p4,15"
    DRAW LeftArrow$
    END IF

    IF y% > 360 AND y% < 390 AND x% > 623 AND x% < 637 THEN
    MouseHide
    pressed% = 1
    explorebut% = 9
    PSET (637, 375), 15
    RightArrow$ = "h15d30e15bl5p4,15"
    DRAW RightArrow$
    END IF

MouseShow

END SUB

SUB expbuttonsrelease (explorebut%)
 
  IF explorebut% = 1 THEN
  explorebut% = 0
  MouseHide
  smallbutton 581, 60, 638, 90, 15, 10, 2
  PSET (625, 74)
  ForwardArrow$ = "c9 h12 d5 l20 d15 r20 d5 e12 bl10 p9,9"
  DRAW ForwardArrow$
  END IF

  IF explorebut% = 2 THEN
  explorebut% = 0
  MouseHide
  smallbutton 581, 92, 638, 122, 15, 10, 2
  PSET (625, 107)
  BackwardArrow$ = "c9 u7 l20 u5 g12 f12 u5 r20 u8 bl10 p9,9"
  DRAW BackwardArrow$
  END IF

  IF explorebut% = 3 THEN
  explorebut% = 0
  MouseHide
  smallbutton 581, 124, 638, 154, 15, 10, 2
  loadfont "winfont", "MEM", 593, 129, 13, FREEFILE
  END IF

  IF explorebut% = 4 THEN
  explorebut% = 0
  MouseHide
  smallbutton 581, 156, 638, 186, 15, 10, 2
  loadfont "winfont", "RST", 593, 161, 13, FREEFILE
  END IF

  IF explorebut% = 5 THEN
  explorebut% = 0
  MouseHide
  smallbutton 581, 210, 638, 240, 15, 10, 2
  PSET (609, 215)
  updirarrow$ = "c9g10r20h10bd5p9,9d15r20l40"
  DRAW updirarrow$
    IF LEN(currentdir$) > 3 THEN
    updir
    MouseHide
    filescroll% = 1
    readdir "current.dir", FREEFILE     'Refreshing...
    showfiles filescroll%
    dirlist currentdir$
    MouseShow
    END IF
  END IF

  IF explorebut% = 6 THEN
  explorebut% = 0
  MouseHide
  PSET (605, 340), 15
  UpArrow$ = "f15l30e15bd5p0,15"
  DRAW UpArrow$
  END IF

  IF explorebut% = 7 THEN
  explorebut% = 0
  MouseHide
  PSET (605, 410), 15
  DownArrow$ = "h15r30g15bu5p0,15"
  DRAW DownArrow$
  END IF

  IF explorebut% = 8 THEN
  explorebut% = 0
  MouseHide
  PSET (573, 375), 15
  LeftArrow$ = "e15d30h15br5p0,15"
  DRAW LeftArrow$
  END IF

  IF explorebut% = 9 THEN
  explorebut% = 0
  MouseHide
  PSET (637, 375), 15
  RightArrow$ = "h15d30e15bl5p0,15"
  DRAW RightArrow$
  END IF

  IF explorebut% = 10 THEN
  explorebut% = 0
  MouseHide
  smallbutton 581, 263, 638, 293, 15, 10, 2
  loadfont "winfont", "QView", 585, 268, 0, FREEFILE
  editfile
  END IF

MouseShow

END SUB

SUB explorer

  IF filescroll% = 0 THEN filescroll% = 1
LINE (0, 0)-(640, 454), 0, BF
readdir "current.dir", FREEFILE
drives
expbuttonson% = 1
expbuttons
showfiles filescroll%
dirlist currentdir$
pulldown

END SUB

SUB finish

infobox 320, 175, 490, 250, 1, "    Do you want to exit to Dos?"
prsinf% = captureinf%(1)
  IF expon% = 0 AND prsinf% = 2 THEN
  loadbgd FREEFILE, 149, 100, 491, 250, 1
  setdesktop
  EXIT SUB
    ELSEIF expon% = 1 AND prsinf% = 2 THEN
    showfiles filescroll%
    EXIT SUB
  ELSEIF prsinf% = 1 THEN
  TIMER OFF
  CLOSE
  SCREEN 0
  CLS
  savecfg "", ""
  SHELL LEFT$(windospath$, 2)
  CHDIR windospath$
  END
  END IF

END SUB

SUB fprint (text$, textx%, texty%, colour%, file%)
'text$:        The text you want printed
'textx%,texty%:Where to place the font
'colour%:      Take a wild guess
'file%:        The number of the opened file
'                (if you are not sure use file%=FREEFILE)
'lpi: lines per integer
'fws: font word spacing
'fls: font letter spacing
'p% : pointer
 
GET file%, 1, lpi%
GET file%, 2, fws%
GET file%, 3, fls%
  FOR count% = 1 TO LEN(text$)
    m% = ASC(MID$(text$, count%, 1)) - 29
    IF m% > 3 THEN
      GET file%, m%, a1%
      GET file%, m% + 1, a2%
      FOR n% = a1% TO a2% - 1 STEP lpi%
        FOR z% = 0 TO lpi% - 1
          GET file%, n% + z%, l%
          xcoor% = p% + textx%: ycoor% = (16 * z%) + texty%
          LINE (xcoor%, ycoor%)-(xcoor%, ycoor% + 15), colour%, , l%
'         LINE (p% + textx%, (16 * z%) + texty%)-(p% + textx%, (16 * z%) + 15 + texty%), colour%, , l%
        NEXT z%
        p% = p% + 1
      NEXT n%
      p% = p% + fls%
    ELSE
      p% = p% + fws%
    END IF
  NEXT count%
END SUB

FUNCTION infbutpress% (x%, y%, boxtype%)

  IF boxtype% = 1 THEN              'OK, Cancel' box

    IF x% > 190 AND x% < 310 AND y% > 200 AND y% < 220 THEN
    MouseHide
    smallbutton 190, 200, 310, 220, 7, 8, 15
    loadfont "winfont", "OK", 190 + 53, 200, 12, FREEFILE
    infbutpress% = 1      'Button that is being pressed.
    MouseShow
    
    ELSEIF x% > 330 AND x% < 450 AND y% > 200 AND y% < 220 THEN
    MouseHide
    smallbutton 330, 200, 450, 220, 7, 8, 15
    loadfont "winfont", "Cancel", 330 + 38, 200, 12, FREEFILE
    infbutpress% = 2       'Button that is being pressed.
    MouseShow
   
       ELSE
       infbutpress% = 0

    END IF
 
  END IF

'----------------------------------------------------------------------------

  IF boxtype% = 2 THEN       'Run box
   
    IF x% > 190 AND x% < 270 AND y% > 208 AND y% < 228 THEN
    MouseHide
    smallbutton 190, 208, 270, 228, 7, 8, 15
    loadfont "winfont", "Run", 219, 208, 12, FREEFILE
    infbutpress% = 1      'Button that is being pressed.
    MouseShow

    ELSEIF x% > 280 AND x% < 360 AND y% > 208 AND y% < 228 THEN
    MouseHide
    smallbutton 280, 208, 360, 228, 7, 8, 15
    loadfont "winfont", "Quick", 302, 208, 12, FREEFILE
    infbutpress% = 2       'Button that is being pressed.
    MouseShow

    ELSEIF x% > 370 AND x% < 450 AND y% > 208 AND y% < 228 THEN
    MouseHide
    smallbutton 370, 208, 450, 228, 7, 8, 15
    loadfont "winfont", "Cancel", 387, 208, 12, FREEFILE
    infbutpress% = 3       'Button that is being pressed.
    MouseShow
 
    ELSEIF x% > 192 AND x% < 447 AND y% > 176 AND y% < 191 THEN
    MouseHide
    runpath$ = inputline(63, 32)
    MouseShow

      ELSE infbutpress% = 0

    END IF

  END IF

'----------------------------------------------------------------------------

  IF boxtype% = 3 THEN

    IF x% > 190 AND x% < 310 AND y% > 208 AND y% < 228 THEN
    MouseHide
    smallbutton 190, 208, 310, 228, 7, 8, 15
    loadfont "winfont", "OK", 242, 208, 12, FREEFILE
    infbutpress% = 1
    MouseShow

    ELSEIF x% > 330 AND x% < 450 AND y% > 208 AND y% < 228 THEN
    MouseHide
    smallbutton 330, 208, 450, 228, 7, 8, 15
    loadfont "winfont", "Cancel", 367, 208, 12, FREEFILE
    infbutpress% = 2
    MouseShow

    ELSEIF x% > 192 AND x% < 447 AND y% > 176 AND y% < 191 THEN
    MouseHide
    commandline$ = inputline(63, 32)
    MouseShow
     
      ELSE infbutpress% = 0

    END IF

  END IF

'----------------------------------------------------------------------------

END FUNCTION

FUNCTION infbutrelease% (buttonnum%, boxtype%)
 
  IF boxtype% = 1 THEN
   
    IF buttonnum% = 1 THEN
    MouseHide
    smallbutton 190, 200, 310, 220, 7, 15, 8
    loadfont "winfont", "OK", 190 + 51, 200, 12, FREEFILE
    MouseShow
   
    ELSEIF buttonnum% = 2 THEN
    MouseHide
    smallbutton 330, 200, 450, 220, 7, 15, 8
    loadfont "winfont", "Cancel", 330 + 35, 200, 12, FREEFILE
    MouseShow
    END IF

  END IF
'----------------------------------------------------------------------------

  IF boxtype% = 2 THEN       'Run box
  
    IF buttonnum% = 1 THEN
    MouseHide
    smallbutton 190, 208, 270, 228, 7, 15, 8
    loadfont "winfont", "Run", 217, 208, 12, FREEFILE
    MouseShow

    ELSEIF buttonnum% = 2 THEN
    MouseHide
    smallbutton 280, 208, 360, 228, 7, 15, 8
    loadfont "winfont", "Quick", 300, 208, 12, FREEFILE
    MouseShow

    ELSEIF buttonnum% = 3 THEN
    MouseHide
    smallbutton 370, 208, 450, 228, 7, 15, 8
    loadfont "winfont", "Cancel", 385, 208, 12, FREEFILE
    MouseShow

    END IF

  END IF

'----------------------------------------------------------------------------

  IF boxtype% = 3 THEN

    IF buttonnum% = 1 THEN
    MouseHide
    smallbutton 190, 208, 310, 228, 7, 8, 15
    loadfont "winfont", "OK", 240, 208, 12, FREEFILE
    MouseShow

    ELSEIF buttonnum% = 2 THEN
    MouseHide
    smallbutton 330, 208, 450, 228, 7, 8, 15
    loadfont "winfont", "Cancel", 365, 208, 12, FREEFILE
    MouseShow

    END IF

  END IF

END FUNCTION

SUB infobox (x%, y%, x2%, y2%, boxtype%, text$)
MouseHide

KEY(10) OFF
taskbarpress% = 0
driveson% = 0
expbuttonson% = 0
pulldownon% = 0

xx1% = x% + 30 - (x2% - x%)
yy1% = y% + 50 - (y2% - y%)
xx2% = x2% - 30
yy2% = y2% - 20

  DO UNTIL x% >= x2%                    'Zooming form
  x% = x% + 3
    IF y% < y2% THEN y% = y% + 1.5
  LINE (640 - x%, 350 - y%)-(x%, y%), 7, BF
  LINE (640 - x% + 1, 350 - y% + 1)-(x% - 1, y% - 1), 15, B
  SOUND 32000, .07
  LOOP

raisedframe xx1%, yy1%, xx2%, yy2%

  IF boxtype% = 1 THEN       'Information box
  loadfont "timesnew.qbf", "Information Box", 235, 110, 10, FREEFILE
  smallbutton 190, 200, 310, 220, 7, 15, 8
  loadfont "winfont", "OK", 240, 200, 12, FREEFILE
  smallbutton 330, 200, 450, 220, 7, 15, 8
  loadfont "winfont", "Cancel", 365, 200, 12, FREEFILE
  loadfont "winfont", text$, xx1% + 10, yy1% + 10, 0, FREEFILE
  END IF

  IF boxtype% = 2 THEN       'Run box
  loadfont "timesnew.qbf", "Run Box", 270, 110, 10, FREEFILE
  smallbutton 190, 208, 270, 228, 7, 15, 8
  loadfont "winfont", "Run", 217, 208, 12, FREEFILE
  smallbutton 280, 208, 360, 228, 7, 15, 8
  loadfont "winfont", "Quick", 300, 208, 12, FREEFILE
  smallbutton 370, 208, 450, 228, 7, 15, 8
  loadfont "winfont", "Cancel", 385, 208, 12, FREEFILE
  loadfont "winfont", text$, xx1% + 10, yy1% + 5, 0, FREEFILE
  locatex% = 25
  locatey% = 12
  startscroll% = 32
  LOCATE locatey%, locatex%
  PRINT SPACE$(startscroll%)
  END IF

  IF boxtype% = 3 THEN
  loadfont "timesnew.qbf", "Input Box", 245, 110, 10, FREEFILE
  smallbutton 190, 208, 310, 228, 7, 15, 8
  loadfont "winfont", "OK", 240, 208, 12, FREEFILE
  smallbutton 330, 208, 450, 228, 7, 15, 8
  loadfont "winfont", "Cancel", 365, 208, 12, FREEFILE
  loadfont "winfont", text$, xx1% + 10, yy1% + 5, 0, FREEFILE
  locatex% = 25
  locatey% = 12
  startscroll% = 32
  LOCATE locatey%, locatex%
  PRINT SPACE$(startscroll%)
  END IF

MouseShow
END SUB

FUNCTION inputline$ (limit%, startscroll%)

'limit% = 63
'startscroll% = 30
locatex% = 25
locatey% = 12
word$ = ""

LOCATE locatey%, locatex%
PRINT "_";
PRINT SPACE$(startscroll% - 1)

  DO
 
  KH$ = INKEY$
   
    IF KH$ <> "" THEN
     
      IF ASC(KH$) <> 34 AND ASC(KH$) <> 27 AND ASC(KH$) <> 13 AND ASC(KH$) <> 9 THEN
       
        IF ASC(KH$) = 8 AND LEN(word$) > 0 THEN
        word$ = LEFT$(word$, LEN(word$) - 1)
         
          IF LEN(word$) < startscroll% THEN
          LOCATE locatey%, locatex%
          PRINT RIGHT$(word$, startscroll%) + SPACE$(startscroll% - LEN(word$))
            ELSE
            LOCATE locatey%, locatex%: PRINT RIGHT$(word$, startscroll%)
          END IF
       
        ELSEIF ASC(KH$) <> 8 AND LEN(word$) < limit% THEN
        word$ = word$ + KH$
           
          IF LEN(word$) > startscroll% THEN
          LOCATE locatey%, locatex%: PRINT RIGHT$(word$, startscroll%)
            ELSE
            LOCATE locatey%, locatex%: PRINT word$
          END IF
       
        END IF
     
      END IF
   
    END IF
 
  LOOP UNTIL KH$ = CHR$(13)

  IF word$ = "" THEN
  LOCATE locatey%, locatex%
  PRINT SPACE$(startscroll%)
  END IF

inputline$ = word$

END FUNCTION

SUB loadbgd (file%, x%, y%, x2%, y2%, timeerase%)

TIMER OFF
  IF timeerase% = 1 THEN LINE (558, 457)-(638, 478), 7, BF  'Erasing the time...

KEY(10) OFF
MouseHide

  IF deskback% < 0 THEN
  DEF SEG = &HA000
  BLOAD graphicspath$ + "\" + bgdfile$
  DEF SEG
    ELSE
    LINE (x%, y%)-(x2%, y2%), deskback%, BF
  END IF

MouseShow
KEY(10) ON
TIMER ON

END SUB

SUB loadfont (filename$, text$, textx%, texty%, colour%, file%)

  IF filename$ = "" THEN
  LOCATE texty% \ 16 + 2, textx% \ 8: PRINT text$
  EXIT SUB
  END IF

'----------------------------------------------------------------------------
  IF UCASE$(RIGHT$(filename$, 3)) = "QBF" THEN
  OPEN fontspath$ + "\" + filename$ FOR RANDOM AS file% LEN = 2
  fprint text$, textx%, texty%, colour%, file%
  CLOSE file%
  EXIT SUB
  END IF

'----------------------------------------------------------------------------
x% = textx%
y% = texty% + 5
clr% = colour%
bold% = 1
DRAW "s4"

real.bold% = bold%
 
  WHILE bold% > -1
  DRAW "BM " + LTRIM$(STR$(x%)) + "," + LTRIM$(STR$(y%)) + " C" + LTRIM$(STR$(clr%))
    FOR k% = 1 TO LEN(text$)
    char$ = MID$(text$, k%, 1)
    DRAW windosfont$(ASC(char$))
      IF real.bold% > 0 THEN DRAW "BR1"
    NEXT k%
  bold% = bold% - 1
  x% = x% + 1
  WEND

END SUB

SUB mainbarpress (x%, y%, pressed%, buttons%)
 
  IF x% > 5 AND x% < 70 AND y% > 458 AND y% < 477 THEN    'Main button
  buttons% = 1
  pressed% = 1
  MouseHide
  smallbutton 5, 458, 70, 477, 7, 0, 15
  loadfont "winfont", "MAIN", 23, 458, 12, FREEFILE
  MouseShow
  END IF
 
  IF x% > 90 AND x% < 140 AND y% > 458 AND y% < 477 THEN
  buttons% = 2
  pressed% = 1
  MouseHide
  smallbutton 90, 458, 140, 477, 15, 2, 10
  loadfont "winfont", "cut", 108, 458, 3, FREEFILE
  MouseShow
  END IF

  IF x% > 143 AND x% < 193 AND y% > 458 AND y% < 477 THEN
  buttons% = 3
  pressed% = 1
  MouseHide
  smallbutton 143, 458, 193, 477, 15, 2, 10
  loadfont "winfont", "copy", 158, 458, 3, FREEFILE
  MouseShow
  END IF

  IF x% > 196 AND x% < 246 AND y% > 458 AND y% < 477 THEN
  buttons% = 4
  pressed% = 1
  MouseHide
  smallbutton 196, 458, 246, 477, 15, 2, 10
  loadfont "winfont", "paste", 209, 458, 3, FREEFILE
  MouseShow
  END IF

  IF x% > 249 AND x% < 299 AND y% > 458 AND y% < 477 THEN
  buttons% = 5
  pressed% = 1
  MouseHide
  smallbutton 249, 458, 299, 477, 15, 2, 10
  loadfont "winfont", "clear", 263, 458, 3, FREEFILE
  MouseShow
  END IF

  IF x% > 310 AND x% < 360 AND y% > 458 AND y% < 477 THEN
  buttons% = 6
  pressed% = 1
  MouseHide
  smallbutton 310, 458, 360, 477, 15, 2, 10
  loadfont "winfont", "refresh", 318, 458, 13, FREEFILE
  MouseShow
  END IF

  IF x% > 20 AND x% < 100 AND y% > 30 AND y% < 60 AND expon% = 0 THEN
  buttons% = 7
  pressed% = 1
  MouseHide
  smallbutton 20, 30, 100, 60, 15, 2, 10
  loadfont "winfont", "DiskTop", 34, 34, 12, FREEFILE
  MouseShow
  END IF

  IF x% > 20 AND x% < 100 AND y% > 70 AND y% < 100 AND expon% = 0 THEN
  buttons% = 8
  pressed% = 1
  MouseHide
  smallbutton 20, 70, 100, 100, 15, 2, 10
  loadfont "winfont", "Trash", 40, 75, 12, FREEFILE
  MouseShow
  END IF

END SUB

SUB mainbarrelease (x%, y%, pressed%, buttons%)
 
  IF buttons% = 1 THEN
  smallbutton 5, 458, 70, 477, 7, 15, 0
  loadfont "winfont", "MAIN", 22, 458, 12, FREEFILE
  taskbarpress% = 0
  driveson% = 0
  expbuttonson% = 0
  pulldownon% = 0
  KEY(10) OFF
  menu1
  END IF

  IF buttons% = 2 THEN
  smallbutton 90, 458, 140, 477, 15, 10, 2
  loadfont "winfont", "cut", 105, 458, 3, FREEFILE
  END IF

  IF buttons% = 3 THEN
  smallbutton 143, 458, 193, 477, 15, 10, 2
  loadfont "winfont", "copy", 155, 458, 3, FREEFILE
  END IF

  IF buttons% = 4 THEN
  smallbutton 196, 458, 246, 477, 15, 10, 2
  loadfont "winfont", "paste", 206, 458, 3, FREEFILE
  END IF

  IF buttons% = 5 THEN
  smallbutton 249, 458, 299, 477, 15, 10, 2
  loadfont "winfont", "clear", 260, 458, 3, FREEFILE
  END IF

  IF buttons% = 6 THEN
  smallbutton 310, 458, 360, 477, 15, 10, 2
  loadfont "winfont", "refresh", 315, 458, 13, FREEFILE
  LINE (0, 0)-(640, 454), 0, BF
  taskbar 0
    IF expon% = 0 THEN
    loadbgd FREEFILE, 1, 1, 640, 454, 1
    readdir "current.dir", FREEFILE
    setdesktop
      ELSE
      explorer
      setdrives
      buttons% = 0
    END IF
  END IF
 
  IF buttons% = 7 THEN
  smallbutton 20, 30, 100, 60, 15, 10, 2
  loadfont "winfont", "DiskTop", 30, 34, 12, FREEFILE
  pulldownon% = 1
  taskbarpress% = 1
  expon% = 1
  CLS
  taskbar 0
  buttons% = 0
  driveson% = 1
  explorer
  END IF

  IF buttons% = 8 THEN
  smallbutton 20, 70, 100, 100, 15, 10, 2
  loadfont "winfont", "Trash", 36, 75, 12, FREEFILE
  END IF

MouseShow

END SUB

SUB mainmenu1press (x%, y%, pressed%, buttons%)
 
  IF x% > 5 AND x% < 146 AND y% > 250 AND y% < 275 THEN
  pressed% = 1
  buttons% = 1
  MouseHide
  smallbutton 5, 250, 146, 275, 10, 8, 15
  loadfont "winfont", "Shortcuts", 13, 252, 13, FREEFILE
  MouseShow
  END IF
 
  IF x% > 5 AND x% < 146 AND y% > 290 AND y% < 315 THEN
  pressed% = 1
  buttons% = 2
  MouseHide
  smallbutton 5, 290, 146, 315, 10, 8, 15
  loadfont "winfont", "Settings", 13, 292, 13, FREEFILE
  MouseShow
  END IF

  IF x% > 5 AND x% < 146 AND y% > 330 AND y% < 355 THEN
  pressed% = 1
  buttons% = 3
  MouseHide
  smallbutton 5, 330, 146, 355, 10, 8, 15
  loadfont "winfont", "Find File", 13, 332, 13, FREEFILE
  MouseShow
  END IF

  IF x% > 5 AND x% < 146 AND y% > 370 AND y% < 395 THEN
  pressed% = 1
  buttons% = 4
  MouseHide
  smallbutton 5, 370, 146, 395, 10, 8, 15
  loadfont "winfont", "Run Program", 13, 372, 13, FREEFILE
  MouseShow
  END IF

  IF x% > 5 AND x% < 146 AND y% > 410 AND y% < 435 THEN
  pressed% = 1
  buttons% = 5
  MouseHide
  smallbutton 5, 410, 146, 435, 10, 8, 15
  loadfont "winfont", "Exit (F10)", 13, 412, 13, FREEFILE
  MouseShow
  END IF

  IF x% > 5 AND x% < 70 AND y% > 458 AND y% < 477 THEN    'Main button
  pressed% = 1
  buttons% = 6
  MouseHide
  smallbutton 5, 458, 70, 477, 7, 0, 15
  loadfont "winfont", "MAIN", 23, 458, 12, FREEFILE
  MouseShow
  END IF

END SUB

SUB mainmenu1release (x%, y%, pressed%, buttons%)
 
  IF buttons% = 1 THEN
  smallbutton 5, 250, 146, 275, 10, 15, 8
  loadfont "winfont", "Shortcuts", 10, 252, 13, FREEFILE
  END IF

  IF buttons% = 2 THEN
  smallbutton 5, 290, 146, 315, 10, 15, 8
  loadfont "winfont", "Settings", 10, 292, 13, FREEFILE
  END IF

  IF buttons% = 3 THEN
  smallbutton 5, 330, 146, 355, 10, 15, 8
  loadfont "winfont", "Find File", 10, 332, 13, FREEFILE
  END IF

  IF buttons% = 4 THEN                          'Run
    IF expon% = 0 THEN
    loadbgd FREEFILE, 1, 170, 150, 454, 1       'Refreshing...
      ELSE
      LINE (1, 170)-(150, 454), 0, BF
      expbuttons
      dirlist currentdir$
      showfiles filescroll%
    END IF
  infobox 320, 175, 490, 250, 2, "   Enter path and press <ENTER>"
  butprs% = captureinf(2)
    IF butprs% = 1 THEN
    ELSEIF butprs% = 2 THEN
    quickexe runpath$, ""
    ELSEIF butprs% = 3 THEN
      IF expon% = 0 THEN
      loadbgd FREEFILE, 149, 100, 491, 250, 1
      setdesktop
        ELSE
        showfiles filescroll%
      END IF
    END IF
  menu1press% = 0
  END IF

  IF buttons% = 5 THEN                  'Exit to dos
    IF expon% = 0 THEN
    loadbgd FREEFILE, 1, 170, 150, 454, 1
      ELSE
      LINE (1, 170)-(150, 454), 0, BF
      expbuttons
      dirlist currentdir$
      showfiles filescroll%
    END IF
  menu1press% = 0
  MouseShow
  finish
  END IF
 
  IF buttons% = 6 THEN
  taskbarpress% = 1
  menu1press% = 0
  smallbutton 5, 458, 70, 477, 7, 15, 0
  loadfont "winfont", "MAIN", 22, 458, 12, FREEFILE
    IF expon% = 0 THEN
    loadbgd FREEFILE, 1, 170, 150, 454, 1
    setdesktop
      ELSE
      LINE (1, 170)-(150, 454), 0, BF
      expbuttons
      dirlist currentdir$
      showfiles filescroll%
    END IF
  taskbar 0
  KEY(10) ON
  driveson% = 1
  expbuttonson% = 1
  pulldownon% = 1
  buttons% = 0
  END IF
 
MouseShow

END SUB

SUB menu1

menu1press% = 1

menux1% = 1
menuy1% = 454
menux2% = 150
menuy2% = 455

  FOR ycounter% = menuy1% TO 170 STEP -8
  LINE (menux1%, ycounter%)-(menux2%, menuy2%), 7, BF
  LINE (menux1% + 1, ycounter% + 1)-(menux2% - 1, menuy2% - 1), 15, B
  SOUND 32000, .07
  NEXT ycounter%

'(x%, y%, x2%, y2%, colour%, topcol%, botcol%) for button
'(filename$, text$, textx%, texty%, colour%, file%) for font

loadfont "impact.qbf", "WinDos", 25, 190, 14, FREEFILE

  FOR buttony% = 250 TO 430 STEP 40
  smallbutton 5, buttony%, 146, buttony% + 25, 10, 15, 8
  NEXT buttony%

loadfont "winfont", "Shortcuts", 10, 252, 13, FREEFILE
loadfont "winfont", "Settings", 10, 292, 13, FREEFILE
loadfont "winfont", "Find File", 10, 332, 13, FREEFILE
loadfont "winfont", "Run Program", 10, 372, 13, FREEFILE
loadfont "winfont", "Exit (F10)", 10, 412, 13, FREEFILE

END SUB

DEFLNG A-Z
SUB MouseDriver (ax%, bx%, cx%, dx%)
  DEF SEG = VARSEG(mouse$)
  mouse% = SADD(mouse$)
  CALL Absolute(ax%, bx%, cx%, dx%, mouse%)
END SUB

SUB MouseHide
 ax% = 2
 MouseDriver ax%, 0, 0, 0
END SUB

FUNCTION MouseInit%

  ax% = 0
  MouseDriver ax%, 0, 0, 0
  MouseInit% = ax%
END FUNCTION

SUB MousePut (x%, y%)
  ax% = 4
  cx% = x%
  dx% = y%
  MouseDriver ax%, 0, cx%, dx%
END SUB

SUB MouseRange (x1%, y1%, x2%, y2%)
  ax% = 7
  cx% = x1%
  dx% = x2%
MouseDriver ax%, 0, cx%, dx%
  ax% = 8
  cx% = y1%
  dx% = y2%
  MouseDriver ax%, 0, cx%, dx%
END SUB

SUB MouseShow
  ax% = 1
  MouseDriver ax%, 0, 0, 0
END SUB

DEFSNG A-Z
'DEFLNG A-Z
SUB MouseStatus (Lb%, Rb%, xMouse%, yMouse%)
  ax% = 3
  MouseDriver ax%, bx%, cx%, dx%
  Lb% = ((bx% AND 1) <> 0)
  Rb% = ((bx% AND 2) <> 0)
  xMouse% = cx%
  yMouse% = dx%
END SUB

SUB prg (x%, y%, extension$)

leng% = 60
wid% = 30
ctrllen% = 5
ctrlwid% = 4

'Drawing a black border...
LINE (x%, y%)-(x% + leng%, y% + wid%), 0, BF

'Drawing the border...
LINE (x%, y%)-(x% + leng%, y% + wid%), 7, B
LINE (x% + 1, y% + 1)-(x% + leng% - 1, y% + wid% - 1), 7, B

'Drawing the control box...
LINE (x% + 3, y% + 3)-(x% + 3 + ctrllen%, y% + 3 + ctrlwid%), 7, BF
LINE (x% + 4, (y% + 3 + y% + 3 + ctrlwid%) / 2)-(x% + 3 + ctrllen% - 1, (y% + 3 + y% + 3 + ctrlwid%) / 2)

'Drawing the title bar...
LINE (x% + 3 + ctrllen% + 2, y% + 3)-(x% + leng% - 3, y% + 3 + ctrlwid%), 9, BF

'Drawing type...
  IF LCASE$(extension$) = "bat" THEN
  loadfont "terminal.qbf", LCASE$(extension$), x% + 19, y% + 7, 10, FREEFILE
  EXIT SUB
  END IF
loadfont "terminal.qbf", LCASE$(extension$), x% + 19, y% + 5, 10, FREEFILE

END SUB

SUB pullbutt (x%, y%, pressed%, active%, pullpress%)

  IF x% > 1 AND x% < 50 AND y% > 2 AND y% < 16 THEN
  MouseHide
  pulldown
    IF active% <> 1 THEN
      IF active% <> 0 AND active% <> 4 THEN pullrefresh 0
      IF active% = 4 THEN pullrefresh 1
    taskbarpress% = 0
    expbuttonson% = 0
    active% = 1
    driveson% = 0
    tinybutton 1, 2, 50, 16, 7, 0, 15
    loadfont "winfont", "File", 10, 0, 0, FREEFILE
    pullmenu x%, y%, pressed%, active%
      ELSE
      GOSUB disappear
      taskbarpress% = 1
      expbuttonson% = 1
    END IF
  pullpress% = 1
  MouseShow
  END IF

  IF x% > 51 AND x% < 130 AND y% > 2 AND y% < 16 THEN
  MouseHide
  pulldown
    IF active% <> 2 THEN
      IF active% <> 0 AND active% <> 4 THEN pullrefresh 0
      IF active% = 4 THEN pullrefresh 1
    taskbarpress% = 0
    expbuttonson% = 0
    active% = 2
    driveson% = 0
    tinybutton 51, 2, 130, 16, 7, 0, 15
    loadfont "winfont", "View", 75, 0, 0, FREEFILE
    pullmenu x%, y%, pressed%, active%
      ELSE
      GOSUB disappear
      taskbarpress% = 1
      expbuttonson% = 1
    END IF
  pullpress% = 1
  MouseShow
  END IF
 
  IF x% > 131 AND x% < 205 AND y% > 2 AND y% < 16 THEN
  MouseHide
  pulldown
    IF active% <> 3 THEN
      IF active% <> 0 AND active% <> 4 THEN pullrefresh 0
      IF active% = 4 THEN pullrefresh 1
    taskbarpress% = 0
    expbuttonson% = 0
    active% = 3
    driveson% = 0
    tinybutton 131, 2, 205, 16, 7, 0, 15
    loadfont "winfont", "Quick", 150, 0, 0, FREEFILE
    pullmenu x%, y%, pressed%, active%
      ELSE
      GOSUB disappear
      taskbarpress% = 1
      expbuttonson% = 1
    END IF
  pullpress% = 1
  MouseShow
  END IF
 
  IF x% > 590 AND x% < 637 AND y% > 2 AND y% < 16 THEN
  MouseHide
  pulldown
    IF active% <> 4 THEN
      IF active% <> 0 THEN pullrefresh 0
    taskbarpress% = 0
    expbuttonson% = 0
    active% = 4
    driveson% = 0
    tinybutton 590, 2, 637, 16, 7, 0, 15
    loadfont "winfont", "Help", 600, 0, 0, FREEFILE
    pullmenu x%, y%, pressed%, active%
      ELSE
      GOSUB disappear
      taskbarpress% = 1
      expbuttonson% = 1
    END IF
  pullpress% = 1
  MouseShow
  END IF

'----------------------------------------------------------------------------
'Testing for more clicks on the menus...

  IF active% = 1 THEN                          'FILE menu
    IF x% > 1 AND x% < 180 THEN

      IF y% > 27 AND y% < 43 THEN        'Quick Execute  pressed
      x1% = 6: y1% = 27: x2% = 175: y2% = 43: text$ = "Quick Execute": xtext% = 8: ytext% = 25
      GOSUB blink
      GOSUB disappear
        FOR counter% = 1 TO 4000
          IF selobj%(counter%) <> 0 THEN
          whatfile counter%, file$, extension$, directory%
            IF directory% = 0 THEN
              extension$ = LCASE$(extension$)
                IF extension$ = "exe" OR extension$ = "com" OR extension$ = "bat" THEN
                quickexe file$ + "." + extension$, ""
                EXIT SUB
                END IF
            END IF
          END IF
        NEXT counter%
      STOP        'Add InfoBox (no selected files) !!!!!!!!!!!!!!!!!!!!!!!!!!
      END IF
     
      IF y% > 42 AND y% < 58 THEN        'Command Line  pressed
      x1% = 6: y1% = 42: x2% = 175: y2% = 58: text$ = "Command Line . . .": xtext% = 8: ytext% = 40
      GOSUB blink
      GOSUB disappear
      infobox 320, 175, 490, 250, 3, "         COMMAND LINE :"
      butprs% = captureinf(3)
      END IF

      IF y% > 72 AND y% < 88 THEN        'Create  pressed
      GOSUB disappear
      END IF

      IF y% > 87 AND y% < 103 THEN        'Delete  pressed
      x1% = 6: y1% = 87: x2% = 175: y2% = 103: text$ = "Delete . . .": xtext% = 8: ytext% = 85
      GOSUB blink
      GOSUB disappear
'Add INFOBOX to confirm the deletion!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      STOP
      erasefile ""
      END IF

      IF y% > 102 AND y% < 118 THEN        'Rename  pressed
      x1% = 6: y1% = 102: x2% = 175: y2% = 118: text$ = "Rename . . .": xtext% = 8: ytext% = 100
      GOSUB blink
      GOSUB disappear
      END IF
     
      IF y% > 132 AND y% < 148 THEN        'Send To  pressed
      GOSUB disappear
      END IF

      IF y% > 147 AND y% < 163 THEN         'Copy To  pressed
      x1% = 6: y1% = 147: x2% = 175: y2% = 163: text$ = "Copy To . . .": xtext% = 8: ytext% = 145
      GOSUB blink
      GOSUB disappear
      END IF
     
      IF y% > 162 AND y% < 178 THEN        'Move To  pressed
      x1% = 6: y1% = 162: x2% = 175: y2% = 178: text$ = "Move To . . .": xtext% = 8: ytext% = 160
      GOSUB blink
      GOSUB disappear
      END IF

      IF y% > 192 AND y% < 208 THEN        'Select All  pressed
      x1% = 6: y1% = 192: x2% = 175: y2% = 208: text$ = "Select All": xtext% = 8: ytext% = 190
      GOSUB blink
        FOR counter% = 1 TO 1000
          IF directories$(counter%) = "" THEN counter% = counter% - 1: EXIT FOR
        selobj%(counter%) = 1
        NEXT counter%
          FOR counter2% = 1 TO 3000
            IF files$(counter2%) = "" THEN EXIT FOR
          selobj%(counter% + counter2%) = 1
          NEXT counter2%
      GOSUB disappear
      END IF
     
      IF y% > 207 AND y% < 223 THEN        'Deselect All  pressed
      x1% = 6: y1% = 207: x2% = 175: y2% = 223: text$ = "Deselect All": xtext% = 8: ytext% = 205
      GOSUB blink
      ERASE selobj%
      GOSUB disappear
      END IF
     
      IF y% > 222 AND y% < 238 THEN        'Select  pressed
      x1% = 6: y1% = 222: x2% = 175: y2% = 238: text$ = "Select . . .": xtext% = 8: ytext% = 220
      GOSUB blink
      GOSUB disappear
      END IF
     
      IF y% > 252 AND y% < 268 THEN        'Compress  pressed
      x1% = 6: y1% = 252: x2% = 175: y2% = 268: text$ = "Compress . . .": xtext% = 8: ytext% = 250
      GOSUB blink
      GOSUB disappear
      END IF
     
      IF y% > 267 AND y% < 283 THEN        'Decompress  pressed
      x1% = 6: y1% = 267: x2% = 175: y2% = 283: text$ = "Decompress . . .": xtext% = 8: ytext% = 265
      GOSUB blink
      GOSUB disappear
      END IF
     
      IF y% > 297 AND y% < 313 THEN        'Associate  pressed
      x1% = 6: y1% = 297: x2% = 175: y2% = 313: text$ = "Associate . . .": xtext% = 8: ytext% = 295
      GOSUB blink
      GOSUB disappear
      END IF
     
      IF y% > 327 AND y% < 343 THEN        'Print  pressed
      x1% = 6: y1% = 327: x2% = 175: y2% = 343: text$ = "Print . . .": xtext% = 8: ytext% = 325
      GOSUB blink
      GOSUB disappear
      END IF
     
      IF y% > 357 AND y% < 373 THEN        'Exit DiskTop  pressed
      x1% = 6: y1% = 357: x2% = 175: y2% = 373: text$ = "Exit DiskTop": xtext% = 8: ytext% = 355
      GOSUB blink
      CLS
      expon% = 0
      loadbgd 1, 0, 0, 640, 480, 0
      taskbar 0
      setdesktop
      active% = 0
      taskbarpress% = 1
      expbuttonson% = 1
      END IF

    END IF
  END IF


  IF active% = 2 THEN                              'VIEW menu
    IF x% > 50 AND x% < 230 THEN
   
      IF y% > 27 AND y% < 43 THEN        'As Large  pressed
      x1% = 55: y1% = 27: x2% = 225: y2% = 43: text$ = "AS  Large": xtext% = 57: ytext% = 25
      GOSUB blink
      GOSUB disappear
      END IF

      IF y% > 42 AND y% < 58 THEN        'As List  pressed
      x1% = 55: y1% = 42: x2% = 225: y2% = 58: text$ = "AS  List": xtext% = 57: ytext% = 40
      GOSUB blink
      GOSUB disappear
      END IF
     
      IF y% > 72 AND y% < 88 THEN        'Arrange Icons  pressed
      GOSUB disappear
      END IF
     
      IF y% > 87 AND y% < 103 THEN       'Attributes  pressed
      GOSUB disappear
      END IF

      IF y% > 117 AND y% < 133 THEN      'Options  pressed
      x1% = 55: y1% = 117: x2% = 225: y2% = 133: text$ = "Options . . .": xtext% = 57: ytext% = 115
      GOSUB blink
      GOSUB disappear
      END IF

      IF y% > 147 AND y% < 163 THEN        'System Files  pressed
      x1% = 55: y1% = 147: x2% = 225: y2% = 163: text$ = "System Files": xtext% = 57: ytext% = 145
      GOSUB blink
      GOSUB disappear
      olddir$ = currentdir$
      currentdir$ = defaultdrive$ + "\"
      quickexe editor + " " + system1 + " " + system2 + " " + system3, olddir$
      END IF
     
      IF y% > 177 AND y% < 193 THEN        'File Contents  pressed
      x1% = 55: y1% = 177: x2% = 225: y2% = 193: text$ = "File Cont. (more mem)": xtext% = 57: ytext% = 175
      GOSUB blink
      GOSUB disappear
      editfile
      END IF

      IF y% > 192 AND y% < 208 THEN        'Properties  pressed
      x1% = 55: y1% = 192: x2% = 225: y2% = 208: text$ = "Properties . . .": xtext% = 57: ytext% = 190
      GOSUB blink
      GOSUB disappear
      END IF

    END IF
  END IF


  IF active% = 3 THEN
    IF x% > 130 AND x% < 310 THEN
'    GOSUB disappear
    END IF
  END IF


  IF active% = 4 THEN
    IF x% > 520 AND x% < 637 THEN

      IF y% > 27 AND y% < 43 THEN        'About  pressed
      x1% = 525: y1% = 27: x2% = 632: y2% = 43: text$ = "About . . .": xtext% = 526: ytext% = 25
      GOSUB blink
      GOSUB disappear
      olddir$ = currentdir$
      currentdir$ = windospath$
      quickexe about, olddir$
      END IF
   
    END IF
  END IF


EXIT SUB

'==================================SUBS======================================
blink:
MouseHide
  FOR counter% = 1 TO 5
  LINE (x1%, y1%)-(x2%, y2%), 2, BF
  loadfont "winfont", text$, xtext%, ytext%, 15, FREEFILE
  delay 3
  LINE (x1%, y1%)-(x2%, y2%), 7, BF
  loadfont "winfont", text$, xtext%, ytext%, 1, FREEFILE
  delay 3
  NEXT counter%
MouseShow
RETURN

disappear:
MouseHide
pulldown
  IF active% = 4 THEN pullrefresh 1 ELSE pullrefresh 0
active% = 0
driveson% = 1
taskbarpress% = 1
expbuttonson% = 1
MouseShow
RETURN

END SUB

SUB pulldown
LOCATE 1, 1

  IF expon% = 1 THEN
  LINE (0, 1)-(640, 17), 7, BF
  LINE (1, 2)-(638, 16), 15, B
  loadfont "winfont", "File", 10, 0, 0, FREEFILE
  loadfont "winfont", "View", 75, 0, 0, FREEFILE
  loadfont "winfont", "Quick", 150, 0, 0, FREEFILE
  loadfont "winfont", "Help", 600, 0, 0, FREEFILE
    ELSE
    loadbgd FREEFILE, 0, 0, 640, 17, 0
  END IF

END SUB

SUB pullmenu (x%, y%, pressed%, active%)

triangle$ = "d12e6h6d5br1p1,1"
 
  IF active% = 1 THEN
  x1% = 1: x2% = 180: y1% = 20: y2% = 390
  GOSUB scrollmenu
  loadfont "winfont", "Quick Execute", 8, 25, 1, FREEFILE
  loadfont "winfont", "Command Line . . .", 8, 40, 1, FREEFILE
  crackline 0, x1% + 7, 65, x2% - 7, 65             'Drawing double line...
  loadfont "winfont", "Create", 8, 70, 1, FREEFILE
  PSET (165, 73), 1
  DRAW triangle$
  loadfont "winfont", "Delete . . .", 8, 85, 1, FREEFILE
  loadfont "winfont", "Rename . . .", 8, 100, 1, FREEFILE
  crackline 0, x1% + 7, 125, x2% - 7, 125             'Drawing double line...
  loadfont "winfont", "Send To", 8, 130, 1, FREEFILE
  PSET (165, 133), 1
  DRAW triangle$
  loadfont "winfont", "Copy To . . .", 8, 145, 1, FREEFILE
  loadfont "winfont", "Move To . . .", 8, 160, 1, FREEFILE
  crackline 0, x1% + 7, 185, x2% - 7, 185             'Drawing double line...
  loadfont "winfont", "Select All", 8, 190, 1, FREEFILE
  loadfont "winfont", "Deselect All", 8, 205, 1, FREEFILE
  loadfont "winfont", "Select . . .", 8, 220, 1, FREEFILE
  crackline 0, x1% + 7, 245, x2% - 7, 245             'Drawing double line...
  loadfont "winfont", "Compress . . .", 8, 250, 1, FREEFILE
  loadfont "winfont", "Decompress . . .", 8, 265, 1, FREEFILE
  crackline 0, x1% + 7, 290, x2% - 7, 290             'Drawing double line...
  loadfont "winfont", "Associate . . .", 8, 295, 1, FREEFILE
  crackline 0, x1% + 7, 320, x2% - 7, 320             'Drawing double line...
  loadfont "winfont", "Print . . .", 8, 325, 1, FREEFILE
  crackline 0, x1% + 7, 350, x2% - 7, 350             'Drawing double line...
  loadfont "winfont", "Exit DiskTop", 8, 355, 1, FREEFILE
  END IF

  IF active% = 2 THEN
  x1% = 50: x2% = 230: y1% = 20: y2% = 220
  GOSUB scrollmenu
  loadfont "winfont", "AS  Large", 57, 25, 1, FREEFILE
  loadfont "winfont", "AS  List", 57, 40, 1, FREEFILE
  crackline 0, x1% + 7, 65, x2% - 7, 65             'Drawing double line...
  loadfont "winfont", "Arrange Icons", 57, 70, 1, FREEFILE
  PSET (214, 73), 1
  DRAW triangle$
  loadfont "winfont", "Attributes", 57, 85, 1, FREEFILE
  PSET (214, 88), 1
  DRAW triangle$
  crackline 0, x1% + 7, 110, x2% - 7, 110             'Drawing double line...
  loadfont "winfont", "Options . . .", 57, 115, 1, FREEFILE
  crackline 0, x1% + 7, 140, x2% - 7, 140             'Drawing double line...
  loadfont "winfont", "System Files", 57, 145, 1, FREEFILE
  crackline 0, x1% + 7, 170, x2% - 7, 170             'Drawing double line...
  loadfont "winfont", "File Cont. (more mem)", 57, 175, 1, FREEFILE
  loadfont "winfont", "Properties . . .", 57, 190, 1, FREEFILE
  END IF
 
  IF active% = 3 THEN
  x1% = 130: x2% = 310: y1% = 20: y2% = 350
  GOSUB scrollmenu
  END IF
 
  IF active% = 4 THEN
  x1% = 520: x2% = 637: y1% = 20: y2% = 100
  GOSUB scrollmenu
  loadfont "winfont", "About . . .", 526, 25, 1, FREEFILE
  END IF

EXIT SUB


'----------------------------------------------------------------------------
scrollmenu:
  FOR height% = y1% + 1 TO y2% STEP 10
  LINE (x1%, y1%)-(x2%, height%), 7, BF
  LINE (x1% + 1, y1% + 1)-(x2% - 1, height% - 1), 15, B
  NEXT height%
RETURN

END SUB

SUB pullrefresh (refresh1%)
    
  IF refresh1% = 0 THEN
  LINE (0, 41)-(500, 54), 0, BF
  setdrives
  showfiles filescroll%
    ELSE
    LINE (520, 41)-(640, 91), 0, BF
    setdrives
    expbuttons
    showfiles filescroll%
  END IF

END SUB

SUB quickexe (program$, switchto$)

  MouseHide
  KEY(10) OFF
  TIMER OFF
  SCREEN 0
  CLS
  SHELL LEFT$(currentdir$, 2)
  CHDIR currentdir$
  SHELL program$ + " " + cmdline$
    IF switchto$ <> "" THEN
    SHELL LEFT$(switchto$, 2)
    CHDIR switchto$
    currentdir$ = switchto$
    END IF
  PRINT "Press any key to continue . . ."
  SLEEP
  ERASE selobj%
  SCREEN 12
  CLS
  mainbarrelease 0, 0, 0, 6
  TIMER ON
  KEY(10) ON
  MouseShow
  MousePut 320, 240

END SUB

SUB raisedframe (x%, y%, x2%, y2%)

LINE (x%, y%)-(x2%, y%), 15
xx% = x% + 1
LINE (xx%, y%)-(xx%, y2%), 15
LINE (x%, y%)-(x%, y2%), 15
LINE (x2%, y%)-(x2%, y2%), 0
xx% = x2% - 1
LINE (xx%, y%)-(xx%, y2%), 0
LINE (x%, y2%)-(x2%, y2%), 0

END SUB

SUB readdir (file$, file%)

CHDIR LEFT$(currentdir$, 3)
SHELL "dir " + currentdir$ + " /ogn >" + windospath$ + "\" + file$

pulldown

ERASE directories$
ERASE files$


OPEN windospath$ + "\" + file$ FOR INPUT AS file%

  FOR counter = 1 TO 4                  'Skipping junk...
  LINE INPUT #file%, junk$
    IF EOF(file%) = -1 THEN EXIT FOR
  NEXT counter


    DO UNTIL EOF(file%) = -1
    LINE INPUT #file%, junk$            'Reading something...
      IF MID$(junk$, 1, 1) <> " " AND MID$(junk$, 1, 2) <> ". " AND junk$ <> "" THEN
      space% = INSTR(1, junk$, " ")
      first$ = MID$(junk$, 1, space% - 1)
      extension$ = MID$(junk$, 10, 3)
        IF INSTR(junk$, "<DIR>") > 1 THEN    'Directory is found.
        dirnumber% = dirnumber% + 1
          IF extension$ <> "   " THEN    'Stupid WINDOWS directory extensions.
            FOR number% = 3 TO 1 STEP -1
            character$ = MID$(extension$, number%, 1)
              IF character$ <> " " THEN EXIT FOR
            NEXT number%
              extension$ = MID$(extension$, 1, number%)
              directories$(dirnumber%) = first$ + "." + extension$
          ELSE
            directories$(dirnumber%) = first$
          END IF
        extension$ = ""
        ELSE
        filenumber% = filenumber% + 1
        filename$ = first$ + "." + extension$
        files$(filenumber%) = filename$
        END IF
      END IF
    LOOP

CLOSE file%

filenum% = dirnumber% + filenumber%

END SUB

SUB readini

PRINT "Reading configuration . . ."

OPEN ininame FOR INPUT AS #1
 
  DO UNTIL EOF(1) = -1
  LINE INPUT #1, something$
    IF LEFT$(UCASE$(something$), 10) = "BACKGROUND" THEN
    GOSUB lookspace
    bgdfile$ = MID$(something$, equal%, LEN(something$) - equal% + 1)
      ELSEIF LEFT$(UCASE$(something$), 6) = "WINDOS" THEN
      GOSUB lookspace
      windospath$ = MID$(something$, equal%, LEN(something$) - equal% + 1)
    ELSEIF LEFT$(UCASE$(something$), 5) = "TRASH" THEN
    GOSUB lookspace
    trashpath$ = MID$(something$, equal%, LEN(something$) - equal% + 1)
      ELSEIF LEFT$(UCASE$(something$), 4) = "LOGS" THEN
      GOSUB lookspace
      logspath$ = MID$(something$, equal%, LEN(something$) - equal% + 1)
    ELSEIF LEFT$(UCASE$(something$), 4) = "TEMP" THEN
    GOSUB lookspace
    temppath$ = MID$(something$, equal%, LEN(something$) - equal% + 1)
      ELSEIF LEFT$(UCASE$(something$), 8) = "GRAPHICS" THEN
      GOSUB lookspace
      graphicspath$ = MID$(something$, equal%, LEN(something$) - equal% + 1)
    ELSEIF LEFT$(UCASE$(something$), 5) = "FONTS" THEN
    GOSUB lookspace
    fontspath$ = MID$(something$, equal%, LEN(something$) - equal% + 1)
      ELSEIF LEFT$(UCASE$(something$), 9) = "ARCHTOOLS" THEN
      GOSUB lookspace
      archpath$ = MID$(something$, equal%, LEN(something$) - equal% + 1)
    ELSEIF LEFT$(UCASE$(something$), 8) = "DESKBACK" THEN
    GOSUB lookspace
    deskback$ = MID$(something$, equal%, LEN(something$) - equal% + 1)
    deskback% = VAL(deskback$)
      ELSEIF LEFT$(UCASE$(something$), 7) = "EXPCALL" THEN
      GOSUB lookspace
      expcall$ = MID$(something$, equal%, LEN(something$) - equal% + 1)
      expcall% = VAL(expcall$)
    ELSEIF LEFT$(UCASE$(something$), 10) = "CURRENTDIR" THEN
    GOSUB lookspace
    currentdir$ = MID$(something$, equal%, LEN(something$) - equal% + 1)
      ELSEIF LEFT$(UCASE$(something$), 10) = "FILESCROLL" THEN
      GOSUB lookspace
      filescroll$ = MID$(something$, equal%, LEN(something$) - equal% + 1)
      filescroll% = VAL(filescroll$)
    ELSEIF LEFT$(UCASE$(something$), 12) = "DEFAULTDRIVE" THEN
    GOSUB lookspace
    defaultdrive$ = MID$(something$, equal%, LEN(something$) - equal% + 1)
    END IF
 
  LOOP

CLOSE #1
EXIT SUB

'----------------------------------------------------------------------------
lookspace:
equal% = INSTR(something$, "=")
  DO
  equal% = equal% + 1
  character$ = MID$(something$, equal%, 1)
  LOOP WHILE character$ = " "
RETURN
'----------------------------------------------------------------------------

END SUB

SUB runitem (file$, extension$, directory%)

  IF directory% = 1 THEN
  ERASE selobj%
    IF file$ = ".." THEN
    updir
    GOTO skip
      ELSE
      changedir file$
    END IF
skip:
  MouseHide
  filescroll% = 1
  readdir "current.dir", FREEFILE     'Refreshing...
  showfiles filescroll%
  dirlist currentdir$
  MouseShow
  END IF
'----------------------------------------------------------------------------

  IF extension$ = "EXE" OR extension$ = "COM" THEN
  writebatch file$, extension$, 1
  savecfg file$, extension$
  SHELL LEFT$(windospath$, 2)
  SHELL "CD " + RIGHT$(windospath$, LEN(windospath$) - 2)
  RUN runner
  END IF
'----------------------------------------------------------------------------

  IF extension$ = "BAT" THEN
  TIMER OFF
  MouseHide
  SCREEN 0
  CLS
  SHELL "CD " + MID$(currentdir$, 3, LEN(currentdir$) - 2)
  SHELL file$ + ".BAT"
  PRINT "Press any key to continue . . ."
  SLEEP
  SCREEN 12
  CLS
  mainbarrelease 0, 0, 0, 6
  setdrives
  TIMER ON
  END IF

END SUB

SUB savecfg (file$, extension$)

file1% = FREEFILE
OPEN windospath$ + "\" + ininame FOR INPUT AS file1%
file2% = FREEFILE
OPEN windospath$ + "\" + MID$(ininame, 1, INSTR(ininame, ".")) + "TMP" FOR OUTPUT AS file2%

  DO UNTIL EOF(file1%) = -1
  LINE INPUT #file1%, rubbish$
    IF LCASE$(MID$(rubbish$, 1, 7)) = "expcall" THEN EXIT DO
    IF LCASE$(MID$(rubbish$, 1, 5)) = "batch" THEN EXIT DO
    IF LCASE$(MID$(rubbish$, 1, 10)) = "currentdir" THEN EXIT DO
    IF LCASE$(MID$(rubbish$, 1, 10)) = "filescroll" THEN EXIT DO
  PRINT #file2%, rubbish$
  LOOP

CLOSE file1%
CLOSE file2%

KILL windospath$ + "\" + ininame
NAME windospath$ + "\" + MID$(ininame, 1, INSTR(ininame, ".")) + "TMP" AS windospath$ + "\" + ininame

OPEN windospath$ + "\" + ininame FOR APPEND AS file1%
PRINT #file1%, "CurrentDir = "; currentdir$
PRINT #file1%, "FileScroll ="; filescroll%

  IF file$ = "" AND extension$ = "" THEN
  PRINT #file1%, "ExpCall = 0"
  CLOSE file1%
  EXIT SUB
    ELSE
    PRINT #file1%, "ExpCall = 1"
    PRINT #file1%, "Batch = "; temppath$ + "\" + batch
  END IF

CLOSE file1%

END SUB

SUB selectobject (file$, extension$, directory%, xfile%, yfile%, sel%)

  IF file$ = "" AND extension$ = "" THEN EXIT SUB

  IF sel% = 1 THEN
  LINE (xfile% - 20, yfile% - 20)-(xfile% + 80, yfile% + 50), 1, BF
    ELSE
    LINE (xfile% - 20, yfile% - 20)-(xfile% + 80, yfile% + 50), 0, BF
  END IF
'----------------------------------------------------------------------------

  IF directory% = 1 THEN
  dir xfile%, yfile%
  loadfont "", MID$(file$, 1, 1) + LCASE$(MID$(file$, 2, LEN(file$) - 1)), xfile% + 8, yfile% + 35, 15, FREEFILE
  EXIT SUB
  END IF

'----------------------------------------------------------------------------
extension$ = LCASE$(extension$)

  IF extension$ = "exe" OR extension$ = "com" OR extension$ = "bat" THEN
  prg xfile%, yfile%, extension$
    ELSEIF extension$ = "txt" OR extension$ = "doc" OR extension$ = "ini" THEN
    txt xfile%, yfile%
  ELSEIF extension$ = "srt" THEN
  shortcut xfile%, yfile%
    ELSEIF extension$ = "zip" OR extension$ = "rar" OR extension$ = "pak" OR extension$ = "arj" OR extension$ = "zoo" THEN
    arch xfile%, yfile%
  ELSE
  unknown xfile%, yfile%
  END IF

loadfont "", MID$(file$, 1, 1) + LCASE$(MID$(file$, 2, LEN(file$) - 1)) + "." + extension$, xfile%, yfile% + 35, 15, FREEFILE

END SUB

SUB setdesktop

smallbutton 20, 30, 100, 60, 15, 10, 2
loadfont "winfont", "DiskTop", 30, 34, 12, FREEFILE

smallbutton 20, 70, 100, 100, 15, 10, 2
loadfont "winfont", "Trash", 36, 75, 12, FREEFILE

END SUB

SUB setdrives

MouseHide

LINE (-1, 50)-(640, 51), 10, B
LINE (-1, 49)-(640, 52), 2, B

  FOR drivenum% = 1 TO 26
  junk$ = drive$(drivenum%)
    IF junk$ = "" THEN EXIT FOR
  NEXT drivenum%

lastx% = INT(640 / drivenum%)

  FOR counter% = 1 TO drivenum%
  smallbutton lastx% * counter% - lastx%, 20, lastx% * counter% - 1, 40, 6, 7, 8
  NEXT counter%

loadfont "winfont", "A:", lastx% / 2 - 7, 20, 2, FREEFILE

  FOR counter% = 2 TO drivenum%
  loadfont "winfont", CHR$(65 + counter%) + ":", (counter% * lastx% + (counter% - 1) * lastx% + 1) / 2 - 7, 20, 2, FREEFILE
  NEXT counter%

MouseShow

END SUB

SUB shortcut (x%, y%)

leng% = 60
wid% = 30

LINE (x%, y%)-(x% + leng%, y% + wid%), 0, BF
LINE (x% + leng% / 3, y% + wid% / 3)-(x% + leng%, y% + wid%), 3, BF
LINE (x% + leng% / 3 - 10, y% + wid% / 3 - 5)-(x% + leng% - 10, y% + wid% - 5), 12, BF
LINE (x%, y%)-(x% + leng% - 20, y% + wid% - 10), 14, BF

END SUB

SUB showfiles (startfile%)

MouseHide

startx% = 40
starty% = 90
stepx% = 104
stepy% = 80

LINE (0, 55)-(564, 420), 0, BF        'Erasing old files...

'----------------------------------------------------------------------------
  DO
  dircount% = dircount% + 1              'Counting directories...
  LOOP UNTIL directories$(dircount%) = ""

dircount% = dircount% - 1
'----------------------------------------------------------------------------
 
  FOR counter% = startfile% TO startfile% + 19
 
    IF startx% >= 540 THEN startx% = 40: starty% = starty% + stepy%

    IF counter% <= dircount% THEN
    filename$ = directories$(counter%)
    directory% = 1
    position% = 1            'No GOSUB to make it faster...
    GOTO selcheck
point1:
    startx% = startx% + stepx%
     
      ELSE
      filename$ = files$(counter% - dircount%)
        IF filename$ = "" THEN EXIT FOR
      extension$ = LCASE$(MID$(filename$, LEN(filename$) - 2, 3))
      directory% = 0
      position% = 2
      GOTO selcheck
point2:
      startx% = startx% + stepx%
   
    END IF
    
  NEXT counter%

MouseShow
EXIT SUB

'===================================SUBS=====================================
selcheck:
  IF directory% = 0 THEN
  realfile$ = LEFT$(filename$, LEN(filename$) - 4)
    ELSE
    realfile$ = filename$
  END IF

  IF selobj%(counter%) = 1 THEN
  selectobject realfile$, extension$, directory%, startx%, starty%, 1
    ELSE
    selectobject realfile$, extension$, directory%, startx%, starty%, 0
  END IF

  IF position% = 1 THEN GOTO point1 ELSE GOTO point2

END SUB

SUB smallbutton (x%, y%, x2%, y2%, colour%, topcol%, botcol%)

LINE (x%, y%)-(x2%, y2%), topcol%, BF                    'Main rectangles
LINE (x% + 2, y% + 2)-(x2% - 2, y2% - 2), colour%, BF
                                       
LINE (x2%, y%)-(x2% - 1, y% + 1), botcol%                 'Corner lines
LINE (x%, y2%)-(x% + 1, y2% - 1), botcol%

  FOR count% = 0 TO 1 STEP 1
  LINE (x% + count%, y2% - count%)-(x2% - count%, y2% - count%), botcol%
  LINE (x2% - count%, y2% - count%)-(x2% - count%, y% + count%), botcol%
  NEXT count%

 PAINT (x% + 4, y2% - 1), botcol%

END SUB

SUB taskbar (zoom%)

MouseHide

x% = 320
y% = 455
x2% = 640
y2% = 480

  IF zoom% = 1 THEN
    DO UNTIL x% = x2%                           'Drawing a zooming taskbar...
    x% = x% + 5
    LINE (640 - x%, y%)-(x%, y2%), 7, BF
    LINE (640 - x% + 1, y% + 1)-(x% - 1, y2% - 1), 15, B
    SOUND 32000, .1
    LOOP
  ELSE
  LINE (0, 455)-(640, 480), 7, BF               'Drawing taskbar as quickly
  LINE (1, 456)-(639, 479), 15, B               'as we can...
  END IF

smallbutton 5, 458, 70, 477, 7, 15, 0              'Drawing the main button...
loadfont "winfont", "MAIN", 22, 458, 12, FREEFILE

crackline 1, 74, 459, 74, 476                 'Drawing double line...
                                     
  FOR counter% = 90 TO 290 STEP 53           'Drawing a few buttons...
  smallbutton counter%, 458, counter% + 50, 477, 15, 10, 2
  NEXT counter%
                                             
smallbutton 310, 458, 360, 477, 15, 10, 2     'Drawing a refresh button...

crackline 1, 555, 459, 555, 476                 'Drawing double line...

crackline 1, 371, 459, 371, 476                 'Drawing double line...

loadfont "winfont", "cut", 105, 458, 3, FREEFILE
loadfont "winfont", "copy", 155, 458, 3, FREEFILE
loadfont "winfont", "paste", 206, 458, 3, FREEFILE
loadfont "winfont", "clear", 260, 458, 3, FREEFILE
loadfont "winfont", "refresh", 315, 458, 13, FREEFILE

MouseShow

END SUB

SUB tinybutton (x%, y%, x2%, y2%, colour%, topcol%, botcol%)

LINE (x%, y%)-(x2%, y2%), botcol%, B        'Drawing main rectangle...
PAINT (x% + 1, y% + 1), colour%, botcol%
LINE (x%, y%)-(x2%, y%), topcol%
LINE (x%, y%)-(x%, y2%), topcol%

END SUB

SUB txt (x%, y%)

leng% = 60
wid% = 30

'Drawing a black frame...
LINE (x%, y%)-(x% + leng% + 5, y% + wid% + 5), 0, BF

'Drawing circles...
  FOR counterx% = x% + 5 TO x% + leng% STEP 5
  CIRCLE (counterx%, y% + 5), 5, 10
  NEXT counterx%

'Drawing paper...
PSET (x%, y% + 5), 11
DRAW "d" + STR$(wid%) + "r" + STR$(leng% + 5) + "u" + STR$(wid%)

LINE (x% + 2, y% + 10)-(x% + leng% + 3, y% + wid% + 3), 15, BF

END SUB

SUB unknown (x%, y%)

leng% = 60
wid% = 30

PSET (x%, y%), 15
unknownfile$ = "d" + STR$(wid%) + "r" + STR$(leng%) + "u" + STR$(wid% - 10) + "h10 d10 r10 h10 l" + STR$(leng% - 10)
DRAW unknownfile$

loadfont "timesnew.qbf", "Xx", x% + 10, y%, 15, FREEFILE

END SUB

SUB updir
 
  FOR counter% = LEN(currentdir$) TO 1 STEP -1
  character$ = MID$(currentdir$, counter%, 1)
    IF character$ = "\" THEN EXIT FOR
  NEXT counter%

currentdir$ = LEFT$(currentdir$, counter% - 1)
 
  IF LEN(currentdir$) = 2 THEN
  currentdir$ = currentdir$ + "\"
  END IF

END SUB

SUB whatfile (number%, file$, extension$, directory%)

  FOR counter% = 1 TO number%
    IF counter% = 1001 THEN GOTO checkfiles
  something$ = directories$(counter%)
    IF something$ = "" THEN GOTO checkfiles
  NEXT counter%

file$ = something$            'This is a directory !!!
extension$ = ""
directory% = 1
EXIT SUB

'----------------------------------------------------------------------------
checkfiles:

  FOR counter2% = 1 TO number% - (counter% - 1)
  something$ = files$(counter2%)
    IF something$ = "" OR counter2% = 3000 THEN
    file$ = ""
    extension$ = ""
    EXIT SUB
    END IF
  NEXT counter2%

dot% = INSTR(1, something$, ".")
extension$ = MID$(something$, dot% + 1, 3)
file$ = MID$(something$, 1, dot% - 1)
directory% = 0

END SUB

SUB writebatch (file$, extension$, wtype%)

file% = FREEFILE

OPEN temppath$ + "\runprg.bat" FOR OUTPUT AS file%
PRINT #file%, "@ECHO OFF"

  SELECT CASE wtype%
    CASE 1
    PRINT #file%, LEFT$(currentdir$, 2)
    PRINT #file%, "CD " + MID$(currentdir$, 3, LEN(currentdir$) - 3 + 1)
    PRINT #file%, file$ + "." + extension$
    PRINT #file%, LEFT$(windospath$, 2)
    PRINT #file%, "CD " + MID$(windospath$, 3, LEN(windospath$) - 3 + 1)
  END SELECT
                                                            
PRINT #file%, "PAUSE"
CLOSE file%

END SUB

