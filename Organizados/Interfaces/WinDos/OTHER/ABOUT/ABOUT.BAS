'Copyright (C) HiTech Laboratories, Denis Andrianov.
'WinDos Demo
'============================================================================
'$INCLUDE: 'swmpbas.bi'

DECLARE SUB ReadINI ()
DECLARE SUB LoadFont (Text$, Size%, Bold%, Colour%, X%, Y%)
DECLARE SUB SetPal (Colour%, R%, G%, B%)
DECLARE SUB Fade (ColAttr%, FadeTo%, Pause%)
DECLARE SUB Delay (time%)
DECLARE SUB Finish ()

'============================================================================
FarHeap& = SETMEM(0)
temp& = SETMEM(-FarHeap& + 4000)

CONST MainINI = "windos.ini"
CONST MainFNT = "windos.fnt"

DIM SHARED ChannelNum%
DIM SHARED FontsPath$
DIM SHARED WindosFont$(255)
DIM SHARED Pals(255, 2)
'1st num = colour attrubute
'2nd num = 0(Red), 1(green), 2(blue)

ReadINI

OPEN FontsPath$ + MainFNT FOR INPUT AS #1
  FOR Counter% = 1 TO 255
  LINE INPUT #1, WindosFont$(Counter%)
  NEXT Counter%
CLOSE #1

SetPal 16, 60, 60, 20      'Main fading attribute (changes)
Pals(16, 0) = 60
Pals(16, 1) = 60
Pals(16, 2) = 20

SetPal 17, 50, 40, 45      'Blue+Green
Pals(17, 0) = 50
Pals(17, 1) = 40
Pals(17, 2) = 45

SetPal 18, 10, 50, 10      'Green
Pals(18, 0) = 10
Pals(18, 1) = 50
Pals(18, 2) = 10

SetPal 19, 60, 10, 10      'Red
Pals(19, 0) = 60
Pals(19, 1) = 10
Pals(19, 2) = 10

SetPal 20, 0, 0, 0      'Spare attribute (black by default)

'============================================================================

SCREEN 13
CLS

FileName$ = "sweet5.mod" + CHR$(0)
IsSound% = ModInitSound%
ChannelNum% = ModLoadModule%(VARSEG(FileName$), SADD(FileName$))
  IF ChannelNum% >= 4 THEN
  ModPlayModule
  END IF

LINE (0, 1)-(320, 20), 15, BF
LINE (0, 21)-(320, 40), 1, BF
LINE (0, 41)-(320, 60), 4, BF

LoadFont "  WinDos", 8, 3, 10, 90, 22

  IF IsSound% = 0 THEN
    LoadFont "No sound available !!!", 4, 1, 16, 95, 120
  ELSEIF ChannelNum% < 4 THEN
    LoadFont "Music can't be loaded !!!", 4, 1, 16, 90, 120
  ELSE
    LoadFont "Turn up your speakers !!!", 4, 1, 16, 90, 120
  END IF
Fade 16, 0, 10

LINE (0, 61)-(320, 200), 0, BF

LoadFont "This program was created by", 4, 1, 16, 80, 120
Fade 16, 17, 5
Fade 16, 0, 5

LINE (0, 61)-(320, 200), 0, BF

LoadFont "HiTech Laboratories", 4, 1, 16, 105, 120
LoadFont "1999 - 2003", 4, 1, 16, 125, 140
Fade 16, 17, 5
Fade 16, 0, 5

LINE (0, 61)-(320, 200), 0, BF

LoadFont "by Denis Andrianov", 4, 1, 16, 105, 120
Fade 16, 17, 5
Delay 200
Fade 16, 0, 5

LINE (0, 61)-(320, 200), 0, BF

LoadFont "This program is devoted to:", 4, 1, 16, 77, 120
Fade 16, 17, 5
Fade 16, 0, 5

LINE (0, 61)-(320, 200), 0, BF

LoadFont "Tatiana Shmotina", 4, 1, 16, 110, 100
LoadFont "and", 4, 1, 16, 150, 120
LoadFont "Everyone who died in the KYPCK (Kursk) submarine", 4, 1, 16, 1, 140
LoadFont "accident.", 4, 1, 16, 135, 152
Fade 16, 17, 5
Delay 700
Fade 16, 0, 10

LINE (0, 61)-(320, 200), 0, BF

LoadFont "If you happen to find a bug in this program, please", 4, 1, 16, 1, 80
LoadFont "send me a letter via E-Mail.", 4, 1, 16, 1, 100
COLOR 16
LOCATE 20, 1
PRINT "E-MAIL: D_ANDRIANOV@HOTMAIL.COM"
Fade 16, 18, 5
Delay 700
Fade 16, 0, 5

LINE (0, 61)-(320, 200), 0, BF

LoadFont "If you like this program, please send us a", 4, 1, 16, 1, 100
LoadFont "donation. Read README.TXT for more details.", 4, 1, 16, 30, 120
Fade 16, 18, 5
Delay 400
Fade 16, 0, 5

LINE (0, 61)-(320, 200), 0, BF

LoadFont "CREDITS:", 4, 1, 16, 135, 120
Fade 16, 17, 5
Fade 16, 0, 5

LINE (0, 61)-(320, 200), 0, BF

LoadFont "Libraries:", 4, 1, 16, 130, 120
Fade 16, 17, 5
Fade 16, 0, 5

LINE (0, 61)-(320, 200), 0, BF

LoadFont "Future 3.0", 4, 1, 16, 1, 100
LoadFont "-- Michael Rye Sorensen", 4, 1, 16, 140, 100
LoadFont "-- Jorden Chamid", 4, 1, 16, 140, 115
LoadFont "SWMP 1.41", 4, 1, 16, 1, 140
LoadFont "-- Lord Excess", 4, 1, 16, 140, 140
Fade 16, 18, 5
Delay 400
Fade 16, 0, 5

LINE (0, 61)-(320, 200), 0, BF

LoadFont "Fonts:", 4, 1, 16, 135, 120
Fade 16, 17, 5
Fade 16, 0, 5

LINE (0, 61)-(320, 200), 0, BF

LoadFont "QBF Font Collection", 4, 1, 16, 1, 120
LoadFont "-- Bobby K. (Krusty)", 4, 1, 16, 140, 120
Fade 16, 18, 5
Delay 400
Fade 16, 0, 5

LINE (0, 61)-(320, 200), 0, BF

LoadFont "Special Thanks To:", 4, 1, 16, 105, 120
Fade 16, 17, 5
Fade 16, 0, 5

LINE (0, 61)-(320, 200), 0, BF

LoadFont "Ilia Leikin", 4, 1, 16, 1, 85
LoadFont "Ross Koger", 4, 1, 16, 1, 100
LoadFont "Daniel Slode", 4, 1, 16, 1, 115
LoadFont "Laurens van Klaveren", 4, 1, 16, 1, 130
LoadFont "Yuri Prokushev", 4, 1, 16, 1, 145
LoadFont "Steven Cambian", 4, 1, 16, 1, 160

Fade 16, 18, 5
Delay 400
Fade 16, 0, 5

LINE (0, 61)-(320, 200), 0, BF

PSET (80, 190), 16
DRAW "ta10e50be5e45 ta-10f45bf5f50 ta30h65bh10h65 ta0r125br10r125 ta-30g65bg10g69 ta0"
PAINT (160, 100), 16, 16
Fade 16, 19, 5
LoadFont "Thank You for using our software.", 4, 1, 20, 60, 140
Fade 20, 18, 5
Delay 400
Fade 16, 0, 3
  FOR i% = 1 TO 3
  Fade 16, 19, 3
  Fade 16, 0, 3
  NEXT
Delay 200
Fade 20, 0, 10

LINE (0, 61)-(320, 200), 0, BF

LoadFont "WinDos from Russia with love!!!", 4, 1, 16, 60, 120
Fade 16, 19, 5
Delay 200
Fade 16, 0, 5

LINE (0, 61)-(320, 200), 0, BF

LoadFont "No animals were harmed in the making", 4, 1, 16, 45, 100
LoadFont "of this program.", 4, 1, 16, 115, 120
LoadFont "(exceptions apply)", 4, 1, 16, 104, 140
Fade 16, 17, 5
Delay 200
Fade 16, 0, 5

LINE (0, 61)-(320, 200), 0, BF

LoadFont "Welcome to WinDos environment.", 4, 1, 16, 63, 120
Fade 16, 17, 5
Delay 200
Fade 16, 0, 5

LINE (0, 61)-(320, 200), 0, BF

LoadFont "Enjoy the music!", 4, 1, 16, 110, 120
Fade 16, 17, 5
Delay 200
Fade 16, 0, 5

Finish

SUB Delay (time%)

'time%=67 (1 second)

  FOR i% = 1 TO time%
  WAIT &H3DA, 8, 8
  WAIT &H3DA, 8
 
    IF INKEY$ = CHR$(27) THEN Finish

  NEXT i%

END SUB

SUB Fade (ColAttr%, FadeTo%, Pause%)

CurR! = Pals(ColAttr%, 0)
CurG! = Pals(ColAttr%, 1)
CurB! = Pals(ColAttr%, 2)

ToR! = Pals(FadeTo%, 0)
ToG! = Pals(FadeTo%, 1)
ToB! = Pals(FadeTo%, 2)

Pals(ColAttr%, 0) = Pals(FadeTo%, 0)
Pals(ColAttr%, 1) = Pals(FadeTo%, 1)
Pals(ColAttr%, 2) = Pals(FadeTo%, 2)

StepR! = (CurR! - ToR!) / 32
StepG! = (CurG! - ToG!) / 32
StepB! = (CurB! - ToB!) / 32

  FOR Counter% = 1 TO 32
  CurR! = CurR! - StepR!
  CurG! = CurG! - StepG!
  CurB! = CurB! - StepB!
  SetPal ColAttr%, INT(CurR!), INT(CurG!), INT(CurB!)
  Delay Pause%
  NEXT

END SUB

SUB Finish
 
  IF ChannelNum% >= 4 THEN
  ModStopModule
  END IF

ModCloseSound
END

END SUB

SUB LoadFont (Text$, Size%, Bold%, Colour%, X%, Y%)

DRAW "S" + LTRIM$(STR$(Size%))

real.bold% = Bold%

  WHILE Bold% > -1
  DRAW "BM " + LTRIM$(STR$(X%)) + "," + LTRIM$(STR$(Y%)) + " C" + LTRIM$(STR$(Colour%))
    FOR k% = 1 TO LEN(Text$)
    char$ = MID$(Text$, k%, 1)
    DRAW WindosFont$(ASC(char$))
      IF real.bold% > 0 THEN DRAW "BR1"
    NEXT k%
  Bold% = Bold% - 1
  X% = X% + 1
  WEND

END SUB

SUB ReadINI

File% = FREEFILE
OPEN MainINI FOR INPUT AS #File%

  DO UNTIL EOF(File%) = -1
  LINE INPUT #File%, something$
  something$ = LTRIM$(RTRIM$(something$))

    IF LEFT$(UCASE$(something$), 5) = "FONTS" THEN
    GOSUB LookSpace
    FontsPath$ = MID$(something$, equal%, LEN(something$) - equal% + 1)
      IF RIGHT$(FontsPath$, 1) <> "\" THEN
      FontsPath$ = FontsPath$ + "\"
      END IF
    END IF

  LOOP

CLOSE #File%

EXIT SUB

'----------------------------------------------------------------------------
LookSpace:
equal% = INSTR(something$, "=")
  DO
  equal% = equal% + 1
  character$ = MID$(something$, equal%, 1)
  LOOP WHILE character$ = " "
RETURN
'----------------------------------------------------------------------------

END SUB

SUB SetPal (Colour%, R%, G%, B%)

OUT &H3C8, Colour%
OUT &H3C9, R%
OUT &H3C9, G%
OUT &H3C9, B%

END SUB

