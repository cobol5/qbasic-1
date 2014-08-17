'Make.BAS by Denis Andrianov.
'Crap code... after all you only have to run this once......

'Fonts
'TED file format developed by PARADiSE
'By Nick Gardiner
'shadow@georgeg.force9.co.uk

'============================================================================
DECLARE SUB ShadeButton (x%, y%, x2%, y2%, DotNum&)
DECLARE SUB TinyButton (x%, y%, x2%, y2%, colour%, topcol%, botcol%)
DECLARE SUB SetPal (c%, r%, g%, b%)
DECLARE FUNCTION getcol% ()
DECLARE SUB init ()
DECLARE SUB displayted (text$, fontname$, palname$, x%, y%, jumperror%, SetPal1%, CLR%)

DEFINT A-Z


DIM x AS INTEGER
DIM y AS INTEGER
DIM d AS INTEGER
REDIM SHARED pic(32200) AS INTEGER
REDIM SHARED mp(15751) AS INTEGER
DIM vsp AS INTEGER
DIM vsp2 AS INTEGER
vsp = VARSEG(pic(0))
vsp2 = vsp + 2000

SCREEN 12
CLS


FOR i = 0 TO 13
SetPal i, 0, i * 4, 0
NEXT i


FOR y = 0 TO 479
  FOR x = 0 TO 639
    PSET (x, y), INT(RND * 14)
  NEXT x
NEXT y



CALL displayted("WINDOS   COMPRESSION", "font022.ted", "font003.pal", 33, 20, 10, 0, 13)
'END

                                 '5 11 12
ShadeButton 330, 420, 450, 460, 14000  'Bottom buttons
ShadeButton 190, 420, 310, 460, 14000

ShadeButton 10, 100, 300, 295, 90000      'Left frame
ShadeButton 339, 100, 629, 295, 90000     'Right frame

CALL displayted("BEGIN", "font012.ted", "font003.pal", 208, 430, 10, 0, 14)
CALL displayted("DONE", "font012.ted", "font003.pal", 356, 430, 10, 0, 14)



FOR d = 1 TO 2

  FOR y = 0 TO 479
    FOR x = 0 TO 639

      s% = POINT(x, y) + POINT(x + 1, y) + POINT(x, y + 1) + POINT(x + 1, y + 1)
      s% = s% \ 4
      PSET (x, y), s%

    NEXT x
  NEXT y


NEXT d


GOTO skip
'==========================
'super button


x = 100
y = 130
step1 = 10

  FOR i = 3 TO 13 STEP 2
  LINE (x - step1, y - step1)-(x + step1, y + step1), i, BF
  step1 = step1 - 1
  NEXT


x = 200
y = 230
step1 = 10

  FOR i = 13 TO 3 STEP -2
  LINE (x - step1, y - step1)-(x + step1, y + step1), i, BF
  step1 = step1 - 1
  NEXT

'===================================


skip:

TinyButton 330, 420, 450, 460, 7, 15, 0
TinyButton 190, 420, 310, 460, 7, 15, 0

TinyButton 10, 100, 300, 295, 7, 8, 13   'Left frame
TinyButton 339, 100, 629, 295, 7, 8, 13  'Right frame




DEF SEG = &HA000

OUT &H3CE, 4
OUT &H3CF, 0
BSAVE "compress.im1", 0, (640& * 480) / 8

OUT &H3CE, 4
OUT &H3CF, 1
BSAVE "compress.im2", 0, (640& * 480) / 8

OUT &H3CE, 4
OUT &H3CF, 2
BSAVE "compress.im3", 0, (640& * 480) / 8

OUT &H3CE, 4
OUT &H3CF, 3
BSAVE "compress.im4", 0, (640& * 480) / 8

DEF SEG

DEFSNG A-Z
SUB displayted (text$, fontname$, palname$, x%, y%, jumperror%, SetPal1%, CLR%)

OPEN fontname$ FOR BINARY AS #1
size = LOF(1)


FOR char% = 1 TO LEN(text$)
SEEK #1, 21

currentchr$ = LEFT$(text$, char%)
currentchr$ = RIGHT$(currentchr$, 1)


DO
thischar$ = " "
GET #1, , thischar$
IF thischar$ = currentchr$ THEN GOSUB drawchar: GOTO nextchar

xwidth$ = " ": yhight$ = " "
GET #1, , xwidth$
GET #1, , yhight$
xwidth% = ASC(xwidth$)
yhight% = ASC(yhight$)
cpos = SEEK(1)
IF cpos > size THEN
x% = x% + jumperror%
GOTO nextchar
END IF
fartoseek = (xwidth% * yhight%) + cpos
SEEK #1, fartoseek
LOOP

nextchar:

NEXT

CLOSE #1


GOTO endthis

drawchar:
xwidth$ = " ": yhight$ = " "
GET #1, , xwidth$
GET #1, , yhight$
xwidth% = ASC(xwidth$)
yhight% = ASC(yhight$)

FOR yy% = 1 TO yhight%
 FOR xx% = 1 TO xwidth%
  COL$ = " "
  GET #1, , COL$
  COL% = ASC(COL$)
    IF COL% > 0 THEN
    PSET (x% + xx%, y% + yy%), CLR%
    END IF
 NEXT
NEXT
x% = x% + xwidth%
RETURN

endthis:
END SUB

FUNCTION getcol%

STATIC i%
STATIC times%

  IF times% = 3 THEN times% = 0
times% = times% + 1

  SELECT CASE times%
    CASE 1
    OUT &H3C7, i%
    getcol% = INP(&H3C9)
    i% = i% + 1

    CASE 2, 3
    getcol% = INP(&H3C9)
  END SELECT

END FUNCTION

DEFINT A-Z
SUB SetPal (c, r, g, b)

OUT &H3C8, c
OUT &H3C9, r
OUT &H3C9, g
OUT &H3C9, b

END SUB

SUB ShadeButton (x%, y%, x2%, y2%, DotNum&)

'INT ((upperbound - lowerbound + 1)*RND + lowerbound)

  FOR i& = 0 TO DotNum&
  xr% = INT((x2% - x% + 1) * RND + x%)
  yr% = INT((y2% - y% + 1) * RND + y%)
  cr% = INT((7 - 2 + 1) * RND + 2)
  PSET (xr%, yr%), cr%
  NEXT

END SUB

SUB TinyButton (x%, y%, x2%, y2%, colour%, topcol%, botcol%)

'LINE (x%, y%)-(x2%, y2%), colour%, BF      'Drawing main rectangle...

LINE (x%, y%)-(x2%, y%), topcol%
LINE (x%, y%)-(x%, y2%), topcol%

LINE (x2%, y%)-(x2%, y2%), botcol%
LINE (x%, y2%)-(x2%, y2%), botcol%

END SUB

