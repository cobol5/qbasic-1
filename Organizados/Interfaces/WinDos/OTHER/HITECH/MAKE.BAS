'Make.BAS by Denis Andrianov.
'Crap code... after all you only have to run this once......

'Fonts
'TED file format developed by PARADiSE
'By Nick Gardiner
'shadow@georgeg.force9.co.uk

'============================================================================
DECLARE FUNCTION getcol% ()
DECLARE SUB init ()
DECLARE SUB displayted (text$, fontname$, palname$, x%, y%, jumperror%, setpal%)


DIM x AS INTEGER
DIM y AS INTEGER
DIM D AS INTEGER
REDIM SHARED pic(32200) AS INTEGER
REDIM SHARED mp(15751) AS INTEGER
DIM vsp AS INTEGER
DIM vsp2 AS INTEGER
vsp = VARSEG(pic(0))
vsp2 = vsp + 2000

SCREEN 13
CLS

FOR i% = 0 TO 127
nul% = 0
ii% = i% \ 2
OUT &H3C8, i%
OUT &H3C9, nul%
OUT &H3C9, ii%
OUT &H3C9, nul%
NEXT

FOR i% = 128 TO 255
nul% = (i% - 127) \ 2
ii% = 63
OUT &H3C8, i%
OUT &H3C9, nul%
OUT &H3C9, ii%
OUT &H3C9, nul%
NEXT i%

DEF SEG = (&HA000)
FOR y = 1 TO 100
  FOR x = 0 TO 319
    POKE (y * 320 + x), RND * 250
  NEXT x
NEXT y
DEF SEG = (&HA000 + &H7D0)
FOR y = 1 TO 99
  FOR x = 0 TO 319
    POKE (y * 320 + x), RND * 250
  NEXT x
NEXT y

CALL displayted("HITECH", "font022.ted", "font022.pal", 60, 40, 10, 0)
CALL displayted("LABS", "font022.ted", "font022.pal", 90, 100, 10, 0)


FOR D = 1 TO 2

  DEF SEG = (&HA000)

  FOR y = 1 TO 100
    FOR x = 0 TO 319

      s% = PEEK(y * 320 + x) + PEEK(y * 320 + x + 1) + PEEK(y * 320 + x - 1) + PEEK(y * 320 + 320 + x) + PEEK(y * 320 - 320 + x) '+ PEEK(y * 320 - 320 + x - 1) + PEEK(y * 320 - 320 + x + 1) + PEEK(y * 320 + 320 + x - 1) + PEEK(y * 320 + 320 + x + 1)
      s% = s% \ 5
      POKE (y * 320 + x), s%

    NEXT x
  NEXT y

  DEF SEG = (&HA000 + &H7D0)

  FOR y = 1 TO 99
    FOR x = 0 TO 319

      s% = PEEK(y * 320 + x) + PEEK(y * 320 + x + 1) + PEEK(y * 320 + x - 1) + PEEK(y * 320 + 320 + x) + PEEK(y * 320 - 320 + x) '+ PEEK(y * 320 - 320 + x - 1) + PEEK(y * 320 - 320 + x + 1) + PEEK(y * 320 + 320 + x - 1) + PEEK(y * 320 + 320 + x + 1)
      s% = s% \ 5
      POKE (y * 320 + x), s%

    NEXT x
  NEXT y
NEXT D



FOR y = 1 TO 100
  ym% = 320 * y
  FOR x = 0 TO 319
    yd% = ym% + x
    DEF SEG = (&HA000)
    e = PEEK(yd%)
    DEF SEG = vsp
    POKE (yd%), e
  NEXT x
NEXT y
DEF SEG = (&HA7D0)
FOR y = 1 TO 99
  ym% = 320 * y
  FOR x = 0 TO 319
    yd% = ym% + x
    DEF SEG = (&HA7D0)
    e = PEEK(yd%)
    DEF SEG = vsp2
    POKE (yd%), e
  NEXT x
NEXT y


DEF SEG = VARSEG(pic(0))
BSAVE "logo.bin", 0, 64400
DEF SEG

OPEN "logo.pal" FOR BINARY AS #1
FOR k% = 1 TO (256 * 3) / 2
num1% = getcol%
num2% = getcol%
num3% = num2% * 256 + num1%
PUT #1, , num3%
NEXT
CLOSE

SUB displayted (text$, fontname$, palname$, x%, y%, jumperror%, setpal%)

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

IF setpal% = 0 THEN
OPEN palname$ FOR BINARY AS #1
FOR pp% = 0 TO 255
 r$ = " "
 g$ = " "
 b$ = " "
 GET #1, , r$
 GET #1, , g$
 GET #1, , b$
 r% = ASC(r$)
 g% = ASC(g$)
 b% = ASC(b$)
   IF r% > 0 OR g% > 0 OR b% > 0 THEN
'   OUT 968, pp%
'   OUT 969, r%
'   OUT 969, g%
'   OUT 969, b%
   END IF
NEXT
CLOSE #1
END IF


GOTO endthis

drawchar:
xwidth$ = " ": yhight$ = " "
GET #1, , xwidth$
GET #1, , yhight$
xwidth% = ASC(xwidth$)
yhight% = ASC(yhight$)

FOR yy% = 1 TO yhight%
 FOR xx% = 1 TO xwidth%
  col$ = " "
  GET #1, , col$
  col% = ASC(col$)
    IF col% > 0 THEN
    PSET (x% + xx%, y% + yy%), 250'col%
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

