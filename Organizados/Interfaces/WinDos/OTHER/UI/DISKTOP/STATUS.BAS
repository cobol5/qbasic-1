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


FOR i = 0 TO 10
SetPal i, i * 6, i * 6, 0
NEXT i



'INT ((upperbound - lowerbound + 1)*RND + lowerbound)

FOR y = 455 TO 479 STEP 2
  FOR x = 0 TO 639 STEP 2
    'PSET (x, y), INT((10 - 4 + 1) * RND + 4)
    LINE (x, y)-(x + 1, y + 1), INT((10 - 5 + 1) * RND + 5), BF
  NEXT x
NEXT y



'CALL displayted("PRINT", "font012.ted", "font003.pal", 211, 430, 10, 0, 14)



FOR d = 1 TO 2

  FOR y = 455 TO 479
    FOR x = 0 TO 639

      s% = POINT(x, y) + POINT(x + 1, y) + POINT(x, y + 1) + POINT(x + 1, y + 1)
      s% = s% \ 4
      PSET (x, y), s%

    NEXT x
  NEXT y


NEXT d


SLEEP

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

IF SetPal1% = 0 THEN
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

