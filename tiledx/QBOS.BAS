DECLARE FUNCTION MouseInit% ()
DECLARE SUB MouseDriver (AX%, bx%, CX%, DX%)
DECLARE SUB TEXT ()
'Filename: QBOS.BAS
'Windows operating system

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

