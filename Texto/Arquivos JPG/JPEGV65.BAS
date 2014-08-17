DECLARE SUB chg.seg (ARR%(), RSEG%, ROFF%)
DECLARE SUB FPrint (t$, XStartPos%, YStartPos%, Colour&, XSize%, YSize%, ChSpacing%, font%)
DECLARE SUB SVGAGetModeInfo (md&)
'JPEG VIEWER V 2 by Antoni Gual    agual@eic.ictnet.es
'----------------------------------------------------------------------------
'-Thanks to all people who e-mailed me about first version, they helped me in
'  crucial issues and encouraged me to release this new version.
'-Thanks to Dmitri Brant, whose JPEG programs (in ABC packets) put me to work!
'  His program structure can still be seen in this program.
'----------------------------------------------------------------------------
'-Now It Works in QBasic as well as in QB 4.5(Thanks to the fine Mark Andryk's
'  interrupt converter -see it in ABC packets- and to the invaluable help from
'  Mike Gregory who found the ellusive bug that prevented compatibility)
'  (BTW: Those who say QBasic is half as fast as QB 4.5 are right!)
'-QB 4.5 still need to be called with /l qb.qlb
'-Now it includes an assembler routine -needed to call interrupts in QBasic-
'  (No interrupts = No SVGA)
'-New User Friendly file selector, featuring LFN in W9x. I copied an idea and
' layout by William Yu, but i reprogrammed all from scratch.
'-Solved some bugs in SUB JPEGGetParms, it displayed wrong values.
'-Converted putpixel routine to a single LONG parameter for color (slower)
'-Killed the bug in SVGAPutpixel that made white text look yellow - Torben
'  Schramme found it!
'-Now it does'nt put a limit in the quantity  ofSVGA modes your card can have,
'  so you are now allowed to expend as much money for your card as you want;).
'-Does'nt keep in memory an array with all info for all SVGA modes.In fact
'   this info can be retrieved from the graphics card when it's needed.
'-Changed SVGAPrint to a new routine that scales chars, so they can be read
'   in all modes.
'-Added a complete BSAVE-BLOAD routine. You can cut'n paste BLOAD to your progs
'   to load images saved with JPEG VIEWER's BSAVE.
'   (My routines are much SLOWER than QB's BSAVE-BLOAD but they have the
'    advantage of saving everything to a single file. You could use the faster
'    QB's routines, but I warn you: A single image in 800X600x32 mode will
'    generate 32 files!)
'-Added uncompressed size and compression rate to the file parameters display.
'----------------------------------------------------------------------------
TYPE RegTypeX
   ax    AS INTEGER
   bx    AS INTEGER
   cx    AS INTEGER
   dx    AS INTEGER
   bp    AS INTEGER
   si    AS INTEGER
   DI    AS INTEGER
   flags AS INTEGER
   ds    AS INTEGER
   es    AS INTEGER
END TYPE

DECLARE SUB SVGABLoad (fil$)
DECLARE SUB SVGABSave (fil$)
DECLARE SUB DetectWinTemp ()
DECLARE FUNCTION getcurdir$ (save%)
DECLARE FUNCTION DetectWindows% ()
DECLARE SUB interrupty (intnum AS INTEGER, inreg AS RegTypeX, outreg AS RegTypeX)
DECLARE FUNCTION DiskReady% (d$)
DECLARE FUNCTION SelectAFile$ (header$, ext$)
DECLARE FUNCTION mouseint% (func%, fil%, col%)
DECLARE FUNCTION SVGASelectMode% ()
DECLARE SUB svgappixel (x%, y%, clr&)
DECLARE SUB JPEGViewParms ()
DECLARE SUB menu ()
DECLARE SUB JPEGGetParms (jfile%)
DECLARE FUNCTION filesel$ (prompt$, match%, ext$)
DECLARE SUB JPEGGet8x8 (vector%(), comp%, dcCoef%)
DECLARE FUNCTION JPEGGetByte% ()
DECLARE FUNCTION svgasetmode% (mode%)
DECLARE SUB SVGAPrint (cad$, y%, x%, clr&)
DECLARE SUB JPEGPut (jfile%, x0%, Y0%)
DECLARE SUB svgappgrey (x%, y%, lum%)
DECLARE SUB SVGAGetData ()




'.............................................................................

DEFINT A-Z
CONST TOTALBUF = 2000
CONST dc = 0, AC = 1
CONST false = 0, true = NOT false
CONST white = &HFFFFFF
CONST SVGABSaveIdent = "AGV"
CONST bloadsiz = 16384
CONST banksteps = 65536 \ bloadsiz
CONST palsize = 256 * 3


TYPE vesainfoblock
	VESASignature       AS STRING * 4
	VESAVersion         AS INTEGER
	OEMStringPtr        AS LONG
	Capabilities        AS STRING * 4
	VIDEOMODEPTR        AS INTEGER
	VIDEOMODESEG        AS INTEGER
	TotalMemory         AS INTEGER
	Reserved            AS STRING * 236
'we manage this  part
	modenum             AS INTEGER
	modemax             AS INTEGER
	bytespixel          AS INTEGER
	xres                AS INTEGER
	yres                AS INTEGER
	bytesrow            AS LONG
	bpp                 AS INTEGER
	winsize             AS LONG
	winseg              AS INTEGER
	numberofbanks       AS INTEGER
	bw                  AS INTEGER
	charx               AS INTEGER
	chary               AS INTEGER
END TYPE


TYPE vesaModeinfoBlock
	Modeattributes      AS INTEGER
	WinAAttributes      AS STRING * 1
	WinBAttributes      AS STRING * 1
	WinGranularity      AS INTEGER
	winsize             AS INTEGER
	winAsegment         AS INTEGER
	WinBSegment         AS INTEGER
	WinFuncPtr          AS LONG
	bytesperscanline    AS INTEGER
	xres                AS INTEGER
	yres                AS INTEGER
	XCharSize           AS STRING * 1
	YCharSize           AS STRING * 1
	NumberOfPlanes      AS STRING * 1
	bpp                 AS STRING * 1
	numberofbanks       AS STRING * 1
	MemoryModel         AS STRING * 1
	BankSize            AS STRING * 1
	numpages            AS STRING * 1
	Rsvd                AS STRING * 1
	RedMaskSize         AS STRING * 1
	RedFieldPosition    AS STRING * 1
	GreenMaskSize       AS STRING * 1
	GreenFieldPosition  AS STRING * 1
	BlueMaskSize        AS STRING * 1
	BlueFieldPosition   AS STRING * 1
	RsvdMaskSize        AS STRING * 1
	DirectColorModeInfo AS STRING * 1
	Reserved            AS STRING * 216
END TYPE




TYPE JpegType
  jfifmajor     AS STRING * 1
  jfifMinor     AS STRING * 1
  densunits     AS STRING * 1   'density units and values (not used)
  Xdens         AS INTEGER
  ydens         AS INTEGER
  ThWidth       AS STRING * 1   'thumbnail size
  Theigth       AS STRING * 1
  rows          AS INTEGER      'jpeg height
  cols          AS INTEGER      'jpeg width
  samplesy      AS INTEGER      'sampling ratios
  samplescbcr   AS INTEGER
  qty           AS INTEGER      'number of quantization tables
  qtcbr         AS INTEGER
  HDCTY         AS INTEGER      'number of huffman tables (DC and AC)
  HDCTCBR       AS INTEGER
  HaCTY         AS INTEGER
  HaCTcbr       AS INTEGER
  numcomp       AS INTEGER      'number of components
  restart       AS INTEGER      'blocks between restart marks
  SIZE          AS LONG         'FILE SIZE
  IMAGESTART    AS LONG         'NOT USED
END TYPE

TYPE HuffmanEntry               'a type for huffman tables
  Index         AS LONG
  code          AS INTEGER
  mask          AS LONG
  Length        AS INTEGER
END TYPE

TYPE zigzagtype
 xp             AS INTEGER
 yp             AS INTEGER
END TYPE

'for menu definition
TYPE menutype
	top     AS INTEGER
	heigth  AS INTEGER
	left    AS INTEGER
	wdth    AS INTEGER
	typ     AS INTEGER
	curs    AS INTEGER
	count   AS INTEGER
	tline   AS INTEGER
	file    AS INTEGER
END TYPE





'a few global variables

DIM SHARED jfile
DIM SHARED win, temppath$
DIM SHARED viw AS INTEGER, lasty AS INTEGER
DIM BUF$, bufptr AS LONG, endptr AS LONG, find$: find$ = CHR$(255) + CHR$(0)
DIM SHARED buf2ptr AS INTEGER, buf2 AS LONG
DIM SHARED vesainfo AS vesainfoblock, CURBANK
DIM SHARED regs AS RegTypeX
DIM SHARED JPEG AS JpegType
DIM SHARED imgcomment$
DIM SHARED time!, bltime!
REDIM SHARED hufftbl(0, 0, 0) AS HuffmanEntry
REDIM SHARED quant(0, 0, 0)
DIM SHARED display AS vesaModeinfoBlock
'converts unsigned word stored in a long to an integer
DEF fnuns2int% (u&)
  IF u& > 32767 THEN fnuns2int% = u& - 65536 ELSE fnuns2int% = u&
END DEF

'-------------------init tables

'***  2^ table,masks...
DIM SHARED PwrsOf2(-1 TO 23) AS LONG, BIT1(-1 TO 15) AS INTEGER
DIM SHARED bit2(-1 TO 15) AS LONG, bit3(-1 TO 15) AS INTEGER
DIM SHARED bit4(0 TO 15) AS INTEGER
FOR i = -1 TO 23:
temp& = 2 ^ i: PwrsOf2(i) = temp&
IF i < 16 THEN
	bit3(i) = fnuns2int%(temp& - 1)
	bit2(i) = ((NOT temp&) AND &HFFFF&) + 1
	BIT1(i) = fnuns2int%(-(temp& - 1))
	IF i < 15 THEN bit4(i + 1) = fnuns2int%(temp&)
END IF
'PRINT : PRINT HEX$(pwrsof2(i)), HEX$(BIT1(i)), HEX$(bit2(i));
NEXT
DIM SHARED zz(0 TO 63) AS zigzagtype
RESTORE zig2: FOR i = 0 TO 63: READ zz(i).xp, zz(i).yp: NEXT


DetectWinTemp

'--------- Main Loop ----------

SCREEN 0: CLS
SVGAGetData
dummy$ = getcurdir(1)
bs$ = temppath$ + "jpeg.bsav"
DO
  
	SCREEN 7: WIDTH 80, 50: CLS
	f$ = SelectAFile("Select a JPEG file to view", "jpg")
	IF f$ = "" THEN EXIT DO
	jfile = FREEFILE
	OPEN f$ FOR BINARY AS #jfile
	endptr = TOTALBUF: BUF$ = SPACE$(endptr): bufptr = endptr + 1
	JPEGGetParms jfile
	JPEGViewParms
	menu
	CLOSE #jfile
	DO
		r$ = UCASE$(INKEY$)
		SELECT CASE r$
		CASE "V"
			OPEN f$ FOR BINARY AS #jfile
			endptr = TOTALBUF: BUF$ = SPACE$(endptr): bufptr = endptr + 1
			JPEGGetParms jfile
			IF vesainfo.modenum THEN
				'next line is the only way i've found to quit svga back to 50 lines mode
				a = svgasetmode(3): SCREEN 7: WIDTH 80, 50: CLS
			END IF
			fail = SVGASelectMode
			CURBANK = -1
			Y0 = ((vesainfo.yres - JPEG.rows) \ 16) * 8
			x0 = ((vesainfo.xres - JPEG.cols) \ 16) * 8
			IF Y0 < 0 THEN
				yy = vesainfo.chary + 2
				FPrint "The centered jpeg is bigger than the screen", 0, yy, white, vesainfo.charx, vesainfo.chary, 2, 3
				yy = yy + vesainfo.chary + 2
				FPrint "JPEG images must be decoded from the beggining", 0, yy, white, vesainfo.charx, vesainfo.chary, 2, 3
				yy = yy + vesainfo.chary + 2
				FPrint "Be patient until image starts to show.", 0, yy, white, vesainfo.charx, vesainfo.chary, 2, 3
				yy = yy + vesainfo.chary + 2
				FPrint "Please wait....or press H to go to MENU.", 0, yy, white, vesainfo.charx, vesainfo.chary, 2, 3
			END IF
			lasty = 4000
			time! = TIMER
			buf2ptr = 0
			JPEGPut jfile, x0, Y0
			CLOSE #jfile
			time! = TIMER - time!
		CASE "S"
			IF vesainfo.modenum > 255 THEN
				SVGABSave (bs$)
				FPrint "File Bsaved with the name " + bs$, 0, vesainfo.chary * 2, white, vesainfo.charx, vesainfo.chary, 2, 3
			ELSE
				CLS
				PRINT "Must be displaying a JPEG to BSAVE it!"
				PRINT "First press V to wiew a JPEG, and when image is there, press S"
				PRINT
				PRINT "Press H to go to menu..."
			END IF
		CASE "L"
			bltime! = TIMER
			SVGABLoad (bs$)
			bltime! = TIMER - bltime!
		CASE "H"
			a = svgasetmode(3): SCREEN 7: WIDTH 80, 50: CLS
			JPEGViewParms
			menu
		CASE "N", CHR$(27)
			EXIT DO
		CASE ELSE
			r$ = ""
		END SELECT
	
	LOOP UNTIL r$ = CHR$(27)
	CLOSE jfile
	a = svgasetmode(3): SCREEN 7: WIDTH 80, 50: CLS
LOOP UNTIL r$ = CHR$(27)
  dummy$ = getcurdir(0)
  LOCATE 12, 1
  PRINT " Thank you for trying JPEG VIEWER V 2 by Antoni Gual (agual@eic.ictnet.es)"
END
'--------- End of Program----------

'error handlers

diskreadyerror: errata% = ERR: RESUME NEXT


ANYERROR: SCREEN 7: CLS : CLOSE : RESUME

JPEGGetErrors:
  SCREEN 7: CLS
  CLOSE
  SELECT CASE ERR
  CASE 99: PRINT "Not a Valid JPEG/JFIF file"
  CASE 100: PRINT "Only 8x8 samples supported"
  CASE 101: PRINT "Arithmetic coding not supported"
  CASE 102: PRINT "End of jpeg Found"
  CASE 103: PRINT "Error Getting SoS marker"
  CASE 104: PRINT "Unexpected file format"
  CASE 105: PRINT "16 bits Quantization tables not supported"
  CASE 106: PRINT "Not a JFIF format"
  CASE ELSE: PRINT "Error "; ERR; "While getting JPEG parameters"
  END SELECT
END


'------------data------------------


zig2:  'Zigzag patterns for reordering quantization tables and vectors
DATA 0,0
DATA 0,1,1,0
DATA 2,0,1,1,0,2
DATA 0,3,1,2,2,1,3,0
DATA 4,0,3,1,2,2,1,3,0,4
DATA 0,5,1,4,2,3,3,2,4,1,5,0
DATA 6,0,5,1,4,2,3,3,2,4,1,5,0,6
DATA 0,7,1,6,2,5,3,4,4,3,5,2,6,1,7,0
DATA 7,1,6,2,5,3,4,4,3,5,2,6,1,7
DATA 2,7,3,6,4,5,5,4,6,3,7,2
DATA 7,3,6,4,5,5,4,6,3,7
DATA 4,7,5,6,6,5,7,4
DATA 7,5,6,6,5,7
DATA 6,7,7,6
DATA 7,7

SUB DetectWinTemp
'detect Windows 95 and the temp path, setting global variables
'used by  all routines based in SHELL commands, i.e. the file selector
'or the current dir saver

'find temp path
temppath$ = ENVIRON$("TEMP") + "\"
IF temppath$ = "\" THEN
		PRINT "Sorry. Some routines in this program require the TEMP variable to be set"
		END
END IF

'detect windows 95 & 98
tempfile$ = temppath$ + "detwin.txt"
doscmd$ = "ver >" + tempfile$
SHELL doscmd$
f1 = FREEFILE: OPEN tempfile$ FOR INPUT AS #f1:
win = 0
WHILE NOT EOF(f1) AND win = 0
	LINE INPUT #f1, a$
	IF INSTR(a$, "Windows") THEN win = -1
WEND
CLOSE f1
KILL tempfile$
END SUB

FUNCTION DiskReady% (d$)
'
'Nearly Self-contained drive check routine
' Use as you want, only give me credit
'
'returns:   0 if drive exists and it's ready
'           1 if drive is not ready
'           2 if drive does not exist
'           3 if drive exists and disk is an audio CD
'
'supposed to run in any dos from MSDOS 3.1. Tested in Win 95 and DOS 6.2
'detects RAM disks and it's supposed to detect network units
'Does not use interrupt calls!
'---------------------------------------------------------------------------
'To use it into your programs simply copy it and add the line
'   diskreadyerror: errata% = ERR: RESUME NEXT
'(without the leading ') after the END of the main program
'---------------------------------------------------------------------------
SHARED errata%
errata% = 0
drive$ = LEFT$(UCASE$(d$), 1) + ":"
IF drive$ = "B:" THEN
	OUT &H70, &H10
	IF (INP(&H71) AND 7) = 0 THEN DiskReady% = 2: EXIT FUNCTION
END IF
ON ERROR GOTO diskreadyerror
num% = FREEFILE
OPEN drive$ + "\track01.cda" FOR INPUT AS #num%
SELECT CASE errata%
	CASE 53: DiskReady% = 0
	CASE 71: DiskReady% = 1
	CASE 76: DiskReady% = 2
	CASE 0: DiskReady% = 3: CLOSE num%
	CASE ELSE
		PRINT "Unexpected error value "; errata%; "in Diskready function": END
END SELECT
ON ERROR GOTO 0

END FUNCTION

SUB FPrint (t$, XStartPos, YStartPos, Colour&, XSize, YSize, ChSpacing, font)
'Print scalable size characters.
'positions and sizes in pixels!
'font numbers valid:
' font 2: 14 pixel high
' font 3: 8  pixel high
' font 4: 16 pixel high
' All integer routines, as fast  as possible
STATIC FontSeg, FontOffset
	IF LEN(t$) = 0 THEN EXIT SUB
	IF FontSeg = 0 THEN
		regs.ax = &H1130
		regs.bx = font * 256
		CALL interrupty(&H10, regs, regs)
		FontSeg = regs.es
		FontOffset = regs.bp
	END IF
  
	SELECT CASE font
	CASE 2: ChYSize = 14
	CASE 3: ChYSize = 8
	CASE 6: ChYSize = 16  'LO+HI
	CASE ELSE
		EXIT SUB
	END SELECT
  
	ChXSize = 8
	YStep = ChYSize - 1: Xstep = ChXSize - 1
	YEndPos = YStartPos + YSize: XChStart = XStartPos - XSize
	FOR StrPtr = 1 TO LEN(t$)
		Char = ASC(MID$(t$, StrPtr))
		ChOffset& = 0& + FontOffset + Char * ChYSize - 1
		XChStart = XChStart + XSize + ChSpacing: XChEnd = XChStart + XSize
		ChBytePtr = 1: YCounter = 2  '<<<<
		DEF SEG = FontSeg: fontBits = PEEK(ChOffset& + ChBytePtr): DEF SEG
		FOR PixelY = YStartPos TO YEndPos
			WHILE YCounter > YStep
			   YCounter = YCounter - YSize: ChBytePtr = ChBytePtr + 1
			   DEF SEG = FontSeg: fontBits = PEEK(ChOffset& + ChBytePtr): DEF SEG
			WEND
			ChBitPtr = 7: XCounter = 2  '<<<<<
			Pixel = fontBits AND PwrsOf2(ChBitPtr)
			FOR PixelX = XChStart TO XChEnd
				WHILE XCounter > Xstep
					XCounter = XCounter - XSize
					ChBitPtr = ChBitPtr - 1
					Pixel = fontBits AND PwrsOf2(ChBitPtr)
				WEND
				IF Pixel THEN svgappixel PixelX, PixelY, Colour&
				XCounter = XCounter + Xstep
			NEXT
			YCounter = YCounter + YStep
		NEXT
	NEXT
END SUB

FUNCTION getcurdir$ (save%)
'if save <>0 then save cur dir
'if save =0 then chdir to curdir
'needs the shared variables temppath$ and win to be set

STATIC a$
IF save% THEN
	tempfile$ = temppath$ + "curdir.txt"
	SHELL "cd >" + tempfile$
	f% = FREEFILE: OPEN tempfile$ FOR INPUT AS f%
	LINE INPUT #f%, a$
	CLOSE f%: KILL tempfile$
	IF LEFT$(a$, 1) <> "\" THEN a$ = a$ + "\"
ELSE
	IF win THEN a1$ = CHR$(34) + a$ ELSE a1$ = a$
	SHELL a1$
	SHELL "cd " + a1$
END IF
getcurdir$ = a$
END FUNCTION

SUB ideas
' OK que funcione en qbasic
' ok enviar a fichero temp
'    selector de modos SVGA con menu
' OK no guardar todos los datos de todos los modos!
'    visor : encuadre, zoom
'    otros formatos GIF, bmp,pcx
' OK quitar el salvar directorio de selectAfile (rutina aparte)
'    permitir escribir en el selector de ficheros!
END SUB

SUB interrupty (intnum AS INTEGER, inreg AS RegTypeX, outreg AS RegTypeX)

STATIC a() AS LONG, bReady AS INTEGER, i AS INTEGER, p AS INTEGER, S1 AS INTEGER, S2 AS INTEGER
IF bReady = 0 THEN
	i = 50: DIM a(1 TO i) AS LONG
	a(1) = &H53EC8B55: a(2) = &H1E575651: a(3) = &H5E8B9C06: a(4) = &HA078B0E
	a(5) = &HC70774E4: a(6) = &HE9FFFF07: a(7) = &HEC8300A1: a(8) = &HB3F88A0A
	a(9) = &HE85E89CD: a(10) = &HCBEA46C7: a(11) = &H74253C90: a(12) = &H75263C04
	a(13) = &HEA46C714: a(14) = &H46C701E8: a(15) = &HC7CB00EC: a(16) = &H2C2EE46
	a(17) = &HF046C7: a(18) = &H85E8B90: a(19) = &H5E8B37FF: a(20) = &HE37FF06
	a(21) = &H50008FB8: a(22) = &HE85E8D16: a(23) = &H8BDA8C53: a(24) = &H378B0A5E
	a(25) = &H8E0C5E8B: a(26) = &H10448B1F: a(27) = &H75FFFF3D: a(28) = &H50C28B02
	a(29) = &H3D12448B: a(30) = &H275FFFF: a(31) = &HC08EC28B: a(32) = &H5C8B048B
	a(33) = &H44C8B02: a(34) = &H8B06548B: a(35) = &H748B0C7C: a(36) = &H9CCB1F0A
	a(37) = &H83EC8B55: a(38) = &H1E5620C5: a(39) = &H89E476C5: a(40) = &H25C8904
	a(41) = &H89044C89: a(42) = &H7C890654: a(43) = &H12448C0C: a(44) = &H8F10448F
	a(45) = &H448F0A44: a(46) = &HE448F08: a(47) = &H9D0EC483: a(48) = &H5E5F1F07
	a(49) = &HCA5D5B59: a(50) = &H9165000A
	S1 = 0: S2 = 0: p = VARPTR(a(1)): DEF SEG = VARSEG(a(1))
	FOR i = 0 TO 199
		S1 = (S1 + PEEK(p + i)) MOD 255: S2 = (S2 + S1) MOD 255
	NEXT i
	IF S1 OR S2 THEN ERROR 2: intnum = -1: EXIT SUB ' Checksum Error
	bReady = -1
END IF
DEF SEG = VARSEG(a(1))
CALL ABSOLUTE(intnum, VARSEG(inreg), VARPTR(inreg), VARSEG(outreg), VARPTR(outreg), VARPTR(a(1)))
DEF SEG
END SUB

SUB JPEGGet8x8 (vector(), comp, dcCoef) STATIC
'reads file ,decodes, and returns a 8x8 block of a component (Y, Cb or cr)

	CONST fix029 = 2446&
	CONST FIX039 = -3196&
	CONST FIX054 = 4433&
	CONST FIX076 = 6270&
	CONST FIX089 = -7373&
	CONST FIX117 = 9633&
	CONST fix150 = 12299&
	CONST FIX184 = -15137&
	CONST FIX196 = -16069&
	CONST fix205 = 16819&
	CONST FIX256 = -20995&
	CONST fix307 = 25172&
	CONST x1& = &H20000
	CONST x2& = &H2000
	CONST x = &H1000
	DIM z1 AS LONG, z2 AS LONG, z3 AS LONG, z4 AS LONG, z5 AS LONG
	DIM tmp0 AS LONG, tmp1 AS LONG, tmp2 AS LONG, tmp3 AS LONG
	DIM tmp10 AS LONG, tmp11 AS LONG, tmp12 AS LONG, tmp13 AS LONG
   SELECT CASE comp
   CASE 1
	 huffdcnum = JPEG.HDCTY
	 huffacnum = JPEG.HaCTY
	 quantnum = JPEG.qty
   CASE 2
	 huffdcnum = JPEG.HDCTCBR
	 huffacnum = JPEG.HaCTcbr
	 quantnum = JPEG.qtcbr
   END SELECT

'clear vector
REDIM vector(0 TO 7, o TO 7)

'Get the DC coefficient
hnum = huffdcnum: tk = 0: GOSUB dekode1
cat = dekode: GOSUB getnbits1: dcCoef = dcCoef + getnbits
vector(0, 0) = dcCoef
'Get AC Coefficients
K = 1: hnum = huffacnum: tk = 1
DO
  GOSUB dekode1
  SELECT CASE dekode
  CASE 0 'EOB Encountered
	EXIT DO
  CASE 3270 'ZRL encountered  15*256+0
	K = K + 16
  CASE ELSE
	K = K + dekode \ 16
	cat = dekode AND 15: GOSUB getnbits1
	'zigzag!
	vector(zz(K).xp, zz(K).yp) = getnbits
	K = K + 1
  END SELECT
LOOP UNTIL K > 63


IF NOT viw THEN EXIT SUB

'dct& quantization
FOR u = 7 TO 0 STEP -1
	'if all row zeros, copy first value
	IF (vector(1, u) OR vector(2, u) OR vector(3, u) OR vector(4, u) OR vector(5, u) OR vector(6, u) OR vector(7, u)) = 0 THEN
	  tmp0 = vector(0, u) * quant(quantnum, 0, u) * 2
	  vector(0, u) = tmp0
	  vector(1, u) = tmp0
	  vector(2, u) = tmp0
	  vector(3, u) = tmp0
	  vector(4, u) = tmp0
	  vector(5, u) = tmp0
	  vector(6, u) = tmp0
	  vector(7, u) = tmp0
	ELSE
	z2 = vector(2, u) * quant(quantnum, 2, u)
	z3 = vector(6, u) * quant(quantnum, 6, u)
	z1 = (z2 + z3) * FIX054
	tmp2 = z1 + (z3 * FIX184)
	tmp3 = z1 + (z2 * FIX076)
	z2 = vector(0, u) * quant(quantnum, 0, u)
	z3 = vector(4, u) * quant(quantnum, 4, u)
	tmp0 = x2& * (z2 + z3)
	tmp1 = x2& * (z2 - z3)
	tmp10 = tmp0 + tmp3
	tmp13 = tmp0 - tmp3
	tmp11 = tmp1 + tmp2
	tmp12 = tmp1 - tmp2
	tmp0 = vector(7, u) * quant(quantnum, 7, u)
	tmp1 = vector(5, u) * quant(quantnum, 5, u)
	tmp2 = vector(3, u) * quant(quantnum, 3, u)
	tmp3 = vector(1, u) * quant(quantnum, 1, u)
	z1 = tmp0 + tmp3
	z2 = tmp1 + tmp2
	z3 = tmp0 + tmp2
	z4 = tmp1 + tmp3
	z5 = (z3 + z4) * FIX117
	tmp0 = tmp0 * fix029
	tmp1 = tmp1 * fix205
	tmp2 = tmp2 * fix307
	tmp3 = tmp3 * fix150
	z1 = z1 * FIX089
	z2 = z2 * FIX256
	z3 = z3 * FIX196
	z4 = z4 * FIX039
	z3 = z3 + z5
	z4 = z4 + z5
	tmp0 = tmp0 + z1 + z3
	tmp1 = tmp1 + z2 + z4
	tmp2 = tmp2 + z2 + z3
	tmp3 = tmp3 + z1 + z4
	vector(0, u) = (tmp10 + tmp3) \ x
	vector(7, u) = (tmp10 - tmp3) \ x
	vector(1, u) = (tmp11 + tmp2) \ x
	vector(6, u) = (tmp11 - tmp2) \ x
	vector(2, u) = (tmp12 + tmp1) \ x
	vector(5, u) = (tmp12 - tmp1) \ x
	vector(3, u) = (tmp13 + tmp0) \ x
	vector(4, u) = (tmp13 - tmp0) \ x
  END IF
NEXT
FOR V = 0 TO 7
	z2 = vector(V, 2)
	z3 = vector(V, 6)
	z1 = (z2 + z3) * FIX054
	tmp2 = z1 + (z3 * FIX184)
	tmp3 = z1 + (z2 * FIX076)
	tmp0 = x2& * (vector(V, 0) + vector(V, 4))
	tmp1 = x2& * (vector(V, 0) - vector(V, 4))
	tmp10 = tmp0 + tmp3
	tmp13 = tmp0 - tmp3
	tmp11 = tmp1 + tmp2
	tmp12 = tmp1 - tmp2
	tmp0 = vector(V, 7)
	tmp1 = vector(V, 5)
	tmp2 = vector(V, 3)
	tmp3 = vector(V, 1)
	z1 = tmp0 + tmp3
	z2 = tmp1 + tmp2
	z3 = tmp0 + tmp2
	z4 = tmp1 + tmp3
	z5 = (z3 + z4) * FIX117
	tmp0 = tmp0 * fix029
	tmp1 = tmp1 * fix205
	tmp2 = tmp2 * fix307
	tmp3 = tmp3 * fix150
	z1 = z1 * FIX089
	z2 = z2 * FIX256
	z3 = z3 * FIX196
	z4 = z4 * FIX039
	z3 = z3 + z5
	z4 = z4 + z5
	tmp0 = tmp0 + z1 + z3
	tmp1 = tmp1 + z2 + z4
	tmp2 = tmp2 + z2 + z3
	tmp3 = tmp3 + z1 + z4
	vector(V, 0) = (tmp10 + tmp3) \ x1&
	vector(V, 7) = (tmp10 - tmp3) \ x1&
	vector(V, 1) = (tmp11 + tmp2) \ x1&
	vector(V, 6) = (tmp11 - tmp2) \ x1&
	vector(V, 2) = (tmp12 + tmp1) \ x1&
	vector(V, 5) = (tmp12 - tmp1) \ x1&
	vector(V, 3) = (tmp13 + tmp0) \ x1&
	vector(V, 4) = (tmp13 - tmp0) \ x1&
NEXT
EXIT SUB


'--------------subroutines-------------------------


dekode1:
WHILE buf2ptr < 16: buf2 = (buf2 AND &HFFFF&) * 256 OR JPEGGetByte: buf2ptr = buf2ptr + 8: WEND
tmp0 = buf2 \ PwrsOf2(buf2ptr - 16)
i = 0
DO UNTIL hufftbl(tk, hnum, i).Index = (tmp0 AND hufftbl(tk, hnum, i).mask): i = i + 1: LOOP
dekode = hufftbl(tk, hnum, i).code 'return the appropriate code
buf2ptr = buf2ptr - hufftbl(tk, hnum, i).Length
RETURN

getnbits1:
WHILE buf2ptr < 16: buf2 = (buf2 AND &HFFFF&) * 256 OR JPEGGetByte: buf2ptr = buf2ptr + 8: WEND
c1 = buf2ptr - cat
getnbits = buf2 \ PwrsOf2(c1) AND bit3(cat)
IF getnbits AND bit4(cat) THEN  ELSE getnbits = getnbits + BIT1(cat)
buf2ptr = c1
RETURN

END SUB

FUNCTION JPEGGetByte STATIC
'***buffered, all file access goes thru it
'gets a single byte from file. At reading, it converts the pairs FF 00 to 00's

SHARED BUF$, bufptr AS LONG, endptr  AS LONG, find$
IF bufptr > endptr THEN
	GET #jfile, , BUF$: bufptr = SADD(BUF$): endptr = TOTALBUF + bufptr - 1
	i0 = INSTR(BUF$, find$)
	IF PEEK(endptr) = 255 THEN endptr = endptr - 1: SEEK #jfile, SEEK(jfile) - 1
	DO WHILE i0 > 0
		MID$(BUF$, i0 + 1) = MID$(BUF$, i0 + 2): endptr = endptr - 1
		i0 = INSTR(i0 + 1, BUF$, find$)
	LOOP
  
END IF
JPEGGetByte = PEEK(bufptr): bufptr = bufptr + 1
END FUNCTION

SUB JPEGGetParms (jfile)
REDIM huffamount(1 TO 16)
REDIM hufftbl(0 TO 1, 0 TO 1, 0 TO 255) AS HuffmanEntry
REDIM quant(0 TO 1, 0 TO 7, 0 TO 7) '2 quantization tables (Y, CbCr)
DIM GETword AS LONG
'ON ERROR GOTO JPEGGetErrors
JPEG.SIZE = LOF(jfile)
QTables = 0 'Initialize some checkpoint variables
ACTables = 0
dctables = 0
JPEG.restart = GETword
SEEK jfile, 1
GOSUB getword1
IF GETword <> 65496 THEN ERROR 99
DO  'Primary control loop for markers
  IF JPEGGetByte = 255 THEN 'Marker Found
	  d = JPEGGetByte
	  SELECT CASE d 'which one is it?
	  CASE &HC0, &HC1  'SOF0
		  'get jpeg attributes
		  GOSUB getword1: temp4& = GETword   'Length of segment
		  temp0 = JPEGGetByte                'Data precision
		  IF temp0 <> 8 THEN ERROR 100       'we do not support 12 or 16-bit samples
		  GOSUB getword1: JPEG.rows = GETword
		  GOSUB getword1: JPEG.cols = GETword
		  temp0 = JPEGGetByte                'Number of components
		  FOR i = 1 TO temp0
			  id = JPEGGetByte
			  SELECT CASE id
			  CASE 1
				  temp1 = JPEGGetByte
				  JPEG.samplesy = (temp1 AND 15) * (temp1 \ 16)
				  JPEG.qty = JPEGGetByte
			  CASE 2, 3
				  temp1 = JPEGGetByte
				  JPEG.samplescbcr = (temp1 AND 15) * (temp1 \ 16)
				  JPEG.qtcbr = JPEGGetByte
			  END SELECT
		  NEXT i
	  CASE &HC9 'SOF9
		ERROR 101
	  CASE &HC4 'DHT
		  IF ACTables < 2 OR dctables < 2 THEN
			  'get huffman tables
			  GOSUB getword1
			  l0 = GETword: c0 = 2
			  DO
				  temp0 = JPEGGetByte: c0 = c0 + 1
				  t0 = (temp0 AND 16) \ 16
				  temp0 = temp0 AND 15
				  total = 0
				  FOR i = 1 TO 16
					  temp1 = JPEGGetByte: c0 = c0 + 1
					  total = total + temp1
					  huffamount(i) = temp1
				  NEXT i
				  curnum& = 0
				  curindex = -1
				  FOR i = 1 TO 16
					  FOR j = 1 TO huffamount(i)
						  i1 = 16 - i: curindex = curindex + 1
						  hufftbl(t0, temp0, curindex).code = JPEGGetByte: c0 = c0 + 1
						  TMP& = curnum& * PwrsOf2(i1)
						  hufftbl(t0, temp0, curindex).Index = TMP&
						  'this mask is to use a faster method of huffman decoding
						  hufftbl(t0, temp0, curindex).mask = bit2(i1)
						  hufftbl(t0, temp0, curindex).Length = i
						  curnum& = curnum& + 1
					  NEXT j
					  curnum& = curnum& * 2
				  NEXT i
				  
				  IF t0 THEN ACTables = ACTables + 1 ELSE dctables = dctables + 1
			  LOOP UNTIL c0 >= l0
		  END IF
	  CASE &HCC 'DAC
		ERROR 101
	  CASE &HD8 'SOI
	  CASE &HD9 'EOI
		ERROR 102
	  CASE &HDA 'SOS
		  'get SOS
		  GOSUB getword1: temp4& = GETword
		  temp0 = JPEGGetByte
		  IF temp0 <> 1 AND temp0 <> 3 THEN GetSOS = 0: EXIT SUB
		  JPEG.numcomp = temp0
		  FOR i = 1 TO temp0
			  temp1 = JPEGGetByte
			  SELECT CASE temp1
			  CASE 1
				  temp2 = JPEGGetByte
				  JPEG.HaCTY = temp2 AND 15
				  JPEG.HDCTY = temp2 \ 16
			  CASE 2, 3
				  temp2 = JPEGGetByte
				  JPEG.HaCTcbr = temp2 AND 15
				  JPEG.HDCTCBR = temp2 \ 16
			  CASE ELSE
				ERROR 103
			  END SELECT
		  NEXT i
		  num = 3: GOSUB getstring
		  IF (dctables = 2 AND ACTables = 2 AND QTables = 2) OR JPEG.numcomp = 1 THEN
			'TABLE ENDED, IMAGE START
			ON ERROR GOTO 0
			EXIT SUB
		  ELSE
			ERROR 104
		  END IF
	  CASE &HDD 'DRI
		  GOSUB getword1: temp0 = GETword
		  GOSUB getword1: JPEG.restart = GETword
	  CASE &HDB 'DQT
		  IF QTables < 2 THEN
			  GOSUB getword1: l0 = GETword
			  c0 = 2
			  DO
				  temp0 = JPEGGetByte: c0 = c0 + 1
				  IF temp0 AND &HF0 THEN ERROR 105
				  temp0 = temp0 AND 15
				  xp = 0: yp = 0
				  FOR i = 0 TO 63
					  quant(temp0, zz(i).xp, zz(i).yp) = JPEGGetByte: c0 = c0 + 1
				  NEXT i
				  QTables = QTables + 1
			  LOOP UNTIL c0 >= l0
		  END IF
	  CASE &HE0 'APP0
		  GOSUB getword1
		  l& = GETword
		  num = 5: GOSUB getstring
		  IF getstr$ <> ("JFIF" + CHR$(0)) THEN ERROR 106
		  JPEG.jfifmajor = CHR$(JPEGGetByte)
		  JPEG.jfifMinor = CHR$(JPEGGetByte)
		  JPEG.densunits = CHR$(JPEGGetByte)
		  GOSUB getword1: JPEG.Xdens = GETword
		  GOSUB getword1: JPEG.ydens = GETword
		  JPEG.ThWidth = CHR$(JPEGGetByte)
		  JPEG.Theigth = CHR$(JPEGGetByte)
	  CASE &HFE 'COM
		  GOSUB getword1: num = GETword - 2:
		  GOSUB getstring: imgcomment$ = getstr$
	  END SELECT
  END IF
  IF LEN(INKEY$) THEN EXIT SUB
LOOP
ERASE huffamount
ON ERROR GOTO 0
EXIT SUB


'------subroutines-------------------

'not intel byte order!!
getword1:
temp9 = JPEGGetByte
GETword = 256& * temp9 OR JPEGGetByte
RETURN

getstring:
getstr$ = SPACE$(num)
FOR i = 1 TO num
 MID$(getstr$, i, 1) = CHR$(JPEGGetByte)
NEXT
RETURN


END SUB

SUB JPEGPut (jfile, x0, Y0)
'Routine that decodes the file and puts it into the screen

REDIM YVector1(0 TO 7, 0 TO 7)              '4 vectors for Y attribute
REDIM YVector2(0 TO 7, 0 TO 7)
REDIM YVector3(0 TO 7, 0 TO 7)
REDIM YVector4(0 TO 7, 0 TO 7)
REDIM CbVector(0 TO 7, 0 TO 7)              '1 vector for Cb attribute
REDIM CrVector(0 TO 7, 0 TO 7)              '1 vector for Cr attribute
DIM mcu AS LONG

'We initialize the dc coefficients as they are accumulative
dcY = 0: dcCb = 0: dcCr = 0

xindex = 0: yindex = 0
buf2ptr = 0
mcu = 0: lastj = -1

SELECT CASE JPEG.numcomp
'Y-Cb-Cr color jpeg
CASE 3
	SELECT CASE JPEG.samplesy
	CASE 4
		DO
			GOSUB skip
			JPEGGet8x8 YVector1(), 1, dcY
			JPEGGet8x8 YVector2(), 1, dcY
			JPEGGet8x8 YVector3(), 1, dcY
			JPEGGet8x8 YVector4(), 1, dcY
			JPEGGet8x8 CbVector(), 2, dcCb
			JPEGGet8x8 CrVector(), 2, dcCr
			IF viw THEN
				FOR i = 0 TO 7
					YI = yindex + i: IF YI >= JPEG.rows THEN EXIT FOR
					i2 = i \ 2: y1 = YI + Y0
					FOR j = 0 TO 7
						xj = xindex + j: IF xj >= JPEG.cols THEN EXIT FOR
						j2 = j \ 2
						y = YVector1(i, j) + 128
						GOSUB ToRGB
					NEXT j
				NEXT
				FOR i = 8 TO 15
					YI = yindex + i: IF YI >= JPEG.rows THEN EXIT FOR
					i2 = i \ 2: y1 = YI + Y0: I8 = i - 8
					FOR j = 0 TO 7
						xj = xindex + j: IF xj >= JPEG.cols THEN EXIT FOR
						j2 = j \ 2
						y = YVector3(I8, j) + 128
						GOSUB ToRGB
					NEXT j
				NEXT
				FOR i = 0 TO 7
					YI = yindex + i: IF YI >= JPEG.rows THEN EXIT FOR
					i2 = i \ 2: y1 = YI + Y0
					FOR j = 8 TO 15
						xj = xindex + j: IF xj >= JPEG.cols THEN EXIT FOR
						j2 = j \ 2
						y = YVector2(i, j - 8) + 128
						GOSUB ToRGB
					NEXT j
				NEXT i
				FOR i = 8 TO 15
					YI = yindex + i: IF YI >= JPEG.rows THEN EXIT FOR
					i2 = i \ 2: y1 = YI + Y0: I8 = i - 8
					FOR j = 8 TO 15
						xj = xindex + j: IF xj >= JPEG.cols THEN EXIT FOR
						j2 = j \ 2
						y = YVector4(I8, j - 8) + 128
						GOSUB ToRGB
					NEXT j
				NEXT i
			END IF
			IF JPEG.restart THEN mcu = mcu + 1: IF JPEG.restart = mcu THEN GOSUB rstrt
			xindex = xindex + 16
			IF xindex >= JPEG.cols THEN
				xindex = 0: yindex = yindex + 16
				IF LEN(INKEY$) THEN EXIT DO
			END IF
		LOOP UNTIL yindex >= JPEG.rows OR yindex + Y0 >= vesainfo.yres

	'case 2 not tested (never found a jpeg with this structure)
	CASE 2
		DO
			GOSUB skip
			JPEGGet8x8 YVector1(), 1, dcY
			JPEGGet8x8 YVector2(), 1, dcY
			JPEGGet8x8 CbVector(), 2, dcCb
			JPEGGet8x8 CrVector(), 2, dcCr
			IF viw THEN
				FOR i = 0 TO 7
				YI = yindex + i: IF YI >= JPEG.rows THEN EXIT FOR
				i2 = i \ 2: y1 = YI + Y0
					FOR j = 0 TO 7
						xj = xindex + j: IF xj >= JPEG.cols THEN EXIT FOR
						y = YVector1(i, j) + 128
						j2 = j \ 2
						GOSUB ToRGB
					 
					NEXT j
					FOR j = 8 TO 15
						xj = xindex + j: IF xj >= JPEG.cols THEN EXIT FOR
						y = YVector2(i, j - 8) + 128
						j2 = j \ 2
						GOSUB ToRGB
							
					NEXT j
				NEXT i
			END IF
			IF JPEG.restart THEN mcu = mcu + 1: IF JPEG.restart = mcu THEN GOSUB rstrt
			xindex = xindex + 16
			IF xindex >= JPEG.cols THEN
				xindex = 0: yindex = yindex + 8
				IF LEN(INKEY$) THEN EXIT DO
			END IF
		LOOP UNTIL yindex >= JPEG.rows OR yindex + Y0 >= vesainfo.yres
 
	CASE 1
		DO
			GOSUB skip
			JPEGGet8x8 YVector1(), 1, dcY
			JPEGGet8x8 CbVector(), 2, dcCb
			JPEGGet8x8 CrVector(), 2, dcCr
			IF viw THEN
				FOR i = 0 TO 7
					YI = yindex + i: IF YI >= JPEG.rows THEN EXIT FOR
					i2 = i \ 2: y1 = YI + Y0
					FOR j = 0 TO 7
						xj = xindex + j: IF xj >= JPEG.cols THEN EXIT FOR
						y = YVector1(i, j) + 128
						j2 = j \ 2
						GOSUB ToRGB
					NEXT j
				NEXT i
			END IF
			IF JPEG.restart THEN mcu = mcu + 1: IF JPEG.restart = mcu THEN GOSUB rstrt
			xindex = xindex + 8
			IF xindex >= JPEG.cols THEN
				xindex = 0: yindex = yindex + 8
				IF LEN(INKEY$) THEN EXIT DO
			END IF
		LOOP UNTIL yindex >= JPEG.rows OR (yindex + Y0) >= vesainfo.yres
	END SELECT

'monochrome jpeg
CASE 1
	DO
		GOSUB skip
		JPEGGet8x8 YVector1(), 1, dcY
		IF viw THEN
		FOR i = 0 TO 7
			YI = yindex + i: IF YI >= JPEG.rows THEN EXIT FOR
			y1 = YI + Y0
			FOR j = 0 TO 7
				xj = xindex + j: IF xj >= JPEG.cols THEN EXIT FOR
				y = YVector1(i, j) + 128
				IF y < 0 THEN
					y = 0
				ELSEIF y > 255 THEN
					y = 255
				END IF
				svgappgrey xj + x0, y1, y
			NEXT j
		NEXT i
		END IF
		IF JPEG.restart THEN mcu = mcu + 1: IF JPEG.restart = mcu THEN GOSUB rstrt
		xindex = xindex + 8
		IF xindex >= JPEG.cols THEN
			xindex = 0: yindex = yindex + 8
			IF LEN(INKEY$) THEN EXIT DO
		END IF
	LOOP UNTIL yindex >= JPEG.rows OR yindex + Y0 >= vesainfo.yres
END SELECT
ERASE hufftbl, quant
ERASE YVector1, YVector2, YVector3, YVector4, CbVector, CrVector
BUF$ = ""
ON ERROR GOTO 0
EXIT SUB

rstrt:
	buf2ptr = (((buf2ptr) \ 8) * 8) - 16
	dcY = 0: dcCb = 0: dcCr = 0: mcu = 0
RETURN


ToRGB:
	CrV = CrVector(i2, j2)
	CbV = CbVector(i2, j2)
	r = y + 70 * CrV \ 50
	g = y - (34 * CbV + 71 * CrV) \ 100
	b = y + 126 * CbV \ 71

	IF r > 255 THEN
		r = 255
	ELSEIF r < 0 THEN
		r = 0
	END IF
	clr& = 65536 * r
	IF g > 255 THEN
		g = 255
	ELSEIF g < 0 THEN
		g = 0
	END IF
	clr& = clr& OR (256& * g)
	IF b > 255 THEN
		b = 255
	ELSEIF b < 0 THEN
		b = 0
	END IF
	clr& = clr& OR b
	svgappixel xj + x0, y1, clr&
RETURN



skip:
	viw = -1
	xi0 = xindex + x0
	IF xi0 >= vesainfo.xres THEN
		viw = 0
	ELSEIF xi0 < 0 THEN
		viw = 0
	ELSEIF (yindex + Y0) < 0 THEN
		viw = 0
	END IF
RETURN
END SUB

SUB JPEGViewParms
 SHARED f$
 PRINT "Parameters of this JPEG File"
 PRINT
 PRINT "File Name            :  "; f$
 PRINT USING "File  size           : ######,###               bytes"; JPEG.SIZE
 PRINT "Comment              : "; imgcomment$
 PRINT USING "JFIF Format Version  :         ##     .    ##"; ASC(JPEG.jfifmajor); ASC(JPEG.jfifMinor)
 PRINT USING "Rows X Cols          :       ####     x  ####   pixel"; JPEG.rows; JPEG.cols
 temp& = 3& * JPEG.rows * JPEG.cols
 PRINT USING "Uncompressed size    : ######,###               bytes"; temp&
 PRINT USING "Compression ratio    :       ####.#   to    1"; temp& / JPEG.SIZE
 SELECT CASE ASC(JPEG.densunits)
 CASE 0: unit$ = " ratio"
 CASE 1: unit$ = " dots/inch"
 CASE 2:  unit$ = " dots/cm"
 END SELECT
 PRINT USING "Density           X/Y:       ####     /  ####   \        \"; JPEG.Xdens; JPEG.ydens; unit$
 IF JPEG.restart THEN
	PRINT USING "Restart each         :      #####               blocks"; JPEG.restart
 ELSE
	PRINT "No Restart marks in this file"
 END IF
 PRINT USING "Thumbnail w x h      :        ###     x   ###   pixel "; ASC(JPEG.ThWidth); ASC(JPEG.Theigth)
 IF JPEG.numcomp = 3 THEN a$ = " Color Y + Cb + Cr" ELSE a$ = " Black & White"
 PRINT "Color components     : "; a$
 PRINT "Num of samples      Y: "; JPEG.samplesy; : LOCATE , 49: PRINT "CbCr: "; JPEG.samplescbcr
 PRINT
 PRINT "Quantization tables Y: "; JPEG.qty + 1; : LOCATE , 49: PRINT "Cbcr: "; JPEG.qtcbr - JPEG.qty
 PRINT "Huffman tables DC   Y: "; JPEG.HDCTY + 1; : LOCATE , 49: PRINT "CbCr: "; JPEG.HDCTCBR - JPEG.HDCTY
 PRINT "Huffman tables aC   Y: "; JPEG.HaCTY + 1; : LOCATE , 49: PRINT "CbCr: "; JPEG.HaCTcbr - JPEG.HaCTY
 PRINT

END SUB

SUB menu
	PRINT "      MENU"
	PRINT "      ===="
	PRINT
	PRINT "     V:  View JPEG"
	PRINT "     S:  Bsave (After View)"
	PRINT "     L:  Bload"
	PRINT "     N:  Next file"
	PRINT "   Esc:  Exit program"
	PRINT "     H:  Display file params and this MENU"
	PRINT
	PRINT USING "Last Decoding time: ####.## sec "; time!
	PRINT USING "Last Bload time   :  ###.## sec "; bltime!
END SUB

FUNCTION SelectAFile$ (header$, ext$)
'
' User friendly file selector routine.Returns full path of a file
' by Antoni Gual agual@eic.ictnet.es
' Fully reprogrammed from a layout by William Yu
'
' Use as you want, only give me credit
'
' FEATURES:
' Should not display DOS error messages when disk not ready
' Tested in Win 95/98 and  DOS 6.1-Win 3.11.
' Auto detects num of screen lines set by main program.
' Tested with QBasic
' Not self contained, needs DiskReady and DetectWinTemp routines!
' No mouse!
' In W95/98 displays LFN, but returns 8.3 format
'   (QB file functions can't handle LFN)
'
' REMARKS:
' For non Win 9x users: It Runs faster if temp path is set to a RAM drive!
' Creates some auxiliar files: To do it, uses path in temp system variable
' To use it with DOS < 5.0 try changing constant DOS to 3 (Sorry..I.Can't test it)
' To avoid drive checking each time the routine is run, read comment
'    6 lines above EXIT FUNCTION
'
'-------------------------------------------------------------------------
	'PUT DOS TO 3 IF THE PROGRAM HAS TO RUN UNDER DOS BELOW 5.0
	DIM z$
	CONST dos = 5

	'keys
	CONST kpgup = -&H49, kpgdn = -&H51
	CONST kleft = -&H4B, kright = -&H4D, kup = -&H48, kdown = -&H50
	CONST kenter = &HD, kesc = &H1B

	q$ = CHR$(34)

	'colors
	CONST fgiuns = 7, bgiuns = 0, fgauns = 15, bgauns = 0
	CONST fgisel = 12, bgisel = 0, fgasel = 15, bgasel = 4

	'auxiliar files
	DIM auxf$(1 TO 3):
	auxf$(3) = temppath$ + "}{drive.lst": auxf$(2) = temppath$ + "}{dir.lst"
	auxf$(1) = temppath$ + "}{file.lst"
	tempfile$ = temppath$ + "temp.txt"
   
	'Detect Nr of lines of screen
	DEF SEG = &H40
	scrl = PEEK(&H84) + 1
	DEF SEG

	DIM wd(0 TO 4, 1 TO 3)
	'vertical window sizes
	CONST wintop = 8
	winbot = scrl - 7: winheight = winbot - wintop + 1

	'indexs ( they avoid for a type definition in the main module)
	CONST wdxpos = 0, wdwdth = 1, wdtop = 2, wdcur = 3, wdcount = 4


	'init
	wd(wdxpos, 1) = 8: wd(wdxpos, 2) = 38: wd(wdxpos, 3) = 68
	IF win THEN
		wd(wdwdth, 1) = 24: wd(wdwdth, 2) = 24: wd(wdwdth, 3) = 6
	ELSE
		wd(wdwdth, 1) = 12: wd(wdwdth, 2) = 8: wd(wdwdth, 3) = 6
	END IF
	wd(wdtop, 1) = 1: wd(wdtop, 2) = 1: wd(wdtop, 3) = 1


	'strings with windows width
	DIM t$(1 TO 3): FOR i = 1 TO 3: t$(i) = SPACE$(wd(wdwdth, i)): NEXT
	h$ = SPACE$(64)


	'prepare command strings
	IF dos = 5 THEN
		IF win THEN x$ = " /Z"
			dosdirs$ = "DIR  /AD /ON /B >" + auxf$(2)
		   dosfiles$ = "DIR *." + ext$ + " /A-D /ON /B >" + auxf$(1)
	ELSE
		dosdirs$ = "DIR *.* |FIND " + q$ + "<DIR>" + q$ + " >" + auxf$(2)
		dosfiles$ = "DIR *." + ext$ + " | FIND " + q$ + ":" + q$ + "|FIND /V " + q$
		dosfiles$ = dosfiles$ + "<DIR>" + q$ + "|FIND /V " + q$ + ":\" + q$ + " >" + auxf$(1)
	END IF

	temp = INSTR(ext$, "."): IF temp THEN ext$ = MID$(ext$, temp + 1)

	'check all possible drives and build a drive list
	file$ = auxf$(3): GOSUB filexist
	IF exist = false THEN
		f = FREEFILE: OPEN auxf$(3) FOR OUTPUT AS #f
		COLOR 7, 0: CLS : PRINT "Checking existing drives: please wait!"
		FOR i = ASC("A") TO ASC("Z")
			IF DiskReady(CHR$(i)) <> 2 THEN
				LSET t$(3) = "-[" + CHR$(i) + ":]-"
				PRINT #f, t$(3)
			END IF
		NEXT
		CLOSE #f
	END IF
	f = FREEFILE: OPEN auxf$(3) FOR INPUT AS #f
	w1 = 3: GOSUB ITEMCOUNT
	CLOSE f

	'init screen
	SCREEN 0: COLOR 7, 1: CLS
	COLOR 14: LOCATE 1, 40 - LEN(header$) \ 2: PRINT header$;
	'top rectangle
	LOCATE 3, 4: COLOR 9, 0: PRINT CHR$(218); STRING$(72, 196); CHR$(191)
	LOCATE 4, 4: PRINT CHR$(179); STRING$(72, 0); CHR$(179)
	LOCATE 5, 4: PRINT CHR$(192); STRING$(72, 196); CHR$(217)

	'other rectangles
	FOR x1 = 1 TO 3: GOSUB rect: NEXT
	COLOR 14, 1: LOCATE scrl - 2, 2
	PRINT "Up/Dn Pgup/PgDn Move cursor, Left/Right Change Panel, Enter Select, Esc Quit";

	'clear keyboard buffer
	WHILE LEN(INKEY$): WEND

	'init drive an dir cursor
	GOSUB curdir
   
	'cursor will start in files window
	actwin = 1
  
	'here we enter the main loop
updatedrive:
	f = FREEFILE: OPEN auxf$(3) FOR INPUT AS #f
		w1 = 3: GOSUB updatewin
	CLOSE f

'update dir list
updatedir:

	GOSUB curdir
	SHELL dosdirs$
	f = FREEFILE
	IF dos = 5 THEN
		OPEN auxf$(2) FOR APPEND AS #f: PRINT #f, ".":
		IF LEN(curdir$) > 3 THEN PRINT #f, ".."
		CLOSE f
	END IF
	OPEN auxf$(2) FOR INPUT AS #f
		w1 = 2: GOSUB ITEMCOUNT: GOSUB updatewin
	CLOSE f

	'update file list
	SHELL dosfiles$
	f = FREEFILE: OPEN auxf$(1) FOR INPUT AS #f
		w1 = 1: GOSUB ITEMCOUNT: GOSUB updatewin
		IF wd(wdcount, 1) = 0 THEN actwin = 2: w1 = 2: GOSUB updatewin
	CLOSE f

'keys loop
'program will stay in this loop unless window change or press enter or esc
movecursor:
	w1 = actwin
	OPEN auxf$(actwin) FOR INPUT AS #f
	DO
		GOSUB updatewin
		DO: V$ = INKEY$: LOOP UNTIL LEN(V$)
		V = ASC(RIGHT$(V$, 1)): IF ASC(V$) = 0 THEN V = -V
		SELECT CASE V
		CASE kup:
			 wd(wdcur, w1) = wd(wdcur, w1) - 1
			 IF wd(wdcur, w1) < 1 THEN wd(wdcur, w1) = 1
		CASE kdown:
			 wd(wdcur, w1) = wd(wdcur, w1) + 1
			 IF wd(wdcur, w1) > wd(wdcount, w1) THEN wd(wdcur, w1) = wd(wdcount, w1)
		CASE kpgup:
			 wd(wdcur, w1) = wd(wdcur, w1) - winheight
			 IF wd(wdcur, w1) < 1 THEN wd(wdcur, w1) = 1
		CASE kpgdn:
			 wd(wdcur, w1) = wd(wdcur, w1) + winheight
			 IF wd(wdcur, w1) > wd(wdcount, w1) THEN wd(wdcur, w1) = wd(wdcount, w1)
		'change active window
		CASE kleft:
			IF actwin > 1 THEN
			   IF actwin <> 2 OR wd(wdcount, 1) > 0 THEN
				actwin = actwin - 1: GOSUB updatewin: CLOSE #f: GOTO movecursor
			   END IF
			END IF
		'change active window
		CASE kright:
			IF actwin < 3 THEN
				actwin = actwin + 1: GOSUB updatewin: CLOSE #f: GOTO movecursor
			END IF
		'select file in file window, change dir or drive in other windows
		CASE kenter:
			WHILE DiskReady(newdrive$) = 1
				err$ = "Disk " + newdrive$ + " not ready   [R]etry/[C]ancel?": GOSUB errmsg
				IF a$ <> "R" THEN CLOSE f: SHELL "C:": GOTO updatedrive
			WEND
			SELECT CASE actwin
			'select file and exit
			CASE 1:
				IF LEN(newfile$) THEN
					CLOSE f: TMP$ = curdir$ + newfile$
					IF win THEN
						'truename fails with filenames so i use it only with dir name
						doscmd$ = "TRUENAME |find " + q$ + ":\" + q$ + ">" + tempfile$
						SHELL doscmd$
						OPEN tempfile$ FOR INPUT AS f: LINE INPUT #f, TP$: CLOSE f
						TMP$ = TP$: IF LEN(TMP$) > 3 THEN TMP$ = TMP$ + "\"
						KILL tempfile$
						doscmd$ = "DIR /A-D " + q$ + newfile$ + q$ + "|FIND " + q$ + ":" + q$ + "|FIND /V " + q$ + ":\" + q$ + ">" + tempfile$
						SHELL doscmd$
						OPEN tempfile$ FOR INPUT AS f: LINE INPUT #f, TP$: CLOSE f
						t$ = SPACE$(12): LSET t$ = TP$: MID$(t$, 9) = "."
						TMP$ = TMP$ + RTRIM$(t$)
					END IF
					SelectAFile$ = TMP$: EXIT DO
				ELSE
					actwin = actwin + 1: GOSUB updatewin
				END IF
			'change dir
			CASE 2:
				IF newdir$ <> "." THEN
					IF (LEN(curdir$) > 3) OR (newdir$ <> "..") THEN
						CLOSE f: SHELL "CD " + newdir$: GOTO updatedir
					END IF
				END IF
			'change drive
			CASE 3:
				 CLOSE f: SHELL newdrive$: GOTO updatedrive
			END SELECT
		CASE kesc:
			CLOSE #f: SelectAFile$ = "": EXIT DO
		END SELECT
	LOOP
	KILL auxf$(1)
	KILL auxf$(2)
	'KILL auxf$(3) 'this file (}{drive.lst) keeps the list of valid drives
	'              to avoid drive testing each time the routine is run
	'              put a REM in  this line and erase the file at program's end.
	'              (Or don't erase it, if your drives are all fixed)
	IF win THEN z$ = q$ ELSE z$ = ""
	CLS
EXIT FUNCTION

'---------------------------gosubs------------------------------------------
'update window W1 from the contents of already opened file f
updatewin:
	SEEK #f, 1
	IF actwin = w1 THEN isactive = true ELSE isactive = false
	IF wd(wdcur, w1) < wd(wdtop, w1) THEN wd(wdtop, w1) = wd(wdcur, w1)
	IF wd(wdcur, w1) > (wd(wdtop, w1) + winheight - 1) THEN wd(wdtop, w1) = wd(wdcur, w1) - winheight + 1
	i = 1: j = wintop: K = wd(wdleft, w1)
	WHILE NOT EOF(f) AND j <= winbot
		LINE INPUT #f, a$
		IF i >= wd(wdtop, w1) THEN
			LSET t$(w1) = a$
			IF i = wd(wdcur, w1) THEN
				IF isactive THEN COLOR fgasel, bgasel ELSE COLOR fgisel, bgisel
				SELECT CASE w1
				 CASE 1: newfile$ = RTRIM$(a$)
					'IF LEN(NEWFILE$) > 9 THEN MID$(NEWFILE$, 9) = "."
				 CASE 2: newdir$ = RTRIM$(a$): IF win THEN newdir$ = q$ + newdir$
				 CASE 3: newdrive$ = MID$(t$(w1), 3, 2)
				END SELECT
			ELSE
				IF isactive THEN COLOR fgauns, bgauns ELSE COLOR fgiuns, bgiuns
			END IF
			LOCATE j, K: PRINT t$(w1); : j = j + 1
		END IF
		i = i + 1
	WEND
	LSET t$(w1) = "": COLOR fgiuns, bgiuns
	FOR j1 = j TO winbot
	   LOCATE j1, K: PRINT t$(w1)
	NEXT
RETURN

curdir:
	tempfile$ = temppath$ + "curr.dir"
	SHELL "CD > " + tempfile$
	f = FREEFILE: OPEN tempfile$ FOR INPUT AS #f
	LINE INPUT #f, curdir$
	IF LEN(curdir$) <> 3 THEN curdir$ = curdir$ + "\"
	curdrive$ = LEFT$(curdir$, 1)
	CLOSE f: KILL tempfile$
	OPEN auxf$(3) FOR INPUT AS #f
		i = 0
		DO: i = i + 1: LINE INPUT #f, a$: LOOP UNTIL INSTR(a$, curdrive$)
	CLOSE f
	wd(wdcur, 3) = i
	LSET h$ = curdir$ + "*." + ext$
	LOCATE 4, 6: COLOR 10: PRINT h$;
RETURN


'GUI: draw a shadowed rectangle, dimensions in wd(?,x1)
rect:
	COLOR 9, 7: LOCATE wintop - 1, wd(wdxpos, x1) - 1
	PRINT CHR$(218); STRING$(wd(wdwdth, x1), 196); CHR$(191)
	FOR j = wintop TO winbot
	  LOCATE j, wd(wdxpos, x1) - 1
	  PRINT CHR$(179); STRING$(wd(wdwdth, x1), 32); CHR$(179)
	NEXT j
	LOCATE winbot + 1, wd(wdxpos, x1) - 1
	PRINT CHR$(192); STRING$(wd(wdwdth, x1), 196); CHR$(217)
	FOR j = wintop TO winbot + 1
		LOCATE j, wd(wdxpos, x1) + wd(wdwdth, x1) + 1: COLOR 0
		PRINT STRING$(2, 219)
	NEXT j
	LOCATE winbot + 2, wd(wdxpos, x1) + 2: PRINT STRING$(wd(wdwdth, x1) + 1, 219)
RETURN

'check if a file exists
filexist:
	f = FREEFILE: OPEN file$ FOR BINARY AS #f
	IF LOF(f) = 0 THEN exist = false: CLOSE #f: KILL file$ ELSE exist = true: CLOSE #f
RETURN

'count items in list file
ITEMCOUNT:
	j = 0: WHILE NOT EOF(f): LINE INPUT #f, a$: j = j + 1: WEND
	wd(wdcount, w1) = j: wd(wdcur, w1) = 1
RETURN

'displays an eror message and waits for a key
errmsg:
	LSET h$ = err$
	LOCATE 4, 6: COLOR 12: PRINT h$;
	a$ = UCASE$(INPUT$(1)): LSET h$ = "": LOCATE 4, 6: PRINT h$
RETURN

END FUNCTION

SUB SVGABLoad (fil$)
DIM ptrscr AS LONG, ptrx AS LONG
f = FREEFILE
OPEN fil$ FOR BINARY AS #f
BUF$ = SPACE$(LEN(SVGABSaveIdent))
GET #f, , BUF$
IF BUF$ <> SVGABSaveIdent THEN
	PRINT "File "; fil$; " has not been Bsaved with JPEG viewer":
	EXIT SUB
END IF
GET #f, , vesainfo.modenum
GET #f, , vesainfo.bpp
dummy = svgasetmode(vesainfo.modenum)
IF vesainfo.bpp = 8 THEN
	BUF$ = SPACE$(palsize)
	GET #f, , BUF$
	i = 1
	OUT &H3C8, 0
	WHILE i < palsize: OUT &H3C9, ASC(MID$(BUF$, i, 1)): i = i + 1: WEND
END IF
BUF$ = SPACE$(bloadsiz)
xseg = VARSEG(BUF$): xptr = SADD(BUF$)
l& = SEEK(f)
i = 0
WHILE l& < LOF(f)
	regs.ax = &H4F05: regs.bx = 0: regs.dx = i: CALL interrupty(&H10, regs, regs)
	i = i + 1
	ptrscr = 0
	FOR j = 1 TO banksteps
		GET #f, , BUF$
		l& = l& + bloadsiz
		ptrx = xptr
		FOR K = 1 TO bloadsiz
			DEF SEG = xseg: temp = PEEK(ptrx): ptrx = ptrx + 1
			DEF SEG = &HA000: POKE ptrscr, temp: ptrscr = ptrscr + 1
		NEXT
		DEF SEG
	NEXT
WEND
CLOSE f
BUF$ = ""
END SUB

SUB SVGABSave (fil$)
DIM ptrscr AS LONG, ptrx AS LONG

ON ERROR GOTO 0
f = FREEFILE
OPEN fil$ FOR BINARY AS #f
BUF$ = SVGABSaveIdent
PUT #f, , BUF$
PUT #f, , vesainfo.modenum
PUT #f, , vesainfo.bpp
IF vesainfo.bpp = 8 THEN
BUF$ = SPACE$(palsize)
	i = 1
	OUT &H3C7, 0
	WHILE i < palsize: MID$(BUF$, i, 1) = CHR$(INP(&H3C9)): i = i + 1: WEND
PUT #f, , BUF$
END IF


BUF$ = SPACE$(bloadsiz)
xseg = VARSEG(BUF$): xptr = SADD(BUF$)
FOR i = 0 TO vesainfo.numberofbanks
	regs.ax = &H4F05: regs.bx = 0: regs.dx = i: CALL interrupty(&H10, regs, regs)
	ptrscr = 0
	FOR j = 1 TO banksteps
	ptrx = xptr
		FOR K = 1 TO bloadsiz
			DEF SEG = &HA000: temp = PEEK(ptrscr): ptrscr = ptrscr + 1
			DEF SEG = xseg: POKE ptrx, temp: ptrx = ptrx + 1
		NEXT
		DEF SEG
		PUT #1, , BUF$
	NEXT
NEXT
CLOSE f
BUF$ = ""


END SUB

SUB SVGAGetData
CONST VESAOK = &H4F
regs.ax = &H4F00
regs.es = VARSEG(vesainfo)
regs.DI = VARPTR(vesainfo)
CALL interrupty(&H10, regs, regs)
IF regs.ax <> VESAOK THEN PRINT "SORRY...VESA CARD NOT DETECTED": END
'get amount of mode numbers
a$ = MKL$(vesainfo.VIDEOMODEPTR)
DEF SEG = vesainfo.VIDEOMODESEG
	ptr1 = vesainfo.VIDEOMODEPTR
	i = 0
	DO UNTIL md& = 65535
		i = i + 1
		temp = PEEK(ptr1)
		ptr1 = ptr1 + 1
		md& = PEEK(ptr1) * 256& + temp
		ptr1 = ptr1 + 1
	LOOP
DEF SEG
vesainfo.modemax = i - 1
END SUB

SUB SVGAGetModeInfo (md&)
		regs.ax = &H4F01
		regs.cx = md&
		regs.es = VARSEG(display)
		regs.DI = VARPTR(display)
		CALL interrupty(&H10, regs, regs)
END SUB

SUB svgappgrey (x, y, lum) STATIC
SHARED CURBANK
IF y <> lasty THEN lasty = y: off1& = vesainfo.bytesrow * y
offset& = off1& + x
bank = offset& \ vesainfo.winsize
offset& = offset& MOD vesainfo.winsize
IF bank <> CURBANK THEN
  CURBANK = bank
  regs.ax = &H4F05
  regs.bx = 0
  regs.dx = CURBANK
  CALL interrupty(&H10, regs, regs)
END IF

DEF SEG = vesainfo.winseg: POKE offset&, lum: DEF SEG
END SUB

SUB svgappixel (x, y, clr&) STATIC
'sets a pixel in SVGA screen
SHARED CURBANK
IF y <> lasty THEN lasty = y: off1& = vesainfo.bytesrow * y
offset& = off1& + x * vesainfo.bytespixel
bank = offset& \ vesainfo.winsize
offset& = offset& MOD vesainfo.winsize
IF bank <> CURBANK THEN
  SWAP CURBANK, bank
  GOSUB switchbank
END IF
bufptr = VARPTR(clr&)
b = PEEK(bufptr)
g = PEEK(bufptr + 1)
r = PEEK(bufptr + 2)


SELECT CASE vesainfo.bpp
CASE 32:
	DEF SEG = vesainfo.winseg
	POKE offset& + 2, r
	POKE offset& + 1, g
	POKE offset&, b
	DEF SEG
CASE 16:
	temp& = (b \ 8) OR ((g * 8) AND &H7E0) OR (256& * r AND &HF800)
	b1 = VARPTR(temp&): a = PEEK(b1): a1 = PEEK(b1 + 1)
	DEF SEG = vesainfo.winseg: POKE offset&, a: POKE offset& + 1, a1: DEF SEG
CASE 24:
	'need to test for window boundary because 65536 (window size)is not divisible
	' by 3
	DEF SEG = vesainfo.winseg
	POKE offset&, b
	IF offset& = 65535 THEN CURBANK = CURBANK + 1: GOSUB switchbank: offset& = -1
	POKE offset& + 1, g
	IF offset& = 65534 THEN CURBANK = CURBANK + 1: GOSUB switchbank: offset& = -2
	POKE offset& + 2, r
	DEF SEG
CASE 8:
	a = (r AND &HE0) OR (g \ 8 AND &H1C) OR (b \ 64)
	DEF SEG = vesainfo.winseg: POKE offset&, a: DEF SEG
CASE 15:
	temp& = (b \ 8) OR (g * 4) AND &H7E0 OR (128& * r AND &H7C00)
	b1 = VARPTR(temp&): a = PEEK(b1): a1 = PEEK(b1 + 1)
	DEF SEG = vesainfo.winseg: POKE offset&, a: POKE offset& + 1, a1: DEF SEG
END SELECT
EXIT SUB

switchbank:
  regs.ax = &H4F05
  regs.bx = 0
  regs.dx = CURBANK
  CALL interrupty(&H10, regs, regs)
RETURN

END SUB

SUB SVGAPrint (cad$, y, x, clr&)
'Fixed size char printing in SVGA.
STATIC sg, of
'locate charmap in bios
  IF sg = 0 THEN
	  regs.ax = &H1130
	  regs.bx = 6 * 256
	  CALL interrupty(&H10, regs, regs)
	  sg = regs.es
	  of = regs.bp
  END IF
  x0 = x
  IF vesainfo.bw THEN grey = (clr& \ 65536 + (clr& AND &HFF00) \ 256 + (clr& AND 255)) \ 3
  FOR i = 1 TO LEN(cad$)  'for each char in string
	  a = ASC(MID$(cad$, i)) * 16 + of
	  x0 = x0 + 8
	  FOR j = 0 TO 15 'for each scan line in character map
		  DEF SEG = sg: lin = PEEK(a + j): DEF SEG
		  IF lin THEN
			  yj = y + j
			  FOR K = 0 TO 7 'for each bit  in scan line
				IF PwrsOf2(K) AND lin THEN
					IF vesainfo.bw THEN
						
						svgappgrey x0 - K, yj, grey
					ELSE
					   svgappixel x0 - K, yj, clr&
					END IF
				END IF
			  NEXT
		  END IF
	  NEXT
  NEXT
  
END SUB

FUNCTION SVGASelectMode%
SHARED f$
CLS
PRINT "The file "; f$; " is "; JPEG.cols; " X "; JPEG.rows
PRINT
PRINT "Suitable SVGA VESA modes:"
PRINT "Be careful. Check docs for the maximum resolution your monitor can handle."
PRINT "(It could be below the maximum the video card can give)"
PRINT "Use at your own risk!"
PRINT "Setting a resolution beyond the capacity of the monitor can DAMAGE it"
PRINT "If your monitor behaves strangely, press twice ESCAPE key inmediately!!"
REDIM modes&(1 TO vesainfo.modemax)
j = 1
FOR i = 1 TO vesainfo.modemax
	'get mode numbers string
	DEF SEG = vesainfo.VIDEOMODESEG
		ptr1 = vesainfo.VIDEOMODEPTR
		ii = 2 * i - 2
		md& = PEEK(ptr1 + ii + 1) * 256& + PEEK(ptr1 + ii)
	DEF SEG
	'get info about current mode
	SVGAGetModeInfo (md&)
	IF display.Modeattributes AND 1 THEN
		SELECT CASE ASC(display.MemoryModel)
		CASE 4       'PALETTE MODE
			PRINT USING "##.- MODE \ \H  #### x #### x ## "; j; HEX$(md&); display.xres; display.yres; ASC(display.bpp)
			modes&(j) = md&: j = j + 1
		CASE 6       'TRUECOLOR MODE
			IF JPEG.numcomp > 1 THEN   'not color modes for monochrome images
				PRINT USING "##.- MODE \ \H  #### x #### x ## "; j; HEX$(md&); display.xres; display.yres; ASC(display.bpp)
				modes&(j) = md&: j = j + 1
			END IF
		END SELECT
	END IF
NEXT
j = j - 1
DO
INPUT "Select a mode"; K
LOOP UNTIL K > 0 AND K <= j
md = modes&(K)

a = svgasetmode(md)
IF (vesainfo.bytespixel = 1) AND (JPEG.numcomp > 1) THEN
	GOSUB setaproxpal
ELSEIF JPEG.numcomp = 1 THEN
	GOSUB setgreypal: vesainfo.bw = true
END IF
	
ERASE modes&
EXIT FUNCTION


setgreypal:
OUT &H3C8, 0            'create the greyscale palette
FOR i1 = 0 TO 255
	temp = i1 \ 4
	OUT &H3C9, temp
	OUT &H3C9, temp
	OUT &H3C9, temp
NEXT
RETURN


setaproxpal:
'create approximative color palette
OUT &H3C8, 0
FOR i1 = 0 TO 7
	FOR j1 = 0 TO 7
		FOR K1 = 0 TO 3
			OUT &H3C9, i1 * 8
			OUT &H3C9, j1 * 8
			OUT &H3C9, K1 * 16
		NEXT
	NEXT
NEXT
RETURN

END FUNCTION

FUNCTION svgasetmode (mode)
' Sets an SVGA mode, and saves some useful parameters
IF mode > 255 THEN
	SVGAGetModeInfo (mode)
	
	vesainfo.xres = display.xres
	vesainfo.yres = display.yres
	vesainfo.bytesrow = display.bytesperscanline
	vesainfo.bpp = ASC(display.bpp)

	SELECT CASE vesainfo.bpp
	CASE 8: vesainfo.bytespixel = 1
	CASE 15, 16: vesainfo.bytespixel = 2
	CASE 24: vesainfo.bytespixel = 3
	CASE 32: vesainfo.bytespixel = 4
	END SELECT

	vesainfo.winsize = 1024& * display.winsize
	vesainfo.winseg = display.winAsegment
	vesainfo.bw = false
	vesainfo.numberofbanks = (vesainfo.bytesrow * vesainfo.yres) \ vesainfo.winsize + 1
	vesainfo.charx = vesainfo.xres \ 70
	vesainfo.chary = vesainfo.yres \ 20
END IF
	vesainfo.modenum = mode

regs.ax = &H4F02  'Set the mode.
regs.bx = mode
CALL interrupty(&H10, regs, regs)
IF regs.ax <> &H4F THEN svgasetmode = 0: EXIT FUNCTION
regs.ax = &H4F07  'Set the top of the screen.
regs.bx = 0
regs.dx = 0
regs.cx = 0
CALL interrupty(&H10, regs, regs)
svgasetmode = 1
END FUNCTION

