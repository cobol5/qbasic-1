' JPEG loader by Petter Holmberg of Enhanced Creations
' Version 1.0, 24/5 1999

DECLARE SUB LoadJPEG (filename$, jpegerror%)
DECLARE SUB ReadSOI (soierror%)
DECLARE SUB ReadAPP0 (app0type%, app0error%)
DECLARE SUB ReadDQT (dqterror%)
DECLARE SUB ReadSOF0 (sof0error%)
DECLARE SUB ReadDHT (dhterror%)
DECLARE SUB ReadDRI ()
DECLARE SUB ReadSOS (soserror%)
DECLARE SUB DecodeBlock (decodeerror%)
DECLARE FUNCTION NextBit% ()
DECLARE SUB ReadEOI (eoierror%)

TYPE JPEGType
  filename AS STRING * 12
  version AS INTEGER
  units AS INTEGER
  xden AS INTEGER
  yden AS INTEGER
  xthumb AS INTEGER
  ythumb AS INTEGER
  numdqts AS INTEGER
  p AS INTEGER
  x AS INTEGER
  y AS INTEGER
  nf AS INTEGER
  ri AS LONG
END TYPE

TYPE ComponentType
  ci AS INTEGER
  h AS INTEGER
  v AS INTEGER
  tqi AS INTEGER
END TYPE

TYPE ScanType
  cs AS INTEGER
  td AS INTEGER
  ta AS INTEGER
END TYPE

DIM JPEG AS JPEGType
DIM component(1 TO 3) AS ComponentType
DIM dqttable%(3, 63)
DIM huffval%(1, 1, 191)
DIM huffsize%(1, 1, 191)
DIM huffcode&(1, 1, 191)
DIM decodeinfo(1 TO 3) AS ScanType
DIM block&(7, 7)

jpegerror% = 0
t0! = 0
t1! = 0

CLS
SCREEN 0
WIDTH 80, 25

PRINT "JPEG loader by Petter Holmberg of Enhanced Creations"
PRINT "Version 1.0, 24/5 1999"
PRINT
PRINT "Loading JPEG images in QuickBASIC is a problem that has always been impossible,"
PRINT "mainly because the JPEG File Interchange Format (JFIF) is so advanced and so"
PRINT "poorly documented outside of the official standard documents, which are very"
PRINT "hard to understand. Still, JPEG is the most used image file formats for"
PRINT "digitalized photographic images, and many people have asked for a working JPEG"
PRINT "loader in QuickBASIC. Now, they don't have to ask anymore. Here it is!"
PRINT "This program has been a true challange to create! The JPEG standard includes"
PRINT "lots of advanced algorithms needed to compress image data as much as possible."
PRINT "In this initial version of my JPEG loader, only baseline 8-bit grayscale JPEG"
PRINT "images are supported, but I hope to extend it to color images as well in the"
PRINT "near future. The problem is that all color images in the JFIF format uses 16.7"
PRINT "million colors, so regular QB screen modes aren't enough."
PRINT "This version should be about 95% compatible with the basic JFIF specification,"
PRINT "so I still have to add some stuff to make it truly complete. I have also"
PRINT "deliberately avoided any attempts to optimize the loader for speed, which I"
PRINT "will start doing in the next version. This version is terribly slow. Even with"
PRINT "a fast Intel PII or AMD K7 processor it will take a couple of minutes to fully"
PRINT "load a 320*200 picture. So stay tuned for huge optimizations in the near future!"
PRINT "Enjoy!"
PRINT
PRINT "Petter Holmberg, -99 (http://ec.quickbasic.com, petter.holmberg@usa.net) ";

DO: LOOP WHILE INKEY$ = ""

CLS

PRINT "Now type the name of a JPEG image file to load. Make sure it's a baseline"
PRINT "8-bit grayscale image, or otherwise the loader won't be able to display it!"
PRINT
INPUT "Filename: ", filename$
IF INSTR(filename$, ".") = 0 THEN filename$ = filename$ + ".JPG"
PRINT

LoadJPEG filename$, jpegerror%

minutes% = (t1! - t0!) \ 60
seconds% = (t1! - t0) - minutes% * 60

IF jpegerror% THEN CLOSE #1
SELECT CASE jpegerror%
CASE 0
 PRINT "JPEG file successfully loaded and displayed!"
 PRINT "Total loading time:"; minutes%; "minutes and"; seconds%; "seconds."
CASE 1
 PRINT "No SOI marker found. File does not appear to be a valid JFIF image file."
CASE 2
 PRINT "Invalid APP0 marker. File does not appear to be a valid JFIF image file."
CASE 3
 PRINT "Invalid DQT marker. File does not appear to be a valid JFIF image file."
CASE 4
 PRINT "Invalid SOF0 marker. File does not appear to be a valid JFIF image file."
CASE 5
 PRINT "Invalid DHT marker. File does not appear to be a valid JFIF image file."
CASE 6
 PRINT "No SOS marker found. File does not appear to be a valid JFIF image file."
CASE 7
 PRINT "Sorry, but this version only supports 1-component grayscale image files."
CASE 8
 PRINT "No EOI marker found. Abnormal termination of image loading."
END SELECT

SUB DecodeBlock (decodeerror%)

SHARED JPEG AS JPEGType
SHARED component() AS ComponentType
SHARED huffval%(), huffsize%(), huffcode&()
SHARED block&(), dqttable%()

STATIC olddc%

DIM blockdata&(63)
DIM idctvalues!(7, 7)
DIM newbyte AS STRING * 1

' Get DC coefficient:

decodeerror% = 0
eob% = 0
blockofs% = 0
bitsleft% = 0
dcnumbits% = 0
numbits% = 0
bits& = 0
dcbits& = 0

DO
   bits& = bits& * 2 + NextBit%
   numbits% = numbits% + 1

   FOR i% = 0 TO 11
      IF huffsize%(0, 0, i%) = numbits% AND huffcode&(0, 0, i%) = bits& THEN
       dcnumbits% = huffval%(0, 0, i%)
       codefound% = 1
       EXIT FOR
      END IF
   NEXT i%
LOOP UNTIL codefound% = 1

FOR getbits% = 1 TO dcnumbits%
   dcbits& = dcbits& * 2 + NextBit%
NEXT getbits%

IF NOT dcbits& AND 2 ^ (dcnumbits% - 1) THEN
 dcbits& = -(2 ^ (dcnumbits%) - 1) + dcbits&
END IF

blockdata&(0) = dcbits&
blockofs% = 1

' Get AC coefficients:

DO
   numbits% = 0
   bits& = 0
   acbits& = 0
   codefound% = 0

   DO
      bits& = bits& * 2 + NextBit%
      numbits% = numbits% + 1

      FOR i% = 0 TO 191
         IF huffsize%(1, 0, i%) = numbits% AND huffcode&(1, 0, i%) = bits& THEN
          acrunsize% = huffval%(1, 0, i%)
          codefound% = 1
          EXIT FOR
         END IF
      NEXT i%
   LOOP UNTIL codefound% = 1

   acnumbits% = acrunsize% AND &HF
   zrun% = acrunsize% \ &H10

   IF acnumbits% = 0 THEN
    IF zrun% = &HF THEN
     blockofs% = blockofs% + 16
    ELSE
     EXIT DO
    END IF
   ELSE
    blockofs% = blockofs% + zrun%

    FOR getbits% = 1 TO acnumbits%
       acbits& = acbits& * 2 + NextBit%
    NEXT getbits%

    IF NOT acbits& AND 2 ^ (acnumbits% - 1) THEN
     acbits& = -(2 ^ (acnumbits%) - 1) + acbits&
    END IF

    blockdata&(blockofs%) = acbits&
    blockofs% = blockofs% + 1
   END IF
LOOP UNTIL blockofs% > 63

' Dequantization:

FOR dqloop% = 0 TO 63
      blockdata&(dqloop%) = blockdata&(dqloop%) * dqttable%(0, dqloop%)
NEXT dqloop%

' Zig-zag scan:
block&(0, 0) = blockdata&(0)
block&(1, 0) = blockdata&(1)
block&(0, 1) = blockdata&(2)
block&(0, 2) = blockdata&(3)
block&(1, 1) = blockdata&(4)
block&(2, 0) = blockdata&(5)
block&(3, 0) = blockdata&(6)
block&(2, 1) = blockdata&(7)
block&(1, 2) = blockdata&(8)
block&(0, 3) = blockdata&(9)
block&(0, 4) = blockdata&(10)
block&(1, 3) = blockdata&(11)
block&(2, 2) = blockdata&(12)
block&(3, 1) = blockdata&(13)
block&(4, 0) = blockdata&(14)
block&(5, 0) = blockdata&(15)
block&(4, 1) = blockdata&(16)
block&(3, 2) = blockdata&(17)
block&(2, 3) = blockdata&(18)
block&(1, 4) = blockdata&(19)
block&(0, 5) = blockdata&(20)
block&(0, 6) = blockdata&(21)
block&(1, 5) = blockdata&(22)
block&(2, 4) = blockdata&(23)
block&(3, 3) = blockdata&(24)
block&(4, 2) = blockdata&(25)
block&(5, 1) = blockdata&(26)
block&(6, 0) = blockdata&(27)
block&(7, 0) = blockdata&(28)
block&(6, 1) = blockdata&(29)
block&(5, 2) = blockdata&(30)
block&(4, 3) = blockdata&(31)
block&(3, 4) = blockdata&(32)
block&(2, 5) = blockdata&(33)
block&(1, 6) = blockdata&(34)
block&(0, 7) = blockdata&(35)
block&(1, 7) = blockdata&(36)
block&(2, 6) = blockdata&(37)
block&(3, 5) = blockdata&(38)
block&(4, 4) = blockdata&(39)
block&(5, 3) = blockdata&(40)
block&(6, 2) = blockdata&(41)
block&(7, 1) = blockdata&(42)
block&(7, 2) = blockdata&(43)
block&(6, 3) = blockdata&(44)
block&(5, 4) = blockdata&(45)
block&(4, 5) = blockdata&(46)
block&(3, 6) = blockdata&(47)
block&(2, 7) = blockdata&(48)
block&(3, 7) = blockdata&(49)
block&(4, 6) = blockdata&(50)
block&(5, 5) = blockdata&(51)
block&(6, 4) = blockdata&(52)
block&(7, 3) = blockdata&(53)
block&(7, 4) = blockdata&(54)
block&(6, 5) = blockdata&(55)
block&(5, 6) = blockdata&(56)
block&(4, 7) = blockdata&(57)
block&(5, 7) = blockdata&(58)
block&(6, 6) = blockdata&(59)
block&(7, 5) = blockdata&(60)
block&(7, 6) = blockdata&(61)
block&(6, 7) = blockdata&(62)
block&(7, 7) = blockdata&(63)

' IDCT:

block&(0, 0) = olddc% + block&(0, 0)
olddc% = block&(0, 0)

FOR y% = 0 TO 7
   FOR x% = 0 TO 7
      idctvalues!(x%, y%) = 0
      FOR v% = 0 TO 7
         FOR u% = 0 TO 7
            IF u% = 0 THEN cu! = 1 / SQR(2) ELSE cu! = 1
            IF v% = 0 THEN cv! = 1 / SQR(2) ELSE cv! = 1
            idctvalues!(x%, y%) = idctvalues!(x%, y%) + cu! * cv! * block&(u%, v%) * COS((2 * x% + 1) * u% * 3.14159265358979# / 16) * COS((2 * y% + 1) * v% * 3.14159265358979# / 16)
         NEXT u%
      NEXT v%
      idctvalues!(x%, y%) = (idctvalues!(x%, y%) * .25)
   NEXT x%
NEXT y%

FOR y% = 0 TO 7
   FOR x% = 0 TO 7
      block&(x%, y%) = idctvalues!(x%, y%) + 128
   NEXT x%
NEXT y%

END SUB

' This SUB controls the loading of the JPEG image.
'
SUB LoadJPEG (filename$, jpegerror%)
SHARED JPEG AS JPEGType
SHARED block&()
SHARED t0!, t1!

OPEN filename$ FOR BINARY ACCESS READ AS #1

JPEG.filename = UCASE$(filename$)

ReadSOI jpegerror%
IF jpegerror% = 1 THEN EXIT SUB

ReadAPP0 1, jpegerror%
IF jpegerror% = 2 THEN EXIT SUB

DO
   ReadAPP0 0, jpegerror%
LOOP UNTIL jpegerror% = 2

ReadDQT jpegerror%
IF jpegerror% = 3 THEN EXIT SUB

DO
   ReadDQT jpegerror%
LOOP UNTIL jpegerror% = 3

ReadSOF0 jpegerror%
IF jpegerror% = 4 THEN EXIT SUB

ReadDHT jpegerror%
IF jpegerror% = 5 THEN EXIT SUB

DO
   ReadDHT jpegerror%
LOOP UNTIL jpegerror% = 5

ReadDRI

ReadSOS jpegerror%
IF jpegerror% = 6 THEN EXIT SUB

IF JPEG.nf <> 1 THEN
 jpegerror% = 7
 EXIT SUB
END IF

PRINT "Filename:             "; JPEG.filename
PRINT "Version:              "; HEX$(JPEG.version)
PRINT "X density:           "; JPEG.xden
PRINT "Y density:           "; JPEG.yden
PRINT "X thumbnail:         "; JPEG.xthumb
PRINT "Y thumbnail:         "; JPEG.ythumb
PRINT "Number of DQTs:      "; JPEG.numdqts
PRINT "Sample precision:    "; JPEG.p
PRINT "Number of columns:   "; JPEG.x
PRINT "Number of rows:      "; JPEG.y
PRINT "Number of components:"; JPEG.nf
PRINT "Restart interval:    "; JPEG.ri
PRINT
PRINT "Press any key to view image..."

DO: LOOP WHILE INKEY$ = ""

BEEP

SCREEN 13

OUT &H3C8, 0
FOR i% = 0 TO 63
   OUT &H3C9, i%
   OUT &H3C9, i%
   OUT &H3C9, i%
NEXT i%

t0! = TIMER

FOR py% = 0 TO JPEG.y - 1 STEP 8
   FOR px% = 0 TO JPEG.x - 1 STEP 8
      DecodeBlock jpegerror%
      FOR y% = 0 TO 7
         FOR x% = 0 TO 7
            pixel% = block&(x%, y%) \ 4
            IF pixel% < 0 THEN pixel% = 0
            IF pixel% > 63 THEN pixel% = 63
            PSET (px% + x%, py% + y%), pixel%
         NEXT x%
      NEXT y%
   NEXT px%
NEXT py%

t1! = TIMER

ReadEOI jpegerror%

CLOSE #1

BEEP

DO: LOOP WHILE INKEY$ = ""

SCREEN 0
WIDTH 80, 25

END SUB

FUNCTION NextBit%
STATIC bitsleft%, byte AS STRING * 1, byte2 AS STRING * 1

IF bitsleft% = 0 THEN
 GET #1, , byte
 bitsleft% = 8
 IF byte = CHR$(&HFF) THEN
  GET #1, , byte2
  IF byte2 <> CHR$(0) THEN GET #1, , byte
 END IF
END IF

IF ASC(byte) AND (2 ^ (bitsleft% - 1)) THEN bit% = 1
bitsleft% = bitsleft% - 1

NextBit% = bit%

END FUNCTION

SUB ReadAPP0 (app0type%, app0error%)
SHARED JPEG AS JPEGType

app0error% = 2

DIM marker AS STRING * 2
DIM lengthval AS STRING * 2
DIM units AS STRING * 1
DIM density AS STRING * 2
DIM readthumbs AS STRING * 1

GET #1, , marker
IF marker <> CHR$(&HFF) + CHR$(&HE0) THEN
 SEEK #1, SEEK(1) - 2
 EXIT SUB
END IF

GET #1, , lengthval
length% = ASC(LEFT$(lengthval, 1)) * &H100 + ASC(RIGHT$(lengthval, 1))

DIM idstring AS STRING * 5
GET #1, , idstring
IF idstring = "JFIF" + CHR$(0) THEN
 IF app0type% <> 1 THEN EXIT SUB
ELSE
 SEEK #1, SEEK(1) + length% - 5
 EXIT SUB
END IF

GET #1, , JPEG.version

GET #1, , units
JPEG.units = ASC(units)

GET #1, , density
JPEG.xden = ASC(LEFT$(density, 1)) * &H100 + ASC(RIGHT$(density, 1))
GET #1, , density
JPEG.yden = ASC(LEFT$(density, 1)) * &H100 + ASC(RIGHT$(density, 1))

GET #1, , readthumbs
JPEG.xthumb = ASC(readthumbs)
GET #1, , readthumbs
JPEG.ythumb = ASC(readthumbs)

SEEK #1, SEEK(1) + JPEG.xthumb * JPEG.ythumb

app0error% = 0

END SUB

SUB ReadDHT (dhterror%)
SHARED JPEG AS JPEGType
SHARED huffval%(), huffsize%(), huffcode&()

dhterror% = 5

DIM marker AS STRING * 2
DIM lengthval AS STRING * 2
DIM tcth AS STRING * 1
DIM ncodes AS STRING * 1
DIM incode AS STRING * 1

GET #1, , marker
IF marker <> CHR$(&HFF) + CHR$(&HC4) THEN
 SEEK #1, SEEK(1) - 2
 EXIT SUB
END IF

GET #1, , lengthval
length% = ASC(LEFT$(lengthval, 1)) * &H100 + ASC(RIGHT$(lengthval, 1))

DIM bits%(1 TO 16)

GET #1, , tcth
tableclass% = ASC(tcth) \ &H10
tableid% = ASC(tcth) AND &HF

FOR getnumcodes% = 1 TO 16
   GET #1, , ncodes
   bits%(getnumcodes%) = ASC(ncodes)
NEXT getnumcodes%

valofs% = 0
FOR scancodes% = 1 TO 16
   FOR getcodes% = 1 TO bits%(scancodes%)
      GET #1, , incode
      huffval%(tableclass%, tableid%, valofs%) = ASC(incode)
      valofs% = valofs% + 1
   NEXT getcodes%
NEXT scancodes%

k% = 0
j% = 1
FOR i% = 1 TO 16
   WHILE j% <= bits%(i%)
      huffsize%(tableclass%, tableid%, k%) = i%
      k% = k% + 1
      j% = j% + 1
   WEND
   j% = 1
NEXT i%
huffsize%(tableclass%, tableid%, k%) = 0

k% = 0
code& = 0
si% = huffsize%(tableclass%, tableid%, 0)
DO
   DO
      huffcode&(tableclass%, tableid%, k%) = code&
      code& = code& + 1
      k% = k% + 1
   LOOP WHILE huffsize%(tableclass%, tableid%, k%) = si%

   IF huffsize%(tableclass%, tableid%, k%) = 0 THEN EXIT DO

   DO
      code& = code& * 2
      si% = si% + 1
   LOOP UNTIL huffsize%(tableclass%, tableid%, k%) = si%
LOOP WHILE huffsize%(tableclass%, tableid%, k%) = si%

dhterror% = 0

END SUB

SUB ReadDQT (dqterror%)
SHARED JPEG AS JPEGType
SHARED dqttable%()

dqterror% = 3

DIM marker AS STRING * 2
DIM lengthval AS STRING * 2
DIM pt AS STRING * 1
DIM dqtvalue AS STRING * 1

GET #1, , marker
IF marker <> CHR$(&HFF) + CHR$(&HDB) THEN
 SEEK #1, SEEK(1) - 2
 EXIT SUB
END IF

GET #1, , lengthval
length% = ASC(LEFT$(lengthval, 1)) * &H100 + ASC(RIGHT$(lengthval, 1)) - 3

DO
   GET #1, , pt
   precision% = ASC(pt) \ &H10
   tableid% = ASC(pt) AND &HF

   FOR readtable% = 0 TO 63
      GET #1, , dqtvalue
      dqttable%(tableid%, readtable%) = ASC(dqtvalue)
   NEXT readtable%

   length% = length% - 64
   JPEG.numdqts = JPEG.numdqts + 1
LOOP UNTIL length% = 0

dqterror% = 0

END SUB

SUB ReadDRI
SHARED JPEG AS JPEGType

DIM marker AS STRING * 2
DIM ri AS STRING * 2

GET #1, , marker
IF marker <> CHR$(&HFF) + CHR$(&HDD) THEN
 SEEK #1, SEEK(1) - 2
 EXIT SUB
END IF

GET #1, , length%

GET #1, , ri
JPEG.ri = ASC(LEFT$(ri, 1)) * &H100 + ASC(RIGHT$(ri, 1))

END SUB

SUB ReadEOI (eoierror%)

eoierror% = 8

DIM marker AS STRING * 2

GET #1, , marker
IF marker <> CHR$(&HFF) + CHR$(&HD9) THEN EXIT SUB

eoierror% = 0

END SUB

SUB ReadSOF0 (sof0error%)
SHARED JPEG AS JPEGType
SHARED component() AS ComponentType

sof0error% = 4

DIM marker AS STRING * 2
DIM lengthval AS STRING * 2
DIM p AS STRING * 1
DIM xy AS STRING * 2
DIM nf AS STRING * 1
DIM C AS STRING * 1

GET #1, , marker
IF marker <> CHR$(&HFF) + CHR$(&HC0) THEN
 SEEK #1, SEEK(1) - 2
 EXIT SUB
END IF

GET #1, , lengthval
length% = ASC(LEFT$(lengthval, 1)) * &H100 + ASC(RIGHT$(lengthval, 1))

GET #1, , p
JPEG.p = ASC(p)

GET #1, , xy
JPEG.y = ASC(LEFT$(xy, 1)) * &H100 + ASC(RIGHT$(xy, 1))
GET #1, , xy
JPEG.x = ASC(LEFT$(xy, 1)) * &H100 + ASC(RIGHT$(xy, 1))

GET #1, , nf
JPEG.nf = ASC(nf)

FOR loadcinfo% = 1 TO JPEG.nf
   GET #1, , C
   component(loadcinfo%).ci = ASC(C)
   GET #1, , C
   component(loadcinfo%).h = ASC(C) \ &H10
   component(loadcinfo%).v = ASC(C) AND &HF
   GET #1, , C
   component(loadcinfo%).tqi = ASC(C)
NEXT loadcinfo%

sof0error% = 0

END SUB

SUB ReadSOI (soierror%)

soierror% = 1

DIM marker AS STRING * 2

GET #1, , marker
IF marker <> CHR$(&HFF) + CHR$(&HD8) THEN EXIT SUB

soierror% = 0

END SUB

SUB ReadSOS (soserror%)
SHARED decodeinfo() AS ScanType

soserror% = 6

DIM marker AS STRING * 2
DIM lengthval AS STRING * 2
DIM nsvalue AS STRING * 1
DIM cs AS STRING * 1
DIM tdta AS STRING * 1
DIM garbage AS STRING * 3

GET #1, , marker
IF marker <> CHR$(&HFF) + CHR$(&HDA) THEN
 SEEK #1, SEEK(1) - 2
 EXIT SUB
END IF

GET #1, , lengthval
length% = ASC(LEFT$(lengthval, 1)) * &H100 + ASC(RIGHT$(lengthval, 1))

GET #1, , nsvalue
ns% = ASC(nsvalue)
   
FOR readfield1% = 1 TO ns%
   GET #1, , cs
   decodeinfo(readfield1%).cs = ASC(cs)
   GET #1, , tdta
   decodeinfo(readfield1%).td = ASC(tdta) \ &H10
   decodeinfo(readfield1%).ta = ASC(tdta) AND &HF
NEXT readfield1%

FOR readfield2% = 1 TO ns%
   GET #1, , garbage
NEXT readfield2%

soserror% = 0

END SUB

