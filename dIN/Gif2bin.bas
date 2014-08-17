DECLARE SUB getitifyawantta (A$)

'converts 256 color gifs to bin

DEFINT A-Z

SCREEN 12: CLS
FILES "*.gif"
INPUT "Gif file.(Name only)----> ", fil1$
fil2$ = fil1$ + ".bin"
fil1$ = fil1$ + ".gif"

SCREEN 13: CLS
CALL getitifyawantta(fil1$)

'begin conversion here

xlength = xlength - 1
ylength = ylength - 1

x1 = Xstart: y1 = Ystart
x2 = xlength: y2 = ylength

pixx = (4 + INT(((x2 - x1 + 1) * (8) + 7) / 8) * 1 * ((y2 - y1) + 1)) / 2
'pixx = 4 + INT(((x2 - x1 + 1) * (8) + 7) / 8) * 1 * ((y2 - y1) + 1)

LOCATE 24, 1
'PRINT Xstart; Ystart; xlength; ylength; pixx;

REDIM pix(1 TO pixx)
REDIM pix2(1 TO pixx)
GET (x1, y1)-(x2, y2), pix
'PUT (102, 0), pix, PSET

OPEN fil2$ FOR BINARY AS #2
FOR iii = 1 TO pixx
 PUT #2, , pix(iii)
 'GET #2, iii, pix2(iii)
NEXT iii
CLOSE

OPEN fil2$ FOR BINARY AS #2
FOR iii = 1 TO pixx
 GET #2, , pix2(iii)
NEXT iii
CLOSE

'PUT (204, 0), pix2, PSET

END

SUB getitifyawantta (A$)
SHARED Xstart, Ystart, xlength, ylength

'I don't know who wrote the GIF decoder, but its the first one
'I've tried that actually worked.

 DIM Prefix(4095), Suffix(4095), OutStack(4095), ShiftOut(8)
 
 'The following line is for the QB environment(slow).
 DIM Ybase AS LONG, Powersof2(11) AS LONG, WorkCode AS LONG
 
 'For more speed, unremark the next line and remark the one above,
 'before you compile... (Change back when inside the environment.)
 'DIM Ybase AS INTEGER, Powersof2(11) AS INTEGER, WorkCode AS INTEGER
 
 FOR A = 0 TO 7
   ShiftOut(8 - A) = 2 ^ A
 NEXT A
 
 FOR A = 0 TO 11
   Powersof2(A) = 2 ^ A
 NEXT A
 
 OPEN A$ FOR BINARY AS #1
  A$ = "      "
  GET #1, , A$
  IF A$ <> "GIF87a" THEN PRINT "Not a GIF87a file.": END

 GET #1, , TotalX
 GET #1, , TotalY
 GOSUB GetByte

 NumColors = 2 ^ ((A AND 7) + 1)
 NoPalette = (A AND 128) = 0

 GOSUB GetByte
 Background = A

 GOSUB GetByte
 IF A <> 0 THEN PRINT "Bad screen descriptor.": END

 IF NoPalette = 0 THEN P$ = SPACE$(NumColors * 3): GET #1, , P$
 DO
     GOSUB GetByte
     IF A = 44 THEN
         EXIT DO
     ELSEIF A <> 33 THEN
         PRINT "Unknown extension type.": END
     END IF
     GOSUB GetByte
     DO: GOSUB GetByte: A$ = SPACE$(A): GET #1, , A$: LOOP UNTIL A = 0
 LOOP

 GET #1, , Xstart
 GET #1, , Ystart
 GET #1, , xlength
 GET #1, , ylength
 XEnd = Xstart + xlength
 YEnd = Ystart + ylength

 GOSUB GetByte
 IF A AND 128 THEN PRINT "Can't handle local colormaps.": END
 Interlaced = A AND 64
 PassNumber = 0
 PassStep = 8
 GOSUB GetByte
 ClearCode = 2 ^ A
 EOSCode = ClearCode + 1
 FirstCode = ClearCode + 2: NextCode = FirstCode
 StartCodeSize = A + 1: CodeSize = StartCodeSize
 StartMaxCode = 2 ^ (A + 1) - 1: MaxCode = StartMaxCode
 BitsIn = 0: BlockSize = 0: BlockPointer = 1
 x = Xstart: y = Ystart: Ybase = y * 320&

 SCREEN 13: DEF SEG = &HA000
 IF NoPalette = 0 THEN
     OUT &H3C7, 0: OUT &H3C8, 0
     FOR A = 1 TO NumColors * 3: OUT &H3C9, ASC(MID$(P$, A, 1)) \ 4: NEXT
 END IF

 LINE (0, 0)-(319, 199), Background, BF

 DO
     GOSUB GetCode
     IF Code <> EOSCode THEN
         IF Code = ClearCode THEN
             NextCode = FirstCode
             CodeSize = StartCodeSize
             MaxCode = StartMaxCode
             GOSUB GetCode
             CurCode = Code: LastCode = Code: LastPixel = Code
             IF x < 320 THEN POKE x + Ybase, LastPixel
             x = x + 1: IF x = XEnd THEN GOSUB NextScanLine
         ELSE
             CurCode = Code: StackPointer = 0
             IF Code > NextCode THEN EXIT DO 'bad GIF if this happens
             IF Code = NextCode THEN
                 CurCode = LastCode
                 OutStack(StackPointer) = LastPixel
                 StackPointer = StackPointer + 1
             END IF
 
             DO WHILE CurCode >= FirstCode
                 OutStack(StackPointer) = Suffix(CurCode)
                 StackPointer = StackPointer + 1
                 CurCode = Prefix(CurCode)
             LOOP
 
             LastPixel = CurCode
             IF x < 320 THEN POKE x + Ybase, LastPixel
             x = x + 1: IF x = XEnd THEN GOSUB NextScanLine
 
             FOR A = StackPointer - 1 TO 0 STEP -1
                 IF x < 320 THEN POKE x + Ybase, OutStack(A)
                 x = x + 1: IF x = XEnd THEN GOSUB NextScanLine
             NEXT
 
             IF NextCode < 4096 THEN
                 Prefix(NextCode) = LastCode
                 Suffix(NextCode) = LastPixel
                 NextCode = NextCode + 1
                 IF NextCode > MaxCode AND CodeSize < 12 THEN
                     CodeSize = CodeSize + 1
                     MaxCode = MaxCode * 2 + 1
                 END IF
             END IF
             LastCode = Code
         END IF
     END IF
 LOOP UNTIL DoneFlag OR Code = EOSCode
 GOTO tt

'------------------------------------------------------------------------
GetByte:
  A$ = " "
  GET #1, , A$
  A = ASC(A$)
RETURN

'------------------------------------------------------------------------
NextScanLine:
     IF Interlaced THEN
         y = y + PassStep
         IF y >= YEnd THEN
             PassNumber = PassNumber + 1
             SELECT CASE PassNumber
             CASE 1: y = 4: PassStep = 8
             CASE 2: y = 2: PassStep = 4
             CASE 3: y = 1: PassStep = 2
             END SELECT
         END IF
     ELSE
         y = y + 1
     END IF
     x = Xstart: Ybase = y * 320&: DoneFlag = y > 199
RETURN

'------------------------------------------------------------------------
GetCode:
     IF BitsIn = 0 THEN GOSUB ReadBufferedByte: LastChar = A: BitsIn = 8
     WorkCode = LastChar \ ShiftOut(BitsIn)
     DO WHILE CodeSize > BitsIn
         GOSUB ReadBufferedByte: LastChar = A
         WorkCode = WorkCode OR LastChar * Powersof2(BitsIn)
         BitsIn = BitsIn + 8
     LOOP
     BitsIn = BitsIn - CodeSize
     Code = WorkCode AND MaxCode
RETURN

'------------------------------------------------------------------------
ReadBufferedByte:
     IF BlockPointer > BlockSize THEN
         GOSUB GetByte: BlockSize = A
         A$ = SPACE$(BlockSize): GET #1, , A$
         BlockPointer = 1
     END IF
     A = ASC(MID$(A$, BlockPointer, 1)): BlockPointer = BlockPointer + 1
RETURN

'------------------------------------------------------------------------
tt:
 CLOSE #1
 END SUB

