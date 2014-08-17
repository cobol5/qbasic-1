'$INCLUDE: 'rellib.bi'

DEFINT A-Z
DECLARE SUB RelSavePic (Layer%, FileName$, ImageType%, Gr.Mode%)

'The command "RelSavePic" has not yet been designed to save the pics in
'a .PCX format.  :(  BUT, it _IS_ designed to save your pictures in .BMP,
'.GIF, .RAS, .TGA, .WMF, and MORE, and **even** as a ( :O ! ) .COM-based
'executable, too!!!  :*D !!!

'Converted to RelLib by Adigun Azikiwe Polack.

'Special Thanks to Dav for the original "SAVE13" routines that are used
'                                                             here!!!  ;*) !!

SUB RelSavePic (Layer%, FileName$, ImageType%, Gr.Mode%)

SELECT CASE Gr.Mode%
  CASE 1: VertPX% = 132
  CASE 2: VertPX% = 99
  CASE 3: VertPX% = 79
  CASE 4: VertPX% = 66
  CASE 5: VertPX% = 56
  CASE ELSE: VertPX% = 199
END SELECT

IF ImageType% < 1 OR ImageType% > 16 THEN EXIT SUB

OPEN FileName$ FOR OUTPUT AS 5          'Name of file to create

OUT 967, 0: Pal$ = ""
FOR c% = 1 TO 768                       'Grab palette
  Pal$ = Pal$ + CHR$(INP(969))          'Hold it as Pal$
NEXT

SELECT CASE ImageType%
  CASE 1, 2                         'Make a RAW image
    IF ImageType% = 2 THEN PRINT #5, Pal$;       'Add Pal$
    GOSUB GetRawImage
  CASE 3, 4                         'Make a BLOADable file
    BH$ = "FD00A0000000FA"          'Save a BSAVE header 1st
    FOR I = 1 TO LEN(BH$) STEP 2
      PRINT #5, CHR$(VAL("&H" + MID$(BH$, I, 2)));
    NEXT
   
    'I know...just BSAVE it. Right?  Nope.  I didn't want to put any
    'DEF SEG's in this SUB.  That's why there's no POKEing either.
    'You can add this SUB to your programs without DEF SEG worries.

    GOSUB GetRawImage
    IF ImageType% = 4 THEN PRINT #5, Pal$;   'Add Pal$
  CASE 5, 7               'Make a BMP/DIB/CLP
    IF ImageType% = 7 THEN          'CLP header
      A$ = "50C30100080028FE00000E0000002800000040010000C800000001000800"
      A$ = A$ + "0000000000FA000000000000000000000001000000010000"
    ELSE                            'BMP/DIB header
      A$ = "424D36FE000000000000360400002800000040010000C800000001000"
      A$ = A$ + "800000000000000000000000000000000000000000000000000"
    END IF
    FOR I% = 1 TO LEN(A$) STEP 2
      PRINT #5, CHR$(VAL("&H" + MID$(A$, I%, 2)));
    NEXT
    FOR A% = 0 TO 255: OUT 967, A%     'Grab and save the pallette
      R$ = CHR$(INP(969) * 4)          'Red
      G$ = CHR$(INP(969) * 4)          'Green
      B$ = CHR$(INP(969) * 4)          'Blue
      PRINT #5, B$; G$; R$; CHR$(0);   'Save'em all
    NEXT
    FOR y% = 199 TO 0 STEP -1
      FOR x% = 0 TO 319
        PRINT #5, CHR$(RelPoint(Layer%, x%, y%));
      NEXT: RelLine Layer%, 0, y%, 319, y%, 0
    NEXT
  CASE 6
    A$ = "424D0000000000000000360400002800000040010000C800000001000800"
    A$ = A$ + "010000000000000000000000000000000000000000000000"
    FOR I% = 1 TO LEN(A$) STEP 2
      PRINT #5, CHR$(VAL("&H" + MID$(A$, I%, 2)));
    NEXT
    FOR A% = 0 TO 255: OUT 967, A%
      R$ = CHR$(INP(969) * 4)
      G$ = CHR$(INP(969) * 4)
      B$ = CHR$(INP(969) * 4)
      PRINT #5, B$; G$; R$; CHR$(0);
    NEXT
    FOR y% = 199 TO 0 STEP -1
      FOR x% = 0 TO 319
        clr% = RelPoint(Layer%, x%, y%)
        IF clr% = RelPoint(Layer%, x% + 1, y%) THEN
          c% = 2: x% = x% + 2
          DO WHILE RelPoint(Layer%, x%, y%) = clr%
            c% = c% + 1: x% = x% + 1
            IF x% = 319 THEN EXIT DO
            IF c% = 255 THEN EXIT DO
          LOOP: PRINT #5, CHR$(c%); CHR$(clr%);
          x% = x% - 1
        ELSE
          PRINT #5, CHR$(1); CHR$(clr%);
        END IF
      NEXT: RelLine Layer%, 0, y%, 319, y%, 0
      PRINT #5, CHR$(0); CHR$(0);
    NEXT: PRINT #5, CHR$(0); CHR$(1);
  CASE 8                                'Make TGA
    TH$ = "0001010000000118000000004001C8000800"    'TGA Header
    FOR I = 1 TO LEN(TH$) STEP 2
      PRINT #5, CHR$(VAL("&H" + MID$(TH$, I, 2)));
    NEXT
    FOR A% = 0 TO 255: OUT 967, A%     'Grab and save the pallette
      R$ = CHR$(INP(969) * 4)
      G$ = CHR$(INP(969) * 4)
      B$ = CHR$(INP(969) * 4)
      PRINT #5, B$; G$; R$;
    NEXT
    FOR y% = 199 TO 0 STEP -1
      FOR x% = 0 TO 319
        PRINT #5, CHR$(RelPoint(Layer%, x%, y%));
      NEXT: RelLine Layer%, 0, y%, 319, y%, 0
    NEXT
  CASE 9                               'Make a GIF87a
    '==============================================================
    'Yep. This is Rich's * famous * code. You can find the original
    'source in the public domain ABC packets.  I condensed it here.
    '==============================================================
    DIM PF(7176), SF(7176), GC(7176), SHT(7) AS LONG
    x% = 1: y% = 0: cb% = 0
    c& = 0: mc% = 512: cs% = 9: nc% = 258
    FOR A = 0 TO 7: SHT(A) = 2 ^ A: NEXT
    HDR$ = "4749463837614001C800D70000"           'GIF Header
    FOR I = 1 TO LEN(HDR$) STEP 2
      PRINT #5, CHR$(VAL("&H" + MID$(HDR$, I, 2)));
    NEXT: OUT 967, 0
    FOR A = 1 TO 768                              'Save Pallette
      PRINT #5, CHR$((INP(969) * 4));
    NEXT: ID$ = "2C000000004001C8000708"          'GIF ID
    FOR I = 1 TO LEN(ID$) STEP 2
      PRINT #5, CHR$(VAL("&H" + MID$(ID$, I, 2)));
    NEXT
    FOR A% = 0 TO 7176
      PF(A%) = -1: SF(A%) = -1: GC(A%) = -1
    NEXT: A = 256: GOSUB PutGIF
    PF = RelPoint(Layer%, 0, 0): RelPset Layer%, 0, 0, 0           'Get First Pixel
    DO
      DO
        SF = RelPoint(Layer%, x%, y%): RelPset Layer%, x%, y%, 0
        x% = x% + 1: IF x% > 319 THEN : x% = 0: y% = y% + 1
        Idx% = ((PF * 256&) XOR SF) MOD 7177
        IF Idx% = 0 THEN os% = 1 ELSE os% = 7177 - Idx%
        DO
          IF GC(Idx%) = -1 THEN fd% = 0: EXIT DO
          IF PF(Idx%) = PF AND SF(Idx%) = SF THEN fd% = 1: EXIT DO
          Idx% = Idx% - os%: IF Idx% < 0 THEN Idx% = Idx% + 7177
        LOOP
        IF fd% = 1 THEN PF = GC(Idx%)
        IF y% > (VertPX% + 1) THEN
          A = PF: GOSUB PutGIF: A = 257: GOSUB PutGIF
          IF cb% <> 0 THEN A = 0: GOSUB PutGIF
          bl% = bl% - 1: IF bl% <= 0 THEN bl% = 255: PRINT #5, CHR$(255);
          PRINT #5, CHR$(0); : CLOSE #5: EXIT SUB
        END IF
      LOOP WHILE fd%
      A = PF: GOSUB PutGIF: PF(Idx%) = PF: SF(Idx%) = SF
      GC(Idx%) = nc%: PF = SF: nc% = nc% + 1
      IF nc% = mc% + 1 THEN
        mc% = mc% * 2
        IF cs% = 12 THEN
          A = 256: GOSUB PutGIF
          FOR A% = 0 TO 7176
            PF(A%) = -1: SF(A%) = -1: GC(A%) = -1
          NEXT: nc% = 258: cs% = 9: mc% = 512
        ELSE
          cs% = cs% + 1
        END IF
      END IF
    LOOP: EXIT SUB
  CASE 10                       'Make OS2 BMP Header
     A$ = "424D1AFD0000000000001A0300000C0000004001C80001000800"
     FOR I% = 1 TO LEN(A$) STEP 2
        PRINT #5, CHR$(VAL("&H" + MID$(A$, I%, 2)));
     NEXT
     FOR A% = 0 TO 255: OUT 967, A%
        R$ = CHR$(INP(969) * 4)           'Almost like BMP...
        G$ = CHR$(INP(969) * 4)
        B$ = CHR$(INP(969) * 4)
        PRINT #5, B$; G$; R$;             'No CHR$(0) at end.
     NEXT
     FOR y% = 199 TO 0 STEP -1
        FOR x% = 0 TO 319
           PRINT #5, CHR$(RelPoint(Layer%, x%, y%));
        NEXT: RelLine Layer%, 0, y%, 319, y%, 0
     NEXT
  CASE 11                        'RAS Sun Raster Image file
     A$ = "59A66A9500000140000000C8000000080000FA"
     A$ = A$ + "00000000010000000100000300"
     FOR I% = 1 TO LEN(A$) STEP 2
        PRINT #5, CHR$(VAL("&H" + MID$(A$, I%, 2)));
     NEXT
     FOR t% = 1 TO 3                   'Change RAW Pal$ data
        FOR U% = 1 TO LEN(Pal$) STEP 3
           PRINT #5, CHR$(ASC(MID$(Pal$, U%, 1)) * 4);
        NEXT
     NEXT
     GOSUB GetRawImage
  CASE 12                           'WMF 1st Header
    A$ = "D7CDC69A0000000000004001C800600000000000F9560100090000034881"
    A$ = A$ + "00000100227F000000000400000003010800050000000B02000000000500"
    A$ = A$ + "00000C022D15E12105020000F70000030001"
    FOR I = 1 TO LEN(A$) STEP 2
      PRINT #5, CHR$(VAL("&H" + MID$(A$, I, 2)));
    NEXT
    FOR A% = 0 TO 255: OUT 967, A%     'Grab and save the pallette
      R$ = CHR$(INP(969) * 4)          'Red
      G$ = CHR$(INP(969) * 4)          'Green
      B$ = CHR$(INP(969) * 4)          'Blue
      PRINT #5, B$; G$; R$; CHR$(0);   'Save'em all
    NEXT                           'WMF 2nd header
    A$ = "0400000034020000030000003500227F0000430F2000CC000000C8004001"
    A$ = A$ + "000000002D15E121000000002800000040010000C8000000010008000000"
    A$ = A$ + "000000FA000000000000000000000001000000010000"
    FOR I = 1 TO LEN(A$) STEP 2
      PRINT #5, CHR$(VAL("&H" + MID$(A$, I, 2)));
    NEXT
    FOR A% = 0 TO 255: OUT 967, A%     'Grab and save the pallette
      R$ = CHR$(INP(969) * 4)          'Red
      G$ = CHR$(INP(969) * 4)          'Green
      B$ = CHR$(INP(969) * 4)          'Blue
      PRINT #5, B$; G$; R$; CHR$(0);   'Save'em all
    NEXT
    FOR y% = 199 TO 0 STEP -1
      FOR x% = 0 TO 319
        PRINT #5, CHR$(RelPoint(Layer%, x%, y%));
      NEXT: RelLine Layer%, 0, y%, 319, y%, 0
    NEXT                    'WMF Tailer
    A$ = "030000000000"
    FOR I = 1 TO LEN(A$) STEP 2
      PRINT #5, CHR$(VAL("&H" + MID$(A$, I, 2)));
    NEXT
  CASE 13                 'TIFF Header #1
    A$ = "49492A00080000000F00FE00040001000000000000000001030001000000"
    A$ = A$ + "400100000101030001000000C80000000201030001000000080000000301"
    A$ = A$ + "030001000000010000000601030001000000030000001101040001000000"
    A$ = A$ + "D20700001501030001000000010000001601030001000000C80000001701"
    A$ = A$ + "04000100000000FA00001A01050001000000C20000001B01050001000000"
    A$ = A$ + "CA0000002801030001000000020000004001030000030000D20000004986"
    A$ = A$ + "010000010000D20600000000000080FC0A001027000080FC0A0010270000"
    A$ = A$ + ""
    FOR I% = 1 TO LEN(A$) STEP 2
      PRINT #5, CHR$(VAL("&H" + MID$(A$, I%, 2)));
    NEXT                                       'Raw (*4)
    OUT 967, 0: Pal$ = ""
    FOR c% = 1 TO 768                       'Grab pallette
      Pal$ = Pal$ + CHR$(INP(969) * 4)        'Hold it as Pal$
    NEXT
    FOR t% = 1 TO LEN(Pal$) STEP 3
      A$ = MID$(Pal$, t%, 2): B$ = B$ + A$
    NEXT: PRINT #5, B$; B$; B$;                 'TIFF header #2
    A$ = "3842494D03ED000000000010004800000001000100480000000100013842"
    A$ = A$ + "494D03F300000000000700000000000000003842494D03F5000000000048"
    A$ = A$ + "002F66660001006C66660006000000000000002F6666000100A1999A0006"
    A$ = A$ + "000000000000003200000001005A00000006000000000000003500000001"
    A$ = A$ + "002D000000060000000000003842494D03F80000000000700000FFFFFFFF"
    A$ = A$ + "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF03E800000000FFFFFFFFFFFF"
    A$ = A$ + "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF03E800000000FFFFFFFFFFFFFFFF"
    A$ = A$ + "FFFFFFFFFFFFFFFFFFFFFFFFFFFF03E800000000FFFFFFFFFFFFFFFFFFFF"
    A$ = A$ + "FFFFFFFFFFFFFFFFFFFFFFFF03E80000"
    FOR I% = 1 TO LEN(A$) STEP 2
      PRINT #5, CHR$(VAL("&H" + MID$(A$, I%, 2)));
    NEXT
    GOSUB GetRawImage
  CASE 14                          'COM file, write COM data
    A$ = "B81300CD10B81210BB0000B90001BA2E01CD10FCBE2E04"
    A$ = A$ + "2BFFB800A08EC0B9007DF3A5B400CD16B80300CD10CD20"
    FOR I% = 1 TO LEN(A$) STEP 2
      PRINT #5, CHR$(VAL("&H" + MID$(A$, I%, 2)));
    NEXT
    PRINT #5, Pal$;
    GOSUB GetRawImage
END SELECT

CLOSE #5

EXIT SUB


'=============================================================
'Several ImageType% use this routine.

GetRawImage:
  FOR y% = 0 TO 199              'Save Screen, pixel by pixel
    FOR x% = 0 TO 319
        PRINT #5, CHR$(RelPoint(Layer%, x%, y%)); 'Save the pixel
    NEXT ': RelLine Layer%, 0, y%, 319, y%, 0
  NEXT
RETURN

'==============================================================
'This is for the GIF routine
PutGIF:
  c& = c& + A * SHT(cb%): cb% = cb% + cs%
  DO WHILE cb% > 7: bl% = bl% - 1
     IF bl% <= 0 THEN bl% = 255: PRINT #5, CHR$(255);
     PRINT #5, CHR$(c& AND 255);
     c& = c& \ 256: cb% = cb% - 8
  LOOP
RETURN

END SUB

