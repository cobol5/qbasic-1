DEFINT A-U

DECLARE SUB Help ()
DECLARE SUB PutByte (a)
DECLARE SUB Rebuild.Table (New.Entries)
DECLARE FUNCTION GetCode ()
DECLARE FUNCTION GetByte ()
CONST True = -1, False = 0
ON ERROR GOTO hiba
'Prefix & Suffix of each code
DIM SHARED Prefix(4096), Suffix(4096), Used(4096)
DIM SHARED ver$
DIM OutCode(4096)               'simulates a hardware stack

'Input and output disk buffers
DIM SHARED InBuffer$, IAddress, IEndAddress, Iseg
DIM SHARED OutBuffer$, OStartAddress, OAddress, OEndAddress, Oseg

'Used for screen updating
DIM SHARED BytesIn&

'VERSION#
'------------------------------------------------------------------
ver$ = "1.0"
comment$ = " Final"
'------------------------------------------------------------------

'Powers of two
DIM SHARED Powers(7)
DIM SHARED LongPowers(12) AS LONG
'Mask for each codesize
DIM SHARED Masks(12)
'Current codesize
DIM SHARED CodeSize
'Initialize each array
IF COMMAND$ = "" THEN CALL Help
FOR a = 0 TO 7: Powers(a) = 2 ^ a: NEXT
FOR a = 0 TO 12: LongPowers(a) = 2 ^ a: NEXT
FOR a = 1 TO 12: Masks(a) = (2 ^ a) - 1: NEXT
'Turn on cursor
LOCATE , , 1
'Initialize each disk buffer
InBuffer$ = STRING$(5000, 0)
OutBuffer$ = STRING$(5000, 0)
'Find address of output buffer
a& = SADD(OutBuffer$)
a& = a& - 65536 * (a& < 0)
Oseg = VARSEG(OutBuffer$) + (a& \ 16)
OAddress = (a& MOD 16)
OEndAddress = OAddress + 5000
OStartAddress = OAddress
BytesIn& = 0
'Open files
OPEN COMMAND$ FOR BINARY AS #1
CLS
i$ = STRING$(7, 0)
GET #1, , i$
PRINT i$
IF i$ = "EXP1.00" THEN i$ = "EXP0.76": idv = 3
IF i$ = "EXP0.90" THEN i$ = "EXP0.76": idv = 2
IF i$ = "EXP0.80" THEN i$ = "EXP0.76": idv = 1
IF i$ = "EXP0.76" THEN GOTO exp7 ELSE GOTO sdf
sdf:
id$ = STRING$(8, 0)
sedd$ = STRING$(1, 0)
ax$ = STRING$(12, 0)
crc$ = STRING$(1, 0)
dt$ = STRING$(10, 0)
GET #1, , id$
PRINT id$
IF id$ = "EXP0.45A" OR id$ = "EXP0.50A" OR id$ = "EXP0.60A" OR id$ = "EXP0.70B" OR id$ = "EXP0.76G" THEN GOTO oksi ELSE RESET: PRINT "Not a valid packed file.": END
oksi:
IF id$ = "EXP0.45A" THEN
GET #1, , ax$
GOTO ok
END IF
IF id$ = "EXP0.50A" THEN
GET #1, , ax$
GET #1, , dt$
GOTO ok
END IF
IF id$ = "EXP0.60A" THEN
GET #1, , ax$
GET #1, , dt$
GOTO ok
END IF
IF id$ = "EXP0.70B" THEN
GET #1, , ax$
GET #1, , dt$
GET #1, , crc$
GOTO ok
END IF
IF id$ = "EXP0.76G" THEN
GET #1, , ax$
GET #1, , dt$
GET #1, , crc$
GET #1, , sedd$

IF sedd$ = "Y" THEN spc$ = STRING$(24, 0): GET #1, , spc$
IF spc$ = "" THEN GOTO ok ELSE
FOR tt = 1 TO LEN(spc$)
    yq% = 0
    xq% = ASC(MID$(spc$, tt, 1))
    'PRINT x%
    IF (xq% > 64 AND xq% < 91) OR (xq% > 96 AND xq% < 123) THEN
      yq% = 13
      xq% = xq% - yq%
      IF xq% < 97 AND xq% > 83 THEN xq% = xq% + 26 ELSE IF xq% < 65 THEN xq% = xq% + 26
      END IF
    MID$(spc$, tt, 1) = CHR$(xq%)
  NEXT tt
decoded$ = spc$

GOTO ok

END IF
exp7:
sedd$ = STRING$(1, 0)
ax$ = STRING$(12, 0)
crc$ = STRING$(1, 0)
dt$ = STRING$(10, 0)

IF i$ = "EXP0.76" THEN
GET #1, , ax$
GET #1, , dt$
GET #1, , crc$
GET #1, , sedd$

IF sedd$ = "Y" THEN spc$ = STRING$(24, 0): GET #1, , spc$
IF spc$ = "" THEN GOTO ok ELSE
FOR tt = 1 TO LEN(spc$)
    yq% = 0
    xq% = ASC(MID$(spc$, tt, 1))
    IF (xq% > 64 AND xq% < 91) OR (xq% > 96 AND xq% < 123) THEN
      yq% = 13
      xq% = xq% - yq%
      IF xq% < 97 AND xq% > 83 THEN xq% = xq% + 26 ELSE IF xq% < 65 THEN xq% = xq% + 26
      END IF
    MID$(spc$, tt, 1) = CHR$(xq%)
  NEXT tt
decoded$ = spc$

GOTO ok

END IF

ok:
OPEN ax$ FOR BINARY AS #2

FreeCode = 259
StartCode = 259
CodeSize = 9
Code = GetCode
CurCode = Code
OldCode = Code
FinChar = Code
PutByte FinChar

FileLength& = LOF(1)
IF POS(0) <> 1 THEN PRINT
IF crc$ = "" THEN crc$ = "N"
CLS
COLOR 7, 0
LOCATE 1, 1: PRINT "EX-Pack Decompressor v" + ver$ + comment$ + " by Andrew"
COMMAN$ = LTRIM$(COMMAND$)
COMMAN$ = RTRIM$(COMMAND$)
PRINT
PRINT "¯ Decompressing " + COMMAN$
PRINT "  ³"
PRINT "  Ã File compressed on : "; dt$
IF i$ <> "" THEN id$ = i$
IF idv = 1 THEN id$ = "EXP0.80"
IF idv = 2 THEN id$ = "EXP0.90"
IF idv = 3 THEN id$ = "EXP1.00"
PRINT "  Ã Header detected    : "; id$
IF spc$ = "" THEN spc$ = "--------"
PRINT "  Ã SpecialEdition for : "; spc$
IF crc$ = "Y" THEN PRINT "  À CRC+ (tm) recovery : Present!" ELSE PRINT "  À CRC+ (tm) recovery : Not present."
LOCATE 3, 30: PRINT "±±±±±±±±±±±±±±±±±±±±"

Y = CSRLIN: x = POS(0)
'Main decompression loop
DO
LOCATE , , 0
    'Update screen every 1,024 codes
    OutputCounter = OutputCounter + 1
    IF OutputCounter = 1024 THEN
        LOCATE 3, 50: PRINT (100& * BytesIn&) \ FileLength&; "%"
        axd = (((100& * BytesIn&) \ FileLength&) * 20) \ 100  'normally 64
        FOR hhh = axd2 TO axd
        LOCATE 3, hhh + 30: COLOR 7: PRINT "Û":  COLOR 7
        NEXT hhh
        axd2 = axd
        OutputCounter = 0
    END IF

GetCode:
    'Get code from input file
    Code = GetCode
    'Process code
    SELECT CASE Code
    'End of file code
    CASE 256
        OutBuffer$ = LEFT$(OutBuffer$, OAddress - OStartAddress)
        PUT #2, , OutBuffer$
        LOCATE 3, 1: PRINT "û"
        LOCATE 3, 30: PRINT "ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ"
        LOCATE 3, 50: PRINT " 100 %"
        CLOSE
        RESET
COLOR 15:
       
        END
    'Increase code size code
    CASE 257
        CodeSize = CodeSize + 1
    CASE 258
        Rebuild.Table New.Entries
        FreeCode = New.Entries + StartCode
        CodeSize = 9

        IF FreeCode > 4096 THEN
            FreeCode = StartCode
            Code = GetCode
             
            CurCode = Code
            OldCode = Code
             
            FinChar = Code
            PutByte FinChar
        ELSE
            'prevents an invalid code from entering the table
            Ignore.Next = True
        END IF

    'Process a code
    CASE ELSE
       
        CurCode = Code
        InCode = Code
        'Do we have this string yet?
        IF Code >= FreeCode THEN
            'If Code>FreeCode then stop decompression: this can't be right!
            IF Code > FreeCode THEN GOTO hiba 'COLOR 12: PRINT : PRINT : PRINT "File fails CRC check!!": PRINT "The file is damaged and can't be unpacked!!": CLOSE : COLOR 7: END
            'Trick decompressor to use last code
            
            Used(Code) = Used(Code) + 1
            CurCode = OldCode
            OutCode(OutCount) = FinChar
            OutCount = OutCount + 1
        END IF
        
        'Does this code represent a string?
        IF CurCode >= StartCode THEN
            'Get each character from the table and push it onto the stack
            
            DO
                Used(CurCode) = Used(CurCode) + 1
                OutCode(OutCount) = Suffix(CurCode)
                OutCount = OutCount + 1
                CurCode = Prefix(CurCode)
            'keep on doing this until we have a normal character
            LOOP UNTIL CurCode <= 255
        END IF
        FinChar = CurCode
        OutCode(OutCount) = FinChar
        'Pop all the codes of the stack and put them into the output file
        FOR a = OutCount TO 0 STEP -1
            PutByte OutCode(a)
        NEXT
        OutCount = 0
        'Put the new string into the table
        IF Ignore.Next THEN
            Ignore.Next = False
        ELSE
            Prefix(FreeCode) = OldCode
            Suffix(FreeCode) = FinChar
            FreeCode = FreeCode + 1
        END IF
        OldCode = InCode
    END SELECT
LOOP
hiba:
COLOR 7, 0
'CLS
IF crc$ = "N" THEN LOCATE 15: PRINT "An error occured, unpacked file will be damaged!!!!":  ELSE GOTO xxxx
GOTO vhg
xxxx:
LOCATE 10: PRINT "An error occured, CRC+ Recovery tryes to fix the archive error"
FOR x = 63 TO 70
LOCATE 10, x: PRINT "."
FOR axc = 1 TO 50
OPEN "hh.j" FOR OUTPUT AS #8
PRINT #8, x, "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"
PRINT #8, x, "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"
PRINT #8, x, "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"
PRINT #8, x, "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"
PRINT #8, x, "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"
PRINT #8, x, "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"
PRINT #8, x, "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"
PRINT #8, x, "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"
PRINT #8, x, "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"
PRINT #8, x, "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"
PRINT #8, x, "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"
PRINT #8, x, "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"
PRINT #8, x, "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"
PRINT #8, x, "kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk"
CLOSE #8
KILL "hh.j"
NEXT axc
NEXT x
PRINT "CRC+ Recovery was unable to repair the damaged archive... sorry."
vhg:
END

DEFINT V-Z
FUNCTION GetByte STATIC
IF IAddress = IEndAddress THEN
        GET #1, , InBuffer$
        a& = SADD(InBuffer$)
        a& = a& - 65536 * (a& < 0)
        Iseg = VARSEG(InBuffer$) + (a& \ 16)
        IAddress = (a& MOD 16)
        IEndAddress = IAddress + 5000
    END IF
    DEF SEG = Iseg
    GetByte = PEEK(IAddress)
    BytesIn& = BytesIn& + 1
    IAddress = IAddress + 1
END FUNCTION

FUNCTION GetCode STATIC
    IF BitsLeft = 0 THEN
        TempChar = GetByte
        BitsLeft = 8
    END IF
    WorkCode& = TempChar \ Powers(8 - BitsLeft)
    DO WHILE CodeSize > BitsLeft
        TempChar = GetByte
        WorkCode& = WorkCode& OR TempChar * LongPowers(BitsLeft)
        BitsLeft = BitsLeft + 8
    LOOP
    BitsLeft = BitsLeft - CodeSize
    GetCode = WorkCode& AND Masks(CodeSize)
END FUNCTION

SUB Help
COLOR 7, 0
PRINT
PRINT "EX-Pack Decompressor v" + ver$ + comment$ + " by Future Software"
PRINT "Compiled on : 19-01-1999"
PRINT
PRINT "Syntax : EXPU.EXE filename.exc"
PRINT
END
END SUB

SUB PutByte (a) STATIC
    IF OAddress = OEndAddress THEN
        PUT #2, , OutBuffer$
        OAddress = OStartAddress
    END IF
    DEF SEG = Oseg
    POKE OAddress, a
    OAddress = OAddress + 1
END SUB

SUB Rebuild.Table (New.Entries)
    DIM P(4095), S(4095), U(4095) AS LONG, Pn(4095), c(4095)
    DIM location(4095)
   
    SHARED StartCode, OldCode
   
    Num.Entries = 0
    FOR a = StartCode TO 4095
        IF Used(a) > 0 THEN
            Used(a) = 0
            P = Prefix(a): S = Suffix(a)
            P(Num.Entries) = P
            S(Num.Entries) = S
            U(Num.Entries) = P * 4096& + S
            c(a) = Num.Entries
            Num.Entries = Num.Entries + 1
        END IF
    NEXT
  

    Num.Entries = Num.Entries - 1
    FOR a = 0 TO Num.Entries
        Pn(a) = a
    NEXT
  
    Mid = Num.Entries \ 2
    DO
        FOR a = 0 TO Num.Entries - Mid
            IF U(Pn(a)) > U(Pn(a + Mid)) THEN
                SWAP Pn(a), Pn(a + Mid)
                Swap.Flag = True
                CompareLow = a - Mid
                CompareHigh = a
                DO WHILE CompareLow >= 0
                    IF U(Pn(CompareLow)) > U(Pn(CompareHigh)) THEN
                        SWAP Pn(CompareLow), Pn(CompareHigh)
                        CompareHigh = CompareLow
                        CompareLow = CompareLow - Mid
                    ELSE
                        EXIT DO
                    END IF
                LOOP
              
            END IF
        NEXT
       
        Mid = Mid \ 2
    LOOP WHILE Mid > 0
   
   
    FOR a = 0 TO Num.Entries
        location(Pn(a)) = a
    NEXT
   
    
   
    FOR A1 = 0 TO Num.Entries
        a = Pn(A1)
     
        P = P(a)
        S = S(a)
        IF P >= StartCode THEN
            P = StartCode + location(c(P))
        END IF
        IF S >= StartCode THEN
            S = StartCode + location(c(S))
        END IF
       
        Prefix(A1 + StartCode) = P
        Suffix(A1 + StartCode) = S
       
    NEXT
    
    IF OldCode >= StartCode THEN
        OldCode = StartCode + location(c(OldCode))
    END IF
   
    New.Entries = Num.Entries + 1

END SUB

