'Robert Claypool
'Not All Opcodes Have been Translated
DEFINT A-Z


TYPE ExeHeader
     Signature  AS STRING * 2   '2
     Length     AS INTEGER      '4
     Size       AS INTEGER      '6
     Table      AS INTEGER      '8 Number of relocation entries
     CParHeader AS INTEGER      'A  header size in paragraphs
     MinPara    AS INTEGER      'C  Min para required in addition to exe size
     MaxPara    AS INTEGER      'E
     SSInit     AS INTEGER      '10
     SPInit     AS INTEGER      '12
     Checksum   AS INTEGER      '14
     IPReg      AS INTEGER      '16
     CSReg      AS INTEGER      '18
     RelocOffset AS INTEGER     '1A
     Overlay     AS INTEGER     '  Overlay number (normally 0000h = main program)
END TYPE

TYPE WinExeHdr
    Signature    AS STRING * 2'0
    LinkVer      AS STRING * 1'Byte'2
    LinkRev      AS STRING * 1'Byte'3
    EntryTableOffset AS INTEGER'4
    EntryTableLength AS INTEGER'6
    CRC          AS LONG'8
    FileTypeTags AS INTEGER'C
    ADSSegNum     AS INTEGER'E
    DynamHeapSize AS INTEGER'10
    DSAddedStack  AS INTEGER'12
    IP  AS INTEGER'14
    CS  AS INTEGER'16
    SP  AS INTEGER'18
    SS  AS INTEGER'1A
    SegTableEntryTotal AS INTEGER'1C
    ModRefTable  AS INTEGER'1E
    NonResNameTableBytesTotal AS INTEGER'30
    SegTableOffset AS INTEGER'32
    ResTableOffset AS INTEGER'34
    ResNameTableOffset AS INTEGER'36
    ModRefTableOffset AS INTEGER'38
    ImpNameTableOffset AS INTEGER'40
    NonResNameTableLoc        AS LONG'42
END TYPE

TYPE PossibleLocations
    PositionInFile AS LONG
    StartOfHeader  AS LONG
    EndOfHeader    AS LONG
END TYPE

DECLARE FUNCTION SignedByte% (Value%)
DECLARE FUNCTION HEXSignedByte$ (Value%)

DECLARE FUNCTION Map1D22DX% (Value1D%, TotalX%)
DECLARE FUNCTION Map1D22DY% (Value1D%, X%, TotalX%)

DECLARE FUNCTION LongHex$ (Value%, Length%)
DECLARE FUNCTION ExeFileTypeDetermination% (EH AS STRING, NH AS STRING)
DECLARE FUNCTION ExtStrip$ (Filename$)
DECLARE FUNCTION ExtFix$ (Filename$, Extension$)

DECLARE SUB Trap1 (Inst$, Inst%, InstBase%)

DIM FileStart AS ExeHeader, InterpretStart AS LONG, AWinExeHdr  AS WinExeHdr
DIM BaseData AS PossibleLocations, CurrData AS PossibleLocations

DIM SHARED Byte AS STRING * 1, Byte2 AS STRING * 1
DIM SHARED IncDec$(0 TO 3), RegSet$(0 TO 2, 0 TO 7), Float$(0 TO 7)

IncDec$(0) = "INC"
IncDec$(1) = "DEC"
IncDec$(2) = "PUSH"
IncDec$(3) = "POP"
FOR j = 0 TO 2
   FOR i = 0 TO 7
       READ RegSet$(j, i)
   NEXT i
NEXT j

PRINT "Filename to open:"
INPUT A$

OPEN "b", 1, ExtFix(A$, ".exe")

OPEN "O", 2, ExtFix(ExtStrip(A$), ".asm")

SEEK 1, 1
GET 1, , FileStart

'Get if windows prog
SEEK 1, &H3C + 1
GET 1, , WinHdrLoc&
SEEK 1, WinHdrLoc& + 1
GET 1, , AWinExeHdr

FIleType = ExeFileTypeDetermination(FileStart.Signature, AWinExeHdr.Signature)
IF FIleType > 1 THEN GOTO WinFileOptions
IF FIleType = 0 THEN GOTO ComFile
'End get
OrdinaryEXE:
InterpretStart = VAL("&H" + HEX$(FileStart.CSReg) + "0&") + VAL("&H" + HEX$(FileStart.IPReg) + "&") + (FileStart.CParHeader * 16&)
ComFile:
SEEK 1, InterpretStart + 1
DO
  GET 1, , Byte
  SELECT CASE ASC(Byte)
         CASE 0, 1, 2, 3
              ByteOri% = ASC(Byte)
              CALL Trap1("ADD ", ByteOri, 0)
         CASE 4
              GET 1, , Byte
              PRINT #2, "MOV   AL, " + HEX$(ASC(Byte))
         CASE 5
              GET 1, , Integ%
              PRINT #2, "MOV   AX, " + HEX$(Integ)
        CASE 6
             PRINT #2, "PUSH ES"
        CASE 7
             PRINT #2, "POP  ES"
        CASE 8, 9, 10, 11
             ByteOri% = ASC(Byte)
             CALL Trap1("OR  ", ByteOri, 8)
        CASE 12
             GET 1, , Byte
             PRINT #2, "OR  AL, " + HEX$(ASC(Byte))
        CASE 13
             GET 1, , Integ%
             PRINT #2, "OR   AX, " + HEX$(Integ)
        CASE 14
             PRINT #2, "PUSH CS"
        CASE 15
             GET 1, , Byte
             SELECT CASE ASC(Byte)
                    CASE 0
                         GET 1, , Byte
                    CASE ELSE
             END SELECT
        CASE 22
             PRINT #2, "PUSH SS"
        CASE &H1F
             PRINT #2, "POP DS"
        CASE &H2B
              GET 1, , Byte
              SELECT CASE ASC(Byte)
                     CASE &HF7
                          PRINT #2, "SUB SI,DI"
                     CASE ELSE
                     PRINT #2, "2B " + HEX$(ASC(Byte))
              END SELECT
        CASE &H33
             GET 1, , Byte
             SELECT CASE ASC(Byte)
                    CASE &HC0
                         PRINT #2, "XOR AX, AX"
                    CASE &HED
                         PRINT #2, "XOR BP, BP"
                    CASE &HFF
                         PRINT #2, "XOR DI, DI"
                    CASE ELSE
                         PRINT #2, "33 " + HEX$(ASC(Byte))
             END SELECT
        CASE &H36
             GET 1, , Byte
             SELECT CASE ASC(Byte)
                    CASE &HC7
                         GET 1, , Byte
                         SELECT CASE ASC(Byte)
                                CASE &H6
                                     GET 1, , Integ%
                                     GET 1, , Integ2%
                                     PRINT #2, "MOV   SS:[" + HEX$(Integ%) + "], " + HEX$(Integ2%)
                                CASE ELSE
                                     PRINT #2, "36 C7 " + HEX$(ASC(Byte))
                         END SELECT
                    CASE ELSE
                         PRINT #2, "36 " + HEX$(ASC(Byte))
             END SELECT
        CASE &H3C
             GET 1, , Byte
             PRINT #2, "CMP  AL, " + HEX$(ASC(Byte))
        CASE &H3D
             GET 1, , Integ%
             PRINT #2, "CMP  AX, " + LongHex(Integ, 4)
        'CASE 3E: Too Long and torturous to try to implement currently
        CASE &H3F
             PRINT #2, "AAS"
        CASE &H40 TO &H5F
             Temp% = Map1D22DX(ASC(Byte) - &H40, 8)
             PRINT #2, IncDec$(Map1D22DY(ASC(Byte) - &H40, Temp%, 8)) + "  " + RegSet$(1, Temp%)
        'CASE &H65 Not Known as a command
        CASE &H73
             GET 1, , Byte
             PRINT #2, "JNB  " + HEX$(ASC(Byte)) + ";Relative jump"
         CASE &H75
             GET 1, , Byte
             PRINT #2, "JNZ  " + HEX$(ASC(Byte)) + ";Relative jump"
        CASE &H77
             GET 1, , Byte
             PRINT #2, "JA   " + HEX$(ASC(Byte)) + ";Relative jump"
        CASE &H81
             GET 1, , Byte
             SELECT CASE ASC(Byte)
                    CASE &HFE
                         GET 1, , Integ%
                         PRINT #2, "CMP SI, " + HEX$(Integ%)
                    CASE &HC4
                         GET 1, , Integ%
                         PRINT #2, "ADD SP, " + HEX$(Integ%)
                    CASE ELSE
                    PRINT #2, "81 " + HEX$(ASC(Byte))
             END SELECT
        CASE &H83
             GET 1, , Byte
             SELECT CASE ASC(Byte)
                    CASE &HC3
                         GET 1, , Byte
                         PRINT #2, "ADD BX, " + HEX$(ASC(Byte)) + ";Signed Byte"
                    CASE ELSE
                         PRINT #2, "83 " + HEX$(ASC(Byte))
             END SELECT
        CASE &H89
             GET 1, , Byte
             SELECT CASE ASC(Byte)
                    CASE &H3E
                         GET 1, , Integ%
                         PRINT #2, "MOV  [" + HEX$(Integ%) + "], DI"
                    CASE ELSE
                         PRINT #2, "89 " + HEX$(ASC(Byte))
             END SELECT
        CASE &H8B
             GET 1, , Byte
             SELECT CASE ASC(Byte)
                    CASE &H36
                         GET 1, , Integ%
                         PRINT #2, "MOV SI, [" + HEX$(Integ%) + "]"
                    CASE &HC8
                         PRINT #2, "MOV CX, AX"
                    CASE &HE3
                         PRINT #2, "MOV SP, BX"
                    CASE ELSE

                         PRINT #2, "8B " + HEX$(ASC(Byte))
             END SELECT
        CASE &H8C
             GET 1, , Byte
             SELECT CASE ASC(Byte)
                    CASE &HDA
                         PRINT #2, "MOV  DX, DS"
                    CASE &HC8
                         PRINT #2, "MOV  AX, CS"
                    CASE ELSE
                         PRINT #2, "8C " + HEX$(ASC(Byte))
             END SELECT
        CASE &H8E
             GET 1, , Byte
             SELECT CASE ASC(Byte)
                    CASE &HD7
                        PRINT #2, "MOV SS, DI"
                    CASE ELSE
                        PRINT #2, "8E " + HEX$(ASC(Byte))
             END SELECT
        CASE &H90
             PRINT #2, "NOP"
        CASE &H98
             PRINT #2, "CBW"
        CASE &HA0
             GET 1, , Integ%
             PRINT #2, "MOV  AL,  [" + LongHex$(Integ%, 4) + "]"
        CASE &HA1
             GET 1, , Integ%
             PRINT #2, "MOV  AX, [" + LongHex$(Integ%, 4) + "]"
        CASE &HA2
             GET 1, , Integ%
             PRINT #2, "MOV  [" + LongHex$(Integ%, 4) + "], AL"
        CASE &HA3
             GET 1, , Integ%
             PRINT #2, "MOV  [" + LongHex$(Integ%, 4) + "], AX"
        CASE &HB4
             GET 1, , Byte
             PRINT #2, "MOV  AH," + HEX$(ASC(Byte))
             AX% = ASC("&H" + HEX$(ASC(Byte)) + LongHex(AX%, 2))
        CASE &HB8
             GET 1, , Integ%
             PRINT #2, "MOV  AX, " + HEX$(Integ%)
             AX% = Integ%
        CASE &HB9
             GET 1, , Integ%
             PRINT #2, "MOV  CX, " + HEX$(Integ%)
             AX% = Integ%
        CASE &HBA
             GET 1, , Integ%
             PRINT #2, "MOV  DX, " + HEX$(Integ%)
             DX% = Integ%
        CASE &HBB
             GET 1, , Integ%
             PRINT #2, "MOV  BX, " + HEX$(Integ%)
             BX% = Integ%
        CASE &HBC
             GET 1, , Integ%
             PRINT #2, "MOV  SP, " + HEX$(Integ%)
             DX% = Integ%
        CASE &HBD
             GET 1, , Integ%
             PRINT #2, "MOV  BP, " + HEX$(Integ%)
             DX% = Integ%
        CASE &HBE
            GET 1, , Integ%
            PRINT #2, "MOV  SI, " + HEX$(Integ%)
            SI% = Integ%
        CASE &HBF
            GET 1, , Integ%
            PRINT #2, "MOV  DI, " + HEX$(Integ%)
            DI% = Integ%
        CASE &HC7
             GET 1, , Byte
             SELECT CASE ASC(Byte)
                    CASE &H6
                        GET 1, , Integ%
                        PRINT #2, "MOV   [" + HEX$(Integ%);
                        GET 1, , Integ%
                        PRINT #2, "], " + HEX$(Integ%)
                    CASE ELSE
                        PRINT #2, "C7 " + HEX$(ASC(Byte))
             END SELECT
        CASE &HCD
             GET 1, , Byte
             PRINT #2, "INT " + HEX$(ASC(Byte))
        CASE &HCE
             PRINT #2, "INTO"
        CASE &HD1
             GET 1, , Byte
             SELECT CASE ASC(Byte)
                    CASE &HEB
                         PRINT #2, "SHR   BX, 1"
                    CASE ELSE
             END SELECT
        CASE &HE9
             GET 1, , Integ%
             PRINT #2, "JMP  " + LongHex(Integ%, 4)
        CASE &HF2
             GET 1, , Byte
             SELECT CASE ASC(Byte)
                    CASE &HAE
                         PRINT #2, "REPNE  SCASB"
                     CASE ELSE
                     PRINT #2, "F2 " + HEX$(ASC(Byte))
             END SELECT
        CASE &HF3
             GET 1, , Byte
             SELECT CASE ASC(Byte)
                    CASE &HA4
                         PRINT #2, "REP  MOVSB"
                    CASE &HA6
                         PRINT #2, "REPZ CMPSB"
                    CASE ELSE
                    PRINT #2, "F3 " + HEX$(ASC(Byte))
             END SELECT
        CASE &HFA
             PRINT #2, "CLI"
        CASE &HFB
             PRINT #2, "STI"
        CASE &HFC
             PRINT #2, "CLD"
        CASE ELSE
            PRINT #2, HEX$(ASC(Byte))
 END SELECT
LOOP UNTIL EOF(1)
END
WinFileOptions:
    DO
      PRINT "Windows .EXE File Options"
      PRINT "1. Get Module Definition String"
      PRINT "2. Disassemble as if ordinary .EXE File"
      PRINT "3. Exit Program"
FailedReturn1:
      DO
        Key$ = INKEY$
      LOOP UNTIL Key$ <> ""
      SELECT CASE Key$
        CASE "1"
             GOSUB MDS
        CASE "2"
             GOTO OrdinaryEXE
        CASE "3"
             END
        CASE ELSE
             GOTO FailedReturn1
      END SELECT
    LOOP
MDS:
  SEEK #1, AWinExeHdr.NonResNameTableLoc + 1
  PRINT HEX$(AWinExeHdr.NonResNameTableLoc)
  Temp% = 0
  PRINT AWinExeHdr.NonResNameTableBytesTotal
  DO
    GET #1, , Byte
    A$ = INPUT$(ASC(Byte), 1)
    PRINT ASC(Byte)
    PRINT A$
    Temp% = Temp% + 1 + LEN(A$)
    IF Temp% >= AWinExeHdr.NonResNameTableBytesTotal THEN EXIT DO
  LOOP
RETURN
DATA AL, CL, DL, BL, AH, CH, DH, BH
DATA AX,CX,DX,BX,SP,BP,SI,DI
DATA BX + SI,BX + DI,BP + SI,BP + DI, SI, DI, BP, BX
DATA SLDT, STR,LLDT, LTR, VERR, VERW, 0F 00 30

FUNCTION ExeFileTypeDetermination (EH AS STRING, NH AS STRING)
    Temp% = 0
    IF EH = "MZ" THEN Temp% = Temp% OR 1
    IF NH = "NE" THEN Temp% = Temp% OR 2
    IF NH = "PE" THEN Temp% = Temp% OR 4
    ExeFileTypeDetermination = Temp%
END FUNCTION

FUNCTION ExtFix$ (Filename$, Extension$)
    IF INSTR(Filename$, ".") = 0 THEN
       ExtFix$ = Filename$ + Extension$
    ELSE
       ExtFix$ = Filename$
    END IF
END FUNCTION

FUNCTION ExtStrip$ (Filename$)
    Temp% = INSTR(Filename$, ".")
    IF Temp% = 0 THEN
       ExtStrip$ = Filename$
    ELSE
      ExtStrip$ = LEFT$(Filename$, Temp% - 1)
    END IF
END FUNCTION

FUNCTION HEXSignedByte$ (Value%)
    IF Value AND &H80 THEN
       HEXSignedByte$ = "- " + HEX$(256 - Value%)
    ELSE
       HEXSignedByte$ = "+ " + HEX$(Value%)
    END IF
END FUNCTION

FUNCTION LongHex$ (Value%, Length%)
    TString$ = HEX$(Value%)
    Temp% = LEN(TString$)
    SELECT CASE Temp%
           CASE 0
           LongHex$ = STRING$(Length%, "0")
           CASE IS >= Length%
           LongHex$ = RIGHT$(TString$, 3)
           CASE IS < Length%
                LongHex$ = STRING$(Length - Temp%, "0") + TString$
           CASE ELSE
           PRINT "Error In Subroutine LongHex, Value="; Value%; "Length="; Length%
    END SELECT
END FUNCTION

FUNCTION Map1D22DX (Value1D, TotalX)
    Map1D22DX = Value1D MOD TotalX
END FUNCTION

FUNCTION Map1D22DY (Value1D, X, TotalX)
    Map1D22DY = (Value1D - X) / TotalX
END FUNCTION

SUB NEHdrNameValue (DStr AS WinExeHdr)
    PRINT "Signature Word:" + DStr.Signature
    PRINT "Linker Version:"; ASC(DStr.LinkVer); "."; ASC(DStr.LinkRev)
    PRINT "Entry Table Offset:"; DStr.EntryTableOffset

END SUB

FUNCTION SignedByte% (Value%)
    IF Value AND &H80 THEN
       SignedByte = 256 - Value%
    ELSE
       SignedByte = Value%
    END IF
END FUNCTION

SUB Trap1 (Inst$, InstVal%, InstBase%)
    RegByte% = (InstVal% - InstBase%) MOD 2
    GET 1, , Byte
    ByteVal% = ASC(Byte)
    SELECT CASE ByteVal%
           CASE 0 TO 5, 7 TO 13, 15 TO 21, 23 TO 29, 31 TO 37, 39 TO 45, 47 TO 53, 55 TO 61, 63
                Operand1$ = "[" + RegSet$(2, ByteVal% MOD 8) + "]"
                Operand2$ = RegSet$(RegByte%, Map1D22DY(ByteVal%, ByteVal% MOD 8, 8))
           CASE 6, 14, 22, 30, 38, 46, 54, 62
                'These Replace what would have been a [BP] Instruction
                GET 1, , Integ%
                Operand1$ = "[" + HEX$(Integ%) + "]"
                Operand2$ = RegSet$(RegByte%, Map1D22DY(ByteVal%, ByteVal% MOD 8, 8))
           CASE 64 TO 127'The whole thing all over except with BP and a Byte
                GET 1, , Byte2
                Operand1$ = "[" + RegSet$(2, ByteVal% MOD 8) + HEXSignedByte(ASC(Byte2)) + "]"
                Operand2$ = RegSet$(RegByte%, Map1D22DY(ByteVal%, ByteVal% MOD 8, 8) - 8)
           CASE 128 TO 191 'The whole thing all over except with BP and an Integer
                GET 1, , Integ%
                Operand1$ = "[" + RegSet$(2, ByteVal% MOD 8) + " + " + HEX$(Integ%) + "]"
                Operand2$ = RegSet$(RegByte%, Map1D22DY(ByteVal%, ByteVal% MOD 8, 8) - 16)
           CASE 191 TO 255' A bunch of selves
                Operand1$ = RegSet$(RegByte%, ByteVal% MOD 8)
                Operand2$ = RegSet$(RegByte%, Map1D22DY(ByteVal%, ByteVal% MOD 8, 8) - 24)
           CASE ELSE
                Operand1$ = " " + HEX$(ASC(Byte))
    END SELECT
    
    IF LEFT$(Operand1$, 1) = " " THEN
        PRINT LongHex(InstVal%, 2) + Operand1$
    ELSE
        IF InstVal% - InstBase% > 1 THEN SWAP Operand1$, Operand2$
        PRINT #2, Inst$ + Operand1$ + ", " + Operand2$
    END IF
END SUB

SUB Trap2 (Inst$(), InstVal%, InstBase%)
    ByteBase% = (InstVal% - InstBase%) MOD 2
    GET 1, , Byte
    ByteVal% = ASC(Byte)
    SELECT CASE ByteVal%
           CASE 0 TO 5, 7 TO 13, 15 TO 21, 23 TO 29, 31 TO 37, 39 TO 45, 47 TO 53, 55 TO 61, 63
                Operand1$ = "[" + RegSet$(3, ByteVal% MOD 8) + "]"
           CASE 6, 14, 22, 30, 38, 46, 54, 62
                'These Replace what would have been a [BP] Instruction
                GET 1, , Integ%
                Operand1$ = "[" + HEX$(Integ%) + "]"
           CASE 64 TO 127
                GET 1, , Byte2
                Operand1$ = "[" + RegSet$(2, ByteVal% MOD 8) + HEXSignedByte(ASC(Byte2)) + "]"
           CASE 128 TO 191
                GET 1, , Integ%
                Operand1$ = "[" + RegSet$(3, ByteVal% MOD 8) + " + " + HEX$(Integ%) + "]"
           CASE 191 TO 255
                Operand1$ = RegSet$(ByteBase%, ByteVal% MOD 8)
           CASE ELSE
                Operand1$ = " " + HEX$(ByteVal%)
    END SELECT
    IF LEFT$(Operand1$, 1) = " " THEN
        PRINT LongHex(InstVal%, 2) + Operand1$
    ELSE
        IF InstVal% - InstBase% > 1 THEN SWAP Operand1$, Operand2$
        PRINT #2, "ADD " + Operand1$ + ", " + Operand2$
    END IF
END SUB


