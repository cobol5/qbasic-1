'Copyright (c) 1997-1999, Future Software - http://future.hypermart.net
'-=========================================================================
'Greetings programmer!
'This program was originally created by Rich Geldreich, but modified
'by Andrew Nagy with help of Peter Radics. They made a real program of it,
'first it were only some SUBs...
'Finally, Jorden Chamid did the last touches

'Email us here:
'Andrew Nagy : nag@freemail.c3.hu
'Jorden Chamid : Jorden@Future.hypermart.net
'-=========================================================================

DEFINT A-U
'$INCLUDE: 'qbex.bi'
DECLARE FUNCTION DetectSMARTDRV% ()
DECLARE FUNCTION GetByte () 'by Rich Geldreich
DECLARE SUB MailCOD ()
DECLARE SUB Demo ()
DECLARE SUB News ()
DECLARE SUB Stopp ()
DECLARE SUB ErrorMsgs (EFlag%)
DECLARE SUB GetArg () 'gets the commandline parameters
DECLARE SUB Hash (Prefix, Suffix, Index, Found) 'by Rich Geldreich
DECLARE SUB Help () 'this screen shows if nothing entered for commandline
DECLARE SUB Info () 'info about bugtesters, betatesters, so...
DECLARE SUB PutByte (a) 'by Rich Geldreich
DECLARE SUB PutCode (a) 'by Rich Geldreich
DECLARE SUB Rebuild.Table (New.Entries) 'by Rich Geldreich
DECLARE SUB SpecEd () 'this sub is the SpecialEdition registrations place
DECLARE SUB Test () 'this is not true test, just some infof about the packed file...

Start! = TIMER

DIM SHARED Arg$(0 TO 85)
DIM SHARED spced$, ver$, comment$, VErr$
DIM SHARED Prefix(6576), Suffix(6576), Code(6576)
DIM SHARED Used(4096), Buff&
DIM SHARED decoded$, out$, lkj
DIM SHARED InBuffer$, IAddress, IEndAddress, Iseg
DIM SHARED OutBuffer$, OStartAddress, OAddress, OEndAddress, Oseg
DIM SHARED CodeSize, CurrentBit, Char&
DIM SHARED Shift(12) AS LONG
DIM SHARED inreg AS RegType, outreg AS RegType
DIM SHARED InRegX AS RegTypeX, OutRegX AS RegTypeX
CONST True = -1, False = 0 'by Rich Geldreich
ON ERROR GOTO ErrHandler

'=====================================
Buff& = 2200 'String buffer size
'=====================================

dae$ = STRING$(10, 0)   'date buffer
InBuffer$ = STRING$(Buff&, 0)   'input buffer
OutBuffer$ = STRING$(Buff&, 0)  'output buffer

a& = SADD(OutBuffer$)
a& = a& - 65536 * (a& < 0)
Oseg = VARSEG(OutBuffer$) + (a& \ 16)  'Segment of buffer
OAddress = (a& MOD 16)                 'Current address in disk buffer
OEndAddress = OAddress + Buff&         'end address of  buffer
OStartAddress = OAddress               'Start of buffer

FOR a = 0 TO 12: READ Shift(a): NEXT   'by Rich Geldreich
DATA 1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192 'by Rich Geldreich

'-==================================================================
ver$ = "1.0" 'version # (shows everywhere...)
comment$ = " Final" 'comment if available (shows everywhere!!)
lkj = 8
'-==================================================================

x = 24
COLOR 7, 1
CLS
' here opens the SpecialEdition keyfile
OPEN "expc.spc" FOR BINARY AS #8
IF LOF(8) = 0 THEN lfas = 1: CLOSE #8: GOTO tovabb ELSE CLOSE #8 'is it a true file?????
OPEN "expc.spc" FOR INPUT AS #8
INPUT #8, spced$
sss$ = spced$
FOR tt = 1 TO LEN(spced$)
        yq% = 0
        xq% = ASC(MID$(spced$, tt, 1))
        'PRINT x%
        IF (xq% > 64 AND xq% < 91) OR (xq% > 96 AND xq% < 123) THEN
          yq% = 13
          xq% = xq% - yq%
          IF xq% < 97 AND xq% > 83 THEN xq% = xq% + 26 ELSE IF xq% < 65 THEN xq% = xq% + 26
          END IF
        MID$(spced$, tt, 1) = CHR$(xq%)
  NEXT tt
hdr1n:
decoded$ = spced$
CLOSE #8
tovabb:
IF lfas = 1 THEN KILL "expc.spc" ELSE 'it doesn't exist

GetArg 'gets commandline params...
IF Arg$(1) = "" THEN 'no commandline
CALL Help
END
ELSE
END IF

IF UCASE$(Arg$(1)) = "/C" THEN crc$ = "N" ELSE crc$ = "Y"
IF UCASE$(Arg$(1)) = "/D" THEN ban = 0 ELSE ban = 1
IF UCASE$(Arg$(1)) = "/S" THEN tui = 0 ELSE tui = 1
IF UCASE$(Arg$(1)) = "/H" THEN spced = 0 ELSE IF spced$ <> "" THEN spced = 1
IF UCASE$(Arg$(1)) = "/B" THEN CALL Info
IF UCASE$(Arg$(1)) = "/MC" THEN MailCODe = 1
IF UCASE$(Arg$(1)) = "/T" THEN CALL Test
IF UCASE$(Arg$(1)) = "/A" THEN CALL SpecEd
fileok = 0
CLS

ujra:
lkj = lkj + 1
halk = 0
InBuffer$ = STRING$(Buff&, 0)   'input buffer
OutBuffer$ = STRING$(Buff&, 0)  'output buffer
a& = SADD(OutBuffer$)
a& = a& - 65536 * (a& < 0)
Oseg = VARSEG(OutBuffer$) + (a& \ 16)  'Segment of buffer
OAddress = (a& MOD 16)                 'Current address in disk buffer
OEndAddress = OAddress + Buff&         'end address of  buffer
OStartAddress = OAddress               'Start of buffer

fileok = fileok + 1
file$ = Arg$(fileok) 'first parameter is file...
IF file$ = "" THEN GOTO hmm
IF LEFT$(file$, 1) = "/" THEN GOTO ujra
IF MID$(file$, LEN(file$), 1) = "." THEN a = 1 'hasn't got extension...
IF MID$(file$, LEN(file$) - 1, 1) = "." THEN a = 2
IF MID$(file$, LEN(file$) - 2, 1) = "." THEN a = 3
IF MID$(file$, LEN(file$) - 3, 1) = "." THEN a = 4

IF a = 1 THEN PRINT "File without extension cannot be packed!!" 'WOW!!!!!!
file$ = RTRIM$(file$)
file$ = LTRIM$(file$)
out$ = file$
IF a = 2 THEN
out$ = out$ + "  "
MID$(out$, LEN(file$), 3) = "EXC" 'output filename (ex. qb.ex$)
END IF
IF a = 3 THEN
out$ = out$ + " "
MID$(out$, LEN(file$) - 1, 3) = "EXC"'output filename (ex. qb.ex$)
END IF
IF a = 4 THEN
MID$(out$, LEN(file$) - 2, 3) = "EXC"'output filename (ex. qb.ex$)
END IF

OPEN file$ FOR BINARY AS #1
FileLength& = LOF(1)
afl = LEN(FileLength&)
size$ = STRING$(lafl, 0)
IF FileLength& = 0 THEN
        CLOSE #1
        KILL COMMAND$
        PRINT COMMAND$; " not found"
        END
END IF
OPEN out$ FOR BINARY AS #2
IF LOF(2) <> 0 THEN
        CLOSE #2
KILL out$
OPEN out$ FOR BINARY AS #2
END IF
aa = LEN(Arg$(fileok))
ax$ = file$ + SPACE$(12 - aa) 'filename in header
id$ = "EXP" + UCASE$(ver$) 'ID of an EX-Packed file

dae$ = DATE$
PUT #2, , id$
PUT #2, , ax$
PUT #2, , dae$
PUT #2, , crc$
'==================================================================
CurrentLoc& = 2
StartCode = 259                 'First LZW code that is available
NextCode = 259
MaxCode = 512
CodeSize = 9
CurrentBit = 0
Char& = 0
GOSUB ClearTable

Prefix = GetByte

IF spced$ = "" THEN GOTO hdr2 ELSE GOTO hdr1 'if no SpecialEdition info
hdr1:

'==================================================================
IF spced = 0 THEN GOTO tib 'whats this??????????????
ss$ = sss$ + SPACE$(24 - LEN(sss$)) 'password buffer
be$ = "Y" 'marks password exist for unpacker program...
PUT #2, , be$ 'mark
PUT #2, , ss$ 'encoded password
'==================================================================
' prints the special line
tib:
COLOR 7, 1
LOCATE 3, 5: COLOR 11, 1: PRINT "EX-Pack v" + ver$ + comment$ + " by Andrew" + " -- SpecialEdition for : " + decoded$
IF lfas = 0 AND spced = 0 THEN
be$ = "N"
PUT #2, , be$
END IF
GOTO t0:

hdr2:
COLOR 7, 1
LOCATE 3, 5: COLOR 11, 1: PRINT "EX-Pack v" + ver$ + comment$ + " based on LZW by Andrew"
be$ = "N"
PUT #2, , be$

t0:

IF crc$ = "N" THEN GOTO noc
noc:
COLOR 11, 1
LOCATE 4, 1: PRINT "    ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ   "
LOCATE 22, 1: PRINT "    ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ   "
COLOR 7
LOCATE 6, 8: PRINT "þ SMARTDRIVE  :"

IF DetectSMARTDRV THEN
'LOCATE 6, 10: PRINT "û"
COLOR 10:
LOCATE 6, 24: PRINT "Exist !"
ELSE
'LOCATE 6, 10: PRINT "û"
LOCATE 6, 24: COLOR 12: PRINT "Not exist !!!"
END IF
COLOR 7                                 'Restore default MS-DOS colour.
LOCATE 7, 8: PRINT "þ Compressing : Using LZW algorythm"
LOCATE 23, 19: COLOR 7: PRINT "±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±": COLOR 7

y = CSRLIN - 3
LOCATE 20, 5: COLOR 15, 3: PRINT "< STOP >"; : COLOR 0, 1: PRINT "Ü"
LOCATE 21, 6: COLOR 0, 1: PRINT "ßßßßßßßß": COLOR 7, 1
DO
IF INKEY$ = CHR$(13) THEN CALL Stopp
LOCATE , , 0
       
        DO
                IF CurrentLoc& > FileLength& THEN
                        PutCode Prefix
                        PutCode 256
                        PutCode 0: PutCode 0
                        LOCATE 23, 19: COLOR 14: PRINT "ÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛÛ": COLOR 7
                        LOCATE 23, 5: PRINT "All Complete!"
                        OutBuffer$ = LEFT$(OutBuffer$, OAddress - OStartAddress)
                        BytesOut& = LOF(2) + (OAddress - OStartAddress)
                        LOCATE lkj, 8: PRINT file$
                        LOCATE lkj, 22: PRINT " ("; 100 - ((100& * BytesOut&) \ CurrentLoc&); "%)   ";
                        LOCATE lkj, 31: PRINT "ÍÍÍ" + CHR$(16); " "; out$
                        LOCATE 20, 5: PRINT "              ": COLOR 7, 1
                        LOCATE 21, 5: PRINT "            ": COLOR 7, 1
                        PUT #2, , OutBuffer$
                        CLOSE
                        IF MailCODe = 1 THEN CALL MailCOD
                        IF fileok = 8 THEN GOTO hmm
                        GOTO ujra
hmm:
                        LOCATE lkj + 1, 8: COLOR 10, 1: PRINT "ALL OK!"
                        COLOR 7, 0
                        END

                ELSE

                        Suffix = GetByte
                        CurrentLoc& = CurrentLoc& + 1
                        Hash Prefix, Suffix, Index, Found
                        IF Found = True THEN
                                Prefix = Code(Index)
                                'update how many times this string was used
                                Used(Prefix) = Used(Prefix) + 1
                        END IF
                END IF
        LOOP WHILE Found = True

        'only increase the code size when required
        DO WHILE Prefix >= MaxCode AND CodeSize < 12
                PutCode 257
                MaxCode = MaxCode * 2
                CodeSize = CodeSize + 1
        LOOP

        PutCode Prefix

        'Put the new string into the hash table.
        Prefix(Index) = Prefix
        Suffix(Index) = Suffix
        Code(Index) = NextCode  'remember this strings code

        'Prefix is now equal to the character that caused the failure now.
        Prefix = Suffix

        NextCode = NextCode + 1
        'if there are too many strings then rebuild the encoding table
        IF NextCode > 4096 THEN

                PutCode 258 'send rebuild table code to decompressor

                Rebuild.Table New.Entries
                NextCode = New.Entries + StartCode

                IF NextCode > 4096 THEN
                        GOSUB ClearTable
                        NextCode = StartCode        'reset NextCode to top of tree
                END IF

                CodeSize = 9
                MaxCode = 512


        END IF

        'let the impatient user know we haven't hung up (yet!)
        PrintCounter = PrintCounter + 1     'see if time to update the
        IF PrintCounter = 512 THEN          'screen
                LOCATE 23, 4: PRINT (100& * CurrentLoc&) \ FileLength&; "%"; " Complete"

                BytesOut& = LOF(2) + (OAddress - OStartAddress)
                IF tui = 0 THEN GOTO t1
                bbbbb = (((100& * CurrentLoc&) \ FileLength&) * 20) \ 35
                FOR ixc = 19 TO 19 + bbbbb
                LOCATE 23, ixc: COLOR 14: PRINT "Û": COLOR 7 'bbbbb + 32: COLOR 14: PRINT "±": COLOR 7
                NEXT ixc

t1:
                LOCATE lkj, 8: PRINT file$
                LOCATE lkj, 22: PRINT " ("; (100& * CurrentLoc&) \ FileLength&; "%)"
                'LOCATE 11, 10: PRINT "Ã Compression ratio :"; 100 - ((100& * BytesOut&) \ CurrentLoc&); "% "
                'LOCATE 12, 10: PRINT "Ã Size of file was  :"; CurrentLoc&
                'LOCATE 13, 10: PRINT "À Compressed size   :"; BytesOut&
                PrintCounter = 0
        END IF
LOOP
'clears the hash table
ClearTable:
        FOR a = 0 TO 6576
                Prefix(a) = -1
                Suffix(a) = -1
                Code(a) = -1
        NEXT
RETURN

ErrHandler:
RESET
EFlag% = ERR
CALL ErrorMsgs(EFlag%)
COLOR 15, 12:
LOCATE 10, 19: PRINT "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ WARNING! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
LOCATE 11, 19: PRINT "³   An ERROR occured during the operation!   ³"
LOCATE 12, 19: PRINT "³                                            ³"
LOCATE 13, 19: PRINT "³                                            ³"
LOCATE 14, 19: PRINT "³                                            ³"
LOCATE 15, 19: PRINT "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
LOCATE 13, 21: PRINT "Type of error : "; VErr$
LOCATE 14, 21: PRINT "Code of error :"; ERR
COLOR 7, 0
BEEP
END
DEFINT A-Z

FUNCTION DetectSMARTDRV%
inreg.ax = &H4A10                    'function 4Ah, Subfunction 10h
inreg.bx = 0                         'Sorry, lost the info on this !
inreg.cx = &HEBAB                    'Sorry, lost the info on this !
CALL INTERRUPT(&H2F, inreg, outreg)  'Call DOS (Int 2fh)
DetectSMARTDRV = 0                   'Assume it's not there.
IF outreg.ax = &HBABE THEN DetectSMARTDRV = -1
END FUNCTION

SUB ErrorMsgs (EFlag%)
IF EFlag% = 0 THEN EXIT SUB
SELECT CASE EFlag%
                CASE 1:  VErr$ = "SHARE not active"
                CASE 2:  VErr$ = "Syntax Error"
                CASE 3:  VErr$ = "Return w/o gosub"
                CASE 4:  VErr$ = "Out of Data"
                CASE 5:  VErr$ = "Illegal Function Call"
                CASE 6:  VErr$ = "Overflow"
                CASE 7:  VErr$ = "Out of Memory"
                CASE 8:  VErr$ = "Label not defined"
                CASE 9:  VErr$ = "Subscript out of range"
                CASE 10: VErr$ = "Duplicate Definition"
                CASE 11: VErr$ = "Division by Zero"
                CASE 12: VErr$ = "Illegal in direct mode"
                CASE 13: VErr$ = "Type Mismatch"
                CASE 14: VErr$ = "Out of string space"
                CASE 15: VErr$ = "Fossil not available"
                CASE 16: VErr$ = "String formula too complex"
                CASE 17: VErr$ = "Carrier Dropped!"
                CASE 18: VErr$ = "Function not defined"
                CASE 19: VErr$ = "No RESUME"
                CASE 20: VErr$ = "RESUME w/o error"
                CASE 24: VErr$ = "Modem or Device Timeout"
                CASE 25: VErr$ = "Modem or Device fault"
                CASE 26: VErr$ = "FOR w/o NEXT"
                CASE 27: VErr$ = "Out of paper"
                CASE 29: VErr$ = "WHILE w/o WEND"
                CASE 30: VErr$ = "WEND w/o WHILE"
                CASE 33: VErr$ = "Duplicate Label"
                CASE 35: VErr$ = "Subprogram not defined"
                CASE 37: VErr$ = "Aurgument-count mismatch"
                CASE 38: VErr$ = "Array not defined"
                CASE 39: VErr$ = "CASE ELSE expected"
                CASE 40: VErr$ = "Variable Required"
                CASE 50: VErr$ = "Buffer or Field overflow"
                CASE 51: VErr$ = "System error"
                CASE 52: VErr$ = "Bad file name or number"
                CASE 53: VErr$ = "File not found"
                CASE 54: VErr$ = "Bad file mode"
                CASE 55: VErr$ = "File already open"
                CASE 56: VErr$ = "Field statement active"
                CASE 57: VErr$ = "Modem or Device I/O error"
                CASE 58: VErr$ = "File already exists"
                CASE 59: VErr$ = "Bad record length"
                CASE 61: VErr$ = "Disk full"
                CASE 62: VErr$ = "Input past end of file"
                CASE 63: VErr$ = "Bad record number"
                CASE 64: VErr$ = "Invalid filename"
                CASE 67: VErr$ = "Too many files"
                CASE 68: VErr$ = "Drive/Device Unavailable"
                CASE 69: VErr$ = "Comm buffer overflow"
                CASE 70: VErr$ = "Permission denied"
                CASE 71: VErr$ = "Disk/Printer not ready"
                CASE 72: VErr$ = "Disk media error"
                CASE 73: VErr$ = "Feature not available"
                CASE 74: VErr$ = "Rename across disks"
                CASE 75: VErr$ = "Path/File access error"
                CASE 76: VErr$ = "Path not found"
                CASE 255: VErr$ = "Invalid command line parameter"
                CASE ELSE: VErr$ = "UNIDENTIFIED ERROR"
END SELECT
VErr$ = VErr$ + " !!!"
END SUB

SUB GetArg
Arg$(0) = ""
IF COMMAND$ = "" THEN EXIT SUB
i = 1: j = 1
DO UNTIL i >= LEN(COMMAND$)
        WHILE MID$(COMMAND$, i, 1) <> " " AND MID$(COMMAND$, i, 1) <> ""
                Arg$(j) = Arg$(j) + MID$(COMMAND$, i, 1)
                i = i + 1
        WEND
        i = i + 1: j = j + 1
LOOP
END SUB

'Reads one byte from the input buffer, and fills the buffer if it's emty.
FUNCTION GetByte
        IF IAddress = IEndAddress THEN
                GET #1, , InBuffer$
                a& = SADD(InBuffer$)
                a& = a& - 65536 * (a& < 0)
                Iseg = VARSEG(InBuffer$) + (a& \ 16)
                IAddress = (a& MOD 16)
                IEndAddress = IAddress + Buff&
        END IF
        DEF SEG = Iseg
        GetByte = PEEK(IAddress)
        IAddress = IAddress + 1
END FUNCTION

'Attempts to finds a prefix:suffix string.
SUB Hash (Prefix, Suffix, Index, Found)

        Index = (Prefix * 256& XOR Suffix) MOD 6577 'XOR hashing
        IF Index = 0 THEN  'is Index lucky enough to be 0?
                offset = 1     'Set offset to 1, because 6577-0=6577
        ELSE
                offset = 6577 - Index
        END IF
        DO 'until we find a match or don't
                IF Code(Index) = -1 THEN      'is there nothing here?
                        Found = False             'yup, not found
                        EXIT SUB
                'is this entry what we're looking for?
                ELSEIF Prefix(Index) = Prefix AND Suffix(Index) = Suffix THEN
                        Found = True              'yup, found
                        EXIT SUB
                ELSE 'retry until we find what were looking for or we find a blank
                         'entry
                        Index = Index - offset
                        IF Index < 0 THEN 'is index too far down?
                                Index = Index + 6577 'yup, bring it up then
                        END IF
                END IF
        LOOP
END SUB

SUB Help
COLOR 11, 1
CLS
COLOR 11, 1
LOCATE 4, 1: PRINT "    ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ   "
LOCATE 22, 1: PRINT "    ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ   "

IF spced$ = "" THEN GOTO hdrrr2 ELSE GOTO hdrrr1
hdrrr1:
LOCATE 3, 5: COLOR 11, 1: PRINT "EX-Pack v" + ver$ + comment$ + " by Future Software" + " -- SpecialEdition for : " + decoded$
GOTO huha
hdrrr2:
LOCATE 3, 5: COLOR 11, 1: PRINT "EX-Pack v" + ver$ + comment$ + " based on LZW by Andrew"
huha:
COLOR 7, 1
LOCATE 5:
PRINT "    Compiled on : 19-01-1999"
PRINT
COLOR 15, 1
PRINT
PRINT "    Usage : EXPC.EXE /options filename.ext"
COLOR 7, 1
PRINT
PRINT "    Options :"
PRINT "    /h - Don't add SpecialEdition info into header."
PRINT "    /c - Don't add CRC+ Recovery."
PRINT "    /t - Tests the header of packed file."
PRINT "    /a - SpecialEdition registration."
PRINT "    /s - Simplifyed TUI for faster packing."
PRINT "    /mc- MailCOD (Gives UUENCODEed outputfile)"
PRINT
PRINT "    /b - About the authors, BETA testers."
LOCATE 23: PRINT "    Internet : http://future.hypermart.net "
COLOR 7, 0
END
END SUB

SUB Info
COLOR 11, 1
CLS
COLOR 11, 1
LOCATE 4, 1: PRINT "    ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ   "
LOCATE 22, 1: PRINT "    ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ   "

IF spced$ = "" THEN GOTO hdrr2 ELSE GOTO hdrr1
hdrr1:
LOCATE 3, 5: PRINT "EX-Pack v" + ver$ + comment$ + " by Future Software" + " -- SpecialEdition for : " + decoded$
GOTO tovbb
hdrr2:
LOCATE 3, 5: PRINT "EX-Pack v" + ver$ + comment$ + " based on LZW by Andrew"
tovbb:
'COLOR 7, 0
COLOR 15
LOCATE 5:
PRINT "    About the software, and it's developer team:"
COLOR 7
PRINT
PRINT "    EX-Pack is an LZW algorythm based compressor wich is fully made "
PRINT "    in QuickBasic 4.5 . The software is FREEWARE. This means you can"
PRINT "    spread it freely and use it as you want, but you shouldn't modify"
PRINT "    or disassemble it any way."
PRINT
PRINT "    Authors:"
PRINT "    Andras Nagy  - TUI, code, engine tunings, packed file header, etc."
PRINT "    Peter Radics - Adding some routines, TUI enhanceing, tips, testing."
PRINT "    Jorden Chamid - Final touches."
PRINT
PRINT "    Official beta testers:"
PRINT "    Ferenc Sorosics - Wide testing!! Explored many bugs!"
PRINT
PRINT "    Our homepage is at"
PRINT "    http://future.hypermart.net"
DO: LOOP UNTIL INKEY$ <> ""
CLS : SCREEN 0: WIDTH 80, 25: PALETTE
END
END SUB

SUB MailCOD
  file$ = out$
  IF INSTR(file$, " ") THEN file$ = LEFT$(file$, INSTR(file$, " ") - 1)
  unam$ = file$
  IF INSTR(unam$, ".") THEN unam$ = LEFT$(file$, INSTR(file$, ".") - 1)
  pnam$ = file$
  IF INSTR(pnam$, "\") THEN pnam$ = MID$(pnam$, INSTR(file$, "\") + 1)
  OPEN file$ FOR BINARY AS #1
  OPEN unam$ + ".mlc" FOR OUTPUT AS #2
  PRINT #2, "begin 644 " + pnam$
  bl& = 45: fl& = LOF(1): bf$ = SPACE$(bl&)
  WHILE fl&
    IF fl& < bl& THEN bl& = fl&: bf$ = SPACE$(bl&)
    GET #1, , bf$: fl& = fl& - bl&
    IF bl& MOD 3 <> 0 THEN bf$ = bf$ + STRING$(3 - bl& MOD 3, 0)
    FOR i = 1 TO bl& STEP 3
      c1 = ASC(MID$(bf$, i, 1))
      c2 = ASC(MID$(bf$, i + 1, 1))
      c3 = ASC(MID$(bf$, i + 2, 1))
      l$ = l$ + CHR$(c1 \ 4 + 32)
      l$ = l$ + CHR$((c1 * 16 + c2 \ 16 AND &H3F) + 32)
      l$ = l$ + CHR$((c2 * 4 + c3 \ 64 AND &H3F) + 32)
      l$ = l$ + CHR$((c3 AND &H3F) + 32)
    NEXT
    PRINT #2, CHR$(32 + bl&); l$: l$ = ""
  WEND
  PRINT #2, : PRINT #2, "end"
  CLOSE #1, #2
LOCATE lkj, 49: COLOR 10: PRINT "MailCODed OK!": COLOR 7
KILL file$
END SUB

'Throws a byte into the output buffer and writes the buffer if it's full.
SUB PutByte (a)
        IF OAddress = OEndAddress THEN
                PUT #2, , OutBuffer$
                OAddress = OStartAddress
        END IF
        DEF SEG = Oseg
        POKE OAddress, a
        OAddress = OAddress + 1
END SUB

'Throws one multi-bit code to the output file.
SUB PutCode (a)
        SHARED MaxCode
        IF a >= MaxCode THEN STOP

        Char& = Char& + a * Shift(CurrentBit)
        CurrentBit = CurrentBit + CodeSize
        DO WHILE CurrentBit > 7
                PutByte Char& AND 255
                Char& = Char& \ 256
                CurrentBit = CurrentBit - 8
        LOOP
END SUB

'This is the "experimental" part of the program. This procedure eliminates
'any strings which are not used in the encoding table: the usual result of
'doing this is greater compression.
'It isn't documented well yet... I'm still working on it.
SUB Rebuild.Table (New.Entries)
        DIM p(4096), S(4096), U(4096) AS LONG, Pn(4096), c(4096)
        DIM Location(4096)

        SHARED StartCode, MaxCode, Prefix
        Num.Entries = 0

        FOR a = 0 TO 6576
                c = Code(a)
                IF c <> -1 THEN 'valid code?
                        IF Used(c) > 0 THEN 'was it used at all?
                                Used(c) = 0
                                p = Prefix(a): S = Suffix(a)
                                p(Num.Entries) = p          'put it into a temporary table
                                S(Num.Entries) = S
                                U(Num.Entries) = p * 4096& + S
                                c(c) = Num.Entries
                                Num.Entries = Num.Entries + 1
                        END IF
                END IF
        NEXT


        Num.Entries = Num.Entries - 1
        FOR a = 0 TO Num.Entries
                Pn(a) = a
        NEXT
                'sort the table according to it's prefix:suffix
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
                Location(Pn(a)) = a
        NEXT
        'clear the old hash table
        FOR a = 0 TO 6576
                Prefix(a) = -1
                Suffix(a) = -1
                Code(a) = -1
        NEXT

        'put each prefix:suffix into the hash table
        FOR A1 = 0 TO Num.Entries
                a = Pn(A1)

                p = p(a)
                S = S(a)
                IF p >= StartCode THEN 'is it pointing twards a string?
                        p = StartCode + Location(c(p)) 'yup; update the pointer
                END IF
                IF S >= StartCode THEN
                        S = StartCode + Location(c(S))
                END IF
                'where does this prefix:suffix go?
                Hash p, S, Index, 0
                'put it there
                Prefix(Index) = p
                Suffix(Index) = S
                Code(Index) = A1 + StartCode

        NEXT
        '# of entries in the hash table now
        New.Entries = Num.Entries + 1
END SUB

SUB SpecEd
x:
CLS
COLOR 15, 3: PRINT "SpecialEdition Registration       for EX-Pack v0.76g and above                  "
COLOR 7, 1
PRINT
INPUT "Enter name (Min. 5, max. 24 chars) : ", spa$
IF spa$ = "" THEN PRINT "Aborted.": END
IF LEN(spa$) < 5 THEN PRINT "You must enter minimum 5 characters.": END
IF LEN(spa$) > 24 THEN PRINT "You must enter maximum 24 characters.": END
Tst$ = spa$
INPUT "Enter key for name: ", spb$
GOTO ROT13
bn:
IF spb$ = Tst$ THEN
PRINT "Key OK!! SpecialEdition info saved into 'EXPC.SPC'!"
OPEN "expc.spc" FOR OUTPUT AS #5
WRITE #5, spb$
CLOSE #5
END
ELSE
BEEP: COLOR 12: PRINT "Wrong key!!!!": END
END IF

ROT13:
  FOR tt = 1 TO LEN(Tst$)
        y% = 0
        x% = ASC(MID$(Tst$, tt, 1))
        'PRINT x%
        IF (x% > 64 AND x% < 91) OR (x% > 96 AND x% < 123) THEN
          y% = 13
          x% = x% - y%
          IF x% < 97 AND x% > 83 THEN x% = x% + 26 ELSE IF x% < 65 THEN x% = x% + 26
          END IF
        MID$(Tst$, tt, 1) = CHR$(x%)
  NEXT tt
GOTO bn
END SUB

SUB Stopp
COLOR 15, 12:
LOCATE 10, 19: PRINT "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ WARNING! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
LOCATE 11, 19: PRINT "³ You are about to stop packing the current  ³"
LOCATE 12, 19: PRINT "³ file.                                      ³"
LOCATE 13, 19: PRINT "³ Do you really want to do that?             ³"
LOCATE 14, 19: PRINT "³                                            ³"
LOCATE 15, 19: PRINT "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
LOCATE 14, 21: COLOR 14: PRINT "Y"; : COLOR 7: PRINT "es/"; : COLOR 14: PRINT "N"; : COLOR 7: PRINT "o"
DO
hjhj:
IF UCASE$(INKEY$) = "Y" THEN GOTO elvet
IF UCASE$(INKEY$) = "N" THEN GOTO nono ELSE GOTO hjhj
LOOP WHILE INKEY$ = ""

elvet:
COLOR 7, 0
'CLS
PRINT "Packing stopped."
RESET
CLS : SCREEN 0: WIDTH 80, 25: PALETTE
END
nono:
COLOR 7, 1
LOCATE 10, 19: PRINT "                                              "
LOCATE 11, 19: PRINT "                                              "
LOCATE 12, 19: PRINT "                                              "
LOCATE 13, 19: PRINT "                                              "
LOCATE 14, 19: PRINT "                                              "
LOCATE 15, 19: PRINT "                                              "
END SUB

SUB Test
COLOR 11, 1
CLS
COLOR 11, 1
LOCATE 4, 1: PRINT "    ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ   "
LOCATE 22, 1: PRINT "    ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ   "

IF spced$ = "" THEN GOTO hdrrrr2 ELSE GOTO hdrrrr1
hdrrrr1:
LOCATE 3, 5: COLOR 11, 1: PRINT "EX-Pack v" + ver$ + comment$ + " by Andrew" + " -- SpecialEdition for : " + decoded$
GOTO tovbbb
hdrrrr2:
LOCATE 3, 5: COLOR 11, 1: PRINT "EX-Pack v" + ver$ + comment$ + " based on LZW by Andrew"
tovbbb:
LOCATE 6:
COLOR 15, 1: PRINT "    þ Testing archive header..."
COLOR 7, 1
CLOSE
RESET
OPEN Arg$(2) FOR BINARY AS #6
id$ = STRING$(7, 0)
ax$ = STRING$(12, 0)
crc$ = STRING$(1, 0)
dt$ = STRING$(10, 0)
size& = LOF(6)
GET #6, , id$
PRINT "     ú Header version   : "; id$
IF id$ = "EXP0.80" OR id$ = "EXP0.90" OR id$ = "EXP1.00" THEN GOTO oksi ELSE RESET: GOTO hujjaj
CLS : SCREEN 0: WIDTH 80, 25: PALETTE
END

hujjaj:
OPEN Arg$(1) FOR BINARY AS #5
id$ = STRING$(8, 0)
ax$ = STRING$(12, 0)
crc$ = STRING$(1, 0)
dt$ = STRING$(10, 0)
size& = LOF(5)
GET #5, , id$
PRINT "     ú Header version   : "; id$

IF id$ = "EXP0.45A" OR id$ = "EXP0.50A" OR id$ = "EXP0.60A" OR id$ = "EXP0.70B" OR id$ = "EXP0.76G" THEN GOTO oksi ELSE RESET: PRINT "Not a valid packed file.": END

oksi:
'PRINT "Ssssssssssssssssssssssssssssssssssss"
IF id$ = "EXP0.45A" THEN
GET #5, , ax$
PRINT "     ú Packed file name : "; ax$
PRINT "     ú Packed size      :"; size&; "bytes"
PRINT
PRINT
PRINT "    The testing of the packed file has finished..."

CLS : SCREEN 0: WIDTH 80, 25: PALETTE
END
END IF

IF id$ = "EXP0.50A" THEN
GET #5, , ax$
GET #5, , dt$
PRINT "     ú Packed file name : "; ax$
PRINT "     ú Packed on date   : "; dt$
PRINT "     ú Packed size      :"; size&; "bytes"
PRINT
PRINT
PRINT "    The testing of the packed file has finished..."

CLS : SCREEN 0: WIDTH 80, 25: PALETTE
END
END IF

IF id$ = "EXP0.60A" THEN
GET #5, , ax$
GET #5, , dt$
PRINT "     ú Packed file name : "; ax$
PRINT "     ú Packed on date   : "; dt$
PRINT "     ú CRC+ Recovery    : Present! (Built in...)"
PRINT "     ú Packed size      :"; size&; "bytes"
PRINT
PRINT
PRINT "    The testing of the packed file has finished..."

CLS : SCREEN 0: WIDTH 80, 25: PALETTE
END
END IF

IF id$ = "EXP0.70B" THEN
GET #5, , ax$
GET #5, , dt$
GET #5, , crc$
PRINT "     ú Packed file name : "; ax$
PRINT "     ú Packed on date   : "; dt$
IF crc$ = "Y" THEN crc$ = "Present!"
PRINT "     ú CRC+ Recovery    : "; crc$
PRINT "     ú Packed size      :"; size&; "bytes"
PRINT
PRINT
PRINT "    The testing of the packed file has finished..."

CLS : SCREEN 0: WIDTH 80, 25: PALETTE
END
END IF

IF id$ = "EXP0.76G" THEN
GET #5, , ax$
GET #5, , dt$
GET #5, , crc$
IF spced$ <> "" THEN GET #5, , spccode$ ELSE
PRINT "     ú Packed file name : "; ax$
PRINT "     ú Packed on date   : "; dt$
IF crc$ = "Y" THEN crc$ = "Present!"
PRINT "     ú CRC+ Recovery    : "; crc$
PRINT "     ú Packed size      :"; size&; "bytes"
PRINT
PRINT
PRINT "    The testing of the packed file has finished..."

CLS : SCREEN 0: WIDTH 80, 25: PALETTE
END
END IF


IF id$ = "EXP0.76" THEN
nullahet:
GET #6, , ax$
GET #6, , dt$
GET #6, , crc$
ssss$ = STRING$(1, 0)
GET #6, , ssss$
IF ssss$ = "N" THEN GOTO nincsn
IF ssss$ = "Y" THEN GOTO vann ELSE GOTO nincsn
vann:
bevan$ = STRING$(24, 0)
GET #6, , bevan$
PRINT "     ú Packed file name : "; ax$
PRINT "     ú Packed on date   : "; dt$
IF crc$ = "Y" THEN crc$ = "Present!"
PRINT "     ú CRC+ Recovery    : "; crc$
PRINT "     ú Packed size      :"; size&; "bytes"

FOR tt = 1 TO LEN(bevan$)
        y% = 0
        x% = ASC(MID$(bevan$, tt, 1))
        'PRINT x%
        IF (x% > 64 AND x% < 91) OR (x% > 96 AND x% < 123) THEN
          y% = 13
          x% = x% - y%
          IF x% < 97 AND x% > 83 THEN x% = x% + 26 ELSE IF x% < 65 THEN x% = x% + 26
          END IF
        MID$(bevan$, tt, 1) = CHR$(x%)
  NEXT tt
decoded$ = bevan$
PRINT "     ú SpecialEdition   : "; decoded$
PRINT
PRINT
PRINT "    The testing of the packed file has finished..."

CLS : SCREEN 0: WIDTH 80, 25: PALETTE
END
nincsn:
PRINT "     ú Packed file name : "; ax$
PRINT "     ú Packed on date   : "; dt$
IF crc$ = "Y" THEN crc$ = "Present!"
PRINT "     ú CRC+ Recovery    : "; crc$
PRINT "     ú Packed size      :"; size&; "bytes"
PRINT
PRINT
PRINT "    The testing of the packed file has finished..."
CLS : SCREEN 0: WIDTH 80, 25: PALETTE
END
END IF

IF id$ = "EXP1.00" THEN
GOTO nullahet
CLS : SCREEN 0: WIDTH 80, 25: PALETTE
END
END IF

CLS : SCREEN 0: WIDTH 80, 25: PALETTE
END


END SUB

