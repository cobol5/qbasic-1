DECLARE SUB xms.version (ES%, BX%)
DECLARE FUNCTION EMS.FREE$ ()
DECLARE FUNCTION EMS.DETECT% ()
DECLARE FUNCTION COPYVID13$ ()
DECLARE FUNCTION BIOSXMS.DETECT% ()
DECLARE SUB XMS.ENTRIE (ES%, BX%)
DECLARE FUNCTION INT2STR$ (SWORD%)
DECLARE FUNCTION XMM.DETECT% ()
DECLARE FUNCTION XMS.FREE$ (ES%, BX%)
DECLARE SUB DEMO ()
DECLARE FUNCTION LOW.FREE% ()
DECLARE FUNCTION EMS.VERSION% ()

'This program will give an introduction on memory issues as viewed from
'inside QBASIC intrepreter. The demo is nothing more then informing
'you about what we have available for low memory, expanded and
'extended memory management.
'Some other contribution will move on to the use of this dataspaces in
'some meaningful manner.

'NOTE:
'The XMS/EMS version numbers are not trivial here, like in a lot of other
'cases, since both the xms and ems specifications have been changing
'a lot !




DEMO

FUNCTION BIOSXMS.DETECT%
'------------------------------------------------------
'THIS FUNCTION RETURNS XMS MEMORY BLOCKS OF 1KB
'FOR 286 AND 386 WITHOUT XMS MANAGER INSTALLED

'STACKPASSING :AX%

'IN:

'OUT          :AX%--NR OF 1 KB EXTENDED MEMBLOCKS

'ERRORRETURN  :AX%= 0 THEN FUNCTION NOT SUPPORTED OR XMS
'                     HANDLER INSTALLED
'-------------------------------------------------------
asm$ = asm$ + CHR$(&H55)                            'PUSH BP
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)               'MOV BP,SP
asm$ = asm$ + CHR$(&H50)                            'PUSH AX

asm$ = asm$ + CHR$(&HB4) + CHR$(&H88)               'MOV AH,88
asm$ = asm$ + CHR$(&HCD) + CHR$(&H15)               'INT 15
asm$ = asm$ + CHR$(&H72) + CHR$(&H5)                'JC 5
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)   'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)                'MOV [BX],AX

asm$ = asm$ + CHR$(&H58)                            'POP AX
asm$ = asm$ + CHR$(&H5D)                            'POP BP
asm$ = asm$ + CHR$(&HCA) + CHR$(&H2) + CHR$(&H0)    'RETF 2
'________________________________________
Codeoff% = SADD(asm$)
DEF SEG = VARSEG(asm$)
CALL ABSOLUTE(ax%, Codeoff%)
'________________________________________
DEF SEG
BIOSXMS.DETECT% = ax%
END FUNCTION

SUB DEMO
CLS

PRINT SPACE$(17) + "MEMORY INFO IN QBASIC"
PRINT STRING$(60, "_"): COLOR 0, 7:
PRINT "LOW (CONVENTIONAL) MEMORY INFORMATION": COLOR 7, 0: PRINT
A% = LOW.FREE%
IF A% THEN PRINT "LOW MEMORY FREE IN PARA'S( BYTES)     :"; : COLOR 0, 7: PRINT " "; HEX$(A%); " ("; 16& * A%; ")": COLOR 7, 0
                                          
PRINT STRING$(60, "_"): COLOR 0, 7: PRINT "EXTENDED MEMORY INFORMATION":
COLOR 7, 0: PRINT


'XMS ?

IF XMM.DETECT THEN PRINT "XMM "; : COLOR 0, 7: PRINT "DETECTED" ELSE PRINT "TOTAL XMS AVAILABLE FOR BIOS :"; : COLOR 0, 7: PRINT HEX$(BIOSXMS.DETECT%)
COLOR 7, 0
XMS.ENTRIE ES%, BX%
PRINT "DRIVER ENTRIE SEG" + SPACE$(21) + ":"; : COLOR 0, 7: PRINT " "; HEX$(ES%): COLOR 7, 0
PRINT "DRIVER ENTRIE OFF" + SPACE$(21) + ":"; : COLOR 0, 7: PRINT " "; HEX$(BX%): COLOR 7, 0
SEGDR% = ES%: OFFDR% = BX%
xms.version ES%, BX%
PRINT "XMS VERSION" + SPACE$(27); ":";
COLOR 0, 7: PRINT " "; HEX$((ES% AND &HFF00) \ 256); "."; HEX$(ES% AND &HFF)
COLOR 7, 0
IF BX% THEN PRINT "HMA "; : COLOR 0, 7: PRINT "EXISTS": COLOR 7, 0 ELSE PRINT "HMA"; : COLOR .7: PRINT "NOT available "; ""

'LET US INITIALIZE A FEW:
'------------------------
STRINGSEG% = VARSEG(ALLOC$): DEF SEG = STRINGSEG%: FREE$ = XMS.FREE$(SEGDR%, OFFDR%)
'--------------------------------------------------------------------------
'FREE MEMORY ?
'--------------
Codeoff% = SADD(FREE$): CALL ABSOLUTE(ax%, dx%, Codeoff%)
PRINT "TOTAL XMS IN KB/ BYTES" + SPACE$(16) + ":"; : COLOR 0, 7: PRINT dx%; "("; dx% * &H400&; ")"
COLOR 7, 0
PRINT "FREE  XMS IN KB/ BYTES" + SPACE$(16) + ":"; : COLOR 0, 7: PRINT ax%; "("; ax% * &H400&; ")"
COLOR 7, 0

'EMS ?
PRINT STRING$(60, "_"): COLOR 0, 7: PRINT "EXPANDED MEMORY INFORMATION":
COLOR 7, 0: PRINT


'LETS DO SOME INITIALIZING
FREE$ = EMS.FREE$:
'----------------------------------------------------------------------

'1ST LETS DETERMINE IF EMS IS AVAILABLE:

DEF SEG = STRINGSEG%
PRINT "EMS "; : COLOR 0, 7: A% = EMS.DETECT%
IF A% THEN PRINT "NOT INSTALLED, ERROR "; A%; " DETECTED": EXIT SUB:  ELSE PRINT "INSTALLED"
COLOR 7, 0
B% = EMS.VERSION%
IF B% AND &HFF00 THEN PRINT "ERROR GETTING VERSION :"; B% ELSE PRINT "EMS VERSION" + SPACE$(27) + ":"; : COLOR 0, 7: PRINT " "; HEX$((B% AND &HF0) \ &HF); "."; HEX$(B% AND &HF)
COLOR 7, 0

'SECOND LET US DETERMINE THE AMOUNT OF AVAILABLE EMS

OFFCODE% = SADD(FREE$):
CALL ABSOLUTE(BX%, dx%, OFFCODE%)
IF dx% THEN PRINT "AVAILABLE EMS PAGES(16 KB)/BYTES      :"; : COLOR 0, 7: PRINT dx%; "("; dx% * 16 * 1024&; ")": COLOR 7, 0: PRINT "FREE EMS PAGES(16 KB)/BYTES           :"; : COLOR 0, 7: PRINT BX%; "("; BX% * 16 * 1024&; ")":  ELSE PRINT "ERROR IN EMS SOFTWARE "; BX%: EXIT SUB

COLOR 7, 0:
PRINT STRING$(60, "_"): PRINT : PRINT "good bye"


END SUB

'**************** EMSSTATUS%() ******************
'*** RETURNS WHETHER EMS IS AVAILABLE.  -1 IS ***
'*** RETURNED IF IT IS AVAILABLE, 0 OTHERWISE ***
'ORIGINAL:J. LEGER
'IN:
'OUT: AX 0 SUCSES
'     AX ELSE ERROR IN AH
'************************************************
FUNCTION EMS.DETECT%

asm$ = ""
asm$ = asm$ + CHR$(&H55)                         'PUSH BP
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)            'MOV BP,SP
asm$ = asm$ + CHR$(&HB4) + CHR$(&H40)            'MOV AH,40
asm$ = asm$ + CHR$(&HCD) + CHR$(&H67)            'INT 67
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H88) + CHR$(&H27)            'MOV [BX],AH
asm$ = asm$ + CHR$(&H5D)                         'POP BP
asm$ = asm$ + CHR$(&HCB)                         'RETF


DEF SEG = VARSEG(asm$)
CALL ABSOLUTE(EMS%, SADD(asm$))
DEF SEG
EMS.DETECT% = EMS%

END FUNCTION

FUNCTION EMS.FREE$
'----------------------------------------

'THIS FUNCTION RETURNS TOTAL EMS MEMORY
'AND FREE EMS MEMORY

'STACKPASSING: BX%,DX%

'IN          :

'OUT         : DX% 0     ERROR
'            :     ELSE  TOTAL EMS
'            : BX% ERRORCODE/ FREE EMS
'----------------------------------------

asm$ = asm$ + CHR$(&H55)                            'PUSH BP
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)               'MOV BP,SP
asm$ = asm$ + CHR$(&H50)                            'PUSH AX
asm$ = asm$ + CHR$(&H52)                            'PUSH DX

asm$ = asm$ + CHR$(&HB4) + CHR$(&H42)               'MOV AH,42
asm$ = asm$ + CHR$(&HCD) + CHR$(&H67)               'INT 67   GET PAGE COUNT
asm$ = asm$ + CHR$(&H8) + CHR$(&HE4)                'OR AH,AH
asm$ = asm$ + CHR$(&H74) + CHR$(&H4)                'JZ+4  NO ERROR
asm$ = asm$ + CHR$(&H31) + CHR$(&HD2)               'IF ERROR XOR DX,DX DX=0
asm$ = asm$ + CHR$(&H89) + CHR$(&HC3)               'IF ERROR MOV BX,AX BX=ERRORCODE
asm$ = asm$ + CHR$(&H89) + CHR$(&HD8)               'MOV AX,BX TEMP NEED BX
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)   'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H89) + CHR$(&H17)               'MOV [BX],DX   IN DX% 0 OR TOTAL EMS
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8)   'MOV BX,[BP+08]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)                'MOV [BX],AX   IN BX% ERRORCODE OR EMS FREE

asm$ = asm$ + CHR$(&H5A)                            'POP DX
asm$ = asm$ + CHR$(&H58)                            'POP AX
asm$ = asm$ + CHR$(&H5D)                            'POP BP
asm$ = asm$ + CHR$(&HCA) + CHR$(&H4) + CHR$(&H0)    'RETF 4
'________________________________________
EMS.FREE$ = asm$


END FUNCTION

FUNCTION EMS.VERSION%
'---------------------------------------
'THIS FUNCTION RETURNS THE EMS VERSION
'STACKPASSING: AX%
'IN          :
'OUT         : AH 0    SUCSES
'                 ELSE ERRORCODE
'              AL VERSION


'THE VERSION IS IMPORTANT TO CHECK SINCE
'ALL ELABORATE EMS FUNCTIONS ARE MOSTLY
'----------------------------------------
asm$ = ""
asm$ = asm$ + CHR$(&H55)                             'PUSH BP
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)                'MOV BP,SP
asm$ = asm$ + CHR$(&HB4) + CHR$(&H46)                'MOV AH,46
asm$ = asm$ + CHR$(&HCD) + CHR$(&H67)                'INT 67
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)    'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)                 'MOV [BX],AX
asm$ = asm$ + CHR$(&H5D)                             'POP BP
asm$ = asm$ + CHR$(&HCA) + CHR$(&H2) + CHR$(&H0)     'RETF 2
'________________________________________
Codeoff% = SADD(asm$)
DEF SEG = VARSEG(asm$)
CALL ABSOLUTE(ax%, Codeoff%)
'________________________________________
DEF SEG
EMS.VERSION% = ax%

END FUNCTION

FUNCTION INT2STR$ (SWORD%)
'THIS FUNCTION IS TRANSLATING SWORD INTEGERS INTO A STRING. ITS ONLY USE
'IS WHEN YOU STILL USE ASM$ FOR ASSEMBLER FUNCTIONS( LIKE I DO). IN THAT
'CASE YOU CAN MAKE YOUR INTEGER VALUES USABLE ..
'THIS FUNCTION SIMPLY TRANSLATES THE HEXA BYTES
'INTO STRINGBYTES AS IS.
'----------------------------------------------------
DEF SEG = VARSEG(SWORD%)
PTR% = VARPTR(SWORD%)
INT2STR$ = CHR$(PEEK(PTR%)) + CHR$(PEEK(PTR% + 1))
DEF SEG

END FUNCTION

FUNCTION LOW.FREE%
'-------------------------------------------------------------------
'THIS FUNCTION ALLOCATES LOWER MEMORY
'STACK PASSING :BX%
'
'IN            :BX%= FFFF WHOLE MB REQUESTED<G>
'
'OUT           :BX%= 0 OR AVAILABLE MEMORY

'---------------------------------------------------------------------
asm$ = asm$ + CHR$(&H55)                                    'PUSH BP
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)                       'MOV BP,SP
asm$ = asm$ + CHR$(&H50)                                    'PUSH AX
asm$ = asm$ + CHR$(&H53)                                    'PUSH BX

asm$ = asm$ + CHR$(&HBB) + CHR$(&HFF) + CHR$(&HFF)          'MOV BX,FFFF THE WHOLE MB
asm$ = asm$ + CHR$(&HB4) + CHR$(&H48)                       'MOV AH,48
asm$ = asm$ + CHR$(&HCD) + CHR$(&H21)                       'INT 21

asm$ = asm$ + CHR$(&H72) + CHR$(&H2)                        'JC +2
asm$ = asm$ + CHR$(&H31) + CHR$(&HDB)                       'XOR BX,BX IF NO ERROR
asm$ = asm$ + CHR$(&H89) + CHR$(&HD8)                       'MOV AX,BX
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)           'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)                        'MOV [BX],AX 0=NO ERROR

asm$ = asm$ + CHR$(&H5B)                                    'POP BX
asm$ = asm$ + CHR$(&H58)                                    'POP AX
asm$ = asm$ + CHR$(&H5D)                                    'POP BP
asm$ = asm$ + CHR$(&HCA) + CHR$(&H2) + CHR$(&H0)            'RETF 2
'________________________________________
Codeoff% = SADD(asm$)
DEF SEG = VARSEG(asm$)
CALL ABSOLUTE(BX%, Codeoff%)
'________________________________________
DEF SEG
LOW.FREE% = BX%

END FUNCTION

FUNCTION XMM.DETECT%
'------------------------------------------------------------
'THIS FUNCTION RETURNS IF XMS MEMORY MANAGER INSTALLED

'STACKPASSING :AX%

'IN:

'OUT          :AX% =  &H0080 XMM INSTALLED

'ERRORRETURN  :AX% <> &H0080 XMM NOT INSTALLED( QBASIC:AX%=0)
'------------------------------------------------------------
asm$ = asm$ + CHR$(&H55)                           'PUSH BP
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)              'MOV BP,SP
asm$ = asm$ + CHR$(&H50)                           'PUSH AX
asm$ = asm$ + CHR$(&HB8) + CHR$(&H0) + CHR$(&H43)  'MOV AX,4300
asm$ = asm$ + CHR$(&HCD) + CHR$(&H2F)              'INT 2F
asm$ = asm$ + CHR$(&H3C) + CHR$(&H80)              'CMP AL,80
asm$ = asm$ + CHR$(&H74) + CHR$(&H2)               'JZ 2
asm$ = asm$ + CHR$(&H31) + CHR$(&HC0)              'XOR AX,AX
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)  'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)               'MOV [BX],AX

asm$ = asm$ + CHR$(&H58)                           'POP AX
asm$ = asm$ + CHR$(&H5D)                           'POP BP
asm$ = asm$ + CHR$(&HCA) + CHR$(&H2) + CHR$(&H0)   'RETF 2
'________________________________________
Codeoff% = SADD(asm$)
DEF SEG = VARSEG(asm$)
CALL ABSOLUTE(ax%, Codeoff%)
'________________________________________
DEF SEG
XMM.DETECT% = ax%
END FUNCTION

SUB XMS.ENTRIE (ES%, BX%)
'--------------------------------------------------
'THIS FUNCTION DETECTS DRIVER ENTRIE,
'THE FREE XMS SIZE AND TOTAL XMS SIZE

'STACK PASSING : ES%,BX%

'IN            :

'OUT           : ES%= DRIVER ENTRIE SEGMENT
'                BX%= DRIVER ENTRIE OFFSET

'--------------------------------------------------
asm$ = asm$ + CHR$(&H55)                             'PUSH BP
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)                'MOV BP,SP
asm$ = asm$ + CHR$(&H50)                             'PUSH AX
asm$ = asm$ + CHR$(&H53)                             'PUSH BX
asm$ = asm$ + CHR$(&H6)                              'PUSH ES

asm$ = asm$ + CHR$(&HB8) + CHR$(&H10) + CHR$(&H43)   'MOV AX,4310
asm$ = asm$ + CHR$(&HCD) + CHR$(&H2F)                'INT 2F GET DRIVER ENTRIE
asm$ = asm$ + CHR$(&H89) + CHR$(&HD8)                'MOV AX,BX (NEED TO STORE BX)
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8)    'MOV BX,[BP+08]
asm$ = asm$ + CHR$(&H8C) + CHR$(&H7)                 'MOV [BX],ES SEG DRIVER
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)    'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)                 'MOV [BX],AX OFF DRIVER

asm$ = asm$ + CHR$(&H7)                              'POP ES
asm$ = asm$ + CHR$(&H5B)                             'POP BX
asm$ = asm$ + CHR$(&H58)                             'POP AX
asm$ = asm$ + CHR$(&H5D)                             'POP BP
asm$ = asm$ + CHR$(&HCA) + CHR$(&H4) + CHR$(&H0)     'RETF 4
'________________________________________
Codeoff% = SADD(asm$)
DEF SEG = VARSEG(asm$)
CALL ABSOLUTE(ES%, BX%, Codeoff%)

'________________________________________
DEF SEG

END SUB

FUNCTION XMS.FREE$ (ES%, BX%)
'----------------------------------------------------------------
'THIS FUNCTION RETURNS IF FREE AND TOTAL XMS MEMORY

'STACKPASSING :AX%,DX%

'IN:          :ES[BX] DRIVER ENTRIE

'OUT          :AX% =  FREE XMS
'              DX% =  TOTAL XMS

'----------------------------------------------------------------
'INIT
'-----
ES$ = INT2STR$(ES%): BX$ = INT2STR$(BX%)
'----------------------------------------------------------------
asm$ = ""
asm$ = asm$ + CHR$(&H55)                            'PUSH BP
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)               'MOV BP,SP
asm$ = asm$ + CHR$(&H50)                            'PUSH AX
asm$ = asm$ + CHR$(&H53)                            'PUSH BX
asm$ = asm$ + CHR$(&H52)                            'PUSH DX

asm$ = asm$ + CHR$(&HB4) + CHR$(&H8)                'MOV AH,0800 QUERY FREE XMS
asm$ = asm$ + CHR$(&H9A) + BX$ + ES$                'CALL DRIVER
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)   'MOV BX,[BP+06]
asm$ = asm$ + CHR$(&H89) + CHR$(&H17)               'MOV [BX],DX
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8)   'MOV BX,[BP+08]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)                'MOV [BX],AX

asm$ = asm$ + CHR$(&H5A)                            'POP DX
asm$ = asm$ + CHR$(&H5B)                            'POP BX
asm$ = asm$ + CHR$(&H58)                            'POP AX
asm$ = asm$ + CHR$(&H5D)                            'POP BP
asm$ = asm$ + CHR$(&HCA) + CHR$(&H4) + CHR$(&H0)    'RETF 4



XMS.FREE$ = asm$
END FUNCTION

SUB xms.version (ES%, BX%)
'----------------------------------------------------------------
'THIS FUNCTION RETURNS THE XMSVERSION and HMA AVAILABILITY

'STACKPASSING :AX%,DX%

'IN:          :ES[BX] DRIVER ENTRIE

'OUT          :AX% =  XMS VERSION
'              DX% =  0 HMA unavailable
'                       HMA exists

'QBASIC       :ES% = XMS version
'              BX% = HMA exists
'----------------------------------------------------------------
'INIT
'-----
ES$ = INT2STR$(ES%): BX$ = INT2STR$(BX%)
'----------------------------------------------------------------
asm$ = asm$ + CHR$(&H55)                              'push bp
asm$ = asm$ + CHR$(&H89) + CHR$(&HE5)                 'mov bp,sp

asm$ = asm$ + CHR$(&HB4) + CHR$(&H0)                  'mov ah,0
asm$ = asm$ + CHR$(&H9A) + BX$ + ES$                  'CALL DRIVER
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8)     'mov bx,[bp+08]
asm$ = asm$ + CHR$(&H89) + CHR$(&H7)                  'mov [bx],ax
asm$ = asm$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)     'mov bx,[bp+06]
asm$ = asm$ + CHR$(&H89) + CHR$(&H17)                 'mov [bx],dx
asm$ = asm$ + CHR$(&H5D)                              'pop bp
asm$ = asm$ + CHR$(&HCA) + CHR$(&H4) + CHR$(&H0)      'retf 4
'________________________________________
Codeoff% = SADD(asm$)
DEF SEG = VARSEG(asm$)
CALL ABSOLUTE(ax%, dx%, Codeoff%)
'________________________________________
DEF SEG
ES% = ax%: BX% = dx%

END SUB

