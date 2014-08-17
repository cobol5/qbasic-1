.MODEL MEDIUM, BASIC
.386
.STACK 200H
        XMSBLOCKSTRUCT  STRUC
                BYTES           DD ?            ;IN BYTES
                SHANDLE         DW ?            ;SOURCE HANDLE
                SOFFSET         DD ?            ;SOURCE OFFSET
                DHANDLE         DW ?            ;DEST   HANDLE
                DOFFSET         DD ?            ;DEST   OFFSET
        XMSBLOCKSTRUCT  ENDS
        EXTRN   RELSPRITE:FAR
        EXTRN   RELSPRITESOLID:FAR
        EXTRN   RELSPRITETRANS:FAR
        EXTRN   RELSPRITEGAMMA:FAR
        EXTRN   RELSPRITECOLOR:FAR
.DATA
        ALIGN 2
        XMSDUDE         DD ?               ;XMS CONTROL DRIVER ADDRESS
        XMSBLOCK        XMSBLOCKSTRUCT <>  ;XMS MEMMOVE STRUCT     
.CODE
PUBLIC RELXMSINIT
PUBLIC RELXMSGETFREE
PUBLIC RELXMSGETTOTAL
PUBLIC RELXMSALLOCATE
PUBLIC RELXMSDEALLOCATE
PUBLIC RELXMSFROMBASE
PUBLIC RELXMSTOBASE
PUBLIC RELXMSPUT
PUBLIC RELXMSPUTSOLID

;============================================================================
;RELXMSINIT
;DECLARE FUNCTION RELXMSINIT%()
;DES: CHECKS IF AN XMS DRIVER IS INSTALLED AND INITIALIZES IT IF FOUND :*)
;RETURNS:
;-1 IF AN XMS DRIVER IS INSTALLED 0 IF OTHERWISE
;============================================================================
RELXMSINIT PROC
        MOV AX,4300H                    ;LIM SPECS :*)
        INT 2FH                         ;STANDARD INT
        CMP AL,80H                      ;WILL BE 80H IF THERE IS AN XMS DRIVER
        JNE LBL_NOXMSDRIVER             ;NO DRIVER PRESENT
        MOV AX,4310H                    ;GET ADDRESS OF XMSDUDE
        INT 2FH                         ;ES:BX(ADDRESS RETURNED
        MOV WORD PTR [XMSDUDE],BX       ;BLAST THE DAMN VALUES
        MOV WORD PTR [XMSDUDE+2],ES     ;INTEL'S BACKWARD DWORD NOTATION
        MOV AL,-1
        SUB AH,AH
        RET
LBL_NOXMSDRIVER:
SUB AX,AX
RET
RELXMSINIT ENDP

;============================================================================
;RELXMSGETFREE
;DECLARE FUNCTION RELXMSGETFREE&()
;DES: QUERIES FREE EXTENDED MEMORY
;RETURNS:
;A LONG INTEGER OF LARGEST FREE XMS MEM IN KB
;============================================================================

RELXMSGETFREE PROC
        MOV AX,0800H            ;AH=08H
        CALL [XMSDUDE]
        XOR DX,DX               ;CLEAR TOTAL
        RET                
RELXMSGETFREE ENDP

;============================================================================
;RELXMSGETTOTAL
;DECLARE FUNCTION RELXMSGETTOTAL&()
;DES: QUERIES FREE EXTENDED MEMORY
;RETURNS:
;A LONG INTEGER OF TOTAL FREE XMS MEM IN KB
;============================================================================

RELXMSGETTOTAL PROC
        MOV AX,0800H            ;AH=08H
        CALL [XMSDUDE]
        MOV AX,DX               ;MOV THE DX TO AX SINCE QB EXPECTS IT
        XOR DX,DX
        RET                
RELXMSGETTOTAL ENDP

;============================================================================
;RELXMSALLOCATE
;DECLARE FUNCTION RELXMSALLOCATE%(KB&)
;PARAMS:AMOUNT IN KB TO ALLOCATE(LONG INT)
;DES: ALLOCATES AN XMS MEMORY FOR USE
;RETURNS:
;THE XMS HANDLE OF THE XMSBLOCK(SHORT INT)
;STACK
;KB&            =4
;CS             =2
;IP             =0
;============================================================================

RELXMSALLOCATE PROC
        MOV CX,BP               ;USE CX SINCE ITS THE ONLY REG FREE AFTER
                                ;CALL TO XMS DUDE
        MOV BP,SP
        MOV DX,[BP+4]           ;AMOUNT IN KB
        MOV AX,0900H            ;FUNCTION NUMBER
                                ;ON RETURN:
                                ;DX=XMS HANDLE(16 BIT)
                                ;AX=ERRORFLAG
        CALL [XMSDUDE]
        OR AX,AX                ;CHECK IF UNSUCCESSFUL
        JZ LBL_XMS_ALLOC_FAIL   ;0=FAIL
        MOV AX,DX               ;PUT HANDLE TO AX
        MOV BP,CX
        RET 4
LBL_XMS_ALLOC_FAIL:
MOV BP,CX
RET 4
RELXMSALLOCATE ENDP

;============================================================================
;RELXMSDEALLOCATE
;DECLARE SUB RELXMSDEALLOCATE()
;PARAMS:XMSHANDLE(SHORT INT)
;DES: ALLOCATES AN XMS MEMORY FOR USE
;RETURNS:NONE
;STACK
;HANDLE         =4
;CS             =2
;IP             =0
;============================================================================

RELXMSDEALLOCATE PROC
        MOV CX,BP               ;USE CX SINCE ITS THE ONLY REG FREE AFTER
                                ;CALL TO XMS DUDE
        MOV BP,SP
        MOV DX,[BP+4]           ;HANDLE
        MOV AX,0A00H            ;FUNCTION NUMBER
                                ;ON RETURN:
                                ;DX=XMS HANDLE(16 BIT)
                                ;AX=ERRORFLAG
        CALL [XMSDUDE]
        MOV BP,CX
        RET 2
RELXMSDEALLOCATE ENDP


;============================================================================
;RELXMSFROMBASE
;DECLARE SUB RELXMSFROMBASE (BYVAL BaseSEG%, BYVAL BaseOFF&, BYVAL XmsHandle%,
; _BYVAL XMSOffset&, BYVAL Length&)
;PARAMS:
        ;BaseSEG=BaseLayer SEGment(Varseg) Short Int
        ;BaseOFF=Baselayer OFFset(Varptr) Long Int as QB wraps things
        ;XmsHandle=Handle returned by RelXMSAllocate (Short Int)
        ;XmsOffSet=32 bit Offset in XMS data (Long Int)
        ;Length=32 bit Length(IN BYTES) of data to copy(Long Int)
;DES: COPIES A BLOCK OF DATA FROM BASE/CONVENTIONAL MEMORY TO XMS
;RETURNS:NONE
;STACK
;BaseSEG        =18     >20
;BaseOFF        =14     >18
;XmsHandle      =12     >14
;XmsOff         =8      >12
;Length         =4      >8
;CS             =2
;IP             =0
;============================================================================

RELXMSFROMBASE PROC
  MOV CX,BP                     ;SAVE BP
  MOV DX,DS                     ;SAVE DS
  MOV BP,SP
  XOR AX,AX                     ;MUST BE 0
  MOV XMSBLOCK.SHandle, AX      ;TO TREAT IT AS SEG:OFF PAIR
  MOV EAX, [bp+16]              ;BASE SEGMENTOFFSET(32 BIT)
  MOV XMSBLOCK.SOffset, EAX     ;REDUNDANT BUT FASTER
  MOV AX, [bp+12]               ;XMS HANDLE(16 BIT)
  MOV XMSBLOCK.DHandle, AX      
  MOV EAX, [BP+8]               ;32 BIT XMS OFFSET
  MOV XMSBLOCK.DOffset, EAX
  MOV EBX, [BP+4]
  MOV XMSBLOCK.BYTES, EBX
  MOV AX, SEG XMSBLOCK
  MOV DS, AX
  MOV SI, OFFSET XMSBLOCK
  MOV AX, 0B00H
  CALL [XMSDUDE]
  MOV DS,DX
  MOV BP,CX
  RET 16

RELXMSFROMBASE ENDP

;============================================================================
;RELXMSTOBASE
;DECLARE SUB RELXMSTOBASE (BYVAL BaseSEG%, BYVAL BaseOFF&, BYVAL XmsHandle%,
; _BYVAL XMSOffset&, BYVAL Length&)
;PARAMS:
        ;BaseSEG=BaseLayer SEGment(Varseg) Short Int
        ;BaseOFF=Baselayer OFFset(Varptr) Long Int as QB wraps things
        ;XmsHandle=Handle returned by RelXMSAllocate (Short Int)
        ;XmsOffSet=32 bit Offset in XMS data (Long Int)
        ;Length=32 bit Length(IN BYTES) of data to copy(Long Int)
;DES: COPIES A BLOCK OF DATA FROM BASE/CONVENTIONAL MEMORY TO XMS
;RETURNS:NONE
;STACK
;BaseSEG        =18     >20
;BaseOFF        =14     >18
;XmsHandle      =12     >14
;XmsOff         =8      >12
;Length         =4      >8
;CS             =2
;IP             =0
;============================================================================

RELXMSTOBASE PROC
  MOV CX,BP                     ;SAVE BP
  MOV DX,DS                     ;SAVE DS
  MOV BP,SP
  XOR AX,AX                     ;MUST BE 0
  MOV XMSBLOCK.DHandle, AX      ;TO TREAT IT AS SEG:OFF PAIR
  MOV EAX, [BP+16]              ;BASE SEGMENT:OFFSET(32 BIT)
  MOV XMSBLOCK.DOffset, EAX     ;REDUNDANT BUT FASTER
  MOV AX, [BP+12]               ;XMS HANDLE(16 BIT)
  MOV XMSBLOCK.SHandle, AX      
  MOV EAX, [BP+8]               ;32 BIT XMS OFFSET
  MOV XMSBLOCK.SOffset, EAX
  MOV EBX, [BP+4]
  MOV XMSBLOCK.BYTES, EBX
  MOV AX, SEG XMSBLOCK
  MOV DS, AX
  MOV SI, OFFSET XMSBLOCK
  MOV AX, 0B00H
  CALL [XMSDUDE]
  MOV DS,DX
  MOV BP,CX
  RET 16

RELXMSTOBASE ENDP


;============================================================================
;RELXMSPUT
;DECLARE SUB RELXMSPUT (BYVAL LayerSEG%, BYVAL X%, BYVAL Y%,
;_BYVAL SPRITESEGMENT%,BYVAL SPRITEOFFSET%, BYVAL XMSHANDLE%,BYVAL DUMMYSEG%)
;PARAMS:
        ;LayerSEG=BaseLayer SEGment(Varseg) Short Int
        ;X%=X coord of Sprite
        ;Y%=Y coord of Sprite
        ;SpriteSegment=Varseg of sprite Array
        ;Spriteoffset=Varptr of sprite Array
        ;XmsHandle=Handle returned by RelXMSAllocate (Short Int)
        ;Dummy=Intermediary buffer to move XMS data to mem then
;DES: Puts a sprite to layer using data from XMS thereby conserving memory.
;RETURNS:NONE
;STACK
;LayerSEG       =20
;X              =18
;Y              =16
;SprSeg         =14
;Sproffset      =12
;XMShandle      =10
;DummySeg       =8
;CS             =6
;IP             =4
;DS             =2
;BP             =0
;============================================================================


RELXMSPUT PROC
          PUSH DS                       ;SAVE DS
          PUSH BP                       ;SAVE BP
          MOV BP,SP
          XOR AX,AX                     ;MUST BE 0
          MOV XMSBLOCK.DHandle, AX      ;TO TREAT IT AS SEG:OFF PAIR
          MOV AX, [BP+8]                ;BASE SEGMENT:OFFSET(32 BIT)
          SHL EAX,16                    ;PUT SEG AT HI WORD
          SUB AX,AX                     ;ZERO OUT DEST OFFSET
          MOV XMSBLOCK.DOffset, EAX     ;REDUNDANT BUT FASTER
          MOV AX, [BP+10]               ;XMS HANDLE(16 BIT)
          MOV XMSBLOCK.SHandle, AX

          ;DO SPRITE STUFF
          MOV AX,[BP+14]                ;SPRITE SEG
          MOV ES,AX
          MOV DI,[BP+12]                ;SPR OFFSET
          MOV BX,ES:[DI]                ;WIDTH*8
          SHR BX,3                      ;DIV BY 8
          ;DEC BX                        ;CORRECT 0 BASED WIDTH
          MOV AX,ES:[DI+2]              ;HEIGHT
          ;DEC AX                        ;DO AS WIDTH

          MUL BX                        ;CALC LENGTH
                                        ;DX:AX=PRODUCT
          MOV BX,AX                     ;SAVE AX
          MOV AX,DX
          SHL AX,16
          MOV AX,BX                     ;RESTORE AX      
          ADD EAX,4                     ;4 BYTE HEADER
                                        ;GET BYTE SIZE
                                        ;EAX=BYTELEN
          XOR EBX,EBX                   ;32 BIT XMS OFFSET
          MOV EBX,[BP+12]               ;SPR OFFSET
          MOV XMSBLOCK.SOffset, EBX
          MOV XMSBLOCK.BYTES, EAX       ;BYTES TO MOVE
          MOV AX, SEG XMSBLOCK
          MOV DS, AX
          MOV SI, OFFSET XMSBLOCK
          MOV AX, 0B00H
          CALL [XMSDUDE]
          ;DO A CALL TO RELSPRITE
          ;MOV AX,[BP+20]                ;LAYER SEG
          ;MOV BX,[BP+18]                ;X
          ;MOV CX,[BP+16]                ;Y
          ;MOV DX,[BP+14]                ;SPR SEG
          ;MOV FS,[BP+12]                ;SPR OFFSET
          ;PUSH AX
          ;PUSH BX
          ;PUSH CX
          ;PUSH DX
          ;PUSH FS

	  MOV DX,0
          MOV FS,DX                     ;DUMMY OFFSET
          MOV AX,[BP+20]                ;LAYER SEG
          MOV BX,[BP+18]                ;X
          MOV CX,[BP+16]                ;Y
          MOV DX,[BP+8]                 ;DUMMY SEG
          PUSH AX
          PUSH BX
          PUSH CX
          PUSH DX
          PUSH FS
          CALL RELSPRITE
          POP BP
          POP DS
          RET 14
RELXMSPUT ENDP

;============================================================================
;RELXMSPUTSOLID
;DECLARE SUB RELXMSPUTSOLID (BYVAL LayerSEG%, BYVAL X%, BYVAL Y%,
;_BYVAL SPRITESEGMENT%,BYVAL SPRITEOFFSET%, BYVAL XMSHANDLE%,BYVAL DUMMYSEG%)
;PARAMS:
        ;LayerSEG=BaseLayer SEGment(Varseg) Short Int
        ;X%=X coord of Sprite
        ;Y%=Y coord of Sprite
        ;SpriteSegment=Varseg of sprite Array
        ;Spriteoffset=Varptr of sprite Array
        ;XmsHandle=Handle returned by RelXMSAllocate (Short Int)
        ;Dummy=Intermediary buffer to move XMS data to mem then
;DES: Puts a sprite to layer using data from XMS thereby conserving memory.
;RETURNS:NONE
;STACK
;LayerSEG       =20
;X              =18
;Y              =16
;SprSeg         =14
;Sproffset      =12
;XMShandle      =10
;DummySeg       =8
;CS             =6
;IP             =4
;DS             =2
;BP             =0
;============================================================================


RELXMSPUTSOLID PROC
          PUSH DS                       ;SAVE DS
          PUSH BP                       ;SAVE BP
          MOV BP,SP
          XOR AX,AX                     ;MUST BE 0
          MOV XMSBLOCK.DHandle, AX      ;TO TREAT IT AS SEG:OFF PAIR
          MOV AX, [BP+8]                ;BASE SEGMENT:OFFSET(32 BIT)
          SHL EAX,16                    ;PUT SEG AT HI WORD
          SUB AX,AX                     ;ZERO OUT DEST OFFSET
          MOV XMSBLOCK.DOffset, EAX     ;REDUNDANT BUT FASTER
          MOV AX, [BP+10]               ;XMS HANDLE(16 BIT)
          MOV XMSBLOCK.SHandle, AX

          ;DO SPRITE STUFF
          MOV AX,[BP+14]                ;SPRITE SEG
          MOV ES,AX
          MOV DI,[BP+12]                ;SPR OFFSET
          MOV BX,ES:[DI]                ;WIDTH*8
          SHR BX,3                      ;DIV BY 8
          DEC BX                        ;CORRECT 0 BASED WIDTH
          MOV AX,ES:[DI+2]              ;HEIGHT
          DEC AX                        ;DO AS WIDTH
          MUL BX                        ;CALC HEIGHT
          ADD EAX,4                     ;2 BYTE HEADER
                                        ;GET BYTE SIZE
                                        ;EAX=BYTELEN
          XOR EBX,EBX                   ;32 BIT XMS OFFSET
          MOV EBX,[BP+12]               ;SPR OFFSET
          DEC EBX                       ;-1
          SHL EBX,1                     ;*2
          MOV XMSBLOCK.SOffset, EBX
          MOV XMSBLOCK.BYTES, EAX       ;BYTES TO MOVE
          MOV AX, SEG XMSBLOCK
          MOV DS, AX
          MOV SI, OFFSET XMSBLOCK
          MOV AX, 0B00H
          CALL [XMSDUDE]
          ;DO A CALL TO RELSPRITE
          MOV DX,0
          MOV FS,DX                     ;DUMMY OFFSET
          MOV AX,[BP+20]                ;LAYER SEG
          MOV BX,[BP+18]                ;X
          MOV CX,[BP+16]                ;Y
          MOV DX,[BP+8]                 ;DUMMY SEG
          PUSH AX
          PUSH BX
          PUSH CX
          PUSH DX
          PUSH FS
          CALL RELSPRITESOLID
          POP BP
          POP DS
          RET 14
RELXMSPUTSOLID ENDP


END





