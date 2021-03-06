MODEL MEDIUM,BASIC
.STACK 20H
.386
.CODE

;FUNCTION RELSIZE(BYVAL X1,BYVAL Y1,BYVAL X2,BYVAL Y2)
;STACK
;X1             =12
;Y1             =10
;X2             =8
;Y2             =6
;QB RETURN SEG  =4
;QB RETURN OFF  =2
;BP             =0

PUBLIC RELSIZE
RELSIZE PROC

PUSH BP

MOV BP,SP
MOV AX,[BP+12]          ;X1
MOV BX,[BP+10]          ;Y1
MOV CX,[BP+8]           ;X2
MOV DX,[BP+6]           ;Y2
CMP AX,CX
JLE LBL_NO_SWAP_X
        XCHG AX,CX
LBL_NO_SWAP_X:

CMP BX,DX
JLE LBL_NO_SWAP_Y
        XCHG BX,DX
LBL_NO_SWAP_Y:

SUB CX,AX
INC CX          ;DELTA X
SUB DX,BX
INC DX          ;DELTA Y
MOV AX,CX       ;MOV CX TO AX FOR MULTIPLICATION

MUL DX          ;AX=((X2-X1)+1)*((Y2-Y1)+1)
ADD AX,4        ;WIDTH*8 AND HEIGHT
SHR AX,1

POP BP
RET 8

RELSIZE ENDP

END



