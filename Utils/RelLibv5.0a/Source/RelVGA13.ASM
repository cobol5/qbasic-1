MODEL MEDIUM, BASIC
.STACK 20H
.386
.CODE

;SUB RELINITVGA13
;STACK
;QB RET SEG     =2
;QB RET OFF     =0

PUBLIC RELINITVGA13
RELINITVGA13 PROC

MOV AX,0013H              
INT 10H                 ;SWITCH TO QB SCREEN 13
RET 

RELINITVGA13 ENDP
END





