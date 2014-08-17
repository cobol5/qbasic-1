;Copyright (C) HiTech Laboratories, Denis Andrianov
;=============================================================================
;
; DISABLE INT 10 FUNCTION
; Call with:
;
; AX = 0FFFEh    to restore normal functionality of INT 10
; AX = 0FFFFh    to disable all further calls to INT 10
; BX = 0FFFFh    in order for this function to be called
; INT 10
;
;
;

HiTech SEGMENT 'code'
       ASSUME CS:HiTech,DS:HiTech
       ORG 100h


Main PROC
       jmp Init



NewInt10 PROC

       push ax
       pushf

       cmp bx,0ffffh                ;Is the function being called?
       jne FurtherChecks            ;If not, continue with further checks.

       cmp ax,0ffffh                ;Disable INT 10?
       je  DisableInt               ;If yes, continue at DisableInt.

       cmp ax,0fffeh                ;Enable INT 10?
       je  EnableInt                ;If yes, continue at EnableInt.

       jmp FurtherChecks            ;If sub-function wasn't selected, go to
                                    ;FurtherChecks.

DisableInt:
       mov al,1                     ;Make DisableInt10 = 1
       mov CS:DisableInt10,al
       jmp FurtherChecks

EnableInt:
       xor al,al                    ;Make DisableInt10 = 0
       mov CS:DisableInt10,al

FurtherChecks:
       mov al,CS:DisableInt10
       cmp al,1
       je SkipInt10
       popf
       pop ax
       jmp CS:Int10

SkipInt10:
       popf
       pop ax
       iret

DisableInt10  DB  0
Int10         DD  0

NewInt10 ENDP

ResidentEnd=$
Main ENDP



Init PROC

       mov ax,3510h                 ;Save vector 10h.
       int 21h
       mov word ptr CS:Int10,BX     ;Save offset.
       mov word ptr CS:Int10+2,ES   ;Save segment.

       mov ax,2510h                 ;Hook vector 10h.
       mov dx,offset NewInt10       ;DS = segment, DX = offset
       int 21h

       mov ah,09h                   ;Print messages.
       mov dx,offset Copyright
       int 21h

       mov ah,09h
       mov dx,offset Loaded
       int 21h

       mov ax,3100h                 ;Terminate and stay resident.
       mov dx,(ResidentEnd-Main+10fh)/16
       int 21h

Init ENDP


Copyright DB 'Copyright (C) HiTech Laboratories, Denis Andrianov',13,13,10,'$'
Loaded    DB 'AntiINT10 has been loaded.',13,10,'$'


HiTech ENDS
       END Main
