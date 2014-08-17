;SWMP adapted to QuickBasic 4.5 by Denis Andrianov.
;MODPLAY.LIB was written by Lord Excess.

INCLUDELIB      MODPLAY.LIB
EXTRN           Mod_Driver:FAR,Mod_End_Seg:FAR

;============================================================================
.model medium,basic
.stack 20h
.code

;============================================================================;DECLARE FUNCTION ModInitSound%

public ModInitSound
ModInitSound proc basic

                push    ds
                push    es
                xor     ax,ax
                xor     bx,bx
                call    far ptr Mod_Driver     ;Detect and init the driver
                or      ax,ax
                jz      short InitError
NoInitError:    call    far ptr [Mod_End_Seg]   
                mov     bx,ax
                pop     ax
                mov     es,ax
                sub     bx,ax                  ;BX=prog length in paragraphs
                mov     ah,4ah
                int     21h                    ;Set memory control block
                jmp     short EndInit

InitError:      pop es

EndInit:        pop ds
                ret

ModInitSound endp

;============================================================================;DECLARE SUB ModCloseSound ()

public ModCloseSound
ModCloseSound proc basic

                mov     bx,1
                call    FAR PTR Mod_Driver 
                ret

ModCloseSound endp

;============================================================================;DECLARE FUNCTION ModLoadModule% (BYVAL FileSegment%, BYVAL FileOffset%)

public ModLoadModule
ModLoadModule proc basic

                push    ds
                push    bp
                mov     bp,sp
                mov     bx,2
                mov     ds,[bp+10]
                mov     dx,[bp+8]
                call    FAR PTR Mod_Driver      ;Load module
                or      ax,ax
                jnz     EndLoadModule

                mov     bx,1                    ;Error does occur
                call    FAR PTR Mod_Driver

EndLoadModule:  pop bp
                pop ds
                ret 4

ModLoadModule endp

;============================================================================;DECLARE SUB ModPlayModule ()

public ModPlayModule
ModPlayModule proc basic

                mov     bx,3
                mov     ax,1
                call    FAR PTR Mod_Driver  
                ret

ModPlayModule endp

;============================================================================;DECLARE SUB ModStopModule ()

public ModStopModule
ModStopModule proc basic

                mov     bx,4
                call    FAR PTR Mod_Driver   
                ret

ModStopModule endp

;============================================================================

end
