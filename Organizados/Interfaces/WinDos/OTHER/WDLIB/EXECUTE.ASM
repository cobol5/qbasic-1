;Copyright (C) HiTech Laboratories, Denis Andrianov
;=============================================================================

.model medium, basic
.286
.code

;=============================================================================
;DECLARE FUNCTION Execute% (BYVAL PathSeg%, BYVAL PathOff%, BYVAL CommandSeg%, BYVAL CommandOff%)

;ProgramPath$ and CommandLine$ should be terminated with CHR$(0) !!!
;Only the first 63 characters of CommandLine$ will be used !!!

;Function returns:
;0 = OK
;1 = invalid function
;2 = file not found
;3 = path not found
;4 = too many files open
;5 = access denied
;8 = insufficient memory
;10 = invalid environment block
;11 = invalid format

;Stack frame:
;CommandOff%    bp+6
;CommandSeg%      +8
;PathOff%         +10
;PathSeg%         +12




PUBLIC Execute
Execute PROC

push  bp
mov   bp,sp

push  ds
push  es
push  si
push  di

;**********************************
;Copy data into our CommandTail.
;**********************************

mov   ax,[bp+8]                       ;Source
mov   ds,ax
mov   si,[bp+6]

mov   ax,seg CommandTail              ;Destination
mov   es,ax
mov   di,offset CommandTail+2         ;Skip length and first space char

cld                                   ;Set SI, DI increment
mov   cx,63                           ;Max 63 chars
xor   dl,dl                           ;Char counter

CopyCommandLine:
lodsb
or    al,al
jz    short CopiedCommandLine         ;If CHR$(0) is found then exit loop
stosb
inc   dl
loop  CopyCommandLine

CopiedCommandLine:

inc   dl                              ;Include first space char!
mov   bx,offset CommandTail
mov   byte ptr cs:[bx],dl

;*****************************************
;Prepare registers and call the interrupt.
;*****************************************

mov   ax,seg Parameters
mov   es,ax
mov   bx,offset Parameters

mov   ax,[bp+12]
mov   ds,ax
mov   dx,[bp+10]

mov   ax,4b00h                        ;Load and execute program
int   21h

;*****************************************************
;Test for errors, adjust function's value accordingly.
;*****************************************************

jc    short ExecuteError              ;Jump if error, error code in ax
xor   ax,ax                           ;Else set function to 0

ExecuteError:
push  ax                              ;Save error code

;**********************************
;Clean the rubbish (Command Tail).
;**********************************

mov   ax,seg CommandTail              ;Destination
mov   ds,ax
mov   bx,offset CommandTail+2         ;Skip length and first space char

mov   cx,63
mov   al,13

CleanChar:
mov   byte ptr ds:[bx],al
inc   bx
loop  CleanChar

;**********************************
;Exit function.
;**********************************

pop   ax
pop   di
pop   si
pop   es
pop   ds
pop   bp

ret   8


;----------------------------------
;Function's data

CommandTail  db   64                  ;Length of command tail
             db   32,63 dup(13),13    ;Command tail text
Parameters   dw   0                   ;Environment block is passed from parent
             dw   offset CommandTail  ;Command tail offset
             dw   seg CommandTail     ;Command tail segment
             dw   0,0                 ;Dummy FCB pointer 1
             dw   0,0                 ;Dummy FCB pointer 2


Execute ENDP

;=============================================================================

END
