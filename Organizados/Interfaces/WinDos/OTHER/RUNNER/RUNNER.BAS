'Copyright (C) HiTech Laboratories, Denis Andrianov
'Runner module for Desktop and Disktop
'============================================================================


'**********************
'Generate a COM file.
'**********************

$COM



'**********************
'Set up the strings.
'**********************

DIM Program$        * 170    'Max program path = 79 chars, but in some cases
                             'Program$ = Program$ + CommandLine$ !
DIM CommandLine$    * 90     'Max chars passed by RUNNER as CmdLine = 63, but 
                             'this prog can receive up to ~83 chars via DOS!

DIM ExecuteProgram$ * 80     'Must be set up before using the EXECUTE
DIM ExecuteCMDLine$ * 90     'procedure.

DIM EnvVar$         * 8
DIM CommandCom$     * 80

DIM Something$      * 100
DIM Value$          * 90

DIM WindosPath$     * 70      'Max possible path = 66 chars!
DIM Batch$          * 80
DIM PrgmPath$       * 70

DIM PressKey$       * 5
DIM GFXMode$        * 5
DIM Extension$      * 3


'*****************************************
'Decide whether to run Disktop or Desktop.
'*****************************************

CommandLine$ = COMMAND$
CommandLine$ = TRIM$(CommandLine$)
CommandLine$ = UCASE$(CommandLine$)
LenCommandLine% = LEN(CommandLine$)

  IF LenCommandLine% = 0 THEN
  END
  END IF

  IF CommandLine$ = "/DISKTOP" THEN
  ExpCall% = 1
  END IF

  IF CommandLine$ = "/DESKTOP" THEN
  ExpCall% = 2
  END IF


'***********************************************************
'Find the location of COMMAND.COM by reading the environment
'variable table and placing the path in CommandCom$.
'***********************************************************

CONST EnvVarLen = 8
EnvVar$ = "COMSPEC="
EnvError = 0
EnvZeroCount = 0




$ASM
push  ds
push  es

mov   dx,cs:[2ch]                   ;Save the environment seg in dx.
xor   si,si                         ;Offset within the table = 0.


BeginEnvSearch:

xor   bx,bx                         ;Position within EnvVar$ = 0 (BL)
                                    ;Difference flag = 0 (BH)
push  si
push  si
mov   cx,EnvVarLen
cld


VerifyNextChar:

  pop   si
  mov   ds,dx                       ;DS:SI point to the environment.
  lodsb
  push  si

  cmp   al,0
  jne   CopyCharEnvVar
  inc   CS:EnvZeroCount
  cmp   CS:EnvZeroCount,2
  je    EnvironNotFound
  jmp   VerifyNextChar


CopyCharEnvVar:

  mov   CS:EnvZeroCount, 0          ;Reset char 0 counter.
  mov   ah,al                       ;Save environment char in ah.

  push  cs
  pop   ds
  mov   si,offset EnvVar$           ;DS:SI point to the EnvVar$ string.
  push  dx
  xor   dh,dh
  mov   dl,bl
  add   si,dx                       ;Set SI to correct position.
  pop   dx
  lodsb
  inc   bl

  cmp   ah,al
  je    SameChar
  mov   bh,1

SameChar:
  loop  VerifyNextChar


cmp   bh,0
je    EnvironFound

pop   si
pop   si
inc   si
mov   CS:EnvZeroCount,0
jmp   BeginEnvSearch



EnvironFound:
pop   si
pop   ax
mov   ds,dx                   ;DS:SI point to the COMMAND.COM string.

push  cs
pop   es
mov   di,offset CommandCom$   ;ES:DI point to the destination string.


CopyCommandPath:
lodsb
or    al,al
jz    CopiedCommandPath       ;If CHR$(0) is found then exit loop.
stosb
loop  CopyCommandPath


EnvironNotFound:
pop   ax
pop   ax
mov   CS:EnvError,1


CopiedCommandPath:
pop   es
pop   ds

$ASM



  IF EnvError THEN

  $ASM
  mov    ah, 09h
  mov    dx, offset CommandErrMsg$
  int    21h
  $ASM

  END

  $ASM
  CommandErrMsg$  db  'COMSPEC environment variable not found.', 13, 10, '$'
  $ASM

  END IF


'********************************
'Save DOS screen.
'********************************
Start:

  IF GFXMode% = 0 THEN
  ExecuteProgram$ = "SAVETEXT.COM"
  ExecuteCMDLine$ = ""
  Execute
  END IF


'********************************
'Determine the shell and run it.
'********************************
   
  IF ExpCall% = 1 THEN
    ExecuteProgram$ = "DI_MAIN.EXE"
    ASM call GFXMode12
  ELSE
    ExecuteProgram$ = "DE_MAIN.EXE"
  END IF

ExecuteCMDLine$ = ""
Execute


'**********************
'Read WINDOS.INI file.
'**********************

OPEN "windos.ini" FOR INPUT AS 1
  


  DO

  INPUT #1, Something$
      
    IF EOF = -1 THEN
    EXIT LOOP
    END IF

  Something$ = UCASE$(Something$)
  Equal% = INSTR(Something$, "=")

    IF Equal% > 0 THEN
    StrLength% = LEN(Something$)
    StrLength% = StrLength% - Equal%
    Equal% = Equal% + 1             
    Value$ = MID$(Something$, Equal%, StrLength%)
    Value$ = TRIM$(Value$)
    Value% = VAL(Value$)
    Equal% = Equal% - 1
    Equal% = Equal% - 1
    Something$ = LEFT$(Something$, Equal%)
    Something$ = TRIM$(Something$)
    END IF


    IF Something$ = "WINDOS" THEN
    WindosPath$ = Value$
    END IF

    IF Something$ = "BATCH" THEN
    Batch$ = Value$
    END IF

    IF Something$ = "EXPCALL" THEN
    ExpCall% = Value%
    END IF

  LOOP


CLOSE 1


'**********************************
'Check if to exit.
'**********************************

  IF ExpCall% = 0 THEN
  ASM call GFXMode3
  END
  END IF


'********************************
'Read the batch for program info.
'********************************

OPEN Batch$ FOR INPUT AS 1

INPUT #1, PrgmPath$
INPUT #1, Program$
INPUT #1, CommandLine$
INPUT #1, PressKey$
INPUT #1, GFXMode$

CLOSE 1


PressKey% = VAL(PressKey$)
GFXMode% = VAL(GFXMode$)


'***************************************
'Change path for the program to work in.
'***************************************

PrgmPath$ = UCASE$(PrgmPath$)
Drive% = ASC(PrgmPath$)
Drive% = Drive% - 65
CHDIR PrgmPath$
CHDRIVE Drive%


'***************************************
'Execute the program.
'***************************************

Extension$ = RIGHT$(Program$, 3)
Extension$ = UCASE$(Extension$)


  IF Extension$ = "BAT" THEN
    ExecuteProgram$ = CommandCom$
    Program$ = Program$ + " "
    Program$ = Program$ + CommandLine$
    Program$ = LEFT$(Program$, 60)
    ExecuteCMDLine$ = "/C " + Program$
  ELSE
    ExecuteProgram$ = Program$
    ExecuteCMDLine$ = CommandLine$
  END IF

  IF GFXMode% THEN
    ASM call GFXMode12
  ELSE
    ASM call GFXMode3
  END IF

Execute


'**************************************
'Change back to WINDOS directory.
'**************************************

Drive% = ASC(WindosPath$)
Drive% = Drive% - 65
CHDIR WindosPath$
CHDRIVE Drive%


  IF PressKey% THEN
  Temp% = GETKEY
  END IF


GOTO Start


'****************************************************************************
'****************************************************************************
'****************************************************************************
'****************************************************************************
'****************************************************************************



SUB Execute

LenProgram% = LEN(ExecuteProgram$)
LenCommandLine% = LEN(ExecuteCMDLine$)

$ASM
;*****************************
;Save segment registers.
;*****************************

push  ds
push  es


;*******************************************
;Adjust the strings by cutting with CHR$(0).
;*******************************************

mov   di,offset ExecuteProgram$
mov   ax,LenProgram                   
add   di,ax
mov   byte ptr cs:[di],0
mov   di,offset ExecuteCMDLine$
mov   ax,LenCommandLine
add   di,ax
mov   byte ptr cs:[di],0


;*********************************************
;Copy user string into the procedure's string.
;*********************************************

push  cs                              ;Source
pop   ds
mov   si,offset ExecuteCMDLine$

push  cs                              ;Destination
pop   es
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


;***********************************************
;Fill in the rest of CommandTail and Parameters.
;***********************************************

inc   dl                              ;Include first space char!
mov   bx,offset CommandTail
mov   byte ptr cs:[bx],dl             ;Command line length.

push  cs
pop   ax
mov   bx,offset Parameters
add   bx,4
mov   word ptr cs:[bx],ax             ;Fill in the correct segment.


;*****************************************
;Prepare registers and call the interrupt.
;*****************************************

mov   bx,offset Parameters            ;ES is already properly set!
mov   dx,offset ExecuteProgram$       ;DS is already properly set!

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

mov   bx,offset CommandTail+2         ;Skip length and first space char
                                 
mov   cx,63
mov   al,13

CleanChar:
mov   byte ptr cs:[bx],al
inc   bx
loop  CleanChar

;**********************************
;Exit function.
;**********************************

pop  ax                  ;Error code
pop  es
pop  ds

jmp  ExitExecute

;***********************************
;Function's data
;***********************************

CommandTail  db   64                  ;Length of command tail
             db   32,63 dup(13),13    ;Command tail text
Parameters   dw   0                   ;Environment block is passed from parent
             dw   offset CommandTail  ;Command tail offset
             dw   0                   ;Command tail segment
             dw   0,0                 ;Dummy FCB pointer 1
             dw   0,0                 ;Dummy FCB pointer 2

;***********************************

ExitExecute:

$ASM
END SUB


'/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
'\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

$ASM
GFXMode12 PROC NEAR

  mov    ax, 12h
  int    10h
  ret

GFXMode12 ENDP
$ASM


'/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
'\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

$ASM
GFXMode3 PROC NEAR

  mov    ax, 03h
  int    10h
  ret

GFXMode3 ENDP
$ASM


