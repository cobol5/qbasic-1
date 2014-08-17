;**********************************************************************
;   ASM Source Created Using BASM 6.5(c) 1998  by Kevin Diggins 
;**********************************************************************
;      File Name: RUNNER.ASM
;  Creation Date: 12-25-2002
;  Creation Time: 00:19:01
;**********************************************************************

.286p

MaxString Equ 81 

Code Segment Byte Public
Assume CS:Code,DS:Code,ES:Code
           Org 256
_StartProgram:

;**********************************************************************
;Get the command line
;**********************************************************************
Push ES                                     ;Save PSP
Push DS
Push DS
Mov  DI,0
Lea  BX,ES:[81h]
GetTail:
Mov  AL,ES:[BX]
Cmp  AL,13
Je   GotTail
Mov  Command$[DI],AL
Inc  DI
Inc  BX
Jmp  GetTail
GotTail:
Mov  Command$ [DI],0
Pop  DS
Pop  ES
Cld
Mov  SI,Offset Command$ + 1
Mov  DI,Offset Command$
Mov  CX,80
Rep  Movs Byte Ptr [DI],Byte Ptr ES:[SI]
Pop  ES                                     ;Restore PSP
;**********************************************************************
;                         Free Unused Memory
;**********************************************************************

Mov  BX,Offset _EndProgram
Add  BX,256              ;Room for our stack
Mov  SP,BX               ;Setup our stack
Shr  BX,4                ;Convert to paragraphs
Inc  BX                  ;Round up
Mov  AH,4Ah              ;Shrink mem
Int  21h                 ;Thru DOS
Push DS                  
Pop  AX                  
Mov  DefSeg,AX           ;DefSeg defaults to DATA
Mov  $DataSeg,AX         ;Set $DataSeg to DATA
;**********************************************************************
;                        Translation begins
;**********************************************************************
;CommandLine$ = COMMAND$ 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset COMMAND$
     Mov  DI,Offset COMMANDLINE$
     Call MoveStr
;**********************************************************************
;CommandLine$ = TRIM$ CommandLine$ 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset COMMANDLINE$
     Call Trim$
     Call Concat_2
     Mov  DI,Offset COMMANDLINE$
     Call Assign
;**********************************************************************
;CommandLine$ = UCASE$ CommandLine$ 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset COMMANDLINE$
     Mov  DI,Offset $_1_
     Call MoveStr
     Mov  AX,Offset $_1_
     Call Ucase$
     Call Concat_2
     Mov  DI,Offset COMMANDLINE$
     Call Assign
;**********************************************************************
;LenCommandLine = LEN CommandLine$ 
;**********************************************************************
     Xor  AX,AX
     Mov  $Var0,AX
     Mov  SI,Offset COMMANDLINE$
     Call Len
     Add  AX,$Var0
     Mov  LENCOMMANDLINE,AX
;**********************************************************************
;IF LenCommandLine = 0 THEN 
;**********************************************************************
     Cmp  LENCOMMANDLINE,0
     Je   _True_01
     Jmp  _EndIf_01
_True_01:
;**********************************************************************
;END 
;**********************************************************************
     Mov  AL,0
     Mov  AH,4Ch
     Int  21h
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_01:
;**********************************************************************
;IF CommandLine$ = "/DISKTOP" THEN 
;**********************************************************************
     Mov  SI,Offset COMMANDLINE$
     Mov  DI,Offset $tring_16
     Call StrCompare
     Je   _True_02
     Jmp  _EndIf_02
_True_02:
;**********************************************************************
;ExpCall = 1 
;**********************************************************************
     Mov  EXPCALL,1
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_02:
;**********************************************************************
;IF CommandLine$ = "/DESKTOP" THEN 
;**********************************************************************
     Mov  SI,Offset COMMANDLINE$
     Mov  DI,Offset $tring_18
     Call StrCompare
     Je   _True_03
     Jmp  _EndIf_03
_True_03:
;**********************************************************************
;ExpCall = 2 
;**********************************************************************
     Mov  EXPCALL,2
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_03:
     Envvarlen           EQU          8
;**********************************************************************
;EnvVar$ = "COMSPEC=" 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset $tring_19
     Mov  DI,Offset ENVVAR$
     Call MoveStr
;**********************************************************************
;EnvError = 0 
;**********************************************************************
     Mov  ENVERROR,0
;**********************************************************************
;EnvZeroCount = 0 
;**********************************************************************
     Mov  ENVZEROCOUNT,0
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
     
;**********************************************************************
;IF EnvError THEN 
;**********************************************************************
     Cmp  ENVERROR,0
     Jne  _True_04
     Jmp  _EndIf_04
_True_04:
     mov    ah, 09h
     mov    dx, offset CommandErrMsg$
     int    21h
;**********************************************************************
;END 
;**********************************************************************
     Mov  AL,0
     Mov  AH,4Ch
     Int  21h
     CommandErrMsg$  db  'COMSPEC environment variable not found.', 13, 10, '$'
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_04:
     
Start:
;**********************************************************************
;IF GFXMode = 0 THEN 
;**********************************************************************
     Cmp  GFXMODE,0
     Je   _True_05
     Jmp  _EndIf_05
_True_05:
;**********************************************************************
;ExecuteProgram$ = "SAVETEXT.COM" 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset $tring_23
     Mov  DI,Offset EXECUTEPROGRAM$
     Call MoveStr
;**********************************************************************
;ExecuteCMDLine$ = "" 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset $tring_24
     Mov  DI,Offset EXECUTECMDLINE$
     Call MoveStr
;**********************************************************************
;Execute 
;**********************************************************************
     Call Execute
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_05:
;**********************************************************************
;IF ExpCall = 1 THEN 
;**********************************************************************
     Cmp  EXPCALL,1
     Je   _True_06
     Jmp  _EndIf_06
_True_06:
;**********************************************************************
;ExecuteProgram$ = "DI_MAIN.EXE" 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset $tring_25
     Mov  DI,Offset EXECUTEPROGRAM$
     Call MoveStr
     call GFXMode12
;**********************************************************************
;ELSE 
;**********************************************************************
     Jmp _EndIf_06A
_EndIf_06:
;**********************************************************************
;ExecuteProgram$ = "DE_MAIN.EXE" 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset $tring_26
     Mov  DI,Offset EXECUTEPROGRAM$
     Call MoveStr
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_06A:
;**********************************************************************
;ExecuteCMDLine$ = "" 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset $tring_27
     Mov  DI,Offset EXECUTECMDLINE$
     Call MoveStr
;**********************************************************************
;Execute 
;**********************************************************************
     Call Execute
;**********************************************************************
;OPEN "windos.ini" FOR INPUT AS 1 
;**********************************************************************
     Mov  DX,Offset $tring_28
     Mov  BX,Offset HANDLE_1
     Call ReadFile
;**********************************************************************
;DO 
;**********************************************************************
DoLoop_01:
;**********************************************************************
;INPUT #1 Something$ 
;**********************************************************************
     
     Mov  SOMETHING$,0
     Mov  DI,Offset SOMETHING$
     Mov  AX,HANDLE_1
     Call DiskInput
;**********************************************************************
;IF EOF = -1 THEN 
;**********************************************************************
     Cmp  EOF,-1
     Je   _True_07
     Jmp  _EndIf_07
_True_07:
;**********************************************************************
;EXIT LOOP 
;**********************************************************************
     Jmp DoLoop_01A
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_07:
;**********************************************************************
;Something$ = UCASE$ Something$ 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset SOMETHING$
     Mov  DI,Offset $_1_
     Call MoveStr
     Mov  AX,Offset $_1_
     Call Ucase$
     Call Concat_2
     Mov  DI,Offset SOMETHING$
     Call Assign
;**********************************************************************
;Equal = INSTR Something$ "=" 
;**********************************************************************
     Xor  AX,AX
     Mov  $Var0,AX
     Mov  DI,Offset SOMETHING$
     Mov  SI,Offset $tring_30
     Call _Instr
     Add  AX,$Var0
     Mov  EQUAL,AX
;**********************************************************************
;IF Equal > 0 THEN 
;**********************************************************************
     Cmp  EQUAL,0
     Jg   _True_08
     Jmp  _EndIf_08
_True_08:
;**********************************************************************
;StrLength = LEN Something$ 
;**********************************************************************
     Xor  AX,AX
     Mov  $Var0,AX
     Mov  SI,Offset SOMETHING$
     Call Len
     Add  AX,$Var0
     Mov  STRLENGTH,AX
;**********************************************************************
;StrLength = StrLength - Equal 
;**********************************************************************
     Xor  AX,AX
     Add  AX,STRLENGTH
     Sub  AX,EQUAL
     Mov  STRLENGTH,AX
;**********************************************************************
;Equal = Equal + 1 
;**********************************************************************
     Inc  EQUAL
;**********************************************************************
;Value$ = MID$ Something$ Equal StrLength 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset SOMETHING$
     Mov  AX,EQUAL
     Mov  BX,STRLENGTH
     Call Mid$
     Call Concat_2
     Mov  DI,Offset VALUE$
     Call Assign
;**********************************************************************
;Value$ = TRIM$ Value$ 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset VALUE$
     Call Trim$
     Call Concat_2
     Mov  DI,Offset VALUE$
     Call Assign
;**********************************************************************
;Value = VAL Value$ 
;**********************************************************************
     Xor  AX,AX
     Mov  $Var0,AX
     Mov  DI,Offset VALUE$
     Call Val
     Add  AX,$Var0
     Mov  VALUE,AX
;**********************************************************************
;Equal = Equal -1 
;**********************************************************************
     Dec  EQUAL
;**********************************************************************
;Equal = Equal -1 
;**********************************************************************
     Dec  EQUAL
;**********************************************************************
;Something$ = LEFT$ Something$ Equal 
;**********************************************************************
     Call ClrWork
     Push EQUAL
     Push Offset SOMETHING$
     Call Left$
     Call Concat_2
     Mov  DI,Offset SOMETHING$
     Call Assign
;**********************************************************************
;Something$ = TRIM$ Something$ 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset SOMETHING$
     Call Trim$
     Call Concat_2
     Mov  DI,Offset SOMETHING$
     Call Assign
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_08:
;**********************************************************************
;IF Something$ = "WINDOS" THEN 
;**********************************************************************
     Mov  SI,Offset SOMETHING$
     Mov  DI,Offset $tring_34
     Call StrCompare
     Je   _True_09
     Jmp  _EndIf_09
_True_09:
;**********************************************************************
;WindosPath$ = Value$ 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset VALUE$
     Mov  DI,Offset WINDOSPATH$
     Call MoveStr
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_09:
;**********************************************************************
;IF Something$ = "BATCH" THEN 
;**********************************************************************
     Mov  SI,Offset SOMETHING$
     Mov  DI,Offset $tring_35
     Call StrCompare
     Je   _True_10
     Jmp  _EndIf_10
_True_10:
;**********************************************************************
;Batch$ = Value$ 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset VALUE$
     Mov  DI,Offset BATCH$
     Call MoveStr
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_10:
;**********************************************************************
;IF Something$ = "EXPCALL" THEN 
;**********************************************************************
     Mov  SI,Offset SOMETHING$
     Mov  DI,Offset $tring_36
     Call StrCompare
     Je   _True_11
     Jmp  _EndIf_11
_True_11:
;**********************************************************************
;ExpCall = Value 
;**********************************************************************
     Mov  AX,VALUE
     Mov  EXPCALL,AX
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_11:
;**********************************************************************
;LOOP 
;**********************************************************************
     Jmp DoLoop_01
     
DoLoop_01A:
;**********************************************************************
;CLOSE 1 
;**********************************************************************
     Mov  BX,HANDLE_1
     Call CloseFile
;**********************************************************************
;IF ExpCall = 0 THEN 
;**********************************************************************
     Cmp  EXPCALL,0
     Je   _True_12
     Jmp  _EndIf_12
_True_12:
     call GFXMode3
;**********************************************************************
;END 
;**********************************************************************
     Mov  AL,0
     Mov  AH,4Ch
     Int  21h
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_12:
;**********************************************************************
;OPEN Batch$ FOR INPUT AS 1 
;**********************************************************************
     Mov  DX,Offset BATCH$
     Mov  BX,Offset HANDLE_1
     Call ReadFile
;**********************************************************************
;INPUT #1 PrgmPath$ 
;**********************************************************************
     
     Mov  PRGMPATH$,0
     Mov  DI,Offset PRGMPATH$
     Mov  AX,HANDLE_1
     Call DiskInput
;**********************************************************************
;INPUT #1 Program$ 
;**********************************************************************
     
     Mov  PROGRAM$,0
     Mov  DI,Offset PROGRAM$
     Mov  AX,HANDLE_1
     Call DiskInput
;**********************************************************************
;INPUT #1 CommandLine$ 
;**********************************************************************
     
     Mov  COMMANDLINE$,0
     Mov  DI,Offset COMMANDLINE$
     Mov  AX,HANDLE_1
     Call DiskInput
;**********************************************************************
;INPUT #1 PressKey$ 
;**********************************************************************
     
     Mov  PRESSKEY$,0
     Mov  DI,Offset PRESSKEY$
     Mov  AX,HANDLE_1
     Call DiskInput
;**********************************************************************
;INPUT #1 GFXMode$ 
;**********************************************************************
     
     Mov  GFXMODE$,0
     Mov  DI,Offset GFXMODE$
     Mov  AX,HANDLE_1
     Call DiskInput
;**********************************************************************
;CLOSE 1 
;**********************************************************************
     Mov  BX,HANDLE_1
     Call CloseFile
;**********************************************************************
;PressKey = VAL PressKey$ 
;**********************************************************************
     Xor  AX,AX
     Mov  $Var0,AX
     Mov  DI,Offset PRESSKEY$
     Call Val
     Add  AX,$Var0
     Mov  PRESSKEY,AX
;**********************************************************************
;GFXMode = VAL GFXMode$ 
;**********************************************************************
     Xor  AX,AX
     Mov  $Var0,AX
     Mov  DI,Offset GFXMODE$
     Call Val
     Add  AX,$Var0
     Mov  GFXMODE,AX
;**********************************************************************
;PrgmPath$ = UCASE$ PrgmPath$ 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset PRGMPATH$
     Mov  DI,Offset $_1_
     Call MoveStr
     Mov  AX,Offset $_1_
     Call Ucase$
     Call Concat_2
     Mov  DI,Offset PRGMPATH$
     Call Assign
;**********************************************************************
;Drive = ASC PrgmPath$ 
;**********************************************************************
     Xor  AX,AX
     Mov  $Var0,AX
     Mov  AL,[PRGMPATH$]
     Add  AX,$Var0
     Mov  DRIVE,AX
;**********************************************************************
;Drive = Drive -65 
;**********************************************************************
     Sub  DRIVE,65
;**********************************************************************
;CHDIR PrgmPath$ 
;**********************************************************************
     Mov  DX,Offset PRGMPATH$
     Mov  AH,3Bh
     Int  21h
     Call TrapError
;**********************************************************************
;CHDRIVE Drive 
;**********************************************************************
     Mov  DX,DRIVE
     Mov  AH,0Eh
     Int  21h
     Call TrapError
;**********************************************************************
;Extension$ = RIGHT$ Program$ 3 
;**********************************************************************
     Call ClrWork
     Push 3
     Push Offset PROGRAM$
     Call Right$
     Call Concat_2
     Mov  DI,Offset EXTENSION$
     Call Assign
;**********************************************************************
;Extension$ = UCASE$ Extension$ 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset EXTENSION$
     Mov  DI,Offset $_1_
     Call MoveStr
     Mov  AX,Offset $_1_
     Call Ucase$
     Call Concat_2
     Mov  DI,Offset EXTENSION$
     Call Assign
;**********************************************************************
;IF Extension$ = "BAT" THEN 
;**********************************************************************
     Mov  SI,Offset EXTENSION$
     Mov  DI,Offset $tring_39
     Call StrCompare
     Je   _True_13
     Jmp  _EndIf_13
_True_13:
;**********************************************************************
;ExecuteProgram$ = CommandCom$ 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset COMMANDCOM$
     Mov  DI,Offset EXECUTEPROGRAM$
     Call MoveStr
;**********************************************************************
;Program$ = Program$ + " " 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset PROGRAM$
     Call ConcatWork
     Mov  SI,Offset $tring_40
     Call ConcatWork
     Mov  DI,Offset PROGRAM$
     Call Assign
;**********************************************************************
;Program$ = Program$ + CommandLine$ 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset PROGRAM$
     Call ConcatWork
     Mov  SI,Offset COMMANDLINE$
     Call ConcatWork
     Mov  DI,Offset PROGRAM$
     Call Assign
;**********************************************************************
;Program$ = LEFT$ Program$ 60 
;**********************************************************************
     Call ClrWork
     Push 60
     Push Offset PROGRAM$
     Call Left$
     Call Concat_2
     Mov  DI,Offset PROGRAM$
     Call Assign
;**********************************************************************
;ExecuteCMDLine$ = "/C " + Program$ 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset $tring_41
     Call ConcatWork
     Mov  SI,Offset PROGRAM$
     Call ConcatWork
     Mov  DI,Offset EXECUTECMDLINE$
     Call Assign
;**********************************************************************
;ELSE 
;**********************************************************************
     Jmp _EndIf_13A
_EndIf_13:
;**********************************************************************
;ExecuteProgram$ = Program$ 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset PROGRAM$
     Mov  DI,Offset EXECUTEPROGRAM$
     Call MoveStr
;**********************************************************************
;ExecuteCMDLine$ = CommandLine$ 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset COMMANDLINE$
     Mov  DI,Offset EXECUTECMDLINE$
     Call MoveStr
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_13A:
;**********************************************************************
;IF GFXMode THEN 
;**********************************************************************
     Cmp  GFXMODE,0
     Jne  _True_14
     Jmp  _EndIf_14
_True_14:
     call GFXMode12
;**********************************************************************
;ELSE 
;**********************************************************************
     Jmp _EndIf_14A
_EndIf_14:
     call GFXMode3
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_14A:
;**********************************************************************
;Execute 
;**********************************************************************
     Call Execute
;**********************************************************************
;Drive = ASC WindosPath$ 
;**********************************************************************
     Xor  AX,AX
     Mov  $Var0,AX
     Mov  AL,[WINDOSPATH$]
     Add  AX,$Var0
     Mov  DRIVE,AX
;**********************************************************************
;Drive = Drive -65 
;**********************************************************************
     Sub  DRIVE,65
;**********************************************************************
;CHDIR WindosPath$ 
;**********************************************************************
     Mov  DX,Offset WINDOSPATH$
     Mov  AH,3Bh
     Int  21h
     Call TrapError
;**********************************************************************
;CHDRIVE Drive 
;**********************************************************************
     Mov  DX,DRIVE
     Mov  AH,0Eh
     Int  21h
     Call TrapError
;**********************************************************************
;IF PressKey THEN 
;**********************************************************************
     Cmp  PRESSKEY,0
     Jne  _True_15
     Jmp  _EndIf_15
_True_15:
;**********************************************************************
;Temp = GETKEY 
;**********************************************************************
     Xor  AX,AX
     Call GetKey
     Mov  TEMP,AX
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_15:
;**********************************************************************
;GOTO Start 
;**********************************************************************
     Jmp Start
     GFXMode12 PROC NEAR
     
     mov    ax, 12h
     int    10h
     ret
     
     GFXMode12 ENDP
     GFXMode3 PROC NEAR
     
     mov    ax, 03h
     int    10h
     ret
     
     GFXMode3 ENDP
     
ExitToDos:
     
     Mov  AH,4Ch
     Xor  AL,AL
     Int  21h
     
     
;************** User Defined SUBS & FUNCTIONS ***************
     
     
     Execute Proc Near
     Public Execute
     Push BP
     Mov  BP,SP
;**********************************************************************
;LenProgram = LEN ExecuteProgram$ 
;**********************************************************************
     Xor  AX,AX
     Mov  $Var0,AX
     Mov  SI,Offset EXECUTEPROGRAM$
     Call Len
     Add  AX,$Var0
     Mov  LENPROGRAM,AX
;**********************************************************************
;LenCommandLine = LEN ExecuteCMDLine$ 
;**********************************************************************
     Xor  AX,AX
     Mov  $Var0,AX
     Mov  SI,Offset EXECUTECMDLINE$
     Call Len
     Add  AX,$Var0
     Mov  LENCOMMANDLINE,AX
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
     
     Mov  SP,BP
     Pop  BP
     Ret 
     Execute Endp
     
;*************  BASM Library Code Begins Here  **************
     
     
     
     MoveStr Proc Near
     Cld
MoveStr1:
     Lodsb
     Stosb
     Or  AL,AL
     Jnz MoveStr1
     Ret
     MoveStr Endp
     
     
     
     
     Len Proc Near
     Push CX
     Push SI
     Push DI
     Mov  DI,SI
     Xor  AL,AL
     Mov  CX,0ffffh
     Cld
     Repnz Scasb
     Not  CX
     Dec  CX
     Mov  AX,CX
     Pop  DI
     Pop  SI
     Pop  CX
     Ret
     Len Endp
     
     
     
     
     Concat Proc Near
     Push CX
     Push DI
     Push SI
     Push SI
     Push DI
     Pop  SI
     Call Len
     Mov  BX,AX
     Pop  SI
     Call Len
     Mov  CX,AX
     Inc  CX
     Pop  SI
     Pop  DI
     Add  DI,BX
     Cld
     Rep  Movs Byte Ptr[DI],Byte Ptr ES:[SI]
     Pop  CX
     Ret
     Concat Endp
     
     
     
     
     Concat_2 Proc Near
     Mov  SI,Offset $_1_
     Mov  DI,Offset $_0_
     Call Concat
     Ret
     Concat_2 Endp
     
     
     
     
     StrCompare Proc Near
     Push AX
     Push DI
     Push SI
     Cld
StrCmp1:
     Lodsb
     Scasb
     Jne  StrCmp2
     Or   AL,AL
     Jne  StrCmp1
StrCmp2:
     Pop  SI
     Pop  DI
     Pop  AX
     Ret
     StrCompare Endp
     
     
     
     
     Val Proc Near
     Push BP
     Mov  BP,SP
     Sub  SP,2
     Xor  BX,BX
     Mov  [BP-2],BX
     Mov  SI,BX
     Mov  CX,1
Val_1:
     Mov  AX,[DI]
     Xor  AH,AH
     Cmp  AL,AH
     Je   Val_4
     Cmp  AL,48
     Jb   Val_2
     Cmp  AL,57
     Jg   Val_2
     Sub  AL,'0'
     Push AX
     Inc  SI
Val_2:
     Cmp  AL,45
     Jne Val_3
     Mov Word Ptr[BP-2],1
Val_3:
     Inc  DI
     Jmp  Val_1
Val_4:
     Cmp  SI,0
     Je   Val_5
     Pop  AX
     Mul  CX
     Add  BX,AX
     Mov  AX,CX
     Mov  CX,10
     Mul  CX
     Mov  CX,AX
     Dec  SI
     Cmp  SI,1
     Jge  Val_4
     Mov  AX,BX
     Jmp  Val_6
Val_5:
     Xor  AX,AX
Val_6:
     Cmp  Word Ptr[BP-2],1
     Jne  Val_7
     Neg  AX
Val_7:
     Mov  SP,BP
     Pop  BP
     Ret
     Val Endp
     
     
     
     
     TrapError Proc Near
     Jnc  NoError
     Mov  Err,AX
NoError:
     Clc
     Ret
     TrapError Endp
     
     
     
     
     Ucase$ Proc Near
     Mov  DI,AX
     Mov  CX,MaxString
     Cld
     Mov  SI,DI
Ucase1:
     Lodsb
     Cmp  AL,'a'
     Jb   Ucase2
     Cmp  AL,'z'
     Jg   Ucase2
     Sub  AL,'a'-'A'
Ucase2:
     Stosb
     Loop Ucase1
     Ret
     Ucase$ Endp
     
     
     
     
     _Instr Proc Near
     Push BP
     Call Len
     Mov  BX,AX
     Push SI
     Mov  SI,DI
     Call Len
     Mov  DX,AX
     Pop  SI
     Mov  BP,SI
     Dec  BX
     Cld
     Push DI
Instr_1:
     Mov  SI,BP
     Lodsb
     Mov  CX,DX
     Repnz Scasb
     Jnz  Instr_3
     Mov  DX,CX
     Mov  CX,BX
     Repz Cmpsb
     Jz   Instr_2
     Add  DI,CX
     Sub  DI,BX
     Cmp  DX,BX
     Ja   Instr_1
     Jmp  Instr_3
Instr_2:
     Sub  DI,BX
     Mov  AX,DI
     Pop  BX
     Sub  AX,BX
     Pop  BP
     Ret
Instr_3:
     Xor  AX,AX
     Pop  BX
     Pop  BP
     Ret
     _Instr Endp
     
     
     
     
     Trim$ Proc Near
     Mov  DI,Offset $_2_
     Call MoveStr
     Mov  SI,Offset $_2_
     Call Ltrim$
     Mov  SI,Offset $_1_
     Mov  DI,Offset $_2_
     Call MoveStr
     Mov  SI,Offset $_2_
     Call Rtrim$
     Ret
     Trim$ Endp
     
     
     
     
     Rtrim$ Proc Near
     Mov  DI,Offset $_2_
     Call MoveStr
     Mov  SI,Offset $_2_
     Mov  DI,Offset $_1_
     Call MoveStr
     Mov  SI,Offset $_1_
     Call Len
     Mov  DI,Offset $_1_
     Mov  CX,AX
     Add  DI,AX
Rtrim1:
     Mov  BL,[DI]
     Cmp  BL,32
     Jg   Rtrim2
     Xor  AX,AX
     Mov  [DI],AL
     Dec  CX
     Dec  DI
     Cmp  CX,0
     Jge  Rtrim1
Rtrim2:
     Ret
     Rtrim$ Endp
     
     
     
     
     Ltrim$ Proc Near
     Mov  DI,Offset $_2_
     Call MoveStr
     Mov  SI,Offset $_2_
     Xor  CX,CX
Ltrim1:
     Cmp  [SI],Byte Ptr 32
     Jne  Ltrim2
     Inc  SI
     Inc  CX
     Cmp  CX,MaxString
     Jne  Ltrim1
Ltrim2:
     Mov  CX,MaxString
     Mov  DI,Offset $_1_
     Cld
     Rep  Movs Byte Ptr [DI],Byte Ptr ES:[SI]
     Ret
     Ltrim$ Endp
     
     
     
     
     DiskInput Proc Near
     Mov  $Handle,AX
     Xor  AX,AX
     Mov  SI,AX
     Mov  [DI],AL
     Mov  Eof,AX
DiskInLoop:
     Xor  CX,CX
     Mov  $_1_,CL
     Inc  CX
     Mov  BX,$Handle
     Mov  DX,Offset $_1_
     Mov  AH,3fh
     Int  21h
     Cmp  AX,1
     Je   DiskInOk
     Mov  Eof,-1
     Jmp  DiskInExit
DiskInOk:
     Cmp  $_1_,13
     Je   DiskInLF
     Cmp  $_1_,44
     Je   DiskInExit
     Cmp  $_1_,26
     Je   DiskEof
     Mov  AL,$_1_
     Mov  [DI],AL
     Inc  DI
     Inc  SI
     Cmp  SI,MaxString
     Jne  DiskInLoop
DiskInLF:
     Xor  CX,CX
     Mov  $_1_,CL
     Inc  CX
     Mov  BX,$Handle
     Mov  DX,Offset $_1_
     Mov  AH,3fh
     Int  21h
DiskInExit:
     Xor  AX,AX
     Mov  [DI],AL
     Ret
DiskEof:
     Mov  Eof,-1
     Xor  AX,AX
     Mov  [DI],AL
     Ret
     DiskInput Endp
     
     
     
     
     Left$ Proc Near
     Push BP
     Mov  BP,SP
     Mov  SI,[BP+4]
     Mov  DI,Offset $_1_
     Mov  CX,[BP+6]
     Cld
     Rep  Movs Byte Ptr [DI],Byte Ptr ES:[SI]
     Xor  AX,AX
     Mov  [DI],AL
     Pop  BP
     Ret  04
     Left$ Endp
     
     
     
     
     Right$ Proc Near
     Push BP
     Mov  BP,SP
     Sub  SP,2
     Xor  AX,AX
     Mov  [BP-2],AX
     Mov  SI,[BP+4]
     Call Len
     Sub  AX,[BP+6]
     Inc  AX
     Mov  [BP-2],AX
     Cmp  AX,0
     Jge  Right_01
     Mov  AX,1
     Mov  [BP-2],AX
Right_01:
     Mov  DI,Offset $_1_
     Mov  [DI],Byte Ptr 0
     Mov  SI,[BP+4]
     Mov  DI,Offset $_1_
     Add  SI,[BP-2]
     Dec  SI
     Mov  CX,MaxString
     Sub  CX,[BP-2]
     Cld
     Rep  Movs Byte Ptr [DI],Byte Ptr ES:[SI]
     Xor  AX,AX
     Mov  [DI],AL
     Mov  SP,BP
     Pop  BP
     Ret  04
     Right$ Endp
     
     
     
     
     ClrWork Proc Near
     Mov  DI,Offset $_0_
     Xor  AX,AX
     Mov  [DI],AX
     Ret
     ClrWork Endp
     
     
     
     
     ConcatWork Proc Near
     Mov  DI,Offset $_0_
     Call Concat
     Ret
     ConcatWork Endp
     
     
     
     
     Assign Proc Near
     Mov  SI,Offset $_0_
     Call MoveStr
     Ret
     Assign Endp
     
     
     
     
     Mid$ Proc Near
     Push BX
     Push AX
     Call ClrWork
     Cld
     Mov  DI,Offset $_1_
     Pop  AX
     Add  SI,AX
     Dec  SI
     Pop  CX
     Rep  Movs Byte Ptr[DI],Byte Ptr ES:[SI]
     Mov  [DI],Byte Ptr 0
     Ret
     Mid$ Endp
     
     
     
     
     Getkey Proc Near
     Xor  AX,AX
     Int  16h
     Cmp  AL,0
     Je   GetKey_1
     Xor  AH,AH
     Jmp  GetKey_2
GetKey_1:
     Mov  AL,AH
     Xor  AH,AH
     Neg  AX
GetKey_2:
     Ret
     GetKey Endp
     
     
     
     
     CloseFile Proc Near
     Mov  AH,3Eh
     Int  21h
     Call TrapError
     Ret
     CloseFile Endp
     
     
     
     
     ReadFile Proc Near
     Push BX
     Xor  AX,AX
     Mov  Eof,AX
     Mov  AH,3DH
     Int  21h
     Call TrapError
     Pop  BX
     Mov  [BX],AX
     Ret
     ReadFile Endp
     
     

;***************** System Initialized Data *****************

$Attr        DW 7
$CrLf        DB 13,10,'$'
$CrLf2       DB 13,10,0
FgColor      DW 7
BgColor      DW 0
Err          DW 0
DefSeg       DW 0
$DataSeg     DW 0
$Empty       DB 0,0
$Handle      DW 0
Eof          DW 0
$Var0        DW 0
$Var1        DW 0
$Var2        DW 0
$Var3        DW 0
Command$     DB 81 DUP(?)

;***************** Users Initialized Data *******************

LENCOMMANDLINE DW  0
$tring_16     DB  "/DISKTOP",0
EXPCALL       DW  0
$tring_18     DB  "/DESKTOP",0
$tring_19     DB  "COMSPEC=",0
ENVERROR      DW  0
ENVZEROCOUNT  DW  0
GFXMODE       DW  0
$tring_23     DB  "SAVETEXT.COM",0
$tring_24     DB  0,0
$tring_25     DB  "DI_MAIN.EXE",0
$tring_26     DB  "DE_MAIN.EXE",0
$tring_27     DB  0,0
$tring_28     DB  "windos.ini",0
HANDLE_1      DW  0
$tring_30     DB  "=",0
EQUAL         DW  0
STRLENGTH     DW  0
VALUE         DW  0
$tring_34     DB  "WINDOS",0
$tring_35     DB  "BATCH",0
$tring_36     DB  "EXPCALL",0
PRESSKEY      DW  0
DRIVE         DW  0
$tring_39     DB  "BAT",0
$tring_40     DB  " ",0
$tring_41     DB  "/C ",0
TEMP          DW  0
LENPROGRAM    DW  0

;*************** System Un-Initialized Data *****************

$_0_         DB  MaxString DUP(?)
$_1_         DB  MaxString DUP(?)
$_2_         DB  MaxString DUP(?)

;**************** Users Un-Initialized Data *****************

Program$      DB 171 DUP(?)
CommandLine$  DB 91 DUP(?)
ExecuteProgram$ DB 81 DUP(?)
ExecuteCMDLine$ DB 91 DUP(?)
EnvVar$       DB 9 DUP(?)
CommandCom$   DB 81 DUP(?)
Something$    DB 101 DUP(?)
Value$        DB 91 DUP(?)
WindosPath$   DB 71 DUP(?)
Batch$        DB 81 DUP(?)
PrgmPath$     DB 71 DUP(?)
PressKey$     DB 6 DUP(?)
GFXMode$      DB 6 DUP(?)
Extension$    DB 4 DUP(?)
;************************************************************
_EndProgram Label Byte
 Code Ends

End _StartProgram
