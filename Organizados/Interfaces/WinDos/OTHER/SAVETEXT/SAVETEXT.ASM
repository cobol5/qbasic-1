;**********************************************************************
;   ASM Source Created Using BASM 6.5(c) 1998  by Kevin Diggins 
;**********************************************************************
;      File Name: SAVETEXT.ASM
;  Creation Date: 12-25-2002
;  Creation Time: 00:11:30
;**********************************************************************

.286p

MaxString Equ 81 

Code Segment Byte Public
Assume CS:Code,DS:Code,ES:Code
           Org 256
_StartProgram:

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
;OPEN "windos.ini" FOR INPUT AS 1 
;**********************************************************************
     Mov  DX,Offset $tring_04
     Mov  BX,Offset HANDLE_1
     Call ReadFile
;**********************************************************************
;DO 
;**********************************************************************
DoLoop_01:
;**********************************************************************
;INPUT #1 Buffer$ 
;**********************************************************************
     
     Mov  BUFFER$,0
     Mov  DI,Offset BUFFER$
     Mov  AX,HANDLE_1
     Call DiskInput
;**********************************************************************
;IF EOF = -1 THEN 
;**********************************************************************
     Cmp  EOF,-1
     Je   _True_01
     Jmp  _EndIf_01
_True_01:
;**********************************************************************
;EXIT LOOP 
;**********************************************************************
     Jmp DoLoop_01A
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_01:
;**********************************************************************
;Buffer$ = UCASE$ Buffer$ 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset BUFFER$
     Mov  DI,Offset $_1_
     Call MoveStr
     Mov  AX,Offset $_1_
     Call Ucase$
     Call Concat_2
     Mov  DI,Offset BUFFER$
     Call Assign
;**********************************************************************
;Equal = INSTR Buffer$ "=" 
;**********************************************************************
     Xor  AX,AX
     Mov  $Var0,AX
     Mov  DI,Offset BUFFER$
     Mov  SI,Offset $tring_06
     Call _Instr
     Add  AX,$Var0
     Mov  EQUAL,AX
;**********************************************************************
;IF Equal > 0 THEN 
;**********************************************************************
     Cmp  EQUAL,0
     Jg   _True_02
     Jmp  _EndIf_02
_True_02:
;**********************************************************************
;StrLength = LEN Buffer$ 
;**********************************************************************
     Xor  AX,AX
     Mov  $Var0,AX
     Mov  SI,Offset BUFFER$
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
;Value$ = MID$ Buffer$ Equal StrLength 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset BUFFER$
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
;Equal = Equal -1 
;**********************************************************************
     Dec  EQUAL
;**********************************************************************
;Equal = Equal -1 
;**********************************************************************
     Dec  EQUAL
;**********************************************************************
;Buffer$ = LEFT$ Buffer$ Equal 
;**********************************************************************
     Call ClrWork
     Push EQUAL
     Push Offset BUFFER$
     Call Left$
     Call Concat_2
     Mov  DI,Offset BUFFER$
     Call Assign
;**********************************************************************
;Buffer$ = TRIM$ Buffer$ 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset BUFFER$
     Call Trim$
     Call Concat_2
     Mov  DI,Offset BUFFER$
     Call Assign
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_02:
;**********************************************************************
;IF Buffer$ = "TEMP" THEN 
;**********************************************************************
     Mov  SI,Offset BUFFER$
     Mov  DI,Offset $tring_09
     Call StrCompare
     Je   _True_03
     Jmp  _EndIf_03
_True_03:
;**********************************************************************
;Filename$ = Value$ 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset VALUE$
     Mov  DI,Offset FILENAME$
     Call MoveStr
;**********************************************************************
;EXIT LOOP 
;**********************************************************************
     Jmp DoLoop_01A
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_03:
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
;Value$ = RIGHT$ Filename$ 1 
;**********************************************************************
     Call ClrWork
     Push 1
     Push Offset FILENAME$
     Call Right$
     Call Concat_2
     Mov  DI,Offset VALUE$
     Call Assign
;**********************************************************************
;IF Value$ < > "\" THEN 
;**********************************************************************
     Mov  SI,Offset VALUE$
     Mov  DI,Offset $tring_10
     Call StrCompare
     Jne  _True_04
     Jmp  _EndIf_04
_True_04:
;**********************************************************************
;Filename$ = Filename$ + "\" 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset FILENAME$
     Call ConcatWork
     Mov  SI,Offset $tring_10   
     Call ConcatWork
     Mov  DI,Offset FILENAME$
     Call Assign
;**********************************************************************
;END IF 
;**********************************************************************
_EndIf_04:
;**********************************************************************
;Filename$ = Filename$ + "dos.scr" 
;**********************************************************************
     Call ClrWork
     Mov  SI,Offset FILENAME$
     Call ConcatWork
     Mov  SI,Offset $tring_11
     Call ConcatWork
     Mov  DI,Offset FILENAME$
     Call Assign
;**********************************************************************
;OPEN Filename$ FOR OUTPUT AS 1 
;**********************************************************************
     Mov  DX,Offset FILENAME$
     Mov  AX,Offset HANDLE_1
     Call CreateFile
;**********************************************************************
;PUTCHAR 1 &HFD 0 &HB8 0 0 &HA0 &H0F 
;**********************************************************************
     Mov  AX,253
     Mov  BX,HANDLE_1
     Call PutChar
     Mov  AX,0
     Mov  BX,HANDLE_1
     Call PutChar
     Mov  AX,184
     Mov  BX,HANDLE_1
     Call PutChar
     Mov  AX,0
     Mov  BX,HANDLE_1
     Call PutChar
     Mov  AX,0
     Mov  BX,HANDLE_1
     Call PutChar
     Mov  AX,160
     Mov  BX,HANDLE_1
     Call PutChar
     Mov  AX,15
     Mov  BX,HANDLE_1
     Call PutChar
;**********************************************************************
;FOR Offs = 0 TO &HF9F 
;**********************************************************************
     Mov  OFFS,-1
ForLoop_01:
     Inc  OFFS
     Mov  AX,OFFS
     Cmp  AX,3999
     Jle  ForLoop_01_Ok
     Jmp  Next_01
ForLoop_01_Ok:
     push   ds
     mov    ax, 0B800h
     mov    ds, ax
     mov    si, cs:Offs
     lodsb
     xor    ah, ah
     mov    cs:ByteData, ax
     pop    ds
;**********************************************************************
;PUTCHAR 1 ByteData 
;**********************************************************************
     Mov  AX,BYTEDATA
     Mov  BX,HANDLE_1
     Call PutChar
;**********************************************************************
;NEXT 
;**********************************************************************
     Jmp ForLoop_01
Next_01:
;**********************************************************************
;CLOSE 1 
;**********************************************************************
     Mov  BX,HANDLE_1
     Call CloseFile
     
ExitToDos:
     
     Mov  AH,4Ch
     Xor  AL,AL
     Int  21h
     
     
;************** User Defined SUBS & FUNCTIONS ***************
     
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
     
     
     
     
     CreateFile Proc Near
     Push DX
     Push AX
     Xor  CX,CX
     Mov  Eof,CX
     Mov  AH,3Ch
     Int  21h
     Call TrapError
     Pop  BX
     Mov  [BX],AX
     Mov  BX,AX
     Mov  AH,3Eh
     Int  21h
     Call TrapError
     Pop  DX
     Mov  AL,1
     Mov  AH,3DH
     Int  21h
     Call TrapError
     Ret
     Createfile Endp
     
     
     
     
     PutChar Proc Near
     Mov  AH,AL
     Mov  $Var0,AX
     Mov  DX,Offset $Var0
     Mov  CX,1
     Mov  AH,40h
     Int  21h
     Call TrapError
     Ret
     PutChar Endp
     
     

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

;***************** Users Initialized Data *******************

$tring_04     DB  "windos.ini",0
HANDLE_1      DW  0
$tring_06     DB  "=",0
EQUAL         DW  0
STRLENGTH     DW  0
$tring_09     DB  "TEMP",0
$tring_10     DB  "\",0
$tring_11     DB  "dos.scr",0
OFFS          DW  0
BYTEDATA      DW  0

;*************** System Un-Initialized Data *****************

$_0_         DB  MaxString DUP(?)
$_1_         DB  MaxString DUP(?)
$_2_         DB  MaxString DUP(?)

;**************** Users Un-Initialized Data *****************

Filename$     DB 80 DUP(?)
Buffer$       DB 121 DUP(?)
Value$        DB 111 DUP(?)
;************************************************************
_EndProgram Label Byte
 Code Ends

End _StartProgram
