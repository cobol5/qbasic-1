'Copyright (C) HiTech Laboratories, Denis Andrianov
'SaveText v1.0 module for Desktop and Disktop
'December, 2002
'============================================================================


'**********************
'Generate a COM file.
'**********************

$COM


'**********************
'Set up strings.
'**********************

DIM Filename$       * 79       'Max file path = 79 chars.

DIM Buffer$         * 120
DIM Value$          * 110


'**********************
'Read WINDOS.INI file.
'**********************

OPEN "windos.ini" FOR INPUT AS 1
  

  DO

  INPUT #1, Buffer$
      
    IF EOF = -1 THEN
    EXIT LOOP
    END IF

  Buffer$ = UCASE$(Buffer$)
  Equal% = INSTR(Buffer$, "=")

    IF Equal% > 0 THEN
    StrLength% = LEN(Buffer$)
    StrLength% = StrLength% - Equal%
    Equal% = Equal% + 1             
    Value$ = MID$(Buffer$, Equal%, StrLength%)
    Value$ = TRIM$(Value$)
    Equal% = Equal% - 1
    Equal% = Equal% - 1
    Buffer$ = LEFT$(Buffer$, Equal%)
    Buffer$ = TRIM$(Buffer$)
    END IF


    IF Buffer$ = "TEMP" THEN
    Filename$ = Value$
    EXIT LOOP
    END IF

  LOOP


CLOSE 1
  

'****************************
'Construct a full file name.
'****************************

Value$ = RIGHT$(Filename$, 1)

  IF Value$ <> "\" THEN
  Filename$ = Filename$ + "\"
  END IF

Filename$ = Filename$ + "dos.scr"
   

'*****************
'Save the screen.
'*****************

'+--------------+
'| BSAVE FORMAT |
'+--------------+
'
'Offset in file | Description
'---------------+-----------------
'0              | Always = FD
'1              | Seg low bits
'2              | Seg high bits
'3              | Offset low bits
'4              | Offset high bits
'5              | Length low bits
'6              | Length high bits
'....           | Data


OPEN Filename$ FOR OUTPUT AS 1
PUTCHAR 1, &HFD, 0, &HB8, 0, 0, &HA0, &H0F                 'Write the header


  FOR Offs% = 0 TO &HF9F
  
  $ASM
  push   ds
  mov    ax, 0B800h
  mov    ds, ax
  mov    si, cs:Offs
  lodsb
  xor    ah, ah
  mov    cs:ByteData, ax
  pop    ds
  $ASM
  
  PUTCHAR 1, ByteData%
  NEXT


CLOSE 1



