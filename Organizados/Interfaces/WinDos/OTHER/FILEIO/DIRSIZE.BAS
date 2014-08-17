'Copyright (C) HiTech Labs, Denis Andrianov
'Directory Size v1.0
'============================================================================

'$INCLUDE: 'windos.bi'

DECLARE SUB CalcDirSize (Directory$)
DECLARE FUNCTION ReadDir% (DirSpec$, BYVAL SearchType%)
DECLARE FUNCTION AddSlash$ (Strng$)

CONST FindFirst = &H4E00        'Functions of interrupt 21h.
CONST FindNext = &H4F00

DIM SHARED Regs AS RegTypeX
DIM SHARED DTA AS DTAStructure
DIM SHARED SizeOfAllFiles&

'============================================================================
OldDirectory$ = CURDIR$

CLS

Directory$ = COMMAND$
CHDRIVE Directory$
CalcDirSize Directory$

COLOR 15
PRINT SizeOfAllFiles&

CHDRIVE OldDirectory$
CHDIR OldDirectory$

FUNCTION AddSlash$ (Strng$)

  IF RIGHT$(Strng$, 1) <> "\" THEN
    AddSlash$ = Strng$ + "\"
  ELSE
    AddSlash$ = Strng$
  END IF

END FUNCTION

SUB CalcDirSize (Directory$)
                                  
STATIC TempMask$
TempMask$ = "*.*"
                                  
                                  
CalcDirSizeStart:
CHDIR Directory$
  
  IF ReadDir(TempMask$, FindFirst) THEN

    FOR Counter% = 1 TO FileDirCount%
      IF NOT (ReadDir(TempMask$, FindNext)) THEN EXIT SUB
    NEXT

    DO
    Filename$ = UCASE$(RTRIM$(DTA.Filename))
    FileDirCount% = FileDirCount% + 1

      IF (ASC(DTA.Attributes) AND 16) = 16 THEN        'This is a directory.
          IF LEFT$(Filename$, 1) <> "." THEN
          PRINT AddSlash$(Directory$) + Filename$
          CalcDirSize AddSlash$(Directory$) + Filename$
          GOTO CalcDirSizeStart
          END IF
      ELSE                                             'This is a file.
        SizeOfAllFiles& = SizeOfAllFiles& + DTA.FileSize
      END IF

      IF NOT (ReadDir(TempMask$, FindNext)) THEN
      EXIT SUB
      END IF

    LOOP

  END IF


END SUB

FUNCTION ReadDir% (DirSpec$, BYVAL SearchType%)

  IF SearchType% <> FindFirst AND SearchType% <> FindNext THEN
  EXIT FUNCTION
  END IF

  IF SearchType% = FindFirst THEN
  'Setting up DTA...
  Regs.ax = &H1A00
  Regs.ds = VARSEG(DTA)
  Regs.dx = VARPTR(DTA)
  CALL InterruptX(&H21, Regs, Regs)
  END IF

'Finding FIRST or NEXT entries...
DirSpec$ = DirSpec$ + CHR$(0)
Regs.ax = SearchType%
Regs.cx = 55                      'Read all files!!!
Regs.ds = SSEG(DirSpec$)
Regs.dx = SADD(DirSpec$)
CALL InterruptX(&H21, Regs, Regs)

  IF Regs.flags AND 1 THEN
    ReadDir% = 0
  ELSE
    NullByte% = INSTR(DTA.Filename, CHR$(0))
      IF NullByte% > 0 THEN
      DTA.Filename = LEFT$(DTA.Filename, NullByte% - 1)
      END IF
    ReadDir% = -1
  END IF

END FUNCTION

