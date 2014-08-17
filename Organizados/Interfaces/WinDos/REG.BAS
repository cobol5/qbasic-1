'Copyright (C) HiTech Labs, Denis Andrianov
'Extension Registration Procedures
'============================================================================

'Reg declarations.
'$INCLUDE: 'reg.bi'

'----------------------------------------------------------------------------

CONST SpaceChar% = 32
CONST OldSyntaxChar$ = "+"
CONST ExtStartChar$ = "["
CONST ExtTermChar$ = "="
CONST ExtEndChar$ = "]"
CONST LabelStartChar$ = "("
CONST LabelEndChar$ = ")"
CONST BatchStartChar$ = "{"
CONST BatchEndChar$ = "}"
CONST FlagStartChar$ = "<"
CONST FlagEndChar$ = ">"

'The following flags must be enclosed in FlagStartChar% and FlagEndChar%
'characters!

CONST FFile$ = "<FILE>"                       'Newer and older syntax
CONST FFileExt$ = "<FILE.EXT>"
CONST FDrive$ = "<DRIVE>"
CONST FDir$ = "<DIR>"
CONST FDirSlash$ = "<DIR\>"
CONST FList$ = "<LIST:>"

CONST FKeys$ = "<KEYS:>"                      'Older syntax
CONST FPause$ = "<PAUSE>"
CONST FDontPause$ = "<NO_PAUSE>"
CONST FSwitchDir$ = "<SWITCH_TO_DIR>"

'============================================================================
'Notes:
'       The mask may contain a '?' wildcard. '*' is not supported.
'       The function is case insensitive.
'
'Function returns:
'                  -1  if  Extension$ matches the Mask$
'                   0  otherwise
'----------------------------------------------------------------------------
'
FUNCTION Reg.CompareExt% (Extension$, Mask$)

ExtensionInt$ = UCASE$(RTRIM$(Extension$))      'Do not modify original vars
MaskInt$ = UCASE$(RTRIM$(Mask$))


  FOR i% = 1 TO 3
    MaskChar$ = MID$(MaskInt$, i%, 1)

      IF MaskChar$ <> "?" THEN
        IF MaskChar$ <> MID$(ExtensionInt$, i%, 1) THEN
        Reg.CompareExt% = 0
        EXIT FUNCTION
        END IF
      END IF

  NEXT


Reg.CompareExt% = -1
END FUNCTION

'============================================================================
'Notes:
'       RegFile$    is the registration file to be read.
'       Extension$  is the extension to search for.
'       Labels$()   is a blank array to be filled with labels associated
'                   with Extension$.
'
'Function returns:
'                   number of labels
'----------------------------------------------------------------------------
'
FUNCTION Reg.GetLabels% (RegFile$, Extension$, Labels$())

'********************************
'Definitions
'********************************

MaxIndex% = UBOUND(Labels$)
ItemNum% = 0


'********************************
'Code starts here.
'********************************

File% = FREEFILE
OPEN RegFile$ FOR INPUT AS #File%



  DO WHILE Reg.Skip2Ext%(File%, Extension$)          'Read until EOF.


      DO UNTIL EOF(File%)

      '*****************************
      'Read next line and analyse.
      '*****************************

      LINE INPUT #File%, Buffer$
      Buffer$ = LTRIM$(Buffer$)
      FirstChar$ = LEFT$(Buffer$, 1)

        '***********************
        'Label is found.
        '***********************

        IF FirstChar$ = LabelStartChar$ THEN

          EndCharPos% = INSTR(Buffer$, LabelEndChar$)

            IF EndCharPos% THEN
              IF ItemNum% <= MaxIndex% THEN
              Labels$(ItemNum%) = MID$(Buffer$, 2, EndCharPos% - 2)
              ItemNum% = ItemNum% + 1
              END IF
            END IF
        
        '************************************
        'End of extension descr. is reached.
        '************************************

        ELSEIF FirstChar$ = ExtEndChar$ THEN
          EXIT DO

        END IF


      LOOP
  LOOP



CLOSE #File%


'**************************
'Exit function with value.
'**************************

Reg.GetLabels% = ItemNum%


END FUNCTION

'============================================================================
'Notes:
'       RegFile$    is the registration file to be read.
'       Extension$  is the extension to be searched for in the file.
'       LabelIndex% is the index of the bunch of data to write or return.
'       BatchFile$  is a file to be written based on the commands in
'                   RegFile$ for a label with LabelIndex%.
'       State       file manager status
'       RegFlag     is a collection of flags that may be set and returned.
'
'Function returns:
'               -1  if successfully found the data and written the batch file
'                0  otherwise
'----------------------------------------------------------------------------
'
FUNCTION Reg.MakeBatch% (RegFile$, Extension$, LabelIndex%, BatchFile$, State AS FMStateStruc, RegFlag AS RegFlagsStruc)

'********************************
'Default values.
'********************************

'ItemNum% = 0

RegFlag.Format = RegNewFormat%
RegFlag.Executable = ""
RegFlag.SwitchDir = 0
RegFlag.Pause = 0
RegFlag.DontPause = 0
RegFlag.KeysOn = 0
RegFlag.KeyFile = ""
RegFlag.ListOn = 0
RegFlag.ListFile = ""


'********************************
'Code starts here.
'********************************

File% = FREEFILE
OPEN RegFile$ FOR INPUT AS #File%



  DO WHILE Reg.Skip2Ext%(File%, Extension$)          'Read until EOF.


      DO UNTIL EOF(File%)

      '*****************************
      'Read next line and analyse.
      '*****************************

      LINE INPUT #File%, Buffer$
      Buffer$ = LTRIM$(Buffer$)
      FirstChar$ = LEFT$(Buffer$, 1)

        '***********************
        'Batch start is found.
        '***********************

        IF FirstChar$ = BatchStartChar$ THEN
        ItemNum% = ItemNum% + 1

          IF ItemNum% = LabelIndex% THEN

          Buffer$ = MID$(Buffer$, 2)
          File2% = FREEFILE
          OPEN BatchFile$ FOR OUTPUT AS #File2%


            DO
            EndCharPos% = INSTR(Buffer$, BatchEndChar$)

              IF EndCharPos% THEN
              Buffer$ = LEFT$(Buffer$, EndCharPos% - 1)
              END IF

              IF Reg.ReplaceFlags(Buffer$, State, RegFlag, Translated$) = RegNewFormat% THEN
                PRINT #File2%, Translated$
              ELSE
                EXIT DO
              END IF

              IF EndCharPos% THEN
              EXIT DO
              END IF

            LINE INPUT #File%, Buffer$
            LOOP UNTIL EOF(File%)


          CLOSE #File2%
          Success% = -1
          GOTO MakeBatchExit

          END IF

        '************************************
        'End of extension descr. is reached.
        '************************************

        ELSEIF FirstChar$ = ExtEndChar$ THEN
          EXIT DO

        END IF


      LOOP
  LOOP


'*****************************
'Terminate function.
'*****************************

MakeBatchExit:
CLOSE #File%
Reg.MakeBatch% = Success%

END FUNCTION

'============================================================================
'Notes:
'      The function takes a Buffer$ of text and substitutes a specific value
'      (found in State) for every flag found in the string. The result is
'      stored in Translated$ and several fields in RegFlag may be set.
'      Executable and CommandLine fields are set only when the old format is
'      used.
'
'Function returns:
'              RegNewFormat%  if Buffer$ is in the new format
'              RegOldFormat%  if Buffer$ is in the old format
'----------------------------------------------------------------------------
'
FUNCTION Reg.ReplaceFlags% (Buffer$, State AS FMStateStruc, RegFlag AS RegFlagsStruc, Translated$)

'**********************
'Initialize variables.
'**********************

Translated$ = ""
LocalBuffer$ = RTRIM$(LTRIM$(Buffer$))
LenBuffer% = LEN(LocalBuffer$)
CharStart% = 1                        'Position within Buffer$ while scanning
LenFKeys% = LEN(FKeys$)
LenFList% = LEN(FList$)
Format% = RegNewFormat%                  'Assume new format.


'***********************
'Test for old format.
'***********************

  IF LEFT$(LocalBuffer$, 1) = OldSyntaxChar$ THEN
  LocalBuffer$ = MID$(LocalBuffer$, 2)
  LenBuffer% = LenBuffer% - 1
 
    DO                                'Extracting the executable
    GOSUB ExtractWord
      IF LenSelection% THEN EXIT DO
    LOOP WHILE CharStart% <= LenBuffer%

  RegFlag.Executable = Selection$
  Format% = RegOldFormat%
  END IF


'*****************************
'Translation of flags begins.
'*****************************

  DO WHILE CharStart% <= LenBuffer%
  GOSUB ExtractWord
  Flag$ = UCASE$(Selection$)

    IF Flag$ = FFileExt$ THEN
    Translated$ = Translated$ + RTRIM$(State.FileExt)

    ELSEIF Flag$ = FFile$ THEN
    Translated$ = Translated$ + RTRIM$(State.File)

    ELSEIF Flag$ = FDrive$ THEN
    Translated$ = Translated$ + State.Drive

    ELSEIF Flag$ = FDirSlash$ THEN
    Translated$ = Translated$ + RTRIM$(State.DirSlash)

    ELSEIF Flag$ = FDir$ THEN
    Translated$ = Translated$ + RTRIM$(State.Dir)

    ELSEIF Flag$ = FSwitchDir$ THEN
    RegFlag.SwitchDir = -1

    ELSEIF Flag$ = FPause$ THEN
    RegFlag.Pause = -1

    ELSEIF Flag$ = FDontPause$ THEN
    RegFlag.DontPause = -1

    ELSEIF LEFT$(Flag$, LenFKeys% - 1) = LEFT$(FKeys$, LenFKeys% - 1) AND RIGHT$(Flag$, 1) = FlagEndChar$ THEN
    RegFlag.KeysOn = -1
    RegFlag.KeyFile = LTRIM$(RTRIM$(MID$(Selection$, LenFKeys%, LEN(Selection$) - LenFKeys%)))

    ELSEIF LEFT$(Flag$, LenFList% - 1) = LEFT$(FList$, LenFList% - 1) AND RIGHT$(Flag$, 1) = FlagEndChar$ THEN
    ListFile$ = LTRIM$(RTRIM$(MID$(Selection$, LenFList%, LEN(Selection$) - LenFList%)))
    RegFlag.ListOn = -1
    RegFlag.ListFile = ListFile$
    Translated$ = Translated$ + ListFile$

    ELSE
    Translated$ = Translated$ + Selection$
    END IF

  LOOP


'****************************
'Set final values and exit.
'****************************

RegFlag.Format = Format%

  IF Format% = RegOldFormat% THEN
  Translated$ = LTRIM$(Translated$)
  RegFlag.CommandLine = Translated$
  END IF

Reg.ReplaceFlags% = Format%
EXIT FUNCTION


'/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
'\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

ExtractWord:


'*****************
'Reset variables.
'*****************

Selection$ = ""
LenSelection% = 0


'*****************
'Extract word.
'*****************

  FOR CharPos% = CharStart% TO LenBuffer%
  Character$ = MID$(LocalBuffer$, CharPos%, 1)
  AscChar% = ASC(Character$)

    SELECT CASE AscChar%

      '******************
      'Space char found.
      '******************

      CASE SpaceChar%

        IF LenSelection% THEN
          CharStart% = CharPos%
        ELSE
          CharStart% = CharPos% + 1
          Selection$ = " "
        END IF

      RETURN

      '**********************
      'Flag beginning found.
      '**********************

      CASE ASC(FlagStartChar$)

        IF LenSelection% THEN
        CharStart% = CharPos%
        RETURN
        END IF

    END SELECT


  '********************
  'Select a character.
  '********************

  Selection$ = Selection$ + Character$
  LenSelection% = LenSelection% + 1


  '********************
  'Flag end found.
  '********************

    IF AscChar% = ASC(FlagEndChar$) THEN
    CharStart% = CharPos% + 1
    RETURN
    END IF

  NEXT


CharStart% = LenBuffer% + 1
RETURN


END FUNCTION

'============================================================================
'Notes:
'      The function reads the File% until it finds the corresponding
'      Extension$.
'
'Function returns:
'                  -1  if successful
'                   0  if EOF is reached
'----------------------------------------------------------------------------
'
FUNCTION Reg.Skip2Ext% (File%, Extension$)


  DO UNTIL EOF(File%)
  LINE INPUT #File%, Buffer$
  Buffer$ = LTRIM$(Buffer$)

    IF LEFT$(Buffer$, 1) = ExtStartChar$ THEN
      EndCharPos% = INSTR(Buffer$, ExtTermChar$)

        IF EndCharPos% THEN
          IF Reg.CompareExt%(Extension$, MID$(Buffer$, 2, EndCharPos% - 2)) THEN
          Reg.Skip2Ext% = -1
          EXIT FUNCTION
          END IF
        END IF

    END IF

  LOOP


Reg.Skip2Ext% = 0

END FUNCTION

