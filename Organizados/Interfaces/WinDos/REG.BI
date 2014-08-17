'Copyright (C) HiTech Labs, Denis Andrianov
'Extension Registration procedures.
'============================================================================

DECLARE FUNCTION Reg.MakeBatch% (RegFile$, Extension$, LabelIndex%, BatchFile$, State AS ANY, RegFlag AS ANY)
DECLARE FUNCTION Reg.GetLabels% (RegFile$, Extension$, Labels$())
DECLARE FUNCTION Reg.CompareExt% (Extension$, Mask$)
DECLARE FUNCTION Reg.ReplaceFlags% (Buffer$, State AS ANY, RegFlag AS ANY, Translated$)
DECLARE FUNCTION Reg.Skip2Ext% (File%, Extension$)


CONST MaxDirNameSize = 65              'With trailing '\'
CONST MaxFileNameSize = 12
CONST MaxPathNameSize = 2 + MaxDirNameSize + MaxFileNameSize
CONST MaxCMDLineSize = 125

CONST RegNewFormat% = 1                'Extension description format
CONST RegOldFormat% = 2


TYPE FMStateStruc
   Drive        AS STRING * 1
   Dir          AS STRING * MaxDirNameSize
   DirSlash     AS STRING * MaxDirNameSize
   FileExt      AS STRING * MaxFileNameSize
   File         AS STRING * MaxFileNameSize
END TYPE

TYPE RegFlagsStruc
   Format       AS INTEGER
   Executable   AS STRING * MaxPathNameSize
   CommandLine  AS STRING * MaxCMDLineSize
   SwitchDir    AS INTEGER
   Pause        AS INTEGER
   DontPause    AS INTEGER
   KeysOn       AS INTEGER
   KeyFile      AS STRING * MaxPathNameSize
   ListOn       AS INTEGER
   ListFile     AS STRING * MaxPathNameSize
END TYPE

