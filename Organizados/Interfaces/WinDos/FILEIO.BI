'Copyright (C) HiTech Labs, Denis Andrianov
'File I/O procedures.
'============================================================================

DECLARE SUB DosScr (BYVAL Save%)
DECLARE FUNCTION GetDrives$ ()
DECLARE FUNCTION ReadDir% (DirSpec$, BYVAL SearchType%, BYVAL Filter%)

DECLARE FUNCTION ChangeDirDrive% (Path$)
DECLARE FUNCTION ChangeFileAtt% (File$, Attribute%)

DECLARE SUB EmptyDir (Directory$)
DECLARE FUNCTION KillFileDir% (File$, Extension$, Directory$, BYVAL Directory%)
DECLARE FUNCTION MoveFileDir% (Source$, Destination$, BYVAL Directory%)
DECLARE FUNCTION CopyDir% (Source$, Destination$)
DECLARE FUNCTION CopyFile% (Source$, Destination$)
DECLARE FUNCTION MoveDirOnDisk% (Source$, Destination$)

DECLARE SUB FlushDOSBuffers ()

