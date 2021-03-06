'INT 21 - Windows95 - FAT32 - GET EXTENDED FREE SPACE ON DRIVE
'        AX = 7303h
'        DS:DX -> ASCIZ string for drive ("C:\" or "\\SERVER\Share")
'        ES:DI -> buffer for extended free space structure (see #01789)
'        CX = length of buffer for extended free space
'Return: CF clear if successful
'            ES:DI buffer filled
'        CF set on error
'            AX = error code
'Notes:  on DOS versions which do not support the FAT32 calls, this function
'          returns CF clear/AL=00h (which is the DOS v1+ method for reporting
'          unimplemented functions)
'        under DOS 7.x (i.e. "MSDOS Mode" under Windows95), the ASCIZ string
'          pointed at by DS:DX *must* include the drive letter, or this function
'          will return CF set/AX=0015h (invalid drive).  In a DOS box, omitting
'          the drive letter (DS:DX -> "\") results in the free space for the
'          current default drive, as expected
'BUG:    this function returns a maximum of 2GB free space even on an FAT32
'          partition larger than 2GB under some versions of Win95 and Win98,
'          apparently by limiting the number of reported free clusters to no
'          more than 64K -- but only in a DOS window if a TSR has hooked INT 21h
'SeeAlso: AX=7302h,AX=7304h,AX=7305h,AH=36h
'
'Format of extended free space structure:
'Offset  Size    Description     (Table 01789)
' 00h    WORD    (ret) size of returned structure
' 02h    WORD    (call) structure version (0000h)
'                (ret) actual structure version (0000h)
' 04h    DWORD   number of sectors per cluster (with adjustment for compression)
' 08h    DWORD   number of bytes per sector
' 0Ch    DWORD   number of available clusters
' 10h    DWORD   total number of clusters on the drive
' 14h    DWORD   number of physical sectors available on the drive, without
'                  adjustment for compression
' 18h    DWORD   total number of physical sectors on the drive, without
'                  adjustment for compression
' 1Ch    DWORD   number of available allocation units, without adjustment
'                  for compression
' 20h    DWORD   total allocation units, without adjustment for compression
' 24h  8 BYTEs   reserved




TYPE RegTypeX
  ax    AS INTEGER
  bx    AS INTEGER
  cx    AS INTEGER
  dx    AS INTEGER
  bp    AS INTEGER
  si    AS INTEGER
  di    AS INTEGER
  flags AS INTEGER
  ds    AS INTEGER
  es    AS INTEGER
END TYPE

DIM Regs AS RegTypeX



TYPE ExtendedFreeSpaceStructure
  StrucSize         AS INTEGER
  StrucVersion      AS INTEGER
  SectorsPerCluster AS LONG
  BytesPerSector    AS LONG
  FreeClusters      AS LONG
  TotalClusters     AS LONG
  FreeSectors       AS LONG
  TotalSectors      AS LONG
  FreeAllocUnits    AS LONG
  TotalAllocUnits   AS LONG
  Reserved          AS DOUBLE
END TYPE

DIM DiskInfo AS ExtendedFreeSpaceStructure


CommandLine$ = COMMAND$
  IF LEN(CommandLine$) THEN
    Drive0$ = LEFT$(CommandLine$, 1) + ":\" + CHR$(0)
  ELSE
    Drive0$ = "C:\" + CHR$(0)
  END IF

Regs.ax = &H7303
Regs.ds = SSEG(Drive0$)
Regs.dx = SADD(Drive0$)
Regs.es = VARSEG(DiskInfo)
Regs.di = VARPTR(DiskInfo)
Regs.cx = 44

CALL interruptx(&H21, Regs, Regs)


FreeDiskSpace@ = DiskInfo.BytesPerSector * DiskInfo.SectorsPerCluster
TotalDiskSpace@ = FreeDiskSpace@ * DiskInfo.TotalClusters
FreeDiskSpace@ = FreeDiskSpace@ * DiskInfo.FreeClusters


CLS

PRINT "Free space on drive " + LEFT$(Drive0$, 1)
PRINT FreeDiskSpace@, "bytes"

PRINT "Total space on drive " + LEFT$(Drive0$, 1)
PRINT TotalDiskSpace@, "bytes"

