
TYPE RegType
  ax    AS INTEGER
  bx    AS INTEGER
  cx    AS INTEGER
  dx    AS INTEGER
  bp    AS INTEGER
  si    AS INTEGER
  di    AS INTEGER
  flags AS INTEGER
END TYPE

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

TYPE DTAStructure
  DOS            AS STRING * 19
  CreateTime     AS STRING * 2
  Attributes     AS STRING * 1
  AccessTime     AS STRING * 2
  AccessDate     AS STRING * 2
  FileSize       AS LONG
  Filename       AS STRING * 13
END TYPE

TYPE ZipCentralFileHeader
   VersionMade      AS INTEGER
   VersionExtract   AS INTEGER
   GeneralFlag      AS INTEGER
   Compression      AS INTEGER
   FileTime         AS INTEGER
   FileDate         AS INTEGER
   CRC32            AS LONG
   CompressedSize   AS LONG
   UncompressedSize AS LONG
   FileLen          AS INTEGER
   ExtraLen         AS INTEGER
   CommentLen       AS INTEGER
   DiskStart        AS INTEGER
   InternalAtt      AS INTEGER
   ExternalAtt      AS LONG
   LocalHeaderOff   AS LONG
END TYPE

TYPE ZipCentralDirEndHeader
   Disk             AS INTEGER
   CentrDirDisk     AS INTEGER
   Entries          AS INTEGER
   TotalEntries     AS INTEGER
   CentrDirSize     AS LONG
   CentrDirOff      AS LONG
   CommentLen       AS INTEGER
END TYPE

TYPE DiskBlockStructure
  Info           AS INTEGER
  SerialNumber   AS LONG
  Label          AS STRING * 11
  Other          AS STRING * 8
END TYPE

TYPE ExtendedDiskStructure
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
  Reserved          AS STRING * 8
END TYPE

