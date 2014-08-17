'Copyright (C) HiTech Labs, Denis Andrianov
'WinDos include file.


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

