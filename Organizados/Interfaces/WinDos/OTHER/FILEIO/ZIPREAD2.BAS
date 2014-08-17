DECLARE FUNCTION ZipRead% (File$, Directory$)

CONST FilesFull = 1000
CONST DirectoriesFull = 2000

DIM SHARED Files$(1 TO FilesFull)              'File array.
DIM SHARED Extensions$(1 TO FilesFull)         'Extensions array.
DIM SHARED Directories$(1 TO DirectoriesFull)  'Directory array.
DIM SHARED FileSizes&(1 TO FilesFull)          'File sizes.

DIM SHARED FileNum%             'Number of files+directories.
DIM SHARED DirCount%            'Number of directories.
DIM SHARED FileCount%           'Number of files.

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

DIM SHARED ZipFile AS ZipCentralFileHeader
DIM SHARED ZipFileEnd AS ZipCentralDirEndHeader


CLS

OldTime! = TIMER
Success% = ZipRead%("a.zip", "")
TimeTaken! = TIMER - OldTime!

  FOR i% = 1 TO FileCount%
  PRINT Files$(i%), Extensions$(i%), FileSizes&(i%)
  NEXT

  FOR i% = 1 TO DirCount%
  PRINT Directories$(i%)
  NEXT

PRINT
PRINT "Files:"; FileCount%
PRINT "Dirs:"; DirCount%
PRINT "Total:"; FileNum%
PRINT "Time taken:"; TimeTaken!
PRINT

  IF Success% THEN
    PRINT "Success"
  ELSE
    PRINT "Failed"
  END IF



'        central file header signature   4 bytes  (0x02014b50)
'        version made by                 2 bytes
'        version needed to extract       2 bytes
'        general purpose bit flag        2 bytes
'        compression method              2 bytes
'        last mod file time              2 bytes
'        last mod file date              2 bytes
'        crc-32                          4 bytes
'        compressed size                 4 bytes
'        uncompressed size               4 bytes
'        filename length                 2 bytes
'        extra field length              2 bytes
'        file comment length             2 bytes
'        disk number start               2 bytes
'        internal file attributes        2 bytes
'        external file attributes        4 bytes
'        relative offset of local header 4 bytes
'
'        filename (variable size)
'        extra field (variable size)
'        file comment (variable size)
'
'      End of central dir record:
'
'        end of central dir signature    4 bytes  (0x06054b50)
'        number of this disk             2 bytes
'        number of the disk with the
'        start of the central directory  2 bytes
'        total number of entries in
'        the central dir on this disk    2 bytes
'        total number of entries in
'        the central dir                 2 bytes
'        size of the central directory   4 bytes
'        offset of start of central
'        directory with respect to
'        the starting disk number        4 bytes
'        zipfile comment length          2 bytes
'        zipfile comment (variable size)

'File$ = a ZIP file.
'ZipDir$ = directory inside a ZIP file, blank if first directory.
'
'Returns:
'           -1 if successful
'            0 if failed
'
FUNCTION ZipRead% (File$, ZipDir$)


'*********************************
'Initialize variables.
'*********************************

CONST ZipMaxBufferSize = 512

CDHeader$ = "PK" + CHR$(1) + CHR$(2)
EndCDHeader$ = "PK"
ZipRead% = -1


'*********************************
'Replace "\" with "/" if found for
'compatibility with ZIP.
'*********************************

Directory$ = UCASE$(ZipDir$)

  DO

    IF BackSlash% THEN
    MID$(Directory$, BackSlash%) = "/"
    END IF

  BackSlash% = INSTR(Directory$, "\")
  LOOP WHILE BackSlash%


'*********************************
'Read the end of central directory.
'*********************************

File% = FREEFILE

ON LOCAL ERROR GOTO ZipOpenError
OPEN File$ FOR BINARY AS #File%
ON LOCAL ERROR GOTO 0

FileSize& = LOF(File%)

  IF FileSize& >= ZipMaxBufferSize THEN
    BufferSize% = ZipMaxBufferSize
  ELSE
    BufferSize% = FileSize&
  END IF

Location& = FileSize& - ZipMaxBufferSize + 1



  DO

    IF Location& <= 0 THEN
    GOTO ZipGetError
    END IF

  Buffer$ = SPACE$(BufferSize%)

  ON LOCAL ERROR GOTO ZipGetError
  GET #File%, Location&, Buffer$
  ON LOCAL ERROR GOTO 0


  'Search the buffer backwards.
    FOR Find% = BufferSize% - 3 TO 1 STEP -1
      IF MID$(Buffer$, Find%, 4) = EndCDHeader$ THEN EXIT FOR
    NEXT
                                              

    IF Find% THEN
    Location& = Location& + Find% - 1 + 4
    ON LOCAL ERROR GOTO ZipGetError
    GET #File%, Location&, ZipFileEnd
    ON LOCAL ERROR GOTO 0
    EXIT DO
    END IF

  Location& = Location& - BufferSize% + 3
  LOOP


'PRINT ZipFileEnd.Disk
'PRINT ZipFileEnd.CentrDirDisk
'PRINT ZipFileEnd.Entries
'PRINT ZipFileEnd.TotalEntries
'PRINT ZipFileEnd.CentrDirSize
'PRINT ZipFileEnd.CentrDirOff
'PRINT ZipFileEnd.CommentLen


'*********************************
'Run some tests.
'*********************************
  
  IF ZipFileEnd.Disk <> ZipFileEnd.CentrDirDisk THEN
  GOTO ZipReadEnd
  END IF


'*********************************
'Read the central directory.
'*********************************

Location& = ZipFileEnd.CentrDirOff + 1


  DO UNTIL EOF(File%)

  Filename$ = SPACE$(4)
  ON LOCAL ERROR GOTO ZipGetError
  GET #File%, Location&, Filename$
  ON LOCAL ERROR GOTO 0

    IF INSTR(Filename$, CDHeader$) = 0 THEN
    EXIT DO
    END IF

  ON LOCAL ERROR GOTO ZipGetError
  GET #File%, , ZipFile
  ON LOCAL ERROR GOTO 0

    IF ZipFile.FileLen < 0 THEN
    GOTO ZipGetError
    END IF

  Filename$ = SPACE$(ZipFile.FileLen)
  ON LOCAL ERROR GOTO ZipGetError
  GET #File%, , Filename$
  ON LOCAL ERROR GOTO 0
  Filename$ = UCASE$(Filename$)


    IF INSTR(Filename$, Directory$) = 1 THEN
    Depth% = LEN(Directory$) + 1
    Slash% = INSTR(Depth%, Filename$, "/")

      IF Slash% THEN

          IF DirCount% < DirectoriesFull THEN
          Filename$ = MID$(Filename$, Depth%, Slash% - Depth%)
          Exists% = 0

            FOR Counter% = DirCount% TO 1 STEP -1
              IF Filename$ = Directories$(Counter%) THEN
              Exists% = -1
              END IF
            NEXT

            IF NOT (Exists%) THEN
            DirCount% = DirCount% + 1
            Directories$(DirCount%) = Filename$
            END IF

          END IF

      ELSE

          IF FileCount% < FilesFull THEN

            IF RIGHT$(Filename$, 1) <> "/" THEN
            FileCount% = FileCount% + 1
            Dot% = INSTR(Depth%, Filename$, ".")

              IF Dot% THEN
                Files$(FileCount%) = MID$(Filename$, Depth%, Dot% - Depth%)
                Extensions$(FileCount%) = MID$(Filename$, Dot% + 1)
              ELSE
                Files$(FileCount%) = MID$(Filename$, Depth%)
              END IF

            FileSizes&(FileCount%) = ZipFile.UncompressedSize
            END IF

          END IF

      END IF

    END IF


  Location& = Location& + 42 + ZipFile.FileLen + ZipFile.ExtraLen + ZipFile.CommentLen + 4
  LOOP



'*********************************
'Finish the job.
'*********************************

ZipReadEnd:

CLOSE #File%
FileNum% = DirCount% + FileCount%
EXIT FUNCTION


'============================================================================
'============================================================================
'============================================================================
ZipGetError:
CLOSE #File%

ZipOpenError:
ZipRead% = 0
EXIT FUNCTION

END FUNCTION

