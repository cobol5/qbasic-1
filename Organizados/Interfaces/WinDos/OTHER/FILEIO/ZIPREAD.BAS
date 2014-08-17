DECLARE FUNCTION ZipRead% (File$, Directory$)

CONST MaxBufferSize = 2048
CONST FilesFull = 1000
CONST DirectoriesFull = 2000

DIM SHARED Files$(1 TO FilesFull)              'File array.
DIM SHARED Extensions$(1 TO FilesFull)         'Extensions array.
DIM SHARED Directories$(1 TO DirectoriesFull)  'Directory array.
DIM SHARED FileSizes&(1 TO FilesFull)          'File sizes.

DIM SHARED FileNum%             'Number of files+directories.
DIM SHARED DirCount%            'Number of directories.
DIM SHARED FileCount%           'Number of files.

TYPE ZipLocalHeader
   Version          AS INTEGER
   GeneralFlag      AS INTEGER
   Compression      AS INTEGER
   FileTime         AS INTEGER
   FileDate         AS INTEGER
   CRC32            AS LONG
   CompressedSize   AS LONG
   UncompressedSize AS LONG
   FileLen          AS INTEGER
   ExtraLen         AS INTEGER
END TYPE

DIM SHARED ZipFile AS ZipLocalHeader


CLS

OldTime! = TIMER
Success% = ZipRead%("a:\test.zip", "")
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

'File$ = a ZIP file.
'ZipDir$ = directory inside a ZIP file, blank if first directory.
'
'Returns:
'           -1 if successful
'            0 if failed
'
FUNCTION ZipRead% (File$, ZipDir$)


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
'Initialize variables, open ZIP.
'*********************************

ZipRead% = -1
DirCount% = 0
FileCount% = 0
File% = FREEFILE

ON LOCAL ERROR GOTO ZipOpenError
OPEN File$ FOR BINARY AS #File%
ON LOCAL ERROR GOTO 0

Remaining& = LOF(File%)
CurByte& = 1
LastThree$ = "   "


'*********************************
'Read buffer, search for header
'and locate the file names.
'*********************************

  DO

    IF Remaining& >= MaxBufferSize THEN
      BufferSize% = MaxBufferSize
    ELSE
      BufferSize% = Remaining&
    END IF

  Remaining& = Remaining& - BufferSize%
  Buffer$ = SPACE$(BufferSize%)

  ON LOCAL ERROR GOTO ZipGetError
  GET #File%, CurByte&, Buffer$
  ON LOCAL ERROR GOTO 0

  Buffer$ = LastThree$ + Buffer$




    DO


      IF LocalHeaderPos% THEN

      ON LOCAL ERROR GOTO ZipGetError
      GET #File%, CurByte& - 1 + LocalHeaderPos% + 4 - 3, ZipFile
      ON LOCAL ERROR GOTO 0

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

      END IF


    LocalHeaderPos% = INSTR(LocalHeaderPos% + 1, Buffer$, "PK")
    LOOP WHILE LocalHeaderPos%


  CurByte& = CurByte& + BufferSize%
  LastThree$ = RIGHT$(Buffer$, 3)
  LOOP WHILE Remaining&



'*********************************
'Finish the job.
'*********************************

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

