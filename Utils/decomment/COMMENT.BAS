'               ----- C O M M E N T   R E M O V E R -----
'
'                              Version 0.1.1a
'                     (C) Oracle (webmaster@qbnz.com)
'                               2003/09/17
'
' This program removes comments from an existing QBasic program. It is useful
' for getting those modules down under 64K for compiling/running, without
' having to worry about commenting your programs. It also removes most blank
' lines.
'
' The decomment.zip file contains:
' *  comment.bas
' *  test.bas
' *  readme.txt
'
' This program is ALPHA. So it may (somehow, inconcievibly) damage your stuff.
' This program is provided AS IS, without any guarantee of fitness for any
' purpose whatsoever. This program is released under the GPL.
'
' For instructions, please consult the README.TXT file. If you want to suggest
' an addition to the program, please first consult the TODO.TXT file.

DECLARE FUNCTION decomment (filetodecomment$, newfile$)

' path$ variable. Set this either as an absolute path to where you want to
' want to select files from, or relative from QB if you're running it from
' the IDE.

DIM SHARED path AS STRING
path = "td13\"

CLS

DO
  INPUT "File to remove comments from? ", file$
  INPUT "New file name? ", newfile$

  IF decomment(file$, newfile$) THEN
    PRINT : PRINT "Success!"
    INPUT "Again? ", yn$
    IF LCASE$(LEFT$(yn$, 1)) = "n" THEN END
  ELSE
    PRINT "Error! Not completed!"
    PRINT "Debugging soon..."
  END IF
LOOP

FUNCTION decomment (file$, newfile$)

' Add .bas extension if you're lazy ;)
IF NOT (INSTR(file$, ".")) THEN file$ = file$ + ".bas"
IF NOT (INSTR(newfile$, ".")) THEN newfile$ = newfile$ + ".bas"

handle% = FREEFILE
OPEN path + file$ FOR INPUT AS #handle%
handle2% = FREEFILE
OPEN path + newfile$ FOR OUTPUT AS #handle2%

DO UNTIL EOF(handle%)
  LINE INPUT #handle%, line$
  ' Find out if the line has a comment in it.
  comment% = INSTR(line$, CHR$(39))
  IF comment% = 0 THEN comment% = INSTR(line$, "REM")
  ' Remove everything after the comment, if it exists
  count = count + 1
  IF comment% <> 0 THEN
    ' Check first to make sure it's not a metacommand
    IF INSTR(line$, "$") = 0 THEN line$ = LEFT$(line$, comment% - 1)
    'IF count > 34 AND count < 54 THEN PRINT INSTR(line$, "$INC")
  END IF
  ' Add the line to the new file.
  IF line$ <> "" THEN
    PRINT #handle2%, line$
  END IF
LOOP
CLOSE #handle%
CLOSE #handle2%

decomment = -1
END FUNCTION

