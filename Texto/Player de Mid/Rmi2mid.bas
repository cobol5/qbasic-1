'RMI2MID
'Utility to convert Microsoft RMID files to Standard MIDI format.
'By Jesse Dorland (jessedorland@hotmail.com)
'Released to the Public Domain, October 1998
'
'RMI2MID converts Microsoft RMID (.RMI) files to Standard MIDI (.MID)
'format, so they can be used with QMIDI.
'
'At the "Input File" prompt, type the name of the file you want to convert.
'If you don't provide a file extension, ".RMI" is assumed.
'
'At the "Output File" prompt, enter the file name that you want the
'new data to be written to.  If you don't provide an extension, ".MID" is
'assumed.
'

CLS
PRINT "RMI2MID v1.0"
PRINT "By Jesse Dorland"

GetInputFile:
PRINT
INPUT "Input File: ", InputFile$
IF InputFile$ = "" THEN END
IF INSTR(InputFile$, ".") = 0 THEN InputFile$ = InputFile$ + ".RMI"

OPEN InputFile$ FOR BINARY AS #1
IF LOF(1) = 0 THEN
    CLS
    PRINT
    PRINT "The file you specified does not exist.  Please type in"
    PRINT "a different file name."
    CLOSE #1
    KILL InputFile$
    GOTO GetInputFile:
END IF
Header$ = INPUT$(12, #1)
IF NOT (LEFT$(Header$, 4) = "RIFF" AND RIGHT$(Header$, 4) = "RMID") THEN
    CLS
    PRINT
    PRINT InputFile$
    PRINT "is not a valid .RMI file.  Please specify a different file."
    CLOSE #1
    GOTO GetInputFile:
END IF

GetOutputFile:
PRINT
INPUT "Output File: ", OutputFile$
IF OutputFile$ = "" THEN END
IF INSTR(OutputFile$, ".") = 0 THEN OutputFile$ = OutputFile$ + ".MID"

OPEN OutputFile$ FOR BINARY AS #2
IF LOF(2) <> 0 THEN
    CLS
    PRINT
    PRINT OutputFile$
    PRINT "already exists.  Please specify a different file."
    CLOSE #2
    GOTO GetOutputFile:
END IF
SEEK #1, 21
DO UNTIL (EOF(1))
    Buffer$ = INPUT$(4096, #1)
    PUT #2, , Buffer$
LOOP
CLOSE
END

