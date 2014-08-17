' If there are any errors goto errordetected
ON ERROR GOTO errordetected

' Example Errors
PSET (100, 100)
IF COMMAND$ = "k" THEN PRINT "hello"
x = 5 / 0

'To let you know the program worked
CLS
PRINT "Program Has Terminated Succsesfully"
END

'The beginning of the part of the program that lets you know what error it was
errordetected:
attempt = 0
SELECT CASE ERR
CASE 1
error$ = "NEXT without FOR"
CASE 2
error$ = "Syntax error"
CASE 3
error$ = "RETURN without GOSUB"
CASE 4
error$ = "Out of DATA"
CASE 5
error$ = "Illegal function call"
CASE 6
error$ = "Overflow"
CASE 7
error$ = "Out of memory"
CASE 8
error$ = "Label not defined"
CASE 9
error$ = "Subscript out of range"
CASE 10
error$ = "Duplicate definition"
CASE 11
error$ = "Division by zero"
CASE 12
error$ = "Illegal in direct mode"
CASE 13
error$ = "Type mismatch"
CASE 14
error$ = "Out of string space"
CASE 16
error$ = "String formula too complex"
CASE 17
error$ = "Cannot continue"
CASE 18
error$ = "Function not defined"
CASE 19
error$ = "No RESUME"
CASE 20
error$ = "RESUME without error"
CASE 24
error$ = "Device timeout"
CASE 25
error$ = "Device fault"
CASE 26
error$ = "FOR without NEXT"
CASE 27
error$ = "Out of paper"
message$ = "Put paper in the printer"
CASE 29
error$ = "WHILE without WEND"
CASE 30
error$ = "WEND without WHILE"
CASE 33
error$ = "Duplicate label"
CASE 35
error$ = "Subprogram not defined"
CASE 37
error$ = "Argument-count mismatch"
CASE 38
error$ = "Array not defined"
CASE 40
error$ = "Variable required"
CASE 50
error$ = "FIELD overflow"
CASE 51
error$ = "Internal error"
CASE 52
error$ = "Bad file name or number"
CASE 53
error$ = "File not found"
CASE 54
error$ = "Bad file mode"
CASE 55
error$ = "File already open"
CLOSE
attempt = 1
CASE 56
error$ = "FIELD statement active"
CASE 57
error$ = "Device I/O error"
CASE 58
error$ = "File already exists"
CASE 59
error$ = "Bad record length"
CASE 61
error$ = "Disk full"
CASE 62
error$ = "Input past end of file"
CASE 63
error$ = "Bad record number"
CASE 64
error$ = "Bad file name"
CASE 67
error$ = "Too many files"
attempt = 1
CLOSE
CASE 68
error$ = "Device unavailable"
CASE 69
error$ = "Communication-buffer overflow"
CASE 70
error$ = "Permission denied"
message$ = "Remove Write-Protection on drive: " + ERDEV$
CASE 71
error$ = "Disk not ready"
message$ = "Insert a disk in drive: " + ERDEV$
CASE 72
error$ = "Disk-media error"
message$ = "Insert a new disk in drive: " + ERDEV$
CASE 73
error$ = "Feature unavailable"
message$ = "Don't try to run Qbasic 4.5 programs in Qbasic 1.1"
CASE 74
error$ = "Rename across disks"
CASE 75
error$ = "Path/File access error"
CASE 76
error$ = "Path not found"
END SELECT
CLS
SCREEN 0
IF ERDEV$ <> olderdev$ THEN errdevice$ = ERDEV$
IF ERDEV$ = olderdev$ THEN errdevice$ = ""
PRINT "Program Interupted By Unexpected Error"
PRINT "Error: "; error$; "  Error Code:"; ERR; ""
IF ERL = 0 THEN PRINT "Error Occured On Line: N/A"
IF ERL <> 0 THEN PRINT "Error Occured On Line:"; ERL; ""
IF attempt = 1 THEN PRINT "The error has been auto-fixed"
IF attempt <> 1 THEN PRINT "The error line will be bypassed"
IF message$ <> "" THEN PRINT "Suggestions: "
IF message$ <> "" THEN PRINT message$

wantend:
' Just in case you want to leave
INPUT "Would you like to exit? > "; exit$
exit$ = LCASE$(exit$)
SELECT CASE exit$
CASE "y"
GOTO yesend
CASE "n"
GOTO noend
CASE "yes"
GOTO yesend
CASE "no"
GOTO noend
CASE "aye"
GOTO yesend
CASE "nay"
GOTO noend
CASE ELSE
GOTO wantend
END SELECT
yesend:
SYSTEM
noend:
'Clear the suggestion
message$ = ""
'If the error was corrected then resume on the error line
IF attempt = 1 THEN RESUME
' RESUME right after the error
RESUME NEXT

