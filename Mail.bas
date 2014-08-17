ON ERROR GOTO errorman
IF COMMAND$ = "" THEN GOTO mailhelp
IF COMMAND$ = "/?" THEN GOTO help
GOTO mail
mailhelp:
        PRINT "Mail for Windows95 or NT (Must support user names and passwords)"
        PRINT
        PRINT "Usage: Mail [user] [/?] "
        PRINT "User:    Person to send mail to"
        PRINT "/?:      Help"
        INPUT "User to send mail to"; senduser$
        userdir$ = "C:\windows\profiles\" + senduser$ + "\desktop"
        CHDIR userdir$
        INPUT ".Txt file to send (none if not any), include full drive and path: ", txtfile$
        IF LCASE$(txtfile$) = "none" OR txtfiles$ = "" THEN GOTO editer
        IF RIGHT$(txtfile$, 4) = ".txt" THEN GOTO 50
        txtfile$ = txtfile$ + ".txt"
50      OPEN txtfile$ FOR INPUT AS #1
        OPEN "YOURMail.txt" FOR APPEND AS #2
        DO WHILE NOT EOF(1)
                LINE INPUT #1, text$
                PRINT #2, text$
        LOOP
        CLOSE
        PRINT "File sent!"
        SYSTEM

mail:
        INPUT "User to send mail to"; senduser$
        userdir$ = "C:\windows\profiles\" + senduser$ + "\desktop"
        CHDIR userdir$
        INPUT ".Txt file to send (none if not any), include full drive and path: ", txtfile$
        IF LCASE$(txtfile$) = "none" OR txtfiles$ = "" THEN GOTO editer
        IF RIGHT$(txtfile$, 4) = ".txt" THEN GOTO 60
        txtfile$ = txtfile$ + ".txt"
60      OPEN txtfile$ FOR INPUT AS #1
        OPEN "YOURMail.txt" FOR APPEND AS #2
        DO WHILE NOT EOF(1)
                LINE INPUT #1, text$
                PRINT #2, text$
        LOOP
        CLOSE
        PRINT "File sent!"
        SYSTEM


editer:
        CLOSE
        PRINT "This is a VERY primitive text editer, Type 'done' on a blank line to quit (file is saved on ever enter/return)"
        OPEN "YOURMail.txt" FOR APPEND AS #1
        DO
        LINE INPUT stuff$
        IF LCASE$(stuff$) = "done" THEN GOTO done
        PRINT #1, stuff$
        LOOP

help:
        PRINT "Mail for Windows95 or NT (Must support user names and passwords)"
        PRINT
        PRINT "Usage: Mail [user] [/?] "
        PRINT "User:    Person to send mail to"
        PRINT "/?:      Help"
        SYSTEM

done:
        PRINT "Message sent!"
        SYSTEM

errorman:
        IF ERR = 76 THEN PRINT "User doesn't exist"
        IF ERR = 53 THEN PRINT ".TXT file not found"
        SYSTEM

        

