DECLARE SUB LINQUT (LENGTH!, XVAL!, YVAL!)
COMMON SHARED INQUT$    'NEED TO HAVE THAT OR THE PROGRAM WON'T WORK AT ALL.
CLS
LINQUT 8, 3, 3

                        'Hey!  You don't need any of this to run the program,
                        'just copy the sub and make sure you COMMON SHARE the
                        'INQUT$ variable.  That's all you need.  Thanks.
                        '           Eric            EricMD@ix.netcom.com

SUB LINQUT (LENGTH, XVAL, YVAL)        'Hey!  Thanks for using this sub.  Put
K$ = ""                                'it in any program you want, just leave
A$ = ""                                'this screen the way it is.  Ok?
DO UNTIL K$ = CHR$(13)                 'Thanks.  Oh, Length is the number of
GOSUB KBHIT                            'characters available, XVAL and YVAL
IF LEN(K$) > 1 THEN                    'indicate the location on the screen.

IF RIGHT$(K$, 1) = "K" AND LEN(A$) >= 1 THEN A$ = LEFT$(A$, LEN(A$) - 1)
K$ = ""
END IF
IF K$ = "" AND LEN(A$) >= 1 THEN A$ = LEFT$(A$, LEN(A$) - 1): K$ = ""
IF LEN(A$) < LENGTH THEN A$ = A$ + K$
LOCATE YVAL, XVAL
PRINT A$ + SPACE$(LENGTH - LEN(A$))
LOOP
INQUT$ = A$
GOTO SUBOVER                         'If you like this sub, please write me
KBHIT:                               'at EricMD@ix.netcom.com so I know
K$ = ""                              'people use my stuff and I'll write more.
DO UNTIL K$ <> ""
K$ = INKEY$
LOOP
RETURN
SUBOVER:
IF LEN(INQUT$) < LENGTH THEN INQUT$ = LEFT$(INQUT$, LEN(INQUT$) - 1)
END SUB

