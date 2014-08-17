CONST ConfigFile = "HTEdit.cfg"

COMMON SHARED MainTxtChanged%
COMMON SHARED Port$
COMMON SHARED Filename$
COMMON SHARED WordWrap%
COMMON SHARED Sep1$
COMMON SHARED Sep2$
COMMON SHARED Sep3$

SUB ReadCFG ()
ON LOCAL ERROR GOTO ReadError

Header$ = SPACE$(6)
Sep1$ = " "
Sep2$ = " "
Sep3$ = " "
Port$ = "    "

File% = 1
OPEN ConfigFile FOR BINARY AS #File%

GET #File%, , Header$
GET #File%, , WordWrap%
GET #File%, , Sep1$
GET #File%, , Sep2$
GET #File%, , Sep3$
GET #File%, , Port$

CLOSE #File%

  IF Header$ <> "HTEdit" THEN
  KILL ConfigFile
  MSGBOX "Configuration file corrupt."
  END IF

AfterReadError:
EXIT SUB

'============================================================================
ReadError:
MSGBOX "Cannot read configuration file."
CLOSE
RESUME AfterReadError

END SUB

SUB SaveCFG ()
ON LOCAL ERROR GOTO WriteError

Header$ = "HTEdit"

File% = 1
OPEN ConfigFile FOR BINARY AS #File%

PUT #File%, , Header$
PUT #File%, , WordWrap%
PUT #File%, , Sep1$
PUT #File%, , Sep2$
PUT #File%, , Sep3$
PUT #File%, , Port$

CLOSE #File%

AfterWriteError:
EXIT SUB

'============================================================================
WriteError:
MSGBOX "Cannot update configuration file."
CLOSE
RESUME AfterWriteError

END SUB

