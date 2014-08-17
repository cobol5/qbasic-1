' Mega Hacker game by Julian Yap version 1.0
' send all comments, criticisms, bug reports to:
' HEISTER@HOTMAIL.COM

DEFINT A-Z
DECLARE SUB RestoreSaved ()
DECLARE SUB SaveGame ()
DECLARE SUB MinusPrice ()
DECLARE SUB School ()
DECLARE SUB BBS ()
DECLARE SUB Bank ()
DECLARE SUB FBI ()
DECLARE SUB Store ()
DECLARE SUB Ringup ()
DECLARE SUB ClearCrap ()
DECLARE SUB EndScreen ()
DECLARE SUB Instructions ()
DECLARE SUB DisplayHeader ()
DECLARE SUB Welcome ()
DECLARE SUB LameIntro ()
COMMON SHARED Location$, Computer$, Money%, scrambler$, Modem$, Status$, Pass%
CONST sick$ = "Choose a number and press <ENTER>"
CONST filename$ = "MEGAHACK.SAV"

RANDOMIZE TIMER
' pass is now NOT
' if NOT pass ? "hoo" works!
Pass% = 1
OPEN filename$ FOR APPEND AS #1: CLOSE #1
LameIntro
DisplayHeader
Welcome

SUB Bank

Location$ = "Commonwealth Bank"
Status$ = sick$
MinusPrice
ClearCrap

CLS 2
PRINT "Connected to Commonwealth Bank"
PRINT

IF Modem$ = "9.6 Baud" OR Modem$ = "28.8 Baud" THEN
PRINT "Sorry, this online bank requires an ISDN connection"
PRINT
PRINT "Disconnected from Commonwealth Bank"
Status$ = "Press any key to return to Dial Up menu"
ClearCrap
BEEP
SLEEP
Ringup
END IF

IF scrambler$ = "None" OR scrambler$ = "Cruddy" THEN
    IF scrambler$ = "None" THEN i$ = "no" ELSE i$ = "a Cruddy"
    CLS 2
    PRINT "Suddenly you hear a click."
    PRINT "Agent Mulder from the FBI busts open your door."
    PRINT
    PRINT "Unfortunately your scrambler wasn't adequate to cope with the"
    PRINT "decryption methods of the Commonwealth Bank"
    PRINT
    PRINT "Your hacking days are over."
    PRINT "It serves you right for hacking in the wrong places with"
    PRINT i$; " scrambler."
    GOTO BankBail
END IF

' Now we can play
PRINT "  1) Hack some Credit Card numbers now!"
PRINT
PRINT "  2) Hang up"

DO WHILE (choice < 1) OR (choice > 2)
LOCATE 12, 1
INPUT "Choice (1-2):  ", choice
LOOP

SELECT CASE choice
CASE 1
GOTO BankFunTime
CASE ELSE
BEEP
Ringup
END SELECT

BankFunTime:
CLS 2
IF NOT Pass THEN
    PRINT "Sorry but you can't hack the Bank without a Credit card generator."
    PRINT "Try hacking Warlock's Underground BBS"
    GOTO BankBail
ELSE
    Location$ = "Commonwealth Bank - Hacking"
    ClearCrap
    number% = RND * 3 + 1
    dough% = RND * 400 + 301  ' 300 to 700
    IF number% = 2 THEN
        PRINT "Well done the credit card frauding worked."
        PRINT "You managed to leech out "; dough%; "dollars."
        Money% = Money% + dough%
        PRINT
        PRINT "You now have "; Money%; "dollars on hand."
    ELSE
        PRINT "Too bad, your credit card number proved to be invalid."
        PRINT "You didn't manage to leech out any cash."
    END IF
END IF

BankBail:
PRINT
PRINT "Disconnected from Commonwealth Bank"
BEEP
Status$ = "Press any key to return to Dail Up menu"
ClearCrap
SLEEP
Ringup
END SUB

SUB BBS

Location$ = "Warlock's Underground BBS"
Status$ = sick$
MinusPrice
ClearCrap

CLS 2
PRINT "Connected to Warlock's Underground BBS"
PRINT

IF Modem$ = "9.6 Baud" THEN
PRINT "Sorry, modems with speeds under 28.8 baud are not supported by this BBS"
GOTO BBSBail
END IF

IF scrambler$ = "None" THEN
    CLS 2
    PRINT "Suddenly you hear a click."
    PRINT "Agent Mulder from the FBI busts open your door."
    PRINT
    PRINT "Unfortunately your lack of a scrambler wasn't adequate to cope with the"
    PRINT "amateur tracing methods of Warlock's Underground BBS."
    PRINT
    PRINT "Your hacking days are over."
    PRINT "It serves you right for hacking in the wrong places with no Scrambler."
    BEEP
    Status$ = "Press any key to continue"
    ClearCrap
    SLEEP
    CALL EndScreen
END IF

' Now we can play
PRINT "  1) Hack this hole"
PRINT
PRINT "  2) Hang up"

DO WHILE (choice < 1) OR (choice > 2)
LOCATE 12, 1
INPUT "Choice (1-2):  ", choice
LOOP

SELECT CASE choice
CASE 1
GOTO BBSFunTime
CASE ELSE
BEEP
Ringup
END SELECT

BBSFunTime:
Location$ = "Warlock's Underground BBS - Hacking"
ClearCrap

CLS 2
PRINT "You must hack the BBS code number."
PRINT "It is between 100 and 1000."
PRINT
PRINT "You have 8 tries."       ' about a 1/7 chance
number% = RND * 900 + 101
count% = 0
151 PRINT
IF count% = 7 THEN
    PRINT "You have only one try left."
    INPUT "Do you want to bail? (Y/N)", Bail$
    PRINT
    IF UCASE$(Bail$) = "Y" THEN GOTO BBSBail
END IF
IF count% = 8 THEN
    CLS 2
    PRINT "You have no more tries left and you didn't hack it."
    PRINT "Luckily, you weren't caught."
    GOTO BBSBail
END IF
INPUT "Hack the code (100-1000):", guess%
IF guess% < 99 OR guess% > 1001 THEN
PRINT "Hey Hacker!  A number from 100 to 1000!"
GOTO 151
END IF
count% = count% + 1
PRINT "Tries "; count%
IF (guess% > number%) THEN PRINT "LOWER."
IF (guess% < number%) THEN PRINT "HIGHER."
IF (guess% <> number%) THEN GOTO 151
CLS 2
PRINT "Well done, you hacked it in "; count%; "tries!"
PRINT
PRINT "You continue to hack and display your hacking abilities to the BBS"
PRINT "The members are so pleased that they give a credit-card generating program"
PRINT "and the password which enables you to get money from the bank!"
PRINT
PRINT "So what are you waiting for! HACK THE BANK, NOW!"
Pass% = -1

BBSBail:
PRINT
PRINT "Disconnected from Warlock's Underground BBS"
BEEP
Status$ = "Press any key to return to Dail Up menu"
ClearCrap
SLEEP
Ringup

END SUB

' Clears stuff in the header then prints new stuff
SUB ClearCrap

VIEW PRINT
LOCATE 3, 11: PRINT STRING$(42, " ")
LOCATE 3, 63: PRINT STRING$(18, " ")
LOCATE 4, 8: PRINT STRING$(19, " ")
LOCATE 4, 38: PRINT STRING$(15, " ")
LOCATE 4, 60: PRINT STRING$(21, " ")
LOCATE 24, 1: PRINT STRING$(80, " ")

LOCATE 3, 11: PRINT Location$
LOCATE 3, 63: PRINT Computer$
LOCATE 4, 8: PRINT Money%
LOCATE 4, 38: PRINT scrambler$
LOCATE 4, 60: PRINT Modem$
LOCATE 24, 1: PRINT Status$
VIEW PRINT 6 TO 22
END SUB

' Displays the Header
SUB DisplayHeader
CLS
COLOR 9
PRINT "MEGA HACKER!"
PRINT
PRINT "Location:                 ";
PRINT "                          ";
PRINT "Computer:                 ";
PRINT "Money:                    ";
PRINT "Scrambler:                ";
PRINT "Modem:                    ";
PRINT STRING$(80, "п")
LOCATE 23, 1: PRINT STRING$(80, "м");
COLOR 7
END SUB

' End screen if you die or quit
SUB EndScreen

Location$ = "End Screen"
Status$ = "Press any key to end"
ClearCrap

CLS 2
PRINT
PRINT
PRINT "         ллллллллл"
PRINT "        л         л"
PRINT "       л           л"
PRINT "      л             л"
PRINT "      л             л"
PRINT "      л   R.I.P     л"
PRINT "      л             л"
PRINT "      л    An       л"
PRINT "      л  Unworthy   л"
PRINT "      л  Opponent   л"
PRINT "      л             л"
PRINT "      л             л"
PRINT "______л             л______________________________"
SLEEP
CLS 0
BLOAD "die.b"
SYSTEM
END SUB

SUB FBI

Location$ = "FBI Headquarters"
Status$ = "Choose a number and press enter"
MinusPrice
ClearCrap

CLS 2
PRINT "Connected to FBI Headquarters"
PRINT

IF Modem$ = "9.6 Baud" OR Modem$ = "28.8 Baud" OR Modem$ = "ISDN" THEN
PRINT "Sorry, the FBI Headquarters requires a Cable Modem connection"
PRINT
PRINT "Disconnected from FBI Headquarters"
Status$ = "Press any key to return to Dial Up menu"
ClearCrap
BEEP
SLEEP
Ringup
END IF

IF scrambler$ = "None" OR scrambler$ = "Cruddy" OR scrambler$ = "Average" THEN
    CLS 2
    PRINT "Suddenly you hear a click."
    PRINT "Agent Mulder from the FBI busts open your door."
    PRINT
    PRINT "Unfortunately your scrambler wasn't adequate to cope with the"
    PRINT "decryption methods of the FBI."
    PRINT
    PRINT "Your hacking days are over."
    PRINT "It serves you right for hacking in the FBI with a second rate Scrambler."
    GOTO FBIBail
END IF

' Now we can play
PRINT "  1) Hack around for the X-Files."
PRINT
PRINT "  2) Hang up"

DO WHILE (choice < 1) OR (choice > 2)
LOCATE 12, 1
INPUT "Choice (1-2):  ", choice
LOOP

SELECT CASE choice
CASE 1
GOTO FBIFunTime
CASE ELSE
BEEP
Ringup
END SELECT

FBIFunTime:
CLS 2
PRINT "You hack around and find a file which looks like the elusive X-Files."
PRINT
PRINT "You see the name `Agent Fox Mulder' on the file but find it is password"
PRINT "protected.  You must now depend upon your X-Files experience to guess it."
PRINT
INPUT "Password: ", a$
CLS 2
IF LCASE$(a$) = "trustno1" THEN
    PRINT "You got it!  Wow!  You being a die-hard X-Files fan has finally paid off."
    BEEP
    Status$ = "Press any key to continue"
    ClearCrap
    SLEEP
    CLS 0
    BLOAD "kudos.b"
    GOTO nexus
ELSE
    PRINT "You missed it"
END IF

FBIBail:
PRINT
PRINT "Disconnected from FBI Headquarters"
BEEP
Status$ = "Press any key to return to Dail Up menu"
ClearCrap
SLEEP
Ringup

nexus:
END
END SUB

' Show the instructions
SUB Instructions

Location$ = "Instructions"
Status$ = "Press any to key to return to the Welcome Screen"
ClearCrap

CLS 2
PRINT
PRINT "Overview:"
PRINT
PRINT "     A friend at school asks you to hack into the school's computer"
PRINT "to change their marks.  If you are successful in completing your first"
PRINT "mission you must show your friends that you are still a good hacker."
PRINT
PRINT "     You must continue performing dangerous hacking missions to"
PRINT "prove your abilities to your friends and prove that you are the"
PRINT "Supreme Elite Hacker."
SLEEP
CALL Welcome
END SUB

' A really lame intro
SUB LameIntro
Seconds! = .06          ' higher number makes it go slower
FOR a = 1 TO 20
    CLS
    LOCATE 10, a
    PRINT "Mega Hacker!"
    Start! = TIMER: DO WHILE TIMER < Start! + Seconds!: LOOP
NEXT a

FOR a = 49 TO 33 STEP -1
    CLS
    LOCATE 10, 20: PRINT "Mega Hacker!"
    LOCATE 10, a
    PRINT "Written by Julian Yap 1997 v1.0"
    Start! = TIMER: DO WHILE TIMER < Start! + Seconds!: LOOP
NEXT a

PLAY "l20o2a+>fa+f>c<f>d<a+"
CLS
BLOAD "intro.b"
SLEEP
END SUB

' Adjust price for each call
SUB MinusPrice
IF Modem$ = "9.6 Baud" OR Modem$ = "28.8 Baud" THEN
    Money% = Money% - 1
ELSEIF Modem$ = "ISDN" THEN
    Money% = Money% - 10
ELSE
    Money% = Money% - 100
END IF
END SUB

SUB RestoreSaved
Location$ = "Restore Saved Game"
Status$ = "Press any to key to return to the Welcome Screen"
ClearCrap

OPEN filename$ FOR INPUT AS #1
INPUT #1, Computer$, Money%, Scrabler$, Modem$, Pass%
CLOSE #1

CLS 2
PRINT "Your game has been restored to its last saved position!"
SLEEP
CALL Ringup

END SUB

' Ring up menu
SUB Ringup

Location$ = "Dail Up Menu"
Status$ = sick$
ClearCrap

CLS 2
PRINT
PRINT "Select an option:"
PRINT
PRINT "  1) Dial up Eleeto Grammar School"
PRINT "  2) Dial up Warlock's Underground BBS"
PRINT "  3) Dial up Bank"
PRINT "  4) Dial up FBI Headquarters"
PRINT "  5) Dial up Joe's Computer Store"
PRINT
PRINT "  6) Save game"
PRINT "  7) Exit game"

DO WHILE (choice < 1) OR (choice > 7)
LOCATE 18, 1
INPUT "Choice (1-7):  ", choice
LOOP

PRINT
IF choice >= 1 AND choice <= 5 THEN
PLAY "l12o2a+a+>a+<a+>a+<a+>a+<a+b"
PLAY "t100l32o2a+>a+<a+>a+<a+>a+<a+>a+<a+>a+<a+>a+<a+>a+<a"
PLAY "MBO3N0N0N0"
BEEP
    SELECT CASE choice
    CASE 1
        School
    CASE 2
        BBS
    CASE 3
        Bank
    CASE 4
        FBI
    CASE 5
        Store
    END SELECT
ELSEIF choice = 6 THEN SaveGame
ELSE CALL EndScreen
END IF

END SUB

SUB SaveGame

CLS 2
KILL filename$
OPEN filename$ FOR APPEND AS #1
WRITE #1, Computer$, Money%, scrambler$, Modem$, Pass%
CLOSE #1
PRINT
PRINT "Your game has been saved!"
BEEP
Status$ = "Press any key to return to Dail Up menu"
ClearCrap
SLEEP
Ringup

END SUB

SUB School

Location$ = "Elleeto Grammar School"
Status$ = sick$
MinusPrice
ClearCrap

CLS 2
PRINT "Connected to Eleeto Grammar School"
PRINT
PRINT "  1) Hack student records"
PRINT
PRINT "  2) Hang up"

DO WHILE (choice < 1) OR (choice > 2)
LOCATE 12, 1
INPUT "Choice (1-2):  ", choice
LOOP

SELECT CASE choice
CASE 1
GOTO FunTime
CASE ELSE
BEEP
Ringup
END SELECT

FunTime:
Location$ = "Eleeto Grammar School - Student Records"
ClearCrap

CLS 2
PRINT "You must hack the student code."
PRINT
PRINT "You have 7 tries."
number% = RND * 100 + 1
count% = 0
150 PRINT
IF count% = 6 THEN
    PRINT "You have only one try left."
    INPUT "Do you want to bail? (Y/N)", Bail$
    PRINT
    IF UCASE$(Bail$) = "Y" THEN GOTO Bail
END IF
IF count% = 7 THEN
    CLS 2
    PRINT "You have no more tries left and you didn't hack it."
    PRINT "Luckily, you weren't caught."
    GOTO Bail
END IF
INPUT "Hack the code (1-100):", guess%
IF guess% < 0 OR guess% > 101 THEN
PRINT "Hey Hacker!  A number from 1 to 100!"
GOTO 150
END IF
count% = count% + 1
PRINT "Tries "; count%
IF (guess% > number%) THEN PRINT "LOWER."
IF (guess% < number%) THEN PRINT "HIGHER."
IF (guess% <> number%) THEN GOTO 150
CLS 2
PRINT "Well done, you hacked it in "; count%; "tries!"
PRINT
PRINT "You continue to hack and change one of your friend's marks."
PRINT "Your friend is so pleased that he pays you 15 dollars!"
Money% = Money% + 15

Bail:
PRINT
PRINT "Disconnected from Eleeto Grammar School"
BEEP
Status$ = "Press any key to return to Dail Up menu"
ClearCrap
SLEEP
Ringup

END SUB

SUB Store

MinusPrice

Restart:
choice = 0
c = 0
Location$ = "Joe's Computer Store"
Status$ = sick$
ClearCrap
CLS 2
PRINT "Connected to Joe's Computer Store"
PRINT
PRINT "  1) Check out Computers"
PRINT "  2) Check out Scramblers"
PRINT "  3) Check out Modems"
PRINT
PRINT "  4) Hack this place"
PRINT "  5) Hang up"

DO WHILE (choice < 1) OR (choice > 5)
LOCATE 15, 1
INPUT "Choice (1-5):  ", choice
LOOP

SELECT CASE choice
CASE 1
GOTO Computers
CASE 2
GOTO Scramblers
CASE 3
GOTO Modems
CASE 4
    CLS 2
    PRINT "You begin to hack the defenceless Computer Store"
    PRINT
    PRINT "Suddenly you hear a click."
    PRINT "Your computer has CRASHED!"
    PRINT
    PRINT "Joe finds out you tried to hack his computer system and reports"
    PRINT "you to the police."
    PRINT
    PRINT "Your hacking days are over."
    PRINT "It serves you right for hacking your friend Joe's machine."
    BEEP
    Status$ = "Press any key to continue"
    ClearCrap
    SLEEP
    CALL EndScreen
CASE ELSE
BEEP
Ringup
END SELECT

Computers:
CLS 2
PRINT "Computer Section"
PRINT
PRINT " 1) Purchase a Pentium 166 Desktop                   3500$"
PRINT
PRINT " 2) Back to Main Menu"

DO WHILE c < 1 OR c > 2
LOCATE 12, 1
INPUT "Choice (1-2):  ", c
LOOP

Status$ = "Press any key to return to Main Menu"
ClearCrap
CLS 2

SELECT CASE c
CASE 1
    IF Money - 3500 <= 0 THEN
        PRINT "You do not have enough money to make that purchase"
    ELSE Money = Money - 3500
        Computer$ = "P166 Desktop"
        PRINT "Thank you for purchasing a top quality Pentium 166 desktop."
        PRINT
        PRINT "Your machine will be able to keep up with labour intensive equipment,"
        PRINT "including top of the line Scramblers and Cable Modems."
    END IF
CASE 2
END SELECT
GOTO Sorry

Scramblers:
CLS 2
PRINT "Scramblers Section"
PRINT
PRINT " 1) Purchase a Cruddy Scrambler                      100$"
PRINT " 2) Purchase an Average Scrambler                    350$"
PRINT " 3) Purchase a Super Scrambler                       2500$"
PRINT
PRINT " 4) Back to Main Menu"

DO WHILE c < 1 OR c > 4
LOCATE 14, 1
INPUT "Choice (1-4):  ", c
LOOP

Status$ = "Press any key to return to Main Menu"
ClearCrap
CLS 2
SELECT CASE c
CASE 1
    IF Money - 100 <= 0 THEN
        PRINT "You do not have enough money to make that purchase"
    ELSE Money = Money - 100
        scrambler$ = "Cruddy"
        PRINT "Thank you for purchasing a Cruddy Scrambler."
        PRINT
        PRINT "Your scrambler will be able to scramble many of today's telephone"
        PRINT "lines and prevent your calls being traced by amateur equipment."
    END IF
CASE 2
    IF Money - 350 <= 0 THEN
        PRINT "You do not have enough money to make that purchase"
    ELSE Money = Money - 350
        scrambler$ = "Average"
        PRINT "Thank you for purchasing an Average Scrambler."
        PRINT
        PRINT "Your scrambler will be able to scramble many of today's telephone"
        PRINT "lines and prevent your calls being traced by professional equipment."
    END IF
CASE 3
    IF Computer$ = "486 Laptop" THEN
        PRINT "Joe recommends that you purchase a Pentium desktop before you buy this,"
        PRINT "otherwise it won't work."
        GOTO Sorry
    END IF
    IF Money - 2500 <= 0 THEN
        PRINT "You do not have enough money to make that purchase"
    ELSE Money = Money - 2500
        scrambler$ = "Super"
        PRINT "Thank you for purchasing a Super Scrambler."
        PRINT
        PRINT "Your scrambler will be able to scramble all of today's telephone"
        PRINT "lines and prevent your calls being traced by ANYBODY!"
    END IF
END SELECT
GOTO Sorry

Modems:
CLS 2
PRINT "Modems Section"
PRINT
PRINT " 1) Purchase a 28.8 Baud Modem                       400$"
PRINT " 2) Purchase an ISDN Modem                           1000$"
PRINT " 3) Purchase a Cable Modem                           10000$"
PRINT
PRINT " 4) Back to Main Menu"

DO WHILE c < 1 OR c > 4
LOCATE 14, 1
INPUT "Choice (1-4):  ", c
LOOP

Status$ = "Press any key to return to Main Menu"
ClearCrap
CLS 2
SELECT CASE c
CASE 1
    IF Money - 400 <= 0 THEN
        PRINT "You do not have enough money to make that purchase"
    ELSE Money = Money - 400
        Modem$ = "28.8 Baud"
        PRINT "Thank you for purchasing a 28.8 Baud Modem."
        PRINT
        PRINT "Your modem will be able to use all of today's normal telephone"
        PRINT "lines at high speed."
        PRINT
        PRINT "Remember, calls with this modem cost 1 dollar each."
    END IF
CASE 2
    IF Money - 1000 <= 0 THEN
        PRINT "You do not have enough money to make that purchase"
    ELSE Money = Money - 1000
        Modem$ = "ISDN"
        PRINT "Thank you for purchasing an ISDN modem."
        PRINT
        PRINT "Your modem will be able to use today's special ISDN telephone lines"
        PRINT "at a speed more than double that of a conventional modem."
        PRINT
        PRINT "Remember, calls with this modem cost 10 dollars each."
    END IF
CASE 3
    IF Computer$ = "486 Laptop" THEN
        PRINT "Joe recommends that you purchase a Pentium desktop before you buy this,"
        PRINT "otherwise it won't work."
        GOTO Sorry
    END IF
    IF Money - 10000 <= 0 THEN
        PRINT "You do not have enough money to make that purchase"
    ELSE Money = Money - 10000
        Modem$ = "Cable"
        PRINT "Thank you for purchasing a Cable Modem."
        PRINT
        PRINT "Your modem will be able to use today's cable television lines at an"
        PRINT "enormous speed of up to 10 megabytes per second."
        PRINT
        PRINT "Remember, calls with this modem cost 100 dollars each."
    END IF
CASE 4
END SELECT

Sorry:
BEEP
SLEEP
GOTO Restart

END SUB

' Welcome screen
SUB Welcome
' Remember to set all variables to default

choice = 0

Location$ = "Welcome Screen"
Status$ = sick$
ClearCrap

CLS 2
PRINT
PRINT "Select an option:"
PRINT
PRINT "  1) Start a new game"
PRINT "  2) Restore a previously saved game"
PRINT "  3) Read Instructions"
PRINT "  4) EXIT Mega Hacker game"

DO WHILE (choice < 1) OR (choice > 4)
LOCATE 14, 1
INPUT "Choice (1-4):  ", choice
LOOP

SELECT CASE choice
CASE 1
    Computer$ = "486 Laptop"
    Money% = 250
    scrambler$ = "None"
    Modem$ = "9.6 Baud"
    Ringup
CASE 2
    RestoreSaved
CASE 3
    Instructions
CASE 4
    CALL EndScreen
END SELECT

END SUB


