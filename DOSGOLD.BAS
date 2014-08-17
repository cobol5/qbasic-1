DECLARE SUB calc ()
DECLARE SUB insult ()
DECLARE SUB CREDITS ()
DECLARE SUB DUMSONG ()
DECLARE SUB HILO ()
DECLARE SUB INSULTUN ()
DECLARE SUB BASH ()
DECLARE SUB FORTUNE ()
DECLARE SUB CHEESY ()
DECLARE SUB STEVE ()
DECLARE SUB SECRET ()
DECLARE SUB BIRTHDAY ()
DECLARE SUB TIME ()
DECLARE SUB BEEPREG ()
DECLARE SUB ROCK ()
DECLARE SUB MUSIC2 ()
'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
Qp = VAL(RIGHT$(TIME$, 2))
RANDOMIZE (Qp)
da$ = "MBT180o2P2P8L8GGGL2E-P24P8L8FFFL2D"
MUSIC1$ = "MBo3L8ED+ED+Eo2Bo3DCL2o2A"
MORT$ = "t230l7o3ao4ep24o3ao4cp24o3ab-p24ao4cp24o3b-o4co4o3gp24t230l7o3ao4ep24o3ao4cp24o3ab-p24ao4cp24o3b-o4co4o3gp24t230l7o3ao4ep24o3ao4cp24o3agp24gap4"
MORT1$ = "t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24aaaaaaaap24ggggp24o4ccccp20t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24aaaaaaaap24ggggp24o3eeeep20t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24aaaaaaaap24ggggp24o4ccccp20t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24l7aaa"
MISS$ = "<g6p20g10g6b-6>c6<g6p20g10g6b-6>c6<g6p20g10g6p10>>b-10g10d4p10b-10g10d-4p10b-10g10c4p10<b-10>c10p8<"
CLS : CREDITS
574737 CLS
INPUT "Do you enjoy the sunrise"; a$
IF a$ = "calc" THEN GOTO calc
IF a$ = "music" THEN GOTO 97123
IF a$ = "skip" THEN GOTO 79
IF a$ = "gregdos" THEN 1046
IF a$ = "list" THEN GOTO 1234
IF a$ = "exit" THEN SYSTEM
IF a$ = "insult" THEN GOTO 555
IF a$ = "beep" THEN GOTO 66697
IF a$ = "color" THEN GOTO 1050
IF a$ = "hilo" THEN GOTO 100034
IF a$ = "cheesy" THEN GOTO 100000
IF a$ = "fortune" THEN GOTO 67
IF a$ = "name" THEN GOTO 57
IF a$ = "rock" THEN GOTO 7767
IF a$ = "bash" THEN GOTO 8989
IF a$ = "suck" THEN GOTO 11221
IF a$ = "secret" THEN GOTO 123456789
IF a$ = "birthday" THEN GOTO 1279721
IF a$ = "time" THEN GOTO 1234321
'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
PLAY MISS$
CLS : DUMSONG
79 SLEEP 2: DO: BEEP: PRINT "Press space bar to continue": LOOP UNTIL INKEY$ = " "
'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
CLS
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT "DosGold"
SLEEP 1
CLS
xyz = INT(RND * 14) + 1
COLOR xyz, 0
SLEEP 1
'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
57 CLS
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT TAB(15); "Greg Lavery made DosGold"
PRINT : PRINT : PRINT
DIM b$(40)
PRINT "Hello"
PRINT "My name is Greg Lavery"
INPUT "What's your name (First $ last)"; a$
L = LEN(a$)
IF a$ = "calc" THEN GOTO calc
IF a$ = "music" THEN GOTO 97123
IF a$ = "skip" THEN GOTO 79
IF a$ = "gregdos" THEN 1046
IF a$ = "list" THEN GOTO 1234
IF a$ = "exit" THEN SYSTEM
IF a$ = "insult" THEN GOTO 555
IF a$ = "beep" THEN GOTO 66697
IF a$ = "color" THEN GOTO 1050
IF a$ = "hilo" THEN GOTO 100034
IF a$ = "cheesy" THEN GOTO 100000
IF a$ = "fortune" THEN GOTO 67
IF a$ = "name" THEN GOTO 57
IF a$ = "rock" THEN GOTO 7767
IF a$ = "bash" THEN GOTO 8989
IF a$ = "suck" THEN GOTO 11221
IF a$ = "secret" THEN GOTO 123456789
IF a$ = "birthday" THEN GOTO 1279721
IF a$ = "time" THEN GOTO 1234321
PRINT : PRINT "Thank You, ";
FOR i = 1 TO L: b$(i) = MID$(a$, i, 1): NEXT i
FOR i = L TO 1 STEP -1: PRINT b$(i); : NEXT i
PRINT ", Oops!  I guess I got it backwards.  A smart"
PRINT "kid like me shouldn't have made a mistake like that, whoops.": PRINT
PRINT "I just noticed your letters are out of order."
PRINT "Let's put them in order: ";
FOR j = 2 TO L: i = j - 1: T$ = b$(j)
11057 IF T$ > b$(i) THEN 13057
b$(i + 1) = b$(i): i = i - 1: IF i > 0 THEN 11057
13057 b$(i + 1) = T$: NEXT j
FOR i = 1 TO L: PRINT b$(i); : NEXT i: PRINT : PRINT
15057 PRINT "Don't you like that better?"; : INPUT d$
d$ = LEFT$(d$, 1)
IF d$ = "y" THEN 18057
IF d$ = "n" THEN 17057
PRINT : PRINT "You shouldn't of said that say y or n": GOTO 15057
17057 CLS
PRINT : PRINT "You are a big moron": GOTO 20057
18057 PRINT : PRINT "I really enjoyed meeting you (kind of) "; a$; "."
20057 PRINT "Have a spiffy day!"
SLEEP 1
'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
CLS
SLEEP 1
GOTO 25
'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
1 CLS
PRINT "DosGold"
xyz = INT(RND * 14) + 1
COLOR xyz, 0
GOTO 25
'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
24 COLOR i, j
IF i = j THEN COLOR 1, 0
3 CLS
PRINT "                This is the registered version of DosGold"
PRINT "                          The time is: "; TIME$
PRINT "                    The date of today is: "; DATE$
PRINT "                    type -list- to show the commands"
2 INPUT "Command:  ", comm$
IF comm$ = "list" THEN GOTO 1234
IF comm$ = "exit" THEN SYSTEM
IF comm$ = "insult" THEN GOTO 555
IF comm$ = "beep" THEN GOTO 66697
IF comm$ = "color" THEN GOTO 1050
IF comm$ = "hilo" THEN GOTO 100034
IF comm$ = "cheesy" THEN GOTO 100000
IF comm$ = "fortune" THEN GOTO 67
IF comm$ = "rock" THEN GOTO 7767
IF comm$ = "bash" THEN GOTO 8989
IF comm$ = "suck" THEN GOTO 11221
IF comm$ = "secret" THEN GOTO 123456789
IF comm$ = "birthday" THEN GOTO 1279721
IF comm$ = "music" THEN GOTO 97123
IF comm$ = "unreg" THEN GOTO 25
IF comm$ = "time" THEN GOTO 1234321
IF comm$ = "calc" THEN GOTO calc
PLAY MUSIC1$: PRINT "Type one of the commands"
BEEP: GOTO 24
'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
1234 CLS
PRINT "These are the really spiffy commands": PRINT
PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออป"
PRINT "บ hilo- Guess the right number and get money  บ"
PRINT "บ rock- Play rock, paper, scissors            บ"
PRINT "บ fortune- Have tour fortune told             บ"
PRINT "บ music- Listen to spiffy music               บ"
PRINT "บ calc- Use a scientific calculator           บ"
PRINT "บ cheesy- Play a cheesy game                  บ"
PRINT "บ insult- Lets you insult the computer        บ"
PRINT "บ beep- Listen to some beeps                  บ"
PRINT "บ color- Change the color of text             บ"
PRINT "บ birthday- Listen to happy birthday          บ"
PRINT "บ time- See what time it is                   บ"
PRINT "บ unreg- Unregister for the fun of it         บ"
PRINT "บ list- List all of the commands              บ"
PRINT "บ exit- You better know or ka-stinking pow    บ"
PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออผ"
PLAY MORT1$
GOTO 2
'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
25 SLEEP 2: CLS
PRINT "                                   DosGold"
PRINT "                            The time is: "; TIME$
PRINT "                      The date of today is: "; DATE$
PRINT "                      Type -list- to show the commands"
INPUT "Command:  ", comm$
IF comm$ = "list" THEN GOTO 12345
IF comm$ = "exit" THEN SYSTEM
IF comm$ = "insult" THEN GOTO 5555
IF comm$ = "beep" THEN GOTO 6666
IF comm$ = "name" THEN GOTO 57
IF comm$ = "reg" THEN GOTO 9000
PRINT "Type one of the commands"
BEEP: SLEEP 1: GOTO 12345
'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
12345 CLS
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT "These are the really spiffy commands": PRINT
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออป"
PRINT "บ list- Lists all of the commands             บ"
PRINT "บ exit- You better know                       บ"
PRINT "บ name- Have a conversation with the computer บ"
PRINT "บ insult- Lets you insult the computer        บ"
PRINT "บ beep- A series of beeps                     บ"
PRINT "บ reg- Register                               บ"
PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออผ"
xyz = INT(RND * 14) + 1
COLOR xyz, 0
INPUT "Command:  ", comm$
IF comm$ = "list" THEN GOTO 12345
IF comm$ = "exit" THEN SYSTEM
IF comm$ = "insult" THEN GOTO 5555
IF comm$ = "beep" THEN GOTO 6666
IF comm$ = "name" THEN GOTO 57
IF comm$ = "reg" THEN GOTO 9000
PRINT "Type one of the commands"
BEEP: GOTO 25
'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
9000 CLS
xyz = INT(RND * 14) + 1
COLOR xyz, 0
INPUT "Please enter registration password"; R$
IF R$ = "gregdos" THEN 1046
PRINT "Wrong password jerkboy": GOTO 6911
1046 CLS
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT "Thank you for registering": SLEEP 1
'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
1050 CLS
xyz = INT(RND * 14) + 1
COLOR xyz, 0
1051 PLAY MUSIC1$
CLS : PRINT "Choose from these colors"
PRINT "ษออออออออออออออออออออออป"
COLOR 0, 1
PRINT "บ  0 = Black           บ"
COLOR 1, 0
PRINT "บ  1 = Blue            บ"
COLOR 2
PRINT "บ  2 = Booger Green    บ"
COLOR 3
PRINT "บ  3 = Light blue      บ"
COLOR 4
PRINT "บ  4 = Red             บ"
COLOR 5
PRINT "บ  5 = Purple          บ"
COLOR 6
PRINT "บ  6 = Yucky Orange    บ"
COLOR 7
PRINT "บ  7 = Light Gray      บ"
COLOR 8
PRINT "บ  8 = Gray            บ"
COLOR 9
PRINT "บ  9 = Mid-Blue        บ"
COLOR 10
PRINT "บ 10 = Light Green     บ"
COLOR 11
PRINT "บ 11 = Very-Light Blue บ"
COLOR 12
PRINT "บ 12 = Pinkish Red     บ"
COLOR 13
PRINT "บ 13 = Pink            บ"
COLOR 14
PRINT "บ 14 = Yellow          บ"
COLOR 15
PRINT "บ 15 = White           บ"
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT "ศออออออออออออออออออออออผ"
PRINT "Add 16 to the number of the color if you want that color to be flashing"
INPUT "What color do you want for your text"; i
IF i > -1 AND i < 32 THEN GOTO 1414
PRINT "Pick one of the colors":
1414 PRINT : INPUT "What color do you want for your backround"; j
IF i = j + 16 THEN 16
IF i = j THEN 16
IF j > -1 AND j < 16 THEN GOTO 24
16 PRINT "Don't pick the same colors": SLEEP 2: GOTO 1051
'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
6666 CLS
PRINT "You will now here a series of beeps": SLEEP 2: GOTO 7000
6911 PRINT "That's not the right password you jerk"
7000 CLS : BEEP: BEEP: BEEP: BEEP: BEEP: PRINT "Having fun": BEEP: BEEP: BEEP: BEEP: BEEP: CLS : PRINT "YOU ARE NOT VERY SMART": BEEP: BEEP: BEEP: BEEP: BEEP: BEEP: BEEP: BEEP: BEEP: BEEP: BEEP: CLS
GOTO 1
'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
555 CLS : insult: GOTO 24
5555 CLS : INSULTUN: GOTO 25
100034 CLS : HILO: GOTO 24
100000 CLS : CHEESY: GOTO 24
67 CLS : FORTUNE: GOTO 24
66697 CLS : BEEPREG: GOTO 24
7767 CLS : ROCK: GOTO 24
8989 CLS : BASH: GOTO 66697
11221 CLS : STEVE: GOTO 24
123456789 CLS : SECRET: GOTO 24
1279721 CLS : BIRTHDAY: GOTO 24
1234321 CLS : TIME: GOTO 24
97123 CLS : MUSIC2: GOTO 24
calc: CLS : calc: COLOR 1, 0: GOTO 3
'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
'ษปศผอบ

SUB BASH
CLS
INPUT "What's your name?"; NAMEBASH$
CLS
PRINT ""; NAMEBASH$; ", I'm going to BASH your stinking head in"
SLEEP 1
CLS
PRINT : PRINT : PRINT "I think you should shove it up Steve's butt"
SLEEP 1
CLS

END SUB

SUB BEEPREG
CLS
PLAY MUSIC1$
PRINT "I hope you enjoy this": SLEEP 1
PRINT "Ha-Ha"
BEEP
SLEEP 1
CLS
PRINT "Enjoying yourself?"
BEEP
SLEEP 1
CLS
PRINT "Sucker"
BEEP
SLEEP 1
CLS
PRINT "You like this don't you"
BEEP
SLEEP 1
CLS
PRINT "If you don't like this too bad"
BEEP
SLEEP 1
CLS
PRINT "Beep, beep, beep, the never ending beeps"
BEEP
SLEEP 1
CLS
PRINT "You want this to end don't you"
BEEP
SLEEP 1
CLS
PRINT "I think you should reset your computer while you still can"
BEEP
SLEEP 1
CLS
PRINT "I like this command, don't you"
BEEP
SLEEP 1

END SUB

SUB BIRTHDAY
CLS
PRINT "Thanks to Steve Verhagen for this program!": SLEEP 2: CLS
xyz = INT(RND * 14) + 1
COLOR xyz, 0
INPUT "Please tell me your name so you can have fun"; KADFG$: CLS
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT " Happy ": PLAY "O3 L8 C L10 C ": CLS
PRINT " Birthday ": PLAY "L5 D C": CLS
PRINT " To ": PLAY "F": CLS
PRINT " You ": PLAY "L3 E": CLS
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT " Happy ": PLAY "O3 L8 C L10 C": CLS
PRINT " Birthday ": PLAY "L5 D C": CLS
PRINT " To ": PLAY "G": CLS
PRINT " You ": PLAY "L3 F": CLS
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT " Happy ": PLAY "O3; L8; C; L10; C; ": CLS
PRINT " Birthday ": PLAY "L4; O4; C; L5; O3; A; ": CLS
PRINT " Dear, ": PLAY "L6; F; ": CLS
PRINT " "; KADFG$; " ": PLAY "L4; D;": CLS
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT " Happy ": PLAY "O3; L8; A#; L8; A#": CLS
PRINT " Birthday ": PLAY "L8 A L4 F": CLS
PRINT " To ": PLAY "L7 G": CLS
PRINT " You ": PLAY "L4 F ": SLEEP 1: CLS

END SUB

SUB calc
begin: CLS
CONST pi = 3.141592654#
COLOR 28
PRINT "Welcome to the CALCULATOR 1000"
COLOR 1
'ษปศผอบ
PRINT
PRINT "ษอออออออออออออออออป"
PRINT "บ 1 = add         บ"
PRINT "บ 2 = subtract    บ"
PRINT "บ 3 = times       บ"
PRINT "บ 4 = divide      บ"
PRINT "บ 5 = exponets    บ"
PRINT "บ 6 = square root บ"
PRINT "บ 7 = square      บ"
PRINT "บ 8 = ใ pi (3.14) บ"
PRINT "บ exit = to exit  บ"
PRINT "ศอออออออออออออออออผ"
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT : INPUT "What functon do you want to do"; j$
PLAY da$
IF j$ = "1" THEN GOTO calc10
IF j$ = "2" THEN GOTO calc20
IF j$ = "3" THEN GOTO calc30
IF j$ = "4" THEN GOTO calc40
IF j$ = "5" THEN GOTO calc50
IF j$ = "6" THEN GOTO calc60
IF j$ = "7" THEN GOTO calc70
IF j$ = "8" THEN GOTO calc80
IF j$ = "exit" THEN GOTO calcend
PRINT "Wrong function pick 1 through 8 or exit"
SLEEP 2
GOTO begin
calc10: CLS : INPUT "Give me two numbers to add"; a: INPUT b
CLS : PRINT : PRINT a; "plus"; b; "="; a + b: SLEEP 4: GOTO begin
calc20: CLS : INPUT "Give me two numbers to subtract"; a: INPUT b
CLS : PRINT : PRINT a; "minus"; b; "="; a - b: SLEEP 4: GOTO begin
calc30: CLS : INPUT "Give me two numbers to multiply"; a: INPUT b
IF a = b THEN GOTO calc32
CLS : PRINT : PRINT a; "times"; b; "="; a * b: SLEEP 4: GOTO begin
calc32: PRINT : PRINT a; "squared ="; a * b: SLEEP 4: GOTO begin
calc40: CLS : INPUT "Give me two numbers to divide"; a:
calc42: INPUT "a number that's not zero"; b
IF b = 0 GOTO calc42
CLS : PRINT : PRINT a; "divided by"; b; "="; a / b: SLEEP 4: GOTO begin
calc50: CLS : INPUT "Give me two numbers first a number then the exponet"; a: INPUT b
CLS : PRINT : PRINT a; "with the exponet of"; b; "="; a ^ b: SLEEP 4: GOTO begin
calc60: CLS : INPUT "Give me a number to get the square root of (not a negative)"; a
IF a < 0 GOTO calc60
CLS : PRINT : PRINT "The square root of"; a; "is"; SQR(a): SLEEP 4: GOTO begin
calc70: CLS : INPUT "Give me a number to square"; a
CLS : PRINT : PRINT a; "squared is"; a * a: SLEEP 4: GOTO begin
calc80: CLS : INPUT "Give me a number to multiply by pi"; a
CLS : PRINT : PRINT "pi *"; a; "="; pi * a: SLEEP 4: GOTO begin
calcend: CLS

END SUB

SUB CHEESY
chesbegin: CLS
PLAY MUSIC1$
PRINT "You smell like Gunnar's batootskee"
PRINT "you stinkle tinkle"
INPUT "do you like the program so far"; a$
IF a$ = "y" THEN GOTO ches20
IF a$ = "n" THEN GOTO ches10
PRINT "type y or n": SLEEP 1: GOTO chesbegin
ches10: FOR j% = 1 TO 11: BEEP: NEXT j%: GOTO chesbegin
ches20: CLS : INPUT "What is your name?"; N$
PRINT "hi "; N$; ""
ches30: INPUT "Do you like cheese?"; c$
IF c$ = "y" THEN GOTO ches40
IF c$ = "n" THEN GOTO ches50
PRINT "Type y or n idiot": CLS : GOTO ches30
ches40: PRINT "So do I": GOTO chesend
ches50: SLEEP 1: PRINT "Only losers don't like cheese, now you're going to pay"
FOR a% = 1 TO 10
ches35: Qp = VAL(RIGHT$(TIME$, 2)): RANDOMIZE (Qp): xyz = INT(RND * 31) + 1
IF xyz = 16 THEN GOTO ches35
COLOR xyz: CLS : BEEP: PRINT "ha ha": BEEP: SLEEP 1
NEXT a%
GOTO chesbegin
chesend: SLEEP 1: CLS
PRINT "You may now exit this program": SLEEP 1

END SUB

SUB CREDITS
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT "Greg Lavery made DosGold"
PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : SLEEP 3
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PLAY da$
PRINT "DosGold now without CAPS LOCK"
PRINT : PRINT : PRINT : PRINT : PRINT : SLEEP 3
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT "But I'd like to thank Dirk Olson for rock, and others"
PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : SLEEP 3
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT "and Chris Peters for the music: Jurassic Park, Hey, Mortal Kombat, &"
PRINT "Mission Impossible (they aren't his but he found them somewhere)"
PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : PRINT : SLEEP 6
PRINT : PRINT : PRINT : PRINT : PRINT : PRINT
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT "Thanks and enjoy, P.S. the reg password is -gregdos-": SLEEP 5
xyz = INT(RND * 14) + 1
COLOR xyz, 0
END SUB

SUB DUMSONG
PRINT "I'm a moron": PLAY "d": PLAY "a": PLAY "c": PLAY "g"
CLS : COLOR 30, 0: PRINT "Isn't this really spiffy": PLAY "d": PLAY "a": PLAY "c": PLAY "g": PLAY "e": PLAY "b": PLAY "d": PLAY "a"
CLS : COLOR 26, 0: PRINT "I like cheese, I like cheese": PLAY "e": PLAY "b": PLAY "d": PLAY "a": PLAY "f": PLAY "c": PLAY "e": PLAY "b"
CLS : COLOR 31, 0: PRINT "Would you tell me how to get how to get to Sesame Street!"

END SUB

SUB FAMTREE
    CLS
    INPUT FAM$
    OPEN FAM$ FOR OUTPUT AS #1
    DO
        INPUT "   NAME:       ", Name$
        INPUT "   AGE:        ", Age$
        WRITE #1, Name$, Age$
        INPUT "Add another entry"; R$
    LOOP WHILE UCASE$(R$) = "Y"
    CLOSE #1
    'Print the file to the screen.
    OPEN "LIST" FOR INPUT AS #1
    CLS
    PRINT "Entries in file:": PRINT
    DO WHILE NOT EOF(1)
        INPUT #1, Rec1$, Rec2$   'Read entries from file.
        PRINT Rec1$, Rec2$       'Print the entries on the screen.
    LOOP
    CLOSE #1
END SUB

SUB FORTUNE
CLS
SLEEP 1
PRINT "I think you smell like cheese!"
PRINT : INPUT "Please tell me your name"; N$: PLAY MUSIC1$
PRINT : INPUT "Please tell me your age"; a$
SLEEP 1
CLS
6790 PRINT "I am a fortune teller, "; N$; "!"
INPUT "Are you wondering how I knew your name?"; w$
IF w$ = "y" THEN GOTO 67140
IF w$ = "n" THEN GOTO 67150
PRINT "Type y or n "; N$; "!": SLEEP 1: CLS : GOTO 6790
67140 PRINT "Well good": SLEEP 1: CLS
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT "Isn't this jiffy": GOTO 67151
67150 PRINT "This might convince you, I know the time."
67151 PRINT "The time at the tone is:"
67152 SLEEP 1
67153 CLS
BEEP
BEEP
BEEP
BEEP
BEEP
BEEP
BEEP
BEEP
67154 PRINT TIME$
67159 INPUT "Did that convince you?"; c$
67162 IF c$ = "y" THEN GOTO 67180
67163 IF c$ = "n" THEN GOTO 67150
67164 PRINT "Type y or n "; N$; "!": GOTO 67159
67180 PRINT "I know your age"
67181 PRINT "Your age is "; a$; ""
67190 INPUT "Isn't that cool"; qwe$
67192 IF qwe$ = "y" THEN GOTO 67987
67193 IF qwe$ = "n" THEN GOTO 67272
67194 PRINT "Type y or n!": GOTO 67192
CLS
67272 PRINT "Shove it"
BEEP
BEEP
BEEP
BEEP
BEEP
BEEP
SLEEP 1: CLS : BEEP: BEEP: BEEP: BEEP: BEEP: BEEP
xyz = INT(RND * 14) + 1
COLOR xyz, 0
67987 PRINT "Sucker": BEEP: BEEP: BEEP: BEEP: BEEP: BEEP
xyz = INT(RND * 14) + 1: COLOR xyz, 0: SLEEP 1
CLS
67999 PRINT "Thank you": BEEP: BEEP: BEEP: BEEP: BEEP: BEEP:
xyz = INT(RND * 14) + 1: COLOR xyz, 0: SLEEP 1

END SUB

SUB HILO
CLS
COLOR 18, 0
PRINT "Hi-Lo"
COLOR 17, 0
PRINT "Hi-Lo"
COLOR 19, 0
PRINT "Hi-Lo"
COLOR 20, 0
PRINT "Hi-Lo"
COLOR 21, 0
PRINT "Hi-Lo"
COLOR 22, 0
PRINT "Hi-Lo"
COLOR 23, 0
PRINT "!!!!!!!!!!!!!!!!!!!!!!!!!!"
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT "Greg Lavery made this radical game"
xyz = INT(RND * 14) + 1
COLOR xyz, 0
INPUT "What is your name"; K$
PLAY MUSIC1$: CLS
PRINT "Hello "; K$; " how is it going?"
INPUT w$: CLS
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT "Well that is just "; w$; ""
PRINT : INPUT "Please give me a (one word) compliment!"; O$
CLS : xyz = INT(RND * 14) + 1: COLOR xyz, 0
INPUT "Also please insult me (with one word)."; P$
CLS : PRINT : PRINT : PRINT
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT "This is the game of Hi-Lo!": PRINT
PRINT "You will have 10 tries to guess the secret number"
PRINT "which is between 0 and 100.  If you guess the amount,"
PRINT "you win (7-amount of guesses)*25!"
PRINT "if you do not guess the amount, you lose money.": PRINT
160 R = O
162 Q = VAL(RIGHT$(TIME$, 2))
165 RANDOMIZE (Q)
170 b = O: PRINT
180 Y = INT(100 * RND(1))
200 INPUT "Your guess"; a
220 b = b + 1
230 IF a = Y THEN 300
240 IF a > Y THEN 270
250 PRINT "Higher": GOTO 280
270 PRINT "Lower"
280 PRINT : IF b < 10 THEN 200
290 PRINT "You blew it . . . too bad . . . the number was"; Y
295 R = O - 250
297 PRINT "Your total winnings are now "; R; " dollars!"
298 GOTO 350
300 PRINT "GOT IT!!!!!!!!!!!!!!!!!!!!"
310 R = R + (7 - b) * 25
320 PRINT "Your total winnings are now"; R; "dollars!"
350 PRINT : INPUT "Play again (y or n)"; a$
IF a$ = "y" THEN 170
361 IF a$ = "n" THEN 370
363 PRINT "type y or n you panzy whacker": GOTO 350
370 PRINT "So, "; K$; " did you have a joyous time?"
371 INPUT S$: IF S$ = "y" THEN 380
372 IF S$ = "n" THEN 400
373 PRINT "Type y or n you stupid moron": GOTO 370
380 PRINT "That's nice, you really are "; O$; "!": GOTO 990
400 SLEEP 1: CLS
BEEP
BEEP
CLS
401 PRINT "You are a big "; P$; "!!!!"
SLEEP 1
990 PRINT : PRINT "So long "; K$; ".  Hope you had a "; w$; " time!"
SLEEP 1

END SUB

SUB insult
CLS
PRINT "Insult me"
INPUT insul$
CLS
PRINT ""; insul$; ", "
SHELL "PAUSE"

END SUB

SUB INSULTUN
CLS
PRINT "Insult me"
INPUT insul$
CLS
PRINT ""; insul$; ", "
SHELL "PAUSE"

END SUB

SUB MUSIC2
music:
260 CLS
SCREEN 0
LOCATE 2, 24
COLOR 3, 0
PRINT "     ษอออออออออออออป"
LOCATE 3, 24
PRINT "ษออออผMusic Libraryศออออป"
LOCATE 4, 24
'ษปศผอบ
PRINT "ศอออออออออออออออออออออออผ"
LOCATE 6, 25
COLOR 4
PRINT "1 = Mortal Kombat Song"
LOCATE 8, 25
COLOR 15
PRINT "2 = Mission: Impossible Song"
LOCATE 10, 25
COLOR 2
PRINT "3 = Sports Song:  Hey"
LOCATE 12, 25
COLOR 6
PRINT "4 = Jurassic Park Song"
LOCATE 14, 25
COLOR 7
PRINT "5 = All Apologies: Nirvana  (Part of it)"
LOCATE 16, 25
COLOR 12
PRINT "6 = Birthday"
LOCATE 18, 25
COLOR 1
PRINT "7 = End"
LOCATE 20, 25
COLOR 13
INPUT "Which one: ", msu$
IF msu$ = "1" THEN GOTO mortal
IF msu$ = "2" THEN GOTO mission
IF msu$ = "3" THEN GOTO sports
IF msu$ = "4" THEN GOTO jurassic
IF msu$ = "5" THEN GOTO ALLAPO
IF msu$ = "6" THEN GOTO birth
IF msu$ = "7" THEN GOTO 264
GOTO 260

'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_

mortal:
SCREEN 0
CLS
PRINT "If you don't want to listen to the whole song then hold ctrl and press"
PRINT "pause/Break"
SLEEP 3
CLS
COLOR 4
LOCATE 10, 31
PRINT "   MORTAL KOMBAT!!"
LOCATE 11, 31
PRINT " MOVIE SOUNDTRACK!!!"
PLAY "t230l7o3ap24ap24o4cp24o3ap24o4dp24o3ao4p24ep24dp24cp24cp24ep24cp24gp24cp24ep24cp24o3gp24gp24bp24gp24o4cp24o3gp24o4dp24cp24o3fp24fp24ap24fp24o4cp24o3fp24o4cp24o3bp24"
PLAY "t230l7o3ap24ap24o4cp24o3ap24o4dp24o3ao4p24ep24dp24cp24cp24ep24cp24gp24cp24ep24cp24o3gp24gp24bp24gp24o4cp24o3gp24o4dp24cp24o3fp24fp24ap24fp24o4cp24o3fp24o4cp24o3bp24"
CLS
LOCATE 10, 31
PRINT "     EXCELLENT!!"
PLAY "t230l7o3ao4ep24o3ao4cp24o3ab-p24ao4cp24o3b-o4co4o3gp24"
CLS
LOCATE 10, 31
PRINT "       KANO"
PLAY "t230l7o3ao4ep24o3ao4cp24o3ab-p24ao4cp24o3b-o4co4o3gp24"
CLS
LOCATE 10, 31
PRINT "     LIU KANG"
PLAY "t230l7o3ao4ep24o3ao4cp24o3ab-p24ao4cp24o3b-o4co4o3gp24"
CLS
LOCATE 10, 31
PRINT "      RAIDEN"
PLAY "t230l7o3ao4ep24o3ao4cp24o3agp24gap4"
CLS
LOCATE 10, 31
PRINT "   JOHNNY KAGE"
PLAY "t230l7o3ao4ep24o3ao4cp24o3ab-p24ao4cp24o3b-o4co4o3gp24"
CLS
LOCATE 10, 31
PRINT "     SCORPION"
PLAY "t230l7o3ao4ep24o3ao4cp24o3ab-p24ao4cp24o3b-o4co4o3gp24"
CLS
LOCATE 10, 31
PRINT "     SUB-ZERO"
PLAY "t230l7o3ao4ep24o3ao4cp24o3ab-p24ao4cp24o3b-o4co4o3gp24"
CLS
LOCATE 10, 31
PRINT "      SONYA"
PLAY "t230l7o0cp5.cp5.cp5.cp8"
CLS
LOCATE 10, 31
PRINT "  MORTAL KOMBAT!!"
PLAY "t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24aaaaaaaap24ggggp24o4ccccp20"
PLAY "t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24aaaaaaaap24ggggp24o3eeeep20"
PLAY "t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24aaaaaaaap24ggggp24o4ccccp20"
PLAY "t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24l7aaaap8"
PLAY "t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24aaaaaaaap24ggggp24o4ccccp20"
PLAY "t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24aaaaaaaap24ggggp24o3eeeep20"
PLAY "t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24aaaaaaaap24ggggp24o4ccccp20"
PLAY "t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24l7aaaap8"
CLS
LOCATE 10, 31
PRINT "THIS WAS MADE BY OPIUMDUDE"
PLAY "t230l7o3ao4ep24o3ao4cp24o3ab-p24ao4cp24o3b-o4co4o3gp24"
CLS
LOCATE 10, 31
PRINT "      VERY KOOL"
PLAY "t230l7o3ao4ep24o3ao4cp24o3ab-p24ao4cp24o3b-o4co4o3gp24"
CLS
LOCATE 10, 31
PRINT "       FIGHT! "
PLAY "t230l7o3ao4ep24o3ao4cp24o3ab-p24ao4cp24o3b-o4co4o3gp24"
CLS
LOCATE 10, 31
PRINT "   MORTAL KOMBAT!!!"
PLAY "t230l7o3ao4ep24o3ao4cp24o3agp24gap4"
PLAY "t230l7o3ao4ep24o3ao4cp24o3ab-p24ao4cp24o3b-o4co4o3gp24"
PLAY "t230l7o3ao4ep24o3ao4cp24o3ab-p24ao4cp24o3b-o4co4o3gp24"
PLAY "t230l7o3ao4ep24o3ao4cp24o3ab-p24ao4cp24o3b-o4co4o3gp24"
PLAY "t230l7o0cp5.cp5.cp5.cp8"
CLS
LOCATE 10, 31
PRINT "       EXCELLENT"
PLAY "t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24aaaaaaaap24ggggp24o4ccccp20"
CLS
LOCATE 10, 31
PRINT "        KANO"
PLAY "t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24aaaaaaaap24ggggp24o3eeeep20"
CLS
LOCATE 10, 31
PRINT "      LIU KANG"
PLAY "t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24aaaaaaaap24ggggp24o4ccccp20"
CLS
LOCATE 10, 31
PRINT "       RAIDEN"
PLAY "t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24aaaaaaaap24ggggp24o3eeeep20"
CLS
LOCATE 10, 31
PRINT "    JOHNNY KAGE"
PLAY "t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24aaaaaaaap24ggggp24o4ccccp20"
CLS
LOCATE 10, 31
PRINT "      SCORPION"
PLAY "t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24aaaaaaaap24ggggp24o3eeeep20"
CLS
LOCATE 10, 31
PRINT "      SUB-ZERO"
PLAY "t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24aaaaaaaap24ggggp24o4ccccp20"
CLS
LOCATE 10, 31
PRINT "       SONYA"
PLAY "t230l25o3aaaaaaaap24aaaaaaaap24aaaaaaaap24l7aaaap8"
CLS
LOCATE 10, 31
PRINT "   MORTAL KOMBAT!!"
PLAY "t230l6o3ap24ap24o4cp24o3ap24o4dp24o3ao4p24ep24dp24cp24cp24ep24cp24gp24cp24ep24cp24o3gp24gp24bp24gp24o4cp24o3gp24o4dp24cp24o3fp24fp24ap24fp24o4cp24o3fp24o4cp24o3bp24"
PLAY "t230l6o3ap24ap24o4cp24o3ap24o4dp24o3ao4p24ep24dp24cp24cp24ep24cp24gp24cp24ep24cp24o3gp24gp24bp24gp24o4cp24o3gp24o4dp24cp24o3fp24fp24ap24fp24o4cp24o3fp24o4cp24o3bp24"
GOTO 264

'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_

mission:
CLS
PRINT "If you don't want to listen to the whole song then hold ctrl and press"
PRINT "pause/Break"
SLEEP 3
CLS
COLOR 14, 4
LOCATE 9, 23
PRINT "                                   "
LOCATE 10, 23
PRINT "     Mission: Impossible Theme     "
LOCATE 11, 23
PRINT "                                   "
a$ = "<g6p20g10g6b-6>c6<g6p20g10g6b-6>c6<g6p20g10g6p10"
b$ = ">>b-10g10d4p10b-10g10d-4p10b-10g10c4p10<b-10>c10p8<"
c$ = "g6p20g3b-10>c6<g6p20g3f6f#6g6b-10g10>f#4p10<b-10g10>f4p10<b-10g10>e4p20e-10d10p20<"
d$ = "<g10g6b-10b10>e-10c10<g4g#6p20>e-10c10<g-4g6p20>"
e$ = "e-10c10<f4e-10p20e-10f10p10>b-10g10>f#4p10<b-10g10>f4p10<b-10g10>e4p10"
f$ = "e-10d10p20<a3b-6>c6<a6p10>f10g1"
PLAY a$
PLAY b$
PLAY c$
PLAY d$
PLAY e$
PLAY f$
COLOR 1, 0
GOTO 264

'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_

sports:
CLS
PRINT "If you don't want to listen to the whole song then hold ctrl and press"
PRINT "pause/Break"
SLEEP 3
CLS
SCREEN 0
PLAY "T 130 ml <A B>MN l2 c. l16 c <L2 B. l16 b ml l1 a p4 "
PLAY "l4 mL a b>MN L2 C. L16 C <L2 B. L16 B L1 a p4 "
PLAY "l4 ml A b>mn l2 c. l16 C <L2 B. L16 B L2> D. L16 D L1 C"
PLAY "<  l24 g p20 g p12 g p20 g p12 g p12 l8 g P16 > L4 D. L16 D L8 C "
COLOR 31
PRINT "                            !!!!!!!!HEY!!!!!!!!"
PLAY " P 2< L24 G P20 G P12 G P12L12 A  P6 L6 AP16 > L4 D. L16 D L8 C"
PRINT "      !!!!!!!!!!!!!!!!!!!!!!!!!   H   E   Y   !!!!!!!!!!!!!!!!!!!!!!!"
PLAY "P 2< L24 G P20 G P12 G P12L12 A  P6 L6 AP16 > L4 D. L16 D L8 C"
PRINT "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   H      E      Y    !!!!!!!!!!!!!!!!!!!!!!!!!!!!"
COLOR 4
GOTO 264
jurassic:
'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
CLS
PRINT "If you don't want to listen to the whole song then hold ctrl and press"
PRINT "pause/Break"
SLEEP 3
CLS
SCREEN 0
LOCATE 12, 27
SLEEP 2
PRINT "Welcome to Jurassic Park"
SLEEP 2

PLAY "T60O1"
PLAY "L4>D.L16DC#L4D.L16DC#"
PLAY "L8D.L16EL8E.L16GL4G.L16F#D"
PLAY "L8E.L16C#<L8AL16>F#DL4E.L16AD"
PLAY "L8G.L16F#L8F#.L16EL4E.L16DC#"
PLAY "L4D.L16DC#L4D.L16DC#"
PLAY "L8D.L16EL8E.L16GL4G.L16F#D"
PLAY "L8E.L16C#L8<AL16>F#DL4E.L16AD"
PLAY "L8G.L16F#L8F#.L16EL2E"
PLAY "L16P8D#C#L8D#L16<B>EL8EF#F#C#P8"
PLAY "L16P8D#C#L8D#L16<B>F#L8GAL4E"
PLAY "L16P8F#EL8F#L16DAL8AF#>C#<C#"
PLAY "L16P8>DC#L8D<DGF#L4E"
PLAY "L4D.L16DC#L4D.L16DC#"
PLAY "L8D.L16EL8E.L16GL4G.L16F#D"
PLAY "L8E.L16C#L8<AL16>F#DL4E.L16AD"
PLAY "L8G.L16F#L8F#.L16EL4E.L16GE"
PLAY "L8>D.L16C#L8C#.L16<BL4BL8AL16>DC#"
PLAY "L8D<AGL16>DC#L8D<AGL16>DC#"
PLAY "L16C#L8D.L8<ADL4>DL8C#L16>DC#"
PLAY "L8D<AGL16>DC#L8D<AGL16DC#"
PLAY "L16C#L8D.<ADL4>C.L16DC#"
PLAY "L8D<AGL16>DC#L8D<AGL16>DC#"
PLAY "L16C#L8D.<ADL4>DC#"
PLAY "L1D"
PLAY "L16<<<BAL8BL16G>CL8CDD<G"
PLAY "L16P8BAL8BL16G>DL8E-FL4C"
PLAY "L16>DCL16D<L16B->FL8FDA<A"
PLAY "L16P8>D-C#L8D-L16<B->E-L8E-FL4C"
PLAY "L4D.L16DC#L4D.L16DC#"
PLAY "L8D.L16EL8E.L16GL4G.L16F#D"
PLAY "L8E.L16C#L8<A>L16F#DL4E.L16AD"
PLAY "L8G.L16F#L8F#.L16EL4E.>L16DC#"
PLAY "L8D<AG>L16DC#L8D<AGL16>DC#"
PLAY "L16C#L8D.<ADL4>D.C#."
PLAY "L1D"
PLAY "L1<D"
PLAY "T90L4<<B-.L16B-B-L2B-"
PLAY "L4B-.L16B-B-L2B-"
PLAY "L8P8MSB-MNL4>FL8MSCL4MNGL16GA"
PLAY "L8B-.L16B-L8AFL2G"
PLAY "L8P8B-AFF#.L16DGAL8B-"
PLAY "L4<B-.L16>CDL4DC#"
PLAY "L8P8MS<B-MNL4>FL8MSCL4MNGL16GA"
PLAY "L8B-.L16B-L8AFL2G"
PLAY "L8P8B-AFF#.L16DCDL8E-"
PLAY "L8D.L16GL8G.L16AL2A"
PLAY "L8P8>B->C<L16B-AL8B-L4G"
PLAY "L8P8G>C<B-L4B-A"
PLAY "L8P8<A>DGGF#L4E"
PLAY "L8P8A<A>GL4F#E"
PLAY "L8P8D<D>CL4<BA"
PLAY "L8>P8DE-L16DCL8DCL4<B-"
PLAY "L8P8B->E-D-L4D-C"
PLAY "L8<P8CFB-B-AG"
PLAY "L8P8>C<CB-L3AG"
PLAY "L16AGMSF#EL8DP8P16MNDMSC#<BL16>GFE"
PLAY "L8MNDEF#GL4AB"
PLAY "L8P8>MSCMNL4GL8MSDMNL4AL16AB"
PLAY "L8>C.L16CL8<BGL2A"
PLAY "L8P8C<BGG#.L16EL16ABL8>C"
PLAY "L4<C.L16FAL2B"
PLAY "L8MSP8CL4MNGL8MSDL4MNAL16AB"
PLAY "L8>C.L16CL8<BGL2A"
PLAY "L8>P8FECC#.L16<AL16>DEL8D"
PLAY "L16<P16G-MSFE-L8D-P8P16L16MNDMSC<B-P16>L16GFE"
PLAY "L16P16MSCFCL8MNCFP16L16>MSCFCL8FP8"
PLAY "L16MNP16C<BAB>CDEL8FGAB"
PLAY "L2>C<<<MSL8CL4MNGL8MSDMNL4AL16AB"
PLAY "L4>C>>L2C.P4"
GOTO 264

'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_

ALLAPO:
CLS : PRINT "If you don't want to listen to the whole song then hold ctrl and press": PRINT "pause/Break": SLEEP 3
COLOR 2: CLS : LOCATE 10, 25: PRINT "ALL APOLOGEES YEAH"
ALLAPO$ = "o2 f8 f8 f+8 f8 g+8 g+8 f+8 f4 f8 f+8 f8 o3 c#8 c#8 o2 b8 a#8 o2 f8 f8 f+8 f8 g+8 g+8 f+8 f4 f8 f+8 f8 o3 f8 f8 d#8 c#8"
PLAY ALLAPO$: GOTO 264

'-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_

birth: CLS
PRINT "If you don't want to listen to the whole song then hold ctrl and press"
PRINT "pause/Break"
SLEEP 3
CLS
COLOR 1, 0: PRINT "Please tell me your name so you can have fun": INPUT KADFG$: CLS
COLOR 2, 0: PRINT " Happy ": PLAY "O3 L8 C L10 C ": CLS
PRINT " Birthday ": PLAY "L5 D C": CLS
PRINT " To ": PLAY "F": CLS
PRINT " You ": PLAY "L3 E": CLS
COLOR 3, 0: PRINT " Happy ": PLAY "O3 L8 C L10 C": CLS
PRINT " Birthday ": PLAY "L5 D C": CLS
PRINT " To ": PLAY "G": CLS
PRINT " You ": PLAY "L3 F": CLS
COLOR 4, 0: PRINT " Happy ": PLAY "O3; L8; C; L10; C; ": CLS
PRINT " Birthday ": PLAY "L4; O4; C; L5; O3; A; ": CLS
PRINT " Dear, ": PLAY "L6; F; ": CLS
PRINT " "; KADFG$; " ": PLAY "L4; D;": CLS
COLOR 5, 0: PRINT " Happy ": PLAY "O3; L8; A#; L8; A#": CLS
PRINT " Birthday ": PLAY "L8 A L4 F": CLS
PRINT " To ": PLAY "L7 G": CLS
PRINT " You ": PLAY "L4 F ": SLEEP 1: CLS
GOTO 264

264 END SUB

SUB ROCK
begi: CLS
PRINT "Rock, scissors, paper"
PRINT : PRINT "Dirk Olson made this weird game"
PRINT : PRINT : PRINT
PRINT "Please eat the computer or insult me with one word you dorkwad"
INPUT Z$: IF Z$ = "" THEN GOTO begi
307767 CLS : INPUT "How many games?"; Q
IF Q = -2 THEN GOTO score
IF Q < 1 THEN PRINT "Too low "; Z; " !": SLEEP 2: CLS : GOTO 307767
IF Q < 501 THEN GOTO 607767
PRINT "You will die if you play that many games.": GOTO 307767
607767 FOR G = 1 TO Q
PRINT : PRINT "game number"; G
X = INT(RND(1) * 3 + 1)
907767 PRINT "3=rock...2=scissors...1=paper"
INPUT "1...2...3...What's your choice"; K
IF (K - 1) * (K - 2) * (K - 3) <> O THEN PRINT "that is not a choice.": GOTO 907767
PRINT "this is my dumb choice..."
ON X GOTO 1407767, 1507767, 1607767
1407767 PRINT "Paper": GOTO 1707767
1507767 PRINT "Scissors": GOTO 1707767
1607767 PRINT "Rock"
1707767 IF X = K THEN GOTO 2507767
IF X > K THEN GOTO 2307767
IF X = 1 THEN GOTO 2107767
2007767 PRINT "You win!!!": H = H + 1: GOTO 2607767
2107767 IF K <> 3 THEN GOTO 2007767
2207767 PRINT "You lost - HA-HA-HA-HA!!!": c = c + 1: GOTO 2607767
2307767 IF K <> 1 OR X <> 3 THEN GOTO 2207767
GOTO 2007767
2507767 PRINT "It's a tie!!!"
2607767 NEXT G
score: PRINT : PRINT "Here is the final score:": PLAY MUSIC1$
PRINT "You won"; H; "game(s)."
PRINT "You lost"; c; "game(s)."
3007767 PRINT "And"; Q - (c + H); "game(s) ended in a tie."
3017767 PRINT : PRINT "Would you like to play again?"
INPUT a$
IF a$ = "n" THEN GOTO 3117767
IF a$ = "y" THEN GOTO 307767
PRINT : PRINT "Press y or n or die you scumbag": GOTO 3017767
PRINT : PRINT "thanks for playing this cheesy game!!"
3117767 PRINT "You are a "; Z$; " for quitting because only "; Z$; "s quit": SLEEP 2: CLS :
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT "Stupid "; Z$; " ": SHELL "PAUSE"
END SUB

SUB SECRET
CLS
COLOR 16, 1: PRINT "Welcome to the secret commands list!!"
PRINT : PRINT : PRINT "You have just entered the secret commands what are you gonna do know"
xyz = INT(RND * 14) + 1
COLOR xyz, 0
PRINT : PRINT : PLAY MUSIC1$
PRINT "ษอออออออออออออออออออออออออออออออออป"
PRINT "บ  suck- this tells you who sucks บ"
PRINT "บ  bash- real funny try it        บ"
PRINT "ศอออออออออออออออออออออออออออออออออผ"
SLEEP 6

END SUB

SUB STEVE
CLS
PRINT "What is your name"
INPUT Nam$
51221 CLS : PRINT "Steve Sucks royally": PLAY MUSIC1$
PRINT "Don't you agree"
INPUT a$
IF a$ = "y" THEN GOTO 901221
IF a$ = "n" THEN GOTO 101221
PRINT "Type y or n you stupid moron retard": SLEEP 1: CLS : GOTO 51221
101221 CLS : PRINT ""; Nam$; " bites": SLEEP 1: CLS : PRINT "Well, I think "; Nam$; ",": SLEEP 1: GOTO 981221
901221 CLS : PRINT "I agree, Steve really"
SLEEP 1: CLS
COLOR 3, 0: PRINT "really": SLEEP 1: CLS
COLOR 4, 0: PRINT "really": SLEEP 1: CLS
COLOR 5, 0: PRINT "really": SLEEP 1: CLS
981221 a$ = "SSSSSSSSSSSSSUUUUUUUUUUCCCCCCCCCCCCKKKKKKKKKKKKKSSSSSSSSSSS!!!!!!"
COLOR 11, 0: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$: PRINT a$:
INPUT JERRYBOBERRY$: PRINT : PRINT "HI "; JERRYBOBERRY$; "!"
SLEEP 1

END SUB

SUB TIME
CLS
PRINT "The time at the tone is:": BEEP:  PRINT TIME$: SLEEP 2
END SUB

