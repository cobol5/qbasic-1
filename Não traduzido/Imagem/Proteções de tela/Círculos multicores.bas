'�����������������������������������������������������������Ŀ
'� COLORS  .BAS                             VGA Palette Demo �
'�����������������������������������������������������������͵
'� This program is by Mallard.  For other programs like this �
'� one, or for more information, please contact me.  I can   �
'� be reached at "mallard@gcomm.com" via Internet e-mail or  �
'� or via WWW - "http://www.lookup.com/homepages/80948/qb/   �
'� index.html". Please upload as much source code as you can �
'� to this home page!   Feel free to distribute this program,�
'� or use part or all of it in one of your own as long as I  �
'� am given credit.  Thanks!              �Mallard�          �
'�����������������������������������������������������������;
'

RANDOMIZE TIMER
SCREEN 13

recalc:
'CLS

red = 0
green = 0
blue = 0

redbig = INT(RND * 5) + 1
greenbig = INT(RND * 5) + 1
bluebig = INT(RND * 5) + 1
IF redbig = 2 THEN redbig = .5
IF greenbig = 2 THEN greenbig = .5
IF bluebig = 2 THEN bluebig = .5
IF redbig = 3 THEN redbig = .25
IF greenbig = 3 THEN greenbig = .25
IF bluebig = 3 THEN bluebig = .25
IF redbig = 4 THEN redbig = .75
IF greenbig = 4 THEN greenbig = .75
IF bluebig = 4 THEN bluebig = .75
IF redbig = 5 THEN redbig = 0: red = 1
IF greenbig = 5 THEN greenbig = 0: green = 1
IF bluebig = 5 THEN bluebig = 0: blue = 1

FOR i = 30 TO 92
 red = red + redbig
 blue = blue + bluebig
 green = green + greenbig
 PALETTE i, 65536 * INT(blue) + 256 * INT(green) + INT(red)
NEXT i

c = 32
dir = 1

circles:
DO
x = INT(RND * 320) + 1
y = INT(RND * 200) + 1
r = INT(RND * 150) + 1
FOR i = r TO 1 STEP -1
CIRCLE (x, y), i, c
PAINT (x, y), c
SELECT CASE dir
        CASE 1
        c = c + 1
        IF c = 92 THEN dir = 2
        CASE 2
        c = c - 1
        IF c = 32 THEN dir = 1
END SELECT
NEXT i
c = 32
dir = 1
IF INKEY$ <> "" THEN END
GOTO recalc
LOOP

'�����������������������������������������������������������Ŀ
'� COLORS  .BAS                             VGA Palette Demo �
'�����������������������������������������������������������;


