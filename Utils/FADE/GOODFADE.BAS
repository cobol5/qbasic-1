DECLARE SUB Fade ()
DECLARE SUB Switch ()
DIM SHARED col(255, 3), col2(255, 3)
SCREEN 13
FOR i = 0 TO 255        ' Making a b&w palette to fade to
FOR j = 1 TO 3          '
col(i, j) = i / 4       '
NEXT j                  '
NEXT i                  '
FOR i = 0 TO 255        ' Making the screen
LINE (i, 0)-(i, 199), i '
NEXT i                  '
Fade                    ' Guess...
COLOR 255
Switch
Fade
FOR i = 0 TO 255
FOR j = 1 TO 3
col(i, j) = 0
NEXT j
NEXT i
Fade
Switch
Fade
Switch
Fade

'This is the better fade stuff. It fades to col(255,3)
SUB Fade
FOR i = 0 TO 255
OUT &H3C7, i
col2(i, 1) = INP(&H3C9)
col2(i, 2) = INP(&H3C9)
col2(i, 3) = INP(&H3C9)
NEXT i
FOR j = 0 TO 63
FOR i = 0 TO 255
OUT &H3C8, i
OUT &H3C9, INT(((63 - j) * col2(i, 1)) / 63 + (j * col(i, 1)) / 63)
OUT &H3C9, INT(((63 - j) * col2(i, 2)) / 63 + (j * col(i, 2)) / 63)
OUT &H3C9, INT(((63 - j) * col2(i, 3)) / 63 + (j * col(i, 3)) / 63)
NEXT i
NEXT j
END SUB

SUB Switch
DIM col3(255, 3)
FOR i = 0 TO 255
FOR j = 1 TO 3
col3(i, j) = col2(i, j)
NEXT j
NEXT i
FOR i = 0 TO 255
FOR j = 1 TO 3
col2(i, j) = col(i, j)
NEXT j
NEXT i
FOR i = 0 TO 255
FOR j = 1 TO 3
col(i, j) = col3(i, j)
NEXT j
NEXT i
END SUB

