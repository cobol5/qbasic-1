DECLARE SUB Fade ()
DIM SHARED col(255, 3)

SUB Fade
FOR i = 0 TO 255
DIM col2(255, 3)
OUT &H3C7, i
col2(i, 1) = INP(&H3C9)
col2(i, 2) = INP(&H3C9)
col2(i, 3) = INP(&H3C9)
NEXT i
FOR j = 0 TO 63
FOR i = 0 TO 255
OUT &H3C6, &HFF
OUT &H3C8, i
OUT &H3C9, INT(((63 - j) * col2(i, 1)) / 63 + (j * col(i, 1)) / 63)
OUT &H3C9, INT(((63 - j) * col2(i, 2)) / 63 + (j * col(i, 2)) / 63)
OUT &H3C9, INT(((63 - j) * col2(i, 3)) / 63 + (j * col(i, 3)) / 63)
NEXT i
NEXT j
END SUB

