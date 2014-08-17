'         F o r c a
'        -_-_-_-_-_-
'
'    Fernando Regis Ä 1998
'
'   regis@intervista.com.br
'
0
DEFINT A-Z
CLS
SCREEN 7
COLOR , 4
PLAY "l64"
LOCATE 1, 14
COLOR 7
PRINT "ÄÍFÍOÍRÍCÍAÍÄ";
COLOR 3
LOCATE 20, 7
PRINT "ÄÍFÍEÍRÍNÍAÍNÍDÍO  RÍEÍGÍIÍSÍÄ";
LOCATE 24, 19
PRINT "ESC sai";
T$ = "FERNANDO"
COLOR 7
nl = 1
FOR i = 9 TO 39 STEP 2
   LOCATE 20, i
   PRINT MID$(T$, nl, 1);
   nl = nl + 1
NEXT i
nl = 1
T$ = "REGIS"
FOR i = 26 TO 39 STEP 2
   LOCATE 20, i
   PRINT MID$(T$, nl, 1);
   nl = nl + 1
NEXT i
COLOR 15
nl = 1
T$ = "FORCA"
FOR i = 16 TO 39 STEP 2
   LOCATE 1, i
   PRINT MID$(T$, nl, 1);
   nl = nl + 1
NEXT i
COLOR 7
LOCATE 7, 12
PRINT "Digite uma palavra";
LOCATE 9, 11
PRINT "e ENTER quando pronto";
FOR i = 9 TO 319 STEP 8
   LINE (i, 127)-(i + 4, 127)
NEXT i
pa$ = ""
nl = 0
DO
   T$ = ""
   WHILE T$ = "": T$ = INKEY$: WEND
   SELECT CASE T$
      CASE CHR$(27)
         SCREEN 0
         SYSTEM
      CASE CHR$(8)
         IF nl <> 0 THEN
            nl = nl - 1
            pa$ = LEFT$(pa$, LEN(pa$) - 1)
            LOCATE 16, 2 + nl: PRINT " ";
            LINE (9 + (nl * 8), 127)-((9 + (nl * 8)) + 4, 127)
         END IF
      CASE CHR$(13)
         IF nl <> 0 THEN
            EXIT DO
         END IF
      CASE ELSE
         IF ASC(T$) > 31 AND ASC(T$) < 126 THEN
            pa$ = pa$ + T$
            LOCATE 16, 2 + nl: PRINT "þ";
            nl = nl + 1
         END IF
         IF nl >= 39 THEN EXIT DO
   END SELECT
LOOP
pa$ = UCASE$(pa$)
CLS
COLOR 3
LOCATE 24, 16
PRINT "ESC cancela";
COLOR 15
LINE (0, 0)-(1, 133), , B
LINE (0, 133)-(319, 134), , B
LINE (0, 0)-(40, 0)
LINE (40, 0)-(40, 10)
FOR i = 9 TO 319 STEP 8
   LINE (i, 127)-(i + 4, 127)
   nl = nl - 1
   IF nl = 0 THEN EXIT FOR
NEXT i
LOCATE 20, 14: PRINT CHR$(25) + " Tentativas " + CHR$(25); "";
TA$ = ""
ja$ = ""
nl = 0
DO
   T$ = ""
   WHILE T$ = "": T$ = INKEY$: WEND
   SELECT CASE T$
      CASE CHR$(27)
         SCREEN 0
         SYSTEM
      CASE ELSE
         T$ = LEFT$(UCASE$(T$), 1)
         IF INSTR(pa$, T$) = 0 THEN
            IF INSTR(TA$, T$) = 0 AND LEN(TA$) < 40 THEN
               IF ASC(T$) > 31 AND ASC(T$) < 126 THEN
                  LOCATE 21, 1 + LEN(TA$)
                  TA$ = TA$ + T$
                  GOSUB BONECO
                  PRINT T$
               END IF
            ELSE
               PLAY "a"
            END IF
         ELSE
            IF INSTR(ja$, T$) = 0 THEN
               FOR i = 1 TO LEN(pa$)
                  IF MID$(pa$, i, 1) = T$ THEN
                     LOCATE 16, 1 + i
                     PRINT T$;
                     nl = nl + 1
                     ja$ = ja$ + T$
                  END IF
                  IF nl = LEN(pa$) THEN
                     GOSUB ganhou
                  END IF
               NEXT i
            ELSE
               PLAY "a"
            END IF
         END IF
   END SELECT
LOOP
SYSTEM
BONECO:
SELECT CASE LEN(TA$)
   CASE 1
      CIRCLE (40, 18), 7                   ' Cabeca
   CASE 2
      LINE (40, 26)-(40, 33)               ' Pescoco
   CASE 3
      LINE (42, 33)-(55, 31)               ' Braco direito
   CASE 4
      LINE (38, 33)-(25, 31)               ' Braco esquerdo
   CASE 5
      LINE (40, 35)-(40, 50), , B          ' Corpo
   CASE 6
      LINE (38, 51)-(25, 69)               ' Perna esquerda
   CASE 7
      LINE (42, 51)-(55, 69)               ' Perna direita
   CASE 8
      FOR i = 1 TO 33 STEP 2
         CIRCLE (40, 43), i, 3, , , 1
      NEXT i
      COLOR 13
      LOCATE 10, 11
      PRINT " A palavra certa ‚ ";
      LOCATE 12, 1 + INT(20 - (LEN(pa$) / 2))
      PRINT pa$;
      LOCATE 24, 8
      PRINT " Pressione qualquer tecla ";
      WHILE INKEY$ = "": WEND
      CLEAR
      GOTO 0
END SELECT
RETURN

ganhou:
CLS
LOCATE 1, 13
COLOR 7
PRINT "ÄÍVÍEÍNÍCÍEÍUÍÄ";
nl = 1
COLOR 15
T$ = "VENCEU"
FOR i = 15 TO 39 STEP 2
   LOCATE 1, i
   PRINT MID$(T$, nl, 1);
   nl = nl + 1
NEXT i
   RANDOMIZE TIMER
   FOR i = 3 TO 22 STEP 1
      COLOR RND(1) * 15
      LOCATE i, 1 + INT(20 - (LEN(pa$) / 2))
      PRINT pa$;
   NEXT i
   COLOR 7
   LOCATE 24, 9
   PRINT "Pressione qualquer tecla";
   WHILE INKEY$ = "": WEND
   IF INKEY$ = CHR$(27) THEN
      CLS
      SYSTEM
   END IF
   CLEAR
   GOTO 0
RETURN

