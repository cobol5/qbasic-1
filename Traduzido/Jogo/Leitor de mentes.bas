 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
  CLS
  SCREEN 13
  FOR losses% = 1 TO 11
    LOCATE 1, 1: COLOR 254
    PRINT "EXPERT!"
    RANDOMIZE TIMER
    tsize% = INT(RND * 5) + 1
    colit% = INT(RND * 255) + 1
    plush% = 0
    plusv% = INT(RND * 160) + 20
    csize% = tsize% - 1
    IF losses% = 11 THEN
      tsize% = 5
      colit% = 15
      plush% = 5
      plusv% = 80
      csize% = 4
    END IF
    FOR copyV% = 0 TO 7
      FOR copyH% = 0 TO 80
        col% = POINT(copyH%, copyV%)
        IF col% = 254 THEN
          CIRCLE (copyH% * tsize% + plush%, copyV% * tsize% + plusv%), csize%, colit%
          PAINT (copyH% * tsize% + plush%, copyV% * tsize% + plusv%), colit%, colit%
        END IF
      NEXT copyH%
    NEXT copyV%
    FOR plays% = 40 TO 1 STEP -1
      PLAY "MBMFMLT255L64N" + STR$(plays%)
    NEXT plays%
    IF losses% <> 11 THEN CLS
  NEXT losses%
  FOR sounds% = 1 TO 5
    FOR plays% = 40 TO 1 STEP -2
      PLAY "MBMFMLT255L64N" + STR$(plays%)
    NEXT plays%
    LOCATE 16, 8 + (sounds% * 4): COLOR 4: PRINT "VC!"
  NEXT sounds%
  FOR plays% = 40 TO 1 STEP -1
    PLAY "MBMFMLT255L64N" + STR$(plays%)
  NEXT plays%
  atyy! = TIMER
  DO
  LOOP UNTIL TIMER > atyy! + 1
  CLS
  atyy! = TIMER
  DO
  LOOP UNTIL TIMER > atyy! + 1
  CLS : SCREEN 12: CLS
  COLOR 14
  LOCATE 2, 25: PRINT "by ?Unknown? creator v1.0"
  COLOR 7
  LOCATE 5, 10: PRINT " Leio  as mentes...................(mentira)"
  LOCATE 8, 12: PRINT "  N∆o acredita? Ainda bem pois sou vigarista igual a mae Dinah!"
  LOCATE 9, 12: PRINT "  Pressione SPACE amiguinho, ou ESC para "
  LOCATE 10, 12: PRINT "                sair."
DO
Cranio$ = INKEY$
IF Cranio$ = " " THEN GOTO Cranio
IF Cranio$ = CHR$(27) THEN GOTO COVARDE
IF Cranio$ = "" THEN  ELSE
LOOP
COVARDE:
  CLS
  SCREEN 13
  FOR losses% = 1 TO 11
    LOCATE 1, 1: COLOR 254
    PRINT "COVARDE?"
    RANDOMIZE TIMER
    tsize% = INT(RND * 5) + 1
    colit% = INT(RND * 255) + 1
    plush% = 0
    plusv% = INT(RND * 160) + 20
    csize% = tsize% - 1
    IF losses% = 11 THEN
      tsize% = 5
      colit% = 15
      plush% = 5
      plusv% = 80
      csize% = 4
    END IF
    FOR copyV% = 0 TO 7
      FOR copyH% = 0 TO 80
        col% = POINT(copyH%, copyV%)
        IF col% = 254 THEN
          CIRCLE (copyH% * tsize% + plush%, copyV% * tsize% + plusv%), csize%, colit%
          PAINT (copyH% * tsize% + plush%, copyV% * tsize% + plusv%), colit%, colit%
        END IF
      NEXT copyH%
    NEXT copyV%
    FOR plays% = 40 TO 1 STEP -1
      PLAY "MBMFMLT255L64N" + STR$(plays%)
    NEXT plays%
    IF losses% <> 11 THEN CLS
  NEXT losses%
  FOR sounds% = 1 TO 5
    FOR plays% = 40 TO 1 STEP -2
      PLAY "MBMFMLT255L64N" + STR$(plays%)
    NEXT plays%
    LOCATE 16, 8 + (sounds% * 4): COLOR 4: PRINT "EU!"
  NEXT sounds%
  FOR plays% = 40 TO 1 STEP -1
    PLAY "MBMFMLT255L64N" + STR$(plays%)
  NEXT plays%
  atyy! = TIMER
  DO
  LOOP UNTIL TIMER > atyy! + 1
  CLS
  atyy! = TIMER
  DO
  LOOP UNTIL TIMER > atyy! + 1
END
Cranio:
PRINT
PRINT
INPUT "  Digite 1 numero pequeno, e porque eu sou burro e idiota!  (maior que 6)", ok%
INPUT "  Digite outro numero pequeno para facilitar, como sabe que eu sou besta e BURRO! (maior que 6)", okx%
CLS
LOCATE 2, 20: PRINT " Aguarde , amiguinho... estou calculando..."
FOR X = 1 TO 9999
X% = 4
y% = X% + h% + c%
h% = 10
u% = h% - X% + c% + z%
c% = 3056
z% = 456
NEXT X
LOCATE 5, 23: PRINT " Pronto,  se vocà for expert, faáa as contas de cabeáa"
LOCATE 6, 20: PRINT "se n∆o pegue um l†pis e um papel ok? Como sou burro tambem vou fazer no papel! Pressione qualquer tecla"

DO
LOOP WHILE INKEY$ = ""
CLS
LOCATE 2, 25: PRINT " PENSE NUM NUMERO."
DO
LOOP WHILE INKEY$ = ""
CLS
LOCATE 4, 20: PRINT " SOME COM "; ok%
DO
LOOP WHILE INKEY$ = ""
CLS
LOCATE 8, 20: PRINT " SOME COM MAIS"; okx%
DO
LOOP WHILE INKEY$ = ""
CLS
LOCATE 12, 20: PRINT " SUBTRAIA 10"
DO
LOOP WHILE INKEY$ = ""
CLS
LOCATE 12, 20: PRINT " TIRE O NUMERO QUE VOCE PENSOU NO INICIO."
DO
LOOP WHILE INKEY$ = ""
CLS

LOCATE 2, 20: PRINT " AGORA, MANTENHA O RESULTADO NA SUA CABEÄA "
LOCATE 3, 20: PRINT "                   DEPOIS PRESSIONE ENTER E SAIBA"
LOCATE 4, 27: PRINT " QUE NUMERO QUE ESTA NA SUA CABEÄA"
LOCATE 10, 1: PRINT h%
LOCATE 11, 1: PRINT u%
LOCATE 12, 1: PRINT z%
LOCATE 13, 1: PRINT X%
xxxx% = 10
gfg% = ok% + okx% - xxxx%
DO
LOOP WHILE INKEY$ = ""
LOCATE 10, 25: PRINT " O RESULTADO FOI : "; gfg%
LOCATE 20, 1: PRINT "    SOU BURRO MESMO, SE NAO DEU "; gfg%; ","
LOCATE 21, 1: PRINT "    fui eu quem fui burro e nao  fiz o calculo direito."
LOCATE 23, 17: PRINT " Sabe o que vocà Ç ???"
DO
LOOP WHILE INKEY$ = ""
PRINT "EXPERT!"
INPUT "        AGORA DESCARREGUE A SUA RAIVA QUE GUARDA A TEMPOS E ESCREVA O QUE QUISER AQUI!       ", ESCREVA$
 

