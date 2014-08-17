DECLARE SUB CntrPrnt (a$, x%, f%)
DEFINT A-Z

DIM cp(0 TO 80) AS LONG
DIM cpr(80), cpg(80), cpb(80)
DIM cps(0 TO 80)           'wheel scores
DIM puz$(100)              'puzzles
DIM txtw$(7)               'scrolling text window array
DIM pzb(5) AS STRING * 13  'puzzle board  5 rows by 13 spaces
DIM pzbp(5, 13)            'puzzle board pointer, if letter is visible

soundz = 1   ' 1 = on  sound effects

pls = 0

start:
 IF replay = 0 THEN
    SCREEN 13
    pln$ = "Pontos"
 END IF

 txtw = 7                   'scrolling text window pointer
 tmp$ = "*": GOSUB text.display

 FOR i = 1 TO 5
   pzb(i) = STRING$(13, " ")
 NEXT i

 TILE$ = " ABCDEFGHIJKLMNOPQRSTUVWXYZ-,'~"
 used$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
 valid$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
 validv$ = "AEIOU"
 solve.try = 0

 IF replay = 0 THEN GOSUB setup
 LINE (0, 179)-(106, 199), 0, BF
 LINE (108, 11)-(319, 25), 1, BF

 tticks = 1
 GOSUB spin.wheel
 tticks = 0
 happys = 5

 GOSUB update.letter.board
 GOSUB update.players
 GOSUB pick.puzzle            'select puzzle for the round
 GOSUB update.letter.board
 LOCATE 1, 8: PRINT used$

 fspin = 1


top:
 GOSUB update.players
 IF fspin = 1 THEN
   tmp$ = " ": GOSUB text.display
   tmp$ = "Pressione Espa‡o": GOSUB text.display
   tmp$ = "para iniciar o game.": GOSUB text.display
  ELSE
   'SLEEP 2
   '10 second timer
   timerb! = TIMER
   elapse = 10
   k$ = ""
   DO UNTIL elapse < 1 OR k$ <> ""
     timere! = TIMER
     count = INT(timere! - timerb!)
     elapse = 9 - count
     IF elapse <> oelapse THEN
        IF soundz <> 0 THEN SOUND 500, .1
        oelapse = elapse
     END IF
     LOCATE 22, 6: PRINT elapse
     k$ = UCASE$(INKEY$)
     IF k$ <> "" THEN IF k$ = CHR$(27) THEN GOTO game.over.lose
   LOOP
 END IF

 IF happys = 0 THEN GOTO game.over.lose
 IF solved = 1 THEN GOTO game.over.win

 IF fspin = 1 THEN k$ = "" ELSE k$ = " "
 WHILE k$ = "": k$ = INKEY$: WEND
 k$ = UCASE$(k$)
 IF k$ = CHR$(27) THEN GOTO game.over.lose

 IF k$ = " " THEN
    IF solve.try = 0 THEN GOSUB spin.wheel
     
    'check here if zapped or whatever
    IF cps(56) = 0 AND solve.try = 0 THEN
       IF soundz = 1 THEN SOUND 1000, .7: SOUND 800, 1
       happys = happys - 1
       tmp$ = "*": GOSUB text.display
       tmp$ = "Z A P P E D !": GOSUB text.display
       fspin = 0
       GOTO top
    END IF
   
    'choose a letter
    IF fspin = 0 THEN
       tmp$ = " ": GOSUB text.display
      ELSE
       tmp$ = "*": GOSUB text.display
       fspin = 0
    END IF
    tmp$ = "Selecione uma letra,": GOSUB text.display
    tmp$ = "ou press. enter para resolver.": GOSUB text.display
    flag = 0
    WHILE flag = 0
restart.timer:
      '10 second timer
      timerb! = TIMER
      elapse = 10
      DO UNTIL elapse < 1
        timere! = TIMER
        count = INT(timere! - timerb!)
        elapse = 9 - count
        IF elapse <> oelapse THEN
           IF soundz <> 0 THEN SOUND 700, .1
           oelapse = elapse
        END IF
        LOCATE 22, 6: PRINT elapse
        k$ = UCASE$(INKEY$)
        IF k$ <> "" THEN
           IF k$ = CHR$(27) THEN GOTO game.over.lose
           IF k$ = CHR$(13) THEN GOSUB solve.it: k$ = "": GOTO restart.timer
           IF k$ = "=" THEN debugf = 1
           IF INSTR(1, valid$, k$) = 0 THEN k$ = "" ELSE EXIT DO
        END IF
      LOOP
      LOCATE 22, 6: PRINT "   "
     
      IF k$ = "" THEN
         tmp$ = "*": GOSUB text.display
         tmp$ = "O tempo acabou!": GOSUB text.display
         happys = happys - 1
         IF solve.try = 1 THEN happys = 0
         IF soundz = 1 THEN SOUND 400, 1: SOUND 300, 2
         GOTO top
       ELSE
         flag2 = 99
         flag = 1
         IF INSTR(1, used$, k$) > 0 THEN
           MID$(used$, INSTR(1, used$, k$), 1) = " "
           flag2 = 0
           FOR i = 1 TO 5
             FOR ii = 1 TO 13
               IF MID$(pzb(i), ii, 1) = k$ THEN
                  pzbp(i, ii) = 1
                  flag2 = flag2 + 1
               END IF
             NEXT ii
           NEXT i
         END IF
      END IF
    WEND
   
    'is letter already used
    IF flag2 = 99 THEN
       tmp$ = "*": GOSUB text.display
       tmp$ = k$ + " j  est  sendo usado.": GOSUB text.display
       'flag2 = 0
       happys = happys - 1
       IF solve.try = 1 THEN happys = 0
    END IF
    IF flag2 > 0 AND flag2 <> 99 THEN
       'good guess
       IF soundz = 1 THEN SOUND 400, 1: SOUND 600, 2
       tmp$ = "*": GOSUB text.display
       tmp$ = "Boa adivinha": GOSUB text.display
       IF INSTR(1, validv$, k$) > 0 THEN
          IF solve.try = 0 THEN
           tmp$ = " ": GOSUB text.display
           tmp$ = "Desculpe... ": GOSUB text.display
           tmp$ = "Sem pontos para vogais": GOSUB text.display
          END IF
         ELSE
          IF solve.try = 0 THEN pls = pls + (cps(56) * flag2)
       END IF
       GOSUB update.letter.board
      ELSE
       'bad guess
       IF soundz = 1 THEN SOUND 400, 1: SOUND 300, 2
       IF flag2 = 0 THEN
         happys = happys - 1
         IF solve.try = 1 THEN happys = 0
         tmp$ = "*": GOSUB text.display
         tmp$ = "NÆo existe nenhum " + k$: GOSUB text.display
       END IF
    END IF
 END IF
 LOCATE 1, 8: PRINT used$

GOTO top


'======================================================================
solve.it:
 tmp$ = "*": GOSUB text.display
 tmp$ = "O que ‚ o": GOSUB text.display
 tmp$ = "letras restantes?": GOSUB text.display
 tmp$ = " ": GOSUB text.display
 tmp$ = " ": GOSUB text.display
 solve.try = 1
 'WHILE solve.try = 1
 ' a$ = UCASE$(INPUT$(1))
 'WEND
RETURN

'======================================================================
game.over.win:
 tmp$ = "*": GOSUB text.display
 tmp$ = "  Bom Trabalho!": GOSUB text.display
 tmp$ = " ": GOSUB text.display
 tmp$ = "      Game Over": GOSUB text.display
 tmp$ = " ": GOSUB text.display
 tmp$ = "       Continuar (S/N)": GOSUB text.display
 tmp$ = " ": GOSUB text.display
 aa$ = UCASE$(INPUT$(1))
 IF aa$ = "S" THEN replay = 1: GOTO start
 IF aa$ = "N" THEN GOTO fini
 IF aa$ = CHR$(27) THEN GOTO fini

GOTO game.over.win

game.over.lose:
 tmp$ = "*": GOSUB text.display
 tmp$ = "      Game Over": GOSUB text.display
 tmp$ = " ": GOSUB text.display
 tmp$ = " Jogar de novo?(S/N)": GOSUB text.display
 tmp$ = " ": GOSUB text.display
 tmp$ = " ": GOSUB text.display
 aa$ = UCASE$(INPUT$(1))
 IF aa$ = "S" THEN replay = 1: GOTO start
 IF aa$ = "N" THEN GOTO fini
 IF aa$ = CHR$(27) THEN GOTO fini
GOTO game.over.lose

'======================================================================
text.display:
 'scrolling text window
 IF tmp$ = "*" THEN
    FOR i = 1 TO 7: txtw$(i) = "                       ": NEXT i
    tmp$ = " "
 END IF

 txtw = txtw - 1
 IF txtw < 1 THEN txtw = 7
 txtw$(txtw) = "                       "
 MID$(txtw$(txtw), 1) = tmp$
 tmp = txtw
 FOR i = 7 TO 1 STEP -1
     LOCATE i + 14, 16
     PRINT txtw$(tmp)
     tmp = tmp + 1
     IF tmp > 7 THEN tmp = 1
 NEXT i
RETURN

'======================================================================
setup:
 OPEN "wheel.puz" FOR INPUT AS #1
 WHILE NOT EOF(1)
  pmax = pmax + 1
  LINE INPUT #1, puz$(pmax)
 WEND
 CLOSE

'-------------------------------------------------------------
 'setup.palette

 RESTORE wheel.data

 FOR i = 32 TO 79 STEP 3
   READ cpr(i)         'RED
   READ cpg(i)         'GREEN
   READ cpb(i)         'BLUE
   cpr(i + 1) = cpr(i)
   cpg(i + 1) = cpg(i)
   cpb(i + 1) = cpb(i)
   cpr(i + 2) = cpr(i)
   cpg(i + 2) = cpg(i)
   cpb(i + 2) = cpb(i)
 NEXT i

 FOR i = 32 TO 79 STEP 3
   READ cps(i)
   cps(i + 1) = cps(i)
   cps(i + 2) = cps(i)
 NEXT i

wheel.data:
 DATA 63,0,0, 0,63,0, 0,0,63
 DATA 63,0,0, 0,63,0, 0,0,63
 DATA 63,0,0, 0,63,0, 0,0,63
 DATA 63,0,0, 0,63,0, 0,0,63
 DATA 63,0,0, 0,63,0, 0,0,63

 'zap
 DATA 0,0,0

 'score data
 DATA 1000, 800, 300, 600, 200, 100, 150, 350, 150, 100, 200, 300, 500, 700, 750, 0

 FOR i = 32 TO 79
   'PALETTE i, cp(i)
   OUT &H3C8, i
   OUT &H3C9, cpr(i)
   OUT &H3C9, cpg(i)
   OUT &H3C9, cpb(i)
 NEXT i

 '------------------------
 ' draw wheel, letter board, read in all graphics

 LINE (0, 10)-(320, 200), 2, BF

 OPEN "main.bin" FOR BINARY AS #1
 tempsize = (LOF(1) / 2)
 REDIM tempim(1 TO tempsize)
 FOR i = 1 TO tempsize
   GET #1, , tempim(i)
 NEXT i
 CLOSE #1
 PUT (3, 12), tempim, PSET

 OPEN "pointer.bin" FOR BINARY AS #1
 tempsize = (LOF(1) / 2)
 REDIM ptr1(1 TO tempsize)
 FOR i = 1 TO tempsize
   GET #1, , ptr1(i)
 NEXT i
 CLOSE #1
 PUT (50, 103), ptr1, PSET

 OPEN "pointer2.bin" FOR BINARY AS #1
 tempsize = (LOF(1) / 2)
 REDIM ptr2(1 TO tempsize)
 FOR i = 1 TO tempsize
   GET #1, , ptr2(i)
 NEXT i
 CLOSE #1

 OPEN "happy.bin" FOR BINARY AS #1
 tempsize = (LOF(1) / 2)
 REDIM happy(1 TO tempsize)
 FOR i = 1 TO tempsize
   GET #1, , happy(i)
 NEXT i
 CLOSE #1

 OPEN "unhappy.bin" FOR BINARY AS #1
 tempsize = (LOF(1) / 2)
 REDIM unhappy(1 TO tempsize)
 FOR i = 1 TO tempsize
   GET #1, , unhappy(i)
 NEXT i
 CLOSE #1


 '--------------------------------------------------------------
 'draw letter board

 LINE (107, 10)-(319, 199), 1, BF
 LINE (107, 10)-(319, 199), 15, B

 'MESSAGE box
 LINE (115, 109)-(310, 171), 0, BF
 LINE (115, 109)-(310, 171), 15, B

 'SCORE AREA
 LINE (0, 179)-(106, 199), 0, BF

RETURN

'======================================================================
update.players:

 LOCATE 24, 1: CntrPrnt pln$, 15, 1
 LOCATE 25, 1: CntrPrnt LTRIM$(STR$(pls)), 15, 1

 '
 'check to see if puzzle solved
 '

 ix = 110
 FOR i = 1 TO 5
     ix = ix + 30
     IF i <= happys THEN
        PUT (ix, 180), happy, PSET
       ELSE
        PUT (ix, 180), unhappy, PSET
     END IF
 NEXT i
RETURN

'======================================================================
pick.puzzle:
 ERASE pzb, pzbp
 FOR i = 1 TO 5: pzb(i) = STRING$(13, " "): NEXT i
 ptr = INT(RND * pmax) + 1
 xx = 0: yy = 0
 catagory$ = MID$(puz$(ptr), INSTR(1, puz$(ptr), "~") + 1)
 COLOR 12: LOCATE 3, 16: PRINT catagory$: COLOR 15
 puzzle$ = MID$(puz$(ptr), 1, INSTR(1, puz$(ptr), "~") - 1)
 opuz$ = puzzle$
 flag = 1
 WHILE flag = 1
   flag = 0
   ptr = INSTR(1, opuz$, "*")
   IF ptr = 1 THEN xx = xx + 1: opuz$ = MID$(opuz$, 2): flag = 1
   IF ptr > 1 THEN
      xx = xx + 1
      tmp$ = MID$(opuz$, 1, ptr - 1)
      yy = ((13 - LEN(tmp$)) \ 2) + 1
      MID$(pzb(xx), yy) = tmp$
      opuz$ = MID$(opuz$, ptr + 1): flag = 1
   END IF
 WEND
 'FOR i = 1 TO 5: PRINT "."; pzb(i); "."; LEN(pzb(i)): NEXT i: END
RETURN

'======================================================================
update.letter.board:
 solved = 1    'flag set to 0 if puzzled not solved
 xx = 116: yy = 30
 FOR i = 1 TO 5
   FOR ii = 1 TO 13
     IF debugf = 1 THEN pzbp(i, ii) = 1
     IF MID$(pzb(i), ii, 1) = "," THEN pzbp(i, ii) = 1
     IF MID$(pzb(i), ii, 1) = "'" THEN pzbp(i, ii) = 1
     IF MID$(pzb(i), ii, 1) = "-" THEN pzbp(i, ii) = 1
     IF MID$(pzb(i), ii, 1) = " " THEN
        'no letters
        aa$ = " "
        GOSUB print.letter
       ELSE
        IF pzbp(i, ii) = 1 THEN
           'visible letter
           aa$ = MID$(pzb(i), ii, 1)
           GOSUB print.letter
          ELSE
           'covered letter
           aa$ = "~"
           solved = 0
           GOSUB print.letter
        END IF
     END IF
     xx = xx + 15
   NEXT ii
   xx = 116: yy = yy + 15
 NEXT i
 debugf = 0
RETURN

'======================================================================
print.letter:
  aa = INSTR(1, TILE$, aa$)
  OPEN "GRAFX1.DAT" FOR BINARY AS #1
  REDIM tempim(1 TO 93)
  ptr = ((aa - 1) * 93 * 2) + 1
  'PRINT ptr;
  SEEK #1, ptr
  FOR jjj = 1 TO 93  'tempsize
    GET #1, , tempim(jjj)
  NEXT jjj
  CLOSE #1
  PUT (xx, yy), tempim, PSET

RETURN

'======================================================================
spin.wheel:
  RANDOMIZE TIMER
  iii = INT(RND * 25) + 48
  'iii = 1   'debug- un-rem to force 1 step wheel rotation
  ticks = 1
  FOR ii = 1 TO iii
   FOR i = 1 TO 48
     'IF ii > (iii \ 3) * 1 THEN ticks = 2
     'IF ii > (iii \ 3) * 2 THEN ticks = 3
     'IF ii > (iii \ 4) * 1 THEN ticks = 2
     'IF ii > (iii \ 4) * 2 THEN ticks = 3
     'IF ii > (iii \ 4) * 3 THEN ticks = 4
     IF ii > (iii \ 5) * 1 THEN ticks = 2
     IF ii > (iii \ 5) * 2 THEN ticks = 3
     IF ii > (iii \ 5) * 3 THEN ticks = 4
     IF ii > (iii \ 5) * 4 THEN ticks = 5
     'LOCATE 25, 1: PRINT ticks;
     GOSUB vdelay
     IF INKEY$ = CHR$(27) THEN GOTO game.over.lose
     GOSUB color.update
   NEXT i
  NEXT ii

RETURN

'======================================================================
vdelay:

 IF tticks = 1 THEN ticks = 0
 FOR delay! = 1 TO 900 * ticks   '---change 900 to a lower number for Qbasic
 NEXT delay!

RETURN

'======================================================================
color.update:
 FOR i = 79 TO 32 STEP -1
   'cp(i + 1) = cp(i)
   cpr(i + 1) = cpr(i)    'rotate pal, RED
   cpg(i + 1) = cpg(i)    'rotate pal, GREEN
   cpb(i + 1) = cpb(i)    'rotate pal, BLUE
   cps(i + 1) = cps(i)    'rotate scoring values
 NEXT i
 'cp(32) = cp(80)
 cpr(32) = cpr(80)
 cpg(32) = cpg(80)
 cpb(32) = cpb(80)
 cps(32) = cps(80)

 'update palette
 FOR i = 32 TO 79
   'PALETTE i, cp(i)
   OUT &H3C8, i
   OUT &H3C9, cpr(i)
   OUT &H3C9, cpg(i)
   OUT &H3C9, cpb(i)
 NEXT i

 IF cps(55) <> cps(56) THEN
    PUT (50, 103), ptr2, PSET
    click = 1
   ELSE
    PUT (50, 103), ptr1, PSET
    IF click = 1 AND tticks = 0 THEN click = 0: IF soundz = 1 THEN SOUND 300, .1
 END IF
 LOCATE 8, 5: PRINT "     ";
 IF cps(56) = 0 THEN
    tmp$ = "*ZAP*"
    COLOR 12
 END IF
 IF cps(56) > 1 THEN
    tmp$ = LTRIM$(STR$(cps(56)))
    IF LEN(tmp$) = 3 THEN tmp$ = " " + tmp$ + " " ELSE tmp$ = tmp$ + "!"
 END IF
 LOCATE 8, 5: PRINT tmp$
 COLOR 15
RETURN


'======================================================================
fini:
 SCREEN 0, 0, 0
 END

SUB CntrPrnt (a$, x, f)
 xx = (x - LEN(a$)) \ 2
 i = xx + POS(0)
 LOCATE , i
 IF f > 0 THEN
    PRINT a$;
   ELSE
    PRINT a$
 END IF
END SUB

