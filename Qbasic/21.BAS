DECLARE SUB Voce ()
DECLARE SUB Empate ()
DECLARE SUB Carteador ()
DECLARE SUB INICIO ()
DECLARE SUB figura (a)
dinheiro = 500
INICIO
1 SCREEN 9
COLOR 15, 1
CLS
IF dinheiro = 0 THEN PRINT "Voce faliu. Tente na proxima vez": INICIO: END
PRINT "Voce tem $"; dinheiro
INPUT "Aposta: ", aposta
IF aposta > dinheiro THEN
        PRINT "Voce nao pode apostar mais do que tem!"
        R$ = INPUT$(1)
        GOTO 1
        END IF
IF aposta < 1 THEN
        PRINT "Voce nao pode apostar numeros negativos!"
        R$ = INPUT$(1)
        GOTO 1
        END IF
SCREEN 9
COLOR 15, 1
RANDOMIZE TIMER
2 carta = INT(RND * 10) + 1
pontos = pontos + carta
CLS
a = carta
figura a
3 LOCATE 8, 25: PRINT "- = S U A S   C A R T A S = -"
LOCATE 9, 1
LOCATE , 28: PRINT "ЩЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЛ"
LOCATE , 28: PRINT "К Ultima carta: "; carta
LOCATE , 28: PRINT "К Total de pontos: "; pontos
LOCATE , 28: PRINT "ШЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭМ"
LOCATE 10, 51: PRINT "К"
LOCATE 11, 51: PRINT "К"
IF pontos > 21 THEN
        PRINT
        PRINT "Voce estorou!"
        PRINT "O carteador joga"
        R$ = INPUT$(1)
        GOTO mostrar
        END IF
PRINT
LOCATE , 28: PRINT "ЩЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЛ"
LOCATE , 28: PRINT "КEntre com sua escolha:К"
LOCATE , 28: PRINT "ЬЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЙ"
LOCATE , 28: PRINT "К  1 - Comprar         К"
LOCATE , 28: PRINT "К  2 - Mostrar         К"
LOCATE , 28: PRINT "К  3 - Dobrar          К"
LOCATE , 28: PRINT "ЬЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЙ"
LOCATE , 28: PRINT "К  0 - Sair            К"
LOCATE , 28: PRINT "ШЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭМ"
LOCATE , 28: INPUT "Entre com sua escolha: ", esc
SELECT CASE esc
        CASE 1
                GOTO 2
        CASE 2
                GOTO mostrar
        CASE 3
                GOTO dobrar
        CASE 0
                END
        CASE ELSE
                GOTO 3
        END SELECT
dobrar:
IF dinheiro < aposta * 2 THEN
        PRINT "Voce nao pode dobrar. Voce tem $"; dinheiro; " apenas."
        PRINT "Voce apostou $"; aposta
        PRINT "Para dobrar e necessario $"; aposta * 2
        PRINT "A vez e do carteador."
        R$ = INPUT$(1)
ELSE
        aposta = aposta * 2
END IF
mostrar:
COLOR 15, 6
WHILE comp < 17
CLS
carta = INT(RND * 10) + 1
comp = comp + carta
a = carta
figura a
LOCATE 9, 1
LOCATE 12, 28: PRINT "ЩЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЛ"
LOCATE 13, 28: PRINT "К Ultima carta: "; carta
LOCATE 14, 28: PRINT "К Pontos do carteador: "; comp
LOCATE 15, 28: PRINT "ШЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭМ"
LOCATE 13, 55: PRINT "К"
LOCATE 14, 55: PRINT "К"
IF pontos > 21 THEN GOTO 4
BEEP
SLEEP 1
WEND
4 COLOR 15, 4
CLS
LOCATE 1, 28: PRINT "ЩЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЛ"
LOCATE 2, 28: PRINT "К Suas cartas: "; pontos
LOCATE 3, 28: PRINT "К Pontos do carteador: "; comp
LOCATE 4, 28: PRINT "ШЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭМ"
LOCATE 2, 55: PRINT "К"
LOCATE 3, 55: PRINT "К"
IF comp < 22 AND pontos > 21 THEN
        Carteador
        dinheiro = dinheiro - aposta
        END IF
IF comp > 21 AND pontos < 22 THEN
        Voce
        dinheiro = dinheiro + aposta
        END IF
IF comp = pontos AND comp < 22 THEN
        Empate
        END IF
IF comp < pontos AND pontos < 22 AND comp < 22 THEN
        Voce
        dinheiro = dinheiro + aposta
        END IF
IF comp > pontos AND pontos < 22 AND comp < 22 THEN
        Carteador
        dinheiro = dinheiro - aposta
        END IF
R$ = INPUT$(1)
comp = 0
pontos = 0
GOTO 1

SUB Carteador
LOCATE 6, 12
LOCATE , 12: PRINT "     ллллллА    лллА   ллА  ллА  лллллллА   лллА"
LOCATE , 12: PRINT "     ллА  ллА лллАлллА лллА ллА  ллА      лллАлллА"
LOCATE , 12: PRINT "     лллллА   ллА  ллА лллллллА  ллА      ллА  ллА"
LOCATE , 12: PRINT "     ллА ллА  лллллллА ллА лллА  ллА      лллллллА"
LOCATE , 12: PRINT "     ллллллА  ллА  ллА ллА  ллА  лллллллА ллА  ллА"
LOCATE , 12: PRINT ""
LOCATE , 12: PRINT "лллллллА    лллА    ллА  ллА  ллА  ллА  лллллллА  ллА  ллА"
LOCATE , 12: PRINT "ллА       лллАлллА  лллА ллА  ллА  ллА  ллА  ллА  ллА  ллА"
LOCATE , 12: PRINT "ллА лллА  ллА  ллА  лллллллА  лллллллА  ллА  ллА  ллА  ллА"
LOCATE , 12: PRINT "ллА  лА   лллллллА  ллА лллА  ллА  ллА  ллА  ллА  ллА  ллА"
LOCATE , 12: PRINT "ллллллА   ллА  ллА  ллА  ллА  ллА  ллА  лллллллА  лллллллА"
PRINT
LOCATE , 22: PRINT "Pressione qualquer tecla para continuar"
R$ = INPUT$(1)
END SUB

SUB Empate
LOCATE 6, 12
LOCATE , 12: PRINT "лллллллА  ллА  ллА  лллллллА   лллА    лллллллА  лллллллА  ллА"
LOCATE , 12: PRINT "ллА       лллАлллА  ллА  ллА лллАлллА    лллА    ллА       ллА"
LOCATE , 12: PRINT "лллллллА  лллллллА  лллллллА ллА  ллА    лллА    лллллллА  ллА"
LOCATE , 12: PRINT "ллА       лА лА лА  ллА      лллллллА    лллА    ллА         "
LOCATE , 12: PRINT "лллллллА  лА лА лА  ллА      ллА  ллА    лллА    лллллллА  ллА"
PRINT
LOCATE , 22: PRINT "Pressione qualquer tecla para continuar"
R$ = INPUT$(1)
END SUB

SUB figura (a)
CLS
PRINT "кФФФФФФФФФП    кФФФФФФФФФТП"
PRINT "Г         Г    Г Black   ГГ"
PRINT "Г         Г    Г Jack    ГГ"
PRINT "Г         Г    Г         ГГ"
PRINT "Г         Г    Г лл   лл ГГ"
PRINT "РФФФФФФФФФй    УФФФФФФФФФДГ"
PRINT "               РФФФФФФФФФСй  "
PRINT
SELECT CASE a
        CASE 1
                LOCATE 5, 21
                PRINT "A"
                SLEEP 1
                LOCATE 5, 21
                PRINT " "
                LOCATE 2, 3
                PRINT "A"
                LOCATE 5, 9
                PRINT "A"
        CASE 2
                LOCATE 5, 21
                PRINT "2"
                SLEEP 1
                LOCATE 5, 21
                PRINT " "
                LOCATE 2, 3
                PRINT "2"
                LOCATE 5, 9
                PRINT "2"
        CASE 3
                LOCATE 5, 21
                PRINT "3"
                SLEEP 1
                LOCATE 5, 21
                PRINT " "
                LOCATE 2, 3
                PRINT "3"
                LOCATE 5, 9
                PRINT "3"
        CASE 4
                LOCATE 5, 21
                PRINT "4"
                SLEEP 1
                LOCATE 5, 21
                PRINT " "
                LOCATE 2, 3
                PRINT "4"
                LOCATE 5, 9
                PRINT "4"
        CASE 5
                LOCATE 5, 21
                PRINT "5"
                SLEEP 1
                LOCATE 5, 21
                PRINT " "
                LOCATE 2, 3
                PRINT "5"
                LOCATE 5, 9
                PRINT "5"
        CASE 6
                LOCATE 5, 21
                PRINT "6"
                SLEEP 1
                LOCATE 5, 21
                PRINT " "
                LOCATE 2, 3
                PRINT "6"
                LOCATE 5, 9
                PRINT "6"
        CASE 7
                LOCATE 5, 21
                PRINT "7"
                SLEEP 1
                LOCATE 5, 21
                PRINT " "
                LOCATE 2, 3
                PRINT "7"
                LOCATE 5, 9
                PRINT "7"
        CASE 8
                LOCATE 5, 21
                PRINT "Q"
                SLEEP 1
                LOCATE 5, 21
                PRINT " "
                LOCATE 2, 3
                PRINT "Q"
                LOCATE 5, 9
                PRINT "Q"
        CASE 9
                LOCATE 5, 21
                PRINT "J"
                SLEEP 1
                LOCATE 5, 21
                PRINT " "
                LOCATE 2, 3
                PRINT "J"
                LOCATE 5, 9
                PRINT "J"
        CASE 10
                LOCATE 5, 21
                PRINT "K"
                SLEEP 1
                LOCATE 5, 21
                PRINT " "
                LOCATE 2, 3
                PRINT "K"
                LOCATE 5, 9
                PRINT "K"
        END SELECT
END SUB

SUB INICIO
SCREEN 9
COLOR 11, 1
LOCATE 4, 16: PRINT "ЩЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЛ"
LOCATE 5, 16: PRINT "К лллллллА  ллА         лллА    лллллллА  ллА  ллА К"
LOCATE 6, 16: PRINT "К ллА  ллА  ллА       лллАлллА  ллА       лллллА   К"
LOCATE 7, 16: PRINT "К ллллллА   ллА       ллА  ллА  ллА       лллА     К"
LOCATE 8, 16: PRINT "К ллА  ллА  ллА       лллллллА  ллА       лллллА   К"
LOCATE 9, 16: PRINT "К ллллллА   лллллллА  ллА  ллА  лллллллА  ллА  ллА К"
LOCATE 10, 16: PRINT "К                                                  К"
LOCATE 11, 16: PRINT "К          ллА    лллА    лллллллА  ллА  ллА       К"
LOCATE 12, 16: PRINT "К          ллА  лллАлллА  ллА       лллллА         К"
LOCATE 13, 16: PRINT "К          ллА  ллА  ллА  ллА       лллА           К"
LOCATE 14, 16: PRINT "К     лл   ллА  лллллллА  ллА       лллллА         К"
LOCATE 15, 16: PRINT "К     лллллллА  ллА  ллА  лллллллА  ллА  ллА       К"
LOCATE 16, 16: PRINT "ШЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭМ"
LOCATE 23, 2: PRINT "Pronto para iniciar...       "; DATE$; "     http://www.infobrasil.ezdir.net"
SLEEP 3
PLAY "EDCDC"
END SUB

SUB Voce
LOCATE 6, 12
LOCATE , 12: PRINT "        ллА  ллА  лллллллА  лллллллА  лллллллА"
LOCATE , 12: PRINT "        лллАлллА  ллА  ллА  ллА       ллА     "
LOCATE , 12: PRINT "         лллллА   ллА  ллА  ллА       лллллллА"
LOCATE , 12: PRINT "          лллА    ллА  ллА  ллА       ллА     "
LOCATE , 12: PRINT "           лА     лллллллА  лллллллА  лллллллА"
LOCATE , 12: PRINT ""
LOCATE , 12: PRINT "лллллллА    лллА    ллА  ллА  ллА  ллА  лллллллА  ллА  ллА"
LOCATE , 12: PRINT "ллА       лллАлллА  лллА ллА  ллА  ллА  ллА  ллА  ллА  ллА"
LOCATE , 12: PRINT "ллА лллА  ллА  ллА  лллллллА  лллллллА  ллА  ллА  ллА  ллА"
LOCATE , 12: PRINT "ллА  лА   лллллллА  ллА лллА  ллА  ллА  ллА  ллА  ллА  ллА"
LOCATE , 12: PRINT "ллллллА   ллА  ллА  ллА  ллА  ллА  ллА  лллллллА  лллллллА"
PRINT
LOCATE , 22: PRINT "Pressione qualquer tecla para continuar"
R$ = INPUT$(1)
END SUB

