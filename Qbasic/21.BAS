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
LOCATE , 28: PRINT "浜様様様様様様様様様様融"
LOCATE , 28: PRINT "� Ultima carta: "; carta
LOCATE , 28: PRINT "� Total de pontos: "; pontos
LOCATE , 28: PRINT "藩様様様様様様様様様様夕"
LOCATE 10, 51: PRINT "�"
LOCATE 11, 51: PRINT "�"
IF pontos > 21 THEN
        PRINT
        PRINT "Voce estorou!"
        PRINT "O carteador joga"
        R$ = INPUT$(1)
        GOTO mostrar
        END IF
PRINT
LOCATE , 28: PRINT "浜様様様様様様様様様様融"
LOCATE , 28: PRINT "�Entre com sua escolha:�"
LOCATE , 28: PRINT "麺様様様様様様様様様様郵"
LOCATE , 28: PRINT "�  1 - Comprar         �"
LOCATE , 28: PRINT "�  2 - Mostrar         �"
LOCATE , 28: PRINT "�  3 - Dobrar          �"
LOCATE , 28: PRINT "麺様様様様様様様様様様郵"
LOCATE , 28: PRINT "�  0 - Sair            �"
LOCATE , 28: PRINT "藩様様様様様様様様様様夕"
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
LOCATE 12, 28: PRINT "浜様様様様様様様様様様様様融"
LOCATE 13, 28: PRINT "� Ultima carta: "; carta
LOCATE 14, 28: PRINT "� Pontos do carteador: "; comp
LOCATE 15, 28: PRINT "藩様様様様様様様様様様様様夕"
LOCATE 13, 55: PRINT "�"
LOCATE 14, 55: PRINT "�"
IF pontos > 21 THEN GOTO 4
BEEP
SLEEP 1
WEND
4 COLOR 15, 4
CLS
LOCATE 1, 28: PRINT "浜様様様様様様様様様様様様融"
LOCATE 2, 28: PRINT "� Suas cartas: "; pontos
LOCATE 3, 28: PRINT "� Pontos do carteador: "; comp
LOCATE 4, 28: PRINT "藩様様様様様様様様様様様様夕"
LOCATE 2, 55: PRINT "�"
LOCATE 3, 55: PRINT "�"
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
LOCATE , 12: PRINT "     栩栩栩�    栩朧   栩�  栩�  栩栩栩朧   栩朧"
LOCATE , 12: PRINT "     栩�  栩� 栩朧栩朧 栩朧 栩�  栩�      栩朧栩朧"
LOCATE , 12: PRINT "     栩栩朧   栩�  栩� 栩栩栩朧  栩�      栩�  栩�"
LOCATE , 12: PRINT "     栩� 栩�  栩栩栩朧 栩� 栩朧  栩�      栩栩栩朧"
LOCATE , 12: PRINT "     栩栩栩�  栩�  栩� 栩�  栩�  栩栩栩朧 栩�  栩�"
LOCATE , 12: PRINT ""
LOCATE , 12: PRINT "栩栩栩朧    栩朧    栩�  栩�  栩�  栩�  栩栩栩朧  栩�  栩�"
LOCATE , 12: PRINT "栩�       栩朧栩朧  栩朧 栩�  栩�  栩�  栩�  栩�  栩�  栩�"
LOCATE , 12: PRINT "栩� 栩朧  栩�  栩�  栩栩栩朧  栩栩栩朧  栩�  栩�  栩�  栩�"
LOCATE , 12: PRINT "栩�  朧   栩栩栩朧  栩� 栩朧  栩�  栩�  栩�  栩�  栩�  栩�"
LOCATE , 12: PRINT "栩栩栩�   栩�  栩�  栩�  栩�  栩�  栩�  栩栩栩朧  栩栩栩朧"
PRINT
LOCATE , 22: PRINT "Pressione qualquer tecla para continuar"
R$ = INPUT$(1)
END SUB

SUB Empate
LOCATE 6, 12
LOCATE , 12: PRINT "栩栩栩朧  栩�  栩�  栩栩栩朧   栩朧    栩栩栩朧  栩栩栩朧  栩�"
LOCATE , 12: PRINT "栩�       栩朧栩朧  栩�  栩� 栩朧栩朧    栩朧    栩�       栩�"
LOCATE , 12: PRINT "栩栩栩朧  栩栩栩朧  栩栩栩朧 栩�  栩�    栩朧    栩栩栩朧  栩�"
LOCATE , 12: PRINT "栩�       朧 朧 朧  栩�      栩栩栩朧    栩朧    栩�         "
LOCATE , 12: PRINT "栩栩栩朧  朧 朧 朧  栩�      栩�  栩�    栩朧    栩栩栩朧  栩�"
PRINT
LOCATE , 22: PRINT "Pressione qualquer tecla para continuar"
R$ = INPUT$(1)
END SUB

SUB figura (a)
CLS
PRINT "敖陳陳陳陳�    敖陳陳陳陳多"
PRINT "�         �    � Black   崖"
PRINT "�         �    � Jack    崖"
PRINT "�         �    �         崖"
PRINT "�         �    � 栩   栩 崖"
PRINT "青陳陳陳陳�    団陳陳陳陳干"
PRINT "               青陳陳陳陳匝  "
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
LOCATE 4, 16: PRINT "浜様様様様様様様様様様様様様様様様様様様様様様様様融"
LOCATE 5, 16: PRINT "� 栩栩栩朧  栩�         栩朧    栩栩栩朧  栩�  栩� �"
LOCATE 6, 16: PRINT "� 栩�  栩�  栩�       栩朧栩朧  栩�       栩栩朧   �"
LOCATE 7, 16: PRINT "� 栩栩栩�   栩�       栩�  栩�  栩�       栩朧     �"
LOCATE 8, 16: PRINT "� 栩�  栩�  栩�       栩栩栩朧  栩�       栩栩朧   �"
LOCATE 9, 16: PRINT "� 栩栩栩�   栩栩栩朧  栩�  栩�  栩栩栩朧  栩�  栩� �"
LOCATE 10, 16: PRINT "�                                                  �"
LOCATE 11, 16: PRINT "�          栩�    栩朧    栩栩栩朧  栩�  栩�       �"
LOCATE 12, 16: PRINT "�          栩�  栩朧栩朧  栩�       栩栩朧         �"
LOCATE 13, 16: PRINT "�          栩�  栩�  栩�  栩�       栩朧           �"
LOCATE 14, 16: PRINT "�     栩   栩�  栩栩栩朧  栩�       栩栩朧         �"
LOCATE 15, 16: PRINT "�     栩栩栩朧  栩�  栩�  栩栩栩朧  栩�  栩�       �"
LOCATE 16, 16: PRINT "藩様様様様様様様様様様様様様様様様様様様様様様様様夕"
LOCATE 23, 2: PRINT "Pronto para iniciar...       "; DATE$; "     http://www.infobrasil.ezdir.net"
SLEEP 3
PLAY "EDCDC"
END SUB

SUB Voce
LOCATE 6, 12
LOCATE , 12: PRINT "        栩�  栩�  栩栩栩朧  栩栩栩朧  栩栩栩朧"
LOCATE , 12: PRINT "        栩朧栩朧  栩�  栩�  栩�       栩�     "
LOCATE , 12: PRINT "         栩栩朧   栩�  栩�  栩�       栩栩栩朧"
LOCATE , 12: PRINT "          栩朧    栩�  栩�  栩�       栩�     "
LOCATE , 12: PRINT "           朧     栩栩栩朧  栩栩栩朧  栩栩栩朧"
LOCATE , 12: PRINT ""
LOCATE , 12: PRINT "栩栩栩朧    栩朧    栩�  栩�  栩�  栩�  栩栩栩朧  栩�  栩�"
LOCATE , 12: PRINT "栩�       栩朧栩朧  栩朧 栩�  栩�  栩�  栩�  栩�  栩�  栩�"
LOCATE , 12: PRINT "栩� 栩朧  栩�  栩�  栩栩栩朧  栩栩栩朧  栩�  栩�  栩�  栩�"
LOCATE , 12: PRINT "栩�  朧   栩栩栩朧  栩� 栩朧  栩�  栩�  栩�  栩�  栩�  栩�"
LOCATE , 12: PRINT "栩栩栩�   栩�  栩�  栩�  栩�  栩�  栩�  栩栩栩朧  栩栩栩朧"
PRINT
LOCATE , 22: PRINT "Pressione qualquer tecla para continuar"
R$ = INPUT$(1)
END SUB

