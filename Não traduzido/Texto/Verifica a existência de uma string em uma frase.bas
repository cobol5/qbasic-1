'Funcao para achar uma expressao em uma string
'
'Feito por Guilherme Goettems Schneider
'E-mail: guigs@via-rs.net
'http://www.geocities.com/guibocca
'AGOSTO 1999
'
DECLARE FUNCTION TEM! (ex$, fr$)

'A funcao TEM retorna a posicao onde a expressao comeca ou zero se nao achou

'Exemplos
'--------
CLS

INPUT "Digite uma frase que contenha a palavra 'hoje': ", f$
IF TEM("hoje", f$) THEN PRINT "OK" ELSE PRINT "A palavra nao foi digitada"

'para achar uma expressao sem se importar com maiusculo/minusculo use LCASE$
INPUT "Quem descobriu o Brasil? ", f$
IF TEM("cabral", LCASE$(f$)) THEN PRINT "CERTO!!!" ELSE PRINT "ERRADO!!!"
'neste caso, qualquer resposta que contenha cabral sera' aceita.

'destacando uma palavra...
INPUT "Uma frase contendo a palavra 'vermelho': ", f$
z = TEM("vermelho", f$)
IF z THEN
  IF z > 1 THEN PRINT MID$(f$, 1, z - 1);
  COLOR 12: PRINT MID$(f$, z, 8);
  COLOR 7: PRINT RIGHT$(f$, LEN(f$) - z - 7)
ELSE
  PRINT "Nao encontrado"
END IF

INPUT "Finalizar o programa? ", f$
IF TEM("s", LCASE$(f$)) THEN END ELSE RUN

FUNCTION TEM (ex$, fr$)
  t = 0
  IF LEN(fr$) >= LEN(ex$) THEN
    FOR i = 1 TO 1 + LEN(fr$) - LEN(ex$)
      IF MID$(fr$, i, LEN(ex$)) = ex$ THEN t = i: EXIT FOR
    NEXT
  END IF
  TEM = t
END FUNCTION

