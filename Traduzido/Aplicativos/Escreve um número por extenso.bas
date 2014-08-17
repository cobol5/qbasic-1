Begin:
CLS
COLOR 14
LOCATE 2, 25: PRINT "Algarismo por Extenso"
COLOR 15
LOCATE 5, 25: PRINT "Numeral M ximo : 999"
COLOR 2
LOCATE 15, 20: PRINT "  Digite no espa‡o abaixo, o numeral no qual"
LOCATE 16, 25: PRINT "  ser  escrito por extenso."
COLOR 6, 6
LOCATE 20, 40: PRINT "UUU"
COLOR 15, 6
DO
a$ = INKEY$
IF a$ = "1" THEN GOTO cento
IF a$ = "2" THEN GOTO duzentos
IF a$ = "3" THEN GOTO trezentos
IF a$ = "4" THEN GOTO quatrocentos
IF a$ = "5" THEN GOTO quinhentos
IF a$ = "6" THEN GOTO seiscentos
IF a$ = "7" THEN GOTO Setecentos
IF a$ = "8" THEN GOTO Oitocentos
IF a$ = "9" THEN GOTO novecentos
IF a$ = "0" THEN GOTO tucao
IF a$ = "" THEN  ELSE
IF a$ = CHR$(27) THEN END
COLOR 15, 0
LOOP

cento:
COLOR 15, 6: LOCATE 20, 40: PRINT "1"
COLOR 15, 0
LOCATE 22, 10: PRINT "Cento"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO raciocina
IF b$ = "2" THEN GOTO vinte
IF b$ = "3" THEN GOTO trinta
IF b$ = "4" THEN GOTO quarenta
IF b$ = "5" THEN GOTO cinquenta
IF b$ = "6" THEN GOTO sessenta
IF b$ = "7" THEN GOTO setenta
IF b$ = "8" THEN GOTO oitenta
IF b$ = "9" THEN GOTO noventa
IF b$ = "0" THEN GOTO Raciocina3
IF b$ = "" THEN  ELSE
LOOP

duzentos:
COLOR 15, 6: LOCATE 20, 40: PRINT "2"
COLOR 15, 0
LOCATE 22, 10: PRINT "Duzentos"
DO
c$ = INKEY$
IF c$ = "1" THEN GOTO raciocina
IF c$ = "2" THEN GOTO vinte
IF c$ = "3" THEN GOTO trinta
IF c$ = "4" THEN GOTO quarenta
IF c$ = "5" THEN GOTO cinquenta
IF c$ = "6" THEN GOTO sessenta
IF c$ = "7" THEN GOTO setenta
IF c$ = "8" THEN GOTO oitenta
IF c$ = "9" THEN GOTO noventa
IF c$ = "0" THEN GOTO raciocina2
IF c$ = "" THEN  ELSE
LOOP

trezentos:
COLOR 15, 6: LOCATE 20, 40: PRINT "3"
COLOR 15, 0
LOCATE 22, 10: PRINT "Trezentos"
DO
d$ = INKEY$
IF d$ = "1" THEN GOTO raciocina
IF d$ = "2" THEN GOTO vinte
IF d$ = "3" THEN GOTO trinta
IF d$ = "4" THEN GOTO quarenta
IF d$ = "5" THEN GOTO cinquenta
IF d$ = "6" THEN GOTO sessenta
IF d$ = "7" THEN GOTO setenta
IF d$ = "8" THEN GOTO oitenta
IF d$ = "9" THEN GOTO noventa
IF d$ = "0" THEN GOTO raciocina2
IF d$ = "" THEN  ELSE
LOOP

quatrocentos:
COLOR 15, 6: LOCATE 20, 40: PRINT "4"
COLOR 15, 0
LOCATE 22, 10: PRINT "Quatrocentos"
DO
e$ = INKEY$
IF e$ = "1" THEN GOTO raciocina
IF e$ = "2" THEN GOTO vinte
IF e$ = "3" THEN GOTO trinta
IF e$ = "4" THEN GOTO quarenta
IF e$ = "5" THEN GOTO cinquenta
IF e$ = "6" THEN GOTO sessenta
IF e$ = "7" THEN GOTO setenta
IF e$ = "8" THEN GOTO oitenta
IF e$ = "9" THEN GOTO noventa
IF e$ = "0" THEN GOTO raciocina2
IF e$ = "" THEN  ELSE
LOOP
quinhentos:
COLOR 15, 6: LOCATE 20, 40: PRINT "5"
COLOR 15, 0
LOCATE 22, 10: PRINT "Quinhentos"
DO
f$ = INKEY$
IF f$ = "1" THEN GOTO raciocina
IF f$ = "2" THEN GOTO vinte
IF f$ = "3" THEN GOTO trinta
IF f$ = "4" THEN GOTO quarenta
IF f$ = "5" THEN GOTO cinquenta
IF f$ = "6" THEN GOTO sessenta
IF f$ = "7" THEN GOTO setenta
IF f$ = "8" THEN GOTO oitenta
IF f$ = "9" THEN GOTO noventa
IF f$ = "0" THEN GOTO raciocina2
IF f$ = "" THEN  ELSE
LOOP
seiscentos:
COLOR 15, 6: LOCATE 20, 40: PRINT "6"
COLOR 15, 0
LOCATE 22, 10: PRINT "Seiscentos"
DO
g$ = INKEY$
IF g$ = "1" THEN GOTO raciocina
IF g$ = "2" THEN GOTO vinte
IF g$ = "3" THEN GOTO trinta
IF g$ = "4" THEN GOTO quarenta
IF g$ = "5" THEN GOTO cinquenta
IF g$ = "6" THEN GOTO sessenta
IF g$ = "7" THEN GOTO setenta
IF g$ = "8" THEN GOTO oitenta
IF g$ = "9" THEN GOTO noventa
IF g$ = "0" THEN GOTO raciocina2
IF g$ = "" THEN  ELSE
LOOP
Setecentos:
COLOR 15, 6: LOCATE 20, 40: PRINT "7"
COLOR 15, 0
LOCATE 22, 10: PRINT "Setecentos"
DO
h$ = INKEY$
IF h$ = "1" THEN GOTO raciocina
IF h$ = "2" THEN GOTO vinte
IF h$ = "3" THEN GOTO trinta
IF h$ = "4" THEN GOTO quarenta
IF h$ = "5" THEN GOTO cinquenta
IF h$ = "6" THEN GOTO sessenta
IF h$ = "7" THEN GOTO setenta
IF h$ = "8" THEN GOTO oitenta
IF h$ = "9" THEN GOTO noventa
IF h$ = "0" THEN GOTO raciocina2
IF h$ = "" THEN  ELSE
LOOP
Oitocentos:
COLOR 15, 6: LOCATE 20, 40: PRINT "8"
COLOR 15, 0
LOCATE 22, 10: PRINT "Oitocentos"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO raciocina
IF b$ = "2" THEN GOTO vinte
IF b$ = "3" THEN GOTO trinta
IF b$ = "4" THEN GOTO quarenta
IF b$ = "5" THEN GOTO cinquenta
IF b$ = "6" THEN GOTO sessenta
IF b$ = "7" THEN GOTO setenta
IF b$ = "8" THEN GOTO oitenta
IF b$ = "9" THEN GOTO noventa
IF b$ = "0" THEN GOTO raciocina2
IF b$ = "" THEN  ELSE
LOOP
novecentos:
COLOR 15, 6: LOCATE 20, 40: PRINT "9"
COLOR 15, 0
LOCATE 22, 10: PRINT "Novecentos"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO raciocina
IF b$ = "2" THEN GOTO vinte
IF b$ = "3" THEN GOTO trinta
IF b$ = "4" THEN GOTO quarenta
IF b$ = "5" THEN GOTO cinquenta
IF b$ = "6" THEN GOTO sessenta
IF b$ = "7" THEN GOTO setenta
IF b$ = "8" THEN GOTO oitenta
IF b$ = "9" THEN GOTO noventa
IF b$ = "0" THEN GOTO raciocina2
IF b$ = "" THEN  ELSE
LOOP

raciocina:
COLOR 15, 6: LOCATE 20, 41: PRINT "1"
DO
c$ = INKEY$
IF c$ = "1" THEN GOTO onze
IF c$ = "2" THEN GOTO doze
IF c$ = "3" THEN GOTO treze
IF c$ = "4" THEN GOTO quatorze
IF c$ = "5" THEN GOTO quinze
IF c$ = "6" THEN GOTO dezesseis
IF c$ = "7" THEN GOTO dezessete
IF c$ = "8" THEN GOTO dezoito
IF c$ = "9" THEN GOTO dezenove
IF c$ = "0" THEN GOTO dez
IF c$ = "" THEN  ELSE
LOOP
raciocina2:
COLOR 15, 6: LOCATE 20, 41: PRINT "0"
DO
z$ = INKEY$
IF z$ = "1" THEN GOTO um
IF z$ = "2" THEN GOTO dois
IF z$ = "3" THEN GOTO tres
IF z$ = "4" THEN GOTO quatro
IF z$ = "5" THEN GOTO cinco
IF z$ = "6" THEN GOTO seis
IF z$ = "7" THEN GOTO sete
IF z$ = "8" THEN GOTO oito
IF z$ = "9" THEN GOTO nove
IF z$ = "0" THEN GOTO Xaco
IF z$ = "" THEN  ELSE
LOOP
vinte:
COLOR 15, 6: LOCATE 20, 41: PRINT "2"
COLOR 15, 0
LOCATE 22, 25: PRINT "e vinte"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO um
IF b$ = "2" THEN GOTO dois
IF b$ = "3" THEN GOTO tres
IF b$ = "4" THEN GOTO quatro
IF b$ = "5" THEN GOTO cinco
IF b$ = "6" THEN GOTO seis
IF b$ = "7" THEN GOTO sete
IF b$ = "8" THEN GOTO oito
IF b$ = "9" THEN GOTO nove
IF b$ = "0" THEN GOTO Xaco
IF b$ = "" THEN  ELSE
LOOP

Xaco:
COLOR 15, 6
LOCATE 20, 42: PRINT "0"
COLOR 15, 0
DO
y$ = INKEY$
IF y$ = " " THEN GOTO Begin
IF y$ = CHR$(27) THEN END
LOOP
trinta:
COLOR 15, 6: LOCATE 20, 41: PRINT "3"
COLOR 15, 0
LOCATE 22, 25: PRINT "e trinta"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO um
IF b$ = "2" THEN GOTO dois
IF b$ = "3" THEN GOTO tres
IF b$ = "4" THEN GOTO quatro
IF b$ = "5" THEN GOTO cinco
IF b$ = "6" THEN GOTO seis
IF b$ = "7" THEN GOTO sete
IF b$ = "8" THEN GOTO oito
IF b$ = "9" THEN GOTO nove
IF b$ = "0" THEN GOTO Xaco
IF b$ = "" THEN  ELSE
LOOP
quarenta:
COLOR 15, 6: LOCATE 20, 41: PRINT "4"
COLOR 15, 0
LOCATE 22, 25: PRINT "e quarenta"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO um
IF b$ = "2" THEN GOTO dois
IF b$ = "3" THEN GOTO tres
IF b$ = "4" THEN GOTO quatro
IF b$ = "5" THEN GOTO cinco
IF b$ = "6" THEN GOTO seis
IF b$ = "7" THEN GOTO sete
IF b$ = "8" THEN GOTO oito
IF b$ = "9" THEN GOTO nove
IF b$ = "0" THEN GOTO Xaco
IF b$ = "" THEN  ELSE
LOOP
cinquenta:
COLOR 15, 6: LOCATE 20, 41: PRINT "5"
COLOR 15, 0
LOCATE 22, 25: PRINT "e cinquenta"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO um
IF b$ = "2" THEN GOTO dois
IF b$ = "3" THEN GOTO tres
IF b$ = "4" THEN GOTO quatro
IF b$ = "5" THEN GOTO cinco
IF b$ = "6" THEN GOTO seis
IF b$ = "7" THEN GOTO sete
IF b$ = "8" THEN GOTO oito
IF b$ = "9" THEN GOTO nove
IF b$ = "0" THEN GOTO Xaco
IF b$ = "" THEN  ELSE
LOOP
sessenta:
COLOR 15, 6: LOCATE 20, 41: PRINT "6"
COLOR 15, 0
LOCATE 22, 25: PRINT "e sessenta"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO um
IF b$ = "2" THEN GOTO dois
IF b$ = "3" THEN GOTO tres
IF b$ = "4" THEN GOTO quatro
IF b$ = "5" THEN GOTO cinco
IF b$ = "6" THEN GOTO seis
IF b$ = "7" THEN GOTO sete
IF b$ = "8" THEN GOTO oito
IF b$ = "9" THEN GOTO nove
IF b$ = "0" THEN GOTO Xaco
IF b$ = "" THEN  ELSE
LOOP
setenta:
COLOR 15, 6
LOCATE 20, 41: PRINT "7"
COLOR 15, 0
LOCATE 22, 25: PRINT "e setenta"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO um
IF b$ = "2" THEN GOTO dois
IF b$ = "3" THEN GOTO tres
IF b$ = "4" THEN GOTO quatro
IF b$ = "5" THEN GOTO cinco
IF b$ = "6" THEN GOTO seis
IF b$ = "7" THEN GOTO sete
IF b$ = "8" THEN GOTO oito
IF b$ = "9" THEN GOTO nove
IF b$ = "0" THEN GOTO Xaco
IF b$ = "" THEN  ELSE
LOOP
oitenta:
COLOR 15, 6: LOCATE 20, 41: PRINT "8"
COLOR 15, 0
LOCATE 22, 25: PRINT "e oitenta"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO um
IF b$ = "2" THEN GOTO dois
IF b$ = "3" THEN GOTO tres
IF b$ = "4" THEN GOTO quatro
IF b$ = "5" THEN GOTO cinco
IF b$ = "6" THEN GOTO seis
IF b$ = "7" THEN GOTO sete
IF b$ = "8" THEN GOTO oito
IF b$ = "9" THEN GOTO nove
IF b$ = "0" THEN GOTO Xaco
IF b$ = "" THEN  ELSE
LOOP
noventa:
COLOR 15, 6: LOCATE 20, 41: PRINT "9"
COLOR 15, 0
LOCATE 22, 25: PRINT "e noventa"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO um
IF b$ = "2" THEN GOTO dois
IF b$ = "3" THEN GOTO tres
IF b$ = "4" THEN GOTO quatro
IF b$ = "5" THEN GOTO cinco
IF b$ = "6" THEN GOTO seis
IF b$ = "7" THEN GOTO sete
IF b$ = "8" THEN GOTO oito
IF b$ = "9" THEN GOTO nove
IF b$ = "0" THEN GOTO Xaco
IF b$ = "" THEN  ELSE
LOOP

onze:
COLOR 15, 6: COLOR 15, 6: LOCATE 20, 42: PRINT "1"
COLOR 15, 0
LOCATE 22, 25: PRINT "e onze."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP

dez:
COLOR 15, 6: LOCATE 20, 42: PRINT "0"
COLOR 15, 0
LOCATE 22, 25: PRINT "e dez."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP

doze:
COLOR 15, 6: LOCATE 20, 42: PRINT "2"
COLOR 15, 0
LOCATE 22, 25: PRINT "e doze."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
treze:
COLOR 15, 6: LOCATE 20, 42: PRINT "3"
COLOR 15, 0
LOCATE 22, 25: PRINT "e treze."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
quatorze:
COLOR 15, 6: LOCATE 20, 42: PRINT "4"
COLOR 15, 0
LOCATE 22, 25: PRINT "e quatorze."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
quinze:
COLOR 15, 6: LOCATE 20, 42: PRINT "5"
COLOR 15, 0
LOCATE 22, 25: PRINT "e quinze."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
dezesseis:
COLOR 15, 6: LOCATE 20, 42: PRINT "6"
COLOR 15, 0
LOCATE 22, 25: PRINT "e dezesseis."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
dezessete:
COLOR 15, 6: LOCATE 20, 42: PRINT "7"
COLOR 15, 0
LOCATE 22, 25: PRINT "e dezessete."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
dezoito:
COLOR 15, 6: LOCATE 20, 42: PRINT "8"
COLOR 15, 0
LOCATE 22, 25: PRINT "e dezoito."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
dezenove:
COLOR 15, 6: LOCATE 20, 42: PRINT "9"
COLOR 15, 0
LOCATE 22, 25: PRINT "e dezenove."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP

um:
COLOR 15, 6: LOCATE 20, 42: PRINT "1"
COLOR 15, 0
LOCATE 22, 37: PRINT "e um."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
dois:
COLOR 15, 6: LOCATE 20, 42: PRINT "2"
COLOR 15, 0
LOCATE 22, 37: PRINT "e dois."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
tres:
COLOR 15, 6: LOCATE 20, 42: PRINT "3"
COLOR 15, 0
LOCATE 22, 37: PRINT "e trˆs."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
quatro:
COLOR 15, 6: LOCATE 20, 42: PRINT "4"
COLOR 15, 0
LOCATE 22, 37: PRINT "e quatro."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
cinco:
COLOR 15, 6: LOCATE 20, 42: PRINT "5"
COLOR 15, 0
LOCATE 22, 37: PRINT "e cinco."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
seis:
COLOR 15, 6: LOCATE 20, 42: PRINT "6"
COLOR 15, 0
LOCATE 22, 37: PRINT "e seis."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
sete:
COLOR 15, 6:
LOCATE 20, 42: PRINT "7"
COLOR 15, 0
LOCATE 22, 37: PRINT "e sete."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
oito:
COLOR 15, 6
LOCATE 20, 42: PRINT "8"
COLOR 15, 0
LOCATE 22, 37: PRINT "e oito."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
nove:
COLOR 15, 6
LOCATE 20, 42: PRINT "9"
COLOR 15, 0
LOCATE 22, 37: PRINT "e nove."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP

Raciocina3:
COLOR 15, 6: LOCATE 20, 41: PRINT "0"
DO
z$ = INKEY$
IF z$ = "1" THEN GOTO um
IF z$ = "2" THEN GOTO dois
IF z$ = "3" THEN GOTO tres
IF z$ = "4" THEN GOTO quatro
IF z$ = "5" THEN GOTO cinco
IF z$ = "6" THEN GOTO seis
IF z$ = "7" THEN GOTO sete
IF z$ = "8" THEN GOTO oito
IF z$ = "9" THEN GOTO nove
IF z$ = "0" THEN GOTO xacu
IF z$ = "" THEN  ELSE
LOOP
xacu:
COLOR 15, 6: LOCATE 20, 42: PRINT "0"
COLOR 15, 0
LOCATE 22, 10: PRINT "                                                  "
LOCATE 22, 10: PRINT "Cem"
DO
y$ = INKEY$
IF y$ = " " THEN GOTO Begin
IF y$ = CHR$(27) THEN END
LOOP

tucao:
COLOR 15, 6: LOCATE 20, 40: PRINT "0"
COLOR 15, 0
DO
j$ = INKEY$
IF j$ = "1" THEN GOTO Raciocina4
IF j$ = "2" THEN GOTO vinte2
IF j$ = "3" THEN GOTO trinta2
IF j$ = "4" THEN GOTO quarenta2
IF j$ = "5" THEN GOTO cinquenta2
IF j$ = "6" THEN GOTO sessenta2
IF j$ = "7" THEN GOTO setenta2
IF j$ = "8" THEN GOTO oitenta2
IF j$ = "9" THEN GOTO noventa2
IF j$ = "0" THEN GOTO Raciocina5
IF j$ = "" THEN  ELSE
LOOP

Raciocina4:
COLOR 15, 6: LOCATE 20, 41: PRINT "1"
COLOR 15, 0
DO
j$ = INKEY$
IF j$ = "1" THEN GOTO onze2
IF j$ = "2" THEN GOTO doze2
IF j$ = "3" THEN GOTO treze2
IF j$ = "4" THEN GOTO quartoze2
IF j$ = "5" THEN GOTO quinze2
IF j$ = "6" THEN GOTO dezesseis2
IF j$ = "7" THEN GOTO dezessete2
IF j$ = "8" THEN GOTO dezoito2
IF j$ = "9" THEN GOTO dezenove2
IF j$ = "0" THEN GOTO Raciocina6
IF j$ = "" THEN  ELSE
LOOP

Raciocina6:
COLOR 15, 6: LOCATE 20, 42: PRINT "0"
COLOR 15, 0
LOCATE 22, 10: PRINT "                                                  "
LOCATE 22, 10: PRINT "Dez"
DO
j$ = INKEY$
IF j$ = CHR$(27) THEN END
IF j$ = " " THEN GOTO Begin
LOOP

doze2:
COLOR 15, 6: LOCATE 20, 42: PRINT "2"
COLOR 15, 0
LOCATE 22, 25: PRINT "Doze."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
treze2:
COLOR 15, 6: LOCATE 20, 42: PRINT "3"
COLOR 15, 0
LOCATE 22, 25: PRINT "Treze."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
quartoze2:
COLOR 15, 6: LOCATE 20, 42: PRINT "4"
COLOR 15, 0
LOCATE 22, 25: PRINT "Quatorze."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
quinze2:
COLOR 15, 6: LOCATE 20, 42: PRINT "5"
COLOR 15, 0
LOCATE 22, 25: PRINT "Quinze."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
dezesseis2:
COLOR 15, 6: LOCATE 20, 42: PRINT "6"
COLOR 15, 0
LOCATE 22, 25: PRINT "Dezesseis."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
dezessete2:
COLOR 15, 6: LOCATE 20, 42: PRINT "7"
COLOR 15, 0
LOCATE 22, 25: PRINT "Dezessete."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
dezoito2:
COLOR 15, 6: LOCATE 20, 42: PRINT "8"
COLOR 15, 0
LOCATE 22, 25: PRINT "Dezoito."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
dezenove2:
COLOR 15, 6: LOCATE 20, 42: PRINT "9"
COLOR 15, 0
LOCATE 22, 25: PRINT "Dezenove."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP

Raciocina5:
COLOR 15, 6: LOCATE 20, 41: PRINT "0"
COLOR 15, 0
DO
j$ = INKEY$
IF j$ = "1" THEN GOTO um2
IF j$ = "2" THEN GOTO dois2
IF j$ = "3" THEN GOTO tres2
IF j$ = "4" THEN GOTO quatro2
IF j$ = "5" THEN GOTO Cinco2
IF j$ = "6" THEN GOTO Seis2
IF j$ = "7" THEN GOTO Sete2
IF j$ = "8" THEN GOTO Oito2
IF j$ = "9" THEN GOTO Nove2
IF j$ = "0" THEN GOTO zero
IF j$ = "" THEN  ELSE
LOOP

zero:
COLOR 15, 6: LOCATE 20, 42: PRINT "0"
COLOR 15, 0
LOCATE 22, 25: PRINT "Zero."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
um2:
COLOR 15, 6: LOCATE 20, 42: PRINT "1"
COLOR 15, 0
LOCATE 22, 25: PRINT "Um."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
dois2:
COLOR 15, 6: LOCATE 20, 42: PRINT "2"
COLOR 15, 0
LOCATE 22, 25: PRINT "Dois."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
tres2:
COLOR 15, 6: LOCATE 20, 42: PRINT "3"
COLOR 15, 0
LOCATE 22, 25: PRINT "Trˆs."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
quatro2:
COLOR 15, 6: LOCATE 20, 42: PRINT "4"
COLOR 15, 0
LOCATE 22, 25: PRINT "Quatro."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
Cinco2:
COLOR 15, 6: LOCATE 20, 42: PRINT "5"
COLOR 15, 0
LOCATE 22, 25: PRINT "Cinco."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
Seis2:
COLOR 15, 6: LOCATE 20, 42: PRINT "6"
COLOR 15, 0
LOCATE 22, 25: PRINT "Seis2."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
Sete2:
COLOR 15, 6: LOCATE 20, 42: PRINT "7"
COLOR 15, 0
LOCATE 22, 25: PRINT "Sete."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
Oito2:
COLOR 15, 6: LOCATE 20, 42: PRINT "8"
COLOR 15, 0
LOCATE 22, 25: PRINT "Oito."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP
Nove2:
COLOR 15, 6: LOCATE 20, 42: PRINT "9"
COLOR 15, 0
LOCATE 22, 25: PRINT "Nove."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP

vinte2:
COLOR 15, 6: LOCATE 20, 41: PRINT "2"
COLOR 15, 0
LOCATE 22, 25: PRINT "Vinte"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO um
IF b$ = "2" THEN GOTO dois
IF b$ = "3" THEN GOTO tres
IF b$ = "4" THEN GOTO quatro
IF b$ = "5" THEN GOTO cinco
IF b$ = "6" THEN GOTO seis
IF b$ = "7" THEN GOTO sete
IF b$ = "8" THEN GOTO oito
IF b$ = "9" THEN GOTO nove
IF b$ = "0" THEN GOTO Xaco
IF b$ = "" THEN  ELSE
LOOP

trinta2:
COLOR 15, 6: LOCATE 20, 41: PRINT "3"
COLOR 15, 0
LOCATE 22, 25: PRINT "Trinta"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO um
IF b$ = "2" THEN GOTO dois
IF b$ = "3" THEN GOTO tres
IF b$ = "4" THEN GOTO quatro
IF b$ = "5" THEN GOTO cinco
IF b$ = "6" THEN GOTO seis
IF b$ = "7" THEN GOTO sete
IF b$ = "8" THEN GOTO oito
IF b$ = "9" THEN GOTO nove
IF b$ = "0" THEN GOTO Xaco
IF b$ = "" THEN  ELSE
LOOP
quarenta2:
COLOR 15, 6: LOCATE 20, 41: PRINT "4"
COLOR 15, 0
LOCATE 22, 25: PRINT "Quarenta"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO um
IF b$ = "2" THEN GOTO dois
IF b$ = "3" THEN GOTO tres
IF b$ = "4" THEN GOTO quatro
IF b$ = "5" THEN GOTO cinco
IF b$ = "6" THEN GOTO seis
IF b$ = "7" THEN GOTO sete
IF b$ = "8" THEN GOTO oito
IF b$ = "9" THEN GOTO nove
IF b$ = "0" THEN GOTO Xaco
IF b$ = "" THEN  ELSE
LOOP
cinquenta2:
COLOR 15, 6: LOCATE 20, 41: PRINT "5"
COLOR 15, 0
LOCATE 22, 25: PRINT "Cinquenta"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO um
IF b$ = "2" THEN GOTO dois
IF b$ = "3" THEN GOTO tres
IF b$ = "4" THEN GOTO quatro
IF b$ = "5" THEN GOTO cinco
IF b$ = "6" THEN GOTO seis
IF b$ = "7" THEN GOTO sete
IF b$ = "8" THEN GOTO oito
IF b$ = "9" THEN GOTO nove
IF b$ = "0" THEN GOTO Xaco
IF b$ = "" THEN  ELSE
LOOP
sessenta2:
COLOR 15, 6: LOCATE 20, 41: PRINT "6"
COLOR 15, 0
LOCATE 22, 25: PRINT "Sessenta"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO um
IF b$ = "2" THEN GOTO dois
IF b$ = "3" THEN GOTO tres
IF b$ = "4" THEN GOTO quatro
IF b$ = "5" THEN GOTO cinco
IF b$ = "6" THEN GOTO seis
IF b$ = "7" THEN GOTO sete
IF b$ = "8" THEN GOTO oito
IF b$ = "9" THEN GOTO nove
IF b$ = "0" THEN GOTO Xaco
IF b$ = "" THEN  ELSE
LOOP
setenta2:
COLOR 15, 6: LOCATE 20, 41: PRINT "7"
COLOR 15, 0
LOCATE 22, 25: PRINT "Setenta"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO um
IF b$ = "2" THEN GOTO dois
IF b$ = "3" THEN GOTO tres
IF b$ = "4" THEN GOTO quatro
IF b$ = "5" THEN GOTO cinco
IF b$ = "6" THEN GOTO seis
IF b$ = "7" THEN GOTO sete
IF b$ = "8" THEN GOTO oito
IF b$ = "9" THEN GOTO nove
IF b$ = "0" THEN GOTO Xaco
IF b$ = "" THEN  ELSE
LOOP
oitenta2:
COLOR 15, 6: LOCATE 20, 41: PRINT "8"
COLOR 15, 0
LOCATE 22, 25: PRINT "Oitenta"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO um
IF b$ = "2" THEN GOTO dois
IF b$ = "3" THEN GOTO tres
IF b$ = "4" THEN GOTO quatro
IF b$ = "5" THEN GOTO cinco
IF b$ = "6" THEN GOTO seis
IF b$ = "7" THEN GOTO sete
IF b$ = "8" THEN GOTO oito
IF b$ = "9" THEN GOTO nove
IF b$ = "0" THEN GOTO Xaco
IF b$ = "" THEN  ELSE
LOOP
noventa2:
COLOR 15, 6: LOCATE 20, 41: PRINT "9"
COLOR 15, 0
LOCATE 22, 25: PRINT "Noventa"
DO
b$ = INKEY$
IF b$ = "1" THEN GOTO um
IF b$ = "2" THEN GOTO dois
IF b$ = "3" THEN GOTO tres
IF b$ = "4" THEN GOTO quatro
IF b$ = "5" THEN GOTO cinco
IF b$ = "6" THEN GOTO seis
IF b$ = "7" THEN GOTO sete
IF b$ = "8" THEN GOTO oito
IF b$ = "9" THEN GOTO nove
IF b$ = "0" THEN GOTO Xaco
IF b$ = "" THEN  ELSE
LOOP

onze2:
COLOR 15, 6: LOCATE 20, 42: PRINT "1"
COLOR 15, 0
LOCATE 22, 25: PRINT "Onze."
DO
x$ = INKEY$
IF x$ = " " THEN GOTO Begin
IF x$ = CHR$(27) THEN END
LOOP

