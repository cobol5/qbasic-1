COLOR 15, 1
CLS
PRINT "Pense em um n£mero"
BEEP
r$ = INPUT$(1)
PRINT "Multiplique por 4"
BEEP
r$ = INPUT$(1)
PRINT "Divida por 2"
BEEP
r$ = INPUT$(1)
PRINT "Subtraia 3"
BEEP
r$ = INPUT$(1)
PRINT "Multiplique por 6"
BEEP
r$ = INPUT$(1)
PRINT "Adicione 7"
BEEP
r$ = INPUT$(1)
PRINT "Divida por 2"
BEEP
r$ = INPUT$(1)
INPUT "Entre com o resultado: ", result
numero = ((((((result * 2) - 7) + 3) / 6) * 2) / 4)
PRINT "O n£mero que vc pensou foi: "; numero

