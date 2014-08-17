DEFINT A-Z
DECLARE FUNCTION EDInput$ (Ren%, Col%, Max%, Var$)
DIM SHARED DirCursor AS INTEGER

Example:    '** Can delete **
CLS '** Can delete **
LOCATE 10, 10: PRINT "Your name (15) ?"
LOCATE 12, 10: PRINT "Where age (2)  ?"
LOCATE 14, 10: PRINT "Your phone (15)?"

LOCATE 8, 1: PRINT "Try typing more chars than Max% in function and see the result" '** Can delete **
LOCATE 21, 1: PRINT "You can use Home, End, Delete, Left, Right, Up and Down arrows" '** Can delete **
LOCATE 22, 1: PRINT "after capture a String and Insert Chars or change the first, last, mid" '** Can delete **
LOCATE 23, 1: PRINT "or goto to the next variable or go to the previous and edit again." '** Can delete **

YourName:  '** Can delete **
Name$ = EDInput(10, 30, 15, Name$) '** Can delete **
IF DirCursor = 1 THEN BEEP: GOTO YourName '** Can delete **

Age: '** Can delete **
Age$ = EDInput(12, 30, 2, Age$) '** Can delete **
IF DirCursor = 1 THEN GOTO YourName '** Can delete **

YourPhone: '** Can delete **
Phone$ = EDInput(14, 30, 15, Phone$) '** Can delete **
IF DirCursor = 1 THEN GOTO Age '** Can delete **

CLS
PRINT "This is the value of the variables..."
PRINT "Name  : "; Name$ '** Can delete **
PRINT "Age   : "; Age$  '** Can delete **
PRINT "Phone : "; Phone$ '** Can delete **

END

FUNCTION EDInput$ (Ren%, Col%, Max%, Var$)

'You can add TextColor an BackColor in this part
'COLOR 15, 1
'LOCATE Ren%, Col%: PRINT STRING$(Max%, " ")

LOCATE Ren%, Col%, 1: PRINT Var$;
DirCursor = 0
InicioCadena = Col% + 1
FinCadena = Col% + (Max% - 1)
Col% = Col% + LEN(Var$)
Cadena$ = Var$

DO
Teclado$ = INKEY$

SELECT CASE Teclado$

CASE CHR$(13), CHR$(9), CHR$(0) + "P" 'Enter, Tab, Down arrow Keys
AceptarValor:
 EDInput = Cadena$ + Cadena2$
 EXIT FUNCTION
CASE CHR$(0) + "H" ' Up arrow key
 EDInput = Cadena$ + Cadena2$
 DirCursor = 1
EXIT FUNCTION
CASE CHR$(27) ' Esc key
' EDInput = CHR$(27) ' Enable this line to kept the Esc Val in variable
EXIT FUNCTION

CASE CHR$(0) + "G" ' Home key
Col% = Col% - LEN(Cadena$)
Cadena2$ = Cadena$
Cadena$ = ""
LOCATE Ren%, Col%, 1

CASE CHR$(0) + "O" ' End Key
Col% = Col% + LEN(Cadena2$)
Cadena$ = Cadena$ + Cadena2$
Cadena2$ = ""
LOCATE Ren%, Col%, 1

CASE CHR$(0) + "K" ' Left Arrow key
IF Col% >= InicioCadena THEN
 Col% = Col% - 1: LOCATE Ren%, Col%, 1
 Cadena$ = LEFT$(Cadena$, LEN(Cadena$) - 1)
 Cadena2$ = CHR$(SCREEN(Ren%, Col%)) + Cadena2$
GOSUB ImprimeCadena
END IF

CASE CHR$(0) + "M" ' Right Arrow key
IF LEN(Cadena2$) >= 1 THEN
 Cadena$ = Cadena$ + CHR$(SCREEN(Ren%, Col%))
 Cadena2$ = RIGHT$(Cadena2$, LEN(Cadena2$) - 1)
 Col% = Col% + 1
 LOCATE Ren%, Col%, 1
 GOSUB ImprimeCadena
END IF

CASE CHR$(0) + "S" ' Delete Key
IF LEN(Cadena2$) >= 1 THEN
 Cadena2$ = RIGHT$(Cadena2$, LEN(Cadena2$) - 1)
 GOSUB ImprimeCadena
END IF

CASE CHR$(8) ' BackSpace Key
IF Col% >= InicioCadena THEN
 Col% = Col% - 1
 LOCATE Ren%, Col%, 1
 Cadena$ = LEFT$(Cadena$, LEN(Cadena$) - 1)
GOSUB ImprimeCadena
END IF

'CASE " " TO "¥" ' To ingnore control chars, enable this line and ren follow
CASE CHR$(1) TO CHR$(255) ' This line allow capture control Chars()
 IF Col% > FinCadena THEN
  IF CambioAutomatico$ = "S" THEN EDInput = Cadena$ + Cadena2$: EXIT FUNCTION
  IF CambioAutomatico$ <> "S" THEN BEEP
 END IF
IF Col% <= FinCadena THEN
 IF (LEN(Cadena$) + LEN(Cadena2$)) < Max% THEN
  LOCATE Ren%, Col%, 1: PRINT Teclado$; : Col% = Col% + 1
  Cadena$ = Cadena$ + Teclado$
  GOSUB ImprimeCadena
 END IF
END IF
END SELECT
LOOP
ImprimeCadena:
 LOCATE Ren%, Col%: PRINT STRING$(Max% - LEN(Cadena$), " ")
 LOCATE Ren%, Col%, 1: PRINT Cadena2$
 LOCATE Ren%, Col%, 1
RETURN
END FUNCTION

