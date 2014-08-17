DECLARE SUB ShellSort (Array$())
DIM SHARED test$(10)

test$(0) = "5"
test$(1) = "32"
test$(9) = "hg"

ShellSort test$()

  FOR i% = 0 TO 10
  PRINT test$(i%)
  NEXT

DEFINT A-Z
'*    Sorts an array using the Comb sort algorithm.
SUB CombSort (Array$()) STATIC
   FirstEl = LBOUND(Array$)
   LastEl = UBOUND(Array$)
   Gap = LastEl
   DO
      IF Gap * 8 / 11 > 1 THEN
         Gap = Gap * 8 / 11
      ELSE
         Gap = 1
      END IF
      Switch = 0
      FOR i = FirstEl TO LastEl - Gap
         J = i + Gap
         IF Array$(i) > Array$(J) THEN
            SWAP Array$(i), Array$(J)
            Switch = Switch + 1
         END IF
      NEXT i
   LOOP UNTIL Switch = 0 AND Gap = 1
END SUB

'*    Sorts an array using the shell sort algorithm.
SUB ShellSort (Array$()) STATIC
   FirstEl = LBOUND(Array$)
   LastEl = UBOUND(Array$)
   Span = LastEl \ 2
   DO WHILE Span > 0
      Boundary = LastEl - Span
      DO
         Flag = 0
         FOR i = FirstEl TO Boundary
            IF Array$(i) > Array$(i + Span) THEN
               SWAP Array$(i), Array$(i + Span)
               Flag = i
            END IF
         NEXT i
         Boundary = Flag - Span
      LOOP WHILE Flag
      Span = Span \ 2
   LOOP
END SUB

