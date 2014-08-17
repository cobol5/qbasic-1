
  DO
  KeyPress$ = INKEY$

    SELECT CASE LEN(KeyPress$)
      CASE 2
        PRINT ASC(LEFT$(KeyPress$, 1)), ASC(RIGHT$(KeyPress$, 1))
      CASE 1
        PRINT ASC(KeyPress$)
    END SELECT

  LOOP

