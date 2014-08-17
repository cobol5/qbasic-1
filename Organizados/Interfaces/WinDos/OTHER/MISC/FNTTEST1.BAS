SCREEN 12
CLS

LINE (0, 0)-(639, 479), 2, BF
Text$ = "123456789abcdefghijklmnopqrstuvwxyz"



  FOR LetterNum% = 1 TO LEN(Text$)
  LetterOffset% = &HFA6E + ASC(MID$(Text$, LetterNum%, 1)) * 8

    FOR LineNum% = 0 TO 7

    DEF SEG = &HF000
    ScanLine% = PEEK(LetterOffset% + LineNum%)
    
    DEF SEG = &HA000
    VideoOffset% = (640 \ 8) * LineNum% + LetterNum%
    OldByte% = PEEK(VideoOffset%)
    ScanLine% = ScanLine% OR OldByte%

    'OUT &H3C4, 2
    'OUT &H3C5, 2 ^ i%
    POKE VideoOffset%, ScanLine%

    NEXT

  NEXT


DEF SEG

