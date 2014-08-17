DEFINT A-Z

DECLARE SUB OFont (Text$, X, Y, Fore, Back)
DECLARE SUB OCenter (Text$, Y, Fore, Back)
DECLARE SUB PressAndClear ()

SCREEN 13

             
LINE (1, 0)-(318, 0), 4
LINE (0, 1)-(0, 198), 4
LINE (1, 199)-(318, 199), 4
LINE (319, 1)-(319, 198), 4
LINE (1, 1)-(318, 198), 41, B
LINE (2, 2)-(317, 197), 42, B
LINE (3, 3)-(316, 196), 14, B
PAINT (4, 4), 25, 14
OCenter "PHILLIP JAY COHEN'S", 10, 2, 144
OFont "OUTLINED FONT", 60, 25, 43, 114
OFont "DEMO PROGRAM", 180, 25, 32, 1
OCenter "Syntax:", 50, 15, 8
OCenter "OFont Text$, X, Y, Fore, Back", 65, 40, 4
OFont "Where:", 10, 85, 9, 33
OFont "Text$ = The text to be printed", 70, 85, 11, 3
OCenter "X = The horizontal coordinate for the", 100, 35, 5
OCenter "upper-left hand corner of the text", 110, 35, 5
OFont "Y = The vertical coordinate for the", 10, 125, 42, 41
OCenter "upper-left hand corner of the text", 135, 42, 41
OCenter "Fore = The text's foreground color", 150, 13, 37
OCenter "Back = The text's background color", 165, 44, 6
OCenter "PRESS A KEY", 185, 15, 8
PressAndClear
OCenter "Also Including", 50, 15, 8
OFont "A centering routine,", 25, 65, 40, 4
OFont "which has the", 190, 65, 11, 3
OCenter "same syntax, except for the X argument", 75, 11, 3
OCenter "These routines only work in screen", 90, 35, 5
OCenter "mode 13, but have many advantages,", 100, 35, 5
OCenter "Including:", 115, 44, 6
OCenter "Outlining (obviously), placement at", 130, 42, 41
OCenter "exact coordinates (unlike PRINT),", 140, 42, 41
OCenter "avoid erasing the background, and", 150, 42, 41
OCenter "are relatively FAST for QBasic", 160, 42, 41
OCenter "MORE -->", 173, 13, 37
PressAndClear
OCenter "Please,", 50, 15, 8
OCenter "Feel free to distribute these routines", 65, 40, 4
OCenter "Try to give me credit when you use", 80, 11, 3
OCenter "them, if you can (I don't think that's", 90, 11, 3
OCenter "asking too much, do you?)", 100, 11, 3
OCenter "Unless you have a lot of programing", 115, 9, 33
OCenter "experience, don't try to understand", 125, 9, 33
OCenter "the OFont routine; you don't need to", 135, 9, 33
OCenter "know how the routine works to use it", 145, 9, 33
OCenter "For more info, you can contact me at:", 160, 35, 5
OCenter "cohennet@erols.com", 173, 44, 6
PressAndClear
SCREEN 0
WIDTH 80
END

SUB OCenter (Text$, Y, Fore, Back)
OFont Text$, 160 - INT(LEN(Text$) * 4), Y, Fore, Back 'Call with appropriate
END SUB                                              'Starting X coordinate

SUB OFont (Text$, X, Y, Fore, Back)
DEF SEG = &HFFA6                              'Stores masks for letters
FOR Letter = 1 TO LEN(Text$)                  'Does each letter
Address = (8 * ASC(MID$(Text$, Letter))) + 14 'Address for start of letter
FOR Height = 0 TO 7                       'Each letter is an 8x8 pixel matrix
Mask = PEEK(Address + Height) * 128   'Address for mask of each line of letter

LINE (X + Curntx + 1, Y + Height + 1)-(X + 9 + Curntx, Y + Height + 1), Fore, , Mask
NEXT
Curntx = Curntx + 8                   'Advances X axis by 8 for next letter
NEXT                                  'Continue to next letter
DEF SEG = &HA000                      'Change to video memory
IF Back > 0 THEN                      'Background color can't be color 0
FOR V = Y TO Y + 7                    'Again, they're 8x8 pixels
FOR H = X TO (LEN(Text$) * 8) - 1 + X 'Calculates length of text in pixels
PK0& = PEEK(H + V * 320&)             'Is point at H,V = to foreground color?
PK1& = PEEK(H + 1 + (V + 1) * 320&)   'Is point at H+1, V+1 = to foreground?
PK2& = PEEK(H + 1 + V * 320&)         'Is point at H+1, V = to foreground?
PT& = H + V * 320&                    'Video memory pointer
IF PK0& <> Fore THEN                  'If this is foreground, don't overlap it
IF PK1& = Fore OR PK2& = Fore THEN POKE PT&, Back    'Put pixel into memory
END IF
NEXT H     'Next horizontal
NEXT V     'Next vertical
END IF
DEF SEG    'Put us back where
END SUB    'We started

SUB PressAndClear
Buffer = &H1A
DEF SEG = &H40
POKE Buffer, PEEK(Buffer + 2)    'Clear keyboard buffer
DEF SEG
DO: LOOP WHILE INKEY$ = ""       'Wait for key press
LINE (4, 45)-(315, 180), 25, BF  'Clear our working area
END SUB

