OPEN "htedit.cfg" FOR BINARY AS #1

text$ = "HTEdit"
PUT #1, , text$

wordwrap% = 0
PUT #1, , wordwrap%

sep1$ = "Ä"
PUT #1, , sep1$

sep2$ = "Í"
PUT #1, , sep2$

sep3$ = "ð"
PUT #1, , sep3$

port$ = "LPT1"
PUT #1, , port$

CLOSE #1


