DECLARE FUNCTION xRelLoadBMP% (BYVAL Layer%, BYVAL x%, BYVAL y%, BYVAL FileSeg%, BYVAL FileOff%, BYVAL PalSeg%, BYVAL PalOff%, BYVAL SwitchPal%)
DECLARE FUNCTION RelLoadBMP% (Layer%, x%, y%, File$, Pal$, SwitchPal%)
DECLARE FUNCTION xRelLoadPCX% (BYVAL Layer%, BYVAL x%, BYVAL y%, BYVAL FileSeg%, BYVAL FileOff%, BYVAL PalSeg%, BYVAL PalOff%, BYVAL SwitchPal%)
DECLARE FUNCTION RelLoadPCX% (Layer%, x%, y%, File$, Pal$, SwitchPal%)
DECLARE SUB xRelFindFile (BYVAL MaskSeg%, BYVAL MaskOff%, BYVAL FileSeg%, BYVAL FileOff%, BYVAL Attribute%)
DECLARE FUNCTION RelFindFile$ (Mask$, Attribute%)
DECLARE SUB xRelMode7Map (BYVAL Layer%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL sx%, BYVAL sy%, BYVAL sz%, BYVAL Hz%, BYVAL c%, BYVAL sa%, BYVAL cx&, BYVAL cy&, BYVAL TSeg%, BYVAL TOff%, BYVAL Mseg%, BYVAL MOff%, BYVAL xM%, BYVAL yM%)
DECLARE SUB RelMode7Map (Layer%, x1%, y1%, x2%, y2%, Angle%, scalex%, scaley%, scalez%, Horz%, camx!, camy!, Texture%(), TextureOffset%, Map%())
DECLARE SUB xRelMode7 (BYVAL Layer%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL scalex%, BYVAL scaley%, BYVAL scalez%, BYVAL Horz%, BYVAL cosa%, BYVAL sina%, BYVAL camx&, BYVAL camy&, BYVAL TextSeg%, BYVAL TextOff%)
DECLARE SUB RelMode7 (Layer%, x1%, y1%, x2%, y2%, Angle%, scalex%, scaley%, scalez%, Horz%, camx!, camy!, Texture%(), TextureOffset%)
DECLARE SUB xRelPsetWu (BYVAL Layer%, BYVAL x&, BYVAL y&, BYVAL clr%)
DECLARE SUB RelPsetWu (Layer%, x!, y!, clr%)

FUNCTION RelFindFile$ (Mask$, Attribute%)
  Fil$ = SPACE$(12)
  Mask$ = Mask$ + CHR$(0)
  xRelFindFile VARSEG(Mask$), SADD(Mask$), VARSEG(Fil$), SADD(Fil$), Attribute%
  Fil$ = LTRIM$(RTRIM$(Fil$))
  IF Fil$ = CHR$(0) THEN Fil$ = ""
  RelFindFile$ = Fil$
END FUNCTION

DEFINT A-Z
FUNCTION RelLoadBMP% (Layer%, x%, y%, File$, Pal$, SwitchPal%)
   File$ = File$ + CHR$(0)
   RelLoadBMP% = xRelLoadBMP%(Layer%, x%, y%, VARSEG(File$), SADD(File$), VARSEG(Pal$), SADD(Pal$), BYVAL SwitchPal%)
END FUNCTION

FUNCTION RelLoadPCX% (Layer%, x%, y%, File$, Pal$, SwitchPal%)
   File$ = File$ + CHR$(0)
   RelLoadPCX% = xRelLoadPCX%(Layer%, x%, y%, VARSEG(File$), SADD(File$), VARSEG(Pal$), SADD(Pal$), BYVAL SwitchPal%)
END FUNCTION

DEFSNG A-Z
SUB RelMode7 (Layer%, x1%, y1%, x2%, y2%, Angle%, scalex%, scaley%, scalez%, Horz%, camx!, camy!, Texture%(), TextureOffset%)

'xRelMode7   Proc  Uses es di ds si,\
'            Layer:word,scalex:word, scaley:word, scalez:word, horz:word,\
'            cosa: word, sina: word, camx: word, camy: word, TextSeg: word, TextOff: word
A! = (Angle% * 3.141593) / 180
cosa% = COS(A!) * 256
sina% = SIN(A!) * 256
camx& = camx! * 65536
camy& = camy! * 65536
IF Horz% < 1 THEN Horz% = 1

xRelMode7 Layer%, x1%, y1%, x2%, y2%, scalex%, scaley%, scalez%, Horz%, cosa%, sina%, camx&, camy&, VARSEG(Texture%(TextureOffset%)), VARPTR(Texture%(TextureOffset%))

END SUB

SUB RelMode7Map (Layer%, x1%, y1%, x2%, y2%, Angle%, scalex%, scaley%, scalez%, Horz%, camx!, camy!, Texture%(), TextureOffset%, Map%())
'xRelMode7map   Proc  Uses es di ds si,\
'                Layer:word, x1:word, y1:word, x2:word, y2:word, scalex:word, scaley:word, scalez:word, horz:word,\
'                cosa: word, sina: word, camx: Dword, camy: Dword, TextSeg: word, TextOff: word, Mapseg:Word, MapOff: word, xMapsize:word,yMapsize: word

A! = (Angle% * 3.141593) / 180
cosa% = COS(A!) * 256
sina% = SIN(A!) * 256
camx& = camx! * 65536
camy& = camy! * 65536
Mx% = UBOUND(Map%, 1) + 1
My% = UBOUND(Map%, 2) + 1

IF Horz% < 1 THEN Horz% = 1
xRelMode7Map Layer%, x1%, y1%, x2%, y2%, scalex%, scaley%, scalez%, Horz%, cosa%, sina%, camx&, camy&, VARSEG(Texture%(TextureOffset%)), VARPTR(Texture%(TextureOffset%)), VARSEG(Map%(0, 0)), VARPTR(Map%(0, 0)), Mx%, My%

END SUB

SUB RelPsetWu (Layer%, x!, y!, clr%)
x& = x! * 256
y& = y! * 256
xRelPsetWu Layer%, x&, y&, clr%
END SUB

