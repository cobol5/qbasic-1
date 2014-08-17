'$INCLUDE: 'Rellib.bi'
DEFINT A-Z

DECLARE SUB RelMosaic (DestSeg%, x1%, y1%, x2%, y2%, size%)

SUB RelMosaic (DestSeg%, x1%, y1%, x2%, y2%, size%)

'Original Coding for "RelMosaic" by Richard Eric M. Lope.
'SUCCESSFULLY converted to and optimized for Rellib by Adigun Azikiwe Polack.
'                          ~~~           

IF size% <= 1 THEN EXIT SUB
IF size% >= 256 THEN size% = 256

FOR Xsize% = x1% TO x2% STEP size%
  FOR Ysize% = y1% TO y2% STEP size%
    P = RelPoint(DestSeg%, Xsize%, Ysize%)
    RelBoxF DestSeg%, Xsize% - 1, Ysize% - 1, Xsize% + size% - 1, Ysize% + size% - 1, P
  NEXT Ysize%
NEXT Xsize%

END SUB

