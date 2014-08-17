DECLARE SUB RelBorder (Destseg%, X%, Y%, Col%)
DECLARE SUB RelBorderH (Destseg%, X%, Col%)
DECLARE SUB RelBorderV (Destseg%, Y%, Col%)
DECLARE SUB RelBorderTrans (Destseg%, X%, Y%, Col%)
DECLARE SUB RelBorderTransH (Destseg%, X%, Col%)
DECLARE SUB RelBorderTransV (Destseg%, Y%, Col%)
'$INCLUDE: 'Rellib.bi'

DEFSNG A-Z
SUB RelBorder (Destseg%, X%, Y%, Col%) STATIC

RelBoxF Destseg%, 0, 0, X% - 1, 199, Col%
RelBoxF Destseg%, 320 - X%, 0, 319, 199, Col%
RelBoxF Destseg%, 0, -1, 319, (Y% - 1), Col%
RelBoxF Destseg%, 0, 200, 319, 200 - Y%, Col%

END SUB

DEFINT A-Z
SUB RelBorderH (Destseg%, X%, Col%) STATIC

RelBoxF Destseg%, 0, 0, X% - 1, 199, Col%
RelBoxF Destseg%, 320 - X%, 0, 319, 199, Col%

END SUB

SUB RelBorderTrans (Destseg%, X%, Y%, Col%) STATIC

RelBoxTransF Destseg%, 0, 0, X% - 1, 199, Col%
RelBoxTransF Destseg%, 320 - X%, 0, 319, 199, Col%
RelBoxTransF Destseg%, 0, -1, 319, (Y% - 1), Col%
RelBoxTransF Destseg%, 0, 200, 319, 200 - Y%, Col%

END SUB

DEFSNG A-Z
SUB RelBorderTransH (Destseg%, X%, Col%) STATIC

RelBoxTransF Destseg%, 0, 0, X% - 1, 199, Col%
RelBoxTransF Destseg%, 320 - X%, 0, 319, 199, Col%

END SUB

DEFINT A-Z
SUB RelBorderTransV (Destseg%, Y%, Col%) STATIC

RelBoxTransF Destseg%, 0, -1, 319, (Y% - 1), Col%
RelBoxTransF Destseg%, 0, 200, 319, 200 - Y%, Col%

END SUB

SUB RelBorderV (Destseg%, Y%, Col%) STATIC

RelBoxF Destseg%, 0, -1, 319, (Y% - 1), Col%
RelBoxF Destseg%, 0, 200, 319, 200 - Y%, Col%

END SUB

