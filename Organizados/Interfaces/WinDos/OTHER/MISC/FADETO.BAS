DECLARE FUNCTION FadeToColour% (BYVAL Attrib%, BYVAL r%, BYVAL g%, BYVAL b%)

'Returns:
'          -1 (TRUE)  if fading is complete
'           0 (FALSE) if fading is in process
'
FUNCTION FadeToColour% (BYVAL Attrib%, BYVAL r%, BYVAL g%, BYVAL b%)


'***********************************
'Get old palette.
'***********************************

OUT &H3C7, Attrib%
OldR% = INP(&H3C9)
OldG% = INP(&H3C9)
OldB% = INP(&H3C9)


'***********************************
'Have we finished fading?
'***********************************

  IF r% = OldR% THEN
    IF g% = OldG% THEN
      IF b% = OldB% THEN
      FadeToColour% = -1
      EXIT FUNCTION
      END IF
    END IF
  END IF


'***********************************
'Adjust palette.
'***********************************

  IF r% <> OldR% THEN StepR% = (r% < OldR%) * 2 + 1
  IF g% <> OldG% THEN StepG% = (g% < OldG%) * 2 + 1
  IF b% <> OldB% THEN StepB% = (b% < OldB%) * 2 + 1


'***********************************
'Enable new palette.
'***********************************

OUT &H3C8, Attrib%
OUT &H3C9, OldR% + StepR%
OUT &H3C9, OldG% + StepG%
OUT &H3C9, OldB% + StepB%

END FUNCTION

