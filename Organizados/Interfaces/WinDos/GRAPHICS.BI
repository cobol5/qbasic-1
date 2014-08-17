'Copyright (C) HiTech Labs, Denis Andrianov
'Graphics declarations
'============================================================================

DECLARE SUB Button (BYVAL x%, BYVAL y%, BYVAL x2%, BYVAL y2%, BYVAL TopCol%, BYVAL BotCol%, BYVAL Colour%, BYVAL LightCol%, BYVAL ButType%)
DECLARE SUB CrackLine (BYVAL Vertical%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%)

DECLARE SUB InfoBox (BYVAL BoxType%, Text$)
DECLARE FUNCTION CaptureInf% (BYVAL BoxType%)
DECLARE FUNCTION InfButPress% (BYVAL x%, BYVAL y%, BYVAL BoxType%)
DECLARE SUB InfButRelease (BYVAL ButtonNum%, BYVAL BoxType%)

DECLARE SUB InputLine (BYVAL LocateX%, BYVAL LocateY%, BYVAL Init%, BYVAL Limit%, BYVAL StartScroll%, KH$)

DECLARE SUB Icon (BYVAL x%, BYVAL y%, BYVAL IconType%)
'IconTypes:
'0-dir
'1-exe,com
'2-text
'3-archives
'4-music
'6-programming
'100-unknown

DECLARE SUB SetPal (BYVAL Colour%, BYVAL Red%, BYVAL Green%, BYVAL Blue%)

