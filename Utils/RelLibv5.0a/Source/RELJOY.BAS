DEFINT A-Z
'$INCLUDE: 'rellib.bi'

'These FAST Joystick/Gamepad Routines are created for RelLib by
'                                                     Adigun Azikiwe Polack.

'A Big Special Thanks to Phillip Jay Cohen and SJ Zero!!!  ;) !!

DECLARE SUB RelJoyDetect (Success.or.Fail)
DECLARE SUB RelJoy (JoyX%, JoyY%)
DECLARE SUB RelJoyTrig (but1%, but2%, but3%, but4%)

DEFSNG A-Z
SUB RelJoy (JoyX%, JoyY%)

' JoyX% returns vertical postion of specified joystick
' JoyY% returns horizontal postion of specified joystick

' Special Thanks to Phillip Jay Cohen for this routine!!!  ;) !

XMask% = 1
YMask% = 2
JoyX% = 1: JoyY% = 1
OUT &H201, 0 ' Reset joystick port
DO
  Joy% = INP(&H201) ' Poll joystick port
  IF Joy% AND XMask% THEN JoyX% = JoyX% + 1
  IF Joy% AND YMask% THEN JoyY% = JoyY% + 1
LOOP UNTIL (Joy% AND XMask%) = 0 AND (Joy% AND YMask%) = 0
END SUB

DEFINT A-Z
SUB RelJoyDetect (Success.or.Fail)

' This is an original QB Sub by Adigun Azikiwe Polack.
' Special Thanks to Phillip Jay Cohen!!!  ;) !

IF STICK(1) < 2 THEN Success.or.Fail = 0 ELSE Success.or.Fail = 1

END SUB

DEFSNG A-Z
SUB RelJoyTrig (but1%, but2%, but3%, but4%)

'Special Thanks to Phillip Jay Cohen for this routine!!!  ;) !

JoystK% = INP(&H201)
but1% = (JoystK% AND 16) = 0
but2% = (JoystK% AND 32) = 0
but3% = (JoystK% AND 64) = 0
but4% = (JoystK% AND 128) = 0

END SUB

