REM $INCLUDE: 'RelLib.BI'
DEFINT A-Z

'Special Thanks to Earl L. Montgomery for some of the "Out" commands that
'really helped me out in these group of 256-color low-res graphic resolution
'commands!!!  ;D  ;) !!

DECLARE SUB RelInit320x133 ()
DECLARE SUB RelInit320x100 ()
DECLARE SUB RelInit320x80 ()
DECLARE SUB RelInit320x67 ()
DECLARE SUB RelInit320x57 ()

'Created for RelLib by Adigun Azikiwe Polack.

SUB RelInit320x100
RelInitVGA13
OUT &H3D4, 9: OUT &H3D5, 3

END SUB

SUB RelInit320x133
RelInitVGA13
OUT &H3D4, 9: OUT &H3D5, 2

END SUB

SUB RelInit320x57
RelInitVGA13
OUT &H3D4, 9: OUT &H3D5, 6

END SUB

SUB RelInit320x67
RelInitVGA13
OUT &H3D4, 9: OUT &H3D5, 5

END SUB

SUB RelInit320x80
RelInitVGA13
OUT &H3D4, 9: OUT &H3D5, 4

END SUB

