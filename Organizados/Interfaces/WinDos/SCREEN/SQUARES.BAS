'Copyright (C) HiTech Laboratories, Denis Andrianov
'Acid Rain ScreenSaver
'Compiles with BASM 6.5
'============================================================================


'************************************
'Generate a COM file.
'************************************

$COM


'************************************
'Switch to video mode.
'************************************

$ASM
mov    ax,0013h
int    10h
$ASM


'************************************
'Reset the palette.
'************************************

  FOR i = 0 TO 255
  SetPal i, 0, 0, 0
  NEXT i


'************************************
'Plot an image.
'************************************

Set = RANDOM(5)


   FOR y = 0 TO 199

     IF y = 0 THEN
     DEF SEG = &HA000
     Off = 0
     END IF

     IF y = 100 THEN
     DEF SEG = &HA7D0
     Off = 0
     END IF


     FOR x = 0 TO 319
     Off = Off + 1

       IF Set = 1 THEN
       Colour = y XOR x
       END IF

       IF Set = 2 THEN
       Temp = x - y
       Temp = Temp XOR x
       Colour = y OR Temp
       END IF

       IF Set = 3 THEN
       Temp = x - y
       Temp = Temp XOR x
       Colour = y XOR Temp
       END IF

       IF Set = 4 THEN
       xTemp = x + y
       yTemp = NOT (y)
       yTemp = yTemp + x
       Temp = xTemp XOR yTemp
       Colour = y XOR Temp
       END IF

       IF Set = 5 THEN
       xTemp = x - 160
       xTemp = xTemp * xTemp
       yTemp = y - 100
       yTemp = yTemp * yTemp
       Colour = xTemp XOR yTemp
       Colour = Colour / 40
       END IF

     POKE Off, Colour
     NEXT x


   NEXT y

DEF SEG


'************************************
'Initialize the palette.
'************************************

  FOR i = 0 TO 63
  Temp = 63 - i


    IF Set = 5 THEN

      SetPal i, 0, 0, i

      ColAttr = i + 64
      SetPal ColAttr, 0, i, 63

      ColAttr = i + 128
      SetPal ColAttr, 0, Temp, 63

      ColAttr = i + 192
      SetPal ColAttr, 0, 0, Temp

    ELSE

      SetPal i, i, 0, 0

      ColAttr = i + 64
      SetPal ColAttr, Temp, 0, 0

      ColAttr = i + 128
      SetPal ColAttr, 0, 0, i

      ColAttr = i + 192
      SetPal ColAttr, 0, 0, Temp

    END IF


  NEXT


'************************************
'Check for the existance of the mouse.
'************************************

$ASM
xor    ax,ax
int    33h
mov    MouseExists,ax
$ASM


  IF MouseExists = -1 THEN

  $ASM
  mov    ax,4                       ;Place the mouse at (160,100)
  mov    cx,160
  mov    dx,100
  int    33h
  $ASM

  OldXMouse = 160
  OldYMouse = 100

  END IF


'************************************
'Rotate the palette.
'************************************
  
  DO

  GetPal 255                   
  OldR = r                     
  OldG = g                     
  OldB = b                     
                               
    FOR i = 254 TO 0 STEP -1   
    ColAttr = i + 1            
    GetPal i                   
    SetPal ColAttr, r, g, b    
    NEXT i                     
                               
  SetPal 0, OldR, OldG, OldB   
    
    IF MouseExists = -1 THEN   
    GOSUB MouseStatus          
      IF XMouse <> OldXMouse THEN EXIT DO    
      IF YMouse <> OldYMouse THEN EXIT DO    
    END IF                                   

  Temp = INKEY
    IF Temp <> 0 THEN EXIT DO

  GOSUB SmallDelay
  GOSUB SmallDelay
  LOOP 


'************************************
'Restore text screen mode and exit.
'************************************

$ASM
mov    ax,03h
int    10h
$ASM

END


'============================================================================
'============================================================================
'============================================================================
MouseStatus:
$ASM

mov    ax,03h
int    33h
mov    XMouse,cx
mov    YMouse,dx

$ASM
RETURN

'----------------------------------------------------------------------------
SmallDelay:
$ASM

mov    dx,3dah

BitNotZero:
in     al,dx
xor    al,8
and    al,8
jz     short BitNotZero

BitZero:
in     al,dx
and    al,8
jz     short BitZero

$ASM
RETURN

'----------------------------------------------------------------------------
SUB GetPal (Attrib)
$ASM

mov    dx,3c7h
mov    ax,[bp+4]
out    dx,al

mov    dx,3c9h
xor    ah,ah

in     al,dx
mov    word ptr r,ax

in     al,dx
mov    word ptr g,ax

in     al,dx
mov    word ptr b,ax

$ASM
END SUB

'----------------------------------------------------------------------------
SUB SetPal (Attrib, r, g, b)
$ASM

mov    dx,3c8h
mov    ax,[bp+4]
out    dx,al

mov    dx,3c9h
mov    ax,[bp+6]
out    dx,al

mov    ax,[bp+8]
out    dx,al

mov    ax,[bp+10]
out    dx,al

$ASM
END SUB


