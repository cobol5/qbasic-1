'Martin Rampersad's Keyboard Status Manipulator (C)1997-Forever.
'Fool Your Friends! Or Your Enemies!!
'Just Kidding.

'This Program Locks the key you want on, or turns it off for you.
'This is handy if you do not want to type in:

'Select Case LCASE$(Key.Typed$)

'You may simply turn Caps Lock on or off.
'Use it, Abuse it, just don't take credit. - Martin Rampersad.

DEF SEG = 0
Status% = PEEK(&H417)           'Read keyboard status.
POKE &H417, (Status% XOR &H40)  'Change status. (Default Caps Lock)

'States:         
        'POKE &H417, (Status% XOR &H40) = Caps Lock
        'POKE &H417, (Status% XOR &H20) = Num Lock
        'POKE &H417, (Status% XOR &H10) = Scroll Lock
        'POKE &H417, (Status% XOR &H8) = Alt Lock (Pressing A = Alt + A)
        'POKE &H417, (Status% XOR &H4) = Ctrl Lock (Pressing A = Ctrl + A)

        'POKE &H417, (Status% XOR &H1) = Shift Lock (Pressing A = Shift + A)

        'POKE &H417, (&H0) = All Off

