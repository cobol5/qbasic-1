'SAMPLE.BAS
'QMIDI v1.0 demonstration program.
'by Jesse Dorland (jessedorland@hotmail.com)
'Released to the Public Domain, April 1997
'
'This program demonstrates the routines in QMIDI, a background MIDI file
'player for QBasic/QuickBASIC.
'
'The features demonstrated here:
'       MIDI Loading
'       MIDI Playing
'       MIDI Timing
'       Background MIDI Playing
'
'NOTE: You MUST load the two sound drivers included with QMIDI before running
'this program.  See QMIDI.DOC Section 3 for information if you don't know
'how.
'
'The final demonstration in this program requires a VGA graphics card.  The
'other demos have no special requirements.
'
'QMIDI.BAS contains the core source code for QMIDI.  Cut and paste the entire
'contents of QMIDI.BAS into your own code to use the QMIDI routines.
'
'
'Declare QMIDI subroutines.
DECLARE FUNCTION int86qb$ (intnr%, flag%, AX%, bx%, cx%, dx%, di%, si%, bp%, ds%, es%)
DECLARE FUNCTION int2str$ (sword%)
DECLARE FUNCTION PlayMIDI% ()
DECLARE FUNCTION LoadAndPlayMIDI% (Filename$)
DECLARE SUB LoadMIDI (Filename$)
DECLARE SUB StopMIDI ()
DECLARE SUB Pokestring (SEGJE%, OFFJE%, Main$)
DECLARE FUNCTION TimeMIDI! ()
'The following subroutines are not included with QMIDI.
DECLARE SUB SetPal (Col%, R%, G%, B%)
DECLARE SUB GetPal (Col%, R%, G%, B%)
DECLARE SUB Center (Text$)
'**************************************************************************
'The following code is REQUIRED in programs that use QMIDI in order for
'all features to work properly.
'**************************************************************************
DIM SHARED MIDI.LOADED AS INTEGER
DIM SHARED MIDI.PLAYTIME AS SINGLE
DIM SHARED MIDI.INTERRUPT AS INTEGER
MIDI.LOADED = 0
MIDI.PLAYTIME = 0
MIDI.INTERRUPT = &H81
'**************************************************************************
'The preceding code is REQUIRED in programs that use QMIDI in order for
'all features to work properly.
'**************************************************************************

'Set text mode
SCREEN 0: WIDTH 80, 25: CLS
'Change the background to a purpl-ish color.  It's just a change of pace from
'the usual black.
SetPal 0, 32, 0, 21
'PRINT THE MAIN SCREEN.
'Print the title in light cyan.
COLOR 11, 0
Center "QMIDI DEMO"
'Print a description of the program in bright white.
COLOR 15, 0
Center "A demonstration of the features in QMIDI v1.0"
PRINT
'Print the sound driver warning.
COLOR 12, 0
Center "NOTE: If the sound drivers have not been loaded, quit now!"
Center "Press Q to quit or any other key to begin the demo."
'Get a key press.
A$ = UCASE$(INPUT$(1))
'If the user pressed Q abort the program.
IF A$ = "Q" THEN CLS : END

'START THE LOADMIDI DEMONSTRATION
CLS
'Print the LoadMIDI syntax in yellow.
COLOR 14, 0
Center "LoadMIDI (Filename$)"
'Print the LoadMIDI description in white.
COLOR 15, 0
Center "Loads a MIDI file into memory."
'Print the loading message.
PRINT
COLOR 12, 0
Center "Loading sample MIDI file..."
'Load one of the sample MIDI files.
LoadMIDI "SAMPLE2.MID"
'Tell the user that we're done loading.
Center "Done!!"
PRINT
'Prompt the user for a key press.
Center "Press Q to quit or any other key to continue the demo."
A$ = UCASE$(INPUT$(1))
'If the user pressed Q, abort the program.
IF A$ = "Q" THEN CLS : END

'START THE PLAYMIDI DEMONSTRATION
CLS
'Print the PlayMIDI syntax in yellow.
COLOR 14, 0
Center "PlayMIDI ()"
'Print the PlayMIDI description in white.
COLOR 15, 0
Center "Plays a MIDI file currently loaded in memory."
'Print a "playing" message.
PRINT
COLOR 12, 0
Center "Playing music..."
'If the MIDI file could not be played, print an error message.
IF PlayMIDI = 0 THEN
Center "ERROR!  Could not play MIDI file.  Check to make sure all QMIDI files"
Center "are in the current directory!"
ELSE
'Otherwise, everything should be going OK.
Center "You should now be listening to music."
END IF
'Prompt the user for a key press.
PRINT
Center "Press Q to quit the demo or any other key to continue."
A$ = UCASE$(INPUT$(1))
'If the user pressed Q, abort the program.
IF A$ = "Q" THEN StopMIDI: CLS : END

'START THE STOPMIDI DEMONSTRATION
CLS
'Print the StopMIDI syntax in yellow.
COLOR 14, 0
Center "StopMIDI ()"
'Print the StopMIDI description in white.
COLOR 15, 0
Center "Stops the currently playing MIDI file."
COLOR 12, 0
'Print a "stopping..." message.
PRINT
Center "Stopping music..."
'Attempt to stop the music.
StopMIDI
'The music should no longer be playing.
Center "You shouldn't hear any more music."
'Prompt the user for a key press.
PRINT
Center "Press Q to quit the demo or any other key to continue."
A$ = UCASE$(INPUT$(1))
'If the user pressed Q, abort the program.
IF A$ = "Q" THEN CLS : END

'START THE LOADANDPLAYMIDI DEMONSTRATION
CLS
'Print the LoadAndPlayMIDI syntax in yellow.
COLOR 14, 0
Center "LoadAndPlayMIDI (Filename$)"
'Print the LoadAndPlayMIDI description in white.
COLOR 15, 0
Center "Loads a MIDI file into memory and begins playing it."
'Display a message while loading the file.
COLOR 12, 0
PRINT
Center "Loading and playing MIDI file..."
'Attempt to load one of the sample MIDI files and begin playing it.
IF LoadAndPlayMIDI%("SAMPLE3.MID") = 0 THEN
'If an error occurred, print an error message.
Center "ERROR!!  File could not be played.  Make sure all QMIDI files are"
Center "present in the current directory!"
ELSE
'Otherwise, the file is loaded and should be playing.
Center "Done!!  You should be listening to music now."
END IF
'Prompt the user for a key press.
PRINT
Center "Press Q to quit the demo or any other key to continue."
A$ = UCASE$(INPUT$(1))
'If the user pressed Q, abort the program.
IF A$ = "Q" THEN CLS : END

'START THE TIMEMIDI DEMONSTRATION
CLS
'Print the TimeMIDI syntax in yellow.
COLOR 14, 0
Center "TimeMIDI ()"
'Print the TimeMIDI description in white.
COLOR 15, 0
Center "Returns the number of seconds a MIDI file has been playing."
PRINT
'Print how long the MIDI file started in the last demo has been playing/
COLOR 13, 0
Center "MIDI file has been playing" + STR$(INT(TimeMIDI!)) + " seconds and counting!"
COLOR 12, 0
Center "(remember, this file has been playing since the last demo, so"
Center "the counter may not be at 0 when this screen appears)"
'Prompt the user for a key press.
PRINT
Center "Press Q to quit the demo or any other key to continue."
COLOR 13, 0
DO
A$ = UCASE$(INKEY$)
'If the user pressed Q, stop the music and abort the program.
IF A$ = "Q" THEN StopMIDI: CLS : END
IF A$ <> "" THEN EXIT DO
'Update the MIDI time counter.
LOCATE 4, 1
Center "MIDI file has been playing" + STR$(INT(TimeMIDI!)) + " seconds and counting!"
LOOP

'START THE BACKGROUND PLAYING DEMONSTRATION
CLS
'Print the title in yellow.
COLOR 14
Center "Background MIDI Playing"
'Print the description.
COLOR 12
PRINT
Center "The following is a demonstration of how QMIDI can play MIDI files in"
Center "the background while the program continues to run.  This last portion"
Center "of the demo requires VGA graphics."
'Prompt the user for a key press.
PRINT
Center "Press Q to quit the demo or any other key to continue."
Center "Once the next demo has started, press any key to end the program."
A$ = UCASE$(INPUT$(1))
'If the user pressed Q, abort the program.
IF A$ = "Q" THEN StopMIDI: CLS : END
'Stop the currently playing MIDI file.
StopMIDI
'Switch to 256-color VGA mode.
SCREEN 13
'Print the title and "press a key" messages.
COLOR 14
Center "QMIDI v1.0 Background Music Demo"
COLOR 15
LOCATE 25, 1
Center "Press any key to quit.;;"
'Dimension an array to hold the original palette information for colors
'16-31.
DIM PalStore%(1 TO 16, 1 TO 3)
FOR I% = 16 TO 31
'Get the palette information for color I%
GetPal I%, R%, G%, B%
'Store the palette information for color I%
PalStore%(I% - 15, 1) = R%
PalStore%(I% - 15, 2) = G%
PalStore%(I% - 15, 3) = B%
NEXT I%
'Start printing on line 10.
LOCATE 10, 1
'Set the printing color to 16
Col% = 16
FOR I% = 1 TO 6
FOR K% = 1 TO 5
'Read one line of the giant "QMIDI" text.
READ Text$
'Set the printing color.
COLOR Col%
'Print the text.
PRINT Text$;
'Decrement the Col% variable.
Col% = Col% - 1
'We only want to use 5 colors from 16-20 for the "QMIDI" text.
IF Col% < 16 THEN Col% = 20
NEXT K%
'Go to the next line
PRINT
NEXT I%
'Change the background to a purpl-ish color.  If you don't like this color,
'you can change the values.  A black background made it retina-burning to
'look at.
SetPal 0, 32, 0, 21
'Draw a border around the screen text.
LINE (3, 37)-(317, 40), 22, BF
LINE (3, 142)-(317, 145), 29, BF
'Draw a box around the large "QMIDI" text.
LINE (3, 65)-(317, 122), 25, B

'This program contains 14 "captions."  Each caption is some text that scrolls
'from left to right underneath the large "QMIDI" text.
'
'Start reading data from the Captions label.
RESTORE Captions
'Start on caption 1.
CapNo% = 1
'Read in a caption
READ Caption$, CapCol%
'Start printing the caption at the far right end of the screen.
CapX% = 40
'Set the color to the 8.
COLOR 8
'Print how long the MIDI file has been playing
LOCATE 7, 1
Center "Playing time:" + STR$(PlayTime!) + " seconds."
'Set the color to white.
COLOR 15
'Attempt to load and play a sample MIDI file.
IF LoadAndPlayMIDI%("SAMPLE1.MID") = 0 THEN
'If an error occurred, quit, because the demonstration is essentially
'pointless without a MIDI file playing.
LOCATE 1, 1
PRINT "ERROR!!  Failed to load the sample MIDI file.  Make sure all QMIDI"
PRINT "files are present in the current directory!"
PRINT
PRINT "Press a key to quit!"
A$ = INPUT$(1)
SCREEN 0: WIDTH 80, 25: CLS
END
END IF

'Set the printing color.
Col% = 17
'A key press will end the program.
DO WHILE INKEY$ = ""
'Get the number of seconds the MIDI file has been playing.
PlayTime! = INT(TimeMIDI!)
'SAMPLE1.MID runs for approximately 90 seconds.  If more than 90 seconds
'has elapsed, the file needs to be restarted.
IF PlayTime! > 90 THEN
        StopMIDI
        'Since it played correctly the first time, we can assume that it will
        'start without error subsequent times.
        A% = PlayMIDI
END IF
'Wait for vertical retrace 6 times.  This makes the palette rotate at a
'good speed, and reduces screen flicker.
FOR I% = 1 TO 6: WAIT &H3DA, 8, 8: WAIT &H3DA, 8: NEXT I%
'Clear line 7.
LOCATE 7, 22: PRINT STRING$(11, 32);
'Print the number of seconds the MIDI file has been playing.
COLOR 8
LOCATE 7, 22
PRINT LTRIM$(STR$(PlayTime!)) + " seconds.";
COLOR CapCol%
'If the beginning of the text has gone off the left side of the screen...
IF CapX% <= 0 THEN
'Locate the cursor at the beginning of the line
LOCATE 17, 1
ELSE
'Otherwise, locate the cursor at the beginning of the text.
LOCATE 17, CapX%
END IF
'If the end of the caption has gone off the right side of the screen...
IF 40 - CapX% < LEN(Caption$) THEN
'Clip the end of the string so it will fit on one line.
CapText$ = LEFT$(Caption$, 40 - CapX%)
ELSE
'Otherwise, make no changes to the string.
CapText$ = Caption$
END IF
'If the beginning of the caption has gone off the left side of the screen...
IF CapX% <= 0 THEN
'Clip the beginning of the string so it will fit.
CapText$ = RIGHT$(CapText$, CapX% - 1 + LEN(CapText$))
END IF
'Print the caption.
PRINT CapText$;
'Decrement the column position of the caption text, so it will scroll one
'column to the left.
CapX% = CapX% - 1
'If the entire caption has scrolled off the left side of the screen...
IF CapX% <= -LEN(Caption$) THEN
'Reset the caption column position to the far right side.
CapX% = 40
'Switch to the next caption.
CapNo% = CapNo% + 1
'Make sure that we haven't gone past the last caption - if we have, loop
'back to the beginning.
IF CapNo% > 14 THEN CapNo% = 1
'Start reading data from the Captions label.
RESTORE Captions
'Read data until the desired caption is reached.
FOR I% = 1 TO CapNo%
'Read in the next caption and caption color.
READ Caption$, CapCol%
NEXT I%
END IF
'When the caption color changes, the large "QMIDI" text changes to similar
'colors.  The following code sets which colors should be used for the "QMIDI"
'text.
SELECT CASE CapCol%
        'If light green was used for the caption text, use green shades for
        'the "QMIDI" text.
        CASE IS = 10: NoRed% = 1: NoGreen% = 0: NoBlue% = 1
        'If light cyan was used for the caption text, use cyan shades
        '(green and blue mixed create Cyan)
        CASE IS = 11: NoRed% = 1: NoGreen% = 0: NoBlue% = 0
        'If light red was used for the caption text, use red shades
        'for the "QMIDI" text.
        CASE IS = 12: NoRed% = 0: NoGreen% = 1: NoBlue% = 1
        'If light magenta was used for the caption text, use purple shades
        '(red and blue mixed create Purple)
        CASE IS = 13: NoRed% = 0: NoGreen% = 1: NoBlue% = 0
        'If yellow was used for the caption text, use yellow shades
        '(red and green mixed create Yellow)
        CASE IS = 14: NoRed% = 0: NoGreen% = 0: NoBlue% = 1
        'If white was used for the caption text, use neutral colors
        '(red, green, and blue mixed create shades of Gray)
        CASE IS = 15: NoRed% = 0: NoGreen% = 0: NoBlue% = 0
END SELECT
'Rotate the palette of colors 16-31
FOR I% = 16 TO 31
'Get the Red value of color Col%.
R% = PalStore%(Col% - 15, 1)
'If red shouldn't be shown, clear the R% variable.
IF NoRed% THEN R% = 0
'Do the same for the Green value...
G% = PalStore%(Col% - 15, 2)
IF NoGreen% THEN G% = 0
'and again for the Blue value.
B% = PalStore%(Col% - 15, 3)
IF NoBlue% THEN B% = 0
'Set the palette of color I%
SetPal I%, R%, G%, B%
'Decrement Col% to get new palette values
Col% = Col% - 1
'Make sure the Col% variable does not go outside the range of 16 and 30.
IF Col% < 16 THEN Col% = 30
NEXT I%
'Set the color of the MIDI counter to the same value as the caption color.
'Since part of the MIDI counter is not reprinted each time in order to
'reduce flicker, the palette must be changed if the color is to be changed.

'Get the color values of the caption color and the color used for the
'MIDI counter.
GetPal CapCol%, R%, G%, B%
GetPal 8, R2%, G2%, B2%
'If they are not the same, execute some code that will provide a smooth
'transition from one color to the other.
IF R% <> R2% OR G% <> G2% OR B% <> B2% THEN
'Make the color values of the counter color become a little more similar
'to those of the caption color.
IF R2% < R% THEN R2% = R2% + 4
IF R2% > R% THEN R2% = R2% - 4
IF G2% < G% THEN G2% = G2% + 4
IF G2% > G% THEN G2% = G2% - 4
IF B2% < B% THEN B2% = B2% + 4
IF B2% > B% THEN B2% = B2% - 4
SetPal 8, R2%, G2%, B2%
END IF
LOOP
'When a key is pressed, stop the music...
StopMIDI
'Restore text mode....
SCREEN 0: WIDTH 80, 25: CLS
'And quit.
END

'The following is data for the large "QMIDI" text that appears in the final
'demonstration.
DATA " ллллллллл"," ллл   ллл"," лллл"," лллллл  "," лллл"
DATA " лл     лл"," лл л л лл","  лл "," лл    л ","  лл "
DATA " лл     лл"," лл  л  лл","  лл "," лл     л","  лл "
DATA " лл  л  лл"," лл     лл","  лл "," лл    л ","  лл "
DATA " ллллллллл"," лл     лл"," лллл"," лллллл  "," лллл"
DATA "     л    ","          ","     ","         ","     "


'The following is data for the "captions" that scroll by underneath the large
'"QMIDI" text in the final demonstration.
Captions:
DATA "Playing in the Background!! ",11
DATA "'Impossible!' you say? ",10
DATA "Not with QMIDI!! ",12
DATA "With QMIDI you can: ",13
DATA "* Play almost any type of MIDI file! ",14
DATA "* Play MIDI files of nearly any size - the only limit is the amount of free base memory you have! ",15
DATA "* Play MIDI files totally in the background, with little or no slowdown! ",10
DATA "QMIDI works with: ",14
DATA "QBasic v1.x               QuickBASIC v4.x               Visual BASIC for DOS       and       MS Professional Development System! ",15
DATA "Thanks to Rick Elbers for his QBasic interrupt routines and Mike Huff for the QuickBASIC MIDI code that I ported to QBasic. ",14
DATA "If you'd like to suggest an enhancement for a future version of QMIDI, or if you have any questions or comments, please feel free to e-mail me at: ",12
DATA "jessedorland@hotmail.com ",10
DATA "I hope you like QMIDI!!  Don't forget to tell me what you think of it!  So long! ",15
DATA "WARNING!!!  This program will self-destruct in 5 seconds... actually, it just loops back to the beginning. =) ",12

SUB Center (Text$)
'As far as we know, a carriage return should be printed.
NoCR% = 0
'Get the number of screen columns.
DEF SEG = &H40
MaxCol% = PEEK(&H4A) \ 2
DEF SEG
'If the last two characters in the string are semi colons, than a carriage
'return should not be printed.
IF RIGHT$(Text$, 2) = ";;" THEN NoCR% = 1: Text$ = LEFT$(Text$, LEN(Text$) - 2)

'Locate the cursor on the appropriate place
LOCATE CSRLIN, MaxCol% - LEN(Text$) \ 2, 0
'Print the text.
IF NoCR% THEN
        PRINT Text$;
ELSE
        PRINT Text$
END IF
END SUB

SUB GetPal (Col%, R%, G%, B%)
'Send the color to port 3C7H
OUT &H3C7, Col%
'Retrieve the Red, Green, and Blue color values.
R% = INP(&H3C9)
G% = INP(&H3C9)
B% = INP(&H3C9)
END SUB

FUNCTION int2str$ (sword%)
'This function is translating SWORD Integers into a string. Its only use
'is when you still use asm$ for assembler functions( like I% do). In that
'case you can make your integer values usable ..
'THis function simply translates the hexa bytes
'into stringbytes as is.
'----------------------------------------------------
DEF SEG = VARSEG(sword%)
ptr% = VARPTR(sword%)
int2str$ = CHR$(PEEK(ptr%)) + CHR$(PEEK(ptr% + 1))
DEF SEG

END FUNCTION

FUNCTION int86qb$ (intnr%, flag%, AX%, bx%, cx%, dx%, di%, si%, bp%, ds%, es%)
'------------------------------------------------------------------------
'Conversion of all integers in strings
flag$ = LEFT$(int2str$(flag%), 1): AX$ = int2str$(AX%):
bx$ = int2str$(bx%): cx$ = int2str$(cx%): dx$ = int2str$(dx%):
di$ = int2str$(di%): si$ = int2str$(si%): bp$ = int2str$(bp%):
ds$ = int2str$(ds%): es$ = int2str$(es%):
'Now all integersvalues are direct usable in asm$
'------------------------------------------------------------------------
DIM datas%(11)    'that is all we need
dataseg% = VARSEG(datas%(0)): flagoff% = VARPTR(datas%(0))
dataseg$ = int2str$(dataseg%): flagoff$ = int2str$(flagoff%)
axoff$ = int2str$(flagoff% + 2): bxoff$ = int2str$(flagoff% + 4)
cxoff$ = int2str$(flagoff% + 6): dxoff$ = int2str$(flagoff% + 8)
dioff$ = int2str$(flagoff% + 10): sioff$ = int2str$(flagoff% + 12)
bpoff$ = int2str$(flagoff% + 14): dsoff$ = int2str$(flagoff% + 16)
esoff$ = int2str$(flagoff% + 18): intnroff$ = int2str$(flagoff% + 20)
'Now all integers are directly adressable in memory through asm$
'We could have used only this as pointers and leave the immediate values
'but....we don't.
'------------------------------------------------------------------------
asm$ = ""
asm$ = asm$ + CHR$(&H9C)                       'pushf
asm$ = asm$ + CHR$(&H50)                       'push ax
asm$ = asm$ + CHR$(&H53)                       'push bx
asm$ = asm$ + CHR$(&H51)                       'push cx
asm$ = asm$ + CHR$(&H52)                       'push dx
asm$ = asm$ + CHR$(&H57)                       'push di
asm$ = asm$ + CHR$(&H56)                       'push si
asm$ = asm$ + CHR$(&H55)                       'push bp
asm$ = asm$ + CHR$(&H1E)                       'push ds
asm$ = asm$ + CHR$(&H6)                        'push es

'2)LOAD REGISTERS
'First the flags( questionable if this ever happens):
asm$ = asm$ + CHR$(&HB4) + flag$              'mov ax,flag$
asm$ = asm$ + CHR$(&H9E)                      'sahf stores ah into flags

'Segment registers mbv ax :  only when not (accidentally) 0
IF ds% <> 0 THEN                              'safety
  asm$ = asm$ + CHR$(&HB8) + ds$              'mov ax,ds$
  asm$ = asm$ + CHR$(&H8E) + CHR$(&HD8)       'mov ds,ax
END IF
IF es% <> 0 THEN                              'safety
  asm$ = asm$ + CHR$(&HB8) + es$              'mov ax,es$
  asm$ = asm$ + CHR$(&H8E) + CHR$(&HC0)       'mov es,ax
END IF
'the rest of registers :
asm$ = asm$ + CHR$(&HB8) + AX$                'mov ax,ax$
asm$ = asm$ + CHR$(&HBB) + bx$                'mov bx,bx$
asm$ = asm$ + CHR$(&HB9) + cx$                'mov cx,cx$
asm$ = asm$ + CHR$(&HBA) + dx$                'mov dx,dx$
asm$ = asm$ + CHR$(&HBF) + di$                'mov di,di$
asm$ = asm$ + CHR$(&HBE) + si$                'mov si,si$
asm$ = asm$ + CHR$(&HBD) + bp$                'mov bp,bp$
'oke now the interrupt himself
asm$ = asm$ + CHR$(&HCD) + CHR$(intnr%)       'interrupt nr

'First make our DATAS% adressable:
asm$ = asm$ + CHR$(&H1E)                         'push ds
asm$ = asm$ + CHR$(&HB8) + dataseg$              'mov ax,DATASseg$
asm$ = asm$ + CHR$(&H8E) + CHR$(&HD8)            'mov ds,ax
                                        
'We first pushed DS segment register since we want to adress our DATA_segment
asm$ = asm$ + CHR$(&H8F) + CHR$(&H6) + dsoff$    'pop dsoff$
asm$ = asm$ + CHR$(&H8C) + CHR$(&H6) + esoff$    'mov esoff$,ES

asm$ = asm$ + CHR$(&H9F)                         'lahf
asm$ = asm$ + CHR$(&H88) + CHR$(&H26) + flagoff$ 'mov flagoff$,ah
                  
'The rest is more simple I% think:
asm$ = asm$ + CHR$(&H89) + CHR$(&H2E) + bpoff$   'mov bpoff$,bp
asm$ = asm$ + CHR$(&H89) + CHR$(&H36) + sioff$   'mov sioff$,si
asm$ = asm$ + CHR$(&H89) + CHR$(&H3E) + dioff$   'mov dioff$,di
asm$ = asm$ + CHR$(&H89) + CHR$(&H16) + dxoff$   'mov dxoff$,dx
asm$ = asm$ + CHR$(&H89) + CHR$(&HE) + cxoff$    'mov cxoff$,cx
asm$ = asm$ + CHR$(&H89) + CHR$(&H1E) + bxoff$   'mov bxoff$,bx
asm$ = asm$ + CHR$(&HA3) + axoff$                'mov axoff$,ax
                             
'Now let us neatly finish by just popping everything back in order
asm$ = asm$ + CHR$(&H7)                          'pop es
asm$ = asm$ + CHR$(&H1F)                         'pop ds
asm$ = asm$ + CHR$(&H5D)                         'pop bp
asm$ = asm$ + CHR$(&H5E)                         'pop si
asm$ = asm$ + CHR$(&H5F)                         'pop di
asm$ = asm$ + CHR$(&H5A)                         'pop dx
asm$ = asm$ + CHR$(&H59)                         'pop cx
asm$ = asm$ + CHR$(&H5B)                         'pop bx
asm$ = asm$ + CHR$(&H58)                         'pop ax
asm$ = asm$ + CHR$(&H9D)                         'popf
'We are done?
asm$ = asm$ + CHR$(&HCB)                         'retf

DEF SEG = VARSEG(asm$)
offcode% = SADD(asm$): CALL absolute(offcode%): DEF SEG

ui$ = HEX$(intnr%)
FOR I% = 0 TO 9
  hx$ = HEX$(datas%(I%))
  hx$ = STRING$(4 - LEN(hx$), "0") + hx$
  ui$ = ui$ + "," + hx$
NEXT

int86qb$ = ui$

END FUNCTION

'LoadAndPlayMIDI - Loads a MIDI file into memory and plays it
FUNCTION LoadAndPlayMIDI% (Filename$)
'See if an extension was supplied, and if not, add one.
IF INSTR(Filename$, ".") = 0 THEN Filename$ = Filename$ + ".MID"
'The sound driver needs an ASCIIZ string (a string that ends with
'character 0) for a file name.
IF RIGHT$(Filename$, 1) <> CHR$(0) THEN Filename$ = Filename$ + CHR$(0)
'If MIDI.INTERRUPT is not defined, set it to 81H
IF MIDI.INTERRUPT = 0 THEN MIDI.INTERRUPT = &H81
'Initialize the MIDI driver and load the file in memory
A$ = int86qb$(MIDI.INTERRUPT, 0, SADD(Filename$), &H500, 0, VARSEG(Filename$), 0, 0, 0, 0, 0)
MIDI.LOADED = -1
'Start the music!!
A$ = int86qb$(MIDI.INTERRUPT, 0, 0, &H501, 0, 0, 0, 0, 0, 0, 0)
AX$ = MID$(A$, 9, 4)
AX% = VAL(AX$)
'If the music was started successfully...
IF NOT AX% THEN
       'Set the LoadAndPlayMIDI% variable
       LoadAndPlayMIDI% = -1
       'And set the MIDI.PLAYTIME variable so that TimeMIDI! will be
       'accurate
       MIDI.PLAYTIME = TIMER
END IF

END FUNCTION

'LoadMIDI - loads a MIDI file into memory
SUB LoadMIDI (Filename$)
'See if an extension was supplied, and if not, add one.
IF INSTR(Filename$, ".") = 0 THEN Filename$ = Filename$ + ".MID"
'The sound driver needs an ASCIIZ string (a string that ends with
'character 0) for a file name.
IF RIGHT$(Filename$, 1) <> CHR$(0) THEN Filename$ = Filename$ + CHR$(0)
'If MIDI.INTERRUPT is not defined, set it to 81H
IF MIDI.INTERRUPT = 0 THEN MIDI.INTERRUPT = &H81
'Initialize the MIDI driver and load the file in memory
A$ = int86qb$(MIDI.INTERRUPT, 0, SADD(Filename$), &H500, 0, VARSEG(Filename$), 0, 0, 0, 0, 0)
'Assume that the file is loaded
MIDI.LOADED = -1
END SUB

'PlayMIDI - Begins playing a MIDI file in the background.
FUNCTION PlayMIDI%
'If no MIDI file is loaded, exit now
IF MIDI.LOADED = 0 THEN EXIT FUNCTION
'If MIDI.INTERRUPT is not defined, set it to 81H
IF MIDI.INTERRUPT = 0 THEN MIDI.INTERRUPT = &H81
'Start the music!!
A$ = int86qb$(MIDI.INTERRUPT, 0, 0, &H501, 0, 0, 0, 0, 0, 0, 0)
AX$ = MID$(A$, 9, 4)
AX% = VAL(AX$)
'If the music was started successfully....
IF NOT AX% THEN
       'Set the PlayMIDI% variable
       PlayMIDI% = -1
       'and set the MIDI.PLAYTIME variable so that TimeMIDI! will be
       'accurate.
       MIDI.PLAYTIME = TIMER
END IF
END FUNCTION

SUB Pokestring (SEGJE%, OFFJE%, Main$)
'------------------------------------------------------
'This function pokes a string (might be ASCIIZ)into
'memory at a given location, making it possible to
'access strings in byte form
'------------------------------------------------------

DEF SEG = SEGJE%
FOR I% = 0 TO LEN(Main$) - 1
  POKE OFFJE% + I%, ASC(MID$(Main$, I% + 1, 1))
NEXT
DEF SEG

END SUB

SUB SetPal (Col%, R%, G%, B%)
'Send the color to port 3C8H
OUT &H3C8, Col%
'Set the red, green, and blue color values.
OUT &H3C9, R%
OUT &H3C9, G%
OUT &H3C9, B%
END SUB

'StopMIDI - Stops playing MIDI file
SUB StopMIDI
'If MIDI.INTERRUPT is not defined, set it to 81H
IF MIDI.INTERRUPT = 0 THEN MIDI.INTERRUPT = &H81
'Stop the music!!
A$ = int86qb$(MIDI.INTERRUPT, 0, 0, &H502, 0, 0, 0, 0, 0, 0, 0)
'No MIDI file is playing, so reset the timer
MIDI.PLAYTIME = 0
END SUB

FUNCTION TimeMIDI!
'If a MIDI file is playing, carry out the timing routine
IF MIDI.PLAYTIME THEN
'Get the current time
CurrentTime! = TIMER
'If midnight has come since the MIDI file started playing, change
'CurrentTime! accordingly
IF CurrentTime! - MIDI.PLAYTIME < 0 THEN
CurrentTime! = 86400 + CurrentTime!
END IF
'Get the final result
TimeMIDI! = CurrentTime! - MIDI.PLAYTIME
END IF
END FUNCTION

