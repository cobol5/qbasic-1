'QMIDI v1.0
'QBasic/QuickBASIC background MIDI player.
'by Jesse Dorland (jessedorland@hotmail.com)
'Released to the Public Domain, April 1997
'
'See SAMPLE.BAS for a QMIDI demo.
'
'Thanks to Rick Elbers, who created the INT86QB routine.
'Also thanks to Mike Huff, who wrote some QuickBASIC MIDI code from which
'QMIDI was adapted.
'
'For all of you who ever wanted to play MIDI files in the background using
'QBasic, here's everything you need.
'
'Let me start off by clarifying that QMIDI works in *QBasic*; it doesn't
'require QuickBASIC (although it will function properly in QuickBASIC).
'One thing this program absolutely *MUST* have is a sound card with
'Sound Blaster compatibility (100% compatibility is not necessary, but
'your card must be at least mostly SB compatible).
'
'VERY IMPORTANT!!!  You really should read QMIDI.DOC before using these
'routines.  Section 3 is a must-read if you've never used QMIDI before.
'You will save yourself A LOT of potential trouble by reading the
'documentation.
'
'Use QMIDI freely - commercially, non-commercially, in a jar, in a car...
'you get the idea.  All I ask is that you tell me what you think of it and
'give me some ideas for improvements.  My e-mail address is at the top of
'the code and in the documentation.
'
'-Jesse
'
'DISCLAIMER
'----------
'If your computer suffers crashes, loss of data, or any other irregularities
'as a result of using QMIDI, I won't be responsible for it.  Reading the
'documentation should ensure safe operation, but I'm not making any
'guarantees.  You have been warned....

'int86qb$ is a function written by Rick Elbers that executes an interrupt
'using CALL ABSOLUTE
DECLARE FUNCTION int86qb$ (intnr%, flag%, AX%, bx%, cx%, dx%, di%, si%, bp%, ds%, es%)
'int2str$ is used by int86qb$
DECLARE FUNCTION int2str$ (sword%)
'PlayMIDI plays the MIDI file that is currently in memory.
DECLARE FUNCTION PlayMIDI% ()
'LoadAndPlay loads and plays a MIDI file
DECLARE FUNCTION LoadAndPlayMIDI% (Filename$)
'LoadMIDI loads a MIDI file into memory, but DOES NOT begin playing it.
'This is useful if you want to load some music but not play it until later.
DECLARE SUB LoadMIDI (Filename$)
'StopMIDI stops the MIDI file that is currently playing, if there is one.
DECLARE SUB StopMIDI ()
'Pokestring is used by int86qb$
DECLARE SUB Pokestring (SEGJE%, OFFJE%, Main$)
'MIDITime! returns how long the current MIDI file has been plaing
DECLARE FUNCTION TimeMIDI! ()
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

'*********************************************************************
'INSERT YOUR OWN CODE BEGINNING HERE - SEE SAMPLE.BAS FOR A QMIDI DEMO
'*********************************************************************

FUNCTION int2str$ (sword%)
'This function is translating SWORD Integers into a string. Its only use
'is when you still use asm$ for assembler functions( like i do). In that
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
                    
'The rest is more simple i think:
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
FOR i% = 0 TO 9
  hx$ = HEX$(datas%(i%))
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
a$ = int86qb$(MIDI.INTERRUPT, 0, SADD(Filename$), &H500, 0, VARSEG(Filename$), 0, 0, 0, 0, 0)
MIDI.LOADED = -1
'Start the music!!
a$ = int86qb$(MIDI.INTERRUPT, 0, 0, &H501, 0, 0, 0, 0, 0, 0, 0)
AX$ = MID$(a$, 9, 4)
AX% = VAL(AX$)
IF NOT AX% THEN
       LoadAndPlayMIDI% = -1
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
a$ = int86qb$(MIDI.INTERRUPT, 0, SADD(Filename$), &H500, 0, VARSEG(Filename$), 0, 0, 0, 0, 0)
MIDI.LOADED = -1
END SUB

'PlayMIDI - Begins playing a MIDI file in the background.
FUNCTION PlayMIDI%
'If no MIDI file is loaded, exit now
IF MIDI.LOADED = 0 THEN EXIT FUNCTION
'If MIDI.INTERRUPT is not defined, set it to 81H
IF MIDI.INTERRUPT = 0 THEN MIDI.INTERRUPT = &H81
'Start the music!!
a$ = int86qb$(MIDI.INTERRUPT, 0, 0, &H501, 0, 0, 0, 0, 0, 0, 0)
AX$ = MID$(a$, 9, 4)
AX% = VAL(AX$)
IF NOT AX% THEN
       PlayMIDI% = -1
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
FOR i% = 0 TO LEN(Main$) - 1
  POKE OFFJE% + i%, ASC(MID$(Main$, i% + 1, 1))
NEXT
DEF SEG

END SUB

'StopMIDI - Stops playing MIDI file
SUB StopMIDI
'If MIDI.INTERRUPT is not defined, set it to 81H
IF MIDI.INTERRUPT = 0 THEN MIDI.INTERRUPT = &H81
'Stop the music!!
a$ = int86qb$(MIDI.INTERRUPT, 0, 0, &H502, 0, 0, 0, 0, 0, 0, 0)
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

