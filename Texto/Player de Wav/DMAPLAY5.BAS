'------------------------------------------------------------------
' DMA Play 5.1 (1998.3.14 version with single buffer)
' a versatile DMA-based Microsoft WAVE file player in QBasic and QB45.
' by Mike Huff (v1), Martin Rampersad (v2), Toshi Horie (v3,4,5)
' Realtime 16-bit to 8-bit stereo/mono conversions added up to 44Khz
' Program downloaded from http://www.ocf.berkeley.edu/~horie/project.html
'        ------------- programming notes ----------------
' can somebody translate SUB Convert2 to inline x86 assembly?
' Speed benchmarks with SBPro, QB45, Cyrix P120+:
' ~1600 cycles free on 44Khz stereo realtime downsampled wave file
' 17590 cycles free on 22Khz stereo wave file
' 32767+ cycles free on 22Khz mono wave file
' realtime conversion requires Pentium 100 level computer with QB45
' If you get stuttering and 0 cycles free, your computer is too slow
' This program runs about 10x faster compiled compared to QBasic 1.1
' better error correction and autodetection
' WAV file type autodetection for mono 16bit WAV autodetect
' double buffer is added for better sound quality
' By Mike Huff (1996)  * Now plays whole file in QB45 and Qbasic *
' Added WAVE file header reader to determine length and sampling freq.
' DMAPlay16 (stereo) actually completes transfer of 40 Megabyte song
' Toshi gives special thanks to Ethan Brodsky, who helped immensely.
' It works without static now! It was just an alignment problem.
'-----------------------------------------------------------------
DECLARE SUB ConvertStereo (Freq&, Bseg1&, Boff1&, L&)
DECLARE FUNCTION Ulong2int% (Ulong&)
DECLARE SUB Convert1 (Bseg&, Boff&, L&)
DECLARE SUB Convert2 (Bseg&, Boff&, L&)
DECLARE SUB Convert4 (Bseg&, Boff&, L&)
DECLARE FUNCTION DEC2HEX$ (longnum&)
DECLARE SUB WavInfo (Length&, Freq&, StereoWav%, SixteenBit%)
DECLARE FUNCTION int2ULong& (signedint%)
DECLARE FUNCTION SpeakerStatus% ()
DECLARE FUNCTION DMAStatus% ()
DECLARE FUNCTION DMADone% (DMA16%, L&)
DECLARE FUNCTION ResetDSP% ()
DECLARE SUB FMVolume (Right%, Left%, Getvol%)
DECLARE SUB VocVolume (Right%, Left%, Getvol%)
DECLARE SUB MasterVolume (Right%, Left%, Getvol%)
DECLARE SUB MicVolume (Gain%, Getvol%)
DECLARE SUB LineVolume (Right%, Left%, Getvol%)
DECLARE SUB CDVolume (Right%, Left%, Getvol%)
DECLARE SUB InputSource (InputSrc%, GetSrc%)
DECLARE SUB WriteDSP (byte%)
DECLARE SUB SetStereo (OnOff%)
DECLARE FUNCTION ReadDSP% ()
DECLARE SUB WriteDAC (byte%)
DECLARE SUB SpeakerState (OnOff%)
DECLARE SUB DMAState (StopGo%)
DECLARE FUNCTION ReadDAC% ()
DECLARE SUB DMAPlay (Segment&, Offset&, Length&, Freq&, StereoWav%)
DECLARE SUB DMAPlay16 (Segment&, Offset&, Length&, Freq&, StereoWav%)
DECLARE SUB DMARecord (Segment&, Offset&, Length&, Freq&)
DECLARE SUB GetBLASTER ()
DECLARE FUNCTION DSPVersion! ()
COMMON SHARED Baseport%, LenPort%, DMA%, DMA16%, cardversion%
'$DYNAMIC:  'needed for compilation in QB45
Playerversion$ = "DMAPlay 5.1"
TYPE WaveHeaderType
        RiffID           AS STRING * 4 'should be 'RIFF'
        RiffLength       AS LONG
        'rept. chunk id and size then chunk data
        WavID            AS STRING * 4 'should be 'WAVE'
        FmtID            AS STRING * 4
        FmtLength        AS LONG
    'FMT ' chunk - common fields
        wavformattag     AS INTEGER ' word - format category e.g. 0x0001=PCM
        Channels         AS INTEGER ' word - number of Channels 1=mono 2=stereo
        SamplesPerSec    AS LONG    'dword - sampling rate e.g. 44100Hz
        avgBytesPerSec   AS LONG    'dword - to estimate buffer size
        blockalign       AS INTEGER ' word - buffer size must be int. multiple of this
    'FMT - format-specific fields
    'e.g. PCM-format-specific has BitsPerSample (word)
    ' for PCM data,
    '      wAvgBytesPerSec=RoundUp(wChannels*wBitsPerSec*wBitsPerSample/8)
    '          wBlockAlign=wBitsPerSample/8
    ' assuming no FACT, CUE points, Playlist, Assoc. Data List chunks
        FmtSpecific      AS INTEGER ' word
        DataID           AS STRING * 4
        DataLength       AS LONG
END TYPE
'data section stored like this:  colon means -or-
' <wave-data> -> { <data-ck> : <wave-list> }
'    <data-ck> ->  data( <wave-data> )

' <wave-list> -> LIST( 'wavl' {  <data-ck> : //wave samples
'                                <silence-ck> }... ) //silence
' <silence-ck> -> slnt( <dwSamples:DWORD> ) //count of silence samples
'                                           // not necessarily 0.
'                                           // use last data sample's value.
DIM SHARED Wave(0) AS WaveHeaderType
'PCM Data is stored as follows; 0123 are sample #s, L,R=left,right
'                  byte0 byte1 byte2 byte3
'  8 bit mono   -  0     1     2     3
'  8 bit stereo -  0L    0R    1L    1R
' 16 bit mono   -  0lo   0hi   1lo   1hi
' 16 bit stereo -  0Llo  0Lhi  0Rlo  0Rhi
' ___Sample Size___data fmt_____range_________________
'    1 to 8 bits  Unsigned int 0-0xFF
'    9 or more    signed int   most negative to most positive
'
'in PCM 'data' Chunk
'  dwChunkStart - file pos. of 'data' chunk relative to start of data section in 'wavl' LIST chunk
'  dwBlockStart - file pos. of cue point relative to .....
'  dwSampleOffs = 0
'  fccChunk - FOURCC value 'data'

SCREEN 0: CLS
'WIDTH 80, 50
PRINT Playerversion$
PRINT "By Mike Huff (SB, SBPro) and Toshi Horie (SB16, SBPro realtime downmixing)"
PRINT "Modified By Martin Rampersad (To play entire file instead of first 32k)"
PRINT "Comments, etc. can be sent to MHuff@gnn.com or to Martin_Rampersad@juno.com"
GetBLASTER ' Parses BLASTER environment
PRINT STRING$(80, 196)
IF ResetDSP% THEN 'resets DSP (returns true if sucessful)
   'PRINT "DSP reset sucessfully!"
ELSE
   PRINT "DSP failed to reset, try another port.": END
END IF
PRINT "Sound Card DSP version:"; DSPVersion!
cardversion% = FIX(DSPVersion!)
IF DMA16% = 0 AND cardversion% >= 4 THEN
    PRINT "Set Hx parameter in BLASTER environment.": END
END IF
IF DMA% = 0 THEN PRINT "set Dx parameter in BLASTER environment.": END
SpeakerState 1 'turn the speaker on
MasterVolume 8, 8, 0  '15,15,0 cranks the master volume all the way up.
'Volume 8, 8, 0  '15,15,0 cranks the master volume all the way up.
MasterVolume Right%, Left%, -1 'this puts the mixer volumes in Right% and Left%
PRINT "Master volume is set at: Right-"; Right%; " Left-"; Left%

'WavBuffer size MUST be divisible by 4 for stereo files
CONST blocklen = 32764

'create 32K-32K double buffer
DIM WavBuffer(1) AS STRING * blocklen


'Filename$ = "C:\SOUND\AOIUSAGI.WAV"
'Filename$ = "C:\QB45\SBPRO.WAV"
'Filename$ = "C:\QB45\WHOOSH1.WAV"
'Filename$ = "C:\QB45\B.WAV"
'Filename$ = "C:\QB45\ARROWS.WAV"
'Filename$ = "C:\QB45\A4416.WAV"
'Filename$ = "C:\SOUND\CD_AUDIO\STEREO.WAV"
Filename$ = "C:\WINDOWS\MEDIA\THEMIC~1.WAV"

'rem it out for QBasic
'IF COMMAND$ > "" THEN Filename$ = COMMAND$
OPEN Filename$ FOR BINARY AS #1
IF LOF(1) = 0 THEN
    PRINT "**"; Filename$; " doesn't exist.**"
    CLOSE : KILL Filename$: END
END IF
HeaderSize = 45                'assume .WAV file (use 45)
                               'I think it's 32 for VOC, 0 for RAW files


PRINT : PRINT "Playing " + Filename$
GET #1, 1, Wave(0): 'BASIC defines beginning of file as 1
Freq& = 22000: 'default playback frequency
'Length& = LOF(1) - HeaderSize

WavInfo Length&, Freq&, StereoWav%, SixteenBit%
css = cardversion% = 3 AND SixteenBit% AND StereoWav% 'conversion active?
cmm = cardversion% = 3 AND SixteenBit% AND NOT StereoWav% '16mono->8mono
LOCATE 1, 64: PRINT "cycles free"
SEEK #1, HeaderSize
Bseg& = int2ULong&(VARSEG(WavBuffer(0)))
Boff& = int2ULong&(VARPTR(WavBuffer(0))): 'should always be 0 in BASIC
RLength& = Length&: 'RLength& is number of remaining bytes
        IF RLength& >= blocklen THEN
                L& = blocklen
        ELSE
                L& = RLength&
        END IF
        GET #1, , WavBuffer(0) 'fill first buffer
        IF css THEN Convert2 Bseg&, Boff&, L&
        IF cmm THEN Convert4 Bseg&, Boff&, L&
t1# = TIMER
DO
        '............play block in the background.......................
        IF SixteenBit% THEN
            SELECT CASE cardversion%
            CASE 4 'SB16, AWE32?
                DMAPlay16 Bseg1&, Boff1&, L&, Freq&, StereoWav%
                LOCATE 1, 14: COLOR 14: PRINT "SB16 mode"
            CASE 3 'SBPro
                LOCATE 1, 13: COLOR 14
                IF StereoWav% THEN
                  PRINT "SBPro Realtime stereo"
                  DMAPlay Bseg&, Boff&, L& \ 4, Freq&, StereoWav%
                ELSE 'mono 16 to 8 bit conversion
                    PRINT "SBPro Realtime mono"
                    DMAPlay Bseg&, Boff&, L& \ 2, Freq&, StereoWav%
                END IF
            CASE ELSE 'SB
                    COLOR 14, 4
                    PRINT "SB Realtime conversions from stereo to mono not supported.": END
            END SELECT
        ELSE   '8 bit
                IF StereoWav% THEN
                    SELECT CASE cardversion%
                        CASE 3
                            DMAPlay Bseg&, Boff&, L&, Freq& * 2, StereoWav%
                        CASE 4
                            DMAPlay Bseg&, Boff&, L&, Freq& * 2, StereoWav%
                        CASE ELSE
                            PRINT "SBPro required for stereo.": END
                    END SELECT
                ELSE 'mono 8-bit (no error checking)
                    DMAPlay Bseg&, Boff&, L&, Freq&, StereoWav%
                END IF
                LOCATE 1, 14: COLOR 14: PRINT "SB mode"; Freq&; "Hz"
        END IF
        IF RLength& <= 0 THEN GOTO last
        '..............................................................
        'fill alternate buffer
                IF RLength& >= blocklen THEN
                        L& = blocklen
                ELSE
                        L& = RLength&
                END IF
                
                    GET #1, , WavBuffer(0)
                    IF css THEN ConvertStereo Freq&, Bseg&, Boff&, L&
                    IF cmm THEN Convert4 Bseg&, Boff&, L&
                RLength& = RLength& - L& 'update remaining length

        'done filling alternate buffer,,,,,,,,,,,,,,
last:
        LOCATE 1, 37: PRINT TIME$; "  "; 100 - INT(RLength& / Length& * 100); "% done  "; cycles%
        cycles% = 0
        IF INKEY$ > "" THEN stopflag = 1
        DO UNTIL DMADone%(DMA16%, L&)
              
               'now CPU is free to do graphics, etc.
               'LINE (RND * 640, RND * 350)-(RND * 640, RND * 350), RND * 16
               '
               '
               ' put your graphics update calls here
               '
               '
                IF cycles% < 32767 THEN cycles% = cycles% + 1
                'LOCATE 23, 1
                'PRINT USING "###.##s"; (TIMER - t1#)
                IF INKEY$ > "" THEN stopflag = 1
        LOOP
        IF stopflag THEN EXIT DO: 'stop here so it doesn't freeze the computer
LOOP UNTIL EOF(1)

'Use DMARecord to record in the background.
'and use DMAPlay to playback the same buffer you recorded to or you could
'even write the buffer to a file.
'DMARecord VARSEG(WavBuffer(0)), VARPTR(WavBuffer(0)), Length&, Freq&

LOCATE 23, 1: PRINT "DMA transfer completed!";
DMAState 0: 'stop sound
SpeakerState 0: 'turn the speaker off
MasterVolume 0, 0, 0 'mute (clicks)
quit% = ResetDSP%
CLOSE
END

REM $STATIC
SUB CDVolume (Right%, Left%, Getvol%)
OUT Baseport% + 4, &H28
IF Getvol% THEN
   Left% = INP(Baseport% + 5) \ 16
   Right% = INP(Baseport% + 5) AND &HF
   EXIT SUB
ELSE
   OUT Baseport% + 5, (Right% + Left% * 16) AND &HFF
END IF
END SUB

SUB Convert1 (Bseg&, Boff&, L&)
'converts 16 bit signed stereo buffer
'to an 8 bit unsigned stereo buffer in place
'slower original routine
'PRE: BSeg&:BOff& points to a array buffer of length L& chars
'POST: L&\2 of the buffer is converted to 8bit, rest is 16-bit garbage
DEF SEG = Ulong2int(Bseg&)
FOR I = 0 TO L& - 1 STEP 2
    addr = I + Boff&
    lowbyte% = PEEK(addr)
    hibyte% = PEEK(addr + 1)
    V& = hibyte% * 256& + lowbyte%
    IF (V& AND &H8000) THEN
        V& = V& + &H8000
        vi% = V& \ 256
    ELSE
        vi% = V& \ 256 + &H80
    END IF
    POKE Boff& + I \ 2, vi%
NEXT
END SUB

SUB Convert2 (Bseg&, Boff&, L&)
'converts 44KHz stereo to 22Khz stereo
'optimized conversion routine playing every other sample.
'used for 22Khz or higher stereo waves on SBPro
'no more long integer calculations
DEF SEG = Ulong2int(Bseg&)
FOR I = 0 TO L& - 1 STEP 8
    'LEFT CHANNEL
    addr = I + Boff&
    hibyte% = PEEK(addr + 1)    'i+1
    POKE Boff& + I \ 4, hibyte% + &H80

    'RIGHT CHANNEL
    hibyte% = PEEK(addr + 3)    'i+4
    POKE Boff& + I \ 4 + 1, hibyte% + &H80
NEXT

END SUB

SUB Convert3 (Bseg&, Boff&, L&)
'converts 16-bit 44Khz stereo to 8-bit 44Khz mono for SBPro
'not working yet!
FOR I = 0 TO L& - 1 STEP 4
    addr = I + Boff&
    'Left=Left>>2
    Leftlo% = PEEK(addr) \ 2
    Lefthi% = PEEK(addr + 1) \ 2
    'Right=Right>>2
    Rightlo% = PEEK(addr + 2) \ 2
    Righthi% = PEEK(addr + 3) \ 2
    mix& = Lefthi% * 256 + Leftlo%
    mix& = mix& + Righthi% * 256 + Rightlo%
    mixhi% = (mix& AND &HFF00) \ &H100
    mixlo% = (mix& AND &HFF)
    POKE Boff& + I \ 4, mixlo%
    POKE Boff& + I \ 4 + 1, mixhi%
NEXT I
END SUB

SUB Convert4 (Bseg&, Boff&, L&)
'convert 16 bit mono to 8 bit mono
DEF SEG = Ulong2int(Bseg&)
FOR I = 0 TO L& - 1 STEP 2
    addr = I + Boff&
    hibyte% = PEEK(addr + 1)    'i+1
    POKE Boff& + I \ 2, hibyte% + &H80
NEXT

END SUB

SUB ConvertStereo (Freq&, Bseg1&, Boff1&, L&)
IF Freq& > 22050 THEN
    Convert2 Bseg1&, Boff1&, L&
ELSE
    Convert2 Bseg1&, Boff1&, L&
END IF
END SUB

FUNCTION DEC2HEX$ (longnum&)
DEC2HEX$ = HEX$(longnum&)
END FUNCTION

FUNCTION DMADone% (DMA16%, L&)
countlo% = INP(LenPort%)
counthi% = INP(LenPort%)
count& = CLNG(counthi% * 256&) + CLNG(countlo%)
'if you have problems with L&-8, then use L&-1
'LOCATE 21, 1
'PRINT counthi%; "   "
IF count& > L& - 1 THEN
        IF DMA16% THEN
                ack16% = INP(Baseport% + &HF) 'ack to SB
                'OUT &HA0, &H20 'acknowledge SB interrupt 8-15
                'OUT &H20, &H20 'acknowledge SB interrupt 1-15
        ELSE
                ack% = INP(Baseport% + &HE)
                'OUT &H20, &H20 'acknowledge SB interrupt 1-15
        END IF
        DMADone% = -1
        'SOUND 300, .4
END IF
END FUNCTION

SUB DMAPlay (Segment&, Offset&, Length&, Freq&, StereoWav%)
' Transfers and plays the contents of the buffer.
Length& = Length& - 1
page% = 0
addr& = Segment& * 16 + Offset&
SELECT CASE DMA%
    CASE 0
       PgPort% = &H87
       AddPort% = &H0
       LenPort% = &H1
       ModeReg% = &H48
    CASE 1
       PgPort% = &H83
       AddPort% = &H2
       LenPort% = &H3
       ModeReg% = &H49
    CASE 2
       PgPort% = &H81
       AddPort% = &H4
       LenPort% = &H5
       ModeReg% = &H4A
    CASE 3
       PgPort% = &H82
       AddPort% = &H6
       LenPort% = &H7
       ModeReg% = &H4B
    CASE ELSE
       PRINT "8-bit DMA channels 0-3 only!": END
       EXIT SUB
END SELECT
Lengthlo% = Length& AND &HFF
Lengthhi% = (Length& AND &HFF00&) \ &H100
IF StereoWav% THEN SetStereo 1
OUT &HA, &H4 + DMA%: 'DMA channel to use (DRQ#)
OUT &HC, &H0
OUT &HB, ModeReg%
OUT AddPort%, addr& AND &HFF:  'buffer address of sound data low byte
OUT AddPort%, (addr& AND &HFF00&) \ &H100: 'high byte
IF (addr& AND 65536) THEN page% = page% + 1: '64K pages for 8-bit DMA
IF (addr& AND 131072) THEN page% = page% + 2
IF (addr& AND 262144) THEN page% = page% + 4
IF (addr& AND 524288) THEN page% = page% + 8
OUT PgPort%, page%: 'output page of phys. addr of sample block
OUT LenPort%, Lengthlo%: 'size of block to DMA controller -Low
OUT LenPort%, Lengthhi%: 'high byte
OUT &HA, DMA%: 'release DMA channel

'LOCATE 21, 1: PRINT "seg:"; DEC2HEX$(Segment&),
'PRINT "offset:"; DEC2HEX$(Offset&), "addr:"; DEC2HEX$(addr&)


TimeConst% = 256 - 1000000 \ Freq&

IF Freq& < 22728 THEN
   'IF Freq& > 22222 AND cardversion% = 1 THEN PRINT "SB 1.x: 4000-22222Hz only"
   'IF Freq& < 4000 THEN PRINT "SB: 4000+Hz only"
   WriteDSP &H40
   WriteDSP TimeConst%
   WriteDSP &H14:                      '8 bit output over DMA
   WriteDSP (Length& AND &HFF)
   WriteDSP ((Length& AND &HFFFF&) \ &H100)
ELSE 'SBPro (DSP version 3.x) can play 8-bit mono/stereo wave files
   IF cardversion% = 3 THEN
      'high speed 8 bit output up to 44kHz mono or 22Khz stereo
      WriteDSP &H40:  'output sampling rate const
      WriteDSP TimeConst%
      WriteDSP &H48
      WriteDSP Lengthlo%
      WriteDSP Lengthhi%
      WriteDSP &H91
   ELSE
      PRINT "You need a Sound Blaster Pro to play at 8 bit high speed."
      EXIT SUB
   END IF
END IF
END SUB

SUB DMAPlay16 (Segment&, Offset&, L&, Freq&, StereoWav%)
' Transfers and plays the contents of the buffer.
' Try only on an SoundBlaster 16 !!
' 1 page=128K in 16 bit mode
' DMA16% (16-bit DMA channel) passed implicitly

IF cardversion% < 4 THEN PRINT "You need an SB16 for this mode!": END
L& = L& - 1: page% = 0
addr& = Segment& * 16 + Offset&

SELECT CASE DMA16%
    CASE 4
       PgPort% = &H0
       AddPort% = &HC0
       LenPort% = &HC2
       ModeReg% = &H48: '58h for autoinit/48h for not
    CASE 5
       PgPort% = &H8B
       AddPort% = &HC4
       LenPort% = &HC6
       ModeReg% = &H49
    CASE 6
       PgPort% = &H89
       AddPort% = &HC8
       LenPort% = &HCA
       ModeReg% = &H4A
    CASE 7
       PgPort% = &H8A 'ok
       AddPort% = &HCC 'ok
       LenPort% = &HCE 'ok
       ModeReg% = &H4B 'ok
    CASE ELSE
       PRINT "16 bit DMA channels 4-7 only!"
       EXIT SUB
END SELECT

page% = (addr& \ 131072) * 2
Offset2& = (addr& - (page% * 65536)) \ 2
Lengthlo% = ((L& \ 2) AND &HFF): 'number of words-1
Lengthhi% = (((L& \ 2) AND &HFF00&) \ &H100) 'this may be wrong sometimes


'LOCATE 21, 1: PRINT "seg:"; DEC2HEX$(Segment&),
'PRINT "offset:"; DEC2HEX$(Offset&), "addr:"; DEC2HEX$(addr&)


OUT &HD8, 0: 'clear flip flop
OUT &HD6, ModeReg%: 'write mode reg
OUT AddPort%, (Offset2& AND &HFF):            'Buffer base offset lo
OUT AddPort%, (Offset2& AND &HFF00&) \ &H100: 'Buffer base offset hi
OUT PgPort%, page%:                          'output page of phys. addr of sample block
OUT LenPort%, Lengthlo%:                     'DMA count = length of buffer
OUT LenPort%, Lengthhi%:                     'DMA count high byte
OUT &HD4, DMA16% - 4: 'write single mask (select Channel16)

FreqHi% = (Freq& AND &HFF00&) \ &H100
FreqLo% = Freq& AND &HFF
WriteDSP &H41: 'set output sampling rate
WriteDSP FreqHi%
WriteDSP FreqLo%
WriteDSP &HB0:  '16 bit DAC, single cycle, FIFO off (ok)
IF StereoWav% THEN 'subtract 10h for unsigned
        WriteDSP &H30: '30h=Mode byte for 16 bit signed stereo
ELSE
        WriteDSP &H10: '10h=Mode byte for 16 bit signed mono
END IF
WriteDSP Lengthlo%
WriteDSP Lengthhi%
END SUB

SUB DMARecord (Segment&, Offset&, Length&, Freq&)
Length& = Length& - 1
memloc& = Segment& * 16 + Offset&
page% = 0
SELECT CASE DMA%
    CASE 0
       PgPort% = &H87
       AddPort% = &H0
       LenPort% = &H1
       ModeReg% = &H44
    CASE 1
       PgPort% = &H83
       AddPort% = &H2
       LenPort% = &H3
       ModeReg% = &H45
    CASE 2
       PgPort% = &H81
       AddPort% = &H4
       LenPort% = &H5
       ModeReg% = &H46
    CASE 3
       PgPort% = &H82
       AddPort% = &H6
       LenPort% = &H7
       ModeReg% = &H47
    CASE ELSE
       EXIT SUB
END SELECT

OUT &HA, &H4 + DMA%
OUT &HC, &H0
OUT &HB, ModeReg%
OUT AddPort%, memloc& AND &HFF
OUT AddPort%, (memloc& AND &HFFFF&) \ &H100
IF (LongByte& AND 65536) THEN page% = page% + 1
IF (LongByte& AND 131072) THEN page% = page% + 2
IF (LongByte& AND 262144) THEN page% = page% + 4
IF (LongByte& AND 524288) THEN page% = page% + 8
OUT PgPort%, page%
OUT LenPort%, Length& AND &HFF
OUT LenPort%, (Length& AND &HFFFF&) \ &H100
OUT &HA, DMA%

IF Freq& <= 23000 THEN
   TimeConst% = 256 - 1000000 \ Freq&
   WriteDSP &H40
   WriteDSP TimeConst%
   WriteDSP &H24
   WriteDSP (Length& AND &HFF)
   WriteDSP ((Length& AND &HFFFF&) \ &H100)
ELSE
   IF DSPVersion! >= 3 THEN
      TimeConst% = ((65536 - 256000000 / Freq&) AND &HFFFF&) \ &H100
      WriteDSP &H40
      WriteDSP TimeConst%
      WriteDSP (Length& AND &HFF)
      WriteDSP ((Length& AND &HFFFF&) \ &H100)
      WriteDSP &H99
   ELSE
      PRINT "You need a Sound Blaster with a DSP 3.x+ to record at high speed."
      EXIT SUB
   END IF
END IF

END SUB

SUB DMAState (StopGo%)
' Stops or continues DMA play.
IF StopGo% THEN WriteDSP &HD4 ELSE WriteDSP &HD0
END SUB

FUNCTION DSPVersion!
' Gets the DSP version.
WriteDSP &HE1
Temp% = ReadDSP%
Temp2% = ReadDSP%
DSPVersion! = VAL(STR$(Temp%) + "." + STR$(Temp2%))
END FUNCTION

SUB FMVolume (Right%, Left%, Getvol%)
OUT Baseport% + 4, &H26
IF Getvol% THEN
   Left% = INP(Baseport% + 5) \ 16
   Right% = INP(Baseport% + 5) AND &HF
   EXIT SUB
ELSE
   OUT Baseport% + 5, (Right% + Left% * 16) AND &HFF
END IF
END SUB

SUB GetBLASTER
' This subroutine parses the BLASTER environment string and returns settings
' implicitly using common shared variables Baseport%, DMA%,DMA16%
IF LEN(ENVIRON$("BLASTER")) = 0 THEN PRINT "BLASTER environment variable not set.": EXIT SUB
FOR index% = 1 TO LEN(ENVIRON$("BLASTER"))
   SELECT CASE MID$(ENVIRON$("BLASTER"), index%, 1)
      CASE "A"
        Baseport% = VAL("&H" + MID$(ENVIRON$("BLASTER"), index% + 1, 3))
      CASE "I"
        IRQ% = VAL(MID$(ENVIRON$("BLASTER"), index% + 1, 1))
      CASE "D"
        DMA% = VAL(MID$(ENVIRON$("BLASTER"), index% + 1, 1))
      CASE "H"
        DMA16% = VAL(MID$(ENVIRON$("BLASTER"), index% + 1, 1))
   END SELECT
NEXT

END SUB

SUB InputSource (InputSrc%, GetSrc%)
OUT Baseport% + 4, &HC
IF GetSrc% THEN
   InputSrc% = INP(Baseport% + 5) AND 2 + INP(Baseport% + 5) AND 4
ELSE
   OUT Baseport% + 5, InputSrc% AND 7
END IF
END SUB

FUNCTION int2ULong& (signedint%)
IF signedint% < 0 THEN
        int2ULong& = CLNG(signedint% + 65536)
ELSE
        int2ULong& = CLNG(signedint%)
END IF
END FUNCTION

SUB LineVolume (Right%, Left%, Getvol%)
OUT Baseport% + 4, &H2E
IF Getvol% THEN
   Left% = INP(Baseport% + 5) \ 16
   Right% = INP(Baseport% + 5) AND &HF
   EXIT SUB
ELSE
   OUT Baseport% + 5, (Right% + Left% * 16) AND &HFF
END IF
END SUB

SUB MasterVolume (Right%, Left%, Getvol%)
OUT Baseport% + 4, &H22
'PRINT BasePort%
IF Getvol% THEN
   Left% = INP(Baseport% + 5) \ 16
   Right% = INP(Baseport% + 5) AND &HF
   EXIT SUB
ELSE
   OUT Baseport% + 5, (Right% + Left% * 16) AND &HFF
END IF
END SUB

SUB MicVolume (Volume%, Getvol%)
OUT Baseport% + 4, &HA
IF Getvol% THEN
   Volume% = INP(Baseport% + 5) AND &HF
   EXIT SUB
ELSE
   OUT Baseport% + 5, Volume% AND &HF
END IF
END SUB

FUNCTION ReadDAC%
' Reads a byte from the DAC.
WriteDSP &H20
ReadDAC% = ReadDSP%
END FUNCTION

FUNCTION ReadDSP%
WAIT (Baseport% + &HE), &H80: 'wait for bit 7 on pollport
DO: DSPIn% = INP(Baseport% + 10): LOOP UNTIL DSPIn% <> &HAA
ReadDSP% = DSPIn%
END FUNCTION

FUNCTION ResetDSP%
ct = 0: stat = 0: ready = &HAA
OUT Baseport% + &H6, 1
DO
        OUT Baseport% + &H6, 0
        stat = INP(Baseport% + &HE)
        stat = INP(Baseport% + &HA)
        IF stat = ready THEN EXIT DO
        ct = ct + 1
LOOP WHILE ct < 100 'wait about 100 ms
IF stat = ready THEN ResetDSP% = 1 ELSE ResetDSP% = 0
END FUNCTION

SUB SetStereo (OnOff%)
'only needed on SBPro
MixerReg% = Baseport% + 4
MixerData% = Baseport% + 5
        OUT MixerReg%, &HE
        IF OnOff% THEN
                OUT MixerData%, 2
        ELSE
                OUT MixerData%, 0
        END IF
END SUB

SUB SpeakerState (OnOff%)
' Turns speaker on or off.
IF OnOff% THEN WriteDSP &HD1 ELSE WriteDSP &HD3
END SUB

FUNCTION SpeakerStatus%
OUT Baseport% + 4, &HD8
IF INP(Baseport% + 5) = &HFF THEN SpeakerStatus% = -1 ELSE SpeakerStatus% = 0
END FUNCTION

FUNCTION Ulong2int% (Ulong&)
IF Ulong& > 32767 THEN
        Sint% = CINT(Ulong& - 65536)
ELSE
        Sint% = CINT(Ulong&)
END IF
Ulong2int% = Sint%
END FUNCTION

SUB VocVolume (Right%, Left%, Getvol%)
OUT Baseport% + 4, &H4
IF Getvol% THEN
   Left% = INP(Baseport% + 5) \ 16
   Right% = INP(Baseport% + 5) AND &HF
   EXIT SUB
ELSE
   OUT Baseport% + 5, (Right% + Left% * 16) AND &HFF
END IF
END SUB

SUB WavInfo (Length&, Freq&, StereoWav%, SixteenBit%)
'PRE: Wave(0) array filled from WAV file header
'blocklen passed because it is a constant
'POST: Length&, Freq& set
IF UCASE$(Wave(0).RiffID) <> "RIFF" THEN PRINT "NOT A RIFF FILE": END
'PRINT "RiffLength:"; Wave(0).RiffLength
IF UCASE$(Wave(0).WavID) <> "WAVE" THEN PRINT "NOT A WAVE FILE": END
IF Wave(0).wavformattag <> 1 THEN PRINT "Not PCM format": END
PRINT "Channels:"; Wave(0).Channels;
Freq& = Wave(0).SamplesPerSec
SELECT CASE Wave(0).Channels
    CASE 2
        StereoWav% = 1: PRINT "(Stereo)"
    CASE 1
        StereoWav% = 0: PRINT "(mono)"
    CASE ELSE
        StereoWav% = -1: END
END SELECT

PRINT "SamplesPerSec:"; Freq&
'assume no weird sampling rate like 9bit/sec
'PRINT "should equal blockalign:"; (Wave(0).avgBytesPerSec / Freq&)
PRINT "BlockAlign:"; Wave(0).blockalign
IF (blocklen MOD Wave(0).blockalign) <> 0 THEN PRINT "Internal error: make blocklen=32752": END
PRINT "FmtSpecific:"; Wave(0).FmtSpecific; "bits/sample"
IF Wave(0).FmtSpecific = 16 THEN SixteenBit% = 1
'PRINT "DataID:"; Wave(0).DataID
IF UCASE$(Wave(0).DataID) <> "DATA" THEN PRINT "Not Data chunk": END
PRINT "DataLength:"; Wave(0).DataLength; "bytes"
Length& = Wave(0).DataLength
playtime# = Length& / Freq& / Wave(0).blockalign
pmin = INT(playtime#) \ 60
psec = INT(playtime#) MOD 60
IF pmin > 0 THEN
    PRINT USING "Play Length: ##:"; pmin;
    PRINT USING "##"; psec
ELSE
    PRINT USING "Play Length ##.##s"; playtime#
END IF
PRINT "start of actual data:"; SEEK(1)
END SUB

SUB WriteDAC (byte%)
' Writes a byte to the DAC.
WriteDSP &H10
WriteDSP byte%
END SUB

SUB WriteDSP (byte%)
' Writes a byte to the DSP
DO: LOOP WHILE INP(Baseport% + 12) AND &H80
OUT Baseport% + 12, byte%
END SUB

SUB WriteMixer (cmd%, value%)
MixerReg% = Baseport% + 4
MixerData% = Baseport% + 5
OUT MixerReg%, cmd%
OUT MixerData%, value%
END SUB

