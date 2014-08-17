;Copyright (C) HiTech Laboratories, Denis Andrianov
;=============================================================================

.model medium, basic
.286
.code

;=============================================================================
;DECLARE SUB Fontm12 (BYVAL TextSeg%, BYVAL TextOff%, BYVAL TextLen%, BYVAL x%, BYVAL y%)

;Stack frame:
;y%             bp+6
;x%               +8
;TextLen%         +10
;TextOff%         +12
;TextSeg%         +14




  DEF SEG = &HF000


    FOR LetterNum% = 1 TO LEN(Text$)
    LetterOffset% = &HFA6E + ASC(MID$(Text$, LetterNum%, 1)) * 8
    xPosition% = x% + (LetterNum% - 1) * 8
    yPosition% = y%

      FOR LineNum% = 0 TO 7
      ScanLine% = PEEK(LetterOffset% + LineNum%)

        IF ScanLine% AND 128 THEN
        PSET (xPosition%, yPosition%), Colour%
        END IF

        IF ScanLine% AND 64 THEN
        PSET (xPosition% + 1, yPosition%), Colour%
        END IF

        IF ScanLine% AND 32 THEN
        PSET (xPosition% + 2, yPosition%), Colour%
        END IF

        IF ScanLine% AND 16 THEN
        PSET (xPosition% + 3, yPosition%), Colour%
        END IF

        IF ScanLine% AND 8 THEN
        PSET (xPosition% + 4, yPosition%), Colour%
        END IF

        IF ScanLine% AND 4 THEN
        PSET (xPosition% + 5, yPosition%), Colour%
        END IF

        IF ScanLine% AND 2 THEN
        PSET (xPosition% + 6, yPosition%), Colour%
        END IF

        IF ScanLine% AND 1 THEN
        PSET (xPosition% + 7, yPosition%), Colour%
        END IF

      yPosition% = yPosition% + 1
      NEXT

    NEXT



  DEF SEG











PUBLIC Fontm12
Fontm12 PROC

push  bp
mov   bp,sp

push  ds
push  es
push  si
push  di


;**********************************
;Exit function.
;**********************************

pop   ax
pop   di
pop   si
pop   es
pop   ds
pop   bp

ret   10



Fontm12 ENDP

;=============================================================================

END
