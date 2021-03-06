.MODEL MEDIUM, BASIC
.STACK 100h
.386
.DATA

SINE    DW 0,4,9,13,18,22,27,31,36,40,44,49,53,58,62
        DW 66,71,75,79,83,88,92,96,100,104,108,112,116,120,124
        DW 128,132,136,139,143,147,150,154,158,161,165,168,171,175,178
        DW 181,184,187,190,193,196,199,202,204,207,210,212,215,217,219
        DW 222,224,226,228,230,232,234,236,237,239,241,242,243,245,246
        DW 247,248,249,250,251,252,253,254,254,255,255,255,256,256,256
        DW 256,256,256,256,255,255,255,254,254,253,252,251,250,249,248
        DW 247,246,245,243,242,241,239,237,236,234,232,230,228,226,224
        DW 222,219,217,215,212,210,207,204,202,199,196,193,190,187,184
        DW 181,178,175,171,168,165,161,158,154,150,147,143,139,136,132
        DW 128,124,120,116,112,108,104,100,96,92,88,83,79,75,71
        DW 66,62,58,53,49,44,40,36,31,27,22,18,13,9,4
        DW 0,-4,-9,-13,-18,-22,-27,-31,-36,-40,-44,-49,-53,-58,-62
        DW -66,-71,-75,-79,-83,-88,-92,-96,-100,-104,-108,-112,-116,-120,-124
        DW -128,-132,-136,-139,-143,-147,-150,-154,-158,-161,-165,-168,-171,-175,-178
        DW -181,-184,-187,-190,-193,-196,-199,-202,-204,-207,-210,-212,-215,-217,-219
        DW -222,-224,-226,-228,-230,-232,-234,-236,-237,-239,-241,-242,-243,-245,-246
        DW -247,-248,-249,-250,-251,-252,-253,-254,-254,-255,-255,-255,-256,-256,-256
        DW -256,-256,-256,-256,-255,-255,-255,-254,-254,-253,-252,-251,-250,-249,-248
        DW -247,-246,-245,-243,-242,-241,-239,-237,-236,-234,-232,-230,-228,-226,-224
        DW -222,-219,-217,-215,-212,-210,-207,-204,-202,-199,-196,-193,-190,-187,-184
        DW -181,-178,-175,-171,-168,-165,-161,-158,-154,-150,-147,-143,-139,-136,-132
        DW -128,-124,-120,-116,-112,-108,-104,-100,-96,-92,-88,-83,-79,-75,-71
        DW -66,-62,-58,-53,-49,-44,-40,-36,-31,-27,-22,-18,-13,-9,-4

COSINE  DW 256,256,256,256,255,255,255,254,254,253,252,251,250,249,248
        DW 247,246,245,243,242,241,239,237,236,234,232,230,228,226,224
        DW 222,219,217,215,212,210,207,204,202,199,196,193,190,187,184
        DW 181,178,175,171,168,165,161,158,154,150,147,143,139,136,132
        DW 128,124,120,116,112,108,104,100,96,92,88,83,79,75,71
        DW 66,62,58,53,49,44,40,36,31,27,22,18,13,9,4
        DW 0,-4,-9,-13,-18,-22,-27,-31,-36,-40,-44,-49,-53,-58,-62
        DW -66,-71,-75,-79,-83,-88,-92,-96,-100,-104,-108,-112,-116,-120,-124
        DW -128,-132,-136,-139,-143,-147,-150,-154,-158,-161,-165,-168,-171,-175,-178
        DW -181,-184,-187,-190,-193,-196,-199,-202,-204,-207,-210,-212,-215,-217,-219
        DW -222,-224,-226,-228,-230,-232,-234,-236,-237,-239,-241,-242,-243,-245,-246
        DW -247,-248,-249,-250,-251,-252,-253,-254,-254,-255,-255,-255,-256,-256,-256
        DW -256,-256,-256,-256,-255,-255,-255,-254,-254,-253,-252,-251,-250,-249,-248
        DW -247,-246,-245,-243,-242,-241,-239,-237,-236,-234,-232,-230,-228,-226,-224
        DW -222,-219,-217,-215,-212,-210,-207,-204,-202,-199,-196,-193,-190,-187,-184
        DW -181,-178,-175,-171,-168,-165,-161,-158,-154,-150,-147,-143,-139,-136,-132
        DW -128,-124,-120,-116,-112,-108,-104,-100,-96,-92,-88,-83,-79,-75,-71
        DW -66,-62,-58,-53,-49,-44,-40,-36,-31,-27,-22,-18,-13,-9,-4
        DW 0,4,9,13,18,22,27,31,36,40,44,49,53,58,62
        DW 66,71,75,79,83,88,92,96,100,104,108,112,116,120,124
        DW 128,132,136,139,143,147,150,154,158,161,165,168,171,175,178
        DW 181,184,187,190,193,196,199,202,204,207,210,212,215,217,219
        DW 222,224,226,228,230,232,234,236,237,239,241,242,243,245,246
        DW 247,248,249,250,251,252,253,254,254,255,255,255,256,256,256
        
.CODE

;/===========================================================================
;SUB RELSPRITEROTATE (BYVAL DESTSEG%,BYVAL X%,BYVAL Y%,BYVAL ANGLE%,BYVAL SPRSEG%,BYVAL SPROFF%)

;STACK

;DEST SEG               =30
;X                      =28
;Y                      =26
;ANGLE                  =24
;SPRSEG                 =22
;SPROFF                 =20
;QB RETSEG              =18
;QB RETOFF              =16
;DS                     =14
;BP                     =12
;SPR WIDTH              =10
;SPR HEIGHT             =8
;XCOUNTER               =6
;YCOUNTER               =4
;COS(ANGLE)             =2
;SIN(ANGLE)             =0
;XCENTER                =-2
;YCENTER                =-4


;ES:DI                  =DEST
;DS:SI                  =SPRITE

PUBLIC RELSPRITEROTATE
RELSPRITEROTATE PROC

;SAVE SOME IMPORTANT VALS TO STACK 
PUSH DS
PUSH BP

;SUBRACT SP BY 12 TO ADD PSEUDO REGS
XOR EAX,EAX
PUSH EAX
PUSH EAX
PUSH EAX

MOV BP,SP

MOV ES,[BP+30]          ;POINT DEST SEG TO ES(STANDARD IN ALL OF RELLIB)
PUSH EAX                ;TOO LAZY TO REORGANIZE STACK ;-)

;SAVE SIN AND COS VALUES TO STACK FOR LATER USE

;SIN
MOV SI,[BP+24]
SHL SI,1
MOV AX,SINE[SI]          ;GET THE VALUE FROM OUR NIFTY TABLE(*256)
MOV [BP],AX

;COS

MOV SI,[BP+24]          ;ANGLE
SHL SI,1                ;REFERENCE ANGLE POINTER TO CORRECT DATA AS ITS DW
MOV AX,COSINE[SI]       ;GET THE VALUE FROM OUR NIFTY TABLE(*256)
MOV [BP+2],AX           ;SAVE IT


;SPRITE NOW

MOV DS,[BP+22]          ;SPRITE SEGMENT
MOV SI,[BP+20]          ;OFFSET OF SPR

MOV AX,[SI]             ;WIDTH

SHR AX,3                ;CORRECT OUR WIDTH
MOV BX,[SI+2]           ;HEIGHT    
MOV [BP+10],AX          ;SAVE WIDTH
MOV [BP+8],BX           ;SAVE HEIGHT
ADD SI,4                ;SI POINTS TO FIRST PIXEL FO SPR

;YCENTER        ;YC=Y-(HEIGHT\2)
SHR BX,1        ;GET THE CENTER
MOV [BP-4],BX   ;SAVE TO STACK      
;XCENTER
SHR AX,1        ;GET THE CENTER
MOV [BP-2],AX   ;SAVE TO STACK      


LBL_ROTATE_PIXEL_R:

        MOV CL,[SI]     ;COLOR
        INC SI          ;POINT IT TO NEXT COLOR
        CMP CL,0
        JE LBL_SKIP_ZERO_R

        MOV CH,CL       ;FOR PADDING PURPOSES
        MOV GS,CX       ;SAVE THE COLORS



        ;LET'S GET THE CENTERS AND PUT THEM ON THE STACK

        ;YCENTER        ;YC=Y-(HEIGHT\2)
        MOV AX,[BP-4]   ;GET CENTER
        MOV DX,[BP+4]   ;GET CURRENT Y COUNTER
        SUB DX,AX       ;
        MOV [BP+20],DX  ;SAVE IT TO STACK(SPR OFF)
        MOV CX,DX

        ;XCENTER        ;XC=X-(WIDTH\2)
        MOV AX,[BP-2]   ;CENTER
        MOV BX,[BP+6]   ;GET CURRENT X COUNTER
        SUB BX,AX       ;
        MOV [BP+22],BX  ;SAVE IT TO STACK(SPR SEG)


                        ;MATRIX ROTATION FORMULA TIME!!!!!
                        ;NOTE:
                        ;WE NEED TO FIND THE X AND Y CENTER OF OUR SPRITE THEN
                        ;USE THIS FORMULAE(ANGLE=THETA)
                        ;=================
                        ;FOR THE RISE:Y
                        ;V=YC*COS(ANGLE)+XC*SIN(ANGLE)
                        ;FOR THE RUN: X
                        ;U=XC*COS(ANGLE)-YC*SIN(ANGLE)

       ;GET COSINE OF ANGLE(REMEMBER THAT IT WILL BE SAR'ED L8R

       ;WE GET U FIRST = RUN
       ;U=XC*COS(ANGLE)-YC*SIN(ANGLE)

       MOV AX,[BP+2]    ;COS*256
       IMUL BX,AX       ;BX=XC*(COS(ANGLE)*256))

       MOV AX,[BP]      ;SIN*256
       IMUL AX,CX       ;AX=YC*(SIN(ANGLE)*256)) 
      
       SUB BX,AX        ;U=XC-YC=COS-SIN(THANKS CHRIS/ALAN!!!!!!!!)
       SAR BX,8         ;SCALE IT DOWN LIKE AN IDIOT
       ADD BX,[BP+28]   ;BX=X+U         ;ROTATED X
       ADD BX,[BP-2]    ;REMOVE FOR CENTERED ROTATE

       ;CHECK IF WE ARE INSIDE BOUNDS ELSE SKIP PIXEL
       CMP BX,0
       JL LBL_SKIP_ZERO_R
       CMP BX,318              ;WE ARE STORING WORDS
       JGE LBL_SKIP_ZERO_R


       ;LETS GET V
       ;V=YC*COS(ANGLE)+XC*SIN(ANGLE)

       MOV DX,[BP+2]    ;COS*256
       IMUL CX,DX       ;CX=YC*(COS(ANGLE)*256))  ;OLDCX=XCENTER

       MOV AX,[BP+22]   ;XC
       MOV DX,[BP]      ;SIN*256
       IMUL AX,DX       ;AX=XC*(SIN(ANGLE*256))
       ADD AX,CX        ;V=AX=COS+SIN  ;GP BACK TO MATRIX FORMULA
       SAR AX,8         ;SCALE DOWN BY 256
       ADD AX,[BP+26]   ;ADD Y
       ADD AX,[BP-4]    ;REMOVE FOR CENTERED ROTATE

       ;CHECK RISE VALUE
       CMP AX,0
       JL LBL_SKIP_ZERO_R
       CMP AX,199
       JG LBL_SKIP_ZERO_R

       ;u AND v ARE OK LETS PUT IT DEST SEG....

;PLOT IT
;NOTES:  AX=Y(u),BX=X(v)
        MOV DX,AX
        XCHG DH,DL
        MOV DI,DX       ;DX=DX*256
        SHR DI,2        ;DI=DX\4=64
        ADD DI,DX
        ADD DI,BX       ;CENTERED ROTATE

                        ;PLOT IT
        MOV CX,GS
        MOV ES:[DI],CX  ;PLOT IT

LBL_SKIP_ZERO_R:
        MOV CX,[BP+6]           ;GET CURRENT X COUNTER STATUS
        INC CX                  ;ADD 1 TO X COUNTER
        MOV [BP+6],CX           ;UPDATE X COUNTER
        CMP CX,[BP+10]          ;CHECK IF OVER THE WIDTH
        JL LBL_ROTATE_PIXEL_R   ;NEXT PIXEL

        MOV CX,[BP+4]           ;Y COUNTER
        INC CX
        CMP CX,[BP+8]           ;HEIGHT
        JGE LBL_END_IT_R        ;IF WE ARE OVER THEN EXIT

        MOV [BP+4],CX
        MOV WORD PTR [BP+6],0   ;NEXT SCANLINE
        JMP LBL_ROTATE_PIXEL_R  ;NEXT PIXEL

LBL_END_IT_R:

ADD SP,16
POP BP
POP DS

RET 12

RELSPRITEROTATE ENDP

;/===========================================================================
;SUB RELSPRITEROTATETRANS (BYVAL DESTSEG%,BYVAL X%,BYVAL Y%,BYVAL ANGLE%,BYVAL SPRSEG%,BYVAL SPROFF%)

;STACK

;DEST SEG               =30
;X                      =28
;Y                      =26
;ANGLE                  =24
;SPRSEG                 =22
;SPROFF                 =20
;QB RETSEG              =18
;QB RETOFF              =16
;DS                     =14
;BP                     =12
;SPR WIDTH              =10
;SPR HEIGHT             =8
;XCOUNTER               =6
;YCOUNTER               =4
;COS(ANGLE)             =2
;SIN(ANGLE)             =0
;XCENTER                =-2
;YCENTER                =-4


;ES:DI                  =DEST
;DS:SI                  =SPRITE

PUBLIC RELSPRITEROTATETRANS
RELSPRITEROTATETRANS PROC

;SAVE SOME IMPORTANT VALS TO STACK 
PUSH DS
PUSH BP

;SUBRACT SP BY 12 TO ADD PSEUDO REGS
XOR EAX,EAX
PUSH EAX
PUSH EAX
PUSH EAX

MOV BP,SP

MOV ES,[BP+30]          ;POINT DEST SEG TO ES(STANDARD IN ALL OF RELLIB)
PUSH EAX                ;TOO LAZY TO REORGANIZE STACK ;-)

;SAVE SIN AND COS VALUES TO STACK FOR LATER USE

;SIN
MOV SI,[BP+24]
SHL SI,1
MOV AX,SINE[SI]          ;GET THE VALUE FROM OUR NIFTY TABLE(*256)
MOV [BP],AX

;COS

MOV SI,[BP+24]          ;ANGLE
SHL SI,1                ;REFERENCE ANGLE POINTER TO CORRECT DATA AS ITS DW
MOV AX,COSINE[SI]       ;GET THE VALUE FROM OUR NIFTY TABLE(*256)
MOV [BP+2],AX           ;SAVE IT


;SPRITE NOW

MOV DS,[BP+22]          ;SPRITE SEGMENT
MOV SI,[BP+20]          ;OFFSET OF SPR

MOV AX,[SI]             ;WIDTH

SHR AX,3                ;CORRECT OUR WIDTH
MOV BX,[SI+2]           ;HEIGHT    
MOV [BP+10],AX          ;SAVE WIDTH
MOV [BP+8],BX           ;SAVE HEIGHT
ADD SI,4                ;SI POINTS TO FIRST PIXEL FO SPR

;YCENTER        ;YC=Y-(HEIGHT\2)
SHR BX,1        ;GET THE CENTER
MOV [BP-4],BX   ;SAVE TO STACK      
;XCENTER
SHR AX,1        ;GET THE CENTER
MOV [BP-2],AX   ;SAVE TO STACK      


LBL_ROTATE_PIXEL_RT:

        MOV CL,[SI]     ;COLOR
        INC SI          ;POINT IT TO NEXT COLOR
        CMP CL,0
        JE LBL_SKIP_ZERO_RT

        MOV CH,CL       ;FOR PADDING PURPOSES
        MOV GS,CX       ;SAVE THE COLORS



        ;LET'S GET THE CENTERS AND PUT THEM ON THE STACK

        ;YCENTER        ;YC=Y-(HEIGHT\2)
        MOV AX,[BP-4]   ;GET CENTER
        MOV DX,[BP+4]   ;GET CURRENT Y COUNTER
        SUB DX,AX       ;
        MOV [BP+20],DX  ;SAVE IT TO STACK(SPR OFF)
        MOV CX,DX

        ;XCENTER        ;XC=X-(WIDTH\2)
        MOV AX,[BP-2]   ;CENTER
        MOV BX,[BP+6]   ;GET CURRENT X COUNTER
        SUB BX,AX       ;
        MOV [BP+22],BX  ;SAVE IT TO STACK(SPR SEG)


                        ;MATRIX ROTATION FORMULA TIME!!!!!
                        ;NOTE:
                        ;WE NEED TO FIND THE X AND Y CENTER OF OUR SPRITE THEN
                        ;USE THIS FORMULAE(ANGLE=THETA)
                        ;=================
                        ;FOR THE RISE:Y
                        ;V=YC*COS(ANGLE)+XC*SIN(ANGLE)
                        ;FOR THE RUN: X
                        ;U=XC*COS(ANGLE)-YC*SIN(ANGLE)

       ;GET COSINE OF ANGLE(REMEMBER THAT IT WILL BE SAR'ED L8R

       ;WE GET U FIRST = RUN
       ;U=XC*COS(ANGLE)-YC*SIN(ANGLE)

       MOV AX,[BP+2]    ;COS*256
       IMUL BX,AX       ;BX=XC*(COS(ANGLE)*256))

       MOV AX,[BP]      ;SIN*256
       IMUL AX,CX       ;AX=YC*(SIN(ANGLE)*256)) 
      
       SUB BX,AX        ;U=XC-YC=COS-SIN(THANKS CHRIS/ALAN!!!!!!!!)
       SAR BX,8         ;SCALE IT DOWN LIKE AN IDIOT
       ADD BX,[BP+28]   ;BX=X+U         ;ROTATED X
       ADD BX,[BP-2]    ;REMOVE FOR CENTERED ROTATE

       ;CHECK IF WE ARE INSIDE BOUNDS ELSE SKIP PIXEL
       CMP BX,0
       JL LBL_SKIP_ZERO_RT
       CMP BX,318              ;WE ARE STORING WORDS
       JGE LBL_SKIP_ZERO_RT


       ;LETS GET V
       ;V=YC*COS(ANGLE)+XC*SIN(ANGLE)

       MOV DX,[BP+2]    ;COS*256
       IMUL CX,DX       ;CX=YC*(COS(ANGLE)*256))  ;OLDCX=XCENTER

       MOV AX,[BP+22]   ;XC
       MOV DX,[BP]      ;SIN*256
       IMUL AX,DX       ;AX=XC*(SIN(ANGLE*256))
       ADD AX,CX        ;V=AX=COS+SIN  ;GP BACK TO MATRIX FORMULA
       SAR AX,8         ;SCALE DOWN BY 256
       ADD AX,[BP+26]   ;ADD Y
       ADD AX,[BP-4]    ;REMOVE FOR CENTERED ROTATE

       ;CHECK RISE VALUE
       CMP AX,0
       JL LBL_SKIP_ZERO_RT
       CMP AX,199
       JG LBL_SKIP_ZERO_RT

       ;u AND v ARE OK LETS PUT IT DEST SEG....

;PLOT IT
;NOTES:  AX=Y(u),BX=X(v)
        MOV DX,AX
        XCHG DH,DL
        MOV DI,DX       ;DX=DX*256
        SHR DI,2        ;DI=DX\4=64
        ADD DI,DX
        ADD DI,BX       ;CENTERED ROTATE

        MOV CX,GS       ;COLOR OF SPR PIXEL
        MOV AL,ES:[DI]  ;COLOR OF DEST
        MOV AH,ES:[DI+2]

;TRANSLUCENT PLOTTING
;NOTES AX=COLOR OF DEST
;BX=PIXEL COLOR
        MOV BX,CX
        AND AL,15
        AND AH,15
        AND CL,15
        AND CH,15
        ;FIRST COLOR
        ADD AL,CL
        SHR AL,1
        SUB BL,CL       ;BL=BASE COLOR
        ADD BL,AL
        ;SECOND COLOR
        ADD AH,CH
        SHR AH,1
        SUB BH,CH
        ADD BH,AH

        MOV ES:[DI],BX  ;PLOT IT

LBL_SKIP_ZERO_RT:
        MOV CX,[BP+6]           ;GET CURRENT X COUNTER STATUS
        INC CX                  ;ADD 1 TO X COUNTER
        MOV [BP+6],CX           ;UPDATE X COUNTER
        CMP CX,[BP+10]          ;CHECK IF OVER THE WIDTH
        JL LBL_ROTATE_PIXEL_RT   ;NEXT PIXEL

        MOV CX,[BP+4]           ;Y COUNTER
        INC CX
        CMP CX,[BP+8]           ;HEIGHT
        JGE LBL_END_IT_RT        ;IF WE ARE OVER THEN EXIT

        MOV [BP+4],CX
        MOV WORD PTR [BP+6],0   ;NEXT SCANLINE
        JMP LBL_ROTATE_PIXEL_RT  ;NEXT PIXEL

LBL_END_IT_RT:

ADD SP,16
POP BP
POP DS

RET 12

RELSPRITEROTATETRANS ENDP


END
