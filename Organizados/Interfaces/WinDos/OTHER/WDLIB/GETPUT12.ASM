;Copyright (C) HiTech Laboratories, Denis Andrianov
;=============================================================================

.model medium,basic
.286


.data
SOffset   dw   0
xRead     dw   0
yRead     dw   0
xSkip     dw   0




.code
;=============================================================================
;DECLARE SUB Get12 (BYVAL Seg1%, BYVAL Seg2%, BYVAL Seg3%, BYVAL Seg4%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%)

Public Get12
Get12 Proc

;************************************
;Save used registers.

push   bp
mov    bp,sp

push   ds
push   es
push   si
push   di

;************************************
;Prepare and copy.

call   far ptr FillScreenVars


mov    ax,0a000h           ;Set source segment (video buffer)
mov    ds,ax

mov    ax,[bp+20]          ;Set destination segment (Seg1)
mov    es,ax
xor    bl,bl
call   far ptr Video2Array ;Save first bit plane

mov    ax,[bp+18]          ;Set destination segment (Seg2)
mov    es,ax
mov    bl,1
call   far ptr Video2Array ;Save second bit plane

mov    ax,[bp+16]          ;Set destination segment (Seg3)
mov    es,ax
mov    bl,2
call   far ptr Video2Array ;Save third bit plane

mov    ax,[bp+14]          ;Set destination segment (Seg4)
mov    es,ax
mov    bl,3
call   far ptr Video2Array ;Save fourth bit plane


;************************************
;Restore saved registers and exit the procedure.

pop    di
pop    si
pop    es
pop    ds
pop    bp

ret    16

;************************************
;************************************
;Copy bytes from video to specified segment.
;Parameters: BL = bit plane number.

Video2Array LABEL FAR

mov    dx,3ceh             ;Command to the video
mov    al,4                ;controller to select
out    dx,al               ;a bit plane.
mov    dx,3cfh
mov    al,bl               ;Bit plane number.
out    dx,al

push   ds
mov    ax,@data
mov    ds,ax

mov    si,SOffset
xor    di,di
mov    ax,xSkip
mov    bx,xRead
mov    cx,yRead
cld                        ;Set SI and DI increment

pop    ds


GetLine:
  push   cx
  mov    cx,bx             ;cx = xRead
  rep    movsb
  pop    cx
  add    si,ax             ;si = si + xSkip
loop   GetLine

ret


Get12 ENDP




;=============================================================================
;DECLARE SUB Put12 (BYVAL Seg1%, BYVAL Seg2%, BYVAL Seg3%, BYVAL Seg4%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%)

Public Put12
Put12 Proc

;************************************
;Save used registers.

push   bp
mov    bp,sp

push   ds
push   es
push   si
push   di

;************************************
;Prepare and copy.

call   far ptr FillScreenVars


mov    ax,0a000h           ;Set destination segment (video buffer)
mov    es,ax

mov    ax,[bp+20]          ;Set source segment (Seg1)
mov    ds,ax
mov    bl,1
call   far ptr Array2Video ;Load first bit plane

mov    ax,[bp+18]          ;Set source segment (Seg2)
mov    ds,ax
mov    bl,2
call   far ptr Array2Video ;Load second bit plane

mov    ax,[bp+16]          ;Set source segment (Seg3)
mov    ds,ax
mov    bl,4
call   far ptr Array2Video ;Load third bit plane

mov    ax,[bp+14]          ;Set source segment (Seg4)
mov    ds,ax
mov    bl,8
call   far ptr Array2Video ;Load fourth bit plane


;************************************
;Restore saved registers and exit the procedure.

pop    di
pop    si
pop    es
pop    ds
pop    bp

ret    16

;************************************
;************************************
;Copy bytes from video to specified segment.
;Parameters: BL = bit plane number.

Array2Video LABEL FAR

mov    dx,3c4h             ;Command to the video
mov    al,2                ;controller to modify
out    dx,al               ;a bit plane.
mov    dx,3c5h
mov    al,bl
out    dx,al

push   ds
mov    ax,@data            ;Set our variables'
mov    ds,ax               ;segment.

xor    si,si
mov    di,SOffset
mov    ax,xSkip
mov    bx,xRead
mov    cx,yRead
cld                        ;Set SI and DI increment

pop    ds


PutLine:
  push   cx
  mov    cx,bx             ;cx = xRead
  rep    movsb
  pop    cx
  add    di,ax             ;di = di + xSkip
loop   PutLine

ret


Put12 ENDP




;=============================================================================
;*****************************INTERNAL PROCEDURES*****************************
;=============================================================================

;This procedure is used by Get12 and Put12.
;BP must be set up properly before calling!

;Stack Frame:
;y2%    bp+6
;x2%      +8
;y1%      +10
;x1%      +12
;Seg4%    +14
;Seg3%    +16
;Seg2%    +18
;Seg1%    +20


FillScreenVars Proc

push   ds
mov    ax,@data
mov    ds,ax

;***********************************
;Find the starting offset in the video buffer.
;Offset = y1%(640 / 8) + (x1% / 8)

mov    ax,[bp+10]          ;ax = y1%
mov    bx,640/8
mul    bx                  ;ax = y1%(640 / 8)
push   ax

xor    dx,dx
mov    ax,[bp+12]          ;ax = x1%
mov    bx,8
div    bx                  ;ax = x1% / 8

pop    bx
add    ax,bx
mov    SOffset,ax          ;SOffset = y1%(640 / 8) + (x1% / 8)

;************************************
;Find how many bytes to read in sequence.

mov    bx,8                ;Common divisor

xor    dx,dx
mov    ax,[bp+12]
div    bx                  ;ax = x1% / 8
push   ax

xor    dx,dx
mov    ax,[bp+8]
div    bx                  ;ax = x2% / 8

pop    bx
sub    ax,bx
inc    ax
mov    xRead,ax            ;xRead = (x2% / 8) - (x1% / 8) + 1

;************************************
;Find how many bytes to skip after reading each row.

mov    bx,640/8
sub    bx,ax
mov    xSkip,bx            ;xSkip = (640 / 8) - xRead

;************************************
;Find how many rows to read.

mov    ax,[bp+6]
mov    bx,[bp+10]
sub    ax,bx
inc    ax
mov    yRead,ax            ;yRead = y2% - y1% + 1

;************************************
;Restore registers and exit.

pop    ds
ret


FillScreenVars ENDP

;=============================================================================

END
