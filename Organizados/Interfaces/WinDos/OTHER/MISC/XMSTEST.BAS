'$INCLUDE: 'future.bi'

Set640x480 16
Future.LoadBMP "c:\program\basprog\windos\graphics\frame256.bmp", 0, 0

temp% = IsXMSInstalled%
handle% = AllocateXMS(340000)

Future.XMSGet 0, 0, 522, 322, handle%

OPEN "frame.icn" FOR BINARY AS #1

'32766
temp$ = ""

  DO UNTIL BytePos& >= 340000
  MoveFromXMS handle%, VARSEG(temp$), VARPTR(temp$), 32766, BytePos&
  PUT #1, BytePos& + 1, temp$
    IF 340000 - BytePos& < 32766 THEN
      BytePos& = BytePos& + (340000 - BytePos&)
    ELSE
      BytePos& = BytePos& + 32766
    END IF
  LOOP

CLOSE
ResetScreen
DeallocateXMS handle%

