'$INCLUDE: '..\future.bi'

DIM buffer AS STRING

buffer = SPACE$(30000)

Set640x480 16
'Future.LoadBMP "c:\program\basprog\windos\graphics\shrtcuts.bmp", 0, 0
'Future.CLS 0

  IF NOT IsXMSInstalled% THEN
  PRINT "XMS driver is not installed."
  END
  END IF

OPEN "c:\program\basprog\windos\mytemp\frame.pic" FOR BINARY AS #1
GET #1, , buffer
CLOSE #1

XMSHandle% = AllocateXMS%(340000)

StrSeg% = VARSEG(buffer)
StrOff% = SADD(buffer)
MoveToXMS XMSHandle%, StrSeg%, StrOff%, 30000&, 0

Future.XMSPut 0, 0, XMSHandle%

DeallocateXMS XMSHandle%


SLEEP
resetscreen

