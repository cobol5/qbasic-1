'
'
'                                  SETUP
'
'               Do run this software before starting Battlecraft
'
'
'
'
'
'
'
'
'
'
'
'                             лллллллллллллллл
'                             лSeah Boon Siewл
'                             лллллллллллллллл
ON TIMER(1) GOSUB x
TIMER ON
PRINT "Initialising Computer Speed"
DO
delay = delay + 1
LOOP UNTIL yuni = 1
TIMER OFF
OPEN "timer.dat" FOR OUTPUT AS #1
PRINT #1, delay
CLOSE #1
PRINT "Setup Complete!"
k$ = INPUT$(1)
RUN "present.bas"
x:
yuni = 1
RETURN

