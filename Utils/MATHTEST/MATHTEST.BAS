PRINT "Testing your computers math speeds, please wait 5 seconds"
ON TIMER(5) GOSUB done
TIMER ON
DO
a& = a& + 1
LOOP

done:
        a& = a& / 5
        PRINT "Your computer can do "; a&; " calculations per second"
        SYSTEM

