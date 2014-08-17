'   using formulas from the Astronomical
'   Almanac 1997
'
'   Keith Burnett, 1997
'
CLS
'   make sure everything runs in double precision
DEFDBL A-Z
'   define an integer part function that works
'   correctly for negative numbers
DEF FNipart (x) = SGN(x) * INT(ABS(x))
'   define a function to put angles back in range 0 to r
DEF FNrange (x, r)
    DO UNTIL x > 0
	x = x + r
    LOOP
    DO UNTIL x < r
	x = x - r
    LOOP
    FNrange = x
END DEF
'   define a function to find atn(y/x) in correct quadrant
DEF FNatn2 (y, x)
    a = ATN(y / x)
    IF x < 0 THEN a = a + pi
    IF (y < 0) AND (x > 0) THEN a = a + twopi
    FNatn2 = a
END DEF
'   define a function to find arc sine!
DEF FNasin (x)
    c = SQR(1 - x * x)
    t = x / c
    FNasin = ATN(t)
END DEF
'   define a function to find arc cos
DEF FNacos (x)
    IF x = 0 THEN x = .00000000000001#
    s = SQR(1 - x * x)
    t = s / x
    FNacos = ATN(x)
END DEF
'   define a few constants
pi = 4 * ATN(1)
twopi = 2 * pi
degs = 180 / pi
rads = pi / 180
'   define observer's position for the topocentric correction
'   Put your own long and lat here.
glong = -(1 + 55 / 60)'west negative
glat = 52.5'north positive
asl = 120
'   get details
INPUT "days since start of 1997 : ", days
INPUT "              hours (UT) : ", hrs
INPUT "              minutes    : ", mins
ut = hrs + mins / 60
j = -1096.5 + days + ut / 24
t = j / 36525#
PRINT "             julian date : "; j + 2451545
'   calculate siderial time (omitting powers above t^2 in the series)
lst = 100.46# + 36000.77# * t + glong + 15# * ut
lst = FNrange(lst, 360)
PRINT "                     lst : "; lst / 15
'   calculate the geocentric longitude - see page D46 of AA 1997
l = rads * 218.32# + rads * 481267.883# * t
l = l + rads * 6.29# * SIN(rads * FNrange((134.9# + 477198.85# * t), 360))
l = l - rads * 1.27# * SIN(rads * FNrange((259.2# - 413335.38# * t), 360))
l = l + rads * .66# * SIN(rads * FNrange((235.7# + 890534.23# * t), 360))
l = l + rads * .21# * SIN(rads * FNrange((269.9# + 954397.7# * t), 360))
l = l - rads * .19# * SIN(rads * FNrange((357.5# + 35999.05# * t), 360))
l = l - rads * .11# * SIN(rads * FNrange((186.6# + 966404.05# * t), 360))
l = FNrange(l, twopi)
PRINT "geocentric long  of Moon : "; degs * l
'   calculate the geocentric latitude
b = rads * 5.13# * SIN(rads * FNrange((93.3# + 483202.03# * t), 360))
b = b + rads * .28# * SIN(rads * FNrange((228.2# + 960400.87# * t), 360))
b = b - rads * .28# * SIN(rads * FNrange((318.3# + 6003.18# * t), 360))
b = b - rads * .17# * SIN(rads * FNrange((217.6# - 407332.2# * t), 360))
PRINT "  geocentric lat of Moon : "; degs * b
'   calculate the geocentric horizontal parallax
gp = rads * .9508
gp = gp + rads * .0518# * COS(rads * FNrange((134.9# + 477198.85# * t), 360))
gp = gp + rads * .0095# * COS(rads * FNrange((259.2# - 413335.38# * t), 360))
gp = gp + rads * .0078# * COS(rads * FNrange((235.7# + 890534.23# * t), 360))
gp = gp + rads * .0028# * COS(rads * FNrange((269.9# + 954397.7# * t), 360))
PRINT "     geocentric parallax : "; degs * gp
'   from the parallax, get the semidiameter and the radius vector
sd = .2725 * gp
rm = 1 / (SIN(gp))
PRINT "           semi-diameter : "; degs * sd
PRINT "        distance of Moon : "; rm
'   now find the geocentric direction cosines
x = COS(b) * COS(l)
y = .9175 * COS(b) * SIN(l) - .3978 * SIN(b)
z = .3978 * COS(b) * SIN(l) + .9175 * SIN(b)
gra = FNatn2(y, x)
gdec = FNasin(z)
PRINT "           geocentric RA : "; gra * degs / 15
PRINT "          geocentric DEC : "; degs * gdec
'   now find topocentric rectangular coordinates
x1 = rm * x - COS(rads * glat) * COS(rads * lst)
y1 = rm * y - COS(rads * glat) * SIN(rads * lst)
z1 = rm * z - SIN(rads * glat)
r1 = SQR(x1 * x1 + y1 * y1 + z1 * z1)
tra = FNatn2(y1, x1)
tdec = FNasin(z1 / r1)
tp = FNasin(1 / r1)
sd1 = .2725 * tp
PRINT "          topocentric RA : "; tra * degs / 15
PRINT "         topocentric DEC : "; degs * tdec
PRINT "    topocentric parallax : "; degs * tp
PRINT "   topocentric semi-diam : "; degs * sd1
PRINT "    topocentric distance : "; r1
END

