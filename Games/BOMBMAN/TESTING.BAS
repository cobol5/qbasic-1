CLS : SCREEN 13
FOR EX = 200 TO 220
PALETTE EX, EX - 180
NEXT EX
PALETTE 1, 460557
PALETTE 2, 856606
PALETTE 3, 1318445
PALETTE 4, 2370616
PALETTE 5, 3881787
PALETTE 6, 45
PALETTE 7, 1315604
PALETTE 8, 3543820
PALETTE 9, 1520918
PALETTE 10, 50
PALETTE 11, 141370
PALETTE 12, 1317660
PALETTE 13, 2621440
PALETTE 14, 2625038
PALETTE 15, (63 + 63 * 256 + 63 * 65536)
PALETTE 16, 2627865
PALETTE 17, 3025193
PALETTE 18, 60
PALETTE 19, 16191
PALETTE 20, 256 * 40
PALETTE 22, 1318445
PALETTE 23, 1520918
PALETTE 24, 1317660
PALETTE 25, 141370
PALETTE 26, 1380666
PALETTE 27, 4128831
PALETTE 28, 3025193
PALETTE 29, 2627865
PALETTE 30, 2887459
PALETTE 31, 2621440
PALETTE 32, 2625038
PALETTE 33, 16191
PALETTE 34, 3543820
PALETTE 35, 1315604
PALETTE 36, 3881787
PALETTE 37, 856606

PALETTE 0, 0
PALETTE 1, 460557
PALETTE 2, 856606
PALETTE 3, 1318445
PALETTE 4, 2370616
PALETTE 5, 3881787
PALETTE 6, 45
PALETTE 7, 1315604
PALETTE 8, 3543820
PALETTE 9, 1520918
PALETTE 10, 50
PALETTE 11, 141370
PALETTE 12, 1317660
PALETTE 13, 2621440
PALETTE 14, 2625038
PALETTE 15, (63 + 63 * 256 + 63 * 65536)
PALETTE 16, 2627865
PALETTE 17, 3025193
PALETTE 18, 60
PALETTE 19, 16191
PALETTE 20, 256 * 40
PALETTE 21, 6975
PALETTE 22, 256 * 20
PALETTE 30, 65536 * 60 + 256 * 30
DIM F15(1 TO 2000), F25(1 TO 2000)
 DEF SEG = VARSEG(F15(1))
 BLOAD "C:\T1\" + A$ + ".GRH", VARPTR(F15(1))
 DEF SEG
 DEF SEG = VARSEG(F25(1))
 BLOAD "C:\T2\" + A$ + ".GRH", VARPTR(F25(1))
 DEF SEG

