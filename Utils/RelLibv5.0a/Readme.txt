--------------------------------------------------------------------
R E L L I B   -   U n d o c u m e n t e d   V e r s i o n   5 . 0 a
                  (**SPECIAL** EXCLUSIVE QuickBASIC Caliber
                   Programming Compo Version!!  ;*D ! )
--------------------------------------------------------------------
Presented and Designed by Richard Eric M. Lope (Relsoft) and
                          Adigun Azikiwe Polack.



Hi,

RelLib is a Screen 13 game development lib for QB45(should also work
with QB71 and VB dos). Pls. Read RelLib.DOC for more details.
Use WORDPAD to view RelLib.DOC!!!!


Eric 

PS.  The source for this LIB is open and you can use/abuse at your 
own risk.


___________________________________________________________________
(Additional notes from Adigun Azikiwe Polack):

HELLO AND GOD'S __FIERY__ ALL ETERNAL BLESSING TO YOU ALL!!!  ;) !!

In honor of the "QuickBASIC Caliber Programming Compo (Summer and
Autumn 2003)", I have just added **PLENTY** of new commands that
will blow your mind away in such _RAVISHINGLY INTENSE_ excitement
and then plenty!!!  ;) !!  Below are a WHOLE SHIPLOAD of all-new
commands that Almighty God has so enabled and empowered me to add
ON-PURPOSE to help you in your game development for QuickBASIC 4.5,
so don't you even miss them now!  :D !

Also, *please* see the "RelLib.bi" file for even more
information.  Thank you so vitally much and __VASTLY SPLENDID
PROGRAMMING TO YOU__ using the new RelLib Undocumented Version
5.0a!!  ;*) !


                                           
					    - Adigun Azikiwe Polack
        			 One of the Founders of "Aura Flow"
     Official Founder of the "QuickBASIC Caliber Programming Compo"
						 September 19, 2003
___________________________________________________________________


/=======================================================
September 19, 2003 (**SPECIAL** EXCLUSIVE QBCPC Version;
                    Undocumented Version 5.0a).

Added the following by Adigun Azikiwe Polack:
   
G R A P H I C S   M O D E S --------------------------------------
   - RelInit320x133 (inits a *NEW* Graphics Mode of 320x133!!)
   - RelInit320x100 (inits a *NEW* Graphics Mode of 320x100!!)
   - RelInit320x80  (inits a *NEW* Graphics Mode of 320x80!!)
   - RelInit320x67  (inits a *NEW* Graphics Mode of 320x67!!)
   - RelInit320x57  (inits a *NEW* Graphics Mode of 320x57!!) 
------------------------------------------------------------------
In total here, FIVE (5) new commands added.
------------------------------------------------------------------



D R A W I N G   P R I M I T I V E S ------------------------------
   - RelCircle
   - RelCircleF
   - RelCircleT
   - RelCircleFT
   - RelCircleI
   - RelCircleFI
   - RelEllipse
   - RelEllipseF
   - RelEllipseT
   - RelEllipseFT
   - RelEllipseI
   - RelEllipseFI
   - RelBorder
   - RelBorderH
   - RelBorderV
   - RelBorderTrans
   - RelBorderTransH
   - RelBorderTransV
------------------------------------------------------------------
In total here, EIGHTEEN (18) new commands added.
------------------------------------------------------------------



8 x 8   B I O S   F O N T   P R I N T I N G ----------------------
   - RelPrintB
   - RelPrintC
   - RelPrintSB
   - RelPrintSC
   - RelPrintScoreL
   - RelPrintScoreR
   - RelPrnScShadowSL
   - RelPrnScShadowSR
   - RelPrnScShadowL
   - RelPrnScShadowR
   - RelPrintShadow
   - RelPrintShadowB
   - RelPrintShadowC
   - RelPrintShadowS
   - RelPrintShadowSB
   - RelPrintShadowSC
   - RelPrintTransB
   - RelPrintTransC
   - RelPrintTransSB
   - RelPrintTransSC
   - RelPrintES
   - RelPrintESB
   - RelPrintESC
   - RelPrintScoreESL
   - RelPrintScoreESR
   - RelPrintScoreSL
   - RelPrintScoreSR
   - RelPrintTrScoreSL
   - RelPrintTrScoreSR
   - RelPrintTrScoreL
   - RelPrintTrScoreR
------------------------------------------------------------------
In total here, THIRTY-ONE (31) new commands added.
------------------------------------------------------------------



P P 2 5 6   F O N T   P R I N T I N G ----------------------------
   - RelSc256X
         (Displays score with a justification set at
         RIGHT.)
   - RelScGamma256X 
         (Displays score with a justification set at
         RIGHT, Gamma-enhanced!  :D )
   - RelFontGamma256X
   - RelFontTrans256X
   - RelFont256X
   - RelScGamma256XZ
         (Displays score with a justification set at
         RIGHT, Gamma-enhanced and *EVEN* lets you
         remove the excess zeros from the _unused_
         left portion of your score, too!!!
         ;*) !! )
   - RelSc256XZ      
         (Displays score with a justification set at
         RIGHT, Translucency-supported and *EVEN*
         lets you remove the excess zeros from the
         _unused_ left portion of your score, too!!!
         ;*) !! )
   - RelFontGamma256WX
   - RelFontGamma256W
   - RelFontTrans256WX
   - RelFont256WX
   - RelFont256W
------------------------------------------------------------------
In total here, TWELVE (12) new commands added.
------------------------------------------------------------------



I M A G E   S A V I N G ------------------------------------------
   - RelSavePic 
         (Lets you save your picture from *ANY*
          RelLib graphics resolution to any of the
          following:
             • .RAW
             • .RAW w/ palette
             • .BSV (BSAVE)
             • .BSV (BSAVE) w/ palette
             • .BMP (Windows standard BMP format)
             • .BMP (RLE-encoded BMP format) 
             • .CLP (Windows Clipboard format) 
             • .TGA (Truevision Targa format) 
             • .GIF (Compuserve GIF87a format) 
             • .BMP (OS/2-based BMP format) 
             • .RAS (Sun Raster Image;
                     grayscale only) 
             • .WMF (Windows Meta File format)
             • .TIF (Tagged Image File format;
                     grayscale only)
             • .COM (an EXECUTABLE with which
                     **actually** lets the user
                     view the image that was saved
                     in this _very_ format!
                     Perfect for still shots and
                     some opening screens, too!!!
                     ;*D !! )

SPECIAL THANKS to Dav and Rich Geldreich for helping me out in
  the programming of this very routine for RelLib!!!  ;D !!

------------------------------------------------------------------
In total here, ONE (1) new command added.
------------------------------------------------------------------



P A L E T T E   M A N I P U L A T I O N --------------------------
   - RelFadeToNegative
   - RelFadeToNegativeX
   - RelNegativePalX
------------------------------------------------------------------
In total here, THREE (3) new commands added.
------------------------------------------------------------------



M I S C E L L A N E O U S   G R A P H I C S   E F F E C T S ------
   - RelScanlinesH
   - RelScanlinesV
------------------------------------------------------------------
In total here, TWO (2) new commands added.
------------------------------------------------------------------



P P 2 5 6   I M A G E   L O A D I N G ----------------------------
   - InitImageData
   - MakeImageIndex

Special thanks to CHRIS CHADWICK for the routines!!!  ;) !

------------------------------------------------------------------
In total here, TWO (2) new commands added.
------------------------------------------------------------------



G A M E P A D / J O Y S T I C K ----------------------------------
   - RelJoyDetect
   - RelJoy
   - RelJoyTrig
------------------------------------------------------------------
In total here, THREE (3) new commands added.
------------------------------------------------------------------

OVERALL GRAND TOTAL:
          77 **ALL-NEW** ROUTINES!!!  :*D  ;) !!!
	  ~~~~~~~~~~~~~~~~~~~~~~~~~~

/=======================================================
Sept. 1 2003 (Version 4.1) beta.
1. Added Mode 7
2. Mode & map
3. RelPsetWU
4. RelSmooth

/=======================================================
July 9, 2003 (Version 4) beta.

1. 3 types of Docs. HTML, Wordpad, and pure text.
2. Added RelFindFiles

More Changes...June 30, 2003

1. Scrapped RelSpriteFlipV, RelspriteFlipH,
	RelSpriteFlipVH. to
	RELSPRITEFLIP which is all but you need to do 
	all the above routines.
2. RelSpriteFlipGAMMA
3. RelSpriteFlipTrans
4. Changed RelloadBMP to a Function
5. Added RelLoadPCX(also a Function)
	*error codes are in "RelLib.Doc"
6. Optimized some stuff.

	
Changes May 11, 2003 Ahh.. first update!!!
Happy Mom's day!!!!

1. Bugfix for RelGet.
2. Added:
	A) RelSpriteFlipH
	B) RelSpriteFlipV
	C) RelSpriteFlipVH	
	D) RelWrapX
	E) RelWrapY
	F) RelWater
	G) RelRefrac
	H) RelShadowPut
	I) RelHilitePut
3. Made RelBoxF and RelBoxTransF support true clipping
	and they are faster than ever!!!

/======================================================	
Changes November 6, 2002
Optimized RelLoadBMP by 1000% ;*)
Changed the calling convention to....
DECLARE SUB RelLoadBMP (DESTSEG%, File$, SwitchPal%)
sorry....

/======================================================	
Update!!!!!
June 25,2002
added RelFfix

/======================================================	
August ??, 2002
Xms Routines

September 22,2002
Added RelAngle


/=======================================================
Update!!!!
Ver 2.0(Joakim's version)

1. RelSpriteOnSprite
	*transparent PUT routine but uses the normal QB GET/PUT array
	as a destination so you can use your normal get/put array as 
	a double buffer. This is also autoclipped and as fast as 
	RelSprite. See example file(SpriteON.Bas)
2. RelSpriteOnSpriteS
	*Same as RelSpriteOnSprite but puts the sprite Solidly.
	Almost as fast as RelSpriteSolid
3. RelCollideSpr
	*An extremely fast PIXEL-Perfect collision detection.  I can't
	Notice an FPS decrease on my RPG engine when I used this 
	Instead of RelCollideSprB on a 486.  Pls. See Collide.EXE/BAS
	on the Examples folder.
4. RelPsetOnSprite
	*Psets a pixel on a QB get/put array
5. RelPsetOnSpriteT
	*Psets a pixel on a QB get/put array, Translucently.
6. RelPointOnSprite
	*Returns the color of the pixel on the get/put array.

/======================================================================
VER 1.1

What's New since the last update at QB45.com

1. Diagonal Line Routine(Normal and Translucent)
2. Sprite Rotations(Normal and Translucent)
3. RelInside, RelInsideC which are used for range checking in RPGs
4. Byte size variable emulators along with an example(Bytesamp.bas)
	RelAddHi
	RelAddLow
	RelGetHi
	RelGetLow
5. RelMouseReset
More or less 10 procedures. ;*)





