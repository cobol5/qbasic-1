'컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
' R e l L i b   u n d o c u m e n t e d   v e r s i o n   5 . 0 a
' (**SPECIAL** QuickBASIC Caliber Programming Compo Version)
'컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
' Presented to you by Richard Eric M. Lope and Adigun Azikiwe Polack.



' Greetings and God __BLESS__ you all, wonderful QuickBASIC 4.5 users!!!  :D
'
' Please enjoy this lib!!  ;*) !  Now, be sure to put the remark marks ( ' )
' _in front_ of ONLY the commands that you do not need in your program that
' involves RelLib and this _very_ .BI file.  Thank you and ***MOST EXCELLENT
' PROGRAMMING*** to you in QuickBASIC!!!  :D !!!
'
'
'
'                                                    - Adigun Azikiwe Polack
'                                         One of the Founders of "Aura Flow"
'             Official Founder of the "QuickBASIC Caliber Programming Compo"
'                                                         September 19, 2003






DEFINT A-Z
'/======================SCREEN SWITCHING===================================
DECLARE SUB RelInitText ()
DECLARE SUB RelInitVGA13 ()
DECLARE SUB RelInit320x133 ()    '*
DECLARE SUB RelInit320x100 ()    '*
DECLARE SUB RelInit320x80 ()     '*
DECLARE SUB RelInit320x67 ()     '*
DECLARE SUB RelInit320x57 ()     '*
'/======================SCREEN BUFFER======================================
DECLARE SUB RelPCopy (BYVAL DestSeg%, BYVAL SrcSeg%)
DECLARE SUB RelPcopyTrans (BYVAL DestSeg%, BYVAL SrcSeg%)
DECLARE SUB RelPcopyTransB (BYVAL DestSeg%, BYVAL SrcSeg%)
DECLARE SUB RelPcopyBlended (BYVAL DestSeg%, BYVAL SrcSeg%)
DECLARE SUB RelPCopySF (BYVAL DestSeg%, BYVAL SrcSeg%)
DECLARE SUB RelPCopyGamma (BYVAL DestSeg%, BYVAL SrcSeg%, BYVAL GammaVal%)
DECLARE SUB RelCompress (BYVAL DestSeg%, BYVAL DestOff%, BYVAL SrcSeg%)
DECLARE SUB RelXCompress (BYVAL DestSeg%, BYVAL DestOff%, BYVAL SrcSeg%, BYVAL SrcOff%, BYVAL PERCENT%)
DECLARE SUB RelCLS (BYVAL DestSeg%, BYVAL C%)
DECLARE SUB RelPageSwap (BYVAL DestSeg%, BYVAL SrcSeg%)
DECLARE SUB RelCopyLineH (BYVAL DestSeg%, BYVAL SrcSeg%, BYVAL Ypos%)
DECLARE SUB RelCopyLineV (BYVAL DestSeg%, BYVAL SrcSeg%, BYVAL Xpos%)
DECLARE SUB RelCopyBlock (BYVAL DestSeg%, BYVAL SrcSeg%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%)
DECLARE SUB RelGammaBlock (BYVAL DestSeg%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL GammaVal%)
DECLARE SUB RelWrapX (BYVAL DestSeg%, BYVAL SrcSeg%, BYVAL x%)
'/======================DRAWING PRIMITIVES=================================
DECLARE SUB RelPset (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL C%)
DECLARE SUB RelPsetF (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL C%)
DECLARE SUB RelPsetTrans (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL C%)
DECLARE SUB RelBoxF (BYVAL DestSeg%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL C%)
DECLARE SUB RelBoxTransF (BYVAL DestSeg%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL C%)
DECLARE SUB RelBoxTrans (BYVAL DestSeg%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL C%)
DECLARE SUB RelBox (BYVAL DestSeg%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL C%)
DECLARE SUB RelLine (BYVAL DestSeg%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL C%)
DECLARE SUB RelLineTrans (BYVAL DestSeg%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL C%)
DECLARE SUB RelLineH (BYVAL DestSeg%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL C%)
DECLARE SUB RelLineV (BYVAL DestSeg%, BYVAL x1%, BYVAL y1%, BYVAL y2%, BYVAL C%)
DECLARE SUB RelLineTransV (BYVAL DestSeg%, BYVAL x1%, BYVAL y1%, BYVAL y2%, BYVAL C%)
DECLARE SUB RelLineTransH (BYVAL DestSeg%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL C%)
DECLARE SUB RelCircle (DestSeg%, Xcenter%, Ycenter%, size%, col%)    '*
DECLARE SUB RelCircleF (DestSeg%, Xcenter%, Ycenter%, size%, col%)   '*
DECLARE SUB RelCircleT (DestSeg%, Xcenter%, Ycenter%, size%, col%)   '*
DECLARE SUB RelCircleFT (DestSeg%, Xcenter%, Ycenter%, size%, col%)  '*
DECLARE SUB RelCircleI (DestSeg%, Xcenter%, Ycenter%, size%, col%, Impose%)   '*
DECLARE SUB RelCircleFI (DestSeg%, Xcenter%, Ycenter%, size%, col%, Impose%)  '*
DECLARE SUB RelEllipse (DestSeg%, Xcenter%, Ycenter%, Xrad%, Yrad%, col%)     '*
DECLARE SUB RelEllipseF (DestSeg%, Xcenter%, Ycenter%, Xrad%, Yrad%, col%)    '*
DECLARE SUB RelEllipseT (DestSeg%, Xcenter%, Ycenter%, Xrad%, Yrad%, col%)    '*
DECLARE SUB RelEllipseFT (DestSeg%, Xcenter%, Ycenter%, Xrad%, Yrad%, col%)   '*
DECLARE SUB RelEllipseI (DestSeg%, Xcenter%, Ycenter%, Xrad%, Yrad%, col%, Impose%)    '*
DECLARE SUB RelEllipseFI (DestSeg%, Xcenter%, Ycenter%, Xrad%, Yrad%, col%, Impose%)   '*
DECLARE SUB RelBorder (DestSeg%, x%, y%, col%)  '*
DECLARE SUB RelBorderH (DestSeg%, x%, col%)     '*
DECLARE SUB RelBorderV (DestSeg%, y%, col%)     '*
DECLARE SUB RelBorderTrans (DestSeg%, x%, y%, col%)  '*
DECLARE SUB RelBorderTransH (DestSeg%, x%, col%)     '*
DECLARE SUB RelBorderTransV (DestSeg%, y%, col%)     '*

'/======================SPRITE=============================================
DECLARE SUB RelSprite (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL SPRITESEGMENT%, BYVAL SPRITEOFFSET%)
DECLARE SUB RelSpriteSolid (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL SPRITESEGMENT%, BYVAL SPRITEOFFSET%)
DECLARE SUB RelSpriteFast (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL SPRITESEGMENT%, BYVAL SPRITEOFFSET%)
DECLARE SUB RelSpriteTrans (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL SPRITESEGMENT%, BYVAL SPRITEOFFSET%)
DECLARE SUB RelSpriteSF (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL SPRITESEGMENT%, BYVAL SPRITEOFFSET%)
DECLARE SUB RelSpriteColor (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL SPRITESEGMENT%, BYVAL SPRITEOFFSET%, BYVAL C%)
DECLARE SUB RelSpriteColorX (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL SPRITESEGMENT%, BYVAL SPRITEOFFSET%, BYVAL C%, BYVAL STARTC%)
DECLARE SUB RelSpriteSolidX (BYVAL DestSeg%, BYVAL SrcSeg%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL NewX%, BYVAL NewY%)
DECLARE SUB RelSpriteGamma (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL SPRITESEGMENT%, BYVAL SPRITEOFFSET%, BYVAL GammaVal%)
DECLARE SUB RelSpriteRotate (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL Angle%, BYVAL SprSeg%, BYVAL SprOff%)
DECLARE SUB RelSpriteRotateTrans (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL Angle%, BYVAL SprSeg%, BYVAL SprOff%)
DECLARE SUB RelSpriteHiLite (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL SPRITESEGMENT%, BYVAL SPRITEOFFSET%, BYVAL GammaVal%)
DECLARE SUB RelSpriteShadow (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL SPRITESEGMENT%, BYVAL SPRITEOFFSET%, BYVAL GammaVal%)
DECLARE SUB RelSpriteFlip (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL SPRITESEGMENT%, BYVAL SPRITEOFFSET%, BYVAL FlipMode%)
DECLARE SUB RelSpriteFlipT (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL SPRITESEGMENT%, BYVAL SPRITEOFFSET%, BYVAL FlipMode%)
DECLARE SUB RelSpriteFlipG (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL SPRITESEGMENT%, BYVAL SPRITEOFFSET%, BYVAL FlipMode%, BYVAL GammaVal%)
DECLARE SUB RelSpriteFlipC (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL SPRITESEGMENT%, BYVAL SPRITEOFFSET%, BYVAL FlipMode%, BYVAL clr%)
DECLARE SUB RelGet (BYVAL DestSeg%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL SprSeg%, BYVAL SprOff%)
'/======================COLLISION DETECTION=================================
DECLARE FUNCTION RelCollide% (BYVAL DestSeg%, BYVAL x%, BYVAL y%, BYVAL SPRITESEGMENT%, BYVAL SPRITEOFFSET%)
DECLARE FUNCTION RelCollideSpr% (BYVAL SPR1X%, BYVAL SPR1Y%, BYVAL SPR1SEGMENT%, BYVAL SPR1OFFSET%, BYVAL SPR2X%, BYVAL SPR2Y%, BYVAL SPR2SEGMENT%, BYVAL SPR2OFFSET%)
DECLARE FUNCTION RelCollideSprB% (BYVAL SPR1X%, BYVAL SPR1Y%, BYVAL SPR1SEGMENT%, BYVAL SPR1OFFSET%, BYVAL SPR2X%, BYVAL SPR2Y%, BYVAL SPR2SEGMENT%, BYVAL SPR2OFFSET%)
'/======================KEYBOARD============================================
DECLARE SUB RelKeyBoardON ()
DECLARE SUB RelKeyBoardOFF ()
DECLARE FUNCTION RelKey% (BYVAL Scancode%)
'/======================FONT================================================
DECLARE SUB RelPrint (DestSeg%, x%, y%, Font$, Mincolor%, Italic%)
DECLARE SUB RelPrintB (DestSeg%, x%, y%, Font$, Mincolor%, Italic%)  '*
DECLARE SUB RelPrintC (DestSeg%, x%, y%, Font$, col%, Italic%)       '*
DECLARE SUB RelPrintS (DestSeg%, x%, y%, Font$, Mincolor%, Xscale%, Yscale%, Italic%)
DECLARE SUB RelPrintSB (DestSeg%, x%, y%, Font$, Mincolor%, Xscale%, Yscale%, Italic%)   '*
DECLARE SUB RelPrintSC (DestSeg%, x%, y%, Font$, col%, Xscale%, Yscale%, Italic%)        '*
DECLARE SUB RelPrintScoreL (DestSeg%, Score#, x%, y%, Mincolor%, Italic%, Shade%)        '*
DECLARE SUB RelPrintScoreR (DestSeg%, Score#, x%, y%, Mincolor%, Italic%, Shade%)        '*
DECLARE SUB RelPrnScShadowSL (DestSeg%, Score#, x%, y%, Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%, Shade%)  '*
DECLARE SUB RelPrnScShadowSR (DestSeg%, Score#, x%, y%, Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%, Shade%)  '*
DECLARE SUB RelPrnScShadowL (DestSeg%, Score#, x%, y%, Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%, Shade%)  '*
DECLARE SUB RelPrnScShadowR (DestSeg%, Score#, x%, y%, Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%, Shade%)  '*
DECLARE SUB RelPrintShadow (DestSeg%, x%, y%, Font$, Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%)   '*
DECLARE SUB RelPrintShadowB (DestSeg%, x%, y%, Font$, Mincolor%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%)  '*
DECLARE SUB RelPrintShadowC (DestSeg%, x%, y%, Font$, col%, Italic%, ShaX%, ShaY%, Shadcol%, ShadTrans%)          '*
DECLARE SUB RelPrintShadowS (DestSeg%, x%, y%, Font$, Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%)   '*
DECLARE SUB RelPrintShadowSB (DestSeg%, x%, y%, Font$, Mincolor%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, ShadMinCol%, ShadTrans%)  '*
DECLARE SUB RelPrintShadowSC (DestSeg%, x%, y%, Font$, col%, Xscale%, Yscale%, Italic%, ShaX%, ShaY%, Shadcol%, ShadTrans%)          '*
DECLARE SUB RelPrintTrans (DestSeg%, x%, y%, Font$, Mincolor%, Italic%)
DECLARE SUB RelPrintTransB (DestSeg%, x%, y%, Font$, Mincolor%, Italic%)  '*
DECLARE SUB RelPrintTransC (DestSeg%, x%, y%, Font$, col%, Italic%)       '*
DECLARE SUB RelPrintTransS (DestSeg%, x%, y%, Font$, Mincolor%, Xscale%, Yscale%, Italic%)
DECLARE SUB RelPrintTransSB (DestSeg%, x%, y%, Font$, Mincolor%, Xscale%, Yscale%, Italic%)  '*
DECLARE SUB RelPrintTransSC (DestSeg%, x%, y%, Font$, col%, Xscale%, Yscale%, Italic%)       '*
DECLARE SUB RelPrintES (DestSeg%, x%, y%, Font$, col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%)   '*
DECLARE SUB RelPrintESB (DestSeg%, x%, y%, Font$, col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%)  '*
DECLARE SUB RelPrintESC (DestSeg%, x%, y%, Font$, col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%)  '*
DECLARE SUB RelPrintScoreESL (DestSeg%, Score#, x%, y%, col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%, Shade%)   '*
DECLARE SUB RelPrintScoreESR (DestSeg%, Score#, x%, y%, col%, Shadcol%, Xscale%, Yscale%, Trans%, ShadTrans%, Italic%, Shade%)   '*
DECLARE SUB RelPrintScoreSL (DestSeg%, Score#, x%, y%, Mincolor%, Xscale%, Yscale%, Italic%, Shade%)   '*
DECLARE SUB RelPrintScoreSR (DestSeg%, Score#, x%, y%, Mincolor%, Xscale%, Yscale%, Italic%, Shade%)   '*
DECLARE SUB RelPrintTrScoreSL (DestSeg%, Score#, x%, y%, Mincolor%, Xscale%, Yscale%, Italic%, Shade%)   '*
DECLARE SUB RelPrintTrScoreSR (DestSeg%, Score#, x%, y%, Mincolor%, Xscale%, Yscale%, Italic%, Shade%)   '*
DECLARE SUB RelPrintTrScoreL (DestSeg%, Score#, x%, y%, Mincolor%, Italic%, Shade%)  '*
DECLARE SUB RelPrintTrScoreR (DestSeg%, Score#, x%, y%, Mincolor%, Italic%, Shade%)  '*

DECLARE SUB RelScore256 (DestSeg%, x%, y%, Score&, Centered%, FontArray%(), FontArrayIndex%())
DECLARE SUB RelScoreGamma256 (DestSeg%, x%, y%, Score&, Centered%, FontArray%(), FontArrayIndex%(), Gamma%)   '
DECLARE SUB RelScoreTrans256 (DestSeg%, x%, y%, Score&, Centered%, FontArray%(), FontArrayIndex%())
DECLARE SUB RelSc256X (DestSeg%, x%, y%, Score#, FontArray%(), FontArrayIndex%(), FontPos%, TransSwitch%)   '*
DECLARE SUB RelScGamma256X (DestSeg%, x%, y%, Score#, FontArray%(), FontArrayIndex%(), FontPos%, Gamma%)    '*
DECLARE SUB RelFontFixedT256 (DestSeg%, x%, y%, Text$, Centered%, FontArray%(), FontArrayIndex%())
DECLARE SUB RelFontFixed256 (DestSeg%, x%, y%, Text$, Centered%, FontArray%(), FontArrayIndex%())
DECLARE SUB RelFontGamma256 (DestSeg%, x%, y%, Text$, Centered%, FontArray%(), FontArrayIndex%(), Gamma%)    '
DECLARE SUB RelFontGamma256X (DestSeg%, x%, y%, Text$, Centered%, FontArray%(), FontArrayIndex%(), Gamma%, Spacing!)   '*
DECLARE SUB RelFontTrans256 (DestSeg%, x%, y%, Text$, Centered%, FontArray%(), FontArrayIndex%())
DECLARE SUB RelFontTrans256X (DestSeg%, x%, y%, Text$, Centered%, FontArray%(), FontArrayIndex%(), Spacing!)    '*
DECLARE SUB RelFont256 (DestSeg%, x%, y%, Text$, Centered%, FontArray%(), FontArrayIndex%())
DECLARE SUB RelFont256X (DestSeg%, x%, y%, Text$, Centered%, FontArray%(), FontArrayIndex%(), Spacing!)         '*
DECLARE SUB RelScGamma256XZ (DestSeg%, x%, y%, Score#, FontArray%(), FontArrayIndex%(), FontPos%, Gamma%, GammaZeros%, RemoveExcZeros%)   '*
DECLARE SUB RelSc256XZ (DestSeg%, x%, y%, Score#, FontArray%(), FontArrayIndex%(), FontPos%, TransSwitch%, TransZeros%, RemoveExcZeros%)  '*
DECLARE SUB RelFontGamma256WX (DestSeg%, x%, y%, Text$, Centered%, FontArray%(), FontArrayIndex%(), Gamma%, TypeAmount%, Spacing!)        '*
DECLARE SUB RelFontGamma256W (DestSeg%, x%, y%, Text$, Centered%, FontArray%(), FontArrayIndex%(), Gamma%, TypeAmount%)                   '*
DECLARE SUB RelFontTrans256WX (DestSeg%, x%, y%, Text$, Centered%, FontArray%(), FontArrayIndex%(), TypeAmount%, Spacing!)                '*
DECLARE SUB RelFont256WX (DestSeg%, x%, y%, Text$, Centered%, FontArray%(), FontArrayIndex%(), TypeAmount%, Spacing!)                     '*
DECLARE SUB RelFont256W (DestSeg%, x%, y%, Text$, Centered%, FontArray%(), FontArrayIndex%(), TypeAmount%)                                '*
'/======================MEMORY==============================================
DECLARE SUB RelMemCopy (BYVAL DestSeg%, BYVAL DestOff%, BYVAL SrcSeg%, BYVAL SrcOff%, BYVAL NumBytes%)
DECLARE SUB RelAddHI (BYVAL SEGINTVAL%, BYVAL OFFINTVAL%, BYVAL BYTEVAL%)
DECLARE SUB RelAddLOW (BYVAL SEGINTVAL%, BYVAL OFFINTVAL%, BYVAL BYTEVAL%)
DECLARE FUNCTION RelGetHI (BYVAL Intval%)
DECLARE FUNCTION RelGetLow (BYVAL Intval%)
'/======================MOUSE===============================================
DECLARE SUB RelMouseShow ()
DECLARE SUB RelMouseHide ()
DECLARE SUB RelMouseReset ()
DECLARE SUB RelMouseLimitXY (BYVAL MinX%, BYVAL MinY%, BYVAL MaxX%, BYVAL MaxY%)
DECLARE SUB RelMouseSetXY (BYVAL x%, BYVAL y%)
DECLARE FUNCTION RelMouseX% ()
DECLARE FUNCTION RelMouseY% ()
DECLARE FUNCTION RelMouseClick% ()
DECLARE FUNCTION RelMouseDetect% ()
DECLARE FUNCTION RelMouseInside (BYVAL MinX%, BYVAL MinY%, BYVAL MaxX%, BYVAL MaxY%)
'/======================IMAGE LOADING=======================================
DECLARE FUNCTION RelLoadBMP% (Layer%, x%, y%, File$, Pal$, SwitchPal%)
DECLARE FUNCTION RelLoadPCX% (Layer%, x%, y%, File$, Pal$, SwitchPal%)
'/======================IMAGE SAVING========================================
'The following command is added by Adigun Azikiwe Polack.

DECLARE SUB RelSavePic (Layer%, FileName$, ImageType%, Gr.Mode%)   '*

'----------------------For "ImageType%":
CONST RAW = 1
CONST RAW.PAL = 2
CONST BSV = 3
CONST BSV.PAL = 4
CONST BMP = 5
CONST BMP.RLE = 6
CONST CLP = 7
CONST TGA = 8
CONST GIF = 9
CONST BMP.OS2 = 10
CONST RAS = 11
CONST WMF = 12
CONST TIF = 13
CONST COMexe = 14
'---------------------------------------

'------------------------For "Gr.Mode%":
CONST Gr.320x200 = 0
CONST Gr.320x133 = 1
CONST Gr.320x100 = 2
CONST Gr.320x80 = 3
CONST Gr.320x67 = 4
CONST Gr.320x57 = 5
'---------------------------------------


'/======================PALETTES============================================
DECLARE SUB RelReadRGB (ColorVal%, r%, G%, B%)
DECLARE SUB RelWriteRGB (ColorNum%, r%, G%, B%)
DECLARE SUB RelReadPal (PalString$)
DECLARE SUB RelFade (r%, G%, B%, Counts%)
DECLARE SUB RelFadeStep (StartCol%, EndCol%, r%, G%, B%)
DECLARE SUB RelFadeToNegative (Counts%)                        '*
DECLARE SUB RelFadeToNegativeX (StartCol%, EndCol%, Counts%)   '*
DECLARE SUB RelFadeToPal (PalString$, Counts%)
DECLARE SUB RelFadeToPalStep (StartCol%, EndCol%, PalString$)
DECLARE SUB RelFadeToPalX (StartCol%, EndCol%, PalString$, Counts%)
DECLARE SUB RelFadeX (StartCol%, EndCol%, r%, G%, B%, Counts%)
DECLARE SUB RelLoadPal (File$, PalString$, Switch%)
DECLARE SUB RelLoadPalPP256 (File$)
DECLARE SUB RelNegativePal ()
DECLARE SUB RelNegativePalX (StartCol%, EndCol%)               '*
DECLARE SUB RelRotatePalB (StartCol%, EndCol%)
DECLARE SUB RelRotatePalF (StartCol%, EndCol%)
DECLARE SUB RelSavePal (File$)
DECLARE SUB RelSwitchToPal (PalString$)

'/======================OnSprite SUBS/FUNCTION==============================
'Special procedures Allegro Style!!! Joakim's subs :*)
DECLARE SUB RelPsetOnSprite (BYVAL DESTSprSEG%, BYVAL DESTSprOFF%, BYVAL x%, BYVAL y%, BYVAL C%)
DECLARE SUB RelPsetOnSpriteT (BYVAL DESTSprSEG%, BYVAL DESTSprOFF%, BYVAL x%, BYVAL y%, BYVAL C%)
DECLARE SUB RelSpriteOnSprite (BYVAL DESTSprSEG%, BYVAL DESTSprOFF%, BYVAL x%, BYVAL y%, BYVAL SprSeg%, BYVAL SprOff%)
DECLARE SUB RelSpriteOnSpriteS (BYVAL DESTSprSEG%, BYVAL DESTSprOFF%, BYVAL x%, BYVAL y%, BYVAL SprSeg%, BYVAL SprOff%)
DECLARE FUNCTION RelPointOnSprite% (BYVAL DESTSprSEG%, BYVAL DESTSprOFF%, BYVAL x%, BYVAL y%)

'/======================XMS SUBS/FUNCTION===================================
DECLARE FUNCTION RelXmsInit% ()
DECLARE FUNCTION RelXmsGetFree& ()
DECLARE FUNCTION RelXmsGetTotal& ()
DECLARE FUNCTION RelXmsAllocate% (BYVAL AmountKbs&)
DECLARE SUB RelXmsDeallocate (BYVAL XmsHandle%)
DECLARE SUB RelXMSFromBase (BYVAL BaseSeg%, BYVAL BaseOFF&, BYVAL XmsHandle%, BYVAL XMSOffset&, BYVAL Length&)
DECLARE SUB RelXMStoBase (BYVAL BaseSeg%, BYVAL BaseOFF&, BYVAL XmsHandle%, BYVAL XMSOffset&, BYVAL Length&)
DECLARE SUB RelXMSPut (BYVAL BaseSeg%, BYVAL x%, BYVAL y%, BYVAL SprSeg%, BYVAL SprOff%, BYVAL XmsHandle%, BYVAL DummySeg%)

'/======================Misc GFX============================================
DECLARE SUB RelWater (BYVAL DestSeg%, BYVAL SourceSeg%)
DECLARE SUB RelRefrac (BYVAL DestSeg%, BYVAL SourceSeg%, BYVAL TextSeg%)
DECLARE SUB RelSmooth (BYVAL Layer%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%)
DECLARE SUB RelMosaic (DestSeg%, x1%, y1%, x2%, y2%, size%)   '*
DECLARE SUB RelPsetWu (Layer%, x!, y!, clr%)
DECLARE SUB RelScanlinesH (Layer%, x1%, y1%, x2%, y2%, col%, LineLoc%, Impose%)   '*
DECLARE SUB RelScanlinesV (Layer%, x1%, y1%, x2%, y2%, col%, LineLoc%, Impose%)   '*
'/======================Mode 7================================================
DECLARE SUB RelMode7Map (Layer%, x1%, y1%, x2%, y2%, Angle%, scalex%, scaley%, scalez%, Horz%, camx!, camy!, Texture%(), TextureOffset%, Map%())
DECLARE SUB RelMode7 (Layer%, x1%, y1%, x2%, y2%, Angle%, scalex%, scaley%, scalez%, Horz%, camx!, camy!, Texture%(), TextureOffset%)
'/======================FUNCTIONS===========================================
DECLARE FUNCTION RelSize% (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%)
DECLARE FUNCTION RelPoint% (BYVAL DestSeg%, BYVAL x%, BYVAL y%)
DECLARE FUNCTION RelInside% (BYVAL x%, BYVAL y%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%)
DECLARE FUNCTION RelInsideC% (BYVAL x%, BYVAL y%, BYVAL XC%, BYVAL YC%, BYVAL Radius%)
DECLARE FUNCTION RelAngle% (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%)
'/======================File================================================
DECLARE FUNCTION RelFindFile$ (Mask$, Attribute%)
'/======================MISC================================================
DECLARE SUB RelWait ()
DECLARE SUB RelDelay (BYVAL Msec%)
DECLARE SUB RelFfix ()


'/======================CONSTANTS===========================================
'KEY CONSTANTS
CONST KEYESC = 1, KEYENTER = 28, KEYSPACE = 57, KEYTAB = &HF
CONST KEYUP = 72, KEYDOWN = 80, KEYLEFT = 75, KEYRIGHT = 77

'Of course!!!!! :)
CONST FALSE = 0, TRUE = NOT FALSE
CONST VIDEO = &HA000

'FlipMode(for use with RelSpriteFlip)
CONST FLIPNONE = 0, FLIPH = 1, FLIPV = 2, FLIPVH = 3

'File Find
CONST VOLUMELABEL = &H8, DIRECTORY = &H10, ARCHIVE = &H20
CONST READONLY = &H1, HIDDEN = &H2, SYSTEMFILE = &H4
                                           
'misc...

                                             



'/================ P P 2 5 6   I M A G E   L O A D I N G  P R O C E D U R E S
'                                / added for Rellib by: Adigun Azikiwe POLACK
'/ (Special thanks to CHRIS CHADWICK for the PP256 IMAGE LOADING ROUTINES!!!)
DECLARE SUB InitImageData (FileName$, ImageArray%())        '*
DECLARE SUB MakeImageIndex (ImageArray(), IndexArray())     '*


'/=============== F A S T   G A M E P A D / J O Y S T I C K   R O U T I N E S
'                                / added for Rellib by: Adigun Azikiwe POLACK
'/ (Special thanks to Phillip Jay Cohen and SJ Zero for these HOT routines!!!)
DECLARE SUB RelJoyDetect (Success.or.Fail)                  '*
DECLARE SUB RelJoy (JoyX%, JoyY%)                           '*
DECLARE SUB RelJoyTrig (but1%, but2%, but3%, but4%)         '*

CONST Success = 1
CONST Fail = 0



'* = Added by Adigun Azikiwe Polack.
' = Were *originally* created by Adigun Azikiwe Polack.


'/===========================================================================

'ATTENTION ALL RELLIB 4.1 USERS!  Here are the following changes that were
'made, *DESPITE* the documentations in the "Docs" directory themselves.

' DECLARE SUB RelFontGamma256 (DestSeg%, x%, y%, Text$, Centered%,
'             ~~~~~~~~~~~~~~~  FontArray%(), FontArrayIndex%(), Gamma%)

' DECLARE SUB RelScoreGamma256 (DestSeg%, x%, y%, Score&, Centered%,
'             ~~~~~~~~~~~~~~~~  FontArray%(), FontArrayIndex%(), Gamma%)


'                                                     - Adigun Azikiwe POLACK

'/===========================================================================

