�  ��	 
   �  ��    �)     R)  D    �  B	  �E 
        =   �  !�}
                  �	  �� 
       ,  � 	  �� 
         �  	  �� 
       '      	  �� 
         �  	  �� 
       -      	 �
            
  �-
       3     	  �I
       	- 	
 $  	  �e
       
, 
 R 	  ��
         m  	  ��
         w  	  ��
         �  	  ��
         �  	  ��
       .     	  �
       .     	  �)
       .     	  �E
       .       �a
       4   �    �  
       &   �    �i 
       2           a Skins) Location of palette files (Palette Path): *.PAL File Description: Associated With: Palette Description: Apply Cancel, -------------------------------------------- Current Palette Settings: Disktop: Compression Tools: Properties: Printing Tool:	 Browse... OK� Skins�	PathLbl�PalFilesLst�
PalPathTxt�	FileDescrLbl	AssocLbl	
ProgramLbl 	
PaletteLbl/	PalDescrLbl;ApplyButH	CancelButS	LineLblb	SettingsLblp	
DisktopLbl	CompressLbl�	PropertiesLbl�	PrintLbl�	DisktopPalLbl�	CompressPalLbl�	PropertiesPalLbl�	PrintPalLbl�	BrowseBut  OkBut        0Q��$ ����� � �� c X  � ��V Rf  4� Cq�   � y �  �   +                  �   	Form_Loadq  
PalPathTxt� TEXT�  PalPath� DefaultDrive6 
WindosPath 	IconsPathk GraphicsPath� 	FontsPath   ScrPath* DesktopPath@ ShortcutsPathJ TempPath ArchPath� KeysPath� ScreenSaverX ScrDelay   AnyKey   Editor� 
Resolution   
Background� DefIcon� ObjectIndex   FALSEk TRUE# NMALLOC PalFilesLst   PATH   CloseBut_Click  ScrSav`Options, ENABLED   Skinsz PalFilesLst_Click� AddSlash FILENAME   File� Strng7 ProgramF Description� DescrLen� 
ProgramLbl   CAPTION   PalDescrLbl   ApplyBut_Click� 
DisktopPal� CompressPal� PropertiesPal� PrintPalg DisktopPalLbl   CompressPalLbl� PropertiesPalLbl   PrintPalLbl� FormCall   BrowseBut_Click   Index  ChDirectory PalPathTxt_Change   CancelBut_Click�PalPathChangeError�AfterPalPathChangeError  OpenPalFileErrorCReadPalFileError� AfterReadError+AfterReadPalFileError   OkBut_Click   	CheckPath   	Directory  BadPalPathErrorQAfterBadPalPathError   
PalFileLsto PalFileLst.ENABLED   Temp  AfterOpenPalFileError�  D  X     D  z     �    � 4  �    � �  �    � 
 common.bi'  � 5   Copyright (C) HiTech Laboratories, Denis Andrianov.-  � 2   Common declarations for most modules in OPTIONS.  �    L=      � � ���    � � ���    � ���    � ���    � $���    � 4���    � D���    � T���    � d���    � t��   � ���   � ���y      � ����   � ����   � ����   � ����     � ���   � ���*     � ��6   �  ��@     � 4��J   � D��X   � T��f     � h��q   � x��    �    L-   # d �  #  �nt �    D  � #      D  ��       	  ��������. &  A 	 Form_Load 0�   v  V        �  y  � �  ���    y  c  q     � � `  � � `  � � `  � � `   � +  U �     �    L=  ��   � � �  m Error reading palette path."���  m +    Q 	  v ������       PalFilesLst_Click 0�  v  �        c  q E+g      ��  g  �  � �     ���    d!N,    ��  � ,�     � F�    ���    FN7    ��  � 7�     ���       � �    ��  � �    ���    o    , R `  7 k `    U �    �    L=  N�  m Error opening file."���  m o   ���  m Error reading file."���  m �    Q 	  � ������/       ApplyBut_Click 8�   v  z       R `Et &  m DISKTOP";   �  � ` m COMPRESS;   �  � ` m PROPERTY;   �  � ` m PRINT";   �  � ` R     Q 	  ��������      BrowseBut_Click 0,   v         d   4��	     Q 	  ��������       PalPathTxt_Change 0�   v  C       �k   c  q  � �  ���     � � �     � �    m   R `  m   k `    m   � `  m   � `  m   � `  m   � `    U �     �    L=  ��k   � � �  m �    Q 	  B ������        CancelBut_Click 8.   v  X        ��   � � �    Q 	  ��������      OkBut_Click 0�  v  �        c  q E+ c  q    c  q g       g �a     � `-a   � `-a    � `-a  ( � `-a    ( � `� ( � `� ( � `� ( � `� (gy  (8   X (U �   (P   P  P  P    P       m Error. Possible reasons:e 	 m; 1. The directory either doesn't exist or isn't a full path." e 	 m# 2. The skins haven't been selected." dm Error"�    Q 	  ��������!     