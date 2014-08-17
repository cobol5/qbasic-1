DECLARE FUNCTION MitNull$ (Zahl%, stellen%)  'Zahl mit wÑhlbaren Stellen mit
                                             'fÅhrenden Nullen darstellen
DECLARE SUB MakeSound ()                     'Speaker Åber I/O-Port ansteuern
DECLARE SUB SaveIni ()                       'Initialisierungsdaten abspeichern
DECLARE SUB Optionen ()                      'Optionen-Einstellfenster anzeig.
DECLARE SUB Hilfe ()                         '2 Hilfe-Bildschirme anzeigen
DECLARE SUB DirectSound (herz%, sec!)        'Ton direkt auf Speaker ausgeben
DECLARE FUNCTION WeekDay% (Mon%, Day%, Year%) 'Wochentag ermitteln

'****************************************************************************
' CLOCKFIX.BAS = QBasic-Programm mit Uhr, Wochentag, Datum und Wecker
' ===========================================================================
' Eigenschaften von ClockFix:
' - Komfortable Digital-Uhr mit akustischer und optischer Weckfunktion
' - LÑuft unter Windows in einem kleinen DOS-Teilfenster (halber SCREEN 0)
' - Zeigt Uhrzeit, Wochentag, Datum und Weckzeit an
' - Einfache Bedienung:
'     - Weckzeit wird Åber Cursortasten eingestellt und bei Verlassen des
'       Programms in der Datei CLOCKFIX.INI gesichert
'     - Die Leer-Taste schaltet den Alarm ein/aus
'     - AusfÅhrliche Hilfe Åber F1-Taste aufrufbar
'     - Optionsfenster durch F2-Taste aufrufbar
' - Der Alarmton ist ein normalerweise dezentes 10-maliges Piepsen des
'   Piepsers wÑhrend 1 Minute.
' - Die LautstÑrke und LÑnge des Wecktons ist im Optionsfenster Ñnderbar
' - ZusÑtzlich escheint an den FensterrÑndern eine rote Blinkanzeige als
'   Nachweckfunktion. So wird man auch dann auf den Alarm aufmerksam gemacht,
'   wenn man zwischenzeitlich nicht am PC war. Die LÑnge der optischen Nach-
'   weckfunktion ist ebenfalls im Optionenfenster einstellbar.
' - Die PIF-Datei CLOCKFIX.PIF sorgt dafÅr, da· das Programm unter Windows
'   auch dann in Hintergrund weiterlÑuft, wenn ein anderes Fenster aktiv ist.
'
' Die Anzeige erfolgt in einem Textbildschirm SCREEN 0 mit halber Spalten-
' zahl 40*25. Dieser Bildschirm wird im Dos-Teilfenster von Win3.1 und 95/98
' sehr platzsparend in einem Fenster halber Grî·e angezeigt.
'
' Credits 1: Dank an Garry Spencer,   gspencer@stim.tec.tn.us    fÅr den aus
'            Garry's Pogramm WEEKDAY.BAS entnommene Function WEEKDAY "to
'            calculate the day of the week when given the date in integer
'            form: Mon%, Day%, Year% (year: 1582 to 2450)
'            Note: Returns (0=Sunday...6=Saturday)  or -1 if an error occurs"
'
' Credits 2: Dank an James Vahn fÅr die Idee zur Subroutine DirectSound, die
'            der ABC-Collection, Snippet 'PC SPEAKER FREQUENCY' entnommen wur-
'            de.
'
' To-Do-Liste:  Verbesserungsmîglichkeiten (Ideensammlung fÅr Version 2.x)
'          - akustische Nachweckfunktion als 3.Alternative zu Alarm ein/aus
'            (z.B. Nachwecken nach 5 min)
'          - Weckton wahlweise auch Kikeriki oder Kuckuck aus Soundblaster
'            und Big-Ben-Schlag aus PC-Speaker
'          - WÑhlbare Farbe fÅr Digitalanzeige, getrennt fÅr Uhr- u.Weckzeit
'          - Stoppuhr mit RESET, START, STOP-Buttons
'          - Countdown-Timer mit Stellmîglichkeiten fÅr Minuten u.Sekunden
'                sowie START und RESET-Buttons
'          - Jahr 2000 Timer
'          - fÅhrende Nullen ausblenden bei Uhr- und Weckzeit (?)
'          - PIF-Datei fÅr Windows zum Weiterlaufen im Hintergrund
'          - Installationsprogramm fÅr Windows-OberflÑche mit PIF-Datei
'          - eventuell mit Mausbedienung
'          - Farbe der Balken (Kopf/Fu·zeile und Warnbalken) einstell
'            bar machen
'
' (c) Thomas Antoni, 19.10.99 - 2212.99, Version V1.3
'       thomas.antoni@erlf.siemens.de
'****************************************************************************

'------------- Deklaration der globalen Variablen ---------------------------
'Grî·en, die in der Initialisierungsdatei CLOCKFIX.INI abgelegt werden.
'Auf diese Variablen greifen die SUBs 'Optionen', 'SaveIni' u.'MakeSound' zu.
COMMON SHARED whour%         'Weckzeit, Stunde, 0...23
COMMON SHARED wmin%          'Weckzeit, Minuten, 0...59
COMMON SHARED wein%          '1=Weckalarm EIN, 0=Weckalarm AUS
COMMON SHARED weckdauer%     'Weckdauer, 1...60 min
COMMON SHARED weckton%       'Art des Wecktons 0...3= zaghaft bis laut
COMMON SHARED nachweckdauer% 'optische Nachweckdauer, 0...800 min

'-------------- Deklaration der Konstanten ----------------------------------
CONST uhrzeile% = 4      'Bildschirmposition der 7-Segment-Uhrzeitanzeige
CONST uhrspalte% = 5

CONST weckzeile% = 19    'Bildschirmposition fÅr Weckanzeige
CONST weckspalte% = 15

'------- Zeichengenerator-Felder fÅr die Siebensegment-Anzeigen laden -------
DIM digit$(0 TO 10, 1 TO 4)         'Zeichengenerator fÅr normal gro·e Digits
DIM baby$(0 TO 10, 1 TO 4)          'Dito fÅr kleine Baby-Digits
'             |         |
'  Zahlen 0-9 und ":"   Zeile innerhalb eines Zeichens

FOR ziffer% = 0 TO 10               'normal gro·e Digits
  FOR zeile% = 1 TO 4
    READ digit$(ziffer%, zeile%)
  NEXT zeile%
NEXT ziffer%

FOR ziffer% = 0 TO 10               'kleine Baby-Digits
  baby$(ziffer%, 1) = ""
  FOR zeile% = 2 TO 4
    READ baby$(ziffer%, zeile%)
  NEXT zeile%
NEXT ziffer%

'---------------- Initialisierungsdatei CLOCKFIX.INI lesen -----------------
ON ERROR GOTO neu                   'Fehlerbearbeitung "nicht vorhandene
                                    'Datei" (kommt bei kompiliertem Programm)
OPEN "clockfix.ini" FOR INPUT AS #1
IF LOF(1) = 0 THEN                  'ini-Datei existiert noch nicht (LÑnge =
                                    '0 Bytes (funktioniert nur bei BAS-Progr.)
  whour% = 12                       'Vorbesetzg.wenn Clockfix.ini nicht vorh.:
  wmin% = 30                        '- Weckzeit auf 12:30h vorbesetzen
  weckdauer% = 1                    '- 1 min Weckdauer
  nachweckdauer% = 120              '- 2h optishe blinkende Nachweckdauer
ELSE
  INPUT #1, x$, whour%, wmin%, wein%, weckdauer%, weckton%, nachweckdauer%
                                  'Initialisierungswerte aus Datei Åbernehmen
END IF
CLOSE #1
GOTO weiter
neu:
  whour% = 12                       'Vorbesetzg.wenn Clockfix.ini nicht vorh.:
  wmin% = 30                        '- Weckzeit auf 12:30h vorbesetzen
  weckdauer% = 1                    '- 1 min Weckdauer
  nachweckdauer% = 120              '- 2h optishe blinkende Nachweckdauer
weiter:
stellmin% = -1                      'Vorbesetzung: Minuten stellen
gespeichert% = -1                   'Vorbesetzung: Ini-Datei abgespeichert

anfang:
'-------------- Statische (unverÑnderliche) Bildelemente anzeigen -----------
'--------- Kopfzeile ---------------
WIDTH 40, 25                        'Halbe Spaltenzahl , halb-breites DOS-
                                    'Fenster unter Windows
COLOR 0, 7: CLS                     'Schwarz auf hellgrau
COLOR 15, 4                         'Kopfzeile wei· auf rot
PRINT " ClockFix V1.3   (c) Thomas Antoni 1999 ";

'--------- Weckerfeld --------------
'öberschrift, Kasten u.Bedienhinweise:
COLOR 0, 7                          'Schwarz auf hellgrau
LOCATE 13, 2: PRINT "⁄ƒƒƒƒƒƒƒƒƒƒƒ W E C K E R ƒƒƒƒƒƒƒƒƒƒƒƒø"
LOCATE 14, 2: PRINT "≥                                    ≥"
LOCATE 15, 2: PRINT "≥ Alarm EIN/AUS    Wecker  stellen   ≥"
LOCATE 16, 2: PRINT "≥ [Leertaste]      [Cursor-Tasten]   ≥"
LOCATE 17, 2: PRINT "≥": LOCATE 17, 39: PRINT "≥";
LOCATE 18, 2: PRINT "≥": LOCATE 18, 39: PRINT "≥";
LOCATE 19, 2: PRINT "≥": LOCATE 19, 39: PRINT "≥";
LOCATE 20, 2: PRINT "≥": LOCATE 20, 39: PRINT "≥";
LOCATE 21, 2: PRINT "≥": LOCATE 21, 39: PRINT "≥";
LOCATE 22, 2: PRINT "¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ";

'Linien zu Alarm Ein/Aus:
LOCATE 17, 5: PRINT "ƒƒƒƒ¬ƒƒƒƒ"
LOCATE 18, 9: PRINT "≥"

'---- Fu·zeile mit Bedienhinweisen ----------
LOCATE 25, 1
COLOR 15, 4
PRINT " Ende [Esc]   Optionen [F2]  Hilfe [F1] ";

'-------------------- Tasten abfragen --------------------------------------
DO
  COLOR 0, 7            'schwarz auf hellgrau
  up% = 0: dn% = 0      'Vorbesetzung: Cursor hoch/tief-Taste nicht betÑtigt
  taste$ = INKEY$

'---- Weckzeit, wein% und Optionen in Datei sich- ---
'---- ern, wenn 5 sec keine Taste betÑtigt ----------
  IF taste$ = "" THEN
    IF tastealt$ <> "" THEN          'Taste gerade losgelassen
      starttime! = TIMER             '10-sec-Timer starten
    ELSE                             'Taste schon lÑnger losgelassen
      IF (TIMER - starttime! > 5) AND NOT gespeichert% THEN
                                     'Taste schon seit 5 sec losgelassen?
        CALL SaveIni                 'Initialisierungswerte in Datei sichern
        gespeichert% = -1            'Merker "Ini schon gespeichert = True
      END IF                         '(nach Ablauf der 5 sec. soll nur 1x
                                     'gespeichert werden)
    END IF
  ELSE
    gespeichert% = 0                 'Merker "ini schon gespeichert" rÅcksetzen
  END IF
  tastealt$ = taste$

'---- Tasten auswerten ------------------------------
   SELECT CASE taste$
     CASE CHR$(0) + "H": up% = -1       'Cursor hoch-Taste betÑtigt
     CASE CHR$(0) + "P": dn% = -1       'Cursor tief-Taste bestÑtigt
     CASE CHR$(0) + "M": stellmin% = -1 'Cursor rechts-Taste betÑtigt
     CASE CHR$(0) + "K": stellmin% = 0  'Cursor links-Taste betÑtigt
     CASE CHR$(32)                      'Leertaste betÑtigt
       wein% = NOT wein%                'Wecker EIN/AUS toggeln
       nachweck% = 0                    'Nachweckperiode beenden
       nachweckgestartet% = 0           'Merker "Nachweckperiode gestartet"
                                        'rÅcksetzen
'       weck% = 0                        'Weckperiode beenden
       weckcounter% = 0                  'Weck-Piepszeit auf "abgelaufen"
                                        'stellen
       weckgestartet% = 0               'Merker "Weckperiode gestartet" rÅcks.
       COLOR 0, 7                       'schwarz auf hellgrau
       LOCATE weckzeile%, 6: PRINT "       "
       LOCATE , 6: PRINT "       "      'gro·es Alarm-Anzeigefeld lîschen
       LOCATE , 6: PRINT "       "

       '--- eventuell gerade aktive optische
       '--- Nachweckanzeige lîschen
       COLOR 0, 7                  'schwarz auf hellgrau, keine Seitenbalken
       FOR zeile% = 2 TO 24
         LOCATE zeile%, 1: PRINT " ";
         LOCATE zeile%, 40: PRINT " ";
       NEXT zeile%

     CASE CHR$(0) + CHR$(59)            'F1-Taste betÑtigt
       CALL Hilfe                       'Hilfe-Fenster anzeigen
       GOTO anfang
     CASE CHR$(0) + CHR$(60)            'F2-Taste betÑtigt
       CALL Optionen                    'Optionen-Einstellfenster anzeigen
       GOTO anfang
     CASE CHR$(27)                      'Esc-Taste betÑtigt
       COLOR 15, 0                      'normalen DOS-S/W-Bildschirm 80*25
       WIDTH 80, 25: CLS                'wiederherstellen
       LOCATE 12, 30: PRINT "..... und TschÅ·"
     END
     'SYSTEM
    
   END SELECT

'----- Uhrzeit als gro·e Digitaluhr mit 7-Segmentanzeige anzeigen -----------
  hour10% = VAL(LEFT$(TIME$, 1))       'Zehnerstellen der Uhrzeit HH:MM:SS
  hour1% = VAL(MID$(TIME$, 2, 1))      '... als Dezimalstellen isolieren
  min10% = VAL(MID$(TIME$, 4, 1))
  min1% = VAL(MID$(TIME$, 5.1))
  sec10% = VAL(MID$(TIME$, 7, 1))
  sec1% = VAL(RIGHT$(TIME$, 1))

  FOR zeile% = 1 TO 4                  'Digitalanzeige zeilenweise ausgeben
    LOCATE uhrzeile% + zeile% - 1, uhrspalte%
    PRINT digit$(hour10%, zeile%);
    PRINT digit$(hour1%, zeile%);
    PRINT digit$(10, zeile%);          'Doppelpunkt
    PRINT digit$(min10%, zeile%);
    PRINT digit$(min1%, zeile%);
    PRINT digit$(10, zeile%);
    PRINT baby$(sec10%, zeile%);
    PRINT baby$(sec1%, zeile%)
  NEXT zeile%

'------ Wochentag und Datum anzeigen ---------------------------------------
  d$ = DATE$
  tag% = VAL(MID$(d$, 4, 2))
  monat% = VAL(LEFT$(d$, 2))
  jahr% = VAL(RIGHT$(d$, 4))

  SELECT CASE WeekDay%(monat%, tag%, jahr%) 'Wochentag ermitteln
    CASE 0: LOCATE 9, 12: PRINT "Sonntag ";       '... und mittig anzeigen
    CASE 1: LOCATE 9, 12: PRINT "Montag ";
    CASE 2: LOCATE 9, 11: PRINT "Dienstag ";
    CASE 3: LOCATE 9, 12: PRINT "Mittwoch ";
    CASE 4: LOCATE 9, 10: PRINT "Donnerstag ";
    CASE 5: LOCATE 9, 12: PRINT "Freitag ";
    CASE 6: LOCATE 9, 12: PRINT "Samstag ";
  END SELECT

  PRINT USING "##."; tag%; monat%;          'Datum dd.mm.jjjj anzeigen
  PRINT USING "####"; jahr%;

'--------------------- Wecker EIN/AUS-Feld anzeigen -------------------------
' 3 Modi:
'   (1) Wecker Ein ==> EIN-Anzeige in rotem Feld
'   (2) Wecker Ein und Nachweckperiode lÑuft
'                  ==> blinkende ALARM-Anzeige in rotem Feld
'   (3) Wecker Aus ==> AUS-Anzeige in grÅnem Feld

  IF wein% THEN                     'Wecker ein ?
    IF NOT nachweck% THEN           'keine Nachweckperiode ==> Normalanzeige

  '-- Modus (1) ---
      COLOR 15, 4                   'Wecker-EIN-Anzeige wei· auf rot
      LOCATE weckzeile%, 7: PRINT "     "
      LOCATE , 7: PRINT " EIN "
      COLOR 4, 7
      LOCATE , 7: PRINT "ﬂﬂﬂﬂﬂ"

  '--- Modus (2) ---
    ELSEIF sec1% MOD 2 = 0 THEN     'Nachweckperiode lÑuft ==> Blinkanzeige

      '--- gerade sec ==> keine ALARM-Anzeige im Recht-
      '--- eck, Alarmanzeige in den Seitenbalken
      COLOR 0, 7                    'schwarz auf hellgrau
      LOCATE weckzeile%, 6: PRINT "⁄ƒƒ¡ƒƒø"
      LOCATE , 6: PRINT "≥ALARM≥"
      LOCATE , 6: PRINT "¿ƒƒƒƒƒŸ"

      COLOR 15, 4                    'wei· auf rote Seitenbalken
      FOR zeile% = 2 TO 24
        LOCATE zeile%, 1: PRINT " ";
        LOCATE zeile%, 40: PRINT " ";
      NEXT zeile%
    ELSE

      '-- ungerade sec ==> ALARM-Anzeige im Rechteck,
      '--- keine Alarmanzeige in den Seitenbalken
      COLOR 15, 4                   'wei· auf rot
      LOCATE weckzeile%, 6: PRINT "       "
      LOCATE , 6: PRINT " ALARM "
      COLOR 4, 7
      LOCATE , 6: PRINT "ﬂﬂﬂﬂﬂﬂﬂ"

      COLOR 0, 7                     'schwarz auf hellgrau, keine Seitenbalken
      FOR zeile% = 2 TO 24
        LOCATE zeile%, 1: PRINT " ";
        LOCATE zeile%, 40: PRINT " ";
      NEXT zeile%
    END IF
  ELSE

  '--- Modus (2) ---
    COLOR 15, 2                      'Wecker-AUS-Anzeige wei· auf grÅn
    LOCATE weckzeile%, 7: PRINT "     "
    LOCATE , 7: PRINT " AUS "
    COLOR 2, 7
    LOCATE , 7: PRINT "ﬂﬂﬂﬂﬂ"
  END IF
  COLOR 0, 7                      'schwarz auf hellgrau

'------------- Wecker stellen, wenn Hîher-/Tiefertaste gedrÅckt -------------
  IF up% THEN                     'Hîher ==> hour/ min inkrementieren
    IF stellmin% THEN
      wmin% = wmin% + 1
      IF wmin% = 60 THEN wmin% = 0
    ELSE
      whour% = whour% + 1
      IF whour% = 24 THEN whour% = 0
    END IF
  END IF

  IF dn% THEN                     'Tiefer ==> hour/min dekrementieren
    IF stellmin% THEN
      IF wmin% > 0 THEN wmin% = wmin% - 1 ELSE wmin% = 59
    ELSE
      IF whour% > 0 THEN whour% = whour% - 1 ELSE whour% = 23
    END IF
  END IF

  wmin10% = wmin% \ 10
  wmin1% = wmin% MOD 10

  whour10% = whour% \ 10
  whour1% = whour% MOD 10

'---------------------- Weckzeit anzeigen -----------------------------------
  FOR zeile% = 2 TO 4                   'Digitalanzeige zeilenweise ausgeben
    LOCATE weckzeile% + zeile% - 2, weckspalte%
    PRINT baby$(whour10%, zeile%);
    PRINT baby$(whour1%, zeile%);
    PRINT baby$(10, zeile%);            'Doppelpunkt
    PRINT baby$(wmin10%, zeile%);
    PRINT baby$(wmin1%, zeile%);
  NEXT zeile%

  LOCATE weckzeile% - 1, weckspalte%    'Links-/Rechtspfeil Åber Minuten
  IF stellmin% THEN                     'bzw. Stunden anzeigen
    PRINT SPC(13); CHR$(27); CHR$(26)
  ELSE
    PRINT "   "; CHR$(27); CHR$(26); SPC(12);
  END IF

  LOCATE weckzeile%, 33: PRINT CHR$(24); 'Hoch-/Tief-Pfeil anzeigen
  LOCATE weckzeile% + 1, 33: PRINT CHR$(25);

'----------------------- Weckton erzeugen -----------------------------------
  IF (hour10% = whour10%) AND (hour1% = whour1%) AND (min10% = wmin10%) AND (min1% = wmin1%) AND wein% THEN
    IF nachweckdauer% > 0 THEN nachweck% = -1
                                'Merker "Nachweckperiode lÑuft" setzen falls
                                'optisches Nachwecken i.SUB Optionen aktiviert
    IF NOT weckgestartet% THEN  'Weckperiode noch nicht gestartet?
      weckgestartet% = -1       'Merker "Weckperiode gestartet" = True
      weckcounter% = weckdauer% 'Wecktimer auf Weckzeit setzen (aus Optionen)
      minalt% = min1%           'Minutenvergleichsspeicher initialisieren
    END IF
  END IF

  IF weckcounter% > 0 THEN      'Weckzeit noch nicht abgelaufen?
    IF min1% <> minalt% THEN    'neue Minute und Wecktimer
      weckcounter% = weckcounter% - 1  'Wecktimer dekrementieren
      minalt% = min1%
    END IF

    IF (sec1% = 0) OR (sec1% = 2) THEN
      IF NOT soundgemacht% THEN
        CALL MakeSound          'Weckton direkt auf I/O-Portausgeben; SOUND und
                                'Play werden nicht verwendet, da sonst Dauer-
                                'piepsen im Windows-Hintergrunfenster
        soundgemacht% = -1      'True
      END IF
    ELSE
      soundgemacht% = 0
    END IF

  ELSE
    weckgestartet% = 0           'Merker "Weckperiode gestartet" rÅcksetzen
                                 'erfolgt auch bei Leertaste und Programmstart
  END IF

'----- optische Nachweckfunktion bearbeiten --------------------------------
'Anmerkung: Die Merker nachweck% und nachweckgestartet% werden zurÅckgesetzt
'           wenn Leertaste gedrÅckt und bei Neustart des Programms
 
  IF nachweck% THEN                 'lÑuft Nachweckperiode?
    IF NOT nachweckgestartet% THEN  'Nachweckperiode noch nicht gestartet?
      nachweckgestartet% = -1       'Merker "Nachweckperiode gestartet" setz.
      nachweckcounter% = nachweckdauer%  'Nachweckzeit setzen (aus Optionen)
      min1alt% = min1%              'Minutenvergleichsspeicher initialisieren
    END IF
    IF min1% <> min1alt% AND nachweckcounter% > 0 THEN 'neue Minute und Nach-
                                    'wecktimer noch nicht abgelaufen?
      nachweckcounter% = nachweckcounter% - 1 'Timer dekrementieren
      min1alt% = min1%
    ELSEIF nachweckcounter% = 0 THEN
      nachweck% = 0                 'Nachweckperiode vorbei
      nachweckgestartet% = 0        'Merker "Nachweckperiode gestartet" fÅrs
                                    'nÑchste Mal rÅcksetzen
      COLOR 10, 7                   'schwarz auf hellgrau
      LOCATE weckzeile%, 6: PRINT "       " 'gro·es Alarm-Anzeigefeld lîschen
      LOCATE , 6: PRINT "       "
      LOCATE , 6: PRINT "       "
                                    'rote optische Nachweckbalken lîschen
     COLOR 0, 7                     'schwarz auf hellgrau, keine Seitenbalken
      FOR zeile% = 2 TO 24
        LOCATE zeile%, 1: PRINT " ";
        LOCATE zeile%, 40: PRINT " ";
      NEXT zeile%

    END IF
  END IF

'----------------------- Ende der Dauerschleife -----------------------------
LOOP



'
'Zeichengenerator fÅr 7-Segment-Anzeige bestehend aus den folgenden
'====================================================================
'4 ASCII-Zeichen:     219=€ , 223=ﬂ, 220=‹, 32=[Blank]
'
DATA "€ﬂﬂ€ "
DATA "€  € "
DATA "€  € "
DATA "ﬂﬂﬂﬂ "

DATA "   € "
DATA "   € "
DATA "   € "
DATA "   ﬂ "

DATA "ﬂﬂﬂ€ "
DATA "‹‹‹€ "
DATA "€    "
DATA "ﬂﬂﬂﬂ "

DATA "ﬂﬂﬂ€ "
DATA "‹‹‹€ "
DATA "   € "
DATA "ﬂﬂﬂﬂ "

DATA "€  € "
DATA "€‹‹€ "
DATA "   € "
DATA "   ﬂ "

DATA "€ﬂﬂﬂ "
DATA "€‹‹‹ "
DATA "   € "
DATA "ﬂﬂﬂﬂ "

DATA "€ﬂﬂﬂ "
DATA "€‹‹‹ "
DATA "€  € "
DATA "ﬂﬂﬂﬂ "
                    
DATA "ﬂﬂﬂ€ "
DATA "   € "
DATA "   € "
DATA "   ﬂ "

DATA "€ﬂﬂ€ "
DATA "€‹‹€ "
DATA "€  € "
DATA "ﬂﬂﬂﬂ "

DATA "€ﬂﬂ€ "
DATA "€‹‹€ "
DATA "   € "
DATA "ﬂﬂﬂﬂ "

DATA "  "
DATA "‹ "
DATA "  "
DATA "ﬂ "

'------- Baby-Fonts --------------
DATA "€ﬂ€ "
DATA "€ € "
DATA "ﬂﬂﬂ "

DATA "  € "
DATA "  € "
DATA "  ﬂ "

DATA "ﬂﬂ€ "
DATA "€ﬂﬂ "
DATA "ﬂﬂﬂ "
     
DATA "ﬂﬂ€ "
DATA "ﬂﬂ€ "
DATA "ﬂﬂﬂ "

DATA "€ € "
DATA "ﬂﬂ€ "
DATA "  ﬂ "

DATA "€ﬂﬂ "
DATA "ﬂﬂ€ "
DATA "ﬂﬂﬂ "

DATA "€ﬂﬂ "
DATA "€ﬂ€ "
DATA "ﬂﬂﬂ "

DATA "ﬂﬂ€ "
DATA "  € "
DATA "  ﬂ "

DATA "€ﬂ€ "
DATA "€ﬂ€ "
DATA "ﬂﬂﬂ "

DATA "€ﬂ€ "
DATA "ﬂﬂ€ "
DATA "ﬂﬂﬂ "

DATA "  "
DATA "ﬂ "
DATA "ﬂ "

END

SUB DirectSound (herz%, sec!)
'****************************************************************************
' DirectSound = Subroutine zur Direkt-Ausgabe eines Tons auf den Speaker
' ======================================================================
' Die Ansteuerung des PC-Speakers erfolgt direkt Åber die I/O-Ports. Dies
' hat gegenÅber dem SOUND- und PLAY-Befehl den Vorteilil, da· die Soundausgabe
' auch in einem Programm funktioniert, das gerade in einem Windows-Hinter-
' grundfenster lÑuft. Bei PLAY und SOUND wird in diesem Falle leider ein
' Dauerpiepsen erzeugt.
'
' Die Subroutine ersetzt den SOUND-Befehl und hat Ñhnliche öbergabepara-
' meter wie dieser:
' Es wird ein Ton der Frequenz <herz%> und der Dauer von <sec!> Sekunden
' auf den PC-Speaker ausgegeben. Beim SOUND-Befehl wird demgegenÅber die
' TonlÑnge in Anzahl Systemtakten † 56 ms statt in sec angegeben.
'
' Credits: Die Idee zu diesem Programm stammt von James Vahn und wurde der
' ~~~~~~~~ ABC-Collection, Snippet 'PC SPEAKER FREQUENCY' entnommen.
'          Thanks a lot, James!
'
' Thomas Antoni, 01.11.99
'   thomas.antoni@erlf.siemens.de
'****************************************************************************

Divisor& = 1193180 / herz%
LSB% = Divisor& MOD 256
MSB% = Divisor& \ 256

Old% = INP(&H61)     '8255 PPI chip. Save the original.
OUT &H43, 182        '8253 Timer chip. 10110110b Channel 2, mode 3
Port% = INP(&H61)    'get the 8255 port contents.
OUT &H61, Port% OR 3 'enable the speaker and use channel 2.

OUT &H42, LSB%       'Output Frequency Lo and Hi Byte
OUT &H42, MSB%

time! = TIMER        'Wait until sec! elapsed
WHILE TIMER < time! + sec!: WEND
OUT &H61, Old%       'turn it off.
END SUB

SUB Hilfe
'*****************************************************************************
'
' Hilfe = Subroutine zum Anzeigen 2er Hilfeseiten bei F1-Taste
' ===========================================================================
'
'*****************************************************************************

WIDTH 80, 25 'Textbildschirm voller Breite
CLS

'-------- Kopfzeile anzeigen ------------
COLOR 15, 4                         'Kopfzeile wei· auf rot
PRINT "     Hilfeseite 1/2  fÅr ClockFix                   (c) Thomas Antoni, 1999     ";

'-------- Hilfetext anzeigen ------------
COLOR 1, 7                          'blau auf hellgrau
LOCATE 3, 1
PRINT "         ƒƒƒ €ﬂﬂ €    ‹ﬂﬂ‹ €ﬂﬂ €  € €ﬂﬂ ﬂ€ﬂ €   € © ƒƒƒƒƒƒƒƒ   Programmiert von"
PRINT "       ƒƒƒƒƒ €   €    €  € €   €‹ﬂ  €‹‹  €   ﬂ‹ﬂ    ƒƒƒƒƒƒƒƒ   Thomas Antoni"
PRINT "     ƒƒƒƒƒƒƒ €   €  ‹ €  € €   € ﬂ‹ €    €  ‹ﬂ ﬂ‹   ƒƒƒƒƒƒƒƒ   in QBasic 1.1"
PRINT "   ƒƒƒƒƒƒƒƒƒ ﬂﬂﬂ ﬂﬂﬂﬂ  ﬂﬂ  ﬂﬂﬂ ﬂ  ﬂ ﬂ   ﬂﬂﬂ ﬂ   ﬂ   ƒƒƒƒƒƒƒƒ   fÅr MS-DOS"
PRINT
PRINT "   Dies Programm ist 'Mailware': Wem es gefÑllt, der sende eine E-Mail an"
PRINT "                      thomas.antoni@erlf.siemens.de"
PRINT
COLOR 0, 7               'schwarz auf hellgrau
'PRINT ' STRING$(80, "ƒ");
PRINT " Bedientasten................ [Cursor-Tasten]...Weckzeit einstellen"
PRINT "                              [Leertaste]....Alarm EIN/AUS-schalten"
PRINT "                              [F1]-Taste.............Hilfe aufrufen"
PRINT "                              [F2]-Taste..Optionen Ñndern (Weckton)"
PRINT "                              [Esc]................Programm beenden"
PRINT
PRINT " Weckton..................... 1 min lang piepst alle 10 sec 1x der PC-Speaker"
PRINT
PRINT " Optische Nachweckfunktion... Nach Weckerablauf blinkt 2 h lang das rote"
PRINT "                              EIN-Feld und rote Seitenbalken"
PRINT
PRINT " Speicherfunktion............ Weckzeit und EIN/AUS-Modus werden in der Datei"
PRINT "                              CLOCKFIX.INI gesichert.";

'-------- Fu·zeile anzeigen und Beenden-Dialog ---
LOCATE 25, 1
COLOR 15, 4                         'Farbe wei· auf rot
PRINT "               NÑchste Hilfe-Seite....[Esc] oder [beliebige Taste]             ";
WHILE INKEY$ = "": WEND

'--------- zweite Hilfeseite anzeigen ------------

COLOR 15, 4               'wei· auf rot
LOCATE 1, 17: PRINT "2"   'Anzeige "Hilfeseite 2" statt "1" in Kopfzeile
COLOR 0, 7                'schwarz auf hellgrau
FOR zeile% = 12 TO 23     'Hilfetext von Hilfeseite 1 lîschen"
  LOCATE zeile%, 1
  PRINT SPC(79);
NEXT zeile%
LOCATE 10, 1
PRINT
PRINT " ClockFix unter Windows 95/98/NT einrichten:                        "
PRINT " --------------------------------------------"
PRINT " - ClockFix auf ein Teilfenster verkleinern: [Alt + Enter], falls erforderlich"
PRINT " - ClockFix auf dem Desktop einrichten: CLOCKFIX.EXE mit rechter Maustaste aus"
PRINT "     dem Explorer auf den Desktop ziehen und 'VerknÅpfung hier erstellen'"
PRINT " - Eigenschaften von ClockFix festlegen: Mit rechter Maustaste auf das ClockFix-"
PRINT "     Icon clicken und dann unter 'Eigenschaften' folgendes einstellen:"
PRINT "     - Programm....Anderes Symbol...CLOCKFIX.ICO"
PRINT "     - Programm....Beim Beenden schlie·en        <== ja"
PRINT "     - Sonstiges...immer vorÅbergehend aussetzen <== nein"
PRINT "             (ClockFix lÑuft auch im Hintergrundfenster weiter)"
PRINT "     - Bildschirm...Darstellung...Teilbild       <==ja"
PRINT "     - Die Fenstergrî·e von ClockFix ist Åber die Schriftart einstellbar"
'-------- Fu·zeile anzeigen und Beenden-Dialog ---
LOCATE 25, 1
COLOR 15, 4                         'Farbe wei· auf rot
PRINT "             ZurÅck zu ClockFix....[Esc]  oder [beliebige Taste]              ";
WHILE INKEY$ = "": WEND

END SUB

'***************************************************************************
' MakeSound = Subroutine zum Erzeugen eines Wecktons von zaghaft bis laut
' =======================================================================
'
' Thomas Antoni, 12.11.99
'***************************************************************************
SUB MakeSound
SELECT CASE weckton%
  CASE 0: CALL DirectSound(65, .25)       'Weckton 65 Hz fÅr 0.25 sec
  CASE 1: CALL DirectSound(100, .4)
  CASE 2: CALL DirectSound(200, .5)
  CASE 3:
    CALL DirectSound(392, .3)
    CALL DirectSound(494, .2)
END SELECT
END SUB

FUNCTION MitNull$ (Zahl%, stellen%)
'****************************************************************************
' MitNull = QBasic-Function zum Erzeugen von fÅhrenden Nullen
' =====================================================================
' Aus der Åbergebenen zahl% wird eine Zeichenkette mit der gewÅnschten
' Anzahl von stellen% erzeugt. Nicht vorhandene Dezimalstellen werden
' mit Nullen aufgefÅllt.
'
' Beispiel: MitNull$(123, 6) ==> "000123
'
' Thomas.Antoni, 03.11.99
'   thomas.antoni@erlf.siemens.de
'***************************************************************************+

MitNull$ = RIGHT$(STR$(Zahl% + 10 ^ stellen%), stellen%)
END FUNCTION

SUB Optionen
'*****************************************************************************
'
' Optionen =  Subroutine zum éndern der Optionen wenn F2 betÑtigt
' ===========================================================================
'
'*****************************************************************************

WIDTH 80, 25 'Textbildschirm voller Breite
COLOR 0, 7   'schwarz auf hellgrau
CLS

'--------------------- Bildschirm aufbauen (statische Elemente) -------------

'-------- Kopfzeile anzeigen ------------
COLOR 15, 4                         'Kopfzeile wei· auf rot
PRINT "      Optionen fÅr ClockFix                         (c) Thomas Antoni, 1999     ";

'-------- Optionentext anzeigen ------------
COLOR 1, 7                          'blau auf hellgrau
LOCATE 3, 1
PRINT "               ƒƒƒ ‹ﬂﬂ‹ €ﬂﬂ‹ ﬂﬂ€ﬂﬂ ﬂ€ﬂ ‹ﬂﬂ‹ €   € €ﬂﬂﬂ €   € ƒƒƒƒƒ "
PRINT "             ƒƒƒƒƒ €  € €‹‹ﬂ   €    €  €  € €ﬂ‹ € €‹‹  €ﬂ‹ € ƒƒƒƒƒ "
PRINT "           ƒƒƒƒƒƒƒ €  € €      €    €  €  € €  ﬂ€ €    €  ﬂ€ ƒƒƒƒƒ "
PRINT "         ƒƒƒƒƒƒƒƒƒ  ﬂﬂ  ﬂ      ﬂ   ﬂﬂﬂ  ﬂﬂ  ﬂ   ﬂ ﬂﬂﬂﬂ ﬂ   ﬂ ƒƒƒƒƒ "
PRINT "  [Cursortasten].......Optionen Ñndern"
PRINT "  [F3].................Normal-Einstellung wiederherstellen"
PRINT "  [Esc]................Optionen abspeichern und RÅcksprung zu ClockFix"
PRINT
'LOCATE , 18: PRINT CHR$(27); CHR$(26);  'Rechts-/Linkspfeil anzeigen
LOCATE 18, 72: PRINT CHR$(24);          'Hoch-/Tief-Pfeil blau anzeigen
LOCATE 19, 72: PRINT CHR$(25);
COLOR 0, 7               'schwarz auf hellgrau
LOCATE 11
PRINT "        ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¬ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
PRINT "        ≥     Weckdauer    ≥ Weckton wÑhlen  ≥     Nachweckdauer    ≥"
PRINT "        ≥                  ≥                 ≥                      ≥"
PRINT "        ≥    1...60 min    ≥                 ≥      0...800 min     ≥"
PRINT "        √ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ≈ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¥"
PRINT "        ≥                  ≥                 ≥                      ≥"
PRINT "        ≥     001 min      ≥     zaghaft     ≥       120 min        ≥"
PRINT "        ≥                  ≥     leise       ≥                      ≥"
PRINT "        ≥                  ≥     normal      ≥                      ≥"
PRINT "        ≥                  ≥     laut        ≥  (Dauer des roten    ≥"
PRINT "        ≥                  ≥                 ≥    Blinksignals)     ≥"
PRINT "        ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ¡ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ";
'--- Optionsspalte:    |                   |                 |
'                 opti%=0:            opti%=1:          opti%=2:
'--- Variable:    weckdauer% (1-60)   weckton% (0-3)    nachweckdauer% (0-800)

optionini:
'------- Optionsspalten initialisieren -----------
FOR opti% = 1 TO 2                  'Optionsspalten 1 und 2 rÅcksetzen
  GOSUB OptionReset
NEXT opti%
opti% = 0: GOSUB OptionSet          'Optionsspalte 0 = aktive Soalte

'-------- Fu·zeile anzeigen und Beenden-Dialog ---
LOCATE 25, 1
COLOR 15, 4                         'Farbe wei· auf rot
PRINT "        ZurÅck zu ClockFix und Optionen speichern....[Esc]                      ";

'------ auf Tastendruck warten und  gegebenenfalls Weckton ausgeben----------
DO
  DO
    IF opti% = 1 AND TIMER MOD 2 = 0 THEN 'Falls Optionsspalte "Weckton" ange-
      IF NOT soundgemacht% THEN        'wÑhlt:  zu jeder 2. sec Weckton ausge-
        CALL MakeSound                 'ben falls noch nicht erfolgt
        soundgemacht% = -1
      END IF
    ELSE
      soundgemacht% = 0
    END IF
    taste$ = INKEY$
  LOOP WHILE taste$ = ""               'warten bis Taste betÑtigt

'-------------------------- Tasten abfragen ---------------------------------
SELECT CASE taste$

'--- F3-Taste -------------------
    CASE CHR$(0) + CHR$(61)            'Normaleinstellung wiederherstellen:
      weckdauer% = 1                   ' - 1 min Weckzeit
      weckton% = 0                     ' - zaghafter Weckton
      nachweckdauer% = 120             ' - 2h optische Nachweckdauer
      GOTO optionini                   'RÅcksprung zur Anzeige der Optionen

'--- Cursor-Hoch-Taste ----------
    CASE CHR$(0) + "H"                 'Cursor hoch-Taste betÑtigt
      SELECT CASE opti%
      CASE 0
        IF weckdauer% < 60 THEN
          weckdauer% = weckdauer% + 1
          GOSUB OptionSet
        END IF
      CASE 1
        IF weckton% > 0 THEN
           weckton% = weckton% - 1
        ELSE
           weckton% = 3
        END IF
        GOSUB OptionSet
      CASE 2
        IF nachweckdauer% < 800 THEN
          nachweckdauer% = nachweckdauer% + 1
          GOSUB OptionSet
        END IF
      END SELECT
   
'--- Cursor-Tief-Taste ----------
    CASE CHR$(0) + "P"
      SELECT CASE opti%
      CASE 0
        IF weckdauer% > 1 THEN
          weckdauer% = weckdauer% - 1
          GOSUB OptionSet
        END IF
      CASE 1
        IF weckton% < 3 THEN
           weckton% = weckton% + 1
        ELSE
           weckton% = 0
        END IF
        GOSUB OptionSet
      CASE 2
        IF nachweckdauer% > 0 THEN
          nachweckdauer% = nachweckdauer% - 1
          GOSUB OptionSet
        END IF
      END SELECT

'--- Cursor-Rechts-Taste ----------
    CASE CHR$(0) + "M"
      IF opti% < 2 THEN
        GOSUB OptionReset              'alte Option zurÅcksetzen
        opti% = opti% + 1              'nÑchste Optionsspalte
        GOSUB OptionSet                'neue Option farblich aktivieren
      END IF
   
'--- Cursor-Links-Taste -----------
   CASE CHR$(0) + "K"
      IF opti% > 0 THEN
        GOSUB OptionReset              'alte Option RÅcksetzen
        opti% = opti% - 1              'vorhergehende Optionsspalte
        GOSUB OptionSet                'neue Option farblich aktivieren
      END IF

'--- Esc-Taste betÑtigt ==> -------
'--- Abspeichern und RÅcksprung ---

    CASE CHR$(27)
      CALL SaveIni                     'Konfiguration in CLOCKFIX.INI sichern
      EXIT SUB                         '...und RÅcksprung zum Hauptprogramm
  END SELECT
LOOP

'--------- Sub OptionReset - setzt die aktuelle Otionsspalte zurÅck --------
OptionReset:
COLOR 0, 7                             'Normalfarbe schwarz auf hellgrau
SELECT CASE opti%
  CASE 0
    LOCATE 18, 10: PRINT "     "; MitNull$(weckdauer%, 3); " min      "; 'Weckdauer mit
                                       'drei Stellen und fÅhrenden Nullen
  CASE 1
    LOCATE 18, 29: PRINT "     zaghaft     ";
    LOCATE 19, 29: PRINT "     leise       ";
    LOCATE 20, 29: PRINT "     normal      ";
    LOCATE 21, 29: PRINT "     laut        ";
    SELECT CASE weckton%
        CASE 0: LOCATE 18, 29: PRINT "   "; CHR$(16); " zaghaft     ";
        CASE 1: LOCATE 19, 29: PRINT "   "; CHR$(16); " leise       ";
        CASE 2: LOCATE 20, 29: PRINT "   "; CHR$(16); " normal      ";
        CASE 3: LOCATE 21, 29: PRINT "   "; CHR$(16); " laut        ";
    END SELECT
  CASE 2
    LOCATE 18, 47: PRINT "       "; MitNull$(nachweckdauer%, 3); " min        ";
                           'Nachweckdauer 3-stellig mit fÅhrenden Nullen
END SELECT
RETURN

'------------ Sub OptionSet - aktiviert d.aktuelle Otionsspalte -------------
OptionSet:
COLOR 14, 1                            'aktive Spalte gelb auf blau
SELECT CASE opti%
  CASE 0
    pfeilspalte% = 18                   'Spalte fÅr Rechts-/Links-Pfeil
    LOCATE 18, 10: PRINT "     "; MitNull$(weckdauer%, 3); " min      "; 'Weckdauer mit
                                       'drei Stellen und fÅhrenden Nullen
  CASE 1
    pfeilspalte% = 36                  'Spalte fÅr Rechts-/Links-Pfeil
    COLOR 0, 7                         'nicht aktive Wecktîne in sw. auf grau
    LOCATE 18, 29: PRINT "     zaghaft     ";
    LOCATE 19, 29: PRINT "     leise       ";
    LOCATE 20, 29: PRINT "     normal      ";
    LOCATE 21, 29: PRINT "     laut        ";
      COLOR 14, 1                      'aktiver Weckton-Text gelb auf blau
      SELECT CASE weckton%
        CASE 0: LOCATE 18, 29: PRINT "   "; CHR$(16); " zaghaft     ";
        CASE 1: LOCATE 19, 29: PRINT "   "; CHR$(16); " leise       ";
        CASE 2: LOCATE 20, 29: PRINT "   "; CHR$(16); " normal      ";
        CASE 3: LOCATE 21, 29: PRINT "   "; CHR$(16); " laut        ";
      END SELECT
  CASE 2
    pfeilspalte% = 57                  'Spalte fÅr Rechts-/Links-Pfeil
    LOCATE 18, 47: PRINT "       "; MitNull$(nachweckdauer%, 3); " min        ";
                           'Nachweckdauer 3-stellig mit fÅhrenden Nullen
END SELECT
COLOR 0, 7                 'Normalfarbe schwarz auf hellgrau
LOCATE 11, 18: PRINT SPACE$(42)
LOCATE 11, pfeilspalte%    'Spalte fÅr Rechts-/Links-Pfeil
COLOR 1, 7                 'Pfeile blau auf hellgrau
PRINT CHR$(27); CHR$(26);  'Rechts-/Linkspfeil anzeigen
COLOR 0, 7                 'wieder Normalfarbe schwarz auf hellgrau
RETURN

END SUB

SUB SaveIni
'****************************************************************************
' SaveIni = Subroutine zum Abspeichern der Weckzeit, des Alarm-EIN/AUS-Zus-
' tands und der Optionen in der Ini-Datei CLOCKFIX.INI
'
' T.Antoni, 11.11.99
'***************************************************************************
OPEN "clockfix.ini" FOR OUTPUT AS #1
WRITE #1, "Ini-Datei fÅr ClockFix (c)T.Antoni", whour%, wmin%, wein%, weckdauer%, weckton%, nachweckdauer%
CLOSE #1
END SUB

FUNCTION WeekDay% (Mon%, Day%, Year%)
'****************************************************************************
' WeekDay% = Function for Evaluating the Day of the Week
'            Funktion zum Ermitteln des Wochtages zu einem vorgegebenen Datum
' ===========================================================================
'
' English Description
' ~~~~~~~~~~~~~~~~~~~~~~
' Function WeekDay% to calculate the day of the week when given the date in
' integer form: Mon%, Day%, Year% (year: 1582 to 2450)
' Note: Returns (0=Sunday...6=Saturday)  or -1 if an error occurs"
'
' Deutsche Beschreibung
' ~~~~~~~~~~~~~~~~~~~~~~
' An die Funktion wird das Jahr (1582...2450), der Monat und der Tag jeweils
' als Integer-Grî·en Åbergeben. Die Funktion liefert 0...6 fÅr Sonntag bis
' Samstag zurÅck bzw. -1 wenn ein Fehler auftritt.
' Die Auswertung des RÅckmeldewerts im Hauptprogramm erfolgt normalerweise in
' einem SELECT CASE Block.
'
' Beispiel 1 : Anzeigen des Wochentages zum 10.11.99 (war ein Donnerstag):
' ~~~~~~~~~~     CASE SELECT Weekday%(11, 10, 1999)
'                  CASE 0: PRINT "Sonntag"
'                  ...
'                  CASE 6: PRINT "Samstag"
'                END SELECT
'
' Beispiel 2: Den heutigen Wochentag anzeigen:
' ~~~~~~~~~~~    d$ = DATE$
'                tag% = VAL(MID$(d$, 4, 2))
'                monat% = VAL(LEFT$(d$, 2))
'                jahr% = VAL(RIGHT$(d$, 4))
'                SELECT CASE WeekDay%(monat%, tag%, jahr%)
'                  CASE 0: PRINT "Sonntag"
'                  ...
'
' Credits : Thanks to Garry Spencer (gspencer@stim.tec.tn.us) for giving me
' ~~~~~~~~~ the idea for this subroutine. The basic functions have been
'           derived from Garry's Pogramm WEEKDAY.BAS.
'
' Thomas Antoni, 11.11.99
'   thomas.antoni@erlf.siemens.de
'****************************************************************************

DTmp% = 4: Days% = 0: Ofs% = 0: Leap% = 0: WeekDay% = -1
IF Year% < 1582 OR Year% > 2450 OR Mon% < 1 OR Mon% > 12 OR Day% < 1 THEN EXIT FUNCTION
FOR YTmp% = 1582 TO Year%
  DTmp% = (DTmp% + 1 + Leap%) MOD 7
    SELECT CASE 0
      CASE (YTmp% MOD 400): Leap% = 1
      CASE (YTmp% MOD 100): Leap% = 0
      CASE (YTmp% MOD 4): Leap% = 1
      CASE ELSE: Leap% = 0
    END SELECT
NEXT YTmp%
FOR MTmp% = 1 TO Mon%: Ofs% = Ofs% + Days%
  SELECT CASE MTmp%
    CASE 1: Days% = 31
    CASE 2: Days% = 28 + Leap%:
    CASE 3: Days% = 31
    CASE 4: Days% = 30
    CASE 5: Days% = 31
    CASE 6: Days% = 30
    CASE 7: Days% = 31
    CASE 8: Days% = 31
    CASE 9: Days% = 30
    CASE 10: Days% = 31
    CASE 11: Days% = 30
    CASE 12: Days% = 31
  END SELECT
NEXT MTmp%
IF Day% <= Days% THEN WeekDay% = (DTmp% + Ofs% + Day% - 1) MOD 7
END FUNCTION

