PROGRAM Kredittilung

  REAL    :: zinssatz, rate, rest, zinsen, zinssumme
  INTEGER :: laufzeit

  !! 1. Eingabe von Kredithöhe und Zinssatz
  DO
    WRITE(*,*) 'Bitte geben Sie die Höhe des Kredits in EUR ein.'
    READ(*,*) rest
    IF (rest > 0) EXIT
  END DO

  DO
    WRITE(*,*) 'Bitte geben Sie den Zinssatz in Prozent ein.'
    READ(*,*) zinssatz
    IF ((zinssatz >= 0) .AND. (zinssatz < 100)) EXIT
  END DO

  !! 2. Berechnung der Zinsen für das erste Jahr
    zinsen = zinssatz * rest / 100

  !! 3. Eingabe der Rate, bis Zinsen des ersten Jahres übersteigt wurden
  DO
    WRITE(*,*) 'Bitte geben Sie die Kreditrate ein.'
    READ(*,*) rate
    IF ((rate > zinsen) .AND. (rate <= rest)) EXIT
  END DO

  !! 4. Initialisierung der Laufzeit und der Zinssumme zu null
  laufzeit  = 0
  zinssumme = 0

  !! 5. Wiederholung solange rest positiv
  DO WHILE (rest >= 0)
    !! Erhöhnung der Laufzeit
    laufzeit = laufzeit + 1
    !! Berechnung der Zinsen
    zinsen =  rest * zinssatz / 100
    !! Berechnung von rest
    rest = rest + zinsen - rate
    !! Erhöhung der Zinssumme
    zinssumme = zinssumme + zinsen
  END DO

  !! 6. Ausführung, falls rest negativ
  IF (rest < 0) THEN
    !! Berechnung der letzten Rate als Summe der üblichen Rate und rest
    rate = rate + rest
    !! Kommentierte Ausgabe der letzten Rate
    WRITE(*,*) 'Die letzte Rate beträgt: EUR', rate
  END IF

  !! 7. Kommentierte Ausgabe der Laufzeit in Jahren und der Zinssumme in EURO
  WRITE(*,*) 'Die Laufzeit beträgt ', laufzeit, 'Jahre.'
  WRITE(*,*) 'Die Zinssumme beträgt:   EUR', zinssumme

END PROGRAM
