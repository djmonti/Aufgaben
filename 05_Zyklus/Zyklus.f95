PROGRAM Zyklus

  INTEGER :: n, zahl, summe = 0 , mini = huge(mini), maxi = -huge(maxi)-1
  REAL    :: mittelwert = 0

  DO
    WRITE(*,*) 'Bitte geben Sie die Anzahl der einzugebenden Zahlen ein.'
    READ(*,*) n
    IF (n > 0) EXIT
  END DO

  DO i = 1, n

    WRITE(*,*) 'Bitte geben Sie die', i, '-te Zahl ein.'
    READ(*,*) zahl

    IF (zahl > maxi) maxi = zahl
    IF (zahl < mini) mini = zahl

    summe      = summe + zahl
    mittelwert = summe / i
    WRITE(*,*) 'Ergebnis'
    WRITE(*,*) 'Das Maximum beträgt:   ', maxi
    WRITE(*,*) 'Das Minimum beträgt:   ', mini
    WRITE(*,*) 'Die Summe beträgt:     ', summe
    WRITE(*,*) 'Der Mittelwert beträgt:', mittelwert
  END DO
  WRITE(*,*) 'Ergebnis'
  WRITE(*,*) 'Das Maximum beträgt:   ', maxi
  WRITE(*,*) 'Das Minimum beträgt:   ', mini
  WRITE(*,*) 'Die Summe beträgt:     ', summe
  WRITE(*,*) 'Der Mittelwert beträgt:', mittelwert
END PROGRAM
