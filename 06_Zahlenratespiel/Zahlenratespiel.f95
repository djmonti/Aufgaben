PROGRAM Zahlenratespiel

  INTEGER          :: l, r, n, i = 0, maxi
  REAL             :: l_alt, r_alt
  CHARACTER(LEN=1) :: a,j = 'j'

  DO WHILE (j == 'j')
    i = 0

    !! Eingabe zweier ganzer Zahlen l,r bis l<r
    DO
      WRITE(*,*) 'Bite geben Sie das Intervall ein mit |I|>2'
      READ(*,*) l, r
      IF (r - l > 0) EXIT
    END DO

    !! Maximale Anzahl der Versuche
    maxi = INT((log(REAL(r) - l) + 1)/log(2.0))

    DO
      i = i + 1

      IF (r < l) THEN !! Beispiel mit n>5 und n<6
        WRITE(*,*) 'Die Aussage war widersprüchlich. Programm beendet.'
        STOP
      END IF

      !! Ausgabe einer Zahl aus Intervall
    !  IF ((r - l == 0) .AND. (a == '>')) THEN !! Bedingung für n = obere Grenze
    !    n = r
    !  ELSE
        n = ((r - l)) / 2 + l  !! n ist Mitte des Intervalls
    !  END IF

      WRITE(*,*) 'Ist die Zahl', n, '?'

      !! Eingabe des Mitspielers >,<,=
      DO
        WRITE(*,*) 'Spieler? [<,>,=]'
        READ(*,*) a

        IF (a == '=') THEN !! n ist richtig
           EXIT
        END IF

        IF (a == '<') THEN !! n <
          IF ((n >= l) .AND. (n <= r)) THEN
            r = n - 1  !! rechte Grenze ist n - 1
            EXIT
          ELSE
            WRITE(*,*) 'Die Aussage war widersprüchlich. Programm beendet.'
            STOP
          END IF
        END IF

        IF (a == '>') THEN !! n <
          IF ((n >= l) .AND. (n <= r)) THEN
            l = n + 1  !! linke Grenze ist n + 1
            EXIT
          ELSE
            WRITE(*,*) 'Die Aussage war widersprüchlich. Programm beendet.'
            STOP
          END IF
        END IF
      END DO
      IF (a == '=') EXIT
    END DO


    !! Ausgabe der benötigten Versuche bei erraten und maximale
    WRITE(*,*) 'Es wurde(n)       ', i, 'Versuch(e) benötigt.'
    WRITE(*,*) 'Mögliche Versuche:', maxi, '.'

    !! Abfrage, ob nochmal spielen
    DO
      WRITE(*,*) 'Erneut Spielen? [j/n]'
      READ(*,*) j
      IF ((j == 'j') .OR. (j == 'n')) EXIT
    END DO
  END DO
END PROGRAM
