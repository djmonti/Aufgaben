PROGRAM Quadrate

  IMPLICIT NONE
  INTEGER,DIMENSION(:,:),ALLOCATABLE :: quadrat
  INTEGER                          :: i,j,n,spalte,zeile,diag,s,a,b

  DO
    WRITE(*,*) 'Geben Sie eine ungerade Ordnung der Matrix an.'
    READ(*,*) n
    IF (n<1) STOP
    IF (n/2.0 /= REAL(n/2)) EXIT
  END DO

  ALLOCATE(quadrat(n,n))

  DO i=1,n
    DO j=1,n
      quadrat(i,j)=0
    END DO
  END DO

  !quadrat(1,n)=1

  a=1
  b=n
  DO i=1,n**2
    quadrat(a,b)=i
    a=a-1; b=b-1
    IF (quadrat(a,b)/=0 .OR. a==0 .OR. b==0) THEN
      a=a+1;b=b+1
    END IF
    DO WHILE (quadrat(a,b)/=0)
      IF (a==1 .AND. b==1) THEN
        a=2; b=1
      ELSE IF (a==1 .AND. b/=1) THEN
        a=n;b=b-1
      ELSE IF (a/=1 .AND. b==1) THEN
        a=a-1;b=n
      ELSE IF (a/=1 .AND. b/=1) THEN
        a=a+1;b=b
      END IF
    END DO
  END DO


  DO i=1,n
    WRITE(*,*) quadrat(i,:)
  END DO

  !Zeilen
  DO i=1,n
    zeile=0
    DO j=1,n
      zeile=zeile+quadrat(i,j)
    END DO
    WRITE(*,*) i,'. Zeilensumme  =', zeile
  END DO

  !Spalten
  DO i=1,n
    spalte=0
    DO j=1,n
      spalte=spalte+quadrat(j,i)
    END DO
    WRITE(*,*) i,'. Spaltensumme =', spalte
  END DO

  !Diagonale
  diag=0
  DO i=1,n
    diag=diag+quadrat(i,i)
  END DO
  WRITE(*,*) 'Diagonale von rechts       =', diag


  diag=0
  DO i=1,n
    diag=diag+quadrat(i,n-i+1)
  END DO
  WRITE(*,*) 'Diagonale von links        =', diag

  !Summe
  s=n*(n**2+1)/2
  WRITE(*,*) 'Die Summe müsste            ', s, 'betragen.'

  DEALLOCATE(quadrat)
END PROGRAM
