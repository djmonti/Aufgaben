MODULE functions
  PUBLIC ::  fibo_iterativ, fibo_rekursiv, prim, umkehr

CONTAINS
 !1. Fibonaccifolgen
 !1.1. iterative Berechnung
  FUNCTION fibo_iterativ(x)
    INTEGER, PARAMETER  :: dble = SELECTED_INT_KIND(18)
    INTEGER (KIND=dble) :: x, fibo_iterativ, j, x_1, x_2
      x_1           = 0
      x_2           = 1
      fibo_iterativ = 0
      DO j = 1, x
        fibo_iterativ = x_1 + x_2
        x_2           = x_1
        x_1           = fibo_iterativ
        IF (fibo_iterativ < 0) THEN
          WRITE(*,*) 'Überlauf'
          STOP
        END IF
      END DO
  END FUNCTION

 !1.2. rekusive Berechnung
  !RECURSIVE FUNCTION fibo_rekursiv(y,a,b) RESULT(z)
  !  INTEGER, PARAMETER  :: dble = SELECTED_INT_KIND(18)
  !  INTEGER (KIND=dble) :: y, a, b, z
  !  IF (y == 1) THEN
  !    z = a
  !  ELSE IF (y == 2) THEN
  !    z = b
  !  ELSE
  !    fibo_rekursiv(y - 1, b, a + b)
  !  END IF

  !END FUNCTION

 !2. Primzahlen
  FUNCTION prim(x)
    INTEGER :: x, m = 1, k
    LOGICAL :: prim

    IF (((MOD(x,2)) /= 0) .AND. (x > 3)) THEN
      DO k = 3, NINT(SQRT(REAL(x))) + 1, 2
        m = MOD(x,k)
        IF (m == 0) EXIT
      END DO
      prim = (m /= 0)
    ELSE IF ((x < 4) .AND. (x > 1)) THEN
      prim = .TRUE.
    ELSE
      prim = .FALSE.
    END IF
  END FUNCTION

 !3. Umkehrzahlen
  FUNCTION umkehr(z)
    INTEGER :: umkehr, z, j, i, rest, z_alt
    j      = 0
    umkehr = 0
    z_alt  = z
    DO
      z = z / 10
      IF (z >= 1) THEN
        j = j + 1
      ELSE
        EXIT
      END IF
    END DO
    z = z_alt
    DO i = 0, j
      rest       = MOD(z,10)
      z          = z - rest
      umkehr     = umkehr + (rest*(10**(j-i)))
      z          = z / 10
    END DO
    z = z_alt
  END FUNCTION

END MODULE
