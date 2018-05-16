MODULE module
  PUBLIC :: prim, umkehr

CONTAINS
  FUNCTION prim(x)
    INTEGER :: x, m = 1, k
    LOGICAL :: prim

    IF (((MOD(x,2)) /= 0) .AND. (x > 3)) THEN
      DO k = 3, INT(sqrt(real(x))), 2
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

PROGRAM Prim_Mirp
  USE module
  INTEGER :: l, r, n

  DO
    WRITE(*,*) 'Geben Sie ein Intervall ein.'
    READ(*,*) l, r
    IF ((r-l > 0) .AND. (l > 0)) EXIT
  END DO

  DO n = l, r
    IF (prim(n)) THEN
      IF (prim(umkehr(n))) THEN
        WRITE(*,*) n,'ist Mirpzahl'
      ELSE
        WRITE(*,*) n,'ist Primzahl'
      END IF
    END IF
  END DO
END PROGRAM
