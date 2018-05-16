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
  INTEGER :: l, r, n, k,x,y, start, start1

  DO k = 2,9
    start = 10**k
    start1 = start
    x = 0
    y = 0

    DO WHILE (x /= 1)
      IF ((prim(start)) .AND. (prim(umkehr(start)))) THEN
        WRITE(*,*) start,'ist Mirpzahl'
        x = 1
      END IF
      start = start + 1
    END DO

    DO WHILE (y /= 1)
      IF ((prim(start1)) .AND. (.NOT.(prim(umkehr(start1))))) THEN
        WRITE(*,*) start1, 'ist Primzahl'
        y = 1
      END IF
      start1 = start1 + 1
    END DO
  END DO
END PROGRAM
