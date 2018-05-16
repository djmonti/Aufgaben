PROGRAM Primzahlen
  IMPLICIT NONE

  INTEGER :: x, m = 1, k
  LOGICAL :: prim

  READ(*,*) x

  IF (((MOD(x,2)) /= 0) .AND. (x > 3)) THEN
    DO k = 3, NINT(SQRT(REAL(x))), 2
     m = MOD(x,k)
     IF (m == 0) EXIT

    END DO
    prim = (m /= 0)
  ELSE IF ((x < 4) .AND. (x > 1)) THEN
   prim = .TRUE.
  ELSE
   prim = .FALSE.
  END IF

  WRITE(*,*) prim

END PROGRAM Primzahlen
