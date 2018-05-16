MODULE fibo
  IMPLICIT NONE
  
CONTAINS
  FUNCTION fibo_iterativ(x)
    INTEGER, PARAMETER  :: dble = SELECTED_INT_KIND(18)
    INTEGER (KIND=dble) :: x, fibo_iterativ, x_1, x_2
    INTEGER             :: j
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

  RECURSIVE FUNCTION fibo_rekursiv(x,a,b) RESULT(res)
    INTEGER, PARAMETER  :: dble = SELECTED_INT_KIND(18)
    INTEGER (KIND=dble) :: x, a, b, res
    IF (x == 0) THEN
      res = 0
    ELSE IF (x == 1) THEN
      res = a
    ELSE IF (x == 2) THEN
      res = b
    ELSE
      res = fibo_rekursiv(x - 1, b, a + b)
    END IF

  END FUNCTION

END MODULE

PROGRAM Fibonacci
  USE fibo
  IMPLICIT NONE
  INTEGER, PARAMETER  :: dble = SELECTED_INT_KIND(18)
  INTEGER (KIND=dble) :: i

  DO
    WRITE(*,*) 'Bitte geben Sie eine ganze Zahl i ein.'
    READ(*,*) i

    IF (i < 0) STOP

    WRITE(*,*) 'f(i) iterativ:', fibo_iterativ(i)
    WRITE(*,*) 'f(i) rekursiv:', fibo_rekursiv(INT(i, KIND=dble),INT(1, KIND=dble),INT(1, KIND=dble))
  END DO
END PROGRAM
