MODULE Daten
  IMPLICIT NONE

  INTERFACE OPERATOR (.UEBER.)
    MODULE PROCEDURE ueber
  END INTERFACE

CONTAINS

  FUNCTION ueber(n,k)
    INTEGER,INTENT(IN)    :: n,k
    INTEGER               :: i, ueber

    ueber=1

    DO i=1,MIN(k,n-k)
      ueber=(ueber*((n-i+1))/i)
    END DO
  END FUNCTION

  FUNCTION mat(n,inv)
    INTEGER,INTENT(IN)    :: n
    LOGICAL               :: inv
    INTEGER,DIMENSION(n,n):: mat
    INTEGER               :: i,j

    IF (inv) THEN
      DO i=1,n
        DO j=1,n
          mat(i,j)=((-1)**(i+j))*(((n+i-1).UEBER.(i-1))*((n-1).UEBER.(n-j)))*n/(i+j-1)
        END DO
      END DO
    ELSE
      DO i=1,n
        DO j=1,n
          mat(i,j)=(((n+i-1).UEBER.(i-1))*((n-1).UEBER.(n-j)))*n/(i+j-1)
        END DO
      END DO
    END IF
  END FUNCTION
END MODULE

PROGRAM Dekker
  USE Daten
  IMPLICIT NONE

  INTEGER                             :: n,i
  INTEGER,DIMENSION(:,:),ALLOCATABLE  :: m,inv,pro

  DO
    DO
      WRITE(*,*) 'Geben Sie die Dimension ein.'
      READ(*,*) n
      IF (n>0) THEN
        EXIT
      ELSE
        STOP
      END IF
    END DO

    ALLOCATE(m(n,n)); ALLOCATE(inv(n,n)); ALLOCATE(pro(n,n))

    m    = mat(n,.FALSE.)
    inv  = mat(n,.TRUE.)
    pro  = MATMUL(m,inv)

    WRITE(*,*) 'Boothroyd/Dekker-Matrix'
    DO i=1,n
      WRITE(*,*) m(i,:)
    END DO
    WRITE(*,*) 'Inverse'
    DO i=1,n
      WRITE(*,*) inv(i,:)
    END DO
    WRITE(*,*) 'Produkt'
    DO i=1,n
      WRITE(*,*) pro(i,:)
    END DO

    DEALLOCATE(m); DEALLOCATE(inv); DEALLOCATE(pro)
  END DO
END PROGRAM
