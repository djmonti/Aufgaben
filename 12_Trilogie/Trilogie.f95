MODULE TRILOGMOD
  IMPLICIT NONE

  INTEGER, PARAMETER :: one = SELECTED_INT_KIND(0)

  TYPE trilog
    INTEGER(KIND=one) :: VAR
  END TYPE

  TYPE(TRILOG), PARAMETER :: FALSE= TRILOG(-1), MAYBE= TRILOG(0), TRUE= TRILOG(1)

  INTERFACE OPERATOR ( .AND. )
    MODULE PROCEDURE AND
  END INTERFACE

  INTERFACE OPERATOR ( .OR. )
    MODULE PROCEDURE OR
  END INTERFACE

  INTERFACE OPERATOR ( .NOT. )
    MODULE PROCEDURE NOT
  END INTERFACE

  INTERFACE OPERATOR ( == )
    MODULE PROCEDURE EQAL
  END INTERFACE

CONTAINS

  FUNCTION AND (a,b)
    TYPE(TRILOG), INTENT(IN)  :: a,b
    TYPE(TRILOG)              :: AND

    AND = TRILOG(MIN(a%VAR,b%var))
  END FUNCTION AND

  FUNCTION OR (a,b)
    TYPE(TRILOG), INTENT(IN) :: a,b
    TYPE(TRILOG)             :: OR

    OR = TRILOG(MAX(a%VAR,b%var))
  END FUNCTION OR

  FUNCTION NOT (a)
    TYPE(TRILOG), INTENT(IN) :: a
    TYPE(TRILOG)             :: NOT

    NOT = TRILOG(-1*a%VAR)
  END FUNCTION NOT

  FUNCTION EQAL (a,b)
    TYPE(TRILOG), INTENT(IN) :: a,b
    LOGICAL                  :: EQAL

    EQAL = a%VAR == b%VAR
  END FUNCTION EQAL

  SUBROUTINE TEXT(a)    !TRILOG zu CHARACTER und Ausgabe
    TYPE(TRILOG), INTENT(IN) :: a
    CHARACTER(LEN=1)         :: b

    IF (a%VAR == 1) THEN
      b = 'T'
    ELSE IF (a%VAR == -1) THEN
      b = 'F'
    ELSE
      b = '?'
    END IF
    WRITE(*,*) b
  END SUBROUTINE TEXT

  FUNCTION JTEXT(a)     !Nur TRILOG zu CHARACTER
    TYPE(TRILOG), INTENT(IN) :: a
    CHARACTER(LEN=1)         :: JTEXT
    IF (a%VAR == 1) THEN
      JTEXT = 'T'
    ELSE IF (a%VAR == -1) THEN
      JTEXT = 'F'
    ELSE
      JTEXT = '?'
    END IF
  END FUNCTION JTEXT
END MODULE TRILOGMOD

PROGRAM MAIN
  USE TRILOGMOD
  IMPLICIT NONE

  TYPE(TRILOG), DIMENSION(3)       :: arr = (/FALSE,MAYBE,TRUE/)
  TYPE(TRILOG)                     :: a,b,c
  CHARACTER(LEN=2), DIMENSION(3,3) :: cara, cara2
  LOGICAL, DIMENSION(3,3)          :: logi, logi2
  INTEGER, PARAMETER               :: sml = SELECTED_INT_KIND(1)
  INTEGER(KIND=sml)                :: i,j,k = 0

  DO i = 1, 3     !Alle Werte für Nicht a
    DO j = 1, 3
      cara(i,j) = ' ' // JTEXT(.NOT.(arr(i)))
    END DO
  END DO
  WRITE(*,*) 'NOT 1. Wert:  ', cara

  DO i = 1, 3     !Alle Werte für a
    DO j = 1, 3
      cara(i,j) = ' ' // JTEXT(arr(i))
    END DO
  END DO
  WRITE(*,*) '    1. Wert:  ', cara

  DO i = 1, 3     !Alle Werte für b
    DO j = 1, 3
      cara(i,j) = ' ' // JTEXT(arr(j))
    END DO
  END DO
  WRITE(*,*) '    2. Wert:  ',cara

  DO i = 1, 3     !logisches Und
    DO j = 1, 3
      cara(i,j) = ' ' // JTEXT(arr(i) .AND. arr(j))
    END DO
  END DO
  WRITE(*,*) '        AND:  ',cara

  DO i = 1, 3     !logsiches Oder
    DO j = 1, 3
      cara(i,j) = ' ' // JTEXT(arr(i) .OR. arr(j))
    END DO
  END DO
  WRITE(*,*) '         OR:  ',cara

  DO i = 1, 3     !De-Morgen 1 und 2
    DO j = 1, 3
      logi(i,j)  = (.NOT. (arr(i) .AND. arr(j))) == ((.NOT. arr(i)) .OR. (.NOT. arr(j)))
      logi2(i,j) = (.NOT. (arr(i) .OR. arr(j))) == ((.NOT. arr(i)) .AND. (.NOT. arr(j)))
    END DO
  END DO
  WRITE(*,*) '     DE-M 1:  ',logi
  WRITE(*,*) '     DE-M 2:  ',logi2

  DO i = 1, 3     !Nacheinander alle Werte für c
    c = arr(i)
    DO j = 1, 3   !Schleife wie oben, die mit gegebenem c Rest berechnet
      a = arr(j)
      DO k = 1, 3
        b = arr(k)
        logi(j,k)  = (a .AND. (b .OR. c)) == ((a .AND. b) .OR. (a .AND. c))
        logi2(j,k) = (a .OR. (b .AND. c)) == ((a .OR. b) .AND. (a .OR. c))
      END DO
    END DO
    WRITE(*,*) '    3. Wert:  ', ' ' // JTEXT(c)
    WRITE(*,*) '    DIST 1 :  ', logi
    WRITE(*,*) '    DIST 2 :  ', logi27
  END DO
END PROGRAM
