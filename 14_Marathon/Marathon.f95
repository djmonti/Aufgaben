MODULE Daten
  IMPLICIT NONE

  TYPE zeit
    INTEGER             :: h=0,m=0
    REAL                :: s=0.0
  END TYPE

  TYPE ergebnis
    CHARACTER(LEN=30)   :: name
    TYPE(zeit)          :: zeit
  END TYPE

  INTERFACE OPERATOR (<)
    MODULE PROCEDURE smaller
  END INTERFACE



CONTAINS
  SUBROUTINE leseliste(datei,einzulesen,liste)
    CHARACTER(LEN=12), INTENT(IN)   :: datei
    INTEGER, INTENT(IN)             :: einzulesen
    INTEGER                         :: anzahl, ios, i
    TYPE(ergebnis), DIMENSION(:), ALLOCATABLE, INTENT(OUT)   :: liste

    ALLOCATE(liste(einzulesen))

    OPEN(UNIT=20,FILE=datei,IOSTAT=ios,STATUS='old',ACTION='read')

    IF (ios==0) THEN
      DO i=1, einzulesen
        READ(20,*) liste(i)%zeit%h, liste(i)%zeit%m, liste(i)%zeit%s, liste(i)%name
      END DO
    ELSE
      WRITE(*,*) 'Datei konnte nicht gelesen werden.'
    END IF

    CLOSE(20)
  END SUBROUTINE

  FUNCTION smaller(a,b)
    TYPE(zeit), INTENT(IN) :: a,b
    LOGICAL             :: smaller

    IF (a%h < b%h) THEN
      smaller = .TRUE.
    ELSE IF (a%h == b%h) THEN
      IF (a%m < b%m) THEN
        smaller = .TRUE.
      ELSE IF (a%m == b%m) THEN
        IF (a%s < b%s) THEN
          smaller = .TRUE.
        ELSE
          smaller = .FALSE.
        END IF
      ELSE
        smaller = .FALSE.
      END IF
    ELSE
      smaller = .FALSE.
    END IF
  END FUNCTION



END MODULE

PROGRAM Marathon
  USE Daten
  IMPLICIT NONE

  INTEGER                                     :: z,i,j,ios
  TYPE(ergebnis), DIMENSION(:), ALLOCATABLE   :: erg
  TYPE(ergebnis)                              :: best, alt



  DO
    WRITE(*,*) 'Geben Sie die maximale Anzahl an Marathonläufern ein.'
    READ(*,*) z
    IF (z>0) EXIT
  END DO

  CALL leseliste('marathon.dat',z,erg)

  OPEN(UNIT=21,FILE='ergebnis.txt',IOSTAT=ios,ACTION='write',STATUS='replace')

  IF (ios==0) THEN
    DO j=1,z
      best%zeit%h = 8
      DO i=1,z
        IF (erg(i)%zeit < best%zeit .AND. alt%zeit < erg(i)%zeit) THEN
          best = erg(i)
        END IF
      END DO
      alt = best
      WRITE(*,*)  j,'. ', best
      WRITE(21,*) j,'. ', best
    END DO
  ELSE
    WRITE(*,*) 'Datei konnte nicht erstellt werden.'
  END IF


  DEALLOCATE(erg)
  CLOSE(21)
END PROGRAM
