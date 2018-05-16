MODULE modul
    IMPLICIT NONE
    TYPE teil
        INTEGER, DIMENSION(4)   :: seiten
        CHARACTER(LEN=1)        :: z
    END TYPE

    TYPE(teil),PARAMETER        :: kein_teil = teil((/0,0,0,0/),'t')
    INTEGER                     :: m,n
    TYPE(teil),DIMENSION(:),ALLOCATABLE     :: liste
    TYPE(teil),DIMENSION(:,:),ALLOCATABLE   :: puzzle

CONTAINS

    SUBROUTINE lies_puzzle(x)
        CHARACTER(LEN=*),INTENT(IN)     :: x
        INTEGER                         :: ios,j,i

        OPEN(UNIT=120,FILE=x,IOSTAT=ios,STATUS='old',ACTION='read')
        IF (ios==0) THEN
            READ(120,*) m,n
            ALLOCATE(puzzle(m,n))
            ALLOCATE(liste(m*n))

            DO i=1,m*n
                READ(120,*) liste(i)
            END DO
            !WRITE(*,*) liste
        ELSE
            WRITE(*,*) 'Fehler beim Öffnen der Datei.'
        END IF
        CLOSE(120)
    END SUBROUTINE

    FUNCTION dreh(a)
        TYPE(teil)      :: a,dreh
        INTEGER         :: i,l

        l   = a%seiten(4)

        DO i=4,2,-1
            a%seiten(i)    = a%seiten(i-1)
        END DO

        a%seiten(1)        = l
        dreh = a

    END FUNCTION

    FUNCTION passendes_teil(form,seite)
        INTEGER     :: form,seite
        TYPE(teil)  :: passendes_teil
        INTEGER     :: i,j,k

        OUTER: DO i=1,m*n
            passendes_teil=liste(i)
            DO j=1,4
                IF (passendes_teil%seiten(seite) == -form) THEN
                    liste(i)        = kein_teil
                    EXIT OUTER
                ELSE
                    passendes_teil=dreh(passendes_teil)
                END IF
            END DO
        END DO OUTER

    END FUNCTION

    SUBROUTINE loese_puzzle
        INTEGER     :: i,j

        DO i=1,m*n
            IF(liste(i)%seiten(3) == 0 .AND. liste(i)%seiten(4) == 0) THEN
                puzzle(1,1) = liste(i)
                liste(i)    = kein_teil
                EXIT
            END IF
        END DO

        DO i=1,m
            IF (i == 1) THEN
                DO j=2,n
                    puzzle(i,j)=passendes_teil(puzzle(1,j-1)%seiten(1),3)
                END DO
            ELSE
                DO j=1,n
                    puzzle(i,j)=passendes_teil(puzzle(i-1,j)%seiten(2),4)
                END DO
            END IF
        END DO

    END SUBROUTINE

    SUBROUTINE schreibe_loesche_puzzle
        INTEGER :: i
        DO i=1,m
            WRITE(*,*) puzzle(i,:)%z
        END DO

        DEALLOCATE(liste)
        DEALLOCATE(puzzle)
    END SUBROUTINE

END MODULE

PROGRAM puzzleloes
    USE modul
    IMPLICIT NONE
    CHARACTER(LEN=30)    :: datei

    WRITE(*,*) 'Bitte geben Sie den Dateinamen ein.'
    READ(*,*) datei

    CALL lies_puzzle(datei)

    CALL loese_puzzle
    !WRITE(*,*) puzzle
    CALL schreibe_loesche_puzzle



END PROGRAM
