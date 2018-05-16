MODULE mode
    IMPLICIT NONE

    TYPE kindt
        CHARACTER(LEN=10)   :: namme
        INTEGER             :: alter
        TYPE(kindt),POINTER :: next => null() !Subroutine wird gespart
    END TYPE

    TYPE(kindt), POINTER    :: start !deklariert pointer für Hauptprogramm

CONTAINS
    SUBROUTINE build_cylce
        TYPE(kindt), pointer :: hilfe
        INTEGER              :: ios

        OPEN(UNIT=121,FILE='kreim.dat',ACTION='read',STATUS='old', IOSTAT=ios)
        ALLOCATE(start)
        IF(ios/=0) STOP !falls Datei beschädigt
        READ(UNIT=121,FMT=*) start%namme, start%alter !erstes Element

        hilfe => start

        DO
            ALLOCATE(hilfe%next)
            READ(UNIT=121,FMT=*,IOSTAT=ios) hilfe%next%namme, hilfe%next%alter
            IF (ios/=0) EXIT
            hilfe => hilfe%next !erstellen der Liste
        END DO

        DEALLOCATE(hilfe%next)
        hilfe%next => start !Herstellen des Zyklus
        CLOSE(121)

    END SUBROUTINE

    SUBROUTINE del_next(pntr)
        TYPE(kindt),POINTER :: pntr, tmp

        tmp => pntr%next
        pntr%next => tmp%next        !mittleres Element wird gelöscht
        pntr => pntr%next
        DEALLOCATE(tmp)      ! tmp%next wird gelöscht
    END SUBROUTINE

    LOGICAL FUNCTION last_one(hilfe)
        TYPE(kindt),pointer         :: hilfe

        IF (hilfe%next%Alter==hilfe%Alter .AND. hilfe%next%namme==hilfe%namme) THEN
            LAST_ONE=.TRUE.
        ELSE
            LAST_ONE=.FALSE.
        END IF
    END FUNCTION

    SUBROUTINE PUT_CYCLE(ubergabe)
        TYPE(kindt),POINTER :: ubergabe,tmp

        tmp => ubergabe
        WRITE(*,*) ubergabe%namme, ubergabe%alter
        DO
            IF (ASSOCIATED(ubergabe,tmp%next)) EXIT
            tmp => tmp%next
            WRITE(*,*) tmp%namme, tmp%alter !Ausgabe
        END DO
    END SUBROUTINE
END MODULE



PROGRAM kreim
    USE mode
    IMPLICIT NONE
    INTEGER     :: i,alta

    CALL build_cylce

    DO
        IF (last_one(start)) EXIT
        DO i=1,21 !21 Silben am Anfang
            start => start%next
        END DO
        alta=start%alter
        DO i=1,alta-1 !Alter -1 mal verrückt
            start => start%next
        END DO
        WRITE(*,*) 'Es fliegt raus: ', start%next%namme
        CALL DEL_NEXT(start)
        CALL PUT_CYCLE(start)0
    END DO

END PROGRAM kreim
