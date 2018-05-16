MODULE queuemod
    IMPLICIT NONE

    INTEGER,PARAMETER       :: kurz=SELECTED_INT_KIND(2), skurz=SELECTED_INT_KIND(1)

    TYPE node
        INTEGER(KIND=kurz)  :: inhalt
        TYPE(node),POINTER  :: next => NULL()
    END TYPE

    TYPE queue
        INTEGER             :: length = 0
        TYPE(node),pointer  :: head => NULL(), tail => NULL()
    END TYPE

    TYPE zeit
        INTEGER(KIND=skurz) :: std,min,sek
    END TYPE

CONTAINS
    LOGICAL FUNCTION empty(insert)
        TYPE(queue) :: insert
        empty = (.NOT.(ASSOCIATED(insert%head)))
    END FUNCTION

    SUBROUTINE enqueue(insert,menge)
        TYPE(queue)                     :: insert
        INTEGER,INTENT(IN)              :: menge
        TYPE(node),POINTER              :: anfug,tmp

        insert%length= insert%length+1

        ALLOCATE(anfug)

        anfug%inhalt = menge
        tmp => insert%tail
        tmp%next => anfug
        insert%tail  => anfug
        DEALLOCATE(tmp)
    END SUBROUTINE

    SUBROUTINE dequeue(insert)
        TYPE(queue)                     :: insert
        TYPE(node),POINTER              :: tmp

        IF (ASSOCIATED(insert%head)) THEN
            IF (ASSOCIATED(insert%head%next)) THEN
                tmp => insert%head
                insert%head => tmp%next
                DEALLOCATE(tmp)
                insert%length= insert%length-1
            ELSE
                insert%head => null()
            END IF
        ELSE
            WRITE(*,*) 'Entfernen nicht möglich.'
        END IF
    END SUBROUTINE

    SUBROUTINE putt(insert)
        TYPE(queue),INTENT(IN)  :: insert
        TYPE(node),POINTER      :: tmp

        IF (empty(insert)) THEN
            WRITE(*,*) 'Kasse ist leer.'
        ELSE
            tmp => insert%head

            DO
                WRITE(*,*) tmp%inhalt
                IF (.NOT.(ASSOCIATED(tmp%next))) EXIT
                tmp => tmp%next
            END DO
        END IF
        DEALLOCATE(tmp)
    END SUBROUTINE

    SUBROUTINE random_int(n)
        INTEGER,INTENT(OUT) :: n
        REAL                :: h

        CALL RANDOM_NUMBER(h)
        n = 300 + FLOOR((300+1-10)*h)
    END SUBROUTINE

    SUBROUTINE inttotime(i)
        TYPE(zeit)              :: zeitz
        INTEGER                 :: i
        zeitz%std=MOD(i,3600)
        i=i-zeitz%std*3600
        zeitz%min=MOD(i,60)
        i=i-zeitz%min
        zeitz%sek=i
        WRITE(*,*) zeitz%std,'h',zeitz%min,'min',zeitz%sek,'s'
    END SUBROUTINE

END MODULE

PROGRAM markt
    USE queuemod
    IMPLICIT NONE

    INTEGER                                 :: k,i,j,inh
    REAL                                    :: w,h
    TYPE(queue),DIMENSION(:),ALLOCATABLE    :: kasse

    CALL RANDOM_SEED()

    DO
        WRITE(*,*) 'Geben Sie die Anzahl der geöffneten Kassen ein.'
        READ(*,*) k
        IF (k>0) EXIT
    END DO

    DO
        WRITE(*,*) 'Geben Sie die Ankunftswahrscheinlichkeit eines neuen Kunden im Kassenbereich innerhalb eines Zeitschritts ein.'
        READ(*,*) w
        IF (w>0 .AND. w<1) EXIT
    END DO

    ALLOCATE(kasse(k))

    WRITE(*,*) '1'

    DO i=1,50400
        CALL RANDOM_NUMBER(h)
        WRITE(*,*) '2'
        IF (h<w) THEN
            CALL random_int(inh)
            WRITE(*,*) '3'
            CALL enqueue(kasse(minz(k)),inh)
            WRITE(*,*) '4'
            CALL ausgabe(i)
            WRITE(*,*) '5'
        END IF

        DO j=1,k
            IF (.NOT.(empty(kasse(j)))) THEN
                kasse(j)%head%inhalt = kasse(j)%head%inhalt-1
                IF (kasse(j)%head%inhalt == 0) THEN
                    WRITE(*,*) '6'
                    CALL dequeue(kasse(j))
                    WRITE(*,*) '7'
                    CALL ausgabe(i)
                END IF
                WRITE(*,*) '8'
            END IF
            WRITE(*,*) '9'
        END DO
    END DO

    DEALLOCATE(kasse)
CONTAINS
    FUNCTION minz(x)
        INTEGER,INTENT(IN)  :: x
        INTEGER             :: minz,j, mint
        DO j=2,x
            mint=kasse(1)%length
            IF(kasse(j)%length < mint) THEN
                mint = kasse(j)%length
                minz = j
            END IF
        END DO
    END FUNCTION

    SUBROUTINE ausgabe(i)
        INTEGER,INTENT(IN)  :: i
        INTEGER             :: l

        CALL inttotime(i)
        DO l=1,k
            WRITE(*,*) 'Kasse:', l
            CALL putt(kasse(l))
        END DO
    END SUBROUTINE

END PROGRAM
