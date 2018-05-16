PROGRAM PoinTr
    IMPLICIT NONE
    INTEGER, POINTER    :: p,q,r
    INTEGER, TARGET     :: i,k
    
    NULLIFY(p,q)
    WRITE(*,*) " Is p ASSOCIATED? ", ASSOCIATED(p)
END PROGRAM

