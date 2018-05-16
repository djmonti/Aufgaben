PROGRAM Umkehrzahl
  INTEGER :: umkehr, z, j = 0, i, rest, z_alt

  READ(*,*) z


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
    WRITE(*,*) j
    z = z_alt
    DO i = 0, j
      rest       = MOD(z,10)
      z          = z - rest
      umkehr     = umkehr + (rest*(10**(j-i)))
      z          = z / 10
    END DO

WRITE(*,*) umkehr

END PROGRAM
