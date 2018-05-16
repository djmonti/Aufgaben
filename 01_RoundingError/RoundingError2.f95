PROGRAM RoundingError
IMPLICIT NONE
!! Choose REAL type with decimal precision >=P and decimal range >=R :
! INTEGER, PARAMETER :: real_kind= SELECTED_REAL_KIND(P=6, R=37)
INTEGER, PARAMETER :: real_kind= SELECTED_REAL_KIND(P=15, R=307)
! INTEGER, PARAMETER :: real_kind= SELECTED_REAL_KIND(P=18, R=4931)
REAL(KIND=real_kind) :: x, y, z, b, e, d
WRITE(*,*) ' Please enter integer or real values for x and y: '
READ (*,*) x, y
z= x**4 - 4*y**4 - 4*y**2
b= x**4
d= 4*y**4
e= 4*y**2
  WRITE(*,*) ' 1. x**4 - 4*y**4 - 4*y**2 = ', z
  WRITE(*,*) '    x**4 = ', b
  WRITE(*,*) '    4*y**4 = ', d
  WRITE(*,*) '    4*y**2 = ', e
z= x*x*x*x - 4*y*y*y*y - 4*y*y
b= x*x*x*x
d= 4*y*y*y*y
e= 4*y*y
  WRITE(*,*) ' 2. x*x*x*x - 4*y*y*y*y - 4*y*y = ', z
  WRITE(*,*) '    x*x*x*x = ', b
  WRITE(*,*) '    4*y*y*y*y = ', d
  WRITE(*,*) '    4*y*y = ', e
z= (x**2)**2 - (2*y**2)**2 - (2*y)**2
b= (x**2)**2
d= (2*y**2)**2
e= (2*y)**2
  WRITE(*,*) ' 3. (x**2)**2 - (2*y**2)**2 - (2*y)**2 = ', z
  WRITE(*,*) '    (x**2)**2 = ', b
  WRITE(*,*) '    (2*y**2)**2 = ', d
  WRITE(*,*) '    (2*y)**2 ', e
z= (x**2)**2 - (2*y)**2*(y**2 + 1)
b= (x**2)**2
d= (2*y)**2*(y**2 + 1)
  WRITE(*,*) ' 4. (x**2)**2 - (2*y)**2*(y**2 + 1) = ', z
  WRITE(*,*) '    (x**2)**2 = ', b
  WRITE(*,*) '    (2*y)**2*(y**2 + 1) = ', d
z= (x*x - 2*y*(y+1)) * (x*x + 2*y*(y+1)) + 8*y**3
b= (x*x - 2*y*(y+1)) * (x*x + 2*y*(y+1))
d= 8*y**3
  WRITE (*,*) ' 5. (x*x-2*y*(y+1)) * (x*x+2*y*(y+1)) + 8*y**3 = ', z
  WRITE(*,*) '    (x*x - 2*y*(y+1)) * (x*x + 2*y*(y+1)) = ', b
  WRITE(*,*) '    8*y**3) = ', d
z= (x**2 - 2*y**2) * (x**2 + 2*y**2) - 4*y**2
b= (x**2 - 2*y**2) * (x**2 + 2*y**2)
d= 4*y**2
  WRITE (*,*) ' 6. (x**2-2*y**2) * (x**2+2*y**2) - 4*y**2 = ', z
  WRITE(*,*) '    x**2 - 2*y**2) * (x**2 + 2*y**2) = ', b
  WRITE(*,*) '    4*y**2 = ', d
END PROGRAM RoundingError
