MODULE TYPES
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: SP, DP, QP
  PUBLIC :: F, dF, ndf

  INTEGER, PARAMETER :: SP = selected_real_KIND(6)
  INTEGER, PARAMETER :: DP = selected_real_KIND(8)
  INTEGER, PARAMETER :: QP = Selected_Real_KIND(p=16)

  REAL(KIND=SP), PARAMETER :: sPI = 3.141592653589793238462643383279502884197_SP
  REAL(KIND=DP), PARAMETER :: dPI = 3.141592653589793238462643383279502884197_DP

  TYPE snode
     REAL(KIND=SP) :: X, Y, Z
  END TYPE snode

  TYPE dnode
     REAL(KIND=DP) :: X, Y, Z
  END TYPE dnode

CONTAINS

  REAL(kind=DP) FUNCTION f(x)
    IMPLICIT NONE
    !INTEGER, PARAMETER :: DP = selected_real_KIND(8)
    REAL(DP), INTENT(in) :: x
    f = (x+1)*(x-3)
  END FUNCTION f

  REAL(kind=DP) FUNCTION dF(x)
    IMPLICIT NONE
    REAL(DP), INTENT(in) :: x
    dF = 2*x - 2.0
  END FUNCTION dF

  ! Numeric derivative
  REAL(kind=DP) FUNCTION ndF(x)
    IMPLICIT NONE
    REAL(DP), INTENT(in) :: x
    !REAL(kind=16) :: f
    REAL(DP) :: h
    h = 0.001_dp
    ndF = ( f(x+h) - f(x) ) / h
  END FUNCTION ndF

END MODULE TYPES



! 1D gradient descent method
PROGRAM MAIN
  USE types, ONLY: F, dF => nDF, WP => DP
  IMPLICIT NONE

  INTEGER, Parameter :: max_iter = 100 ! Maximum number of iterations
  REAL(WP), Parameter :: step = 0.05_wp  ! step size
  REAL(WP), Parameter :: threshold = 0.00001_wp ! Convergence threshold

  REAL(WP) :: conv = threshold + 10.0_wp
  REAL(WP) :: x0, y0
  REAL(WP) :: tmp_y = 0.0_wp
  INTEGER :: iter =0


  ! ---
  x0 = 2.0_wp ! initial starting point; guess.
  y0 = f(x0)

  PRINT *, REPEAT("=", 42)
  PRINT *, "Iter       x          f(x)        conv"
  PRINT *, REPEAT("=", 42)

  DO WHILE( conv > threshold .AND. iter < max_iter)
     x0 = x0 - step * Df(x0)
     y0 = f(x0)
     iter = iter + 1
     conv = ABS( tmp_y - y0 )
     tmp_y = y0
     PRINT 100, iter, x0, y0, conv
  END DO
  PRINT *, REPEAT("-", 42)

  100 FORMAT(I3, 3X, 3F12.8)
  PRINT *, "The End!"
  STOP 0
END PROGRAM MAIN
