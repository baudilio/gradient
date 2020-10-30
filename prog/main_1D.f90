! 1D gradient descent method
PROGRAM MAIN
  !USE types, ONLY : F, df, wp=>sp
  USE types, ONLY : F, df=>df, wp=>sp
  IMPLICIT NONE

  INTEGER, PARAMETER :: max_iter = 100 ! Maximum number of iterations
  REAL(WP), PARAMETER :: step = 0.1_wp  ! step size
  REAL(WP), PARAMETER :: threshold = 0.00001_wp ! Convergence threshold

  REAL(WP) :: conv = threshold + 10.0_wp
  REAL(WP) :: x0, y0
  REAL(WP) :: tmp_y = 0.0_wp
  INTEGER :: iter = 0


  ! ---
  x0 = 2.0_wp ! initial starting point; guess.
  y0 = f(x0)

  PRINT *, KIND(x0)

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
  IF (iter == max_iter) THEN
     PRINT *, "WARNING: maximum number of iteretions reached!"
     PRINT *, "Convergence NOT attained."
  ENDIF
  100 FORMAT(I3, 3X, 3F12.8)
  PRINT *, "The End!"
  STOP 0
END PROGRAM MAIN
