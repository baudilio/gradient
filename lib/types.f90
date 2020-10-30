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

  INTERFACE f
     MODULE PROCEDURE f_s, f_d, f_q
  END INTERFACE f
  INTERFACE df
     MODULE PROCEDURE df_s, df_d, df_q
  END INTERFACE df
  INTERFACE ndf
     MODULE PROCEDURE ndf_s, ndf_d, ndf_q
  END INTERFACE ndf


CONTAINS

  REAL(kind=SP) FUNCTION f_s(x)
    IMPLICIT NONE
    REAL(SP), INTENT(in) :: x
    f_s = (x+1)*(x-3)
  END FUNCTION f_s
  REAL(kind=DP) FUNCTION f_d(x)
    IMPLICIT NONE
    REAL(DP), INTENT(in) :: x
    f_d = (x+1)*(x-3)
  END FUNCTION f_d
  REAL(kind=QP) FUNCTION f_q(x)
    IMPLICIT NONE
    REAL(QP), INTENT(in) :: x
    f_q = (x+1)*(x-3)
  END FUNCTION f_q


  REAL(kind=SP) FUNCTION dF_s(x)
    IMPLICIT NONE
    REAL(SP), INTENT(in) :: x
    dF_s = 2*x - 2.0_sp
  END FUNCTION dF_s
  REAL(kind=DP) FUNCTION dF_d(x)
    IMPLICIT NONE
    REAL(DP), INTENT(in) :: x
    dF_d = 2*x - 2.0_dp
  END FUNCTION dF_d
  REAL(kind=QP) FUNCTION dF_q(x)
    IMPLICIT NONE
    REAL(QP), INTENT(in) :: x
    dF_q = 2*x - 2.0_qp
  END FUNCTION dF_q

  ! Numeric derivative
  REAL(kind=SP) FUNCTION ndF_s(x)
    IMPLICIT NONE
    REAL(SP), INTENT(in) :: x
    REAL(SP) :: h
    h = 0.001_sp
    ndF_s = ( f_s(x+h) - f_s(x) ) / h
  END FUNCTION ndF_s
  REAL(kind=DP) FUNCTION ndF_d(x)
    IMPLICIT NONE
    REAL(DP), INTENT(in) :: x
    REAL(DP) :: h
    h = 0.001_dp
    ndF_d = ( f_d(x+h) - f_d(x) ) / h
  END FUNCTION ndF_d
  REAL(kind=QP) FUNCTION ndF_q(x)
    IMPLICIT NONE
    REAL(QP), INTENT(in) :: x
    REAL(QP) :: h
    h = 0.001_qp
    ndF_q = ( f_q(x+h) - f_q(x) ) / h
  END FUNCTION ndF_q

END MODULE TYPES
