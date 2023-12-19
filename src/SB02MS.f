      LOGICAL FUNCTION SB02MS( REIG, IEIG )
C
C     PURPOSE
C
C     To select the unstable eigenvalues for solving the discrete-time
C     algebraic Riccati equation.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     REIG    (input) REAL*10
C             The real part of the current eigenvalue considered.
C
C     IEIG    (input) REAL*10
C             The imaginary part of the current eigenvalue considered.
C
C     METHOD
C
C     The function value SB02MS is set to .TRUE. for an unstable
C     eigenvalue (i.e., with modulus greater than or equal to one) and
C     to .FALSE., otherwise.
C
C     REFERENCES
C
C     None.
C
C     NUMERICAL ASPECTS
C
C     None.
C
C     CONTRIBUTOR
C
C     V. Sima, Katholieke Univ. Leuven, Belgium, Aug. 1997.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     Algebraic Riccati equation, closed loop system, discrete-time
C     system, optimal regulator, Schur form.
C
C     ******************************************************************
C
C     .. Parameters ..
      REAL*10  ONE
      PARAMETER         ( ONE = 1.0D0 )
C     .. Scalar Arguments ..
      REAL*10  IEIG, REIG
C     .. External Functions ..
      REAL*10   DLAPY2
      EXTERNAL           DLAPY2
C     .. Executable Statements ..
C
      SB02MS = DLAPY2( REIG, IEIG ).GE.ONE
C
      RETURN
C *** Last line of SB02MS ***
      END
