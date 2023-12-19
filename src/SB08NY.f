      SUBROUTINE SB08NY( DA, A, B, EPSB )
C
C     PURPOSE
C
C     To compute the coefficients of B(z) = A(1/z) * A(z) and a norm for
C     the accuracy of the computed coefficients.
C
C     ARGUMENTS
C
C     Input/Output Parameters
C
C     DA      (input) INTEGER
C             The degree of the polynomials A(z) and B(z).  DA >= 0.
C
C     A       (input) REAL*16 array, dimension (DA+1)
C             This array must contain the coefficients of the polynomial
C             A(z) in increasing powers of z.
C
C     B       (output) REAL*16 array, dimension (DA+1)
C             This array contains the coefficients of the polynomial
C             B(z).
C
C     EPSB    (output) REAL*16
C             A value used for checking the accuracy of the computed
C             coefficients.
C
C     CONTRIBUTOR
C
C     Release 3.0: V. Sima, Katholieke Univ. Leuven, Belgium, Aug. 1997.
C     Supersedes Release 2.0 routine SB08BZ by A.J. Geurts.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     Laplace transform, polynomial operations, spectral factorization.
C
C     ******************************************************************
C
C     .. Parameters ..
      REAL*16  THREE
      PARAMETER         ( THREE = 3.0D0 )
C     .. Scalar Arguments ..
      INTEGER           DA
      REAL*16  EPSB
C     .. Array Arguments ..
      REAL*16  A(*), B(*)
C     .. Local Scalars ..
      INTEGER           I
C     .. External Functions ..
      REAL*16  DDOT, DLAMCH
      EXTERNAL          DDOT, DLAMCH
C     .. Executable Statements ..
C
      DO 20 I = 1, DA + 1
         B(I) = DDOT( DA-I+2, A(1), 1, A(I), 1 )
   20 CONTINUE
C
      EPSB = THREE*DLAMCH( 'Epsilon' )*B(1)
C
      RETURN
C *** Last line of SB08NY ***
      END
