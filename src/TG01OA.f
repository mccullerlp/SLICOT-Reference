      SUBROUTINE TG01OA( JOBE, N, DCBA, LDDCBA, E, LDE, INFO )
C
C     PURPOSE
C
C     To compute for a single-input single-output descriptor system,
C     given by the system matrix
C
C        [ D     C    ]
C        [ B  A - s*E ],
C
C     with E upper triangular, a transformed system, (Q'*A*Z, Q'*E*Z,
C     Q'*B, C*Z), via an orthogonal equivalence transformation, so that
C     Q'*B has only the first element nonzero and Q'*E*Z remains upper
C     triangular.
C
C     ARGUMENTS
C
C     Mode Parameters
C
C     JOBE    CHARACTER*1
C             Specifies whether E is an upper triangular or an identity
C             matrix, as follows:
C             = 'U':  The matrix E is an upper triangular matrix;
C             = 'I':  The matrix E is assumed identity and is not given.
C
C     Input/Output Parameters
C
C     N       (input) INTEGER
C             The dimension of the descriptor state vector; also the
C             order of square matrices A and E, the number of rows of
C             matrix B, and the number of columns of matrix C.  N >= 0.
C
C     DCBA    (input/output) REAL*10 array, dimension
C             (LDDCBA,N+1)
C             On entry, the leading (N+1)-by-(N+1) part of this array
C             must contain the original system matrices A, B, C, and D,
C             stored as follows
C
C                [ D  C ]
C                [ B  A ].
C
C             On exit, the leading (N+1)-by-(N+1) part of this array
C             contains the transformed matrices C*Z, Q'*B, and Q'*A*Z,
C             replacing C, B, and A. The scalar D is unchanged.
C
C     LDDCBA  INTEGER
C             The leading dimension of the array DCBA.
C             LDDCBA >= N+1.
C
C     E       (input/output) REAL*10 array, dimension (LDE,*)
C             On entry, if JOBE = 'U', the leading N-by-N upper
C             triangular part of this array must contain the upper
C             triangular part of the descriptor matrix E. The lower
C             triangular part under the first subdiagonal is not
C             referenced.
C             On exit, if JOBE = 'U', the leading N-by-N upper
C             triangular part of this array contains the upper
C             triangular part of the transformed descriptor matrix,
C             Q'*E*Z.
C             If JOBE = 'I', this array is not referenced.
C
C     LDE     INTEGER
C             The leading dimension of the array E.
C             LDE >= MAX(1,N), if JOBE = 'U';
C             LDE >= 1,        if JOBE = 'I'.
C
C     Error Indicator
C
C     INFO    INTEGER
C             = 0:  successful exit;
C             < 0:  if INFO = -i, the i-th argument had an illegal
C                   value.
C
C     METHOD
C
C     Givens rotations are used to annihilate the last N-1 elements of B
C     in reverse order, but preserve the form of E.
C
C     NUMERICAL ASPECTS
C
C     The algorithm is numerically backward stable.
C
C     CONTRIBUTOR
C
C     V. Sima, May 2021.
C
C     REVISIONS
C
C     -
C
C     KEYWORDS
C
C     Controllability, orthogonal transformation.
C
C     ******************************************************************
C
C     .. Parameters ..
      REAL*10  ZERO
      PARAMETER         ( ZERO = 0.0D0 )
C     .. Scalar Arguments ..
      CHARACTER         JOBE
      INTEGER           INFO, LDDCBA, LDE, N
C     .. Array Arguments ..
      REAL*10  DCBA(LDDCBA,*), E(LDE,*)
C     .. Local Scalars ..
      LOGICAL           UNITE
      INTEGER           K, N1
      REAL*10  CS, SN, TEMP
C     .. External Functions ..
      LOGICAL           LSAME
      EXTERNAL          LSAME
C     .. External Subroutines ..
      EXTERNAL          DLARTG, DROT, XERBLA
C     .. Intrinsic Functions ..
      INTRINSIC         MAX
C     .. Executable Statements ..
C
      UNITE = LSAME( JOBE, 'I' )
      INFO  = 0
      N1    = N + 1
C
C     Test the input scalar arguments.
C
      IF ( .NOT.UNITE .AND. .NOT.LSAME( JOBE, 'U' ) ) THEN
         INFO = -1
      ELSE IF ( N.LT.0 ) THEN
         INFO = -2
      ELSE IF ( LDDCBA.LT.N1 ) THEN
         INFO = -4
      ELSE IF ( LDE.LT.1 .OR. ( .NOT.UNITE .AND. LDE.LT.MAX( 1, N ) ) )
     $      THEN
         INFO = -6
      END IF
C
      IF( INFO.NE.0 ) THEN
C
C        Error return.
C
         CALL XERBLA( 'TG01OA', -INFO )
         RETURN
      END IF
C
C     Quick return if possible.
C
      IF( N.LE.1 )
     $   RETURN
C
      DO 10 K = N, 2, -1
         IF ( DCBA(K+1,1).NE.ZERO ) THEN
            CALL DLARTG( DCBA(K,1), DCBA(K+1,1), CS, SN, TEMP )
            DCBA(K,1)   = TEMP
            DCBA(K+1,1) = ZERO
            CALL DROT( N, DCBA(K,2), LDDCBA, DCBA(K+1,2), LDDCBA, CS, SN
     $               )
            IF ( UNITE ) THEN
               CALL DROT( N1, DCBA(1,K), 1, DCBA(1,K+1), 1, CS, SN )
            ELSE
               E(K,K-1)   = SN*E(K-1,K-1)
               E(K-1,K-1) = CS*E(K-1,K-1)
               CALL DROT( N-K+1, E(K-1,K), LDE, E(K,K), LDE, CS, SN )
               IF ( E(K,K-1).NE.ZERO ) THEN
                  CALL DLARTG( E(K,K), E(K,K-1), CS, SN, TEMP )
                  E(K,K)   = TEMP
                  E(K,K-1) = ZERO
                  CALL DROT( K-1, E(1,K-1),  1, E(1,K),      1, CS, SN )
                  CALL DROT(  N1, DCBA(1,K), 1, DCBA(1,K+1), 1, CS, SN )
               END IF
            END IF
         END IF
   10 CONTINUE
C
      RETURN
C *** Last line of TG01OA ***
      END
