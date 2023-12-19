      SUBROUTINE NF01BB( IFLAG, NFUN, LX, IPAR, LIPAR, U, LDU, Y, LDY,
     $                   X, NFEVL, E, J, LDJ, JTE, DWORK, LDWORK, INFO )
C
C     PURPOSE
C
C     This is the FCN routine for optimizing all parameters of a Wiener
C     system using SLICOT Library routine MD03AD. See the argument FCN
C     in the routine MD03AD for the description of parameters.
C
C     ******************************************************************
C
C     .. Parameters ..
C     .. CJTE is initialized to activate the calculation of J'*e ..
C     .. NOUT is the unit number for printing intermediate results ..
      CHARACTER         CJTE
      PARAMETER         ( CJTE = 'C' )
      INTEGER           NOUT
      PARAMETER         ( NOUT = 6 )
      REAL*10  ONE
      PARAMETER         ( ONE = 1.0D0 )
C     .. Scalar Arguments ..
      INTEGER           IFLAG, INFO, LDJ, LDU, LDWORK, LDY, LIPAR, LX,
     $                  NFEVL, NFUN
C     .. Array Arguments ..
      INTEGER           IPAR(*)
      REAL*10  DWORK(*), E(*), J(LDJ,*), JTE(*), U(LDU,*),
     $                  X(*), Y(LDY,*)
C     .. Local Scalars ..
      INTEGER           BSN, I, JWORK, L, M, N, NN, NSMP, ST
      REAL*10  ERR
C     .. External Functions ..
      REAL*10  DNRM2
      EXTERNAL          DNRM2
C     .. External Subroutines ..
      EXTERNAL          DAXPY, NF01AD, NF01BD
C
C     .. Executable Statements ..
C
      L = IPAR(2)
      M = IPAR(5)
      N = IPAR(6)
      IF ( L.EQ.0 ) THEN
         NSMP = NFUN
      ELSE
         NSMP = NFUN/L
      END IF
C
      INFO = 0
      IF ( IFLAG.EQ.1 ) THEN
C
C        Call NF01AD to compute the output y of the Wiener system (in E)
C        and then the error functions (also in E). The array U must
C        contain the input to the linear part of the Wiener system, and
C        Y must contain the original output Y of the Wiener system.
C        IPAR(6) must contain the number of states of the linear part, n.
C        Workspace: need:    NFUN + MAX( 2*NN, (N + L)*(N + M) + 2*N +
C                                        MAX( N*(N + L), N + M + L ) ),
C                                                               if M>0,
C                            NFUN + MAX( 2*NN, (N + L)*N + 2*N +
C                                        MAX( N*(N + L), L ) ), if M=0,
C                            where NN = IPAR(7) (number of neurons);
C                   prefer:  larger.
C
         CALL NF01AD( NSMP, M, L, IPAR(6), LIPAR-2, X, LX, U, LDU, E,
     $                NSMP, DWORK, LDWORK, INFO )
C
         DO 10 I = 1, L
            CALL DAXPY( NSMP, -ONE, Y(1,I), 1, E((I-1)*NSMP+1), 1 )
   10    CONTINUE
C
         DWORK(1) = NFUN + MAX( 2*IPAR(7), (N + L)*(N + M) + 2*N +
     $                          MAX( N*(N + L), N + M + L ) )
C
      ELSE IF ( IFLAG.EQ.2 ) THEN
C
C        Call NF01BD to compute the Jacobian in a compressed form.
C        Workspace: need:    2*NFUN + MAX( 2*NN, (N + L)*(N + M) + 2*N +
C                                          MAX( N*(N + L), N + M + L )),
C                                                              if M > 0,
C                            2*NFUN + MAX( 2*NN, (N + L)*N + 2*N +
C                                          MAX( N*(N + L), L ) ),
C                                                              if M = 0;
C                   prefer:  larger.
C
         CALL NF01BD( CJTE, NSMP, M, L, IPAR(6), LIPAR-2, X, LX, U,
     $                LDU, E, J, LDJ, JTE, DWORK, LDWORK, INFO )
         NFEVL = IPAR(6)*( M + L + 1 ) + L*M
         DWORK(1) = 2*NFUN + MAX( 2*IPAR(7), (N + L)*(N + M) + 2*N +
     $                            MAX( N*(N + L), N + M + L ) )
C
      ELSE IF ( IFLAG.EQ.3 ) THEN
C
C        Set the parameter LDJ, the length of the array J, and the sizes
C        of the workspace for FCN (IFLAG = 1 or 2), and JTJ.
C
         ST  = IPAR(1)
         BSN = IPAR(4)
         NN  = IPAR(7)
C
         LDJ     = NFUN
         IPAR(1) = NFUN*( BSN + ST )
         IF ( M.GT.0 ) THEN
            JWORK = MAX( N*( N + L ), N + M + L )
         ELSE
            JWORK = MAX( N*( N + L ), L )
         END IF
         IPAR(2) = LDJ + MAX( ( N + L )*( N + M ) + 2*N + JWORK, 2*NN )
         IPAR(3) = LDJ + IPAR(2)
         IPAR(4) = 0
         IPAR(5) = NFUN
C
      ELSE IF ( IFLAG.EQ.0 ) THEN
C
C        Special call for printing intermediate results.
C
         ERR = DNRM2( NFUN, E, 1 )
         WRITE( NOUT, '('' Norm of current error = '', D15.6)') ERR
      END IF
      RETURN
C
C *** Last line of NF01BB ***
      END
