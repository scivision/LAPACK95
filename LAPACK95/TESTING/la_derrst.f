      SUBROUTINE DERRST( PATH, NUNIT )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     April 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER*3        PATH
      INTEGER            NUNIT
*     ..
*
*  Purpose
*  =======
*
*  DERRST tests the error exits for DSYTRD, DORGTR, DORMTR, DSPTRD,
*  DOPGTR, DOPMTR, DSTEQR, SSTERF, SSTEBZ, SSTEIN, DPTEQR, DSBTRD,
*  DSYEV, SSYEVX, SSYEVD, DSBEV, SSBEVX, SSBEVD,
*  DSPEV, SSPEVX, SSPEVD, DSTEV, SSTEVX, SSTEVD, and SSTEDC.
*
*  Arguments
*  =========
*
*  PATH    (input) CHARACTER*3
*          The LAPACK path name for the routines to be tested.
*
*  NUNIT   (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     NMAX has to be at least 3 or LIW may be too small
*     .. Parameters ..
      INTEGER            NMAX, LIW, LW
      PARAMETER          ( NMAX = 3, LIW = 12*NMAX, LW = 20*NMAX )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, INFO, J, M, N, NSPLIT, NT
*     ..
*     .. Local Arrays ..
      INTEGER            I1( NMAX ), I2( NMAX ), I3( NMAX ), IW( LIW )
      DOUBLE PRECISION   A( NMAX, NMAX ), C( NMAX, NMAX ), D( NMAX ),
     $                   E( NMAX ), Q( NMAX, NMAX ), R( NMAX ),
     $                   TAU( NMAX ), W( LW ), X( NMAX ),
     $                   Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHKXER, DOPGTR, DOPMTR, DORGTR, DORMTR, DPTEQR,
     $                   LA_TEST_DSBEV,  LA_TEST_DSBEVD, DSBEVX, DSBTRD,
     $                   LA_TEST_DSPEV, LA_TEST_DSPEVD, LA_TEST_DSPEVX,
     $                   DSPTRD, DSTEBZ, DSTEDC, DSTEIN, DSTEQR, DSTERF,
     $                   LA_TEST_DSTEV, LA_TEST_DSTEVD, LA_TEST_DSTEVR,
     $                   LA_TEST_DSTEVX, LA_TEST_DSYEV, LA_TEST_DSYEVD,
     $                   LA_TEST_DSYEVR, DSYEVX, DSYTRD
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*6        SRNAMT
      INTEGER            INFOT, NOUT
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, NOUT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      WRITE( NOUT, FMT = * )
      C2 = PATH( 2: 3 )
*
*     Set the variables to innocuous values.
*
      DO 20 J = 1, NMAX
         DO 10 I = 1, NMAX
            A( I, J ) = 1.D0 / DBLE( I+J )
   10    CONTINUE
   20 CONTINUE
      DO 30 J = 1, NMAX
         D( J ) = DBLE( J )
         E( J ) = 0.0D0
         I1( J ) = J
         I2( J ) = J
         TAU( J ) = 1.D0
   30 CONTINUE
      OK = .TRUE.
      NT = 0
*
*     Test error exits for the ST path.
*
!      
      IF( LSAMEN( 2, C2, 'ST' ) ) THEN
*
*        DSYTRD
*
         SRNAMT = 'DSYTRD'
         INFOT = 1
         CALL DSYTRD( '/', 0, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'DSYTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSYTRD( 'U', -1, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'DSYTRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DSYTRD( 'U', 2, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'DSYTRD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DSYTRD( 'U', 0, A, 1, D, E, TAU, W, 0, INFO )
         CALL CHKXER( 'DSYTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        DORGTR
*
         SRNAMT = 'DORGTR'
         INFOT = 1
         CALL DORGTR( '/', 0, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'DORGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DORGTR( 'U', -1, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'DORGTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DORGTR( 'U', 2, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'DORGTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DORGTR( 'U', 3, A, 3, TAU, W, 1, INFO )
         CALL CHKXER( 'DORGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        DORMTR
*
         SRNAMT = 'DORMTR'
         INFOT = 1
         CALL DORMTR( '/', 'U', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DORMTR( 'L', '/', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DORMTR( 'L', 'U', '/', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DORMTR( 'L', 'U', 'N', -1, 0, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DORMTR( 'L', 'U', 'N', 0, -1, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DORMTR( 'L', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DORMTR( 'R', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DORMTR( 'L', 'U', 'N', 2, 0, A, 2, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL DORMTR( 'L', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL DORMTR( 'R', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INFO )
         CALL CHKXER( 'DORMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        DSPTRD
*
         SRNAMT = 'DSPTRD'
         INFOT = 1
         CALL DSPTRD( '/', 0, A, D, E, TAU, INFO )
         CALL CHKXER( 'DSPTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSPTRD( 'U', -1, A, D, E, TAU, INFO )
         CALL CHKXER( 'DSPTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 2
*
*        DOPGTR
*
         SRNAMT = 'DOPGTR'
         INFOT = 1
         CALL DOPGTR( '/', 0, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'DOPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DOPGTR( 'U', -1, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'DOPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DOPGTR( 'U', 2, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'DOPGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        DOPMTR
*
         SRNAMT = 'DOPMTR'
         INFOT = 1
         CALL DOPMTR( '/', 'U', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'DOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DOPMTR( 'L', '/', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'DOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DOPMTR( 'L', 'U', '/', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'DOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DOPMTR( 'L', 'U', 'N', -1, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'DOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DOPMTR( 'L', 'U', 'N', 0, -1, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'DOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DOPMTR( 'L', 'U', 'N', 2, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'DOPMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        DPTEQR
*
         SRNAMT = 'DPTEQR'
         INFOT = 1
         CALL DPTEQR( '/', 0, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DPTEQR( 'N', -1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DPTEQR( 'V', 2, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DPTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        DSTEBZ
*
         SRNAMT = 'DSTEBZ'
         INFOT = 1
         CALL DSTEBZ( '/', 'E', 0, 0.0D0, 1.0D0, 1, 0, 0.0D0, D, E, M,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSTEBZ( 'A', '/', 0, 0.0D0, 0.0D0, 0, 0, 0.0D0, D, E, M,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DSTEBZ( 'A', 'E', -1, 0.0D0, 0.0D0, 0, 0, 0.0D0, D, E, M,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DSTEBZ( 'V', 'E', 0, 0.0D0, 0.0D0, 0, 0, 0.0D0, D, E, M,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DSTEBZ( 'I', 'E', 0, 0.0D0, 0.0D0, 0, 0, 0.0D0, D, E, M,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DSTEBZ( 'I', 'E', 1, 0.0D0, 0.0D0, 2, 1, 0.0D0, D, E, M,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DSTEBZ( 'I', 'E', 1, 0.0D0, 0.0D0, 1, 0, 0.0D0, D, E, M,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'DSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DSTEBZ( 'I', 'E', 1, 0.0D0, 0.0D0, 1, 2, 0.0D0, D, E, M,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'DSTEBZ', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*        DSTEIN
*
         SRNAMT = 'DSTEIN'
         INFOT = 1
         CALL DSTEIN( -1, D, E, 0, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL CHKXER( 'DSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DSTEIN( 0, D, E, -1, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL CHKXER( 'DSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DSTEIN( 0, D, E, 1, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL CHKXER( 'DSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DSTEIN( 2, D, E, 0, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL CHKXER( 'DSTEIN', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        DSTEQR
*
         SRNAMT = 'DSTEQR'
         INFOT = 1
         CALL DSTEQR( '/', 0, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSTEQR( 'N', -1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DSTEQR( 'V', 2, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        DSTERF
*
         SRNAMT = 'DSTERF'
         INFOT = 1
         CALL DSTERF( -1, D, E, INFO )
         CALL CHKXER( 'DSTERF', INFOT, NOUT, LERR, OK )
         NT = NT + 1
*
*        DSTEDC
*
         SRNAMT = 'DSTEDC'
         INFOT = 1
         CALL DSTEDC( '/', 0, D, E, Z, 1, W, 1, IW, 1, INFO )
         CALL CHKXER( 'DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSTEDC( 'N', -1, D, E, Z, 1, W, 1, IW, 1, INFO )
         CALL CHKXER( 'DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DSTEDC( 'V', 2, D, E, Z, 1, W, 23, IW, 28, INFO )
         CALL CHKXER( 'DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DSTEDC( 'N', 1, D, E, Z, 1, W, 0, IW, 1, INFO )
         CALL CHKXER( 'DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DSTEDC( 'I', 2, D, E, Z, 2, W, 12, IW, 12, INFO )
         CALL CHKXER( 'DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DSTEDC( 'V', 2, D, E, Z, 2, W, 22, IW, 28, INFO )
         CALL CHKXER( 'DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DSTEDC( 'N', 1, D, E, Z, 1, W, 1, IW, 0, INFO )
         CALL CHKXER( 'DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DSTEDC( 'I', 2, D, E, Z, 2, W, 19, IW, 11, INFO )
         CALL CHKXER( 'DSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DSTEDC( 'V', 2, D, E, Z, 2, W, 23, IW, 27, INFO )
         CALL CHKXER( 'DSTEDC', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        DSTEVD
*
         SRNAMT = 'DSTEVD'
         DO I = 2,3
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_DSTEVD('N', NMAX, D, E, Z, NMAX, W, NMAX, IW,
     $         NMAX, INFO)
             CALL CHKXER( 'DSTEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DSTEVD('V', NMAX, D, E, Z, NMAX, W, NMAX, IW,
     $         NMAX, INFO)
             CALL CHKXER( 'DSTEVD ', INFOT, NOUT, LERR, OK )
           END DO
         END DO 
         
*
*        DSTEV
*
         SRNAMT = 'DSTEV '
         DO I = 2,3
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_DSTEV('N', NMAX, D, E, Z, NMAX, W, INFO)
             CALL CHKXER( 'DSTEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DSTEV('V', NMAX, D, E, Z, NMAX, W, INFO)
             CALL CHKXER( 'DSTEV ', INFOT, NOUT, LERR, OK )
           END DO
         END DO 
         
*
*        DSTEVX
*
         SRNAMT = 'DSTEVX'
         DO I = 2, 10
           INFOT = I
           IF (I/=9) THEN
             DO J = 1, 1
               INFO = J + 100*I
               CALL LA_TEST_DSTEVX( 'V', 'A', NMAX, D, E, 0.0D0, 0.0D0,
     &           0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'DSTEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSTEVX( 'N', 'A', NMAX, D, E, 0.0D0, 0.0D0,
     &           0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'DSTEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSTEVX( 'V', 'I', NMAX, D, E, 0.0D0, 0.0D0,
     &           0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'DSTEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSTEVX( 'N', 'I', NMAX, D, E, 0.0D0, 0.0D0,
     &           0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'DSTEVX', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DSTEVX( 'V', 'V', NMAX, D, E, 0.0D0, 0.0D0,
     &           0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'DSTEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSTEVX( 'N', 'V', NMAX, D, E, 0.0D0, 0.0D0,
     &           0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'DSTEVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDIF
         ENDDO
         NT = NT + 9
*
*        DSTEVR
*
         N = 1
         SRNAMT = 'DSTEVR'
         DO I = 2,8
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_DSTEVR( 'N', 'A', NMAX, D, E, 0.0D0,
     $           0.0D0, NMAX-1, NMAX, 0.0D0, M,
     $           R, Z, NMAX, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
               CALL CHKXER( 'DSTEVR', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DSTEVR( 'N', 'V', NMAX, D, E, 0.0D0,
     $           0.0D0, NMAX, NMAX, 0.0D0, M,
     $           R, Z, NMAX, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
               CALL CHKXER( 'DSTEVR', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DSTEVR( 'N', 'I', NMAX, D, E, 0.0D0,
     $           0.0D0, NMAX, NMAX, 0.0D0, M,
     $           R, Z, NMAX, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
               CALL CHKXER( 'DSTEVR', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DSTEVR( 'V', 'A', NMAX, D, E, 0.0D0,
     $           0.0D0, NMAX, NMAX, 0.0D0, M,
     $           R, Z, NMAX, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
               CALL CHKXER( 'DSTEVR', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DSTEVR( 'V', 'V', NMAX, D, E, 0.0D0,
     $           0.0D0, NMAX, NMAX, 0.0D0, M,
     $           R, Z, NMAX, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
               CALL CHKXER( 'DSTEVR', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DSTEVR( 'V', 'I', NMAX, D, E, 0.0D0,
     $           0.0D0, NMAX, NMAX, 0.0D0, M,
     $           R, Z, NMAX, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
               CALL CHKXER( 'DSTEVR', INFOT, NOUT, LERR, OK ) 
             ENDDO
           ENDDO
         NT = NT + 9
*
*        DSYEVD
*
         SRNAMT = 'DSYEVD'
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_DSYEVD('N', 'U', NMAX, A, NMAX, X, W, 1,
     $         IW, 1, INFO )
             CALL CHKXER( 'DSYEVD', INFOT, NOUT, LERR, OK )
             
             INFO = J + 100*I
             CALL LA_TEST_DSYEVD('N', 'L', NMAX, A, NMAX, X, W, 1,
     $         IW, 1, INFO )
             CALL CHKXER( 'DSYEVD', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DSYEVD('V', 'U', NMAX, A, NMAX, X, W, 1,
     $         IW, 1, INFO )
             CALL CHKXER( 'DSYEVD', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DSYEVD('V', 'L', NMAX, A, NMAX, X, W, 1,
     $         IW, 1, INFO )
             CALL CHKXER( 'DSYEVD', INFOT, NOUT, LERR, OK )
           END DO
         END DO             

*
*        DSYEVR
*
         SRNAMT = 'DSYEVR'
         DO I = 1,10
           IF (I/=9) THEN
             INFOT = I
             DO J = 1, 1
               INFO = J + 100*I
               CALL LA_TEST_DSYEVR( 'N', 'A', 'U', NMAX, A, NMAX, 0.0D0,
     $           0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     $           IW( 2*N+1 ), 10*N, INFO )
               CALL CHKXER( 'DSYEVR', INFOT, NOUT, LERR, OK )
      
               INFO = J + 100*I 
               CALL LA_TEST_DSYEVR( 'N', 'A', 'L', NMAX, A, NMAX, 0.0D0,
     $           0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     $           IW( 2*N+1 ), 10*N, INFO )
               CALL CHKXER( 'DSYEVR', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DSYEVR( 'N', 'V', 'U', NMAX, A, NMAX, 0.0D0,
     $           0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     $           IW( 2*N+1 ), 10*N, INFO )
               CALL CHKXER( 'DSYEVR', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DSYEVR( 'N', 'V', 'L', NMAX, A, NMAX, 0.0D0,
     $           0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     $           IW( 2*N+1 ), 10*N, INFO )
               CALL CHKXER( 'DSYEVR', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSYEVR( 'N', 'I', 'U', NMAX, A, NMAX, 0.0D0,
     $           0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     $           IW( 2*N+1 ), 10*N, INFO )
               CALL CHKXER( 'DSYEVR', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DSYEVR( 'N', 'I', 'L', NMAX, A, NMAX, 0.0D0,
     $           0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     $           IW( 2*N+1 ), 10*N, INFO )
               CALL CHKXER( 'DSYEVR', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSYEVR( 'V', 'A', 'U', NMAX, A, NMAX, 0.0D0,
     $           0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     $           IW( 2*N+1 ), 10*N, INFO )
               CALL CHKXER( 'DSYEVR', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSYEVR( 'V', 'A', 'L', NMAX, A, NMAX, 0.0D0,
     $           0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     $           IW( 2*N+1 ), 10*N, INFO )
               CALL CHKXER( 'DSYEVR', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSYEVR( 'V', 'V', 'U', NMAX, A, NMAX, 0.0D0,
     $           0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     $           IW( 2*N+1 ), 10*N, INFO )
               CALL CHKXER( 'DSYEVR', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSYEVR( 'V', 'V', 'L', NMAX, A, NMAX, 0.0D0,
     $           0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     $           IW( 2*N+1 ), 10*N, INFO )
               CALL CHKXER( 'DSYEVR', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSYEVR( 'V', 'I', 'U', NMAX, A, NMAX, 0.0D0,
     $           0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     $           IW( 2*N+1 ), 10*N, INFO )
               CALL CHKXER( 'DSYEVR', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSYEVR( 'V', 'I', 'L', NMAX, A, NMAX, 0.0D0,
     $           0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     $           IW( 2*N+1 ), 10*N, INFO )
               CALL CHKXER( 'DSYEVR', INFOT, NOUT, LERR, OK )
 
             ENDDO
           ENDIF
         ENDDO
         NT = NT + 11
*
*        DSYEV
*
         SRNAMT = 'DSYEV '
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_DSYEV('N', 'U', NMAX, A, NMAX, X, W, 1,
     $         INFO )
             CALL CHKXER( 'DSYEV ', INFOT, NOUT, LERR, OK )
             
             INFO = J + 100*I
             CALL LA_TEST_DSYEV('N', 'L', NMAX, A, NMAX, X, W, 1,
     $         INFO )
             CALL CHKXER( 'DSYEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DSYEV('V', 'U', NMAX, A, NMAX, X, W, 1,
     $         INFO )
             CALL CHKXER( 'DSYEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DSYEV('V', 'L', NMAX, A, NMAX, X, W, 1,
     $         INFO )
             CALL CHKXER( 'DSYEV ', INFOT, NOUT, LERR, OK )
           END DO
         END DO      
*
*        DSYEVX
*
         SRNAMT = 'DSYEVX'
         DO I = 1, 10
           IF (I/=9) THEN
             INFOT = I
             DO J = 1, 1
               INFO = J + 100*I
               CALL LA_TEST_DSYEVX( 'N', 'A', 'U', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO ) 
               CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSYEVX( 'N', 'A', 'L', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO ) 
               CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSYEVX( 'N', 'V', 'U', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO ) 
               CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSYEVX( 'N', 'V', 'L', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO ) 
               CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DSYEVX( 'N', 'I', 'U', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO ) 
               CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSYEVX( 'N', 'I', 'L', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO ) 
               CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSYEVX( 'V', 'A', 'U', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO ) 
               CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSYEVX( 'V', 'A', 'L', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO ) 
               CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSYEVX( 'V', 'V', 'U', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO ) 
               CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSYEVX( 'V', 'V', 'L', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO ) 
               CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DSYEVX( 'V', 'I', 'U', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO ) 
               CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSYEVX( 'V', 'I', 'L', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO ) 
               CALL CHKXER( 'DSYEVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDIF
         ENDDO
         NT = NT + 12
*
*        DSPEVD
*
         SRNAMT = 'DSPEVD'
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_DSPEVD('N', 'U', NMAX, A, X, Z, 1,
     $         W, 1, IW, 1, INFO )
             CALL CHKXER( 'DSPEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DSPEVD('N', 'L', NMAX, A, X, Z, 1,
     $         W, 1 , IW, 1, INFO )
             CALL CHKXER( 'DSPEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DSPEVD('V', 'U', NMAX, A, X, Z, 1,
     $         W, 1 , IW, 1, INFO )
             CALL CHKXER( 'DSPEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DSPEVD('V', 'L', NMAX, A, X, Z, 1,
     $         W, 1 , IW, 1, INFO )
             CALL CHKXER( 'DSPEVD ', INFOT, NOUT, LERR, OK )
           END DO
         END DO 
         
*
*        DSPEV
*
         SRNAMT = 'DSPEV '
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_DSPEV('N', 'U', NMAX, A, W, Z, 1,
     $         X, INFO )
             CALL CHKXER( 'DSPEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DSPEV('N', 'L', NMAX, A, W, Z, 1,
     $         X, INFO )
             CALL CHKXER( 'DSPEV ', INFOT, NOUT, LERR, OK )
             
             INFO = J + 100*I
             CALL LA_TEST_DSPEV('V', 'U', NMAX, A, W, Z, 1,
     $         X, INFO )
             CALL CHKXER( 'DSPEV ', INFOT, NOUT, LERR, OK )
             
             INFO = J + 100*I
             CALL LA_TEST_DSPEV('V', 'L', NMAX, A, W, Z, 1,
     $         X, INFO )
             CALL CHKXER( 'DSPEV ', INFOT, NOUT, LERR, OK )
           END DO
         END DO 
*
*        DSPEVX
*
         SRNAMT = 'DSPEVX'
         DO I = 1, 10
           INFOT = I
           IF (I/=9) THEN
             DO J = 1, 1
               INFO = J + 100*I
               CALL LA_TEST_DSPEVX( 'V', 'A', 'U', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO ) 
               CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSPEVX( 'V', 'A', 'L', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO ) 
               CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DSPEVX( 'V', 'V', 'U', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO ) 
               CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSPEVX( 'V', 'V', 'L', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO ) 
               CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSPEVX( 'V', 'I', 'U', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO ) 
               CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSPEVX( 'V', 'I', 'L', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO ) 
               CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSPEVX( 'N', 'A', 'U', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO ) 
               CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSPEVX( 'N', 'A', 'L', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO ) 
               CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )

                              INFO = J + 100*I
               CALL LA_TEST_DSPEVX( 'N', 'V', 'U', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO ) 
               CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSPEVX( 'N', 'V', 'L', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO ) 
               CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSPEVX( 'N', 'I', 'U', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO ) 
               CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSPEVX( 'N', 'I', 'L', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO ) 
               CALL CHKXER( 'DSPEVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDIF
         ENDDO
         NT = NT + 10
*
*     Test error exits for the SB path.
*
      ELSE IF( LSAMEN( 2, C2, 'SB' ) ) THEN
*
*        DSBTRD
*
         SRNAMT = 'DSBTRD'
         INFOT = 1
         CALL DSBTRD( '/', 'U', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DSBTRD( 'N', '/', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DSBTRD( 'N', 'U', -1, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DSBTRD( 'N', 'U', 0, -1, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DSBTRD( 'N', 'U', 1, 1, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DSBTRD( 'V', 'U', 2, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'DSBTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        DSBEVD
*
         SRNAMT = 'DSBEVD'
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_DSBEVD('N', 'U', NMAX, NMAX, A, 1, X, Z, 1,
     $         W, 1, IW, 1, INFO )
             CALL CHKXER( 'DSBEVD', INFOT, NOUT, LERR, OK )
             INFO = J + 100*I
             CALL LA_TEST_DSBEVD('N', 'L', NMAX, NMAX, A, 1, X, Z, 1,
     $         W, 1, IW, 1, INFO )
             CALL CHKXER( 'DSBEVD', INFOT, NOUT, LERR, OK )
             INFO = J + 100*I
             CALL LA_TEST_DSBEVD('V', 'U', NMAX, NMAX, A, 1, X, Z, 1,
     $         W, 1 , IW, 1, INFO )
             CALL CHKXER( 'DSBEVD', INFOT, NOUT, LERR, OK )
             INFO = J + 100*I
             CALL LA_TEST_DSBEVD('V', 'L', NMAX, NMAX, A, 1, X, Z, 1,
     $         W, 1, IW, 1, INFO )
             CALL CHKXER( 'DSBEVD', INFOT, NOUT, LERR, OK )
           END DO
         END DO  

*
*        DSBEV
*
         SRNAMT = 'DSBEV '
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_DSBEV('N', 'U', NMAX, NMAX, A, 1, X, Z, 1,
     $        W, INFO )
             CALL CHKXER( 'DSBEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DSBEV('N', 'L', NMAX, NMAX, A, 1, X, Z, 1,
     $         W, INFO )
             CALL CHKXER( 'DSBEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DSBEV('V', 'U', NMAX, NMAX, A, 1, X, Z, 1,
     $         W, INFO )
             CALL CHKXER( 'DSBEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DSBEV('V', 'L', NMAX, NMAX, A, 1, X, Z, 1,
     $         W, INFO )
             CALL CHKXER( 'DSBEV ', INFOT, NOUT, LERR, OK )
           END DO
         END DO                  
*
*        DSBEVX
*
         SRNAMT = 'DSBEVX'
         DO I = 1, 11
           INFOT = I
           IF (I/=9) THEN
             DO J = 1, 11
               INFO = J + 100*I 
               CALL LA_TEST_DSBEVX( 'N', 'A', 'U', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I 
               CALL LA_TEST_DSBEVX( 'N', 'A', 'L', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I 
               CALL LA_TEST_DSBEVX( 'N', 'V', 'U', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I 
               CALL LA_TEST_DSBEVX( 'N', 'V', 'L', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I 
               CALL LA_TEST_DSBEVX( 'N', 'I', 'U', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I 
               CALL LA_TEST_DSBEVX( 'N', 'I', 'L', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I 
               CALL LA_TEST_DSBEVX( 'V', 'A', 'U', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I 
               CALL LA_TEST_DSBEVX( 'V', 'A', 'L', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I 
               CALL LA_TEST_DSBEVX( 'V', 'V', 'U', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I 
               CALL LA_TEST_DSBEVX( 'V', 'V', 'L', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I 
               CALL LA_TEST_DSBEVX( 'V', 'I', 'U', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I 
               CALL LA_TEST_DSBEVX( 'V', 'I', 'L', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'DSBEVX', INFOT, NOUT, LERR, OK )

             ENDDO
           ENDIF
         ENDDO
         NT = NT + 13
      END IF
*
*     Print a summary line.
*
      IF( OK ) THEN
         WRITE( NOUT, FMT = 9999 )PATH, NT
      ELSE
         WRITE( NOUT, FMT = 9998 )PATH
      END IF
*
 9999 FORMAT( 1X, A3, ' routines passed the tests of the error exits',
     $      ' (', I3, ' tests done)' )
 9998 FORMAT( ' *** ', A3, ' routines failed the tests of the error ',
     $      'exits ***' )
*
      RETURN
*
*     End of DERRST
*
      END
