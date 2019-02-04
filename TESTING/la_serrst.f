      SUBROUTINE SERRST( PATH, NUNIT )
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
*  SERRST tests the error exits for SSYTRD, SORGTR, SORMTR, SSPTRD,
*  SOPGTR, SOPMTR, SSTEQR, SSTERF, SSTEBZ, SSTEIN, SPTEQR, SSBTRD,
*  SSYEV, SSYEVX, SSYEVD, SSBEV, SSBEVX, SSBEVD,
*  SSPEV, SSPEVX, SSPEVD, SSTEV, SSTEVX, SSTEVD, and SSTEDC.
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
      INTEGER            I1( NMAX ), I2( NMAX ), I3( NMAX ), IW( LIW ),
     &                   IN(100)
      REAL               A( NMAX, NMAX ), C( NMAX, NMAX ), D( NMAX ),
     $                   E( NMAX ), Q( NMAX, NMAX ), R( NMAX ),
     $                   TAU( NMAX ), W( LW ), X( NMAX ),
     $                   Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHKXER, SOPGTR, SOPMTR, SORGTR, SORMTR, SPTEQR,
     $                   LA_TEST_SSBEV, LA_TEST_SSBEVD, LA_TEST_SSBEVX,
     &                   SSBTRD,  LA_TEST_SSPEV, LA_TEST_SSPEVD,
     $                   LA_TEST_SSPEVX, SSPTRD, SSTEBZ, SSTEDC, SSTEIN,
     &                   SSTEQR, SSTERF, LA_TEST_SSTEV, LA_TEST_SSTEVD,
     &                   LA_TEST_SSTEVR, LA_TEST_SSTEVX, LA_TEST_SSYEV,
     $                   LA_TEST_SSYEVD, LA_TEST_SSYEVR, LA_TEST_SSYEVX,
     &                   SSYTRD
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
      INTRINSIC          REAL
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
            A( I, J ) = 1. / REAL( I+J )
   10    CONTINUE
   20 CONTINUE
      DO 30 J = 1, NMAX
         D( J ) = REAL( J )
         E( J ) = 0.0
         I1( J ) = J
         I2( J ) = J
         TAU( J ) = 1.
   30 CONTINUE
      OK = .TRUE.
      NT = 0
*
*     Test error exits for the ST path.
*
      IF( LSAMEN( 2, C2, 'ST' ) ) THEN
*
*        SSYTRD
*
         SRNAMT = 'SSYTRD'
         INFOT = 1
         CALL SSYTRD( '/', 0, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'SSYTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SSYTRD( 'U', -1, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'SSYTRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SSYTRD( 'U', 2, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'SSYTRD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL SSYTRD( 'U', 0, A, 1, D, E, TAU, W, 0, INFO )
         CALL CHKXER( 'SSYTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        SORGTR
*
         SRNAMT = 'SORGTR'
         INFOT = 1
         CALL SORGTR( '/', 0, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'SORGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SORGTR( 'U', -1, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'SORGTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SORGTR( 'U', 2, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'SORGTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL SORGTR( 'U', 3, A, 3, TAU, W, 1, INFO )
         CALL CHKXER( 'SORGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        SORMTR
*
         SRNAMT = 'SORMTR'
         INFOT = 1
         CALL SORMTR( '/', 'U', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'SORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SORMTR( 'L', '/', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'SORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SORMTR( 'L', 'U', '/', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'SORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SORMTR( 'L', 'U', 'N', -1, 0, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'SORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL SORMTR( 'L', 'U', 'N', 0, -1, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'SORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL SORMTR( 'L', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INFO )
         CALL CHKXER( 'SORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL SORMTR( 'R', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'SORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL SORMTR( 'L', 'U', 'N', 2, 0, A, 2, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'SORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL SORMTR( 'L', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'SORMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL SORMTR( 'R', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INFO )
         CALL CHKXER( 'SORMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        SSPTRD
*
         SRNAMT = 'SSPTRD'
         INFOT = 1
         CALL SSPTRD( '/', 0, A, D, E, TAU, INFO )
         CALL CHKXER( 'SSPTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SSPTRD( 'U', -1, A, D, E, TAU, INFO )
         CALL CHKXER( 'SSPTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 2
*
*        SOPGTR
*
         SRNAMT = 'SOPGTR'
         INFOT = 1
         CALL SOPGTR( '/', 0, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'SOPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SOPGTR( 'U', -1, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'SOPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL SOPGTR( 'U', 2, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'SOPGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        SOPMTR
*
         SRNAMT = 'SOPMTR'
         INFOT = 1
         CALL SOPMTR( '/', 'U', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'SOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SOPMTR( 'L', '/', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'SOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SOPMTR( 'L', 'U', '/', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'SOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SOPMTR( 'L', 'U', 'N', -1, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'SOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL SOPMTR( 'L', 'U', 'N', 0, -1, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'SOPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL SOPMTR( 'L', 'U', 'N', 2, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'SOPMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        SPTEQR
*
         SRNAMT = 'SPTEQR'
         INFOT = 1
         CALL SPTEQR( '/', 0, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'SPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SPTEQR( 'N', -1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'SPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL SPTEQR( 'V', 2, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'SPTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        SSTEBZ
*
         SRNAMT = 'SSTEBZ'
         INFOT = 1
         CALL SSTEBZ( '/', 'E', 0, 0.0, 1.0, 1, 0, 0.0, D, E, M, NSPLIT,
     $                X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'SSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SSTEBZ( 'A', '/', 0, 0.0, 0.0, 0, 0, 0.0, D, E, M, NSPLIT,
     $                X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'SSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SSTEBZ( 'A', 'E', -1, 0.0, 0.0, 0, 0, 0.0, D, E, M,
     $                NSPLIT, X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'SSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL SSTEBZ( 'V', 'E', 0, 0.0, 0.0, 0, 0, 0.0, D, E, M, NSPLIT,
     $                X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'SSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL SSTEBZ( 'I', 'E', 0, 0.0, 0.0, 0, 0, 0.0, D, E, M, NSPLIT,
     $                X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'SSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL SSTEBZ( 'I', 'E', 1, 0.0, 0.0, 2, 1, 0.0, D, E, M, NSPLIT,
     $                X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'SSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL SSTEBZ( 'I', 'E', 1, 0.0, 0.0, 1, 0, 0.0, D, E, M, NSPLIT,
     $                X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'SSTEBZ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL SSTEBZ( 'I', 'E', 1, 0.0, 0.0, 1, 2, 0.0, D, E, M, NSPLIT,
     $                X, I1, I2, W, IW, INFO )
         CALL CHKXER( 'SSTEBZ', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*        SSTEIN
*
         SRNAMT = 'SSTEIN'
         INFOT = 1
         CALL SSTEIN( -1, D, E, 0, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL CHKXER( 'SSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SSTEIN( 0, D, E, -1, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL CHKXER( 'SSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SSTEIN( 0, D, E, 1, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL CHKXER( 'SSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL SSTEIN( 2, D, E, 0, X, I1, I2, Z, 1, W, IW, I3, INFO )
         CALL CHKXER( 'SSTEIN', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        SSTEQR
*
         SRNAMT = 'SSTEQR'
         INFOT = 1
         CALL SSTEQR( '/', 0, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'SSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SSTEQR( 'N', -1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'SSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL SSTEQR( 'V', 2, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'SSTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        SSTERF
*
         SRNAMT = 'SSTERF'
         INFOT = 1
         CALL SSTERF( -1, D, E, INFO )
         CALL CHKXER( 'SSTERF', INFOT, NOUT, LERR, OK )
         NT = NT + 1
*
*        SSTEDC
*
         SRNAMT = 'SSTEDC'
         INFOT = 1
         CALL SSTEDC( '/', 0, D, E, Z, 1, W, 1, IW, 1, INFO )
         CALL CHKXER( 'SSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SSTEDC( 'N', -1, D, E, Z, 1, W, 1, IW, 1, INFO )
         CALL CHKXER( 'SSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL SSTEDC( 'V', 2, D, E, Z, 1, W, 23, IW, 28, INFO )
         CALL CHKXER( 'SSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL SSTEDC( 'N', 1, D, E, Z, 1, W, 0, IW, 1, INFO )
         CALL CHKXER( 'SSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL SSTEDC( 'I', 2, D, E, Z, 2, W, 12, IW, 12, INFO )
         CALL CHKXER( 'SSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL SSTEDC( 'V', 2, D, E, Z, 2, W, 22, IW, 28, INFO )
         CALL CHKXER( 'SSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL SSTEDC( 'N', 1, D, E, Z, 1, W, 1, IW, 0, INFO )
         CALL CHKXER( 'SSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL SSTEDC( 'I', 2, D, E, Z, 2, W, 19, IW, 11, INFO )
         CALL CHKXER( 'SSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL SSTEDC( 'V', 2, D, E, Z, 2, W, 23, IW, 27, INFO )
         CALL CHKXER( 'SSTEDC', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        SSTEVD
*
         SRNAMT = 'SSTEVD'
         DO I = 2,3
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_SSTEVD('N', NMAX, D, E, Z, NMAX, W, NMAX, IW,
     &         NMAX, INFO)
             CALL CHKXER( 'SSTEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSTEVD('V', NMAX, D, E, Z, NMAX, W, NMAX, IW,
     &         NMAX, INFO)
             CALL CHKXER( 'SSTEVD ', INFOT, NOUT, LERR, OK )
           END DO
         END DO     
         NT = NT + 7
*
*        SSTEV
*
         SRNAMT = 'SSTEV '
         DO I = 2,3
           IN(I) = 1
         END DO
         DO I = 2,3
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_SSTEV('N', NMAX, D, E, Z, NMAX, W, INFO)
             CALL CHKXER( 'SSTEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSTEV('V', NMAX, D, E, Z, NMAX, W, INFO)
             CALL CHKXER( 'SSTEV ', INFOT, NOUT, LERR, OK )
           END DO
         END DO
         NT = NT + 3
*
*        SSTEVX
*
         SRNAMT = 'SSTEVX'
         DO I = 2, 10
           INFOT = I
           IF (I/=9) THEN
             DO J = 1, 1
               INFO = J + 100*I
               CALL LA_TEST_SSTEVX( 'V', 'A', NMAX, D, E, 0.0D0, 0.0D0,
     &           0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSTEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSTEVX( 'N', 'A', NMAX, D, E, 0.0D0, 0.0D0,
     &           0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSTEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSTEVX( 'V', 'I', NMAX, D, E, 0.0D0, 0.0D0,
     &           0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSTEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSTEVX( 'N', 'I', NMAX, D, E, 0.0D0, 0.0D0,
     &           0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSTEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSTEVX( 'V', 'V', NMAX, D, E, 0.0D0, 0.0D0,
     &           0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSTEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSTEVX( 'N', 'V', NMAX, D, E, 0.0D0, 0.0D0,
     &           0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSTEVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDIF
         ENDDO                          
         NT = NT + 9
*
*        SSTEVR
*
         N = 1
         SRNAMT = 'SSTEVR'
         DO I = 2,8
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_SSTEVR( 'N', 'A', NMAX, D, E, 0.0D0,
     &         0.0D0, NMAX-1, NMAX, 0.0D0, M,
     &         R, Z, NMAX, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
             CALL CHKXER( 'SSTEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSTEVR( 'N', 'V', NMAX, D, E, 0.0D0,
     &         0.0D0, NMAX, NMAX, 0.0D0, M,
     &         R, Z, NMAX, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
             CALL CHKXER( 'SSTEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSTEVR( 'N', 'I', NMAX, D, E, 0.0D0,
     &         0.0D0, NMAX, NMAX, 0.0D0, M,
     &         R, Z, NMAX, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
             CALL CHKXER( 'SSTEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSTEVR( 'V', 'A', NMAX, D, E, 0.0D0,
     &         0.0D0, NMAX, NMAX, 0.0D0, M,
     &         R, Z, NMAX, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
             CALL CHKXER( 'SSTEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSTEVR( 'V', 'V', NMAX, D, E, 0.0D0,
     &         0.0D0, NMAX, NMAX, 0.0D0, M,
     &         R, Z, NMAX, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
             CALL CHKXER( 'SSTEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSTEVR( 'V', 'I', NMAX, D, E, 0.0D0,
     &         0.0D0, NMAX, NMAX, 0.0D0, M,
     &         R, Z, NMAX, IW, X, 20*N, IW( 2*N+1 ), 10*N, INFO )
             CALL CHKXER( 'SSTEVR', INFOT, NOUT, LERR, OK )
           ENDDO
         ENDDO                                         
         NT = NT + 9
*
*        SSYEVD
*
         SRNAMT = 'SSYEVD'
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_SSYEVD('N', 'U', NMAX, A, NMAX, X, W, 1,
     &         IW, 1, INFO )
             CALL CHKXER( 'SSYEVD', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSYEVD('N', 'L', NMAX, A, NMAX, X, W, 1,
     &         IW, 1, INFO )
             CALL CHKXER( 'SSYEVD', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSYEVD('V', 'U', NMAX, A, NMAX, X, W, 1,
     &         IW, 1, INFO )
             CALL CHKXER( 'SSYEVD', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSYEVD('V', 'L', NMAX, A, NMAX, X, W, 1,
     &         IW, 1, INFO )
             CALL CHKXER( 'SSYEVD', INFOT, NOUT, LERR, OK )
           END DO
         END DO 

*
*        SSYEVR
*
         SRNAMT = 'SSYEVR'
         DO I = 1,10
           IF(I/=9)THEN
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_SSYEVR( 'N', 'A', 'U', NMAX, A, NMAX, 0.0D0,
     &         0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     &         IW( 2*N+1 ), 10*N, INFO )
             CALL CHKXER( 'SSYEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSYEVR( 'N', 'A', 'L', NMAX, A, NMAX, 0.0D0,
     &         0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     &         IW( 2*N+1 ), 10*N, INFO )
             CALL CHKXER( 'SSYEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSYEVR( 'N', 'V', 'U', NMAX, A, NMAX, 0.0D0,
     &         0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     &         IW( 2*N+1 ), 10*N, INFO )
             CALL CHKXER( 'SSYEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSYEVR( 'N', 'V', 'L', NMAX, A, NMAX, 0.0D0,
     &         0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     &         IW( 2*N+1 ), 10*N, INFO )
             CALL CHKXER( 'SSYEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSYEVR( 'N', 'I', 'U', NMAX, A, NMAX, 0.0D0,
     &         0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     &         IW( 2*N+1 ), 10*N, INFO )
             CALL CHKXER( 'SSYEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSYEVR( 'N', 'I', 'L', NMAX, A, NMAX, 0.0D0,
     &         0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     &         IW( 2*N+1 ), 10*N, INFO )
             CALL CHKXER( 'SSYEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSYEVR( 'V', 'A', 'U', NMAX, A, NMAX, 0.0D0,
     &         0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     &         IW( 2*N+1 ), 10*N, INFO )
             CALL CHKXER( 'SSYEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSYEVR( 'V', 'A', 'L', NMAX, A, NMAX, 0.0D0,
     &         0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     &         IW( 2*N+1 ), 10*N, INFO )
             CALL CHKXER( 'SSYEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSYEVR( 'V', 'V', 'U', NMAX, A, NMAX, 0.0D0,
     &         0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     &         IW( 2*N+1 ), 10*N, INFO )
             CALL CHKXER( 'SSYEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSYEVR( 'V', 'V', 'L', NMAX, A, NMAX, 0.0D0,
     &         0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     &         IW( 2*N+1 ), 10*N, INFO )
             CALL CHKXER( 'SSYEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSYEVR( 'V', 'I', 'U', NMAX, A, NMAX, 0.0D0,
     &         0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     &         IW( 2*N+1 ), 10*N, INFO )
             CALL CHKXER( 'SSYEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSYEVR( 'V', 'I', 'L', NMAX, A, NMAX, 0.0D0,
     &         0.0D0, NMAX, NMAX, 0.0D0, M, R, Z, NMAX, IW, Q, 26*N,
     &         IW( 2*N+1 ), 10*N, INFO )
             CALL CHKXER( 'SSYEVR', INFOT, NOUT, LERR, OK )
           ENDDO
         ENDIF
       ENDDO
         
         NT = NT + 11
*
*        SSYEV
*
         SRNAMT = 'SSYEV '
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_SSYEV('N', 'U', NMAX, A, NMAX, X, W, 1,
     &         INFO )
             CALL CHKXER( 'SSYEV ', INFOT, NOUT, LERR, OK )
             
             INFO = J + 100*I
             CALL LA_TEST_SSYEV('N', 'L', NMAX, A, NMAX, X, W, 1,
     &         INFO )
             CALL CHKXER( 'SSYEV ', INFOT, NOUT, LERR, OK )
             INFO = J + 100*I
             CALL LA_TEST_SSYEV('V', 'U', NMAX, A, NMAX, X, W, 1,
     &         INFO )
             CALL CHKXER( 'SSYEV ', INFOT, NOUT, LERR, OK )
             INFO = J + 100*I
             CALL LA_TEST_SSYEV('V', 'L', NMAX, A, NMAX, X, W, 1,
     &         INFO )
             CALL CHKXER( 'SSYEV ', INFOT, NOUT, LERR, OK )
           END DO
         END DO        
         NT = NT + 5
*
*        SSYEVX
*
         SRNAMT = 'SSYEVX'
         DO I = 1, 10
           IF (I/=9) THEN
             INFOT = I
             DO J = 1, 1
               INFO = J + 100*I
               CALL LA_TEST_SSYEVX( 'N', 'A', 'U', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO )
               CALL CHKXER( 'SSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSYEVX( 'N', 'A', 'L', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO )
               CALL CHKXER( 'SSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSYEVX( 'N', 'V', 'U', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO )
               CALL CHKXER( 'SSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSYEVX( 'N', 'V', 'L', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO )
               CALL CHKXER( 'SSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSYEVX( 'N', 'I', 'U', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO )
               CALL CHKXER( 'SSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSYEVX( 'N', 'I', 'L', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO )
               CALL CHKXER( 'SSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSYEVX( 'V', 'A', 'U', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO )
               CALL CHKXER( 'SSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 1               00*I
               CALL LA_TEST_SSYEVX( 'V', 'A', 'L', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO )
               CALL CHKXER( 'SSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSYEVX( 'V', 'V', 'U', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO )
               CALL CHKXER( 'SSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSYEVX( 'V', 'V', 'L', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO )
               CALL CHKXER( 'SSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSYEVX( 'V', 'I', 'U', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO )
               CALL CHKXER( 'SSYEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSYEVX( 'V', 'I', 'L', NMAX, A, NMAX, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, IW, I3, INFO )
               CALL CHKXER( 'SSYEVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDIF
         ENDDO
         NT = NT + 12
*
*        SSPEVD
*
         SRNAMT = 'SSPEVD'
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_SSPEVD('N', 'U', NMAX, A, X, Z, 1,
     &         W, 1, IW, 1, INFO )
             CALL CHKXER( 'SSPEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSPEVD('N', 'L', NMAX, A, X, Z, 1,
     &         W, 1 , IW, 1, INFO )
             CALL CHKXER( 'SSPEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSPEVD('V', 'U', NMAX, A, X, Z, 1,
     &         W, 1 , IW, 1, INFO )
             CALL CHKXER( 'SSPEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSPEVD('V', 'L', NMAX, A, X, Z, 1,
     &         W, 1 , IW, 1, INFO )
             CALL CHKXER( 'SSPEVD ', INFOT, NOUT, LERR, OK )
           END DO
         END DO      

*
*        SSPEV
*
         SRNAMT = 'SSPEV '
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_SSPEV('N', 'U', NMAX, A, W, Z, 1,
     &         X, INFO )
             CALL CHKXER( 'SSPEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSPEV('N', 'L', NMAX, A, W, Z, 1,
     &         X, INFO )
             CALL CHKXER( 'SSPEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSPEV('V', 'U', NMAX, A, W, Z, 1,
     &         X, INFO )
             CALL CHKXER( 'SSPEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSPEV('V', 'L', NMAX, A, W, Z, 1,
     &         X, INFO )
             CALL CHKXER( 'SSPEV ', INFOT, NOUT, LERR, OK )
           END DO
         END DO          
         NT = NT + 4
*
*        SSPEVX
*
         SRNAMT = 'SSPEVX'
         DO I = 1, 10
           INFOT = I
           IF (I/=9) THEN
             DO J = 1, 1
               INFO = J + 100*I
               CALL LA_TEST_SSPEVX( 'V', 'A', 'U', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSPEVX( 'V', 'A', 'L', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSPEVX( 'V', 'V', 'U', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSPEVX( 'V', 'V', 'L', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSPEVX( 'V', 'I', 'U', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSPEVX( 'V', 'I', 'L', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSPEVX( 'N', 'A', 'U', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSPEVX( 'N', 'A', 'L', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSPEVX( 'N', 'V', 'U', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSPEVX( 'N', 'V', 'L', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSPEVX( 'N', 'I', 'U', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSPEVX( 'N', 'I', 'L', NMAX, A, 0.0D0,
     &           0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSPEVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDIF
         ENDDO
         
         NT = NT + 10
*
*     Test error exits for the SB path.
*
      ELSE IF( LSAMEN( 2, C2, 'SB' ) ) THEN
*
*        SSBTRD
*
         SRNAMT = 'SSBTRD'
         INFOT = 1
         CALL SSBTRD( '/', 'U', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'SSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SSBTRD( 'N', '/', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'SSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SSBTRD( 'N', 'U', -1, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'SSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SSBTRD( 'N', 'U', 0, -1, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'SSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL SSBTRD( 'N', 'U', 1, 1, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'SSBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL SSBTRD( 'V', 'U', 2, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'SSBTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        SSBEVD
*
         SRNAMT = 'SSBEVD'
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_SSBEVD('N', 'U', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, 1, IW, 1, INFO )
             CALL CHKXER( 'SSBEVD', INFOT, NOUT, LERR, OK )
             INFO = J + 100*I
             CALL LA_TEST_SSBEVD('N', 'L', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, 1, IW, 1, INFO )
             CALL CHKXER( 'SSBEVD', INFOT, NOUT, LERR, OK )
             INFO = J + 100*I
             CALL LA_TEST_SSBEVD('V', 'U', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, 1 , IW, 1, INFO )
             CALL CHKXER( 'SSBEVD', INFOT, NOUT, LERR, OK )
             INFO = J + 100*I
             CALL LA_TEST_SSBEVD('V', 'L', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, 1, IW, 1, INFO )
             CALL CHKXER( 'SSBEVD', INFOT, NOUT, LERR, OK )
           END DO
         END DO   

*
*        SSBEV
*
         SRNAMT = 'SSBEV '
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_SSBEV('N', 'U', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, INFO )
             CALL CHKXER( 'SSBEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSBEV('N', 'L', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, INFO )
             CALL CHKXER( 'SSBEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSBEV('V', 'U', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, INFO )
             CALL CHKXER( 'SSBEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SSBEV('V', 'L', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, INFO )
             CALL CHKXER( 'SSBEV ', INFOT, NOUT, LERR, OK )
           END DO
         END DO 
         NT = NT + 6
*
*        SSBEVX
*
         SRNAMT = 'SSBEVX'
         DO I = 1, 11
           INFOT = I
           IF (I/=9) THEN
             DO J = 1, 11
               INFO = J + 100*I
               CALL LA_TEST_SSBEVX( 'N', 'A', 'U', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSBEVX( 'N', 'A', 'L', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSBEVX( 'N', 'V', 'U', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSBEVX( 'N', 'V', 'L', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSBEVX( 'N', 'I', 'U', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSBEVX( 'N', 'I', 'L', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSBEVX( 'V', 'A', 'U', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSBEVX( 'V', 'A', 'L', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSBEVX( 'V', 'V', 'U', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSBEVX( 'V', 'V', 'L', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSBEVX( 'V', 'I', 'U', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSBEVX( 'V', 'I', 'L', NMAX, NMAX, A, NMAX,
     &           Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, IW, I3, INFO )
               CALL CHKXER( 'SSBEVX', INFOT, NOUT, LERR, OK )
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
*     End of SERRST
*
      END
