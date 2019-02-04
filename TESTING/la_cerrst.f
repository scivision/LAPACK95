      SUBROUTINE CERRST( PATH, NUNIT )
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
*  CERRST tests the error exits for CHETRD, CUNGTR, CUNMTR, CHPTRD,
*  CUNGTR, CUPMTR, CSTEQR, CSTEIN, CPTEQR, CHBTRD,
*  CHEEV, CHEEVX, CHEEVD, CHBEV, CHBEVX, CHBEVD,
*  CHPEV, CHPEVX, CHPEVD, and CSTEDC.
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
*     .. Parameters ..
      INTEGER            NMAX, LIW, LW
      PARAMETER          ( NMAX = 3, LIW = 12*NMAX, LW = 20*NMAX )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, INFO, J, M, NT
*     ..
*     .. Local Arrays ..
      INTEGER            I1( NMAX ), I2( NMAX ), I3( NMAX ), IW( LIW ),
     &                   IN(100)
      REAL               D( NMAX ), E( NMAX ), R( LW ), RW( LW ),
     $                   X( NMAX )
      COMPLEX            A( NMAX, NMAX ), C( NMAX, NMAX ),
     $                   Q( NMAX, NMAX ), TAU( NMAX ), W( LW ),
     $                   Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           LA_TEST_CHBEV, LA_TEST_CHBEVD, LA_TEST_CHBEVX,
     &  CHBTRD,
     $                   LA_TEST_CHEEV, LA_TEST_CHEEVD, CHEEVR, CHEEVX,
     $                   CHETRD, CHKXER, LA_TEST_CHPEV, LA_TEST_CHPEVD,
     $                   CHPEVX, CHPTRD, CPTEQR, CSTEDC, CSTEIN, CSTEQR,
     $                   CUNGTR, CUNMTR, CUPGTR, CUPMTR
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
*        CHETRD
*
         SRNAMT = 'CHETRD'
         INFOT = 1
         CALL CHETRD( '/', 0, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'CHETRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHETRD( 'U', -1, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'CHETRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CHETRD( 'U', 2, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'CHETRD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CHETRD( 'U', 0, A, 1, D, E, TAU, W, 0, INFO )
         CALL CHKXER( 'CHETRD', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        CUNGTR
*
         SRNAMT = 'CUNGTR'
         INFOT = 1
         CALL CUNGTR( '/', 0, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'CUNGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CUNGTR( 'U', -1, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'CUNGTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CUNGTR( 'U', 2, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'CUNGTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CUNGTR( 'U', 3, A, 3, TAU, W, 1, INFO )
         CALL CHKXER( 'CUNGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        CUNMTR
*
         SRNAMT = 'CUNMTR'
         INFOT = 1
         CALL CUNMTR( '/', 'U', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CUNMTR( 'L', '/', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CUNMTR( 'L', 'U', '/', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CUNMTR( 'L', 'U', 'N', -1, 0, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CUNMTR( 'L', 'U', 'N', 0, -1, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CUNMTR( 'L', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CUNMTR( 'R', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CUNMTR( 'L', 'U', 'N', 2, 0, A, 2, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CUNMTR( 'L', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CUNMTR( 'R', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        CHPTRD
*
         SRNAMT = 'CHPTRD'
         INFOT = 1
         CALL CHPTRD( '/', 0, A, D, E, TAU, INFO )
         CALL CHKXER( 'CHPTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHPTRD( 'U', -1, A, D, E, TAU, INFO )
         CALL CHKXER( 'CHPTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 2
*
*        CUPGTR
*
         SRNAMT = 'CUPGTR'
         INFOT = 1
         CALL CUPGTR( '/', 0, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'CUPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CUPGTR( 'U', -1, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'CUPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CUPGTR( 'U', 2, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'CUPGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        CUPMTR
*
         SRNAMT = 'CUPMTR'
         INFOT = 1
         CALL CUPMTR( '/', 'U', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'CUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CUPMTR( 'L', '/', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'CUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CUPMTR( 'L', 'U', '/', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'CUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CUPMTR( 'L', 'U', 'N', -1, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'CUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CUPMTR( 'L', 'U', 'N', 0, -1, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'CUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CUPMTR( 'L', 'U', 'N', 2, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'CUPMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        CPTEQR
*
         SRNAMT = 'CPTEQR'
         INFOT = 1
         CALL CPTEQR( '/', 0, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'CPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CPTEQR( 'N', -1, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'CPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CPTEQR( 'V', 2, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'CPTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        CSTEIN
*
         SRNAMT = 'CSTEIN'
         INFOT = 1
         CALL CSTEIN( -1, D, E, 0, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'CSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CSTEIN( 0, D, E, -1, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'CSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CSTEIN( 0, D, E, 1, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'CSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CSTEIN( 2, D, E, 0, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'CSTEIN', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        CSTEQR
*
         SRNAMT = 'CSTEQR'
         INFOT = 1
         CALL CSTEQR( '/', 0, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'CSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CSTEQR( 'N', -1, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'CSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CSTEQR( 'V', 2, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'CSTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        CSTEDC
*
         SRNAMT = 'CSTEDC'
         INFOT = 1
         CALL CSTEDC( '/', 0, D, E, Z, 1, W, 1, RW, 1, IW, 1, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CSTEDC( 'N', -1, D, E, Z, 1, W, 1, RW, 1, IW, 1, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CSTEDC( 'V', 2, D, E, Z, 1, W, 4, RW, 23, IW, 28, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CSTEDC( 'N', 2, D, E, Z, 1, W, 0, RW, 1, IW, 1, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CSTEDC( 'V', 2, D, E, Z, 2, W, 3, RW, 23, IW, 28, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CSTEDC( 'N', 2, D, E, Z, 1, W, 1, RW, 0, IW, 1, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CSTEDC( 'I', 2, D, E, Z, 2, W, 1, RW, 16, IW, 12, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CSTEDC( 'V', 2, D, E, Z, 2, W, 4, RW, 22, IW, 28, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CSTEDC( 'N', 2, D, E, Z, 1, W, 1, RW, 1, IW, 0, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CSTEDC( 'I', 2, D, E, Z, 2, W, 1, RW, 23, IW, 11, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CSTEDC( 'V', 2, D, E, Z, 2, W, 4, RW, 23, IW, 27, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        CHEEVD
*
         SRNAMT = 'CHEEVD'
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_CHEEVD('N', 'U', NMAX, A, NMAX, X, W, 1,
     &         RW, 1 , IW, 1, INFO )
             CALL CHKXER( 'CHEEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHEEVD('N', 'L', NMAX, A, NMAX, X, W, 1,
     &         RW, 1 , IW, 1, INFO )
             CALL CHKXER( 'CHEEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHEEVD('V', 'U', NMAX, A, NMAX, X, W, 1,
     &         RW,  1, IW, 1, INFO )
             CALL CHKXER( 'CHEEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHEEVD('V', 'L', NMAX, A, NMAX, X, W, 1,
     &         RW, 1, IW, 1, INFO )
             CALL CHKXER( 'CHEEVD ', INFOT, NOUT, LERR, OK )
           END DO
         END DO 
*
*        CHEEV
*
         SRNAMT = 'CHEEV '
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_CHEEV('N', 'U', NMAX, A, NMAX, X, W, 1,
     &         RW, INFO )
             CALL CHKXER( 'CHEEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHEEV('N', 'L', NMAX, A, NMAX, X, W, 1,
     &         RW, INFO )
             CALL CHKXER( 'CHEEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHEEV('V', 'U', NMAX, A, NMAX, X, W, 1,
     &         RW, INFO )
             CALL CHKXER( 'CHEEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHEEV('V', 'L', NMAX, A, NMAX, X, W, 1,
     &         RW, INFO )
             CALL CHKXER( 'CHEEV ', INFOT, NOUT, LERR, OK )
           END DO
         END DO  

*
*        CHEEVX
*
         SRNAMT = 'CHEEVX'
         DO I = 1, 10
           IF (I/=9) THEN
             INFOT = I
             DO J = 1, 1
               INFO = J + 100*I
               CALL LA_TEST_CHEEVX( 'N', 'A', 'U', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHEEVX( 'N', 'A', 'L', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHEEVX( 'N', 'V', 'U', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHEEVX( 'N', 'V', 'L', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHEEVX( 'N', 'I', 'U', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHEEVX( 'N', 'I', 'L', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHEEVX( 'V', 'A', 'U', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHEEVX( 'V', 'A', 'L', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHEEVX( 'V', 'V', 'U', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHEEVX( 'V', 'V', 'L', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHEEVX( 'V', 'I', 'U', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHEEVX( 'V', 'I', 'L', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )
               
             ENDDO
           ENDIF
         ENDDO
         NT = NT + 10
*
*        CHEEVR
*
         SRNAMT = 'CHEEVR'
         DO I = 1,10
           IF(I/=9) THEN
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_CHEEVR( 'N', 'A', 'U', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHEEVR( 'N', 'A', 'L', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHEEVR( 'N', 'V', 'U', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHEEVR( 'N', 'V', 'L', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHEEVR( 'N', 'I', 'U', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHEEVR( 'N', 'I', 'L', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHEEVR( 'V', 'A', 'U', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHEEVR( 'V', 'A', 'L', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHEEVR( 'V', 'V', 'U', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHEEVR( 'V', 'V', 'L', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHEEVR( 'V', 'I', 'U', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )

             INFO = J + 1             00*I
             CALL LA_TEST_CHEEVR( 'V', 'I', 'L', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )
           ENDDO
         ENDIF
       ENDDO                                          
         NT = NT + 12
*
*        CHPEVD
*
         SRNAMT = 'CHPEVD'
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_CHPEVD('N', 'U', NMAX, A, X, Z, 1,
     &         W, 1, RW, 1, IW, 1, INFO )
             CALL CHKXER( 'CHPEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHPEVD('N', 'L', NMAX, A, X, Z, 1,
     &         W, 1, RW, 1, IW, 1, INFO )
             CALL CHKXER( 'CHPEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHPEVD('V', 'U', NMAX, A, X, Z, 1,
     &         W, 1, RW, 1, IW, 1, INFO )
             CALL CHKXER( 'CHPEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHPEVD('V', 'L', NMAX, A, X, Z, 1,
     &         W, 1, RW, 1, IW, 1, INFO )
             CALL CHKXER( 'CHPEVD ', INFOT, NOUT, LERR, OK )
           END DO
         END DO 
*
*        CHPEV
*
         SRNAMT = 'CHPEV '
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_CHPEV('N', 'U', NMAX, A, X, Z, 1,
     &         W, RW,  INFO )
             CALL CHKXER( 'CHPEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHPEV('N', 'L', NMAX, A, X, Z, 1,
     &          W, RW, INFO )
             CALL CHKXER( 'CHPEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHPEV('V', 'U', NMAX, A, X, Z, 1,
     &          W, RW, INFO )
             CALL CHKXER( 'CHPEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHPEV('V', 'L', NMAX, A, X, Z, 1,
     &          W, RW, INFO )
             CALL CHKXER( 'CHPEV ', INFOT, NOUT, LERR, OK )
           END DO
         END DO 

*
*        CHPEVX
*
         SRNAMT = 'CHPEVX'
         DO I = 1, 10
           INFOT = I
           IF (I/=9) THEN
             DO J = 1, 1
               INFO = J + 100*I
               CALL LA_TEST_CHPEVX( 'V', 'A', 'U', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHPEVX( 'V', 'A', 'L', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHPEVX( 'V', 'V', 'U', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHPEVX( 'V', 'V', 'L', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHPEVX( 'V', 'I', 'U', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHPEVX( 'V', 'I', 'L', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHPEVX( 'N', 'A', 'U', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_CHPEVX( 'N', 'A', 'L', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHPEVX( 'N', 'V', 'U', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHPEVX( 'N', 'V', 'L', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHPEVX( 'N', 'I', 'U', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHPEVX( 'N', 'I', 'L', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDIF
         ENDDO
         
         NT = NT + 8
*
*     Test error exits for the HB path.
*
      ELSE IF( LSAMEN( 2, C2, 'HB' ) ) THEN
*
*        CHBTRD
*
         SRNAMT = 'CHBTRD'
         INFOT = 1
         CALL CHBTRD( '/', 'U', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'CHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHBTRD( 'N', '/', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'CHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CHBTRD( 'N', 'U', -1, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'CHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CHBTRD( 'N', 'U', 0, -1, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'CHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CHBTRD( 'N', 'U', 1, 1, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'CHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CHBTRD( 'V', 'U', 2, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'CHBTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        CHBEVD
*
         SRNAMT = 'CHBEVD'
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_CHBEVD('N', 'U', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, 1, RW, 1 , IW, 1, INFO )
             CALL CHKXER( 'CHBEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHBEVD('N', 'L', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, 1, RW, 1 , IW, 1, INFO )                      
             CALL CHKXER( 'CHBEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHBEVD('N', 'L', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, 1, RW, 1 , IW, 1, INFO )
             CALL CHKXER( 'CHBEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHBEVD('N', 'L', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, 1, RW, 1 , IW, 1, INFO )
             CALL CHKXER( 'CHBEVD ', INFOT, NOUT, LERR, OK )
           END DO
         END DO
*
*        CHBEV
*
         SRNAMT = 'CHBEV '
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_CHBEV('N', 'U', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, RW, INFO )
             CALL CHKXER( 'CHBEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHBEV('N', 'L', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, RW, INFO )
             CALL CHKXER( 'CHBEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHBEV('V', 'U', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, RW, INFO )
             CALL CHKXER( 'CHBEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_CHBEV('V', 'L', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, RW, INFO )
             CALL CHKXER( 'CHBEV ', INFOT, NOUT, LERR, OK )
           END DO
         END DO 

*
*        CHBEVX
*
         SRNAMT = 'CHBEVX'
         DO I = 1, 11
           INFOT = I
           IF (I/=9) THEN
             DO J = 1, 11
               INFO = J + 100*I
               CALL LA_TEST_CHBEVX( 'N', 'A', 'U', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHBEVX( 'N', 'A', 'L', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHBEVX( 'N', 'V', 'U', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHBEVX( 'N', 'V', 'L', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHBEVX( 'N', 'I', 'U', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHBEVX( 'N', 'I', 'L', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHBEVX( 'V', 'A', 'U', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )


               INFO = J + 100*I
               CALL LA_TEST_CHBEVX( 'V', 'A', 'L', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHBEVX( 'V', 'V', 'U', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHBEVX( 'V', 'V', 'L', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHBEVX( 'V', 'I', 'U', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHBEVX( 'V', 'I', 'L', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDIF
         ENDDO                                       
         
         NT = NT + 11
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
*     End of CERRST
*
      END
