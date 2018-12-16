      SUBROUTINE ZERRST( PATH, NUNIT )
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
*  ZERRST tests the error exits for ZHETRD, ZUNGTR, CUNMTR, ZHPTRD,
*  ZUNGTR, ZUPMTR, ZSTEQR, CSTEIN, ZPTEQR, ZHBTRD,
*  ZHEEV, CHEEVX, CHEEVD, ZHBEV, CHBEVX, CHBEVD,
*  ZHPEV, CHPEVX, CHPEVD, and ZSTEDC.
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
      INTEGER            I, INFO, J, M, N, NT
*     ..
*     .. Local Arrays ..
      INTEGER            I1( NMAX ), I2( NMAX ), I3( NMAX ), IW( LIW ),
     &                   IN(100)
      DOUBLE PRECISION   D( NMAX ), E( NMAX ), R( LW ), RW( LW ),
     $                   X( NMAX )
      COMPLEX*16         A( NMAX, NMAX ), C( NMAX, NMAX ),
     $                   Q( NMAX, NMAX ), TAU( NMAX ), W( LW ),
     $                   Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHKXER, LA_TEST_ZHBEV, LA_TEST_ZHBEVD,
     &                   LA_TEST_ZHBEVX,
     &                   ZHBTRD, LA_TEST_ZHEEV,
     $                   LA_TEST_ZHEEVD, LA_TEST_ZHEEVR, LA_TEST_ZHEEVX,
     &  ZHETRD,
     &                   LA_TEST_ZHPEV,  LA_TEST_ZHPEVD,
     $                   ZHPEVX, ZHPTRD, ZPTEQR, ZSTEDC, ZSTEIN, ZSTEQR,
     $                   ZUNGTR, ZUNMTR, ZUPGTR, ZUPMTR
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
      IF( LSAMEN( 2, C2, 'ST' ) ) THEN
*
*        ZHETRD
*
         SRNAMT = 'ZHETRD'
         INFOT = 1
         CALL ZHETRD( '/', 0, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'ZHETRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHETRD( 'U', -1, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'ZHETRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZHETRD( 'U', 2, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'ZHETRD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZHETRD( 'U', 0, A, 1, D, E, TAU, W, 0, INFO )
         CALL CHKXER( 'ZHETRD', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        ZUNGTR
*
         SRNAMT = 'ZUNGTR'
         INFOT = 1
         CALL ZUNGTR( '/', 0, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'ZUNGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZUNGTR( 'U', -1, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'ZUNGTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZUNGTR( 'U', 2, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'ZUNGTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZUNGTR( 'U', 3, A, 3, TAU, W, 1, INFO )
         CALL CHKXER( 'ZUNGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        ZUNMTR
*
         SRNAMT = 'ZUNMTR'
         INFOT = 1
         CALL ZUNMTR( '/', 'U', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZUNMTR( 'L', '/', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZUNMTR( 'L', 'U', '/', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZUNMTR( 'L', 'U', 'N', -1, 0, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZUNMTR( 'L', 'U', 'N', 0, -1, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZUNMTR( 'L', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZUNMTR( 'R', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZUNMTR( 'L', 'U', 'N', 2, 0, A, 2, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL ZUNMTR( 'L', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL ZUNMTR( 'R', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INFO )
         CALL CHKXER( 'ZUNMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        ZHPTRD
*
         SRNAMT = 'ZHPTRD'
         INFOT = 1
         CALL ZHPTRD( '/', 0, A, D, E, TAU, INFO )
         CALL CHKXER( 'ZHPTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHPTRD( 'U', -1, A, D, E, TAU, INFO )
         CALL CHKXER( 'ZHPTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 2
*
*        ZUPGTR
*
         SRNAMT = 'ZUPGTR'
         INFOT = 1
         CALL ZUPGTR( '/', 0, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'ZUPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZUPGTR( 'U', -1, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'ZUPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZUPGTR( 'U', 2, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'ZUPGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        ZUPMTR
*
         SRNAMT = 'ZUPMTR'
         INFOT = 1
         CALL ZUPMTR( '/', 'U', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'ZUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZUPMTR( 'L', '/', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'ZUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZUPMTR( 'L', 'U', '/', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'ZUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZUPMTR( 'L', 'U', 'N', -1, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'ZUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZUPMTR( 'L', 'U', 'N', 0, -1, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'ZUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZUPMTR( 'L', 'U', 'N', 2, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'ZUPMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        ZPTEQR
*
         SRNAMT = 'ZPTEQR'
         INFOT = 1
         CALL ZPTEQR( '/', 0, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'ZPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZPTEQR( 'N', -1, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'ZPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZPTEQR( 'V', 2, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'ZPTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        ZSTEIN
*
         SRNAMT = 'ZSTEIN'
         INFOT = 1
         CALL ZSTEIN( -1, D, E, 0, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'ZSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZSTEIN( 0, D, E, -1, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'ZSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZSTEIN( 0, D, E, 1, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'ZSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZSTEIN( 2, D, E, 0, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'ZSTEIN', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        ZSTEQR
*
         SRNAMT = 'ZSTEQR'
         INFOT = 1
         CALL ZSTEQR( '/', 0, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'ZSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZSTEQR( 'N', -1, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'ZSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZSTEQR( 'V', 2, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'ZSTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        ZSTEDC
*
         SRNAMT = 'ZSTEDC'
         INFOT = 1
         CALL ZSTEDC( '/', 0, D, E, Z, 1, W, 1, RW, 1, IW, 1, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZSTEDC( 'N', -1, D, E, Z, 1, W, 1, RW, 1, IW, 1, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZSTEDC( 'V', 2, D, E, Z, 1, W, 4, RW, 23, IW, 28, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZSTEDC( 'N', 2, D, E, Z, 1, W, 0, RW, 1, IW, 1, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZSTEDC( 'V', 2, D, E, Z, 2, W, 3, RW, 23, IW, 28, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZSTEDC( 'N', 2, D, E, Z, 1, W, 1, RW, 0, IW, 1, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZSTEDC( 'I', 2, D, E, Z, 2, W, 1, RW, 16, IW, 12, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZSTEDC( 'V', 2, D, E, Z, 2, W, 4, RW, 22, IW, 28, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL ZSTEDC( 'N', 2, D, E, Z, 1, W, 1, RW, 1, IW, 0, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL ZSTEDC( 'I', 2, D, E, Z, 2, W, 1, RW, 23, IW, 11, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL ZSTEDC( 'V', 2, D, E, Z, 2, W, 4, RW, 23, IW, 27, INFO )
         CALL CHKXER( 'ZSTEDC', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        ZHEEVD
*
         SRNAMT = 'ZHEEVD'
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_ZHEEVD('N', 'U', NMAX, A, NMAX, X, W, 1,
     &         RW, 1 , IW, 1, INFO )
             CALL CHKXER( 'ZHEEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZHEEVD('N', 'L', NMAX, A, NMAX, X, W, 1,
     &         RW, 1 , IW, 1, INFO )
             CALL CHKXER( 'ZHEEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZHEEVD('V', 'U', NMAX, A, NMAX, X, W, 1,
     &         RW,  1, IW, 1, INFO )
             CALL CHKXER( 'ZHEEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZHEEVD('V', 'L', NMAX, A, NMAX, X, W, 1,
     &         RW, 1, IW, 1, INFO )
             CALL CHKXER( 'ZHEEVD ', INFOT, NOUT, LERR, OK )
           END DO
         END DO 
         NT = NT + 12
*
*        ZHEEV
*
         SRNAMT = 'ZHEEV '
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_ZHEEV('N', 'U', NMAX, A, NMAX, X, W, 1,
     &         RW, INFO )
             CALL CHKXER( 'ZHEEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZHEEV('N', 'L', NMAX, A, NMAX, X, W, 1,
     &         RW, INFO )
             CALL CHKXER( 'ZHEEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZHEEV('V', 'U', NMAX, A, NMAX, X, W, 1,
     &         RW, INFO )
             CALL CHKXER( 'ZHEEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZHEEV('V', 'L', NMAX, A, NMAX, X, W, 1,
     &         RW, INFO )
             CALL CHKXER( 'ZHEEV ', INFOT, NOUT, LERR, OK )
           END DO
         END DO
         NT = NT + 5
*
*        ZHEEVX
*
         SRNAMT = 'ZHEEVX'
         DO I = 1, 10
           IF (I/=9) THEN
             INFOT = I
             DO J = 1, 1
               INFO = J + 100*I
               CALL LA_TEST_ZHEEVX( 'N', 'A', 'U', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHEEVX( 'N', 'A', 'L', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHEEVX( 'N', 'V', 'U', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHEEVX( 'N', 'V', 'L', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHEEVX( 'N', 'I', 'U', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHEEVX( 'N', 'I', 'L', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHEEVX( 'V', 'A', 'U', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHEEVX( 'V', 'A', 'L', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHEEVX( 'V', 'V', 'U', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHEEVX( 'V', 'V', 'L', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHEEVX( 'V', 'I', 'U', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHEEVX( 'V', 'I', 'L', NMAX, A, NMAX,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0,
     &           M, X, Z, NMAX, W, NMAX, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHEEVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDIF
         ENDDO  
         NT = NT + 10
*
*        ZHEEVR
*
         SRNAMT = 'ZHEEVR'
         DO I = 1,10
           IF(I/=9)THEN
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_ZHEEVR( 'N', 'A', 'U', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'ZHEEVR', INFOT, NOUT, LERR, OK )  

             INFO = J + 100*I
             CALL LA_TEST_ZHEEVR( 'N', 'A', 'L', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'ZHEEVR', INFOT, NOUT, LERR, OK )  

             INFO = J + 100*I
             CALL LA_TEST_ZHEEVR( 'N', 'V', 'U', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'ZHEEVR', INFOT, NOUT, LERR, OK )  

             INFO = J + 100*I
             CALL LA_TEST_ZHEEVR( 'N', 'V', 'L', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'ZHEEVR', INFOT, NOUT, LERR, OK )  

             INFO = J + 100*I
             CALL LA_TEST_ZHEEVR( 'N', 'I', 'U', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'ZHEEVR', INFOT, NOUT, LERR, OK )  

             INFO = J + 100*I
             CALL LA_TEST_ZHEEVR( 'N', 'I', 'L', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'ZHEEVR', INFOT, NOUT, LERR, OK )  

             INFO = J + 100*I
             CALL LA_TEST_ZHEEVR( 'V', 'A', 'U', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'ZHEEVR', INFOT, NOUT, LERR, OK )  

             INFO = J + 100*I
             CALL LA_TEST_ZHEEVR( 'V', 'A', 'L', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'ZHEEVR', INFOT, NOUT, LERR, OK )  

             INFO = J + 100*I
             CALL LA_TEST_ZHEEVR( 'V', 'V', 'U', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'ZHEEVR', INFOT, NOUT, LERR, OK )  

             INFO = J + 100*I
             CALL LA_TEST_ZHEEVR( 'V', 'V', 'L', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'ZHEEVR', INFOT, NOUT, LERR, OK )  

             INFO = J + 100*I
             CALL LA_TEST_ZHEEVR( 'V', 'I', 'U', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'ZHEEVR', INFOT, NOUT, LERR, OK )  

             INFO = J + 100*I
             CALL LA_TEST_ZHEEVR( 'V', 'I', 'L', NMAX, A, NMAX,
     &         0.0D0, 0.0D0, NMAX, NMAX, 0.0D0,
     &         M, R, Z, NMAX, IW, Q, 2*NMAX, RW, 24*NMAX,
     &         IW( 2*NMAX+1 ),
     &         10*NMAX, INFO )
             CALL CHKXER( 'ZHEEVR', INFOT, NOUT, LERR, OK )  
           ENDDO
         ENDIF
       ENDDO
         NT = NT + 12
*
*        ZHPEVD
*
         SRNAMT = 'ZHPEVD'
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_ZHPEVD('N', 'U', NMAX, A, X, Z, 1,
     &         W, 1, RW, 1, IW, 1, INFO )
             CALL CHKXER( 'ZHPEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZHPEVD('N', 'L', NMAX, A, X, Z, 1,
     &         W, 1, RW, 1, IW, 1, INFO )
             CALL CHKXER( 'ZHPEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZHPEVD('V', 'U', NMAX, A, X, Z, 1,
     &         W, 1, RW, 1, IW, 1, INFO )
             CALL CHKXER( 'ZHPEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZHPEVD('V', 'L', NMAX, A, X, Z, 1,
     &         W, 1, RW, 1, IW, 1, INFO )
             CALL CHKXER( 'ZHPEVD ', INFOT, NOUT, LERR, OK )
           END DO
         END DO
         NT = NT + 13
*
*        ZHPEV
*
         SRNAMT = 'ZHPEV '
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_ZHPEV('N', 'U', NMAX, A, X, Z, 1,
     &         W, RW,  INFO )
             CALL CHKXER( 'ZHPEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZHPEV('N', 'L', NMAX, A, X, Z, 1,
     &         W, RW, INFO )
             CALL CHKXER( 'ZHPEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZHPEV('V', 'U', NMAX, A, X, Z, 1,
     &         W, RW, INFO )
             CALL CHKXER( 'ZHPEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZHPEV('V', 'L', NMAX, A, X, Z, 1,
     &         W, RW, INFO )
             CALL CHKXER( 'ZHPEV ', INFOT, NOUT, LERR, OK )
           END DO
         END DO
         NT = NT + 4
*
*        ZHPEVX
*
         SRNAMT = 'ZHPEVX'
         DO I = 1, 10
           INFOT = I
           IF (I/=9) THEN
             DO J = 1, 1
               INFO = J + 100*I
               CALL LA_TEST_ZHPEVX( 'V', 'A', 'U', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHPEVX( 'V', 'A', 'L', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHPEVX( 'V', 'V', 'U', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHPEVX( 'V', 'V', 'L', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHPEVX( 'V', 'I', 'U', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHPEVX( 'V', 'I', 'L', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHPEVX( 'N', 'A', 'U', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHPEVX( 'N', 'A', 'L', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHPEVX( 'N', 'V', 'U', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHPEVX( 'N', 'V', 'L', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHPEVX( 'N', 'I', 'U', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHPEVX( 'N', 'I', 'L', NMAX, A,
     &           0.0D0, 0.0D0, 0, 0, 0.0D0, M,
     &           X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHPEVX', INFOT, NOUT, LERR, OK )

             ENDDO
           ENDIF
         ENDDO
         NT = NT + 8
*
*     Test error exits for the HB path.
*
      ELSE IF( LSAMEN( 2, C2, 'HB' ) ) THEN
*
*        ZHBTRD
*
         SRNAMT = 'ZHBTRD'
         INFOT = 1
         CALL ZHBTRD( '/', 'U', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'ZHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHBTRD( 'N', '/', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'ZHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZHBTRD( 'N', 'U', -1, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'ZHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZHBTRD( 'N', 'U', 0, -1, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'ZHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZHBTRD( 'N', 'U', 1, 1, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'ZHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZHBTRD( 'V', 'U', 2, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'ZHBTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        ZHBEVD
*
         SRNAMT = 'ZHBEVD'
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_ZHBEVD('N', 'U', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, 1, RW, 1 , IW, 1, INFO )
             CALL CHKXER( 'ZHBEVD ', INFOT, NOUT, LERR, OK )
             INFO = J + 100*I
             CALL LA_TEST_ZHBEVD('N', 'L', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, 1, RW, 1 , IW, 1, INFO )
             CALL CHKXER( 'ZHBEVD ', INFOT, NOUT, LERR, OK )
             INFO = J + 100*I
             CALL LA_TEST_ZHBEVD('N', 'L', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, 1, RW, 1 , IW, 1, INFO )
             CALL CHKXER( 'ZHBEVD ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZHBEVD('N', 'L', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, 1, RW, 1 , IW, 1, INFO )
             CALL CHKXER( 'ZHBEVD ', INFOT, NOUT, LERR, OK )
           END DO
         END DO
         NT = NT + 15
*
*        ZHBEV
*
         SRNAMT = 'ZHBEV '
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_ZHBEV('N', 'U', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, RW,  INFO )
             CALL CHKXER( 'ZHBEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZHBEV('N', 'L', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, RW, INFO )
             CALL CHKXER( 'ZHBEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZHBEV('V', 'U', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, RW, INFO )
             CALL CHKXER( 'ZHBEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZHBEV('V', 'L', NMAX, NMAX, A, 1, X, Z, 1,
     &         W, RW, INFO )
             CALL CHKXER( 'ZHBEV ', INFOT, NOUT, LERR, OK )
           END DO
         END DO
         NT = NT + 6
*
*        ZHBEVX
*
         SRNAMT = 'ZHBEVX'
         DO I = 1, 11
           INFOT = I
           IF (I/=9) THEN
             DO J = 1, 11
               INFO = J + 100*I
               CALL LA_TEST_ZHBEVX( 'N', 'A', 'U', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHBEVX( 'N', 'A', 'L', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHBEVX( 'N', 'V', 'U', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHBEVX( 'N', 'V', 'L', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHBEVX( 'N', 'I', 'U', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHBEVX( 'N', 'I', 'L', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHBEVX( 'V', 'A', 'U', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHBEVX( 'V', 'A', 'L', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHBEVX( 'V', 'V', 'U', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHBEVX( 'V', 'V', 'L', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHBEVX( 'V', 'I', 'U', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHBEVX( 'V', 'I', 'L', NMAX, NMAX, A,
     &           NMAX, Q, NMAX, 0.0D0, 0.0D0, 0,
     &           0, 0.0D0, M, X, Z, NMAX, W, RW, IW, I3, INFO )
               CALL CHKXER( 'ZHBEVX', INFOT, NOUT, LERR, OK )

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
*     End of ZERRST
*
      END
