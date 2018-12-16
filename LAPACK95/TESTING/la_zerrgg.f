      SUBROUTINE ZERRGG( PATH, NUNIT )
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
*  ZERRGG tests the error exits for ZGGES, ZGGESX, ZGGEV, ZGGEVX,
*  ZGGGLM, ZGGHRD, ZGGLSE, ZGGQRF, ZGGRQF, ZGGSVD, ZGGSVP, ZHGEQZ,
*  ZTGEVC, ZTGEXC, ZTGSEN, ZTGSJA, ZTGSNA, and ZTGSYL.
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
      INTEGER            NMAX, LW
      PARAMETER          ( NMAX = 3, LW = 6*NMAX )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            DUMMYK, DUMMYL, I, IFST, ILST, INFO, J, M,
     $                   NCYCLE, NT, SDIM
      DOUBLE PRECISION   ANRM, BNRM, DIF, SCALE, TOLA, TOLB
*     ..
*     .. Local Arrays ..
      LOGICAL            BW( NMAX ), SEL( NMAX )
      INTEGER            IW( LW ), IN(100)
      DOUBLE PRECISION   LS( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   RCE( NMAX ), RCV( NMAX ), RS( NMAX ), RW( LW )
      COMPLEX*16         A( NMAX, NMAX ), ALPHA( NMAX ),
     $                   B( NMAX, NMAX ), BETA( NMAX ), Q( NMAX, NMAX ),
     $                   TAU( NMAX ), U( NMAX, NMAX ), V( NMAX, NMAX ),
     $                   W( LW ), Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN, ZLCTES, ZLCTSX
      EXTERNAL           LSAMEN, ZLCTES, ZLCTSX
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHKXER, LA_TEST_ZGGES, LA_TEST_ZGGESX,
     &                   LA_TEST_ZGGEV, LA_TEST_ZGGEVX,
     &                   LA_TEST_ZGGGLM,
     $                   ZGGHRD, LA_TEST_ZGGLSE, ZGGQRF, ZGGRQF,
     &                   LA_TEST_ZGGSVD,
     &                   ZGGSVP, ZHGEQZ, ZTGEVC, ZTGEXC, ZTGSEN, ZTGSJA,
     &                   ZTGSNA, ZTGSYL
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
*     .. Executable Statements ..
*
      NOUT = NUNIT
      WRITE( NOUT, FMT = * )
      C2 = PATH( 2: 3 )
*
*     Set the variables to innocuous values.
*
      DO 20 J = 1, NMAX
         SEL( J ) = .TRUE.
         DO 10 I = 1, NMAX
            A( I, J ) = ZERO
            B( I, J ) = ZERO
   10    CONTINUE
   20 CONTINUE
      DO 30 I = 1, NMAX
         A( I, I ) = ONE
         B( I, I ) = ONE
   30 CONTINUE
      OK = .TRUE.
      TOLA = 1.0D0
      TOLB = 1.0D0
      IFST = 1
      ILST = 1
      NT = 0
*
*     Test error exits for the GG path.
*
      IF( LSAMEN( 2, C2, 'GG' ) ) THEN
*
*        ZGGHRD
*
         SRNAMT = 'ZGGHRD'
         INFOT = 1
         CALL ZGGHRD( '/', 'N', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'ZGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZGGHRD( 'N', '/', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'ZGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZGGHRD( 'N', 'N', -1, 0, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'ZGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZGGHRD( 'N', 'N', 0, 0, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'ZGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZGGHRD( 'N', 'N', 0, 1, 1, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'ZGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZGGHRD( 'N', 'N', 2, 1, 1, A, 1, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'ZGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZGGHRD( 'N', 'N', 2, 1, 1, A, 2, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'ZGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL ZGGHRD( 'V', 'N', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'ZGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL ZGGHRD( 'N', 'V', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'ZGGHRD', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        ZHGEQZ
*
         SRNAMT = 'ZHGEQZ'
         INFOT = 1
         CALL ZHGEQZ( '/', 'N', 'N', 0, 1, 0, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'ZHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHGEQZ( 'E', '/', 'N', 0, 1, 0, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'ZHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZHGEQZ( 'E', 'N', '/', 0, 1, 0, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'ZHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZHGEQZ( 'E', 'N', 'N', -1, 0, 0, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'ZHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZHGEQZ( 'E', 'N', 'N', 0, 0, 0, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'ZHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZHGEQZ( 'E', 'N', 'N', 0, 1, 1, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'ZHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZHGEQZ( 'E', 'N', 'N', 2, 1, 1, A, 1, B, 2, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'ZHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZHGEQZ( 'E', 'N', 'N', 2, 1, 1, A, 2, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'ZHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL ZHGEQZ( 'E', 'V', 'N', 2, 1, 1, A, 2, B, 2, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'ZHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL ZHGEQZ( 'E', 'N', 'V', 2, 1, 1, A, 2, B, 2, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'ZHGEQZ', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        ZTGEVC
*
         SRNAMT = 'ZTGEVC'
         INFOT = 1
         CALL ZTGEVC( '/', 'A', SEL, 0, A, 1, B, 1, Q, 1, Z, 1, 0, M, W,
     $                RW, INFO )
         CALL CHKXER( 'ZTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZTGEVC( 'R', '/', SEL, 0, A, 1, B, 1, Q, 1, Z, 1, 0, M, W,
     $                RW, INFO )
         CALL CHKXER( 'ZTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZTGEVC( 'R', 'A', SEL, -1, A, 1, B, 1, Q, 1, Z, 1, 0, M,
     $                W, RW, INFO )
         CALL CHKXER( 'ZTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZTGEVC( 'R', 'A', SEL, 2, A, 1, B, 2, Q, 1, Z, 2, 0, M, W,
     $                RW, INFO )
         CALL CHKXER( 'ZTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZTGEVC( 'R', 'A', SEL, 2, A, 2, B, 1, Q, 1, Z, 2, 0, M, W,
     $                RW, INFO )
         CALL CHKXER( 'ZTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZTGEVC( 'L', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 1, 0, M, W,
     $                RW, INFO )
         CALL CHKXER( 'ZTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL ZTGEVC( 'R', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 1, 0, M, W,
     $                RW, INFO )
         CALL CHKXER( 'ZTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL ZTGEVC( 'R', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 2, 1, M, W,
     $                RW, INFO )
         CALL CHKXER( 'ZTGEVC', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the GSV path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GSV' ) ) THEN
*
*        ZGGSVD
*
        SRNAMT = 'ZGGSVD'
        DO I = 2, 9
          IN(I) = 1
        END DO
        DO I = 2, 9
          IF ((I /= 5).AND.(I/=6)) THEN
            INFOT = I
            DO J = 1, 1
              INFO = J + 100*I
              CALL LA_TEST_ZGGSVD('U', 'V',  'Q', NMAX, NMAX, NMAX,
     &          DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &          NMAX, Q, NMAX, W, RW, IW, INFO )
              CALL CHKXER( 'ZGGSVD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZGGSVD('U', 'V', 'N', NMAX, NMAX, NMAX,
     &          DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &          NMAX, Q, NMAX, W, RW,  IW, INFO )
              CALL CHKXER( 'ZGGSVD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZGGSVD('U', 'N', 'Q', NMAX, NMAX, NMAX,
     &          DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &          NMAX, Q, NMAX, W, RW,  IW, INFO )
              CALL CHKXER( 'ZGGSVD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZGGSVD('U', 'N', 'N', NMAX, NMAX, NMAX,
     &          DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &          NMAX, Q, NMAX, W, RW, IW, INFO )
              CALL CHKXER( 'ZGGSVD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZGGSVD('N', 'V', 'Q', NMAX, NMAX, NMAX,
     &          DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &          NMAX, Q, NMAX, W, RW, IW, INFO )
              CALL CHKXER( 'ZGGSVD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZGGSVD('N', 'V', 'N', NMAX, NMAX, NMAX,
     &          DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &          NMAX, Q, NMAX, W, RW, IW, INFO )
              CALL CHKXER( 'ZGGSVD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZGGSVD('N', 'N', 'Q', NMAX, NMAX, NMAX,
     &          DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &          NMAX, Q, NMAX, W, RW, IW, INFO )
              CALL CHKXER( 'ZGGSVD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZGGSVD('N', 'N', 'N', NMAX, NMAX, NMAX,
     &          DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &          NMAX, Q, NMAX, W, RW, IW, INFO )
              CALL CHKXER( 'ZGGSVD', INFOT, NOUT, LERR, OK )
            END DO
          ENDIF
        END DO 

*
*        ZGGSVP
*
         SRNAMT = 'ZGGSVP'
         INFOT = 1
         CALL ZGGSVP( '/', 'N', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'ZGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZGGSVP( 'N', '/', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'ZGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZGGSVP( 'N', 'N', '/', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'ZGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZGGSVP( 'N', 'N', 'N', -1, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'ZGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZGGSVP( 'N', 'N', 'N', 0, -1, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'ZGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZGGSVP( 'N', 'N', 'N', 0, 0, -1, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'ZGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZGGSVP( 'N', 'N', 'N', 0, 0, 0, A, 0, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'ZGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZGGSVP( 'N', 'N', 'N', 0, 0, 0, A, 1, B, 0, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'ZGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL ZGGSVP( 'U', 'N', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 0, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'ZGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL ZGGSVP( 'N', 'V', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 0, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'ZGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL ZGGSVP( 'N', 'N', 'Q', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 0, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'ZGGSVP', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        ZTGSJA
*
         SRNAMT = 'ZTGSJA'
         INFOT = 1
         CALL ZTGSJA( '/', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZTGSJA( 'N', '/', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZTGSJA( 'N', 'N', '/', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZTGSJA( 'N', 'N', 'N', -1, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZTGSJA( 'N', 'N', 'N', 0, -1, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZTGSJA( 'N', 'N', 'N', 0, 0, -1, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZTGSJA( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 0, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL ZTGSJA( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                0, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL ZTGSJA( 'U', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 0, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL ZTGSJA( 'N', 'V', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 0, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'ZTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL ZTGSJA( 'N', 'N', 'Q', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 0, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'ZTGSJA', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*     Test error exits for the GLM path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GLM' ) ) THEN
*
*        ZGGGLM
*
        SRNAMT = 'ZGGGLM'
        DO I = 2, 5
          IN(I) = 1
        END DO
        DO I = 2, 5
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_ZGGGLM( NMAX, NMAX, NMAX, A, NMAX, B, NMAX,
     &        TAU, ALPHA, BETA, W, LW, INFO)
            CALL CHKXER( 'ZGGGLM', INFOT, NOUT, LERR, OK )
          END DO
        END DO 
        NT = NT + 8
*
*     Test error exits for the LSE path.
*
      ELSE IF( LSAMEN( 3, PATH, 'LSE' ) ) THEN
*
*        ZGGLSE
*
        SRNAMT = 'ZGGLSE'
        DO I = 2, 5
          IN(I) = 1
        END DO
        DO I = 2, 5
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_ZGGLSE( NMAX, NMAX, NMAX, A, NMAX, B, NMAX,
     &        TAU, ALPHA, BETA, W, LW, INFO)
            CALL CHKXER( 'ZGGLSE', INFOT, NOUT, LERR, OK )
          END DO
        END DO 
        NT = NT + 8
*
*     Test error exits for the GQR path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GQR' ) ) THEN
*
*        ZGGQRF
*
         SRNAMT = 'ZGGQRF'
         INFOT = 1
         CALL ZGGQRF( -1, 0, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'ZGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZGGQRF( 0, -1, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'ZGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZGGQRF( 0, 0, -1, A, 1, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'ZGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZGGQRF( 0, 0, 0, A, 0, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'ZGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZGGQRF( 0, 0, 0, A, 1, ALPHA, B, 0, BETA, W, LW, INFO )
         CALL CHKXER( 'ZGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL ZGGQRF( 1, 1, 2, A, 1, ALPHA, B, 1, BETA, W, 1, INFO )
         CALL CHKXER( 'ZGGQRF', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        ZGGRQF
*
         SRNAMT = 'ZGGRQF'
         INFOT = 1
         CALL ZGGRQF( -1, 0, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'ZGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZGGRQF( 0, -1, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'ZGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZGGRQF( 0, 0, -1, A, 1, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'ZGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZGGRQF( 0, 0, 0, A, 0, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'ZGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZGGRQF( 0, 0, 0, A, 1, ALPHA, B, 0, BETA, W, LW, INFO )
         CALL CHKXER( 'ZGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL ZGGRQF( 1, 1, 2, A, 1, ALPHA, B, 1, BETA, W, 1, INFO )
         CALL CHKXER( 'ZGGRQF', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*     Test error exits for the ZGS, ZGV, ZGX, and ZXV paths.
*
      ELSE IF( LSAMEN( 3, PATH, 'ZGS' ) .OR.
     $         LSAMEN( 3, PATH, 'ZGV' ) .OR.
     $         LSAMEN( 3, PATH, 'ZGX' ) .OR. LSAMEN( 3, PATH, 'ZXV' ) )
     $          THEN
*
*        ZGGES
*
      SRNAMT = 'ZGGES '
      DO I = 1, 6
        IN(I) = 1
      END DO
      DO I = 1, 6
          INFOT = I
          DO J = 1, 1
             INFO = J + 100*I
            CALL LA_TEST_ZGGES( 'N', 'N', 'N', ZLCTES, 1, A, 1, B, 1,
     &        SDIM, ALPHA,
     &        BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
            CALL CHKXER( 'ZGGES ', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGGES( 'N', 'N', 'S', ZLCTES, 1, A, 1, B, 1,
     &        SDIM, ALPHA,
     &        BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
            CALL CHKXER( 'ZGGES ', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGGES( 'N', 'V', 'N', ZLCTES, 1, A, 1, B, 1,
     &        SDIM, ALPHA,
     &        BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
            CALL CHKXER( 'ZGGES ', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGGES( 'N', 'V', 'S', ZLCTES, 1, A, 1, B, 1,
     &        SDIM, ALPHA,
     &        BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
            CALL CHKXER( 'ZGGES ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
            CALL LA_TEST_ZGGES( 'V', 'N', 'N', ZLCTES, 1, A, 1, B, 1,
     &        SDIM, ALPHA,
     &        BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
            CALL CHKXER( 'ZGGES ', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGGES( 'V', 'N', 'S', ZLCTES, 1, A, 1, B, 1,
     &        SDIM, ALPHA,
     &        BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
            CALL CHKXER( 'ZGGES ', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGGES( 'V', 'V', 'N', ZLCTES, 1, A, 1, B, 1,
     &        SDIM, ALPHA,
     &        BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
            CALL CHKXER( 'ZGGES ', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGGES( 'V', 'V', 'S', ZLCTES, 1, A, 1, B, 1,
     &        SDIM, ALPHA,
     &        BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
            CALL CHKXER( 'ZGGES ', INFOT, NOUT, LERR, OK )
          ENDDO
        ENDDO
         NT = NT + 11
*
*        ZGGESX
*
         SRNAMT = 'ZGGESX'
         DO I = 1, 6
           IN(I) = 1
         END DO
         DO I = 1, 6
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'N', 'N', 'S', ZLCTSX, 'N', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'N', 'N', 'N', ZLCTSX, 'N', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'N', 'V', 'S', ZLCTSX, 'N', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'N', 'V', 'N', ZLCTSX, 'N', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'V', 'N', 'S', ZLCTSX, 'N', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'V', 'N', 'N', ZLCTSX, 'N', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'V', 'V', 'S', ZLCTSX, 'N', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'V', 'V', 'N', ZLCTSX, 'N', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'N', 'N', 'S', ZLCTSX, 'E', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'N', 'N', 'N', ZLCTSX, 'E', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'N', 'V', 'S', ZLCTSX, 'E', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'N', 'V', 'N', ZLCTSX, 'E', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'V', 'N', 'S', ZLCTSX, 'E', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'V', 'N', 'N', ZLCTSX, 'E', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'V', 'V', 'S', ZLCTSX, 'E', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'V', 'V', 'N', ZLCTSX, 'E', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )
!!!
             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'N', 'N', 'S', ZLCTSX, 'V', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'N', 'N', 'N', ZLCTSX, 'V', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'N', 'V', 'S', ZLCTSX, 'V', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'N', 'V', 'N', ZLCTSX, 'V', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'V', 'N', 'S', ZLCTSX, 'V', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'V', 'N', 'N', ZLCTSX, 'V', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'V', 'V', 'S', ZLCTSX, 'V', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'V', 'V', 'N', ZLCTSX, 'V', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )
!!
                          INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'N', 'N', 'S', ZLCTSX, 'B', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'N', 'N', 'N', ZLCTSX, 'B', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'N', 'V', 'S', ZLCTSX, 'B', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'N', 'V', 'N', ZLCTSX, 'B', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'V', 'N', 'S', ZLCTSX, 'B', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'V', 'N', 'N', ZLCTSX, 'B', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'V', 'V', 'S', ZLCTSX, 'B', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGESX( 'V', 'V', 'N', ZLCTSX, 'B', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         ALPHA, BETA, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, RW, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )

             
           ENDDO
         ENDDO
         
!         INFOT = 1
!         CALL ZGGESX( '/', 'N', 'S', ZLCTSX, 'N', 1, A, 1, B, 1, SDIM,
!     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
!     $                1, BW, INFO )
!         CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )
!         INFOT = 2
!         CALL ZGGESX( 'N', '/', 'S', ZLCTSX, 'N', 1, A, 1, B, 1, SDIM,
!     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
!     $                1, BW, INFO )
!         CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )
!         INFOT = 3
!         CALL ZGGESX( 'V', 'V', '/', ZLCTSX, 'N', 1, A, 1, B, 1, SDIM,
!     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
!     $                1, BW, INFO )
!         CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )
!         INFOT = 5
!         CALL ZGGESX( 'V', 'V', 'S', ZLCTSX, '/', 1, A, 1, B, 1, SDIM,
!     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
!     $                1, BW, INFO )
!         CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )
!         INFOT = 6
!         CALL ZGGESX( 'V', 'V', 'S', ZLCTSX, 'B', -1, A, 1, B, 1, SDIM,
!     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
!     $                1, BW, INFO )
!         CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )
!         INFOT = 8
!         CALL ZGGESX( 'V', 'V', 'S', ZLCTSX, 'B', 1, A, 0, B, 1, SDIM,
!     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
!     $                1, BW, INFO )
!         CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )
!         INFOT = 10
!         CALL ZGGESX( 'V', 'V', 'S', ZLCTSX, 'B', 1, A, 1, B, 0, SDIM,
!     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
!     $                1, BW, INFO )
!         CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )
!         INFOT = 15
!         CALL ZGGESX( 'V', 'V', 'S', ZLCTSX, 'B', 1, A, 1, B, 1, SDIM,
!     $                ALPHA, BETA, Q, 0, U, 1, RCE, RCV, W, 1, RW, IW,
!     $                1, BW, INFO )
!         CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )
!         INFOT = 15
!         CALL ZGGESX( 'V', 'V', 'S', ZLCTSX, 'B', 2, A, 2, B, 2, SDIM,
!     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
!     $                1, BW, INFO )
!         CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )
!         INFOT = 17
!         CALL ZGGESX( 'V', 'V', 'S', ZLCTSX, 'B', 1, A, 1, B, 1, SDIM,
!     $                ALPHA, BETA, Q, 1, U, 0, RCE, RCV, W, 1, RW, IW,
!     $                1, BW, INFO )
!         CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )
!         INFOT = 17
!         CALL ZGGESX( 'V', 'V', 'S', ZLCTSX, 'B', 2, A, 2, B, 2, SDIM,
!     $                ALPHA, BETA, Q, 2, U, 1, RCE, RCV, W, 1, RW, IW,
!     $                1, BW, INFO )
!         CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )
!         INFOT = 21
!         CALL ZGGESX( 'V', 'V', 'S', ZLCTSX, 'B', 2, A, 2, B, 2, SDIM,
!     $                ALPHA, BETA, Q, 2, U, 2, RCE, RCV, W, 1, RW, IW,
!     $                1, BW, INFO )
!         CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )
!         INFOT = 24
!         CALL ZGGESX( 'V', 'V', 'S', ZLCTSX, 'V', 1, A, 1, B, 1, SDIM,
!     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 32, RW, IW,
!     $                -1, BW, INFO )
!         CALL CHKXER( 'ZGGESX', INFOT, NOUT, LERR, OK )
         NT = NT + 13
*
*        ZGGEV
*
         SRNAMT = 'ZGGEV'
         DO I = 1, 6
           IN(I) = 1
         END DO
         DO I = 1, 6
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I 
             CALL LA_TEST_ZGGEV( 'N', 'N', 1, A, 1, B, 1, ALPHA,
     &          BETA, Q, 1, U, 1, W, 1, RW, INFO )
             CALL CHKXER( 'ZGGEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGEV( 'V', 'N', 1, A, 1, B, 1, ALPHA,
     &          BETA, Q, 1, U, 1, W, 1, RW, INFO )
             CALL CHKXER( 'ZGGEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGEV( 'N', 'V', 1, A, 1, B, 1, ALPHA,
     &          BETA, Q, 1, U, 1, W, 1, RW, INFO )
             CALL CHKXER( 'ZGGEV ', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_ZGGEV( 'V', 'V', 1, A, 1, B, 1, ALPHA,
     &          BETA, Q, 1, U, 1, W, 1, RW, INFO )
             CALL CHKXER( 'ZGGEV ', INFOT, NOUT, LERR, OK )

           ENDDO
         ENDDO
         
         NT = NT + 10
*
*        ZGGEVX
*
         SRNAMT = 'ZGGEVX'
         DO I = 1, 15
           IN(I) = 1
         END DO
         DO I = 1, 15
           IF (I/=8 .AND. I/=9 .AND. I/=12 .AND. I/=13) THEN
             INFOT = I
             DO J = 1, 1
               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'N', 'N', 'N', 'N', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'N', 'N', 'N', 'E', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'N', 'N', 'N', 'V', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'N', 'N', 'N', 'B', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'N', 'N', 'V', 'N', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'N', 'V', 'N', 'N', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'N', 'N', 'V', 'E', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'N', 'N', 'V', 'V', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 
               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'N', 'N', 'V', 'B', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'N', 'V', 'N', 'E', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'N', 'V', 'N', 'V', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'N', 'V', 'N', 'B', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 
               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'N', 'V', 'V', 'N', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'N', 'V', 'V', 'E', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'N', 'V', 'V', 'V', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'N', 'V', 'V', 'B', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'P', 'N', 'N', 'N', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'P', 'N', 'N', 'E', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'P', 'N', 'N', 'V', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'P', 'N', 'N', 'B', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'P', 'N', 'V', 'N', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'P', 'V', 'N', 'N', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'P', 'N', 'V', 'E', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'P', 'N', 'V', 'V', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 
               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'P', 'N', 'V', 'B', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'P', 'V', 'N', 'E', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'P', 'V', 'N', 'V', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'P', 'V', 'N', 'B', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 
               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'P', 'V', 'V', 'N', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'P', 'V', 'V', 'E', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'P', 'V', 'V', 'V', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'P', 'V', 'V', 'B', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'S', 'N', 'N', 'N', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'S', 'N', 'N', 'E', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'S', 'N', 'N', 'V', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'S', 'N', 'N', 'B', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'S', 'N', 'V', 'N', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'S', 'V', 'N', 'N', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'S', 'N', 'V', 'E', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'S', 'N', 'V', 'V', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 
               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'S', 'N', 'V', 'B', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'S', 'V', 'N', 'E', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'S', 'V', 'N', 'V', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'S', 'V', 'N', 'B', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 
               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'S', 'V', 'V', 'N', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'S', 'V', 'V', 'E', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'S', 'V', 'V', 'V', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'S', 'V', 'V', 'B', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'B', 'N', 'N', 'N', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'B', 'N', 'N', 'E', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'B', 'N', 'N', 'V', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'B', 'N', 'N', 'B', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'B', 'N', 'V', 'N', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'B', 'V', 'N', 'N', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'B', 'N', 'V', 'E', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'B', 'N', 'V', 'V', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 
               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'B', 'N', 'V', 'B', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'B', 'V', 'N', 'E', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'B', 'V', 'N', 'V', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'B', 'V', 'N', 'B', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 
               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'B', 'V', 'V', 'N', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'B', 'V', 'V', 'E', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'B', 'V', 'V', 'V', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_ZGGEVX( 'B', 'V', 'V', 'B', NMAX, A,
     &           NMAX, B, NMAX, ALPHA, BETA, Q,
     &           NMAX, U, NMAX, NMAX, NMAX, LS, RS, ANRM, BNRM,
     &           RCE, RCV, W, NMAX,
     &           RW, IW, BW, INFO )
               CALL CHKXER( 'ZGGEVX', INFOT, NOUT, LERR, OK ) 
               

             ENDDO
           ENDIF
         ENDDO
         
         NT = NT + 12
*
*        ZTGEXC
*
         SRNAMT = 'ZTGEXC'
         INFOT = 3
         CALL ZTGEXC( .TRUE., .TRUE., -1, A, 1, B, 1, Q, 1, Z, 1, IFST,
     $                ILST, INFO )
         CALL CHKXER( 'ZTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZTGEXC( .TRUE., .TRUE., 1, A, 0, B, 1, Q, 1, Z, 1, IFST,
     $                ILST, INFO )
         CALL CHKXER( 'ZTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZTGEXC( .TRUE., .TRUE., 1, A, 1, B, 0, Q, 1, Z, 1, IFST,
     $                ILST, INFO )
         CALL CHKXER( 'ZTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZTGEXC( .FALSE., .TRUE., 1, A, 1, B, 1, Q, 0, Z, 1, IFST,
     $                ILST, INFO )
         CALL CHKXER( 'ZTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZTGEXC( .TRUE., .TRUE., 1, A, 1, B, 1, Q, 0, Z, 1, IFST,
     $                ILST, INFO )
         CALL CHKXER( 'ZTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL ZTGEXC( .TRUE., .FALSE., 1, A, 1, B, 1, Q, 1, Z, 0, IFST,
     $                ILST, INFO )
         CALL CHKXER( 'ZTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL ZTGEXC( .TRUE., .TRUE., 1, A, 1, B, 1, Q, 1, Z, 0, IFST,
     $                ILST, INFO )
         CALL CHKXER( 'ZTGEXC', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
*        ZTGSEN
*
         SRNAMT = 'ZTGSEN'
         INFOT = 1
         CALL ZTGSEN( -1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'ZTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZTGSEN( 1, .TRUE., .TRUE., SEL, -1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'ZTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 0, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'ZTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 0, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'ZTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL ZTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 0, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'ZTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL ZTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 0, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'ZTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 21
         CALL ZTGSEN( 3, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, -5, IW,
     $                1, INFO )
         CALL CHKXER( 'ZTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 23
         CALL ZTGSEN( 0, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 20, IW,
     $                0, INFO )
         CALL CHKXER( 'ZTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 23
         CALL ZTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 20, IW,
     $                0, INFO )
         CALL CHKXER( 'ZTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 23
         CALL ZTGSEN( 5, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 20, IW,
     $                1, INFO )
         CALL CHKXER( 'ZTGSEN', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        ZTGSNA
*
         SRNAMT = 'ZTGSNA'
         INFOT = 1
         CALL ZTGSNA( '/', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZTGSNA( 'B', '/', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZTGSNA( 'B', 'A', SEL, -1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZTGSNA( 'B', 'A', SEL, 1, A, 0, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZTGSNA( 'B', 'A', SEL, 1, A, 1, B, 0, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 0, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL ZTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 0, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL ZTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                0, M, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL ZTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 0, IW, INFO )
         CALL CHKXER( 'ZTGSNA', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        ZTGSYL
*
         SRNAMT = 'ZTGSYL'
         INFOT = 1
         CALL ZTGSYL( '/', 0, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZTGSYL( 'N', -1, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZTGSYL( 'N', 0, 0, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZTGSYL( 'N', 0, 1, 0, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZTGSYL( 'N', 0, 1, 1, A, 0, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZTGSYL( 'N', 0, 1, 1, A, 1, B, 0, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 0, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL ZTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 1, U, 0, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL ZTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 0, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL ZTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 0,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL ZTGSYL( 'N', 1, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL ZTGSYL( 'N', 2, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'ZTGSYL', INFOT, NOUT, LERR, OK )
         NT = NT + 12
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
 9999 FORMAT( 1X, A3, ' routines passed the tests of the error exits (',
     $      I3, ' tests done)' )
 9998 FORMAT( ' *** ', A3, ' routines failed the tests of the error ',
     $      'exits ***' )
*
      RETURN
*
*     End of ZERRGG
*
      END
