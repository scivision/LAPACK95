      SUBROUTINE DERRGG( PATH, NUNIT )
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
*  DERRGG tests the error exits for DGGES, DGGESX, DGGEV, DGGEVX,
*  DGGGLM, DGGHRD, DGGLSE, DGGQRF, DGGRQF, DGGSVD, DGGSVP, DHGEQZ,
*  DTGEVC, DTGEXC, DTGSEN, DTGSJA, DTGSNA, and DTGSYL.
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
      INTEGER            IW( NMAX ), IN(100)
      DOUBLE PRECISION   A( NMAX, NMAX ), B( NMAX, NMAX ), LS( NMAX ),
     $                   Q( NMAX, NMAX ), R1( NMAX ), R2( NMAX ),
     $                   R3( NMAX ), RCE( 2 ), RCV( 2 ), RS( NMAX ),
     $                   TAU( NMAX ), U( NMAX, NMAX ), V( NMAX, NMAX ),
     $                   W( LW ), Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            DLCTES, DLCTSX, LSAMEN
      EXTERNAL           DLCTES, DLCTSX, LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHKXER, LA_TEST_DGGES, LA_TEST_DGGESX,
     &                   LA_TEST_DGGEV, LA_TEST_DGGEVX, LA_TEST_DGGGLM,
     $                   DGGHRD, LA_TEST_DGGLSE, DGGQRF, DGGRQF,
     &                   LA_TEST_DGGSVD,
     &                   DGGSVP,
     $                   DHGEQZ, DTGEVC, DTGEXC, DTGSEN, DTGSJA, DTGSNA,
     $                   DTGSYL
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
*        DGGHRD
*
         SRNAMT = 'DGGHRD'
         INFOT = 1
         CALL DGGHRD( '/', 'N', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'DGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DGGHRD( 'N', '/', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'DGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DGGHRD( 'N', 'N', -1, 0, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'DGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DGGHRD( 'N', 'N', 0, 0, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'DGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DGGHRD( 'N', 'N', 0, 1, 1, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'DGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DGGHRD( 'N', 'N', 2, 1, 1, A, 1, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'DGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DGGHRD( 'N', 'N', 2, 1, 1, A, 2, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'DGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL DGGHRD( 'V', 'N', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'DGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL DGGHRD( 'N', 'V', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'DGGHRD', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        DHGEQZ
*
         SRNAMT = 'DHGEQZ'
         INFOT = 1
         CALL DHGEQZ( '/', 'N', 'N', 0, 1, 0, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DHGEQZ( 'E', '/', 'N', 0, 1, 0, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DHGEQZ( 'E', 'N', '/', 0, 1, 0, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DHGEQZ( 'E', 'N', 'N', -1, 0, 0, A, 1, B, 1, R1, R2, R3,
     $                Q, 1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DHGEQZ( 'E', 'N', 'N', 0, 0, 0, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DHGEQZ( 'E', 'N', 'N', 0, 1, 1, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DHGEQZ( 'E', 'N', 'N', 2, 1, 1, A, 1, B, 2, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DHGEQZ( 'E', 'N', 'N', 2, 1, 1, A, 2, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL DHGEQZ( 'E', 'V', 'N', 2, 1, 1, A, 2, B, 2, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL DHGEQZ( 'E', 'N', 'V', 2, 1, 1, A, 2, B, 2, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'DHGEQZ', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        DTGEVC
*
         SRNAMT = 'DTGEVC'
         INFOT = 1
         CALL DTGEVC( '/', 'A', SEL, 0, A, 1, B, 1, Q, 1, Z, 1, 0, M, W,
     $                INFO )
         CALL CHKXER( 'DTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DTGEVC( 'R', '/', SEL, 0, A, 1, B, 1, Q, 1, Z, 1, 0, M, W,
     $                INFO )
         CALL CHKXER( 'DTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DTGEVC( 'R', 'A', SEL, -1, A, 1, B, 1, Q, 1, Z, 1, 0, M,
     $                W, INFO )
         CALL CHKXER( 'DTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DTGEVC( 'R', 'A', SEL, 2, A, 1, B, 2, Q, 1, Z, 2, 0, M, W,
     $                INFO )
         CALL CHKXER( 'DTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DTGEVC( 'R', 'A', SEL, 2, A, 2, B, 1, Q, 1, Z, 2, 0, M, W,
     $                INFO )
         CALL CHKXER( 'DTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DTGEVC( 'L', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 1, 0, M, W,
     $                INFO )
         CALL CHKXER( 'DTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL DTGEVC( 'R', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 1, 0, M, W,
     $                INFO )
         CALL CHKXER( 'DTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL DTGEVC( 'R', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 2, 1, M, W,
     $                INFO )
         CALL CHKXER( 'DTGEVC', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the GSV path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GSV' ) ) THEN
*
*        DGGSVD
*
        SRNAMT = 'DGGSVD'
        DO I = 2, 9
          IN(I) = 1
        END DO
        DO I = 2, 9
          IF ((I /= 5).AND.(I/=6)) THEN
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_DGGSVD('U', 'V',  'Q', NMAX, NMAX, NMAX,
     &        DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &        NMAX, Q, NMAX, W, IW, INFO )           
            CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGGSVD('U', 'V', 'N', NMAX, NMAX, NMAX,
     &        DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &        NMAX, Q, NMAX, W, IW, INFO )           
            CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGGSVD('U', 'N', 'Q', NMAX, NMAX, NMAX,
     &        DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &        NMAX, Q, NMAX, W, IW, INFO )           
            CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGGSVD('U', 'N', 'N', NMAX, NMAX, NMAX,
     &        DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &        NMAX, Q, NMAX, W, IW, INFO )
            CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGGSVD('N', 'V', 'Q', NMAX, NMAX, NMAX,
     &        DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &        NMAX, Q, NMAX, W, IW, INFO )
            CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGGSVD('N', 'V', 'N', NMAX, NMAX, NMAX,
     &        DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &        NMAX, Q, NMAX, W, IW, INFO )
            CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGGSVD('N', 'N', 'Q', NMAX, NMAX, NMAX,
     &        DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &        NMAX, Q, NMAX, W, IW, INFO )
            CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGGSVD('N', 'N', 'N', NMAX, NMAX, NMAX,
     &        DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &        NMAX, Q, NMAX, W, IW, INFO )
            CALL CHKXER( 'DGGSVD', INFOT, NOUT, LERR, OK )
          END DO
          ENDIF
        END DO 
         NT = NT + 11
*
*        DGGSVP
*
         SRNAMT = 'DGGSVP'
         INFOT = 1
         CALL DGGSVP( '/', 'N', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DGGSVP( 'N', '/', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DGGSVP( 'N', 'N', '/', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DGGSVP( 'N', 'N', 'N', -1, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DGGSVP( 'N', 'N', 'N', 0, -1, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DGGSVP( 'N', 'N', 'N', 0, 0, -1, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DGGSVP( 'N', 'N', 'N', 0, 0, 0, A, 0, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DGGSVP( 'N', 'N', 'N', 0, 0, 0, A, 1, B, 0, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL DGGSVP( 'U', 'N', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 0, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL DGGSVP( 'N', 'V', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 0, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL DGGSVP( 'N', 'N', 'Q', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 0, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'DGGSVP', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        DTGSJA
*
         SRNAMT = 'DTGSJA'
         INFOT = 1
         CALL DTGSJA( '/', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DTGSJA( 'N', '/', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DTGSJA( 'N', 'N', '/', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DTGSJA( 'N', 'N', 'N', -1, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DTGSJA( 'N', 'N', 'N', 0, -1, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DTGSJA( 'N', 'N', 'N', 0, 0, -1, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DTGSJA( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 0, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL DTGSJA( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                0, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL DTGSJA( 'U', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 0, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL DTGSJA( 'N', 'V', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 0, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL DTGSJA( 'N', 'N', 'Q', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 0, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'DTGSJA', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*     Test error exits for the GLM path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GLM' ) ) THEN
*
*        DGGGLM
*
        SRNAMT = 'DGGGLM'
        DO I = 2, 5
          IN(I) = 1
        END DO
        DO I = 2, 5
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_DGGGLM( NMAX, NMAX, NMAX, A, NMAX, B, NMAX,
     &        R1, R2, R3, W, LW, INFO)
            CALL CHKXER( 'DGGGLM', INFOT, NOUT, LERR, OK )
          END DO
        END DO 
        NT = NT + 8
*
*     Test error exits for the LSE path.
*
      ELSE IF( LSAMEN( 3, PATH, 'LSE' ) ) THEN
*
*        DGGLSE
*
        SRNAMT = 'DGGLSE'
        DO I = 2, 5
          IN(I) = 1
        END DO
        DO I = 2, 5
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_DGGLSE( NMAX, NMAX, NMAX, A, NMAX, B, NMAX,
     &        R1, R2, R3, W, LW, INFO)
            CALL CHKXER( 'DGGLSE', INFOT, NOUT, LERR, OK ) 
          END DO
      END DO      
      NT = NT + 8
*
*     Test error exits for the GQR path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GQR' ) ) THEN
*
*        DGGQRF
*
         SRNAMT = 'DGGQRF'
         INFOT = 1
         CALL DGGQRF( -1, 0, 0, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'DGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DGGQRF( 0, -1, 0, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'DGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DGGQRF( 0, 0, -1, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'DGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DGGQRF( 0, 0, 0, A, 0, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'DGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DGGQRF( 0, 0, 0, A, 1, R1, B, 0, R2, W, LW, INFO )
         CALL CHKXER( 'DGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL DGGQRF( 1, 1, 2, A, 1, R1, B, 1, R2, W, 1, INFO )
         CALL CHKXER( 'DGGQRF', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        DGGRQF
*
         SRNAMT = 'DGGRQF'
         INFOT = 1
         CALL DGGRQF( -1, 0, 0, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'DGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DGGRQF( 0, -1, 0, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'DGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DGGRQF( 0, 0, -1, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'DGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DGGRQF( 0, 0, 0, A, 0, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'DGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DGGRQF( 0, 0, 0, A, 1, R1, B, 0, R2, W, LW, INFO )
         CALL CHKXER( 'DGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL DGGRQF( 1, 1, 2, A, 1, R1, B, 1, R2, W, 1, INFO )
         CALL CHKXER( 'DGGRQF', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*     Test error exits for the DGS, DGV, DGX, and DXV paths.
*
      ELSE IF( LSAMEN( 3, PATH, 'DGS' ) .OR.
     $         LSAMEN( 3, PATH, 'DGV' ) .OR.
     $         LSAMEN( 3, PATH, 'DGX' ) .OR. LSAMEN( 3, PATH, 'DXV' ) )
     $          THEN
*
*        DGGES
*
         SRNAMT = 'DGGES '
         DO I = 1, 7
           IN(I) = 1
         END DO
         DO I = 1, 7
             INFOT = I
             DO J = 1, 1
               INFO = J + 100*I
               CALL LA_TEST_DGGES( 'N', 'N', 'N', DLCTES, 1, A, 1,
     &           B, 1,
     &           SDIM, R1, R2, R3, Q, 1, U, 1, W, 1, BW, INFO )
               CALL CHKXER( 'DGGES ', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DGGES( 'N', 'N', 'S', DLCTES, 1, A, 1,
     &           B, 1,
     &           SDIM, R1, R2, R3, Q, 1, U, 1, W, 1, BW, INFO )
               CALL CHKXER( 'DGGES ', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DGGES( 'N', 'V', 'N', DLCTES, 1, A, 1,
     &           B, 1,
     &           SDIM, R1, R2, R3, Q, 1, U, 1, W, 1, BW, INFO )
               CALL CHKXER( 'DGGES ', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DGGES( 'N', 'V', 'S', DLCTES, 1, A, 1,
     &           B, 1,
     &           SDIM, R1, R2, R3, Q, 1, U, 1, W, 1, BW, INFO )
               CALL CHKXER( 'DGGES ', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_DGGES( 'V', 'N', 'N', DLCTES, 1, A, 1,
     &           B, 1,
     &           SDIM, R1, R2, R3, Q, 1, U, 1, W, 1, BW, INFO )
               CALL CHKXER( 'DGGES ', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DGGES( 'V', 'N', 'S', DLCTES, 1, A, 1,
     &           B, 1,
     &           SDIM, R1, R2, R3, Q, 1, U, 1, W, 1, BW, INFO )
               CALL CHKXER( 'DGGES ', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DGGES( 'V', 'V', 'N', DLCTES, 1, A, 1,
     &           B, 1,
     &           SDIM, R1, R2, R3, Q, 1, U, 1, W, 1, BW, INFO )
               CALL CHKXER( 'DGGES ', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DGGES( 'V', 'V', 'S', DLCTES, 1, A, 1,
     &           B, 1,
     &           SDIM, R1, R2, R3, Q, 1, U, 1, W, 1, BW, INFO )
               CALL CHKXER( 'DGGES ', INFOT, NOUT, LERR, OK )
             ENDDO
         ENDDO
         NT = NT + 11
*
*        DGGESX
*
         SRNAMT = 'DGGESX'
         DO I = 1, 7
           IN(I) = 1
         END DO
         DO I = 1, 7
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I  
             CALL LA_TEST_DGGESX( 'N', 'N', 'S', DLCTSX, 'N', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &         NMAX, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'N', 'N', 'N', DLCTSX, 'N', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'N', 'V', 'S', DLCTSX, 'N', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'N', 'V', 'N', DLCTSX, 'N', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I 
             CALL LA_TEST_DGGESX( 'V', 'N', 'S', DLCTSX, 'N', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'V', 'N', 'N', DLCTSX, 'N', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'V', 'V', 'S', DLCTSX, 'N', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'V', 'V', 'N', DLCTSX, 'N', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK ) 

             INFO = J + 100*I 
             CALL LA_TEST_DGGESX( 'N', 'N', 'S', DLCTSX, 'E', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &         NMAX, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'N', 'N', 'N', DLCTSX, 'E', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'N', 'V', 'S', DLCTSX, 'E', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'N', 'V', 'N', DLCTSX, 'E', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I 
             CALL LA_TEST_DGGESX( 'V', 'N', 'S', DLCTSX, 'E', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'V', 'N', 'N', DLCTSX, 'E', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'V', 'V', 'S', DLCTSX, 'E', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'V', 'V', 'N', DLCTSX, 'E', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK ) 

             INFO = J + 100*I  
             CALL LA_TEST_DGGESX( 'N', 'N', 'S', DLCTSX, 'V', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &         NMAX, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'N', 'N', 'N', DLCTSX, 'V', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'N', 'V', 'S', DLCTSX, 'V', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'N', 'V', 'N', DLCTSX, 'V', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I 
             CALL LA_TEST_DGGESX( 'V', 'N', 'S', DLCTSX, 'V', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'V', 'N', 'N', DLCTSX, 'V', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'V', 'V', 'S', DLCTSX, 'V', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'V', 'V', 'N', DLCTSX, 'V', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK ) 

             INFO = J + 100*I  
             CALL LA_TEST_DGGESX( 'N', 'N', 'S', DLCTSX, 'B', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &         NMAX, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'N', 'N', 'N', DLCTSX, 'B', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'N', 'V', 'S', DLCTSX, 'B', NMAX, A,
     &         NMAX, B, NMAX, SDIM,
     &         R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &         NMAX, BW, INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'N', 'V', 'N', DLCTSX, 'B', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I 
             CALL LA_TEST_DGGESX( 'V', 'N', 'S', DLCTSX, 'B', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'V', 'N', 'N', DLCTSX, 'B', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'V', 'V', 'S', DLCTSX, 'B', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_DGGESX( 'V', 'V', 'N', DLCTSX, 'B', 1, A,
     &         1, B, 1, SDIM,
     &         R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &         INFO )
             CALL CHKXER( 'DGGESX', INFOT, NOUT, LERR, OK ) 
           ENDDO
         ENDDO
         NT = NT + 13
*
*        DGGEV
*
         SRNAMT = 'DGGEV '
         DO I = 1, 7
           IN(I) = 1
         END DO
         DO I = 1, 7
             INFOT = I
             DO J = 1, 1
               INFO = J + 100*I
               CALL LA_TEST_DGGEV( 'N', 'N', 1, A, 1, B, 1, R1,
     &           R2, R3, Q, 1, U, 1, W, 1, INFO )
               CALL CHKXER( 'DGGEV ', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DGGEV( 'V', 'N', 1, A, 1, B, 1, R1,
     &           R2, R3, Q, 1, U, 1, W, 1, INFO )
               CALL CHKXER( 'DGGEV ', INFOT, NOUT, LERR, OK ) 
               
               INFO = J + 100*I
               CALL LA_TEST_DGGEV( 'N', 'V', 1, A, 1, B, 1,
     &           R1, R2, R3, Q, 1, U, 1, W, 1, INFO )
               CALL CHKXER( 'DGGEV ', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DGGEV( 'V', 'V', 1, A, 1, B, 1, R1,
     &           R2, R3, Q, 1, U, 1, W, 1, INFO )
               CALL CHKXER( 'DGGEV ', INFOT, NOUT, LERR, OK )  
             ENDDO
           ENDDO
           
           
           NT = NT + 10
*
*        DGGEVX
*
           SRNAMT = 'DGGEVX'
           DO I = 1, 16
             IN(I) = 1
           END DO
           DO I = 1, 16
             IF (I/=9 .AND. I/=10 .AND. I/=13 .AND. I/=14) THEN
               INFOT = I
               DO J = 1, 1
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'N', 'N', 'N', 'N', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'N', 'N', 'N', 'E', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'N', 'N', 'N', 'V', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'N', 'V', 'N', 'B', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK ) 
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'N', 'N', 'V', 'N', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'N', 'V', 'N', 'N', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'N', 'N', 'V', 'E', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'N', 'N', 'V', 'V', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'N', 'N', 'V', 'B', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'N', 'V', 'N', 'E', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'N', 'V', 'N', 'V', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'N', 'V', 'N', 'B', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'N', 'V', 'V', 'N', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'N', 'V', 'V', 'E', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'N', 'V', 'V', 'V', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'N', 'V', 'V', 'B', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'P', 'N', 'N', 'N', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'P', 'N', 'N', 'E', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'P', 'N', 'N', 'V', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'P', 'N', 'N', 'B', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK ) 
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'P', 'N', 'V', 'N', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'P', 'V', 'N', 'N', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'P', 'N', 'N', 'E', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'P', 'N', 'V', 'V', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'P', 'N', 'N', 'B', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'P', 'V', 'N', 'E', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'P', 'V', 'N', 'V', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'P', 'V', 'N', 'B', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'P', 'V', 'V', 'N', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'P', 'V', 'V', 'E', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &           IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'P', 'V', 'V', 'V', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'P', 'V', 'V', 'B', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'S', 'N', 'N', 'N', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'S', 'N', 'N', 'E', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'S', 'N', 'N', 'V', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'S', 'N', 'N', 'B', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'S', 'N', 'V', 'N', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'S', 'V', 'N', 'N', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'S', 'N', 'V', 'E', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'S', 'N', 'V', 'V', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'S', 'N', 'V', 'B', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'S', 'V', 'N', 'E', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'S', 'V', 'N', 'V', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'S', 'V', 'N', 'B', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'S', 'V', 'V', 'N', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'S', 'V', 'V', 'E', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'S', 'V', 'V', 'V', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'S', 'V', 'V', 'B', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'B', 'N', 'N', 'N', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'B', 'N', 'N', 'E', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'B', 'N', 'N', 'V', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'B', 'N', 'N', 'B', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'B', 'N', 'V', 'N', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK ) 
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'B', 'V', 'N', 'N', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'B', 'N', 'V', 'E', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'B', 'N', 'V', 'V', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'B', 'N', 'V', 'B', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'B', 'V', 'N', 'E', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'B', 'V', 'N', 'V', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'B', 'V', 'N', 'B', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'B', 'V', 'V', 'N', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'B', 'V', 'V', 'E', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
                 CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 CALL LA_TEST_DGGEVX( 'B', 'V', 'V', 'V', NMAX, A, NMAX,
     &             B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &             LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &             IW, BW, INFO )
               CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DGGEVX( 'B', 'V', 'V', 'B', NMAX, A, NMAX,
     &           B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &           LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &           IW, BW, INFO )
               CALL CHKXER( 'DGGEVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDIF
         ENDDO
         NT = NT + 12
*
*        DTGEXC
*
         SRNAMT = 'DTGEXC'
         INFOT = 3
         CALL DTGEXC( .TRUE., .TRUE., -1, A, 1, B, 1, Q, 1, Z, 1, IFST,
     $                ILST, W, 1, INFO )
         CALL CHKXER( 'DTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DTGEXC( .TRUE., .TRUE., 1, A, 0, B, 1, Q, 1, Z, 1, IFST,
     $                ILST, W, 1, INFO )
         CALL CHKXER( 'DTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DTGEXC( .TRUE., .TRUE., 1, A, 1, B, 0, Q, 1, Z, 1, IFST,
     $                ILST, W, 1, INFO )
         CALL CHKXER( 'DTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DTGEXC( .FALSE., .TRUE., 1, A, 1, B, 1, Q, 0, Z, 1, IFST,
     $                ILST, W, 1, INFO )
         CALL CHKXER( 'DTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DTGEXC( .TRUE., .TRUE., 1, A, 1, B, 1, Q, 0, Z, 1, IFST,
     $                ILST, W, 1, INFO )
         CALL CHKXER( 'DTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL DTGEXC( .TRUE., .FALSE., 1, A, 1, B, 1, Q, 1, Z, 0, IFST,
     $                ILST, W, 1, INFO )
         CALL CHKXER( 'DTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL DTGEXC( .TRUE., .TRUE., 1, A, 1, B, 1, Q, 1, Z, 0, IFST,
     $                ILST, W, 1, INFO )
         CALL CHKXER( 'DTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL DTGEXC( .TRUE., .TRUE., 1, A, 1, B, 1, Q, 1, Z, 1, IFST,
     $                ILST, W, 1, INFO )
         CALL CHKXER( 'DTGEXC', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*        DTGSEN
*
         SRNAMT = 'DTGSEN'
         INFOT = 1
         CALL DTGSEN( -1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, R1, R2,
     $                R3, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'DTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DTGSEN( 1, .TRUE., .TRUE., SEL, -1, A, 1, B, 1, R1, R2,
     $                R3, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'DTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 0, B, 1, R1, R2, R3,
     $                Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'DTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 0, R1, R2, R3,
     $                Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'DTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL DTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, R1, R2, R3,
     $                Q, 0, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'DTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL DTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, R1, R2, R3,
     $                Q, 1, Z, 0, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'DTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL DTGSEN( 0, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, R1, R2, R3,
     $                Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'DTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL DTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, R1, R2, R3,
     $                Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'DTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL DTGSEN( 2, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, R1, R2, R3,
     $                Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'DTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 24
         CALL DTGSEN( 0, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, R1, R2, R3,
     $                Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 20, IW, 0,
     $                INFO )
         CALL CHKXER( 'DTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 24
         CALL DTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, R1, R2, R3,
     $                Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 20, IW, 0,
     $                INFO )
         CALL CHKXER( 'DTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 24
         CALL DTGSEN( 2, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, R1, R2, R3,
     $                Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 20, IW, 1,
     $                INFO )
         CALL CHKXER( 'DTGSEN', INFOT, NOUT, LERR, OK )
         NT = NT + 12
*
*        DTGSNA
*
         SRNAMT = 'DTGSNA'
         INFOT = 1
         CALL DTGSNA( '/', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DTGSNA( 'B', '/', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DTGSNA( 'B', 'A', SEL, -1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DTGSNA( 'B', 'A', SEL, 1, A, 0, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DTGSNA( 'B', 'A', SEL, 1, A, 1, B, 0, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 0, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL DTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 0, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL DTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                0, M, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL DTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 0, IW, INFO )
         CALL CHKXER( 'DTGSNA', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        DTGSYL
*
         SRNAMT = 'DTGSYL'
         INFOT = 1
         CALL DTGSYL( '/', 0, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DTGSYL( 'N', -1, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DTGSYL( 'N', 0, 0, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DTGSYL( 'N', 0, 1, 0, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DTGSYL( 'N', 0, 1, 1, A, 0, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DTGSYL( 'N', 0, 1, 1, A, 1, B, 0, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 0, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL DTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 1, U, 0, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL DTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 0, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL DTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 0,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL DTGSYL( 'N', 1, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL DTGSYL( 'N', 2, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'DTGSYL', INFOT, NOUT, LERR, OK )
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
*     End of DERRGG
*
      END
