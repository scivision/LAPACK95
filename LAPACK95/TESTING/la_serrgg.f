      SUBROUTINE SERRGG( PATH, NUNIT )
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
*  SERRGG tests the error exits for SGGES, SGGESX, SGGEV, SGGEVX,
*  SGGGLM, SGGHRD, SGGLSE, SGGQRF, SGGRQF, SGGSVD, SGGSVP, SHGEQZ,
*  STGEVC, STGEXC, STGSEN, STGSJA, STGSNA, and STGSYL.
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
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            DUMMYK, DUMMYL, I, IFST, ILST, INFO, J, M,
     $                   NCYCLE, NT, SDIM
      REAL               ANRM, BNRM, DIF, SCALE, TOLA, TOLB
*     ..
*     .. Local Arrays ..
      LOGICAL            BW( NMAX ), SEL( NMAX )
      INTEGER            IW( NMAX ), IN( 100 )
      REAL               A( NMAX, NMAX ), B( NMAX, NMAX ), LS( NMAX ),
     $                   Q( NMAX, NMAX ), R1( NMAX ), R2( NMAX ),
     $                   R3( NMAX ), RCE( 2 ), RCV( 2 ), RS( NMAX ),
     $                   TAU( NMAX ), U( NMAX, NMAX ), V( NMAX, NMAX ),
     $                   W( LW ), Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN, SLCTES, SLCTSX
      EXTERNAL           LSAMEN, SLCTES, SLCTSX
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHKXER, LA_TEST_SGGES, LA_TEST_SGGESX,
     &                   LA_TEST_SGGEV, LA_TEST_SGGEVX,
     &                   LA_TEST_SGGGLM,
     $                   SGGHRD, LA_TEST_SGGLSE, SGGQRF, SGGRQF,
     &                   LA_TEST_SGGSVD,
     &                   SGGSVP, SHGEQZ, STGEVC, STGEXC, STGSEN, STGSJA,
     &                   STGSNA, STGSYL
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
      TOLA = 1.0E0
      TOLB = 1.0E0
      IFST = 1
      ILST = 1
      NT = 0
*
*     Test error exits for the GG path.
*
      IF( LSAMEN( 2, C2, 'GG' ) ) THEN
*
*        SGGHRD
*
         SRNAMT = 'SGGHRD'
         INFOT = 1
         CALL SGGHRD( '/', 'N', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'SGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGGHRD( 'N', '/', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'SGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGGHRD( 'N', 'N', -1, 0, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'SGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SGGHRD( 'N', 'N', 0, 0, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'SGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL SGGHRD( 'N', 'N', 0, 1, 1, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'SGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL SGGHRD( 'N', 'N', 2, 1, 1, A, 1, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'SGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL SGGHRD( 'N', 'N', 2, 1, 1, A, 2, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'SGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL SGGHRD( 'V', 'N', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'SGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL SGGHRD( 'N', 'V', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'SGGHRD', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        SHGEQZ
*
         SRNAMT = 'SHGEQZ'
         INFOT = 1
         CALL SHGEQZ( '/', 'N', 'N', 0, 1, 0, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SHGEQZ( 'E', '/', 'N', 0, 1, 0, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SHGEQZ( 'E', 'N', '/', 0, 1, 0, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SHGEQZ( 'E', 'N', 'N', -1, 0, 0, A, 1, B, 1, R1, R2, R3,
     $                Q, 1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL SHGEQZ( 'E', 'N', 'N', 0, 0, 0, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL SHGEQZ( 'E', 'N', 'N', 0, 1, 1, A, 1, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL SHGEQZ( 'E', 'N', 'N', 2, 1, 1, A, 1, B, 2, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL SHGEQZ( 'E', 'N', 'N', 2, 1, 1, A, 2, B, 1, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL SHGEQZ( 'E', 'V', 'N', 2, 1, 1, A, 2, B, 2, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL SHGEQZ( 'E', 'N', 'V', 2, 1, 1, A, 2, B, 2, R1, R2, R3, Q,
     $                1, Z, 1, W, LW, INFO )
         CALL CHKXER( 'SHGEQZ', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        STGEVC
*
         SRNAMT = 'STGEVC'
         INFOT = 1
         CALL STGEVC( '/', 'A', SEL, 0, A, 1, B, 1, Q, 1, Z, 1, 0, M, W,
     $                INFO )
         CALL CHKXER( 'STGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL STGEVC( 'R', '/', SEL, 0, A, 1, B, 1, Q, 1, Z, 1, 0, M, W,
     $                INFO )
         CALL CHKXER( 'STGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL STGEVC( 'R', 'A', SEL, -1, A, 1, B, 1, Q, 1, Z, 1, 0, M,
     $                W, INFO )
         CALL CHKXER( 'STGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL STGEVC( 'R', 'A', SEL, 2, A, 1, B, 2, Q, 1, Z, 2, 0, M, W,
     $                INFO )
         CALL CHKXER( 'STGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL STGEVC( 'R', 'A', SEL, 2, A, 2, B, 1, Q, 1, Z, 2, 0, M, W,
     $                INFO )
         CALL CHKXER( 'STGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL STGEVC( 'L', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 1, 0, M, W,
     $                INFO )
         CALL CHKXER( 'STGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL STGEVC( 'R', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 1, 0, M, W,
     $                INFO )
         CALL CHKXER( 'STGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL STGEVC( 'R', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 2, 1, M, W,
     $                INFO )
         CALL CHKXER( 'STGEVC', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the GSV path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GSV' ) ) THEN
*
*        SGGSVD
*
        SRNAMT = 'SGGSVD'
        DO I = 2, 9
          IN(I) = 1
        END DO
        DO I = 2, 9
          IF ((I /= 5).AND.(I/=6)) THEN
            INFOT = I
            DO J = 1, 1
              INFO = J + 100*I
              CALL LA_TEST_SGGSVD('U', 'V',  'Q', NMAX, NMAX, NMAX,
     &          DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &          NMAX, Q, NMAX, W, IW, INFO )
              CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SGGSVD('U', 'V', 'N', NMAX, NMAX, NMAX,
     &          DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &          NMAX, Q, NMAX, W, IW, INFO )
              CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SGGSVD('U', 'N', 'Q', NMAX, NMAX, NMAX,
     &          DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &          NMAX, Q, NMAX, W, IW, INFO )
              CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SGGSVD('U', 'N', 'N', NMAX, NMAX, NMAX,
     &          DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &          NMAX, Q, NMAX, W, IW, INFO )
              CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SGGSVD('N', 'V', 'Q', NMAX, NMAX, NMAX,
     &          DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &          NMAX, Q, NMAX, W, IW, INFO )
              CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SGGSVD('N', 'V', 'N', NMAX, NMAX, NMAX,
     &          DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &          NMAX, Q, NMAX, W, IW, INFO )
              CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SGGSVD('N', 'N', 'Q', NMAX, NMAX, NMAX,
     &          DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &          NMAX, Q, NMAX, W, IW, INFO )
              CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SGGSVD('N', 'N', 'N', NMAX, NMAX, NMAX,
     &          DUMMYK, DUMMYL, A, NMAX, B, NMAX, R1, R2, U, NMAX, V,
     &          NMAX, Q, NMAX, W, IW, INFO )
              CALL CHKXER( 'SGGSVD', INFOT, NOUT, LERR, OK )
            END DO
          ENDIF
        END DO
         NT = NT + 11
*
*        SGGSVP
*
         SRNAMT = 'SGGSVP'
         INFOT = 1
         CALL SGGSVP( '/', 'N', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGGSVP( 'N', '/', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGGSVP( 'N', 'N', '/', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SGGSVP( 'N', 'N', 'N', -1, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL SGGSVP( 'N', 'N', 'N', 0, -1, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL SGGSVP( 'N', 'N', 'N', 0, 0, -1, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL SGGSVP( 'N', 'N', 'N', 0, 0, 0, A, 0, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL SGGSVP( 'N', 'N', 'N', 0, 0, 0, A, 1, B, 0, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL SGGSVP( 'U', 'N', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 0, V, 1, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL SGGSVP( 'N', 'V', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 0, Q, 1, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL SGGSVP( 'N', 'N', 'Q', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 0, IW, TAU, W,
     $                INFO )
         CALL CHKXER( 'SGGSVP', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        STGSJA
*
         SRNAMT = 'STGSJA'
         INFOT = 1
         CALL STGSJA( '/', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL STGSJA( 'N', '/', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL STGSJA( 'N', 'N', '/', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL STGSJA( 'N', 'N', 'N', -1, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL STGSJA( 'N', 'N', 'N', 0, -1, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL STGSJA( 'N', 'N', 'N', 0, 0, -1, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL STGSJA( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 0, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL STGSJA( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                0, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL STGSJA( 'U', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 0, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL STGSJA( 'N', 'V', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 0, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL STGSJA( 'N', 'N', 'Q', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 0, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'STGSJA', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*     Test error exits for the GLM path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GLM' ) ) THEN
*
*        SGGGLM
*
        SRNAMT = 'SGGGLM'
        DO I = 2, 5
          IN(I) = 1
        END DO
        DO I = 2, 5
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_SGGGLM( NMAX, NMAX, NMAX, A, NMAX, B, NMAX,
     &        R1, R2, R3, W, LW, INFO)
            CALL CHKXER( 'SGGGLM', INFOT, NOUT, LERR, OK )
          END DO
        END DO
        NT = NT + 8
*
*     Test error exits for the LSE path.
*
      ELSE IF( LSAMEN( 3, PATH, 'LSE' ) ) THEN
*
*        SGGLSE
*
        SRNAMT = 'SGGLSE'
        DO I = 2, 5
          IN(I) = 1
        END DO
        DO I = 2, 5
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_SGGLSE( NMAX, NMAX, NMAX, A, NMAX, B, NMAX,
     &        R1, R2, R3, W, LW, INFO)
            CALL CHKXER( 'SGGLSE', INFOT, NOUT, LERR, OK )
          END DO
        END DO 
        NT = NT + 8
*
*     Test error exits for the GQR path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GQR' ) ) THEN
*
*        SGGQRF
*
         SRNAMT = 'SGGQRF'
         INFOT = 1
         CALL SGGQRF( -1, 0, 0, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'SGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGGQRF( 0, -1, 0, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'SGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGGQRF( 0, 0, -1, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'SGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL SGGQRF( 0, 0, 0, A, 0, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'SGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL SGGQRF( 0, 0, 0, A, 1, R1, B, 0, R2, W, LW, INFO )
         CALL CHKXER( 'SGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL SGGQRF( 1, 1, 2, A, 1, R1, B, 1, R2, W, 1, INFO )
         CALL CHKXER( 'SGGQRF', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        SGGRQF
*
         SRNAMT = 'SGGRQF'
         INFOT = 1
         CALL SGGRQF( -1, 0, 0, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'SGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGGRQF( 0, -1, 0, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'SGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGGRQF( 0, 0, -1, A, 1, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'SGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL SGGRQF( 0, 0, 0, A, 0, R1, B, 1, R2, W, LW, INFO )
         CALL CHKXER( 'SGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL SGGRQF( 0, 0, 0, A, 1, R1, B, 0, R2, W, LW, INFO )
         CALL CHKXER( 'SGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL SGGRQF( 1, 1, 2, A, 1, R1, B, 1, R2, W, 1, INFO )
         CALL CHKXER( 'SGGRQF', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*     Test error exits for the SGS, SGV, SGX, and SXV paths.
*
      ELSE IF( LSAMEN( 3, PATH, 'SGS' ) .OR.
     $         LSAMEN( 3, PATH, 'SGV' ) .OR.
     $         LSAMEN( 3, PATH, 'SGX' ) .OR. LSAMEN( 3, PATH, 'SXV' ) )
     $          THEN
*
*        SGGES
*
      SRNAMT = 'SGGES '
      DO I = 1, 7
        IN(I) = 1
      END DO
      DO I = 1, 7
        INFOT = I
        DO J = 1, 1
          INFO = J + 100*I
          CALL LA_TEST_SGGES( 'N', 'N', 'N', SLCTES, 1, A, 1,
     &      B, 1,
     &      SDIM, R1, R2, R3, Q, 1, U, 1, W, 1, BW, INFO )
          CALL CHKXER( 'SGGES ', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGES( 'N', 'N', 'S', SLCTES, 1, A, 1,
     &      B, 1,
     &      SDIM, R1, R2, R3, Q, 1, U, 1, W, 1, BW, INFO )
          CALL CHKXER( 'SGGES ', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGES( 'N', 'V', 'N', SLCTES, 1, A, 1,
     &      B, 1,
     &      SDIM, R1, R2, R3, Q, 1, U, 1, W, 1, BW, INFO )
          CALL CHKXER( 'SGGES ', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGES( 'N', 'V', 'S', SLCTES, 1, A, 1,
     &      B, 1,
     &      SDIM, R1, R2, R3, Q, 1, U, 1, W, 1, BW, INFO )
          CALL CHKXER( 'SGGES ', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGES( 'V', 'N', 'N', SLCTES, 1, A, 1,
     &      B, 1,
     &      SDIM, R1, R2, R3, Q, 1, U, 1, W, 1, BW, INFO )
          CALL CHKXER( 'SGGES ', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGES( 'V', 'N', 'S', SLCTES, 1, A, 1,
     &      B, 1,
     &      SDIM, R1, R2, R3, Q, 1, U, 1, W, 1, BW, INFO )
          CALL CHKXER( 'SGGES ', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGES( 'V', 'V', 'N', SLCTES, 1, A, 1,
     &      B, 1,
     &      SDIM, R1, R2, R3, Q, 1, U, 1, W, 1, BW, INFO )
          CALL CHKXER( 'SGGES ', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGES( 'V', 'V', 'S', SLCTES, 1, A, 1,
     &      B, 1,
     &      SDIM, R1, R2, R3, Q, 1, U, 1, W, 1, BW, INFO )
          CALL CHKXER( 'SGGES ', INFOT, NOUT, LERR, OK )
        ENDDO
      ENDDO    
      NT = NT + 11
*
*        SGGESX
*
      SRNAMT = 'SGGESX'
      DO I = 1, 7
        IN(I) = 1
      END DO
      DO I = 1, 7
        INFOT = I
        DO J = 1, 1
          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'N', 'N', 'S', SLCTSX, 'N', NMAX, A,
     &      NMAX, B, NMAX, SDIM,
     &      R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &      NMAX, BW, INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'N', 'N', 'N', SLCTSX, 'N', NMAX, A,
     &      NMAX, B, NMAX, SDIM,
     &      R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &      NMAX, BW, INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'N', 'V', 'S', SLCTSX, 'N', NMAX, A,
     &      NMAX, B, NMAX, SDIM,
     &      R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &      NMAX, BW, INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'N', 'V', 'N', SLCTSX, 'N', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'V', 'N', 'S', SLCTSX, 'N', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'V', 'N', 'N', SLCTSX, 'N', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'V', 'V', 'S', SLCTSX, 'N', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'V', 'V', 'N', SLCTSX, 'N', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'N', 'N', 'S', SLCTSX, 'E', NMAX, A,
     &      NMAX, B, NMAX, SDIM,
     &      R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &      NMAX, BW, INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'N', 'N', 'N', SLCTSX, 'E', NMAX, A,
     &      NMAX, B, NMAX, SDIM,
     &      R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &      NMAX, BW, INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'N', 'V', 'S', SLCTSX, 'E', NMAX, A,
     &      NMAX, B, NMAX, SDIM,
     &      R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &      NMAX, BW, INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'N', 'V', 'N', SLCTSX, 'E', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'V', 'N', 'S', SLCTSX, 'E', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'V', 'N', 'N', SLCTSX, 'E', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'V', 'V', 'S', SLCTSX, 'E', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'V', 'V', 'N', SLCTSX, 'E', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'N', 'N', 'S', SLCTSX, 'V', NMAX, A,
     &      NMAX, B, NMAX, SDIM,
     &      R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &      NMAX, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'N', 'N', 'N', SLCTSX, 'V', NMAX, A,
     &      NMAX, B, NMAX, SDIM,
     &      R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &      NMAX, BW, INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'N', 'V', 'S', SLCTSX, 'V', NMAX, A,
     &      NMAX, B, NMAX, SDIM,
     &      R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &      NMAX, BW, INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'N', 'V', 'N', SLCTSX, 'V', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'V', 'N', 'S', SLCTSX, 'V', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'V', 'N', 'N', SLCTSX, 'V', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'V', 'V', 'S', SLCTSX, 'V', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'V', 'V', 'N', SLCTSX, 'V', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'N', 'N', 'S', SLCTSX, 'B', NMAX, A,
     &      NMAX, B, NMAX, SDIM,
     &      R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &      NMAX, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'N', 'N', 'N', SLCTSX, 'B', NMAX, A,
     &      NMAX, B, NMAX, SDIM,
     &      R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &      NMAX, BW, INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'N', 'V', 'S', SLCTSX, 'B', NMAX, A,
     &      NMAX, B, NMAX, SDIM,
     &      R1, R2, R3, Q, NMAX, U, NMAX, RCE, RCV, W, NMAX, IW,
     &      NMAX, BW, INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'N', 'V', 'N', SLCTSX, 'B', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'V', 'N', 'S', SLCTSX, 'B', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'V', 'N', 'N', SLCTSX, 'B', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'V', 'V', 'S', SLCTSX, 'B', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGESX( 'V', 'V', 'N', SLCTSX, 'B', 1, A,
     &      1, B, 1, SDIM,
     &      R1, R2, R3, Q, 1, U, 1, RCE, RCV, W, 1, IW, 1, BW,
     &      INFO )
          CALL CHKXER( 'SGGESX', INFOT, NOUT, LERR, OK )
        ENDDO
      ENDDO  
      
      NT = NT + 13
*
*        SGGEV
*
      SRNAMT = 'SGGEV '
      DO I = 1, 7
        IN(I) = 1
      END DO
      DO I = 1, 7
        INFOT = I
        DO J = 1, 1
          INFO = J + 100*I
          CALL LA_TEST_SGGEV( 'N', 'N', 1, A, 1, B, 1, R1,
     &      R2, R3, Q, 1, U, 1, W, 1, INFO )
          CALL CHKXER( 'SGGEV ', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGEV( 'V', 'N', 1, A, 1, B, 1, R1,
     &      R2, R3, Q, 1, U, 1, W, 1, INFO )
          CALL CHKXER( 'SGGEV ', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGEV( 'N', 'V', 1, A, 1, B, 1,
     &      R1, R2, R3, Q, 1, U, 1, W, 1, INFO )
          CALL CHKXER( 'SGGEV ', INFOT, NOUT, LERR, OK )

          INFO = J + 100*I
          CALL LA_TEST_SGGEV( 'V', 'V', 1, A, 1, B, 1, R1,
     &      R2, R3, Q, 1, U, 1, W, 1, INFO )
          CALL CHKXER( 'SGGEV ', INFOT, NOUT, LERR, OK )
        ENDDO
      ENDDO         
      NT = NT + 10
*
*        SGGEVX
*
      SRNAMT = 'SGGEVX'
      DO I = 1, 16
        IN(I) = 1
      END DO
      DO I = 1, 16
        IF (I/=9 .AND. I/=10 .AND. I/=13 .AND. I/=14) THEN
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'N', 'N', 'N', 'N', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'N', 'N', 'N', 'E', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'N', 'N', 'N', 'V', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'N', 'V', 'N', 'B', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'N', 'N', 'V', 'N', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 1            00*I
            CALL LA_TEST_SGGEVX( 'N', 'V', 'N', 'N', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'N', 'N', 'V', 'E', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'N', 'N', 'V', 'V', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'N', 'N', 'V', 'B', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'N', 'V', 'N', 'E', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'N', 'V', 'N', 'V', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'N', 'V', 'N', 'B', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 1            00*I
            CALL LA_TEST_SGGEVX( 'N', 'V', 'V', 'N', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'N', 'V', 'V', 'E', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'N', 'V', 'V', 'V', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'N', 'V', 'V', 'B', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'P', 'N', 'N', 'N', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'P', 'N', 'N', 'E', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'P', 'N', 'N', 'V', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 1            00*I
            CALL LA_TEST_SGGEVX( 'P', 'N', 'N', 'B', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'P', 'N', 'V', 'N', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'P', 'V', 'N', 'N', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'P', 'N', 'N', 'E', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'P', 'N', 'V', 'V', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'P', 'N', 'N', 'B', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'P', 'V', 'N', 'E', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'P', 'V', 'N', 'V', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'P', 'V', 'N', 'B', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'P', 'V', 'V', 'N', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'P', 'V', 'V', 'E', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'P', 'V', 'V', 'V', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'P', 'V', 'V', 'B', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'S', 'N', 'N', 'N', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )
            
            INFO = J + 1            00*I
            CALL LA_TEST_SGGEVX( 'S', 'N', 'N', 'E', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'S', 'N', 'N', 'V', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'S', 'N', 'N', 'B', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )


            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'S', 'N', 'V', 'N', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'S', 'V', 'N', 'N', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'S', 'N', 'V', 'E', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 1            00*I
            CALL LA_TEST_SGGEVX( 'S', 'N', 'V', 'V', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'S', 'N', 'V', 'B', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'S', 'V', 'N', 'E', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'S', 'V', 'N', 'V', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'S', 'V', 'N', 'B', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'S', 'V', 'V', 'N', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'S', 'V', 'V', 'E', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 1            00*I
            CALL LA_TEST_SGGEVX( 'S', 'V', 'V', 'V', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'S', 'V', 'V', 'B', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'B', 'N', 'N', 'N', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'B', 'N', 'N', 'E', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'B', 'N', 'N', 'V', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'B', 'N', 'N', 'B', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'B', 'N', 'V', 'N', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 1            00*I
            CALL LA_TEST_SGGEVX( 'B', 'V', 'N', 'N', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'B', 'N', 'V', 'E', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'B', 'N', 'V', 'V', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'B', 'N', 'V', 'B', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'B', 'V', 'N', 'E', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'B', 'V', 'N', 'V', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'B', 'V', 'N', 'B', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 1            00*I
            CALL LA_TEST_SGGEVX( 'B', 'V', 'V', 'N', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'B', 'V', 'V', 'E', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'B', 'V', 'V', 'V', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGGEVX( 'B', 'V', 'V', 'B', NMAX, A, NMAX,
     &        B, NMAX, R1, R2, R3, Q, NMAX, U, NMAX, NMAX, NMAX,
     &        LS, RS, ANRM, BNRM, RCE, RCV, W, NMAX,
     &        IW, BW, INFO )
            CALL CHKXER( 'SGGEVX', INFOT, NOUT, LERR, OK )
          ENDDO
        ENDIF
      ENDDO                 

         NT = NT + 12
*
*        STGEXC
*
         SRNAMT = 'STGEXC'
         INFOT = 3
         CALL STGEXC( .TRUE., .TRUE., -1, A, 1, B, 1, Q, 1, Z, 1, IFST,
     $                ILST, W, 1, INFO )
         CALL CHKXER( 'STGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL STGEXC( .TRUE., .TRUE., 1, A, 0, B, 1, Q, 1, Z, 1, IFST,
     $                ILST, W, 1, INFO )
         CALL CHKXER( 'STGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL STGEXC( .TRUE., .TRUE., 1, A, 1, B, 0, Q, 1, Z, 1, IFST,
     $                ILST, W, 1, INFO )
         CALL CHKXER( 'STGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL STGEXC( .FALSE., .TRUE., 1, A, 1, B, 1, Q, 0, Z, 1, IFST,
     $                ILST, W, 1, INFO )
         CALL CHKXER( 'STGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL STGEXC( .TRUE., .TRUE., 1, A, 1, B, 1, Q, 0, Z, 1, IFST,
     $                ILST, W, 1, INFO )
         CALL CHKXER( 'STGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL STGEXC( .TRUE., .FALSE., 1, A, 1, B, 1, Q, 1, Z, 0, IFST,
     $                ILST, W, 1, INFO )
         CALL CHKXER( 'STGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL STGEXC( .TRUE., .TRUE., 1, A, 1, B, 1, Q, 1, Z, 0, IFST,
     $                ILST, W, 1, INFO )
         CALL CHKXER( 'STGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL STGEXC( .TRUE., .TRUE., 1, A, 1, B, 1, Q, 1, Z, 1, IFST,
     $                ILST, W, 1, INFO )
         CALL CHKXER( 'STGEXC', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*        STGSEN
*
         SRNAMT = 'STGSEN'
         INFOT = 1
         CALL STGSEN( -1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, R1, R2,
     $                R3, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'STGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL STGSEN( 1, .TRUE., .TRUE., SEL, -1, A, 1, B, 1, R1, R2,
     $                R3, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'STGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL STGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 0, B, 1, R1, R2, R3,
     $                Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'STGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL STGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 0, R1, R2, R3,
     $                Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'STGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL STGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, R1, R2, R3,
     $                Q, 0, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'STGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL STGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, R1, R2, R3,
     $                Q, 1, Z, 0, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'STGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL STGSEN( 0, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, R1, R2, R3,
     $                Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'STGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL STGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, R1, R2, R3,
     $                Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'STGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL STGSEN( 2, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, R1, R2, R3,
     $                Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'STGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 24
         CALL STGSEN( 0, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, R1, R2, R3,
     $                Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 20, IW, 0,
     $                INFO )
         CALL CHKXER( 'STGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 24
         CALL STGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, R1, R2, R3,
     $                Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 20, IW, 0,
     $                INFO )
         CALL CHKXER( 'STGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 24
         CALL STGSEN( 2, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, R1, R2, R3,
     $                Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 20, IW, 1,
     $                INFO )
         CALL CHKXER( 'STGSEN', INFOT, NOUT, LERR, OK )
         NT = NT + 12
*
*        STGSNA
*
         SRNAMT = 'STGSNA'
         INFOT = 1
         CALL STGSNA( '/', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'STGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL STGSNA( 'B', '/', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'STGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL STGSNA( 'B', 'A', SEL, -1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'STGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL STGSNA( 'B', 'A', SEL, 1, A, 0, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'STGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL STGSNA( 'B', 'A', SEL, 1, A, 1, B, 0, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'STGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL STGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 0, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'STGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL STGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 0, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'STGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL STGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                0, M, W, 1, IW, INFO )
         CALL CHKXER( 'STGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL STGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 0, IW, INFO )
         CALL CHKXER( 'STGSNA', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        STGSYL
*
         SRNAMT = 'STGSYL'
         INFOT = 1
         CALL STGSYL( '/', 0, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'STGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL STGSYL( 'N', -1, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'STGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL STGSYL( 'N', 0, 0, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'STGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL STGSYL( 'N', 0, 1, 0, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'STGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL STGSYL( 'N', 0, 1, 1, A, 0, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'STGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL STGSYL( 'N', 0, 1, 1, A, 1, B, 0, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'STGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL STGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 0, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'STGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL STGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 1, U, 0, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'STGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL STGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 0, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'STGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL STGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 0,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'STGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL STGSYL( 'N', 1, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'STGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL STGSYL( 'N', 2, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'STGSYL', INFOT, NOUT, LERR, OK )
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
*     End of SERRGG
*
      END
