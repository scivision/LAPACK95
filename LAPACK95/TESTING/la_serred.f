      SUBROUTINE SERRED( PATH, NUNIT )
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
*  SERRED tests the error exits for the eigenvalue driver routines for
*  REAL matrices:
*
*  PATH  driver   description
*  ----  ------   -----------
*  SEV   SGEEV    find eigenvalues/eigenvectors for nonsymmetric A
*  SES   SGEES    find eigenvalues/Schur form for nonsymmetric A
*  SVX   SGEEVX   SGEEV + balancing and condition estimation
*  SSX   SGEESX   SGEES + balancing and condition estimation
*  SBD   SGESVD   compute SVD of an M-by-N matrix A
*        SGESDD   compute SVD of an M-by-N matrix A (by divide and
*                 conquer)
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
      INTEGER            NMAX
      REAL               ONE, ZERO
      PARAMETER          ( NMAX = 4, ONE = 1.0E0, ZERO = 0.0E0 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, IHI, ILO, INFO, J, NT, SDIM
      REAL               ABNRM
*     ..
*     .. Local Arrays ..
      LOGICAL            B( NMAX )
      INTEGER            IW( 2*NMAX ), IN(100)
      REAL               A( NMAX, NMAX ), R1( NMAX ), R2( NMAX ),
     $                   S( NMAX ), U( NMAX, NMAX ), VL( NMAX, NMAX ),
     $                   VR( NMAX, NMAX ), VT( NMAX, NMAX ),
     $                   W( 4*NMAX ), WI( NMAX ), WR( NMAX )
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHKXER, LA_TEST_SGEES, LA_TEST_SGEESX,
     &                   LA_TEST_SGEEV, LA_TEST_SGEEVX, LA_TEST_SGESDD,
     &                   LA_TEST_SGESVD
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN, SSLECT
      EXTERNAL           LSAMEN, SSLECT
*     ..
*     .. Arrays in Common ..
      LOGICAL            SELVAL( 20 )
      REAL               SELWI( 20 ), SELWR( 20 )
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*6        SRNAMT
      INTEGER            INFOT, NOUT, SELDIM, SELOPT
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, NOUT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
      COMMON             / SSLCT / SELOPT, SELDIM, SELVAL, SELWR, SELWI
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      WRITE( NOUT, FMT = * )
      C2 = PATH( 2: 3 )
*
*     Initialize A
*
      DO 20 J = 1, NMAX
         DO 10 I = 1, NMAX
            A( I, J ) = ZERO
   10    CONTINUE
   20 CONTINUE
      DO 30 I = 1, NMAX
         A( I, I ) = ONE
   30 CONTINUE
      OK = .TRUE.
      NT = 0
*
      IF( LSAMEN( 2, C2, 'EV' ) ) THEN
*
*        Test SGEEV
*
        SRNAMT = 'SGEEV '
        DO I = 1, 5
          IN(I) = 1
        END DO
        DO I = 1, 5
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_SGEEV('N', 'N', NMAX, A, NMAX, WR, WI, VL,
     &        NMAX, VR, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGEEV ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_SGEEV('N', 'V', NMAX, A, NMAX, WR, WI, VL,
     &        NMAX, VR, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGEEV ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_SGEEV('V', 'N', NMAX, A, NMAX,WR, WI, VL,
     &        NMAX, VR, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGEEV ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_SGEEV('V', 'V', NMAX, A, NMAX, WR, WI, VL,
     &        NMAX, VR, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGEEV ', INFOT, NOUT, LERR, OK )
          END DO
        END DO

*
      else IF( LSAMEN( 2, C2, 'ES' ) ) THEN
*
*        Test SGEES
*
        SRNAMT = 'SGEES '
        DO I = 1, 5
          IN(I) = 1
        END DO
        DO I = 1, 5
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_SGEES('N', 'N', SSLECT, NMAX, A, NMAX,
     &        SDIM, WR, WI, VL, NMAX, W, NMAX, B, INFO )
            CALL CHKXER( 'SGEES ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_SGEES('N', 'V', SSLECT, NMAX, A, NMAX,
     &        SDIM, WR, WI, VL, NMAX, W, NMAX, B, INFO)
            CALL CHKXER( 'SGEES ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_SGEES('V', 'N', SSLECT, NMAX, A, NMAX,
     &        SDIM, WR, WI, VL, NMAX, W, NMAX, B, INFO )
            CALL CHKXER( 'SGEES ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_SGEES('V', 'V', SSLECT, NMAX, A, NMAX,
     &        SDIM, WR, WI, VL, NMAX, W, NMAX, B, INFO )
            CALL CHKXER( 'SGEES ', INFOT, NOUT, LERR, OK )
          END DO
        END DO
         NT = NT + 6
*
      ELSE IF( LSAMEN( 2, C2, 'VX' ) ) THEN
*
*        Test SGEEVX
*
         SRNAMT = 'SGEEVX'
         DO I = 1, 12
           IN(I) = 1
         END DO
         DO I = 1, 12
           IF (I/=7 .AND. I/= 8 .AND. I/=10) THEN
             INFOT = I
             DO J = 1, 1
               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'N', 'N', 'N', 'N', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'N', 'N', 'N', 'E', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'N', 'N', 'N', 'V', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'N', 'N', 'N', 'B', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'N', 'N', 'V', 'N', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'N', 'N', 'V', 'E', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'N', 'N', 'V', 'V', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'N', 'N', 'V', 'B', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'N', 'V', 'N', 'N', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'N', 'V', 'N', 'E', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'N', 'V', 'N', 'V', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'N', 'V', 'N', 'B', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'P', 'N', 'N', 'N', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'P', 'N', 'N', 'E', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'P', 'N', 'N', 'V', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'P', 'N', 'N', 'B', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'P', 'N', 'V', 'N', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'P', 'N', 'V', 'E', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'P', 'N', 'V', 'V', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'P', 'N', 'V', 'B', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'P', 'V', 'N', 'N', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'P', 'V', 'N', 'E', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'P', 'V', 'N', 'V', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'P', 'V', 'N', 'B', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'S', 'N', 'N', 'N', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'S', 'N', 'N', 'E', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'S', 'N', 'N', 'V', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'S', 'N', 'N', 'B', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'S', 'N', 'V', 'N', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'S', 'N', 'V', 'E', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'S', 'N', 'V', 'V', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'S', 'N', 'V', 'B', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'S', 'V', 'N', 'N', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'S', 'V', 'N', 'E', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'S', 'V', 'N', 'V', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'S', 'V', 'N', 'B', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'B', 'N', 'N', 'N', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'B', 'N', 'N', 'E', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'B', 'N', 'N', 'V', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'B', 'N', 'N', 'B', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'B', 'N', 'V', 'N', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'B', 'N', 'V', 'E', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'B', 'N', 'V', 'V', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'B', 'N', 'V', 'B', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'B', 'V', 'N', 'N', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'B', 'V', 'N', 'E', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'B', 'V', 'N', 'V', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SGEEVX( 'B', 'V', 'N', 'B', NMAX, A, NMAX,
     &           WR, WI, VL, NMAX, VR, NMAX,
     &           ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
               CALL CHKXER( 'SGEEVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDIF
         ENDDO       
         NT = NT + 11
*
      ELSE IF( LSAMEN( 2, C2, 'SX' ) ) THEN
*
*        Test SGEESX
*
         SRNAMT = 'SGEESX'
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_SGEESX( 'N', 'N', SSLECT, 'N', NMAX, A, NMAX,
     &         SDIM, WR, WI, VL,
     &         NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
             CALL CHKXER( 'SGEESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SGEESX( 'N', 'N', SSLECT, 'E', NMAX, A, NMAX,
     &         SDIM, WR, WI, VL,
     &         NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
             CALL CHKXER( 'SGEESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SGEESX( 'N', 'N', SSLECT, 'V', NMAX, A, NMAX,
     &         SDIM, WR, WI, VL,
     &         NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
             CALL CHKXER( 'SGEESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SGEESX( 'N', 'N', SSLECT, 'B', NMAX, A, NMAX,
     &         SDIM, WR, WI, VL,
     &         NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
             CALL CHKXER( 'SGEESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SGEESX( 'N', 'S', SSLECT, 'N', NMAX, A, NMAX,
     &         SDIM, WR, WI, VL,
     &         NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
             CALL CHKXER( 'SGEESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SGEESX( 'N', 'S', SSLECT, 'E', NMAX, A, NMAX,
     &         SDIM, WR, WI, VL,
     &         NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
             CALL CHKXER( 'SGEESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SGEESX( 'N', 'S', SSLECT, 'V', NMAX, A, NMAX,
     &         SDIM, WR, WI, VL,
     &         NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
             CALL CHKXER( 'SGEESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SGEESX( 'N', 'S', SSLECT, 'B', NMAX, A, NMAX,
     &         SDIM, WR, WI, VL,
     &         NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
             CALL CHKXER( 'SGEESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SGEESX( 'V', 'N', SSLECT, 'N', NMAX, A, NMAX,
     &         SDIM, WR, WI, VL,
     &         NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
             CALL CHKXER( 'SGEESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SGEESX( 'V', 'N', SSLECT, 'E', NMAX, A, NMAX,
     &         SDIM, WR, WI, VL,
     &         NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
             CALL CHKXER( 'SGEESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SGEESX( 'V', 'N', SSLECT, 'V', NMAX, A, NMAX,
     &         SDIM, WR, WI, VL,
     &         NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
             CALL CHKXER( 'SGEESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SGEESX( 'V', 'N', SSLECT, 'B', NMAX, A, NMAX,
     &         SDIM, WR, WI, VL,
     &         NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
             CALL CHKXER( 'SGEESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SGEESX( 'V', 'S', SSLECT, 'N', NMAX, A, NMAX,
     &         SDIM, WR, WI, VL,
     &         NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
             CALL CHKXER( 'SGEESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SGEESX( 'V', 'S', SSLECT, 'E', NMAX, A, NMAX,
     &         SDIM, WR, WI, VL,
     &         NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
             CALL CHKXER( 'SGEESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SGEESX( 'V', 'S', SSLECT, 'V', NMAX, A, NMAX,
     &         SDIM, WR, WI, VL,
     &         NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
             CALL CHKXER( 'SGEESX', INFOT, NOUT, LERR, OK )

             INFO = J + 100*I
             CALL LA_TEST_SGEESX( 'V', 'S', SSLECT, 'B', NMAX, A, NMAX,
     &         SDIM, WR, WI, VL,
     &         NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
             CALL CHKXER( 'SGEESX', INFOT, NOUT, LERR, OK )
           ENDDO
         ENDDO            
         NT = NT + 7
*
      ELSE IF( LSAMEN( 2, C2, 'BD' ) ) THEN
*
*        Test SGESVD
*
        SRNAMT = 'SGESVD'
        DO I = 2, 6
          IN(I) = 1
        END DO
        DO I = 2, 6
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_SGESVD('A', 'A', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGESVD('A', 'S',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGESVD('A', 'O',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGESVD('A', 'N', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGESVD('S', 'A', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGESVD('S', 'S',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGESVD('S', 'O',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGESVD', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_SGESVD('S', 'N', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGESVD('O', 'A', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGESVD('O', 'S',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGESVD('O', 'O',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGESVD('O', 'N', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGESVD('N', 'A', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGESVD('N', 'S',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGESVD('N', 'O',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_SGESVD('N', 'N', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'SGESVD', INFOT, NOUT, LERR, OK )
          END DO
        END DO
        NT = NT + 8
        IF( OK ) THEN
          WRITE( NOUT, FMT = 9999 )SRNAMT, NT
        ELSE
          WRITE( NOUT, FMT = 9998 )
        END IF
*
*        Test SGESDD
*
        SRNAMT = 'SGESDD'
        DO I = 2, 6
          IN(I) = 1
        END DO
        DO I = 2, 6
          IF (I/=5) THEN
            INFOT = I
            DO J = 1, 1
              INFO = J + 100*I
              CALL LA_TEST_SGESDD( 'N', NMAX, NMAX, A, NMAX, S, U, NMAX,
     &          VT, NMAX, W, NMAX, IW, INFO )
              CALL CHKXER( 'SGESDD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SGESDD( 'A', NMAX, NMAX, A, NMAX, S, U, NMAX,
     &          VT, NMAX, W, NMAX, IW, INFO )
              CALL CHKXER( 'SGESDD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SGESDD( 'S', NMAX, NMAX, A, NMAX, S, U, NMAX,
     &          VT, NMAX, W, NMAX, IW, INFO )
              CALL CHKXER( 'SGESDD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SGESDD( 'O', NMAX, NMAX, A, NMAX, S, U, NMAX,
     &          VT, NMAX, W, NMAX, IW, INFO )
              CALL CHKXER( 'SGESDD', INFOT, NOUT, LERR, OK )

            ENDDO
          ENDIF
        ENDDO       
        
         NT = NT - 2
         IF( OK ) THEN
            WRITE( NOUT, FMT = 9999 )SRNAMT, NT
         ELSE
            WRITE( NOUT, FMT = 9998 )
         END IF
      END IF
*
*     Print a summary line.
*
      IF( .NOT.LSAMEN( 2, C2, 'BD' ) ) THEN
         IF( OK ) THEN
            WRITE( NOUT, FMT = 9999 )SRNAMT, NT
         ELSE
            WRITE( NOUT, FMT = 9998 )
         END IF
      END IF
*
 9999 FORMAT( 1X, A6, ' passed the tests of the error exits (', I3,
     $      ' tests done)' )
 9998 FORMAT( ' *** ', A6, ' failed the tests of the error exits ***' )
      RETURN
*
*     End of SERRED
      END
