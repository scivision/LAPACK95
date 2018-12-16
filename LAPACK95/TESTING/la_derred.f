      SUBROUTINE DERRED( PATH, NUNIT )
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
*  DERRED tests the error exits for the eigenvalue driver routines for
*  DOUBLE PRECISION matrices:
*
*  PATH  driver   description
*  ----  ------   -----------
*  SEV   DGEEV    find eigenvalues/eigenvectors for nonsymmetric A
*  SES   DGEES    find eigenvalues/Schur form for nonsymmetric A
*  SVX   DGEEVX   SGEEV + balancing and condition estimation
*  SSX   DGEESX   SGEES + balancing and condition estimation
*  DBD   DGESVD   compute SVD of an M-by-N matrix A
*        DGESDD   compute SVD of an M-by-N matrix A (by divide and
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
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( NMAX = 4, ONE = 1.0D0, ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, IHI, ILO, INFO, J, NT, SDIM
      DOUBLE PRECISION   ABNRM
*     ..
*     .. Local Arrays ..
      LOGICAL            B( NMAX )
      INTEGER            IW( 2*NMAX ), IN(100)
      DOUBLE PRECISION   A( NMAX, NMAX ), R1( NMAX ), R2( NMAX ),
     $                   S( NMAX ), U( NMAX, NMAX ), VL( NMAX, NMAX ),
     $                   VR( NMAX, NMAX ), VT( NMAX, NMAX ),
     $                   W( 4*NMAX ), WI( NMAX ), WR( NMAX )
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHKXER, LA_TEST_DGEES, LA_TEST_DGEESX,
     &                   LA_TEST_DGEEV,
     &                   LA_TEST_DGEEVX,
     &                   DGESDD, LA_TEST_DGESVD
*     ..
*     .. External Functions ..
      LOGICAL            DSLECT, LSAMEN
      EXTERNAL           DSLECT, LSAMEN
*     ..
*     .. Arrays in Common ..
      LOGICAL            SELVAL( 20 )
      DOUBLE PRECISION   SELWI( 20 ), SELWR( 20 )
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
*        Test DGEEV
*
        SRNAMT = 'DGEEV '
        DO I = 1, 5
          IN(I) = 1
        END DO
        DO I = 1, 5
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_DGEEV('N', 'N', NMAX, A, NMAX, WR, WI, VL,
     &        NMAX, VR, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'DGEEV ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_DGEEV('N', 'V', NMAX, A, NMAX, WR, WI, VL,
     &        NMAX, VR, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'DGEEV ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_DGEEV('V', 'N', NMAX, A, NMAX,WR, WI, VL,
     &        NMAX, VR, NMAX, W, NMAX, INFO )

            CALL CHKXER( 'DGEEV ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_DGEEV('V', 'V', NMAX, A, NMAX, WR, WI, VL,
     &        NMAX, VR, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'DGEEV ', INFOT, NOUT, LERR, OK )
          END DO
        END DO
         NT = NT + 7
*
      ELSE IF( LSAMEN( 2, C2, 'ES' ) ) THEN
*
*        Test DGEES
*
         SRNAMT = 'DGEES '
         DO I = 1, 5
           IN(I) = 1
         END DO
         DO I = 1, 5
           INFOT = I
           DO J = 1, 1
             INFO = J + 100*I
             CALL LA_TEST_DGEES('N', 'N', DSLECT, NMAX, A, NMAX,
     &         SDIM, WR, WI, VL, NMAX, W, NMAX, B, INFO )
             CALL CHKXER( 'DGEES ', INFOT, NOUT, LERR, OK )
             INFO = J + 100*I
             CALL LA_TEST_DGEES('N', 'V', DSLECT, NMAX, A, NMAX,
     &         SDIM, WR, WI, VL, NMAX, W, NMAX, B, INFO)
             CALL CHKXER( 'DGEES ', INFOT, NOUT, LERR, OK )
             INFO = J + 100*I
             CALL LA_TEST_DGEES('V', 'N', DSLECT, NMAX, A, NMAX,
     &         SDIM, WR, WI, VL, NMAX, W, NMAX, B, INFO )
             CALL CHKXER( 'DGEES ', INFOT, NOUT, LERR, OK )
             INFO = J + 100*I
             CALL LA_TEST_DGEES('V', 'V', DSLECT, NMAX, A, NMAX,
     &         SDIM, WR, WI, VL, NMAX, W, NMAX, B, INFO )
             CALL CHKXER( 'DGEES ', INFOT, NOUT, LERR, OK )
           END DO
         END DO
         NT = NT + 6
*
      ELSE IF( LSAMEN( 2, C2, 'VX' ) ) THEN
*
*        Test DGEEVX
*
        SRNAMT = 'DGEEVX'
        DO I = 1, 12
          IN(I) = 1
        END DO
        DO I = 1, 12
          IF (I/=7 .AND. I/= 8 .AND. I/=10) THEN
            INFOT = I
            DO J = 1, 1
              INFO = J + 100*I
              CALL LA_TEST_DGEEVX( 'N', 'N', 'N', 'N', NMAX, A, NMAX,
     &          WR, WI, VL, NMAX, VR, NMAX,
     &          ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
              CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )
              
              INFO = J + 100*I
              CALL LA_TEST_DGEEVX( 'N', 'N', 'N', 'E', NMAX, A, NMAX,
     &          WR, WI, VL, NMAX, VR, NMAX,
     &          ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
              CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )
              
              INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'N', 'N', 'N', 'V', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'N', 'N', 'N', 'B', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'N', 'N', 'V', 'N', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'N', 'N', 'V', 'E', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'N', 'N', 'V', 'V', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'N', 'N', 'V', 'B', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'N', 'V', 'N', 'N', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'N', 'V', 'N', 'E', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'N', 'V', 'N', 'V', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'N', 'V', 'N', 'B', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'P', 'N', 'N', 'N', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'P', 'N', 'N', 'E', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'P', 'N', 'N', 'V', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'P', 'N', 'N', 'B', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'P', 'N', 'V', 'N', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'P', 'N', 'V', 'E', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'P', 'N', 'V', 'V', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'P', 'N', 'V', 'B', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'P', 'V', 'N', 'N', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'P', 'V', 'N', 'E', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'P', 'V', 'N', 'V', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'P', 'V', 'N', 'B', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )
            
            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'S', 'N', 'N', 'N', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'S', 'N', 'N', 'E', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'S', 'N', 'N', 'V', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'S', 'N', 'N', 'B', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'S', 'N', 'V', 'N', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'S', 'N', 'V', 'E', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'S', 'N', 'V', 'V', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'S', 'N', 'V', 'B', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'S', 'V', 'N', 'N', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'S', 'V', 'N', 'E', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'S', 'V', 'N', 'V', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'S', 'V', 'N', 'B', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )
            
            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'B', 'N', 'N', 'N', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'B', 'N', 'N', 'E', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'B', 'N', 'N', 'V', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'B', 'N', 'N', 'B', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'B', 'N', 'V', 'N', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'B', 'N', 'V', 'E', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'B', 'N', 'V', 'V', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'B', 'N', 'V', 'B', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'B', 'V', 'N', 'N', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'B', 'V', 'N', 'E', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'B', 'V', 'N', 'V', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGEEVX( 'B', 'V', 'N', 'B', NMAX, A, NMAX,
     &        WR, WI, VL, NMAX, VR, NMAX,
     &        ILO, IHI, S, ABNRM, R1, R2, W, NMAX, IW, INFO )
            CALL CHKXER( 'DGEEVX', INFOT, NOUT, LERR, OK )
          ENDDO
        ENDIF
      ENDDO
         NT = NT + 11
*
      ELSE IF( LSAMEN( 2, C2, 'SX' ) ) THEN
*
*        Test DGEESX
*
        SRNAMT = 'DGEESX'
        DO I = 1, 4
          IN(I) = 1
        END DO
        DO I = 1, 4
            INFOT = I
            DO J = 1, 1
              INFO = J + 100*I
              CALL LA_TEST_DGEESX( 'N', 'N', DSLECT, 'N', NMAX, A, NMAX,
     &          SDIM, WR, WI, VL,
     &          NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
              CALL CHKXER( 'DGEESX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_DGEESX( 'N', 'N', DSLECT, 'E', NMAX, A, NMAX,
     &          SDIM, WR, WI, VL,
     &          NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
              CALL CHKXER( 'DGEESX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_DGEESX( 'N', 'N', DSLECT, 'V', NMAX, A, NMAX,
     &          SDIM, WR, WI, VL,
     &          NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
              CALL CHKXER( 'DGEESX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_DGEESX( 'N', 'N', DSLECT, 'B', NMAX, A, NMAX,
     &          SDIM, WR, WI, VL,
     &          NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
              CALL CHKXER( 'DGEESX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_DGEESX( 'N', 'S', DSLECT, 'N', NMAX, A, NMAX,
     &          SDIM, WR, WI, VL,
     &          NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
              CALL CHKXER( 'DGEESX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_DGEESX( 'N', 'S', DSLECT, 'E', NMAX, A, NMAX,
     &          SDIM, WR, WI, VL,
     &          NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
              CALL CHKXER( 'DGEESX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_DGEESX( 'N', 'S', DSLECT, 'V', NMAX, A, NMAX,
     &          SDIM, WR, WI, VL,
     &          NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
              CALL CHKXER( 'DGEESX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_DGEESX( 'N', 'S', DSLECT, 'B', NMAX, A, NMAX,
     &          SDIM, WR, WI, VL,
     &          NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
              CALL CHKXER( 'DGEESX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_DGEESX( 'V', 'N', DSLECT, 'N', NMAX, A, NMAX,
     &          SDIM, WR, WI, VL,
     &          NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
              CALL CHKXER( 'DGEESX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_DGEESX( 'V', 'N', DSLECT, 'E', NMAX, A, NMAX,
     &          SDIM, WR, WI, VL,
     &          NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
              CALL CHKXER( 'DGEESX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_DGEESX( 'V', 'N', DSLECT, 'V', NMAX, A, NMAX,
     &          SDIM, WR, WI, VL,
     &          NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
              CALL CHKXER( 'DGEESX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_DGEESX( 'V', 'N', DSLECT, 'B', NMAX, A, NMAX,
     &          SDIM, WR, WI, VL,
     &          NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
              CALL CHKXER( 'DGEESX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_DGEESX( 'V', 'S', DSLECT, 'N', NMAX, A, NMAX,
     &          SDIM, WR, WI, VL,
     &          NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
              CALL CHKXER( 'DGEESX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_DGEESX( 'V', 'S', DSLECT, 'E', NMAX, A, NMAX,
     &          SDIM, WR, WI, VL,
     &          NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
              CALL CHKXER( 'DGEESX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_DGEESX( 'V', 'S', DSLECT, 'V', NMAX, A, NMAX,
     &          SDIM, WR, WI, VL,
     &          NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
              CALL CHKXER( 'DGEESX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_DGEESX( 'V', 'S', DSLECT, 'B', NMAX, A, NMAX,
     &          SDIM, WR, WI, VL,
     &          NMAX, R1( 1 ), R2( 1 ), W, NMAX, IW, NMAX, B, INFO )
              CALL CHKXER( 'DGEESX', INFOT, NOUT, LERR, OK )
            ENDDO
          ENDDO
         NT = NT + 7
*
      ELSE IF( LSAMEN( 2, C2, 'BD' ) ) THEN
*
*        Test DGESVD
*
        SRNAMT = 'DGESVD'
        DO I = 2, 6
          IN(I) = 1
        END DO
        DO I = 2, 6
          INFOT = I
          DO J = 1, 1
           INFO = J + 100*I
            CALL LA_TEST_DGESVD('A', 'A', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'DGESVD', INFOT, NOUT, LERR, OK )
            
            INFO = J + 100*I
            CALL LA_TEST_DGESVD('A', 'S',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
           CALL CHKXER( 'DGESVD', INFOT, NOUT, LERR, OK )
            
            INFO = J + 100*I
            CALL LA_TEST_DGESVD('A', 'O',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'DGESVD', INFOT, NOUT, LERR, OK )
            
            INFO = J + 100*I
            CALL LA_TEST_DGESVD('A', 'N', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'DGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGESVD('S', 'A', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'DGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGESVD('S', 'S',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'DGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGESVD('S', 'O',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'DGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGESVD('S', 'N', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'DGESVD', INFOT, NOUT, LERR, OK )
            
            INFO = J + 100*I
            CALL LA_TEST_DGESVD('O', 'A', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'DGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGESVD('O', 'S',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'DGESVD', INFOT, NOUT, LERR, OK )

           INFO = J + 100*I
            CALL LA_TEST_DGESVD('O', 'O',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'DGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGESVD('O', 'N', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'DGESVD', INFOT, NOUT, LERR, OK )
            
            INFO = J + 100*I
            CALL LA_TEST_DGESVD('N', 'A', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'DGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGESVD('N', 'S',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'DGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_DGESVD('N', 'O',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'DGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            
            CALL LA_TEST_DGESVD('N', 'N', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, INFO )
            CALL CHKXER( 'DGESVD', INFOT, NOUT, LERR, OK )

          END DO
        END DO 
        NT = NT + 8
        IF( OK ) THEN
          WRITE( NOUT, FMT = 9999 )SRNAMT, NT
        ELSE
          WRITE( NOUT, FMT = 9998 )
        END IF
*
*        Test DGESDD
*
        SRNAMT = 'DGESDD'
        DO I = 2, 6
          IN(I) = 1
        END DO
        DO I = 2, 6
          IF (I/=5) THEN
            INFOT = I
            DO J = 1, 1
              INFO = J + 100*I
              CALL LA_TEST_DGESDD( 'N', NMAX, NMAX, A, NMAX, S, U, NMAX,
     &          VT, NMAX, W, NMAX, IW, INFO )
              CALL CHKXER( 'DGESDD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_DGESDD( 'A', NMAX, NMAX, A, NMAX, S, U, NMAX,
     &          VT, NMAX, W, NMAX, IW, INFO )
              CALL CHKXER( 'DGESDD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_DGESDD( 'S', NMAX, NMAX, A, NMAX, S, U, NMAX,
     &          VT, NMAX, W, NMAX, IW, INFO )
              CALL CHKXER( 'DGESDD', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_DGESDD( 'O', NMAX, NMAX, A, NMAX, S, U, NMAX,
     &          VT, NMAX, W, NMAX, IW, INFO )
              CALL CHKXER( 'DGESDD', INFOT, NOUT, LERR, OK )

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
*     End of DERRED
      END
