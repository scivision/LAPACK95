      SUBROUTINE CERRED( PATH, NUNIT )
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
*  CERRED tests the error exits for the eigenvalue driver routines for
*  REAL matrices:
*
*  PATH  driver   description
*  ----  ------   -----------
*  CEV   CGEEV    find eigenvalues/eigenvectors for nonsymmetric A
*  CES   CGEES    find eigenvalues/Schur form for nonsymmetric A
*  CVX   CGEEVX   CGEEV + balancing and condition estimation
*  CSX   CGEESX   CGEES + balancing and condition estimation
*  CBD   CGESVD   compute SVD of an M-by-N matrix A
*        CGESDD   compute SVD of an M-by-N matrix A(by divide and
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
      INTEGER            NMAX, LW
      PARAMETER          ( NMAX = 4, LW = 5*NMAX )
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E0, ZERO = 0.0E0 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, IHI, ILO, INFO, J, NT, SDIM
      REAL               ABNRM
*     ..
*     .. Local Arrays ..
      LOGICAL            B( NMAX )
      INTEGER            IW( 4*NMAX ), IN(100)
      REAL               R1( NMAX ), R2( NMAX ), RW( LW ), S( NMAX )
      COMPLEX            A( NMAX, NMAX ), U( NMAX, NMAX ),
     $                   VL( NMAX, NMAX ), VR( NMAX, NMAX ),
     $                   VT( NMAX, NMAX ), W( 4*NMAX ), X( NMAX )
*     ..
*     .. External Subroutines ..
      EXTERNAL           LA_TEST_CGEES, LA_TEST_CGEESX, LA_TEST_CGEEV,
     &                   LA_TEST_CGEEVX,
     &                   LA_TEST_CGESDD, LA_TEST_CGESVD, CHKXER
*     ..
*     .. External Functions ..
      LOGICAL            CSLECT, LSAMEN
      EXTERNAL           CSLECT, LSAMEN
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
*        Test CGEEV
*
        SRNAMT = 'CGEEV '
        DO I = 1, 4
          IN(I) = 1
        END DO
        DO I = 1, 4
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_CGEEV('N', 'N', NMAX, A, NMAX, X, VL,
     &        NMAX, VR, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'CGEEV ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_CGEEV('N', 'V', NMAX, A, NMAX, X, VL,
     &        NMAX, VR, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'CGEEV ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_CGEEV('V', 'N', NMAX, A, NMAX, X, VL,
     &        NMAX, VR, NMAX, W, NMAX, RW, INFO )

            CALL CHKXER( 'CGEEV ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_CGEEV('V', 'V', NMAX, A, NMAX, X, VL,
     &        NMAX, VR, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'CGEEV ', INFOT, NOUT, LERR, OK )
          END DO
        END DO
*
      ELSE IF( LSAMEN( 2, C2, 'ES' ) ) THEN
*
*        Test CGEES
*
        SRNAMT = 'CGEES '
        DO I = 1, 4
          IN(I) = 1
        END DO
        DO I = 1, 4
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_CGEES('N', 'N', CSLECT, NMAX, A, NMAX,
     &        SDIM, X, VL, NMAX, W, NMAX, RW, B, INFO )
            CALL CHKXER( 'CGEES ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_CGEES('N', 'V', CSLECT, NMAX, A, NMAX,
     &        SDIM, X, VL, NMAX, W, NMAX, RW, B, INFO)
            CALL CHKXER( 'CGEES ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_CGEES('V', 'N', CSLECT, NMAX, A, NMAX,
     &        SDIM, X, VL, NMAX, W, NMAX, RW,  B, INFO )
            CALL CHKXER( 'CGEES ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_CGEES('V', 'V', CSLECT, NMAX, A, NMAX,
     &        SDIM, X, VL, NMAX, W, NMAX, RW, B, INFO )
            CALL CHKXER( 'CGEES ', INFOT, NOUT, LERR, OK )
          END DO
        ENDDO
       
         NT = NT + 6
*
      ELSE IF( LSAMEN( 2, C2, 'VX' ) ) THEN
*
*        Test CGEEVX
*
        SRNAMT = 'CGEEVX'
        DO I = 1, 11
          IN(I) = 1
        END DO
        DO I = 1, 11
          IF (I/=6 .AND. I/= 7 .AND. I/=9) THEN
            INFOT = I
            DO J = 1, 1
              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'N', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'N', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'N', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'N', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'N', 'V', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'N', 'V', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'N', 'V', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'N', 'V', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'V', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'V', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'V', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'V', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'P', 'N', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 1              00*I
              CALL LA_TEST_CGEEVX( 'P', 'N', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'P', 'N', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'P', 'N', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'P', 'N', 'V', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'P', 'N', 'V', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'P', 'N', 'V', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'P', 'N', 'V', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 1              00*I
              CALL LA_TEST_CGEEVX( 'P', 'V', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'P', 'V', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'P', 'V', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'P', 'V', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )
              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'S', 'N', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'S', 'N', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'S', 'N', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 1              00*I
              CALL LA_TEST_CGEEVX( 'S', 'N', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )
              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'S', 'N', 'V', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'S', 'N', 'V', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'S', 'N', 'V', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'S', 'N', 'V', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'S', 'V', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'S', 'V', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'S', 'V', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 1              00*I
              CALL LA_TEST_CGEEVX( 'S', 'V', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'N', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'N', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'N', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'N', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'N', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'N', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 1              00*I
              CALL LA_TEST_CGEEVX( 'N', 'N', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'N', 'N', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'B', 'N', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'B', 'N', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'B', 'N', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'B', 'N', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'B', 'N', 'V', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'B', 'N', 'V', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'B', 'N', 'V', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'B', 'N', 'V', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )
              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'B', 'V', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'B', 'V', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'B', 'V', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CGEEVX( 'B', 'V', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'CGEEVX', INFOT, NOUT, LERR, OK )
            ENDDO
          ENDIF
        ENDDO
        NT = NT + 10
*
      ELSE IF( LSAMEN( 2, C2, 'SX' ) ) THEN
*
*        Test CGEESX
*
        SRNAMT = 'CGEESX'
        DO I = 1, 3
          IN(I) = 1
        END DO
        DO I = 1, 3
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_CGEESX( 'N', 'N', CSLECT, 'N', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'CGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGEESX( 'N', 'N', CSLECT, 'E', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'CGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGEESX( 'N', 'N', CSLECT, 'V', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'CGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGEESX( 'N', 'N', CSLECT, 'B', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'CGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGEESX( 'N', 'S', CSLECT, 'N', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'CGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGEESX( 'N', 'S', CSLECT, 'E', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'CGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 1            00*I
            CALL LA_TEST_CGEESX( 'N', 'S', CSLECT, 'V', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'CGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGEESX( 'N', 'S', CSLECT, 'B', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'CGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGEESX( 'V', 'N', CSLECT, 'N', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'CGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGEESX( 'V', 'N', CSLECT, 'E', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'CGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGEESX( 'V', 'N', CSLECT, 'V', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'CGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGEESX( 'V', 'N', CSLECT, 'B', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'CGEESX', INFOT, NOUT, LERR, OK )

          ENDDO
        ENDDO
        NT = NT + 7
*
      ELSE IF( LSAMEN( 2, C2, 'BD' ) ) THEN
*
*        Test CGESVD
*
        SRNAMT = 'CGESVD'
        DO I = 2, 6
          IN(I) = 1
        END DO
        DO I = 2, 6
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_CGESVD('A', 'A', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'CGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGESVD('A', 'S',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'CGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGESVD('A', 'O',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW,  INFO )
            CALL CHKXER( 'CGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGESVD('A', 'N', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW,  INFO )
            CALL CHKXER( 'CGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGESVD('S', 'A', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW,  INFO )
            CALL CHKXER( 'CGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGESVD('S', 'S',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'CGESVD', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_CGESVD('S', 'O',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'CGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGESVD('S', 'N', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW,  INFO )
            CALL CHKXER( 'CGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGESVD('O', 'A', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'CGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGESVD('O', 'S',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'CGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGESVD('O', 'O',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'CGESVD', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_CGESVD('O', 'N', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'CGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGESVD('N', 'A', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'CGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGESVD('N', 'S',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'CGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGESVD('N', 'O',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'CGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_CGESVD('N', 'N', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'CGESVD', INFOT, NOUT, LERR, OK )
          END DO
        END DO
        
        NT = NT + 8
         IF( OK ) THEN
            WRITE( NOUT, FMT = 9999 )SRNAMT, NT
         ELSE
            WRITE( NOUT, FMT = 9998 )
         END IF
*
*        Test CGESDD
*
         SRNAMT = 'CGESDD'
         DO I = 2, 6
           IN(I) = 1
         END DO
         DO I = 2, 6
           IF (I/=5) THEN
             INFOT = I
             DO J = 1, 1
               INFO = J + 100*I
               CALL LA_TEST_CGESDD( 'N', NMAX, NMAX, A, NMAX, S, U,
     &           NMAX, VT, NMAX, W, NMAX, RW, IW, INFO )
               CALL CHKXER( 'CGESDD', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CGESDD( 'A', NMAX, NMAX, A, NMAX, S, U,
     &           NMAX, VT, NMAX, W, NMAX, RW, IW, INFO )
               CALL CHKXER( 'CGESDD', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CGESDD( 'S', NMAX, NMAX, A, NMAX, S, U,
     &           NMAX, VT, NMAX, W, NMAX, RW, IW, INFO )
               CALL CHKXER( 'CGESDD', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CGESDD( 'O', NMAX, NMAX, A, NMAX, S, U,
     &           NMAX, VT, NMAX, W, NMAX, RW, IW, INFO )
               CALL CHKXER( 'CGESDD', INFOT, NOUT, LERR, OK )
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
*     End of CERRED
*
      END
