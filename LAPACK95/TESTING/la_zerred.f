      SUBROUTINE ZERRED( PATH, NUNIT )
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
*  ZERRED tests the error exits for the eigenvalue driver routines for
*  DOUBLE PRECISION matrices:
*
*  PATH  driver   description
*  ----  ------   -----------
*  ZEV   ZGEEV    find eigenvalues/eigenvectors for nonsymmetric A
*  ZES   ZGEES    find eigenvalues/Schur form for nonsymmetric A
*  ZVX   ZGEEVX   ZGEEV + balancing and condition estimation
*  ZSX   ZGEESX   ZGEES + balancing and condition estimation
*  ZBD   ZGESVD   compute SVD of an M-by-N matrix A
*        ZGESDD   compute SVD of an M-by-N matrix A(by divide and
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
      PARAMETER          ( NMAX = 4, LW = 5*NMAX)
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, IHI, ILO, INFO, J, NT, SDIM
      DOUBLE PRECISION   ABNRM
*     ..
*     .. Local Arrays ..
      LOGICAL            B( NMAX )
      INTEGER            IW( 4*NMAX ), IN(100)
      DOUBLE PRECISION   R1( NMAX ), R2( NMAX ), RW( LW), S( NMAX )
      COMPLEX*16         A( NMAX, NMAX ), U( NMAX, NMAX ),
     $                   VL( NMAX, NMAX ), VR( NMAX, NMAX ),
     $                   VT( NMAX, NMAX ), W( 4*NMAX ), X( NMAX )
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHKXER, LA_TEST_ZGEES, LA_TEST_ZGEESX,
     &                   LA_TEST_ZGEEV,
     &                   LA_TEST_ZGEEVX, LA_TEST_ZGESDD, LA_TEST_ZGESVD
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN, ZSLECT
      EXTERNAL           LSAMEN, ZSLECT
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
*        Test ZGEEV
*
        SRNAMT = 'ZGEEV '
        DO I = 1, 4
          IN(I) = 1
        END DO
        DO I = 1, 4
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_ZGEEV('N', 'N', NMAX, A, NMAX, X, VL,
     &        NMAX, VR, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'ZGEEV ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_ZGEEV('N', 'V', NMAX, A, NMAX, X, VL,
     &        NMAX, VR, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'ZGEEV ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_ZGEEV('V', 'N', NMAX, A, NMAX, X, VL,
     &        NMAX, VR, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'ZGEEV ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_ZGEEV('V', 'V', NMAX, A, NMAX, X, VL,
     &        NMAX, VR, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'ZGEEV ', INFOT, NOUT, LERR, OK )
          END DO
        END DO 
*
      ELSE IF( LSAMEN( 2, C2, 'ES' ) ) THEN
*
*        Test ZGEES
*
        SRNAMT = 'ZGEES '
        DO I = 1, 4
          IN(I) = 1
        END DO
        DO I = 1, 4
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_ZGEES('N', 'N', ZSLECT, NMAX, A, NMAX,
     &        SDIM, X, VL, NMAX, W, NMAX, RW, B, INFO )
            CALL CHKXER( 'ZGEES ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_ZGEES('N', 'V', ZSLECT, NMAX, A, NMAX,
     &        SDIM, X, VL, NMAX, W, NMAX, RW, B, INFO)
            CALL CHKXER( 'ZGEES ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_ZGEES('V', 'N', ZSLECT, NMAX, A, NMAX,
     &        SDIM, X, VL, NMAX, W, NMAX, RW,  B, INFO )
            CALL CHKXER( 'ZGEES ', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_ZGEES('V', 'V', ZSLECT, NMAX, A, NMAX,
     &        SDIM, X, VL, NMAX, W, NMAX, RW, B, INFO )
            CALL CHKXER( 'ZGEES ', INFOT, NOUT, LERR, OK )
          END DO
        END DO
        NT = NT + 6
*
      ELSE IF( LSAMEN( 2, C2, 'VX' ) ) THEN
*
*        Test ZGEEVX
*
        SRNAMT = 'ZGEEVX'
        DO I = 1, 11
          IN(I) = 1
        END DO
        DO I = 1, 11
          IF (I/=6 .AND. I/= 7 .AND. I/=9) THEN
            INFOT = I
            DO J = 1, 1
              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'N', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'N', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'N', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'N', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'N', 'V', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'N', 'V', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'N', 'V', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'N', 'V', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'V', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'V', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'V', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'V', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'P', 'N', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'P', 'N', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'P', 'N', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'P', 'N', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )
!!!

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'P', 'N', 'V', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'P', 'N', 'V', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'P', 'N', 'V', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'P', 'N', 'V', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'P', 'V', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'P', 'V', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'P', 'V', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'P', 'V', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )
              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'S', 'N', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'S', 'N', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'S', 'N', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'S', 'N', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )
              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'S', 'N', 'V', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'S', 'N', 'V', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'S', 'N', 'V', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'S', 'N', 'V', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'S', 'V', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'S', 'V', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'S', 'V', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'S', 'V', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )
              
              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'N', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'N', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'N', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'N', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'N', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'N', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'N', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'N', 'N', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'B', 'N', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'B', 'N', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'B', 'N', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'B', 'N', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'B', 'N', 'V', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'B', 'N', 'V', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'B', 'N', 'V', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'B', 'N', 'V', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )
              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'B', 'V', 'N', 'N', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'B', 'V', 'N', 'E', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'B', 'V', 'N', 'V', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I  
              CALL LA_TEST_ZGEEVX( 'B', 'V', 'N', 'B', NMAX, A, NMAX,
     &          X, VL, NMAX, VR, NMAX, ILO,
     &          IHI, S, ABNRM, R1, R2, W, NMAX, RW, INFO )
              CALL CHKXER( 'ZGEEVX', INFOT, NOUT, LERR, OK )
            ENDDO
          ENDIF
        ENDDO
        
         NT = NT + 10
*
      ELSE IF( LSAMEN( 2, C2, 'SX' ) ) THEN
*
*        Test ZGEESX
*
        SRNAMT = 'ZGEESX'
        DO I = 1, 3
          IN(I) = 1
        END DO
        DO I = 1, 3
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_ZGEESX( 'N', 'N', ZSLECT, 'N', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'ZGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGEESX( 'N', 'N', ZSLECT, 'E', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'ZGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGEESX( 'N', 'N', ZSLECT, 'V', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'ZGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGEESX( 'N', 'N', ZSLECT, 'B', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'ZGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGEESX( 'N', 'S', ZSLECT, 'N', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'ZGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGEESX( 'N', 'S', ZSLECT, 'E', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'ZGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGEESX( 'N', 'S', ZSLECT, 'V', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'ZGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGEESX( 'N', 'S', ZSLECT, 'B', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'ZGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGEESX( 'V', 'N', ZSLECT, 'N', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'ZGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGEESX( 'V', 'N', ZSLECT, 'E', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'ZGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGEESX( 'V', 'N', ZSLECT, 'V', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'ZGEESX', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGEESX( 'V', 'N', ZSLECT, 'B', NMAX, A,
     &        NMAX, SDIM, X, VL, NMAX,
     &        R1( 1 ), R2( 1 ), W, NMAX, RW, B, INFO )
            CALL CHKXER( 'ZGEESX', INFOT, NOUT, LERR, OK )

          ENDDO
        ENDDO
        NT = NT + 7
*
      ELSE IF( LSAMEN( 2, C2, 'BD' ) ) THEN
*
*        Test ZGESVD
*
        SRNAMT = 'ZGESVD'
        DO I = 2, 6
          IN(I) = 1
        END DO
        DO I = 2, 6
          INFOT = I
          DO J = 1, 1
            INFO = J + 100*I
            CALL LA_TEST_ZGESVD('A', 'A', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'ZGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGESVD('A', 'S',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'ZGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGESVD('A', 'O',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW,  INFO )
            CALL CHKXER( 'ZGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGESVD('A', 'N', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW,  INFO )
            CALL CHKXER( 'ZGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGESVD('S', 'A', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW,  INFO )
            CALL CHKXER( 'ZGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGESVD('S', 'S',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'ZGESVD', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_ZGESVD('S', 'O',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'ZGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGESVD('S', 'N', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW,  INFO )
            CALL CHKXER( 'ZGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGESVD('O', 'A', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'ZGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGESVD('O', 'S',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'ZGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGESVD('O', 'O',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'ZGESVD', INFOT, NOUT, LERR, OK )
            INFO = J + 100*I
            CALL LA_TEST_ZGESVD('O', 'N', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'ZGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGESVD('N', 'A', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'ZGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGESVD('N', 'S',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'ZGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGESVD('N', 'O',NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'ZGESVD', INFOT, NOUT, LERR, OK )

            INFO = J + 100*I
            CALL LA_TEST_ZGESVD('N', 'N', NMAX, NMAX, A, NMAX, S, U,
     &        NMAX, VT, NMAX, W, NMAX, RW, INFO )
            CALL CHKXER( 'ZGESVD', INFOT, NOUT, LERR, OK )
          END DO
        END DO                                               
        NT = NT + 8
        IF( OK ) THEN
          WRITE( NOUT, FMT = 9999 )SRNAMT, NT
        ELSE
          WRITE( NOUT, FMT = 9998 )
        END IF
*
*        Test ZGESDD
*
        SRNAMT = 'ZGESDD'
        DO I = 2, 6
          IN(I) = 1
        END DO
        DO I = 2, 6
          IF (I/=5) THEN
            INFOT = I
            DO J = 1, 1
              INFO = J + 100*I
              CALL LA_TEST_ZGESDD( 'N', NMAX, NMAX, A, NMAX, S, U,
     &          NMAX, VT, NMAX, W, NMAX, RW, IW, INFO )
              CALL CHKXER( 'ZGESDD', INFOT, NOUT, LERR, OK ) 

              INFO = J + 100*I
              CALL LA_TEST_ZGESDD( 'A', NMAX, NMAX, A, NMAX, S, U,
     &          NMAX, VT, NMAX, W, NMAX, RW, IW, INFO )
              CALL CHKXER( 'ZGESDD', INFOT, NOUT, LERR, OK ) 

              INFO = J + 100*I
              CALL LA_TEST_ZGESDD( 'S', NMAX, NMAX, A, NMAX, S, U,
     &          NMAX, VT, NMAX, W, NMAX, RW, IW, INFO )
              CALL CHKXER( 'ZGESDD', INFOT, NOUT, LERR, OK ) 

              INFO = J + 100*I
              CALL LA_TEST_ZGESDD( 'O', NMAX, NMAX, A, NMAX, S, U,
     &          NMAX, VT, NMAX, W, NMAX, RW, IW, INFO )
              CALL CHKXER( 'ZGESDD', INFOT, NOUT, LERR, OK ) 
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
*     End of ZERRED
*
      END
