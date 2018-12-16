      SUBROUTINE CERRGE( PATH, NUNIT )
*
*  -- LAPACK test routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER*3        PATH
      INTEGER            NUNIT
*     ..
*
*  Purpose
*  =======
*
*  CERRGE tests the error exits for the COMPLEX routines
*  for general matrices.
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
      PARAMETER          ( NMAX = 4 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, INFO, J, NRHS, LW
      REAL               ANRM, CCOND, RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX ), IN(100)
      REAL               R( NMAX ), R1( NMAX ), R2( NMAX ), IW(NMAX)
      COMPLEX            A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL ALAESM, CGBCON, CGBEQU, CGBRFS, CGBTF2, CGBTRF, CGBTRS,
     $         LA_TEST_CGECON, LA_TEST_CGEEQU, LA_TEST_CGERFS, CGETF2,
     $         LA_TEST_CGETRF, LA_TEST_CGETRI, LA_TEST_CGETRS, CHKXER
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
      INTRINSIC          CMPLX, REAL
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
            A( I, J ) = CMPLX( 1. / REAL( I+J ), -1. / REAL( I+J ) )
            AF( I, J ) = CMPLX( 1. / REAL( I+J ), -1. / REAL( I+J ) )
   10    CONTINUE
         B( J ) = 0.
         R1( J ) = 0.
         R2( J ) = 0.
         W( J ) = 0.
         X( J ) = 0.
         IP( J ) = J
   20 CONTINUE
      OK = .TRUE.
*
*     Test error exits of the routines that use the LU decomposition
*     of a general matrix.
*
      IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        CGETRF
*
         SRNAMT = 'CGETRF'
         INFOT = 2
         INFO = 201
         CALL LA_TEST_CGETRF( NMAX, NMAX, A, NMAX, IP, INFO )
         CALL CHKXER( 'SGETRF', INFOT, NOUT, LERR, OK )
*
*        CGETF2
*
         SRNAMT = 'CGETF2'
         INFOT = 1
         CALL CGETF2( -1, 0, A, 1, IP, INFO )
         CALL CHKXER( 'CGETF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGETF2( 0, -1, A, 1, IP, INFO )
         CALL CHKXER( 'CGETF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGETF2( 2, 1, A, 1, IP, INFO )
         CALL CHKXER( 'CGETF2', INFOT, NOUT, LERR, OK )
*
*        CGETRI
*
         SRNAMT = 'CGETRI'
         DO I = 1, 2
            IN(I) = 1
         END DO
         DO I = 1, 2
            INFOT = I
            DO J = 1, IN(I)
               INFO = J + 100*I
               CALL LA_TEST_CGETRI( NMAX, A, NMAX, IP, W, LW, INFO )
               CALL CHKXER( 'CGETRI', INFOT, NOUT, LERR, OK )
            END DO
         END DO
*
*        CGETRS
*
         SRNAMT = 'CGETRS'
         DO I = 1, 4
            IN(I) = 1
         END DO
         DO I = 1, 4
            INFOT = I
            DO J = 1, IN(I)
               DO NRHS = 1, 2
                  INFO = J + 100*I
                  CALL LA_TEST_CGETRS( 'N', NMAX, NRHS, A, NMAX,
     $                 IP, B, NMAX, INFO )
                  CALL CHKXER( 'CGETRS', INFOT, NOUT, LERR, OK )
               END DO
            END DO
         END DO
*
*        CGERFS
*
         SRNAMT = 'CGERFS'
         DO I = 1, 8
            IN(I) = 1
         END DO
         IN(2) = 2
         IN(5) = 2
         DO I = 1, 8
            INFOT = I
            DO J = 1, IN(I)
               DO NRHS = 1, 2
                  INFO = J + 100*I
                  CALL LA_TEST_CGERFS( 'N', NMAX, NRHS, A, NMAX, AF,
     $                 NMAX, IP, B, NMAX, X, NMAX, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'CGERFS', INFOT, NOUT, LERR, OK )
               END DO
            END DO
         END DO
*
*        CGECON
*
         SRNAMT = 'CGECON'
         DO I = 1, 4
            IN(I) = 1
         END DO
         IN(4) = 2
         DO I = 1, 4
            IF( I.NE.3 ) THEN
               INFOT = I
               DO J = 1, IN(I)
                  INFO = J + 100*I
                  CALL LA_TEST_CGECON( 'I', NMAX, A, NMAX, ANRM, RCOND,
     $                 W, IW, INFO )
                  CALL CHKXER( 'CGECON ', INFOT, NOUT, LERR, OK )
               END DO
            END IF
         END DO
*
*        CGEEQU
*
         SRNAMT = 'CGEEQU'
         DO I = 2, 3
            IN(I) = 1
         END DO
         DO I = 2, 3
            INFOT = I
            DO J = 1, IN(I)
               INFO = J + 100*I
               CALL LA_TEST_CGEEQU( NMAX, NMAX+1, A, NMAX, R1, R2,
     $              RCOND, CCOND, ANRM, INFO )
               CALL CHKXER( 'CGEEQU', INFOT, NOUT, LERR, OK )
            END DO
         END DO
*
*     Test error exits of the routines that use the LU decomposition
*     of a general band matrix.
*
      ELSE IF( LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        CGBTRF
*
         SRNAMT = 'CGBTRF'
         INFOT = 1
         CALL CGBTRF( -1, 0, 0, 0, A, 1, IP, INFO )
         CALL CHKXER( 'CGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGBTRF( 0, -1, 0, 0, A, 1, IP, INFO )
         CALL CHKXER( 'CGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGBTRF( 1, 1, -1, 0, A, 1, IP, INFO )
         CALL CHKXER( 'CGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGBTRF( 1, 1, 0, -1, A, 1, IP, INFO )
         CALL CHKXER( 'CGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CGBTRF( 2, 2, 1, 1, A, 3, IP, INFO )
         CALL CHKXER( 'CGBTRF', INFOT, NOUT, LERR, OK )
*
*        CGBTF2
*
         SRNAMT = 'CGBTF2'
         INFOT = 1
         CALL CGBTF2( -1, 0, 0, 0, A, 1, IP, INFO )
         CALL CHKXER( 'CGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGBTF2( 0, -1, 0, 0, A, 1, IP, INFO )
         CALL CHKXER( 'CGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGBTF2( 1, 1, -1, 0, A, 1, IP, INFO )
         CALL CHKXER( 'CGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGBTF2( 1, 1, 0, -1, A, 1, IP, INFO )
         CALL CHKXER( 'CGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CGBTF2( 2, 2, 1, 1, A, 3, IP, INFO )
         CALL CHKXER( 'CGBTF2', INFOT, NOUT, LERR, OK )
*
*        CGBTRS
*
         SRNAMT = 'CGBTRS'
         INFOT = 1
         CALL CGBTRS( '/', 0, 0, 0, 1, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'CGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGBTRS( 'N', -1, 0, 0, 1, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'CGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGBTRS( 'N', 1, -1, 0, 1, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'CGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGBTRS( 'N', 1, 0, -1, 1, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'CGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGBTRS( 'N', 1, 0, 0, -1, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'CGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CGBTRS( 'N', 2, 1, 1, 1, A, 3, IP, B, 2, INFO )
         CALL CHKXER( 'CGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CGBTRS( 'N', 2, 0, 0, 1, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'CGBTRS', INFOT, NOUT, LERR, OK )
*
*        CGBRFS
*
         SRNAMT = 'CGBRFS'
         INFOT = 1
         CALL CGBRFS( '/', 0, 0, 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $                R2, W, R, INFO )
         CALL CHKXER( 'CGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGBRFS( 'N', -1, 0, 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $                R2, W, R, INFO )
         CALL CHKXER( 'CGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGBRFS( 'N', 1, -1, 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $                R2, W, R, INFO )
         CALL CHKXER( 'CGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGBRFS( 'N', 1, 0, -1, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $                R2, W, R, INFO )
         CALL CHKXER( 'CGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGBRFS( 'N', 1, 0, 0, -1, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $                R2, W, R, INFO )
         CALL CHKXER( 'CGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CGBRFS( 'N', 2, 1, 1, 1, A, 2, AF, 4, IP, B, 2, X, 2, R1,
     $                R2, W, R, INFO )
         CALL CHKXER( 'CGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CGBRFS( 'N', 2, 1, 1, 1, A, 3, AF, 3, IP, B, 2, X, 2, R1,
     $                R2, W, R, INFO )
         CALL CHKXER( 'CGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CGBRFS( 'N', 2, 0, 0, 1, A, 1, AF, 1, IP, B, 1, X, 2, R1,
     $                R2, W, R, INFO )
         CALL CHKXER( 'CGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL CGBRFS( 'N', 2, 0, 0, 1, A, 1, AF, 1, IP, B, 2, X, 1, R1,
     $                R2, W, R, INFO )
         CALL CHKXER( 'CGBRFS', INFOT, NOUT, LERR, OK )
*
*        CGBCON
*
         SRNAMT = 'CGBCON'
         INFOT = 1
         CALL CGBCON( '/', 0, 0, 0, A, 1, IP, ANRM, RCOND, W, R, INFO )
         CALL CHKXER( 'CGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGBCON( '1', -1, 0, 0, A, 1, IP, ANRM, RCOND, W, R, INFO )
         CALL CHKXER( 'CGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGBCON( '1', 1, -1, 0, A, 1, IP, ANRM, RCOND, W, R, INFO )
         CALL CHKXER( 'CGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGBCON( '1', 1, 0, -1, A, 1, IP, ANRM, RCOND, W, R, INFO )
         CALL CHKXER( 'CGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CGBCON( '1', 2, 1, 1, A, 3, IP, ANRM, RCOND, W, R, INFO )
         CALL CHKXER( 'CGBCON', INFOT, NOUT, LERR, OK )
*
*        CGBEQU
*
         SRNAMT = 'CGBEQU'
         INFOT = 1
         CALL CGBEQU( -1, 0, 0, 0, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL CHKXER( 'CGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGBEQU( 0, -1, 0, 0, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL CHKXER( 'CGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGBEQU( 1, 1, -1, 0, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL CHKXER( 'CGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGBEQU( 1, 1, 0, -1, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL CHKXER( 'CGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CGBEQU( 2, 2, 1, 1, A, 2, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL CHKXER( 'CGBEQU', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of CERRGE
*
      END
