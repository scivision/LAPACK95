      SUBROUTINE SERRGE( PATH, NUNIT )
*
*  -- LAPACK test routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
      IMPLICIT NONE
*     .. Scalar Arguments ..
      CHARACTER*3        PATH
      INTEGER            NUNIT
*     ..
*
*  Purpose
*  =======
*
*  SERRGE tests the error exits for the REAL routines
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
      INTEGER            NMAX, LW
      PARAMETER          ( NMAX = 4, LW = 3*NMAX )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, INFO, J, NRHS
      REAL               ANRM, CCOND, RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX ), IW( NMAX ), IN( 100 )
      REAL               A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   R1( NMAX ), R2( NMAX ), W( LW ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL ALAESM, CHKXER, SGBCON, SGBEQU, SGBRFS, SGBTF2, SGBTRF,
     $         SGBTRS, LA_TEST_SGECON, LA_TEST_SGEEQU, LA_TEST_SGERFS,
     $         SGETF2, LA_TEST_SGETRF, LA_TEST_SGETRI, LA_TEST_SGETRS
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
            AF( I, J ) = 1. / REAL( I+J )
   10    CONTINUE
         B( J ) = 0.
         R1( J ) = 0.
         R2( J ) = 0.
         W( J ) = 0.
         X( J ) = 0.
         IP( J ) = J
         IW( J ) = J
   20 CONTINUE
      OK = .TRUE.
*
      IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        Test error exits of the routines that use the LU decomposition
*        of a general matrix.
*
*        SGETRF
*
         SRNAMT = 'SGETRF'
         INFOT = 2
         INFO = 201
         CALL LA_TEST_SGETRF( NMAX, NMAX, A, NMAX, IP, INFO )
         CALL CHKXER( 'SGETRF', INFOT, NOUT, LERR, OK )
*
*        SGETF2
*
         SRNAMT = 'SGETF2'
         INFOT = 1
         CALL SGETF2( -1, 0, A, 1, IP, INFO )
         CALL CHKXER( 'SGETF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGETF2( 0, -1, A, 1, IP, INFO )
         CALL CHKXER( 'SGETF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SGETF2( 2, 1, A, 1, IP, INFO )
         CALL CHKXER( 'SGETF2', INFOT, NOUT, LERR, OK )
*
*        SGETRI
*
         SRNAMT = 'SGETRI'
         DO I = 1, 2
            IN(I) = 1
         END DO
         DO I = 1, 2
            INFOT = I
            DO J = 1, IN(I)
               INFO = J + 100*I
               CALL LA_TEST_SGETRI( NMAX, A, NMAX, IP, W, LW, INFO )
               CALL CHKXER( 'SGETRI', INFOT, NOUT, LERR, OK )
            END DO
         END DO
*
*        SGETRS
*
         SRNAMT = 'SGETRS'
         DO I = 1, 4
            IN(I) = 1
         END DO
         DO I = 1, 4
            INFOT = I
            DO J = 1, IN(I)
               DO NRHS = 1, 2
                  INFO = J + 100*I
                  CALL LA_TEST_SGETRS( 'N', NMAX, NRHS, A, NMAX,
     $                 IP, B, NMAX, INFO )
                  CALL CHKXER( 'SGETRS', INFOT, NOUT, LERR, OK )
               END DO
            END DO
         END DO
*
*        SGERFS
*
         SRNAMT = 'SGERFS'
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
                  CALL LA_TEST_SGERFS( 'N', NMAX, NRHS, A, NMAX, AF,
     $                 NMAX, IP, B, NMAX, X, NMAX, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'SGERFS', INFOT, NOUT, LERR, OK )
               END DO
            END DO
         END DO
*
*        SGECON
*
         SRNAMT = 'SGECON'
         DO I = 1, 4
            IN(I) = 1
         END DO
         IN(4) = 2
         DO I = 1, 4
            IF( I.NE.3 ) THEN
               INFOT = I
               DO J = 1, IN(I)
                  INFO = J + 100*I
                  CALL LA_TEST_SGECON( 'I', NMAX, A, NMAX, ANRM, RCOND,
     $                 W, IW, INFO )
                  CALL CHKXER( 'SGECON ', INFOT, NOUT, LERR, OK )
               END DO
            END IF
         END DO
*
*        SGEEQU
*
         SRNAMT = 'SGEEQU'
         DO I = 2, 3
            IN(I) = 1
         END DO
         DO I = 2, 3
            INFOT = I
            DO J = 1, IN(I)
               INFO = J + 100*I
               CALL LA_TEST_SGEEQU( NMAX, NMAX+1, A, NMAX, R1, R2,
     $              RCOND, CCOND, ANRM, INFO )
               CALL CHKXER( 'SGEEQU', INFOT, NOUT, LERR, OK )
            END DO
         END DO
*
      ELSE IF( LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        Test error exits of the routines that use the LU decomposition
*        of a general band matrix.
*
*        SGBTRF
*
         SRNAMT = 'SGBTRF'
         INFOT = 1
         CALL SGBTRF( -1, 0, 0, 0, A, 1, IP, INFO )
         CALL CHKXER( 'SGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGBTRF( 0, -1, 0, 0, A, 1, IP, INFO )
         CALL CHKXER( 'SGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGBTRF( 1, 1, -1, 0, A, 1, IP, INFO )
         CALL CHKXER( 'SGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SGBTRF( 1, 1, 0, -1, A, 1, IP, INFO )
         CALL CHKXER( 'SGBTRF', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL SGBTRF( 2, 2, 1, 1, A, 3, IP, INFO )
         CALL CHKXER( 'SGBTRF', INFOT, NOUT, LERR, OK )
*
*        SGBTF2
*
         SRNAMT = 'SGBTF2'
         INFOT = 1
         CALL SGBTF2( -1, 0, 0, 0, A, 1, IP, INFO )
         CALL CHKXER( 'SGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGBTF2( 0, -1, 0, 0, A, 1, IP, INFO )
         CALL CHKXER( 'SGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGBTF2( 1, 1, -1, 0, A, 1, IP, INFO )
         CALL CHKXER( 'SGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SGBTF2( 1, 1, 0, -1, A, 1, IP, INFO )
         CALL CHKXER( 'SGBTF2', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL SGBTF2( 2, 2, 1, 1, A, 3, IP, INFO )
         CALL CHKXER( 'SGBTF2', INFOT, NOUT, LERR, OK )
*
*        SGBTRS
*
         SRNAMT = 'SGBTRS'
         INFOT = 1
         CALL SGBTRS( '/', 0, 0, 0, 1, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'SGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGBTRS( 'N', -1, 0, 0, 1, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'SGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGBTRS( 'N', 1, -1, 0, 1, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'SGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SGBTRS( 'N', 1, 0, -1, 1, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'SGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL SGBTRS( 'N', 1, 0, 0, -1, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'SGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL SGBTRS( 'N', 2, 1, 1, 1, A, 3, IP, B, 2, INFO )
         CALL CHKXER( 'SGBTRS', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL SGBTRS( 'N', 2, 0, 0, 1, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'SGBTRS', INFOT, NOUT, LERR, OK )
*
*        SGBRFS
*
         SRNAMT = 'SGBRFS'
         INFOT = 1
         CALL SGBRFS( '/', 0, 0, 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $                R2, W, IW, INFO )
         CALL CHKXER( 'SGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGBRFS( 'N', -1, 0, 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $                R2, W, IW, INFO )
         CALL CHKXER( 'SGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGBRFS( 'N', 1, -1, 0, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $                R2, W, IW, INFO )
         CALL CHKXER( 'SGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SGBRFS( 'N', 1, 0, -1, 0, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $                R2, W, IW, INFO )
         CALL CHKXER( 'SGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL SGBRFS( 'N', 1, 0, 0, -1, A, 1, AF, 1, IP, B, 1, X, 1, R1,
     $                R2, W, IW, INFO )
         CALL CHKXER( 'SGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL SGBRFS( 'N', 2, 1, 1, 1, A, 2, AF, 4, IP, B, 2, X, 2, R1,
     $                R2, W, IW, INFO )
         CALL CHKXER( 'SGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL SGBRFS( 'N', 2, 1, 1, 1, A, 3, AF, 3, IP, B, 2, X, 2, R1,
     $                R2, W, IW, INFO )
         CALL CHKXER( 'SGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL SGBRFS( 'N', 2, 0, 0, 1, A, 1, AF, 1, IP, B, 1, X, 2, R1,
     $                R2, W, IW, INFO )
         CALL CHKXER( 'SGBRFS', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL SGBRFS( 'N', 2, 0, 0, 1, A, 1, AF, 1, IP, B, 2, X, 1, R1,
     $                R2, W, IW, INFO )
         CALL CHKXER( 'SGBRFS', INFOT, NOUT, LERR, OK )
*
*        SGBCON
*
         SRNAMT = 'SGBCON'
         INFOT = 1
         CALL SGBCON( '/', 0, 0, 0, A, 1, IP, ANRM, RCOND, W, IW, INFO )
         CALL CHKXER( 'SGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGBCON( '1', -1, 0, 0, A, 1, IP, ANRM, RCOND, W, IW,
     $                INFO )
         CALL CHKXER( 'SGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGBCON( '1', 1, -1, 0, A, 1, IP, ANRM, RCOND, W, IW,
     $                INFO )
         CALL CHKXER( 'SGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SGBCON( '1', 1, 0, -1, A, 1, IP, ANRM, RCOND, W, IW,
     $                INFO )
         CALL CHKXER( 'SGBCON', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL SGBCON( '1', 2, 1, 1, A, 3, IP, ANRM, RCOND, W, IW, INFO )
         CALL CHKXER( 'SGBCON', INFOT, NOUT, LERR, OK )
*
*        SGBEQU
*
         SRNAMT = 'SGBEQU'
         INFOT = 1
         CALL SGBEQU( -1, 0, 0, 0, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL CHKXER( 'SGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL SGBEQU( 0, -1, 0, 0, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL CHKXER( 'SGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL SGBEQU( 1, 1, -1, 0, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL CHKXER( 'SGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL SGBEQU( 1, 1, 0, -1, A, 1, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL CHKXER( 'SGBEQU', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL SGBEQU( 2, 2, 1, 1, A, 2, R1, R2, RCOND, CCOND, ANRM,
     $                INFO )
         CALL CHKXER( 'SGBEQU', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of SERRGE
*
      END
