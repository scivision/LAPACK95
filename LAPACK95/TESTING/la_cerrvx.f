      SUBROUTINE CERRVX( PATH, NUNIT )
      IMPLICIT NONE
*
*  -- LAPACK90 test routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*     UNI-C, Denmark       
*     April 9, 1999
*
*     .. Scalar Arguments ..
      CHARACTER*3        PATH
      INTEGER            NUNIT
*     ..
*
*  Purpose
*  =======
*
*  CERRVX tests the error exits for the COMPLEX driver routines
*  for solving linear systems of equations.
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
*     .. Local Scalar ..
      CHARACTER*3 PATH1
      SAVE PATH1
      DATA PATH1/'   '/
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NMAX
      PARAMETER          ( NMAX = 4 )
*     ..
*     .. Local Scalars ..
      CHARACTER          EQ
      CHARACTER*2        C2
      CHARACTER*1        CHAR, IEQUED 
      INTEGER            I, INFO, J, NRHS
      INTEGER            KL, KU
      REAL               RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX ), IN( 100 )
      REAL               C( NMAX ), R( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   RW( NMAX )
      COMPLEX            A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL LA_TEST_CGBSV, LA_TEST_CGBSVX, LA_TEST_CGESV,
     &         LA_TEST_CGESVX, LA_TEST_CGTSV, LA_TEST_CGTSVX,
     &         LA_TEST_CHESV, LA_TEST_CHESVX, CHKXER,
     &         LA_TEST_CHPSV, LA_TEST_CHPSVX, LA_TEST_CPBSV,
     $         LA_TEST_CPBSVX, LA_TEST_CPOSV, LA_TEST_CPOSVX,
     &         LA_TEST_CPPSV, LA_TEST_CPPSVX, LA_TEST_CPTSV,
     $         LA_TEST_CPTSVX, LA_TEST_CSPSV, LA_TEST_CSPSVX,
     &         LA_TEST_CSYSV, LA_TEST_CSYSVX
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
         C( J ) = 0.
         R( J ) = 0.
         IP( J ) = J
   20 CONTINUE
      OK = .TRUE.
*
      IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        CGESV
*
         SRNAMT = 'CGESV '
         DO I = 1, 3
            IN(I) = 1
         END DO
         DO I = 1, 3
            INFOT = I
            DO J = 1, IN(I)
               DO NRHS = 1, 2
                  INFO = J + 100*I
                  CALL LA_TEST_CGESV( NMAX, NRHS, A, NMAX, IP, B, NMAX,
     $                 INFO )
                  CALL CHKXER( 'CGESV ', INFOT, NOUT, LERR, OK )
               END DO
             END DO
           END DO
*
*        CGESVX
*
         SRNAMT = 'CGESVX'
         DO I = 1, 12
            IN(I) = 1
         END DO
         IN(3) = 2
         IN(4) = 2
         IN(6) = 4
         IN(8) = 4
         IN(9) = 3
         IN(10) = 3
         DO I = 1, 12
            INFOT = I
            DO J = 1, IN(I)
               DO NRHS = 1, 2
                  INFO = J + 100*I
                  CALL LA_TEST_CGESVX( CHAR, CHAR, NMAX, NRHS, A, NMAX,
     $               AF, NMAX, IP, EQ, R, C, B, NMAX, X, NMAX, RCOND,
     $               R1, R2, W, 2*NMAX, INFO)
                  CALL CHKXER( 'CGESVX', INFOT, NOUT, LERR, OK )
               END DO
            END DO
         END DO

*
      ELSE IF( LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        CGBSV
*
         SRNAMT = 'CGBSV '
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I
               KL = (NMAX-1)/3
               KU = NMAX-2*KL-1
               CALL LA_TEST_CGBSV( NMAX, KL, KU, NRHS, A, NMAX, IP, B,
     $             NMAX, INFO )
               CALL CHKXER( 'CGBSV ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO 
*
*        CGBSVX
*
         SRNAMT = 'CGBSVX'
         DO I = 1, 13
           IN(I) = 1
         END DO
         IN(3) = 2
         IN(4) = 2
         IN(5) = 2
         IN(7) = 4
         IN(9) = 4
         IN(10) = 3
         IN(11) = 3
         DO I = 1, 13
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               IF (I /= 1) THEN
                 INFO = J + 100*I
                 IEQUED = 'N'
                 CALL LA_TEST_CGBSVX( 'F', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'R'
                 CALL LA_TEST_CGBSVX( 'F', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_CGBSVX( 'F', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'B'
                 CALL LA_TEST_CGBSVX( 'F', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 IEQUED = 'N'
                 CALL LA_TEST_CGBSVX( 'F', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'R'
                 CALL LA_TEST_CGBSVX( 'F', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_CGBSVX( 'F', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'B'
                 CALL LA_TEST_CGBSVX( 'F', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'N'
                 CALL LA_TEST_CGBSVX( 'F', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'R'
                 CALL LA_TEST_CGBSVX( 'F', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_CGBSVX( 'F', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'B'
                 CALL LA_TEST_CGBSVX( 'F', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'N'
                 CALL LA_TEST_CGBSVX( 'N', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'R'
                 CALL LA_TEST_CGBSVX( 'N', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_CGBSVX( 'N', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'B'
                 CALL LA_TEST_CGBSVX( 'N', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'N'
                 CALL LA_TEST_CGBSVX( 'N', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'R'
                 CALL LA_TEST_CGBSVX( 'N', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_CGBSVX( 'N', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'B'
                 CALL LA_TEST_CGBSVX( 'N', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'N'
                 CALL LA_TEST_CGBSVX( 'N', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'R'
                 CALL LA_TEST_CGBSVX( 'N', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_CGBSVX( 'N', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'B'
                 CALL LA_TEST_CGBSVX( 'N', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'N'
                 CALL LA_TEST_CGBSVX( 'E', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'R'
                 CALL LA_TEST_CGBSVX( 'E', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_CGBSVX( 'E', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )
                 INFO = J + 100*I
                 IEQUED = 'B'
                 CALL LA_TEST_CGBSVX( 'E', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 1                 00*I
                 IEQUED = 'N'
                 CALL LA_TEST_CGBSVX( 'E', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'R'
                 CALL LA_TEST_CGBSVX( 'E', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_CGBSVX( 'E', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'B'
                 CALL LA_TEST_CGBSVX( 'E', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'N'
                 CALL LA_TEST_CGBSVX( 'E', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'R'
                 CALL LA_TEST_CGBSVX( 'E', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_CGBSVX( 'E', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'B'
                 CALL LA_TEST_CGBSVX( 'E', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )
               ENDIF
             ENDDO
           ENDDO
         ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        CGTSV
*
         SRNAMT = 'CGTSV '
         DO I = 1, 4
           IF (I/=2)  THEN
             INFOT = I
             DO J = 1, 1
               DO NRHS = 1, 2
                 INFO = J + 100*I
                 CALL LA_TEST_CGTSV( NMAX, NRHS, A(1,1), A(1,2),A(1, 3),
     $            B, NMAX, INFO )
                 CALL CHKXER( 'CGTSV ', INFOT, NOUT, LERR, OK )
               END DO
             END DO
           END IF
         END DO 
*
*        CGTSVX
*
         SRNAMT = 'CGTSVX'
         DO I = 1, 14
           IF (I/=2)  THEN
             INFOT = I
             DO J = 1, 1
               DO NRHS = 1, 2
                 INFO = J + 100*I
                 CALL LA_TEST_CGTSVX( 'N', 'N', NMAX, NMAX, A( 1, 1 ),
     &             A( 1, 2 ), A( 1, 3 ),
     &             AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &             IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGTSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 CALL LA_TEST_CGTSVX( 'F', 'N', NMAX, NMAX, A( 1, 1 ),
     &             A( 1, 2 ), A( 1, 3 ),
     &             AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &             IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGTSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 CALL LA_TEST_CGTSVX( 'N', 'T', NMAX, NMAX, A( 1, 1 ),
     &             A( 1, 2 ), A( 1, 3 ),
     &             AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &             IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGTSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 CALL LA_TEST_CGTSVX( 'F', 'T', NMAX, NMAX, A( 1, 1 ),
     &             A( 1, 2 ), A( 1, 3 ),
     &             AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &             IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGTSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 CALL LA_TEST_CGTSVX( 'N', 'C', NMAX, NMAX, A( 1, 1 ),
     &             A( 1, 2 ), A( 1, 3 ),
     &             AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &             IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGTSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 CALL LA_TEST_CGTSVX( 'F', 'C', NMAX, NMAX, A( 1, 1 ),
     &             A( 1, 2 ), A( 1, 3 ),
     &             AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &             IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'CGTSVX', INFOT, NOUT, LERR, OK )
               ENDDO
             ENDDO
           ENDIF
         ENDDO    
*
      ELSE IF( LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        CPOSV
*
         SRNAMT = 'CPOSV '
         DO I = 1, 3
           IN(I) = 1
         END DO
         DO I = 1, 3
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_CPOSV( 'U', NMAX, NRHS, A, NMAX, B, NMAX,
     $                 INFO )
               CALL CHKXER( 'CPOSV ', INFOT, NOUT, LERR, OK )
               INFO = J + 100*I
               CALL LA_TEST_CPOSV( 'L', NMAX, NRHS, A, NMAX, B, NMAX,
     $                 INFO )
               CALL CHKXER( 'CPOSV ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO 
*
*        CPOSVX
*
         SRNAMT = 'CPOSVX'
         DO I = 1, 10
           IN(I) = 1
         END DO
         DO I = 1, 10
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_CPOSVX( 'F', 'U', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPOSVX( 'F', 'U', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPOSVX( 'F', 'L', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPOSVX( 'F', 'L', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPOSVX( 'N', 'U', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPOSVX( 'N', 'U', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPOSVX( 'N', 'L', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPOSVX( 'N', 'L', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPOSVX( 'E', 'U', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPOSVX( 'E', 'U', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPOSVX( 'E', 'L', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPOSVX( 'E', 'L', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDDO
         ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'PP' ) ) THEN
*
*        CPPSV
*
         SRNAMT = 'CPPSV '
         DO I = 1, 3
           IN(I) = 1
         END DO
         DO I = 1, 3
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_CPPSV( 'U', NMAX, NRHS, A(1,1), B,
     $                             NMAX, INFO)

               CALL CHKXER( 'CPPSV ', INFOT, NOUT, LERR, OK )
               INFO = J + 100*I
               CALL LA_TEST_CPPSV( 'L', NMAX, NRHS, A(1,1), B,
     $                             NMAX, INFO)
               CALL CHKXER( 'CPPSV ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO
*
*        CPPSVX
*
         SRNAMT = 'CPPSVX'
         DO I = 1, 10
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_CPPSVX( 'F', 'U', NMAX, NMAX, A, AF, 'N',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPPSVX( 'F', 'U', NMAX, NMAX, A, AF, 'Y',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPPSVX( 'F', 'L', NMAX, NMAX, A, AF, 'N',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPPSVX( 'F', 'L', NMAX, NMAX, A, AF, 'Y',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPPSVX( 'N', 'U', NMAX, NMAX, A, AF, 'N',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPPSVX( 'N', 'U', NMAX, NMAX, A, AF, 'Y',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPPSVX( 'N', 'L', NMAX, NMAX, A, AF, 'N',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPPSVX( 'N', 'L', NMAX, NMAX, A, AF, 'Y',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPPSVX( 'E', 'U', NMAX, NMAX, A, AF, 'N',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPPSVX( 'E', 'U', NMAX, NMAX, A, AF, 'Y',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPPSVX( 'E', 'L', NMAX, NMAX, A, AF, 'N',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPPSVX( 'E', 'L', NMAX, NMAX, A, AF, 'Y',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDDO
         ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'PB' ) ) THEN
*
*        CPBSV
*
         SRNAMT = 'CPBSV '
         DO I = 1, 3
           IN(I) = 1
         END DO
         DO I = 1, 3
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_CPBSV( 'U', NMAX, 1, NRHS, A, NMAX,
     $                               B, NMAX, INFO)
               CALL CHKXER( 'CPBSV ', INFOT, NOUT, LERR, OK )
               INFO = J + 100*I
               CALL LA_TEST_CPBSV( 'L', NMAX, 1, NRHS, A, NMAX,
     $                              B, NMAX, INFO)
               CALL CHKXER( 'CPBSV ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO    

*
*        CPBSVX
*
         SRNAMT = 'CPBSVX'
         DO I = 1, 10
           IN(I) = 1
         END DO
         DO I = 2, 10
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_CPBSVX( 'F', 'U', NMAX, NMAX-1, NMAX, A,
     &           NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPBSVX( 'F', 'U', NMAX, NMAX-1, NMAX, A,
     &           NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPBSVX( 'F', 'L', NMAX, NMAX-1, NMAX, A,
     &           NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPBSVX( 'F', 'L', NMAX, NMAX-1, NMAX, A,
     &           NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPBSVX( 'N', 'U', NMAX, NMAX-1, NMAX, A,
     &           NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPBSVX( 'N', 'U', NMAX, NMAX-1, NMAX, A,
     &           NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPBSVX( 'N', 'L', NMAX, NMAX-1, NMAX, A,
     &           NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPBSVX( 'N', 'L', NMAX, NMAX-1, NMAX, A,
     &           NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPBSVX( 'E', 'U', NMAX, NMAX-1, NMAX, A,
     &           NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPBSVX( 'E', 'U', NMAX, NMAX-1, NMAX, A,
     &           NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPBSVX( 'E', 'L', NMAX, NMAX-1, NMAX, A,
     &           NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPBSVX( 'E', 'L', NMAX, NMAX-1, NMAX, A,
     &           NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDDO
         ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        CPTSV
*
         SRNAMT = 'CPTSV '
         DO I = 1, 3
           IN(I) = 1
         END DO
         DO I = 2, 3
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_CPTSV( NMAX, NRHS, A(1,1), A(1,2),
     $                            B, NMAX, INFO )
               CALL CHKXER( 'CPTSV ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO 
*
*        CPTSVX
*
         SRNAMT = 'CPTSVX'
         DO I = 2, 9
           IN(I) = 1
         END DO
         DO I = 2, 9
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_CPTSVX( 'F', NMAX, NMAX, A( 1, 1 ),
     &           A( 1, 2 ), AF( 1, 1 ), AF( 1, 2 ), B, NMAX, X,
     &           NMAX, RCOND, R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPTSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CPTSVX( 'N', NMAX, NMAX, A( 1, 1 ),
     &           A( 1, 2 ), AF( 1, 1 ), AF( 1, 2 ), B, NMAX, X,
     &           NMAX, RCOND, R1, R2, W, RW, INFO )
               CALL CHKXER( 'CPTSVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDDO
         ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'HE' ) ) THEN
*
*        CHESV
*
        SRNAMT = 'CHESV '
        DO I = 1, 4
          IN(I) = 1
        END DO
        DO I = 1, 4
          INFOT = I
          DO J = 1, IN(I)
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_CHESV( 'U', NMAX, NRHS,  A, NMAX, A(1,2),
     $                               B, NMAX, W, 1, INFO)
              CALL CHKXER( 'CHESV ', INFOT, NOUT, LERR, OK )
              INFO = J + 100*I
              CALL LA_TEST_CHESV( 'L', NMAX, NRHS, A, NMAX, A(1,2),
     $                              B, NMAX, W, 1,INFO)
              CALL CHKXER( 'CHESV ', INFOT, NOUT, LERR, OK )
            END DO
          END DO
        END DO 
        
*
*        CHESVX
*
        SRNAMT = 'CHESVX'
        DO I = 1, 9
          IN(I) = 1
        END DO
        DO I = 1, 9
          INFOT = I
          DO J = 1, 1
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_CHESVX( 'N', 'L', NMAX, NRHS, A, NMAX, AF,
     &          NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &          RW, INFO )
              CALL CHKXER( 'CHESVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CHESVX( 'N', 'U', NMAX, NRHS, A, NMAX, AF,
     &          NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &          RW, INFO )
              CALL CHKXER( 'CHESVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CHESVX( 'F', 'L', NMAX, NRHS, A, NMAX, AF,
     &          NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &          RW, INFO )
              CALL CHKXER( 'CHESVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CHESVX( 'F', 'U', NMAX, NRHS, A, NMAX, AF,
     &          NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &          RW, INFO )
              CALL CHKXER( 'CHESVX', INFOT, NOUT, LERR, OK )
            ENDDO
          ENDDO
        ENDDO                                     
*
      ELSE IF( LSAMEN( 2, C2, 'HP' ) ) THEN
*
*        CHPSV
*
         SRNAMT = 'CHPSV '
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_CHPSV( 'U', NMAX, NRHS, A(1,1), A(1,2),
     $                              B, NMAX, INFO)
               CALL CHKXER( 'CHPSV ', INFOT, NOUT, LERR, OK )
               INFO = J + 100*I
               CALL LA_TEST_CHPSV( 'L', NMAX, NRHS, A(1,1), A(1 ,2),
     $                              B, NMAX, INFO)
               CALL CHKXER( 'CHPSV ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO
*
*        CHPSVX
*
         SRNAMT = 'CHPSVX'
         DO I = 1, 9
           IN(I) = 1
         END DO
         DO I = 1, 9
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_CHPSVX( 'N', 'U', NMAX, NRHS, A, AF, IP, B,
     &           NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
               CALL CHKXER( 'CHPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHPSVX( 'N', 'L', NMAX, NRHS, A, AF, IP, B,
     &           NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
               CALL CHKXER( 'CHPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHPSVX( 'F', 'U', NMAX, NRHS, A, AF, IP, B,
     &           NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
               CALL CHKXER( 'CHPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CHPSVX( 'F', 'L', NMAX, NRHS, A, AF, IP, B,
     &           NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
               CALL CHKXER( 'CHPSVX', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO                      
*
      ELSE IF( LSAMEN( 2, C2, 'SY' ) ) THEN
*
*        CSYSV
*
         SRNAMT = 'CSYSV '
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_CSYSV('U', NMAX, NRHS, A, NMAX,
     $                               A(1,2), B, NMAX, W, 1 ,INFO )
               CALL CHKXER( 'CSYSV ', INFOT, NOUT, LERR, OK )
               INFO = J + 100*I
               CALL LA_TEST_CSYSV('L', NMAX, NRHS, A, NMAX,
     $                               A(1,2), B, NMAX, W, 1 ,INFO )
               CALL CHKXER( 'CSYSV ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO      
*
*        CSYSVX
*
         SRNAMT = 'CSYSVX'
         DO I = 1, 9
           IN(I) = 1
         END DO
         DO I = 1, 9
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_CSYSVX( 'N', 'L', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           RW, INFO )
               CALL CHKXER( 'CSYSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CSYSVX( 'N', 'U', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           RW, INFO )
               CALL CHKXER( 'CSYSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CSYSVX( 'F', 'L', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           RW, INFO )
               CALL CHKXER( 'CSYSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_CSYSVX( 'F', 'U', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           RW, INFO )
               CALL CHKXER( 'CSYSVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDDO
         ENDDO                                                     
*
      ELSE IF( LSAMEN( 2, C2, 'SP' ) ) THEN
*
*        CSPSV
*
        SRNAMT = 'CSPSV '
        DO I = 1, 4
          IN(I) = 1
        END DO
        DO I = 1, 4
          INFOT = I
          DO J = 1, IN(I)
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_CSPSV( 'U', NMAX, NRHS, A(1,1), A(1,2),
     $                              B, NMAX, INFO)
              CALL CHKXER( 'CSPSV ', INFOT, NOUT, LERR, OK )
              INFO = J + 100*I
              CALL LA_TEST_CSPSV( 'L', NMAX, NRHS, A(1,1), A(1 ,2),
     $                              B, NMAX, INFO)
              CALL CHKXER( 'CSPSV ', INFOT, NOUT, LERR, OK )
            END DO
          END DO
        END DO  
*
*        CSPSVX
*
        SRNAMT = 'CSPSVX'
        DO I = 1, 9
          IN(I) = 1
        END DO
        DO I = 1, 9
          INFOT = I
          DO J = 1, IN(I)
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_CSPSVX( 'N', 'U', NMAX, NRHS, A, AF, IP, B,
     &          NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
              CALL CHKXER( 'CSPSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CSPSVX( 'N', 'L', NMAX, NRHS, A, AF, IP, B,
     &          NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
              CALL CHKXER( 'CSPSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CSPSVX( 'F', 'U', NMAX, NRHS, A, AF, IP, B,
     &          NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
              CALL CHKXER( 'CSPSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_CSPSVX( 'F', 'L', NMAX, NRHS, A, AF, IP, B,
     &          NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
              CALL CHKXER( 'CSPSVX', INFOT, NOUT, LERR, OK )
            END DO
          END DO
        END DO                
      END IF
*
*     Print a summary line.
*
      IF( PATH1 /= PATH )THEN
         PATH1 = PATH
         IF( OK ) THEN
            WRITE( NOUT, FMT = 9999 )PATH
         ELSE
            WRITE( NOUT, FMT = 9998 )PATH
         END IF
      END IF
*
 9999 FORMAT( 1X, A3, ' drivers passed the tests of the error exits' )
 9998 FORMAT( ' *** ', A3, ' drivers failed the tests of the error ',
     $      'exits ***' )
*
      RETURN
*
*     End of CERRVX
*
      END
