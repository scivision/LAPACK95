      SUBROUTINE SERRVX( PATH, NUNIT )
*
*  -- LAPACK90 test routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*     UNI-C, Denmark
*     April 8, 1999
*      
*     .. Scalar Arguments ..
      CHARACTER*3        PATH
      INTEGER            NUNIT
*     ..
*
*  Purpose
*  =======
*
*  SERRVX tests the error exits for the REAL driver routines
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
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NMAX, ARGMAX
      PARAMETER          ( NMAX = 4, ARGMAX = 100 )
*     ..
*     .. Local Scalars ..
      CHARACTER          EQ
      CHARACTER*1        CHAR, IEQUED 
      CHARACTER*2        C2
      INTEGER            I, INFO, J, NRHS
      INTEGER            KL, KU
      REAL               RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX ), IW( NMAX ), IN(ARGMAX)
      REAL               A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   C( NMAX ), R( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHKXER, LA_TEST_SGBSV, LA_TEST_SGBSVX,
     &                   LA_TEST_SGESV,
     $                   LA_TEST_SGESVX, LA_TEST_SGTSV, LA_TEST_SGTSVX,
     $                   LA_TEST_SPBSV, LA_TEST_SPBSVX, LA_TEST_SPOSV,
     &                   LA_TEST_SPOSVX, LA_TEST_SPPSV, LA_TEST_SPPSVX,
     &                   LA_TEST_SPTSV, LA_TEST_SPTSVX, LA_TEST_SSPSV,
     &                   LA_TEST_SSPSVX, LA_TEST_SSYSV, LA_TEST_SSYSVX
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
         C( J ) = 0.
         R( J ) = 0.
         IP( J ) = J
   20 CONTINUE
      OK = .TRUE.
*
      IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        SGESV
*
         SRNAMT = 'SGESV '
         DO I = 1, 3
            IN(I) = 1
         END DO
         DO I = 1, 3
            INFOT = I
            DO J = 1, IN(I)
               DO NRHS = 1, 2
                  INFO = J + 100*I
                  CALL LA_TEST_SGESV( NMAX, NRHS, A, NMAX, IP, B, NMAX,
     $                 INFO )
                  CALL CHKXER( 'SGESV ', INFOT, NOUT, LERR, OK )
               END DO
            END DO
         END DO
*
*        SGESVX
*
         SRNAMT = 'SGESVX'
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
                  CALL LA_TEST_SGESVX( CHAR, CHAR, NMAX, NRHS, A, NMAX,
     $               AF, NMAX, IP, EQ, R, C, B, NMAX, X, NMAX, RCOND,
     $               R1, R2, W, IW, INFO)
                  CALL CHKXER( 'SGESVX', INFOT, NOUT, LERR, OK )
               END DO
            END DO
         END DO
*
      ELSE IF( LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        SGBSV
*
        SRNAMT = 'SGBSV '
        DO I = 1, 4
          INFOT = I
          DO J = 1, 1
            DO NRHS = 1, 2
              INFO = J + 100*I
              KL = (NMAX-1)/3
              KU = NMAX-2*KL-1
              CALL LA_TEST_SGBSV( NMAX, KL, KU, NRHS, A, NMAX, IP, B,
     $                            NMAX, INFO )
              CALL CHKXER( 'SGBSV ', INFOT, NOUT, LERR, OK )
            END DO
          END DO
        END DO

*
*        SGBSVX
*
        SRNAMT = 'SGBSVX'
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
                CALL LA_TEST_SGBSVX( 'F', 'T', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'R'
                CALL LA_TEST_SGBSVX( 'F', 'T', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'C'
                CALL LA_TEST_SGBSVX( 'F', 'T', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'B'
                CALL LA_TEST_SGBSVX( 'F', 'T', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )           

                INFO = J + 100*I
                IEQUED = 'N'
                CALL LA_TEST_SGBSVX( 'F', 'N', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'R'
                CALL LA_TEST_SGBSVX( 'F', 'N', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'C'
                CALL LA_TEST_SGBSVX( 'F', 'N', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'B'
                CALL LA_TEST_SGBSVX( 'F', 'N', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'N'
                CALL LA_TEST_SGBSVX( 'F', 'C', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'R'
                CALL LA_TEST_SGBSVX( 'F', 'C', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )


                INFO = J + 100*I
                IEQUED = 'C'
                CALL LA_TEST_SGBSVX( 'F', 'C', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'B'
                CALL LA_TEST_SGBSVX( 'F', 'C', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'N'
                CALL LA_TEST_SGBSVX( 'N', 'T', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'R'
                CALL LA_TEST_SGBSVX( 'N', 'T', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'C'
                CALL LA_TEST_SGBSVX( 'N', 'T', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'B'
                CALL LA_TEST_SGBSVX( 'N', 'T', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'N'
                CALL LA_TEST_SGBSVX( 'N', 'N', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'R'
                CALL LA_TEST_SGBSVX( 'N', 'N', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'C'
                CALL LA_TEST_SGBSVX( 'N', 'N', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'B'
                CALL LA_TEST_SGBSVX( 'N', 'N', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'N'
                CALL LA_TEST_SGBSVX( 'N', 'C', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'R'
                CALL LA_TEST_SGBSVX( 'N', 'C', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'C'
                CALL LA_TEST_SGBSVX( 'N', 'C', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'B'
                CALL LA_TEST_SGBSVX( 'N', 'C', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'N'
                CALL LA_TEST_SGBSVX( 'E', 'T', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'R'
                CALL LA_TEST_SGBSVX( 'E', 'T', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'C'
                CALL LA_TEST_SGBSVX( 'E', 'T', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'B'
                CALL LA_TEST_SGBSVX( 'E', 'T', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'N'
                CALL LA_TEST_SGBSVX( 'E', 'N', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'R'
                CALL LA_TEST_SGBSVX( 'E', 'N', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'C'
                CALL LA_TEST_SGBSVX( 'E', 'N', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'B'
                CALL LA_TEST_SGBSVX( 'E', 'N', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'N'
                CALL LA_TEST_SGBSVX( 'E', 'C', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'R'
                CALL LA_TEST_SGBSVX( 'E', 'C', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'C'
                CALL LA_TEST_SGBSVX( 'E', 'C', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'B'
                CALL LA_TEST_SGBSVX( 'E', 'C', NMAX, NMAX, NMAX, NRHS,
     &            A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &            B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGBSVX', INFOT, NOUT, LERR, OK )

              END IF
            ENDDO
          ENDDO
        ENDDO                          
*
      ELSE IF( LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        SGTSV
*
        SRNAMT = 'SGTSV '
        DO I = 1, 4
          IF (I/=2)  THEN
            INFOT = I
            DO J = 1, 1
              DO NRHS = 1, 2
                INFO = J + 100*I
                CALL LA_TEST_SGTSV( NMAX, NRHS, A(1,1), A(1,2),A(1, 3),
     $                              B, NMAX, INFO )
                CALL CHKXER( 'SGTSV ', INFOT, NOUT, LERR, OK )
              END DO
            END DO
          END IF
        END DO 
*
*        SGTSVX
*
        SRNAMT = 'SGTSVX'
        DO I = 1, 14
          IF (I/=2)  THEN
            INFOT = I
            DO J = 1, 1
              DO NRHS = 1, 2
                INFO = J + 100*I
                CALL LA_TEST_SGTSVX( 'N', 'N', NMAX, NMAX, A( 1, 1 ),
     &            A( 1, 2 ), A( 1, 3 ),
     &            AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &            IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGTSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                CALL LA_TEST_SGTSVX( 'F', 'N', NMAX, NMAX, A( 1, 1 ),
     &            A( 1, 2 ), A( 1, 3 ),
     &            AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &            IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGTSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                CALL LA_TEST_SGTSVX( 'N', 'T', NMAX, NMAX, A( 1, 1 ),
     &            A( 1, 2 ), A( 1, 3 ),
     &            AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &            IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGTSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                CALL LA_TEST_SGTSVX( 'F', 'T', NMAX, NMAX, A( 1, 1 ),
     &            A( 1, 2 ), A( 1, 3 ),
     &            AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &            IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGTSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                CALL LA_TEST_SGTSVX( 'N', 'C', NMAX, NMAX, A( 1, 1 ),
     &            A( 1, 2 ), A( 1, 3 ),
     &            AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &            IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGTSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                CALL LA_TEST_SGTSVX( 'F', 'C', NMAX, NMAX, A( 1, 1 ),
     &            A( 1, 2 ), A( 1, 3 ),
     &            AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &            IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'SGTSVX', INFOT, NOUT, LERR, OK )

              ENDDO
            ENDDO
          ENDIF
        ENDDO                                            
*
      ELSE IF( LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        SPOSV
*
        SRNAMT = 'SPOSV '
        DO I = 1, 3
          IN(I) = 1
        END DO
        DO I = 1, 3
          INFOT = I
          DO J = 1, IN(I)
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_SPOSV( 'U', NMAX, NRHS, A, NMAX, B, NMAX,
     $                              INFO )
              CALL CHKXER( 'SPOSV ', INFOT, NOUT, LERR, OK )
              INFO = J + 100*I
              CALL LA_TEST_SPOSV( 'L', NMAX, NRHS, A, NMAX, B, NMAX,
     $                              INFO )
              CALL CHKXER( 'SPOSV ', INFOT, NOUT, LERR, OK )
            END DO
          END DO
        END DO 
*
*        SPOSVX
*
        SRNAMT = 'SPOSVX'
        DO I = 1, 10
          IN(I) = 1
        END DO
        DO I = 1, 10
          INFOT = I
          DO J = 1, IN(I)
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_SPOSVX( 'F', 'U', NMAX, NMAX, A, NMAX,
     &          AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &          W, IW, INFO )
              CALL CHKXER( 'SPOSVX', INFOT, NOUT, LERR, OK )
              
              INFO = J + 100*I
              CALL LA_TEST_SPOSVX( 'F', 'U', NMAX, NMAX, A, NMAX,
     &          AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &          W, IW, INFO )
              CALL CHKXER( 'SPOSVX', INFOT, NOUT, LERR, OK )
              
              INFO = J + 100*I
              CALL LA_TEST_SPOSVX( 'F', 'L', NMAX, NMAX, A, NMAX,
     &          AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &          W, IW, INFO )
              CALL CHKXER( 'SPOSVX', INFOT, NOUT, LERR, OK )
              
              INFO = J + 100*I
              CALL LA_TEST_SPOSVX( 'F', 'L', NMAX, NMAX, A, NMAX,
     &          AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &          W, IW, INFO )
              CALL CHKXER( 'SPOSVX', INFOT, NOUT, LERR, OK )
              
              INFO = J + 100*I
              CALL LA_TEST_SPOSVX( 'N', 'U', NMAX, NMAX, A, NMAX,
     &          AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &          W, IW, INFO )
              CALL CHKXER( 'SPOSVX', INFOT, NOUT, LERR, OK )
              
              INFO = J + 100*I
              CALL LA_TEST_SPOSVX( 'N', 'U', NMAX, NMAX, A, NMAX,
     &          AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &          W, IW, INFO )
              CALL CHKXER( 'SPOSVX', INFOT, NOUT, LERR, OK )
              
              INFO = J + 100*I
              CALL LA_TEST_SPOSVX( 'N', 'L', NMAX, NMAX, A, NMAX,
     &          AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &          W, IW, INFO )
              CALL CHKXER( 'SPOSVX', INFOT, NOUT, LERR, OK )
              
              INFO = J + 100*I
              CALL LA_TEST_SPOSVX( 'N', 'L', NMAX, NMAX, A, NMAX,
     &          AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &          W, IW, INFO )
              CALL CHKXER( 'SPOSVX', INFOT, NOUT, LERR, OK )
              
              INFO = J + 100*I
              CALL LA_TEST_SPOSVX( 'E', 'U', NMAX, NMAX, A, NMAX,
     &          AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &          W, IW, INFO )
              CALL CHKXER( 'SPOSVX', INFOT, NOUT, LERR, OK )
              
              INFO = J + 100*I
              CALL LA_TEST_SPOSVX( 'E', 'U', NMAX, NMAX, A, NMAX,
     &          AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &          W, IW, INFO )
              CALL CHKXER( 'SPOSVX', INFOT, NOUT, LERR, OK )
              
              INFO = J + 100*I
              CALL LA_TEST_SPOSVX( 'E', 'L', NMAX, NMAX, A, NMAX,
     &          AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &          W, IW, INFO )
              CALL CHKXER( 'SPOSVX', INFOT, NOUT, LERR, OK )
              
              INFO = J + 100*I
              CALL LA_TEST_SPOSVX( 'E', 'L', NMAX, NMAX, A, NMAX,
     &          AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &          W, IW, INFO )
              CALL CHKXER( 'SPOSVX', INFOT, NOUT, LERR, OK )
            ENDDO
          ENDDO
        ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'PP' ) ) THEN
*
*        SPPSV
*
        SRNAMT = 'SPPSV '
        DO I = 1, 3
          IN(I) = 1
        END DO
        DO I = 1, 3
          INFOT = I
          DO J = 1, IN(I)
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_SPPSV( 'U', NMAX, NRHS, A(1,1), B,
     $                             NMAX, INFO)
              CALL CHKXER( 'SPPSV ', INFOT, NOUT, LERR, OK )
              INFO = J + 100*I
              CALL LA_TEST_SPPSV( 'L', NMAX, NRHS, A(1,1), B,
     $                             NMAX, INFO)
              CALL CHKXER( 'SPPSV ', INFOT, NOUT, LERR, OK )
            END DO
          END DO
        END DO

*
*        SPPSVX
*
        SRNAMT = 'SPPSVX'
        DO I = 1, 10
          INFOT = I
          DO J = 1, 1
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_SPPSVX( 'F', 'U', NMAX, NMAX, A, AF, 'N',
     &          C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPPSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPPSVX( 'F', 'U', NMAX, NMAX, A, AF, 'Y',
     &          C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPPSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPPSVX( 'F', 'L', NMAX, NMAX, A, AF, 'N',
     &          C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPPSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPPSVX( 'F', 'L', NMAX, NMAX, A, AF, 'Y',
     &          C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPPSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPPSVX( 'N', 'U', NMAX, NMAX, A, AF, 'N',
     &          C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPPSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPPSVX( 'N', 'U', NMAX, NMAX, A, AF, 'Y',
     &          C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPPSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 1              00*I
              CALL LA_TEST_SPPSVX( 'N', 'L', NMAX, NMAX, A, AF, 'N',
     &          C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPPSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPPSVX( 'N', 'L', NMAX, NMAX, A, AF, 'Y',
     &          C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPPSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPPSVX( 'E', 'U', NMAX, NMAX, A, AF, 'N',
     &          C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPPSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPPSVX( 'E', 'U', NMAX, NMAX, A, AF, 'Y',
     &          C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPPSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPPSVX( 'E', 'L', NMAX, NMAX, A, AF, 'N',
     &          C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPPSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPPSVX( 'E', 'L', NMAX, NMAX, A, AF, 'Y',
     &          C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPPSVX', INFOT, NOUT, LERR, OK )
            ENDDO
          ENDDO
        ENDDO                                                                   
*
      ELSE IF( LSAMEN( 2, C2, 'PB' ) ) THEN
*
*        SPBSV
*
        SRNAMT = 'SPBSV '
        DO I = 1, 3
          IN(I) = 1
        END DO
        DO I = 1, 3
          INFOT = I
          DO J = 1, IN(I)
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_SPBSV( 'U', NMAX, 1, NRHS, A, NMAX,
     $                               B, NMAX, INFO)
              CALL CHKXER( 'SPBSV ', INFOT, NOUT, LERR, OK )
              INFO = J + 100*I
              CALL LA_TEST_SPBSV( 'L', NMAX, 1, NRHS, A, NMAX,
     $                              B, NMAX, INFO)
              CALL CHKXER( 'SPBSV ', INFOT, NOUT, LERR, OK )
            END DO
          END DO
        END DO  
*
*        SPBSVX
*
        SRNAMT = 'SPBSVX'
        DO I = 1, 10
          IN(I) = 1
        END DO
        DO I = 2, 10
          INFOT = I
          DO J = 1, 1
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_SPBSVX( 'F', 'U', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPBSVX( 'F', 'U', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPBSVX( 'F', 'L', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPBSVX( 'F', 'L', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPBSVX( 'N', 'U', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPBSVX( 'N', 'U', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPBSVX( 'N', 'L', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPBSVX( 'N', 'L', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPBSVX( 'E', 'U', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPBSVX( 'E', 'U', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPBSVX( 'E', 'L', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPBSVX( 'E', 'L', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, IW, INFO )
              CALL CHKXER( 'SPBSVX', INFOT, NOUT, LERR, OK )
            ENDDO
          ENDDO
        ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        SPTSV
*
        SRNAMT = 'SPTSV '
        DO I = 1, 3
          IN(I) = 1
        END DO
        DO I = 2, 3
          INFOT = I
          DO J = 1, 1
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_SPTSV( NMAX, NRHS, A(1,1), A(1,2),
     $                            B, NMAX, INFO )
              CALL CHKXER( 'SPTSV ', INFOT, NOUT, LERR, OK )
            END DO
          END DO
        END DO

*
*        SPTSVX
*
        SRNAMT = 'SPTSVX'
        DO I = 1, 3
          IN(I) = 1
        END DO
        DO I = 2, 3
          INFOT = I
          DO J = 1, 1
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_SPTSVX( 'F', NMAX, NMAX, A( 1, 1 ),
     &          A( 1, 2 ), AF( 1, 1 ), AF( 1, 2 ), B, NMAX, X,
     &          NMAX, RCOND, R1, R2, W, INFO )
              CALL CHKXER( 'SPTSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_SPTSVX( 'N', NMAX, NMAX, A( 1, 1 ),
     &          A( 1, 2 ), AF( 1, 1 ), AF( 1, 2 ), B, NMAX, X,
     &          NMAX, RCOND, R1, R2, W, INFO )
              CALL CHKXER( 'SPTSVX', INFOT, NOUT, LERR, OK )
            ENDDO
          ENDDO
        ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'SY' ) ) THEN
*
*        SSYSV
*
         SRNAMT = 'SSYSV '
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_SSYSV('U', NMAX, NRHS, A, NMAX,
     $                               A(1,2), B, NMAX, W, 1 ,INFO )
               CALL CHKXER( 'SSYSV ', INFOT, NOUT, LERR, OK )
               INFO = J + 100*I
               CALL LA_TEST_SSYSV('L', NMAX, NRHS, A, NMAX,
     $                              A(1,2), B, NMAX, W, 1 ,INFO )
               CALL CHKXER( 'SSYSV ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO  
*
*        SSYSVX
*
         SRNAMT = 'SSYSVX'
         DO I = 1, 9
           IN(I) = 1
         END DO
         DO I = 1, 9
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_SSYSVX( 'N', 'L', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           IW, INFO )
               CALL CHKXER( 'SSYSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSYSVX( 'N', 'U', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           IW, INFO )
               CALL CHKXER( 'SSYSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSYSVX( 'F', 'L', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           IW, INFO )
               CALL CHKXER( 'SSYSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSYSVX( 'F', 'U', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           IW, INFO )
               CALL CHKXER( 'SSYSVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDDO
         ENDDO                                           
*
      ELSE IF( LSAMEN( 2, C2, 'SP' ) ) THEN
*
*        SSPSV
*
         SRNAMT = 'SSPSV '
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_SSPSV( 'U', NMAX, NRHS, A(1,1), A(1,2),
     $                              B, NMAX, INFO)
               CALL CHKXER( 'SSPSV ', INFOT, NOUT, LERR, OK )
               INFO = J + 100*I
               CALL LA_TEST_SSPSV( 'L', NMAX, NRHS, A(1,1), A(1 ,2),
     $                              B, NMAX, INFO)
               CALL CHKXER( 'SSPSV ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO  
*
*        SSPSVX
*
         SRNAMT = 'SSPSVX'
         DO I = 1, 9
           IN(I) = 1
         END DO
         DO I = 1, 9
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_SSPSVX( 'N', 'U', NMAX, NRHS, A, AF, IP, B,
     &           NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
               CALL CHKXER( 'SSPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSPSVX( 'N', 'L', NMAX, NRHS, A, AF, IP, B,
     &           NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
               CALL CHKXER( 'SSPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSPSVX( 'F', 'U', NMAX, NRHS, A, AF, IP, B,
     &           NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
               CALL CHKXER( 'SSPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_SSPSVX( 'F', 'L', NMAX, NRHS, A, AF, IP, B,
     &           NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
               CALL CHKXER( 'SSPSVX', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO

       END IF
*
*     Print a summary line.
*
      IF( PATH1 .NE. PATH ) THEN
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
*     End of SERRVX
*
      END
