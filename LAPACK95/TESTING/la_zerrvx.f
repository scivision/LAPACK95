      SUBROUTINE ZERRVX( PATH, NUNIT )
*
*  -- LAPACK90 test routine (version 1.1) --
*     UNI-C, Denmark
*     April 13, 1999
*
      IMPLICIT NONE
*
*     .. Scalar Arguments ..
      CHARACTER*3        PATH
      INTEGER            NUNIT
*     ..
*
*  Purpose
*  =======
*
*  ZERRVX tests the error exits for the COMPLEX*16 driver routines
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
      INTEGER            I, INFO, J, NRHS, IW
      INTEGER            KL, KU
      DOUBLE PRECISION   RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX ), IN( 100 )
      DOUBLE PRECISION   C( NMAX ), R( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   RW( NMAX )
      COMPLEX*16         A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL CHKXER, LA_TEST_ZGBSV, LA_TEST_ZGBSVX, LA_TEST_ZGESV,
     $         LA_TEST_ZGESVX, LA_TEST_ZGTSV, LA_TEST_ZGTSVX,
     $         LA_TEST_ZHESV,
     $         LA_TEST_ZHESVX, LA_TEST_ZHPSV, LA_TEST_ZHPSVX,
     &         LA_TEST_ZPBSV,
     $         LA_TEST_ZPBSVX, LA_TEST_ZPOSV, LA_TEST_ZPOSVX,
     $         LA_TEST_ZPPSV, LA_TEST_ZPPSVX,
     $         LA_TEST_ZPTSV, LA_TEST_ZPTSVX, LA_TEST_ZSPSV,
     &         LA_TEST_ZSPSVX, LA_TEST_ZSYSV, LA_TEST_ZSYSVX
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
      INTRINSIC          DBLE, DCMPLX
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
            A( I, J ) = DCMPLX( 1.D0 / DBLE( I+J ),
     $                  -1.D0 / DBLE( I+J ) )
            AF( I, J ) = DCMPLX( 1.D0 / DBLE( I+J ),
     $                   -1.D0 / DBLE( I+J ) )
   10    CONTINUE
         B( J ) = 0.D0
         R1( J ) = 0.D0
         R2( J ) = 0.D0
         W( J ) = 0.D0
         X( J ) = 0.D0
         C( J ) = 0.D0
         R( J ) = 0.D0
         IP( J ) = J
   20 CONTINUE
      OK = .TRUE.
*
      IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        ZGESV
*
         SRNAMT = 'ZGESV '
         DO I = 1, 3
            IN(I) = 1
         END DO
         DO I = 1, 3
            INFOT = I
            DO J = 1, IN(I)
               DO NRHS = 1, 2
                  INFO = J + 100*I
                CALL LA_TEST_ZGESV( NMAX, NRHS, A, NMAX, IP, B, NMAX,
     $                 INFO )
                  CALL CHKXER( 'ZGESV ', INFOT, NOUT, LERR, OK )
               END DO
            END DO
         END DO
*
*        ZGESVX
*
         SRNAMT = 'ZGESVX'
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
                  CALL LA_TEST_ZGESVX( CHAR, CHAR, NMAX, NRHS, A, NMAX,
     $               AF, NMAX, IP, EQ, R, C, B, NMAX, X, NMAX, RCOND,
     $               R1, R2, W, IW, INFO)
                  CALL CHKXER( 'ZGESVX', INFOT, NOUT, LERR, OK )
               END DO
            END DO
         END DO
*
      ELSE IF( LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        ZGBSV
*
         SRNAMT = 'ZGBSV '
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I
               KL = (NMAX-1)/3
               KU = NMAX-2*KL-1
               CALL LA_TEST_ZGBSV( NMAX, KL, KU, NRHS, A, NMAX, IP, B,
     $                              NMAX, INFO )
               CALL CHKXER( 'ZGBSV ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO  
*
*        ZGBSVX
*
         SRNAMT = 'ZGBSVX'
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
                 CALL LA_TEST_ZGBSVX( 'F', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'R'
                 CALL LA_TEST_ZGBSVX( 'F', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_ZGBSVX( 'F', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 IEQUED = 'B'
                 CALL LA_TEST_ZGBSVX( 'F', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'N'
                 CALL LA_TEST_ZGBSVX( 'F', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'R'
                 CALL LA_TEST_ZGBSVX( 'F', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_ZGBSVX( 'F', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'B'
                 CALL LA_TEST_ZGBSVX( 'F', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'N'
                 CALL LA_TEST_ZGBSVX( 'F', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 IEQUED = 'R'
                 CALL LA_TEST_ZGBSVX( 'F', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_ZGBSVX( 'F', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'B'
                 CALL LA_TEST_ZGBSVX( 'F', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'N'
                 CALL LA_TEST_ZGBSVX( 'N', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'R'
                 CALL LA_TEST_ZGBSVX( 'N', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_ZGBSVX( 'N', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 IEQUED = 'B'
                 CALL LA_TEST_ZGBSVX( 'N', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'N'
                 CALL LA_TEST_ZGBSVX( 'N', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'R'
                 CALL LA_TEST_ZGBSVX( 'N', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_ZGBSVX( 'N', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'B'
                 CALL LA_TEST_ZGBSVX( 'N', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'N'
                 CALL LA_TEST_ZGBSVX( 'N', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
                 
                 INFO = J + 100*I
                 IEQUED = 'R'
                 CALL LA_TEST_ZGBSVX( 'N', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_ZGBSVX( 'N', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'B'
                 CALL LA_TEST_ZGBSVX( 'N', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'N'
                 CALL LA_TEST_ZGBSVX( 'E', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'R'
                 CALL LA_TEST_ZGBSVX( 'E', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_ZGBSVX( 'E', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
                 INFO = J + 1                 00*I
                 IEQUED = 'B'
                 CALL LA_TEST_ZGBSVX( 'E', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'N'
                 CALL LA_TEST_ZGBSVX( 'E', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'R'
                 CALL LA_TEST_ZGBSVX( 'E', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_ZGBSVX( 'E', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'B'
                 CALL LA_TEST_ZGBSVX( 'E', 'N', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'N'
                 CALL LA_TEST_ZGBSVX( 'E', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'R'
                 CALL LA_TEST_ZGBSVX( 'E', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_ZGBSVX( 'E', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'B'
                 CALL LA_TEST_ZGBSVX( 'E', 'C', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
               ENDIF
             ENDDO
           ENDDO
         ENDDO
         
*
      ELSE IF( LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        ZGTSV
*
         SRNAMT = 'ZGTSV '
         DO I = 1, 4
           IF (I/=2)  THEN
             INFOT = I
             DO J = 1, 1
               DO NRHS = 1, 2
                 INFO = J + 100*I
                 CALL LA_TEST_ZGTSV( NMAX, NRHS, A(1,1), A(1,2),A(1, 3),
     $                               B, NMAX, INFO )
                 CALL CHKXER( 'XGTSV ', INFOT, NOUT, LERR, OK )
               END DO
             END DO
           END IF
         END DO 
*
*        ZGTSVX
*
         SRNAMT = 'ZGTSVX'
         DO I = 1, 14
           IF (I/=2)  THEN
             INFOT = I
             DO J = 1, 1
               DO NRHS = 1, 2
                 INFO = J + 100*I
                 CALL LA_TEST_ZGTSVX( 'N', 'N', NMAX, NMAX, A( 1, 1 ),
     &             A( 1, 2 ), A( 1, 3 ),
     &             AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &             IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGTSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 CALL LA_TEST_ZGTSVX( 'F', 'N', NMAX, NMAX, A( 1, 1 ),
     &             A( 1, 2 ), A( 1, 3 ),
     &             AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &             IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGTSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 CALL LA_TEST_ZGTSVX( 'N', 'T', NMAX, NMAX, A( 1, 1 ),
     &             A( 1, 2 ), A( 1, 3 ),
     &             AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &             IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGTSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 CALL LA_TEST_ZGTSVX( 'F', 'T', NMAX, NMAX, A( 1, 1 ),
     &             A( 1, 2 ), A( 1, 3 ),
     &             AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &             IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGTSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 CALL LA_TEST_ZGTSVX( 'N', 'C', NMAX, NMAX, A( 1, 1 ),
     &             A( 1, 2 ), A( 1, 3 ),
     &             AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &             IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGTSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 CALL LA_TEST_ZGTSVX( 'F', 'C', NMAX, NMAX, A( 1, 1 ),
     &             A( 1, 2 ), A( 1, 3 ),
     &             AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &             IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
                 CALL CHKXER( 'ZGTSVX', INFOT, NOUT, LERR, OK )
               ENDDO
             ENDDO
           ENDIF
         ENDDO
         
*
      ELSE IF( LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        ZPOSV
*
         SRNAMT = 'ZPOSV '
         DO I = 1, 3
           IN(I) = 1
         END DO
         DO I = 1, 3
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_ZPOSV( 'U', NMAX, NRHS, A, NMAX, B, NMAX,
     $                                INFO )
               CALL CHKXER( 'ZPOSV ', INFOT, NOUT, LERR, OK )
               INFO = J + 100*I
               CALL LA_TEST_ZPOSV( 'L', NMAX, NRHS, A, NMAX, B, NMAX,
     $                                INFO )
               CALL CHKXER( 'ZPOSV ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO  
*
*        ZPOSVX
*
         SRNAMT = 'ZPOSVX'
         DO I = 1, 10
           IN(I) = 1
         END DO
         DO I = 1, 10
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_ZPOSVX( 'F', 'U', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPOSVX( 'F', 'U', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPOSVX( 'F', 'L', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPOSVX( 'F', 'L', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPOSVX( 'N', 'U', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPOSVX( 'N', 'U', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_ZPOSVX( 'N', 'L', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPOSVX( 'N', 'L', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPOSVX( 'E', 'U', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPOSVX( 'E', 'U', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPOSVX( 'E', 'L', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPOSVX( 'E', 'L', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, RW, INFO )
               CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDDO
         ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'PP' ) ) THEN
*
*        ZPPSV
*
         SRNAMT = 'ZPPSV '
         DO I = 1, 3
           IN(I) = 1
         END DO
         DO I = 1, 3
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_ZPPSV( 'U', NMAX, NRHS, A(1,1), B,
     $                              NMAX, INFO)

               CALL CHKXER( 'ZPPSV ', INFOT, NOUT, LERR, OK )
               INFO = J + 100*I
               CALL LA_TEST_ZPPSV( 'L', NMAX, NRHS, A(1,1), B,
     $                             NMAX, INFO)
               CALL CHKXER( 'ZPPSV ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO 
*
*        ZPPSVX
*
         SRNAMT = 'ZPPSVX'
         DO I = 1, 10
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_ZPPSVX( 'F', 'U', NMAX, NMAX, A, AF, 'N',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPPSVX( 'F', 'U', NMAX, NMAX, A, AF, 'Y',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPPSVX( 'F', 'L', NMAX, NMAX, A, AF, 'N',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPPSVX( 'F', 'L', NMAX, NMAX, A, AF, 'Y',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPPSVX( 'N', 'U', NMAX, NMAX, A, AF, 'N',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPPSVX( 'N', 'U', NMAX, NMAX, A, AF, 'Y',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPPSVX( 'N', 'L', NMAX, NMAX, A, AF, 'N',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 1               00*I
               CALL LA_TEST_ZPPSVX( 'N', 'L', NMAX, NMAX, A, AF, 'Y',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPPSVX( 'E', 'U', NMAX, NMAX, A, AF, 'N',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPPSVX( 'E', 'U', NMAX, NMAX, A, AF, 'Y',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPPSVX( 'E', 'L', NMAX, NMAX, A, AF, 'N',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZPPSVX( 'E', 'L', NMAX, NMAX, A, AF, 'Y',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, RW, INFO )
               CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDDO
         ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'PB' ) ) THEN
*
*        ZPBSV
*
        SRNAMT = 'ZPBSV '
        DO I = 1, 3
          IN(I) = 1
        END DO
        DO I = 1, 3
          INFOT = I
          DO J = 1, IN(I)
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_ZPBSV( 'U', NMAX, 1, NRHS, A, NMAX,
     $                               B, NMAX, INFO)
              CALL CHKXER( 'ZPBSV ', INFOT, NOUT, LERR, OK )
              INFO = J + 100*I
              CALL LA_TEST_ZPBSV( 'L', NMAX, 1, NRHS, A, NMAX,
     $                              B, NMAX, INFO)
              CALL CHKXER( 'ZPBSV ', INFOT, NOUT, LERR, OK )
            END DO
          END DO
        END DO    
*
*        ZPBSVX
*
        SRNAMT = 'ZPBSVX'
        DO I = 1, 10
          IN(I) = 1
        END DO
        DO I = 2, 10
          INFOT = I
          DO J = 1, 1
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_ZPBSVX( 'F', 'U', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, RW, INFO )
              CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZPBSVX( 'F', 'U', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, RW, INFO )
              CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZPBSVX( 'F', 'L', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, RW, INFO )
              CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZPBSVX( 'F', 'L', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, RW, INFO )
              CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZPBSVX( 'N', 'U', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, RW, INFO )
              CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZPBSVX( 'N', 'U', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, RW, INFO )
              CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
              
              INFO = J + 100*I
              CALL LA_TEST_ZPBSVX( 'N', 'L', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, RW, INFO )
              CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZPBSVX( 'N', 'L', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, RW, INFO )
              CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZPBSVX( 'E', 'U', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, RW, INFO )
              CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZPBSVX( 'E', 'U', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, RW, INFO )
              CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZPBSVX( 'E', 'L', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, RW, INFO )
              CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZPBSVX( 'E', 'L', NMAX, NMAX-1, NMAX, A,
     &          NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND,
     &          R1, R2, W, RW, INFO )
              CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
            ENDDO
          ENDDO
        ENDDO
        


        
         INFOT = 1
         CALL ZPBSVX( '/', 'U', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZPBSVX( 'N', '/', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZPBSVX( 'N', 'U', -1, 0, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZPBSVX( 'N', 'U', 1, -1, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZPBSVX( 'N', 'U', 0, 0, -1, A, 1, AF, 1, EQ, C, B, 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZPBSVX( 'N', 'U', 1, 1, 0, A, 1, AF, 2, EQ, C, B, 2, X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZPBSVX( 'N', 'U', 1, 1, 0, A, 2, AF, 1, EQ, C, B, 2, X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = '/'
         CALL ZPBSVX( 'F', 'U', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ = 'Y'
         CALL ZPBSVX( 'F', 'U', 1, 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL ZPBSVX( 'N', 'U', 2, 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL ZPBSVX( 'N', 'U', 2, 0, 0, A, 1, AF, 1, EQ, C, B, 2, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        ZPTSV
*
        SRNAMT = 'ZPTSV '
        DO I = 1, 3
          IN(I) = 1
        END DO
        DO I = 2, 3
          INFOT = I
          DO J = 1, 1
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_ZPTSV( NMAX, NRHS, A(1,1), A(1,2),
     $                            B, NMAX, INFO )
              CALL CHKXER( 'ZPTSV ', INFOT, NOUT, LERR, OK )
            END DO
          END DO
        END DO  
*
*        ZPTSVX
*
        SRNAMT = 'ZPTSVX'
        DO I = 2, 9
          IN(I) = 1
        END DO
        DO I = 2, 9
          INFOT = I
          DO J = 1, 1
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_ZPTSVX( 'F', NMAX, NMAX, A( 1, 1 ),
     &          A( 1, 2 ), AF( 1, 1 ), AF( 1, 2 ), B, NMAX, X,
     &          NMAX, RCOND, R1, R2, W, RW, INFO )
              CALL CHKXER( 'ZPTSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZPTSVX( 'N', NMAX, NMAX, A( 1, 1 ),
     &          A( 1, 2 ), AF( 1, 1 ), AF( 1, 2 ), B, NMAX, X,
     &          NMAX, RCOND, R1, R2, W, RW, INFO )
              CALL CHKXER( 'ZPTSVX', INFOT, NOUT, LERR, OK )
            ENDDO
          ENDDO
        ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'HE' ) ) THEN
*
*        ZHESV
*
        SRNAMT = 'ZHESV '
        DO I = 1, 4
          IN(I) = 1
        END DO
        DO I = 1, 4
          INFOT = I
          DO J = 1, IN(I)
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_ZHESV( 'U', NMAX, NRHS,  A, NMAX, A(1,2),
     $                               B, NMAX, W, 1, INFO)
              CALL CHKXER( 'ZHESV ', INFOT, NOUT, LERR, OK )
              INFO = J + 100*I
              CALL LA_TEST_ZHESV( 'L', NMAX, NRHS, A, NMAX, A(1,2),
     $                              B, NMAX, W, 1,INFO)
              CALL CHKXER( 'ZHESV ', INFOT, NOUT, LERR, OK )
            END DO
          END DO
        END DO  
*
*        ZHESVX
*
        SRNAMT = 'ZHESVX'
         DO I = 1, 9
           IN(I) = 1
         END DO
         DO I = 1, 9
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_ZHESVX( 'N', 'L', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           RW, INFO )
               CALL CHKXER( 'ZHESVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHESVX( 'N', 'U', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           RW, INFO )
               CALL CHKXER( 'ZHESVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHESVX( 'F', 'L', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           RW, INFO )
               CALL CHKXER( 'ZHESVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZHESVX( 'F', 'U', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           RW, INFO )
               CALL CHKXER( 'ZHESVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDDO
         ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'HP' ) ) THEN
*
*        ZHPSV
*
         SRNAMT = 'ZHPSV '
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_ZHPSV( 'U', NMAX, NRHS, A(1,1), A(1,2),
     $                              B, NMAX, INFO)
               CALL CHKXER( 'ZHPSV ', INFOT, NOUT, LERR, OK )
               INFO = J + 100*I
               CALL LA_TEST_ZHPSV( 'L', NMAX, NRHS, A(1,1), A(1 ,2),
     $                              B, NMAX, INFO)
               CALL CHKXER( 'ZHPSV ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO  
*
*        ZHPSVX
*
         SRNAMT = 'ZHPSVX'
         DO I = 1, 9
           IN(I) = 1
         END DO
         DO I = 1, 9
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_ZHPSVX( 'N', 'U', NMAX, NRHS, A, AF, IP, B,
     &           NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
               CALL CHKXER( 'ZHPSVX', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_ZHPSVX( 'N', 'L', NMAX, NRHS, A, AF, IP, B,
     &           NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
               CALL CHKXER( 'ZHPSVX', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_ZHPSVX( 'F', 'U', NMAX, NRHS, A, AF, IP, B,
     &           NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
               CALL CHKXER( 'ZHPSVX', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_ZHPSVX( 'F', 'L', NMAX, NRHS, A, AF, IP, B,
     &           NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
               CALL CHKXER( 'ZHPSVX', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO
*
      ELSE IF( LSAMEN( 2, C2, 'SY' ) ) THEN
*
*        ZSYSV
*
         SRNAMT = 'ZSYSV '
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_ZSYSV('U', NMAX, NRHS, A, NMAX,
     $                               A(1,2), B, NMAX, W, 1 ,INFO )
               CALL CHKXER( 'ZSYSV ', INFOT, NOUT, LERR, OK )
               INFO = J + 100*I
               CALL LA_TEST_ZSYSV('L', NMAX, NRHS, A, NMAX,
     $                               A(1,2), B, NMAX, W, 1 ,INFO )
               CALL CHKXER( 'ZSYSV ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO  
*
*        ZSYSVX
*
         SRNAMT = 'ZSYSVX'
         DO I = 1, 9
           IN(I) = 1
         END DO
         DO I = 1, 9
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_ZSYSVX( 'N', 'L', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           RW, INFO )
               CALL CHKXER( 'ZSYSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZSYSVX( 'N', 'U', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           RW, INFO )
               CALL CHKXER( 'ZSYSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZSYSVX( 'F', 'L', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           RW, INFO )
               CALL CHKXER( 'ZSYSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_ZSYSVX( 'F', 'U', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           RW, INFO )
               CALL CHKXER( 'ZSYSVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDDO
         ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'SP' ) ) THEN
*
*        ZSPSV
*
        SRNAMT = 'ZSPSV '
        DO I = 1, 4
          IN(I) = 1
        END DO
        DO I = 1, 4
          INFOT = I
          DO J = 1, IN(I)
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_ZSPSV( 'U', NMAX, NRHS, A(1,1), A(1,2),
     $                              B, NMAX, INFO)
              CALL CHKXER( 'ZSPSV ', INFOT, NOUT, LERR, OK )
              INFO = J + 100*I
              CALL LA_TEST_ZSPSV( 'L', NMAX, NRHS, A(1,1), A(1 ,2),
     $                              B, NMAX, INFO)
              CALL CHKXER( 'ZSPSV ', INFOT, NOUT, LERR, OK )
            END DO
          END DO
        END DO  
*
*        ZSPSVX
*
        SRNAMT = 'ZSPSVX'
        DO I = 1, 9
          IN(I) = 1
        END DO
        DO I = 1, 9
          INFOT = I
          DO J = 1, IN(I)
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_ZSPSVX( 'N', 'U', NMAX, NRHS, A, AF, IP, B,
     &          NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
              CALL CHKXER( 'ZSPSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZSPSVX( 'N', 'L', NMAX, NRHS, A, AF, IP, B,
     &          NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
              CALL CHKXER( 'ZSPSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZSPSVX( 'F', 'U', NMAX, NRHS, A, AF, IP, B,
     &          NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
              CALL CHKXER( 'ZSPSVX', INFOT, NOUT, LERR, OK )

              INFO = J + 100*I
              CALL LA_TEST_ZSPSVX( 'F', 'L', NMAX, NRHS, A, AF, IP, B,
     &          NMAX, X, NMAX, RCOND, R1, R2, W, RW, INFO )
              CALL CHKXER( 'ZSPSVX', INFOT, NOUT, LERR, OK )
            END DO
          END DO
        END DO
        
      END IF
*
*     Print a summary line.
*
      IF( PATH1 /= PATH ) THEN
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
*     End of ZERRVX
*
      END
