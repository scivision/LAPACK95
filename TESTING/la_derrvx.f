      SUBROUTINE DERRVX( PATH, NUNIT )
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
*  DERRVX tests the error exits for the DOUBLE PRECISION driver routines
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
      INTEGER            NMAX
      PARAMETER          ( NMAX = 4 )
*     ..
*     .. Local Scalars ..
      CHARACTER          EQ
      CHARACTER*2        C2
      CHARACTER*1        CHAR, IEQUED
      INTEGER            I, INFO, J, NRHS
      INTEGER		 KL, KU
      DOUBLE PRECISION   RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX ), IW( NMAX ), IN( 100 )
      DOUBLE PRECISION   A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   C( NMAX ), R( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   W( 2*NMAX ), X( NMAX )

*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHKXER, LA_TEST_DGBSV, DGBSVX, LA_TEST_DGESV,
     $                   LA_TEST_DGESVX, LA_TEST_DGTSV,
     $                   DGTSVX, LA_TEST_DPBSV, LA_TEST_DPBSVX,
     $                   LA_TEST_DPOSV, LA_TEST_DPOSVX, LA_TEST_DPPSV,
     $                   LA_TEST_DPPSVX, LA_TEST_DPTSV, LA_TEST_DPTSVX,
     $                   LA_TEST_DSPSV,
     $                   LA_TEST_DSPSVX, LA_TEST_DSYSV, LA_TEST_DSYSVX
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*6        SRNAMT
      INTEGER            INFOT, NOUT, INFOTC
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, NOUT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
      COMMON /LINFO90/ INFOTC
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      WRITE( NOUT, FMT = * )
      C2 = PATH( 2: 3 )

!     .. Data statements ..



*
*     Set the variables to innocuous values.
*
      DO 20 J = 1, NMAX
         DO 10 I = 1, NMAX
            A( I, J ) = 1.D0 / DBLE( I+J )
            AF( I, J ) = 1.D0 / DBLE( I+J )
   10    CONTINUE
         B( J ) = 0.D0
         R1( J ) = 0.D0
         R2( J ) = 0.D0
         W( J ) = 0.D0
         X( J ) = 0.D0
*          
         C( J ) = 0.1D0
         R( J ) = 0.D0
         IP( J ) = J
   20 CONTINUE
      OK = .TRUE.
*
      IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        GESV 
*
         SRNAMT = 'DGESV '
         DO I = 1, 3
            IN(I) = 1
         END DO
         DO I = 1, 3
            INFOT = I
            DO J = 1, IN(I)
               DO NRHS = 1, 2
                  INFO = J + 100*I
                  CALL LA_TEST_DGESV( NMAX, NRHS, A, NMAX, IP, B, NMAX,
     $                 INFO )
                  CALL CHKXER( 'DGESV ', INFOT, NOUT, LERR, OK )
               END DO
            END DO
         END DO
*
*        DGESVX
*
         SRNAMT = 'DGESVX'
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
                  CALL LA_TEST_DGESVX( CHAR, CHAR, NMAX, NRHS, A, NMAX,
     $               AF, NMAX, IP, EQ, R, C, B, NMAX, X, NMAX, RCOND,
     $               R1, R2, W, IW, INFO)
                  CALL CHKXER( 'DGESVX', INFOT, NOUT, LERR, OK )
               END DO
            END DO
         END DO
*
      ELSE IF( LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        DGBSV
*
         SRNAMT = 'DGBSV '
         DO I = 1, 4
           INFOT = I
            DO J = 1, 1
               DO NRHS = 1, 2
                 INFO = J + 100*I
                 KL = (NMAX-1)/3
                 KU = NMAX-2*KL-1
                 CALL LA_TEST_DGBSV( NMAX, KL, KU, NRHS, A, NMAX, IP, B,
     $             NMAX, INFO )
                  CALL CHKXER( 'DGBSV ', INFOT, NOUT, LERR, OK )
               END DO
            END DO
         END DO
*
*        DGBSVX
*
         SRNAMT = 'DGBSVX'
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
                  CALL LA_TEST_DGBSVX( 'F', 'T', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                  INFO = J + 100*I
                  IEQUED = 'R'
                  CALL LA_TEST_DGBSVX( 'F', 'T', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                 CALL LA_TEST_DGBSVX( 'F', 'T', NMAX, NMAX, NMAX, NRHS,
     &             A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &             B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                 CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )
                 
                INFO = J + 100*I
                IEQUED = 'B'
                  CALL LA_TEST_DGBSVX( 'F', 'T', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
               CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )
                  
                  INFO = J + 100*I
                  IEQUED = 'N' 
                  CALL LA_TEST_DGBSVX( 'F', 'N', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                  INFO = J + 100*I
                  IEQUED = 'R'
                  CALL LA_TEST_DGBSVX( 'F', 'N', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                IEQUED = 'C'
                  CALL LA_TEST_DGBSVX( 'F', 'N', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                  INFO = J + 100*I
                  IEQUED = 'B'
                  CALL LA_TEST_DGBSVX( 'F', 'N', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )
                   
                  INFO = J + 100*I
                  IEQUED = 'N' 
                  CALL LA_TEST_DGBSVX( 'F', 'C', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                  INFO = J + 100*I
                  IEQUED = 'R'
                  CALL LA_TEST_DGBSVX( 'F', 'C', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                IEQUED = 'C'
                  CALL LA_TEST_DGBSVX( 'F', 'C', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'B'
                  CALL LA_TEST_DGBSVX( 'F', 'C', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'N'
                  CALL LA_TEST_DGBSVX( 'N', 'T', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                  INFO = J + 100*I
                  IEQUED = 'R'
                  CALL LA_TEST_DGBSVX( 'N', 'T', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                IEQUED = 'C'
                  CALL LA_TEST_DGBSVX( 'N', 'T', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'B'
                  CALL LA_TEST_DGBSVX( 'N', 'T', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
               CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )
                  
                  INFO = J + 100*I
                  IEQUED = 'N' 
                  CALL LA_TEST_DGBSVX( 'N', 'N', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                  INFO = J + 100*I
                  IEQUED = 'R'
                  CALL LA_TEST_DGBSVX( 'N', 'N', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                 IEQUED = 'C'
                  CALL LA_TEST_DGBSVX( 'N', 'N', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'B'
                  CALL LA_TEST_DGBSVX( 'N', 'N', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
               CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )
                   
                  INFO = J + 100*I
                  IEQUED = 'N' 
                  CALL LA_TEST_DGBSVX( 'N', 'C', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                  INFO = J + 100*I
                  IEQUED = 'R'
                  CALL LA_TEST_DGBSVX( 'N', 'C', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                IEQUED = 'C'
                  CALL LA_TEST_DGBSVX( 'N', 'C', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                  INFO = J + 100*I
                IEQUED = 'B'
                  CALL LA_TEST_DGBSVX( 'N', 'C', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
               CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'N'
                  CALL LA_TEST_DGBSVX( 'E', 'T', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                  INFO = J + 100*I
                  IEQUED = 'R'
                  CALL LA_TEST_DGBSVX( 'E', 'T', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                IEQUED = 'C'
                  CALL LA_TEST_DGBSVX( 'E', 'T', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'B'
                  CALL LA_TEST_DGBSVX( 'E', 'T', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )
                  
                  INFO = J + 100*I
                  IEQUED = 'N' 
                  CALL LA_TEST_DGBSVX( 'E', 'N', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                  INFO = J + 100*I
                  IEQUED = 'R'
                  CALL LA_TEST_DGBSVX( 'E', 'N', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                IEQUED = 'C'
                  CALL LA_TEST_DGBSVX( 'E', 'N', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'B'
                  CALL LA_TEST_DGBSVX( 'E', 'N', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )
                   
                  INFO = J + 100*I
                  IEQUED = 'N' 
                  CALL LA_TEST_DGBSVX( 'E', 'C', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                  INFO = J + 100*I
                  IEQUED = 'R'
                  CALL LA_TEST_DGBSVX( 'E', 'C', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                 INFO = J + 100*I
                IEQUED = 'C'
                  CALL LA_TEST_DGBSVX( 'E', 'C', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                  CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I
                IEQUED = 'B'
                  CALL LA_TEST_DGBSVX( 'E', 'C', NMAX, NMAX, NMAX, NRHS,
     &              A, NMAX, AF, NMAX, IP, IEQUED, R, C,
     &              B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
               CALL CHKXER( 'DGBSVX', INFOT, NOUT, LERR, OK )

                END IF
              ENDDO
            ENDDO
          ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        DGTSV
*
        
        SRNAMT = 'DGTSV '
        DO I = 1, 4
          IF (I/=2)  THEN 
            INFOT = I
            DO J = 1, 1
              DO NRHS = 1, 2
                INFO = J + 100*I
                CALL LA_TEST_DGTSV( NMAX, NRHS, A(1,1), A(1,2),A(1, 3),
     $            B, NMAX, INFO )
                CALL CHKXER( 'DGTSV ', INFOT, NOUT, LERR, OK )
              END DO
            END DO
          END IF
        END DO       

*
*        DGTSVX
*
        SRNAMT = 'DGTSVX'
        DO I = 1, 14
          IF (I/=2)  THEN
            INFOT = I
            DO J = 1, 1
              DO NRHS = 1, 2
                INFO = J + 100*I 
                CALL LA_TEST_DGTSVX( 'N', 'N', NMAX, NMAX, A( 1, 1 ),
     &            A( 1, 2 ), A( 1, 3 ),
     &            AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &            IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'DGTSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I  
                CALL LA_TEST_DGTSVX( 'F', 'N', NMAX, NMAX, A( 1, 1 ),
     &            A( 1, 2 ), A( 1, 3 ),
     &            AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &            IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'DGTSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I 
                CALL LA_TEST_DGTSVX( 'N', 'T', NMAX, NMAX, A( 1, 1 ),
     &            A( 1, 2 ), A( 1, 3 ),
     &            AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &            IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'DGTSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I  
                CALL LA_TEST_DGTSVX( 'F', 'T', NMAX, NMAX, A( 1, 1 ),
     &            A( 1, 2 ), A( 1, 3 ),
     &            AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &            IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'DGTSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I 
                CALL LA_TEST_DGTSVX( 'N', 'C', NMAX, NMAX, A( 1, 1 ),
     &            A( 1, 2 ), A( 1, 3 ),
     &            AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &            IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'DGTSVX', INFOT, NOUT, LERR, OK )

                INFO = J + 100*I  
                CALL LA_TEST_DGTSVX( 'F', 'C', NMAX, NMAX, A( 1, 1 ),
     &            A( 1, 2 ), A( 1, 3 ),
     &            AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     &            IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
                CALL CHKXER( 'DGTSVX', INFOT, NOUT, LERR, OK )
                
              ENDDO
            ENDDO
          ENDIF
        ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        DPOSV
*
        SRNAMT = 'DPOSV '
        DO I = 1, 3
          IN(I) = 1
        END DO
        DO I = 1, 3
          INFOT = I
          DO J = 1, IN(I)
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_DPOSV( 'U', NMAX, NRHS, A, NMAX, B, NMAX,
     $                 INFO )
              CALL CHKXER( 'DPOSV ', INFOT, NOUT, LERR, OK )
              INFO = J + 100*I 
              CALL LA_TEST_DPOSV( 'L', NMAX, NRHS, A, NMAX, B, NMAX,
     $                 INFO )
              CALL CHKXER( 'DPOSV ', INFOT, NOUT, LERR, OK )
            END DO
          END DO
        END DO     
*
*        DPOSVX
*
         SRNAMT = 'DPOSVX'
         DO I = 1, 10
           IN(I) = 1
         END DO
         DO I = 1, 10
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_DPOSVX( 'F', 'U', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, IW, INFO )
               CALL CHKXER( 'DPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPOSVX( 'F', 'U', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, IW, INFO )
               CALL CHKXER( 'DPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPOSVX( 'F', 'L', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, IW, INFO )
               CALL CHKXER( 'DPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPOSVX( 'F', 'L', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, IW, INFO )
               CALL CHKXER( 'DPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPOSVX( 'N', 'U', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, IW, INFO )
               CALL CHKXER( 'DPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPOSVX( 'N', 'U', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, IW, INFO )
               CALL CHKXER( 'DPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPOSVX( 'N', 'L', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, IW, INFO )
               CALL CHKXER( 'DPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPOSVX( 'N', 'L', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, IW, INFO )
               CALL CHKXER( 'DPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPOSVX( 'E', 'U', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, IW, INFO )
               CALL CHKXER( 'DPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPOSVX( 'E', 'U', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, IW, INFO )
               CALL CHKXER( 'DPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPOSVX( 'E', 'L', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, IW, INFO )
               CALL CHKXER( 'DPOSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPOSVX( 'E', 'L', NMAX, NMAX, A, NMAX,
     &           AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, R1, R2,
     &           W, IW, INFO )
               CALL CHKXER( 'DPOSVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDDO
         ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'PP' ) ) THEN
*
*        DPPSV
*
         SRNAMT = 'DPPSV '

         DO I = 1, 3
           IN(I) = 1
         END DO
         DO I = 1, 3
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_DPPSV( 'U', NMAX, NRHS, A(1,1), B,
     $                             NMAX, INFO)
               CALL CHKXER( 'DPPSV ', INFOT, NOUT, LERR, OK )
               INFO = J + 100*I
               CALL LA_TEST_DPPSV( 'L', NMAX, NRHS, A(1,1), B,
     $                             NMAX, INFO)
               CALL CHKXER( 'DPPSV ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO         
*
*        DPPSVX
*
         SRNAMT = 'DPPSVX'
         DO I = 1, 10
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_DPPSVX( 'F', 'U', NMAX, NMAX, A, AF, 'N',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPPSVX( 'F', 'U', NMAX, NMAX, A, AF, 'Y',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPPSVX( 'F', 'L', NMAX, NMAX, A, AF, 'N',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPPSVX( 'F', 'L', NMAX, NMAX, A, AF, 'Y',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPPSVX( 'N', 'U', NMAX, NMAX, A, AF, 'N',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPPSVX( 'N', 'U', NMAX, NMAX, A, AF, 'Y',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPPSVX( 'N', 'L', NMAX, NMAX, A, AF, 'N',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPPSVX( 'N', 'L', NMAX, NMAX, A, AF, 'Y',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPPSVX( 'E', 'U', NMAX, NMAX, A, AF, 'N',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPPSVX( 'E', 'U', NMAX, NMAX, A, AF, 'Y',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPPSVX( 'E', 'L', NMAX, NMAX, A, AF, 'N',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPPSVX( 'E', 'L', NMAX, NMAX, A, AF, 'Y',
     &           C, B, NMAX, X, NMAX, RCOND,
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPPSVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDDO
         ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'PB' ) ) THEN
*
*        DPBSV
*
         SRNAMT = 'DPBSV '

         DO I = 1, 3
           IN(I) = 1
         END DO
         DO I = 1, 3
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
                CALL LA_TEST_DPBSV( 'U', NMAX, 1, NRHS, A, NMAX,
     $                               B, NMAX, INFO)
               CALL CHKXER( 'DPBSV ', INFOT, NOUT, LERR, OK )
               INFO = J + 100*I
               CALL LA_TEST_DPBSV( 'L', NMAX, 1, NRHS, A, NMAX,
     $                              B, NMAX, INFO)
                CALL CHKXER( 'DPBSV ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO          

*
*        DPBSVX
*
         SRNAMT = 'DPBSVX'
         DO I = 1, 10
           IN(I) = 1
         END DO
         DO I = 2, 10
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I 
               CALL LA_TEST_DPBSVX( 'F', 'U', NMAX, NMAX-1, NMAX, A, 
     &           NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, 
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPBSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPBSVX( 'F', 'U', NMAX, NMAX-1, NMAX, A, 
     &           NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, 
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPBSVX', INFOT, NOUT, LERR, OK )  

               INFO = J + 100*I
               CALL LA_TEST_DPBSVX( 'F', 'L', NMAX, NMAX-1, NMAX, A, 
     &           NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, 
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPBSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPBSVX( 'F', 'L', NMAX, NMAX-1, NMAX, A, 
     &           NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, 
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPBSVX', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DPBSVX( 'N', 'U', NMAX, NMAX-1, NMAX, A, 
     &           NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, 
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPBSVX', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DPBSVX( 'N', 'U', NMAX, NMAX-1, NMAX, A, 
     &           NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, 
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPBSVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_DPBSVX( 'N', 'L', NMAX, NMAX-1, NMAX, A, 
     &           NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, 
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPBSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPBSVX( 'N', 'L', NMAX, NMAX-1, NMAX, A, 
     &           NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, 
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPBSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPBSVX( 'E', 'U', NMAX, NMAX-1, NMAX, A, 
     &           NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, 
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPBSVX', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DPBSVX( 'E', 'U', NMAX, NMAX-1, NMAX, A, 
     &           NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, 
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPBSVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_DPBSVX( 'E', 'L', NMAX, NMAX-1, NMAX, A, 
     &           NMAX, AF, NMAX, 'N', C, B, NMAX, X, NMAX, RCOND, 
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPBSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPBSVX( 'E', 'L', NMAX, NMAX-1, NMAX, A, 
     &           NMAX, AF, NMAX, 'Y', C, B, NMAX, X, NMAX, RCOND, 
     &           R1, R2, W, IW, INFO )
               CALL CHKXER( 'DPBSVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDDO
         ENDDO

*
      ELSE IF( LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        DPTSV
*
        SRNAMT = 'DPTSV '
        DO I = 1, 3
          IN(I) = 1
        END DO 
         DO I = 2, 3
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_DPTSV( NMAX, NRHS, A(1,1), A(1,2),
     $                            B, NMAX, INFO )
               CALL CHKXER( 'DPTSV ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO 
*
*        DPTSVX
*
         SRNAMT = 'DPTSVX'
         DO I = 1, 3
           IN(I) = 1
         END DO
         DO I = 2, 3
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_DPTSVX( 'F', NMAX, NMAX, A( 1, 1 ),
     &           A( 1, 2 ), AF( 1, 1 ), AF( 1, 2 ), B, NMAX, X,
     &           NMAX, RCOND, R1, R2, W, INFO ) 
               CALL CHKXER( 'DPTSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DPTSVX( 'N', NMAX, NMAX, A( 1, 1 ),
     &           A( 1, 2 ), AF( 1, 1 ), AF( 1, 2 ), B, NMAX, X,
     &           NMAX, RCOND, R1, R2, W, INFO ) 
               CALL CHKXER( 'DPTSVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDDO
         ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'SY' ) ) THEN
*
*        DSYSV
*
         SRNAMT = 'DSYSV '
         DO I = 1, 4
           IN(I) = 1
         END DO   
         DO I = 1, 4
              INFOT = I
             DO J = 1, 1
               DO NRHS = 1, 2
                 INFO = J + 100*I
                 CALL LA_TEST_DSYSV('U', NMAX, NRHS, A, NMAX,
     $                               A(1,2), B, NMAX, W, 1 ,INFO )
                 CALL CHKXER( 'DSYSV ', INFOT, NOUT, LERR, OK )
                 INFO = J + 100*I
                 CALL LA_TEST_DSYSV('L', NMAX, NRHS, A, NMAX,
     $                               A(1,2), B, NMAX, W, 1 ,INFO )
                 CALL CHKXER( 'DSYSV ', INFOT, NOUT, LERR, OK ) 
               END DO
             END DO
         END DO         
*
*        DSYSVX
*
         SRNAMT = 'DSYSVX'
         DO I = 1, 9
           IN(I) = 1
         END DO
         DO I = 1, 9
           INFOT = I
           DO J = 1, 1
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_DSYSVX( 'N', 'L', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           IW, INFO )
               CALL CHKXER( 'DSYSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSYSVX( 'N', 'U', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           IW, INFO )
               CALL CHKXER( 'DSYSVX', INFOT, NOUT, LERR, OK )
               
               INFO = J + 100*I
               CALL LA_TEST_DSYSVX( 'F', 'L', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           IW, INFO )
               CALL CHKXER( 'DSYSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSYSVX( 'F', 'U', NMAX, NRHS, A, NMAX, AF,
     &           NMAX, IP, B, NMAX, X, NMAX, RCOND, R1, R2, W, NMAX,
     &           IW, INFO )
               CALL CHKXER( 'DSYSVX', INFOT, NOUT, LERR, OK )
             ENDDO
           ENDDO
         ENDDO
*
      ELSE IF( LSAMEN( 2, C2, 'SP' ) ) THEN
*
*        DSPSV
*
         SRNAMT = 'DSPSV '
         DO I = 1, 4
           IN(I) = 1
         END DO
         DO I = 1, 4
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_DSPSV( 'U', NMAX, NRHS, A(1,1), A(1,2),
     $                              B, NMAX, INFO)
               CALL CHKXER( 'DSPSV ', INFOT, NOUT, LERR, OK )
               INFO = J + 100*I
               CALL LA_TEST_DSPSV( 'L', NMAX, NRHS, A(1,1), A(1 ,2),
     $                              B, NMAX, INFO)
               CALL CHKXER( 'DSPSV ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO    
*
*        DSPSVX
*
         SRNAMT = 'DSPSVX'
         DO I = 1, 9
           IN(I) = 1
         END DO
         DO I = 1, 9
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_DSPSVX( 'N', 'U', NMAX, NRHS, A, AF, IP, B,
     &           NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
               CALL CHKXER( 'DSPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSPSVX( 'N', 'L', NMAX, NRHS, A, AF, IP, B,
     &           NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
               CALL CHKXER( 'DSPSVX', INFOT, NOUT, LERR, OK ) 

               INFO = J + 100*I
               CALL LA_TEST_DSPSVX( 'F', 'U', NMAX, NRHS, A, AF, IP, B,
     &           NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
               CALL CHKXER( 'DSPSVX', INFOT, NOUT, LERR, OK )

               INFO = J + 100*I
               CALL LA_TEST_DSPSVX( 'F', 'L', NMAX, NRHS, A, AF, IP, B,
     &           NMAX, X, NMAX, RCOND, R1, R2, W, IW, INFO )
               CALL CHKXER( 'DSPSVX', INFOT, NOUT, LERR, OK ) 
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
*     End of DERRVX
*
      END
