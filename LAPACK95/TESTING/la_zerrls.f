      SUBROUTINE ZERRLS( PATH, NUNIT )
*
*  -- LAPACK90 test routine (version 1.1) --
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
*  ZERRLS tests the error exits for the COMPLEX*16 least squares
*  driver routines (ZGELS, CGELSS, CGELSX, CGELSY, CGELSD).
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
      PARAMETER          ( NMAX = 2 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            INFO, IRNK, I, J, NRHS
      DOUBLE PRECISION   RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX ), IN(100)
      DOUBLE PRECISION   RW( NMAX ), S( NMAX )
      COMPLEX*16         A( NMAX, NMAX ), B( NMAX, NMAX ), W( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CHKXER, LA_TEST_ZGELS,
     &                   LA_TEST_ZGELSD,
     &                   LA_TEST_ZGELSS, LA_TEST_ZGELSX,
     $                   LA_TEST_ZGELSY
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
*     .. Executable Statements ..
*
      NOUT = NUNIT
      C2 = PATH( 2: 3 )
      A( 1, 1 ) = ( 1.0D+0, 0.0D+0 )
      A( 1, 2 ) = ( 2.0D+0, 0.0D+0 )
      A( 2, 2 ) = ( 3.0D+0, 0.0D+0 )
      A( 2, 1 ) = ( 4.0D+0, 0.0D+0 )
      OK = .TRUE.
      WRITE( NOUT, FMT = * )
*
*     Test error exits for the least squares driver routines.
*
      IF( LSAMEN( 2, C2, 'LS' ) ) THEN
*
*        ZGELS
*
        SRNAMT = 'ZGELS '
        DO I = 2, 3
          IN(I) = 1
        END DO
        DO I = 2, 3
          INFOT = I
          DO J = 1, IN(I)
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_ZGELS('N', NMAX, NMAX, NRHS, A,
     &          NMAX, B, NMAX, A(1,1), NMAX, INFO )
              CALL CHKXER( 'ZGELS ', INFOT, NOUT, LERR, OK )
              INFO = J + 100*I
              CALL LA_TEST_ZGELS('T', NMAX, NMAX, NRHS, A, NMAX, B,
     &          NMAX, A(1,1), NMAX, INFO )
              CALL CHKXER( 'ZGELS ', INFOT, NOUT, LERR, OK )
            END DO
          END DO
        END DO 
*
*        ZGELSS
*
        SRNAMT = 'ZGELSS'
        DO I = 2, 4
          IN(I) = 1
        END DO
        DO I = 2, 4
          IF (I /= 3) THEN
            INFOT = I
            DO J = 1, IN(I)
              DO NRHS = 1, 2
                INFO = J + 100*I
                CALL LA_TEST_ZGELSS( NMAX, NMAX, NRHS, A,
     &            NMAX, B, NMAX, A(1,1), RCOND,
     &            IRNK, W, 1 , RW, INFO )
                CALL CHKXER( 'ZGELSS ', INFOT, NOUT, LERR, OK )
              END DO
            END DO
          END IF
        END DO
*
*        ZGELSX
*
        SRNAMT = 'ZGELSX'
        DO I = 2, 4
          IN(I) = 1
        END DO
        DO I = 2, 4
          IF (I /= 3) THEN
            INFOT = I
            DO J = 1, IN(I)
              DO NRHS = 1, 2
                INFO = J + 100*I
                CALL LA_TEST_ZGELSX( NMAX, NMAX, NRHS, A,
     &            NMAX, B, NMAX, A(1,1), RCOND,
     &            IRNK, W, RW, INFO )
                CALL CHKXER( 'ZGELSX ', INFOT, NOUT, LERR, OK )
              END DO
            END DO
          END IF
        END DO 
*
*        ZGELSY
*
        SRNAMT = 'ZGELSY'
        DO I = 2 , 4
          IN(I) = 1
        END DO
        DO I = 2, 4
          IF (I /= 3) THEN
            INFOT = I
            DO J = 1, IN(I)
              DO NRHS = 1, 2
                INFO = J + 100*I
                CALL LA_TEST_ZGELSY( NMAX, NMAX, NRHS, A, NMAX, B,
     &            NMAX, IP, RCOND, IRNK, W, 10, RW, INFO ) 
                CALL CHKXER( 'ZGELSY', INFOT, NOUT, LERR, OK )
              ENDDO
            ENDDO
          ENDIF
        ENDDO

*
*        ZGELSD
*
         SRNAMT = 'ZGELSD'
         DO I = 2, 4
           IN(I) = 1
         END DO
         DO I = 2, 4
           IF (I /= 3) THEN
             INFOT = I
             DO J = 1, IN(I)
               DO NRHS = 1, 2
                 INFO = J + 100*I
                 CALL LA_TEST_ZGELSD( NMAX, NMAX, NRHS, A, NMAX, B,
     &             NMAX, S, RCOND, IRNK, W, 10, RW,
     &             IP, INFO )
                 CALL CHKXER( 'ZGELSD', INFOT, NOUT, LERR, OK ) 
               ENDDO
             ENDDO
           ENDIF
         ENDDO    

      END IF
*
*     Print a summary line.
*
      CALL ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of ZERRLS
*
      END
