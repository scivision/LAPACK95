      SUBROUTINE CERRLS( PATH, NUNIT )
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
*  CERRLS tests the error exits for the COMPLEX least squares
*  driver routines (CGELS, CGELSS, CGELSX, CGELSY, CGELSD).
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
      REAL               RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX ), IN(100)
      REAL               RW( NMAX ), S( NMAX )
      COMPLEX            A( NMAX, NMAX ), B( NMAX, NMAX ), W( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, LA_TEST_CGELS, LA_TEST_CGELSD,
     &                   LA_TEST_CGELSS,
     $                   LA_TEST_CGELSX, LA_TEST_CGELSY, CHKXER
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
      A( 1, 1 ) = ( 1.0E+0, 0.0E+0 )
      A( 1, 2 ) = ( 2.0E+0, 0.0E+0 )
      A( 2, 2 ) = ( 3.0E+0, 0.0E+0 )
      A( 2, 1 ) = ( 4.0E+0, 0.0E+0 )
      OK = .TRUE.
      WRITE( NOUT, FMT = * )
*
*     Test error exits for the least squares driver routines.
*
      IF( LSAMEN( 2, C2, 'LS' ) ) THEN
*
*        CGELS
*
        SRNAMT = 'CGELS '
        DO I = 2, 3
          IN(I) = 1
        END DO
        DO I = 2, 3
          INFOT = I
          DO J = 1, IN(I)
            DO NRHS = 1, 2
              INFO = J + 100*I
              CALL LA_TEST_CGELS('N', NMAX, NMAX, NRHS, A,
     &          NMAX, B, NMAX, A(1,1), NMAX, INFO )
              CALL CHKXER( 'CGELS ', INFOT, NOUT, LERR, OK )
              INFO = J + 100*I
              CALL LA_TEST_CGELS('T', NMAX, NMAX, NRHS, A, NMAX, B,
     &          NMAX, A(1,1), NMAX, INFO )
              CALL CHKXER( 'CGELS ', INFOT, NOUT, LERR, OK )
            END DO
          END DO
        END DO 


*        CGELSS
*
        SRNAMT = 'CGELSS'
        DO I = 2, 4
          IN(I) = 1
         END DO
         DO I = 2, 4
           IF (I /= 3) THEN
             INFOT = I
             DO J = 1, IN(I)
               DO NRHS = 1, 2
                 INFO = J + 100*I
                 CALL LA_TEST_CGELSS( NMAX, NMAX, NRHS, A,
     &             NMAX, B, NMAX, S, RCOND,
     &             IRNK, W, 1 , RW, INFO )
                 CALL CHKXER( 'CGELSS ', INFOT, NOUT, LERR, OK )
               END DO
             END DO
           END IF
         END DO 


*        CGELSX
*
         SRNAMT = 'CGELSX'
         DO I = 2, 4
           IN(I) = 1
         END DO
         DO I = 2, 4
          IF (I /= 3) THEN
             INFOT = I
            DO J = 1, IN(I)
              DO NRHS = 1, 2
                INFO = J + 100*I
                 CALL LA_TEST_CGELSX( NMAX, NMAX, NRHS, A,
     &             NMAX, B, NMAX, A(1,1), RCOND,
     &             IRNK, W, RW, INFO )
                 CALL CHKXER( 'CGELSX ', INFOT, NOUT, LERR, OK )
               END DO
             END DO
          END IF
         END DO
*
*        CGELSY
*
         SRNAMT = 'CGELSY'
         DO I = 2 , 4
           IN(I) = 1
         END DO
         DO I = 2, 4
           IF (I /= 3) THEN
             INFOT = I
             DO J = 1, IN(I)
               DO NRHS = 1, 2
                 INFO = J + 100*I
                 CALL LA_TEST_CGELSY( NMAX, NMAX, NRHS, A, NMAX, B,
     &             NMAX, IP, RCOND, IRNK, W, 10, RW, INFO )
                 CALL CHKXER( 'CGELSY', INFOT, NOUT, LERR, OK )
               ENDDO
             ENDDO
           ENDIF
         ENDDO    
         
!         INFOT = 1
!         CALL CGELSY( -1, 0, 0, A, 1, B, 1, IP, RCOND, IRNK, W, 10, RW,
!     $                INFO )
!         CALL CHKXER( 'CGELSY', INFOT, NOUT, LERR, OK )
!         INFOT = 2
!         CALL CGELSY( 0, -1, 0, A, 1, B, 1, IP, RCOND, IRNK, W, 10, RW,
!     $                INFO )
!         CALL CHKXER( 'CGELSY', INFOT, NOUT, LERR, OK )
!         INFOT = 3
!         CALL CGELSY( 0, 0, -1, A, 1, B, 1, IP, RCOND, IRNK, W, 10, RW,
!     $                INFO )
!         CALL CHKXER( 'CGELSY', INFOT, NOUT, LERR, OK )
!         INFOT = 5
!         CALL CGELSY( 2, 0, 0, A, 1, B, 2, IP, RCOND, IRNK, W, 10, RW,
!     $                INFO )
!         CALL CHKXER( 'CGELSY', INFOT, NOUT, LERR, OK )
!         INFOT = 7
!         CALL CGELSY( 2, 0, 0, A, 2, B, 1, IP, RCOND, IRNK, W, 10, RW,
!     $                INFO )
!         CALL CHKXER( 'CGELSY', INFOT, NOUT, LERR, OK )
!         INFOT = 12
!         CALL CGELSY( 0, 3, 0, A, 1, B, 3, IP, RCOND, IRNK, W, 1, RW,
!     $                INFO )
!         CALL CHKXER( 'CGELSY', INFOT, NOUT, LERR, OK )
*
*        CGELSD
*
         SRNAMT = 'CGELSD'
         DO I = 2, 4
           IN(I) = 1
         END DO
         DO I = 2, 4
           IF (I /= 3) THEN
             INFOT = I
             DO J = 1, IN(I)
               DO NRHS = 1, 2
                 INFO = J + 100*I
                 CALL LA_TEST_CGELSD( NMAX, NMAX, NRHS, A, NMAX, B,
     &             NMAX, S, RCOND, IRNK, W, 10, RW,
     &             IP, INFO )
                 CALL CHKXER( 'CGELSD', INFOT, NOUT, LERR, OK )
               ENDDO
             ENDDO
           ENDIF
         ENDDO    
         
!         INFOT = 1
!         CALL CGELSD( -1, 0, 0, A, 1, B, 1, S, RCOND, IRNK, W, 10,
!     $                RW, IP, INFO )
!         CALL CHKXER( 'CGELSD', INFOT, NOUT, LERR, OK )
!         INFOT = 2
!         CALL CGELSD( 0, -1, 0, A, 1, B, 1, S, RCOND, IRNK, W, 10,
!     $                RW, IP, INFO )
!         CALL CHKXER( 'CGELSD', INFOT, NOUT, LERR, OK )
!         INFOT = 3
!         CALL CGELSD( 0, 0, -1, A, 1, B, 1, S, RCOND, IRNK, W, 10,
!     $                RW, IP, INFO )
!         CALL CHKXER( 'CGELSD', INFOT, NOUT, LERR, OK )
!         INFOT = 5
!         CALL CGELSD( 2, 0, 0, A, 1, B, 2, S, RCOND, IRNK, W, 10,
!     $                RW, IP, INFO )
!         CALL CHKXER( 'CGELSD', INFOT, NOUT, LERR, OK )
!         INFOT = 7
!         CALL CGELSD( 2, 0, 0, A, 2, B, 1, S, RCOND, IRNK, W, 10,
!     $                RW, IP, INFO )
!         CALL CHKXER( 'CGELSD', INFOT, NOUT, LERR, OK )
!         INFOT = 12
!         CALL CGELSD( 2, 2, 1, A, 2, B, 2, S, RCOND, IRNK, W, 1,
!     $                RW, IP, INFO )
!         CALL CHKXER( 'CGELSD', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of CERRLS
*
      END
