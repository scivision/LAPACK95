      SUBROUTINE SERRLS( PATH, NUNIT )
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
*  SERRLS tests the error exits for the REAL least squares
*  driver routines (SGELS, SGELSS, SGELSX, SGELSY, SGELSD).
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
      REAL               A( NMAX, NMAX ), B( NMAX, NMAX ), S( NMAX ),
     $                   W( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CHKXER, LA_TEST_SGELS, SGELSD,
     $                   LA_TEST_SGELSS,
     $                   LA_TEST_SGELSX,
     $                   SGELSY
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
      WRITE( NOUT, FMT = * )
      C2 = PATH( 2: 3 )
      A( 1, 1 ) = 1.0E+0
      A( 1, 2 ) = 2.0E+0
      A( 2, 2 ) = 3.0E+0
      A( 2, 1 ) = 4.0E+0
      OK = .TRUE.
*
      IF( LSAMEN( 2, C2, 'LS' ) ) THEN
*
*        Test error exits for the least squares driver routines.
*
*        SGELS
*
         SRNAMT = 'SGELS '
         DO I = 2, 3
           IN(I) = 1
         END DO
         DO I = 2, 3
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_SGELS('N', NMAX, NMAX, NRHS, A,
     &           NMAX, B, NMAX, A(1,1), NMAX, INFO )
               CALL CHKXER( 'SGELS ', INFOT, NOUT, LERR, OK )
               INFO = J + 100*I
               CALL LA_TEST_SGELS('T', NMAX, NMAX, NRHS, A, NMAX, B,
     &           NMAX, A(1,1), NMAX, INFO )
               CALL CHKXER( 'SGELS ', INFOT, NOUT, LERR, OK )
             END DO
           END DO
         END DO 

*
*        SGELSS
*
         SRNAMT = 'SGELSS'
         DO I = 2, 4
           IN(I) = 1
         END DO
         DO I = 2, 4
           IF (I /= 3) THEN
             INFOT = I
             DO J = 1, IN(I)
               DO NRHS = 1, 2
                 INFO = J + 100*I
                 CALL LA_TEST_SGELSS( NMAX, NMAX, NRHS, A,
     &             NMAX, B, NMAX, A(1,1), RCOND,
     &             IRNK, W, 1 , INFO )
                 CALL CHKXER( 'SGELSS ', INFOT, NOUT, LERR, OK )
               END DO
             END DO
           END IF
         END DO 

*
*        SGELSX
*
         SRNAMT = 'SGELSX'
         DO I = 2, 4
           IN(I) = 1
         END DO
         DO I = 2, 4
           IF (I /= 3) THEN
             INFOT = I
             DO J = 1, IN(I)
               DO NRHS = 1, 2
                 INFO = J + 100*I
                 CALL LA_TEST_SGELSX( NMAX, NMAX, NRHS, A,
     &             NMAX, B, NMAX, A(1,1), RCOND,
     &             IRNK, W, INFO )
                 CALL CHKXER( 'SGELSX ', INFOT, NOUT, LERR, OK )
               END DO
             END DO
          END IF
         END DO 

*        SGELSY
*
         SRNAMT = 'SGELSY'
         DO I = 2, 4
           IN(I) = 1
         END DO
         DO I = 2, 4
           IF (I /= 3) THEN
             INFOT = I
             DO J = 1, IN(I)
               DO NRHS = 1, 2
                 INFO = J + 100*I
                 CALL LA_TEST_SGELSY( NMAX, NMAX, NRHS, A, NMAX, B,
     &             NMAX, IP, RCOND, IRNK, W, 10, INFO )
                 CALL CHKXER( 'SGELSY', INFOT, NOUT, LERR, OK )
               ENDDO
             ENDDO
           ENDIF
         ENDDO     
*
*        SGELSD
*
         SRNAMT = 'SGELSD'
         DO I = 2, 4
           IN(I) = 1
         END DO
         DO I = 2, 4
           IF (I /= 3) THEN
             INFOT = I
             DO J = 1, IN(I)
               DO NRHS = 1, 2
                 INFO = J + 100*I
                 CALL LA_TEST_SGELSD( NMAX, NMAX, NRHS, A, NMAX, B,
     &             NMAX, S, RCOND, IRNK, W, 50*NMAX, IP, INFO )
                 CALL CHKXER( 'SGELSD', INFOT, NOUT, LERR, OK )
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
*     End of SERRLS
*
      END
