      SUBROUTINE DERRLS( PATH, NUNIT )
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
*  DERRLS tests the error exits for the DOUBLE PRECISION least squares
*  driver routines (DGELS, SGELSS, SGELSX, SGELSY, SGELSD).
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
      DOUBLE PRECISION   A( NMAX, NMAX ), B( NMAX, NMAX ), S( NMAX ),
     $                   W( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CHKXER, LA_TEST_DGELS,
     $                   DGELSD, LA_TEST_DGELSS,
     $                   LA_TEST_DGELSX, LA_TEST_DGELSY
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
      A( 1, 1 ) = 1.0D+0
      A( 1, 2 ) = 2.0D+0
      A( 2, 2 ) = 3.0D+0
      A( 2, 1 ) = 4.0D+0
      OK = .TRUE.
*
      IF( LSAMEN( 2, C2, 'LS' ) ) THEN
*
*        Test error exits for the least squares driver routines.
*
*        DGELS
*
         SRNAMT = 'DGELS '
         DO I = 2, 3
           IN(I) = 1
         END DO
         DO I = 2, 3
           INFOT = I
           DO J = 1, IN(I)
             DO NRHS = 1, 2
               INFO = J + 100*I
               CALL LA_TEST_DGELS('N', NMAX, NMAX, NRHS, A,
     &           NMAX, B, NMAX, A(1,1), NMAX, INFO )
               CALL CHKXER( 'DGELS ', INFOT, NOUT, LERR, OK )
               INFO = J + 100*I  
               CALL LA_TEST_DGELS('T', NMAX, NMAX, NRHS, A, NMAX, B,
     &           NMAX, A(1,1), NMAX, INFO )
               CALL CHKXER( 'DGELS ', INFOT, NOUT, LERR, OK )   
             END DO
           END DO
         END DO

*
*        DGELSS
*
         SRNAMT = 'DGELSS'
         DO I = 2, 4
           IN(I) = 1
         END DO
         DO I = 2, 4
           IF (I /= 3) THEN
            INFOT = I
             DO J = 1, IN(I)
               DO NRHS = 1, 2
                 INFO = J + 100*I
                 CALL LA_TEST_DGELSS( NMAX, NMAX, NRHS, A,
     &             NMAX, B, NMAX, A(1,1), RCOND,
     &             IRNK, W, 1 , INFO )
                 CALL CHKXER( 'DGELSS ', INFOT, NOUT, LERR, OK )
               END DO
             END DO
           END IF
         END DO 
*
*        DGELSX
*
         SRNAMT = 'DGELSX'
         DO I = 2, 4
           IN(I) = 1
         END DO
         DO I = 2, 4
           IF (I /= 3) THEN 
             INFOT = I
             DO J = 1, IN(I)
               DO NRHS = 1, 2
                 INFO = J + 100*I
                 CALL LA_TEST_DGELSX( NMAX, NMAX, NRHS, A,
     &             NMAX, B, NMAX, A(1,1), RCOND,
     &             IRNK, W, INFO )
                 CALL CHKXER( 'DGELSX ', INFOT, NOUT, LERR, OK )
               END DO
             END DO
          END IF
         END DO  
        
*
*        DGELSY
*
         SRNAMT = 'DGELSY'
         DO I = 2, 4
           IN(I) = 1
         END DO
         DO I = 2, 4
           IF (I /= 3) THEN
             INFOT = I
             DO J = 1, IN(I)
               DO NRHS = 1, 2
                 INFO = J + 100*I
                 CALL LA_TEST_DGELSY( NMAX, NMAX, NRHS, A, NMAX, B,
     &             NMAX, IP, RCOND, IRNK, W, 10, INFO )
                 CALL CHKXER( 'DGELSY', INFOT, NOUT, LERR, OK )
               ENDDO
             ENDDO
           ENDIF
         ENDDO
         
*
*        DGELSD
*
         SRNAMT = 'DGELSD'
         DO I = 2, 4
           IN(I) = 1
         END DO
         DO I = 2, 4
           IF (I /= 3) THEN
             INFOT = I
             DO J = 1, IN(I)
               DO NRHS = 1, 2
                 INFO = J + 100*I  
                 CALL LA_TEST_DGELSD( NMAX, NMAX, NRHS, A, NMAX, B,
     &             NMAX, S, RCOND, IRNK, W, 50*NMAX, IP, INFO )
                 CALL CHKXER( 'DGELSD', INFOT, NOUT, LERR, OK )
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
*     End of DERRLS
*
      END
      
