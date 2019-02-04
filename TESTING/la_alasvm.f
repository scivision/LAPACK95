      SUBROUTINE ALASVM( TYPE, NOUT, NFAIL, NRUN, NERRS )
*
*  -- LAPACK test routine (version 2.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER*3        TYPE
      INTEGER            NFAIL, NOUT, NRUN, NERRS
*     ..
*
*  Purpose
*  =======
*
*  ALASVM prints a summary of results from one of the -DRV- routines.
*
*  Arguments
*  =========
*
*  TYPE    (input) CHARACTER*3
*          The LAPACK path name.
*
*  NOUT  (input) INTEGER
*          The unit number on which results are to be printed.
*          NOUT >= 0.
*
*  NFAIL   (input) INTEGER
*          The number of tests which did not pass the threshold ratio.
*
*  NRUN    (input) INTEGER
*          The total number of tests.
*
*  NERRS   (input) INTEGER
*          The number of error messages recorded.
*
*     .. Local Scalar ..
      CHARACTER*3 TYPE1
      SAVE TYPE1
      DATA TYPE1/'   '/
*
*  =====================================================================
*
*     .. Executable Statements ..
*
      IF( NFAIL.GT.0 ) THEN
         WRITE( NOUT, FMT = 9999 )TYPE, NFAIL, NRUN
      ELSE
         IF( TYPE .NE. TYPE1 )THEN
            TYPE1 = TYPE
            WRITE( NOUT, FMT = 9998 )TYPE, NRUN
         END IF
      END IF
      IF( NERRS.GT.0 ) THEN
         WRITE( NOUT, FMT = 9997 )NERRS
      END IF
*
 9999 FORMAT( 1X, A3, ' drivers: ', I6, ' out of ', I6,
     $      ' tests failed to pass the threshold' )
 9998 FORMAT( /1X, 'All tests for ', A3, ' drivers  passed the ',
     $      'threshold (', I6, ' tests run)' )
 9997 FORMAT( 14X, I6, ' error messages recorded' )
      RETURN
*
*     End of ALASVM
*
      END
