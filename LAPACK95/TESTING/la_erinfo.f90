SUBROUTINE ERINFO(LINFO, SRNAME, INFO, ISTAT)
!
!  -- LAPACK95 interface driver routine (version 0.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     October 31, 1996
!
!  .. Scalar Arguments ..
   CHARACTER( LEN = * ), INTENT(IN) :: SRNAME
   INTEGER             , INTENT(IN) :: LINFO
   INTEGER             , INTENT(INOUT), OPTIONAL :: INFO, ISTAT
!  .. Scalars in Common ..
   LOGICAL            LERR, OK
   CHARACTER*6        SRNAMT
   INTEGER            INFOT, NOUT
   INTEGER :: INFOTC
!  .. Common blocks ..
   COMMON             / INFOC / INFOT, NOUT, OK, LERR
   COMMON             / SRNAMC / SRNAMT
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: L1, L2
!  .. Intrinsic Functions
!  INTEGER :: LENTRIM
!  INTRINSIC LENTRIM
!  .. Executable Statements ..
!  L1 = LENTRIM(SRNAME)
!  L2 = LENTRIM(SRNAMT)

   L1 = LEN(SRNAME)
   L2 = L1 - 2
   IF( PRESENT(INFO) ) INFO = LINFO
   INFOTC = LINFO
   IF( LINFO .GE. 0 ) RETURN

!
   LERR = .TRUE.
   IF(- LINFO.NE.INFOT .AND. LINFO.LT.0 ) THEN
      IF( INFOT.NE.0 ) THEN
         WRITE( NOUT, FMT = 9999 )SRNAMT, INFO, INFOT
      ELSE
         WRITE( NOUT, FMT = 9997 )SRNAME, -LINFO
      END IF
      OK = .FALSE.
   END IF
   IF( SRNAME(4:L1).NE.SRNAMT(2:L2) .AND. &
       SRNAME(4:L1) /= 'GETRF' .AND. SRNAMT(2:L2) /= 'GECON' ) THEN
      WRITE( NOUT, FMT = 9998 )SRNAME(4:L1), SRNAMT(2:L2)
      OK = .FALSE.
   END IF
!
 9999 FORMAT( ' +++ ERINFO was called from ', A6, ' with INFO = ', I6, &
              ' instead of ', I2, ' +++' )
 9998 FORMAT( ' +++ ERINFO was called with SRNAME = ', A6, &
              ' instead of ', A6, ' +++' )
 9997 FORMAT( ' +++ On entry to ', A6, ' parameter number ', I6, &
              ' had an illegal value +++' )
!
!  IF( LINFO < 0 .OR. LINFO>0 .AND. .NOT.PRESENT(INFO) )THEN
!  Temporary
   IF( .NOT.PRESENT(INFO) ) THEN
      WRITE (*,*) 'Program terminated in LAPACK_95 subroutine ', SRNAME
      WRITE (*,*) 'Error indicator, INFO = ', LINFO
      STOP
   END IF 
END SUBROUTINE ERINFO
