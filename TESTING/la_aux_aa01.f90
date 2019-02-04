SUBROUTINE LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
!  -- LAPACK95 interface driver routine (version 0.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     October 31, 1996
!
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   CHARACTER(LEN=*), INTENT(IN) :: SRNAMT
   INTEGER, INTENT(IN) :: I
   LOGICAL, INTENT(INOUT) :: CTEST, ETEST
!  .. Executable Statements ..
   IF( I == 0 )THEN
      IF( CTEST ) THEN
         WRITE(*,*)'Computational tests of the procedure ', SRNAMT
         CTEST = .FALSE.
      END IF
   ELSE
      IF( ETEST )THEN
         WRITE(*,*)'Error exit tests of the procedure ', SRNAMT
         ETEST = .FALSE.
      END IF
   END IF
END SUBROUTINE LA_AUX_AA01
