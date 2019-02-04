SUBROUTINE LA_TEST_CGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 0.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     October 31, 1996
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_GETRI
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, LDA, LWORK
   INTEGER, INTENT(INOUT) :: INFO
!  .. Array Arguments ..
   INTEGER, INTENT(OUT) :: IPIV(1:N)
   COMPLEX(WP), INTENT(INOUT) :: A(1:LDA,1:N), WORK(*)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GETRI'
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_CGETRI'
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IA1, IA2, IIPIV
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IA1 = N; IA2 = N; IIPIV = N
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
   CASE(0)
         CALL LA_GETRI( A(1:IA1,1:IA2), IPIV(1:IIPIV) )
         INFO = INFOTC
   CASE (1)
      IA2 = IA1-1
   CASE (2)
      IIPIV = IA1-1
   CASE(:-1,3:)
      CALL UESTOP(SRNAMT)
   END SELECT
   IF( I /= 0 ) THEN
     CALL LA_GETRI( A(1:IA1,1:IA2), IPIV(1:IIPIV), INFO )
   END IF
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_CGETRI
