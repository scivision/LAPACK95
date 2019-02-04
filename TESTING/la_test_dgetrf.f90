SUBROUTINE LA_TEST_DGETRF( M, N, A, LDA, IPIV, INFO )
!
!  -- LAPACK95 interface driver routine (version 0.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     October 31, 1996
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_GETRF
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: M, N, LDA
   INTEGER, INTENT(INOUT) :: INFO
!  .. Array Arguments ..
   INTEGER, INTENT(OUT) :: IPIV(1:N)
   REAL(WP), INTENT(INOUT) :: A(1:LDA,1:N)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GETRF'
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_DGETRF'
   INTEGER :: INFOTC
!  .. Common blocks ..
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IA1, IA2, IIPIV
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Intrinsic Functions ..
   INTRINSIC MIN
!  .. Executable Statements ..
   IA1 = M; IA2 = N; IIPIV = MIN(M,N);
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
   CASE(0)
         CALL LA_GETRF( A(1:IA1,1:IA2), IPIV(1:IIPIV) )
         INFO = INFOTC
   CASE (2)
      IIPIV = MIN(M,N)-1
   CASE(:-1,1,3:)
      CALL UESTOP(SRNAMT)
   END SELECT
   IF( I /= 0 ) THEN
     CALL LA_GETRF( A(1:IA1,1:IA2), IPIV(1:IIPIV), INFO = INFO )
   END IF
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_DGETRF
