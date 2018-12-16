SUBROUTINE LA_TEST_SGEEQU( M, N, A, LDA, R, C, ROWCND, COLCND, AMAX, INFO )
!
!  -- LAPACK95 interface driver routine (version 0.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     October 31, 1996
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_GEEQU
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: M, N, LDA
   INTEGER, INTENT(INOUT) :: INFO
   REAL(WP), INTENT(OUT) :: AMAX, COLCND, ROWCND
!  .. Array Arguments ..
   REAL(WP), INTENT(INOUT) :: A(1:LDA,1:N)
   REAL(WP), INTENT(OUT) :: C(1:N), R(1:N)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GEEQU'
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_SGEEQU'
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IA1, IA2, IR, IC
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IA1 = M; IA2 = N; IR = M; IC = N
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
   CASE(0)
         CALL LA_GEEQU( A(1:IA1,1:IA2), R(1:IR), C(1:IC), ROWCND, COLCND, AMAX )
         INFO = INFOTC
   CASE (2)
      IR = IA1-1
   CASE (3)
      IC = IA2-1
   CASE(:-1,1,4:)
      CALL UESTOP(SRNAMT)
   END SELECT
   IF( I /= 0 ) THEN
         CALL LA_GEEQU( A(1:IA1,1:IA2), R(1:IR), C(1:IC), ROWCND, &
              COLCND, AMAX, INFO )
   END IF
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_SGEEQU
