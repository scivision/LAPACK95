SUBROUTINE LA_TEST_DGECON( NORM, N, A, LDA, ANORM, RCOND, WORK, &
                           IWORK, INFO )
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
   CHARACTER(LEN=1), INTENT(IN) :: NORM
   INTEGER, INTENT(IN) :: N, LDA
   INTEGER, INTENT(INOUT) :: INFO
   REAL(WP), INTENT(IN) :: ANORM
   REAL(WP), INTENT(OUT) :: RCOND
!  .. Array Arguments ..
   INTEGER, INTENT(INOUT) :: IWORK(1:N)
   REAL(WP), INTENT(INOUT) :: A(1:LDA,1:N), WORK(*)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GETRF'
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_DGECON'
!  .. Local Scalars ..
   CHARACTER(LEN=1) LNORM
   INTEGER :: I, J, IA1, IA2, IIWORK
   REAL(WP) :: W1
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   W1 = ANORM; RCOND = WORK(1)
   IA1 = N; IA2 = N; IIWORK = N; LNORM = NORM
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
   CASE(0)
         IF( LNORM == 'N' )THEN
            CALL LA_GETRF( A(1:IA1,1:IA2), IWORK(1:IIWORK), RCOND )
         ELSE
            CALL LA_GETRF( A(1:IA1,1:IA2), IWORK(1:IIWORK), RCOND, LNORM )
         END IF
         INFO = 0
   CASE (1)
      IA2 = IA1-1
   CASE (2)
      IIWORK = IA1-1
   CASE(4)
      SELECT CASE(J)
      CASE(1)
         CALL LA_GETRF( A(1:IA1,1:IA2), IWORK(1:IIWORK), NORM = LNORM, &
              INFO = INFO )
      CASE(2)
         LNORM = '/'
      CASE(:0,3,5:)
         CALL UESTOP(SRNAMT)
      END SELECT
   CASE(:-1,5:)
      CALL UESTOP(SRNAMT)
   END SELECT
      IF ( I /= 0 ) THEN
       CALL LA_GETRF( A(1:IA1,1:IA2),IWORK(1:IIWORK), RCOND, LNORM, INFO )
   END IF
   IF( RCOND == 0.0_WP .AND. I == 0 ) INFO = 0
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_DGECON
