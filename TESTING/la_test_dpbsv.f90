SUBROUTINE LA_TEST_DPBSV(UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark,
!     April 5, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_PBSV
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, KD, NRHS, LDAB, LDB
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: UPLO
!  .. Array Arguments ..
   REAL(WP), INTENT(INOUT) :: AB(1:LDAB,1:N), B(1:LDB,1:NRHS)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_PBSV '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_DPBSV '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IAB1, IAB2, IB1, IB2
   CHARACTER*1 :: IUPLO   
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IAB1 = LDAB; IAB2 = N; IB1 = N; IB2 = NRHS; IUPLO=UPLO
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
   CASE(0)
      IF ( NRHS == 1 ) THEN
        CALL LA_PBSV( AB(1:IAB1,1:IAB2), B(1:IB1,1), IUPLO )
        INFO = INFOTC
      ELSE
        CALL LA_PBSV( AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), IUPLO )
        INFO = INFOTC
      END IF
   CASE (1)
      IAB1 = -1
   CASE (2)
      IB1 = IAB1-1
   CASE(3)
      IUPLO = 'T'
   CASE(:-1,4:)
      CALL UESTOP(SRNAMT)
   END SELECT
      IF( I /= 0 ) THEN
      SELECT CASE (NRHS)
      CASE (2:)
         CALL LA_PBSV( AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), IUPLO, INFO )
      CASE(1)
         CALL LA_PBSV( AB(1:IAB1,1:IAB2), B(1:IB1,1), IUPLO, INFO )
      CASE(:-1)
         CALL UESTOP(SRNAMT)
      END SELECT
   END IF
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_DPBSV
