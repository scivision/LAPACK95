SUBROUTINE LA_TEST_DGTSV(N, NRHS, DL, D, DU, B, LDB, INFO)
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark,
!     March 28, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_GTSV
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, NRHS, LDB
   INTEGER, INTENT(INOUT) :: INFO
!  .. Array Arguments ..
   REAL(WP), INTENT(INOUT) :: B(1:LDB,1:NRHS)
   REAL(WP), INTENT(INOUT) :: DL(1:N-1), D(1:N), DU(1:N-1)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GTSV '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_DGTSV '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IDU, ID, IDL,IB1, IB2
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IDL = N-1; ID = N; IDU = N-1; IB1 = N; IB2 = NRHS; 
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
    CASE(0)
      IF ( NRHS == 1 ) THEN
        CALL LA_GTSV( DL(1:IDL), D(1:ID), DU(1:IDU), B(1:IB1, 1))
        INFO = INFOTC
      ELSE
        CALL LA_GTSV( DL(1:IDL), D(1:ID), DU(1:IDU), B(1:IB1,1:IB2) )
        INFO = INFOTC
      END IF
   CASE (1)
      IDL = ID
   CASE(3)
      IDU = ID 
   CASE(4)
      IB1 = ID - 1
   CASE(:-1,2,5:)
      CALL UESTOP(SRNAMT)
   END SELECT
      IF( I /= 0 ) THEN
      SELECT CASE (NRHS)
      CASE (2:)
          CALL LA_GTSV( DL(1:IDL), D(1:ID), DU(1:IDU),&
     &       B(1:IB1, 1:IB2),  INFO)
      CASE(1)
      CALL LA_GTSV( DL(1:IDL), D(1:ID), DU(1:IDU), &
     &      B(1:IB1, 1), INFO )
      CASE(:-1)
         CALL UESTOP(SRNAMT)
      END SELECT
   END IF
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_DGTSV
