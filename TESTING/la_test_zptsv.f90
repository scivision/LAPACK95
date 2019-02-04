SUBROUTINE LA_TEST_ZPTSV( N, NRHS, D, E, B, LDB, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark,
!     April 13, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_PTSV
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, NRHS, LDB
   INTEGER, INTENT(INOUT) :: INFO
!  .. Array Arguments ..
   REAL(WP), INTENT(INOUT) :: D(1:N)
   COMPLEX(WP), INTENT(INOUT) :: E(1:N-1), B(1:LDB,1:N)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_PTSV '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZPTSV '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, ID, IE, IB1, IB2
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   ID = N; IE = N-1; IB1 = N; IB2 = NRHS
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
   CASE(0)
      IF (NRHS == 1) THEN
        CALL LA_PTSV( D(1:ID), E(1:IE),  B(1:IB1,1), INFO )
        INFO = INFOTC
      ELSE
        CALL LA_PTSV( D(1:ID),  E(1:IE),  B(1:IB1,1:IB2), INFO )
        INFO = INFOTC
      END IF
   CASE (2)
      IE = ID - 2
   CASE(3)
      IB1 = ID - 1
   CASE(:-1, 1, 4:)
      CALL UESTOP(SRNAMT)
   END SELECT
   IF( I /= 0  ) THEN
      SELECT CASE (NRHS)
      CASE (2:)
         CALL LA_PTSV( D(1:ID), E(1 :IE), B(1:IB1,1:IB2),  INFO )
      CASE(1)
         CALL LA_PTSV( D(1:ID), E(1:IE), B(1:IB1,1), INFO )
      CASE(:-1)
         CALL UESTOP(SRNAMT)
      END SELECT
   END IF
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_ZPTSV
