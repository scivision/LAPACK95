SUBROUTINE LA_TEST_SSTEV( JOBS, N, D, E, Z, LDZ, WORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     May 27, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_STEV
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, LDZ
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: JOBS
!  .. Array Arguments ..
   REAL(WP), INTENT(INOUT) :: D(1:N), E(1:N)
   REAL(WP), INTENT(OUT)::  Z(1:LDZ, 1:N), WORK(1: MAX(1, 2*N-2))
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_STEV '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_SSTEV '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IE, IZ1, IZ2, ID
   CHARACTER*1 :: IJOBS   
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
   logical lsame
!  .. Executable Statements ..
   IJOBS = JOBS; ID = N; IE = N; IZ1 = MAX (1,N); IZ2 = N
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
     CASE (2)
       IE = IE - 1
     CASE (3)
       IZ1 = IZ1 - 1
       IJOBS = 'V'
     CASE(:-1,1,4:)
       CALL UESTOP(SRNAMT)
   END SELECT
   IF (LSAME(IJOBS,'V')) THEN
     CALL LA_STEV( D(1:ID), E(1:IE), Z(1:IZ1, 1:IZ2), INFO)
   ELSE
     CALL LA_STEV( D(1:ID), E(1:IE), INFO=INFO)
   END IF

   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
 END SUBROUTINE LA_TEST_SSTEV
      
      
