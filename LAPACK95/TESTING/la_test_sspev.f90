SUBROUTINE LA_TEST_SSPEV( JOBS, UPLO, N, AP, W, Z, LDZ, WORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     May 14, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_SPEV
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, LDZ
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: JOBS, UPLO
!  .. Array Arguments ..
   REAL(WP), INTENT(INOUT) :: AP(1: N*(N+1)/2)
   REAL(WP), INTENT(OUT)::  W(N), Z(LDZ, N)
   REAL(WP) :: WORK(1:3*N)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_SPEV '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_SSPEV '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IAP, IW, IZ1, IZ2, IWORK
   CHARACTER*1 :: IUPLO, IJOBS   
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
   LOGICAL LSAME
!  .. Executable Statements ..
   IAP = N*(N+1)/2; IUPLO = UPLO; IW = N; IJOBS = JOBS
   IZ1 =MAX(1,N); IZ2 = N; IWORK = N
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
     CASE (1)
       IAP = IAP - 1
     CASE (2)
       IW = IW - 1
     CASE (3)
       IUPLO = 'T'   
     CASE (4)
       IZ2 = IZ2 - 1
       IJOBS = 'V'
     CASE(:-1,5:)
       CALL UESTOP(SRNAMT)
   END SELECT
   if (lsame(IJOBS, 'V')) then
     CALL LA_SPEV(AP(1:IAP), W(1:IW), IUPLO, Z(1:IZ1, 1:IZ2),INFO )
   else
     CALL LA_SPEV(AP(1:IAP), W(1:IW), IUPLO, INFO=INFO )
   end if
   
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
 END SUBROUTINE LA_TEST_SSPEV
      
      
