SUBROUTINE LA_TEST_DSPEVD( JOBS, UPLO, N, AP, W, Z, LDZ, WORK, LWORK, IWORK, LIWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     May 25, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_SPEVD
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, LDZ, LWORK, LIWORK
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: JOBS, UPLO
!  .. Array Arguments ..
   REAL(WP), INTENT(INOUT) :: AP(1: N*(N+1)/2)
   REAL(WP), INTENT(OUT)::  W(N), Z(LDZ, N)
   INTEGER, INTENT(OUT) :: IWORK(1: LIWORK)
   REAL(WP) :: WORK(1:LWORK)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_SPEVD'
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_DSPEVD'
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IAP, IW, IZ1, IZ2
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
   IF (LSAME(IJOBS, 'V')) THEN
     CALL LA_SPEVD(AP(1:IAP), W(1:IW), IUPLO, Z(1:IZ1, 1:IZ2),INFO )
   ELSE
     CALL LA_SPEVD(AP(1:IAP), W(1:IW), IUPLO, INFO=INFO )
   END IF
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
 END SUBROUTINE LA_TEST_DSPEVD
      
      
