SUBROUTINE LA_TEST_DSBEVD( JOBS, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK, LWORK, IWORK, LIWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     May 14, 1999
!
!  .. Use Statements ..
  USE LA_PRECISION, ONLY: WP => DP
  USE F95_LAPACK, ONLY: LA_SBEVD
!  .. Implicit Statement ..
  IMPLICIT NONE
!  .. Scalar Arguments ..
  INTEGER, INTENT(IN) :: N, LDAB, KD, LDZ, LWORK, LIWORK
  INTEGER, INTENT(INOUT) :: INFO
  CHARACTER*1, INTENT(IN) :: UPLO, JOBS
!  .. Array Arguments ..
  REAL(WP), INTENT(INOUT) :: AB(1:LDAB, 1:N)
  REAL(WP), INTENT(OUT)::  W(1:N), WORK(1 : LWORK), Z(1:LDZ, 1:N)
  INTEGER, INTENT(OUT) :: IWORK(1: LIWORK)
!  .. Parameters ..
  CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_SBEVD'
  CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_DSBEVD'
!  .. Common blocks ..
  INTEGER :: INFOTC
  COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
  INTEGER :: I, J, IAB1, IAB2, IW, IZ1, IZ2
  CHARACTER*1 :: IUPLO, IJOBS   
!  .. Local Arrays ..
  LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
  LOGICAL LSAME
!  .. Executable Statements ..
  IAB1 = KD+1 ; IAB2 = N; IUPLO = UPLO; IW = N; IJOBS = JOBS;
  IZ1 = N; IZ2 = N
  I = INFO / 100; J = INFO - I*100
  SELECT CASE(I)
    CASE (1)
      IAB1 = -2
    CASE (2)
      IW = IW - 1
    CASE (3)
      IUPLO = 'T'   
    CASE (4)
      IZ1 = IZ1 - 1
      IJOBS = 'V'
    CASE(:-1,5:)
      CALL UESTOP(SRNAMT)
  END SELECT
  IF (LSAME (IJOBS, 'V')) THEN 
    CALL LA_SBEVD( AB(1:IAB1,1:IAB2), W(1 :IW), IUPLO, Z(1:IZ1, 1: IZ2), &
&     INFO )
  ELSE
    CALL LA_SBEVD( AB(1:IAB1,1:IAB2), W(1 :IW), IUPLO, INFO=INFO )
  ENDIF
  
    CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
 END SUBROUTINE LA_TEST_DSBEVD
      
      
