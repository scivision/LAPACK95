SUBROUTINE LA_TEST_ZHPEVX( JOBS, RANGE, UPLO, N, AP, VL, VU, IL, IU, &
     &  ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     May 25, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => DP
      USE F95_LAPACK, ONLY: LA_HPEVX
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: N, LDZ, IL, IU
      INTEGER, INTENT(INOUT) :: INFO
      CHARACTER*1, INTENT(IN) :: JOBS, RANGE, UPLO
      INTEGER, INTENT(OUT) :: M
      REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
!  .. Array Arguments ..
      COMPLEX(WP), INTENT(INOUT) :: AP(1: N*(N+1)/2)
      REAL(WP), INTENT(OUT)::  W(1:N)
      COMPLEX(WP), INTENT(OUT) :: Z(1: LDZ, 1: N)
      INTEGER, INTENT(OUT) :: IWORK(1: 5*N), IFAIL(1:N)
      COMPLEX(WP) :: WORK(1:2*N)
      REAL(WP), INTENT(OUT) :: RWORK(1: 7*N)
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_HPEVX'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZHPEVX'
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IAP, IW, IZ1, IZ2, IIL, IIU
      REAL(WP) :: IVL, IVU, IABSTOL
      CHARACTER*1 :: IUPLO, IJOBS, IRANGE
!  .. Local Arrays ..
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
      LOGICAL LSAME
!  .. Executable Statements ..
      IAP = N*(N+1)/2; IUPLO = UPLO; IW = N; IJOBS = JOBS
      IZ1 =MAX(1,N); IZ2 = N; IWORK = N; IVL = VL; IVU=VU
      IIL = IL; IIU = IU; IABSTOL = ABSTOL; IRANGE=RANGE
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
      CASE (5)
      IVL = IVU+1
      IJOBS='V'; IRANGE='V'
      CALL LA_HPEVX(AP(1:IAP), W(1:IW), IUPLO, Z(1:IZ1, 1:IZ2),&
     &  IVL, IVU, M=M, IFAIL=IFAIL, ABSTOL=IABSTOL, INFO=INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE (6)
      CALL LA_HPEVX(AP(1:IAP), W(1:IW), IUPLO, Z(1:IZ1, 1:IZ2),&
     &  IVL, IVU, IIL, IIU, M, IFAIL, IABSTOL, INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE (7)
      IIL = IIU+1
      IJOBS='V'; IRANGE='I'
      CALL LA_HPEVX(AP(1:IAP), W(1:IW), IUPLO, Z(1:IZ1, 1:IZ2),&
     &  IL=IIL, IU=IIU, M=M, IFAIL=IFAIL, ABSTOL=IABSTOL,INFO=INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN 
      CASE (8)
      IIU = IZ2 + 1;  IIL = IIU
      IJOBS='V'; IRANGE='I'
      CALL LA_HPEVX(AP(1:IAP), W(1:IW), IUPLO, Z(1:IZ1, 1:IZ2),&
     &  IL=IIL, IU=IIU, M=M, IFAIL=IFAIL, ABSTOL=IABSTOL,INFO=INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN                                   
      CASE (10)
      IJOBS='V'; IRANGE='V'
      CALL LA_HPEVX(AP(1:IAP), W(1:IW), IUPLO,&
     &  VL=IVL, VU=IVU, M=M, IFAIL=IFAIL, ABSTOL=IABSTOL, INFO=INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN 
      CASE(:-1,9,11:)
      CALL UESTOP(SRNAMT)
      END SELECT
      IF (LSAME(IJOBS, 'V')) THEN
        IF (LSAME (IRANGE, 'V')) THEN
          CALL LA_HPEVX(AP(1:IAP), W(1:IW), IUPLO, Z(1:IZ1, 1:IZ2),&
     &      IVL, IVU, M=M, IFAIL=IFAIL, ABSTOL=IABSTOL, INFO=INFO )
        ELSE IF (LSAME (IRANGE, 'I')) THEN
          CALL LA_HPEVX(AP(1:IAP), W(1:IW), IUPLO, Z(1:IZ1, 1:IZ2),&
     &      IL=IIL, IU=IIU, M=M, IFAIL=IFAIL, ABSTOL=IABSTOL,INFO=INFO )
        ELSE
          CALL LA_HPEVX(AP(1:IAP), W(1:IW), IUPLO, Z(1:IZ1, 1:IZ2),&
     &      M=M, IFAIL=IFAIL, ABSTOL=IABSTOL, INFO=INFO )
        ENDIF
      ELSE
        IF (LSAME (IRANGE, 'V')) THEN
          CALL LA_HPEVX(AP(1:IAP), W(1:IW), IUPLO, &
     &      VL=IVL, VU=IVU, M=M, ABSTOL=IABSTOL, INFO=INFO )
        ELSE IF (LSAME (IRANGE, 'I')) THEN
          CALL LA_HPEVX(AP(1:IAP), W(1:IW), IUPLO, &
     &      IL=IIL, IU=IIU, M=M, ABSTOL=IABSTOL, INFO=INFO )
        ELSE
          CALL LA_HPEVX(AP(1:IAP), W(1:IW), IUPLO, &
     &      M=M, ABSTOL=IABSTOL, INFO=INFO )
        END IF
      ENDIF
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      END SUBROUTINE LA_TEST_ZHPEVX
      
