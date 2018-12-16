SUBROUTINE LA_TEST_SSYEVX( JOBZ, RANGE, UPLO,  N, A, LDA, VL, VU, IL, &
     &  IU, ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     September 11, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_SYEVX
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: N, LDA, LDZ, IL, IU, LWORK
      REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
      INTEGER, INTENT(OUT) :: M
      INTEGER, INTENT(INOUT) :: INFO
      CHARACTER*1, INTENT(IN) :: UPLO, JOBZ, RANGE
!  .. Array Arguments ..
      REAL(WP), INTENT(INOUT) :: A(1:LDA,1:N)
      REAL(WP), INTENT(OUT)::  W(1:N), WORK(1:LWORK), Z(1:LDZ,1:N)
      INTEGER, INTENT(OUT) :: IWORK(1:5*N), IFAIL(1:N)
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_SYEVX'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_SSYEVX'
      LOGICAL LSAME
      EXTERNAL LSAME
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IA1, IA2, IW, IIL, IIU, IIFAIL
      REAL(WP) :: IVU, IVL
      CHARACTER*1 :: IUPLO, IJOBZ, IRANGE   
!  .. Local Arrays ..
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
      IA1 = N; IA2 = N; IUPLO = UPLO; IW = N; IJOBZ = JOBZ
      IVU=VU; IVL = VL;IIL=IL; IIU = IU; IIFAIL = N
      I = INFO / 100; J = INFO - I*100; IRANGE=RANGE
      
      SELECT CASE(I)
      CASE (1)
      IA2 = IA1 - 1
      CASE (2)
        IW = IA1 - 1
      CASE (3)
        IJOBZ = 'T'
        CALL LA_SYEVX( A(1:IA1,1:IA2), W(1 :IW), IJOBZ, IUPLO, &
&         M=M, IFAIL=IFAIL(1:IIFAIL), ABSTOL=ABSTOL, INFO=INFO )
        CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
        RETURN
      CASE (4)
        IUPLO = 'T'
      CASE (5)
        IVL = IVU+1; IJOBZ='V'; IRANGE='V'
        CALL LA_SYEVX( A(1:IA1,1:IA2), W(1 :IW), UPLO=IUPLO, &
&         VL=IVL, VU=IVU,  M=M, IFAIL=IFAIL(1:IIFAIL), ABSTOL=ABSTOL,&
&         INFO=INFO )
        CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
        RETURN
      CASE (6)
        IRANGE='A'; IJOBZ='V'
        CALL LA_SYEVX( A(1:IA1,1:IA2), W(1 :IW), IJOBZ, IUPLO, &
&         IVL, IVU, IL, IU,  M, IFAIL(1:IIFAIL),&
&         ABSTOL, INFO )
        CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
        RETURN
      CASE(7)
        IIL = IIU + 1; IJOBZ='V'; IRANGE='I'
        CALL LA_SYEVX( A(1:IA1,1:IA2), W(1 :IW), IJOBZ, IUPLO, &
&         IL=IIL, IU=IIU, M=M,IFAIL=IFAIL(1 :IIFAIL), &
&         ABSTOL=ABSTOL, INFO=INFO )
        CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
        RETURN
      CASE (8)
        IIU = IA2 + 1;  IIL = IIU
        IJOBZ='V'; IRANGE='I'     
        CALL LA_SYEVX( A(1:IA1,1:IA2), W(1 :IW), IJOBZ, IUPLO, &
&         IL=IIL, IU=IIU, M=M, ABSTOL=ABSTOL, INFO=INFO )
        CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
        RETURN
      CASE(10)
        IIFAIL = IA2 - 1
        IJOBZ='V'; IRANGE='A'
        CALL LA_SYEVX( A(1:IA1,1:IA2), W(1 :IW), IJOBZ, IUPLO, &
&         M=M, IFAIL=IFAIL(1:IIFAIL), ABSTOL=ABSTOL, INFO=INFO )
        CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
    CASE(:-1,9,11:)
      CALL UESTOP(SRNAMT)
  END SELECT
  
  
  IF (LSAME(IRANGE,'A')) THEN
        IF (LSAME(IJOBZ, 'V')) THEN
          CALL LA_SYEVX( A(1:IA1,1:IA2), W(1 :IW), IJOBZ, IUPLO, &
&           M=M, IFAIL=IFAIL(1:IIFAIL), ABSTOL=ABSTOL, INFO=INFO )
        ELSE
          CALL LA_SYEVX( A(1:IA1,1:IA2), W(1 :IW), UPLO=IUPLO, &
&           M=M, ABSTOL=ABSTOL, INFO=INFO )
        END IF
      ELSE
        IF (LSAME(IRANGE, 'V')) THEN
          IF (LSAME(IJOBZ, 'V')) THEN
            CALL LA_SYEVX( A(1:IA1,1:IA2), W(1 :IW), IJOBZ, IUPLO, &
&             IVL, IVU,  M=M, IFAIL=IFAIL(1:IIFAIL), ABSTOL=ABSTOL,&
&             INFO=INFO )
          ELSE
            CALL LA_SYEVX( A(1:IA1,1:IA2), W(1 :IW), UPLO=IUPLO,  &
&             VL=IVL, VU=IVU, M=M, ABSTOL=ABSTOL, INFO=INFO )
          END IF
        ELSE
          IF (LSAME(IJOBZ, 'V')) THEN
            CALL LA_SYEVX( A(1:IA1,1:IA2), W(1 :IW), IJOBZ, IUPLO, &
&             IL=IIL, IU=IIU, M=M,IFAIL=IFAIL(1 :IIFAIL), &
&             ABSTOL=ABSTOL, INFO=INFO )
          ELSE
            CALL LA_SYEVX( A(1:IA1,1:IA2), W(1 :IW), UPLO=IUPLO, &
&             IL=IIL, IU=IIU, M=M, ABSTOL=ABSTOL, INFO=INFO )
          ENDIF
        END IF
      ENDIF
      IF (N/=0) Z(1:IA1, 1:M) = A(1:IA1, 1:M)
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_SSYEVX

