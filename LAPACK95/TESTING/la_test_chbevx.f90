SUBROUTINE LA_TEST_CHBEVX( JOBZ, RANGE, UPLO, N, KD, AB, LDAB, Q, LDQ, &
     &  VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, &
     &  WORK, RWORK, IWORK, IFAIL, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     May 14, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_HBEVX
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: N, LDAB, KD, LDQ, LDZ, IL, IU
      INTEGER, INTENT(INOUT) :: INFO
      INTEGER, INTENT(OUT) :: M
      CHARACTER*1, INTENT(IN) :: UPLO, JOBZ, RANGE
      REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
!  .. Array Arguments ..
      INTEGER, INTENT(OUT) :: IFAIL(1:N)
      COMPLEX(WP), INTENT(INOUT) :: AB(1:LDAB, 1:N)
      COMPLEX(WP), INTENT(OUT):: WORK(1 : N), Z(1:LDZ, 1:N), &
&       Q(1:LDQ,1:N)
      REAL(WP), INTENT(OUT)::  W(1:N)
      INTEGER, INTENT(OUT) :: IWORK(1: 5*N)
      REAL(WP), INTENT(OUT) :: RWORK(1: 7*N)
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_HBEVX'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_CHBEVX'
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IAB1, IAB2, IW, IZ1, IZ2, IQ1, IQ2, &
     &  IIL, IIU, IIFAIL 
      REAL(WP) :: IVL, IVU
      CHARACTER*1 :: IUPLO, IJOBZ, IRANGE   
!  .. Local Arrays ..
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
      LOGICAL LSAME
!  .. Executable Statements ..
      IAB1 = KD+1 ; IAB2 = N; IUPLO = UPLO; IW = N; IJOBZ = JOBZ
      IZ1 = N; IZ2 = N; IRANGE = RANGE; IVL=VL; IVU=VU; IQ1 = N
      IQ2 = N; IIL=IL; IIU=IU; IIFAIL=N
      I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
      CASE (1)
      IAB1 = -2
      CASE (2)
      IW = IW - 1
      CASE (3)
      IUPLO = 'T'   
      CASE (4)
      IZ1 = IZ1 - 1; IJOBZ = 'V'
      CASE (5)
      IVL = IVU+1; IJOBZ = 'V'; IRANGE = 'V'
      CASE (6)
      IJOBZ = 'V'; IRANGE = 'V'
      CALL LA_HBEVX( AB(1:IAB1,1:IAB2), W(1 :IW), IUPLO, &
     &  Z(1:IZ1, 1: IZ2), IVL, IVU, IIL, IIU, M, IFAIL, &
     &  Q, ABSTOL, INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE (7)
      IJOBZ = 'V'; IRANGE = 'I'
      IIL = IIU+1
      CALL LA_HBEVX( AB(1:IAB1,1:IAB2), W(1 :IW), IUPLO, &
     &  Z(1:IZ1, 1: IZ2), IL=IIL, IU=IIU, M=M, IFAIL=IFAIL(1:IIFAIL),&
     &  Q=Q(1:IQ1,1:IQ2), ABSTOL=ABSTOL, INFO=INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE (8)
      IJOBZ = 'V'; IRANGE = 'I'
      IIU = IZ1+1; IIL = IIU
      CALL LA_HBEVX( AB(1:IAB1,1:IAB2), W(1 :IW), IUPLO, &
     &  Z(1:IZ1, 1: IZ2), IL=IIL, IU=IIU, M=M, IFAIL=IFAIL(1:IIFAIL),&
     &  Q=Q(1:IQ1,1:IQ2), ABSTOL=ABSTOL, INFO=INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN   
      CASE (10)
      IIFAIL = IZ1 - 3
      IJOBZ = 'V'; IRANGE = 'V'
      CALL LA_HBEVX( AB(1:IAB1,1:IAB2), W(1 :IW), IUPLO,&
     &  Z(1:IZ1, 1: IZ2), IVL, IVU, M=M, IFAIL=IFAIL(1:IIFAIL),&
     &  Q=Q(1:IQ1,1:IQ2), ABSTOL=ABSTOL, INFO=INFO ) 
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN 
      CASE (11)
      IJOBZ = 'V'; IRANGE = 'V'
      IQ1 = IZ1 - 3
      CALL LA_HBEVX( AB(1:IAB1,1:IAB2), W(1 :IW), IUPLO,&
     &  Z(1:IZ1, 1: IZ2), IVL, IVU, M=M, IFAIL=IFAIL(1:IIFAIL),&
     &  Q=Q(1:IQ1,1:IQ2), ABSTOL=ABSTOL, INFO=INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN  
      CASE(:-1,9,12:)
      CALL UESTOP(SRNAMT)
      END SELECT
      IF (LSAME (IJOBZ, 'V')) THEN
        IF(LSAME(IRANGE,'V')) THEN
          CALL LA_HBEVX( AB(1:IAB1,1:IAB2), W(1 :IW), IUPLO,&
     &      Z(1:IZ1, 1: IZ2), IVL, IVU, M=M, IFAIL=IFAIL(1:IIFAIL),&
     &      Q=Q(1:IQ1,1:IQ2), ABSTOL=ABSTOL, INFO=INFO )
        ELSE IF (LSAME(IRANGE,'I')) THEN
          CALL LA_HBEVX( AB(1:IAB1,1:IAB2), W(1 :IW), IUPLO, &
     &      Z(1:IZ1, 1: IZ2), IL=IIL, IU=IIU, M=M, IFAIL=IFAIL(1:IIFAIL),&
     &      Q=Q(1:IQ1,1:IQ2), ABSTOL=ABSTOL, INFO=INFO )
        ELSE IF (LSAME(IRANGE,'A')) THEN
          CALL LA_HBEVX( AB(1:IAB1,1:IAB2), W(1 :IW), IUPLO,&
     &      Z(1:IZ1, 1: IZ2), M=M, IFAIL=IFAIL(1:IIFAIL), &
     &      Q=Q(1:IQ1,1:IQ2), ABSTOL=ABSTOL, INFO=INFO )
        ENDIF
      ELSE
        IF (LSAME(IRANGE,'V')) THEN
          CALL LA_HBEVX( AB(1:IAB1,1:IAB2), W(1 :IW), IUPLO, &
     &      VL=IVL, VU=IVU, M=M, ABSTOL=ABSTOL, INFO=INFO )
        ELSE IF (LSAME(IRANGE,'I')) THEN
          CALL LA_HBEVX( AB(1:IAB1,1:IAB2), W(1 :IW), IUPLO,&
     &      IL=IIL, IU=IIU, M=M, ABSTOL=ABSTOL, INFO=INFO )
        ELSE IF (LSAME(IRANGE,'A')) THEN
          CALL LA_HBEVX( AB(1:IAB1,1:IAB2), W(1 :IW), IUPLO,&
     &      M=M, ABSTOL=ABSTOL, INFO=INFO )
        ENDIF 
      ENDIF
      
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
    END SUBROUTINE LA_TEST_CHBEVX
