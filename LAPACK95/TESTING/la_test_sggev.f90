SUBROUTINE LA_TEST_SGGEV(JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHAR, ALPHAI, BETA, VL, LDVL, VR, LDVR, WORK, LWORK, INFO)
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     SEPTEMBER 5, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GGEV
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: N, LDA, LDB, LDVL, LDVR, LWORK
      INTEGER, INTENT(INOUT) :: INFO
      CHARACTER*1, INTENT(IN) :: JOBVL, JOBVR
!  .. Array Arguments ..
      REAL(WP), INTENT(INOUT) :: A(1:LDA,1:N), B(1:LDB, 1:N)
      REAL(WP), INTENT(OUT):: WORK(1:LWORK)
      REAL(WP), INTENT(OUT) :: ALPHAR(1:N), ALPHAI(1:N), BETA(1:N), &
     &  VL(1: LDVL, 1:N), VR(1: LDVR, 1:N)
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GGEV '
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_SGGEV '
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IA1, IA2, IB1, IB2, IALPHAI, IALPHAR, IBETA, IVL1, &
     &  IVL2, IVR1, IVR2
      CHARACTER*1 :: IJOBVL, IJOBVR
!  .. Local Arrays ..
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
      LOGICAL LSAME
!  .. Executable Statements ..
      IA1 = N; IA2 = N; IJOBVL = JOBVL; IJOBVR = JOBVR
      IB1 = N; IB2 = N; IALPHAR = N; IALPHAI = N; IBETA = N
      IVL1 = N; IVL2 = N; IVR1 = N; IVR2 = N
      I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
      CASE (1)
      IA2 = IA1 - 1
      CASE (2)
      IB1 = IA1 - 1
      CASE (3)
      IALPHAR = IA1 - 1
      CASE (4)
      IALPHAI = IA1 - 1
      CASE (5)
      IBETA  = IA1 - 1
      CASE (6)
      IJOBVL = 'V'; IJOBVR = 'N'
      IVL1 = IA1 - 1
      CALL LA_GGEV( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHAR(1: IALPHAR), &
     &  ALPHAI(1:IALPHAI), BETA(1: IBETA), VL(1:IVL1,1:IVL2), &
     &  INFO = INFO)
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE (7)
      IJOBVL = 'N'; IJOBVR = 'V'  
      IVR1 = IA1 - 1
      CALL LA_GGEV( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHAR(1: IALPHAR), &
     &  ALPHAI(1:IALPHAI), BETA(1: IBETA),&
     &  VR = VR(1:IVR1,1:IVR2), INFO = INFO)
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN 
      CASE(:-1,8:)
      CALL UESTOP(SRNAMT)
      END SELECT
      IF (LSAME(IJOBVL,'V')) THEN
        IF (LSAME (IJOBVR, 'V')) THEN
          CALL LA_GGEV( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHAR(1: IALPHAR), &
     &      ALPHAI(1:IALPHAI), BETA(1: IBETA), VL(1:IVL1,1:IVL2), &
     &      VR(1:IVR1,1:IVR2), INFO)    
        ELSE
          CALL LA_GGEV( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHAR(1: IALPHAR), &
     &      ALPHAI(1:IALPHAI), BETA(1: IBETA), VL(1:IVL1,1:IVL2), &
     &      INFO = INFO)
        END IF
      ELSE
        IF (LSAME (IJOBVR, 'V')) THEN
          CALL LA_GGEV( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHAR(1: IALPHAR), &
     &      ALPHAI(1:IALPHAI), BETA(1: IBETA),&
     &      VR = VR(1:IVR1,1:IVR2), INFO = INFO)
        ELSE
          CALL LA_GGEV( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHAR(1: IALPHAR), &
     &    ALPHAI(1:IALPHAI), BETA(1: IBETA), &
     &      INFO = INFO)
        END IF
      END IF
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      END SUBROUTINE LA_TEST_SGGEV
      
