SUBROUTINE LA_TEST_CGEEV( JOBVL, JOBVR, N, A, LDA, W, VL, LDVL, VR, LDVR, WORK, LWORK, RWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     Jun 2, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_GEEV
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, LDA, LDVL, LDVR, LWORK
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: JOBVL, JOBVR
!  .. Array Arguments ..
   COMPLEX(WP), INTENT(INOUT) :: A(1: LDA, 1:N)
   COMPLEX(WP), INTENT(OUT)::  W(1: N), VL(1: LDVL, 1: N), &
&    VR(1: LDVR, 1:N)
   COMPLEX(WP), INTENT(OUT) :: WORK(1:LWORK)
   REAL(WP) :: RWORK(1: 2*N)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GEEV '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_CGEEV '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IA1, IA2, IW, IVL1, IVL2, IVR1, IVR2
   CHARACTER*1 :: IJOBVR, IJOBVL   
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
   LOGICAL LSAME
!  .. Executable Statements ..
   IA1 = N; IA2 = N; IW = N; IVL1 = N; IVL2 = N
   IVR1 = N; IVR2 = N; IJOBVL = JOBVL; IJOBVR = JOBVR
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
     CASE (1)
       IA1 = IA1 - 1
     CASE (2)
       IW = IW - 1
     CASE (3)
       IVL1 = IVL1 - 1
       IJOBVL = 'V'
     CASE (4)
       IVR1 = IVR1 - 1
       IJOBVR = 'V' 
     CASE(:-1,5:)
       CALL UESTOP(SRNAMT)
   END SELECT
   IF (LSAME(IJOBVR, 'V')) THEN
     IF (LSAME(IJOBVL,'V')) THEN
       CALL LA_GEEV(A(1:IA1, 1:IA2), W(1:IW), VL(1: IVL1, 1:IVL2), &
&        VR(1: IVR1, 1: IVR2), INFO)
     ELSE
       CALL LA_GEEV(A(1:IA1, 1:IA2),  W(1:IW), VR=VR(1: IVR1, 1: IVR2), &
&        INFO=INFO)
     END IF
   ELSE
     IF (LSAME(IJOBVL,'V')) THEN
       CALL LA_GEEV(A(1:IA1, 1:IA2),  W(1:IW), VL(1: IVL1, 1:IVL2), &
&        INFO = INFO)
     ELSE
       CALL LA_GEEV(A(1:IA1, 1:IA2),  W(1:IW), INFO = INFO )
     END IF                                         
   END IF
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
 END SUBROUTINE LA_TEST_CGEEV
 
      
