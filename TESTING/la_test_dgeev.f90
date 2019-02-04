SUBROUTINE LA_TEST_DGEEV( JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR, LDVR, WORK, LWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     May 29, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_GEEV
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, LDA, LDVL, LDVR, LWORK
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: JOBVL, JOBVR
!  .. Array Arguments ..
   REAL(WP), INTENT(INOUT) :: A(1: LDA, 1:N)
   REAL(WP), INTENT(OUT)::  WR(1: N), WI(1: N), VL(1: LDVL, 1: N), &
&    VR(1: LDVR, 1:N)
   REAL(WP), INTENT(OUT) :: WORK(1:LWORK)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GEEV '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_DGEEV '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IA1, IA2, IWR, IWI, IVL1, IVL2, IVR1, IVR2
   CHARACTER*1 :: IJOBVR, IJOBVL   
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
   LOGICAL LSAME
!  .. Executable Statements ..
   IA1 = N; IA2 = N; IWI = N; IWR = N; IVL1 = N; IVL2 = N
   IVR1 = N; IVR2 = N; IJOBVL = JOBVL; IJOBVR = JOBVR
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
     CASE (1)
       IA1 = IA1 - 1
     CASE (2)
       IWR = IWR - 1
     CASE (3)
       IWI = IWI - 1
     CASE (4)
       IVL1 = IVL1 - 1
       IJOBVL = 'V'
     CASE (5)
       IVR1 = IVR1 - 1
       IJOBVR = 'V' 
     CASE(:-1,6:)
       CALL UESTOP(SRNAMT)
   END SELECT
   IF (LSAME(IJOBVR, 'V')) THEN
     IF (LSAME(IJOBVL,'V')) THEN
       CALL LA_GEEV(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&        VL(1: IVL1, 1:IVL2), VR(1: IVR1, 1: IVR2), INFO)
     ELSE
       CALL LA_GEEV(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&        VR=VR(1: IVR1, 1: IVR2), INFO=INFO)
     END IF
   ELSE
     IF (LSAME(IJOBVL,'V')) THEN
       CALL LA_GEEV(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&        VL(1: IVL1, 1:IVL2),INFO = INFO)
     ELSE
       CALL LA_GEEV(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&        INFO = INFO)
     END IF                                         
   END IF
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
 END SUBROUTINE LA_TEST_DGEEV
 
      
