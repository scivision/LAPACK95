SUBROUTINE LA_TEST_SGEES( JOBVS, SORT, SELECT, N, A, LDA, SDIM, WR, WI, VS, LDVS, WORK, LWORK, BWORK, INFO)
!
!  -- LAPACK95 interface driver routine (version 0.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     May 21, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_GEES
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, LDA, LDVS, LWORK
   INTEGER, INTENT(OUT) :: SDIM
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: JOBVS, SORT
!   LOGICAL SELECT
   INTERFACE
      LOGICAL FUNCTION SELECT(WR, WI)
      USE LA_PRECISION, ONLY: WP => SP
      REAL(WP), INTENT(IN) :: WR, WI
  END FUNCTION SELECT
 END INTERFACE
   
!  .. Array Arguments ..
   REAL(WP), INTENT(INOUT) :: A(1:LDA,1:N)
   REAL(WP), INTENT(OUT) :: WR(1: N), WI(1: N), VS(1: LDVS, 1: N), &
     &  WORK (1: LWORK)
   LOGICAL :: BWORK(1: N)
   LOGICAL LSAME
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GEES '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_SGEES '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IA1, IA2, IWI, IWR, IVS1, IVS2
   CHARACTER*1 :: IJOBVS
!  .. Local Arrays ..
   INTEGER, SAVE, POINTER :: IWORK(:)
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IA1 = N; IA2 = N; IWI = N; IWR = N;
   IVS1 = N; IVS2 = N; IJOBVS = JOBVS
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
     CASE (1)
       IA2 = IA2-1
     CASE (2)
       IWR = IWR-1
     CASE(3)
       IWI = IWI - 1
     CASE (4)
       IVS1 = IVS1 - 1
       IJOBVS = 'V'
     CASE (5)
       CALL LA_GEES( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
     &   VS(1: IVS1, 1: IVS2), SDIM=SDIM, INFO=INFO)
       RETURN
     CASE(:-1,6:)
       CALL UESTOP(SRNAMT)
   END SELECT 
   IF (LSAME (IJOBVS,'V')) THEN
     IF (LSAME(SORT, 'S')) THEN 
       CALL LA_GEES( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
     &   VS(1: IVS1, 1: IVS2), SELECT, SDIM, INFO)
     ELSE 
       CALL LA_GEES( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
     &   VS(1: IVS1, 1: IVS2), INFO=INFO)
     END IF
   ELSE
     IF (LSAME(SORT, 'S')) THEN 
       CALL LA_GEES( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
     &   SELECT=SELECT, SDIM=SDIM, INFO=INFO)
     ELSE
       CALL LA_GEES( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
     &   INFO=INFO)
     END IF
   END IF
   
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_SGEES
