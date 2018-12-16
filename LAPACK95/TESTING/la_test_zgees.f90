SUBROUTINE LA_TEST_ZGEES( JOBVS, SORT, SELECT, N, A, LDA, SDIM, W, VS, LDVS, WORK, LWORK, RWORK, BWORK, INFO)
!
!  -- LAPACK95 interface driver routine (version 0.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     Jun 4, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
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
      LOGICAL FUNCTION SELECT( W )
      USE LA_PRECISION, ONLY: WP => DP
      COMPLEX(WP), INTENT(IN) :: W
  END FUNCTION SELECT
 END INTERFACE
   
!  .. Array Arguments ..
   COMPLEX(WP), INTENT(INOUT) :: A(1:LDA,1:N)
   COMPLEX(WP), INTENT(OUT) :: W(1: N), VS(1: LDVS, 1: N), &
&    WORK (1: LWORK)
   LOGICAL :: BWORK(1: N)
   LOGICAL LSAME
   REAL(WP) :: RWORK(1: N)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GEES '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZGEES '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IA1, IA2, IW, IVS1, IVS2
   CHARACTER*1 :: IJOBVS
!  .. Local Arrays ..
   INTEGER, SAVE, POINTER :: IWORK(:)
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IA1 = N; IA2 = N; IW = N; 
   IVS1 = N; IVS2 = N; IJOBVS = JOBVS
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
     CASE (1)
       IA2 = IA2-1
     CASE (2)
       IW = IW-1
     CASE(3)
       IVS1 = IVS1 - 1
       IJOBVS = 'V'
     CASE (4)
       CALL LA_GEES( A(1:IA1, 1: IA2), W(1:IW), &
&        VS(1: IVS1, 1: IVS2), SDIM=SDIM, INFO=INFO)
       RETURN
     CASE(:-1,5:)
       CALL UESTOP(SRNAMT)
   END SELECT 
   IF (LSAME (IJOBVS,'V')) THEN
     IF (LSAME(SORT, 'S')) THEN 
       CALL LA_GEES( A(1:IA1, 1: IA2), W(1:IW), &
&        VS(1: IVS1, 1: IVS2), SELECT, SDIM, INFO)
     ELSE 
       CALL LA_GEES( A(1:IA1, 1: IA2), W(1:IW), &
&        VS(1: IVS1, 1: IVS2), INFO=INFO)
     END IF
   ELSE
     IF (LSAME(SORT, 'S')) THEN 
       CALL LA_GEES( A(1:IA1, 1: IA2), W(1:IW), &
&        SELECT=SELECT, SDIM=SDIM, INFO=INFO)
     ELSE
       CALL LA_GEES( A(1:IA1, 1: IA2), W(1: IW), &
&        INFO=INFO)
     END IF
   END IF
   
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_ZGEES
