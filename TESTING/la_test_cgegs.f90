SUBROUTINE LA_TEST_CGEGS(JOBVSL, JOBVSR, N, A, LDA, B, LDB, ALPHA, BETA, VSL, LDVSL, VSR, LDVSR, WORK, LWORK, RWORK, INFO)
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     May 5, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_GEGS
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, LDA, LDB, LDVSL, LDVSR, LWORK
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: JOBVSL, JOBVSR
!  .. Array Arguments ..
   COMPLEX(WP), INTENT(INOUT) :: A(1:LDA,1:N), B(1:LDB, 1:N)
   COMPLEX(WP), INTENT(OUT):: WORK(1:LWORK)
   COMPLEX(WP), INTENT(OUT) :: ALPHA(1:N), BETA(1:N), &
&    VSL(1: LDVSL, 1:N), VSR(1: LDVSR, 1:N)
   REAL(WP), INTENT(OUT) :: RWORK(1:8*N)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GEGS '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_CGEGS '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IA1, IA2, IB1, IB2, IALPHA, IBETA, IVSL1, &
&    IVSL2, IVSR1, IVSR2
   CHARACTER*1 :: IJOBVSL, IJOBVSR
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
   LOGICAL LSAME
!  .. Executable Statements ..
   IA1 = N; IA2 = N; IJOBVSL = JOBVSL; IJOBVSR = JOBVSR
   IB1 = N; IB2 = N; IALPHA = N; IBETA = N
   IVSL1 = N; IVSL2 = N; IVSR1 = N; IVSR2 = N
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
     CASE (1)
       IA2 = IA1 - 1
     CASE (2)
       IB1 = IA1 - 1
     CASE (3)
       IALPHA = IA1 - 1
     CASE (4)
       IALPHA = IA1 - 1
     CASE (5)
       IBETA  = IA1 - 1
     CASE (6)
       IVSL1 = IA1 - 1
     CASE (7)
       IVSR1 = IA1 - 1
     CASE(:-1,8:)
       CALL UESTOP(SRNAMT)
   END SELECT

   IF (LSAME(IJOBVSL,'V')) THEN
     IF (LSAME (IJOBVSR, 'V')) THEN
       CALL LA_GEGS( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&        BETA(1: IBETA), VSL(1:IVSL1,1:IVSL2), &
&        VSR(1:IVSR1,1:IVSR2), INFO)    
     ELSE
       CALL LA_GEGS( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&        BETA(1: IBETA), VSL(1:IVSL1,1:IVSL2), &
&        INFO = INFO)
     END IF
   ELSE 
     IF (LSAME (IJOBVSR, 'V')) THEN
       CALL LA_GEGS( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&        BETA(1: IBETA),&
&        VSR = VSR(1:IVSR1,1:IVSR2), INFO = INFO)
     ELSE
       CALL LA_GEGS( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&        BETA(1: IBETA), &
&        INFO = INFO)
     END IF
   END IF
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
 END SUBROUTINE LA_TEST_CGEGS
      
      
