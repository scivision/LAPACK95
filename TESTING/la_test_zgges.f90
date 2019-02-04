SUBROUTINE LA_TEST_ZGGES(JOBVSL, JOBVSR, SORT, SELECT, N, A, LDA, B, LDB, SDIM, &
&   ALPHA, BETA, VSL, LDVSL, VSR, LDVSR, WORK, LWORK, RWORK, BWORK, INFO)
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     September 25, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_GGES
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, LDA, LDB, LDVSL, LDVSR, LWORK
      INTEGER, INTENT(INOUT) :: INFO
      INTEGER, INTENT(OUT) :: SDIM
   CHARACTER*1, INTENT(IN) :: JOBVSL, JOBVSR, SORT
!  .. Array Arguments ..
   COMPLEX(WP), INTENT(INOUT) :: A(1:LDA,1:N), B(1:LDB, 1:N)
   COMPLEX(WP), INTENT(OUT):: WORK(1:LWORK)
      LOGICAL :: BWORK(1: N)
      REAL(WP) :: RWORK(1: 8*N)
   COMPLEX(WP), INTENT(OUT) :: ALPHA(1:N), BETA(1:N), &
&    VSL(1: LDVSL, 1:N), VSR(1: LDVSR, 1:N)

   INTERFACE
     LOGICAL FUNCTION SELECT(ALPHA, BETA)
     USE LA_PRECISION, ONLY: WP => DP
     COMPLEX(WP), INTENT(IN) :: ALPHA, BETA
   END FUNCTION SELECT
 END INTERFACE
 OPTIONAL :: SELECT 
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GGES '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZGGES '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IA1, IA2, IB1, IB2, IALPHA, IBETA, IVSL1, &
     &  IVSL2, IVSR1, IVSR2
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
       IBETA  = IA1 - 1
     CASE (5)
       IVSL1 = IA1 - 1; IJOBVSL = 'V';IJOBVSR = 'V'
         CALL LA_GGES( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), &
&          ALPHA=ALPHA(1: IALPHA),  &
&          BETA=BETA(1: IBETA), VSL=VSL(1:IVSL1,1:IVSL2), &
&          VSR=VSR(1:IVSR1,1:IVSR2), SDIM=SDIM,& 
&          INFO=INFO)
         CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
         RETURN
       CASE (6)
         IVSR1 = IA1 - 1; IJOBVSL = 'V';IJOBVSR = 'V'
         CALL LA_GGES( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), &
&          ALPHA=ALPHA(1: IALPHA),  &
&          BETA=BETA(1: IBETA), VSL=VSL(1:IVSL1,1:IVSL2), &
&          VSR=VSR(1:IVSR1,1:IVSR2),  SDIM=SDIM,&
&          INFO=INFO)
         CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
         RETURN         
       CASE(:-1,7:)
       CALL UESTOP(SRNAMT)
   END SELECT

   IF (LSAME(SORT, 'N')) THEN

     IF (LSAME(IJOBVSL,'V')) THEN
       IF (LSAME (IJOBVSR, 'V')) THEN
         CALL LA_GGES( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), &
     &     ALPHA=ALPHA(1: IALPHA),  &
     &     BETA=BETA(1: IBETA), VSL=VSL(1:IVSL1,1:IVSL2), &
     &     VSR=VSR(1:IVSR1,1:IVSR2), SDIM=SDIM, &
     &     INFO=INFO)
       ELSE
         CALL LA_GGES( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), &
     &     ALPHA=ALPHA(1: IALPHA), &
     &     BETA=BETA(1: IBETA), VSL=VSL(1:IVSL1,1:IVSL2),& 
     &     SDIM=SDIM, INFO=INFO)    
       END IF
      ELSE 
        IF (LSAME (IJOBVSR, 'V')) THEN
          CALL LA_GGES( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), &
     &      ALPHA=ALPHA(1: IALPHA),&
     &      BETA=BETA(1: IBETA), VSR = VSR(1:IVSR1,1:IVSR2), &
     &      SDIM=SDIM, INFO = INFO)
        ELSE
          CALL LA_GGES( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), &
     &      ALPHA=ALPHA(1: IALPHA), &
     &      BETA=BETA(1: IBETA), SDIM=SDIM, INFO = INFO)
        END IF
      END IF
      ELSE
        IF (LSAME(IJOBVSL,'V')) THEN
          IF (LSAME (IJOBVSR, 'V')) THEN
            CALL LA_GGES( A(1:IA1,1:IA2), B(1:IB1, 1:IB2),&
&             ALPHA(1: IALPHA), BETA(1: IBETA), &
&             VSL(1:IVSL1,1:IVSL2), VSR(1:IVSR1,1:IVSR2), &
&             SELECT, SDIM, INFO)    
          ELSE
            CALL LA_GGES( A(1:IA1,1:IA2), B(1:IB1, 1:IB2),&
     &        ALPHA(1:IALPHA), BETA(1: IBETA),&
&             VSL=VSL(1:IVSL1,1:IVSL2), SELECT=SELECT, &
&             SDIM=SDIM, INFO=INFO) 
          END IF
        ELSE 
          IF (LSAME (IJOBVSR, 'V')) THEN
            CALL LA_GGES( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), &
     &         ALPHA(1:IALPHA), BETA(1: IBETA),&
&             VSR = VSR(1:IVSR1,1:IVSR2), SELECT=SELECT, SDIM=SDIM, &
&             INFO = INFO)
          ELSE
         CALL LA_GGES( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), &
     &     ALPHA(1:IALPHA), BETA(1: IBETA), SELECT=SELECT, SDIM=SDIM,&
     &     INFO = INFO)
       END IF
      END IF
      END IF
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_ZGGES
