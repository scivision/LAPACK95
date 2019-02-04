SUBROUTINE LA_TEST_DGGSVD(JOBU, JOBV, JOBQ, M, N, P, K, L, A, LDA, B, LDB, ALPHA, BETA, U, LDU, V, LDV, Q, LDQ, WORK, IWORK, INFO)
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     Jun 2, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => DP
      USE F95_LAPACK, ONLY: LA_GGSVD
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: M, N, P, LDA, LDB,  LDU, LDV, LDQ
      INTEGER, INTENT(OUT) :: K, L
      INTEGER, INTENT(INOUT) :: INFO
      CHARACTER*1, INTENT(IN) :: JOBU, JOBV, JOBQ
!  .. Array Arguments ..
      REAL(WP), INTENT(INOUT) :: A(1:LDA,1: N), B(1: LDB, 1: N)
      REAL(WP), INTENT(OUT):: WORK(1: MAX(3*N, M, P)+N)
      REAL(WP), INTENT(OUT) :: U(1: LDU, 1:M), ALPHA(1: N), BETA(1: N), &
     &  V(1: LDV, 1:P), Q(1: LDQ, 1: N)
      INTEGER, INTENT(OUT) :: IWORK(1: N)
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GGSVD'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_DGGSVD'
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IA1, IA2, IB1, IB2, IU1, IU2, IV1, IV2, IQ1, &
     &  IQ2, IALPHA, IBETA
      CHARACTER*1 :: IJOBU, IJOBV, IJOBQ
!  .. Local Arrays ..
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
      LOGICAL LSAME
!  .. Executable Statements ..
      IA1 = M; IA2 = N; IB1 = P; IB2 = N
      IJOBV = JOBV; IJOBU = JOBU; IJOBQ = JOBQ
      IU1 = MAX(1,M); IU2 = M
      IALPHA = N; IBETA = N; IV1 = MAX(1,P); IV2 = P
      IQ1 = MAX(1,N); IQ2 = N
      
      I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
      CASE (2)
      IB2 = IB2 - 1
      CASE (3)
      IALPHA = IALPHA - 1
      CASE (4)
      IBETA = IBETA- 1
      CASE (7)
      IU1 = IU1 - 1
      IJOBU = 'U'
      CASE (8)
      IV1 = IV1 - 1
      IJOBV = 'V'
      CASE (9)
      IQ1 = IQ1 - 1
      IJOBQ = 'Q'
      CASE(:-1,1,5,6,10:)
      CALL UESTOP(SRNAMT)
      END SELECT
      
      IF (LSAME(IJOBV,'V')) THEN
        IF (LSAME (IJOBU, 'U')) THEN
          IF (LSAME (IJOBQ, 'Q')) THEN
            CALL LA_GGSVD( A(1:IA1,1:IA2), B(1: IB1, 1:IB2), &
     &        ALPHA(1: IALPHA), BETA(1: IBETA), K, L, U(1: IU1, 1: IU2), &
     &        V(1: IV1, 1: IV2), Q(1: IQ1, 1: IQ2), IWORK, INFO)
       ELSE
         CALL LA_GGSVD( A(1:IA1,1:IA2), B(1: IB1, 1:IB2), &
     &     ALPHA(1: IALPHA), BETA(1: IBETA), K, L, U(1: IU1, 1: IU2), &
     &     V(1: IV1, 1: IV2), IWORK=IWORK, INFO=INFO)
       END IF
      ELSE
        IF (LSAME (IJOBQ, 'Q')) THEN
          CALL LA_GGSVD( A(1:IA1,1:IA2), B(1: IB1, 1:IB2), &
     &      ALPHA(1: IALPHA), BETA(1: IBETA), K, L, &
     &      V=V(1: IV1, 1: IV2), Q=Q(1: IQ1, 1: IQ2), IWORK=IWORK, INFO=INFO)
        ELSE
          CALL LA_GGSVD( A(1:IA1,1:IA2), B(1: IB1, 1:IB2), &
     &      ALPHA(1: IALPHA), BETA(1: IBETA), K, L, &
     &      V=V(1: IV1, 1: IV2), IWORK=IWORK, INFO=INFO)
        END IF
      ENDIF
      ELSE
        IF (LSAME (IJOBU, 'U')) THEN
          IF (LSAME (IJOBQ, 'Q')) THEN
            CALL LA_GGSVD( A(1:IA1,1:IA2), B(1: IB1, 1:IB2), &
     &        ALPHA(1: IALPHA), BETA(1: IBETA), K, L, U(1: IU1, 1: IU2), &
     &        Q=Q(1: IQ1, 1: IQ2), IWORK=IWORK, INFO=INFO)
          ELSE
            CALL LA_GGSVD( A(1:IA1,1:IA2), B(1: IB1, 1:IB2), &
     &        ALPHA(1: IALPHA), BETA(1: IBETA), K, L, U(1: IU1, 1: IU2), &
     &        IWORK=IWORK, INFO=INFO)
          END IF
        ELSE
          IF (LSAME (IJOBQ, 'Q')) THEN
            CALL LA_GGSVD( A(1:IA1,1:IA2), B(1: IB1, 1:IB2), &
     &        ALPHA(1: IALPHA), BETA(1: IBETA), K, L, &
     &        Q=Q(1: IQ1, 1: IQ2), IWORK=IWORK, INFO=INFO)
          ELSE
            CALL LA_GGSVD( A(1:IA1,1:IA2), B(1: IB1, 1:IB2), &
     &        ALPHA(1: IALPHA), BETA(1: IBETA), K, L, IWORK=IWORK, &
     &        INFO=INFO)
          END IF
        ENDIF
      ENDIF
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
 END SUBROUTINE LA_TEST_DGGSVD

      
