SUBROUTINE LA_TEST_ZGESVD(JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, RWORK, INFO)
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     May 31, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_GESVD
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: M, N, LDA, LDU, LDVT, LWORK
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: JOBU, JOBVT
!  .. Array Arguments ..
   COMPLEX(WP), INTENT(INOUT) :: A(1:LDA,1:N)
   COMPLEX(WP), INTENT(OUT):: WORK(1:LWORK)
   REAL(WP), INTENT(OUT) :: S(1: MIN(M,N))
   COMPLEX(WP), INTENT(OUT) :: U(1: LDU, 1:N), VT(1: LDVT, 1:N)
   REAL(WP) :: RWORK(1: 5*MIN(M,N))
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GESVD'
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZGESVD'
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IA1, IA2, IS, IU1, IU2, IVT1, IVT2, IRWORK
   CHARACTER*1 :: IJOBU, IJOBVT, IJOB
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
   LOGICAL LSAME
!  .. Executable Statements ..
   IA1 = M; IA2 = N; IJOBVT = JOBVT; IJOBU = JOBU
   IS = MIN(M,N); IU1 = M; IU2 = M; IVT1 = N; IVT2 = N;
   IJOB = 'N'; IRWORK = IS - 1
   I = INFO / 100; J = INFO - I*100
   IF (LSAME(IJOBVT, 'O')) THEN
     IJOB = 'V'
   ELSE
     IF (LSAME(IJOBU, 'O')) THEN
       IJOB = 'U'
     END IF
   END IF

   SELECT CASE(I)
     CASE (2)
       IS = IS - 1
     CASE (3)
       IU1 = IU1 - 1
       IJOBVT = 'A'
       IJOBU = 'A'
     CASE (4)
       IVT2 = IVT2 - 1
       IJOBVT = 'A'
       IJOBU = 'A'
     CASE (5)
       IRWORK = IRWORK - 2
     CASE (6)
       IJOB = 'T'
     CASE(:-1,1,7:)
       CALL UESTOP(SRNAMT)
   END SELECT

   IF ( LSAME(IJOBVT,'A') .OR. LSAME(IJOBVT,'S')) THEN
     IF (LSAME (IJOBU, 'A') .OR. LSAME(IJOBU,'S')) THEN
       CALL LA_GESVD( A(1:IA1,1:IA2), S(1:IS), U(1:IU1, 1:IU2), &
&        VT(1: IVT1, 1: IVT2), WW = RWORK(1: IRWORK), &
&        JOB=IJOB, INFO=INFO)
     ELSE
       CALL LA_GESVD( A(1:IA1,1:IA2), S(1:IS), &
&        VT=VT(1: IVT1, 1: IVT2), WW = RWORK(1: IRWORK), &
&        JOB=IJOB, INFO=INFO)
     END IF
   ELSE 
     IF (LSAME (IJOBU, 'A') .OR. LSAME(IJOBU,'S')) THEN
       CALL LA_GESVD(A(1:IA1,1:IA2), S(1:IS), U(1:IU1, 1:IU2), &
        WW = RWORK(1: IRWORK), JOB=IJOB, INFO=INFO)
     ELSE
       CALL LA_GESVD( A(1:IA1,1:IA2), S(1:IS), WW = RWORK(1: IRWORK), &
&        JOB=IJOB, INFO=INFO)
     END IF
   END IF
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
 END SUBROUTINE LA_TEST_ZGESVD
      
      
