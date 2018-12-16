SUBROUTINE LA_TEST_DGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK, WORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark,
!     April 19, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_GELSX
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB
   INTEGER, INTENT(INOUT) :: INFO
   INTEGER, INTENT(IN) :: RANK
   REAL(WP), INTENT(IN) :: RCOND
!  .. Array Arguments ..
   REAL(WP), INTENT(INOUT) :: A(1:LDA,1:N), B(1:LDB,1:NRHS)
   REAL(WP), INTENT(OUT) :: WORK(1: MAX(MIN(M,N)+3*N, 2*MIN(M,N)+NRHS*NRHS))
   INTEGER, INTENT(INOUT) :: JPVT(1:N)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GELSX'
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_DGELSX'
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IA1, IA2, IB1, IB2, IJPVT
!  .. Local Arrays ..
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IA1 = M; IA2 = N; IB1 = MAX(1,M,N); IB2 = NRHS; IJPVT = N
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
   CASE(0)
      IF (NRHS==1) THEN
        CALL LA_GELSX( A(1:IA1,1:IA2), B(1:IB1,1), RANK,  &
     &    JPVT=JPVT(1:IJPVT), RCOND=RCOND, INFO=INFO )
        INFO = INFOTC
      ELSE
        CALL LA_GELSX( A(1:IA1,1:IA2), B(1:IB1,1:IB2), RANK, & 
     &    JPVT=JPVT(1:IJPVT),  RCOND=RCOND, INFO=INFO )
        INFO = INFOTC
      ENDIF 
   CASE (2)
      IB1 = MAX(1, N, M) -1
   CASE(4)
      IJPVT = IA2 - 1
   CASE(:-1,1,3, 5:)
      CALL UESTOP(SRNAMT)
   END SELECT
      IF( I /= 0) THEN
      SELECT CASE (NRHS)
      CASE (2:)
         CALL LA_GELSX( A(1:IA1,1:IA2), B(1:IB1,1:IB2), RANK,&
     &         JPVT=JPVT(1:IJPVT), RCOND=RCOND, INFO=INFO )
      CASE(1)
         CALL LA_GELSX( A(1:IA1,1:IA2), B(1:IB1,1), RANK, &
     &         JPVT=JPVT(1 :IJPVT), RCOND=RCOND, INFO=INFO)
      CASE(:-1)
         CALL UESTOP(SRNAMT)
      END SELECT
   END IF
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_DGELSX
