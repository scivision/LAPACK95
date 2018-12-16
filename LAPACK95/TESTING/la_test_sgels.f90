SUBROUTINE LA_TEST_SGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark,
!     April 19, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_GELS
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: M, N, NRHS, LDA, LDB, LWORK
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: TRANS   
!  .. Array Arguments ..
   REAL(WP), INTENT(INOUT) :: A(1:LDA,1:N), B(1:LDB,1:NRHS)
   REAL(WP), INTENT(OUT) :: WORK(LWORK)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GELS '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_SGELS '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IA1, IA2, IB1, IB2
   CHARACTER*1   :: ITRANS
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IA1 = M; IA2 = N; IB1 = MAX(1,M,N); IB2 = NRHS; ITRANS = TRANS
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
   CASE(0)
      IF (NRHS==1) THEN
        CALL LA_GELS( A(1:IA1,1:IA2), B(1:IB1,1), ITRANS, INFO )
        INFO = INFOTC
      ELSE
        CALL LA_GELS( A(1:IA1,1:IA2), B(1:IB1,1:IB2), ITRANS, INFO )
        INFO = INFOTC
      ENDIF 
   CASE (2)
      IB1 = max(1, n, m) -1
   CASE(3)
      ITRANS = 'P'
      CASE(:-1,1,4:)
      CALL UESTOP(SRNAMT)
   END SELECT
      IF( I /= 0) THEN
      SELECT CASE (NRHS)
      CASE (2:)
         CALL LA_GELS( A(1:IA1,1:IA2), B(1:IB1,1:IB2), ITRANS, INFO )
      CASE(1)
         CALL LA_GELS( A(1:IA1,1:IA2), B(1:IB1,1), ITRANS, INFO )
         CASE(:-1)
         CALL UESTOP(SRNAMT)
      END SELECT
   END IF
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_SGELS
