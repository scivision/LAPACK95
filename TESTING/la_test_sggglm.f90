SUBROUTINE LA_TEST_SGGGLM( N, M, P, A, LDA, B, LDB, D, X, Y, WORK, LWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark,
!     June 09, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_GGGLM
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: M, N, P, LDA, LDB, LWORK
   INTEGER, INTENT(INOUT) :: INFO
!  .. Array Arguments ..
   REAL(WP), INTENT(INOUT) :: A(1:LDA,1:M), B(1:LDB,1:P)
   REAL(WP), INTENT(INOUT) :: D(1: N)
   REAL(WP), INTENT(OUT) :: WORK(1: LWORK), X(1: M), Y(1: P)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GGGLM'
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_SGGGLM'
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IA1, IA2, IB1, IB2, IY, ID, IX
!  .. Local Arrays ..
   INTEGER, SAVE, POINTER :: IWORK(:)
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IA1 = N; IA2 = M; IB1 = N; IB2 = P; ID = N; IX = M; IY = P
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
   CASE (2)
     IB1 = IB1 - 1
   CASE (3)
     ID = ID - 1
   CASE (4)
     IX = IX - 1
   CASE (5)
     IY = IY - 1
   CASE(:-1,1,6:)
      CALL UESTOP(SRNAMT)
   END SELECT

   CALL LA_GGGLM( A(1:IA1,1:IA2), B(1:IB1,1:IB2),  D(1: ID), &
&                 X(1: IX), Y(1: IY), INFO )
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_SGGGLM
