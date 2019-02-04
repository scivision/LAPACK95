SUBROUTINE LA_TEST_SGGLSE( M, N, P, A, LDA, B, LDB, C, D, X, WORK, LWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark,
!     June 09, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_GGLSE
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: M, N, P, LDA, LDB, LWORK
   INTEGER, INTENT(INOUT) :: INFO
!  .. Array Arguments ..
   REAL(WP), INTENT(INOUT) :: A(1:LDA,1:N), B(1:LDB,1:N)
   REAL(WP), INTENT(INOUT) :: C(1: M), D(1: P)
   REAL(WP), INTENT(OUT) :: WORK(1: LWORK), X(1: N)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GGLSE'
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_SGGLSE'
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IA1, IA2, IB1, IB2, IC, ID, IX
!  .. Local Arrays ..
   INTEGER, SAVE, POINTER :: IWORK(:)
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IA1 = M; IA2 = N; IB1 = P; IB2 = N; IC = M; ID = P; IX = N
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
   CASE (2)
     IB2 = IB2 - 1
   CASE(3)
     IC = IC - 1
   CASE (4)
     ID = ID - 1
   CASE (5)
     IX = IX - 1
   CASE(:-1,1,6:)
      CALL UESTOP(SRNAMT)
   END SELECT

   CALL LA_GGLSE( A(1:IA1,1:IA2), B(1:IB1,1:IB2), C(1: IC), D(1: ID), &
&                 X(1: IX), INFO )
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_SGGLSE
