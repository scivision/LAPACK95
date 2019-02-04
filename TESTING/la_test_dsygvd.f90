SUBROUTINE LA_TEST_DSYGVD( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK, LWORK, IWORK, LIWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     August 19, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => DP
      USE F95_LAPACK, ONLY: LA_SYGVD
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: ITYPE, N, LDA, LDB, LWORK, LIWORK
      INTEGER, INTENT(INOUT) :: INFO
      CHARACTER*1, INTENT(IN) :: JOBZ, UPLO
!  .. Array Arguments ..
      REAL(WP), INTENT(INOUT) :: A(1:LDA,1:N), B(1:LDB, 1:N)
      REAL(WP), INTENT(OUT)::  W(1:N), WORK(1:LWORK)
      INTEGER, INTENT(OUT) :: IWORK(1:LIWORK)
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_SYGVD'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_DSYGVD'
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IA1, IA2, IW, IB1, IB2, IITYPE
      CHARACTER*1 :: IUPLO, IJOBZ   
!  .. Local Arrays ..
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
      IA1 = N; IA2 = N; IUPLO = UPLO; IW = N; IJOBZ = JOBZ
      IB1 = N; IB2 = N; IITYPE = ITYPE
      I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
       CASE (1)
         IA2 = IA1 - 1
       CASE (2)
         IB1 = IA1 - 1
       CASE (3)
         IW = IA1 - 1
       CASE (4)
         IITYPE = 22   
       CASE (5)
         IJOBZ = 'T'
       CASE (6)
         IUPLO = 'T'
       CASE(:-1,7:)
         CALL UESTOP(SRNAMT)
      END SELECT
      CALL LA_SYGVD( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), W(1 :IW), &
     &  IITYPE, IJOBZ, IUPLO, INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      END SUBROUTINE LA_TEST_DSYGVD
