SUBROUTINE LA_TEST_CHEGVD( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK, LWORK, RWORK, LRWORK, IWORK, LIWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     August 19, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_HEGVD
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: ITYPE, N, LDA, LDB, LWORK, LIWORK, LRWORK
      INTEGER, INTENT(INOUT) :: INFO
      CHARACTER*1, INTENT(IN) :: JOBZ, UPLO
!  .. Array Arguments ..
      COMPLEX(WP), INTENT(INOUT) :: A(1:LDA,1:N), B(1:LDB, 1:N)
      REAL(WP), INTENT(OUT)::  W(1:N)
      COMPLEX(WP), INTENT(OUT) :: WORK(1:LWORK)
      INTEGER, INTENT(OUT) :: IWORK(1:LIWORK)
      REAL(WP), INTENT(OUT) :: RWORK(1: LRWORK)
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_HEGVD'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_CHEGVD'
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
      CALL LA_HEGVD( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), W(1 :IW), &
     &  IITYPE, IJOBZ, IUPLO, INFO )
      
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      END SUBROUTINE LA_TEST_CHEGVD
      
      
      
