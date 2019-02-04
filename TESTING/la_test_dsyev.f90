SUBROUTINE LA_TEST_DSYEV( JOBS, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     May 5, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_SYEV
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, LDA, LWORK
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: UPLO
   CHARACTER*1, INTENT(IN) :: JOBS
!  .. Array Arguments ..
   REAL(WP), INTENT(INOUT) :: A(1:LDA,1:N)
   REAL(WP), INTENT(OUT)::  W(1:N), WORK(1:LWORK)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_SYEV '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_DSYEV '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IA1, IA2, IW
   CHARACTER*1 :: IUPLO, IJOBS   
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IA1 = N; IA2 = N; IUPLO = UPLO; IW = N; IJOBS = JOBS
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
     CASE (1)
       IA2 = IA1 - 1
     CASE (2)
       IW = IA1 - 1
     CASE (3)
       IJOBS = 'T'   
     CASE (4)
       IUPLO = 'T'
     CASE(:-1,5:)
       CALL UESTOP(SRNAMT)
   END SELECT
   
   CALL LA_SYEV( A(1:IA1,1:IA2), W(1 :IW), IJOBS, IUPLO, INFO )
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
 END SUBROUTINE LA_TEST_DSYEV
      
      
