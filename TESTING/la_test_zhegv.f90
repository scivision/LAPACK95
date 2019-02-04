SUBROUTINE LA_TEST_ZHEGV( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK, LWORK, RWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     May 20, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_HEGV
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: ITYPE, N, LDA, LDB, LWORK
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: JOBZ, UPLO
!  .. Array Arguments ..
   COMPLEX(WP), INTENT(INOUT) :: A(1:LDA, 1:N), B(1:LDB, 1:N)
   REAL(WP), INTENT(OUT)::  W(1:N), RWORK(1: MAX(1, 3*N-2))
   COMPLEX(WP), INTENT(OUT):: WORK(1:LWORK)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_HEGV '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZHEGV '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IA1,IA2, IW, IB1, IB2, IITYPE
   CHARACTER*1 :: IUPLO, IJOBZ   
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IA1 = N ; IA2 = N; IUPLO = UPLO; IW = N; IJOBZ = JOBZ
   IB1 = N; IB2 = N; IITYPE = ITYPE
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
     CASE (1)
      IA1 = IA1 - 1
     CASE (2)
       IB1 = IA1 - 1
     CASE (3)
       IW = IW - 1
     CASE (4)
       IITYPE = 22   
     CASE (5)
       IJOBZ = 'T'
     CASE (6)  
       IUPLO = 'T' 
     CASE(:-1,7:)
       CALL UESTOP(SRNAMT)
   END SELECT
     CALL LA_HEGV( A(1:IA1, 1:IA2), B(1:IB1, 1:IB2), W(1 :IW), &
&      IITYPE, IJOBZ, IUPLO,INFO )

   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
 END SUBROUTINE LA_TEST_ZHEGV
      
      
