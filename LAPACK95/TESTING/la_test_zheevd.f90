SUBROUTINE LA_TEST_ZHEEVD( JOBZ, UPLO, N, A, LDA, W,  WORK, LWORK, RWORK, LRWORK, IWORK, LIWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark; 
!     May 29, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_HEEVD
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, LDA, LWORK, LRWORK, LIWORK
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: JOBZ, UPLO
!  .. Array Arguments ..
   COMPLEX(WP), INTENT(INOUT) :: A(1:LDA,1:N)
   COMPLEX(WP), INTENT(OUT) :: WORK(1: LWORK)
   REAL(WP), INTENT(OUT) ::  W(1:N)
   REAL(WP) :: RWORK(1: LRWORK)
   INTEGER, INTENT(OUT) :: IWORK(1: LIWORK)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_HEEVD'
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZHEEVD'
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IA1, IA2, IW
   CHARACTER*1 :: IUPLO, IJOBZ
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IA1 = N; IA2 = N; IUPLO = UPLO; IJOBZ = JOBZ; IW = N; IWORK = LWORK
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
   CASE (1)
      IA2 = IA1-1
   CASE (2)
     IW = IW -1
   CASE (3)
     IJOBZ = 'T'
   CASE(4)
      IUPLO = 'T'   
   CASE(:-1,5:)
      CALL UESTOP(SRNAMT)
   END SELECT
        CALL LA_HEEVD( A(1:IA1, 1:IA2), W(1:IW), IJOBZ, IUPLO, INFO)

   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_ZHEEVD
