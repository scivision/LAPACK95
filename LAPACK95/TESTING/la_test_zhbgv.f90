SUBROUTINE LA_TEST_ZHBGV( JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, W, Z, LDZ, WORK, RWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     May 21, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_HBGV
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, KA, KB, LDAB, LDBB, LDZ
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: JOBZ, UPLO
!  .. Array Arguments ..
   COMPLEX(WP), INTENT(INOUT) :: AB(1:LDAB, 1:N), BB( 1:LDBB, 1:N)
   REAL(WP), INTENT(OUT):: W(1:N)
   COMPLEX(WP), INTENT(OUT) :: WORK(1:3*N), Z(1:LDZ, 1:N)
   REAL(WP) :: RWORK(1: 3*N)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_HBGV '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZHBGV '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IAB1, IAB2, IW, IBB1, IBB2, IZ1, IZ2
   CHARACTER*1 :: IUPLO, IJOBZ   
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
   LOGICAL LSAME
!  .. Executable Statements ..
   IAB1 = KA + 1; IAB2 = N; IUPLO = UPLO; IW = N; IJOBZ = JOBZ
   IBB1 = KB + 1; IBB2 = N; IZ1 = N; IZ2 = N 
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
     CASE (1)
      IAB1  = IAB1 - 1
     CASE (2)
       IBB2 = IBB2 - 1
     CASE (3)
       IW = IW - 1
     CASE (4)
       IUPLO = 'T'
     CASE (5)
       IZ2 = IZ2 - 1
       IJOBZ = 'V'
     CASE(:-1,6:)
       CALL UESTOP(SRNAMT)
   END SELECT
   IF (LSAME(IJOBZ, 'V')) THEN
     CALL LA_HBGV( AB(1:IAB1, 1: IAB2), BB(1:IBB1, 1:IBB2), W(1 :IW), &
&       IUPLO, Z(1:IZ1, 1: IZ2), INFO )
   ELSE
     CALL LA_HBGV( AB(1:IAB1, 1: IAB2), BB(1:IBB1, 1:IBB2), W(1 :IW), &
&      IUPLO, INFO = INFO )
   END IF

   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
 END SUBROUTINE LA_TEST_ZHBGV
      
      
