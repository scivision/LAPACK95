SUBROUTINE LA_TEST_ZHBEV( JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK, RWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark; 
!     May 26, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_HBEV
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, KD, LDZ, LDAB
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: JOBZ, UPLO
!  .. Array Arguments ..
   COMPLEX(WP), INTENT(INOUT) :: AB(1: LDAB, 1: N)
   COMPLEX(WP), INTENT(OUT) :: WORK(1: N), Z(1:LDZ, 1:N)
   REAL(WP), INTENT(OUT) ::  W(1:N)
   REAL(WP) :: RWORK(1: MAX(1, 3*N-2))
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_HBEV '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZHBEV '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IAB1, IAB2, IW, IZ1, IZ2
   CHARACTER*1 :: IUPLO, IJOBZ
!  .. Local Arrays ..
   INTEGER, SAVE, POINTER :: IWORK(:)
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
   LOGICAL LSAME
!  .. Executable Statements ..
   IAB1 = KD+1; IAB2=N; IUPLO = UPLO; IJOBZ = JOBZ; IW = N
   IZ1 = N; IZ2 = N
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
   CASE (1)
     IAB1 = -1
   CASE (2)
     IW = IW - 1
   CASE (3)
     IUPLO = 'T'
   CASE(4)
     IJOBZ = 'V'
     IZ2 = IZ2 - 1
   CASE(:-1,5:)
     CALL UESTOP(SRNAMT)
 END SELECT
 IF ( LSAME (IJOBZ,'V')) THEN
   CALL LA_HBEV( AB(1: IAB1, 1: IAB2), W(1:IW), IUPLO, &
&    Z(1:IZ1, 1: IZ2), INFO)
 ELSE
   CALL LA_HBEV( AB(1: IAB1, 1: IAB2), W(1:IW), IUPLO, INFO = INFO)
 ENDIF
 
 CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_ZHBEV
