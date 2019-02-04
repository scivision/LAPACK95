SUBROUTINE LA_TEST_ZHPGV( ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK, RWORK, INFO)
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     May 21, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_HPGV
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, ITYPE, LDZ
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: JOBZ, UPLO  
!  .. Array Arguments ..
   COMPLEX(WP), INTENT(INOUT) :: AP(1:N*(N+1)/2), BP(1: N*(N+1)/2)
   COMPLEX(WP), INTENT(OUT) ::Z(1: LDZ, 1: N)
   COMPLEX(WP) :: WORK(1: MAX(1, 2*N-1))
   REAL(WP), INTENT(OUT) :: W(1:N)
   REAL(WP) :: RWORK(1: MAX(1, 3*N-2))
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_HPGV '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZHPGV '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IAP, IBP, IZ1, IZ2, IW, IITYPE
   CHARACTER*1 :: IUPLO, IJOBZ   
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
   LOGICAL LSAME
!  .. Executable Statements ..
   IAP= N*(N+1)/2; IBP = N*(N+1)/2;  IUPLO= UPLO; IJOBZ = JOBZ
   IZ1 = MAX(1,N); IZ2 = N; IW= N
   IITYPE = ITYPE; 
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)

   CASE (1)
      IAP = IAP - 1
   CASE (2)
      IBP = IAP - 1
   CASE (3)
     IW = IW -1
   CASE (4)
     IITYPE = 22
   CASE (5)
     IUPLO = 'T'
   CASE (6)
     IZ2 = IZ2 - 1
   CASE(:-1,7:)
      CALL UESTOP(SRNAMT)
  END SELECT 
  IF (LSAME (IJOBZ ,'V')) THEN
    CALL LA_HPGV( AP(1:IAP), BP(1: IBP), W(1:IW), IITYPE, IUPLO, &
&     Z(1: IZ1, 1: IZ2), INFO)
  ELSE
    CALL LA_HPGV( AP(1:IAP), BP(1: IBP), W(1:IW), IITYPE, IUPLO, &
&     INFO = INFO)
  END IF
  
  CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_ZHPGV

