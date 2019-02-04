SUBROUTINE LA_TEST_CHPGVX(ITYPE, JOBZ, RANGE, UPLO, N, AP, BP,&
     &  VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, &
     &  INFO )
!
!  -- LAPACK95 interface driver routine (version 0.0) --
!     UNI-C, Denmark
!     August 20, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => SP
!      USE LA_AUXMOD, ONLY: ERINFO
      USE F95_LAPACK, ONLY: LA_HPGVX
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      CHARACTER(LEN=1), INTENT(IN) :: JOBZ, RANGE, UPLO
      INTEGER, INTENT(IN) :: N, IL, IU, LDZ
      INTEGER, INTENT(OUT) :: M
      INTEGER, INTENT(IN) :: ITYPE
      INTEGER, INTENT(INOUT) :: INFO
      REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU 
!  .. Array Arguments ..
      INTEGER, INTENT(INOUT) ::  IWORK(1:5*N)
      COMPLEX(WP), INTENT(INOUT) :: WORK(1:8*N), AP(1:N*(N+1)/2), BP(1:N*(N+1)/2)
      REAL(WP), INTENT(OUT) :: W(1:N)
      COMPLEX(WP), INTENT(OUT) :: Z(1:LDZ,1:MAX(1,N))
      INTEGER, INTENT(OUT) :: IFAIL(1:N)
      REAL(WP), INTENT(OUT) :: RWORK(1: 7*N)
      LOGICAL LSAME
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_HPGVX'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_CHPGVX'
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      CHARACTER(LEN=1) :: IJOBZ, IRANGE, IUPLO
      INTEGER :: I, J, IAP, IBP, IW, IZ1, IZ2, IITYPE
      INTEGER :: IIL, IIU, IIFAIL
      REAL(WP) :: IVL, IVU, IABSTOL
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
      IAP =  N *(N+1)/2; IBP =  N *(N+1)/2; IITYPE= ITYPE
      IW = N;  IZ1 = N; IZ2 = N; IIL = IL; IIU = IU; IIFAIL = N
      IJOBZ = JOBZ; IRANGE = RANGE; IUPLO = UPLO
      IVL = VL; IVU = VU; IABSTOL = ABSTOL
      I =  INFO / 100; J = INFO - I*100
      SELECT CASE(I)
      CASE(1)
      IAP = IAP - 1
      CASE (2)
      IBP = IAP - 1 
      CASE (3)
      IW = IW - 1
      CASE (4)
        IITYPE = 4
      CASE (5)
        IUPLO = 'T'
      CASE (6)
      IZ1 = IZ1 - 1
      CASE (7)   
      IVU = IVL -1
      CASE (8)
      CALL LA_HPGVX( AP(1:IAP), BP(1:IBP), W=W, ITYPE=IITYPE, &
     &  UPLO=IUPLO, Z=Z(1:IZ1,1:IZ2), VL=IVL, VU=IVU, IL=IIL, &
     &  IU=IIU, M=M, IFAIL=IFAIL(1:IIFAIL),&
     &  ABSTOL=IABSTOL, INFO=INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )  
      RETURN
      CASE(9)
      IRANGE = 'I'
      IIU = IIL - 1
      CASE (10)
      IIU = N+1
      CASE (12)
      IIFAIL = IAP - 1
      CASE (:-1, 11, 13:)
      CALL UESTOP(SRNAMT)
      END SELECT
      IF (LSAME(IJOBZ, 'V')) THEN
        IF (LSAME(IRANGE,'A')) THEN
          CALL LA_HPGVX( AP(1:IAP), BP(1:IBP), W, IITYPE,  IUPLO,&
     &      Z=Z(1:IZ1,1:IZ2), M=M, IFAIL=IFAIL(1:IIFAIL),               &
     &      ABSTOL=IABSTOL, INFO=INFO )
        ELSE IF (LSAME(IRANGE,'I')) THEN
          CALL LA_HPGVX( AP(1:IAP), BP(1:IBP), W, IITYPE,  IUPLO, &
     &      Z=Z(1:IZ1,1:IZ2), IL=IIL, IU=IIU, M=M, IFAIL=IFAIL(1:IIFAIL),& 
     &      ABSTOL=IABSTOL, INFO=INFO ) 
        ELSE
          CALL LA_HPGVX( AP(1:IAP), BP(1:IBP), W, IITYPE, IUPLO, &
     &      Z=Z(1:IZ1,1:IZ2), VL=IVL, VU=IVU, M=M, IFAIL=IFAIL(1:IIFAIL),&
     &      ABSTOL=IABSTOL, INFO=INFO )
        ENDIF
      ELSE
        IF (LSAME(IRANGE,'A')) THEN 
          CALL LA_HPGVX(  AP(1:IAP), BP(1:IBP), W, IITYPE,  IUPLO, &
     &      M=M, ABSTOL=IABSTOL, INFO=INFO )
        ELSE IF (LSAME(IRANGE,'I')) THEN
          CALL LA_HPGVX(  AP(1:IAP), BP(1:IBP), W, IITYPE, IUPLO, &
     &      IL=IIL, IU=IIU, M=M, ABSTOL=IABSTOL, INFO=INFO )  
         ELSE
           CALL LA_HPGVX(  AP(1:IAP), BP(1:IBP), W, IITYPE, IUPLO,&
     &       VL=IVL, VU=IVU,  M=M, ABSTOL=IABSTOL,  INFO=INFO )
         END IF
       ENDIF
       CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_CHPGVX
       
