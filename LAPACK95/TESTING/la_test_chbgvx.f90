SUBROUTINE LA_TEST_CHBGVX(JOBZ, RANGE, UPLO, N, KAB, KBB, AB, LDAB, BB,&
     &  LDBB, Q, LDQ, VL, VU, IL, IU, ABSTOL, M, W, Z, &
     &  LDZ, WORK, RWORK, IWORK, IFAIL, INFO )
!
!  -- LAPACK95 interface driver routine (version 0.0) --
!     UNI-C, Denmark
!     August 20, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_HBGVX
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      CHARACTER(LEN=1), INTENT(INOUT) :: JOBZ, RANGE, UPLO
      INTEGER, INTENT(IN) :: N, IL, IU, KAB, KBB, LDAB, LDBB, &
     &  LDQ, LDZ
      INTEGER, INTENT(OUT) :: M
      INTEGER, INTENT(INOUT) :: INFO
      REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU 
!  .. Array Arguments ..
      INTEGER, INTENT(OUT) ::  IWORK(1:5*N)
      COMPLEX(WP), INTENT(INOUT) :: AB(1:LDAB,1:N), BB(1:LDBB,1:N)
      COMPLEX(WP), INTENT(OUT) :: WORK(1:N), Q(1:LDQ,1:N), &
     &  Z(1:LDZ,1:N)
      REAL(WP), INTENT(OUT) :: W(1:N)
      REAL(WP), INTENT(OUT) :: RWORK(1:7*N)
      INTEGER, INTENT(OUT) :: IFAIL(1:N)
      LOGICAL LSAME
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_HBGVX'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_CHBGVX'
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      CHARACTER(LEN=1) :: IJOBZ, IRANGE, IUPLO
      INTEGER :: I, J, IAB1, IAB2, IBB1, IBB2, IQ1, IQ2, IW, IZ1, IZ2
      INTEGER :: IIL, IIU, IIFAIL
      REAL(WP) :: IVL, IVU, IABSTOL
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
      IAB1 = KAB+1; IAB2 = N; IBB1 = KBB+1; IBB2 = N; IQ1 = N; IQ2 = N
      IW = N; IZ1 = N; IZ2 = N; IIL = IL; IIU = IU; IIFAIL = N
      IJOBZ = JOBZ; IRANGE = RANGE; IUPLO = UPLO
      IVL = VL; IVU = VU; IABSTOL = ABSTOL
      I =  INFO / 100; J = INFO - I*100
      IF (IRANGE == 'A') THEN; M=N; ELSE; M=IIL - IIL + 1; ENDIF
      SELECT CASE(I)
      CASE(1)
      IAB1 = -1
      CASE (2)
      IBB1 = -1
      CASE (3)
      IW = IW - 1
      CASE (4)
      IUPLO = 'T'
      CASE (5)
      IZ1 = IZ1 - 1
      CASE (6)   
      IVU = IVL -1
      CASE (7)
      CALL LA_HBGVX( AB(1:IAB1,1:IAB2), BB(1:IBB1,1:IBB2), W=W, &
     &  UPLO=IUPLO, Z=Z(1:IZ1,1:IZ2), VL=IVL, VU=IVU, IL=IIL, &
     &  IU=IIU, M=M, IFAIL=IFAIL(1:IIFAIL),&
     &  Q=Q(1:IQ1,1:IQ2), ABSTOL=IABSTOL, INFO=INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )  
      RETURN
      CASE (8)
      IIU = IIL - 1
      CASE (9)
      IIU = N+1
      CASE (10)
      IIFAIL = IAB1 - 1
      CASE(11)
      IQ2 = IQ2 - 1
      CASE (:-1, 12:)
      CALL UESTOP(SRNAMT)
      END SELECT
      IF (LSAME(IJOBZ, 'V')) THEN
        IF (LSAME(IRANGE,'A')) THEN
          CALL LA_HBGVX( AB(1:IAB1,1:IAB2), BB(1:IBB1,1:IBB2), W, IUPLO,&
     &      Z=Z(1:IZ1,1:IZ2), M=M, IFAIL=IFAIL(1:IIFAIL),               &
     &      Q=Q(1:IQ1,1:IQ2), ABSTOL=IABSTOL, INFO=INFO )
        ELSE IF (LSAME(IRANGE,'I')) THEN
          CALL LA_HBGVX( AB(1:IAB1,1:IAB2), BB(1:IBB1,1:IBB2), W, IUPLO, &
     &      Z=Z(1:IZ1,1:IZ2), IL=IIL, IU=IIU, M=M, IFAIL=IFAIL(1:IIFAIL),& 
     &      Q=Q(1:IQ1,1:IQ2), ABSTOL=IABSTOL, INFO=INFO ) 
        ELSE
          CALL LA_HBGVX( AB(1:IAB1,1:IAB2), BB(1:IBB1,1:IBB2), W, IUPLO, &
     &      Z=Z(1:IZ1,1:IZ2), VL=IVL, VU=IVU, M=M, IFAIL=IFAIL(1:IIFAIL),&
     &      Q=Q(1:IQ1,1:IQ2), ABSTOL=IABSTOL, INFO=INFO )
        ENDIF
      ELSE
        IF (LSAME(IRANGE,'A')) THEN 
          CALL LA_HBGVX( AB(1:IAB1,1:IAB2), BB(1:IBB1,1:IBB2), W, IUPLO, &
     &      M=M, ABSTOL=IABSTOL, INFO=INFO )
        ELSE IF (LSAME(IRANGE,'I')) THEN
          CALL LA_HBGVX( AB(1:IAB1,1:IAB2), BB(1:IBB1,1:IBB2), W, IUPLO, &
     &      IL=IIL, IU=IIU, M=M, ABSTOL=IABSTOL, INFO=INFO )  
        ELSE
          CALL LA_HBGVX( AB(1:IAB1,1:IAB2), BB(1:IBB1,1:IBB2), W, IUPLO,&
     &      VL=IVL, VU=IVU,  M=M, ABSTOL=IABSTOL, INFO=INFO )
        END IF
      ENDIF
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_CHBGVX
