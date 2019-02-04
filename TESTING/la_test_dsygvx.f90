SUBROUTINE LA_TEST_DSYGVX(ITYPE, JOBZ, RANGE, UPLO, N, A, LDA, B,&
     &  LDB, VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK, &
     &  IFAIL, INFO )
!
!  -- LAPACK95 interface driver routine (version 0.0) --
!     UNI-C, Denmark
!     August 20, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => DP
!      USE LA_AUXMOD, ONLY: ERINFO
      USE F95_LAPACK, ONLY: LA_SYGVX
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      CHARACTER(LEN=1), INTENT(INOUT) :: JOBZ, RANGE, UPLO
      INTEGER, INTENT(IN) :: N, IL, IU, LDZ, LDA, LDB, LWORK
      INTEGER, INTENT(OUT) :: M
      INTEGER, INTENT(IN) :: ITYPE
      INTEGER, INTENT(INOUT) :: INFO
      REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU 
!  .. Array Arguments ..
      INTEGER, INTENT(OUT) ::  IWORK(1:5*N)
      REAL(WP), INTENT(INOUT) :: A(1:LDA, 1:N), B(1:LDB, 1:N)
      REAL(WP), INTENT(OUT) :: WORK(1:LWORK), W(1:N), &
&       Z(1:LDZ,1:MAX(1,N))
      INTEGER, INTENT(OUT) :: IFAIL(1:N)
      LOGICAL LSAME
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_SYGVX'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_DSYGVX'
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      CHARACTER(LEN=1) :: IJOBZ, IRANGE, IUPLO
      INTEGER :: I, J, IA1, IA2, IB1, IB2, IW, IZ1, IZ2, IITYPE
      INTEGER :: IIL, IIU, IIFAIL
      REAL(WP) :: IVL, IVU, IABSTOL
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
      IA1 =  N; IA2 = N; IB1 = LDB; IB2=N; IITYPE= ITYPE
      IW = N;  IZ1 = N; IZ2 = N; IIL = IL; IIU = IU; IIFAIL = N
      IJOBZ = JOBZ; IRANGE = RANGE; IUPLO = UPLO
      IVL = VL; IVU = VU; IABSTOL = ABSTOL
      I =  INFO / 100; J = INFO - I*100
      SELECT CASE(I)
      CASE(1)
        IA2 = IA2 - 1
      CASE (2)
        IB2 = IB2 - 1 
      CASE (3)
        IW = IW - 1
      CASE (4)
        IITYPE = 5
      CASE (5)
        IJOBZ = 'T'
        CALL LA_SYGVX(A(1:IA1, 1:IA2), B(1:IB1, 1:IB2), W, &
&         IITYPE, IJOBZ, IUPLO, IVL, IVU, IIL, &
&         IIU, M, IFAIL(1:IIFAIL), IABSTOL, INFO )
        CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
        RETURN
      CASE (6)
        IUPLO = 'T'
      CASE (7)   
        IVU = IVL -1
      CASE (8)
        CALL LA_SYGVX(A(1:IA1, 1:IA2), B(1:IB1, 1:IB2), W, &
&         IITYPE, IJOBZ, IUPLO, IVL, IVU, IIL, &
&         IIU, M, IFAIL(1:IIFAIL), IABSTOL, INFO )
        CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )  
        RETURN
      CASE (9)
        IIU = IIL - 1
      CASE (10)
        IIU = N+1
      CASE (12)
        IIFAIL = IA2 - 1
      CASE (:-1, 11, 13:)
        CALL UESTOP(SRNAMT)
    END SELECT
    IF (LSAME(IJOBZ, 'V')) THEN
      IF (LSAME(IRANGE,'A')) THEN
        CALL LA_SYGVX( A(1:IA1, 1:IA2), B(1:IB1,1:IB2), W,  &
&         IITYPE, IJOBZ, IUPLO, M=M, IFAIL=IFAIL(1:IIFAIL), &
&         ABSTOL=IABSTOL, INFO=INFO )
      ELSE IF (LSAME(IRANGE,'I')) THEN
        CALL LA_SYGVX(A(1:IA1, 1:IA2), B(1:IB1, 1:IB2), W,  &
&         IITYPE, IJOBZ, IUPLO, IL=IIL, IU=IIU, M=M, &
&         IFAIL=IFAIL(1:IIFAIL), ABSTOL=IABSTOL, INFO=INFO ) 
      ELSE
        CALL LA_SYGVX( A(1:IA1, 1:IA2), B(1:IB1, 1:IB2), W, &
&         IITYPE, IJOBZ, IUPLO, IVL, IVU, M=M, &
&         IFAIL=IFAIL(1:IIFAIL), ABSTOL=IABSTOL, INFO=INFO )
      ENDIF
    ELSE
      IF (LSAME(IRANGE,'A')) THEN
        CALL LA_SYGVX( A(1:IA1, 1:IA2), B(1:IB1, 1:IB2), W, IITYPE, &
&         UPLO=IUPLO, M=M, ABSTOL=IABSTOL, INFO=INFO )
      ELSE IF (LSAME(IRANGE,'I')) THEN
        CALL LA_SYGVX( A(1:IA1, 1:IA2), B(1:IB1, 1:IB2), W, IITYPE, &
&         UPLO=IUPLO, IL=IIL, IU=IIU, M=M, ABSTOL=IABSTOL, INFO=INFO )  
      ELSE
        CALL LA_SYGVX( A(1:IA1, 1:IA2), B(1:IB1, 1:IB2), W, IITYPE, &
&         UPLO=IUPLO, VL=IVL, VU=IVU,  M=M, ABSTOL=IABSTOL,  INFO=INFO )
      END IF
    ENDIF
    IF (N/=0) Z(1:IA1, 1:M) = A(1:IA1, 1:M)
    CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_DSYGVX
