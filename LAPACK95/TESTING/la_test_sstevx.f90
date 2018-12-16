SUBROUTINE LA_TEST_SSTEVX( JOBZ, RANGE, N, D, E, VL, VU, IL, IU, &
     &  ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     November 2, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_STEVX
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: N, LDZ, IL, IU
      INTEGER, INTENT(OUT) ::  M
      INTEGER, INTENT(INOUT) :: INFO
      REAL(WP), INTENT(IN) :: VL, VU, ABSTOL
      CHARACTER*1, INTENT(IN) :: JOBZ, RANGE
!  .. Array Arguments ..
      REAL(WP), INTENT(INOUT) :: D(1:N), E(1:N)
      REAL(WP), INTENT(OUT)::  W(1:N), Z(1:LDZ, 1:N), WORK(1: 5*N)
      INTEGER, INTENT(OUT):: IWORK(1: 5*N), IFAIL(1:N)
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_STEVX'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_SSTEVX'
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IE, IZ1, IZ2, ID, IW, IIL, IIU, IIFAIL
      REAL(WP) :: IVL, IVU, IABSTOL
      CHARACTER*1 :: IJOBZ, IRANGE
!  .. Local Arrays ..
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
      LOGICAL LSAME
!  .. Executable Statements ..
      IJOBZ = JOBZ; ID = N; IE = N; IZ1 = MAX (1,N); IZ2 = N
      IW=N; IVL=VL; IVU=VU; IIL=IL; IIU=IU; IIFAIL=N; M=0
      IABSTOL=ABSTOL; IRANGE=RANGE
      I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
      CASE (2)
        IE = IE - 1
      CASE (3)
        IW=ID-1
      CASE (4)
        IZ1 = IZ1 - 1; IJOBZ='V'; IRANGE='V'
      CASE (5)
        IVL = IVU+1; IJOBZ='V'; IRANGE='V'
      CASE (6)
        IJOBZ='V'; IRANGE='V'
        CALL LA_STEVX( D(1:ID), E(1:IE), W(1:IW), Z(1:IZ1, 1:IZ2), &
&         IVL, IVU, IIL, IIU, M, IFAIL(1:IIFAIL), IABSTOL, INFO)
        CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
        RETURN
      CASE (7)
        IIL = IIU+1; IJOBZ='V'; IRANGE='I'
      CASE (8)
        IIU = ID+1;  IJOBZ='V'; IRANGE='I'; IIL = IIU 
      CASE (10)
        IJOBZ='V'; IRANGE='V'
        CALL LA_STEVX( D(1:ID), E(1:IE), W(1:IW), VL=IVL, VU=IVU, &
&         M=M, IFAIL=IFAIL(1:IIFAIL), ABSTOL=IABSTOL, INFO=INFO)
        CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
        RETURN   
      CASE(:-1, 1, 9, 11:)
        CALL UESTOP(SRNAMT)
    END SELECT

      IF (LSAME(IJOBZ,'V')) THEN
        IF (LSAME (IRANGE, 'V')) THEN
          CALL LA_STEVX( D(1:ID), E(1:IE), W(1:IW), Z(1:IZ1, 1:IZ2), &
     &      IVL, IVU, M=M, IFAIL=IFAIL(1:IIFAIL), ABSTOL=IABSTOL, INFO=INFO)
        ELSE IF (LSAME (IRANGE, 'I')) THEN
          CALL LA_STEVX( D(1:ID), E(1:IE), W(1:IW), Z(1:IZ1, 1:IZ2), &
     &      IL=IIL, IU=IIU, M=M, IFAIL=IFAIL(1:IIFAIL), ABSTOL=IABSTOL,&
     &      INFO=INFO)
        ELSE
          CALL LA_STEVX( D(1:ID), E(1:IE), W(1:IW), Z(1:IZ1, 1:IZ2), &
     &      M=M, IFAIL=IFAIL(1:IIFAIL), ABSTOL=IABSTOL, INFO=INFO)
        ENDIF
      ELSE
        IF (LSAME (IRANGE, 'V')) THEN
          CALL LA_STEVX( D(1:ID), E(1:IE), W(1 :IW), VL=IVL, VU=IVU, &
     &      M=M, ABSTOL=IABSTOL, INFO=INFO)
        ELSE IF (LSAME (IRANGE, 'I')) THEN
          CALL LA_STEVX( D(1:ID), E(1:IE), W(1 :IW), IL=IIL, IU=IIU, &
     &      M=M, ABSTOL=IABSTOL, INFO=INFO)
        ELSE
          CALL LA_STEVX( D(1:ID), E(1:IE), W(1 :IW),M=M,ABSTOL=IABSTOL,&
     &      INFO=INFO)
        ENDIF
      END IF
      
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
 END SUBROUTINE LA_TEST_SSTEVX
