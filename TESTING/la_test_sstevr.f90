SUBROUTINE LA_TEST_SSTEVR( JOBZ, RANGE, N, D, E, VL, VU, IL, &
     &  IU, ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     September 11, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_STEVR
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: N, LDZ, IL, IU, LWORK, LIWORK
      REAL(WP), INTENT(IN) ::  ABSTOL, VL, VU
      INTEGER, INTENT(INOUT) :: INFO
      CHARACTER*1, INTENT(IN) :: JOBZ, RANGE
!  .. Array Arguments ..
      REAL(WP), INTENT(INOUT) :: D(1:N), E(1: N)
      REAL(WP), INTENT(OUT)::  W(1:N), WORK(1:LWORK), Z(1:LDZ,1:MAX(1,N))
      INTEGER, INTENT(OUT) :: IWORK(LIWORK)
      INTEGER, INTENT(OUT) :: ISUPPZ(1: 2*MAX(1,N)), M
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_STEVR'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_SSTEVR'
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, ID, IE, IISUPPZ, IW, IIL, IIU, IZ1, IZ2
      REAL(WP) :: IVL, IVU
      CHARACTER*1 :: IRANGE
!  .. Local Arrays ..
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
      LOGICAL LSAME
      EXTERNAL LSAME
!  .. Executable Statements ..
      ID = N; IE = N; IW = N; IISUPPZ = 2* MAX(1,N)
      IRANGE = RANGE
      IVL = VL; IVU = VU; M=0
      IIL = IL; IIU = IU; IZ1 = MAX(1,N); IZ2 = MAX(1,N)
      I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
      CASE (2)
      IE = IE - 1
      CASE (3)
      IW = IW - 1
      CASE (4)
      IZ2 = IZ2 - 1
      CALL LA_STEVR( D(1: ID), E(1:IE), W(1 :IW), Z(1: IZ1, 1:IZ2), &
     &   M=M, ISUPPZ=ISUPPZ(1: IISUPPZ), ABSTOL=ABSTOL, INFO=INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE (5)
      IISUPPZ = IISUPPZ - 1
      CALL LA_STEVR( D(1: ID), E(1:IE), W(1 :IW), Z(1: IZ1, 1:IZ2), &
&       M=M, ISUPPZ=ISUPPZ(1: IISUPPZ), ABSTOL=ABSTOL, INFO=INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN     
      CASE (6)
      IRANGE = 'V'
      IVL = IVU+1.0
      CALL LA_STEVR( D(1: ID), E(1:IE), W(1 :IW),&
     &  VL=IVL, VU=IVU, M=M, INFO=INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE (7)
      CALL LA_STEVR( D(1: ID), E(1:IE), W(1 :IW), Z(1: IZ1, 1:IZ2), &
     &  IVL, IVU, IIL, IIU, M, ISUPPZ(1: IISUPPZ), ABSTOL, INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE (8)
      IRANGE = 'I'
      IIU = IW + 1
      CALL LA_STEVR( D(1: ID), E(1:IE), W(1 :IW), Z(1: IZ1, 1:IZ2), &
     &  IL=IIL, IU=IIU, M=M, ISUPPZ=ISUPPZ(1: IISUPPZ), ABSTOL=ABSTOL, INFO=INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN 
      CASE(:-1,1,9:)
      CALL UESTOP(SRNAMT)
      END SELECT
      
      IF (LSAME(IRANGE, 'A')) THEN
        IF (LSAME(JOBZ,'V')) THEN
          CALL LA_STEVR( D(1: ID), E(1:IE), W(1 :IW), Z(1: IZ1, 1:IZ2), &
     &        M=M, ISUPPZ=ISUPPZ(1: IISUPPZ), ABSTOL=ABSTOL, INFO=INFO )
        ELSE
          CALL LA_STEVR( D(1: ID), E(1:IE), W(1 :IW), &
     &      M=M, ABSTOL=ABSTOL, INFO=INFO )
        END IF
      ELSE IF (LSAME(IRANGE, 'V')) THEN
        IF (LSAME(JOBZ,'V')) THEN
          CALL LA_STEVR( D(1: ID), E(1:IE), W(1 :IW), Z(1: IZ1, 1:IZ2), &
     &       VL=IVL, VU=IVU, M=M, ISUPPZ=ISUPPZ(1: IISUPPZ), ABSTOL=ABSTOL, INFO=INFO )
        ELSE
          CALL LA_STEVR( D(1: ID), E(1:IE), W(1 :IW), &
     &      VL=IVL, VU=IVU, M=M, ABSTOL=ABSTOL, INFO=INFO )
        END IF
      ELSE
        IF (LSAME(JOBZ, 'V')) THEN
          CALL LA_STEVR( D(1: ID), E(1:IE), W(1 :IW), Z(1: IZ1, 1:IZ2), &
     &       IL=IIL, IU=IIU, M=M, ISUPPZ=ISUPPZ(1: IISUPPZ), ABSTOL=ABSTOL, INFO=INFO )
        ELSE
          CALL LA_STEVR( D(1: ID), E(1:IE), W(1 :IW), &
     &      IL=IIL, IU=IIU, M=M, ABSTOL=ABSTOL, INFO=INFO )
        END IF
      END IF
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      END SUBROUTINE LA_TEST_SSTEVR
