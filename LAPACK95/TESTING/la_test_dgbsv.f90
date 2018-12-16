SUBROUTINE LA_TEST_DGBSV( N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark,
!     March 25, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => DP
      USE F95_LAPACK, ONLY: LA_GBSV
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: N, NRHS, LDAB, LDB, KL, KU
      INTEGER, INTENT(INOUT) :: INFO
!  .. Array Arguments ..
      INTEGER, INTENT(OUT) :: IPIV(1:N)
      REAL(WP), INTENT(INOUT) :: AB(1:LDAB,1:N), B(1:LDB,1:N)
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GBSV '
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_DGBSV '
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IAB1, IAB2, IB1, IB2, IIPIV, ISTAT, K
!  .. Local Arrays ..
      INTEGER, SAVE, POINTER :: IWORK(:)
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
      IAB1 = LDAB; IAB2 = N; IB1 = N; IB2 = NRHS; IIPIV = N; K = KL
      I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
      CASE(0)
      IF (NRHS == 1) THEN
        CALL LA_GBSV( AB(1:IAB1,1:IAB2), B(1:IB1,1), KL, &
     &    IPIV(1:IIPIV) )
        INFO = INFOTC 
      ELSE
        CALL LA_GBSV( AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), KL, &
     &    IPIV(1:IIPIV) )
        INFO = INFOTC
      END IF
      CASE (1)
        IAB1 = 2*K
      CASE (2)
        IB1 = IAB1-1
      CASE(3)
        K= -1
      CASE(4)
        IIPIV = IAB1-1
      CASE(:-1,5:)
        CALL UESTOP(SRNAMT)
      END SELECT
      IF( I /= 0) THEN
        SELECT CASE (NRHS)
        CASE (2:)
        CALL LA_GBSV( AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), K, &
     &            IPIV(1:IIPIV), INFO )
        CASE(1)
        CALL LA_GBSV( AB(1:IAB1,1:IAB2), B(1:IB1,1), K, &
     &            IPIV(1:IIPIV),INFO )
        CASE(:-1)
          CALL UESTOP(SRNAMT)
      END SELECT
      END IF
      IF( I == 0 .AND. J == 1 )THEN
        ALLOCATE( IWORK(N), STAT=ISTAT )
        IF(ISTAT == 0 )THEN
          IWORK(1:N) = IPIV(1:N)
        ELSE
          WRITE(*,*)'STOP in ', SRNAMT, 'Memory allocation failed.'
        END IF
      ELSE IF( I == 0 .AND. J == 4 )THEN
        DEALLOCATE( IWORK, STAT=ISTAT )
      END IF
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_DGBSV
      
