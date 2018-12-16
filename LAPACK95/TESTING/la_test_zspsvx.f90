SUBROUTINE LA_TEST_ZSPSVX( FACT, UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX,&
     &  RCOND, FERR, BERR, WORK, RWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     April 7, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => DP
      USE F95_LAPACK, ONLY: LA_SPSVX
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: N, NRHS, LDB, LDX
      INTEGER, INTENT(INOUT) :: INFO
      CHARACTER*1, INTENT(IN) :: FACT, UPLO
      REAL(WP), INTENT(OUT) :: RCOND
!  .. Array Arguments ..
      INTEGER, INTENT(INOUT) :: IPIV(1:N)
      COMPLEX(WP), INTENT(INOUT) :: AP(1:N*(N+1)/2), B(1:LDB,1:NRHS)
      COMPLEX(WP), INTENT(INOUT) :: AFP(1: N*(N+1)/2)
      COMPLEX(WP), INTENT(OUT) :: X(1:LDX,1: NRHS), WORK(1:2*N)
      REAL(WP), INTENT(OUT) :: FERR(1:NRHS), BERR(1:NRHS)
      REAL(WP), INTENT(OUT) :: RWORK (1: N)
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_SPSVX'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZSPSVX'
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IAP, IB1, IB2, IIPIV, IX1, IX2, IAFP, IFERR, &
     &  IBERR
      CHARACTER*1 :: IUPLO, IFACT   
!  .. Local Arrays ..
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
      IAP = N*(N+1)/2; IB1 = N; IB2 = NRHS; IUPLO= UPLO; IIPIV=N
      IX1 = N; IX2 = NRHS; IAFP = N*(N+1)/2; IFACT=FACT; IFERR=NRHS
      IBERR = NRHS
      I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
      CASE(0)
      IF ( NRHS == 1 ) THEN
        CALL LA_SPSVX( AP(1:IAP), B(1:IB1,1), X(1:IX1,1), IUPLO, &
     &    AFP(1: IAFP), IPIV(1:IIPIV), IFACT, FERR(1), &
     &    BERR(1), RCOND, INFO )
        INFO = INFOTC
      ELSE
        CALL LA_SPSVX( AP(1:IAP), B(1:IB1,1:IB2), X(1:IX1,1:IX2), IUPLO, &
     &    AFP(1: IAFP), IPIV(1:IIPIV), IFACT, FERR(1:IFERR), &
     &    BERR(1: IBERR), RCOND, INFO )
        INFO = INFOTC
      END IF
      CASE (1)
      IAP = IAP - 1
      CASE (2)
      IB1 = IB1 - 1
      CASE (3)
      IX1 = IB1 - 1
      CASE (4)
      IUPLO = 'T'
      CASE (5)
      IAFP = IAP - 1
      CASE (6)
      IIPIV = IB1 - 1
      CASE (7)
      IFACT = 'T'
      CASE (8)
      IFERR = 7
      CALL LA_SPSVX( AP(1:IAP), B(1:IB1,1:IB2), X(1:IX1,1:IX2), IUPLO, &
     &  AFP(1: IAFP), IPIV(1:IIPIV), IFACT, FERR(1:IFERR), &
     &  BERR(1: IBERR), RCOND, INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE (9)
      IBERR = 7
      CALL LA_SPSVX( AP(1:IAP), B(1:IB1,1:IB2), X(1:IX1,1:IX2), IUPLO, &
     &  AFP(1: IAFP), IPIV(1:IIPIV), IFACT, FERR(1:IFERR), &
     &  BERR(1: IBERR), RCOND, INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN  
      CASE(:-1,10:)
      CALL UESTOP(SRNAMT)
      END SELECT
      IF( I /= 0 ) THEN
        SELECT CASE (NRHS)
        CASE (2:)
        CALL LA_SPSVX( AP(1:IAP), B(1:IB1,1:IB2), X(1:IX1,1:IX2), IUPLO, &
     &    AFP(1: IAFP), IPIV(1:IIPIV), IFACT, FERR(1:IFERR), &
     &    BERR(1: IBERR), RCOND, INFO )
        CASE(1)
        CALL LA_SPSVX( AP(1:IAP), B(1:IB1,1), X(1:IX1, 1), IUPLO, &
     &    AFP(1: IAFP), IPIV(1:IIPIV), IFACT, FERR(1), &
     &    BERR(1), RCOND, INFO ) 
        CASE(:-1)
        CALL UESTOP(SRNAMT)
      END SELECT
      END IF
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      END SUBROUTINE LA_TEST_ZSPSVX

