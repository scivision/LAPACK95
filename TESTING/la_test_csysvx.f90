SUBROUTINE LA_TEST_CSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, &
     &  B, LDB, X, LDX, RCOND, FERR, BERR, WORK, LWORK, RWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     April 7, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_SYSVX
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: N, NRHS, LDA, LDAF, LDB, LDX, LWORK
      INTEGER, INTENT(INOUT) :: INFO
      REAL(WP), INTENT(OUT) :: RCOND
      CHARACTER*1, INTENT(IN) :: FACT, UPLO  
!  .. Array Arguments ..
      INTEGER, INTENT(INOUT) :: IPIV(1:N)
      COMPLEX(WP), INTENT(INOUT) :: AF(1:LDAF,1:N)
      COMPLEX(WP), INTENT(OUT) :: X(1:LDX, 1:NRHS)
      REAL(WP), INTENT(OUT) :: FERR(1: NRHS), BERR(1: NRHS)
      COMPLEX(WP), INTENT(IN) :: A(1:LDA,1:N), B(1:LDB,1:NRHS), &
     &  WORK(1:LWORK)
      REAL(WP), INTENT(OUT) :: RWORK (1: N)
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_SYSVX'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_CSYSVX'
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IA1, IA2, IAF1, IAF2, IX1, IX2, IB1, IB2, IIPIV,&
     &  IFERR, IBERR
      CHARACTER*1 :: IUPLO, IFACT   
!  .. Local Arrays ..
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
      IA1 = N; IA2 = N; IB1 = N; IB2 = NRHS; IUPLO= UPLO; IIPIV=N
      IX1 = N; IX2 = NRHS; IAF1 = N; IAF2 = N; IFACT=FACT
      IFERR = NRHS; IBERR = NRHS
      I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
      CASE(0)
      IF ( NRHS == 1 )  THEN
        CALL LA_SYSVX( A(1:IA1,1:IA2), B(1:IB1,1), X(1: IX1, 1), &
     &    UPLO, AF(1: IAF1, 1:IAF2), IPIV(1:IIPIV), IFACT, &
     &    FERR(1), BERR(1), RCOND,  INFO )
        INFO = INFOTC 
      ELSE
        CALL LA_SYSVX( A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1:IX1, 1:IX2), &
     &    UPLO, AF(1 : IAF1, 1:IAF2), IPIV(1:IIPIV), IFACT,&
     &    FERR(1: IFERR), BERR(1 : IBERR), RCOND,  INFO )
        INFO = INFOTC
      END IF
      CASE (1)
      IA2 = IA1 - 1
      CASE (2)
      IB1 = IA1 - 1
      CASE(3)
      IX1 = IA1 - 1; IX2 = IB2 - 1
      CASE (4)
      IUPLO = 'T'
      CASE (5)
      IAF1 = IA1 - 1; IAF2 = IA1 - 1
      CASE (6)
      IIPIV = IA1 - 1
      CASE (7)
      IFACT = 'T'
      CALL LA_SYSVX( A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1: IX1, 1:IX2), &
     &  IUPLO, AF(1: IAF1, 1:IAF2), IPIV(1:IIPIV), IFACT,&
     &  FERR(1: IFERR), BERR(1: IBERR), RCOND,  INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE(8)
      IFERR = IB2 - 1
      CALL LA_SYSVX( A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1: IX1, 1:IX2), &
     &  IUPLO, AF(1: IAF1, 1:IAF2), IPIV(1:IIPIV), IFACT,&
     &  FERR(1: IFERR), BERR(1: IBERR), RCOND,  INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE(9)
      IBERR = IB2 - 1
      CALL LA_SYSVX( A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1: IX1, 1:IX2), &
     &  IUPLO, AF(1: IAF1, 1:IAF2), IPIV(1:IIPIV), IFACT,&
     &  FERR(1: IFERR), BERR(1: IBERR), RCOND,  INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN                               
      CASE(:-1,10:)
      CALL UESTOP(SRNAMT)
      END SELECT
      IF( I /= 0 ) THEN
        SELECT CASE (NRHS)
        CASE (2:)
        CALL LA_SYSVX( A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1: IX1, 1:IX2), &
     &    IUPLO, AF(1: IAF1, 1:IAF2), IPIV(1:IIPIV), IFACT,&
     &    FERR(1: IFERR), BERR(1: IBERR), RCOND,  INFO )
        CASE(1)
        CALL LA_SYSVX( A(1:IA1,1:IA2), B(1:IB1,1), X(1: IX1,1), &
     &    IUPLO, AF(1 : IAF1, 1:IAF2), IPIV(1:IIPIV), IFACT,&
     &    FERR(1), BERR(1), RCOND, INFO ) 
        CASE(:-1)
        CALL UESTOP(SRNAMT)
      END SELECT
      END IF
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      END SUBROUTINE LA_TEST_CSYSVX
