SUBROUTINE LA_TEST_ZPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED, &
     &  S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK, RWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark,
!     April 5, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => DP
      USE F95_LAPACK, ONLY: LA_POSVX
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: N, NRHS, LDA, LDB, LDAF, LDX
      INTEGER, INTENT(INOUT) :: INFO
      CHARACTER*1, INTENT(IN) :: FACT, UPLO
      CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
      REAL(WP), INTENT(OUT) :: RCOND 
!  .. Array Arguments ..
      COMPLEX(WP), INTENT(INOUT) :: A(1:LDA,1:N), B(1:LDB,1:NRHS), AF(1:LDAF,1:N)
      REAL(WP), INTENT(INOUT) :: S(1:N)
      REAL(WP), INTENT(OUT) :: FERR(1:NRHS), BERR(1:NRHS)
      COMPLEX(WP), INTENT(OUT) :: X(1:LDX,1:NRHS), WORK(1:3*N)
      REAL(WP), INTENT(OUT) :: RWORK(1:N) 
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_POSVX'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZPOSVX'
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IA1, IA2, IB1, IB2, IX1, IX2, IAF1, IAF2, IS,&
     &  IFERR, IBERR
      CHARACTER*1 :: IUPLO, IFACT, IEQUED
!  .. Local Arrays ..
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
      LOGICAL LSAME
!  .. Executable Statements ..
      IA1 = N; IA2 = N; IB1 = N; IB2 = NRHS; IUPLO= UPLO; IFACT = FACT
      IX1 = N; IX2 = NRHS; IAF1 = N; IAF2 = N; IEQUED = EQUED; IS = N
      IFERR = NRHS; IBERR = NRHS
      I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
      CASE(0)
      IF (NRHS == 1) THEN
        IF (LSAME(IFACT,'F')) THEN
          CALL LA_POSVX( A(1:IA1,1:IA2), B(1:IB1,1), X(1:IX1,1), &
     &      IUPLO, AF(1:IAF1,1:IAF2), IFACT, IEQUED, S(1:IS), FERR(1),&
     &      BERR(1), RCOND, INFO )
        ELSE
          CALL LA_POSVX( A(1:IA1,1:IA2), B(1:IB1,1), X(1:IX1,1), &
     &      IUPLO, AF(1:IAF1,1:IAF2), IFACT, EQUED, S(1:IS), FERR(1),& 
     &      BERR(1), RCOND, INFO )
        ENDIF
        INFO = INFOTC
      ELSE
        IF (LSAME(IFACT,'F')) THEN
          CALL LA_POSVX( A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
     &      IUPLO, AF(1:IAF1,1:IAF2), IFACT, IEQUED, S(1:IS), FERR(1:IFERR),&
     &      BERR(1: IBERR), RCOND, INFO )
        ELSE
          CALL LA_POSVX( A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
     &      IUPLO, AF(1:IAF1,1:IAF2), IFACT, EQUED, S(1:IS), FERR(1:IFERR),&
     &      BERR(1: IBERR), RCOND, INFO )
        ENDIF
        INFO = INFOTC
      END IF
    CASE (1)
      IA2 = IA1 - 1
    CASE (2)
      IB1 = IA1 - 1
    CASE (3)
      IX1 = IA1 - 1
    CASE (4)
      IUPLO = 'T'
    CASE (5)
      IAF1 = IA1 - 1
    CASE (6)
      IFACT = 'T'
    CASE (7)
      IFACT = 'F'
      IEQUED = 'T'
      CALL LA_POSVX( A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
     &  IUPLO, AF(1:IAF1,1:IAF2), IFACT, IEQUED, S(1:IS), FERR(1:IFERR),&
     &  BERR(1: IBERR), RCOND, INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
    CASE (8)
      IS = IA1 - 1
    CASE (9)
      IFACT = 'F' ;  S = +1.0_WP
      IFERR = NRHS - 1
      CALL LA_POSVX( A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
     &  IUPLO, AF(1:IAF1,1:IAF2), IFACT, IEQUED, S(1:IS), FERR(1:IFERR),&
     &  BERR(1: IBERR), RCOND, INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE (10); S = +1.0_WP
      IBERR = IB2 - 1
      CALL LA_POSVX( A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
     &  IUPLO, AF(1:IAF1,1:IAF2), IFACT, IEQUED, S(1:IS), FERR(1:IFERR),&
     &  BERR(1: IBERR), RCOND, INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN 
      CASE(:-1, 11:)
      CALL UESTOP(SRNAMT)
      END SELECT
      IF( I /= 0 ) THEN
        SELECT CASE (NRHS)
        CASE (2:)
        CALL LA_POSVX( A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
     &    IUPLO, AF(1:IAF1,1:IAF2), IFACT, IEQUED, S(1:IS), FERR(1:IFERR),&
     &    BERR(1: IBERR), RCOND, INFO )
        CASE(1)
        CALL LA_POSVX( A(1:IA1,1:IA2), B(1:IB1,1), X(1:IX1,1), &
     &    IUPLO, AF(1:IAF1,1:IAF2), IFACT, IEQUED, S(1:IS), FERR(1),&
     &    BERR(1), RCOND, INFO )
        CASE(:-1)
        CALL UESTOP(SRNAMT)
      END SELECT
      END IF
      
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_ZPOSVX
      
