SUBROUTINE LA_TEST_SPTSVX( FACT, N, NRHS, D, E, DF, EF, B, LDB, X, LDX,&
     &  RCOND, FERR, BERR, WORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark,
!     April 7, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_PTSVX
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: N, NRHS, LDB, LDX
      INTEGER, INTENT(INOUT) :: INFO
      CHARACTER(LEN=1), INTENT(IN) :: FACT
      REAL(WP), INTENT(OUT) :: RCOND 
!  .. Array Arguments ..
      REAL(WP), INTENT(IN) :: D(1:N), E(1:N-1), B(1:LDB,1:NRHS)
      REAL(WP), INTENT(INOUT) :: DF(1:N), EF(1:N)
      REAL(WP), INTENT(OUT) :: X(1:LDX,1:NRHS), WORK(1:2*N)
      REAL(WP), INTENT(OUT) :: FERR(1:NRHS), BERR(1:NRHS)
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_PTSVX'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_SPTSVX'
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, ID, IE, IB1, IB2, IX1, IX2, IDF, IEF, &
     &  IFERR, IBERR
      CHARACTER(LEN=1) :: IFACT
!  .. Local Arrays ..
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
      ID = N; IE = N-1; IB1 = N; IB2 = NRHS; IX1 = N; IX2 = NRHS
      IDF = N; IEF=N-1; IFACT = FACT; IFERR = NRHS; IBERR = NRHS
      I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
      CASE(0)
      IF (NRHS == 1) THEN
        CALL LA_PTSVX( D(1:ID), E(1:IE), B(1:IB1,1), X(1:IX1,1),&
     &    DF(1:IDF), EF(1:IEF), IFACT, FERR(1), BERR(1),&
     &    RCOND, INFO )
        INFO = INFOTC
      ELSE
        CALL LA_PTSVX( D(1:ID), E(1:IE), B(1:IB1,1:IB2), X(1:IX1,1:IX2),&
     &    DF(1:IDF), EF(1:IEF), IFACT, FERR(1: IFERR), BERR(1:IBERR),&
     &    RCOND, INFO ) 
        INFO = INFOTC
      END IF
      CASE (2)
      IE = ID - 2
      CASE(3)
      IB1 = ID - 1
      CASE (4)
      IX1 = ID - 1
      CASE (5)
      IDF = ID - 1
      CASE (6)
      IEF = IE - 1
      CASE (7)
      IFACT = 'T'
      CASE (8)
      IFERR = IB2 - 1
      CALL LA_PTSVX( D(1:ID), E(1:IE), B(1:IB1,1:IB2), X(1:IX1,1:IX2),&
     &  DF(1:IDF), EF(1:IEF), IFACT, FERR(1: IFERR), BERR(1:IBERR),&
     &  RCOND, INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE (9)
      IBERR = IB2 - 1
      CALL LA_PTSVX( D(1:ID), E(1:IE), B(1:IB1,1:IB2), X(1:IX1,1:IX2),&
     &  DF(1:IDF), EF(1:IEF), IFACT, FERR(1: IFERR), BERR(1:IBERR),&
     &  RCOND, INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN  
      CASE(:-1, 1, 10:)
      CALL UESTOP(SRNAMT)
      END SELECT
      IF( I /= 0 ) THEN
        SELECT CASE (NRHS)
        CASE (2:)
        CALL LA_PTSVX( D(1:ID), E(1:IE), B(1:IB1,1:IB2), X(1:IX1,1:IX2),&
     &    DF(1:IDF), EF(1:IEF), IFACT, FERR(1: IFERR), BERR(1:IBERR),&
     &    RCOND, INFO )
        CASE(1)
        CALL LA_PTSVX( D(1:ID), E(1:IE), B(1:IB1,1), X(1:IX1,1),&
     &    DF(1:IDF), EF(1:IEF), IFACT, FERR(1), BERR(1),&
     &    RCOND, INFO )
        CASE(:-1)
        CALL UESTOP(SRNAMT)
      END SELECT
      END IF
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_SPTSVX
      
