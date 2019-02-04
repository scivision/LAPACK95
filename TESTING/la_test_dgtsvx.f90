SUBROUTINE LA_TEST_DGTSVX( FACT, TRANS, N, NRHS, DL, D, DU, DLF, DF,&
     &  DUF, DU2, IPIV, B, LDB, X, LDX, RCOND, FERR, BERR, WORK, &
     &  IWORK,INFO)
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark,
!     March 28, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_GTSVX
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, NRHS, LDB, LDX
   INTEGER, INTENT(INOUT) :: INFO
      CHARACTER(LEN=1), INTENT(IN) :: FACT, TRANS
      REAL(WP), INTENT(OUT) :: RCOND
!  .. Array Arguments ..
      INTEGER, INTENT(INOUT) :: IPIV(1:N)
      INTEGER, INTENT(OUT) :: IWORK(1:N)  
      REAL(WP), INTENT(IN) :: B(1:LDB,1:NRHS)
      REAL(WP), INTENT(IN) :: DL(1:N-1), D(1:N), DU(1:N-1)
      REAL(WP), INTENT(INOUT) :: DF(1:N), DLF(1:N-1), DU2(1:N-2), &
     &  DUF(1:N-1)
      REAL(WP), INTENT(OUT) :: FERR(1:NRHS), BERR(1:NRHS), &
     &  X(1:LDX,1:NRHS), WORK(1:3*N)  
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GTSVX'
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_DGTSVX'
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IDU, ID, IDL,IB1, IB2, IX1, IX2, IDLF,&
     &  IDF, IDUF, IDU2, IIPIV, IFERR, IBERR
      CHARACTER(LEN=1) :: IFACT, ITRANS
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
      IDL = N-1; ID = N; IDU = N-1; IB1 = N; IB2 = NRHS
      IX1=N; IX2=NRHS;IDLF=N-1;IDF=N; IDUF=N-1; IDU2= N-2
      IIPIV = N; IFACT = FACT; ITRANS=TRANS; IFERR=NRHS
      IBERR = NRHS
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
    CASE(0)
      IF ( NRHS == 1 ) THEN
        CALL LA_GTSVX( DL(1:IDL), D(1:ID), DU(1:IDU), B(1:IB1, 1), &
     &    X(1:IX1, 1), DLF(1:IDLF), DF(1:IDF), DUF(1:IDUF), &
     &    DU2(1: IDU2), IPIV(1:IIPIV), IFACT, ITRANS, FERR(1), &
     &    BERR(1), RCOND, INFO )
        INFO = INFOTC
      ELSE
        CALL LA_GTSVX( DL(1:IDL), D(1:ID), DU(1:IDU), B(1:IB1, 1:IB2), &
     &    X(1:IX1, 1:IX2), DLF(1:IDLF), DF(1:IDF), DUF(1:IDUF), &
     &    DU2(1: IDU2), IPIV(1:IIPIV), IFACT, ITRANS, FERR(1:IFERR), &
     &    BERR(1: IBERR), RCOND, INFO )
        INFO = INFOTC
      END IF
      CASE (1)
      IDL = ID
      CASE(3)
      IDU = ID 
      CASE(4)
      IB1 = ID - 1
      CASE (5)
      IX1 = ID - 1
      CASE (6)
      IDLF = IDL - 1
      CASE (7)
      IDF = ID - 1
      CASE (8)
      IDUF = IDL - 1
      CASE (9)
      IDU2 = IDL - 3
      CASE (10)
      IIPIV = ID - 1
      CASE (11)
      IFACT = 'T'
      CASE (12)
      ITRANS = 'E'
      CASE (13)
      IFERR = IB2 - 1
      CASE (14)
      IBERR = IB2 - 1
      CASE(:-1,2,15:)
      CALL UESTOP(SRNAMT)
   END SELECT
      IF( I /= 0 ) THEN
      SELECT CASE (NRHS)
      CASE (2:)
      CALL LA_GTSVX( DL(1:IDL), D(1:ID), DU(1:IDU), B(1:IB1, 1:IB2), &
     &  X(1:IX1, 1:IX2), DLF(1:IDLF), DF(1:IDF), DUF(1:IDUF), &
     &  DU2(1: IDU2), IPIV(1:IIPIV), IFACT, ITRANS, FERR(1:IFERR), &
     &  BERR(1: IBERR), RCOND, INFO )
      CASE(1)
      CALL LA_GTSVX( DL(1:IDL), D(1:ID), DU(1:IDU), B(1:IB1, 1), &
     &  X(1:IX1, 1), DLF(1:IDLF), DF(1:IDF), DUF(1:IDUF), &
     &  DU2(1: IDU2), IPIV(1:IIPIV), IFACT, ITRANS, FERR(1), &
     &  BERR(1), RCOND, INFO ) 
      CASE(:-1)
      CALL UESTOP(SRNAMT)
      END SELECT
      END IF
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_DGTSVX
      
