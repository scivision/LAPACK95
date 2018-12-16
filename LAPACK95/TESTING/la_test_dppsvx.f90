SUBROUTINE LA_TEST_DPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B, &
&   LDB, X, LDX, RCOND, FERR, BERR, WORK, IWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark,
!     April 5, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_PPSVX
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, NRHS, LDB, LDX
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: FACT, UPLO
   CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
   REAL(WP), INTENT(OUT) :: RCOND
!  .. Array Arguments ..
   REAL(WP), INTENT(INOUT) :: AP(N*(N+1)/2), B(1:LDB,1:NRHS), &
&    AFP(1: N*(N+1)/2), S(1:N)
   INTEGER, INTENT(OUT) :: IWORK(1:N)
   REAL(WP), INTENT(OUT) :: FERR(1:NRHS), BERR(1:NRHS)
   REAL(WP), INTENT(OUT) :: X(1:LDX,1:NRHS), WORK(1:3*N)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_PPSVX'
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_DPPSVX'
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IAP1,  IB1, IB2, IX1, IX2, IAFP, IS, &
&    IFERR, IBERR
   CHARACTER*1 :: IUPLO, IFACT, IEQUED
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IAP1 = (N*(N+1))/2; IB1 = N; IB2 = NRHS; IUPLO= UPLO
   IX1=N; IX2=NRHS; IAFP = (N*(N+1))/2; IFACT=FACT; IS = N
   IBERR = NRHS; IFERR = NRHS
   I = INFO / 100; J = INFO - I*100

   SELECT CASE(I)
   CASE(0)
      IF (NRHS == 1) THEN
        CALL LA_PPSVX( AP(1:IAP1), B(1:IB1,1), X(1:IX1,1), &
&         IUPLO, AFP(1:IAFP), IFACT, &
&         EQUED, S(1: IS), FERR(1), BERR(1), RCOND, INFO )
        INFO = INFOTC
      ELSE
        CALL LA_PPSVX( AP(1:IAP1), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
&         IUPLO, AFP(1:IAFP), IFACT, &
&         EQUED, S(1: IS), FERR(1:IFERR), BERR(1:IBERR), RCOND, INFO )
        INFO = INFOTC
      END IF
   CASE (1)
      IAP1 = IAP1-1
   CASE (2)
     IB1 = IAP1-1
   CASE(3)
     IFACT = 'F'
     IX1 = IB1-1
     IF (NRHS == 1) THEN
       CALL LA_PPSVX( AP(1:IAP1), B(1:IB1,1), X(1:IX1,1), &
&        IUPLO, AFP(1:IAFP), IFACT, &
&        EQUED, S(1: IS), FERR(1), BERR(1), RCOND, INFO )
     ELSE
       CALL LA_PPSVX( AP(1:IAP1), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
&        IUPLO, AFP(1:IAFP), IFACT, &
&        EQUED, S(1: IS), FERR(1:IFERR), BERR(1:IBERR), RCOND, INFO )
     ENDIF
     CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
     RETURN
   CASE(4)
     IUPLO='T'
   CASE (5)
     IAFP = IAP1 - 1
   CASE(6)
     IFACT = 'T'
   CASE (7)
     IFACT = 'F'
     IEQUED = 'T'
     IF (NRHS == 1) THEN
       CALL LA_PPSVX( AP(1:IAP1), B(1:IB1,1), X(1:IX1,1), &
     &   IUPLO, AFP(1:IAFP), IFACT, &
     &   IEQUED, S(1: IS), FERR(1), BERR(1), RCOND, INFO )
     ELSE
       CALL LA_PPSVX( AP(1:IAP1), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
     &   IUPLO, AFP(1:IAFP), IFACT, &
     &   IEQUED, S(1: IS), FERR(1:IFERR), BERR(1:IBERR), RCOND, INFO )
     ENDIF                      
     CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
     RETURN
   CASE (8)
     IS = IB1 - 1
      CASE (9)
      IFACT = 'F' ;  S = +1.0_WP 
      IFERR = IB2 - 1
      IF (NRHS == 1) THEN
        CALL LA_PPSVX( AP(1:IAP1), B(1:IB1,1), X(1:IX1,1), &
     &    IUPLO, AFP(1:IAFP), IFACT, &
     &    EQUED, S(1: IS), FERR(1), BERR(1), RCOND, INFO )
      ELSE
        CALL LA_PPSVX( AP(1:IAP1), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
     &    IUPLO, AFP(1:IAFP), IFACT, &
     &    EQUED, S(1: IS), FERR(1:IFERR), BERR(1:IBERR), RCOND, INFO )
      ENDIF
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN         
   CASE (10)
      IBERR = IB2 - 1; S = +1.0_WP; IFACT = 'F'
      IF (NRHS == 1) THEN
        CALL LA_PPSVX( AP(1:IAP1), B(1:IB1,1), X(1:IX1,1), &
     &    IUPLO, AFP(1:IAFP), IFACT, &
     &    EQUED, S(1: IS), FERR(1), BERR(1), RCOND, INFO )
      ELSE
        CALL LA_PPSVX( AP(1:IAP1), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
     &    IUPLO, AFP(1:IAFP), IFACT, &
     &    EQUED, S(1: IS), FERR(1:IFERR), BERR(1:IBERR), RCOND, INFO )
      ENDIF
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN                                           
   CASE(:-1,11:)
      CALL UESTOP(SRNAMT)
  END SELECT
  IF( I /= 0 ) THEN
    SELECT CASE (NRHS)
      CASE (2:)
        CALL LA_PPSVX( AP(1:IAP1), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
&         IUPLO, AFP(1:IAFP), IFACT, &
&         EQUED, S(1: IS), FERR(1:IFERR), BERR(1:IBERR), RCOND, INFO ) 
      CASE(1)
        CALL LA_PPSVX( AP(1:IAP1), B(1:IB1,1), X(1:IX1,1), &
&         IUPLO, AFP(1:IAFP), IFACT, &
&         EQUED, S(1: IS), FERR(1), BERR(1), RCOND, INFO ) 
      CASE(:-1)
        CALL UESTOP(SRNAMT)
    END SELECT
  END IF
  CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_DPPSVX
