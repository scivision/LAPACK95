SUBROUTINE LA_TEST_ZPBSVX(FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB, &
     &  LDAFB, EQUED, S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK, &
     &  RWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark,
!     April 5, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_PBSVX
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, KD, NRHS, LDAB, LDB, LDAFB, LDX
   INTEGER, INTENT(INOUT) :: INFO
      CHARACTER*1, INTENT(IN) :: FACT, UPLO
      CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
      REAL(WP), INTENT(OUT) ::  RCOND 
!  .. Array Arguments ..
      COMPLEX(WP), INTENT(INOUT) :: AB(1:LDAB,1:N), B(1:LDB,1:NRHS),&
     &  AFB(1:LDAFB, 1:N)
      REAL(WP), INTENT(INOUT) :: S(1:N)
      COMPLEX(WP), INTENT(OUT) :: X(1:LDX,1:NRHS), WORK(1:2*N)
      REAL(WP), INTENT(OUT) :: FERR(1:NRHS), BERR(1:NRHS)
      REAL(WP), INTENT(OUT) :: RWORK(1: N)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_PBSVX'
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZPBSVX'
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IAB1, IAB2, IB1, IB2, IX1, IX2, IAFB1, IAFB2, &
     &  IS, IFERR, IBERR
      CHARACTER*1 :: IUPLO, IFACT, IEQUED
      logical :: lsame
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IAB1 = KD+1; IAB2 = N; IB1 = N; IB2 = NRHS; IUPLO=UPLO
   IX1 = N; IX2 = NRHS; IAFB1 = KD+1; IAFB2=N; IFACT=FACT
   IEQUED = EQUED; IS = N; IFERR = NRHS; IBERR = NRHS
   I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
      CASE(0)
      IF ( NRHS == 1 ) THEN
        CALL LA_PBSVX( AB(1:IAB1,1:IAB2), B(1:IB1,1), X(1:IX1,1),&
     &    IUPLO, AFB(1:IAFB1,1:IAFB2), IFACT, IEQUED, S(1:IS),&
     &    FERR(1), BERR(1), RCOND, INFO )
        INFO = INFOTC
      ELSE
        CALL LA_PBSVX( AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), X(1:IX1,1:IX2),&
     &    IUPLO, AFB(1:IAFB1,1:IAFB2), IFACT, IEQUED, S(1:IS),&
     &    FERR(1: IFERR), BERR(1: IBERR), RCOND, INFO ) 
        INFO = INFOTC
      END IF
      CASE (1)
      IAB1 = -1
      CASE (2)
      IB1 = IAB1-1
      CASE (3)
      IX1 = IAB2 - 1
      CASE (4)
      IUPLO = 'T'
      CASE (5)
      IAFB2 = N+1
      CALL LA_PBSVX( AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), X(1:IX1,1:IX2),&
     &  IUPLO, AFB(1:IAFB1,1:IAFB2), IFACT, IEQUED, S(1:IS),&
     &  FERR(1: IFERR), BERR(1: IBERR), RCOND, INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE (6)
      IFACT = 'T'
      CASE (7)
      IEQUED = 'T'
      IFACT  = 'F'
      CALL LA_PBSVX( AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), X(1:IX1,1:IX2),&
     &  IUPLO, AFB(1:IAFB1,1:IAFB2), IFACT, IEQUED, S(1:IS),&
     &  FERR(1: IFERR), BERR(1: IBERR), RCOND, INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE (8)
      IS = IAB2 - 1
      CASE (9)
      IFERR = IX2 - 1
      CALL LA_PBSVX( AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), X(1:IX1,1:IX2),&
     &  IUPLO, AFB(1:IAFB1,1:IAFB2), IFACT, IEQUED, &
     &  FERR=FERR(1: IFERR), BERR=BERR(1: IBERR), RCOND=RCOND, INFO=INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE (10)
      IBERR = IX2 - 1 
      CALL LA_PBSVX( AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), X(1:IX1,1:IX2),&
     &  IUPLO, AFB(1:IAFB1,1:IAFB2), IFACT, IEQUED, &
     &  FERR=FERR(1: IFERR), BERR=BERR(1: IBERR), RCOND=RCOND, INFO=INFO )
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE(:-1,11:)
      CALL UESTOP(SRNAMT)
      END SELECT
      IF( I /= 0 ) THEN
        SELECT CASE (NRHS)
        CASE (2:)
        IF (LSAME(IFACT,'F'))THEN
          CALL LA_PBSVX( AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), X(1:IX1,1:IX2),&
     &      IUPLO, AFB(1:IAFB1,1:IAFB2), IFACT, IEQUED, S(1:IS),&
     &      FERR(1: IFERR), BERR(1: IBERR), RCOND, INFO )
        else
          CALL LA_PBSVX( AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), X(1:IX1,1:IX2),&
     &      IUPLO, FACT=IFACT, EQUED=IEQUED, S=S(1:IS),&
     &      FERR=FERR(1: IFERR), BERR=BERR(1: IBERR), RCOND=RCOND, INFO=INFO )
        endif
        
      CASE(1)
      CALL LA_PBSVX( AB(1:IAB1,1:IAB2), B(1:IB1,1), X(1:IX1,1),&
     &  IUPLO, AFB(1:IAFB1,1:IAFB2), IFACT, IEQUED, S(1:IS),&
     &  FERR(1), BERR(1), RCOND, INFO )
      CASE(:-1)
      CALL UESTOP(SRNAMT)
      END SELECT
  END IF
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_ZPBSVX
      
