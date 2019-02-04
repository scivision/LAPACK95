SUBROUTINE LA_TEST_DSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK, LWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     April 7, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_SYSV
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, NRHS, LDA, LDB, LWORK
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: UPLO  
!  .. Array Arguments ..
   INTEGER, INTENT(OUT) :: IPIV(1:N)
   REAL(WP), INTENT(INOUT) :: A(1:LDA,1:N), B(1:LDB,1:NRHS), WORK(*)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_SYSV '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_DSYSV '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IA1, IA2, IB1, IB2, IIPIV
      CHARACTER*1 :: IUPLO   
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IA1 = N; IA2 = N; IB1 = N; IB2 = NRHS; IUPLO= UPLO; IIPIV=N
   I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
   CASE(0)
      IF ( NRHS == 1 )  THEN
        CALL LA_SYSV( A(1:IA1,1:IA2), B(1:IB1,1), UPLO, &
     &    IPIV(1:IIPIV), INFO )
        INFO = INFOTC 
      ELSE
        CALL LA_SYSV( A(1:IA1,1:IA2), B(1:IB1,1:IB2), IUPLO, &
     &    IPIV(1:IIPIV), INFO)
        INFO = INFOTC
      END IF
   CASE (1)
      IA2 = IA1 - 1
   CASE (2)
      IB1 = IA1 - 1
   CASE (3)
      IUPLO = 'T'
   CASE (4)
      IIPIV = IA1 - 1
   CASE(:-1,5:)
      CALL UESTOP(SRNAMT)
      END SELECT
      IF( I /= 0 ) THEN
        SELECT CASE (NRHS)
          CASE (2:)
          CALL LA_SYSV( A(1:IA1,1:IA2), B(1:IB1,1:IB2), IUPLO, &
     &                    IPIV(1:IIPIV), INFO )
          CASE(1)
          CALL LA_SYSV( A(1:IA1,1:IA2), B(1:IB1,1), IUPLO, &
     &                 IPIV(1: IIPIV), INFO )
          CASE(:-1)
           CALL UESTOP(SRNAMT)
        END SELECT
     END IF
    CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
 END SUBROUTINE LA_TEST_DSYSV
      
