SUBROUTINE LA_TEST_ZPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark,
!     April 13, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_POSV
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, NRHS, LDA, LDB
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: UPLO  
!  .. Array Arguments ..
   COMPLEX(WP), INTENT(INOUT) :: A(1:LDA,1:N), B(1:LDB,1:NRHS)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_POSV '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZPOSV '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IA1, IA2, IB1, IB2
      CHARACTER*1 :: IUPLO   
!  .. Local Arrays ..

   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IA1 = N; IA2 = N; IB1 = N; IB2 = NRHS; IUPLO= UPLO
   I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
   CASE(0)
      IF (NRHS == 1) THEN
        CALL LA_POSV( A(1:IA1,1:IA2), B(1:IB1,1), UPLO, INFO )
        INFO = INFOTC
      ELSE
        CALL LA_POSV( A(1:IA1,1:IA2), B(1:IB1,1:IB2), UPLO, INFO )
        INFO = INFOTC
      END IF           
   CASE (1)
      IA2 = IA1 - 1
   CASE (2)
      IB1 = IA1 - 1
   CASE(3)
      IUPLO = 'T'
   CASE(:-1,4:)
      CALL UESTOP(SRNAMT)
      END SELECT
      IF( I /= 0  ) THEN
        SELECT CASE (NRHS)
          CASE (2:)
           CALL LA_POSV( A(1:IA1,1:IA2), B(1:IB1,1:IB2), IUPLO, INFO )
          CASE(1)
           CALL LA_POSV( A(1:IA1,1:IA2), B(1:IB1,1), IUPLO, INFO )
          CASE(:-1)
           CALL UESTOP(SRNAMT)
        END SELECT
     END IF
    CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
 END SUBROUTINE LA_TEST_ZPOSV
      
