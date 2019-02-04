SUBROUTINE LA_TEST_CSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     April 11, 1999
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_SPSV
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, NRHS, LDB
   INTEGER, INTENT(INOUT) :: INFO
   CHARACTER*1, INTENT(IN) :: UPLO  
!  .. Array Arguments ..
   INTEGER, INTENT(OUT) :: IPIV(1:N)
   COMPLEX(WP), INTENT(INOUT) :: AP(1:N*(N+1)/2), B(1:LDB,1:NRHS)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_SPSV '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_CSPSV '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IAP, IB1, IB2, IIPIV
   CHARACTER*1 :: IUPLO   
!  .. Local Arrays ..
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IAP= N*(N+1)/2; IB1 = N; IB2 = NRHS; IUPLO= UPLO; IIPIV=N
   I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
   CASE(0)
      IF ( NRHS == 1 ) THEN
        CALL LA_SPSV( AP(1:IAP), B(1:IB1,1), IUPLO, &
     &    IPIV(1:IIPIV), INFO )
        INFO = INFOTC
      ELSE
        CALL LA_SPSV( AP(1:IAP), B(1:IB1,1:IB2), IUPLO, &
     &    IPIV(1:IIPIV), INFO)
        INFO = INFOTC
      END IF
   CASE (1)
      IAP = IAP - 1
   CASE (2)
      IB1 = IB1 - 1
   CASE (3)
      IUPLO = 'T'
   CASE (4)
      IIPIV = IB1 - 1
   CASE(:-1,5:)
      CALL UESTOP(SRNAMT)
      END SELECT
      IF( I /= 0 ) THEN
        SELECT CASE (NRHS)
          CASE (2:)
            CALL LA_SPSV( AP(1:IAP), B(1:IB1,1:IB2), IUPLO, &
     &                    IPIV(1:IIPIV), INFO )
          CASE(1)
            CALL LA_SPSV( AP(1:IAP), B(1:IB1,1), IUPLO, &
     &                 IPIV(1: IIPIV), INFO )
          CASE(:-1)
            CALL UESTOP(SRNAMT)
        END SELECT
     END IF
    CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
 END SUBROUTINE LA_TEST_CSPSV
      
