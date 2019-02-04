SUBROUTINE LA_TEST_ZGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
!
!  -- LAPACK95 interface driver routine (version 0.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     October 31, 1996
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_GESV
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   INTEGER, INTENT(IN) :: N, NRHS, LDA, LDB
   INTEGER, INTENT(INOUT) :: INFO
!  .. Array Arguments ..
   INTEGER, INTENT(OUT) :: IPIV(1:N)
   COMPLEX(WP), INTENT(INOUT) :: A(1:LDA,1:N), B(1:LDB,1:N)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GESV '
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZGESV '
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   INTEGER :: I, J, IA1, IA2, IB1, IB2, IIPIV, ISTAT
!  .. Local Arrays ..
   INTEGER, SAVE, POINTER :: IWORK(:)
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IA1 = N; IA2 = N; IB1 = N; IB2 = NRHS; IIPIV = N
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
   CASE(0)
      IF (NRHS == 1) THEN
        CALL LA_GESV( A(1:IA1,1:IA2), B(1:IB1,1), IPIV(1:IIPIV) )
        INFO = INFOTC
      ELSE
        CALL LA_GESV( A(1:IA1,1:IA2), B(1:IB1,1:IB2), IPIV(1:IIPIV) )
        INFO = INFOTC
      END IF            
   CASE (1)
      IA2 = IA1-1
   CASE (2)
      IB1 = IA1-1
   CASE(3)
      IIPIV = IA1-1
   CASE(:-1,4:)
      CALL UESTOP(SRNAMT)
   END SELECT
   IF( I /= 0 ) THEN
      SELECT CASE (NRHS)
      CASE (2:)
         CALL LA_GESV( A(1:IA1,1:IA2), B(1:IB1,1:IB2), IPIV(1:IIPIV), INFO )
      CASE(1)
         CALL LA_GESV( A(1:IA1,1:IA2), B(1:IB1,1), IPIV(1:IIPIV), INFO )
      CASE(:-1)
         CALL UESTOP(SRNAMT)
      END SELECT
   END IF
   IF( I == 0 .AND. J == 1 )THEN
      ALLOCATE( IWORK(N), STAT=ISTAT )
      IF(ISTAT == 0 )THEN
         IWORK(1:N) = IPIV(1:N)
      ELSE
         WRITE(*,*)'STOP in ', SRNAMT, 'Memory allocation failed.'
      END IF
   ELSE IF( I == 0 .AND. J == 4 )THEN
      DEALLOCATE( IWORK, STAT=ISTAT )
   END IF
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_ZGESV
