SUBROUTINE LA_TEST_CGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
!
!  -- LAPACK95 interface driver routine (version 0.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     October 31, 1996
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => SP
   USE LA_AUXMOD, ONLY: ERINFO
   USE F95_LAPACK, ONLY: LA_GETRS
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   CHARACTER*1 TRANS
   INTEGER, INTENT(IN) :: N, NRHS, LDA, LDB
   INTEGER, INTENT(INOUT) :: INFO
!  .. Array Arguments ..
   INTEGER, INTENT(OUT) :: IPIV(1:N)
   COMPLEX(WP), INTENT(INOUT) :: A(1:LDA,1:N), B(1:LDB,1:N)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GETRS'
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_CGETRS'
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   CHARACTER*1 LTRANS
   INTEGER :: I, J, IA1, IA2, IB1, IB2, IIPIV
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IA1 = N; IA2 = N; IB1 = N; IB2 = NRHS; IIPIV = N; LTRANS = TRANS
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
   CASE(0)
      IF ( NRHS ==1 ) THEN
        IF( LTRANS == 'N' )THEN
          CALL LA_GETRS( A(1:IA1,1:IA2), IPIV(1:IIPIV), B(1:IB1,1) )
        ELSE
          CALL LA_GETRS( A(1:IA1,1:IA2), IPIV(1:IIPIV), B(1:IB1,1), &
     &      LTRANS)
        END IF
        INFO = INFOTC
      ELSE
        IF( LTRANS == 'N' )THEN
          CALL LA_GETRS(A(1:IA1,1:IA2), IPIV(1:IIPIV), B(1:IB1,1:IB2))
        ELSE
          CALL LA_GETRS(A(1:IA1,1:IA2), IPIV(1:IIPIV), B(1:IB1,1:IB2),&
     &      LTRANS)
        END IF
        INFO = INFOTC
      END IF
   CASE (1)
      IA2 = IA1-1
   CASE (2)
      IIPIV = IA1-1
   CASE(3)
      IB1 = IA1-1
   CASE(4)
      LTRANS = '/'
   CASE(:-1,5:)
      CALL UESTOP(SRNAMT)
   END SELECT
   IF( I /= 0 ) THEN
      SELECT CASE (NRHS)
      CASE (2:)
         CALL LA_GETRS( A(1:IA1,1:IA2),IPIV(1:IIPIV), B(1:IB1,1:IB2), &
              LTRANS, INFO )
      CASE(1)
         IF( I /= 4 )THEN
            CALL LA_GETRS( A(1:IA1,1:IA2),IPIV(1:IIPIV), B(1:IB1,1), &
                 LTRANS, INFO )
         ELSE
            CALL ERINFO( -I, SRNAME, INFO )
         END IF
      CASE(:-1)
         CALL UESTOP(SRNAMT)
      END SELECT
   END IF
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_CGETRS
