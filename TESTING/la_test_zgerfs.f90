SUBROUTINE LA_TEST_ZGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, &
                           X, LDX, FERR, BERR, WORK, RWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 0.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     October 31, 1996
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE LA_AUXMOD, ONLY: ERINFO
   USE F95_LAPACK, ONLY: LA_GERFS
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   CHARACTER(LEN=1), INTENT(IN) :: TRANS
   INTEGER, INTENT(IN) :: N, NRHS, LDA, LDAF, LDB, LDX
   INTEGER, INTENT(INOUT) :: INFO
!  .. Array Arguments ..
   INTEGER, INTENT(INOUT) :: IPIV(1:N)
   COMPLEX(WP), INTENT(INOUT) :: A(1:LDA,1:N), AF(1:LDAF,1:N), &
                B(1:LDB,1:NRHS), X(1:LDX,1:NRHS), WORK( * )
   REAL(WP), INTENT(INOUT) :: BERR(1:NRHS), FERR(1:NRHS), RWORK(*)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GERFS'
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZGERFS'
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   CHARACTER(LEN=1) :: LTRANS
   INTEGER :: I, J, IA1, IA2, IB1, IB2, IX1, IX2, IAF1, IAF2, IIPIV, &
              IFERR, IBERR
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IA1 = N; IA2 = N; IB1 = N; IB2 = NRHS; IX1 = N; IX2 = NRHS; IAF1 = N
   IAF2 = N; IIPIV = N; LTRANS = TRANS; IFERR = NRHS; IBERR = NRHS
!  .. Executable Statements ..
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
   CASE(0)
      IF (NRHS == 1) THEN
        IF( LTRANS == 'N' )THEN
          CALL LA_GERFS(A(1:IA1,1:IA2), AF(1:IAF1,1:IAF2), &
     &      IPIV(1:IIPIV), B(1:IB1,1),  X(1:IX1,1), FERR=FERR(1), &
     &      BERR=BERR(1) )
        ELSE
          CALL LA_GERFS(A(1:IA1,1:IA2), AF(1:IAF1,1:IAF2), &
     &      IPIV(1:IIPIV), B(1:IB1,1),  X(1:IX1,1), LTRANS, FERR(1), &
     &      BERR(1) )
        END IF
        INFO = INFOTC
      ELSE
        IF( LTRANS == 'N' )THEN
          CALL LA_GERFS(A(1:IA1,1:IA2), AF(1:IAF1,1:IAF2), &
     &      IPIV(1:IIPIV), B(1:IB1,1:IB2),  X(1:IX1,1:IX2), &
     &      FERR=FERR(1:IFERR), BERR=BERR(1:IBERR) )
        ELSE
          CALL LA_GERFS(A(1:IA1,1:IA2), AF(1:IAF1,1:IAF2), &
     &      IPIV(1:IIPIV), B(1:IB1,1:IB2),  X(1:IX1,1:IX2), LTRANS, &
     &      FERR(1:IFERR), BERR(1:IBERR) )
        END IF
        INFO = INFOTC
      ENDIF
   CASE (1)
      IA2 = IA1-1
   CASE (2)
I2:   SELECT CASE(J)
      CASE(1)
         IAF1 = IA1-1
      CASE(2)
         IAF2 = IA1-1
      CASE (3:)
         CALL UESTOP(SRNAMT)
      END SELECT I2
   CASE(3)
      IIPIV = IA1-1
   CASE(4)
      IB1 = IA1-1
   CASE (5)
I5:   SELECT CASE(J)
      CASE(1)
         IX1 = IA1-1
      CASE (2)
         IX2 = IB2-1
      CASE (3:)
         CALL UESTOP(SRNAMT)
      END SELECT I5
   CASE(6)
      LTRANS = '/'
   CASE(7)
      IFERR = IB2-1
   CASE(8)
      IBERR = IB2-1
   CASE(:-1,9:)
      CALL UESTOP(SRNAMT)
   END SELECT
   IF( I /= 0 ) THEN
      SELECT CASE (NRHS)
      CASE (2:)
         CALL LA_GERFS(A(1:IA1,1:IA2), AF(1:IAF1,1:IAF2), IPIV(1:IIPIV), &
              B(1:IB1,1:IB2),  X(1:IX1,1:IX2), LTRANS, FERR(1:IFERR),  &
              BERR(1:IBERR), INFO)
      CASE (1)
         IF( I /= 4 .AND. INFO /= 502 .AND. I /= 7 .AND. I /= 8 ) THEN
            CALL LA_GERFS(A(1:IA1,1:IA2), AF(1:IAF1,1:IAF2), IPIV(1:IIPIV), &
                 B(1:IB1,1),  X(1:IX1,1), LTRANS, FERR(1),  BERR(1), INFO)
         ELSE
            CALL ERINFO( -I, SRNAME, INFO )
         END IF
      CASE(:-1)
         CALL UESTOP(SRNAMT)
      END SELECT
   END IF
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_ZGERFS
