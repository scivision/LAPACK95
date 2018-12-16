SUBROUTINE LA_TEST_ZGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, IPIV, &
                   EQUED, R, C, B, LDB, X, LDX, RCOND, FERR, BERR, &
                   WORK, RWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 0.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     October 31, 1996
!
!  .. Use Statements ..
   USE LA_PRECISION, ONLY: WP => DP
   USE LA_AUXMOD, ONLY: ERINFO
   USE F95_LAPACK, ONLY: LA_GESVX
!  .. Implicit Statement ..
   IMPLICIT NONE
!  .. Scalar Arguments ..
   CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
   CHARACTER(LEN=1), INTENT(IN) :: FACT, TRANS
   INTEGER, INTENT(IN) :: N, NRHS, LDA, LDB, LDAF, LDX
   INTEGER, INTENT(INOUT) :: INFO
   REAL(WP), INTENT(OUT) :: RCOND
!  .. Array Arguments ..
   INTEGER, INTENT(INOUT) :: IPIV(1:N)
   COMPLEX(WP), INTENT(INOUT) :: A(1:LDA,1:N), AF(1:LDAF,1:N), B(1:LDB,1:N), &
                              WORK(*), X(1:LDX,1:NRHS)
   REAL(WP), INTENT(INOUT) :: BERR(1:NRHS), FERR(1:NRHS), R(1:N), C(1:N), &
                              RWORK(*)
!  .. Parameters ..
   CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GESVX'
   CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZGESVX'
!  .. Common blocks ..
   INTEGER :: INFOTC
   COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
   CHARACTER(LEN=1) :: LFACT, LTRANS, LEQUED
   INTEGER :: I, J, IA1, IA2, IB1, IB2, IX1, IX2, IAF1, IAF2, IIPIV, &
              IR, IC, IFERR, IBERR
   LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
   IA1 = N; IA2 = N; IB1 = N; IB2 = NRHS; IX1 = N; IX2 = NRHS
   IAF1 = N; IAF2 = N; IIPIV = N; LFACT = FACT; LTRANS = TRANS
   LEQUED = EQUED; IR = N; IC = N; IFERR = NRHS; IBERR = NRHS
   I = INFO / 100; J = INFO - I*100
   SELECT CASE(I)
   CASE(0)
      IF (NRHS == 1) THEN
        CALL LA_GESVX( A(1:IA1,1:IA2), B(1:IB1,1), X(1:IX1,1), &
        AF(1:IAF1,1:IAF2), IPIV(1:IIPIV), LFACT, LTRANS, LEQUED, &
        R(1:IR), C(1:IC), FERR(1), BERR(1), RCOND, RWORK(1) )
        INFO = INFOTC
      ELSE
        CALL LA_GESVX( A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1:IX1,1:IX2),&
        AF(1:IAF1,1:IAF2), IPIV(1:IIPIV), LFACT, LTRANS, LEQUED, &
        R(1:IR), C(1:IC), FERR(1:IFERR), BERR(1:IBERR), RCOND, &
        RWORK(1) )
        INFO = INFOTC
      ENDIF
   CASE (1)
      IA2 = IA1-1
   CASE (2)
      IB1 = IA1-1
   CASE (3)
I3:   SELECT CASE(J)
      CASE(1)
         IX1 = IA1-1
      CASE (2)
         IX2 = IB2-1
      CASE (3:)
         CALL UESTOP(SRNAMT)
      END SELECT I3
   CASE (4)
I4:   SELECT CASE(J)
      CASE(1)
         IAF1 = IA1-1
      CASE (2)
         IAF2 = IA1-1
      CASE (3:)
         CALL UESTOP(SRNAMT)
      END SELECT I4
   CASE (5)
      IIPIV = IA1-1
   CASE (6)
I6:   SELECT CASE(J)
      CASE(1)
         LFACT = '/'
      CASE (2)
         SELECT CASE(NRHS)
         CASE (2:)
            CALL LA_GESVX(A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
                 AF(1:IAF1,1:IAF2), FACT='F', INFO = INFO)
          CASE (1)
             CALL LA_GESVX(A(1:IA1,1:IA2), B(1:IB1,1), X(1:IX1,1), &
                 AF(1:IAF1,1:IAF2), FACT='F', INFO=INFO)
          CASE DEFAULT
             CALL UESTOP(SRNAMT)
          END SELECT
      CASE (3)
         SELECT CASE(NRHS)
         CASE (2:)
            CALL LA_GESVX(A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
                 IPIV=IPIV(1:IIPIV), FACT='F', INFO=INFO)
          CASE (1)
             CALL LA_GESVX(A(1:IA1,1:IA2), B(1:IB1,1), X(1:IX1,1), &
                  IPIV=IPIV(1:IIPIV), FACT='F', INFO=INFO)
          CASE DEFAULT
             CALL UESTOP(SRNAMT)
          END SELECT
      CASE (4)
         SELECT CASE(NRHS)
         CASE (2:)
            CALL LA_GESVX(A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
                 FACT='F', INFO=INFO)
          CASE (1)
             CALL LA_GESVX(A(1:IA1,1:IA2), B(1:IB1,1), X(1:IX1,1), &
                  FACT='F', INFO=INFO)
          CASE DEFAULT
             CALL UESTOP(SRNAMT)
          END SELECT
      CASE (5:)
         CALL UESTOP(SRNAMT)
      END SELECT I6
   CASE (7)
      LTRANS = '/'; LFACT = 'N'; LEQUED = 'N'
   CASE (8)
      LFACT = 'F'; LTRANS = 'N'
I8:   SELECT CASE (J)
      CASE (1)
         LEQUED = '/'
      CASE (2)
         LEQUED = 'R'
         SELECT CASE (NRHS)
         CASE (2:)
            CALL LA_GESVX(A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
                 AF(1:IAF1,1:IAF2), IPIV(1:IIPIV), LFACT, LTRANS, LEQUED, &
                 INFO=INFO)
         CASE (1)
            CALL LA_GESVX(A(1:IA1,1:IA2), B(1:IB1,1), X(1:IX1,1), &
                 AF(1:IAF1,1:IAF2), IPIV(1:IIPIV), LFACT, LTRANS, LEQUED, &
                 INFO=INFO)
          CASE DEFAULT
             CALL UESTOP(SRNAMT)
         END SELECT
      CASE (3)
         LEQUED = 'B'
         SELECT CASE (NRHS)
         CASE (2:)
            CALL LA_GESVX(A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
                 AF(1:IAF1,1:IAF2), IPIV(1:IIPIV), LFACT, LTRANS, LEQUED, &
                 INFO=INFO)
         CASE (1)
            CALL LA_GESVX(A(1:IA1,1:IA2), B(1:IB1,1), X(1:IX1,1), &
                 AF(1:IAF1,1:IAF2), IPIV(1:IIPIV), LFACT, LTRANS, LEQUED, &
                 INFO=INFO)
          CASE DEFAULT
             CALL UESTOP(SRNAMT)
         END SELECT
      CASE (4)
         LEQUED = 'C'
         SELECT CASE (NRHS)
         CASE (2:)
            CALL LA_GESVX(A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
                 AF(1:IAF1,1:IAF2), IPIV(1:IIPIV), LFACT, LTRANS, LEQUED, &
                 INFO=INFO)
         CASE (1)
            CALL LA_GESVX(A(1:IA1,1:IA2), B(1:IB1,1), X(1:IX1,1), &
                 AF(1:IAF1,1:IAF2), IPIV(1:IIPIV), LFACT, LTRANS, LEQUED, &
                 INFO=INFO)
          CASE DEFAULT
             CALL UESTOP(SRNAMT)
         END SELECT
      CASE (5:)
         CALL UESTOP(SRNAMT)
      END SELECT I8
   CASE (9)
      LFACT = 'F'; R(1) = -1.0_WP; C = +1.0_WP
I9:   SELECT CASE (J)
      CASE(1)
         IR = IA1-1
      CASE(2)
         LEQUED = 'R'
      CASE(3)
         LEQUED = 'B'
      CASE (4:)
         CALL UESTOP(SRNAMT)
      END SELECT I9
   CASE (10)
      LFACT = 'F'; R = +1.0_WP; C(1) = -1.0_WP
I10:  SELECT CASE (J)
      CASE(1)
         IC = IA1-1
      CASE(2)
         LEQUED = 'C'
      CASE(3)
         LEQUED = 'B'
      CASE (4:)
         CALL UESTOP(SRNAMT)
      END SELECT I10
   CASE (11)
      IFERR = IB2-1
   CASE (12)
      IBERR = IB2-1
   CASE (13:)
      CALL UESTOP(SRNAMT)
   END SELECT
   IF( .NOT. ( ( I == 6 .AND. ( J == 2 .OR. J == 3 .OR. J == 4 ) ) &
         .OR. ( I == 8 .AND. ( J == 2 .OR. J == 3 .OR. J == 4 ) )) &
         .AND. ( I /= 0 )) THEN 
      SELECT CASE (NRHS)
      CASE (2:)
         CALL LA_GESVX(A(1:IA1,1:IA2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
              AF(1:IAF1,1:IAF2), IPIV(1:IIPIV), LFACT, LTRANS, LEQUED, &
              R(1:IR), C(1:IC), FERR(1:IFERR), BERR(1:IBERR), RCOND, &
              RWORK(1), INFO)
      CASE (1)
         IF( INFO /= 202 .AND. INFO /= 302 .AND. I /= 11 .AND. I /= 12 )THEN
            CALL LA_GESVX(A(1:IA1,1:IA2), B(1:IB1,1), X(1:IX1,1), &
                 AF(1:IAF1,1:IAF2), IPIV(1:IIPIV), LFACT, LTRANS, LEQUED, &
                 R(1:IR), C(1:IC), FERR(1), BERR(1), RCOND, RWORK(1), INFO)
          ELSE
             CALL ERINFO( -I, SRNAME, INFO )
          END IF
      END SELECT
   END IF
   EQUED = LEQUED
   CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_ZGESVX
