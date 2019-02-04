SUBROUTINE LA_TEST_ZGBSVX( FACT, TRANS, N, KL, KU, NRHS, AB, LDAB, AFB,&
     &  LDAFB, IPIV, EQUED, R, C, B, LDB, X, LDX, RCOND, FERR, BERR, &
     &  WORK, RWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 0.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September 29, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => DP
      USE LA_AUXMOD, ONLY: ERINFO
      USE F95_LAPACK, ONLY: LA_GBSVX
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      CHARACTER(LEN=1), INTENT(INOUT) :: EQUED
      CHARACTER(LEN=1), INTENT(IN) :: FACT, TRANS
      INTEGER, INTENT(IN) :: N, KL, KU, NRHS, LDAB, LDB, LDAFB, LDX
      INTEGER, INTENT(INOUT) :: INFO
      REAL(WP), INTENT(OUT) :: RCOND
!  .. Array Arguments ..
      INTEGER, INTENT(INOUT) :: IPIV(1:N)
      REAL(WP), INTENT(OUT) :: RWORK(1:N)
      COMPLEX(WP), INTENT(INOUT) :: AB(1:LDAB,1:N), AFB(1:LDAFB,1:N),&
     &  B(1:LDB,1:N)
      REAL(WP), INTENT(OUT) :: BERR(1:NRHS), FERR(1:NRHS)
      COMPLEX(WP), INTENT(OUT) :: WORK(*), X(1:LDX,1:NRHS)
      REAL(WP), INTENT(INOUT) :: R(1:N), C(1:N)
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GBSVX'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZGBSVX'
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      CHARACTER(LEN=1) :: IFACT, ITRANS, IEQUED
      INTEGER :: I, J, IAB1, IAB2, IB1, IB2, IX1, IX2, IAFB1, IAFB2, IIPIV, &
     &  IR, IC, IFERR, IBERR, IKL, IKU
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..

      IAB1 = KL+KU+1; IAB2 = N
      IB1 = N; IB2 = NRHS; IX1 = N; IX2 = NRHS
      IAFB1 =  2*KL+KU+1; IAFB2 = N; IIPIV = N; IFACT = FACT; ITRANS = TRANS
      IEQUED = EQUED; IR = N; IC = N; IFERR = NRHS; IBERR = NRHS
      IKL = KL; IKU = KU
      I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
        CASE(0)
          IF (NRHS == 1) THEN
            CALL LA_GBSVX( AB(1:IAB1,1:IAB2), B(1:IB1,1), X(1:IX1,1), KL, &
            AFB(1:IAFB1,1:IAFB2), IPIV(1:IIPIV), IFACT, ITRANS, EQUED, &
            R(1:IR), C(1:IC), FERR(1), BERR(1), RCOND, RPVGRW=RWORK(1) )
            INFO = INFOTC 
          ELSE
            CALL LA_GBSVX( AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), X(1:IX1,1:IX2),&
            KL, AFB(1:IAFB1,1:IAFB2), IPIV(1:IIPIV), IFACT, ITRANS, EQUED, &
            R(1:IR), C(1:IC), FERR(1:IFERR), BERR(1:IBERR), RCOND, &
            RWORK(1) )
            INFO = INFOTC  
          ENDIF
        CASE (1)
          IAB2 = 0
        CASE (2)
          IB1 = IAB2-1
        CASE (3)
    I3:   SELECT CASE(J)
            CASE(1)
              IX1 = IAB2-1
            CASE (2)
              IX2 = IB2-1
            CASE (3:)
              CALL UESTOP(SRNAMT)
          END SELECT I3
        CASE (4)
    I4:   SELECT CASE(J)
            CASE (1)
              IKL = - 1
            CASE (2)
              IKL = - 1
            CASE (3:)
              CALL UESTOP(SRNAMT)
          END SELECT I4
        CASE (5)
    I5:   SELECT CASE(J)
            CASE(1)
              iafb1 = iab1-1
            CASE (2)
              IAFB2 = iab2-1
            CASE (3:)
              CALL UESTOP(SRNAMT)
          END SELECT I5
        CASE (6)
          IIPIV = IAB2-1
        CASE (7)
    I7:   SELECT CASE(J)
            CASE(1)
              IFACT = 'T'
            CASE (2)
              SELECT CASE(NRHS)
                CASE (2:)
                  CALL LA_GBSVX(AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
                  KL, AFB(1:IAFB1,1:IAFB2), FACT='F', INFO = INFO)
                CASE (1)
                  CALL LA_GBSVX(AB(1:IAB1,1:IAB2), B(1:IB1,1), X(1:IX1,1), &
                  KL, AFB(1:IAFB1,1:IAFB2), FACT='F', INFO=INFO)
                CASE DEFAULT
                  CALL UESTOP(SRNAMT)
              END SELECT
            CASE (3)
              SELECT CASE(NRHS)
                CASE (2:)
                  CALL LA_GBSVX(AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
                  KL, IPIV=IPIV(1:IIPIV), FACT='F', INFO=INFO)
                CASE (1)
                  CALL LA_GBSVX(AB(1:IAB1,1:IAB2), B(1:IB1,1), X(1:IX1,1), &
                  KL, IPIV=IPIV(1:IIPIV), FACT='F', INFO=INFO)
                CASE DEFAULT
                  CALL UESTOP(SRNAMT)
              END SELECT
            CASE (4)
              SELECT CASE(NRHS)
                CASE (2:)
                  CALL LA_GBSVX(AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
                  KL, FACT='F', INFO=INFO)
                CASE (1)
                  CALL LA_GBSVX(AB(1:IAB1,1:IAB2), B(1:IB1,1), X(1:IX1,1), &
                  KL, FACT='F', INFO=INFO)
                CASE DEFAULT
                  CALL UESTOP(SRNAMT)
              END SELECT
            CASE (5:)
              CALL UESTOP(SRNAMT)
          END SELECT I7

        CASE (8)
          ITRANS = 'x'; IFACT = 'N'; IEQUED = 'N'
        CASE (9)
          IFACT = 'F'; ITRANS = 'N'
        I9:   SELECT CASE (J)
                CASE (1)
                  IEQUED = 'T'
                CASE (2)
                  IEQUED = 'R'
                  SELECT CASE (NRHS)
                    CASE (2:)
                      CALL LA_GBSVX(AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
                      KL, AFB(1:IAFB1,1:IAFB2), IPIV(1:IIPIV), IFACT, ITRANS, IEQUED, &
                      INFO=INFO)
                    CASE (1)
                      CALL LA_GBSVX(AB(1:IAB1,1:IAB2), B(1:IB1,1), X(1:IX1,1), &
                      AFB=AFB(1:IAFB1,1:IAFB2), IPIV=IPIV(1:IIPIV), FACT=IFACT, &
&                       TRANS=ITRANS, EQUED=IEQUED, &
                      INFO=INFO)
                    CASE DEFAULT
                      CALL UESTOP(SRNAMT)
                  END SELECT
                CASE (3)
                  IEQUED = 'B'
                  SELECT CASE (NRHS)
                    CASE (2:)
                      CALL LA_GBSVX(AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
                      AFB=AFB(1:IAFB1,1:IAFB2), IPIV=IPIV(1:IIPIV), FACT=IFACT, &
&                       TRANS=ITRANS, EQUED=IEQUED, &
                      INFO=INFO)
                    CASE (1)
                      CALL LA_GBSVX(AB(1:IAB1,1:IAB2), B(1:IB1,1), X(1:IX1,1), &
                      AFB=AFB(1:IAFB1,1:IAFB2), IPIV=IPIV(1:IIPIV), FACT=IFACT, &
&                       TRANS=ITRANS, EQUED=IEQUED, &
                      INFO=INFO)
                    CASE DEFAULT
                      CALL UESTOP(SRNAMT)
                  END SELECT
                CASE (4)
                  IEQUED = 'C'
                  SELECT CASE (NRHS)
                    CASE (2:)
                      CALL LA_GBSVX(AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
                      AFB=AFB(1:IAFB1,1:IAFB2), IPIV=IPIV(1:IIPIV), FACT=IFACT, &
&                       TRANS=ITRANS, EQUED=IEQUED, &
                      INFO=INFO)
                    CASE (1)
                      CALL LA_GBSVX(AB(1:IAB1,1:IAB2), B(1:IB1,1), X(1:IX1,1), &
                      AFB=AFB(1:IAFB1,1:IAFB2), IPIV=IPIV(1:IIPIV), FACT=IFACT, &
&                       TRANS=ITRANS, EQUED=IEQUED, &
                      INFO=INFO)
                    CASE DEFAULT
                      CALL UESTOP(SRNAMT)
                  END SELECT
                CASE (5:)
                  CALL UESTOP(SRNAMT)
              END SELECT I9
            CASE (10)
              IFACT = 'F'; R(1) = -1.0_WP; C = +1.0_WP
              I10:   SELECT CASE (J)
                       CASE(1)
                         IR = IAB2-1
                       CASE(2)
                         IEQUED = 'R'
                       CASE(3)
                         IEQUED = 'B'
                       CASE (4:)
                         CALL UESTOP(SRNAMT)
                     END SELECT I10
                     
            CASE (11)
              IFACT = 'F'; R = +1.0_WP; C(1) = -1.0_WP
              I11:  SELECT CASE (J)
                      CASE(1)
                        IC = IAB2-1
                      CASE(2)
                        IEQUED = 'C'
                      CASE(3)
                        IEQUED = 'B'
                      CASE (4:)
                        CALL UESTOP(SRNAMT)
                    END SELECT I11
            CASE (12)
            IFERR = IB2-1; R = +1.0_WP; C = +1.0_WP; IC = IAB2; IR = IAB2
            CASE (13)
            IBERR = IB2-1; R = +1.0_WP; C = +1.0_WP; IC = IAB2; IR = IAB2
            CASE (14:)
              CALL UESTOP(SRNAMT)
          END SELECT

          IF( .NOT. (( I == 7 .AND. ( J == 2 .OR. J == 3 .OR. J == 4 ) ) &
     &      .OR. ( I == 9 .AND. ( J == 2 .OR. J == 3 .OR. J == 4 ) )) &
     &      .AND. I/=0) THEN
            SELECT CASE (NRHS)
              CASE (2:)
                CALL LA_GBSVX(AB(1:IAB1,1:IAB2), B(1:IB1,1:IB2), X(1:IX1,1:IX2), &
                KL=IKL, AFB=AFB(1:IAFB1,1:IAFB2), IPIV=IPIV(1:IIPIV), FACT=IFACT, &
     &            TRANS=ITRANS, EQUED=IEQUED, &
     &            R=R(1:IR), C=C(1:IC), FERR=FERR(1:IFERR), BERR=BERR(1:IBERR), &
     &            RCOND=RCOND, &
                RPVGRW=RWORK(1), INFO=INFO)
              CASE (1)

                IF( INFO /= 302 .AND. I /= 12 .AND. I /= 13 )THEN
                  CALL LA_GBSVX(AB(1:IAB1,1:IAB2), B(1:IB1,1), X(1:IX1,1), &
                  KL=IKL,  AFB=AFB(1:IAFB1,1:IAFB2), IPIV=IPIV(1:IIPIV), FACT=IFACT, &
     &              TRANS=ITRANS, EQUED=IEQUED, &
     &              R=R(1:IR), C=C(1:IC), FERR=FERR(1), BERR=BERR(1), RCOND=RCOND, &
     &              RPVGRW=RWORK(1), INFO=INFO)
                ELSE
                  CALL ERINFO( -I, SRNAME, INFO )
                END IF
            END SELECT
          END IF
          EQUED = IEQUED 
          
          CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
        END SUBROUTINE LA_TEST_ZGBSVX
