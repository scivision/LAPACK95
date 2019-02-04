      SUBROUTINE LA_TEST_CGGEVX(BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, B, LDB,&
     &  ALPHA, BETA, VL, LDVL, VR, LDVR, ILO, IHI, LSCALE, RSCALE,&
     &  ABNRM, BBNRM, RCONDE, RCONDV, WORK, LWORK, RWORK, IWORK, BWORK, INFO)
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     SEPTEMBER 5, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GGEVX
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: N, LDA, LDB, LDVL, LDVR, LWORK
      INTEGER, INTENT(INOUT) :: INFO
      INTEGER, INTENT(OUT):: ILO, IHI
      CHARACTER*1, INTENT(IN) :: BALANC, JOBVL, JOBVR, SENSE
      REAL(WP), INTENT(OUT) :: ABNRM, BBNRM
!  .. Array Arguments ..
      COMPLEX(WP), INTENT(INOUT) :: A(1:LDA,1:N), B(1:LDB, 1:N)
      REAL(WP), INTENT(OUT):: WORK(1:LWORK)
      COMPLEX(WP), INTENT(OUT) :: ALPHA(1:N), BETA(1:N), &
     &  VL(1: LDVL, 1:N), VR(1: LDVR, 1:N)
      REAL(WP), INTENT(OUT) :: LSCALE(1:N), RSCALE(1:N), &
     &  RCONDE(1:N), RCONDV(1:N)
      REAL(WP), INTENT(OUT) :: RWORK (1: 6*N)
      INTEGER :: IWORK(1: N+6)
      LOGICAL :: BWORK(1:N)
      
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GGEVX'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_CGGEVX'
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IA1, IA2, IB1, IB2, IALPHA, IBETA, IVL1, &
     &  IVL2, IVR1, IVR2, ILSCALE, IRSCALE, IRCONDE, IRCONDV
      CHARACTER*1 :: IJOBVL, IJOBVR, IBALANC
!  .. Local Arrays ..
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
      LOGICAL LSAME
!  .. Executable Statements ..
      IA1 = N; IA2 = N; IJOBVL = JOBVL; IJOBVR = JOBVR; IBALANC = BALANC
      IB1 = N; IB2 = N; IALPHA = N; IBETA = N
      IVL1 = N; IVL2 = N; IVR1 = N; IVR2 = N; ILSCALE = N; IRSCALE = N
      IRCONDE = N; IRCONDV = N
      I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
      CASE (1)
        IA2 = IA1 - 1
      CASE (2)
        IB1 = IA1 - 1
      CASE (3)
        IALPHA = IA1 - 1
      CASE (4)
        IBETA  = IA1 - 1
      CASE (5)
        IJOBVL = 'V'
        IVL1 = IA1 - 1
      CASE (6)
        IJOBVR = 'V'
        IVR1 = IA1 - 1
      CASE (7)
        IBALANC = 'T'
      CASE (10)
        ILSCALE = IA2 - 1
      CASE (11)
        IRSCALE = IA2 - 1
      CASE (14)
        IRCONDE = IA2 - 1
        CALL LA_GGEVX( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&         BETA(1: IBETA), VL(1:IVL1,1:IVL2), &
&         VR(1:IVR1,1:IVR2), IBALANC, ILO, IHI, &
&         LSCALE(1:ILSCALE), RSCALE(1: IRSCALE), &
&         ABNRM, BBNRM, RCONDE(1: IRCONDE), RCONDV(1: IRCONDV), INFO)
        CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
        RETURN
      CASE (15)
        IRCONDV = IA2 - 1
        CALL LA_GGEVX( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&         BETA(1: IBETA), VL(1:IVL1,1:IVL2), &
&         VR(1:IVR1,1:IVR2), IBALANC, ILO, IHI, &
&         LSCALE(1:ILSCALE), RSCALE(1: IRSCALE), &
&         ABNRM, BBNRM, RCONDE(1: IRCONDE), RCONDV(1: IRCONDV), INFO)
        CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
        RETURN
      CASE(:-1,8, 9, 12, 13, 16:)
        CALL UESTOP(SRNAMT)
    END SELECT
    IF (LSAME(SENSE, 'B')) THEN
      IF (LSAME(IJOBVL,'V')) THEN
        IF (LSAME (IJOBVR, 'V')) THEN
          CALL LA_GGEVX( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&           BETA(1: IBETA), VL(1:IVL1,1:IVL2), &
&           VR(1:IVR1,1:IVR2), IBALANC, ILO, IHI, &
&           LSCALE(1:ILSCALE), RSCALE(1: IRSCALE), &
&           ABNRM, BBNRM, RCONDE(1: IRCONDE), RCONDV(1: IRCONDV), INFO)    
        ELSE
          CALL LA_GGEVX( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&           BETA(1: IBETA), VL(1:IVL1,1:IVL2), BALANC=IBALANC, &
&           ILO=ILO, IHI=IHI, LSCALE=LSCALE(1:ILSCALE), RSCALE=RSCALE(1 : IRSCALE), &
&           ABNRM=ABNRM, BBNRM=BBNRM, RCONDE=RCONDE(1: IRCONDE), &
&           RCONDV=RCONDV(1 : IRCONDV), INFO = INFO)
        END IF
      ELSE
        IF (LSAME (IJOBVR, 'V')) THEN
          CALL LA_GGEVX( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&           BETA(1: IBETA), VR = VR(1:IVR1,1:IVR2), ILO=ILO, IHI=IHI, &
&           BALANC=IBALANC, LSCALE=LSCALE(1 :ILSCALE), &
&           RSCALE=RSCALE(1: IRSCALE), ABNRM=ABNRM, BBNRM=BBNRM, RCONDE=RCONDE(1 : IRCONDE), &
&           RCONDV=RCONDV(1 : IRCONDV), INFO = INFO)
        ELSE
          CALL LA_GGEVX( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&           BETA(1: IBETA), BALANC=IBALANC, ILO=ILO, IHI=IHI, &
&           LSCALE=LSCALE(1  :ILSCALE), RSCALE=RSCALE(1: IRSCALE), ABNRM=ABNRM, &
&           BBNRM=BBNRM,  RCONDE=RCONDE(1 : IRCONDE), &
&           RCONDV=RCONDV(1 : IRCONDV), INFO = INFO)
        END IF
      END IF
    ELSE
      IF (LSAME(SENSE, 'E')) THEN 
        IF (LSAME(IJOBVL,'V')) THEN
          IF (LSAME (IJOBVR, 'V')) THEN
            CALL LA_GGEVX( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&             BETA(1: IBETA), VL(1:IVL1,1:IVL2), &
&             VR(1:IVR1,1:IVR2), IBALANC, ILO, IHI, LSCALE(1:ILSCALE), RSCALE(1: IRSCALE), &
&             ABNRM, BBNRM, RCONDE(1: IRCONDE), INFO=INFO)    
          ELSE
            CALL LA_GGEVX( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&             BETA(1: IBETA), VL(1:IVL1,1:IVL2), BALANC=IBALANC, &
&             ILO=ILO, IHI=IHI, LSCALE=LSCALE(1:ILSCALE), RSCALE=RSCALE(1 : IRSCALE), &
&             ABNRM=ABNRM, BBNRM=BBNRM, RCONDE=RCONDE(1: IRCONDE), &
&             INFO = INFO)
          END IF
        ELSE
          IF (LSAME (IJOBVR, 'V')) THEN
            CALL LA_GGEVX( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&             BETA(1: IBETA), VR = VR(1:IVR1,1:IVR2), ILO=ILO, IHI=IHI, &
&             BALANC=IBALANC, LSCALE=LSCALE(1 :ILSCALE), &
&             RSCALE=RSCALE(1: IRSCALE), ABNRM=ABNRM, BBNRM=BBNRM, RCONDE=RCONDE(1 : IRCONDE), &
&             INFO = INFO)
          ELSE
            CALL LA_GGEVX( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&             BETA(1: IBETA), BALANC=IBALANC, ILO=ILO, IHI=IHI, &
&             LSCALE=LSCALE(1  :ILSCALE), RSCALE=RSCALE(1: IRSCALE), ABNRM=ABNRM, &
&             BBNRM=BBNRM,  RCONDE=RCONDE(1 : IRCONDE), &
&             INFO = INFO)
          END IF
        END IF
      ELSE
        IF (LSAME(SENSE, 'V')) THEN
          IF (LSAME(IJOBVL,'V')) THEN
            IF (LSAME (IJOBVR, 'V')) THEN
              CALL LA_GGEVX( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&               BETA(1: IBETA), VL(1:IVL1,1:IVL2), &
&               VR(1:IVR1,1:IVR2), IBALANC, ILO, IHI, LSCALE(1:ILSCALE), RSCALE(1: IRSCALE), &
&               ABNRM, BBNRM, RCONDV(1: IRCONDV), INFO=INFO)    
            ELSE
              CALL LA_GGEVX( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&               BETA(1: IBETA), VL(1:IVL1,1:IVL2), BALANC=IBALANC, &
&               ILO=ILO, IHI=IHI, LSCALE=LSCALE(1:ILSCALE), RSCALE=RSCALE(1 : IRSCALE), &
&               ABNRM=ABNRM, BBNRM=BBNRM, &
&               RCONDV=RCONDV(1 : IRCONDV), INFO = INFO)
            END IF
          ELSE
            IF (LSAME (IJOBVR, 'V')) THEN
              CALL LA_GGEVX( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&               BETA(1: IBETA), VR = VR(1:IVR1,1:IVR2), ILO=ILO, IHI=IHI, &
&               BALANC=IBALANC, LSCALE=LSCALE(1 :ILSCALE), &
&               RSCALE=RSCALE(1: IRSCALE), ABNRM=ABNRM, BBNRM=BBNRM, &
&               RCONDV=RCONDV(1 : IRCONDV), INFO = INFO)
            ELSE
              CALL LA_GGEVX( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&               BETA(1: IBETA), BALANC=IBALANC, ILO=ILO, IHI=IHI, &
&               LSCALE=LSCALE(1  :ILSCALE), RSCALE=RSCALE(1: IRSCALE), ABNRM=ABNRM, &
&               BBNRM=BBNRM,  &
&               RCONDV=RCONDV(1 : IRCONDV), INFO = INFO)
            END IF
          END IF
        ELSE
          IF (LSAME(IJOBVL,'V')) THEN
            IF (LSAME (IJOBVR, 'V')) THEN
              CALL LA_GGEVX( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&               BETA(1: IBETA), VL(1:IVL1,1:IVL2), &
&               VR(1:IVR1,1:IVR2), IBALANC, ILO, IHI, LSCALE(1:ILSCALE), RSCALE(1: IRSCALE), &
&               ABNRM, BBNRM, INFO=INFO)    
            ELSE
              CALL LA_GGEVX( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&               BETA(1: IBETA), VL(1:IVL1,1:IVL2), BALANC=IBALANC, &
&               ILO=ILO, IHI=IHI, LSCALE=LSCALE(1:ILSCALE), RSCALE=RSCALE(1 : IRSCALE), &
&               ABNRM=ABNRM, BBNRM=BBNRM, &
&               INFO = INFO)
            END IF
          ELSE
            IF (LSAME (IJOBVR, 'V')) THEN
              CALL LA_GGEVX( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&               BETA(1: IBETA), VR = VR(1:IVR1,1:IVR2), ILO=ILO, IHI=IHI, &
&               BALANC=IBALANC, LSCALE=LSCALE(1 :ILSCALE), &
&               RSCALE=RSCALE(1: IRSCALE), ABNRM=ABNRM, BBNRM=BBNRM,&
&               INFO = INFO)
            ELSE
              CALL LA_GGEVX( A(1:IA1,1:IA2), B(1:IB1, 1:IB2), ALPHA(1: IALPHA), &
&               BETA(1: IBETA), BALANC=IBALANC, ILO=ILO, IHI=IHI, &
&               LSCALE=LSCALE(1  :ILSCALE), RSCALE=RSCALE(1: IRSCALE), ABNRM=ABNRM, &
&               BBNRM=BBNRM,  &
&               INFO = INFO)
            END IF
          END IF
        END IF
      END IF
    END IF
    CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
  END SUBROUTINE LA_TEST_CGGEVX
      
      
