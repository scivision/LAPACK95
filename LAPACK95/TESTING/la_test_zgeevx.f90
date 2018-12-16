SUBROUTINE LA_TEST_ZGEEVX( BALANC, JOBVL, JOBVR,  SENSE, N, A, LDA,&
     &  W, VL, LDVL, VR, LDVR,  ILO, IHI, SCALE, ABNRM, &
     &  RCONDE, RCONDV, WORK, LWORK, RWORK, INFO )
!
!  -- LAPACK95 interface driver routine (version 1.1) --
!     UNI-C, Denmark;
!     May 29, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => DP
      USE F95_LAPACK, ONLY: LA_GEEVX
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: N, LDA, LDVL, LDVR, LWORK
      INTEGER, INTENT(INOUT) :: INFO
      INTEGER, INTENT(OUT) :: ILO, IHI
      CHARACTER*1, INTENT(IN) :: BALANC, JOBVL, JOBVR, SENSE
      REAL(WP), INTENT(OUT) :: ABNRM
!  .. Array Arguments ..
      COMPLEX(WP), INTENT(INOUT) :: A(1: LDA, 1:N)
      COMPLEX(WP), INTENT(OUT)::  W(1: N), VL(1: LDVL, 1: N), &
     &  VR(1: LDVR, 1:N)
      COMPLEX(WP), INTENT(OUT) :: WORK(1:LWORK)
      REAL(WP), INTENT(OUT) :: SCALE(1:N), RCONDE(1:N), RCONDV(1:N)
      REAL(WP), INTENT(OUT) :: RWORK(1: 2*N)
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GEEVX'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_ZGEEVX'
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IA1, IA2, IW, IVL1, IVL2, IVR1, IVR2,&
     &  IRCONDE, IRCONDV, ISCALE
      CHARACTER*1 :: IBALANC, IJOBVR, IJOBVL   
!  .. Local Arrays ..
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
      LOGICAL LSAME
!  .. Executable Statements ..
      IA1 = N; IA2 = N; IW = N; IVL1 = N; IVL2 = N
      IVR1 = N; IVR2 = N; IJOBVL = JOBVL; IJOBVR = JOBVR
      IRCONDE = N; IRCONDV = N; ISCALE=N; IBALANC=BALANC
      I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
      CASE (1)
      IA2 = IA1 - 1
      CASE (2)
      IW = IW - 1
      CASE (3)
      IVL1 = IVL1 - 1
      IJOBVL = 'V'; IJOBVR = 'V'
      CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
     &  VL(1: IVL1, 1:IVL2), VR(1: IVR1, 1: IVR2), &
     &  IBALANC, ILO, IHI, SCALE(1:ISCALE), ABNRM, &
     &  RCONDE(1:IRCONDE),& 
     &  RCONDV(1:IRCONDV), INFO)
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE (4)
      IVR1 = IVR1 - 1
      IJOBVR = 'V'; IJOBVR = 'V'
      CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
     &  VL(1: IVL1, 1:IVL2), VR(1: IVR1, 1: IVR2), &
     &  IBALANC, ILO, IHI, SCALE(1:ISCALE), ABNRM, &
     &  RCONDE(1:IRCONDE),& 
     &  RCONDV(1:IRCONDV), INFO)
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE (5)
      IBALANC = 'T'
      CASE(8)
      ISCALE = IA1 - 1
      CASE (10)
      IRCONDE = IA1 - 1
      IJOBVR = 'V'; IJOBVR = 'V'
      CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
     &  VL(1: IVL1, 1:IVL2), VR(1: IVR1, 1: IVR2), &
     &  IBALANC, ILO, IHI, SCALE(1:ISCALE), ABNRM, &
     &  RCONDE(1:IRCONDE),& 
     &  RCONDV(1:IRCONDV), INFO)
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      Case (11)
      IRCONDV = IA1 - 1
      IJOBVR = 'V'; IJOBVR = 'V'
      CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
     &  VL(1: IVL1, 1:IVL2), VR(1: IVR1, 1: IVR2), &
     &  IBALANC, ILO, IHI, SCALE(1:ISCALE), ABNRM, &
     &  RCONDE(1:IRCONDE),& 
     &  RCONDV(1:IRCONDV), INFO)
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE(:-1,6,7,9,12:)
      CALL UESTOP(SRNAMT)
      END SELECT
      IF (LSAME(IJOBVR, 'V')) THEN
        IF (LSAME(IJOBVL,'V')) THEN
          SELECT CASE (SENSE)
            CASE ('B')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
&               VL(1: IVL1, 1:IVL2), VR(1: IVR1, 1: IVR2), &
&               IBALANC, ILO, IHI, SCALE(1:ISCALE), ABNRM, &
&               RCONDE(1:IRCONDE),&
&               RCONDV(1:IRCONDV), INFO)
            CASE ('E')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
&               VL(1: IVL1, 1:IVL2), VR(1: IVR1, 1: IVR2), &
&               IBALANC, ILO, IHI, SCALE(1:ISCALE), ABNRM, &
&               RCONDE(1:IRCONDE),&
&               INFO=INFO)
            CASE ('V')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
&               VL(1: IVL1, 1:IVL2), VR(1: IVR1, 1: IVR2), &
&               IBALANC, ILO, IHI, SCALE(1:ISCALE), ABNRM, &
&               RCONDV=RCONDV(1:IRCONDV), INFO=INFO)
            CASE ('N')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
&               VL(1: IVL1, 1:IVL2), VR(1: IVR1, 1: IVR2), &
&               IBALANC, ILO, IHI, SCALE(1:ISCALE), ABNRM, &
&               INFO=INFO)
          END SELECT
        ELSE
          SELECT CASE(SENSE)
            CASE ('B')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
&               VR=VR(1: IVR1, 1: IVR2), BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               RCONDE = RCONDE(1:IRCONDE), &
&               RCONDV = RCONDV(1:IRCONDV), INFO = INFO)
            CASE ('E')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
&               VR=VR(1: IVR1, 1: IVR2), BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               RCONDE = RCONDE(1:IRCONDE), &
&               INFO = INFO)
            CASE ('V')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
&               VR=VR(1: IVR1, 1: IVR2), BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               RCONDV = RCONDV(1:IRCONDV), INFO = INFO)
            CASE ('N')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
&               VR=VR(1: IVR1, 1: IVR2), BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               INFO = INFO)
          END SELECT
        END IF
      ELSE
        IF (LSAME(IJOBVL,'V')) THEN
          SELECT CASE(SENSE)
            CASE ('B')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
&               VL(1: IVL1, 1:IVL2),  BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               RCONDE=RCONDE(1:IRCONDE),&
&               RCONDV = RCONDV(1:IRCONDV), INFO = INFO)
            CASE ('E')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
&               VL(1: IVL1, 1:IVL2),  BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               RCONDE=RCONDE(1:IRCONDE),&
&               INFO = INFO)
            CASE ('V')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
&               VL(1: IVL1, 1:IVL2),  BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               RCONDV = RCONDV(1:IRCONDV), INFO = INFO)
            CASE ('N')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
&               VL(1: IVL1, 1:IVL2),  BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               INFO = INFO)
          END SELECT
        ELSE
          SELECT CASE(SENSE)
            CASE ('B')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
&               BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               RCONDE=RCONDE(1:IRCONDE),&
&               RCONDV=RCONDV(1:IRCONDV), INFO = INFO)
            CASE ('E')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
&               BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               RCONDE=RCONDE(1:IRCONDE),&
&               INFO = INFO)
            CASE ('V')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
&               BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               RCONDV = RCONDV(1:IRCONDV), INFO = INFO)
            CASE ('N')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), W(1: IW), &
&               BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               INFO = INFO)
          END SELECT
        END IF                                         
      END IF
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
    END SUBROUTINE LA_TEST_ZGEEVX
    
    
