SUBROUTINE LA_TEST_DGEEVX( BALANC, JOBVL, JOBVR,  SENSE, N, A, LDA,&
     &  WR, WI, VL, LDVL, VR, LDVR,  ILO, IHI, SCALE, ABNRM, &
     &  RCONDE, RCONDV, WORK, LWORK, IWORK, INFO )
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
      REAL(WP), INTENT(INOUT) :: A(1: LDA, 1:N)
      REAL(WP), INTENT(OUT)::  WR(1: N), WI(1: N), VL(1: LDVL, 1: N), &
     &  VR(1: LDVR, 1:N)
      REAL(WP), INTENT(OUT) :: WORK(1:LWORK)
      REAL(WP), INTENT(OUT) :: SCALE(1:N), RCONDE(1:N), RCONDV(1:N)
      INTEGER, INTENT(OUT) :: IWORK(1: 2*N-2)
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GEEVX'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_DGEEVX'
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IA1, IA2, IWR, IWI, IVL1, IVL2, IVR1, IVR2,&
     &  IRCONDE, IRCONDV, ISCALE
      CHARACTER*1 :: IBALANC, IJOBVR, IJOBVL   
!  .. Local Arrays ..
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
      LOGICAL LSAME
!  .. Executable Statements ..
      IA1 = N; IA2 = N; IWI = N; IWR = N; IVL1 = N; IVL2 = N
      IVR1 = N; IVR2 = N; IJOBVL = JOBVL; IJOBVR = JOBVR
      IRCONDE = N; IRCONDV = N; ISCALE=N; IBALANC=BALANC
      I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
      CASE (1)
      IA2 = IA1 - 1
      CASE (2)
      IWR = IWR - 1
      CASE (3)
      IWI = IWI - 1
      CASE (4)
      IVL1 = IVL1 - 1
      IJOBVL = 'V'; IJOBVR = 'V'
      CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&       VL(1: IVL1, 1:IVL2), VR(1: IVR1, 1: IVR2), &
&       IBALANC, ILO, IHI, SCALE(1:ISCALE), ABNRM, &
&       RCONDE(1:IRCONDE),& 
&       RCONDV(1:IRCONDV), INFO)
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
      CASE (5)
      IVR1 = IVR1 - 1
      IJOBVR = 'V'; IJOBVR = 'V'
      CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&       VL(1: IVL1, 1:IVL2), VR(1: IVR1, 1: IVR2), &
&       IBALANC, ILO, IHI, SCALE(1:ISCALE), ABNRM, &
&       RCONDE(1:IRCONDE),& 
&       RCONDV(1:IRCONDV), INFO)
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
    CASE (6)
      IBALANC = 'T'
    CASE(9)
      ISCALE = IA1 - 1
    CASE (11)
      IRCONDE = IA1 - 1
      IJOBVR = 'V'; IJOBVR = 'V'
      CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&       VL(1: IVL1, 1:IVL2), VR(1: IVR1, 1: IVR2), &
&       IBALANC, ILO, IHI, SCALE(1:ISCALE), ABNRM, &
&       RCONDE(1:IRCONDE),& 
&       RCONDV(1:IRCONDV), INFO)
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
    Case (12)
      IRCONDV = IA1 - 1
      IJOBVR = 'V'; IJOBVR = 'V'
      CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&       VL(1: IVL1, 1:IVL2), VR(1: IVR1, 1: IVR2), &
&       IBALANC, ILO, IHI, SCALE(1:ISCALE), ABNRM, &
&       RCONDE(1:IRCONDE),& 
&       RCONDV(1:IRCONDV), INFO)
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
      RETURN
    CASE(:-1,7, 8, 10, 13:)
      CALL UESTOP(SRNAMT)
      END SELECT
      IF (LSAME(IJOBVR, 'V')) THEN
        IF (LSAME(IJOBVL,'V')) THEN
          SELECT CASE (SENSE)
            CASE ('B')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&               VL(1: IVL1, 1:IVL2), VR(1: IVR1, 1: IVR2), &
&               IBALANC, ILO, IHI, SCALE(1:ISCALE), ABNRM, &
&               RCONDE(1:IRCONDE),&
&               RCONDV(1:IRCONDV), INFO)
            CASE ('E')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&               VL(1: IVL1, 1:IVL2), VR(1: IVR1, 1: IVR2), &
&               IBALANC, ILO, IHI, SCALE(1:ISCALE), ABNRM, &
&               RCONDE(1:IRCONDE),&
&               INFO=INFO)
            CASE ('V')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&               VL(1: IVL1, 1:IVL2), VR(1: IVR1, 1: IVR2), &
&               IBALANC, ILO, IHI, SCALE(1:ISCALE), ABNRM, &
&               RCONDV=RCONDV(1:IRCONDV), INFO=INFO)
            CASE ('N')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&               VL(1: IVL1, 1:IVL2), VR(1: IVR1, 1: IVR2), &
&               IBALANC, ILO, IHI, SCALE(1:ISCALE), ABNRM, &
&               INFO=INFO)
          END SELECT
        ELSE
          SELECT CASE(SENSE)
            CASE ('B')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&               VR=VR(1: IVR1, 1: IVR2), BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               RCONDE = RCONDE(1:IRCONDE), &
&               RCONDV = RCONDV(1:IRCONDV), INFO = INFO)
            CASE ('E')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&               VR=VR(1: IVR1, 1: IVR2), BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               RCONDE = RCONDE(1:IRCONDE), &
&               INFO = INFO)
            CASE ('V')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&               VR=VR(1: IVR1, 1: IVR2), BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               RCONDV = RCONDV(1:IRCONDV), INFO = INFO)
            CASE ('N')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&               VR=VR(1: IVR1, 1: IVR2), BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               INFO = INFO)
          END SELECT
        END IF
      ELSE
        IF (LSAME(IJOBVL,'V')) THEN
          SELECT CASE(SENSE)
            CASE ('B')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&               VL(1: IVL1, 1:IVL2),  BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               RCONDE=RCONDE(1:IRCONDE),&
&               RCONDV = RCONDV(1:IRCONDV), INFO = INFO)
            CASE ('E')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&               VL(1: IVL1, 1:IVL2),  BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               RCONDE=RCONDE(1:IRCONDE),&
&               INFO = INFO)
            CASE ('V')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&               VL(1: IVL1, 1:IVL2),  BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               RCONDV = RCONDV(1:IRCONDV), INFO = INFO)
            CASE ('N')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&               VL(1: IVL1, 1:IVL2),  BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               INFO = INFO)
          END SELECT
        ELSE
          SELECT CASE(SENSE)
            CASE ('B')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&               BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               RCONDE=RCONDE(1:IRCONDE),&
&               RCONDV=RCONDV(1:IRCONDV), INFO = INFO)
            CASE ('E')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&               BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               RCONDE=RCONDE(1:IRCONDE),&
&               INFO = INFO)
            CASE ('V')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&               BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               RCONDV = RCONDV(1:IRCONDV), INFO = INFO)
            CASE ('N')
              CALL LA_GEEVX(A(1:IA1, 1:IA2), WR(1:IWR), WI(1: IWI), &
&               BALANC=IBALANC, ILO=ILO, &
&               IHI=IHI, SCALE=SCALE(1:ISCALE), ABNRM=ABNRM,&
&               INFO = INFO)
          END SELECT
        END IF                                         
      END IF
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
    END SUBROUTINE LA_TEST_DGEEVX
    
    
