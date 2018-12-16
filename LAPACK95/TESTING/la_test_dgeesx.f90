     SUBROUTINE LA_TEST_DGEESX( JOBVS, SORT, SELECT, SENSE, N, A, LDA, SDIM, &
     &  WR, WI, VS, LDVS, RCONDE, RCONDV, WORK, LWORK, IWORK, LIWORK, BWORK, INFO)
!
!  -- LAPACK95 interface driver routine (version 0.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     May 21, 1999
!
!  .. Use Statements ..
      USE LA_PRECISION, ONLY: WP => DP
      USE F95_LAPACK, ONLY: LA_GEESX
!  .. Implicit Statement ..
      IMPLICIT NONE
!  .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: N, LDA, LDVS, LWORK, LIWORK
      INTEGER, INTENT(OUT) :: SDIM
      INTEGER, INTENT(INOUT) :: INFO
      CHARACTER*1, INTENT(IN) :: JOBVS, SORT, SENSE
      REAL(WP), INTENT(OUT) :: RCONDV, RCONDE 
!   LOGICAL SELECT
      INTERFACE
      LOGICAL FUNCTION SELECT(WR, WI)
      USE LA_PRECISION, ONLY: WP => DP
      REAL(WP), INTENT(IN) :: WR, WI
      END FUNCTION SELECT
      END INTERFACE
!  .. Array Arguments ..
      REAL(WP), INTENT(INOUT) :: A(1:LDA,1:N)
      REAL(WP), INTENT(OUT) :: WR(1: N), WI(1: N), VS(1: LDVS, 1: N), &
     &  WORK (1: LWORK)
      LOGICAL :: BWORK(1: N)
      INTEGER, INTENT(OUT) :: IWORK(1: LIWORK)
      LOGICAL LSAME
!  .. Parameters ..
      CHARACTER(LEN=8),  PARAMETER :: SRNAME = 'LA_GEESX'
      CHARACTER(LEN=14), PARAMETER :: SRNAMT = 'LA_TEST_DGEESX'
!  .. Common blocks ..
      INTEGER :: INFOTC
      COMMON /LINFO95/ INFOTC
!  .. Local Scalars ..
      INTEGER :: I, J, IA1, IA2, ISDIM, IWI, IWR, IVS1, IVS2
      CHARACTER*1 :: IJOBVS
!  .. Local Arrays ..
      LOGICAL, SAVE :: CTEST = .TRUE., ETEST = .TRUE.
!  .. Executable Statements ..
      IA1 = N; IA2 = N; IWI = N; IWR = N
      IVS1 = N; IVS2 = N; ISDIM = SDIM; IJOBVS = JOBVS
      
      I = INFO / 100; J = INFO - I*100
      SELECT CASE(I)
      CASE (1)
      IA2 = IA2-1
      CASE (2)
      IWR = IWR-1
      CASE(3)
      IWI = IWI - 1
      CASE (4)
      IVS1 = IVS1 - 1
      IJOBVS = 'V'
      CASE(:-1,5:)
      CALL UESTOP(SRNAMT)
      END SELECT
      SDIM = 0
      if (lsame(sense,'E')) then
        IF (LSAME (IJOBVS,'V')) THEN
          IF (LSAME(SORT, 'S')) THEN 
            CALL LA_GEESX( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
     &        VS(1: IVS1, 1: IVS2), SELECT, SDIM, RCONDE, INFO=INFO)
          ELSE 
            CALL LA_GEESX( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
     &        VS(1: IVS1, 1: IVS2), RCONDE=RCONDE, INFO=INFO)
          END IF
        ELSE
          IF (LSAME(SORT, 'S')) THEN 
            CALL LA_GEESX( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
     &        SELECT=SELECT, SDIM=SDIM, RCONDE=RCONDE, INFO=INFO)
          ELSE
            CALL LA_GEESX( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
     &        RCONDE=RCONDE, INFO=INFO)
          END IF
        END IF
      else if (lsame (sense,'V')) then
        IF (LSAME (IJOBVS,'V')) THEN
          IF (LSAME(SORT, 'S')) THEN
            CALL LA_GEESX( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
     &        VS(1: IVS1, 1: IVS2), SELECT, SDIM, RCONDV=RCONDV, &
     &        INFO=INFO)
          ELSE
            CALL LA_GEESX( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
     &        VS(1: IVS1, 1: IVS2), RCONDV=RCONDV, INFO=INFO)
          END IF
        ELSE
          IF (LSAME(SORT, 'S')) THEN
            CALL LA_GEESX( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
     &        SELECT=SELECT, SDIM=SDIM, RCONDV=RCONDV, INFO=INFO)
          ELSE
            CALL LA_GEESX( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
     &        RCONDV=RCONDV, INFO=INFO)
          END IF
        END IF
      else
        if (lsame (sense,'B')) then
          IF (LSAME (IJOBVS,'V')) THEN
            IF (LSAME(SORT, 'S')) THEN
              CALL LA_GEESX( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
&               VS(1: IVS1, 1: IVS2), SELECT, SDIM, RCONDE, RCONDV, &
&               INFO=INFO)
            ELSE
              CALL LA_GEESX( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
&               VS(1: IVS1, 1: IVS2), RCONDE=RCONDE, RCONDV=RCONDV, &
&               INFO=INFO)
            END IF
          ELSE
            IF (LSAME(SORT, 'S')) THEN
              CALL LA_GEESX( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
&               SELECT=SELECT, SDIM=SDIM, RCONDE=RCONDE, RCONDV=RCONDV, &
&               INFO=INFO)
            ELSE
              CALL LA_GEESX( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
&               RCONDE=RCONDE, RCONDV=RCONDV, INFO=INFO)
            END IF
          END IF
        else
          IF (LSAME (IJOBVS,'V')) THEN
            IF (LSAME(SORT, 'S')) THEN
              CALL LA_GEESX( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
&               VS(1: IVS1, 1: IVS2), SELECT, SDIM, INFO=INFO)
            ELSE
              CALL LA_GEESX( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
&               VS(1: IVS1, 1: IVS2), INFO=INFO)
            END IF
          ELSE
            IF (LSAME(SORT, 'S')) THEN
              CALL LA_GEESX( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
&               SELECT=SELECT, SDIM=SDIM, INFO=INFO)
            ELSE
              CALL LA_GEESX( A(1:IA1, 1: IA2), WR(1:IWR), WI(1:IWI), &
&               INFO=INFO)
            END IF
          END IF 
        end if
      endif
      CALL LA_AUX_AA01( I, CTEST, ETEST, SRNAMT )
END SUBROUTINE LA_TEST_DGEESX
