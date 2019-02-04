      PROGRAM LA_SGEEVX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GEEVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N, ILO, IHI
      REAL(WP) :: ABNRM
      REAL(WP), ALLOCATABLE :: WR(:), WI(:)
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), VL(:,:), VR(:,:), SCALE(:)
      REAL(WP), ALLOCATABLE :: RCONDE(:), RCONDV(:)
!  .. "Intrinsic Functions" ..
      INTRINSIC REAL, INT
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_GEEVX Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), WR(N), WI(N), VL(N,N), VR(N,N), SCALE(N) )
      ALLOCATE( RCONDE(N), RCONDV(N) )
      
      OPEN(UNIT=21,FILE='geev1.ma',STATUS='UNKNOWN')
      DO I=1,N
         DO J=1,N
            READ(21,*) A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      AA=A

      WRITE(*,*)'Matrix A:'
      DO I=1,N
         WRITE(*,"(5(I3,1X))") INT(A(I,:))
      ENDDO

      WRITE(*,*) "CALL LA_GEEVX( A, WR, WI, 'B', ILO, IHI, SCALE,", &
                               " ABNRM, RCONDE, RCONDV )"  
!      CALL LA_GEEVX( A, WR, WI, VL, VR, 'B', ILO, IHI, SCALE, ABNRM, RCONDE, RCONDV )
!      CALL LA_GEEVX( A, WR, WI, BALANC='B', ILO=ILO, IHI=IHI, SCALE=SCALE, ABNRM=ABNRM,&
!&       RCONDE=RCONDE, RCONDV=RCONDV )
!        RCONDE=RCONDE, RCONDV=RCONDV )

       CALL LA_GEEVX( A, WR, WI, BALANC='B', ILO=ILO, IHI=IHI, &
            SCALE=SCALE, ABNRM=ABNRM, RCONDE=RCONDE, RCONDV=RCONDV )

      WRITE(*,*) "WR on exit : "
      DO I=1,N
         WRITE(*,"((E14.6,1X))") REAL(WR(I))
      ENDDO
   
      WRITE(*,*) "WI on exit : "
      DO I=1,N
         WRITE(*,"((E14.6,1X))") REAL(WI(I))
      ENDDO
   
      WRITE(*,*)'ILO : ',ILO
      WRITE(*,*)'IHI : ',IHI
      WRITE(*,*)'SCALE : ',SCALE
      WRITE(*,*)'ABNRM on exit :', ABNRM
      WRITE(*,*)'RCONDE on exit :', RCONDE
      WRITE(*,*)'RCONDV on exit :', RCONDV

      END PROGRAM LA_SGEEVX_EXAMPLE








