      PROGRAM LA_SGEESX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GEESX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N, SDIM
      REAL(WP) :: RCONDE, RCONDV
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), WR(:), WI(:), VS(:,:)
      INTERFACE
         FUNCTION SELECT(WR, WI)
           USE LA_PRECISION, ONLY: WP => SP
           REAL(WP), INTENT(IN) :: WR, WI
            LOGICAL :: SELECT
         END FUNCTION SELECT
      END INTERFACE
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_GEESX Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), VS(N,N), WR(N), WI(N) )

      OPEN(UNIT=21,FILE='gees.ma',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N 
            READ(21,*) A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      AA=A

      WRITE(*,*)'Matrix A:'
      DO I=1,N; 
         WRITE(*,"(5(I3,1X))") INT(A(I,:));
      ENDDO 

      WRITE(*,*) 'CALL LA_GEESX( A, WR, WI, SELECT=SELECT, SDIM=SDIM,', &
                 ' RCONDE=RCONDE, RCONDV=RCONDV )'
      CALL LA_GEESX(  A, WR, WI, SELECT=SELECT, SDIM=SDIM, RCONDE=RCONDE, RCONDV=RCONDV  )
     
      WRITE(*,*) 'A on exit : '
      DO I=1,N; 
         WRITE(*,"(5(F12.5,1X))") A(I,:); 
      ENDDO
   
      WRITE(*,*) 'WR on exit : '
      DO I=1,N; 
         WRITE(*,"(5(F12.5,1X))") WR(I); 
      ENDDO

      WRITE(*,*) 'WI on exit : '
      DO I=1,N;    
      WRITE(*,"(5(F12.5,1X))") WI(I)
      ENDDO
   
      WRITE(*,*)'SDIM = ',SDIM
      WRITE(*,*)'RCONDE = ',RCONDE
   
      WRITE(*,*)'RCONDV = ',RCONDV

      END PROGRAM LA_SGEESX_EXAMPLE
      LOGICAL FUNCTION SELECT( WR, WI)
         USE LA_PRECISION, ONLY: WP => SP
         REAL(WP), INTENT(IN) :: WR, WI
         INTRINSIC EPSILON, ABS
         IF( ( ABS(WR) + ABS(WI) ) < 20.0_WP )THEN
            SELECT = .TRUE.
         ELSE
            SELECT = .FALSE.
         END IF
      END FUNCTION SELECT
