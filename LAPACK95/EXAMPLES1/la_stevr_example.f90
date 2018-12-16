      PROGRAM LA_SSTEVR_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_STEVR
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, M, N
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: D(:), E(:), Z(:,:), W(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_STEVR Program Results'
      N = 5; 
      ALLOCATE( D(N), E(N), Z(N,N), W(N) )

      OPEN(UNIT=21,FILE='stevd.ma',STATUS='UNKNOWN')
      DO J=1,N
         READ(21,*) D(J) 
      ENDDO
      CLOSE(21)
      
      OPEN(UNIT=21,FILE='steve.ma',STATUS='UNKNOWN')
      DO J=1,N
         READ(21,*) E(J) 
      ENDDO
      CLOSE(21)

      WRITE(*,*)' Vector D : '
      DO I=1,N
      WRITE(*,"(5(I5,1X))") INT(D(I))
      ENDDO

      WRITE(*,*)' Vector E : '
      DO I=1,N
      WRITE(*,"(5(I5,1X))") INT(E(I))
      ENDDO

      WRITE(*,*) 'CALL LA_STEVR( D, E, W, Z, -5.0_WP, 5.0_WP, M=M )'
      CALL LA_STEVR( D, E, W, Z, -5.0_WP, 5.0_WP, M=M )
      
      WRITE(*,*) ' W on exit :'
      DO I=1,N
      WRITE(*,"(5(F12.5,1X))") W(I)
      ENDDO
   
      WRITE(*,*)'Z on exit:'
      DO I=1,N
         WRITE(*,'(5(F9.5))') Z(I,:)
      ENDDO

      WRITE(*,*)'M = ', M 

      END PROGRAM LA_SSTEVR_EXAMPLE
