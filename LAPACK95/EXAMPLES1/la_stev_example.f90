      PROGRAM LA_SSTEV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_STEV, LA_STEVD
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: D(:), E(:), Z(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_STEV Example Program Results'
      N = 5; 
      ALLOCATE( D(N), E(N), Z(N,N) )

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

      WRITE(*,*) 'CALL LA_STEV( D, E )'
      CALL LA_STEV( D, E )
      
      WRITE(*,*) ' D on exit : '
      DO I=1,N
      WRITE(*,"(5(F12.5,1X))") D(I)
      ENDDO
      WRITE(*,*)
      WRITE(*,*) ' * EXAMPLE 2 * '  
      
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

      WRITE(*,*) "CALL LA_STEVD( D, E, Z, INFO )"
      CALL LA_STEVD( D, E, Z, INFO )
      
      WRITE(*,*)' Z on exit : '
      DO I=1,N; 
         WRITE(*,"(5(F12.6,1X))") Z(I,:); 
      ENDDO
      
      WRITE(*,*)' D on exit : ' 
      DO I=1,N
      WRITE(*,"(5(F12.5,1X))") D(I)
      ENDDO
      
      WRITE(*,*) ' INFO = ', INFO

      END PROGRAM LA_SSTEV_EXAMPLE




