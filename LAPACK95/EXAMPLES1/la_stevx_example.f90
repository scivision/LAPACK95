      PROGRAM LA_SSTEVX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_STEVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N, IL, IU
      REAL(WP) :: ABSTOL
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: D(:), E(:), Z(:,:), W(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_STEV Example Program Results'
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

      WRITE(*,*) 'CALL LA_STEVX( D, E, W, IL=1, IU=3, ABSTOL=1E-2 )'
      IL=1; IU=3; ABSTOL=1E-2
      CALL LA_STEVX( D, E, W, IL=IL, IU=IU, ABSTOL=ABSTOL )
  
      WRITE(*,*) ' W on exit : '
      DO I=1,N
      WRITE(*,"(5(F12.5,1X))") W(I)
      ENDDO

      END PROGRAM LA_SSTEVX_EXAMPLE

