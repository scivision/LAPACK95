      PROGRAM LA_CHPEV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_HPEV, LA_HPEVD
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, INFO, N
      REAL(WP), ALLOCATABLE :: W(:)
!  .. "Local Arrays" ..
      COMPLEX(WP), ALLOCATABLE :: A(:), AA(:), Z(:,:)
!  .. "Intrinsic Functions" ..
      INTRINSIC  AIMAG, INT
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_HPEV Example Program Results'
      N = 5
      ALLOCATE( A(N*(N+1)/2), AA(N*(N+1)/2), W(N), Z(N,N) )

      OPEN(UNIT=21,FILE='hpev.ma',STATUS='UNKNOWN')
      DO I=1,N*(N+1)/2
         READ(21,*) A(I)
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix A : '
      DO I=1,N*(N+1)/2
         WRITE(*,"((I3,1X,'+',1X,I3,'i',1X))") INT(A(I)),INT(AIMAG(A(I)))
      ENDDO
      
      WRITE(*,*) "CALL LA_HPEV( A, W) "
      CALL LA_HPEV( A, W )
      
      WRITE(*,*) 'W on exit : '
      DO I=1,N
      WRITE(*,"(5(E14.6))") W(I)
      ENDDO
      WRITE(*,*)
      WRITE(*,*) ' * EXAMPLE 2 * '
      
      OPEN(UNIT=21,FILE='hpev.mat',STATUS='UNKNOWN')
      DO I=1,N*(N+1)/2
         READ(21,*) A(I)
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix A : '
      DO I=1,N*(N+1)/2
         WRITE(*,"((I3,1X,'+',1X,I3,'i',1X))") INT(A(I)),INT(AIMAG(A(I)))
      ENDDO
   
      WRITE(*,*) "CALL LA_HPEVD( A, W, 'L', Z, INFO )"
      CALL LA_HPEVD( A, W, 'L', Z, INFO )
     
      WRITE(*,*)'Z on exit: '
      DO I=1,N; WRITE(*,*) Z(I,:); 
      ENDDO

      WRITE(*,*) ' INFO = ', INFO

      END PROGRAM LA_CHPEV_EXAMPLE
