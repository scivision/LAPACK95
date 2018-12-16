      PROGRAM LA_CHPEVX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_HPEVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N
      REAL(WP), ALLOCATABLE :: W(:)  
!  .. "Local Arrays" ..
      COMPLEX(WP), ALLOCATABLE :: A(:), AA(:), Z(:,:)
!  .. "Intrinsic Functions" ..
      INTRINSIC AIMAG, INT
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_HPEVX Example Program Results'
      N = 5
      ALLOCATE( A(N*(N+1)/2), AA(N*(N+1)/2), W(N), Z(N,N) )

      OPEN(UNIT=21,FILE='hpev.ma',STATUS='UNKNOWN')
      DO J=1,N*(N+1)/2
         READ(21,*) A(J)
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix A:'
      DO I=1,N*(N+1)/2
         WRITE(*,"((I3,1X,'+',1X,I3,'i',1X))") INT(A(I)), INT(AIMAG(A(I)))
      ENDDO
   
      WRITE(*,*) "CALL LA_HPEVX( A, W, IL=2, IU=5, ABSTOL=1.0E-5_WP ) "
      CALL LA_HPEVX( A, W, IL=2, IU=5, ABSTOL=1.0E-5_WP )
      
      WRITE(*,*) 'W on exit : '
      DO I=1,N
      WRITE(*,"(5(F9.5))") W(I)
      ENDDO

      END PROGRAM LA_CHPEVX_EXAMPLE



