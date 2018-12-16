      PROGRAM LA_SSBEVX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_SBEVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, M, N, KD
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), W(:), Z(:,:), Q(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_SBEVX Example Program Results'
      N = 5; KD = 2;
      ALLOCATE( A(KD+1,N), W(N), Z(N,N), Q(N,N) )
   
      OPEN(UNIT=21,FILE='sbevu.ma',STATUS='UNKNOWN')
      DO I=1,KD+1 
         DO J=1,N
            READ(21,*) A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix A:'
      DO I=1,KD+1; 
         WRITE(*,"(5(I3,1X))") INT(A(I,:)); 
      ENDDO
    
      WRITE(*,*) 'CALL LA_SBEVX( A, W, Z=Z, VL=-4.0_WP, VU=100.0_WP, M=M, Q=Q )'
      CALL LA_SBEVX( A, W, Z=Z, VL=-4.0_WP, VU=100.0_WP, M=M, Q=Q)
      
      WRITE(*,*) 'W on exit : '
      DO I=1,N
      WRITE(*,"(5(F9.5))") W(I)
      ENDDO
      WRITE(*,*) 'M = ',M
      
      WRITE(*,*) 'The matrix Q'
      WRITE(*,'(5(F9.5))') ((Q(I,J),J=1,N),I=1,N)
    
      END PROGRAM LA_SSBEVX_EXAMPLE


