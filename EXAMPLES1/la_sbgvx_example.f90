      PROGRAM LA_SSBGVX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_SBGVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, M, N, KD
      REAL(WP) :: VL, VU
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), B(:,:), W(:), Z(:,:), Q(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_SBGVX Example Program Results'
      N = 5; KD = 2;
      ALLOCATE( A(KD+1,N), B(KD+1,N), W(N), Z(N,N), Q(N,N) )

      OPEN(UNIT=21,FILE='sbgvu.ma',STATUS='UNKNOWN')
      DO I=1,KD+1 
         DO J=1,N
            READ(21,*) A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix A : '
      DO I=1,KD+1; 
         WRITE(*,"(5(I3,1X))") INT(A(I,:)); 
      ENDDO
  
      OPEN(UNIT=21,FILE='sbgvu.mb',STATUS='UNKNOWN')
      DO I=1,KD+1 
         DO J=1,N
            READ(21,*) B(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix B : '
      DO I=1,KD+1; 
         WRITE(*,"(5(I3,1X))") INT(B(I,:)); 
      ENDDO
  
      WRITE(*,*)
      WRITE(*,*) 'CALL LA_SBGVX( A, B, W, Z=Z, VL=0.0_WP, VU=100.0_WP, M=M, Q=Q )'
      VL=0; VU=100
      CALL LA_SBGVX(  A, B, W, Z=Z, VL=0.0_WP, VU=100.0_WP, M=M, Q=Q )
  
      WRITE(*,*) ' W on exit : '
      DO I=1,N
      WRITE(*,"(5(E14.6,1X))") W(I)
      ENDDO

      WRITE(*,*) ' M = ',M
    
      WRITE(*,*) ' Matrix Q on exit : '
      DO I=1,N 
         WRITE(*,'(5(E14.6,1X))') Q(I,:)
      ENDDO


      END PROGRAM LA_SSBGVX_EXAMPLE
