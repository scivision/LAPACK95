      PROGRAM LA_SSBEV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_SBEV, LA_SBEVD
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N, KD
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), W(:), Z(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_SYEV Example Program Results'
      N = 5; KD = 2;
      ALLOCATE( A(KD+1,N), W(N), Z(N,N) )

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
    
      WRITE(*,*) 'CALL LA_SBEV( A, W )'
     
      CALL LA_SBEV(  A, W)
      
      WRITE(*,*) 'W on exit : '
      DO I=1,N
         WRITE(*,"(5(F9.5))") W(I)
      ENDDO

      OPEN(UNIT=21,FILE='sbevl.ma',STATUS='UNKNOWN')
      DO I=1,KD+1 
         DO J=1,N
            READ(21,*) A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      WRITE(*,*)
      WRITE(*,*) ' * EXAMPLE 2 * '
      
      WRITE(*,*)'Matrix A : '
      DO I=1,KD+1; 
         WRITE(*,"(5(I3,1X))") INT(A(I,:)); 
      ENDDO

      WRITE(*,*) "CALL LA_SBEVD( A, W, 'L', Z, INFO )"
      CALL LA_SBEVD( A, W, 'L', Z, INFO )
      
      WRITE(*,*) 'Z on exit : ' 
      DO I=1,N; 
         WRITE(*,"(5(E14.6,1X))") Z(I,:); 
      ENDDO
       
      WRITE(*,*) 'W on exit : '
      DO I=1,N
         WRITE(*,"(5(F9.5))") W(I)
      ENDDO
      
      WRITE(*,*) ' INFO = ', INFO

      END PROGRAM LA_SSBEV_EXAMPLE
