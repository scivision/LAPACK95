      PROGRAM LA_SSBGV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_SBGV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N, KD
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), B(:,:), W(:), Z(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_SBGV Example Program Results'
      N = 5; KD = 2;
      ALLOCATE( A(KD+1,N), B(KD+1,N), W(N), Z(N,N) )

      OPEN(UNIT=21,FILE='sbgvu.ma',STATUS='UNKNOWN')
      DO I=1,KD+1 
         DO J=1,N
            READ(21,*) A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix AB : '
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

      WRITE(*,*)'Matrix BB : '
      DO I=1,KD+1; 
         WRITE(*,"(5(I3,1X))") INT(B(I,:)); 
      ENDDO
  
      WRITE(*,*) 'CALL LA_SBGV( AB, BB, W )'
      CALL LA_SBGV(  A, B, W)
    
      WRITE(*,*) 'W on exit : '
      DO I=1,N
         WRITE(*,"(5(E14.6,1X))") W(I)
      ENDDO

      WRITE(*,*) 'BB on exit : '
      DO I=1,KD+1; 
         WRITE(*,"(5(E14.6,1X))") B(I,:); 
      ENDDO
    
      WRITE(*,*)
      WRITE(*,*) ' * EXAMPLE 2 * '

      OPEN(UNIT=21,FILE='sbgvl.ma',STATUS='UNKNOWN')
      DO I=1,KD+1 
         DO J=1,N
            READ(21,*) A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix AB : '
      DO I=1,KD+1; 
         WRITE(*,"(5(I3,1X))") INT(A(I,:)); 
      ENDDO
    

      OPEN(UNIT=21,FILE='sbgvl.mb',STATUS='UNKNOWN')
      DO I=1,KD+1 
         DO J=1,N
            READ(21,*) B(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix BB : '
      DO I=1,KD+1; 
         WRITE(*,"(5(I3,1X))") INT(B(I,:)); 
      ENDDO
   
      WRITE(*,*) "CALL LA_SBGV( AB, BB, W, 'L', Z, INFO )"
      CALL LA_SBGV( A, B, W, 'L', Z, INFO )
      
      WRITE(*,*)'W on exit : '
      DO I=1,N
         WRITE(*,"(5(E14.6,1X))") W(I)
      ENDDO

      WRITE(*,*) 'Z on exit : '
      DO I=1,N; 
         WRITE(*,"(5(E14.6,1X))") Z(I,:); 
      ENDDO
   
      WRITE(*,*) ' INFO = ', INFO

      END PROGRAM LA_SSBGV_EXAMPLE
