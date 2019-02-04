      PROGRAM LA_SSYGV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_SYGV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:), W(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_SYEV Example Program Results'
      N = 5
      ALLOCATE( A(N,N), AA(N,N), B(N,N), BB(N,N), W(N) )

      OPEN(UNIT=21,FILE='sygv.ma',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N 
            READ(21,*) A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      AA=TRANSPOSE(A)

      WRITE(*,*)'Matrix A : '
      DO I=1,N; 
         WRITE(*,"(5(I3,1X))") INT(A(I,:)); 
      ENDDO
    
      OPEN(UNIT=21,FILE='sygv.mb',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,N 
            READ(21,*) B(I,J)
         ENDDO
      ENDDO
      CLOSE(21)
      
      BB=TRANSPOSE(B)
      
      WRITE(*,*)'Matrix B : '
      DO I=1,N; 
         WRITE(*,"(5(I3,1X))") INT(B(I,:)); 
      ENDDO
    
      WRITE(*,*) 'CALL LA_SYGV( A, B, W )'
      CALL LA_SYGV( A, B, W )
      
      WRITE(*,*)'Matrix B on exit:'
      DO I=1,N; 
         WRITE(*,"(5(E14.6,1X))") B(I,:); 
      ENDDO
      
      WRITE(*,*) 'W on exit : '
      DO I=1,N
         WRITE(*,"(5(E14.6,1X))") W(I)
      ENDDO
       
      WRITE(*,*)
      WRITE(*,*) ' * EXAMPLE 2 * ' 
      
      WRITE(*,*)'Matrix A : '
      DO I=1,N; 
         WRITE(*,"(5(I3,1X))") INT(AA(I,:)); 
      ENDDO
          
      WRITE(*,*)'Matrix B : '
      DO I=1,N; 
         WRITE(*,"(5(I3,1X))") INT(BB(I,:)); 
      ENDDO

      WRITE(*,*) "CALL LA_SYGV( A, B, W, '2', 'V', 'L', INFO )"
      CALL LA_SYGV( AA, BB, W, 2, 'V', 'L', INFO )
      
      WRITE(*,*)'Matrix A on exit:'
      DO I=1,N; 
         WRITE(*,"(5(E14.6,1X))") AA(I,:); 
      ENDDO
    
      WRITE(*,*)'Matrix B on exit:'
      DO I=1,N; 
         WRITE(*,"(5(E14.6,1X))") BB(I,:); 
      ENDDO
    
      WRITE(*,*) 'W on exit : '
      DO I=1,N
         WRITE(*,"(5(F11.5,1X))") W(I)
      ENDDO
      
      WRITE(*,*) ' INFO = ', INFO

      END PROGRAM LA_SSYGV_EXAMPLE
