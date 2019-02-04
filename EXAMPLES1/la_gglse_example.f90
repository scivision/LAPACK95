      PROGRAM LA_SGGLSE_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GGLSE, LA_LANGE
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" .
      REAL(WP) ::  R1
      INTEGER :: I, J, INFO, M, N, P
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), B(:,:), C(:), D(:), X(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_GGLSE Example Program Results'
      M=5; N = 3; P = 2
      ALLOCATE( A(M,N), B(P,N), C(M), D(P), X(N) )

      OPEN(UNIT=21,FILE='gglse.ma',STATUS='UNKNOWN')
      DO I=1,M 
         DO J=1,N
            READ(21,*) A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)
    
      OPEN(UNIT=22,FILE='gglse.mb',STATUS='UNKNOWN')
      DO I=1,P 
         DO J=1,N
            READ(22,*) B(I,J)
         ENDDO
      ENDDO
      CLOSE(22)

      OPEN(UNIT=23,FILE='gglse.mc',STATUS='UNKNOWN')
      DO I=1,M 
         READ(23,*) C(I)
      ENDDO
      CLOSE(23)

      OPEN(UNIT=24,FILE='gglse.md',STATUS='UNKNOWN')
      DO I=1,P 
         READ(24,*) D(I)
      ENDDO
      CLOSE(24)
      
      WRITE(*,*)'Matrix A :'
      DO I=1,M;
         WRITE(*,"(3(F9.5))") A(I,:);
      ENDDO          
      
      WRITE(*,*)'Matrix B : '
      DO I=1,P; 
         WRITE(*,"(3(F9.5))") B(I,:); 
      ENDDO
      
      WRITE(*,*)'Vector C : '
      DO I=1,M; 
         WRITE(*,"(F9.5)") C(I); 
      ENDDO
      
      WRITE(*,*) 'Vector D : '
      DO I=1,P; 
         WRITE(*,"(F9.5))") D(I); 
      ENDDO
      WRITE(*,*)
      WRITE(*,*) 'CALL LA_GGLSE( A, B, C, D, X, INFO )'
      CALL LA_GGLSE( A, B, C, D, X, INFO )
   
      WRITE(*,*) 'C on exit : '
      DO I=1,M; 
         WRITE(*,"(E14.6)") C(I); 
      ENDDO
    
      WRITE(*,*) 'X on exit : '
      DO I=1,N; 
         WRITE(*,"(E14.5)") X(I); 
      ENDDO
       
      WRITE(*,*)'INFO = ',INFO
       
      R1=LA_LANGE(C(N-P+1:M),'F')
      WRITE(*,*)'The residual r = ',R1
     
      END PROGRAM LA_SGGLSE_EXAMPLE
