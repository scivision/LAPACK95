      PROGRAM LA_SGGGLM_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GGGLM
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" .
      INTEGER :: I, J, INFO, M, N, P
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), B(:,:), D(:), X(:), Y(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_GGGLM Example Program Results'
      M=5; N = 4; P = 2
      ALLOCATE( A(M,N), B(M,P), D(M), X(N), Y(P) )

      OPEN(UNIT=21,FILE='ggglm.ma',STATUS='UNKNOWN')
      DO I=1,M 
         DO J=1,N
            READ(21,*) A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)
    
      OPEN(UNIT=22,FILE='ggglm.mb',STATUS='UNKNOWN')
      DO I=1,M 
         DO J=1,P
            READ(22,*) B(I,J)
         ENDDO
      ENDDO
      CLOSE(22)

      OPEN(UNIT=24,FILE='ggglm.d',STATUS='UNKNOWN')
      DO I=1,M 
         READ(24,*) D(I)
      ENDDO
      CLOSE(24)
     
      WRITE(*,*)'Matrix A :'
      DO I=1,M;
         WRITE(*,"(4(F9.5))") A(I,:);
      ENDDO   
      WRITE(*,*)'Matrix B :'
      DO I=1,M; 
         WRITE(*,"(2(F9.5))") B(I,:); 
      ENDDO
      WRITE(*,*)'Vector D :'
      DO I=1,M; 
         WRITE(*,"(F9.5))") D(I); 
      ENDDO
    
      WRITE(*,*) 'CALL LA_GGGLM( A, B, D, X, Y, INFO )'
      CALL LA_GGGLM( A, B, D, X, Y, INFO )
      WRITE(*,*) 'X on exit:'
      DO I=1,N; WRITE(*,"(E14.6)") X(I); ENDDO
   
      WRITE(*,*) 'Y on exit:'
      DO I=1,P; WRITE(*,"(E14.6)") Y(I); ENDDO
    
      WRITE(*,*)'INFO = ',INFO
      
      END PROGRAM LA_SGGGLM_EXAMPLE
