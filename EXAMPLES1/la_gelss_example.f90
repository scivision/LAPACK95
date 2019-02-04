      PROGRAM LA_SGELSS_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GELSS, LA_LANGE
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" .
      REAL(WP) ::  R1, R2, R3
      INTEGER :: RANK, I, J, INFO, M, N, NRHS
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:), S(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_GELSS Example Program Results'
      M=6; N = 4; NRHS = 3
      WRITE(*,'(5H N = , I4, 9H; NRHS = , I4)') N, NRHS
      ALLOCATE( A(M,N), AA(M,N), B(M,NRHS), BB(M,NRHS), S(N) )

      OPEN(UNIT=21,FILE='gelss.ma',STATUS='UNKNOWN')
      DO I=1,M 
         DO J=1,N
            READ(21,*) A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      OPEN(UNIT=22,FILE='gelss.mb',STATUS='UNKNOWN')
      DO I=1,M 
         DO J=1,NRHS
            READ(22,*) B(I,J)
         ENDDO
      ENDDO
      CLOSE(22)
      
      WRITE(*,*)'Matrix A :'
      DO I=1,M;
         WRITE(*,"(24(F9.5))") A(I,:);
      ENDDO
      
      WRITE(*,*)'Matrix B :'
      DO I=1,M; 
         WRITE(*,"(3(F9.5))") B(I,:); 
      ENDDO
     
      WRITE(*,*) 'CALL LA_GELSS( A, B, RANK, S, 0.00001_WP, INFO )'
      CALL LA_GELSS( A, B, RANK, S, 0.00001_WP, INFO=INFO )
      
      WRITE(*,*) ' A on exit : '
      DO I=1,M; 
         WRITE(*,"(4(E14.6))") A(I,:); 
      ENDDO
    
      WRITE(*,*) ' B on exit : '
      DO I=1,M; 
         WRITE(*,"(3(E14.6))") B(I,:); 
      ENDDO
    
      WRITE(*,*) ' S on exit : '
      DO I=1,N; 
         WRITE(*,"(E14.6)") S(I); 
      ENDDO
     
      WRITE(*,*) 'RANK = ', RANK
      WRITE(*,*)'INFO = ',INFO
       
      R1=LA_LANGE(B(N+1:M,1),NORM='F')
      R2=LA_LANGE(B(N+1:M,2),NORM='F')
      R3=LA_LANGE(B(N+1:M,3),NORM='F')
      WRITE(*,*)'The residual r = ',R1,R2,R3
     
      END PROGRAM LA_SGELSS_EXAMPLE
