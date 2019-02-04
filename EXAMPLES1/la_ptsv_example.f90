      PROGRAM LA_SPTSV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_PTSV
!  .. "IMPLICIT STATEMENT" ..
      IMPLICIT NONE
!  .. "Parameters" ..
      REAL(WP), PARAMETER :: ONE = 1.0_WP, THREE = 3.0_WP
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N, NRHS
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:), D(:), E(:), B(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SPTSV Example Program Results.'
      N = 5; NRHS = 3
      WRITE(*,'(5H N = , I4, 9H; NRHS = , I4)') N, NRHS  
      ALLOCATE (A(N), D(N), E(N-1), B(N,NRHS) )
   
      OPEN(UNIT=21,FILE='ptsv.ma',STATUS='UNKNOWN')
      DO I=1,N
         READ(21,'(F3.0)') A(I);
      ENDDO;
      CLOSE(21)
  

      E = THREE; D(1) = 2*E(1)+ONE; D(2:N) = 2*E+ONE
      B(1,1) = D(1)+E(1); B(N,1) = E(N-1)+D(N)
      B(2:N-1,1) = 2*E + D(2:N-1)
      DO I = 2, NRHS; B(:,I) = B(:,1)*I; 
      ENDDO
      WRITE(*,*) '  on entry:'
      WRITE (*,'(2HD:,8(1X,F9.2))') D
      WRITE (*,'(2HE:,8(1X,F9.2))') E
      WRITE(*,*) 'The RHS matrix B:'
      DO J = 1, N; WRITE (*,*) B(J,:); 
      ENDDO
    
      WRITE(*,*)' CALL LA_PTSV( D, E, B, INFO )'
      CALL LA_PTSV( D, E, B, INFO )

      WRITE(*,*) 'Vector D on exit:'
      DO I = 1, N; WRITE (*,'(F8.5)') D(I); 
      ENDDO

      WRITE(*,*) 'Vector E on exit:'
      DO I = 1, N-1; WRITE (*,'(E15.5)') E(I); 
      ENDDO


      WRITE(*,*) 'The matrix B on exit:'
      DO J = 1, N; WRITE (*,*) B(J,:); 
      ENDDO
    
      WRITE(*,*)'INFO = ',INFO 
      
      END PROGRAM LA_SPTSV_EXAMPLE






