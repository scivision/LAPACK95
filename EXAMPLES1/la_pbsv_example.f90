      PROGRAM LA_SPBSV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_PBSV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Parameters" ..
      REAL(WP), PARAMETER :: ZERO = 0.0_WP, ONE = 1.0_WP
!  .. "Local Scalars" ..
      INTEGER :: KD, I, J, INFO, N, NRHS
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: AB(:,:), B(:,:)
      REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SPBSV Example Program Results.'
      N = 7; KD = 3; NRHS = 3
      WRITE(*,'(5H N = , I4, 9H; NRHS = , I4)') N, NRHS
      ALLOCATE ( AB(KD+1,N), AA(KD+1,N), B(N,NRHS), BB(N,NRHS) )
    
      OPEN(UNIT=21,FILE='pbsv.ma',STATUS='UNKNOWN')
      DO I=1,KD+1
         DO J=1,N
         READ(21,'(F3.0)') AB(I,J);
         ENDDO
      ENDDO
      CLOSE(21)

      B(:,1) = ZERO
      DO I = 1, N
         DO J = MAX(1,-N+I+KD+1), KD
         B(I,1) = AB(J,I-J+KD+1) + B(I,1)
         ENDDO
         DO J = MAX(1,KD+2-I), KD+1
         B(I,1) = AB(J,I) + B(I,1)
         ENDDO
      ENDDO
      
      DO J = 2, NRHS; B(:,J) = B(:,1)*J; ENDDO
      AA = AB; BB = B
       
      WRITE(*,*) 'AB on entry:'
      DO I = 1,KD+1;         
         WRITE (*,'(7(F8.5))') AB(I,:);
      ENDDO
       
      WRITE(*,*) 'The RHS matrix B:'
      DO J = 1, N; WRITE (*,*) B(J,:); 
      ENDDO

      CALL LA_PBSV(AB, B, INFO=INFO)
  
      WRITE(*,*)'AB on exit :'
      DO I=1,KD+1; WRITE(*,"(10(E12.6))") AB(I,:); 
      ENDDO     

      WRITE(*,*)'B on exit :'
      DO J=1,N; WRITE(*,"(10(F8.5))") B(J,:); 
      ENDDO

      WRITE(*,*)'INFO = ' ,INFO

      END PROGRAM LA_SPBSV_EXAMPLE




