      PROGRAM LA_SGELS_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GELS
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, M, N, NRHS
!  .. "Local Arrays" ..
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_GELS Example Program Results'
      M=6; N = 4; NRHS = 3
      WRITE(*,'(5H N = , I4, 9H; NRHS = , I4)') N, NRHS
      ALLOCATE( A(M,N), AA(M,N), B(M,NRHS), BB(M,NRHS) )

      OPEN(UNIT=21,FILE='gels.ma',STATUS='UNKNOWN')
      DO J=1,N
         DO I=1,M 
            READ(21,'(F2.0)') A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix A :'
      DO I=1,M;
         WRITE(*,"(24(F9.5))") A(I,:);
      ENDDO
      
      AA=A
      
      OPEN(UNIT=22,FILE='gels.mb',STATUS='UNKNOWN')
      DO J=1,NRHS
         DO I=1,M 
            READ(22,'(F3.0)') B(I,J)
         ENDDO
      ENDDO
      CLOSE(22)
      
      BB=B
      WRITE(*,*)'Matrix B :'
      DO I=1,M; 
         WRITE(*,"(3(F9.5))") B(I,:); 
      ENDDO
         
      WRITE(*,*) 'CALL LA_GELS( A, B )'
      CALL LA_GELS(  A, B )
      
      WRITE(*,*) 'The matrix B on exit :'
      DO I=1,M; 
         WRITE(*,"(3(E13.6))") B(I,:); 
      ENDDO
      WRITE(*,*)
      WRITE(*,*)' * EXAMPLE 2 * '
         
      WRITE(*,*)'Matrix A :'
      DO I=1,M;
         WRITE(*,"(24(F9.5))") AA(I,:);
      ENDDO       

      WRITE(*,*)'Matrix B(:,1) :'
      WRITE(*,"(F9.5)") BB(:,1); 
    
      WRITE(*,*) "CALL LA_GELS( A, B(:,1), 'T', INFO )"
      CALL LA_GELS( A, B(:,1), 'T', INFO )

      WRITE(*,*) 'The matrix B on exit :' 
      WRITE(*,"(E13.6)") B(:,1); 
     
      WRITE(*,*) 'INFO = ', INFO

      END PROGRAM LA_SGELS_EXAMPLE
