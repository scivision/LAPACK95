      PROGRAM LA_SGELSY_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_GELSY
    !  USE F77_LAPACK, ONLY: GEESX_F77 => LA_GEESX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, M, N, NRHS, RANK
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE :: JPVT(:)
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'LA_GELSY Example Program Results'
      M=6; N = 4; NRHS = 3
      WRITE(*,'(5H M = , I4, 5H N = , I4, 9H; NRHS = , I4)') M, N, NRHS
      ALLOCATE( A(M,N), AA(M,N), B(MAX(M,N),NRHS), BB(MAX(M,N),NRHS), JPVT(N) )

      OPEN(UNIT=21,FILE='gelss.ma',STATUS='UNKNOWN')
      DO I=1,M
         DO J=1,N 
            READ(21,'(F2.0)') A(I,J)
         ENDDO
      ENDDO
      CLOSE(21)

      WRITE(*,*)'Matrix A :'
      DO I=1,M;
         WRITE(*,"(24(F9.5))") A(I,:);
      ENDDO

      OPEN(UNIT=22,FILE='gelss.mb',STATUS='UNKNOWN')
      DO I=1,MAX(M,N)
         DO J=1,NRHS
            READ(22,'(F3.0)') B(I,J)
         ENDDO
      ENDDO
      CLOSE(22)
      
      WRITE(*,*)'Matrix B :'
      DO I=1,M; 
         WRITE(*,"(3(F9.5))") B(I,:); 
      ENDDO
      
      AA=A
      BB=B
    
      JPVT(1)=0; JPVT(2)=0; JPVT(3)=1; JPVT(4)=0;

      WRITE(*,*) 'CALL LA_GELSY( A, B, RANK, JPVT, 1.0E-5_WP )'
      
      CALL LA_GELSY( A, B, RANK, JPVT, 1.0E-5_WP )
      
      WRITE(*,*) 'The matrix B on exit :'
      DO I=1,M; 
         WRITE(*,"(3(E15.7),1X)") B(I,:); 
      ENDDO
          
    
      WRITE(*,*)'RANK = ', RANK
      WRITE(*,*)'JPVT = ', JPVT
    
      END PROGRAM LA_SGELSY_EXAMPLE
