      PROGRAM LA_SPPSV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements"
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_PPSV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N, NN, NRHS
!  .. Local Arrays ..
      REAL(WP), ALLOCATABLE :: AP(:) ,B(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SPPSV Example Program Results.'
      N = 5; NRHS = 1
      NN = N*(N+1)/2
      ALLOCATE ( AP(NN), B(N,NRHS) )
 
      OPEN(UNIT=21,FILE='posv.ma',STATUS='UNKNOWN')
      DO I=1,NN
         READ(21,'(F3.0)') AP(I);
      ENDDO;
      CLOSE(21)
  
      WRITE(*,*) 'The array AP :'
      DO I=1,NN
      WRITE(*,'(10(I3,1X,1X),I3,1X)') INT(AP(I)); 
      ENDDO

      OPEN(UNIT=21,FILE='ppsv.mb',STATUS='UNKNOWN')
      DO I=1,N
      DO J=1,NRHS
         READ(21,'(F3.0)') B(I,J);
      ENDDO;
      ENDDO;
      CLOSE(21)
  
      WRITE(*,*) 'The array B :'
      DO I=1,N
       WRITE(*,'(10(I3,1X,1X),I3,1X)') INT(B(I,:)); 
      ENDDO;

      WRITE(*,*) "CALL LA_PPSV( AP, B, 'L' )"
      CALL LA_PPSV(  AP, B, 'L' )
      
      WRITE(*,*)'AP on exit :'
      DO I=1,NN; WRITE(*,"(5(E13.6))") AP(I); 
      ENDDO
     
      WRITE(*,*)'B on exit :'
      DO I=1,N; WRITE(*,"(5(E13.6))") B(I,:); 
      ENDDO
 
      END PROGRAM LA_SPPSV_EXAMPLE




