      PROGRAM LA_SHESV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_HESV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N, NRHS
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE :: IPIV(:)
      COMPLEX(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SHESV Example Program Results'
      N = 5; NRHS = 3
      WRITE(*,'(5H N = , I4, 9H; NRHS = , I4)') N, NRHS
      ALLOCATE ( A(N,N), AA(N,N), B(N,NRHS), BB(N,NRHS), IPIV(N) )
    
      OPEN(UNIT=21,FILE='hesv.ma',STATUS='UNKNOWN')
      DO I=1,N
         DO J=I,N
         READ(21,*) A(I,J);
         ENDDO
      ENDDO;
      CLOSE(21)
 
      WRITE(*,*) 'The matrix A :'
      DO I=1,N
      WRITE(*,"(5(I3,1X,'+',1X,I3,'i',1X))") INT(A(I,1)),INT(AIMAG(A(I,1))), & 
                                             INT(A(I,2)),INT(AIMAG(A(I,2))), &  
                                             INT(A(I,3)),INT(AIMAG(A(I,3))), & 
                                             INT(A(I,4)),INT(AIMAG(A(I,4))), & 
                                             INT(A(I,5)),INT(AIMAG(A(I,5))) 
      ENDDO
  
      OPEN(UNIT=21,FILE='hesv.mb',STATUS='UNKNOWN')
      DO I=1,N
         READ(21,*) B(I,1);
      ENDDO;
      CLOSE(21)
     
      WRITE(*,*) 'The array B :'
      DO I=1,N
      WRITE(*,"((I3,1X,'+',1X,I3,'i',1X))") INT(B(I,1)),INT(AIMAG(B(I,1))) 
      ENDDO
      
      BB=B
      WRITE(*,*)' CALL LA_HESV( A, B, IPIV=IPIV )'
      CALL LA_HESV( A, B, IPIV=IPIV )
   
      WRITE(*,*)'A on exit: '
      DO I=1,N; WRITE(*,*) A(I,:); 
      ENDDO

      WRITE(*,*)'B on exit: '
      DO I=1,N; WRITE(*,*) (B(I,1));
      ENDDO

      WRITE(*,*)'IPIV on exit : ', IPIV
    
      
      END PROGRAM LA_SHESV_EXAMPLE





