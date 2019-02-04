      PROGRAM LA_SSYSV_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_SYSV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N, NRHS
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE :: IPIV(:)
      REAL(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:)
!  .. "Intrinsic Functions" ..
      INTRINSIC  SUM
!  .. "Executable Statements" ..
      WRITE (*,*) 'SSYSV Example Program Results'
      N = 5; NRHS = 3
      WRITE(*,'(5H N = , I4, 9H; NRHS = , I4)') N, NRHS
      ALLOCATE ( A(N,N), AA(N,N), B(N,NRHS), BB(N,NRHS), IPIV(N) )
    
      OPEN(UNIT=21,FILE='sysv.ma',STATUS='UNKNOWN')
      DO I=1,N
         DO J=I,N
         READ(21,'(F3.0)') A(I,J);
         ENDDO
      ENDDO;
      CLOSE(21)

      WRITE(*,*) 'The array A :'
      DO I=1,N
      WRITE(*,'(10(I3,1X,1X),I3,1X)') INT(A(I,1:N)); 
      ENDDO
      AA=TRANSPOSE(A)
      DO I = 1, N 
        DO J = 1, NRHS
        B(I,J) = (SUM(A(I,I:N)) + SUM(A(1:I-1,I)))*J
        ENDDO 
      ENDDO
     
      WRITE(*,*) 'The array B :'
      DO I=1,N
            WRITE(*,'(3(I3,1X,1X))') INT(B(I,1:NRHS));
      ENDDO
      BB=B
      WRITE(*,*)' CALL LA_SYSV( A, B, IPIV=IPIV )'
      CALL LA_SYSV( A, B, IPIV=IPIV )
   
      WRITE(*,*)'A on exit: '
      DO J=1,N; WRITE(*,"(6(E13.5))") A(J,:); 
      ENDDO

      WRITE(*,*)'B on exit: '
      DO I=1,N; WRITE(*,"(3(F9.5))") B(I,:);
      ENDDO

      WRITE(*,*)'IPIV on exit : ', IPIV
    
      WRITE(*,*)' * EXAMPLE 2 * '

      WRITE(*,*) 'The array A :'
      DO I=1,N
      WRITE(*,'(10(I3,1X,1X),I3,1X)') INT(AA(I,1:N)); 
      ENDDO
      WRITE(*,*) 'The array B :'
      DO I=1,N
      WRITE(*,'(3(I3,1X,1X))') INT(BB(I,1));
      ENDDO
      
      WRITE(*,*)"CALL LA_SYSV( A, B(:,1), 'L', IPIV, INFO )"
      CALL LA_SYSV( AA, BB(:,1), 'L', IPIV, INFO )
   
      WRITE(*,*)'A on exit: '
      DO J=1,N; WRITE(*,"(6(E13.5))") AA(J,:); 
      ENDDO

      WRITE(*,*)'B(:,1) on exit: '
      DO I=1,N; WRITE(*,"(3(F9.5))") BB(I,1);
      ENDDO

      WRITE(*,*)' IPIV = ',IPIV
      WRITE(*,*)' INFO = ',INFO


!      DEALLOCATE (A, AA, B, BB, IPIV)
      print *,'after the dealloc'
      END PROGRAM LA_SSYSV_EXAMPLE


