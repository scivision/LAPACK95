      PROGRAM LA_SPPSVX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements"
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_PPSVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Local Scalars" ..
      INTEGER :: I, J, N, NN, NRHS
      REAL(WP) :: RCOND
!  .. Local Arrays ..
      REAL(WP), ALLOCATABLE :: A(:), B(:,:),X(:,:), FERR(:), BERR(:)
!  .. "Executable Statements" ..
      WRITE (*,*) 'SPPSVX Example Program Results.'
      N = 5; NRHS = 1
      NN = N*(N+1)/2
      ALLOCATE ( A(NN), B(N,NRHS), X(N,NRHS),FERR(NRHS), BERR(NRHS) )
 
      OPEN(UNIT=21,FILE='posv.ma',STATUS='UNKNOWN')
      DO I=1,NN
         READ(21,'(F3.0)') A(I);
      ENDDO;
      CLOSE(21)
  
      WRITE(*,*) 'The array A :'
      DO I=1,NN
      WRITE(*,'(10(I3,1X,1X),I3,1X)') INT(A(I)); 
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

      WRITE(*,*) 'CALL LA_PPSVX( A, B, X, "L", FERR=FERR, BERR=BERR, RCOND=RCOND )'
      CALL LA_PPSVX(  A, B, X, 'L', FERR=FERR, BERR=BERR, RCOND=RCOND )

      WRITE(*,*)'X on exit :'
      DO I=1,N; WRITE(*,"(5(E13.6))") X(I,:); 
      ENDDO
      WRITE(*,*)'FERR on exit :'
      DO I=1,NRHS; WRITE(*,"(5(E13.6))") FERR(I); 
      ENDDO
      WRITE(*,*)'BERR = '
      DO I=1,NRHS; WRITE(*,"(5(E13.6))") BERR(I); 
      ENDDO
      WRITE(*,*)'RCOND = ', RCOND

      WRITE(*,*) '\noindent'
      WRITE(*,*) 'The solution of the system $ A\,X = B $ is:'
      WRITE(*,*) '$$ X = \left( \begin{array}{rrr}'
      DO I=1,N; WRITE(*,"(2(F9.5,' & '),F9.5,' \\')") X(I,:); ENDDO
      WRITE(*,*) '\end{array} \right). $$'
   
      END PROGRAM LA_SPPSVX_EXAMPLE
