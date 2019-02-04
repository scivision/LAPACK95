      PROGRAM LA_SSYSVX_EXAMPLE

!  -- LAPACK95 EXAMPLE DRIVER ROUTINE (VERSION 1.0) --
!     UNI-C, DENMARK
!     DECEMBER, 1999
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => SP
      USE F95_LAPACK, ONLY: LA_SYSVX
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Parameters" ..
      REAL(WP), PARAMETER :: ONE = 1.0_WP, ZERO = 0.0_WP
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N, NRHS
      REAL(WP) :: RCOND
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE :: IPIV(:)
      REAL(WP), ALLOCATABLE :: A(:,:), B(:,:), X(:,:), FERR(:), BERR(:)
!  .. "Intrinsic Functions" ..
      INTRINSIC  SUM
!  .. "Executable Statements" ..
      WRITE (*,*) 'SSYSVX Example Program Results'
      N = 5; NRHS = 3
      WRITE(*,'(5H N = , I4, 9H; NRHS = , I4)') N, NRHS
      ALLOCATE ( A(N,N), B(N,NRHS), X(N,NRHS), IPIV(N), FERR(NRHS), BERR(NRHS) )
    
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

      DO J = 1, NRHS; 
         DO I = 1, N
            B(I,J) = (SUM(A(I,I:N)) + SUM(A(1:I,I)) - A(I,I))*J
         ENDDO;
      ENDDO
   
      WRITE(*,*) 'The array B :'
      DO I=1,N
            WRITE(*,'(3(I3,1X,1X))') INT(B(I,1:NRHS));
      ENDDO

      WRITE(*,*)'CALL LA_SYSVX(A, B, X, IPIV=IPIV, FERR=FERR, BERR=BERR, RCOND=RCOND, INFO=INFO)'
      CALL LA_SYSVX(A, B, X, IPIV=IPIV, FERR=FERR, BERR=BERR, RCOND=RCOND, INFO=INFO)
   
      WRITE(*,*) 'FERR on exit :'
      DO I = 1, NRHS; WRITE (*,'(E13.5)') FERR(I); 
      ENDDO
      WRITE(*,*)'BERR on exit :'
      DO I = 1, NRHS; WRITE (*,'(E13.5)') BERR(I); 
      ENDDO
      WRITE(*,*) 'RCOND =', RCOND
      WRITE(*,*)'INFO = ' ,INFO      

      END PROGRAM LA_SSYSVX_EXAMPLE
