PROGRAM LA_CPOSVX_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_POSVX
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, J, INFO, N, NRHS
   REAL(WP) :: RCOND
!  .. LOCAL ARRAYS ..
   INTEGER, ALLOCATABLE :: IPIV(:)
   COMPLEX(WP), ALLOCATABLE :: A(:,:), B(:,:), X(:,:)
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'CPOSVX ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N, NRHS
   PRINT *, 'N = ', N, ' NRHS = ', NRHS
   ALLOCATE ( A(N,N), AA(N,N), B(N,NRHS), BB(N,NRHS), X(N,NRHS), IPIV(N) )
!
      AA = HUGE(1.0_WP)
      DO I = 1, N
        READ (NIN, *) AA(I, I:N)
      ENDDO
      DO J = 1, NRHS
         DO I = 1, N
            BB(I,J) = ( SUM( AA(I,I:N) ) + SUM( AA(1:I-1,I) ) )*J
         ENDDO
      ENDDO
   A=AA; B=BB
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, N
        WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,:)
      ENDDO
      WRITE(NOUT,*) 'The RHS matrix B:'
      DO J = 1, NRHS
        WRITE (NOUT,*) 'RHS', J; WRITE (NOUT,FMT) B(:,J)
      ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_CPOSVX LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_POSVX( A, B, X, INFO=INFO )'
   A=AA; B=BB
   CALL LA_POSVX( A, B, X, INFO=INFO )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_POSVX, INFO = ', INFO
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_POSVX( A, B(1:N,1), X(1:N,1), RCOND=RCOND, INFO=INFO )'
   A=AA; B=BB
   CALL LA_POSVX( A, B(1:N,1), X(1:N,1), RCOND=RCOND, INFO=INFO )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_POSVX, INFO = ', INFO
   WRITE (NOUT,FMT) X(:,1)
   WRITE(NOUT,*) 'RCOND = ', RCOND
! 
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_POSVX:'
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_POSVX( A, B, X )'
   A=AA; B=BB
   CALL LA_POSVX( A, B, X )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_POSVX:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_POSVX(A, B(1:N,1), X(1:N,1) )'
   A=AA; B=BB
   CALL LA_POSVX(A, B(1:N,1), X(1:N,1) )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_POSVX:'
   WRITE (NOUT,FMT) X(:,1)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_POSVX(A, B, X, ''L'', RCOND=RCOND)'
   A=TRANSPOSE(AA); B=BB
   CALL LA_POSVX( A, B, X, 'L', RCOND=RCOND)
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_POSVX:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*) 'RCOND = ', RCOND
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_POSVX( A, B(1:N,1), X(1:N,1), ''L'', RCOND=RCOND )'
   A=TRANSPOSE(AA); B=BB
   CALL LA_POSVX( A, B(1:N,1), X(1:N,1), 'L', RCOND=RCOND )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_POSVX:'
   WRITE (NOUT,FMT) X(1:N,1)
   WRITE(NOUT,*) 'RCOND = ', RCOND
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_POSVX(A(:,:), B(1:N-1,:), X, INFO =INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_POSVX( A(:,:), B(1:N-1,:), X, INFO=INFO)
   WRITE(NOUT,*)'  INFO = ', INFO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_POSVX( A(4:4), B(1:N,1), X(1:N,1), INFO=INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_POSVX( A(4:4,:), B(1:N,1), X(1:N,1), INFO=INFO )
   WRITE(NOUT,*)'   B - the RHS vector.'
   WRITE (NOUT,FMT) B(1:N,1)
   WRITE(NOUT,*)'  INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_POSVX(A, B, X, FACT=''F'', INFO =INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_POSVX( A, B, X, FACT='F', INFO=INFO)
   WRITE(NOUT,*)'    INFO = ', INFO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_POSVX( A, B(1:N,1), X(1:N,1), FACT=''4'', INFO=INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_POSVX( A, B(1:N,1), X(1:N,1), FACT='4', INFO=INFO )
   WRITE(NOUT,*)'   INFO = ', INFO
!
END PROGRAM LA_CPOSVX_ET_EXAMPLE
