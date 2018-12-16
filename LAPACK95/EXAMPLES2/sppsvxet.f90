PROGRAM LA_SPPSVX_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_PPSVX
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   CHARACTER(LEN=1) :: EQU
   INTEGER :: I, J, K, INFO, N, NRHS, NS
   REAL(WP) :: RCOND
!  .. LOCAL ARRAYS ..
   INTEGER, ALLOCATABLE :: IPIV(:)
   REAL(WP), ALLOCATABLE :: A(:), B(:,:), X(:,:), AF(:)
   REAL(WP), ALLOCATABLE :: AA(:), BB(:,:), S(:), FERR(:), BERR(:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'SPPSVX ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N, NRHS
   PRINT *, 'N = ', N, ' NRHS = ', NRHS
   NS = N*(N+1)/2
   ALLOCATE ( A(NS), AA(NS), B(N,NRHS), BB(N,NRHS), X(N,NRHS), IPIV(N), &
              AF(NS), S(N), FERR(NRHS), BERR(NRHS) )
!
      AA = HUGE(1.0_WP)
      READ (NIN, *) AA
      DO K = 1, NRHS
         DO I = 1, N
            DO J = 1, I
               BB(J,K) = BB(J,K) + AA(J+(I-1)*I/2)
               IF ( J /= I ) BB(I,K) = BB(I,K) + AA(J+(I-1)*I/2)
            ENDDO
         ENDDO
         BB(:,K) = BB(:,K)*K
      ENDDO
   A=AA; B=BB
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, N
        WRITE (NOUT,*) 'J = ', I; WRITE (NOUT,FMT) (A(J+(I-1)*I/2),J=1,I)
      ENDDO
      WRITE(NOUT,*) 'The RHS matrix B:'
      DO J = 1, NRHS
        WRITE (NOUT,*) 'RHS', J; WRITE (NOUT,FMT) B(:,J)
      ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_SPPSVX LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSVX( A, B, X, INFO=INFO )'
   A=AA; B=BB
   CALL LA_PPSVX( A, B, X, INFO=INFO )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PPSVX, INFO = ', INFO
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSVX( A, B(1:N,1), X(1:N,1), RCOND=RCOND, INFO=INFO )'
   A=AA; B=BB
   CALL LA_PPSVX( A, B(1:N,1), X(1:N,1), RCOND=RCOND, INFO=INFO )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PPSVX, INFO = ', INFO
   WRITE (NOUT,FMT) X(:,1)
   WRITE(NOUT,*) 'RCOND = ', RCOND
! 
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PPSVX:'
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSVX( A, B, X )'
   A=AA; B=BB
   CALL LA_PPSVX( A, B, X )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PPSVX:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSVX(A, B(1:N,1), X(1:N,1) )'
   A=AA; B=BB
   CALL LA_PPSVX(A, B(1:N,1), X(1:N,1) )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PPSVX:'
   WRITE (NOUT,FMT) X(:,1)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSVX(A, B, X, ''L'', RCOND=RCOND)'
!  A=TRANSPOSE(AA)
   DO I = 1,N
      DO J = I, N
         A(J+(I-1)*(2*N-I)/2) = AA(I+J*(J-1)/2)
      ENDDO
   ENDDO
   B=BB
   CALL LA_PPSVX( A, B, X, 'L', RCOND=RCOND)
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PPSVX:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*) 'RCOND = ', RCOND
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSVX( A, B(1:N,1), X(1:N,1), ''L'', RCOND=RCOND )'
!  A=TRANSPOSE(AA)
   DO I = 1,N
      DO J = I, N
         A(J+(I-1)*(2*N-I)/2) = AA(I+J*(J-1)/2)
      ENDDO
   ENDDO
   B=BB
   CALL LA_PPSVX( A, B(1:N,1), X(1:N,1), 'L', RCOND=RCOND )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PPSVX:'
   WRITE (NOUT,FMT) X(1:N,1)
   WRITE(NOUT,*) 'RCOND = ', RCOND
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSVX(A(:,:), B(1:N-1,:), X, INFO =INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_PPSVX( A(:), B(1:N-1,:), X, INFO=INFO)
   WRITE(NOUT,*)'   INFO = ', INFO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSVX( A(4:4), B(1:N,1), X(1:N,1), INFO=INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_PPSVX( A(4:4), B(1:N,1), X(1:N,1), INFO=INFO )
   WRITE(NOUT,*)'   B - the RHS vector.'
   WRITE (NOUT,FMT) B(1:N,1)
   WRITE(NOUT,*)'  INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSVX(A, B, X, FACT=''F'', INFO =INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_PPSVX( A, B, X, FACT='F', INFO=INFO)
   WRITE(NOUT,*)'   INFO = ', INFO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSVX( A, B(1:N,1), X(1:N,1), FACT=''4'', INFO=INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_PPSVX( A, B(1:N,1), X(1:N,1), FACT='4', INFO=INFO )
   WRITE(NOUT,*)'   INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSVX(A, B, X, ''U'', AF, ''N'', EQU, S, FERR, BERR, RCOND, INFO)'
   A=AA; B=BB; X=HUGE(1.0_WP); EQU = 'N'
   CALL LA_PPSVX(A, B, X, 'U', AF, 'N', EQU, S, FERR, BERR, RCOND, INFO)
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PPSVX, INFO = ', INFO
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSVX(A, B(1:N,1), X(1:N,1), ''U'', AF, ''N'', EQU, S, FERR, BERR, RCOND, INFO)'
   A=AA; B=BB; X=HUGE(1.0_WP); EQU = 'N'
   CALL LA_PPSVX(A, B(1:N,1), X(1:N,1), 'U', AF, 'N', EQU, S, FERR(1), BERR(1), RCOND, INFO)
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_PPSVX, INFO = ', INFO
   WRITE (NOUT,FMT) X(1:N,1)
END PROGRAM LA_SPPSVX_ET_EXAMPLE
