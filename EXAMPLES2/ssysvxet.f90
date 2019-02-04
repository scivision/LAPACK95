PROGRAM LA_SSYSVX_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_SYSVX
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, J, INFO, N, NRHS
!  .. LOCAL ARRAYS ..
   REAL(WP) :: RCOND
   INTEGER, ALLOCATABLE :: IPIV(:)
   REAL(WP), ALLOCATABLE :: A(:,:), B(:,:), X(:,:), AF(:,:)
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:), FERR(:), BERR(:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'SSYSV ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N, NRHS
   PRINT *, 'N = ', N, ' NRHS = ', NRHS
   ALLOCATE ( A(N,N), AA(N,N), B(N,NRHS), BB(N,NRHS), IPIV(N), X(N,NRHS), AF(N,N), &
             FERR(NRHS), BERR(NRHS) )
!
   AA = HUGE(1.0_WP)
   DO I = 1, N
     READ (NIN, *) AA(I,I:N)
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
   WRITE ( NOUT, * )'Details of LA_SSYSVX LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SYSVX( A, B, X, AF=AF, IPIV=IPIV, INFO=INFO )'
   A=AA; B=BB
   CALL LA_SYSVX( A, B, X, AF=AF, IPIV=IPIV, INFO=INFO )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_SYSVX, INFO = ', INFO
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*) 'Pivots: ', IPIV
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SYSVX( A, B(1:N,1), X(1:N,1), AF=AF, IPIV=IPIV, INFO=INFO )'
   A=AA; B=BB
   CALL LA_SYSVX( A, B(1:N,1), X(1:N,1), AF=AF, IPIV=IPIV, INFO=INFO )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_SYSVX, INFO = ', INFO
   WRITE (NOUT,FMT) X(:,1)
   WRITE(NOUT,*) 'Pivots: ', IPIV
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SYSVX( A, B, X )'
   A=AA; B=BB
   CALL LA_SYSVX( A, B, X )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_SYSVX:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SYSVX(A, B(1:N,1), X(1:N,1) )'
   A=AA; B=BB
   CALL LA_SYSVX(A, B(1:N,1), X(1:N,1) )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_SYSVX:'
   WRITE (NOUT,FMT) X(:,1)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SYSVX(A, B, X, IPIV=IPIV)'
   A=AA; B=BB
   CALL LA_SYSVX( A, B, X, IPIV=IPIV)
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_SYSVX:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*) 'Pivots: ', IPIV
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SYSVX( A, B(1:N,1), X(1:N,1), IPIV=IPIV )'
   A=AA; B=BB
   CALL LA_SYSVX( A, B(1:N,1), X(1:N,1), IPIV=IPIV )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_SYSVX:'
   WRITE (NOUT,FMT) X(1:N,1)
   WRITE(NOUT,*) 'Pivots: ', IPIV
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SYSVX(A(4:4,:), B, X, INFO =INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_SYSVX( A(4:4,:), B, X, INFO=INFO)
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_SYSVX, INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SYSVX( A, B(1:N,1), X(1:N-1,1), INFO=INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_SYSVX( A, B(1:N,1), X(1:N-1,1), INFO=INFO )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_SYSVX, INFO = ', INFO
   WRITE (NOUT,FMT) X(1:N,1)
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SYSVX(A, B, X, ''L'', INFO =INFO )'
   A=TRANSPOSE(AA); B=BB; X=HUGE(1.0_WP)
   CALL LA_SYSVX( A, B, X, 'L', INFO=INFO)
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_SYSVX, INFO = ', INFO
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SYSVX( A, B(1:N,1), X(1:N,1), FACT=''F'', INFO=INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_SYSVX( A, B(1:N,1), X(1:N,1), FACT='F', INFO=INFO )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_SYSVX, INFO = ', INFO
   WRITE (NOUT,FMT) X(1:N,1)
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SYSVX(A, B, X, ''U'', AF, IPIV, ''N'', ', &
                      'FERR, BERR, RCOND, INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_SYSVX(A, B, X, 'U', AF, IPIV, 'N', FERR, BERR, RCOND, INFO )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_SYSVX, INFO = ', INFO
   DO J = 1, NRHS; WRITE (NOUT,FMT) X(:,J); END DO
   WRITE(NOUT,*) 'RCOND = ', RCOND
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SYSVX(A, B(1:N,1), X(1:N,1), ''U'', AF, IPIV, ''F'', ', &
                      'FERR, BERR, &'
   WRITE(NOUT,*) '              RCOND, INFO )'
   A=AA; B=BB; X=HUGE(1.0_WP)
   CALL LA_SYSVX(A, B(1:N,1), X(1:N,1), 'U', AF, IPIV, 'F', &
                 FERR(1), BERR(1), RCOND, INFO )
   WRITE(NOUT,*)'   X - the solution vectors computed by LA_SYSVX, INFO = ', INFO
   WRITE (NOUT,FMT) X(1:N,1)
   WRITE(NOUT,*) 'RCOND = ', RCOND
!
END PROGRAM LA_SSYSVX_ET_EXAMPLE
