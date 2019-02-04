PROGRAM LA_SSYGVD_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP =>  SP
   USE F95_LAPACK, ONLY: LA_SYGVD
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
   CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, J, INFO, N 
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:), W(:)
   REAL(WP), ALLOCATABLE :: A(:,:), B(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE(NOUT,*) 'SSYGVD ET_Example Program Results.'
   READ(NIN,*) ! SKIP HEADING IN DATA FILE
   READ(NIN,*) N
   ALLOCATE ( A(N,N), B(N,N), W(N), AA(N,N), BB(N,N) )
      DO I = 1, N
        READ(NIN,*) (AA(I, J), J = 1, N)
      ENDDO
      DO I = 1, N
        READ(NIN,*) (BB(I, J), J = 1, N)
      ENDDO
      A=AA; B=BB
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, N
        WRITE(NOUT,FMT) A(I,:)
      ENDDO
      WRITE(NOUT,*) 'The matrix B:'
      DO I = 1, N
        WRITE(NOUT,FMT) B(I,:)
      ENDDO
!
   WRITE(NOUT,*) '---------------------------------------------------------'
   WRITE(NOUT,*)
   WRITE ( NOUT, * )'Details of LA_SSYGVD LAPACK Subroutine Results.'
   WRITE(NOUT,*)
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SYGVD(A, B, W, INFO=INFO)'
   WRITE(NOUT,*) 'LA_SYGVD computes all the eigenvalues of a real'
   WRITE(NOUT,*) 'symmetric-definite generalized eigenproblem'
   WRITE(NOUT,*) 'A*x = lambda*B*x'
   WRITE(NOUT,*) 'ON ENTRY: A, B'
   WRITE(NOUT,*) '   A - the original matrix (upper triangular)'
   WRITE(NOUT,*) '   B - the original matrix (upper triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, B, W'
   WRITE(NOUT,*) '   A - destroyed matrix A'
   WRITE(NOUT,*) '   B - the triangular factor U from the Cholesky'
   WRITE(NOUT,*) '       factorization'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   B=BB
   CALL LA_SYGVD(A,B,W,INFO=INFO)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_SYGVD:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'INFO = ',INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_SYGVD(A, B, W, JOBZ='V', INFO=INFO)"
   WRITE(NOUT,*) 'LA_SYGVD computes all the eigenvalues and eigenvectors'
   WRITE(NOUT,*) 'of a real symmetric-definite generalized eigenproblem'
   WRITE(NOUT,*) 'A*x = lambda*B*x'
   WRITE(NOUT,*) 'ON ENTRY: A, B'
   WRITE(NOUT,*) '   A - the original matrix (upper triangular)'
   WRITE(NOUT,*) '   B - the original matrix (upper triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, B, W'
   WRITE(NOUT,*) '   A - the eigenvectors normalized as follows:'
   WRITE(NOUT,*) '       Z**T*B*Z = I'
   WRITE(NOUT,*) '   B - the triangular factor U from the Cholesky'
   WRITE(NOUT,*) '       factorization'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   B=BB
   CALL LA_SYGVD(A,B,W,JOBZ='V',INFO=INFO)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_SYGVD:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The normalized eigenvectors computed by LA_SYGVD:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
   WRITE(NOUT,*) 'INFO = ',INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_SYGVD(A, B, W, JOBZ='V', UPLO='L', INFO=INFO)"
   WRITE(NOUT,*) 'LA_SYGVD computes all the eigenvalues and eigenvectors'
   WRITE(NOUT,*) 'of a real symmetric-definite generalized eigenproblem'
   WRITE(NOUT,*) 'A*x = lambda*B*x'
   WRITE(NOUT,*) 'ON ENTRY: A, B'
   WRITE(NOUT,*) '   A - the original matrix (lower triangular)'
   WRITE(NOUT,*) '   B - the original matrix (lower triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, B, W'
   WRITE(NOUT,*) '   A - the eigenvectors normalized as follows:'
   WRITE(NOUT,*) '       Z**T*B*Z = I'
   WRITE(NOUT,*) '   B - the triangular factor L from the Cholesky'
   WRITE(NOUT,*) '       factorization'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   B=BB
   CALL LA_SYGVD(A,B,W,JOBZ='V',UPLO='L',INFO=INFO)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_SYGVD:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The normalized eigenvectors computed by LA_SYGVD:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
   WRITE(NOUT,*) 'INFO = ',INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_SYGVD(A, B, W, 2, 'V', 'L', INFO)"
   WRITE(NOUT,*) 'LA_SYGVD computes all the eigenvalues and eigenvectors'
   WRITE(NOUT,*) 'of a real symmetric-definite generalized eigenproblem'
   WRITE(NOUT,*) 'A*B*x = lambda*x'
   WRITE(NOUT,*) 'ON ENTRY: A, B'
   WRITE(NOUT,*) '   A - the original matrix (lower triangular)'
   WRITE(NOUT,*) '   B - the original matrix (lower triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, B, W'
   WRITE(NOUT,*) '   A - the eigenvectors normalized as follows:'
   WRITE(NOUT,*) '       Z**T*B*Z = I'
   WRITE(NOUT,*) '   B - the triangular factor L from the Cholesky'
   WRITE(NOUT,*) '       factorization'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   B=BB
   CALL LA_SYGVD(A,B,W,2,'V','L',INFO)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_SYGVD:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The normalized eigenvectors computed by LA_SYGVD:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
   WRITE(NOUT,*) 'INFO = ',INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_SYGVD(A, B, W, 3, 'V', INFO=INFO)"
   WRITE(NOUT,*) 'LA_SYGVD computes all the eigenvalues and eigenvectors'
   WRITE(NOUT,*) 'of a real symmetric-definite generalized eigenproblem'
   WRITE(NOUT,*) 'B*A*x = lambda*x'
   WRITE(NOUT,*) 'ON ENTRY: A, B'
   WRITE(NOUT,*) '   A - the original matrix (upper triangular)'
   WRITE(NOUT,*) '   B - the original matrix (upper triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, B, W'
   WRITE(NOUT,*) '   A - the eigenvectors normalized as follows:'
   WRITE(NOUT,*) '       Z**T*inv(B)*Z = I'
   WRITE(NOUT,*) '   B - the triangular factor U from the Cholesky'
   WRITE(NOUT,*) '       factorization'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   B=BB
   CALL LA_SYGVD(A,B,W,3,'V',INFO=INFO)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_SYGVD:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The normalized eigenvectors computed by LA_SYGVD:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
   WRITE(NOUT,*) 'INFO = ',INFO
!
END PROGRAM LA_SSYGVD_ET_EXAMPLE
