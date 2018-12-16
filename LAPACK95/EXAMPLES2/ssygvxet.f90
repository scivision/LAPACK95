PROGRAM LA_SSYGVX_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_SYGVX
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
      CHARACTER(LEN=1) :: UPLO
      INTEGER :: I, J, INFO, N
      INTEGER :: IL, IU, ITYPE
      REAL(WP) :: VL, VU
      INTEGER  ::  M 
!  .. LOCAL ARRAYS ..
      INTEGER, ALLOCATABLE :: IFAIL(:)
      REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:), W(:)
      REAL(WP), ALLOCATABLE :: A(:,:), B(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE(NOUT,*) 'SSYGVX ET_Example Program Results.'
   READ(NIN,*) ! SKIP HEADING IN DATA FILE
      READ(NIN,*) N
      ITYPE = 1; UPLO = 'L'
      VL = -10; VU = 10; IL = 1; IU = N;
   ALLOCATE ( A(N,N), B(N,N), W(N), AA(N,N), BB(N,N), IFAIL(N))
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
      WRITE ( NOUT, * )'Details of LA_SSYGVX LAPACK Subroutine Results.'
      WRITE(NOUT,*)
!
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_SYGVX(A, B, W, INFO=INFO)'
      WRITE(NOUT,*) 'LA_SYGVX computes all the eigenvalues of a real'
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
      CALL LA_SYGVX(A, B, W, INFO=INFO)
      WRITE(NOUT,*) 'The eigenvalues computed by LA_SYGVX:'
      WRITE(NOUT,FMT) W(:)
      WRITE(NOUT,*) 'INFO = ',INFO
!      
      WRITE(NOUT,*)
      WRITE(NOUT,*) "CALL LA_SYGVX(A, B, W, JOBZ='V', IL=IL, IU=IU, INFO=INFO)"
      WRITE(NOUT,*) 'LA_SYGVX computes all the eigenvalues and eigenvectors'
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
      CALL LA_SYGVX(A, B, W, JOBZ='V', IL=IL, IU=IU, INFO=INFO)
      WRITE(NOUT,*) 'The eigenvalues computed by LA_SYGVX: '
      WRITE(NOUT,FMT) W(:)
      WRITE(NOUT,*) 'The normalized eigenvectors computed by LA_SYGVX:'
      DO I = 1, N
        WRITE(NOUT,FMT) A(I,:)
      END DO
      WRITE(NOUT, *) 'The indices of the smallest eigenvalue to be returned: IL= ', IL
      WRITE(NOUT, *) 'The indices of the largest eigenvalue to be returned: IU= ', IU
      WRITE(NOUT,*) 'The orthonormal eigenvectors of the matrix A '
      DO I = 1, N
        WRITE(NOUT,FMT) A(I,:)
      END DO          
      WRITE(NOUT,*) 'INFO = ',INFO
!
      WRITE(NOUT,*)
      WRITE(NOUT,*) "CALL LA_SYGVX(A, B, W, JOBZ='V', VL=VL, VU=VU, INFO=INFO)"
      WRITE(NOUT,*) 'LA_SYGVX computes all the eigenvalues and eigenvectors'
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
      CALL LA_SYGVX(A, B, W, ITYPE, 'V', VL=VL, VU=VU, INFO=INFO)
      WRITE(NOUT,*) 'The eigenvalues computed by LA_SYGVX:'
      WRITE(NOUT,FMT) W(:)
      WRITE(NOUT,*) 'The normalized eigenvectors computed by LA_SYGVX:'
      DO I = 1, N
        WRITE(NOUT,FMT) A(I,:)
      END DO
      WRITE(NOUT, *) 'The lower bound of the interval to be searched for eigenvalues VL= ', VL
      WRITE(NOUT, *) 'The upper bound of the interval to be searched for eigenvalues VU= ', VU 
      WRITE(NOUT,*) 'The orthonormal eigenvectors of the matrix A '
      DO I = 1, N
        WRITE(NOUT,FMT) A(I,:)
      END DO
      WRITE(NOUT,*) 'INFO = ',INFO

      ITYPE = 2
      WRITE(NOUT,*)
      WRITE(NOUT,*) "CALL LA_SYGVX(A, B, W, ITYPE, 'V', M=M, IFAIL=IFAIL, INFO=INFO)"
      WRITE(NOUT,*) 'LA_SYGVX computes all the eigenvalues and eigenvectors'
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
      CALL LA_SYGVX(A, B, W, ITYPE, 'V', M=M, IFAIL=IFAIL, INFO=INFO)
      WRITE(NOUT,*) 'The eigenvalues computed by LA_SYGVX:'
      WRITE(NOUT,FMT) W(:)
      WRITE(NOUT,*) 'The normalized eigenvectors computed by LA_SYGVX:'
      DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
      END DO
      WRITE(NOUT,*) ' IFAIL  computed by LA_SYGVX:'
      WRITE(NOUT, *) IFAIL(:)
      WRITE(NOUT, *) 'The total number of eigenvalues found ', M
      WRITE(NOUT,*) 'The orthonormal eigenvectors of the matrix A '
      DO I = 1, N
        WRITE(NOUT,FMT) A(I,:)
      END DO   
      WRITE(NOUT,*) 'INFO = ',INFO

      ITYPE = 3
      WRITE(NOUT,*)
      WRITE(NOUT,*) "CALL LA_SYGVX(A, B, W, ITYPE, 'V', 'L', VL, VU, M=M, IFAIL=IFAIL, INFO=INFO)"
      WRITE(NOUT,*) 'LA_SYGVX computes all the eigenvalues and eigenvectors'
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
      CALL LA_SYGVX(A, B, W, ITYPE, 'V', 'L', VL, VU, M=M, IFAIL=IFAIL, INFO=INFO)
      WRITE(NOUT,*) 'The eigenvalues computed by LA_SYGVX:'
      WRITE(NOUT,FMT) W(:)
      WRITE(NOUT,*) 'The normalized eigenvectors computed by LA_SYGVX:'
      DO I = 1, N
        WRITE(NOUT,FMT) A(I,:)
      END DO
      WRITE(NOUT,*) ' IFAIL  computed by LA_SYGVX:'
      WRITE(NOUT, *) IFAIL(:)
      WRITE(NOUT, *) 'The total number of eigenvalues found ', M
      WRITE(NOUT, *) 'The lower bound of the interval to be searched for eigenvalues VL= ', VL
      WRITE(NOUT, *) 'The upper bound of the interval to be searched for eigenvalues VU= ', VU
      WRITE(NOUT,*) 'The orthonormal eigenvectors of the matrix A '
      DO I = 1, N
        WRITE(NOUT,FMT) A(I,:)
      END DO                                                                   
      WRITE(NOUT,*) 'INFO = ',INFO

! STARTING ERROR TESTS
      WRITE(NOUT,*)
      WRITE(NOUT,*) "CALL LA_SYGVX(A(1:N-1,1:N), B, W, INFO=INFO)"
      A=AA; B=BB
      CALL LA_SYGVX(A(1:N-1,1:N), B, W, INFO=INFO)
      WRITE(NOUT,*) 'INFO = ',INFO
      
      WRITE(NOUT,*)
      WRITE(NOUT,*) "CALL LA_SYGVX(A, B(1:N,1:N-1), W, INFO=INFO)"
      A=AA; B=BB
      CALL LA_SYGVX(A, B(1:N,1:N-1), W, INFO=INFO)
      WRITE(NOUT,*) 'INFO = ',INFO
      
      WRITE(NOUT,*)
      WRITE(NOUT,*) "CALL LA_SYGVX(A, B, W(1:N-2), INFO=INFO)"
      A=AA; B=BB
      CALL LA_SYGVX(A, B, W(1:N-2), INFO=INFO)
      WRITE(NOUT,*) 'INFO = ',INFO
      
      WRITE(NOUT,*)
      WRITE(NOUT,*) "CALL LA_SYGVX(A, B, W, 5, INFO=INFO)"
      A=AA; B=BB
      CALL LA_SYGVX(A, B, W, 5, INFO=INFO)
      WRITE(NOUT,*) 'INFO = ',INFO
      
      WRITE(NOUT,*)
      WRITE(NOUT,*) "CALL LA_SYGVX(A, B, W, JOBZ='T', INFO=INFO)"
      A=AA; B=BB
      CALL LA_SYGVX(A, B, W, JOBZ='T', INFO=INFO)
      WRITE(NOUT,*) 'INFO = ',INFO
      
      WRITE(NOUT,*)
      WRITE(NOUT,*) "CALL LA_SYGVX(A, B, W, UPLO='T', INFO=INFO)"
      A=AA; B=BB
      CALL LA_SYGVX(A, B, W, UPLO='T', INFO=INFO)
      WRITE(NOUT,*) 'INFO = ',INFO
      
      WRITE(NOUT,*)
      WRITE(NOUT,*) "CALL LA_SYGVX(A, B, W, JOBZ='V', VL=10.0, VU=2.0, INFO=INFO)"
      A=AA; B=BB
      CALL LA_SYGVX(A, B, W, JOBZ='V', VL=10.0, VU=2.0, INFO=INFO)
      WRITE(NOUT,*) 'INFO = ',INFO
      
      WRITE(NOUT,*)
      WRITE(NOUT,*) "CALL LA_SYGVX(A, B, W, JOBZ='V', VL=1.0, VU=10.0, IL=1, IU=3, INFO=INFO)"
      A=AA; B=BB
      CALL LA_SYGVX(A, B, W, JOBZ='V', VL=1.0, VU=10.0, IL=1, IU=3, INFO=INFO)
      WRITE(NOUT,*) 'INFO = ',INFO
      
      
      WRITE(NOUT,*)
      WRITE(NOUT,*) "CALL LA_SYGVX(A, B, W, JOBZ='V', IL=100, IU=3, INFO=INFO)"
      A=AA; B=BB
      CALL LA_SYGVX(A, B, W, JOBZ='V', IL=100, IU=3, INFO=INFO)
      WRITE(NOUT,*) 'INFO = ',INFO
      
      WRITE(NOUT,*)
      WRITE(NOUT,*) "CALL LA_SYGVX(A, B, W, JOBZ='V', IFAIL=IFAIL(1:N-3), INFO=INFO)"
      A=AA; B=BB
      CALL LA_SYGVX(A, B, W, JOBZ='V', IFAIL=IFAIL(1:N-3), INFO=INFO)
      WRITE(NOUT,*) 'INFO = ',INFO
      
      
 END PROGRAM LA_SSYGVX_ET_EXAMPLE

