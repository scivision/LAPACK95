PROGRAM LA_DSYEVX_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_SYEVX
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, J, INFO, M, N 
!  .. LOCAL ARRAYS ..
   INTEGER, ALLOCATABLE :: IFAIL(:) 
   REAL(WP), ALLOCATABLE :: AA(:,:), W(:)
   REAL(WP), ALLOCATABLE :: A(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE(NOUT,*) 'DSYEVX ET_Example Program Results.'
   READ(NIN,*) ! SKIP HEADING IN DATA FILE
   READ(NIN,*) N
   ALLOCATE ( A(N,N), AA(N,N), W(N), IFAIL(N) )
      DO I = 1, N
        READ(NIN,*) (AA(I, J), J = 1, N)
      ENDDO
   A=AA
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, N
        WRITE(NOUT,FMT) A(I,:)
      ENDDO
!
   WRITE(NOUT,*) '---------------------------------------------------------'
   WRITE(NOUT,*)
   WRITE ( NOUT, * )'Details of LA_DSYEVX LAPACK Subroutine Results.'
   WRITE(NOUT,*)
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SYEVX(A, W, INFO=INFO)'
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A - the original matrix (upper triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - destroyed matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   W=0
   CALL LA_SYEVX(A,W,INFO=INFO)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_SYEVX:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'INFO = ',INFO
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_SYEVX(A, W, 'V', IFAIL=IFAIL)"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A - the original matrix (upper triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - orthonormal eigenvectors of the matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   W=0
   CALL LA_SYEVX(A,W,'V',IFAIL=IFAIL)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_SYEVX:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The orthonormal eigenvectors computed by LA_SYEVX:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
   WRITE(NOUT,*) 'The values of the vector "IFAIL":'
   WRITE(NOUT,*) IFAIL(:)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_SYEVX(A, W, 'V', IFAIL=IFAIL, ABSTOL=0.01_WP)"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A - the original matrix (upper triangular)'
   WRITE(NOUT,*) '   ABSTOL - the absolute error tolerance for the eigenvalues'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - orthonormal eigenvectors of the matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   W=0
   CALL LA_SYEVX(A,W,'V',IFAIL=IFAIL, ABSTOL=0.01_WP)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_SYEVX:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The orthonormal eigenvectors computed by LA_SYEVX:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
   WRITE(NOUT,*) 'The values of the vector "IFAIL":'
   WRITE(NOUT,*) IFAIL(:)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_SYEVX(A, W, 'V', 'L', IFAIL=IFAIL)"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A - the original matrix (lower triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - orthonormal eigenvectors of the matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   W=0
   CALL LA_SYEVX(A,W, 'V', 'L', IFAIL=IFAIL)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_SYEVX:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The orthonormal eigenvectors computed by LA_SYEVX:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
   WRITE(NOUT,*) 'The values of the vector "IFAIL":'
   WRITE(NOUT,*) IFAIL(:)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_SYEVX(A, W, UPLO='L')"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A - the original matrix (lower triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - destroyed matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   W=0
   CALL LA_SYEVX(A,W,UPLO='L')
   WRITE(NOUT,*) 'The eigenvalues computed by LA_SYEVX:'
   WRITE(NOUT,FMT) W(:)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_SYEVX(A, W, JOBZ='V', IFAIL=IFAIL, INFO=INFO)"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A - the original matrix (upper triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, W, IFAIL'
   WRITE(NOUT,*) '   A   - orthonormal eigenvectors of the matrix A'
   WRITE(NOUT,*) '   W   - the eigenvalues in ascending order'
   WRITE(NOUT,*) ' IFAIL - the indices of the failed eigenvectors'
   A=AA
   W=0
   IFAIL=0
   CALL LA_SYEVX(A, W, JOBZ='V', IFAIL=IFAIL, INFO=INFO)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_SYEVX:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The orthonormal eigenvectors computed by LA_SYEVX:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
   WRITE(NOUT,*) 'The indices of the eigenvectors that failed to converge:'
   WRITE(NOUT,*) IFAIL(:)
   WRITE(NOUT,*) 'INFO = ',INFO
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_SYEVX(A, W, 'V', IL=2, IU=3, IFAIL=IFAIL)"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A     - the original matrix (upper triangular)'
   WRITE(NOUT,*) '   IL,IU - the indices of the smallest and largest'
   WRITE(NOUT,*) '           eigenvalues to be returned'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - orthonormal eigenvectors of the matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   W=0
   CALL LA_SYEVX(A,W, 'V', IFAIL=IFAIL,IL=2,IU=3)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_SYEVX:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The orthonormal eigenvectors computed by LA_SYEVX:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
   WRITE(NOUT,*) 'The values of the vector "IFAIL":'
   WRITE(NOUT,*) IFAIL(:)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_SYEVX(A, W, 'V', IFAIL=IFAIL, IL=2)"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A     - the original matrix (upper triangular)'
   WRITE(NOUT,*) '   IL,IU - the indices of the smallest and largest'
   WRITE(NOUT,*) '           eigenvalues to be returned (IU = N is assumed).'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - orthonormal eigenvectors of the matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   W=0
   CALL LA_SYEVX(A,W,'V', IFAIL=IFAIL,IL=2)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_SYEVX:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The orthonormal eigenvectors computed by LA_SYEVX:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
   WRITE(NOUT,*) 'The values of the vector "IFAIL":'
   WRITE(NOUT,*) IFAIL(:)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_SYEVX(A, W, VL=0.1_WP, VU=4.0_WP, M=M)"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A     - the original matrix (upper triangular)'
   WRITE(NOUT,*) '   VL,VU - the lower and upper bounds of the interval'
   WRITE(NOUT,*) '           to be searched for eigenvalues'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - destroyed matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   WRITE(NOUT,*) '   M - the total number of eigenvalues found'
   A=AA
   W=0
   CALL LA_SYEVX(A,W,VL=0.1_WP,VU=4.0_WP,M=M)
   WRITE(NOUT,*) 'The total number of eigenvalues found = ',M
   WRITE(NOUT,*) 'The eigenvalues computed by LA_SYEVX:'
   WRITE(NOUT,FMT) W(:M)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_SYEVX(A, W, VU=4.0_WP, M=M)"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A     - the original matrix (upper triangular)'
   WRITE(NOUT,*) '   VL,VU - the lower and upper bounds of the interval'
   WRITE(NOUT,*) '           to be searched for eigenvalues (VL = -infinity'
   WRITE(NOUT,*) '           is assumed)'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - destroyed matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   WRITE(NOUT,*) '   M - the total number of eigenvalues found'
   A=AA
   W=0
   CALL LA_SYEVX(A,W,VU=4.0_WP,M=M)
   WRITE(NOUT,*) 'The total number of eigenvalues found = ',M
   WRITE(NOUT,*) 'The eigenvalues computed by LA_SYEVX:'
   WRITE(NOUT,FMT) W(:M)
! 
END PROGRAM LA_DSYEVX_ET_EXAMPLE
