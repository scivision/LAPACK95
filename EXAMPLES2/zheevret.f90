PROGRAM LA_ZHEEVR_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_HEEVR
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
   CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, J, INFO, M, N
   CHARACTER(LEN = 1) :: UPLO
   REAL(WP) :: VL, VU
   INTEGER :: IL, IU
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: AA(:,:), W(:)
   COMPLEX(WP), ALLOCATABLE :: Z(:,:)
   COMPLEX(WP), ALLOCATABLE :: A(:,:)
   INTEGER, ALLOCATABLE :: ISUPPZ(:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE(NOUT,*) 'ZHEEVR ET_Example Program Results.' 
   READ(NIN,*) ! SKIP HEADING IN DATA FILE
   READ(NIN,*) N
   ALLOCATE ( A(N,N), AA(N,N), W(N) )
   ALLOCATE (Z(N,N), ISUPPZ(N))
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
   WRITE ( NOUT, * )'Details of LA_ZHEEVR LAPACK Subroutine Results.'
   WRITE(NOUT,*)
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HEEVR(A, W, INFO=INFO)'
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A - the original matrix (upper triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - destroyed matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   W=0
   CALL LA_HEEVR(A,W,INFO=INFO)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_HEEVR:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'INFO = ',INFO
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W)"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A - the original matrix (upper triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - orthonormal eigenvectors of the matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   W=0
   CALL LA_HEEVR(A,W)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_HEEVR:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The orthonormal eigenvectors computed by LA_HEEVR:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W, ABSTOL=0.01_WP)"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A - the original matrix (upper triangular)'
   WRITE(NOUT,*) '   ABSTOL - the absolute error tolerance for the eigenvalues'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - orthonormal eigenvectors of the matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   W=0
   CALL LA_HEEVR(A,W,  ABSTOL=0.01_WP)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_HEEVR:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The orthonormal eigenvectors computed by LA_HEEVR:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W, UPLO='L')"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A - the original matrix (lower triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - orthonormal eigenvectors of the matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   W=0
   UPLO = 'L'
   CALL LA_HEEVR(A, W, UPLO='L')
   WRITE(NOUT,*) 'The eigenvalues computed by LA_HEEVR:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The orthonormal eigenvectors computed by LA_HEEVR:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W, UPLO='L')"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A - the original matrix (lower triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - destroyed matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   W=0
   UPLO = 'L'
   CALL LA_HEEVR(A,W,UPLO=UPLO)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_HEEVR:'
   WRITE(NOUT,FMT) W(:)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W, INFO=INFO)"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A - the original matrix (upper triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A   - orthonormal eigenvectors of the matrix A'
   WRITE(NOUT,*) '   W   - the eigenvalues in ascending order'
   A=AA
   W=0
   CALL LA_HEEVR(A,W,INFO=INFO)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_HEEVR:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The orthonormal eigenvectors computed by LA_HEEVR:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
   WRITE(NOUT,*) 'INFO = ',INFO
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W, IL=2, IU=3)"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A     - the original matrix (upper triangular)'
   WRITE(NOUT,*) '   IL,IU - the indices of the smallest and largest'
   WRITE(NOUT,*) '           eigenvalues to be returned'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - orthonormal eigenvectors of the matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   W=0
   CALL LA_HEEVR(A,W,IL=2,IU=3)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_HEEVR:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The orthonormal eigenvectors computed by LA_HEEVR:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W, IL=2)"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A     - the original matrix (upper triangular)'
   WRITE(NOUT,*) '   IL,IU - the indices of the smallest and largest'
   WRITE(NOUT,*) '           eigenvalues to be returned (IU = N is assumed).'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - orthonormal eigenvectors of the matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   W=0
   CALL LA_HEEVR(A,W,IL=2)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_HEEVR:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The orthonormal eigenvectors computed by LA_HEEVR:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W, VL=0.1_WP, VU=4.0_WP, M=M)"
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
   CALL LA_HEEVR(A,W,VL=0.1_WP,VU=4.0_WP,M=M)
   WRITE(NOUT,*) 'The total number of eigenvalues found = ',M
   WRITE(NOUT,*) 'The eigenvalues computed by LA_HEEVR:'
   WRITE(NOUT,FMT) W(:M)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W, VU=4.0_WP, M=M)"
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
   CALL LA_HEEVR(A,W,VU=4.0_WP,M=M)
   WRITE(NOUT,*) 'The total number of eigenvalues found = ',M
   WRITE(NOUT,*) 'The eigenvalues computed by LA_HEEVR:'
   WRITE(NOUT,FMT) W(:M)
! STARTING THE ERROR TESTS:
! ERROR 1
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A(:,1:N-1), W, VL=0.1_WP, VU=4.0_WP, M=M, INFO=INFO)"
   A=AA; W=0
   CALL LA_HEEVR(A(:,1:N-1),W,VL=0.1_WP,VU=4.0_WP,M=M, INFO=INFO)
   WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 2
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W(1:N-1), VL=0.1_WP, VU=4.0_WP, M=M, INFO=INFO)"
   A=AA; W=0
   CALL LA_HEEVR(A,W(1:N-1),VL=0.1_WP,VU=4.0_WP,M=M, INFO=INFO)
   WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 3
   UPLO = 'T'; VL = 1; VU = 10
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W, 'T', VL=VL, VU=VU, M=M, INFO=INFO)"
   A=AA; W=0
   CALL LA_HEEVR(A, W, 'T', VL=VL, VU=VU, M=M, INFO=INFO)
   WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 4
   UPLO = 'U'; VL = 1; VU = 10
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W, JOBZ='V',UPLO='U', VL=VL, VU=VU, M=M, INFO=INFO)"
   A=AA; W=0
   CALL LA_HEEVR(A, W, JOBZ='V',UPLO='U', VL=VL, VU=VU, M=M, INFO=INFO)
   WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 4
   UPLO = 'U'; VL = 1; VU = 10
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W, 'U', VL=VL, VU=VU, M=M, INFO=INFO)"
   A=AA; W=0
   CALL LA_HEEVR(A, W, 'U', VL=VL, VU=VU, M=M, INFO=INFO)
   WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 5
   UPLO = 'U'; VL = 1; VU = 10
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W, JOBZ='V',UPLO='U', VL=VL, VU=VU, M=M, ISUPPZ=ISUPPZ(1:N-1), INFO=INFO)"
   A=AA; W=0
   CALL LA_HEEVR(A, W, JOBZ='V',UPLO='U', VL=VL, VU=VU, M=M, ISUPPZ=ISUPPZ(1:N-1), INFO=INFO)
   WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 5
   UPLO = 'U'; VL = 1; VU = 10; A=AA
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W, JOBZ='V',UPLO='U', VL=VL, VU=VU, M=M, ISUPPZ=ISUPPZ, INFO=INFO)"
   A=AA; W=0
   CALL LA_HEEVR(A, W, JOBZ='V',UPLO='U', VL=VL, VU=VU, M=M, ISUPPZ=ISUPPZ, INFO=INFO)
   WRITE(NOUT,*) 'INFO = ', INFO   
! ERROR 6
   UPLO = 'U'; VL = 10; VU = 1
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W, JOBZ='V',UPLO='U', VL=VL, VU=VU, M=M, INFO=INFO)"
   A=AA; W=0
   CALL LA_HEEVR(A, W, JOBZ='V',UPLO='U', VL=VL, VU=VU, M=M, INFO=INFO)
   WRITE(NOUT,*) 'INFO = ', INFO   
! ERROR 7
   UPLO = 'U'; VL = 1; VU = 10; IL=1; IU=N
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W, 'U', VL=VL, VU=VU, IL=IL, IU=IU, M=M, INFO=INFO)"
   A=AA; W=0
   CALL LA_HEEVR(A, W, 'U', VL=VL, VU=VU, IL=IL, IU=IU, M=M, INFO=INFO)
   WRITE(NOUT,*) 'INFO = ', INFO   
! ERROR 8
   UPLO = 'U'; IL=-1; IU=N
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W, UPLO='U', IL=-1, IU=IU, M=M, INFO=INFO)"
   A=AA; W=0
   CALL LA_HEEVR(A, W, UPLO=UPLO, IL=IL, IU=IU, M=M, INFO=INFO)
   WRITE(NOUT,*) 'INFO = ', INFO
! ERROR 8
   UPLO = 'U'; IL=N; IU=N-1
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W, UPLO='U', IL=N, IU=N-1, M=M, INFO=INFO)"
   A=AA; W=0
   CALL LA_HEEVR(A, W, UPLO='U', IL=IL, IU=IU, M=M, INFO=INFO)
   WRITE(NOUT,*) 'INFO = ', INFO   
! ERROR 9
   UPLO = 'U'; IL=1; IU=N+1
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEVR(A, W, JOBZ='N', UPLO='U', IL=1, IU=N+1, M=M, INFO=INFO)"
   A=AA; W=0
   CALL LA_HEEVR(A, W, JOBZ='N', UPLO='U', IL=IL, IU=IU, M=M, INFO=INFO)
   WRITE(NOUT,*) 'INFO = ', INFO   
   
END PROGRAM LA_ZHEEVR_ET_EXAMPLE
