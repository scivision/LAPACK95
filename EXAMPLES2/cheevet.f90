PROGRAM LA_CHEEV_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_HEEV
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
   INTEGER, PARAMETER :: NIN = 5, NOUT = 6
!  .. LOCAL SCALARS ..
   INTEGER :: I, J, INFO, N 
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: AA(:,:), W(:)
   COMPLEX(WP), ALLOCATABLE :: A(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE(NOUT,*) 'CHEEV ET_Example Program Results.'
   READ(NIN,*) ! SKIP HEADING IN DATA FILE
   READ(NIN,*) N
   ALLOCATE ( A(N,N), AA(N,N), W(N) )
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
   WRITE ( NOUT, * )'Details of LA_CHEEV LAPACK Subroutine Results.'
   WRITE(NOUT,*)
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HEEV(A, W, INFO=INFO)'
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A - the original matrix (upper triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - destroyed matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   CALL LA_HEEV(A,W,INFO=INFO)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_HEEV:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'INFO = ',INFO
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEV(A, W, 'V')"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A - the original matrix (upper triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - orthonormal eigenvectors of the matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   CALL LA_HEEV(A,W,'V')
   WRITE(NOUT,*) 'The eigenvalues computed by LA_HEEV:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The orthonormal eigenvectors computed by LA_HEEV:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEV(A, W, 'V', 'L')"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A - the original matrix (lower triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - orthonormal eigenvectors of the matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   CALL LA_HEEV(A,W,'V','L')
   WRITE(NOUT,*) 'The eigenvalues computed by LA_HEEV:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The orthonormal eigenvectors computed by LA_HEEV:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEV(A, W, UPLO='L')"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A - the original matrix (lower triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - destroyed matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   CALL LA_HEEV(A,W,UPLO='L')
   WRITE(NOUT,*) 'The eigenvalues computed by LA_HEEV:'
   WRITE(NOUT,FMT) W(:)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HEEV(A, W, INFO=INFO)'
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A - the original matrix (upper triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - destroyed matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   CALL LA_HEEV(A,W,INFO=INFO)
   WRITE(NOUT,*) 'The eigenvalues computed by LA_HEEV:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'INFO = ',INFO
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) "CALL LA_HEEV(A, W, 'V' )"
   WRITE(NOUT,*) 'ON ENTRY: A'
   WRITE(NOUT,*) '   A - the original matrix (upper triangular)'
   WRITE(NOUT,*) 'ON EXIT: A, W'
   WRITE(NOUT,*) '   A - orthonormal eigenvectors of the matrix A'
   WRITE(NOUT,*) '   W - the eigenvalues in ascending order'
   A=AA
   CALL LA_HEEV(A,W,'V')
   WRITE(NOUT,*) 'The eigenvalues computed by LA_HEEV:'
   WRITE(NOUT,FMT) W(:)
   WRITE(NOUT,*) 'The orthonormal eigenvectors computed by LA_HEEV:'
   DO I = 1, N
      WRITE(NOUT,FMT) A(I,:)
   END DO
! 
END PROGRAM LA_CHEEV_ET_EXAMPLE
