PROGRAM LA_ZHPEVD_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_HPEVD
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, J, INFO, N, NS
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: AA(:), W(:)
   COMPLEX(WP), ALLOCATABLE :: A(:), Z(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'ZHPEVD ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N
   PRINT *, 'N = ', N
   NS = N*(N+1)/2
   ALLOCATE ( A(NS), AA(NS), W(N), Z(N,N) )
!
      READ (NIN, *) AA
   A=AA
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, N
        WRITE (NOUT,*) 'J = ', I; WRITE (NOUT,FMT) (A(J+(I-1)*I/2),J=1,I)
      ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_ZHPEVD LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HPEVD( A, W, ''U'', Z, INFO )'
   A=AA
   CALL LA_HPEVD( A, W, 'U', Z, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HPEVD( A, W, ''U'', Z )'
   A=AA
   CALL LA_HPEVD( A, W, 'U', Z )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HPEVD( A, W, Z=Z )'
   A=AA
   CALL LA_HPEVD( A, W, Z=Z )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HPEVD( A, W, UPLO=''L'', Z=Z )'
   A=AA
   CALL LA_HPEVD( A, W, UPLO='L', Z=Z )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HPEVD( A, W )'
   A=AA; Z = HUGE(1.0_WP)
   CALL LA_HPEVD( A, W )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HPEVD( A(1:5), W, INFO=INFO )'
   A=AA; Z = HUGE(1.0_WP)
   CALL LA_HPEVD( A(1:5), W, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HPEVD( A, W(1:N-1), INFO=INFO )'
   A=AA; Z = HUGE(1.0_WP)
   CALL LA_HPEVD( A, W(1:N-1), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HPEVD( A, W, UPLO=''9'', INFO=INFO )'
   A=AA; Z = HUGE(1.0_WP)
   CALL LA_HPEVD( A, W, UPLO='9', INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HPEVD( A, W, Z=Z(1:N-1,:), INFO=INFO )'
   A=AA; Z = HUGE(1.0_WP)
   CALL LA_HPEVD( A, W, Z=Z(1:N-1,:), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_HPEVD( A, W, Z=Z(:,1:N-1) )'
   A=AA; Z = HUGE(1.0_WP)
   CALL LA_HPEVD( A, W, Z=Z(:,1:N-1) )
!
END PROGRAM LA_ZHPEVD_ET_EXAMPLE
