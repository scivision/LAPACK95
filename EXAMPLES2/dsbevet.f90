PROGRAM LA_DSBEV_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_SBEV
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, J, INFO, N, KD
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: AA(:,:), W(:)
   REAL(WP), ALLOCATABLE :: A(:,:), Z(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'DSBEV ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N, KD
   PRINT *, 'N = ', N, ' KD = ', KD
   ALLOCATE ( A(KD+1,N), AA(KD+1,N), W(N), Z(N,N) )
!
   AA = HUGE(1.0_WP)
   DO I = 1, KD+1
      READ (NIN, *) (AA(I, J), J = KD-I+2, N)
   ENDDO
   A=AA
   WRITE(NOUT,*) 'The matrix A:'
   DO I = 1, KD+1
      WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,1:N)
   ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_Dsbev LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBEV( A, W, ''U'', Z, INFO )'
   A=AA
   CALL LA_SBEV( A, W, 'U', Z, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBEV( A, W, ''U'', Z )'
   A=AA
   CALL LA_SBEV( A, W, 'U', Z )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBEV( A, W, Z=Z )'
   A=AA
   CALL LA_SBEV( A, W, Z=Z )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBEV( A, W, UPLO=''L'', Z=Z )'
   A = HUGE(1.0_WP)
   WRITE(NOUT,*) 'The transpose matrix of A:'
   DO I = 1, KD+1
      A(I,1:N-I+1) = AA(KD+2-I,I:N)
      WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,1:N)
   ENDDO
   CALL LA_SBEV( A, W, UPLO='L', Z=Z )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBEV( A, W )'
   A=AA; Z = HUGE(1.0_WP)
   CALL LA_SBEV( A, W )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBEV( A(:,1:N-1), W, INFO=INFO )'
   A=AA; Z = HUGE(1.0_WP)
   CALL LA_SBEV( A(:,1:N-1), W, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBEV( A, W(1:N-1), INFO=INFO )'
   A=AA; Z = HUGE(1.0_WP)
   CALL LA_SBEV( A, W(1:N-1), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBEV( A, W, UPLO=''9'', INFO=INFO )'
   A=AA; Z = HUGE(1.0_WP)
   CALL LA_SBEV( A, W, UPLO='9', INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBEV( A, W, Z=Z(1:N-1,:), INFO=INFO )'
   A=AA; Z = HUGE(1.0_WP)
   CALL LA_SBEV( A, W, Z=Z(1:N-1,:), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBEV( A, W, Z=Z(:,1:N-1) )'
   A=AA; Z = HUGE(1.0_WP)
   CALL LA_SBEV( A, W, Z=Z(:,1:N-1) )
!
END PROGRAM LA_DSBEV_ET_EXAMPLE
