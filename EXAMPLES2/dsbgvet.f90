PROGRAM LA_DSBGV_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_SBGV
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, J, INFO, N, KA, KB
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:), W(:)
   REAL(WP), ALLOCATABLE :: A(:,:), B(:,:), Z(:,:), DUMMY(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'DSBGV ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N, KA, KB
   PRINT *, 'N = ', N, ' KA = ', KA, ' KB = ', KB
   ALLOCATE ( A(KA+1,N), AA(KA+1,N), B(KB+1,N), BB(KB+1,N), W(N), Z(N,N) )
!
   AA = HUGE(1.0_WP); BB = HUGE(1.0_WP)
   DO I = 1, KA+1; READ (NIN, *) (AA(I, J), J = KA-I+2, N); ENDDO
   DO I = 1, KB+1; READ (NIN, *) (BB(I, J), J = KB-I+2, N); ENDDO
   A=AA; B=BB
   WRITE(NOUT,*) 'The matrix A:'
   DO I = 1, KA+1; WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) AA(I,:); ENDDO
   WRITE(NOUT,*) 'The matrix B:'
   DO I = 1, KB+1; WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) BB(I,:); ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_DSBGV LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBGV( A, B, W, ''U'', Z, INFO )'
   A=AA; B=BB
   CALL LA_SBGV( A, B, W, 'U', Z, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBGV( A, B, W, ''U'', Z )'
   A=AA; B=BB
   CALL LA_SBGV( A, B, W, 'U', Z )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBGV( A, B, W, Z=Z )'
   A=AA; B=BB
   CALL LA_SBGV( A, B, W, Z=Z )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBGV( A, B, W )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_SBGV( A, B, W )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBGV( DUMMY, B, W, INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_SBGV( DUMMY, B, W, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBGV( A, B(:,1:N-1), W, INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_SBGV( A, B(:,1:N-1), W, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBGV( A, B, W(1:N-1), INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_SBGV( A, B, W(1:N-1), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBGV( A, B, W, UPLO=''9'', INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_SBGV( A, B, W, UPLO='9', INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBGV( A, B, W, Z=Z(1:N-1,:), INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_SBGV( A, B, W, Z=Z(1:N-1,:), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SBGV( A, B, W, Z=Z(:,1:N-1) )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_SBGV( A, B, W, Z=Z(:,1:N-1) )
!
END PROGRAM LA_DSBGV_ET_EXAMPLE
