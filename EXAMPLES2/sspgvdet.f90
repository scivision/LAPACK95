PROGRAM LA_SSPGVD_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_SPGVD
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
   CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, J, INFO, N, NS
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: AA(:), BB(:), W(:)
   REAL(WP), ALLOCATABLE :: A(:), B(:), Z(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'SSPGVD ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N
   PRINT *, 'N = ', N
   NS = N*(N+1)/2
   ALLOCATE ( A(NS), AA(NS), B(NS), BB(NS), W(N), Z(N,N) )
!
      READ (NIN, *) AA, BB
   A=AA; B=BB
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, N
        WRITE (NOUT,*) 'J = ', I; WRITE (NOUT,FMT) (A(J+(I-1)*I/2),J=1,I)
      ENDDO
      WRITE(NOUT,*) 'The matrix B:'
      DO I = 1, N
        WRITE (NOUT,*) 'J = ', I; WRITE (NOUT,FMT) (B(J+(I-1)*I/2),J=1,I)
      ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_SSPGVD LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SPGVD( A, B, W, 1, ''U'', Z, INFO )'
   A=AA; B=BB
   CALL LA_SPGVD( A, B, W, 1, 'U', Z, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SPGVD( A, B, W, 2, ''U'', Z )'
   A=AA; B=BB
   CALL LA_SPGVD( A, B, W, 2, 'U', Z )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SPGVD( A, B, W, 3, Z=Z )'
   A=AA; B=BB
   CALL LA_SPGVD( A, B, W, 3, Z=Z )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SPGVD( A, B, W, UPLO=''L'', Z=Z, INFO=INFO )'
   A=AA; B=BB; W = HUGE(1.0_WP); Z = HUGE(1.0_WP)
   CALL LA_SPGVD( A, B, W, UPLO='L', Z=Z, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' EIGENVALUES:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SPGVD( A(1:5), B, W, INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_SPGVD( A(1:5), B, W, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SPGVD( A, B(1:5), W, INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_SPGVD( A, B(1:5), W, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SPGVD( A, B, W(1:N-1), INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_SPGVD( A, B, W(1:N-1), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SPGVD( A, B, W, 5, INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_SPGVD( A, B, W, 5, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SPGVD( A, B, W, UPLO=''9'', INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_SPGVD( A, B, W, UPLO='9', INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SPGVD( A, B, W, Z=Z(1:N-1,:), INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_SPGVD( A, B, W, Z=Z(1:N-1,:), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SPGVD( A, B, W, Z=Z(:,1:N-1), INFO=INFO )'
   A=AA; B=BB; Z = HUGE(1.0_WP)
   CALL LA_SPGVD( A, B, W, Z=Z(:,1:N-1), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
END PROGRAM LA_SSPGVD_ET_EXAMPLE
