PROGRAM LA_CGGSVD_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_GGSVD
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, INFO, M, P, N, K, L
!  .. LOCAL ARRAYS ..
   INTEGER :: IWORK(1:50)
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:), ALPHA(:), BETA(:)
   COMPLEX(WP), ALLOCATABLE :: A(:,:), B(:,:), U(:,:), V(:,:), Q(:,:), DUMMY(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'CGGSVD ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) M, N, P
   PRINT *, 'M = ', M, ' N = ', N, ' P = ', P
   ALLOCATE ( A(M,N), AA(M,N), B(P,N), BB(P,N), ALPHA(N), BETA(N), &
              U(M,M), V(P,P), Q(N,N) )
!
   READ (NIN, *) AA, BB
   A=AA; B=BB
   WRITE(NOUT,*) 'The matrix A:'
   DO I = 1, M; WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,:); ENDDO
   WRITE(NOUT,*) 'The matrix B:'
   DO I = 1, P; WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) B(I,:); ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_CGGSVD LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GGSVD( A, B, ALPHA, BETA, K, L, U, V, Q, IWORK, INFO ) '
   A=AA; B=BB
   CALL LA_GGSVD( A, B, ALPHA, BETA, K, L, U, V, Q,IWORK, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' K = ', K, ' L = ', L
   WRITE(NOUT,*) ' Alpha'
   WRITE(NOUT,FMT) ALPHA
   WRITE(NOUT,*) ' Beta:'
   WRITE(NOUT,FMT) BETA
   WRITE(NOUT,*) 'U:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) U(:,I); END DO
   WRITE(NOUT,*) 'V:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) V(:,I); END DO
   WRITE(NOUT,*) 'Q:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Q(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GGSVD( A, B, ALPHA, BETA, INFO=INFO ) '
   A=AA; B=BB
   CALL LA_GGSVD( A, B, ALPHA, BETA, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
   WRITE(NOUT,*) ' Alpha'
   WRITE(NOUT,FMT) ALPHA
   WRITE(NOUT,*) ' Beta:'
   WRITE(NOUT,FMT) BETA
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GGSVD( DUMMY, B, ALPHA, BETA, K, L, U, V, Q, IWORK, INFO ) '
   A=AA; B=BB
   CALL LA_GGSVD( DUMMY, B, ALPHA, BETA, K, L, U, V, Q,IWORK, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GGSVD( A, B(:,1:N-1), ALPHA, BETA, K, L, U, V, Q, IWORK, INFO ) '
   A=AA; B=BB
   CALL LA_GGSVD( A, B(:,1:N-1), ALPHA, BETA, K, L, U, V, Q, IWORK,INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GGSVD( A, B, ALPHA(1:N-1), BETA, K, L, U, V, Q, IWORK, INFO ) '
   A=AA; B=BB
   CALL LA_GGSVD( A, B, ALPHA(1:N-1), BETA, K, L, U, V, Q, IWORK,INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GGSVD( A, B, ALPHA, BETA(1:N-1), K, L, U, V, Q,IWORK, INFO ) '
   A=AA; B=BB
   CALL LA_GGSVD( A, B, ALPHA, BETA(1:N-1), K, L, U, V, Q, IWORK,INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GGSVD( A, B, ALPHA, BETA, K, L, U(1:N,:), V, Q,IWORK, INFO ) '
   A=AA; B=BB
   CALL LA_GGSVD( A, B, ALPHA, BETA, K, L, U(1:N,:), V, Q, IWORK,INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GGSV(:,1:M)D( A, B, ALPHA, BETA, K, L, U, V, Q,IWORK, INFO ) '
   A=AA; B=BB
   CALL LA_GGSVD( A, B, ALPHA, BETA, K, L, U, V(:,1:M), Q, IWORK,INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GGSVD( A, B, ALPHA, BETA, K, L, U, V, Q(:,1:P), IWORK,INFO ) '
   A=AA; B=BB
   CALL LA_GGSVD( A, B, ALPHA, BETA, K, L, U, V, Q(:,1:P),IWORK, INFO)
   WRITE(NOUT,*) 'INFO = ', INFO
!
END PROGRAM LA_CGGSVD_ET_EXAMPLE
