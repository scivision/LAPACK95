PROGRAM LA_ZGGLSE_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_GGLSE
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.2,1H,,F7.2,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER  :: I, INFO, M, N, P
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:), CC(:), DD(:)
   COMPLEX(WP), ALLOCATABLE :: A(:, :), B(:,:), C(:), D(:), X(:)
!  .. INTRINSIC FUNCTIONS ..
   INTRINSIC MATMUL
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'ZGGLSE Example Program Results'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) M, N, P
   PRINT *, 'M = ', M, ' N = ', N, ' P = ', P
   ALLOCATE ( A(M,N), AA(M,N), B(P,N), BB(P,N), C(M), CC(M), D(P), DD(P), X(N) )
   READ (NIN,*) AA, BB, CC, DD
   A = AA; B=BB; C = CC; D = DD
   WRITE(NOUT,*) 'The matrix A'
   DO I = 1, M; WRITE (NOUT,FMT) A(I,:); ENDDO
   WRITE(NOUT,*) 'The RHS matrix B:'
   DO I = 1, P; WRITE (NOUT,FMT) B(I,:); ENDDO
   WRITE(NOUT,*) 'The vector C:'; WRITE (NOUT,FMT) C
   WRITE(NOUT,*) 'The vector D:'; WRITE (NOUT,FMT) D
!
   WRITE ( NOUT, * )'--------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_ZGELSE LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GGLSE (A, B, C, D, X)'
   A = AA; B=BB; C = CC; D = DD
   CALL LA_GGLSE (A, B, C, D, X)
   WRITE(NOUT,*)' C - MATMUL(AA, X), MATMUL(BB, X), X'
   WRITE (NOUT,FMT) C - MATMUL( AA, X )
   WRITE (NOUT,FMT) MATMUL( BB, X )
   WRITE(NOUT,FMT) X
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GGLSE (A, B, C, D, X, INFO)'
   A = AA; B=BB; C = CC; D = DD
   CALL LA_GGLSE (A, B, C, D, X, INFO)
   WRITE(NOUT,*)'INFO = ', INFO, ' C - MATMUL(AA, X), MATMUL(BB, X), X'
   WRITE (NOUT,FMT) C - MATMUL( AA, X )
   WRITE (NOUT,FMT) MATMUL( BB, X )
   WRITE(NOUT,FMT) X
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GGLSE (A(:,1:N-1), B, C, D, X, INFO)'
   A = AA; B=BB; C = CC; D = DD
   CALL LA_GGLSE (A(:,1:N-1), B, C, D, X, INFO)
   WRITE(NOUT,*)'INFO = ', INFO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GGLSE (A, B(1:1,:), C, D, X, INFO)'
   A = AA; B=BB; C = CC; D = DD
   CALL LA_GGLSE (A, B(1:1,:), C, D, X, INFO)
   WRITE(NOUT,*)'INFO = ', INFO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GGLSE (A, B, C(1:1), D, X, INFO)'
   A = AA; B=BB; C = CC; D = DD
   CALL LA_GGLSE (A, B, C(1:1), D, X, INFO)
   WRITE(NOUT,*)'INFO = ', INFO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GGLSE (A, B, C, D(1:1), X, INFO)'
   A = AA; B=BB; C = CC; D = DD
   CALL LA_GGLSE (A, B, C, D(1:1), X, INFO)
   WRITE(NOUT,*)'INFO = ', INFO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GGLSE (A, B, C, D, X(1:1))'
   A = AA; B=BB; C = CC; D = DD
   CALL LA_GGLSE (A, B, C, D, X(1:1))
   WRITE(NOUT,*)'INFO = ', INFO
!
END!PROGRAM LA_ZGGLSE_EXAMPLE
