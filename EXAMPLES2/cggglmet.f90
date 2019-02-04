PROGRAM LA_CGGGLM_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_GGGLM
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.2,1H,,F7.2,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER  :: I, INFO, M, N, P
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:), DD(:)
   COMPLEX(WP), ALLOCATABLE :: A(:, :), B(:,:), D(:), X(:), Y(:)
!  .. INTRINSIC FUNCTIONS ..
   INTRINSIC MATMUL
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'CGGGLM Example Program Results'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) M, N, P
   PRINT *, 'M = ', M, ' N = ', N, ' P = ', P
   ALLOCATE ( A(N,M), AA(N,M), B(N,P), BB(N,P), D(N), DD(N), X(M), Y(P) )
   READ (NIN,*) AA, BB, DD
   A = AA; B=BB; D = DD
   WRITE(NOUT,*) 'The matrix A'
   DO I = 1, N; WRITE (NOUT,FMT) A(I,:); ENDDO
   WRITE(NOUT,*) 'The RHS matrix B:'
   DO I = 1, N; WRITE (NOUT,FMT) B(I,:); ENDDO
   WRITE(NOUT,*) 'The vector D:'; WRITE (NOUT,FMT) D
!
   WRITE ( NOUT, * )'--------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_CGGGLM LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GGGLM (A, B, D, X, Y)'
   A = AA; B=BB; D = DD
   CALL LA_GGGLM (A, B, D, X, Y)
   WRITE(NOUT,*)' MATMUL(AA, X) + MATMUL(BB, Y), X, Y'
   WRITE (NOUT,FMT) MATMUL( AA, X ) + MATMUL( BB, Y )
   WRITE(NOUT,FMT) X; WRITE(NOUT,FMT) Y
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GGGLM (A, B, D, X, Y, INFO)'
   A = AA; B=BB; D = DD
   CALL LA_GGGLM (A, B, D, X, Y, INFO)
   WRITE(NOUT,*)'INFO = ', INFO, ' MATMUL(AA, X) + MATMUL(BB, Y), X, Y'
   WRITE (NOUT,FMT) MATMUL( AA, X ) + MATMUL( BB, Y )
   WRITE(NOUT,FMT) X; WRITE(NOUT,FMT) Y
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GGGLM (A(:,1:M-1), B, D, X, Y, INFO)'
   A = AA; B=BB; D = DD
   CALL LA_GGGLM (A(:,1:M-1), B, D, X, Y, INFO)
   WRITE(NOUT,*)'INFO = ', INFO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GGGLM (A, B(1:1,:), D, X, Y, INFO)'
   A = AA; B=BB; D = DD
   CALL LA_GGGLM (A, B(1:1,:), D, X, Y, INFO)
   WRITE(NOUT,*)'INFO = ', INFO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GGGLM (A, B, D(1:1), X, Y, INFO)'
   A = AA; B=BB; D = DD
   CALL LA_GGGLM (A, B, D(1:1), X, Y, INFO)
   WRITE(NOUT,*)'INFO = ', INFO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GGGLM (A, B, D, X(1:1), Y, INFO)'
   A = AA; B=BB; D = DD
   CALL LA_GGGLM (A, B, D, X(1:1), Y, INFO)
   WRITE(NOUT,*)'INFO = ', INFO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GGGLM (A, B, D, X, Y(1:1))'
   A = AA; B=BB; D = DD
   CALL LA_GGGLM (A, B, D, X, Y(1:1))
   WRITE(NOUT,*)'INFO = ', INFO
!
END!PROGRAM LA_CGGGLM_EXAMPLE
