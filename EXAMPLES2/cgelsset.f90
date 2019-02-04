PROGRAM LA_CGELSS_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_GELSS, LA_GETRF
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.2,1H,,F7.2,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER  :: I, INFO, M, N, J, NRHS, RANK
   REAL(WP) :: RCOND
!  .. LOCAL ARRAYS ..
   INTEGER, ALLOCATABLE :: IPIV(:)
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:), S(:)
   COMPLEX(WP), ALLOCATABLE :: A(:, :), B(:,:)
!  .. INTRINSIC FUNCTIONS ..
   INTRINSIC MIN, MAX
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'CGELSS Example Program Results'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) M, N, NRHS
   PRINT *, 'M = ', M, ' N = ', N, ' NRHS = ', NRHS
   ALLOCATE ( A(M,N), AA(M,N), B(MAX(1,M,N),NRHS), BB(MAX(1,M,N),NRHS), &
              S(MIN(M,N)), IPIV(MIN(M,N)) )
   DO I = 1, M; READ (NIN,*) AA(I,:); ENDDO
   DO J = 1, NRHS; BB(:,J) = SUM( AA, DIM=2)*J; ENDDO
   A = AA; B=BB
   WRITE(NOUT,*) 'The matrix A'
   DO I = 1, M; WRITE (NOUT,FMT) A(I,:); ENDDO
   WRITE(NOUT,*) 'The RHS matrix B:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) B(:,J); ENDDO
!
   WRITE ( NOUT, * )'--------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_CGELSS LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELSS (A, B, RANK, S, RCOND, INFO)'
   A = AA; CALL LA_GETRF( A(1:MIN(M,N),1:MIN(M,N)), IPIV, RCOND )
   A = AA; B=BB
   CALL LA_GELSS (A, B, RANK, S, RCOND, INFO)
   WRITE(NOUT,*)'RANK, RCOND, INFO ', RANK, RCOND, INFO
   WRITE(NOUT,*) 'B & S'
   DO J = 1, NRHS; WRITE (NOUT,FMT) B(:,J); END DO
   WRITE(NOUT,FMT) S(:)
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELSS (A, B(:,1), RANK, S, RCOND, INFO)'
   A = AA; B=BB; RCOND = 0.0_WP
   CALL LA_GELSS (A, B(:,1), RANK, S, RCOND, INFO)
   WRITE(NOUT,*)'RANK, RCOND, INFO ', RANK, RCOND, INFO
   WRITE(NOUT,*) 'B & S'
   WRITE (NOUT,FMT) B(:,1)
   WRITE(NOUT,FMT) S(:)
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELSS (A, B, INFO=INFO)'
   A = AA; B=BB;
   CALL LA_GELSS (A, B, INFO=INFO)
   WRITE(NOUT,*)'INFO = ', INFO, ', B'
   DO J = 1, NRHS; WRITE (NOUT,FMT) B(:,J); END DO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELSS (A, B(:,1), INFO=INFO)'
   A = AA; B=BB;
   CALL LA_GELSS (A, B(:,1), INFO=INFO)
   WRITE(NOUT,*)'INFO = ', INFO, ', B(:,1)'
   WRITE(NOUT,FMT) B(:,1)
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELSS (A, B, RANK, INFO=INFO)'
   A = AA; B=BB;
   CALL LA_GELSS (A, B, RANK, INFO=INFO)
   WRITE(NOUT,*)'INFO = ', INFO, ' RANK = ', RANK, ', B'
   DO J = 1, NRHS; WRITE(NOUT,FMT) B(:,J); ENDDO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELSS (A, B(:,1), RANK, S, INFO=INFO)'
   A = AA; B=BB;
   CALL LA_GELSS (A, B(:,1), RANK, S, INFO=INFO)
   WRITE(NOUT,*)'INFO = ', INFO, ' RANK = ', RANK, ', B(:,1) & S'
   WRITE(NOUT,FMT) B(:,1)
   WRITE(NOUT,FMT) S(:)
!
   WRITE (NOUT,*)
   A = AA; CALL LA_GETRF( A(1:MIN(M,N),1:MIN(M,N)), IPIV, RCOND )
   WRITE (NOUT,*) 'CALL LA_GELSS (A, B, RCOND = RCOND, INFO=INFO)'
   A = AA; B=BB;
   CALL LA_GELSS (A, B, RCOND=RCOND, INFO=INFO)
   WRITE(NOUT,*)'INFO = ', INFO, ' RCOND = ', RCOND, ', B'
   DO J = 1, NRHS; WRITE(NOUT,FMT) B(:,J); ENDDO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELSS (A, B(:,1), S=S(1:MAX(M,N)+1), INFO=INFO)'
   A = AA; B=BB;
   CALL LA_GELSS (A, B(:,1), S=S(1:MAX(M,N)+1), INFO=INFO)
   WRITE(NOUT,*)'INFO = ', INFO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELSS (A, B)'
   A = AA; B=BB;
   CALL LA_GELSS (A, B)
   WRITE(NOUT,*)'B'
   DO J = 1, NRHS; WRITE(NOUT,FMT) B(:,J); ENDDO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELSS (A, B(:,1))'
   A = AA; B=BB;
   CALL LA_GELSS (A, B(:,1))
   WRITE(NOUT,*)'B(:,1)'
   WRITE(NOUT,FMT) B(:,1)
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELSS (A, B(MIN(M,N),:), INFO=INFO)'
   A = AA; B=BB;
   CALL LA_GELSS (A, B(MIN(M,N),:), INFO=INFO)
   WRITE(NOUT,*)'INFO = ', INFO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELSS (A(1:N-1,:), B(:,1))'
   A = AA; B=BB;
   CALL LA_GELSS (A(1:N-1,:), B(:,1))
   WRITE(NOUT,*)'INFO = ', INFO
!
END!PROGRAM LA_CGELSS_EXAMPLE
