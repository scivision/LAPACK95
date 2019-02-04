PROGRAM LA_CGELS_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_GELS
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.2,1H,,F7.2,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER  :: I, INFO, M, N, J, NRHS
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:)
   COMPLEX(WP), ALLOCATABLE :: A(:, :), B(:,:)
!  .. INTRINSIC FUNCTIONS ..
   INTRINSIC MIN, MAX
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'CGELS Example Program Results'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) M, N, NRHS
   PRINT *, 'M = ', M, ' N = ', N, ' NRHS = ', NRHS
   ALLOCATE ( A(M,N), AA(M,N), B(MAX(1,M,N),NRHS), BB(MAX(1,M,N),NRHS) )
   DO I = 1, M; READ (NIN,*) AA(I,:); ENDDO
   DO J = 1, NRHS; BB(:,J) = SUM( AA, DIM=2)*J; ENDDO
   A = AA; B=BB;
   WRITE(NOUT,*) 'The matrix A'
   DO I = 1, M; WRITE (NOUT,FMT) A(I,:); ENDDO
   WRITE(NOUT,*) 'The RHS matrix B:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) B(:,J); ENDDO
!
   WRITE ( NOUT, * )'--------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_CGELS LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELS (A, B, INFO=INFO)'
   A = AA; B=BB;
   CALL LA_GELS (A, B, INFO=INFO)
   WRITE(NOUT,*)'INFO = ', INFO, ', B'
   DO J = 1, NRHS; WRITE (NOUT,FMT) B(:,J); END DO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELS (A, B(:,1), INFO=INFO)'
   A = AA; B=BB;
   CALL LA_GELS (A, B(:,1), INFO=INFO)
   WRITE(NOUT,*)'INFO = ', INFO, ', B(:,1)'
   WRITE(NOUT,FMT) B(:,1)
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELS (A, B, ''T'', INFO=INFO)'
   A = AA; B=BB;
   CALL LA_GELS (A, B, 'T', INFO=INFO)
   WRITE(NOUT,*)'INFO = ', INFO, ', B'
   DO J = 1, NRHS; WRITE(NOUT,FMT) B(:,J); ENDDO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELS (A, B(:,1), ''T'', INFO=INFO)'
   A = AA; B=BB;
   CALL LA_GELS (A, B(:,1), 'T', INFO=INFO)
   WRITE(NOUT,*)'INFO = ', INFO, ', B(:,1)'
   WRITE(NOUT,FMT) B(:,1)
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELS (A, B, ''C'', INFO=INFO)'
   A = AA; B=BB;
   CALL LA_GELS (A, B, 'C', INFO=INFO)
   WRITE(NOUT,*)'INFO = ', INFO, ', B'
   DO J = 1, NRHS; WRITE(NOUT,FMT) B(:,J); ENDDO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELS (A, B(:,1), ''1'', INFO=INFO)'
   A = AA; B=BB;
   CALL LA_GELS (A, B(:,1), '1', INFO=INFO)
   WRITE(NOUT,*)'INFO = ', INFO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELS (A, B)'
   A = AA; B=BB;
   CALL LA_GELS (A, B)
   WRITE(NOUT,*)'B'
   DO J = 1, NRHS; WRITE(NOUT,FMT) B(:,J); ENDDO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELS (A, B(:,1)'
   A = AA; B=BB;
   CALL LA_GELS (A, B(:,1))
   WRITE(NOUT,*)'B(:,1) )'
   WRITE(NOUT,FMT) B(:,1)
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELS (A, B(MIN(M,N),:), INFO=INFO)'
   A = AA; B=BB;
   CALL LA_GELS (A, B(MIN(M,N),:), INFO=INFO)
   WRITE(NOUT,*)'INFO = ', INFO
!
   WRITE (NOUT,*)
   WRITE (NOUT,*) 'CALL LA_GELS (A(1:N-1,:), B(:,1))'
   A = AA; B=BB;
   CALL LA_GELS (A(1:N-1,:), B(:,1))
   WRITE(NOUT,*)'INFO = ', INFO
!
END!PROGRAM LA_CGELS_EXAMPLE
