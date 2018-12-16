PROGRAM LA_CPOSV_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_POSV
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, J, INFO, N, NRHS
!  .. LOCAL ARRAYS ..
   COMPLEX(WP), ALLOCATABLE :: A(:,:), B(:,:)
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'CPOSV ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N, NRHS
   PRINT *, 'N = ', N, ' NRHS = ', NRHS
   ALLOCATE ( A(N,N), AA(N,N), B(N,NRHS), BB(N,NRHS) )
!
        READ (NIN, *) AA
      DO J = 1, NRHS
         BB(:,J) = SUM( AA, DIM=2)*J
      ENDDO
      DO I = 2, N
         AA(I,1:I-1) = 0.0_WP
      ENDDO
!
   A=AA; B=BB
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, N
        WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,:)
      ENDDO
      WRITE(NOUT,*) 'The RHS matrix B:'
      DO J = 1, NRHS
        WRITE (NOUT,*) 'RHS', J; WRITE (NOUT,FMT) B(:,J)
      ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_CPOSV LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_POSV(A, B )'
   A=AA; B=BB
   IF (NRHS .GT. 1) THEN
      CALL LA_POSV( A, B )
   ELSE
      CALL LA_POSV( A, B(1:N,1) )
   END IF
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_POSV:'
   DO J = 1, NRHS
      WRITE (NOUT,FMT) B(:,J)
   END DO
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_POSV( A, B, ''L'' )'
   A=TRANSPOSE(AA); B=BB
   CALL LA_POSV( A, B, 'L' )
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_POSV:'
   DO J = 1, NRHS
      WRITE (NOUT,FMT) B(:,J)
   END DO
   A=AA; B=BB
   CALL LA_POSV( A, B(1:N,1), 'U', INFO )
   WRITE(NOUT,*)'B - the solution vectors computed by LA_POSV, INFO = ', INFO
   WRITE (NOUT,FMT) B(1:N,1)
!
END PROGRAM LA_CPOSV_ET_EXAMPLE
