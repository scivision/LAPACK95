!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, J, INFO, N 
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:), W(:)
   COMPLEX(WP), ALLOCATABLE :: A(:,:), B(:,:)
!  .. EXECUTABLE STATEMENTS ..
   READ(NIN,*) ! SKIP HEADING IN DATA FILE
   READ(NIN,*) N
   ALLOCATE ( A(N,N), B(N,N), W(N), AA(N,N), BB(N,N) )
      DO I = 1, N
        READ(NIN,*) (AA(I, J), J = 1, N)
      ENDDO
      DO I = 1, N
        READ(NIN,*) (BB(I, J), J = 1, N)
      ENDDO
      A=AA; B=BB
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, N
        WRITE(NOUT,FMT) A(I,:)
      ENDDO
      WRITE(NOUT,*) 'The matrix B:'
      DO I = 1, N
        WRITE(NOUT,FMT) B(I,:)
      ENDDO
!
   WRITE(NOUT,*) '---------------------------------------------------------'
   WRITE(NOUT,*)
