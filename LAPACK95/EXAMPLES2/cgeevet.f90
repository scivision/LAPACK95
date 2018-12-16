PROGRAM LA_CGEEV_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_GEEV
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, INFO, N
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: AA(:,:)
   COMPLEX(WP), ALLOCATABLE :: A(:,:), W(:), VL(:,:), VR(:,:), DUMMY(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'CGEEV ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N
   PRINT *, 'N = ', N
   ALLOCATE ( A(N,N), AA(N,N), W(N), VL(N,N), VR(N,N) )
!
      READ (NIN, *) AA
   A=AA
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, N; WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,:); ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_CGEEV LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, W, VL, VR, INFO )'
   A=AA
   CALL LA_GEEV( A, W, VL, VR, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'Eigenvectors ( VL and VR ):'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VL(:,I); END DO
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VR(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, W, VL )'
   A=AA
   CALL LA_GEEV( A, W, VL )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'Schur vectors ( Only VL ):'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VL(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, W, VR )'
   A=AA
   CALL LA_GEEV( A, W, VR )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'Schur vectors ( Only VR ):'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VR(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, W )'
   A=AA
   CALL LA_GEEV( A, W )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) W
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( DUMMY, W, VL, VR, INFO )'
   A=AA
   CALL LA_GEEV( DUMMY, W, VL, VR, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, W(1:N-1), VL, VR, INFO )'
   A=AA
   CALL LA_GEEV( A, W(1:N-1), VL, VR, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, W(1:N-1), VL, VR, INFO )'
   A=AA
   CALL LA_GEEV( A, W(1:N-1), VL, VR, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, W, VL(1:N-1,:), VR, INFO )'
   A=AA
   CALL LA_GEEV( A, W, VL(1:N-1,:), VR, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, W, VL(:,1:N-1), VR, INFO )'
   A=AA
   CALL LA_GEEV( A, W, VL(:,1:N-1), VR, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, W, VL, VR(1:N-1,:), INFO )'
   A=AA
   CALL LA_GEEV( A, W, VL, VR(1:N-1,:), INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, W, VL, VR(:,1:N-1), INFO )'
   A=AA
   CALL LA_GEEV( A, W, VL, VR(:,1:N-1) )
   WRITE(NOUT,*) 'INFO = ', INFO
!
END PROGRAM LA_CGEEV_ET_EXAMPLE
