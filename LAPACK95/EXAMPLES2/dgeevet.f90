PROGRAM LA_DGEEV_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_GEEV
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, INFO, N
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: AA(:,:)
   REAL(WP), ALLOCATABLE :: A(:,:), WR(:), WI(:), VL(:,:), VR(:,:), DUMMY(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'DGEEV ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N
   PRINT *, 'N = ', N
   ALLOCATE ( A(N,N), AA(N,N), WR(N), WI(N), VL(N,N), VR(N,N) )
!
      READ (NIN, *) AA
   A=AA
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, N; WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,:); ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_DGEEV LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, WR, WI, VL, VR, INFO )'
   A=AA
   CALL LA_GEEV( A, WR, WI, VL, VR, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) WR
   WRITE(NOUT,FMT) WI
   WRITE(NOUT,*) 'Eigenvectors ( VL and VR ):'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VL(:,I); END DO
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VR(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, WR, WI, VL )'
   A=AA
   CALL LA_GEEV( A, WR, WI, VL )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) WR
   WRITE(NOUT,FMT) WI
   WRITE(NOUT,*) 'Schur vectors ( Only VL ):'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VL(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, WR, WI, VR )'
   A=AA
   CALL LA_GEEV( A, WR, WI, VR )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) WR
   WRITE(NOUT,FMT) WI
   WRITE(NOUT,*) 'Schur vectors ( Only VR ):'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VR(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, WR, WI )'
   A=AA
   CALL LA_GEEV( A, WR, WI )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) WR
   WRITE(NOUT,FMT) WI
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( DUMMY, WR, WI, VL, VR, INFO )'
   A=AA
   CALL LA_GEEV( DUMMY, WR, WI, VL, VR, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, WR(1:N-1), WI, VL, VR, INFO )'
   A=AA
   CALL LA_GEEV( A, WR(1:N-1), WI, VL, VR, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, WR, WI(1:N-1), VL, VR, INFO )'
   A=AA
   CALL LA_GEEV( A, WR, WI(1:N-1), VL, VR, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, WR, WI, VL(1:N-1,:), VR, INFO )'
   A=AA
   CALL LA_GEEV( A, WR, WI, VL(1:N-1,:), VR, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, WR, WI, VL(:,1:N-1), VR, INFO )'
   A=AA
   CALL LA_GEEV( A, WR, WI, VL(:,1:N-1), VR, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, WR, WI, VL, VR(1:N-1,:), INFO )'
   A=AA
   CALL LA_GEEV( A, WR, WI, VL, VR(1:N-1,:), INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEV( A, WR, WI, VL, VR(:,1:N-1), INFO )'
   A=AA
   CALL LA_GEEV( A, WR, WI, VL, VR(:,1:N-1) )
   WRITE(NOUT,*) 'INFO = ', INFO
!
END PROGRAM LA_DGEEV_ET_EXAMPLE
