PROGRAM LA_CGEEVX_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_GEEVX
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, INFO, N, ILO, IHI
   REAL(WP) :: ABNRM
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: AA(:,:), SCALE(:), RCONDE(:), RCONDV(:)
   COMPLEX(WP), ALLOCATABLE :: A(:,:), W(:), VL(:,:), VR(:,:), DUMMY(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'CGEEVX ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N
   PRINT *, 'N = ', N
   ALLOCATE ( A(N,N), AA(N,N), W(N), VL(N,N), VR(N,N), SCALE(N), RCONDE(N), &
              RCONDV(N) )
!
   READ (NIN, *) AA
   A=AA
   WRITE(NOUT,*) 'The matrix A:'
   DO I = 1, N; WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,:); ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_CGEEVX LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, W, VL, VR, BALANC, &'
   WRITE(NOUT,*) '               ILO, IHI, SCALE, ABNRM, RCONDE, RCONDV, INFO )'
   A=AA
   CALL LA_GEEVX( A, W, VL, VR, 'N', ILO, IHI, SCALE, &
                  ABNRM, RCONDE, RCONDV, INFO )
   WRITE(NOUT,*) ' ILO, IHI, INFO:', ILO, IHI, INFO
   WRITE(NOUT,*) ' SCALE, ABNRM, RCONDE, RCONDV:'
   WRITE(NOUT,*)   SCALE, ABNRM, RCONDE, RCONDV
   WRITE(NOUT,*) ' Eigenvalues:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'Eigenvectors ( VL and VR ):'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VL(:,I); END DO
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VR(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, W, VL )'
   A=AA
   CALL LA_GEEVX( A, W, VL )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'Eigenvectors ( Only VL ):'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VL(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, W, VR )'
   A=AA
   CALL LA_GEEVX( A, W, VR )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) W
   WRITE(NOUT,*) 'Eigenvectors ( Only VR ):'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VR(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, W, INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, W, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) W
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, W, BALANC=''P'', INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, W, BALANC='P', INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) W
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, W, BALANC=''S'', INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, W, BALANC='S', INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) W
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, W, BALANC=''B'', INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, W, BALANC='B', INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) W
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, W, BALANC=''1'', INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, W, BALANC='1', INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) W
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( DUMMY, W, VL, VR, INFO=INFO )'
   A=AA
   CALL LA_GEEVX( DUMMY, W, VL, VR, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, W(1:N-1), VL, VR, INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, W(1:N-1), VL, VR, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, W(1:N-1), VL, VR, INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, W(1:N-1), VL, VR, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, W, VL(1:N-1,:), VR, INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, W, VL(1:N-1,:), VR, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, W, VL(:,1:N-1), VR, INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, W, VL(:,1:N-1), VR, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, W, VL, VR(1:N-1,:), INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, W, VL, VR(1:N-1,:), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, W, VL, VR(:,1:N-1), INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, W, VL, VR(:,1:N-1), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
END PROGRAM LA_CGEEVX_ET_EXAMPLE
