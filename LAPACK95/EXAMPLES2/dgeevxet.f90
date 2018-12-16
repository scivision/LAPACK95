PROGRAM LA_DGEEVX_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_GEEVX
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, INFO, N, ILO, IHI
   REAL(WP) :: ABNRM
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: AA(:,:), SCALE(:), RCONDE(:), RCONDV(:)
   REAL(WP), ALLOCATABLE :: A(:,:), WR(:), WI(:), VL(:,:), VR(:,:), DUMMY(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'DGEEVX ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N
   PRINT *, 'N = ', N
   ALLOCATE ( A(N,N), AA(N,N), WR(N), WI(N), VL(N,N), VR(N,N), SCALE(N), &
              RCONDE(N), RCONDV(N) )
!
   READ (NIN, *) AA
   A=AA
   WRITE(NOUT,*) 'The matrix A:'
   DO I = 1, N; WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,:); ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_DGEEVX LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, WR, WI, VL, VR, BALANC, &'
   WRITE(NOUT,*) '               ILO, IHI, SCALE, ABNRM, RCONDE, RCONDV, INFO )'
   A=AA
   CALL LA_GEEVX( A, WR, WI, VL, VR, 'N', ILO, IHI, SCALE, &
                  ABNRM, RCONDE, RCONDV, INFO )
   WRITE(NOUT,*) ' ILO, IHI, INFO:', ILO, IHI, INFO
   WRITE(NOUT,*) ' SCALE, ABNRM, RCONDE, RCONDV:'
   WRITE(NOUT,*)   SCALE, ABNRM, RCONDE, RCONDV
   WRITE(NOUT,*) ' Eigenvalues:'
   WRITE(NOUT,FMT) WR
   WRITE(NOUT,FMT) WI
   WRITE(NOUT,*) 'Eigenvectors ( VL and VR ):'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VL(:,I); END DO
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VR(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, WR, WI, VL )'
   A=AA
   CALL LA_GEEVX( A, WR, WI, VL )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) WR
   WRITE(NOUT,FMT) WI
   WRITE(NOUT,*) 'Eigenvectors ( Only VL ):'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VL(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, WR, WI, VR )'
   A=AA
   CALL LA_GEEVX( A, WR, WI, VR )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) WR
   WRITE(NOUT,FMT) WI
   WRITE(NOUT,*) 'Eigenvectors ( Only VR ):'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VR(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, WR, WI, INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, WR, WI, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) WR
   WRITE(NOUT,FMT) WI
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, WR, WI, BALANC=''P'', INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, WR, WI, BALANC='P', INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) WR
   WRITE(NOUT,FMT) WI
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, WR, WI, BALANC=''S'', INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, WR, WI, BALANC='S', INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) WR
   WRITE(NOUT,FMT) WI
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, WR, WI, BALANC=''B'', INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, WR, WI, BALANC='B', INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) WR
   WRITE(NOUT,FMT) WI
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, WR, WI, BALANC=''1'', INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, WR, WI, BALANC='1', INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Eigenvalues:'
   WRITE(NOUT,FMT) WR
   WRITE(NOUT,FMT) WI
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( DUMMY, WR, WI, VL, VR, INFO=INFO )'
   A=AA
   CALL LA_GEEVX( DUMMY, WR, WI, VL, VR, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, WR(1:N-1), WI, VL, VR, INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, WR(1:N-1), WI, VL, VR, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, WR, WI(1:N-1), VL, VR, INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, WR, WI(1:N-1), VL, VR, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, WR, WI, VL(1:N-1,:), VR, INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, WR, WI, VL(1:N-1,:), VR, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, WR, WI, VL(:,1:N-1), VR, INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, WR, WI, VL(:,1:N-1), VR, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, WR, WI, VL, VR(1:N-1,:), INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, WR, WI, VL, VR(1:N-1,:), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GEEVX( A, WR, WI, VL, VR(:,1:N-1), INFO=INFO )'
   A=AA
   CALL LA_GEEVX( A, WR, WI, VL, VR(:,1:N-1), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
END PROGRAM LA_DGEEVX_ET_EXAMPLE
