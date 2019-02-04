PROGRAM LA_DSTEVD_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_STEVD
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, INFO, N
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: D(:), DD(:), E(:), EE(:), Z(:,:), DUMMY(:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'DSTEVD ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N
   PRINT *, 'N = ', N
   ALLOCATE ( D(N), DD(N), E(N-1), EE(N-1), Z(N,N) )
!
   READ (NIN, *) DD, EE
   WRITE(NOUT,*) 'The matrix A:'
   WRITE (NOUT,*) 'D  '; WRITE (NOUT,FMT) DD
   WRITE (NOUT,*) 'EE '; WRITE (NOUT,FMT) EE
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_DSTEVX LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_STEVD( D, E, Z, INFO )'
   D = DD; E = EE
   CALL LA_STEVD( D, E, Z, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' EIGENVALUES:'
   WRITE(NOUT,FMT) D
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_STEVD( D, E, Z )'
   D = DD; E = EE
   CALL LA_STEVD( D, E, Z )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) D
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_STEVD( D, E )'
   D = DD; E = EE
   CALL LA_STEVD( D, E )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) D
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_STEVD( DUMMY, E, INFO=INFO )'
   D = DD; E = EE
   CALL LA_STEVD( DUMMY, E, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_STEVD( D, E(1:N), INFO=INFO )'
   D = DD; E = EE
   CALL LA_STEVD( D, E(1:N), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_STEVD( D, E, Z=Z(1:N-1,:), INFO=INFO )'
   D = DD; E = EE
   CALL LA_STEVD( D, E, Z=Z(1:N-1,:), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_STEVD( D, E, Z=Z(:,1:N-1) )'
   D = DD; E = EE
   CALL LA_STEVD( D, E, Z=Z(:,1:N-1) )
!
END PROGRAM LA_DSTEVD_ET_EXAMPLE
