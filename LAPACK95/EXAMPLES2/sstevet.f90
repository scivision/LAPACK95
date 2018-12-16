PROGRAM LA_SSTEV_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_STEV
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, INFO, N
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: DD(:), EE(:)
   REAL(WP), ALLOCATABLE :: D(:), E(:), Z(:,:), DUMMY(:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'SSTEV ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N
   PRINT *, 'N = ', N
   ALLOCATE ( D(N), DD(N), E(N), EE(N), Z(N,N) )
!
   READ (NIN, *) DD, EE
   D = DD; E = EE
   WRITE(NOUT,*) 'The matrix A:'
   WRITE (NOUT,*) 'D  '; WRITE (NOUT,FMT) D
   WRITE (NOUT,*) 'E '; WRITE (NOUT,FMT) E
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_SSTEV LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_STEV( D, E, Z, INFO )'
   D = DD; E = EE
   CALL LA_STEV( D, E, Z, INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' EIGENVALUES:'
   WRITE(NOUT,FMT) D
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_STEV( D, E, Z )'
   D = DD; E = EE
   CALL LA_STEV( D, E, Z )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) D
   WRITE(NOUT,*) 'EIGENVECTORS:'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) Z(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_STEV( D, E )'
   D = DD; E = EE
   CALL LA_STEV( D, E )
   WRITE(NOUT,*) ' EIGENVALUES:'
   WRITE(NOUT,FMT) D
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_STEV( DUMMY, E, INFO=INFO )'
   D = DD; E = EE
   CALL LA_STEV( DUMMY, E, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_STEV( D, E(1:N-3), INFO=INFO )'
   D = DD; E = EE
   CALL LA_STEV( D, E(1:N-3), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_STEV( D, E, Z=Z(1:N-1,:), INFO=INFO )'
   D = DD; E = EE
   CALL LA_STEV( D, E, Z=Z(1:N-1,:), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_STEV( D, E, Z=Z(:,1:N-1) )'
   D = DD; E = EE
   CALL LA_STEV( D, E, Z=Z(:,1:N-1) )
!
END PROGRAM LA_SSTEV_ET_EXAMPLE
