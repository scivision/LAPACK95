      PROGRAM LA_ZGESV_ET_EXAMPLE
!
!  -- LAPACK95 INTERFACE DRIVER ROUTINE (VERSION 3.0) --
!     UNI-C, DENMARK; UNIV. OF TENNESSEE, USA; NAG LTD., UK
!     SEPTEMBER, 2000
!
!  .. "Use Statements" ..
      USE LA_PRECISION, ONLY: WP => DP
      USE F95_LAPACK, ONLY: LA_GESV
!  .. "Implicit Statement" ..
      IMPLICIT NONE
!  .. "Parameters" ..
      CHARACTER(LEN=*), PARAMETER :: FMT =                              &
     &                   '(4(1X,1H(,F7.3,1H,,F7.3,1H):))'
      INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. "Local Scalars" ..
      INTEGER :: I, J, INFO, N, NRHS
!  .. "Local Arrays" ..
      INTEGER, ALLOCATABLE :: IPIV(:)
      COMPLEX(WP), ALLOCATABLE :: A(:,:), AA(:,:), B(:,:), BB(:,:), DUMMY(:,:)
      REAL(WP), ALLOCATABLE :: RR(:,:)
!  .. "Executable Statements" ..
      WRITE (NOUT,*) 'ZGESV ET_Example Program Results.'
      READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
      READ ( NIN, * ) N, NRHS
      ALLOCATE( A(N,N), AA(N,N), B(N,NRHS), BB(N,NRHS), IPIV(N), RR(N,N) )
!
      DO I = 1, N; READ (NIN, *) (RR(I, J), J = 1, N); ENDDO
      AA=RR
      DO J = 1, NRHS; BB(:,J) = SUM( AA, DIM=2)*J; ENDDO
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, N; WRITE (NOUT,FMT) AA(I,:); ENDDO
      WRITE(NOUT,*) 'The RHS matrix B:'
      DO J = 1, NRHS; WRITE (NOUT,FMT) BB(:,J);
      ENDDO
!
      WRITE ( NOUT, * )'-----------------------------------------------'
      WRITE ( NOUT, * )
      WRITE ( NOUT, * )'Details of LA_ZGESV LAPACK Subroutine Results.'
      WRITE ( NOUT, * )
!
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_GESV( A, B, IPIV, INFO )'
      A=AA; B=BB
      CALL LA_GESV(  A, B, IPIV, INFO )
      WRITE(NOUT,*) 'B - the solution vectors computed by LA_GESV,',    &
     &              ' INFO = ', INFO
      DO J = 1, NRHS; WRITE (NOUT,FMT) B(:,J); END DO
      WRITE(NOUT,*) 'Pivot vector'
      WRITE(NOUT,*) IPIV
!
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_GESV( A, B(:,1), IPIV, INFO )'
      A=AA; B=BB
      CALL LA_GESV(  A, B(:,1), IPIV, INFO )
      WRITE(NOUT,*) 'B - the solution vector computed by LA_GESV,',     &
     &              ' INFO = ', INFO
      WRITE (NOUT,FMT) B(:,1)
      WRITE(NOUT,*) 'Pivot vector'
      WRITE(NOUT,*) IPIV
!
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_GESV( A, B, INFO=INFO )'
      A=AA; B=BB
      CALL LA_GESV(  A, B, INFO=INFO )
      WRITE(NOUT,*) 'B - the solution vector computed by LA_GESV,',     &
     &              ' INFO = ', INFO
      DO J = 1, NRHS; WRITE (NOUT,FMT) B(:,J); END DO
!
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_GESV( A, B(:,1), INFO=INFO )'
      A=AA; B=BB
      CALL LA_GESV(  A, B(:,1), INFO=INFO )
      WRITE(NOUT,*) 'B - the solution vector computed by LA_GESV,',     &
     &              ' INFO = ', INFO
      WRITE (NOUT,FMT) B(:,1)
!
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_GESV( DUMMY, B, INFO=INFO )'
      A=AA; B=BB
      CALL LA_GESV(  DUMMY, B, INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO
!
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_GESV( DUMMY, B(:,1), INFO=INFO )'
      A=AA; B=BB
      CALL LA_GESV(  DUMMY, B(:,1), INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO
!
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_GESV( A, B(1:N-1,:), INFO=INFO )'
      A=AA; B=BB
      CALL LA_GESV(  A, B(1:N-1,:), INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO
!
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_GESV( A, B(1:N-1,1), INFO=INFO )'
      A=AA; B=BB
      CALL LA_GESV(  A, B(1:N-1,1), INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO
!
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_GESV( A, B, IPIV(1:N-1), INFO=INFO )'
      A=AA; B=BB
      CALL LA_GESV(  A, B, IPIV(1:N-1), INFO=INFO )
      WRITE(NOUT,*) 'INFO = ', INFO
!
      WRITE(NOUT,*)
      WRITE(NOUT,*) 'CALL LA_GESV( A, B(:,1), IPIV(1:N-1) )'
      A=AA; B=BB
      CALL LA_GESV(  A, B(:,1), IPIV(1:N-1) )
      WRITE(NOUT,*) 'INFO = ', INFO
!
      END PROGRAM LA_ZGESV_ET_EXAMPLE
