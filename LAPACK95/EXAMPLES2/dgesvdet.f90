PROGRAM LA_DGESVD_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_GESVD
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, INFO, M, N, MN
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: AA(:,:), S(:), WW(:)
   REAL(WP), ALLOCATABLE :: A(:,:), U(:,:), VT(:,:), DUMMY(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'DGESVD ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) M, N
   PRINT *, 'M = ', M, ' N = ', N
   MN = MIN(M,N)
   ALLOCATE ( A(M,N), AA(M,N), S(MN), U(M,M), VT(N,N), WW(1:MN-1) )
!
      READ (NIN, *) AA
      A=AA
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, M; WRITE (NOUT,*) 'I = ', I; WRITE (NOUT,FMT) A(I,:); ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_DGESVD LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( A, S, U, VT, WW, ''N'', INFO )'
   A=AA
   CALL LA_GESVD( A, S, U, VT, WW, 'N', INFO )
   WRITE(NOUT,*) 'INFO = ', INFO, ' Singular values:'
   WRITE(NOUT,FMT) S
   WRITE(NOUT,*) 'Orthogonal/unitary matrix U):'
   DO I = 1, M; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) U(:,I); END DO
   WRITE(NOUT,*) 'Orthogonal/unitary matrix V**H):'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VT(:,I); END DO
   IF( INFO > 0 )THEN
      WRITE(NOUT,*) 'Unconverged superdiagonal elements:'
      WRITE(NOUT,FMT) WW
   ENDIF
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( A, S, U, VT, WW, ''U'', INFO )'
   A=AA
   CALL LA_GESVD( A, S, U, VT, WW, 'U', INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( A, S, U, VT, WW, ''V'', INFO )'
   A=AA
   CALL LA_GESVD( A, S, U, VT, WW, 'V', INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( A, S, U, VT, WW, ''B'', INFO )'
   A=AA
   CALL LA_GESVD( A, S, U, VT, WW, 'B', INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( A, S, VT=VT, WW=WW, JOB=''U'', INFO=INFO )'
   A=AA
   CALL LA_GESVD( A, S, VT=VT, WW=WW, JOB='U', INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( A, S, U=U, WW=WW, JOB=''V'', INFO=INFO )'
   A=AA
   CALL LA_GESVD( A, S, U=U, WW=WW, JOB='V', INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( A, S, WW=WW, JOB=''V'', INFO=INFO )'
   A=AA
   CALL LA_GESVD( A, S, WW=WW, JOB='V', INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( A, S, WW=WW, JOB=''U'', INFO=INFO )'
   A=AA
   CALL LA_GESVD( A, S, WW=WW, JOB='U', INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( A, S, WW=WW, JOB=''N'', INFO=INFO )'
   A=AA
   CALL LA_GESVD( A, S, WW=WW, JOB='N', INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( A, S, U, VT )'
   A=AA
   CALL LA_GESVD( A, S, U, VT )
   WRITE(NOUT,*) ' Singular values:'
   WRITE(NOUT,FMT) S
   WRITE(NOUT,*) 'Orthogonal/unitary matrix U):'
   DO I = 1, M; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) U(:,I); END DO
   WRITE(NOUT,*) 'Orthogonal/unitary matrix V**H):'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VT(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( A, S, U(:,1:MN), VT(1:MN,:) )'
   A=AA; U = HUGE(1.0_WP); VT = HUGE(1.0_WP)
   CALL LA_GESVD( A, S, U(:,1:MN), VT(1:MN,:) )
   WRITE(NOUT,*) ' Singular values:'
   WRITE(NOUT,FMT) S
   WRITE(NOUT,*) 'Orthogonal/unitary matrix U):'
   DO I = 1, M; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) U(:,I); END DO
   WRITE(NOUT,*) 'Orthogonal/unitary matrix V**H):'
   DO I = 1, N; WRITE(NOUT,*) 'I = ', I; WRITE (NOUT,FMT) VT(:,I); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( A, S )'
   A=AA
   CALL LA_GESVD( A, S )
   WRITE(NOUT,*) ' Singular values:'
   WRITE(NOUT,FMT) S
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( DUMMY, S, INFO=INFO )'
   A=AA
   CALL LA_GESVD( DUMMY, S, INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( A, S(1:MAX(M,N)+1), INFO=INFO )'
   A=AA
   CALL LA_GESVD( A, S(1:MAX(M,N)+1), INFO=INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( A, S, U(1:M-1,:), VT, WW, ''N'',INFO )'
   A=AA
   CALL LA_GESVD( A, S, U(1:M-1,:), VT, WW, 'N', INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( A, S, U(:,1:MN-1), VT, WW, ''N'',INFO )'
   A=AA
   CALL LA_GESVD( A, S, U(:,1:MN-1), VT, WW, 'N', INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( A, S, U, VT(1:MN-1,:), WW, ''N'',INFO )'
   A=AA
   CALL LA_GESVD( A, S, U, VT(1:MN-1,:), WW, 'N', INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( A, S, U, VT(:,1:N-1), WW, ''N'',INFO )'
   A=AA
   CALL LA_GESVD( A, S, U, VT(:,1:N-1), WW, 'N', INFO )
   WRITE(NOUT,*) 'INFO = ', INFO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_GESVD( A, S, U, VT, WW(1:MN) )'
   A=AA
   CALL LA_GESVD( A, S, U, VT, WW(1:MN) )
!
END PROGRAM LA_DGESVD_ET_EXAMPLE
