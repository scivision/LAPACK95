PROGRAM LA_DSYSV_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => DP
   USE F95_LAPACK, ONLY: LA_SYSV
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, J, FAIL, N, NRHS
!  .. LOCAL ARRAYS ..
   INTEGER, ALLOCATABLE :: PIV(:)
   REAL(WP), ALLOCATABLE :: A(:,:), B(:,:)
   REAL(WP), ALLOCATABLE :: AA(:,:), BB(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'DSYSV ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N, NRHS
   PRINT *, 'N = ', N, ' NRHS = ', NRHS
   ALLOCATE ( A(N,N), AA(N,N), B(N,NRHS), BB(N,NRHS), PIV(N) )
!
      AA = HUGE(1.0_WP)
      DO I = 1, N
        READ (NIN, *) AA(I,I:N)
      ENDDO
      DO J = 1, NRHS
         DO I = 1, N
            BB(I,J) = ( SUM( AA(I,I:N) ) + SUM( AA(1:I-1,I) ) )*J
         ENDDO
      ENDDO
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
   WRITE ( NOUT, * )'Details of LA_DSYSV LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SYSV(A, B )'
   A=AA; B=BB
   CALL LA_SYSV( A, B )
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_SYSV:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) B(:,J); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SYSV(A, B(:,1) )'
   A=AA; B=BB
   CALL LA_SYSV( A, B(1:N,1) )
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_SYSV:'
   WRITE (NOUT,FMT) B(:,1)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SYSV( A, B, ''L'' )'
   A=TRANSPOSE(AA); B=BB
   CALL LA_SYSV( A, B, 'L' )
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_SYSV:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) B(:,J); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_SYSV( A, B(1:N,1), IPIV=PIV, INFO=FAIL )'
   A=AA; B=BB
   CALL LA_SYSV( A, B(1:N,1), IPIV=PIV, INFO=FAIL )
   WRITE(NOUT,*)'B - the solution vectors computed by LA_SYSV, INFO = ', FAIL
   WRITE (NOUT,FMT) B(1:N,1)
   WRITE (NOUT,*) 'Pivots: ', PIV
!
END PROGRAM LA_DSYSV_ET_EXAMPLE
