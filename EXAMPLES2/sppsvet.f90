PROGRAM LA_SPPSV_ET_EXAMPLE
!
!  -- LAPACK95 interface driver routine (version 3.0) --
!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!     September, 2000
!
!  .. USE STATEMENTS
   USE LA_PRECISION, ONLY: WP => SP
   USE F95_LAPACK, ONLY: LA_PPSV
!  .. IMPLICIT STATEMENT ..
   IMPLICIT NONE
!  .. PARAMETERS ..
      CHARACTER(LEN=*), PARAMETER :: FMT = '(8(1X,F10.3))'
   INTEGER, PARAMETER :: NIN=5, NOUT=6
!  .. LOCAL SCALARS ..
   INTEGER :: I, J, K, INFO, N, NRHS, NS
!  .. LOCAL ARRAYS ..
   REAL(WP), ALLOCATABLE :: A(:), B(:,:)
   REAL(WP), ALLOCATABLE :: AA(:), BB(:,:)
!  .. EXECUTABLE STATEMENTS ..
   WRITE (NOUT,*) 'SPPSV ET_Example Program Results.'
   READ ( NIN, * )   ! SKIP HEADING IN DATA FILE
   READ ( NIN, * ) N, NRHS
   PRINT *, 'N = ', N, ' NRHS = ', NRHS
   NS = N*(N+1)/2
   ALLOCATE ( A(NS), AA(NS), B(N,NRHS), BB(N,NRHS) )
!
        READ (NIN, *) AA
      BB = 0.0_WP
      DO K = 1, NRHS
         DO I = 1, N
            DO J = 1, I
               BB(J,K) = BB(J,K) + AA(J+(I-1)*I/2)
               IF ( J /= I ) BB(I,K) = BB(I,K) + AA(J+(I-1)*I/2)
            ENDDO
         ENDDO
         BB(:,K) = BB(:,K)*K
      ENDDO
   A=AA; B=BB
      WRITE(NOUT,*) 'The matrix A:'
      DO I = 1, N
        WRITE (NOUT,*) 'J = ', I; WRITE (NOUT,FMT) (A(J+(I-1)*I/2),J=1,I)
      ENDDO
      WRITE(NOUT,*) 'The RHS matrix B:'
      DO J = 1, NRHS
        WRITE (NOUT,*) 'RHS', J; WRITE (NOUT,FMT) B(:,J)
      ENDDO
!
   WRITE ( NOUT, * )'---------------------------------------------------------'
   WRITE ( NOUT, * )
   WRITE ( NOUT, * )'Details of LA_SPPSV LAPACK Subroutine Results.'
   WRITE ( NOUT, * )
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSV(A, B )'
   A=AA; B=BB
   CALL LA_PPSV( A, B )
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_PPSV:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) B(:,J); END DO
!
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSV(A, B(:,1) )'
   A=AA; B=BB
   CALL LA_PPSV( A, B(1:N,1) )
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_PPSV:'
   WRITE (NOUT,FMT) B(:,1)
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSV( A, B, ''L'' )'
   DO I = 1,N
      DO J = I, N
         A(J+(I-1)*(2*N-I)/2) = AA(I+J*(J-1)/2)
      ENDDO
   ENDDO
   B = BB
   CALL LA_PPSV( A, B, 'L' )
   WRITE(NOUT,*)'   B - the solution vectors computed by LA_PPSV:'
   DO J = 1, NRHS; WRITE (NOUT,FMT) B(:,J); END DO
! 
   WRITE(NOUT,*)
   WRITE(NOUT,*) 'CALL LA_PPSV( A, B(:,1), ''L'' )'
   DO I = 1,N
      DO J = I, N
         A(J+(I-1)*(2*N-I)/2) = AA(I+J*(J-1)/2)
      ENDDO
   ENDDO
   B = BB
   CALL LA_PPSV( A, B(1:N,1), 'L', INFO )
   WRITE(NOUT,*)'B - the solution vectors computed by LA_PPSV, INFO = ', INFO
   WRITE (NOUT,FMT) B(1:N,1)
!
END PROGRAM LA_SPPSV_ET_EXAMPLE
